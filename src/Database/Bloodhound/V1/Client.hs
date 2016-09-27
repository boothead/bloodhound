{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.V1.Client
 ( module Database.Bloodhound.Common.Client
 -- *** Index Aliases
 , updateIndexAliases
 , getIndexAliases
 , deleteMapping
 , searchAll
 , searchByIndex
 , searchByType
 , scanSearch
 , getInitialScroll
 , pageSearch
 , mkSearch
 , mkAggregateSearch
 , mkHighlightSearch
       ) where
  
import qualified Blaze.ByteString.Builder     as BB
import           Control.Applicative          as A
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Foldable                (toList)
import qualified Data.HashMap.Strict          as HM
import           Data.Ix
import qualified Data.List                    as LS (filter, foldl')
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Maybe                   (catMaybes, fromMaybe, isJust)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method    as NHTM
import qualified Network.HTTP.Types.Status    as NHTS
import qualified Network.HTTP.Types.URI       as NHTU
import qualified Network.URI                  as URI
import           Prelude                      hiding (filter, head)

import Database.Bloodhound.Common.Client
import Database.Bloodhound.Common.Util
import Database.Bloodhound.V1.Types

-- | 'updateIndexAliases' updates the server's index alias
-- table. Operations are atomic. Explained in further detail at
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html>
--
-- >>> let src = IndexName "a-real-index"
-- >>> let aliasName = IndexName "an-alias"
-- >>> let iAlias = IndexAlias src (IndexAliasName aliasName)
-- >>> let aliasCreate = IndexAliasCreate Nothing Nothing
-- >>> _ <- runBH' $ deleteIndex src
-- >>> respIsTwoHunna <$> runBH' (createIndex defaultIndexSettings src)
-- True
-- >>> runBH' $ indexExists src
-- True
-- >>> respIsTwoHunna <$> runBH' (updateIndexAliases (AddAlias iAlias aliasCreate :| []))
-- True
-- >>> runBH' $ indexExists aliasName
-- True
updateIndexAliases :: MonadBH m => NonEmpty IndexAliasAction -> m Reply
updateIndexAliases actions = bindM2 post url (return body)
  where url = joinPath ["_aliases"]
        body = Just (encode bodyJSON)
        bodyJSON = object [ "actions" .= toList actions]

-- | Get all aliases configured on the server.
getIndexAliases :: (MonadBH m, MonadThrow m)
                => m (Either EsError IndexAliasesSummary)
getIndexAliases = parseEsResponse =<< get =<< url
  where url = joinPath ["_aliases"]

-- | 'deleteMapping' is an HTTP DELETE and deletes a mapping for a given index.
-- Mappings are schemas for documents in indexes.
--
deleteMapping :: MonadBH m => IndexName -> MappingName -> m Reply
deleteMapping (IndexName indexName)
  (MappingName mappingName) =
  -- "_mapping" and mappingName below were originally transposed
  -- erroneously. The correct API call is: "/INDEX/_mapping/MAPPING_NAME"
  delete =<< joinPath [indexName, "_mapping", mappingName]

dispatchSearch :: MonadBH m => Text -> Search -> m Reply
dispatchSearch url search = post url' (Just (encode search))
  where url' = appendSearchTypeParam url (searchType search)

-- | 'searchAll', given a 'Search', will perform that search against all indexes
--   on an Elasticsearch server. Try to avoid doing this if it can be helped.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> reply <- runBH' $ searchAll search
searchAll :: MonadBH m => Search -> m Reply
searchAll = bindM2 dispatchSearch url . return
  where url = joinPath ["_search"]

-- | 'searchByIndex', given a 'Search' and an 'IndexName', will perform that search
--   against all mappings within an index on an Elasticsearch server.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> reply <- runBH' $ searchByIndex testIndex search
searchByIndex :: MonadBH m => IndexName -> Search -> m Reply
searchByIndex (IndexName indexName) = bindM2 dispatchSearch url . return
  where url = joinPath [indexName, "_search"]

-- | 'searchByType', given a 'Search', 'IndexName', and 'MappingName', will perform that
--   search against a specific mapping within an index on an Elasticsearch server.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> reply <- runBH' $ searchByType testIndex testMapping search
searchByType :: MonadBH m => IndexName -> MappingName -> Search
                -> m Reply
searchByType (IndexName indexName)
  (MappingName mappingName) = bindM2 dispatchSearch url . return
  where url = joinPath [indexName, mappingName, "_search"]

-- | For a given scearch, request a scroll for efficient streaming of
-- search results. Note that the search is put into 'SearchTypeScan'
-- mode and thus results will not be sorted. Combine this with
-- 'advanceScroll' to efficiently stream through the full result set
getInitialScroll :: MonadBH m => IndexName -> MappingName -> Search -> m (Maybe ScrollId)
getInitialScroll (IndexName indexName) (MappingName mappingName) search = do
    let url = joinPath [indexName, mappingName, "_search"]
        search' = search { searchType = SearchTypeScan }
    resp' <- bindM2 dispatchSearch url (return search')
    let msr = decode' $ responseBody resp' :: Maybe (SearchResult ())
        msid = maybe Nothing scrollId msr
    return msid

-- | 'scanSearch' uses the 'scan&scroll' API of elastic,
-- for a given 'IndexName' and 'MappingName'. Note that this will
-- consume the entire search result set and will be doing O(n) list
-- appends so this may not be suitable for large result sets. In that
-- case, 'getInitialScroll' and 'advanceScroll' are good low level
-- tools. You should be able to hook them up trivially to conduit,
-- pipes, or your favorite streaming IO abstraction of choice. Note
-- that ordering on the search would destroy performance and thus is
-- ignored.
scanSearch :: (FromJSON a, MonadBH m, MonadThrow m) => IndexName -> MappingName -> Search -> m [Hit a]
scanSearch indexName mappingName search = do
    msid <- getInitialScroll indexName mappingName search
    (hits, msid') <- scroll' msid
    (totalHits, _) <- simpleAccumulator [] (hits, msid')
    return totalHits

-- | 'mkSearch' is a helper function for defaulting additional fields of a 'Search'
--   to Nothing in case you only care about your 'Query' and 'Filter'. Use record update
--   syntax if you want to add things like aggregations or highlights while still using
--   this helper function.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> mkSearch (Just query) Nothing
-- Search {queryBody = Just (TermQuery (Term {termField = "user", termValue = "bitemyapp"}) Nothing), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = From 0, size = Size 10, searchType = SearchTypeQueryThenFetch, fields = Nothing, source = Nothing}
mkSearch :: Maybe Query -> Maybe Filter -> Search
mkSearch query filter = Search query filter Nothing Nothing Nothing False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing

-- | 'mkAggregateSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
--
-- >>> let terms = TermsAgg $ (mkTermsAggregation "user") { termCollectMode = Just BreadthFirst }
-- >>> terms
-- TermsAgg (TermsAggregation {term = Left "user", termInclude = Nothing, termExclude = Nothing, termOrder = Nothing, termMinDocCount = Nothing, termSize = Nothing, termShardSize = Nothing, termCollectMode = Just BreadthFirst, termExecutionHint = Nothing, termAggs = Nothing})
-- >>> let myAggregation = mkAggregateSearch Nothing $ mkAggregations "users" terms
mkAggregateSearch :: Maybe Query -> Aggregations -> Search
mkAggregateSearch query mkSearchAggs = Search query Nothing Nothing (Just mkSearchAggs) Nothing False (From 0) (Size 0) SearchTypeQueryThenFetch Nothing Nothing

-- | 'mkHighlightSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
--
-- >>> let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
-- >>> let testHighlight = Highlights Nothing [FieldHighlight (FieldName "message") Nothing]
-- >>> let search = mkHighlightSearch (Just query) testHighlight
mkHighlightSearch :: Maybe Query -> Highlights -> Search
mkHighlightSearch query searchHighlights = Search query Nothing Nothing Nothing (Just searchHighlights) False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing

-- | 'pageSearch' is a helper function that takes a search and assigns the from
--    and size fields for the search. The from parameter defines the offset
--    from the first result you want to fetch. The size parameter allows you to
--    configure the maximum amount of hits to be returned.
--
-- >>> let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
-- >>> let search = mkSearch (Just query) Nothing
-- >>> search
-- Search {queryBody = Just (QueryMatchQuery (MatchQuery {matchQueryField = FieldName "_all", matchQueryQueryString = QueryString "haskell", matchQueryOperator = Or, matchQueryZeroTerms = ZeroTermsNone, matchQueryCutoffFrequency = Nothing, matchQueryMatchType = Nothing, matchQueryAnalyzer = Nothing, matchQueryMaxExpansions = Nothing, matchQueryLenient = Nothing, matchQueryBoost = Nothing})), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = From 0, size = Size 10, searchType = SearchTypeQueryThenFetch, fields = Nothing, source = Nothing}
-- >>> pageSearch (From 10) (Size 100) search
-- Search {queryBody = Just (QueryMatchQuery (MatchQuery {matchQueryField = FieldName "_all", matchQueryQueryString = QueryString "haskell", matchQueryOperator = Or, matchQueryZeroTerms = ZeroTermsNone, matchQueryCutoffFrequency = Nothing, matchQueryMatchType = Nothing, matchQueryAnalyzer = Nothing, matchQueryMaxExpansions = Nothing, matchQueryLenient = Nothing, matchQueryBoost = Nothing})), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = From 10, size = Size 100, searchType = SearchTypeQueryThenFetch, fields = Nothing, source = Nothing}
pageSearch :: From     -- ^ The result offset
           -> Size     -- ^ The number of results to return
           -> Search  -- ^ The current seach
           -> Search  -- ^ The paged search
pageSearch resultOffset pageSize search = search { from = resultOffset, size = pageSize }
