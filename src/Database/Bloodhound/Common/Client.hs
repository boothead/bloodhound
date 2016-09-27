{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Common.Client
-- Copyright : (C) 2014 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com
-- Stability : provisional
-- Portability : OverloadedStrings
--
-- Client side functions for talking to Elasticsearch servers.
--
-------------------------------------------------------------------------------

module Database.Bloodhound.Common.Client
       ( -- * Bloodhound client functions
         -- | The examples in this module assume the following code has been run.
         --   The :{ and :} will only work in GHCi. You'll only need the data types
         --   and typeclass instances for the functions that make use of them.

         -- $setup
         withBH
       -- ** Indices
       , createIndex
       , deleteIndex
       , updateIndexSettings
       , getIndexSettings
       , optimizeIndex
       , indexExists
       , openIndex
       , closeIndex
       , listIndices

       -- *** Index Templates
       , putTemplate
       , templateExists
       , deleteTemplate
       -- ** Mapping
       , putMapping
       -- ** Documents
       , indexDocument
       , updateDocument
       , getDocument
       , documentExists
       , deleteDocument
       -- ** Searching
       , advanceScroll
       , refreshIndex
       , bulk
       , mkShardCount
       , mkReplicaCount
       , getStatus
       -- ** Snapshot/Restore
       -- *** Snapshot Repos
       , getSnapshotRepos
       , updateSnapshotRepo
       , verifySnapshotRepo
       , deleteSnapshotRepo
       -- *** Snapshots
       , createSnapshot
       , getSnapshots
       , deleteSnapshot
       -- *** Restoring Snapshots
       , restoreSnapshot
       -- ** Nodes
       , getNodesInfo
       , getNodesStats
       -- ** Request Utilities
       , encodeBulkOperations
       , encodeBulkOperation
       -- * Authentication
       , basicAuthHook
       -- * Reply-handling tools
       , isVersionConflict
       , isSuccess
       , isCreated
       , parseEsResponse
       
       , simpleAccumulator
       , scroll'
       , bindM2
       , appendSearchTypeParam
       )
       where

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

import           Database.Bloodhound.Common.Types
import           Database.Bloodhound.Common.Util

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDeriveGeneric
-- >>> import Database.Bloodhound
-- >>> import Test.DocTest.Prop (assert)
-- >>> let testServer = (Server "http://localhost:9200")
-- >>> let runBH' = withBH defaultManagerSettings testServer
-- >>> let testIndex = IndexName "twitter"
-- >>> let testMapping = MappingName "tweet"
-- >>> let defaultIndexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
-- >>> data TweetMapping = TweetMapping deriving (Eq, Show)
-- >>> _ <- runBH' $ deleteIndex testIndex >> deleteMapping testIndex testMapping
-- >>> import GHC.Generics
-- >>> import           Data.Time.Calendar        (Day (..))
-- >>> import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
-- >>> :{
--instance ToJSON TweetMapping where
--          toJSON TweetMapping =
--            object ["tweet" .=
--              object ["properties" .=
--                object ["location" .=
--                  object ["type" .= ("geo_point" :: Text)]]]]
--data Location = Location { lat :: Double
--                         , lon :: Double } deriving (Eq, Generic, Show)
--data Tweet = Tweet { user     :: Text
--                    , postDate :: UTCTime
--                    , message  :: Text
--                    , age      :: Int
--                    , location :: Location } deriving (Eq, Generic, Show)
--exampleTweet = Tweet { user     = "bitemyapp"
--                      , postDate = UTCTime
--                                   (ModifiedJulianDay 55000)
--                                   (secondsToDiffTime 10)
--                      , message  = "Use haskell!"
--                      , age      = 10000
--                      , location = Location 40.12 (-71.34) }
--instance ToJSON   Tweet where
--  toJSON = genericToJSON defaultOptions
--instance FromJSON Tweet where
--  parseJSON = genericParseJSON defaultOptions
--instance ToJSON   Location where
--  toJSON = genericToJSON defaultOptions
--instance FromJSON Location where
--  parseJSON = genericParseJSON defaultOptions
--data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
--instance FromJSON BulkTest where
--  parseJSON = genericParseJSON defaultOptions
--instance ToJSON BulkTest where
--  toJSON = genericToJSON defaultOptions
-- :}

-- | 'mkShardCount' is a straight-forward smart constructor for 'ShardCount'
--   which rejects 'Int' values below 1 and above 1000.
--
-- >>> mkShardCount 10
-- Just (ShardCount 10)
mkShardCount :: Int -> Maybe ShardCount
mkShardCount n
  | n < 1 = Nothing
  | n > 1000 = Nothing
  | otherwise = Just (ShardCount n)

-- | 'mkReplicaCount' is a straight-forward smart constructor for 'ReplicaCount'
--   which rejects 'Int' values below 0 and above 1000.
--
-- >>> mkReplicaCount 10
-- Just (ReplicaCount 10)
mkReplicaCount :: Int -> Maybe ReplicaCount
mkReplicaCount n
  | n < 0 = Nothing
  | n > 1000 = Nothing -- ...
  | otherwise = Just (ReplicaCount n)

appendSearchTypeParam :: Text -> SearchType -> Text
appendSearchTypeParam originalUrl st = addQuery params originalUrl
  where stText = "search_type"
        params
          | st == SearchTypeDfsQueryThenFetch = [(stText, Just "dfs_query_then_fetch")]
          | st == SearchTypeCount             = [(stText, Just "count")]
          | st == SearchTypeScan              = [(stText, Just "scan"), ("scroll", Just "1m")]
          | st == SearchTypeQueryAndFetch     = [(stText, Just "query_and_fetch")]
          | st == SearchTypeDfsQueryAndFetch  = [(stText, Just "dfs_query_and_fetch")]
        -- used to catch 'SearchTypeQueryThenFetch', which is also the default
          | otherwise                         = [(stText, Just "query_then_fetch")]

-- | Severely dumbed down query renderer. Assumes your data doesn't
-- need any encoding
addQuery :: [(Text, Maybe Text)] -> Text -> Text
addQuery q u = u <> rendered
  where
    rendered =
      T.decodeUtf8 $ BB.toByteString $ NHTU.renderQueryText prependQuestionMark q
    prependQuestionMark = True

bindM2 :: (Applicative m, Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = join (f <$> ma <*> mb)

-- | Convenience function that sets up a manager and BHEnv and runs
-- the given set of bloodhound operations. Connections will be
-- pipelined automatically in accordance with the given manager
-- settings in IO. If you've got your own monad transformer stack, you
-- should use 'runBH' directly.
withBH :: ManagerSettings -> Server -> BH IO a -> IO a
withBH ms s f = do
  mgr <- newManager ms
  let env = mkBHEnv s mgr
  runBH env f


-- indexDocument s ix name doc = put (root </> s </> ix </> name </> doc) (Just encode doc)
-- http://hackage.haskell.org/package/http-client-lens-0.1.0/docs/Network-HTTP-Client-Lens.html
-- https://github.com/supki/libjenkins/blob/master/src/Jenkins/Rest/Internal.hs

-- | 'getStatus' fetches the 'Status' of a 'Server'
--
-- >>> serverStatus <- runBH' getStatus
-- >>> fmap status (serverStatus)
-- Just 200
getStatus :: MonadBH m => m (Maybe Status)
getStatus = do
  response <- get =<< url
  return $ decode (responseBody response)
  where url = joinPath []

-- | 'getSnapshotRepos' gets the definitions of a subset of the
-- defined snapshot repos.
getSnapshotRepos
    :: ( MonadBH m
       , MonadThrow m
       )
    => SnapshotRepoSelection
    -> m (Either EsError [GenericSnapshotRepo])
getSnapshotRepos sel = fmap (fmap unGSRs) . parseEsResponse =<< get =<< url
  where
    url = joinPath ["_snapshot", selectorSeg]
    selectorSeg = case sel of
                    AllSnapshotRepos -> "_all"
                    SnapshotRepoList (p :| ps) -> T.intercalate "," (renderPat <$> (p:ps))
    renderPat (RepoPattern t)                  = t
    renderPat (ExactRepo (SnapshotRepoName t)) = t


-- | Wrapper to extract the list of 'GenericSnapshotRepo' in the
-- format they're returned in
newtype GSRs = GSRs { unGSRs :: [GenericSnapshotRepo] }


instance FromJSON GSRs where
  parseJSON = withObject "Collection of GenericSnapshotRepo" parse
    where
      parse = fmap GSRs . mapM (uncurry go) . HM.toList
      go rawName = withObject "GenericSnapshotRepo" $ \o -> do
        GenericSnapshotRepo (SnapshotRepoName rawName) <$> o .: "type"
                                                       <*> o .: "settings"


-- | Create or update a snapshot repo
updateSnapshotRepo
  :: ( MonadBH m
     , SnapshotRepo repo
     )
  => SnapshotRepoUpdateSettings
  -- ^ Use 'defaultSnapshotRepoUpdateSettings' if unsure
  -> repo
  -> m Reply
updateSnapshotRepo SnapshotRepoUpdateSettings {..} repo =
  bindM2 put url (return (Just body))
  where
    url = addQuery params <$> joinPath ["_snapshot", snapshotRepoName gSnapshotRepoName]
    params
      | repoUpdateVerify = []
      | otherwise        = [("verify", Just "false")]
    body = encode $ object [ "type" .= gSnapshotRepoType
                           , "settings" .= gSnapshotRepoSettings
                           ]
    GenericSnapshotRepo {..} = toGSnapshotRepo repo



-- | Verify if a snapshot repo is working. __NOTE:__ this API did not
-- make it into ElasticSearch until 1.4. If you use an older version,
-- you will get an error here.
verifySnapshotRepo
    :: ( MonadBH m
       , MonadThrow m
       )
    => SnapshotRepoName
    -> m (Either EsError SnapshotVerification)
verifySnapshotRepo (SnapshotRepoName n) =
  parseEsResponse =<< bindM2 post url (return Nothing)
  where
    url = joinPath ["_snapshot", n, "_verify"]


deleteSnapshotRepo :: MonadBH m => SnapshotRepoName -> m Reply
deleteSnapshotRepo (SnapshotRepoName n) = delete =<< url
  where
    url = joinPath ["_snapshot", n]


-- | Create and start a snapshot
createSnapshot
    :: (MonadBH m)
    => SnapshotRepoName
    -> SnapshotName
    -> SnapshotCreateSettings
    -> m Reply
createSnapshot (SnapshotRepoName repoName)
               (SnapshotName snapName)
               SnapshotCreateSettings {..} =
  bindM2 put url (return (Just body))
  where
    url = addQuery params <$> joinPath ["_snapshot", repoName, snapName]
    params = [("wait_for_completion", Just (boolQP snapWaitForCompletion))]
    body = encode $ object prs
    prs = catMaybes [ ("indices" .=) . indexSelectionName <$> snapIndices
                    , Just ("ignore_unavailable" .= snapIgnoreUnavailable)
                    , Just ("ignore_global_state" .= snapIncludeGlobalState)
                    , Just ("partial" .= snapPartial)
                    ]


indexSelectionName :: IndexSelection -> Text
indexSelectionName AllIndexes            = "_all"
indexSelectionName (IndexList (i :| is)) = T.intercalate "," (renderIndex <$> (i:is))
  where
    renderIndex (IndexName n) = n


-- | Get info about known snapshots given a pattern and repo name.
getSnapshots
    :: ( MonadBH m
       , MonadThrow m
       )
    => SnapshotRepoName
    -> SnapshotSelection
    -> m (Either EsError [SnapshotInfo])
getSnapshots (SnapshotRepoName repoName) sel =
  fmap (fmap unSIs) . parseEsResponse =<< get =<< url
  where
    url = joinPath ["_snapshot", repoName, snapPath]
    snapPath = case sel of
      AllSnapshots -> "_all"
      SnapshotList (s :| ss) -> T.intercalate "," (renderPath <$> (s:ss))
    renderPath (SnapPattern t)              = t
    renderPath (ExactSnap (SnapshotName t)) = t


newtype SIs = SIs { unSIs :: [SnapshotInfo] }


instance FromJSON SIs where
  parseJSON = withObject "Collection of SnapshotInfo" parse
    where
      parse o = SIs <$> o .: "snapshots"


-- | Delete a snapshot. Cancels if it is running.
deleteSnapshot :: MonadBH m => SnapshotRepoName -> SnapshotName -> m Reply
deleteSnapshot (SnapshotRepoName repoName) (SnapshotName snapName) =
  delete =<< url
  where
    url = joinPath ["_snapshot", repoName, snapName]


-- | Restore a snapshot to the cluster See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/modules-snapshots.html#_restore>
-- for more details.
restoreSnapshot
    :: MonadBH m
    => SnapshotRepoName
    -> SnapshotName
    -> SnapshotRestoreSettings
    -- ^ Start with 'defaultSnapshotRestoreSettings' and customize
    -- from there for reasonable defaults.
    -> m Reply
restoreSnapshot (SnapshotRepoName repoName)
                (SnapshotName snapName)
                SnapshotRestoreSettings {..} = bindM2 post url (return (Just body))
  where
    url = addQuery params <$> joinPath ["_snapshot", repoName, snapName, "_restore"]
    params = [("wait_for_completion", Just (boolQP snapRestoreWaitForCompletion))]
    body = encode (object prs)


    prs = catMaybes [ ("indices" .=) . indexSelectionName <$> snapRestoreIndices
                 , Just ("ignore_unavailable" .= snapRestoreIgnoreUnavailable)
                 , Just ("include_global_state" .= snapRestoreIncludeGlobalState)
                 , ("rename_pattern" .=) <$> snapRestoreRenamePattern
                 , ("rename_replacement" .=) . renderTokens <$> snapRestoreRenameReplacement
                 , Just ("include_aliases" .= snapRestoreIncludeAliases)
                 , ("index_settings" .= ) <$> snapRestoreIndexSettingsOverrides
                 , ("ignore_index_settings" .= ) <$> snapRestoreIgnoreIndexSettings
                 ]
    renderTokens (t :| ts) = mconcat (renderToken <$> (t:ts))
    renderToken (RRTLit t)      = t
    renderToken RRSubWholeMatch = "$0"
    renderToken (RRSubGroup g)  = T.pack (show (rrGroupRefNum g))


getNodesInfo
    :: ( MonadBH m
       , MonadThrow m
       )
    => NodeSelection
    -> m (Either EsError NodesInfo)
getNodesInfo sel = parseEsResponse =<< get =<< url
  where
    url = joinPath ["_nodes", selectionSeg]
    selectionSeg = case sel of
      LocalNode -> "_local"
      NodeList (l :| ls) -> T.intercalate "," (selToSeg <$> (l:ls))
      AllNodes -> "_all"
    selToSeg (NodeByName (NodeName n))            = n
    selToSeg (NodeByFullNodeId (FullNodeId i))    = i
    selToSeg (NodeByHost (Server s))              = s
    selToSeg (NodeByAttribute (NodeAttrName a) v) = a <> ":" <> v

getNodesStats
    :: ( MonadBH m
       , MonadThrow m
       )
    => NodeSelection
    -> m (Either EsError NodesStats)
getNodesStats sel = parseEsResponse =<< get =<< url
  where
    url = joinPath ["_nodes", selectionSeg, "stats"]
    selectionSeg = case sel of
      LocalNode -> "_local"
      NodeList (l :| ls) -> T.intercalate "," (selToSeg <$> (l:ls))
      AllNodes -> "_all"
    selToSeg (NodeByName (NodeName n))            = n
    selToSeg (NodeByFullNodeId (FullNodeId i))    = i
    selToSeg (NodeByHost (Server s))              = s
    selToSeg (NodeByAttribute (NodeAttrName a) v) = a <> ":" <> v

-- | 'createIndex' will create an index given a 'Server', 'IndexSettings', and an 'IndexName'.
--
-- >>> response <- runBH' $ createIndex defaultIndexSettings (IndexName "didimakeanindex")
-- >>> respIsTwoHunna response
-- True
-- >>> runBH' $ indexExists (IndexName "didimakeanindex")
-- True
createIndex :: MonadBH m => IndexSettings -> IndexName -> m Reply
createIndex indexSettings (IndexName indexName) =
  bindM2 put url (return body)
  where url = joinPath [indexName]
        body = Just $ encode indexSettings


-- | 'deleteIndex' will delete an index given a 'Server', and an 'IndexName'.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings (IndexName "didimakeanindex")
-- >>> response <- runBH' $ deleteIndex (IndexName "didimakeanindex")
-- >>> respIsTwoHunna response
-- True
-- >>> runBH' $ indexExists (IndexName "didimakeanindex")
-- False
deleteIndex :: MonadBH m => IndexName -> m Reply
deleteIndex (IndexName indexName) =
  delete =<< joinPath [indexName]

-- | 'updateIndexSettings' will apply a non-empty list of setting updates to an index
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings (IndexName "unconfiguredindex")
-- >>> response <- runBH' $ updateIndexSettings (BlocksWrite False :| []) (IndexName "unconfiguredindex")
-- >>> respIsTwoHunna response
-- True
updateIndexSettings :: MonadBH m => NonEmpty UpdatableIndexSetting -> IndexName -> m Reply
updateIndexSettings updates (IndexName indexName) =
  bindM2 put url (return body)
  where url = joinPath [indexName, "_settings"]
        body = Just (encode jsonBody)
        jsonBody = Object (deepMerge [u | Object u <- toJSON <$> toList updates])


getIndexSettings :: (MonadBH m, MonadThrow m) => IndexName
                 -> m (Either EsError IndexSettingsSummary)
getIndexSettings (IndexName indexName) = do
  parseEsResponse =<< get =<< url
  where url = joinPath [indexName, "_settings"]


-- | 'optimizeIndex' will optimize a single index, list of indexes or
-- all indexes. Note that this call will block until finishing but
-- will continue even if the request times out. Concurrent requests to
-- optimize an index while another is performing will block until the
-- previous one finishes. For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-optimize.html>. Nothing
-- worthwhile comes back in the reply body, so matching on the status
-- should suffice.
--
-- 'optimizeIndex' with a maxNumSegments of 1 and onlyExpungeDeletes
-- to True is the main way to release disk space back to the OS being
-- held by deleted documents.
--
-- Note that this API was deprecated in ElasticSearch 2.1 for the
-- almost completely identical forcemerge API. Adding support to that
-- API would be trivial but due to the significant breaking changes,
-- this library cannot currently be used with >= 2.0, so that feature was omitted.
--
-- >>> let ixn = IndexName "unoptimizedindex"
-- >>> _ <- runBH' $ deleteIndex ixn >> createIndex defaultIndexSettings ixn
-- >>> response <- runBH' $ optimizeIndex (IndexList (ixn :| [])) (defaultIndexOptimizationSettings { maxNumSegments = Just 1, onlyExpungeDeletes = True })
-- >>> respIsTwoHunna response
-- True
optimizeIndex :: MonadBH m => IndexSelection -> IndexOptimizationSettings -> m Reply
optimizeIndex ixs IndexOptimizationSettings {..} =
    bindM2 post url (return body)
  where url = addQuery params <$> joinPath [indexName, "_optimize"]
        params = catMaybes [ ("max_num_segments",) . Just . showText <$> maxNumSegments
                           , Just ("only_expunge_deletes", Just (boolQP onlyExpungeDeletes))
                           , Just ("flush", Just (boolQP flushAfterOptimize))
                           ]
        indexName = indexSelectionName ixs
        body = Nothing


deepMerge :: [Object] -> Object
deepMerge = LS.foldl' go mempty
  where go acc = LS.foldl' go' acc . HM.toList
        go' acc (k, v) = HM.insertWith merge k v acc
        merge (Object a) (Object b) = Object (deepMerge [a, b])
        merge _ b = b


statusCodeIs :: (Int, Int) -> Reply -> Bool
statusCodeIs r resp = inRange r $ NHTS.statusCode (responseStatus resp)

respIsTwoHunna :: Reply -> Bool
respIsTwoHunna = statusCodeIs (200, 299)

existentialQuery :: MonadBH m => Text -> m (Reply, Bool)
existentialQuery url = do
  reply <- head url
  return (reply, respIsTwoHunna reply)


-- | Tries to parse a response body as the expected type @a@ and
-- failing that tries to parse it as an EsError. All well-formed, JSON
-- responses from elasticsearch should fall into these two
-- categories. If they don't, a 'EsProtocolException' will be
-- thrown. If you encounter this, please report the full body it
-- reports along with your ElasticSearch verison.
parseEsResponse :: (MonadThrow m, FromJSON a) => Reply
                -> m (Either EsError a)
parseEsResponse reply
  | respIsTwoHunna reply = case eitherDecode body of
                             Right a -> return (Right a)
                             Left _ -> tryParseError
  | otherwise = tryParseError
  where body = responseBody reply
        tryParseError = case eitherDecode body of
                          Right e -> return (Left e)
                          -- this case should not be possible
                          Left _ -> explode
        explode = throwM (EsProtocolException body)

-- | 'indexExists' enables you to check if an index exists. Returns 'Bool'
--   in IO
--
-- >>> exists <- runBH' $ indexExists testIndex
indexExists :: MonadBH m => IndexName -> m Bool
indexExists (IndexName indexName) = do
  (_, exists) <- existentialQuery =<< joinPath [indexName]
  return exists

-- | 'refreshIndex' will force a refresh on an index. You must
-- do this if you want to read what you wrote.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings testIndex
-- >>> _ <- runBH' $ refreshIndex testIndex
refreshIndex :: MonadBH m => IndexName -> m Reply
refreshIndex (IndexName indexName) =
  bindM2 post url (return Nothing)
  where url = joinPath [indexName, "_refresh"]

stringifyOCIndex :: OpenCloseIndex -> Text
stringifyOCIndex oci = case oci of
  OpenIndex  -> "_open"
  CloseIndex -> "_close"

openOrCloseIndexes :: MonadBH m => OpenCloseIndex -> IndexName -> m Reply
openOrCloseIndexes oci (IndexName indexName) =
  bindM2 post url (return Nothing)
  where ociString = stringifyOCIndex oci
        url = joinPath [indexName, ociString]

-- | 'openIndex' opens an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> reply <- runBH' $ openIndex testIndex
openIndex :: MonadBH m => IndexName -> m Reply
openIndex = openOrCloseIndexes OpenIndex

-- | 'closeIndex' closes an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> reply <- runBH' $ closeIndex testIndex
closeIndex :: MonadBH m => IndexName -> m Reply
closeIndex = openOrCloseIndexes CloseIndex

-- | 'listIndices' returns a list of all index names on a given 'Server'
listIndices :: (MonadThrow m, MonadBH m) => m [IndexName]
listIndices =
  parse . responseBody =<< get =<< url
  where
    url = joinPath ["_cat/indices?v"]
    -- parses the tabular format the indices api provides
    parse body = case T.lines (T.decodeUtf8 (L.toStrict body)) of
      (hdr:rows) -> let ks = T.words hdr
                        keyedRows = [ HM.fromList (zip ks (T.words row)) | row <- rows ]
                        names = catMaybes (HM.lookup "index" <$> keyedRows)
                    in return (IndexName <$> names)
      [] -> throwM (EsProtocolException body)

-- | 'putTemplate' creates a template given an 'IndexTemplate' and a 'TemplateName'.
--   Explained in further detail at
--   <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-templates.html>
--
--   >>> let idxTpl = IndexTemplate (TemplatePattern "tweet-*") (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
--   >>> resp <- runBH' $ putTemplate idxTpl (TemplateName "tweet-tpl")
putTemplate :: MonadBH m => IndexTemplate -> TemplateName -> m Reply
putTemplate indexTemplate (TemplateName templateName) =
  bindM2 put url (return body)
  where url = joinPath ["_template", templateName]
        body = Just $ encode indexTemplate

-- | 'templateExists' checks to see if a template exists.
--
--   >>> exists <- runBH' $ templateExists (TemplateName "tweet-tpl")
templateExists :: MonadBH m => TemplateName -> m Bool
templateExists (TemplateName templateName) = do
  (_, exists) <- existentialQuery =<< joinPath ["_template", templateName]
  return exists

-- | 'deleteTemplate' is an HTTP DELETE and deletes a template.
--
--   >>> let idxTpl = IndexTemplate (TemplatePattern "tweet-*") (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
--   >>> _ <- runBH' $ putTemplate idxTpl (TemplateName "tweet-tpl")
--   >>> resp <- runBH' $ deleteTemplate (TemplateName "tweet-tpl")
deleteTemplate :: MonadBH m => TemplateName -> m Reply
deleteTemplate (TemplateName templateName) =
  delete =<< joinPath ["_template", templateName]

-- | 'putMapping' is an HTTP PUT and has upsert semantics. Mappings are schemas
-- for documents in indexes.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings testIndex
-- >>> resp <- runBH' $ putMapping testIndex testMapping TweetMapping
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Type","application/json; charset=UTF-8"),("Content-Length","21")], responseBody = "{\"acknowledged\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
putMapping :: (MonadBH m, ToJSON a) => IndexName
                 -> MappingName -> a -> m Reply
putMapping (IndexName indexName) (MappingName mappingName) mapping =
  bindM2 put url (return body)
  where url = joinPath [indexName, "_mapping", mappingName]
        -- "_mapping" and mappingName above were originally transposed
        -- erroneously. The correct API call is: "/INDEX/_mapping/MAPPING_NAME"
        body = Just $ encode mapping

versionCtlParams :: IndexDocumentSettings -> [(Text, Maybe Text)]
versionCtlParams cfg =
  case idsVersionControl cfg of
    NoVersionControl -> []
    InternalVersion v -> versionParams v "internal"
    ExternalGT (ExternalDocVersion v) -> versionParams v "external_gt"
    ExternalGTE (ExternalDocVersion v) -> versionParams v "external_gte"
    ForceVersion (ExternalDocVersion v) -> versionParams v "force"
  where
    vt = showText . docVersionNumber
    versionParams v t = [ ("version", Just $ vt v)
                        , ("version_type", Just t)
                        ]

-- | 'indexDocument' is the primary way to save a single document in
--   Elasticsearch. The document itself is simply something we can
--   convert into a JSON 'Value'. The 'DocId' will function as the
--   primary key for the document.
--
-- >>> resp <- runBH' $ indexDocument testIndex testMapping defaultIndexDocumentSettings exampleTweet (DocId "1")
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 201, statusMessage = "Created"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Type","application/json; charset=UTF-8"),("Content-Length","...")], responseBody = "{\"_index\":\"twitter\",\"_type\":\"tweet\",\"_id\":\"1\",\"_version\":1,...\"created\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
indexDocument :: (ToJSON doc, MonadBH m) => IndexName -> MappingName
                 -> IndexDocumentSettings -> doc -> DocId -> m Reply
indexDocument (IndexName indexName)
  (MappingName mappingName) cfg document (DocId docId) =
  bindM2 put url (return body)
  where url = addQuery params <$> joinPath [indexName, mappingName, docId]
        parentParams = case idsParent cfg of
          Nothing -> []
          Just (DocumentParent (DocId p)) -> [ ("parent", Just p) ]
        params = versionCtlParams cfg ++ parentParams
        body = Just (encode document)

-- | 'updateDocument' provides a way to perform an partial update of a
-- an already indexed document.
updateDocument :: (ToJSON patch, MonadBH m) => IndexName -> MappingName
                  -> IndexDocumentSettings -> patch -> DocId -> m Reply
updateDocument (IndexName indexName)
  (MappingName mappingName) cfg patch (DocId docId) =
  bindM2 post url (return body)
  where url = addQuery (versionCtlParams cfg) <$>
              joinPath [indexName, mappingName, docId, "_update"]
        body = Just (encode $ object ["doc" .= toJSON patch])

-- | 'deleteDocument' is the primary way to delete a single document.
--
-- >>> _ <- runBH' $ deleteDocument testIndex testMapping (DocId "1")
deleteDocument :: MonadBH m => IndexName -> MappingName
                  -> DocId -> m Reply
deleteDocument (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  delete =<< joinPath [indexName, mappingName, docId]

-- | 'bulk' uses
--    <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-bulk.html Elasticsearch's bulk API>
--    to perform bulk operations. The 'BulkOperation' data type encodes the
--    index\/update\/delete\/create operations. You pass a 'V.Vector' of 'BulkOperation's
--    and a 'Server' to 'bulk' in order to send those operations up to your Elasticsearch
--    server to be performed. I changed from [BulkOperation] to a Vector due to memory overhead.
--
-- >>> let stream = V.fromList [BulkIndex testIndex testMapping (DocId "2") (toJSON (BulkTest "blah"))]
-- >>> _ <- runBH' $ bulk stream
-- >>> _ <- runBH' $ refreshIndex testIndex
bulk :: MonadBH m => V.Vector BulkOperation -> m Reply
bulk bulkOps = bindM2 post url (return body)
  where url = joinPath ["_bulk"]
        body = Just $ encodeBulkOperations bulkOps

-- | 'encodeBulkOperations' is a convenience function for dumping a vector of 'BulkOperation'
--   into an 'L.ByteString'
--
-- >>> let bulkOps = V.fromList [BulkIndex testIndex testMapping (DocId "2") (toJSON (BulkTest "blah"))]
-- >>> encodeBulkOperations bulkOps
-- "\n{\"index\":{\"_type\":\"tweet\",\"_id\":\"2\",\"_index\":\"twitter\"}}\n{\"name\":\"blah\"}\n"
encodeBulkOperations :: V.Vector BulkOperation -> L.ByteString
encodeBulkOperations stream = collapsed where
  blobs = fmap encodeBulkOperation stream
  mashedTaters = mash (mempty :: Builder) blobs
  collapsed = toLazyByteString $ mappend mashedTaters (byteString "\n")

mash :: Builder -> V.Vector L.ByteString -> Builder
mash = V.foldl' (\b x -> b `mappend` (byteString "\n") `mappend` (lazyByteString x))

mkBulkStreamValue :: Text -> Text -> Text -> Text -> Value
mkBulkStreamValue operation indexName mappingName docId =
  object [operation .=
          object [ "_index" .= indexName
                 , "_type"  .= mappingName
                 , "_id"    .= docId]]

-- | 'encodeBulkOperation' is a convenience function for dumping a single 'BulkOperation'
--   into an 'L.ByteString'
--
-- >>> let bulkOp = BulkIndex testIndex testMapping (DocId "2") (toJSON (BulkTest "blah"))
-- >>> encodeBulkOperation bulkOp
-- "{\"index\":{\"_type\":\"tweet\",\"_id\":\"2\",\"_index\":\"twitter\"}}\n{\"name\":\"blah\"}"
encodeBulkOperation :: BulkOperation -> L.ByteString
encodeBulkOperation (BulkIndex (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "index" indexName mappingName docId
          blob = encode metadata `mappend` "\n" `mappend` encode value

encodeBulkOperation (BulkCreate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "create" indexName mappingName docId
          blob = encode metadata `mappend` "\n" `mappend` encode value

encodeBulkOperation (BulkDelete (IndexName indexName)
                (MappingName mappingName)
                (DocId docId)) = blob
    where metadata = mkBulkStreamValue "delete" indexName mappingName docId
          blob = encode metadata

encodeBulkOperation (BulkUpdate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "update" indexName mappingName docId
          doc = object ["doc" .= value]
          blob = encode metadata `mappend` "\n" `mappend` encode doc

-- | 'getDocument' is a straight-forward way to fetch a single document from
--   Elasticsearch using a 'Server', 'IndexName', 'MappingName', and a 'DocId'.
--   The 'DocId' is the primary key for your Elasticsearch document.
--
-- >>> yourDoc <- runBH' $ getDocument testIndex testMapping (DocId "1")
getDocument :: MonadBH m => IndexName -> MappingName
               -> DocId -> m Reply
getDocument (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  get =<< joinPath [indexName, mappingName, docId]

-- | 'documentExists' enables you to check if a document exists. Returns 'Bool'
--   in IO
--
-- >>> exists <- runBH' $ documentExists testIndex testMapping Nothing (DocId "1")
documentExists :: MonadBH m => IndexName -> MappingName
               -> Maybe DocumentParent -> DocId -> m Bool
documentExists (IndexName indexName) (MappingName mappingName)
               parent (DocId docId) = do
  (_, exists) <- existentialQuery =<< url
  return exists
  where url = addQuery params <$> joinPath [indexName, mappingName, docId]
        parentParam = fmap (\(DocumentParent (DocId p)) -> p) parent
        params = LS.filter (\(_, v) -> isJust v) [("parent", parentParam)]

scroll' :: (FromJSON a, MonadBH m, MonadThrow m) => Maybe ScrollId -> m ([Hit a], Maybe ScrollId)
scroll' Nothing = return ([], Nothing)
scroll' (Just sid) = do
    res <- advanceScroll sid 60
    case res of
      Right SearchResult {..} -> return (hits searchHits, scrollId)
      Left _ -> return ([], Nothing)

-- | Use the given scroll to fetch the next page of documents. If there are no
-- further pages, 'SearchResult.searchHits.hits' will be '[]'.
advanceScroll
  :: ( FromJSON a
     , MonadBH m
     , MonadThrow m
     )
  => ScrollId
  -> NominalDiffTime
  -- ^ How long should the snapshot of data be kept around? This timeout is updated every time 'advanceScroll' is used, so don't feel the need to set it to the entire duration of your search processing. Note that durations < 1s will be rounded up. Also note that 'NominalDiffTime' is an instance of Num so literals like 60 will be interpreted as seconds. 60s is a reasonable default.
  -> m (Either EsError (SearchResult a))
advanceScroll (ScrollId sid) scroll = do
  url <- joinPath ["_search/scroll?scroll=" <> scrollTime]
  parseEsResponse =<< post url (Just . L.fromStrict $ T.encodeUtf8 sid)
  where scrollTime = showText secs <> "s"
        secs :: Integer
        secs = round scroll

simpleAccumulator :: (FromJSON a, MonadBH m, MonadThrow m) => [Hit a] -> ([Hit a], Maybe ScrollId) -> m ([Hit a], Maybe ScrollId)
simpleAccumulator oldHits (newHits, Nothing) = return (oldHits ++ newHits, Nothing)
simpleAccumulator oldHits ([], _) = return (oldHits, Nothing)
simpleAccumulator oldHits (newHits, msid) = do
    (newHits', msid') <- scroll' msid
    simpleAccumulator (oldHits ++ newHits) (newHits', msid')

-- | Was there an optimistic concurrency control conflict when
-- indexing a document?
isVersionConflict :: Reply -> Bool
isVersionConflict = statusCheck (== 409)

isSuccess :: Reply -> Bool
isSuccess = statusCheck (inRange (200, 299))

isCreated :: Reply -> Bool
isCreated = statusCheck (== 201)

statusCheck :: (Int -> Bool) -> Reply -> Bool
statusCheck prd = prd . NHTS.statusCode . responseStatus

-- | This is a hook that can be set via the 'bhRequestHook' function
-- that will authenticate all requests using an HTTP Basic
-- Authentication header. Note that it is *strongly* recommended that
-- this option only be used over an SSL connection.
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = basicAuthHook (EsUsername "myuser") (EsPassword "mypass") }
basicAuthHook :: Monad m => EsUsername -> EsPassword -> Request -> m Request
basicAuthHook (EsUsername u) (EsPassword p) = return . applyBasicAuth u' p'
  where u' = T.encodeUtf8 u
        p' = T.encodeUtf8 p


boolQP :: Bool -> Text
boolQP True  = "true"
boolQP False = "false"
