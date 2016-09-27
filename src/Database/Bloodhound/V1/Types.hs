{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Bloodhound.V1.Types
  ( module Database.Bloodhound.Common.Types
  , IndexAliasCreate(..)
  , IndexAliasAction(..)
  , IndexAliasesSummary(..)
  , IndexAliasSummary(..)
  , LatLon(..)
  , GeoBoundingBox(..)
  , GeoBoundingBoxConstraint(..)
  , DefaultSort(..)
  , FilterAggregation(..)
  , mkAggregations
  , DateHistogramAggregation(..)
  , mkDateHistogram
  , TermsAggregation(..)
  , mkTermsScriptAggregation
  , mkTermsAggregation
  , GeoPoint(..)
  , FilteredQuery(..)
  , BoolQuery(..)
  , mkBoolQuery
  , BoostingQuery(..)
  , DisMaxQuery(..)
  , IndicesQuery(..)
  , HasParentQuery(..)
  , HasChildQuery(..)
  , NestedQuery(..)
  , Highlights(..)
  , PlainHighlight(..)
  , FastVectorHighlight(..)
  , CommonHighlight(..)
  , Search(..)
  , Query(..)
  , Aggregations(..)
  , Filter(..)
  , 
  ) where
  
import           Control.Applicative                as A
import           Control.Arrow                      (first)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Aeson.Types                   (Pair, Parser, emptyObject,
                                                     parseEither, parseMaybe)
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.Char
import           Data.Hashable                      (Hashable)
import qualified Data.HashMap.Strict                as HM
import           Data.List                          (foldl', intercalate, nub)
import           Data.List.NonEmpty                 (NonEmpty (..), toList)
import qualified Data.Map.Strict                    as M
import           Data.Maybe
import           Data.Scientific                    (Scientific)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time.Calendar
import           Data.Time.Clock                    (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX
import qualified Data.Traversable                   as DT
import           Data.Typeable                      (Typeable)
import qualified Data.Vector                        as V
import qualified Data.Version                       as Vers
import           GHC.Enum
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method          as NHTM
import qualified Text.ParserCombinators.ReadP       as RP
import qualified Text.Read                          as TR

import Database.Bloodhound.Common.Types

data IndexAliasCreate = IndexAliasCreate { aliasCreateRouting :: Maybe AliasRouting
                                         , aliasCreateFilter  :: Maybe Filter}
                                         deriving (Read, Show, Eq, Generic, Typeable)

data IndexAliasAction = AddAlias IndexAlias IndexAliasCreate
                      | RemoveAlias IndexAlias deriving (Read, Show, Eq, Generic, Typeable)

newtype IndexAliasesSummary = IndexAliasesSummary { indexAliasesSummary :: [IndexAliasSummary] } deriving (Read, Show, Eq, Generic, Typeable)

{-| 'IndexAliasSummary' is a summary of an index alias configured for a server. -}
data IndexAliasSummary = IndexAliasSummary { indexAliasSummaryAlias  :: IndexAlias
                                           , indexAliasSummaryCreate :: IndexAliasCreate} deriving (Read, Show, Eq, Generic, Typeable)

data Filter = AndFilter [Filter] Cache
            | OrFilter  [Filter] Cache
            | NotFilter  Filter  Cache
            | IdentityFilter
            | BoolFilter BoolMatch
            | ExistsFilter FieldName -- always cached
            | GeoBoundingBoxFilter GeoBoundingBoxConstraint
            | GeoDistanceFilter GeoPoint Distance DistanceType OptimizeBbox Cache
            | GeoDistanceRangeFilter GeoPoint DistanceRange
            | GeoPolygonFilter FieldName [LatLon]
            | IdsFilter MappingName [DocId]
            | LimitFilter Int
            | MissingFilter FieldName Existence NullValue
            | PrefixFilter  FieldName PrefixValue Cache
            | QueryFilter   Query Cache
            | RangeFilter   FieldName RangeValue RangeExecution Cache
            | RegexpFilter  FieldName Regexp RegexpFlags CacheName Cache CacheKey
            | TermFilter    Term Cache
              deriving (Eq, Read, Show, Generic, Typeable)

data GeoFilterType = GeoFilterMemory
                   | GeoFilterIndexed deriving (Eq, Read, Show, Generic, Typeable)

data LatLon = LatLon { lat :: Double
                     , lon :: Double } deriving (Eq, Read, Show, Generic, Typeable)

data GeoBoundingBox =
  GeoBoundingBox { topLeft     :: LatLon
                 , bottomRight :: LatLon } deriving (Eq, Read, Show, Generic, Typeable)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField        :: FieldName
                           , constraintBox     :: GeoBoundingBox
                           -- , bbConstraintcache :: Cache
                           , geoType           :: GeoFilterType
                           } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'DefaultSort' is usually the kind of 'SortSpec' you'll want. There's a
    'mkSort' convenience function for when you want to specify only the most
    common parameters.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Bool
              , sortMode       :: Maybe SortMode
              , missingSort    :: Maybe Missing
              , nestedFilter   :: Maybe Filter } deriving (Eq, Read, Show, Generic, Typeable)

{-| The two main kinds of 'SortSpec' are 'DefaultSortSpec' and
    'GeoDistanceSortSpec'. The latter takes a 'SortOrder', 'GeoPoint', and
    'DistanceUnit' to express "nearness" to a single geographical point as a
    sort specification.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data SortSpec = DefaultSortSpec DefaultSort
              | GeoDistanceSortSpec SortOrder GeoPoint DistanceUnit deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON GeoBoundingBoxConstraint where
  toJSON (GeoBoundingBoxConstraint
          -- (FieldName gbbcGeoBBField) gbbcConstraintBox cache type') =
          (FieldName gbbcGeoBBField) gbbcConstraintBox type') =
    object [gbbcGeoBBField .= gbbcConstraintBox
           -- , "_cache"  .= cache
           , "type" .= type']

instance FromJSON GeoBoundingBoxConstraint where
  parseJSON = withObject "GeoBoundingBoxConstraint" parse
    where parse o = case HM.toList (deleteSeveral ["type", "_cache"] o) of
                      [(fn, v)] -> GeoBoundingBoxConstraint (FieldName fn)
                                   <$> parseJSON v
                                   -- <*> o .:? "_cache" .!= defaultCache
                                   <*> o .: "type"
                      _ -> fail "Could not find field name for GeoBoundingBoxConstraint"

instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory  = String "memory"
  toJSON GeoFilterIndexed = String "indexed"

instance FromJSON GeoFilterType where
  parseJSON = withText "GeoFilterType" parse
    where parse "memory"  = pure GeoFilterMemory
          parse "indexed" = pure GeoFilterIndexed
          parse t         = fail ("Unrecognized GeoFilterType: " <> show t)

instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox gbbTopLeft gbbBottomRight) =
    object ["top_left"      .= gbbTopLeft
           , "bottom_right" .= gbbBottomRight]

instance FromJSON GeoBoundingBox where
  parseJSON = withObject "GeoBoundingBox" parse
    where parse o = GeoBoundingBox
                    <$> o .: "top_left"
                    <*> o .: "bottom_right"

instance ToJSON LatLon where
  toJSON (LatLon lLat lLon) =
    object ["lat"  .= lLat
           , "lon" .= lLon]

instance FromJSON LatLon where
  parseJSON = withObject "LatLon" parse
    where parse o = LatLon <$> o .: "lat"
                           <*> o .: "lon"
instance ToJSON SortSpec where
    toJSON (DefaultSortSpec
            (DefaultSort (FieldName dsSortFieldName) dsSortOrder dsIgnoreUnmapped
             dsSortMode dsMissingSort dsNestedFilter)) =
      object [dsSortFieldName .= omitNulls base] where
        base = [ "order" .= dsSortOrder
               , "ignore_unmapped" .= dsIgnoreUnmapped
               , "mode" .= dsSortMode
               , "missing" .= dsMissingSort
               , "nested_filter" .= dsNestedFilter ]

    toJSON (GeoDistanceSortSpec gdsSortOrder (GeoPoint (FieldName field) gdsLatLon) units) =
      object [ "unit" .= units
             , field .= gdsLatLon
             , "order" .= gdsSortOrder ]

instance FromJSON IndexAliasCreate where
  parseJSON v = withObject "IndexAliasCreate" parse v
    where parse o = IndexAliasCreate <$> optional (parseJSON v)
                                     <*> o .:? "filter"

instance ToJSON IndexAliasCreate where
  toJSON IndexAliasCreate {..} = Object (filterObj <> routingObj)
    where filterObj = maybe mempty (HM.singleton "filter" . toJSON) aliasCreateFilter
          Object routingObj = maybe (Object mempty) toJSON aliasCreateRouting

instance FromJSON IndexAliasesSummary where
  parseJSON = withObject "IndexAliasesSummary" parse
    where parse o = IndexAliasesSummary . mconcat <$> mapM (uncurry go) (HM.toList o)
          go ixn = withObject "index aliases" $ \ia -> do
                     aliases <- ia .:? "aliases" .!= mempty
                     forM (HM.toList aliases) $ \(aName, v) -> do
                       let indexAlias = IndexAlias (IndexName ixn) (IndexAliasName (IndexName aName))
                       IndexAliasSummary indexAlias <$> parseJSON v


instance ToJSON IndexAliasAction where
  toJSON (AddAlias ia opts) = object ["add" .= (iaObj <> optsObj)]
    where Object iaObj = toJSON ia
          Object optsObj = toJSON opts
  toJSON (RemoveAlias ia) = object ["remove" .= iaObj]
    where Object iaObj = toJSON ia

instance ToJSON IndexAlias where
  toJSON IndexAlias {..} = object ["index" .= srcIndex
                                  , "alias" .= indexAlias
                                  ]

instance Monoid Filter where
  mempty = IdentityFilter
  mappend a b = AndFilter [a, b] defaultCache

instance Seminearring Filter where
  a <||> b = OrFilter [a, b] defaultCache

instance ToJSON Filter where
  toJSON (AndFilter filters cache) =
    object ["and" .=
            object [ "filters" .= fmap toJSON filters
                   , "_cache"  .= cache]]

  toJSON (OrFilter filters cache) =
    object ["or" .=
            object [ "filters" .= fmap toJSON filters
                   , "_cache"  .= cache]]

  toJSON (NotFilter notFilter cache) =
    object ["not" .=
            object ["filter"  .= notFilter
                   , "_cache" .= cache]]

  toJSON (IdentityFilter) =
    object ["match_all" .= object []]

  toJSON (TermFilter (Term termFilterField termFilterValue) cache) =
    object ["term" .= object base]
    where base = [termFilterField .= termFilterValue,
                  "_cache"        .= cache]

  toJSON (ExistsFilter (FieldName fieldName)) =
    object ["exists"  .= object
            ["field"  .= fieldName]]

  toJSON (BoolFilter boolMatch) =
    object ["bool"    .= boolMatch]

  toJSON (GeoBoundingBoxFilter bbConstraint) =
    object ["geo_bounding_box" .= bbConstraint]

  toJSON (GeoDistanceFilter (GeoPoint (FieldName distanceGeoField) geoDistLatLon)
          distance distanceType optimizeBbox cache) =
    object ["geo_distance" .=
            object ["distance" .= distance
                   , "distance_type" .= distanceType
                   , "optimize_bbox" .= optimizeBbox
                   , distanceGeoField .= geoDistLatLon
                   , "_cache" .= cache]]

  toJSON (GeoDistanceRangeFilter (GeoPoint (FieldName gddrField) drLatLon)
          (DistanceRange geoDistRangeDistFrom drDistanceTo)) =
    object ["geo_distance_range" .=
            object ["from" .= geoDistRangeDistFrom
                   , "to"  .= drDistanceTo
                   , gddrField .= drLatLon]]

  toJSON (GeoPolygonFilter (FieldName geoPolygonFilterField) latLons) =
    object ["geo_polygon" .=
            object [geoPolygonFilterField .=
                    object ["points" .= fmap toJSON latLons]]]

  toJSON (IdsFilter (MappingName mappingName) values) =
    object ["ids" .=
            object ["type" .= mappingName
                   , "values" .= fmap unpackId values]]

  toJSON (LimitFilter limit) =
    object ["limit" .= object ["value" .= limit]]

  toJSON (MissingFilter (FieldName fieldName) (Existence existence) (NullValue nullValue)) =
    object ["missing" .=
            object ["field"       .= fieldName
                   , "existence"  .= existence
                   , "null_value" .= nullValue]]

  toJSON (PrefixFilter (FieldName fieldName) fieldValue cache) =
    object ["prefix" .=
            object [fieldName .= fieldValue
                   , "_cache" .= cache]]

  toJSON (QueryFilter query False) =
    object ["query" .= toJSON query ]
  toJSON (QueryFilter query True) =
    object ["fquery" .=
            object [ "query"  .= toJSON query
                   , "_cache" .= True ]]

  toJSON (RangeFilter (FieldName fieldName) rangeValue rangeExecution cache) =
    object ["range" .=
            object [ fieldName .= object (rangeValueToPair rangeValue)
                   , "execution" .= rangeExecution
                   , "_cache" .= cache]]

  toJSON (RegexpFilter (FieldName fieldName)
          (Regexp regexText) flags (CacheName cacheName) cache (CacheKey cacheKey)) =
    object ["regexp" .=
            object [fieldName .=
                    object ["value"  .= regexText
                           , "flags" .= flags]
                   , "_name"      .= cacheName
                   , "_cache"     .= cache
                   , "_cache_key" .= cacheKey]]

instance FromJSON Filter where
  parseJSON = withObject "Filter" parse
    where parse o = andFilter `taggedWith` "and"
                <|> orFilter `taggedWith` "or"
                <|> notFilter `taggedWith` "not"
                <|> identityFilter `taggedWith` "match_all"
                <|> boolFilter `taggedWith` "bool"
                <|> existsFilter `taggedWith` "exists"
                <|> geoBoundingBoxFilter `taggedWith` "geo_bounding_box"
                <|> geoDistanceFilter `taggedWith` "geo_distance"
                <|> geoDistanceRangeFilter `taggedWith` "geo_distance_range"
                <|> geoPolygonFilter `taggedWith` "geo_polygon"
                <|> idsFilter `taggedWith` "ids"
                <|> limitFilter `taggedWith` "limit"
                <|> missingFilter `taggedWith` "missing"
                <|> prefixFilter `taggedWith` "prefix"
                <|> queryFilter `taggedWith` "query"
                <|> fqueryFilter `taggedWith` "fquery"
                <|> rangeFilter `taggedWith` "range"
                <|> regexpFilter `taggedWith` "regexp"
                <|> termFilter `taggedWith` "term"
            where taggedWith parser k = parser =<< o .: k
          andFilter o = AndFilter <$> o .: "filters"
                                  <*> o .:? "_cache" .!= defaultCache
          orFilter o = OrFilter <$> o .: "filters"
                                <*> o .:? "_cache" .!= defaultCache
          notFilter o = NotFilter <$> o .: "filter"
                                  <*> o .: "_cache" .!= defaultCache
          identityFilter :: Object -> Parser Filter
          identityFilter m
            | HM.null m = pure IdentityFilter
            | otherwise = fail ("Identityfilter expected empty object but got " <> show m)
          boolFilter = pure . BoolFilter
          existsFilter o = ExistsFilter <$> o .: "field"
          geoBoundingBoxFilter = pure . GeoBoundingBoxFilter
          geoDistanceFilter o = do
            case HM.toList (deleteSeveral ["distance", "distance_type", "optimize_bbox", "_cache"] o) of
              [(fn, v)] -> do
                gp <- GeoPoint (FieldName fn) <$> parseJSON v
                GeoDistanceFilter gp <$> o .: "distance"
                                     <*> o .: "distance_type"
                                     <*> o .: "optimize_bbox"
                                     <*> o .:? "_cache" .!= defaultCache
              _ -> fail "Could not find GeoDistanceFilter field name"
          geoDistanceRangeFilter o = do
            case HM.toList (deleteSeveral ["from", "to"] o) of
              [(fn, v)] -> do
                gp <- GeoPoint (FieldName fn) <$> parseJSON v
                rng <- DistanceRange <$> o .: "from" <*> o .: "to"
                return (GeoDistanceRangeFilter gp rng)
              _ -> fail "Could not find GeoDistanceRangeFilter field name"
          geoPolygonFilter = fieldTagged $ \fn o -> GeoPolygonFilter fn <$> o .: "points"
          idsFilter o = IdsFilter <$> o .: "type"
                                  <*> o .: "values"
          limitFilter o = LimitFilter <$> o .: "value"
          missingFilter o = MissingFilter <$> o .: "field"
                                          <*> o .: "existence"
                                          <*> o .: "null_value"
          prefixFilter o = case HM.toList (HM.delete "_cache" o) of
                             [(fn, String v)] -> PrefixFilter (FieldName fn) v <$> o .:? "_cache" .!= defaultCache
                             _ -> fail "Could not parse PrefixFilter"

          queryFilter q = pure (QueryFilter q False)
          fqueryFilter o = QueryFilter <$> o .: "query" <*> pure True
          rangeFilter o = case HM.toList (deleteSeveral ["execution", "_cache"] o) of
                            [(fn, v)] -> RangeFilter (FieldName fn)
                                         <$> parseJSON v
                                         <*> o .: "execution"
                                         <*> o .:? "_cache" .!= defaultCache
                            _ -> fail "Could not find field name for RangeFilter"
          regexpFilter o = case HM.toList (deleteSeveral ["_name", "_cache", "_cache_key"] o) of
                              [(fn, Object o')] -> RegexpFilter (FieldName fn)
                                                   <$> o' .: "value"
                                                   <*> o' .: "flags"
                                                   <*> o .: "_name"
                                                   <*> o .:? "_cache" .!= defaultCache
                                                   <*> o .: "_cache_key"
                              _ -> fail "Could not find field name for RegexpFilter"
          termFilter o = case HM.toList (HM.delete "_cache" o) of
                         [(termField, String termVal)] -> TermFilter (Term termField termVal)
                                                          <$> o .:? "_cache" .!= defaultCache
                         _ -> fail "Could not find term field for TermFilter"

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation { faFilter :: Filter
                                           , faAggs   :: Maybe Aggregations} deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON Aggregation where
  toJSON (TermsAgg (TermsAggregation term include exclude order minDocCount size shardSize collectMode executionHint termAggs)) =
    omitNulls ["terms" .= omitNulls [ toJSON' term,
                                      "include"        .= include,
                                      "exclude"        .= exclude,
                                      "order"          .= order,
                                      "min_doc_count"  .= minDocCount,
                                      "size"           .= size,
                                      "shard_size"     .= shardSize,
                                      "collect_mode"   .= collectMode,
                                      "execution_hint" .= executionHint
                                    ],
               "aggs"  .= termAggs ]
    where
      toJSON' x = case x of { Left y -> "field" .= y;  Right y -> "script" .= y }

  toJSON (CardinalityAgg (CardinalityAggregation field precisionThreshold)) =
    object ["cardinality" .= omitNulls [ "field"              .= field,
                                         "precisionThreshold" .= precisionThreshold
                                       ]
           ]

  toJSON (DateHistogramAgg (DateHistogramAggregation field interval format preZone postZone preOffset postOffset dateHistoAggs)) =
    omitNulls ["date_histogram" .= omitNulls [ "field"       .= field,
                                               "interval"    .= interval,
                                               "format"      .= format,
                                               "pre_zone"    .= preZone,
                                               "post_zone"   .= postZone,
                                               "pre_offset"  .= preOffset,
                                               "post_offset" .= postOffset
                                             ],
               "aggs"           .= dateHistoAggs ]
  toJSON (ValueCountAgg a) = object ["value_count" .= v]
    where v = case a of
                (FieldValueCount (FieldName n)) -> object ["field" .= n]
                (ScriptValueCount (Script s))   -> object ["script" .= s]
  toJSON (FilterAgg (FilterAggregation filt ags)) =
    omitNulls [ "filter" .= filt
              , "aggs" .= ags]
  toJSON (DateRangeAgg a) = object [ "date_range" .= a
                                   ]
  toJSON (MissingAgg (MissingAggregation{..})) =
    object ["missing" .= object ["field" .= maField]]

data Aggregation = TermsAgg TermsAggregation
                 | CardinalityAgg CardinalityAggregation
                 | DateHistogramAgg DateHistogramAggregation
                 | ValueCountAgg ValueCountAggregation
                 | FilterAgg FilterAggregation
                 | DateRangeAgg DateRangeAggregation
                 | MissingAgg MissingAggregation deriving (Eq, Read, Show, Generic, Typeable)

type Aggregations = M.Map Text Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Text -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data DateHistogramAggregation = DateHistogramAggregation { dateField      :: FieldName
                                                         , dateInterval   :: Interval
                                                         , dateFormat     :: Maybe Text
                                                         -- pre and post deprecated in 1.5
                                                         , datePreZone    :: Maybe Text
                                                         , datePostZone   :: Maybe Text
                                                         , datePreOffset  :: Maybe Text
                                                         , datePostOffset :: Maybe Text
                                                         , dateAggs       :: Maybe Aggregations
                                                         } deriving (Eq, Read, Show, Generic, Typeable)

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

data TermsAggregation = TermsAggregation { term              :: Either Text Text
                                         , termInclude       :: Maybe TermInclusion
                                         , termExclude       :: Maybe TermInclusion
                                         , termOrder         :: Maybe TermOrder
                                         , termMinDocCount   :: Maybe Int
                                         , termSize          :: Maybe Int
                                         , termShardSize     :: Maybe Int
                                         , termCollectMode   :: Maybe CollectionMode
                                         , termExecutionHint :: Maybe ExecutionHint
                                         , termAggs          :: Maybe Aggregations
                                    } deriving (Eq, Read, Show, Generic, Typeable)

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t = TermsAggregation (Left t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data OptimizeBbox = OptimizeGeoFilterType GeoFilterType
                  | NoOptimizeBbox deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft

instance FromJSON OptimizeBbox where
  parseJSON v = withText "NoOptimizeBbox" parseNoOptimize v
            <|> parseOptimize v
    where parseNoOptimize "none" = pure NoOptimizeBbox
          parseNoOptimize _ = mzero
          parseOptimize = fmap OptimizeGeoFilterType . parseJSON

data GeoPoint =
  GeoPoint { geoField :: FieldName
           , latLon   :: LatLon} deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON GeoPoint where
  toJSON (GeoPoint (FieldName geoPointField) geoPointLatLon) =
    object [ geoPointField  .= geoPointLatLon ]

data FilteredQuery =
  FilteredQuery
  { filteredQuery  :: Query
  , filteredFilter :: Filter } deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON FilteredQuery where
  toJSON (FilteredQuery query fFilter) =
    object [ "query"  .= query
           , "filter" .= fFilter ]

instance FromJSON FilteredQuery where
  parseJSON = withObject "FilteredQuery" parse
    where parse o = FilteredQuery
                    <$> o .: "query"
                    <*> o .: "filter"

data Query =
  TermQuery                     Term (Maybe Boost)
  | TermsQuery                  Text (NonEmpty Text)
  | QueryMatchQuery             MatchQuery
  | QueryMultiMatchQuery        MultiMatchQuery
  | QueryBoolQuery              BoolQuery
  | QueryBoostingQuery          BoostingQuery
  | QueryCommonTermsQuery       CommonTermsQuery
  | ConstantScoreFilter         Filter Boost
  | ConstantScoreQuery          Query Boost
  | QueryDisMaxQuery            DisMaxQuery
  | QueryFilteredQuery          FilteredQuery
  | QueryFuzzyLikeThisQuery     FuzzyLikeThisQuery
  | QueryFuzzyLikeFieldQuery    FuzzyLikeFieldQuery
  | QueryFuzzyQuery             FuzzyQuery
  | QueryHasChildQuery          HasChildQuery
  | QueryHasParentQuery         HasParentQuery
  | IdsQuery                    MappingName [DocId]
  | QueryIndicesQuery           IndicesQuery
  | MatchAllQuery               (Maybe Boost)
  | QueryMoreLikeThisQuery      MoreLikeThisQuery
  | QueryMoreLikeThisFieldQuery MoreLikeThisFieldQuery
  | QueryNestedQuery            NestedQuery
  | QueryPrefixQuery            PrefixQuery
  | QueryQueryStringQuery       QueryStringQuery
  | QuerySimpleQueryStringQuery SimpleQueryStringQuery
  | QueryRangeQuery             RangeQuery
  | QueryRegexpQuery            RegexpQuery
  deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON Query where
  toJSON (TermQuery (Term termQueryField termQueryValue) boost) =
    object [ "term" .=
             object [termQueryField .= object merged]]
    where
      base = [ "value" .= termQueryValue ]
      boosted = maybe [] (return . ("boost" .=)) boost
      merged = mappend base boosted

  toJSON (TermsQuery fieldName terms) =
    object [ "terms" .= object conjoined ]
    where conjoined = [fieldName .= terms]

  toJSON (IdsQuery idsQueryMappingName docIds) =
    object [ "ids" .= object conjoined ]
    where conjoined = [ "type"   .= idsQueryMappingName
                      , "values" .= fmap toJSON docIds ]

  toJSON (QueryQueryStringQuery qQueryStringQuery) =
    object [ "query_string" .= qQueryStringQuery ]

  toJSON (QueryMatchQuery matchQuery) =
    object [ "match" .= matchQuery ]

  toJSON (QueryMultiMatchQuery multiMatchQuery) =
      toJSON multiMatchQuery

  toJSON (QueryBoolQuery boolQuery) =
    object [ "bool" .= boolQuery ]

  toJSON (QueryBoostingQuery boostingQuery) =
    object [ "boosting" .= boostingQuery ]

  toJSON (QueryCommonTermsQuery commonTermsQuery) =
    object [ "common" .= commonTermsQuery ]

  toJSON (ConstantScoreFilter csFilter boost) =
    object ["constant_score" .= object ["filter" .= csFilter
                                       , "boost" .= boost]]

  toJSON (ConstantScoreQuery query boost) =
    object ["constant_score" .= object ["query" .= query
                                       , "boost" .= boost]]

  toJSON (QueryDisMaxQuery disMaxQuery) =
    object [ "dis_max" .= disMaxQuery ]

  toJSON (QueryFilteredQuery qFilteredQuery) =
    object [ "filtered" .= qFilteredQuery ]

  toJSON (QueryFuzzyLikeThisQuery fuzzyQuery) =
    object [ "fuzzy_like_this" .= fuzzyQuery ]

  toJSON (QueryFuzzyLikeFieldQuery fuzzyFieldQuery) =
    object [ "fuzzy_like_this_field" .= fuzzyFieldQuery ]

  toJSON (QueryFuzzyQuery fuzzyQuery) =
    object [ "fuzzy" .= fuzzyQuery ]

  toJSON (QueryHasChildQuery childQuery) =
    object [ "has_child" .= childQuery ]

  toJSON (QueryHasParentQuery parentQuery) =
    object [ "has_parent" .= parentQuery ]

  toJSON (QueryIndicesQuery qIndicesQuery) =
    object [ "indices" .= qIndicesQuery ]

  toJSON (MatchAllQuery boost) =
    object [ "match_all" .= omitNulls [ "boost" .= boost ] ]

  toJSON (QueryMoreLikeThisQuery query) =
    object [ "more_like_this" .= query ]

  toJSON (QueryMoreLikeThisFieldQuery query) =
    object [ "more_like_this_field" .= query ]

  toJSON (QueryNestedQuery query) =
    object [ "nested" .= query ]

  toJSON (QueryPrefixQuery query) =
    object [ "prefix" .= query ]

  toJSON (QueryRangeQuery query) =
    object [ "range"  .= query ]

  toJSON (QueryRegexpQuery query) =
    object [ "regexp" .= query ]

  toJSON (QuerySimpleQueryStringQuery query) =
    object [ "simple_query_string" .= query ]

instance FromJSON Query where
  parseJSON v = withObject "Query" parse v
    where parse o = termQuery `taggedWith` "term"
                <|> termsQuery `taggedWith` "terms"
                <|> idsQuery `taggedWith` "ids"
                <|> queryQueryStringQuery `taggedWith` "query_string"
                <|> queryMatchQuery `taggedWith` "match"
                <|> queryMultiMatchQuery
                <|> queryBoolQuery `taggedWith` "bool"
                <|> queryBoostingQuery `taggedWith` "boosting"
                <|> queryCommonTermsQuery `taggedWith` "common"
                <|> constantScoreFilter `taggedWith` "constant_score"
                <|> constantScoreQuery `taggedWith` "constant_score"
                <|> queryDisMaxQuery `taggedWith` "dis_max"
                <|> queryFilteredQuery `taggedWith` "filtered"
                <|> queryFuzzyLikeThisQuery `taggedWith` "fuzzy_like_this"
                <|> queryFuzzyLikeFieldQuery `taggedWith` "fuzzy_like_this_field"
                <|> queryFuzzyQuery `taggedWith` "fuzzy"
                <|> queryHasChildQuery `taggedWith` "has_child"
                <|> queryHasParentQuery `taggedWith` "has_parent"
                <|> queryIndicesQuery `taggedWith` "indices"
                <|> matchAllQuery `taggedWith` "match_all"
                <|> queryMoreLikeThisQuery `taggedWith` "more_like_this"
                <|> queryMoreLikeThisFieldQuery `taggedWith` "more_like_this_field"
                <|> queryNestedQuery `taggedWith` "nested"
                <|> queryPrefixQuery `taggedWith` "prefix"
                <|> queryRangeQuery `taggedWith` "range"
                <|> queryRegexpQuery `taggedWith` "regexp"
                <|> querySimpleQueryStringQuery `taggedWith` "simple_query_string"
            where taggedWith parser k = parser =<< o .: k
          termQuery = fieldTagged $ \(FieldName fn) o ->
                        TermQuery <$> (Term fn <$> o .: "value") <*> o .:? "boost"
          termsQuery o = case HM.toList o of
                           [(fn, vs)] -> do vals <- parseJSON vs
                                            case vals of
                                              x:xs -> return (TermsQuery fn (x :| xs))
                                              _ -> fail "Expected non empty list of values"
                           _ -> fail "Expected object with 1 field-named key"
          idsQuery o = IdsQuery <$> o .: "type"
                                <*> o .: "values"
          queryQueryStringQuery = pure . QueryQueryStringQuery
          queryMatchQuery = pure . QueryMatchQuery
          queryMultiMatchQuery = QueryMultiMatchQuery <$> parseJSON v
          queryBoolQuery = pure . QueryBoolQuery
          queryBoostingQuery = pure . QueryBoostingQuery
          queryCommonTermsQuery = pure . QueryCommonTermsQuery
          constantScoreFilter o = case HM.lookup "filter" o of
            Just x -> ConstantScoreFilter <$> parseJSON x
                                          <*> o .: "boost"
            _ -> fail "Does not appear to be a ConstantScoreFilter"
          constantScoreQuery o = case HM.lookup "query" o of
            Just x -> ConstantScoreQuery <$> parseJSON x
                                         <*> o .: "boost"
            _ -> fail "Does not appear to be a ConstantScoreQuery"
          queryDisMaxQuery = pure . QueryDisMaxQuery
          queryFilteredQuery = pure . QueryFilteredQuery
          queryFuzzyLikeThisQuery = pure . QueryFuzzyLikeThisQuery
          queryFuzzyLikeFieldQuery = pure . QueryFuzzyLikeFieldQuery
          queryFuzzyQuery = pure . QueryFuzzyQuery
          queryHasChildQuery = pure . QueryHasChildQuery
          queryHasParentQuery = pure . QueryHasParentQuery
          queryIndicesQuery = pure . QueryIndicesQuery
          matchAllQuery o = MatchAllQuery <$> o .:? "boost"
          queryMoreLikeThisQuery = pure . QueryMoreLikeThisQuery
          queryMoreLikeThisFieldQuery = pure . QueryMoreLikeThisFieldQuery
          queryNestedQuery = pure . QueryNestedQuery
          queryPrefixQuery = pure . QueryPrefixQuery
          queryRangeQuery = pure . QueryRangeQuery
          queryRegexpQuery = pure . QueryRegexpQuery
          querySimpleQueryStringQuery = pure . QuerySimpleQueryStringQuery

data BoolQuery =
  BoolQuery { boolQueryMustMatch          :: [Query]
            , boolQueryMustNotMatch       :: [Query]
            , boolQueryShouldMatch        :: [Query]
            , boolQueryMinimumShouldMatch :: Maybe MinimumMatch
            , boolQueryBoost              :: Maybe Boost
            , boolQueryDisableCoord       :: Maybe DisableCoord
            } deriving (Eq, Read, Show, Generic, Typeable)

mkBoolQuery :: [Query] -> [Query] -> [Query] -> BoolQuery
mkBoolQuery must mustNot should =
  BoolQuery must mustNot should Nothing Nothing Nothing

data BoostingQuery =
  BoostingQuery { positiveQuery :: Query
                , negativeQuery :: Query
                , negativeBoost :: Boost } deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON BoostingQuery where
  toJSON (BoostingQuery bqPositiveQuery bqNegativeQuery bqNegativeBoost) =
    object [ "positive"       .= bqPositiveQuery
           , "negative"       .= bqNegativeQuery
           , "negative_boost" .= bqNegativeBoost ]

instance FromJSON BoostingQuery where
  parseJSON = withObject "BoostingQuery" parse
    where parse o = BoostingQuery
                    <$> o .: "positive"
                    <*> o .: "negative"
                    <*> o .: "negative_boost"

instance ToJSON BoolQuery where
  toJSON (BoolQuery mustM notM shouldM bqMin boost disableCoord) =
    omitNulls base
    where base = [ "must" .= mustM
                 , "must_not" .= notM
                 , "should" .= shouldM
                 , "minimum_should_match" .= bqMin
                 , "boost" .= boost
                 , "disable_coord" .= disableCoord ]

instance FromJSON BoolQuery where
  parseJSON = withObject "BoolQuery" parse
    where parse o = BoolQuery
                    <$> o .:? "must" .!= []
                    <*> o .:? "must_not" .!= []
                    <*> o .:? "should" .!= []
                    <*> o .:? "minimum_should_match"
                    <*> o .:? "boost"
                    <*> o .:? "disable_coord"

data DisMaxQuery =
  DisMaxQuery { disMaxQueries    :: [Query]
                -- default 0.0
              , disMaxTiebreaker :: Tiebreaker
              , disMaxBoost      :: Maybe Boost
              } deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON DisMaxQuery where
  toJSON (DisMaxQuery queries tiebreaker boost) =
    omitNulls base
    where base = [ "queries"     .= queries
                 , "boost"       .= boost
                 , "tie_breaker" .= tiebreaker ]

instance FromJSON DisMaxQuery where
  parseJSON = withObject "DisMaxQuery" parse
    where parse o = DisMaxQuery
                    <$> o .:? "queries" .!= []
                    <*> o .: "tie_breaker"
                    <*> o .:? "boost"

data IndicesQuery =
  IndicesQuery
  { indicesQueryIndices :: [IndexName]
  , indicesQuery        :: Query
    -- default "all"
  , indicesQueryNoMatch :: Maybe Query } deriving (Eq, Read, Show, Generic, Typeable)

data HasParentQuery =
  HasParentQuery
  { hasParentQueryType      :: TypeName
  , hasParentQuery          :: Query
  , hasParentQueryScoreType :: Maybe ScoreType } deriving (Eq, Read, Show, Generic, Typeable)

data HasChildQuery =
  HasChildQuery
  { hasChildQueryType      :: TypeName
  , hasChildQuery          :: Query
  , hasChildQueryScoreType :: Maybe ScoreType } deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON IndicesQuery where
  toJSON (IndicesQuery indices query noMatch) =
    omitNulls [ "indices" .= indices
              , "no_match_query" .= noMatch
              , "query" .= query ]

instance FromJSON IndicesQuery where
  parseJSON = withObject "IndicesQuery" parse
    where parse o = IndicesQuery
                    <$> o .:? "indices" .!= []
                    <*> o .: "query"
                    <*> o .:? "no_match_query"

instance ToJSON HasParentQuery where
  toJSON (HasParentQuery queryType query scoreType) =
    omitNulls [ "parent_type" .= queryType
              , "score_type" .= scoreType
              , "query" .= query ]

instance FromJSON HasParentQuery where
  parseJSON = withObject "HasParentQuery" parse
    where parse o = HasParentQuery
                    <$> o .: "parent_type"
                    <*> o .: "query"
                    <*> o .:? "score_type"

instance ToJSON HasChildQuery where
  toJSON (HasChildQuery queryType query scoreType) =
    omitNulls [ "query" .= query
              , "score_type" .= scoreType
              , "type"  .= queryType ]

instance FromJSON HasChildQuery where
  parseJSON = withObject "HasChildQuery" parse
    where parse o = HasChildQuery
                    <$> o .: "type"
                    <*> o .: "query"
                    <*> o .:? "score_type"

data NestedQuery =
  NestedQuery
  { nestedQueryPath      :: QueryPath
  , nestedQueryScoreType :: ScoreType
  , nestedQuery          :: Query } deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON NestedQuery where
  toJSON (NestedQuery nqPath nqScoreType nqQuery) =
    object [ "path"       .= nqPath
           , "score_mode" .= nqScoreType
           , "query"      .= nqQuery ]

instance FromJSON NestedQuery where
  parseJSON = withObject "NestedQuery" parse
    where parse o = NestedQuery
                    <$> o .: "path"
                    <*> o .: "score_mode"
                    <*> o .: "query"

data Highlights = Highlights { globalsettings  :: Maybe HighlightSettings
                             , highlightFields :: [FieldHighlight]
                             } deriving (Read, Show, Eq, Generic, Typeable)

data FieldHighlight = FieldHighlight FieldName (Maybe HighlightSettings)
                      deriving (Read, Show, Eq, Generic, Typeable)


data HighlightSettings = Plain PlainHighlight
                       | Postings PostingsHighlight
                       | FastVector FastVectorHighlight
                         deriving (Read, Show, Eq, Generic, Typeable)
data PlainHighlight =
    PlainHighlight { plainCommon  :: Maybe CommonHighlight
                   , plainNonPost :: Maybe NonPostings } deriving (Read, Show, Eq, Generic, Typeable)

 -- This requires that index_options are set to 'offset' in the mapping.
data PostingsHighlight = PostingsHighlight (Maybe CommonHighlight) deriving (Read, Show, Eq, Generic, Typeable)

-- This requires that term_vector is set to 'with_positions_offsets' in the mapping.
data FastVectorHighlight =
    FastVectorHighlight { fvCommon          :: Maybe CommonHighlight
                        , fvNonPostSettings :: Maybe NonPostings
                        , boundaryChars     :: Maybe Text
                        , boundaryMaxScan   :: Maybe Int
                        , fragmentOffset    :: Maybe Int
                        , matchedFields     :: [Text]
                        , phraseLimit       :: Maybe Int
                        } deriving (Read, Show, Eq, Generic, Typeable)

data CommonHighlight =
    CommonHighlight { order             :: Maybe Text
                    , forceSource       :: Maybe Bool
                    , tag               :: Maybe HighlightTag
                    , encoder           :: Maybe HighlightEncoder
                    , noMatchSize       :: Maybe Int
                    , highlightQuery    :: Maybe Query
                    , requireFieldMatch :: Maybe Bool
                    } deriving (Read, Show, Eq, Generic, Typeable)

instance ToJSON FieldHighlight where
    toJSON (FieldHighlight (FieldName fName) (Just fSettings)) =
        object [ fName .= fSettings ]
    toJSON (FieldHighlight (FieldName fName) Nothing) =
        object [ fName .= emptyObject ]

instance ToJSON Highlights where
    toJSON (Highlights global fields) =
        omitNulls (("fields" .= fields)
                  : highlightSettingsPairs global)

instance ToJSON HighlightSettings where
    toJSON hs = omitNulls (highlightSettingsPairs (Just hs))

highlightSettingsPairs :: Maybe HighlightSettings -> [Pair]
highlightSettingsPairs Nothing = []
highlightSettingsPairs (Just (Plain plh)) = plainHighPairs (Just plh)
highlightSettingsPairs (Just (Postings ph)) = postHighPairs (Just ph)
highlightSettingsPairs (Just (FastVector fvh)) = fastVectorHighPairs (Just fvh)


plainHighPairs :: Maybe PlainHighlight -> [Pair]
plainHighPairs Nothing = []
plainHighPairs (Just (PlainHighlight plCom plNonPost)) =
    [ "type" .= String "plain"]
    ++ commonHighlightPairs plCom
    ++ nonPostingsToPairs plNonPost

postHighPairs :: Maybe PostingsHighlight -> [Pair]
postHighPairs Nothing = []
postHighPairs (Just (PostingsHighlight pCom)) =
    ("type" .= String "postings")
    : commonHighlightPairs pCom

fastVectorHighPairs :: Maybe FastVectorHighlight -> [Pair]
fastVectorHighPairs Nothing = []
fastVectorHighPairs (Just
                     (FastVectorHighlight fvCom fvNonPostSettings fvBoundChars
                                          fvBoundMaxScan fvFragOff fvMatchedFields
                                          fvPhraseLim)) =
                        [ "type" .= String "fvh"
                        , "boundary_chars" .= fvBoundChars
                        , "boundary_max_scan" .= fvBoundMaxScan
                        , "fragment_offset" .= fvFragOff
                        , "matched_fields" .= fvMatchedFields
                        , "phraseLimit" .= fvPhraseLim]
                        ++ commonHighlightPairs fvCom
                        ++ nonPostingsToPairs fvNonPostSettings

commonHighlightPairs :: Maybe CommonHighlight -> [Pair]
commonHighlightPairs Nothing = []
commonHighlightPairs (Just (CommonHighlight chScore chForceSource chTag chEncoder
                                      chNoMatchSize chHighlightQuery
                                      chRequireFieldMatch)) =
    [ "order" .= chScore
    , "force_source" .= chForceSource
    , "encoder" .= chEncoder
    , "no_match_size" .= chNoMatchSize
    , "highlight_query" .= chHighlightQuery
    , "require_fieldMatch" .= chRequireFieldMatch]
    ++ highlightTagToPairs chTag

highlightTagToPairs :: Maybe HighlightTag -> [Pair]
highlightTagToPairs (Just (TagSchema _))            = [ "scheme"    .=  String "default"]
highlightTagToPairs (Just (CustomTags (pre, post))) = [ "pre_tags"  .= pre
                                                      , "post_tags" .= post]
highlightTagToPairs Nothing = []



nonPostingsToPairs :: Maybe NonPostings -> [Pair]
nonPostingsToPairs Nothing = []
nonPostingsToPairs (Just (NonPostings npFragSize npNumOfFrags)) =
    [ "fragment_size" .= npFragSize
    , "number_of_fragments" .= npNumOfFrags]

data Search = Search { queryBody       :: Maybe Query
                     , filterBody      :: Maybe Filter
                     , sortBody        :: Maybe Sort
                     , aggBody         :: Maybe Aggregations
                     , highlight       :: Maybe Highlights
                       -- default False
                     , trackSortScores :: TrackSortScores
                     , from            :: From
                     , size            :: Size
                     , searchType      :: SearchType
                     , fields          :: Maybe [FieldName]
                     , source          :: Maybe Source } deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON Search where
  toJSON (Search query sFilter sort searchAggs highlight sTrackSortScores sFrom sSize _ sFields sSource) =
    omitNulls [ "query"        .= query
              , "filter"       .= sFilter
              , "sort"         .= sort
              , "aggregations" .= searchAggs
              , "highlight"    .= highlight
              , "from"         .= sFrom
              , "size"         .= sSize
              , "track_scores" .= sTrackSortScores
              , "fields"       .= sFields
              , "_source"      .= sSource]

{-| 'mkSort' defaults everything but the 'FieldName' and the 'SortOrder' so
    that you can concisely describe the usual kind of 'SortSpec's you want.
-}
mkSort :: FieldName -> SortOrder -> DefaultSort
mkSort fieldName sOrder = DefaultSort fieldName sOrder False Nothing Nothing Nothing

{-| 'Sort' is a synonym for a list of 'SortSpec's. Sort behavior is order
    dependent with later sorts acting as tie-breakers for earlier sorts.
-}
type Sort = [SortSpec]
