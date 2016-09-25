{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.V1.Client
  ( module Database.Bloodhound.Common.Client
  
  , deleteMapping
  ) where
  
import Database.Bloodhound.Common.Client
import Database.Bloodhound.Common.Util
import Database.Bloodhound.V1.Types

-- | 'deleteMapping' is an HTTP DELETE and deletes a mapping for a given index.
-- Mappings are schemas for documents in indexes.
--
deleteMapping :: MonadBH m => IndexName -> MappingName -> m Reply
deleteMapping (IndexName indexName)
  (MappingName mappingName) =
  -- "_mapping" and mappingName below were originally transposed
  -- erroneously. The correct API call is: "/INDEX/_mapping/MAPPING_NAME"
  delete =<< joinPath [indexName, "_mapping", mappingName]
