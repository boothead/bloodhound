{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Common.Util
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

import Database.Bloodhound.Common.Types.Internal

{-| 'Reply' and 'Method' are type synonyms from
'Network.HTTP.Types.Method.Method' -}
type Reply = Network.HTTP.Client.Response L.ByteString
type Method = NHTM.Method

-- Shortcut functions for HTTP methods
delete :: MonadBH m => Text -> m Reply
delete = flip (dispatch NHTM.methodDelete) Nothing
get    :: MonadBH m => Text -> m Reply
get    = flip (dispatch NHTM.methodGet) Nothing
head   :: MonadBH m => Text -> m Reply
head   = flip (dispatch NHTM.methodHead) Nothing
put    :: MonadBH m => Text -> Maybe L.ByteString -> m Reply
put    = dispatch NHTM.methodPut
post   :: MonadBH m => Text -> Maybe L.ByteString -> m Reply
post   = dispatch NHTM.methodPost

parseUrl' :: MonadThrow m => Text -> m Request
parseUrl' t = parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack t))

emptyBody :: L.ByteString
emptyBody = L.pack ""

joinPath' :: [Text] -> Text
joinPath' = T.intercalate "/"

joinPath :: MonadBH m => [Text] -> m Text
joinPath ps = do
  Server s <- bhServer <$> getBHEnv
  return $ joinPath' (s:ps)

dispatch :: MonadBH m
         => Method
         -> Text
         -> Maybe L.ByteString
         -> m Reply
dispatch dMethod url body = do
  initReq <- liftIO $ parseUrl' url
  reqHook <- bhRequestHook A.<$> getBHEnv
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  req <- liftIO $ reqHook $ setRequestIgnoreStatus $ initReq { method = dMethod
                                                             , requestBody = reqBody }
  mgr <- bhManager <$> getBHEnv
  liftIO $ httpLbs req mgr
