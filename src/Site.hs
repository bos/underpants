{-# LANGUAGE OverloadedStrings #-}

module Site (site) where

import Application
import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.CatchIO (try)
import Data.Aeson (FromJSON, Result(..), ToJSON, Value, encode, fromJSON, json)
import Data.Attoparsec.Enumerator (iterParser)
import Snap.Internal.Iteratee.Debug (iterateeDebugWrapper)
import Snap.Types


decodeJSONRequest :: FromJSON a => MonadSnap m => m a
decodeJSONRequest = withRequest $ \req -> do
  let ctype = getHeader "Content-Type" req
  when (ctype /= Just "application/json") .
    withResponse $ finishWith . setResponseCode 415
  jreq <- runRequestBody . try . iterateeDebugWrapper "json" $ iterParser json
  case (fromJSON <$> jreq) :: FromJSON a => Either SomeException (Result a) of
    Right (Success r) -> return r
    _ -> do
      modifyResponse $ setResponseCode 400
      writeText "unable to parse or convert request"
      finishWith =<< getResponse

respond :: ToJSON a => a -> Application ()
respond v = do
  modifyResponse $ setContentType "application/json"
  writeLBS (encode v)

underpants :: Application ()
underpants = do
  undies <- decodeJSONRequest
  respond (undies::Value)

site :: Application ()
site = route [ ("/", underpants) ]
