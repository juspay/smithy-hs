{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Message where

import Com.Example.ExampleServiceClient qualified as Client
import Control.Concurrent.STM qualified as Stm
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai qualified as Wai.Request

data State = State
  { req :: Stm.TMVar Wai.Request,
    res :: Stm.TMVar Wai.Response,
    client :: Client.ExampleServiceClient
  }

compareRequest :: Wai.Request -> Wai.Request -> IO Bool
compareRequest a b = do
  bodyA <- Wai.Request.strictRequestBody a
  bodyB <- Wai.Request.strictRequestBody b

  pure $
    Wai.Request.rawQueryString a == Wai.Request.rawQueryString b
      && (Wai.Request.rawPathInfo a == Wai.Request.rawPathInfo b)
      && (Wai.Request.requestMethod a == Wai.Request.requestMethod b)
      && (Wai.Request.requestHeaders a == Wai.Request.requestHeaders b)
      && bodyA == bodyB

defaultResponse :: Wai.Response
defaultResponse = Wai.responseLBS Http.ok200 [] "{ \"message\": \"Success\" }"
