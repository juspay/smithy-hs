{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Message where

import qualified Control.Concurrent.STM as Stm
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai as Wai.Request

data State = State
  { req :: Stm.TMVar Wai.Request,
    res :: Stm.TMVar Wai.Response
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
defaultResponse = Wai.responseLBS Http.ok200 [] ""
