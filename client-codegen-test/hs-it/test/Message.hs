{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Message where

import Com.Example.ExampleServiceClient qualified as Client
import Control.Concurrent.STM qualified as Stm
import Data.ByteString (ByteString)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai qualified as Wai.Request
import qualified Test.HUnit as HUnit
import Test.HUnit (Assertion)
import qualified Data.Text as T
import Network.HTTP.Types.Header (HeaderName)

data State = State
  { req :: Stm.TMVar Wai.Request,
    res :: Stm.TMVar Wai.Response,
    rBody :: Stm.TMVar ByteString,
    client :: Client.ExampleServiceClient
  }

data RequestInternal = RequestInternal
  { pathInfo :: [T.Text],
    queryString :: Http.Query,
    requestMethod :: Http.Method,
    requestHeaders :: Http.RequestHeaders
  }

defaultHeaderNames :: [HeaderName]
defaultHeaderNames =
  [ "Host",
    "Accept-Encoding",
    "Content-Length"
  ]

assertEqRequest :: RequestInternal -> Wai.Request -> Assertion
assertEqRequest a b = do
  HUnit.assertEqual "Raw path infos should be equal" (pathInfo a) (Wai.Request.pathInfo b)
  HUnit.assertEqual "Raw query strings should be equal" (queryString a) (Wai.Request.queryString b)
  HUnit.assertEqual "Request methods should be equal" (requestMethod a) (Wai.Request.requestMethod b)
  HUnit.assertEqual "Request headers should be equal" (requestHeaders a) (filter (\(h, _) -> h `notElem` defaultHeaderNames) (Wai.Request.requestHeaders b))

defaultResponse :: Wai.Response
defaultResponse = Wai.responseLBS Http.ok200 [] "{ \"message\": \"Success\" }"
