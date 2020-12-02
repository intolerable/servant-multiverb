module Servant.Server.MultiverbsSpec where

import Test.Hspec
import Test.Hspec.Wai

import Servant.API.Multiverbs
import Servant.Server.Multiverbs ()
import Servant

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (pure exampleApp) $ do

  describe "when the param is \"int\"" $ do
    it "should receive a 5 body with a 200 status code" $ do
      post "/int" "" `shouldRespondWith` "5" { matchStatus = 200 }

  describe "when the param is \"string\"" $ do
    it "should receive a \"hi!\" body with a 503 status code" $ do
      post "/string" "" `shouldRespondWith` "\"hi!\"" { matchStatus = 503, matchHeaders = ["My-Header" <:> "My-Header value"] }

  describe "when the param is anything else" $ do
    it "should receive a 5 body with a 500 status code" $ do
      post "/frogs-legs" "" `shouldRespondWith` "[]" { matchStatus = 500 }

exampleApp :: Application
exampleApp =
  serveWithContext (Proxy @ExampleAPI) (() :. EmptyContext) exampleServer

exampleServer :: String -> Handler (Either () (Either HeadersString Int))
exampleServer = \case
  "int" -> pure $ Right $ Right 5
  "string" -> pure $ Right $ Left $ addHeader "My-Header value" "hi!"
  _ -> pure $ Left ()

type HeadersString = Headers '[Header "My-Header" String] String

type ExampleAPI =
  Capture "type" String :> Verbs 'POST '[JSON]
    (Result 500 () :<|> Result 503 (Headers '[Header "My-Header" String] String) :<|> Result 200 Int)
