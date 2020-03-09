module Servant.API.MultiverbsSpec where

import Servant.API.Multiverbs

import Data.Proxy
import Servant.API
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Verbs" $ do

    describe "HasLink" $ do

      it "should be able to construct a link" $ do

        let l = safeLink (Proxy @API) (Proxy @ExampleEndpoint)

        toUrlPiece l `shouldBe` "test"

type API = ExampleEndpoint :<|> RootEndpoint

type ExampleEndpoint =
  "test" :> Gets '[JSON] (Result 503 String :<|> Result 200 [String])

type RootEndpoint = Get '[JSON] String
