-- import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
-- import Control.Monad (replicateM)
-- import Data.Foldable (for_)
import Test.Hspec (Spec, it, shouldBe, shouldReturn)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  it "can parse integers - smoke test" $ do
    read "10" `shouldBe` (10 :: Int)
