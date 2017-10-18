import           System.Exit
import qualified Test.Corba.Runtime.Core.Json as Json

main :: IO ()
main =
  runner [
      Json.tests
    ]

runner :: [IO Bool] -> IO ()
runner tests = do
  b <- fmap and (sequence tests)
  if b
    then exitSuccess
    else exitFailure
