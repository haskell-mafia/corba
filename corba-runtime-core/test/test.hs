
import           Disorder.Core.Main
import qualified Test.Corba.Runtime.Core.Json as Json

main :: IO ()
main =
  disorderMain [
      Json.tests
    ]
