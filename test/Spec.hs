import           Data.Bool   (bool)
import           Recursion   (mc91)
import           System.Exit (ExitCode (..), exitWith)
import           Test.HUnit  (Assertion, Counts (..), Test (..), runTestTT,
                              (@=?))
import           WordNumber  (wordNumber)

main :: IO ()
main = exitProperly $ runTestTT tests

tests :: Test
tests =
  TestList [ testCase "The McCarthy 91 function: [95..110]" $
             [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100] @=?
             map mc91 ([95..110] :: [Integer])
           , testCase "Convert 12324546 to hyphenated words" $
             "one-two-three-two-four-five-four-six" @=? wordNumber 12324546
           ]

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  let nonZero = ((0 /=) . ($ counts))
      exit    = exitWith . bool ExitSuccess (ExitFailure 1)
  exit $ any nonZero [failures, errors]
