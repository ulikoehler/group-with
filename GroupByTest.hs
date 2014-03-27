module GroupByTest (tests) where

import Distribution.TestSuite
import Control.Monad
import Control.GroupBy
import Data.Map (Map)
import qualified Data.Map as Map

multimapElemEq (k1,v1) (k2,v2) = (k1 == k2) && (v1 == v2)

-- Check
multimapListEq :: (Eq a, Eq b) => [(a,[b])] -> [(a,[b])] -> Bool
multimapListEq xs ys =
    let zipped = zipWith multimapElemEq xs ys
    in and zipped

multimapEq :: (Eq a, Eq b) => Map a [b] -> Map a [b] -> Bool
multimapEq x y =
    multimapListEq (Map.toList x) (Map.toList y)


-- TODO Use proper assertion library
testGroupBy = do
    let data1 = ["a","abc","ab","bc"]
    let fn1 = take 1
    let ref1 = Map.fromList [("a",["a","abc","ab"]),("b",["bc"])]
    let result = groupBy fn1 data1
    unlessM (result `multimapEq` ref1) $
        Fail $ "groupBy test 1 failed: " ++ show result

tests :: IO [Test]
tests = return [Test groupByTest]
    where
        groupByTest = TestInstance
            { run = testGroupBy
            , name = "groupBy"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right testGroupBy
            }
