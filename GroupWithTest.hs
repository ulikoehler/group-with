import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Control.GroupWith
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.List (sort)
import qualified Data.Map as Map

multimapElemEq :: (Eq k, Ord v) => (k,[v]) -> (k,[v]) -> Bool
multimapElemEq (k1,v1) (k2,v2) = (k1 == k2) && (sort v1 == sort v2)

-- Check if two multimap-representing lists are equal,
-- disregarding value list order
multimapListEq :: (Ord a, Ord b, Eq a, Eq b) => [(a,[b])] -> [(a,[b])] -> Bool
multimapListEq xs ys =
    let zipped = zipWith multimapElemEq xs ys
    in and zipped

multimapEq :: (Ord a, Ord b, Eq a, Eq b) => Map a [b] -> Map a [b] -> Bool
multimapEq x y =
    multimapListEq (Map.toList x) (Map.toList y)

multimapTupleEq :: (Ord a, Ord b, Eq a, Eq b) => (Map a [b], Map a [b]) -> Bool
multimapTupleEq (x,y) = multimapEq x y

main :: IO ()
main = hspec $ do
  describe "groupWith" $ do
    it "should group a simple value correctly" $
        let data_ = ["a","abc","ab","bc"]
            fn = take 1
            ref = Map.fromList [("a",["a","abc","ab"]),("b",["bc"])]
            result = groupWith fn data_
        in (result, ref) `shouldSatisfy` multimapTupleEq
    it "should return an empty map when given an empty list" $
        -- We need to specialize here, because it's ⊥
        let fn = error "This function should never be called" :: Int -> Int
            data_ = [] :: [Int]
            result = groupWith fn data_
        in result `shouldSatisfy` ((==) 0 . Map.size)
    -- Fuzzing, couldn't get this to compile properly yet
    -- it "should not crash for any input string list" $
    --    property $ (\d -> groupWith (take 1) d `seq` True)
  describe "groupWithUsing" $ do
    it "should return an empty map when given an empty list" $
        -- We need to specialize here, because it's ⊥
        let fn = error "This function should never be called" :: Int -> Int
            data_ = [] :: [Int]
            result = groupWith fn data_
        in result `shouldSatisfy` ((==) 0 . Map.size)
    it "should be usable for counting" $
        -- Instead of building up lists, we count the number of elements
        let t n = 1 -- For each x, count 1
            c = (+) -- Sum up the counts
            fn = take 1
            data_ = ["a","abc","ab","bc"]
            ref = Map.fromList [("a",3),("b",1)] :: Map String Int
            result = groupWithUsing t c fn data_
        in result `shouldBe` ref
  describe "groupWithMultiple" $ do
    it "should group correctly given a simple list" $
        let data_ = ["a","abc","ab","bc"]
            fn x = [take 1 x, take 2 x]
            -- Note the multiple "a"s in the first line:
            -- one from `take 1`, one from `take 2`
            ref = Map.fromList [("a",["a","a","abc","ab"]),
                                ("ab",["ab","abc"]),
                                ("b",["bc"]),
                                ("bc",["bc"])]
            result = groupWithMultiple fn data_
        in (result, ref) `shouldSatisfy` multimapTupleEq
    it "should return an empty map when given an empty list" $
        -- We need to specialize here, because it's ⊥
        let fn = error "This function should never be called" :: Int -> [Int]
            data_ = [] :: [Int]
            result = groupWithMultiple fn data_
        in result `shouldSatisfy` ((==) 0 . Map.size)
  {-
    NOTE: We will use Maybe as monad / applicative functor for this test
  -}
  describe "groupWithM" $ do
    it "should group a simple value correctly" $
        let data_ = ["a","abc","ab","bc"]
            fn = Just . take 1
            ref = Map.fromList [("a",["a","abc","ab"]),("b",["bc"])]
            result = groupWithM fn data_
        in (fromJust result, ref) `shouldSatisfy` multimapTupleEq
    it "should return an empty map when given an empty list" $
        -- We need to specialize here, because it's ⊥
        let fn = error "This function should never be called" :: Int -> Maybe Int
            data_ = [] :: [Int]
            result = groupWithM fn data_
        in (fromJust result) `shouldSatisfy` ((==) 0 . Map.size)
    -- Fuzzing, couldn't get this to compile properly yet
    -- it "should not crash for any input string list" $
    --    property $ (\d -> groupWith (take 1) d `seq` True)