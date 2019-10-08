{- Test suite for dynamic programming algorithms -}

import Control.Monad (forM_)
import Test.Hspec

import CYK

main :: IO ()
main = hspec $ do


  describe "CYK determines if a sentence is valid given a grammar" $ do

    -- Our sample grammar in Chomsky normal form, taken from:
    -- https://en.wikipedia.org/wiki/CYK_algorithm#Example
    let grammar :: Grammar
        grammar = ("S", -- start symbol for this grammar
                    [
                    ("S",   Prod "NP"  "VP" ),
                    ("VP",  Prod "VP"  "PP" ),
                    ("VP",  Prod "V"   "NP" ),
                    ("VP",  Unit "eats"     ),
                    ("PP",  Prod "P"   "NP" ),
                    ("NP",  Prod "Det" "N"  ),
                    ("NP",  Unit "she"      ),
                    ("V",   Unit "eats"     ),
                    ("P",   Unit "with"     ),
                    ("N",   Unit "fish"     ),
                    ("N",   Unit "fork"     ),
                    ("Det", Unit "a"        )
                  ])

    let passes = [ "she eats a fish with a fork",
                   "she eats a fish",
                   "she eats with a fork",
                   "a fish eats a fork with a fish",
                   "a fish eats a fish with a fork"
                 ]

    let fails = [ "she",
                  "a fish eats a",
                  "fish"
                ]

    context "valid sentences" $ do
      forM_ passes $ \test ->
        it test $ check grammar test `shouldBe` True

    context "invalid sentences" $ do
      forM_ fails $ \test ->
        it test $ check grammar test `shouldBe` False

