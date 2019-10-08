{- CYK.hs - Cocke–Younger–Kasami algorithm

   This is a Haskell implementation of the CYK algorithm for parsing context-free grammars.
   This code builds off the ideas in Tikhon Jelvis' blog article about dynamic programming
   using lazy array evaluation in Haskell - http://jelv.is/blog/Lazy-Dynamic-Programming/

   See also:

     https://en.wikipedia.org/wiki/CYK_algorithm
     https://en.wikipedia.org/wiki/Context-free_grammar

-}

module CYK (
  Grammar,
  Replacement(..),
  check
) where

import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.List     (group, sort)
import qualified Data.Array as Array


{- Types -}

-- A grammar is a start symbol with a list of production rules
type Grammar     = (String, [(String, Replacement)])

data Replacement = Unit String
                 | Prod String String


{- Methods -}

-- Check if a given sentence is valid according to a grammar; ie, it can be formed
-- solely using the rules of the grammar, starting with the start symbol
check :: Grammar -> String -> Bool
check (start, grammar) sentence = start `elem` top
  where
    top     = go n 1
    n       = length terms
    terms   = words sentence

    array :: Array.Array (Int,Int) [String]
    array  = Array.listArray bounds [ go l s | (l,s) <- Array.range bounds ]
      where
        bounds = ((1,1), (n,n))


    -- the base cases, row 1 of the array
    go :: Int -> Int -> [String]
    go 1 s = filter f [ a | a@(_, Unit val) <- grammar ] & firsts
      where
        f :: (String, Replacement) -> Bool
        f (_, Unit val) = val == terms !! (s-1)

    -- rows 2 and up of the array
    go l s = filter f [ a | a@(_, Prod _ _) <- grammar ] & firsts
      where
        f :: (String, Replacement) -> Bool
        f (_, Prod b c) = or [    b `elem` (array Array.! (p,  s  ))
                               && c `elem` (array Array.! (l-p,s+p)) | p <- [1..l-1] ]


-- Get the distinct first elements from a list of tuples
firsts :: Ord a => [(a,b)] -> [a] 
firsts = map fst >>> sort >>> group >>> map head

