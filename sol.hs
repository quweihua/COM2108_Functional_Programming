module Sol where

import Data.List
import Data.Maybe
import System.IO
import Debug.Trace

--------------------------------------- Step 1----------------------------------------------
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Enum, Show)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum)
type Card = (Pip, Suit)
type Deck = [Card]

--------------------------------------- Step 2----------------------------------------------
-- pack
pack :: [Card]
pack = [(s, p) | s <- suit, p <- pip]
  where
    suit = [Hearts, Clubs, Spades, Diamonds]
    pip = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
-- sCard
sCard :: Card -> Card 
sCard (p, s) = (succ p, s) 
-- pCard
pCard :: Card -> Card
pCard (s, p) =  (pred p, s)
-- isAce
isAce :: Card -> Bool
isAce (p, s) = p == Ace
-- isKing
isKing :: Card -> Bool
isKing (p, s) = s == King

-- shuffle: not sure about it
shuffle :: 
shuffle seed = dset where -- lecture 11: page 8 of 14
    gen = mkStdGen seed
    weights = take 52 (randoms gen :: [Int])
    dset = (map fst (sortBy (\(_, w1)(_, w2) -> (compare w1 w2)) (zip pack weights))
--------------------------------------- Step 3----------------------------------------------
type Foundation = [Card]
type Column = [[Card]]
type Reserve = [Card]
type EOBoard = (Foundation, Column, Reserve)

eODeal :: Int -> EOBoard
eODeal seed = (foundations, columns, reserve) where -- seed comes from shuffle, page 8 of 14 in lec 11
    shuffled = shuffle seed
    foundations = [] 
    columns = [(take 6 shuffled), (take 6 (drop 6 shuffled)),  (take 6 (drop 12 shuffled)), (take 6 (drop 18 shuffled)), (take 6 (drop 24 shuffled)), (take 6 (drop 30 shuffled)), (take 6 (drop 36 shuffled)), (take 6 (drop 42 shuffled))]
    reserve = [shuffled !! 48, shuffled !! 49,shuffled !! 50, shuffled !! 51]

toFoundations :: EOBoard -> EOBoard 
toFoundations board = toFA (reserveAcesToFoundations board) where 
    toFA :: EOBoard -> EOBoard 
    toFA board 
        | board <> cafBoard = toFA cafBoard
        | board <> rtfBoard = toFA rtfBoard
        | board <> chfBoard = toFA chfBoard
        | otherwise = board
        where 
            cafBoard = (columnsAcesToFoundations board)
            rtfBoard = reserve