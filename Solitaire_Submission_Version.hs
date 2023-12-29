module Solitaire_Submission_Version where

import Data.Function
import Data.List
import Data.Maybe
import System.Random

data Suit = Spades | Clubs | Diamonds | Hearts deriving (Eq, Ord, Enum, Bounded, Show)

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Bounded, Show)

type Card = (Pip, Suit)

type Deck = [Card]

pack :: Deck
pack = [(p, s) | p <- [Ace ..], s <- [Spades ..]]

sCard :: Card -> Card
sCard (King, s) = (Ace, s)
sCard (p, s) = (succ p, s)

pCard :: Card -> Card
pCard (Ace, s) = (King, s)
pCard (p, s) = (pred p, s)

isAce :: Card -> Bool
isAce = (==) Ace . fst

isKing :: Card -> Bool
isKing = (==) King . fst

shuffle :: Deck -> Int -> Deck
shuffle d seed = map fst s
  where
    r = take (length d) (randoms (mkStdGen seed) :: [Int])
    z = zip d r
    s = sortBy (\(_, n1) (_, n2) -> compare n1 n2) z

-- >>> shuffle pack 2
-- [(Five,Clubs),(Eight,Diamonds),(Four,Clubs),(King,Diamonds),(Two,Diamonds),(Nine,Diamonds),(Eight,Clubs),(King,Hearts),(Queen,Hearts),(Four,Diamonds),(Seven,Clubs),(Ten,Diamonds),(Five,Spades),(Three,Clubs),(Ten,Clubs),(Jack,Hearts),(Ace,Spades),(Eight,Spades),(King,Clubs),(Six,Diamonds),(Seven,Diamonds),(Ace,Hearts),(Nine,Spades),(Four,Spades),(Five,Diamonds),(Ace,Diamonds),(Nine,Clubs),(Two,Clubs),(Ace,Clubs),(Ten,Spades),(Five,Hearts),(Ten,Hearts),(Six,Clubs),(Eight,Hearts),(Three,Hearts),(Six,Hearts),(Three,Diamonds),(Queen,Clubs),(Nine,Hearts),(Jack,Spades),(Queen,Diamonds),(Two,Spades),(Seven,Spades),(Two,Hearts),(Seven,Hearts),(King,Spades),(Six,Spades),(Queen,Spades),(Four,Hearts),(Three,Spades),(Jack,Clubs),(Jack,Diamonds)]

type Foundation = [Card]

type Columns = [[Card]]

type Reserve = [Card]

type Stock = [FaceCard]

type SpiderCols = [[FaceCard]]

data Board = EOBoard Foundation Columns Reserve | SBoard Foundation SpiderCols Stock deriving (Eq)

type FaceUp = Bool

type FaceCard = (Card, FaceUp)

showFaceCard :: FaceCard -> String
showFaceCard (card, faceup) =
  if faceup then show card else "<unknown>"

showColumns :: Columns -> String
showColumns = unlines . map ((++) " " . show)

showFaceCardList :: [FaceCard] -> String
showFaceCardList fs = "[" ++ intercalate ", " (map showFaceCard fs) ++ "]"

showSpiderColumns :: SpiderCols -> String
showSpiderColumns = unlines . map ((++) " " . showFaceCardList)

instance Show Board where
  show (EOBoard f c r) =
    "EOBoard"
      ++ "\nFoundations  "
      ++ show f
      ++ "\nColumns"
      ++ "\n"
      ++ showColumns c
      ++ "Reserve   "
      ++ show r
      ++ "\n"
  show (SBoard f c s) =
    "SBoard"
      ++ "\nFoundations  "
      ++ show f
      ++ "\nColumns"
      ++ "\n"
      ++ showSpiderColumns c
      ++ "Stock   "
      ++ (show . length) s
      ++ " Deals remaining \n"

dummyEO :: Board
dummyEO =
  EOBoard
    []
    [ [(Ace, Clubs), (Seven, Diamonds), (Ace, Hearts), (Queen, Hearts), (King, Clubs), (Four, Spades)],
      [(Five, Diamonds), (Queen, Spades), (Three, Diamonds), (Five, Spades), (Six, Spades), (Seven, Hearts)],
      [(King, Hearts), (Ten, Diamonds), (Seven, Spades), (Queen, Diamonds), (Five, Hearts), (Eight, Diamonds)],
      [(Jack, Spades), (Six, Hearts), (Seven, Clubs), (Eight, Spades), (Ten, Clubs), (Queen, Clubs)],
      [(Ace, Spades), (Eight, Clubs), (Ace, Diamonds), (King, Diamonds), (Jack, Hearts), (Four, Clubs)],
      [(Two, Diamonds), (Three, Hearts), (Three, Clubs), (Ten, Hearts), (Six, Diamonds), (Jack, Clubs)],
      [(Nine, Spades), (Four, Diamonds), (Nine, Clubs), (Nine, Hearts), (Three, Spades), (Ten, Spades)],
      [(Two, Clubs), (Two, Spades), (Four, Hearts), (Nine, Diamonds), (King, Spades), (Eight, Hearts)]
    ]
    [(Two, Hearts), (Six, Clubs), (Five, Clubs), (Jack, Diamonds)]

eODeal :: Int -> Board
eODeal seed = EOBoard f c r
  where
    s = shuffle pack seed
    f = []
    c = oDealA (take 48 s)
    r = drop 48 s

oDealA :: Deck -> [Deck]
oDealA [] = []
oDealA d = take 6 d : oDealA (drop 6 d)

sDeal :: Int -> Board
sDeal seed = SBoard f c s
  where
    pack' = map (\x -> (x, False)) $ shuffle (pack ++ pack) seed
    turnFaceUp (card, _) = (card, True)
    turnFirstCardUp l = turnFaceUp (head l) : tail l
    get6 i = (turnFirstCardUp . take 6 . drop (6 * (i -1))) pack'
    get5 i = (turnFirstCardUp . take 5 . drop (5 * (i -5) + 24)) pack'
    f = []
    c = [if i > 4 then get5 i else get6 i | i <- [1 .. 10]]
    s = drop 54 pack'

removeCardInCols :: Columns -> (Card -> Bool) -> Columns
removeCardInCols [] _ = []
removeCardInCols ([] : cs) prop = removeCardInCols cs prop
removeCardInCols (c@(x : xs) : cs) prop
  | prop x = xs : removeCardInCols cs prop
  | otherwise = c : removeCardInCols cs prop

chooseNotNull :: Columns -> Columns
chooseNotNull = filter (not . null)

-- try to insert cards in to columns, if fail return the origin column
insertCardInCols :: Columns -> Card -> Columns
insertCardInCols cols crd =
  map
    ( \col@(h : _) ->
        if (not . isAce) h && pCard crd == h
          then crd : col
          else col
    )
    cols

toFoundations :: Board -> Board
toFoundations SBoard {} = undefined
toFoundations eob
  | eob /= eob' = toFoundations eob'
  | eob /= eob'' = toFoundations eob''
  | eob /= eob''' = toFoundations eob'''
  | eob /= eob'''' = toFoundations eob''''
  | otherwise = eob
  where
    eob' = moveAceFromColToFoundations eob
    eob'' = moveAceFromRsvToFoundations eob
    eob''' = moveCardFromColsToFoundations eob
    eob'''' = moveCardFromRsvToFoundations eob

-- take an EOBoard and put all ace in columns to foundations
moveAceFromColToFoundations :: Board -> Board
moveAceFromColToFoundations (EOBoard f c r) =
  EOBoard
    (f ++ filter isAce (map head c))
    (chooseNotNull (removeCardInCols c isAce))
    r
moveAceFromColToFoundations _ = undefined

-- take an EOBoard and put all ace in reserve to foundations
moveAceFromRsvToFoundations :: Board -> Board
moveAceFromRsvToFoundations (EOBoard f c r) =
  EOBoard
    (f ++ filter isAce r)
    c
    (filter (not . isAce) r)
moveAceFromRsvToFoundations _ = undefined

-- take an EOBoard and put all ace in reserve to foundations
moveCardFromColsToFoundations :: Board -> Board
moveCardFromColsToFoundations (EOBoard f c r) = EOBoard newFoundations newCols r
  where
    heads = map head (filter (not . null) c)
    succs = map sCard f `intersect` heads
    newCols = chooseNotNull $ removeCardInCols c (`elem` succs)
    newFoundations = map (\card -> if sCard card `elem` succs then sCard card else card) f
moveCardFromColsToFoundations _ = undefined

moveCardFromRsvToFoundations :: Board -> Board
moveCardFromRsvToFoundations (EOBoard f c r) = EOBoard newFoundations c newRsv
  where
    succs = map sCard f `intersect` r
    newRsv = filter (not . (`elem` succs)) r
    newFoundations = map (\card -> if sCard card `elem` succs then sCard card else card) f
moveCardFromRsvToFoundations _ = undefined

getCol :: Board -> Columns
getCol (EOBoard _ c _) = c
getCol _ = undefined

findMoves :: Board -> [Board]
findMoves brd@(EOBoard _ c r) = mRsvToCol ++ mColToRsv ++ mColToCol
  where
    brd' = toFoundations brd
    cH = [h | (h : _) <- filter (not . null) c]
    lRsvToCol = [m | m <- r, sCard m `elem` cH] -- R -> C
    lColToCol = [m | m <- cH, sCard m `elem` cH] -- C -> C
    mRsvToCol = filter (/= brd') (map (`rsvToCol` brd') lRsvToCol)
    mColToCol = filter (/= brd') (map (`colToCol` brd') lColToCol)
    mColToRsv = filter (/= brd') (map (`colToRsv` brd') cH) -- C -> R
findMoves _ = undefined

-- Takes an EOBoard and calls findMoves. It returns the best move depending on the board weight
chooseMove :: Board -> Maybe Board
chooseMove brd
  | brd == chooseMoveOne (chooseMoveOne brd) = Nothing
  | brd == chooseMoveOne (chooseMoveOne (chooseMoveOne brd)) = Nothing
  | brd == chooseMoveOne (chooseMoveOne (chooseMoveOne (chooseMoveOne brd))) = Nothing
  | otherwise = Just (chooseMoveOne brd)

chooseMoveOne :: Board -> Board
chooseMoveOne b@EOBoard {}
  | (not . null) afterToFoundationBoards = maximumBy (compare `on` boardWeighted) afterToFoundationBoards
  | otherwise = b
  where
    afterToFoundationBoards = map toFoundations (findMoves b)
chooseMoveOne _ = undefined

boardWeighted :: Board -> Int
boardWeighted (EOBoard f c r) = notEmptyCol + cardValues + rsvPenalty + colOnlyHaveKing
  where
    notEmptyCol = length $ chooseNotNull c
    cardValues = 100 * getCardValues f
    rsvPenalty = -20 * length r
    colOnlyHaveKing = 10 * length (filter isKing (map head (filter ((== 1) . length) c)))
boardWeighted _ = undefined

-- Take a card from Reserves and update the target column
rsvToCol :: Card -> Board -> Board
rsvToCol crd (EOBoard f c r)
  | length c < 8 && isKing crd = EOBoard f ([crd] : c) nR
  | otherwise = EOBoard f nC nR
  where
    cols = filter (not . null) c
    nR = filter (/= crd) r
    nC = insertCardInCols cols crd
rsvToCol _ _ = undefined

-- Move card from column to column
-- If the card is king and there is an empty column, the empty column will be updated with the king card
colToCol :: Card -> Board -> Board
colToCol crd (EOBoard f c r)
  | length c < 8 && isKing crd = EOBoard f ([crd] : c) r
  | otherwise = EOBoard f nC r
  where
    cols = filter (not . null) c
    delC = chooseNotNull $ removeCardInCols cols (crd ==)
    nC = insertCardInCols delC crd
colToCol _ _ = undefined

{-
Take a card from the head of the column and attach it to reserves (if there is enough space)
If the reserve has 5 occupied cells, the card will not be moved
-}
colToRsv :: Card -> Board -> Board
colToRsv crd brd@(EOBoard f c r)
  | consecCards crd c = brd
  | length r < 5 = EOBoard f nC nR
  | otherwise = brd
  where
    cols = filter (not . null) c
    nC = chooseNotNull $ removeCardInCols cols (crd ==)
    nR = crd : r
colToRsv _ _ = undefined

{-
consecCards is a function for checking consecutive card pairs in a column.
Make sure that cards are not exchanged between reserves and columns indefinitely.
-}
consecCards :: Card -> Columns -> Bool
consecCards _ [] = False
consecCards crd (h : t)
  | length h > 1 && pCard crd == h !! 1 = True
  | otherwise = consecCards crd t

cardPipValue :: Card -> Int
cardPipValue (p, _) = fromEnum p

haveWon :: Board -> Bool
haveWon (EOBoard _ [] []) = True
haveWon _ = False

getCardValues :: Foundation -> Int
getCardValues = sum . map cardPipValue

playSolitaire :: Board -> Int
playSolitaire board@(EOBoard f _ _)
  | isNothing nextMove = getCardValues f
  | isJust nextMove = playSolitaire (fromMaybe (EOBoard [] [] []) nextMove)
  where
    nextMove = chooseMove board
playSolitaire _ = undefined

analyseEO :: Int -> Int -> (Int, Double)
analyseEO _ 0 = (0, 0)
analyseEO seed n = (won', average')
  where
    (seed', _) = random (mkStdGen seed)
    (won, average) = analyseEO seed' (n -1)
    score = playSolitaire (eODeal seed')
    won' = won + if score == 52 then 1 else 0
    average' = ((average * fromIntegral (n -1)) + fromIntegral score) / fromIntegral n

-- Step 5
dummySB :: Board
dummySB = sDeal 2

{- Paste the contents of this file, including this comment, into your source file, below all
   of your code. You can change the indentation to align with your own, but other than this,
   ONLY make changes as instructed in the comments.
 -}
-- Constants that YOU must set:
studentName = "Weihua Qu"

studentNumber = "200180838"

studentUsername = "aca20wq"

initialBoardDefined = dummyEO

{- replace XXX with the name of the constant that you defined in step 3 of part 1 -}
secondBoardDefined = dummySB {- replace YYY with the constant defined in step 5 of part 1,
                             or if you have chosen to demonstrate play in a different game
                             of solitaire for part 2, a suitable contstant that will show
                             your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
   work you have completed is tested. DO NOT change anything other than comments (and indentation
   if needed). The comments in the template file are set up so that only the constant eight-off
   board from part 1 and the toFoundations function from part 1 are tested. You will probably
   want more than this tested.

   CHECK with Emma or one of the demonstrators if you are unsure how to change this.

   If you mess this up, your code will not compile, which will lead to being awarded 0 marks
   for functionality and style.
-}

main :: IO ()
main =
  do
    --let deals' = findMoves (iterate chooseMoveOne (eODeal 21) !! 7) !! 3 in
    --print $ toFoundations deals'
    putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

    putStrLn "***The eight-off initial board constant from part 1:"
    print initialBoardDefined

    let board = toFoundations initialBoardDefined
    putStrLn "***The result of calling toFoundations on that board:"
    print board

    {- Move the start comment marker below to the appropriate position.
      If you have completed ALL the tasks for the assignment, you can
      remove the comments from the main function entirely.
      DO NOT try to submit/run non-functional code - you will receive 0 marks
      for ALL your code if you do, even if *some* of your code is correct.
    -}

    let boards = findMoves board -- show that findMoves is working
    putStrLn "***The possible next moves after that:"
    print boards

    let chosen = chooseMove board -- show that chooseMove is working
    putStrLn "***The chosen move from that set:"
    print chosen
    putStrLn "***Now showing a full game" -- display a full game
    score <- displayGame initialBoardDefined 0
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)

    putStrLn "\n\n\n************\nNow looking at the alternative game:"

    putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
    print secondBoardDefined -- show the suitable constant. For spider solitaire this
    -- is not an initial game, but a point from which the game
    -- can be won
    {- start comment marker - move this if appropriate

     putStrLn "***Now showing a full game for alternative solitaire"
     score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                               -- works correctly)
     putStrLn $ "Score: " ++ score
     putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

    -}

{- displayGame takes a Board and move number (should initially be 0) and
   displays the game step-by-step (board-by-board). The result *should* be
   the same as performing playSolitaire on the initial board, if it has been
   implemented correctly.
   DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}
displayGame :: Board -> Int -> IO String
displayGame board n =
  if haveWon board
    then return "A WIN"
    else do
      putStr ("Move " ++ show n ++ ": " ++ show board)
      let maybeBoard = chooseMove board
      if isJust maybeBoard
        then do
          let (Just newBoard) = maybeBoard
          displayGame newBoard (n + 1)
        else do
          let score = show (playSolitaire board)
          return score
