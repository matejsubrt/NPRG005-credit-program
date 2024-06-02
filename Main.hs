import Data.Array
import Data.Char (ord)
import Data.List
import Control.Monad (when)
import Data.Maybe
import Debug.Trace

data PieceType = Stone | Dame deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)
data Piece = Piece PieceType Color deriving (Eq, Show)

type Board = Array (Int, Int) (Maybe Piece)

initializeBoard :: Board
initializeBoard = array ((1,1), (8,8)) [((i, j), initialPiece i j) | i <- [1..8], j <- [1..8]]
  where
    initialPiece i j
      | i <= 3 && even (i + j) = Just (Piece Stone Black)
      | i >= 6 && even (i + j) = Just (Piece Stone White)
      | otherwise = Nothing

showPiece :: Maybe Piece -> String
showPiece Nothing = " "
showPiece (Just (Piece Stone Black)) = "○"
showPiece (Just (Piece Stone White)) = "●"
showPiece (Just (Piece Dame Black)) = "◎"
showPiece (Just (Piece Dame White)) = "◉"

showBoard :: Board -> String
showBoard board = unlines $
    [lettersLine] ++
    [firstHorizontalLine] ++
    concatMap (\i -> [boxLine i, horizontalLine]) [1..7] ++
    [boxLine 8] ++
    [lastHorizontalLine]
    where
        lettersLine = "   ┃" ++ intercalate "│" (map (\c -> " " ++ [c] ++ " ") ['A'..'H']) ++ "┃"
        firstHorizontalLine = "━━━╋" ++ intercalate "┿" (replicate 8 "━━━") ++ "┫"
        horizontalLine = "───╂" ++ intercalate "┼" (replicate 8 "───") ++ "┨"
        lastHorizontalLine = "━━━┻" ++ intercalate "┷" (replicate 8 "━━━") ++ "┛"
        boxLine i = " " ++ show i ++ " ┃" ++ intercalate "│" [padCell (showPiece (board ! (i, j))) | j <- [1..8]] ++ "┃"
            where
                padCell cell = " " ++ cell ++ " "


removeStones :: Board -> [(Int, Int)] -> Board
removeStones board coords = board // [(coord, Nothing) | coord <- coords]


makeMove :: Board -> (Int, Int) -> (Int, Int) -> Board
makeMove board (srcRow, srcCol) (destRow, destCol) = --board // [(dest, board ! src), (src, Nothing)]
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
    in removeStones board intermediateSquares // [((destRow, destCol), board ! (srcRow, srcCol)), ((srcRow, srcCol), Nothing)]


withinBoundsAndHasPiece :: Board -> (Int, Int) -> Bool
withinBoundsAndHasPiece board (row, col) =
    inRange ((1, 1), (8, 8)) (row, col) && isJust (board ! (row, col))

withinBounds :: Board -> (Int, Int) -> Bool
withinBounds board (row, col) =
    inRange ((1, 1), (8, 8)) (row, col)

canJump :: Board -> (Int, Int) -> Piece-> Bool
canJump board (row, col) jumpingPiece =
    any canJumpInDirection [(1, 1), (1, -1), (-1, 1), (-1, -1)]
  where
    pieceColor (Just (Piece _ color)) = color
    pieceColor Nothing = error "Empty square has no color"
    canJumpInDirection (rowDir, colDir) =
        let intermediateSquares = takeWhile (inRange ((1, 1), (8, 8)))
                                  [(row + i * rowDir, col + i * colDir) | i <- [1..7]]
            intermediateSquaresBeforeGap = takeWhile (\coord -> isJust (board ! coord)) intermediateSquares
            endSpaceExists = length intermediateSquaresBeforeGap < length intermediateSquares && length intermediateSquaresBeforeGap > 0
            oppositeColors = all (\coord -> pieceColor (board ! coord) /= pieceColor (Just jumpingPiece)) intermediateSquaresBeforeGap
        in trace ("intermediateSquaresBeforeGap: " ++ show intermediateSquaresBeforeGap) $
           trace ("endSpaceExists: " ++ show endSpaceExists) $
           trace ("oppositeColors: " ++ show oppositeColors) $
           endSpaceExists && oppositeColors



jumps :: Board -> (Int, Int) -> (Int, Int) -> Bool
jumps board (srcRow, srcCol) (destRow, destCol) =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
    in abs rowDiff == abs colDiff &&
       abs rowDiff >= 2 && 
       abs colDiff >= 2 &&
       all (\(r, c) -> isJust (board ! (r, c))) intermediateSquares


validMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
validMove board (srcRow, srcCol) (destRow, destCol) =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
    in
        (abs rowDiff == 1 && abs colDiff == 1) ||
        jumps board (srcRow, srcCol) (destRow, destCol)


validMoveSequence :: Board -> [(Int, Int)] -> Bool -> Bool -> Piece -> Bool
validMoveSequence board coords jumpedAlready starting movingPiece=
    trace ("firstCellHasPiece: " ++ show firstCellHasPiece) firstCellHasPiece &&
    trace ("allCellsWithinBounds: " ++ show allCellsWithinBounds) allCellsWithinBounds &&
    trace ("allTailCellsEmpty: " ++ show allTailCellsEmpty) allTailCellsEmpty &&
    trace ("jumpedAlreadyCondition: " ++ show jumpedAlreadyCondition) jumpedAlreadyCondition &&
    trace ("firstMoveValid: " ++ show firstMoveValid) firstMoveValid &&
    trace ("restMovesValid: " ++ show restMovesValid) restMovesValid
  where
    firstCellHasPiece = isJust (board ! head coords) || not starting
    allCellsWithinBounds = all (withinBounds board) coords
    allTailCellsEmpty = all (\coord -> isNothing (board ! coord)) (tail coords)
    canJumpResult = canJump board (head coords) movingPiece
    jumpedAlreadyCondition = 
        trace ("canJump result: " ++ show canJumpResult) $
        jumpedAlready || 
        (
            not jumpedAlready && 
            (
                not canJumpResult || 
                jumps board (head coords) (head (tail coords))
            )
        )
    firstMoveValid = validMove board (head coords) (head (tail coords))
    restMovesValid = length coords == 2 || validMoveSequence board (tail coords) (jumps board (head coords) (head (tail coords))) False movingPiece



performMove :: Board -> [(Int, Int)] -> Board
performMove board coords = --makeMove board (head coords) (last coords)
    if length coords == 2
        then makeMove board (head coords) (last coords)
        else performMove (makeMove board (head coords) (head (tail coords))) (tail coords)

playerTurn :: Board -> IO Board
playerTurn board = do
    putStrLn "Enter your move (e.g., A4 C6 A8):"
    input <- getLine
    let moves = words input
    let coords = map parseCoords moves
    print coords
    print board
    let validMove = validMoveSequence board coords False True (fromJust (board ! head coords))
    if not validMove
        then do
            putStrLn "Invalid move. Make sure all coordinates are within bounds and have pieces."
            playerTurn board
        else do
            return $ performMove board coords



parseCoords :: String -> (Int, Int)
parseCoords (col:row) = (rowNum, colNum)
    where
        colNum = ord col - ord 'A' + 1
        rowNum = read row

aiTurn :: Board -> Board
aiTurn board = board -- Placeholder for AI logic

main :: IO ()
main = do
  putStrLn "Initial Board State:"
  putStrLn (showBoard initializeBoard)
  loop initializeBoard
  where
    loop board = do
      playerBoard <- playerTurn board
      let updatedBoard = aiTurn playerBoard
      putStrLn (showBoard updatedBoard)
      loop updatedBoard
