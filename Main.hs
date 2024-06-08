import Data.Array
import Data.Char (ord)
import Data.List
import Control.Monad (when)
import Data.Maybe
import Debug.Trace

---------------------------------------------------------------------------------------------------------
-- Types

data PieceType = Stone | Dame deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)
data Piece = Piece PieceType Color deriving (Eq, Show)


type Board = Array (Int, Int) (Maybe Piece)

data GameState = GameState {
    board :: Board,
    currPlayer :: Color,
    gameScore :: Int
}

---------------------------------------------------------------------------------------------------------
-- Initialization

initializeBoard :: Board
initializeBoard = array ((1,1), (8,8)) [((i, j), initialPiece i j) | i <- [1..8], j <- [1..8]]
  where
    initialPiece i j
      | i <= 3 && even (i + j) = Just (Piece Stone Black)
      | i >= 6 && even (i + j) = Just (Piece Stone White)
      | otherwise = Nothing

initialGameState :: GameState
initialGameState = GameState {
    board = initializeBoard,
    currPlayer = White,
    gameScore = 0
}

---------------------------------------------------------------------------------------------------------
-- Show methods

showColor :: Color -> String
showColor Black = "Black"
showColor White = "White"

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

showGameState :: GameState -> String
showGameState gameState =
    showBoard (board gameState) ++ "\n" ++
    "Current player: " ++ showColor (currPlayer gameState) ++ "\n" ++
    "Game score (with respect to White): " ++ show (gameScore gameState) ++ "\n"

---------------------------------------------------------------------------------------------------------
-- Methods

pieceValue :: Piece -> Int
pieceValue (Piece Stone _) = 1
pieceValue (Piece Dame _) = 2

pieceColor :: Piece -> Color
pieceColor (Piece _ color) = color

isStone :: Piece -> Bool
isStone (Piece Stone _) = True
isStone _ = False

isDame :: Piece -> Bool
isDame (Piece Dame _) = True
isDame _ = False

isBlack :: Piece -> Bool
isBlack (Piece _ Black) = True
isBlack _ = False

isWhite :: Piece -> Bool
isWhite (Piece _ White) = True
isWhite _ = False







evaluateBoard :: Board -> Color -> Int
-- if color is black, return number of black pieces - number of white stones plus number of white dames * 2 - the same for black, else opposite
evaluateBoard board color = blackPieces - whitePieces - 2 * whiteDames + 2 * blackDames
    where
        blackPieces = length $ filter (\(coord, piece) -> isJust piece && pieceColor (fromJust piece) == Black && isStone (fromJust piece)) (assocs board)
        whitePieces = length $ filter (\(coord, piece) -> isJust piece && pieceColor (fromJust piece) == White && isStone (fromJust piece)) (assocs board)
        whiteDames = length $ filter (\(coord, piece) -> isJust piece && pieceColor (fromJust piece) == White && isDame (fromJust piece)) (assocs board)
        blackDames = length $ filter (\(coord, piece) -> isJust piece && pieceColor (fromJust piece) == Black && isDame (fromJust piece)) (assocs board)


removeStones :: GameState -> [(Int, Int)] -> GameState
removeStones gameState coords = gameState { board = board gameState // [(coord, Nothing) | coord <- coords], gameScore = newScore }
    where
        -- newScore is the difference between the values of white and black pieces
        removedPiecesValue = sum (map (\coord -> pieceValue (fromJust (board gameState ! coord))) coords)
        newScore = if currPlayer gameState == White then gameScore gameState + removedPiecesValue else gameScore gameState - removedPiecesValue

makeMove :: GameState -> (Int, Int) -> (Int, Int) -> GameState
makeMove gameState (srcRow, srcCol) (destRow, destCol) =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
        updatedGameState = removeStones gameState intermediateSquares
    in updatedGameState { board = board updatedGameState // [((destRow, destCol), board gameState ! (srcRow, srcCol)), ((srcRow, srcCol), Nothing)] }


withinBoundsAndHasPiece :: Board -> (Int, Int) -> Bool
withinBoundsAndHasPiece board (row, col) =
    inRange ((1, 1), (8, 8)) (row, col) && isJust (board ! (row, col))

anyPieceCanJump :: Board -> Color -> Bool
anyPieceCanJump board color = any (\(coord, piece) -> isJust piece && pieceColor (fromJust piece) == color && canJump board coord (fromJust piece)) (assocs board)

canJump :: Board -> (Int, Int) -> Piece-> Bool
canJump board (row, col) jumpingPiece
  | isStone jumpingPiece && isBlack jumpingPiece = any canJumpInDirection [(1, 1), (1, -1)]
  | isStone jumpingPiece && isWhite jumpingPiece = any canJumpInDirection [(-1, 1), (-1, -1)]
  | isDame jumpingPiece = any canJumpInDirection [(1, 1), (1, -1), (-1, 1), (-1, -1)]
  | otherwise = False
  where
      pieceColor (Just (Piece _ color)) = color
      pieceColor Nothing = error "Empty square has no color"
      canJumpInDirection (rowDir, colDir)
        = let
            intermediateSquares
              = takeWhile
                  (inRange ((1, 1), (8, 8)))
                  [(row + i * rowDir, col + i * colDir) | i <- [1 .. 7]]
            intermediateSquaresBeforeGap
              = takeWhile (\ coord -> isJust (board ! coord)) intermediateSquares
            endSpaceExists
              = length intermediateSquaresBeforeGap < length intermediateSquares
                  && length intermediateSquaresBeforeGap > 0
            oppositeColors
              = all
                  (\ coord
                     -> pieceColor (board ! coord) /= pieceColor (Just jumpingPiece))
                  intermediateSquaresBeforeGap
          in
            trace
              ("intermediateSquaresBeforeGap: "
                 ++ show intermediateSquaresBeforeGap)
              $ trace ("endSpaceExists: " ++ show endSpaceExists)
                  $ trace ("oppositeColors: " ++ show oppositeColors)
                      $ endSpaceExists && oppositeColors



jumps :: Board -> (Int, Int) -> (Int, Int) -> Piece -> Bool
jumps board (srcRow, srcCol) (destRow, destCol) piece =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
    in abs rowDiff == abs colDiff &&
       abs rowDiff >= 2 &&
       abs colDiff >= 2 &&
       ((isStone piece && all (\(r, c) -> isJust (board ! (r, c))) intermediateSquares) || isDame piece && any (\(r, c) -> isJust (board ! (r, c))) intermediateSquares)


performMove :: GameState -> [(Int, Int)] -> GameState
performMove gameState coords =
    if length coords == 2
        then makeMove gameState (head coords) (last coords)
        else performMove (makeMove gameState (head coords) (head (tail coords))) (tail coords)


parseCoords :: String -> (Int, Int)
parseCoords (col:row) = (rowNum, colNum)
    where
        colNum = ord col - ord 'A' + 1
        rowNum = read row

aiTurn :: Board -> IO Board
aiTurn board = return board -- Placeholder for AI logic


---------------------------------------------------------------------------------------------------------
-- New GameState methods

withinBounds :: (Int, Int) -> Bool
withinBounds (row, col) = inRange ((1, 1), (8, 8)) (row, col)




validMove :: GameState -> (Int, Int) -> (Int, Int) -> Piece -> Bool
validMove gameState (srcRow, srcCol) (destRow, destCol) piece =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        pieceIsStone = isStone piece
        pieceIsDame = isDame piece
        destEmpty = isNothing (board gameState ! (destRow, destCol))
        noSkip = abs rowDiff == 1 && abs colDiff == 1
        jumped = jumps (board gameState) (srcRow, srcCol) (destRow, destCol) piece
        movedForward = isBlack piece && rowDiff > 0 || isWhite piece && rowDiff < 0

        srcWithinBounds = withinBounds (srcRow, srcCol)
        destWithinBounds = withinBounds (destRow, destCol)

        validStoneMove = (pieceIsStone && (noSkip || jumped) && movedForward)
        validDameMove = (pieceIsDame && abs rowDiff == abs colDiff)
    in
        srcWithinBounds &&
        destWithinBounds &&
        destEmpty &&
        (validStoneMove || validDameMove)

validMoveSequenceAfterJump :: GameState -> [(Int, Int)] -> Piece -> Bool
validMoveSequenceAfterJump gameState coords piece =
    jumpsIfPossible &&
    firstMoveValid &&
    restMovesValid
    where
        pieceCanJump = canJump (board gameState) (head coords) piece
        jumpsIfPossible = not pieceCanJump || jumps (board gameState) (head coords) (head (tail coords)) piece
        firstMoveValid = validMove gameState (head coords) (head (tail coords)) piece
        restMovesValid = length coords == 2 || validMoveSequenceAfterJump gameState (tail coords) piece

validMoveSequence :: GameState ->[(Int, Int)] -> Bool
validMoveSequence gameState coords =
    trace ("First cell correct color: " ++ show firstCellCorrectColor) firstCellCorrectColor &&
    trace ("Jumps if it can: " ++ show jumpsIfPossible) jumpsIfPossible &&
    trace ("Ends if no jump: " ++ show endsIfNoJump) endsIfNoJump &&
    trace ("First move valid: " ++ show firstMoveValid) firstMoveValid &&
    trace ("Rest moves valid: " ++ show restMovesValid) restMovesValid
    where
        pieceExists = isJust (board gameState ! head coords)
        movingPiece = fromJust (board gameState ! head coords)
        aJumpPossible = anyPieceCanJump (board gameState) (currPlayer gameState)
        firstMoveJumps = jumps (board gameState) (head coords) (head (tail coords)) (fromJust (board gameState ! head coords))

        firstCellCorrectColor = pieceExists && pieceColor movingPiece == currPlayer gameState
        jumpsIfPossible = firstMoveJumps || not aJumpPossible
        endsIfNoJump = firstMoveJumps || length coords == 2
        firstMoveValid = validMove gameState (head coords) (head (tail coords)) movingPiece
        restMovesValid = length coords == 2 || validMoveSequenceAfterJump gameState (tail coords) movingPiece


moveInputCorrectFormat :: String -> Bool
moveInputCorrectFormat [col, row] = 'A' <= col && col <= 'H' && '1' <= row && row <= '8'
moveInputCorrectFormat _ = False

playerTurn :: GameState -> IO GameState
playerTurn gameState = do
    putStrLn "Enter your move (e.g., A4 C6 A8):"
    input <- getLine
    let moves = words input
    if not (all moveInputCorrectFormat moves)
        then do
            putStrLn "Invalid move format. Please enter moves in the format 'A4 C6 A8'."
            playerTurn gameState
        else do
            let coords = map parseCoords moves
            --print coords
            --print (board gameState)
            let validMove = validMoveSequence gameState coords
            if not validMove
                then do
                    putStrLn "Invalid move. Please try again."
                    playerTurn gameState
                else do
                    return $ performMove gameState coords -- gameState { board = performMove (board gameState) coords }


createDames :: GameState -> GameState
createDames gameState = gameState { board = newBoard, gameScore = newScore }
    where
        piecesToPromote = filter (\(coord, piece) -> isJust piece && isStone (fromJust piece) && (fst coord == 8 && isBlack (fromJust piece) || fst coord == 1 && isWhite (fromJust piece))) (assocs (board gameState))
        valueChange = sum (map (\(coord, piece) -> pieceValue (fromJust piece)) piecesToPromote)
        newBoard = board gameState // [(coord, Just (Piece Dame (pieceColor (fromJust piece)))) | (coord, piece) <- piecesToPromote]
        newScore = gameScore gameState + valueChange


multiPlayerTurn :: GameState -> IO GameState
multiPlayerTurn gameState = do
    newState <- playerTurn gameState
    -- change player
    return newState { currPlayer = if currPlayer gameState == Black then White else Black }

singlePlayer :: IO ()
singlePlayer = do
    putStrLn "Initial Board State:"
    putStrLn (showGameState initialGameState)
    loop initialGameState
    where
        loop gameState = do
            let currentBoard = board gameState
            let currentPlayer = currPlayer gameState
            let afterPlayerTurnState = if currentPlayer == White then playerTurn gameState else return gameState -- TODO: Implement AI turn
            updatedState <- afterPlayerTurnState
            let damesState = createDames updatedState
            putStrLn (showGameState damesState)
            loop damesState

multiPlayer :: IO ()
multiPlayer = do
    putStrLn "Initial Board State:"
    putStrLn (showGameState initialGameState)
    loop initialGameState
    where
        loop gameState = do
            let currentBoard = board gameState
            let currentPlayer = currPlayer gameState
            updatedState <- multiPlayerTurn gameState
            let damesState = createDames updatedState
            putStrLn (showGameState damesState)
            loop damesState

main :: IO ()
main = do
    putStrLn "Welcome to Dames!"
    putStrLn "Enter number of players (1 or 2)"
    numPlayers <- getLine
    when (numPlayers /= "1" && numPlayers /= "2") $ do
        putStrLn "Invalid number of players. Please enter 1 or 2."
        main

    if numPlayers == "1"
        then do
            putStrLn "You are playing against the AI."
            singlePlayer
        else do
            putStrLn "You are playing against another player."
            multiPlayer

