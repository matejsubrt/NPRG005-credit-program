import Data.Array
import Data.Char (ord, toUpper)
import Data.List
import Control.Monad (when)
import Data.Maybe
import Debug.Trace
import Data.Ord (comparing)

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

-- dummyBoard :: Board
-- -- has a white piece at (6, 2) and black pieces at (5, 3) and (4, 4)
-- dummyBoard = array ((1,1), (8,8)) [((i, j), initialPiece i j) | i <- [1..8], j <- [1..8]]
--   where
--     initialPiece i j
--       | i == 6 && j == 2 = Just (Piece Stone White)
--       | i == 5 && j == 3 = Just (Piece Stone Black)
--       | i == 4 && j == 4 = Just (Piece Stone Black)
--       | otherwise = Nothing

-- dummyGameState :: GameState
-- dummyGameState = GameState {
--     board = dummyBoard,
--     currPlayer = White,
--     gameScore = 0
-- }

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
        boxLine i = " " ++ 
            show i ++
            " ┃" ++
            intercalate "│" [padCell (showPiece (board ! (i, j))) | j <- [1..8]] ++
            "┃"
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

pieceType :: Piece -> PieceType
pieceType (Piece pieceType _) = pieceType


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


getPieceTypeCount :: Board -> Color -> PieceType -> Int
getPieceTypeCount board color pieceType = length $ filter (
    \(coord, piece) -> piece == Just (Piece pieceType color))
    (assocs board)




evaluateBoard :: Board -> Int
evaluateBoard board = whitePieces + 2* whiteDames - blackPieces - 2* blackDames
    where
        blackPieces = getPieceTypeCount board Black Stone
        whitePieces = getPieceTypeCount board White Stone
        whiteDames = getPieceTypeCount board White Dame
        blackDames = getPieceTypeCount board Black Dame

---------------------------------------------------------------------------------------------------------
-- Move methods

removeStones :: GameState -> [(Int, Int)] -> GameState
removeStones gameState coords = 
    gameState { board = board gameState // [(coord, Nothing) | coord <- coords], gameScore = newScore }
    where
        -- newScore is the difference between the values of white and black pieces
        removedPiecesValue = sum (map (\coord -> maybe 0 pieceValue (board gameState ! coord)) coords)
        newScore = if currPlayer gameState == White 
            then gameScore gameState + removedPiecesValue 
            else gameScore gameState - removedPiecesValue

makeMove :: GameState -> (Int, Int) -> (Int, Int) -> GameState
makeMove gameState (srcRow, srcCol) (destRow, destCol) =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
        updatedGameState = removeStones gameState intermediateSquares
    in updatedGameState { 
        board = board updatedGameState // 
            [((destRow, destCol), board gameState ! (srcRow, srcCol)), ((srcRow, srcCol), Nothing)] }


canJump :: Board -> (Int, Int) -> Piece-> Bool
canJump board (row, col) jumpingPiece
  | isStone jumpingPiece && isBlack jumpingPiece = any (canJumpInDirection True) [(1, 1), (1, -1)]
  | isStone jumpingPiece && isWhite jumpingPiece = any (canJumpInDirection True) [(-1, 1), (-1, -1)]
  | isDame jumpingPiece = any (canJumpInDirection False) [(1, 1), (1, -1), (-1, 1), (-1, -1)]
  | otherwise = False
  where
      pieceColor (Just (Piece _ color)) = color
      pieceColor Nothing = error "Empty square has no color"
      canJumpInDirection isStone (rowDir, colDir)
        = let
            intermediateSquares
              = takeWhile
                  (inRange ((1, 1), (8, 8)))
                  [(row + i * rowDir, col + i * colDir) | i <- [1 .. 7]]
            intermediateSquaresBeforeGap
              = if isStone 
                then takeWhile (\ coord -> isJust (board ! coord)) intermediateSquares
                else 
                    let emptyBefore = takeWhile (\ coord -> isNothing (board ! coord)) intermediateSquares
                        fullBeforeGap = takeWhile (\ coord -> isJust (board ! coord)) (drop (length emptyBefore) intermediateSquares)
                    in emptyBefore ++ fullBeforeGap
            endSpaceExists
              = length intermediateSquaresBeforeGap < length intermediateSquares
                  && length intermediateSquaresBeforeGap > 0
            oppositeColors
              = all
                  (\ coord
                     -> pieceColor (board ! coord) /= pieceColor (Just jumpingPiece))
                  intermediateSquaresBeforeGap
        in endSpaceExists && oppositeColors


anyDameCanJump :: Board -> Color -> Bool
anyDameCanJump board color = 
    or [pieceColor piece == color && pieceType piece == Dame && canJump board coord piece |
        (coord, Just piece) <- assocs board]

anyPieceCanJump :: Board -> Color -> Bool
anyPieceCanJump board color = 
    or [ pieceColor piece == color && canJump board coord piece |
         (coord, Just piece) <- assocs board ]
    -- any (
    --     \(coord, piece) -> isJust piece && 
    --     pieceColor (fromJust piece) == color && 
    --     canJump board coord (fromJust piece)) (assocs board)

jumps :: Board -> (Int, Int) -> (Int, Int) -> Piece -> Bool
jumps board (srcRow, srcCol) (destRow, destCol) piece =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
        allIntermediateSquaresFull = all (\(row, col) -> isJust (board ! (row, col))) intermediateSquares
        anyIntermediateSquareFull = any (\(row, col) -> isJust (board ! (row, col))) intermediateSquares
        allIntermediateSquaresEmptyOrOppositeColor = 
            all (\(row, col) -> isNothing (board ! (row, col)) || 
                pieceColor (fromJust (board ! (row, col))) /= pieceColor piece) intermediateSquares
    in abs rowDiff == abs colDiff &&
        abs rowDiff >= 2 &&
        abs colDiff >= 2 &&
        ((isStone piece && allIntermediateSquaresFull) || isDame piece && anyIntermediateSquareFull) &&
        allIntermediateSquaresEmptyOrOppositeColor


performMove :: GameState -> [(Int, Int)] -> GameState
performMove gameState [] = error "No moves to perform"
performMove gameState [coord] = error "Move has to consist of at least 2 coordinates"
performMove gameState [coord1, coord2] = makeMove gameState coord1 coord2
performMove gameState (coord1 : coord2 : rest) =
    performMove (makeMove gameState coord1 coord2) (coord2 : rest)
-- performMove gameState coords =
--     if length coords == 2
--         then makeMove gameState (head coords) (last coords)
--         else performMove (makeMove gameState (head coords) (head (tail coords))) (tail coords)




---------------------------------------------------------------------------------------------------------
-- Helper functions


parseCoords :: String -> (Int, Int)
parseCoords (col:row) = (rowNum, colNum)
    where
        colNum = ord (toUpper col) - ord 'A' + 1
        rowNum = read row

getJumpedPieces :: GameState -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getJumpedPieces gameState (srcRow, srcCol) (destRow, destCol) =
    let rowDiff = destRow - srcRow
        colDiff = destCol - srcCol
        steps = abs rowDiff
        rowStep = rowDiff `div` steps
        colStep = colDiff `div` steps
        intermediateSquares = [(srcRow + i * rowStep, srcCol + i * colStep) | i <- [1..(steps - 1)]]
        intermediateSquaresFull = filter 
            (\(row, col) -> isJust (board gameState ! (row, col))) intermediateSquares
    in intermediateSquaresFull


allFirstMovesFromCoords :: GameState -> Piece -> (Int, Int) -> [(Int, Int)]
allFirstMovesFromCoords gameState (Piece Stone Black) (row, col) =
    let moves = [(x, x) | x <- [1..8]] ++ [(x, -x) | x <- [1..8]]
    in filter (validMove gameState (Piece Stone Black) (row, col)) [(row + i, col + j) | (i, j) <- moves]
allFirstMovesFromCoords gameState (Piece Stone White) (row, col) =
    let moves = [(-x, x) | x <- [1..8]] ++ [(-x, -x) | x <- [1..8]]
    in filter (validMove gameState (Piece Stone White) (row, col) ) [(row + i, col + j) | (i, j) <- moves]
allFirstMovesFromCoords gameState (Piece Dame color) (row, col) =
    let moves = [(x, x)  | x <- [1..8]] ++ 
                [(x, -x) | x <- [1..8]] ++ 
                [(-x, x) | x <- [1..8]] ++ 
                [(-x, -x) | x <- [1..8]]
    in filter (validMove gameState (Piece Dame color) (row, col)) [(row + i, col + j) | (i, j) <- moves]

allJumpSeqsFromCoords :: GameState -> Piece -> (Int, Int) -> Int -> [[(Int, Int)]]
allJumpSeqsFromCoords gameState piece src depth =
    if depth == 0 then []
    else
        let firstMoves = allFirstMovesFromCoords gameState piece src
            jumpMoves = filter (\move -> jumps (board gameState) src move piece) firstMoves
            jumpSeqs = concatMap 
                (\move -> map (move :) (allJumpSeqsFromCoords gameState piece move (depth - 1))) jumpMoves
        in if null jumpSeqs
            then [[]]
            else jumpSeqs



-- Function to get all move sequences from the given coordinates up to a certain depth
allMoveSeqsFromCoords :: GameState -> Piece -> (Int, Int) -> Int -> [[(Int, Int)]]
allMoveSeqsFromCoords gameState piece src depth =
    if depth == 0 then []
    else
        let firstMoves = allFirstMovesFromCoords gameState piece src
            nonJumpMoves = filter (\move -> not (jumps (board gameState) src move piece)) firstMoves
            jumpMoves = filter (\move -> jumps (board gameState) src move piece ) firstMoves
            nonJumpSeqs = map (src :) [[move] | move <- nonJumpMoves]
            jumpSeqs = map 
                (src :) 
                (concatMap 
                    (\move -> map 
                        (move :) 
                        (allJumpSeqsFromCoords gameState piece move (depth - 1))) jumpMoves)
        in if null jumpSeqs
            then nonJumpSeqs
            else jumpSeqs


-- Top-level function to get all move sequences from the given coordinates (depth limited to 4)
allMoveSeqsFromCoords4 :: GameState -> Piece -> (Int, Int) -> [[(Int, Int)]]
allMoveSeqsFromCoords4 gameState piece src = allMoveSeqsFromCoords gameState piece src 4


getPiecesCount :: GameState -> Piece -> Int
getPiecesCount gameState pieceKind = length $ filter (
    \(coord, piece) -> piece == Just (Piece (pieceType pieceKind) (pieceColor pieceKind)))
    (assocs (board gameState))

            -- isJust piece && 
            -- pieceColor (fromJust piece) == pieceColor pieceKind && 
            -- pieceType (fromJust piece) == pieceType pieceKind) (assocs (board gameState))




---------------------------------------------------------------------------------------------------------
-- Validation functions

withinBounds :: (Int, Int) -> Bool
withinBounds (row, col) = inRange ((1, 1), (8, 8)) (row, col)

validMove :: GameState -> Piece -> (Int, Int) -> (Int, Int) -> Bool
validMove gameState piece (srcRow, srcCol) (destRow, destCol) =
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



validMoveSequenceAfterJump :: GameState -> [(Int, Int)] -> [(Int, Int)] -> Piece -> Bool
validMoveSequenceAfterJump gameState coords@(start : rest) alreadyJumpedPieces piece =
    jumpsIfPossible &&
    firstMoveValid &&
    restMovesValid &&
    noPieceJumpedAgain
    where
        pieceCanJump = canJump (board gameState) start piece
        jumpsIfPossible = not pieceCanJump || 
            jumps (board gameState) start (head rest) piece
        jumpedPieces = getJumpedPieces gameState start (head rest)
        firstMoveValid = validMove gameState piece start (head rest)
        noPieceJumpedAgain = null (jumpedPieces `intersect` alreadyJumpedPieces)
        restMovesValid = length coords == 2 || 
            validMoveSequenceAfterJump gameState rest (alreadyJumpedPieces ++ jumpedPieces) piece

validMoveSequence :: GameState ->[(Int, Int)] -> Bool
validMoveSequence gameState coords@(start : rest) =
    firstCellCorrectColor &&
    jumpsIfPossible &&
    dameJumpsIfPossible &&
    endsIfNoJump &&
    firstMoveValid &&
    restMovesValid
    where
        pieceExists = isJust (board gameState ! start)
        movingPiece = fromJust (board gameState ! start)
        aJumpPossible = anyPieceCanJump (board gameState) (currPlayer gameState)
        aDameJumpPossible = anyDameCanJump (board gameState) (currPlayer gameState)
        jumpingPiece = fromJust (board gameState ! start)
        firstMoveJumps = jumps 
            (board gameState) 
            start
            (head rest) 
            jumpingPiece

        jumpedPieces = getJumpedPieces gameState start (head rest)

        firstCellCorrectColor = pieceExists && pieceColor movingPiece == currPlayer gameState
        dameJumpsIfPossible = (firstMoveJumps && pieceType jumpingPiece == Dame) || not aDameJumpPossible
        jumpsIfPossible = firstMoveJumps || not aJumpPossible
        endsIfNoJump = firstMoveJumps || length coords == 2
        firstMoveValid = validMove gameState movingPiece start (head rest)
        restMovesValid = length coords == 2 || 
            validMoveSequenceAfterJump gameState rest jumpedPieces movingPiece


moveInputCorrectFormat :: String -> Bool
moveInputCorrectFormat [col, row] = (('A' <= col && col <= 'H') || ('a' <= col && col <= 'h')) && '1' <= row && row <= '8'
moveInputCorrectFormat _ = False


---------------------------------------------------------------------------------------------------------
-- Turn functions generic

createDames :: GameState -> GameState
createDames gameState = gameState { board = newBoard, gameScore = newScore }
    where
        promote ((8, _), Just (Piece Stone Black)) = True
        promote ((1, _), Just (Piece Stone White)) = True
        promote _ = False
        piecesToPromote = filter promote (assocs (board gameState))
        -- piecesToPromote = filter 
        --     (\(coord, piece) -> 
        --         isJust piece && 
        --         isStone (fromJust piece) && 
        --             (fst coord == 8 && 
        --             isBlack (fromJust piece) || 
        --             fst coord == 1 && isWhite (fromJust piece))) (assocs (board gameState))
        --valueChange = sum (map (\(coord, piece) -> pieceValue (fromJust piece)) piecesToPromote)
        valueChange = sum (map (\(coord, Just piece) -> pieceValue piece) piecesToPromote)
        newBoard = board gameState // 
            [(coord, Just (Piece Dame (pieceColor piece))) | (coord, Just piece) <- piecesToPromote]
        newScore = gameScore gameState + valueChange

switchPlayers :: GameState -> GameState
switchPlayers gameState = gameState { currPlayer = if currPlayer gameState == Black then White else Black }




checkEnd :: GameState -> IO Bool
checkEnd gameState = do
    let blackPieces = getPiecesCount gameState (Piece Stone Black)
    let whitePieces = getPiecesCount gameState (Piece Stone White)
        --length $ filter (\(coord, piece) -> 
    --         isJust piece && 
    --         pieceColor (fromJust piece) == White && 
    --         isStone (fromJust piece)) (assocs (board gameState))
    let whiteDames = getPiecesCount gameState (Piece Dame White)
    let blackDames = getPiecesCount gameState (Piece Dame Black)
    if blackPieces == 0
        then do
            putStrLn "White wins! Black has no pieces left."
            return True
    else if whitePieces == 0
        then do
            putStrLn "Black wins! White has no pieces left."
            return True
    else do
        let allPossibleMoves = concatMap (\(coord, piece) ->
                if isJust piece && pieceColor (fromJust piece) == currPlayer gameState
                then allMoveSeqsFromCoords4 gameState (fromJust piece) coord
                else []) (assocs (board gameState))
        if null allPossibleMoves
            then do
                if currPlayer gameState == Black
                    then putStrLn "White wins! Black has all pieces blocked."
                    else putStrLn "Black wins! White has all pieces blocked."
                return True
            else return False


---------------------------------------------------------------------------------------------------------
-- Turn functions multiplayer

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
            let validMove = validMoveSequence gameState coords
            if not validMove
                then do
                    putStrLn "Invalid move. Please try again."
                    playerTurn gameState
                else do
                    return $ performMove gameState coords


multiPlayerTurn :: GameState -> IO GameState
multiPlayerTurn gameState = do
    newState <- playerTurn gameState
    return newState { currPlayer = if currPlayer gameState == Black then White else Black }


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
            gameEnded <- checkEnd damesState
            if gameEnded
                then return ()
                else loop damesState


---------------------------------------------------------------------------------------------------------
-- Turn functions singleplayer -> AI

aiMinimax :: GameState -> Bool -> Int -> ([[(Int, Int)]], Int)
aiMinimax gameState maximizing depth =
    if depth == 0
        then ([], evaluateBoard (board gameState))
    else
        let turnColor = currPlayer gameState
            allMoves = concatMap (\(coord, piece) ->
                if isJust piece && pieceColor (fromJust piece) == turnColor
                then allMoveSeqsFromCoords4 gameState (fromJust piece) coord
                else []) (assocs (board gameState))
            jumpingMoves = filter (\move -> 
                jumps 
                    (board gameState) 
                    (head move) 
                    (head (tail move)) 
                    (fromJust (board gameState ! head move))) allMoves
            allMovesFinal = if not (null jumpingMoves) then jumpingMoves else allMoves
            results = map (\move ->
                let (seqs, value) = aiMinimax 
                        (switchPlayers (createDames (performMove gameState move))) 
                        (not maximizing) 
                        (depth - 1)
                in (move : seqs, value)) allMovesFinal
            bestMove = (if maximizing then maximumBy else minimumBy) (comparing snd) results-- if maximizing
                       -- then maximumBy (\(_, value1) (_, value2) -> value1 `compare` value2) results
                       -- else minimumBy (\(_, value1) (_, value2) -> value1 `compare` value2) results
        in bestMove


aiTurn :: GameState -> Int -> IO GameState
aiTurn gameState difficulty = do
    -- let minimaxDepth
    --       | difficulty == "1" = 1
    --       | difficulty == "2" = 3
    --       | otherwise = 5
    let minimaxDepth = 2 * difficulty - 1
    (bestMove, _) <- return $ aiMinimax gameState False minimaxDepth
    let newState = performMove gameState (head bestMove)
    return newState { currPlayer = if currPlayer gameState == Black then White else Black }




singlePlayerTurn :: GameState -> Int -> IO GameState
singlePlayerTurn gameState difficulty = do
    newState <- if currPlayer gameState == White 
        then playerTurn gameState 
        else aiTurn gameState difficulty
    return newState { currPlayer = if currPlayer gameState == Black then White else Black }



singlePlayer :: IO ()
singlePlayer = do
    putStrLn "Choose the AI difficulty (1-3):"
    difficulty <- getLine
    when (difficulty /= "1" && difficulty /= "2" && difficulty /= "3") $ do
        putStrLn "Invalid difficulty. Please enter 1, 2 or 3."
        singlePlayer
    putStrLn "Initial Board State:"
    putStrLn (showGameState initialGameState)
    -- convert difficulty to int
    let difficultyInt = read difficulty :: Int
    loop initialGameState difficultyInt
    where
        loop gameState difficulty = do
            let currentBoard = board gameState
            let currentPlayer = currPlayer gameState
            let afterPlayerTurnState = singlePlayerTurn gameState difficulty
            updatedState <- afterPlayerTurnState
            let damesState = createDames updatedState
            putStrLn (showGameState damesState)
            gameEnded <- checkEnd damesState
            if gameEnded
                then return ()
                else loop damesState difficulty

---------------------------------------------------------------------------------------------------------
-- Main

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