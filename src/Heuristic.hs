module Heuristic where 

import Checkers.Types

black_heuristic:: GameState -> Int
-- Takes in a gamestate and returns and int which corresponds how good black's position is in the current gamestate
-- with different algorithms based on how many total pieces are left in the game
black_heuristic gs
    | numTotalPieces gs > 16 = blackEarlyGameAlg gs
    | numTotalPieces gs > 8 = blackMiddleGameAlg gs
    | otherwise = blackLateGameAlg gs

blackEarlyGameAlg:: GameState -> Int
-- Algorithm for black early game heuristic
blackEarlyGameAlg gs = (((length (blackPieces gs)) - (length (redPieces gs))) + (2*((length (blackKings gs)) - (length (redKings gs)))))

blackMiddleGameAlg:: GameState -> Int 
-- Algorithm for black middle game heuristic
blackMiddleGameAlg gs = (blackEarlyGameAlg gs) + ((evalBlackPawns (blackPieces gs)) - (evalRedPawns (redPieces gs)))

blackLateGameAlg:: GameState -> Int 
-- Algorithm for black late game heuristic
blackLateGameAlg gs
    | (blackMiddleGameAlg gs) > (redMiddleGameAlg gs) = (blackMiddleGameAlg gs) + (closenessOfPieces gs)
    | otherwise = (blackMiddleGameAlg gs) - (closenessOfPieces gs)

red_heuristic:: GameState -> Int
-- Takes in a gamestate and returns and int which corresponds how good red's position is in the current gamestate
-- with different algorithms based on how many total pieces are left in the game
red_heuristic gs
    | numTotalPieces gs > 16 = redEarlyGameAlg gs
    | numTotalPieces gs > 8 = redMiddleGameAlg gs
    | otherwise = redLateGameAlg gs

redEarlyGameAlg:: GameState -> Int
-- Algorithm for red early game heuristic
redEarlyGameAlg gs = (((length (redPieces gs)) - (length (blackPieces gs))) + (2*((length (redKings gs)) - (length (blackKings gs)))))

redMiddleGameAlg:: GameState -> Int
-- Algorithm for red middle game heuristic
redMiddleGameAlg gs = (redEarlyGameAlg gs) + ((evalRedPawns (redPieces gs)) - (evalBlackPawns (blackPieces gs)))

redLateGameAlg:: GameState -> Int
-- Algorithm for red late game heuristic
redLateGameAlg gs
    | (redMiddleGameAlg gs) > (blackMiddleGameAlg gs) = (redMiddleGameAlg gs) + (closenessOfPieces gs)
    | otherwise = (redMiddleGameAlg gs) - (closenessOfPieces gs)

numTotalPieces:: GameState -> Int 
-- Takes in a gamestate, and returns the total number of pieces currently in the game 
numTotalPieces gs = (((length (blackPieces gs)) + (length (blackKings gs))) + ((length (redPieces gs) + (length (redKings gs)))))

evalRedPawns:: PieceState -> Int 
-- Takes in the piecestate of red pawns, and evaluates how good the position is
-- based on how far up the board the pawns are in total
evalRedPawns pawns = foldl (\ acc (x,y) -> acc + (evalRedPawn (x,y)) ) 0 pawns

evalRedPawn:: Coord -> Int 
-- Takes a pawn positon, returns 1 if the pawn is on the opponent side of the board
-- and -1 if on your side of the board
evalRedPawn (x, y)
    | y < 4 = 1 
    | otherwise = (-1)

evalBlackPawns:: PieceState -> Int 
-- Takes in the piecestate of black pawns, and evaluates how good the position is
-- based on how far up the board the pawns are in total
evalBlackPawns pawns = foldl (\ acc (x,y) -> acc + (evalBlackPawn (x,y)) ) 0 pawns

evalBlackPawn:: Coord -> Int 
-- Takes a pawn positon, returns 1 if the pawn is on the opponent side of the board
-- and -1 if on your side of the board
evalBlackPawn (x, y)
    | y > 3 = 1 
    | otherwise = (-1)

closenessOfPieces:: GameState -> Int 
-- Takes a gamestate and returns the number of interactions between pieces of opposite teams,
-- where each place where two pieces are within two spots of each other, add one to the total
closenessOfPieces gs = checkClosenessEachPiece ((redPieces gs) ++ (redKings gs)) ((blackPieces gs) ++ (blackKings gs))

checkClosenessEachPiece:: [Coord] -> [Coord] -> Int 
-- Takes two lists of coords and checks each pair of coords to see if they're close together
-- If they are, add one to the accumulator
checkClosenessEachPiece [] (y:ys) = 0
checkClosenessEachPiece (x:xs) (y:ys) = (compareSingleCoord x (y:ys)) + (checkClosenessEachPiece xs (y:ys))

compareSingleCoord:: Coord -> [Coord] -> Int
compareSingleCoord coord (y:ys) = foldl (+) 0 (map (\ (x,y) -> (if (piecesClose coord (x,y)) then 1 else 0)) (y:ys))


piecesClose:: Coord -> Coord -> Bool 
-- Takes two coord of pieces, returns true if the pieces are within two spots of each other
-- and false otherwise
piecesClose (x,y) (a,b)
    | x == a && y == (b-2) = True 
    | x == a && y == (b+2) = True 
    | y == b && x == (a-2) = True 
    | y == b && x == (a+2) = True 
    | x == (a+2) && y == (b+2) = True 
    | x == (a+2) && y == (b-2) = True 
    | x == (a-2) && y == (b+2) = True 
    | x == (a-2) && y == (b-2) = True
    | otherwise = False 