module Moves where

import Checkers.Types

moves :: GameState -> ([Move], [Move])
-- Returns a tuple where the first is the list of possible simple moves in the given gamestate
-- and the second is the list of possible jump moves in the given gamestate
moves gs = (simple_moves gs, jump_moves gs)

simple_moves:: GameState -> [Move]
-- Takes a GameState as input, and calculates all possible simple moves
-- A list of Moves is returned
simple_moves gs
        | status gs == Turn Red 
        = (simpleKing (redKings gs)) ++ (simplePiece (redPieces gs))
        | status gs == Turn Black 
        = (simpleKing (blackKings gs)) ++ (simplePiece (blackPieces gs))
        | otherwise = []
    where
        simplePiece:: PieceState -> [Move]
        -- Takes as input a piecestate of pawns and calculates a list of all possible pawn moves
        -- in the piecestate.
        simplePiece xs = [[P (x,y), pawnOrKing (status gs) (x',y')] | (x,y) <- xs,
                         (x',y') <- let y' = y+(dir (status gs))  in [(x+1,y'),(x-1, y')]
                         , notOccupied (x',y') gs && onBoard (x',y')]
        simpleKing:: PieceState -> [Move]
        -- Takes as input a piecestate of kings and calculates a list of all possible king moves
        -- in the piecestate, removing any possible moves that would result in a repetition of
        -- the gamestate
        simpleKing xs = checkForRepetition (history gs)
                        [[K (x,y), K (x',y')] | (x,y) <- xs,
                        (x',y') <- [(x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]
                        , notOccupied (x',y') gs && onBoard (x',y')]
        
jump_moves:: GameState -> [Move]
-- Takes a GameState as input, and calculates all possible jump moves
-- A list of Moves is returned
jump_moves gs
    | status gs == Turn Red 
    = (jumpKing (redKings gs) ++ jumpPiece (redPieces gs))
    | status gs == Turn Black
    = (jumpKing (blackKings gs) ++ jumpPiece (blackPieces gs))
    | otherwise = []
    where
        jumpKing:: PieceState -> [Move]
        -- Takes in a piecestate of kings and returns all possible jump moves based on 
        -- the current gamestate
        jumpKing ps = [K (x,y):ys | (x,y) <- ps, ys <- jumpKing' (x,y) [] (x,y)]
        jumpKing':: Coord -> [Coord] -> Coord -> [Move]
        -- Helper function for jumpKing: Helps for multiple jumps
        jumpKing' start rem (x,y) = [K (a,b):ys | ((x',y'),(a,b)) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                                    , notElem (x',y') rem && opponent_occupied (x',y') gs && (notOccupied (a,b) gs || start == (a,b)) && onBoard (a,b)
                                    , ys <- jump_over (jumpKing' start ((x',y'):rem) (a,b))]
        jumpPiece:: PieceState -> [Move]
        -- Takes in a piecestate of pawns and returns all possible jump moves based on
        -- the current gamestate
        jumpPiece ps = [P (x,y):ys | (x,y) <- ps, ys <- jumpPiece' (x,y) [] (x,y)]
        jumpPiece':: Coord -> [Coord] -> Coord -> [Move]
        -- Help function for jumpPiece: Helps for multiple jumps
        jumpPiece' start rem (x,y) = [(pawnOrKing (status gs) (a,b)):ys | ((x',y'),(a,b)) <- redOrBlackPieceMoves gs (x,y)
                                    , notElem (x',y') rem && opponent_occupied (x',y') gs && notOccupied (a,b) gs && onBoard (a,b)
                                    , ys <- jump_over (jumpPieceOrJumpKing gs start ((x',y'):rem) (a,b))]
                                    where
                                        redOrBlackPieceMoves:: GameState -> Coord -> [((Int,Int),(Int,Int))]
                                        -- Takes in a gamestate and Coord, and returns all possible jump moves for a simple piece,
                                        -- depending on if it's red or black
                                        redOrBlackPieceMoves gs (x,y)
                                            | status gs == Turn Red = [((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                                            | status gs == Turn Black = [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]
                                            | otherwise = []
                                        jumpPieceOrJumpKing:: GameState -> Coord -> [Coord] -> Coord -> [Move]
                                        -- Takes in a start Coord of the move, a list of Coords that the move has been to, the current Coord,
                                        -- and calls JumpKing' if the piece is now a king, or JumpPiece' if the piece is still a simple piece
                                        jumpPieceOrJumpKing gs start rem (a,b)
                                            | status gs == Turn Red && b == 0 = jumpKing' start rem (a,b)
                                            | status gs == Turn Black && b == 7 = jumpKing' start rem (a,b)
                                            | otherwise = jumpPiece' start rem (a,b)
        -- Allows the jumpKing' function to exit recursively
        jump_over [] = [[]]           
        jump_over z = z

dir:: Status -> Int
-- Takes the board status as input. Returns 1 if it is red's turn and -1 if it is black's turn
-- If the status is not someone's turn, dir returns 0
dir status
    | status == Turn Red = -1
    | status == Turn Black = 1
    | otherwise = 0

notOccupied:: Coord -> GameState -> Bool
-- Takes as input a Coord and a gamestate. Returns true if the Coord is not occupied
-- and false otherwise
notOccupied (x,y) gs = notBP && notBK && notRP && notRK
    where
        notBP = notElem (x,y) (blackPieces gs)
        notBK = notElem (x,y) (blackKings gs)
        notRP = notElem (x,y) (redPieces gs)
        notRK = notElem (x,y) (redKings gs)

onBoard:: Coord -> Bool
-- Takes a Coord as input and returns true if that Coord is on the board, false otherwise
onBoard (x,y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

pawnOrKing:: Status -> Coord -> (PorK Coord)
-- Determines if a piece is a pawn or a king, based of the status of the gamestate
-- and the location of the piece
pawnOrKing status (x,y)
    | status == Turn Red && y == 0 = K (x,y)
    | status == Turn Black && y == 7 = K (x,y)
    | otherwise = P (x,y)

opponent_occupied:: Coord -> GameState -> Bool
-- Takes a Coord and a gamestate and returns true if there exists a piece on that Coord
-- in which it is not the pieces turn to move, false otherwise
opponent_occupied (x,y) gs
    | status gs == Turn Red && blackOnCoord = True
    | status gs == Turn Black && redOnCoord = True
    | otherwise = False
    where   
        blackOnCoord = elem (x,y) (blackPieces gs) || elem (x,y) (blackKings gs)
        redOnCoord = elem (x,y) (redPieces gs) || elem (x,y) (redKings gs)

checkForRepetition:: [Move] -> [Move] -> [Move]
-- Takes in the history of moves in the gamestate and a list of possible simple king moves,
-- then looks at the history of the gamestate to determine if there is a sequence
-- of simple king moves, then removes any potential simple king moves that would create a repeated state
-- Checks a list of historys which includes all iterations of moves between 1 and n where n is the length
-- of the sequence of simple king moves from most recent to the most previous simple king move
checkForRepetition _ [] = []
checkForRepetition [] xs = xs
checkForRepetition (x:xs) (y:ys)
    | not (isSimpleKingMove x) = (y:ys)
    | otherwise = checkForRepetition (take (length (x:xs) - 1) (x:xs)) (removePossibleRepetitions (relativeKingPositions (reverse (takeWhile isSimpleKingMove (x:xs))) []) (y:ys))

isSimpleKingMove:: Move -> Bool
-- Takes a move as input. Returns true if the move is a simple king move, false otherwise
isSimpleKingMove [] = False
isSimpleKingMove (x:xs)
    | length (x:xs) > 2 = False
    | (kingCoord x) && (kingCoord (head xs)) = True
    | otherwise = False

removePossibleRepetitions:: [Move] -> [Move] -> [Move]
-- Takes a list of king moves that represent current king positions relative to where they started 
-- in the current sequence of simple king moves, as well as a list of possible next moves.
-- Removes any next possible moves that would cause a repetition in the game state
removePossibleRepetitions [] ys = ys
removePossibleRepetitions _ [] = []
removePossibleRepetitions (x:xs) (y:ys)
    | length (x:xs) > 1 = (y:ys)
    | editKingCoord y (x:xs) == [] = ys
    | otherwise = y : removePossibleRepetitions (x:xs) ys

relativeKingPositions:: [Move] -> [Move] -> [Move]
-- Takes a history of moves and returns a list of simple king moves where the first Coord is
-- where the king started in the sequence and the second Coord is where the king currently is.
-- The second input is the current list of moves so that the function can work recursively.
relativeKingPositions [] [] = []
relativeKingPositions [] currMoves = currMoves
relativeKingPositions (x:xs) [] = relativeKingPositions xs [x]
relativeKingPositions (x:xs) currMoves = relativeKingPositions xs (editKingCoord x currMoves)

kingCoord:: PorK Coord -> Bool
-- Takes as input a PorK Coord, and returns True if it's a King coordinate and false otherwise
kingCoord (K (x,y)) = True
kingCoord (P (x,y)) = False

editKingCoord:: Move -> [Move] -> [Move]
-- Takes in a simple king move and a list of king moves which represent the starting location
-- and the current location. If the first Coord in the simple king move is the second Coord
-- of any king locations, then change the current king location to the second Coord in the 
-- simple king move. If the first Coord of the king location is now equal to the second Coord,
-- remove it from the list of king moves
editKingCoord (x:xs) [] = [x:xs]
editKingCoord (x:xs) (y:ys)
    | x == (head (tail y)) && (head xs) == (head y) = ys
    | x == (head (tail y)) = ((head y) : xs) : ys
    | otherwise = y : editKingCoord (x:xs) ys
    