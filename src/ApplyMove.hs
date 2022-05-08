module ApplyMove where

import Moves
import Checkers.Types

apply_move:: CheckersEngine
-- Takes a move and a gamestate and attempts to apply the move to the gamestate
-- If the move is not an available move, the gamestate message will be changed to
-- "Illegal move!!" and the gamestate will be returned with no move made
apply_move mv gs
    | moves gs == ([],[]) = gs{status = GameOver}
    | elem mv (fst (moves gs)) && (snd (moves gs) /= []) = gs{message = "Illegal move! A jump is available: " ++ show (snd (moves gs))}
    | elem mv (fst (moves gs)) = make_simple_move mv gs 
    | elem mv (snd (moves gs)) = make_jump_move mv gs []
    | otherwise = gs{message = "Illegal move!!"}
    where
        make_simple_move:: Move -> GameState -> GameState
        -- Takes a simple move and a gamestate and applies the move to the gamestate
        -- moving the applied piece to the new square
        make_simple_move [start,end] gs
            | status gs == Turn Red && elem (getCoord start) (redKings gs)
            = gs{redKings = (getCoord end) : filter (\a -> a /= (getCoord start))  (redKings gs)
                , status = Turn Black
                , message = ""
                , history = [start,end] : history gs}
            | status gs == Turn Red && elem (getCoord start) (redPieces gs) && (snd (getCoord end) == 0)
            = gs{redPieces = filter (\a -> a /= (getCoord start)) (redPieces gs)
                , redKings = (getCoord end) : redKings gs
                , status = Turn Black
                , message = ""
                , history = [start,end] : history gs}
            | status gs == Turn Red && elem (getCoord start) (redPieces gs)
            = gs{redPieces = (getCoord end) : filter (\a -> a /= (getCoord start)) (redPieces gs)
                , status = Turn Black
                , message = ""
                , history = [start,end] : history gs}
            | status gs == Turn Black && elem (getCoord start) (blackKings gs)
            = gs{blackKings = (getCoord end) : filter (\a -> a /= (getCoord start))  (blackKings gs)
                , status = Turn Red
                , message = ""
                , history = [start,end] : history gs}
            | status gs == Turn Black && elem (getCoord start) (blackPieces gs) && (snd (getCoord end) == 7)
            = gs{blackPieces = filter (\a -> a /= (getCoord start)) (blackPieces gs)
                , blackKings = (getCoord end) : blackKings gs
                , status = Turn Red
                , message = ""
                , history = [start,end] : history gs}
            | status gs == Turn Black && elem (getCoord start) (blackPieces gs)
            = gs{blackPieces = (getCoord end) : filter (\a -> a /= (getCoord start)) (blackPieces gs)
                , status = Turn Red
                , message = ""
                , history = [start,end] : history gs}  
            | otherwise = gs
        make_jump_move:: Move -> GameState -> Move -> GameState
        -- Takes a jump move and a gamestate and applies the move to the gamestate
        -- moving the applied piece to the new square and removing any pieces 
        -- that have been captured
        make_jump_move [] gs _ = gs
        make_jump_move [start] gs currMove
            | status gs == Turn Red = checkIfGameOver gs{history = (currMove ++ [start]) : history gs} (Turn Black)
            | status gs == Turn Black = checkIfGameOver gs{history = (currMove ++ [start]) : history gs} (Turn Red)
            | otherwise = gs{status = GameOver}
        make_jump_move (start:(next:rest)) gs currMove
            | status gs == Turn Red && elem (getCoord start) (redKings gs)
             = make_jump_move (next:rest)
                (gs{blackKings = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (blackKings gs)
                , blackPieces = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (blackPieces gs)
                , redKings = (getCoord next) : filter (\a -> a /= (getCoord start)) (redKings gs)
                , message = ""}) (currMove ++ [start])
            | status gs == Turn Red && elem (getCoord start) (redPieces gs) && (snd (getCoord next) == 0 )
             = make_jump_move (next:rest)
                (gs{blackKings = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (blackKings gs)
                , blackPieces = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (blackPieces gs)
                , redPieces = filter (\a -> a /= (getCoord start)) (redPieces gs)
                , redKings = (getCoord next) : redKings gs
                , message = ""}) (currMove ++ [start])
            | status gs == Turn Red && elem (getCoord start) (redPieces gs)
             = make_jump_move (next:rest)
                (gs{blackKings = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (blackKings gs)
                , blackPieces = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (blackPieces gs)
                , redPieces = (getCoord next) : filter (\a -> a /= (getCoord start)) (redPieces gs)
                , message = ""}) (currMove ++ [start])
            | status gs == Turn Black && elem (getCoord start) (blackKings gs)
             = make_jump_move (next:rest)
                (gs{redKings = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (redKings gs)
                , redPieces = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (redPieces gs)
                , blackKings = (getCoord next) : filter (\a -> a /= (getCoord start)) (blackKings gs)
                , message = ""}) (currMove ++ [start])
            | status gs == Turn Black && elem (getCoord start) (blackPieces gs) && (snd (getCoord next) == 7 )
             = make_jump_move (next:rest)
                (gs{redKings = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (redKings gs)
                , redPieces = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (redPieces gs)
                , blackPieces = filter (\a -> a /= (getCoord start)) (blackPieces gs)
                , blackKings = (getCoord next) : blackKings gs
                , message = ""}) (currMove ++ [start])
            | status gs == Turn Black && elem (getCoord start) (blackPieces gs)
             = make_jump_move (next:rest)
                (gs{redKings = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (redKings gs)
                , redPieces = filter (\a -> a /= (jumped (getCoord start) (getCoord next))) (redPieces gs)
                , blackPieces = (getCoord next) : filter (\a -> a /= (getCoord start)) (blackPieces gs)
                , message = ""}) (currMove ++ [start])
            | otherwise = gs

getCoord:: PorK Coord -> Coord
-- Takes in a move and returns the Coord of that move, removing the PorK component
getCoord (P (x,y)) = (x,y)
getCoord (K (x,y)) = (x,y)

jumped:: Coord -> Coord -> Coord
-- Takes in a start and end Coord and determines the Coord that is jumped over by this move
jumped (a,b) (c,d) = ((div (a + c) 2), (div (b + d) 2))

checkIfGameOver:: GameState -> Status -> GameState
-- Determines if the game is over by checking if the player whose turn it is has any legal moves to play
checkIfGameOver gs status
    | status == Turn Red && moves (gs{status = Turn Red}) == ([], [])
     = gs{status = GameOver, message = "Black wins!"}
    | status == Turn Black && moves (gs{status = Turn Black}) == ([], [])
     = gs{status = GameOver, message = "Red wins!"}
    | status == Turn Red = gs{status = Turn Red}
    | status == Turn Black = gs{status = Turn Black}
    | otherwise = gs

