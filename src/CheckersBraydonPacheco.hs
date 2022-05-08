module CheckersBraydonPacheco (moves, apply_move, red_ai, black_ai)

where

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

bot = -10000
top = 10000
depth = 6

red_ai:: GameState -> Move
-- Ai for the red pieces. Takes in a gamestate where it is currently reds turn, and determines
-- the best move to play based on the red_heuristic function. Looks a certain number of moves ahead
-- equal to the current depth
red_ai gs = getRedBestMove depth gs (simpleOrJumpMoves (moves gs))

black_ai:: GameState -> Move
-- Ai for the black pieces. Takes in a gamestate where it is currently blacks turn, and determines
-- the best move to play based on the black_heuristic function. Looks a certain number of moves ahead
-- equal to the current depth
black_ai gs = getBlackBestMove depth gs (simpleOrJumpMoves (moves gs))

getRedBestMove:: Int -> GameState -> [Move] -> Move
-- Initial function that the red_ai function calls. The list of moves is turned into a list of tuples where 
-- the second value in the tuple is number the function maximizes. This function returns the index of
-- the tuple which has the highest number in the 2nd spot, then returns the move at the same index
-- in the given move list
getRedBestMove ply gs mvs = mvs !! (getIndexMaxItem 0 0 0 (map (\mv -> (redMaxEval (ply-1, 0, bot, top) (apply_move mv gs) (simpleOrJumpMoves (moves (apply_move mv gs))))) mvs))

redMaxEval:: (Int, Int, Int, Int) -> GameState -> [Move] -> (Int, Int, Int, Int)
-- Takes in a tuple of ints which represent (currentDepth, heuristic, alpha, beta), a gamestate and a move.
-- Calls redMinEval on the gamestate that results from each move, starting from the first move in the list,
-- then updates the alpha and beta values according to the alpha-beta pruning algorithm, and prunes any values
-- from the list if alpha > beta
-- If at depth 0, returns the red_heuristic on the gamestate as the beta value
redMaxEval (ply, heuristic, alpha, beta) gs [] = (ply, heuristic, alpha, beta)
redMaxEval (ply, heuristic, alpha, beta) gs (x:xs)
    | ply == 0 = (0, get_heuristic, alpha, get_heuristic)
    | alpha > beta = (ply, heuristic, alpha, beta)
    | otherwise = redMaxEval (ply, (if ((get2nd rmine) > heuristic) then (get2nd rmine) else heuristic), (if ((get2nd rmine) > alpha) then (get2nd rmine) else alpha), beta) gs xs     
    where 
        get_heuristic = red_heuristic gs
        rmine = redMinEval (ply-1, 0, alpha, beta) (apply_move x gs) (simpleOrJumpMoves (moves (apply_move x gs)))

redMinEval:: (Int, Int, Int, Int) -> GameState -> [Move] -> (Int, Int, Int, Int)
-- Takes in a tuple of ints which represent (currentDepth, heuristic, alpha, beta), a gamestate and a move.
-- Calls redMaxEval on the gamestate that results from each move, starting from the first move in the list,
-- then updates the alpha and beta values according to the alpha-beta pruning algorithm, and prunes any values
-- from the list if beta > alpha
-- If at depth 0, returns the red_heuristic on the gamestate as the alpha value
redMinEval (ply, heuristic, alpha, beta) gs [] = (ply, heuristic, alpha, beta)
redMinEval (ply, heuristic, alpha, beta) gs (x:xs)
    | ply == 0 = (0, get_heuristic, get_heuristic, beta) 
    | alpha > beta = (ply, heuristic, alpha, beta)
    | otherwise = redMinEval (ply, (if ((get2nd rmaxe) < heuristic) then (get2nd rmaxe) else heuristic) , alpha, (if ((get2nd rmaxe) < beta) then (get2nd rmaxe) else beta)) gs xs
        where 
            get_heuristic = red_heuristic gs
            rmaxe = redMaxEval (ply-1, 0, alpha, beta) (apply_move x gs) (simpleOrJumpMoves (moves (apply_move x gs)))

getBlackBestMove:: Int -> GameState -> [Move] -> Move
-- Initial function that the black_ai function calls. The list of moves is turned into a list of tuples where 
-- the second value in the tuple is number the function maximizes. This function returns the index of
-- the tuple which has the highest number in the 2nd spot, then returns the move at the same index
-- in the given move list
getBlackBestMove ply gs mvs = mvs !! (getIndexMaxItem 0 0 0 (map (\mv -> (blackMaxEval (ply-1, 0, bot, top) (apply_move mv gs) (simpleOrJumpMoves (moves (apply_move mv gs))))) mvs))

blackMaxEval:: (Int, Int, Int, Int) -> GameState -> [Move] -> (Int, Int, Int, Int)
-- Takes in a tuple of ints which represent (currentDepth, heuristic, alpha, beta), a gamestate and a move.
-- Calls blackMinEval on the gamestate that results from each move, starting from the first move in the list,
-- then updates the alpha and beta values according to the alpha-beta pruning algorithm, and prunes any values
-- from the list if alpha > beta
-- If at depth 0, returns the black_heuristic on the gamestate as the beta value
blackMaxEval (ply, heuristic, alpha, beta) gs [] = (ply, heuristic, alpha, beta)
blackMaxEval (ply, heuristic, alpha, beta) gs (x:xs) 
    | ply == 0 = (0, get_heuristic, alpha, get_heuristic)
    | alpha > beta = (ply, heuristic, alpha, beta)
    | otherwise = blackMaxEval (ply, (if ((get2nd bmine) > heuristic) then (get2nd bmine) else heuristic) , (if ((get2nd bmine) > alpha) then (get2nd bmine) else alpha), beta) gs xs     
    where 
        get_heuristic = black_heuristic gs
        bmine = blackMinEval (ply-1, 0, alpha, beta) (apply_move x gs) (simpleOrJumpMoves (moves (apply_move x gs)))

blackMinEval:: (Int, Int, Int, Int) -> GameState -> [Move] -> (Int, Int, Int, Int)
-- Takes in a tuple of ints which represent (currentDepth, heuristic, alpha, beta), a gamestate and a move.
-- Calls blackMaxEval on the gamestate that results from each move, starting from the first move in the list,
-- then updates the alpha and beta values according to the alpha-beta pruning algorithm, and prunes any values
-- from the list if beta > alpha
-- If at depth 0, returns the black_heuristic on the gamestate as the alpha value
blackMinEval (ply, heuristic, alpha, beta) gs [] = (ply, heuristic, alpha, beta)
blackMinEval (ply, heuristic, alpha, beta) gs (x:xs)
    | ply == 0 = (0, get_heuristic, get_heuristic, beta) 
    | alpha > beta = (ply, heuristic, alpha, beta)
    | otherwise = blackMinEval (ply, (if ((get2nd bmaxe) < heuristic) then (get2nd bmaxe) else heuristic) , alpha, (if ((get2nd bmaxe) < beta) then (get2nd bmaxe) else beta)) gs xs
        where 
            get_heuristic = black_heuristic gs
            bmaxe = blackMaxEval (ply-1, 0, alpha, beta) (apply_move x gs) (simpleOrJumpMoves (moves (apply_move x gs)))

getIndexMaxItem:: Int -> Int -> Int -> [(Int,Int, Int, Int)] -> Int 
-- Finds the index of the element in a list of (Int, Int, Int, Int) where the second element of the tuple
-- is maximized
getIndexMaxItem _ maxIndex _ [] = maxIndex
getIndexMaxItem currMax maxIndex currIndex (x:xs)
    | (get2nd x) > currMax = getIndexMaxItem (get2nd x) currIndex (currIndex + 1) xs 
    | otherwise = getIndexMaxItem currMax maxIndex (currIndex + 1) xs

simpleOrJumpMoves:: ([Move], [Move]) -> [Move]
-- Takes in a tuple with a list of simple moves and jump moves
-- If there are jump moves available, it returns the list of jump moves,
-- otherwise it returns the list of simple moves
simpleOrJumpMoves (simpleMoves, jumpMoves)
    | jumpMoves == [] = simpleMoves
    | otherwise = jumpMoves

get1st:: (a, a, a, a) -> a
-- Takes in a tuple with four elements and returns the first element
get1st (a, _, _, _) = a

get2nd:: (a, a, a, a) -> a
-- Takes in a tuple with four elements and returns the second element
get2nd (_, b, _, _) = b

get3rd:: (a, a, a, a) -> a
-- Takes in a tuple with four elements and returns the third element
get3rd (_, _, c, _) = c

get4th:: (a, a, a, a) -> a
-- Takes in a tuple with four elements and returns the third element
get4th (_, _, _, d) = d

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