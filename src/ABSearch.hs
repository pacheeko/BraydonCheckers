module ABSearch where

import Moves
import ApplyMove
import Heuristic
import Checkers.Types

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