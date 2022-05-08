import Checkers.FrontEnd.Basic
import Checkers.FrontEnd.Types
import Checkers.Types

import Moves

applyMove :: Move -> GameState -> GameState
applyMove _ g = g

gameConfig = GameConfig{
    engine = applyMove,
    blackMove = Human,
    redMove = Human,
    state = initialGameState
}

main :: IO ()
main = do
    putStrLn (show (moves initialGameState))
    frontend gameConfig

-- ------------------------------
-- | b1 |   | b2 |    | b3 |    | b4 |
-- -----------------------------
-- | b5 |   | b6 |    | b8 |    | b7 |
-- -----------------------------
-- | b9 |   | bA |    | bB |    | bC |
-- -----------------------------
-- -----------------------------
-- |    |   |    |    | k1 |    |    |
-- -----------------------------
-- |    |   |    |    |    | k2 |    |
-- ------------------------------
-- ------------------------------
-- ------------------------------
-- ------------------------------
-- ------------------------------
