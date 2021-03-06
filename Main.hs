import Control.Monad
import qualified Control.Monad.Loops as L
import Data.List
import System.Random
import qualified Data.Sequence as S
import Data.List.Split
import qualified Data.Foldable as F
import System.IO

data GameState = GameState [[Integer]] deriving (Eq, Read)

instance Show GameState where
    show (GameState cells) = unlines $ map (unwords . map show) cells

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    gs <- insertRandom (GameState (replicate 4 (replicate 4 0)))
    print gs
    L.iterateUntilM (not . hasMove) move gs
    putStrLn "Game over."

hasMove :: GameState -> Bool
hasMove (GameState cells) = 0 `elem` concat cells || 0 `elem` concatMap (\r -> zipWith (-) r (tail r)) (cells ++ transpose cells)

move :: GameState -> IO GameState
move gs@(GameState cells) = do
    a <- getChar
    putStrLn ""
    let gs' = GameState (case a of
                            'i' -> (transpose . map moveRow . transpose) cells
                            'j' -> map moveRow cells
                            'k' -> (transpose . map (reverse . moveRow . reverse) . transpose) cells
                            'l' -> map (reverse . moveRow . reverse) cells
                            _ -> cells)
    if gs' /= gs
        then do
            gs'' <- insertRandom gs'
            print gs''
            return gs''
        else if a `elem` "ijkl"
            then do
                print gs
                return gs
            else do
                putStrLn "Invalid move. Use i, j, k, l for Up, Left, Down, Right."
                print gs
                return gs

moveRow :: [Integer] -> [Integer]
moveRow vs = let (l, r) = partition (== 0) vs in moveRow' (l ++ r)

moveRow' :: [Integer] -> [Integer]
moveRow' [] = []
moveRow' [a] = [a]
moveRow' (0:zs) = moveRow zs ++ [0]
moveRow' (a:b:zs)
    | a == b = a + b : (moveRow' zs ++ [0])
    | otherwise = a : moveRow' (b:zs)

insertRandom :: GameState -> IO GameState
insertRandom (GameState cells) = do
    let is = elemIndices 0 (concat cells)
    i <- randomRIO (0, length is - 1)
    return $ GameState $ chunksOf 4 $ F.toList $ S.update (is !! i) 2 $ S.fromList (concat cells)
