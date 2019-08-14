import SpiralMatrix
import Data.List
import Data.Maybe

knightMoves = [
    [-1,-2],
    [-1, 2],
    [ 1,-2],
    [ 1, 2],
    [-2,-1],
    [-2, 1],
    [ 2,-1],
    [ 2, 1]]

repeatedStep pos mov n = tmp n
    where
        tmp m
            | m == 0 = step pos mov []
            | otherwise = do
                let res = tmp (m-1)
                if ( isNothing $ fst res )
                    then res
                    else step (fst $ fromJust $ fst res) mov (snd res)

-- Position, Moveset, Previous, (maybe new Position, previous with with position)
step :: (Num a, Ord a) => [a] -> [[a]] -> [([a],a)] -> (Maybe ([a],a), [([a],a)])
step pos mov pre
    | length filtered /= 0 = (Just (filtered!!0), pre++[addSpiralValue pos])
    | otherwise = (Nothing, pre++[addSpiralValue pos])
    where
        possiblePositions = map (addSpiralValue) $ map (\i -> vecadd i pos) mov
        addSpiralValue [p,q] = ([p,q], spiral p q) -- appends the spiral value to the end of a position
        best = sortOn (snd) possiblePositions      -- sorts possible moves by their spiral value
        filtered = filter (\i -> not (elem i pre) ) best

vecadd :: Num a => [a] -> [a] -> [a]
vecadd p q = zipWith (+) p q
