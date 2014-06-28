import ManageDB
import Money
import Data.Function (on)
import Data.List
import qualified Data.Set as Set (fromList)

similarTs :: Double -> [Transaction] -> [[Transaction]]
similarTs cutoff ts = nubBy ((==) `on` Set.fromList) $ map (\a -> map snd $ filter (\(score, _) -> score > cutoff) $ similarTransactions a ts) ts

main :: IO ()
main = do
    (Just ts) <- getTransactions
    mapM_ (\t -> do
            putStr $ unlines $ map show t
            getLine
        ) $ similarTs 0.6 ts
