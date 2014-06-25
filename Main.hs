import ManageDB
import Money
import Data.List
import Data.Set (fromList)

similarTs :: Double -> [Transaction] -> [[Transaction]]
similarTs cutoff ts = nubBy (\a b -> (fromList a) == (fromList b)) $ map (\a -> map snd $ filter (\(score, _) -> score > cutoff) $ similarTransactions a ts) ts

main :: IO ()
main = do
    (Just ts) <- getTransactions
    mapM_ (\t -> do
            putStr $ unlines $ map show t
            getLine
        ) $ similarTs 0.6 ts
