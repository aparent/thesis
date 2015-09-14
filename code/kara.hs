import Control.Parallel.Strategies as P
import Data.List(minimumBy)
import Data.Function(on)
import qualified Data.MemoCombinators as Memo
import System.IO

main :: IO()
main = do
  writeFile "data/cutoffs.dat" $ spaceSep $ map (avgCutoff (50,500)) [3..35]
  writeFile "data/naiveMult.dat" $ spaceSep $ map (\x-> (x,naiveMult x)) [4..400]
  writeFile "data/naiveMultDepth.dat" $ spaceSep $ map (\x-> (x,naiveMultDepth x)) [4..400]
  writeFile "data/naiveMultSize.dat" $ spaceSep $ map (\x-> (x,naiveMultSize x)) [4..400]
  mapM_ writeSimpleKaraData [5,9,20,50]
  mapM_ writeAKaraData [8,9,10,11,15,25]
  return ()


writeAKaraData :: Int -> IO()
writeAKaraData n =  writeFile ("data/akara" ++show n ++ ".dat") $ spaceSep $ map (\x-> (x,2*(karaM n x))) [4..400]

writeSimpleKaraData :: Int -> IO()
writeSimpleKaraData n = do
    writeFile ("data/skara" ++show n ++ ".dat") $ spaceSep $ map (\x-> (x,simpleKara n x)) [4..400]
    writeFile ("data/skaraSize" ++show n ++ ".dat") $ spaceSep $ map (\x-> (x,simpleKaraSize n x)) [4..400]
    writeFile ("data/skaraDepth" ++show n ++ ".dat") $ spaceSep $ map (\x-> (x,simpleKaraDepth n x)) [4..400]

spaceSep :: (Show a , Show b) => [(a,b)] -> String
spaceSep = foldl (\x (y1,y2) -> x ++ show y1 ++" " ++ show y2 ++ "\n") ""


ripAdd :: Int -> Int
ripAdd n = 2*n

ctrlAdd :: Int -> Int
ctrlAdd n = 4*n + 1

naiveMult :: Int -> Int
naiveMult n = n * ctrlAdd n

karaM :: Int -> Int -> Int
karaM = Memo.memo2 Memo.integral Memo.integral kara

avgCutoff :: (Int,Int) -> Int -> (Int,Double)
avgCutoff r c = (c,fAvg (karaM c) r)

kara :: Int -> Int -> Int
kara c n | n <= c = naiveMult n
         | otherwise = minimum $ karaSizes c $ splits
  where splits = let sRange =  [div n 4 .. 3 * div n 4 + mod n 4]
                 in zip sRange $ reverse sRange

karaSizes :: Int-> [(Int,Int)] -> [Int]
karaSizes c n =  map kSize n
  where kSize  x = karas x + adders x
        karas  (s1,s2) = 2 * karaM c (max s1 s2)  + karaM c (min s1 s2)
        adders (s1,s2) = 3*(ripAdd $ (max s1 s2)*2-(mod (max s1 s2) 2)) + 4*(ripAdd $ max s1 s2)

fAvg :: (Int -> Int) -> (Int,Int) -> Double
fAvg f (x1,x2) = (fromIntegral $ sum $ map f [x1 .. x2]) / fromIntegral numTerms
	where numTerms = x2 - x1 + 1

-----------------

simpleKara :: Int -> Int -> Int
simpleKara cutoff size = 2 * sKara size
  where sKara n | n <= cutoff = naiveMult n
                | otherwise = 3 * ripAdd n + 4 * ripAdd (half n) + 3 * sKara (half n)
        half n =  div n 2 + mod n 2

simpleKaraSize :: Int -> Int -> Int
simpleKaraSize cuttoff n
    | n <= cuttoff =
        2 * n
    | n `mod` 2 == 1 =
        n + 1 + 3 * simpleKaraSize cuttoff (n `div` 2 + 1)
    | otherwise =
        n + 1 + 3 * simpleKaraSize cuttoff (n `div` 2)

naiveMultSize :: Int -> Int
naiveMultSize n = 4 * n

naiveMultDepth :: Int -> Int
naiveMultDepth n = 4*n*n - 2*n - 1

simpleKaraDepth :: Int -> Int -> Int
simpleKaraDepth cuttoff n
    | n <= cuttoff =
        naiveMultDepth n
    | n `mod` 2 == 1 =
        3 * ripAddDepth + 2 * simpleKaraDepth cuttoff (n `div` 2 + 1)
    | otherwise =
        3 * ripAddDepth + 2 * simpleKaraDepth cuttoff (n `div` 2)
    where ripAddDepth = 2*n
