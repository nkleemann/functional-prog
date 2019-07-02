{- |
Author  : Niklas Kleemann
Date    : 28.10.2018

Automatic cracking of a "subset of" Vigenere Ciphers using trigraphs
distributions, key-length estimation and histogram "analysis".

This program does not work for ALL ciphertexts, it's partial on ciphertexts
that are: between 400 and 20000 characters long (due to statistical nature).
-}

module Main where


import           Control.Arrow   (second)
import           Data.Char       (chr, ord)
import           Data.Function   (on)
import           Data.List.Split (chunksOf)
import           Data.Ord        (comparing)
import           Data.List       (elemIndices, group, maximumBy, sort, transpose)
import qualified Data.Set        as S
import qualified Vigenere        as V


type Trigraph      = (Char, Char, Char)
type CharHistogram = [(Char, [Int])]


main :: IO ()
main = do
    cipher       <- readFile "code0.txt"
    (plain, key) <- crack cipher
    putStrLn $ plain ++ "\nKey was: " ++ key


crack :: String -> IO (String, String)
crack code = return . decrypt code . findKey 'E' $ charHisto code


-- | Find all combinations of adjacent character-tripples.
trigraphs :: String -> [Trigraph]
trigraphs cs = case cs of
    (c1:c2:c3:cs') -> (c1, c2, c3) : trigraphs (c2:c3:cs')
    _              -> []


-- | Find trigraph duplicates and save their positions.
occurences :: [Trigraph] -> [(Trigraph, [Int])]
occurences ts = onlyDups $ map (\t -> (t, (elemIndices t ts))) ts
    where onlyDups = filter ((> 1) . length . snd)


-- | Calculate offsets (distance) between the trigraph position.
offsets :: [(Trigraph, [Int])] -> S.Set (Trigraph, [Int])
offsets = S.fromList . map (second offsH) 
    where offsH is = tail $ map (\x -> x - (head is)) is


-- | Offset values between same trigraphs are multiples of the key length.
-- | We find the concrete length by finding the most common GCD between offsets.
keyLength :: S.Set (Trigraph, [Int]) -> Int
keyLength set = mostCommon $ estimateGCDs $ concat $ (map snd $ S.toList set)
    where mostCommon = head . maximumBy (compare `on` length) . group . sort


-- | Give an estimation of the most common gcd between number pairs in a list.
estimateGCDs :: [Int] -> [Int]
estimateGCDs l = case l of
    (x:y:ys) -> case gcd x y of
        1 ->      estimateGCDs (tail l)
        y -> (y : estimateGCDs (tail l))
    _        -> []


-- | Split the code into chunks of size n and log the occurence of
-- | each character on each position within this chunk.
splitNCount :: String -> Int -> CharHistogram
splitNCount parts code
    = zip ['A'..'Z'] $
        for ['A'..'Z'] $ \c ->
            for (splitCode code parts) $ \s ->
                count s c

        where splitCode n = transpose . chunksOf n
              count s x   = foldl (\n c -> if c == x then (n + 1) else n) 0 s


-- | We are looking for the highest occurence of a letter c at the nth
-- | position within the chunks - We have to transpose to get a list of 
-- | length m (= keylength), containing sequences where each pos. represents 
-- | the freq. of letter c. We then extract the maximum and retain the corr. char.
findEncKey :: CharHistogram -> String
findEncKey = map (chr . (65 +) . snd) . map maxiAt . transpose . map snd
    where maxiAt xs = maximumBy (comparing fst) (zip xs [0..])


-- | Decrypting the Key is a manner of caesar-shifting the encrypted one.
decKey :: Char -> String -> String
decKey mostCommonChar = map shiftBack
    where shiftBack c = chr $ ord 'A' + (ord c + ord 'A' - shift) `mod` 26
          shift       = (ord mostCommonChar - ord 'A')


-- | Helper Functions
decrypt code key = (V.vigenere V.decode code key, key)
findKey mcc      = decKey mcc . findEncKey
charHisto c      = splitNCount c . keyLength . offsets . occurences $ trigraphs c
for              = flip map