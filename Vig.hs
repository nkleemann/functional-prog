{-
    plain = "ESHANDELTSICHUMEINMONOGRAPHISCHESPOLYALPHABETISCHESSUBSTITUTIONSVERFAHREN"
    key   = "PFAHL"
-}

module Vigenere where

import Data.Char
 
encode :: Char -> Char -> Char
encode plain key = toLetter $ mod (ord plain + ord key) 26

decode :: Char -> Char -> Char
decode code key = toLetter $ mod (ord key - ord code) 26

toLetter :: Int -> Char
toLetter = chr . (+) (ord 'A')

vigenere mode plain key = zipWith mode (cycle key) plain