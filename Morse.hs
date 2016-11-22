module Morse where

import MorseLib

----------------
-- Exercise 1 --
----------------

-- Encodes a word
codeWord :: String -> [MorseUnit]
codeWord [] = []
codeWord (x:xs) = (codeSymbol x) ++ shortGap ++ (codeWord xs)

-- Encodes a string (with words)
encode :: String -> [MorseUnit]
encode [] = []
encode xs = encodeWords (words xs)

-- Encodes a list of words
encodeWords :: [String] -> [MorseUnit]
encodeWords [] = []
encodeWords [x] = codeWord x
encodeWords (x:xs) = codeWord x ++ mediumGap ++ encodeWords xs

-- Encodes a list of strings
encodeList :: [String] -> [[MorseUnit]]
encodeList [] = []
encodeList (x:xs) = (encode x) : (encodeList xs)


----------------
-- Exercise 2 --
----------------

-- Tries to find a Morse code chunk in the table
findInTable :: [MorseUnit] -> MorseTable -> Char
findInTable x [] = '-' -- this is a failure
findInTable x (y:ys) | x == fst y = snd y
                     | otherwise  = findInTable x ys 

-- Decodes a Morse code
decode :: [MorseUnit] -> String
decode [] = ""
decode (Silence:Silence:Silence:Silence:xs) = ' ' : (decode xs)
decode xs = decodeBit [head xs] (tail xs)

-- Tries to decode a bit of Morse code
decodeBit :: [MorseUnit] -> [MorseUnit] -> String
decodeBit bit xs | result == '-'         = decodeBit (bit ++ [head xs]) (tail xs)
				 | take 2 xs == shortGap = result : (decode xs')
				 | otherwise             = decodeBit (bit ++ [head xs]) (tail xs)
	where result = findInTable bit table
	      xs' = drop 2 xs

-- Decodes a list of Morse codes
decodeList :: [[MorseUnit]] -> [String]
decodeList [] = []
decodeList (x:xs) = (decode x) : (decodeList xs)


----------------
-- Exercise 3 --
----------------

-- Checks if a Morse code starts with dit
startsWithDit :: [MorseUnit] -> Bool
startsWithDit xs = if length xs >= 2 && take 2 xs == dit then True else False

-- Checks if a Morse code starts with dah
startsWithDah :: [MorseUnit] -> Bool
startsWithDah xs = if length xs >= 4 && take 4 xs == dah then True else False

-- Adds a given value to a MorseTree
addNode :: ([MorseUnit], Char) -> MorseTree -> MorseTree
addNode ([], ch) Nil = Leaf ch
addNode (xs, ch) Nil | startsWithDit xs  = Branch0 (addNode (drop 2 xs, ch) Nil) Nil
					 | startsWithDah xs = Branch0 Nil (addNode (drop 4 xs, ch) Nil)
addNode (xs, ch) (Leaf char) | startsWithDit xs  = Branch1 char (addNode (drop 2 xs, ch) Nil) Nil
					 		 | startsWithDah xs = Branch1 char Nil (addNode (drop 4 xs, ch) Nil)
addNode ([], ch) (Branch0 l r) = Branch1 ch l r
addNode (xs, ch) (Branch0 l r) | startsWithDit xs  = Branch0 (addNode (drop 2 xs, ch) l) r
					 		   | startsWithDah xs = Branch0 l (addNode (drop 4 xs, ch) r)
addNode (xs, ch) (Branch1 char l r) | startsWithDit xs  = Branch1 char (addNode (drop 2 xs, ch) l) r
					 		        | startsWithDah xs = Branch1 char l (addNode (drop 4 xs, ch) r)
  
-- Translates a MorseTable into a MorseTree
toTree :: MorseTable -> MorseTree
toTree [] = Nil
toTree (x:xs) = addNode x (toTree xs)

-- Translates a list of MorseTables into a list of MorseTrees
toTreeList :: [MorseTable] -> [MorseTree]
toTreeList [] = []
toTreeList (x:xs) = (toTree x) : (toTreeList xs)


----------------
-- Exercise 4 --
----------------

toTablePrefix :: MorseTree -> [MorseUnit] -> MorseTable
toTablePrefix Nil _ = []
toTablePrefix (Leaf ch) acc = [(acc, ch)]
toTablePrefix (Branch1 ch l r) acc = (acc,ch) : (toTablePrefix l (acc ++ dit) ++ toTablePrefix r (acc ++ dah))
toTablePrefix (Branch0 l r) acc = toTablePrefix l (acc ++ dit) ++ toTablePrefix r (acc ++ dah)

-- Translates a MorseTree into a MorseTable
toTable :: MorseTree -> MorseTable
toTable tree = toTablePrefix tree []

toTableList :: [MorseTree] -> [MorseTable]
toTableList [] = []
toTableList (x:xs) = (toTable x) : (toTableList xs)

