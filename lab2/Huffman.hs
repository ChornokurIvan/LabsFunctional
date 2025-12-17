{-# LANGUAGE BangPatterns #-}
module Huffman where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import Control.DeepSeq (NFData, rnf)

-- Дерево Хаффмана
data HuffmanTree = Leaf !Char !Int
                 | Node !Int HuffmanTree HuffmanTree
                 deriving (Show, Eq)

instance NFData HuffmanTree where
  rnf (Leaf c freq) = rnf c `seq` rnf freq
  rnf (Node freq left right) = rnf freq `seq` rnf left `seq` rnf right

-- Підрахунок частоти символів
frequencyMap :: ByteString -> Map.Map Char Int
frequencyMap = BS.foldr' (\c m -> Map.insertWith (+) (toEnum $ fromIntegral c) 1 m) Map.empty

-- Створення списку листів з частотами
makeLeaves :: Map.Map Char Int -> [HuffmanTree]
makeLeaves = map (uncurry Leaf) . Map.toList

-- Об'єднання двох дерев
mergeTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTrees t1 t2 = Node (weight t1 + weight t2) t1 t2

-- Вага дерева
weight :: HuffmanTree -> Int
weight (Leaf _ freq) = freq
weight (Node freq _ _) = freq

-- Побудова дерева Хаффмана
buildHuffmanTree :: [HuffmanTree] -> HuffmanTree
buildHuffmanTree [] = error "Cannot build tree from empty list"
buildHuffmanTree [t] = t
buildHuffmanTree trees = buildHuffmanTree $ insertSorted merged $ tail sorted
  where
    sorted = sortBy (comparing weight) trees
    merged = mergeTrees (head sorted) (head $ tail sorted)
    insertSorted x [] = [x]
    insertSorted x (y:ys)
      | weight x <= weight y = x : y : ys
      | otherwise = y : insertSorted x ys

-- Побудова кодової таблиці з дерева
buildCodeTable :: HuffmanTree -> Map.Map Char [Bool]
buildCodeTable tree = Map.fromList $ buildCodes tree []
  where
    buildCodes (Leaf c _) code = [(c, reverse code)]
    buildCodes (Node _ left right) code = 
      buildCodes left (False : code) ++ buildCodes right (True : code)

-- Кодування тексту за допомогою таблиці кодів
encode :: Map.Map Char [Bool] -> ByteString -> [Bool]
encode codeTable = concatMap (\c -> Map.findWithDefault [] (toEnum $ fromIntegral c) codeTable)

-- Декодування бітової послідовності за допомогою дерева
decode :: HuffmanTree -> [Bool] -> ByteString
decode tree bits = BS.pack $ decodeHelper tree tree bits
  where
    decodeHelper _ (Leaf c _) [] = [fromIntegral $ fromEnum c]
    decodeHelper root (Leaf c _) remaining = 
      fromIntegral (fromEnum c) : decodeHelper root root remaining
    decodeHelper root (Node _ left right) (False:bs) = decodeHelper root left bs
    decodeHelper root (Node _ left right) (True:bs) = decodeHelper root right bs
    decodeHelper root (Node _ left right) [] = []  -- Недостатньо бітів
    decodeHelper _ _ [] = []

-- Конвертація списку Bool в ByteString
bitsToByteString :: [Bool] -> ByteString
bitsToByteString bits = BS.pack $ map bitsToByte $ chunksOf 8 (bits ++ replicate (8 - (length bits `mod` 8)) False)
  where
    bitsToByte = foldl (\acc b -> acc * 2 + if b then 1 else 0) 0
    chunksOf n [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Конвертація ByteString в список Bool
byteStringToBitsFixed :: ByteString -> [Bool]
byteStringToBitsFixed = concatMap byteToBits
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = [testBit byte (7 - i) | i <- [0..7]]
      where
        testBit b pos = (fromIntegral b `shiftR` pos) .&. 1 == 1

-- Виправлена версія bitsToByteString
bitsToByteStringFixed :: [Bool] -> (ByteString, Int)  -- Повертає також кількість бітів
bitsToByteStringFixed bits = 
  let padding = (8 - (length bits `mod` 8)) `mod` 8
      paddedBits = bits ++ replicate padding False
      bytes = map bitsToByte $ chunksOf 8 paddedBits
  in (BS.pack bytes, length bits)
  where
    bitsToByte = foldl (\acc b -> acc * 2 + if b then 1 else 0) 0
    chunksOf n [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Повна функція стиснення
compress :: ByteString -> (HuffmanTree, ByteString, Int)
compress input = 
  let freqMap = frequencyMap input
      leaves = makeLeaves freqMap
      tree = buildHuffmanTree leaves
      codeTable = buildCodeTable tree
      encoded = encode codeTable input
      (compressed, bitCount) = bitsToByteStringFixed encoded
  in (tree, compressed, bitCount)

-- Повна функція розпакування
decompress :: HuffmanTree -> ByteString -> Int -> ByteString
decompress tree compressed bitCount = 
  let bits = take bitCount $ byteStringToBitsFixed compressed
  in decode tree bits

