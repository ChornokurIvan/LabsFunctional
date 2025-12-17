{-# LANGUAGE BangPatterns #-}
module Compression where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Binary as Binary
import System.IO
import Control.Exception (bracket)
import Huffman

-- Структура для збереження стисненого файлу
data CompressedFile = CompressedFile
  { originalSize :: Int
  , compressedSize :: Int
  , bitCount :: Int
  , tree :: HuffmanTree
  , data_ :: ByteString
  } deriving (Show)

-- Стиснення файлу
compressFile :: FilePath -> IO (Either String CompressedFile)
compressFile filePath = do
  content <- BS.readFile filePath
  let originalSize' = BS.length content
      (tree', compressed, bitCount') = compress content
      compressedSize' = BS.length compressed
  return $ Right $ CompressedFile originalSize' compressedSize' bitCount' tree' compressed

-- Розпакування файлу
decompressFile :: CompressedFile -> ByteString
decompressFile (CompressedFile _ _ bitCount' tree' data') = 
  decompress tree' data' bitCount'

-- Збереження стисненого файлу
saveCompressed :: FilePath -> CompressedFile -> IO ()
saveCompressed filePath (CompressedFile origSize compSize bitCount' tree' data') = do
  -- Простий формат: спочатку дерево (потрібно серіалізувати), потім дані
  -- Для простоти використовуємо текстове представлення дерева
  let header = BS.concat [
        BS.pack $ map (fromIntegral . fromEnum) $ show tree'
      , BS.singleton 0  -- Роздільник
      , Binary.encode origSize
      , Binary.encode compSize
      , Binary.encode bitCount'
      ]
  BS.writeFile filePath $ BS.concat [header, BS.singleton 0, data']

-- Завантаження стисненого файлу
loadCompressed :: FilePath -> IO (Either String CompressedFile)
loadCompressed filePath = do
  content <- BS.readFile filePath
  -- Спрощена версія - для повної реалізації потрібна серіалізація дерева
  -- Тут повертаємо помилку, оскільки серіалізація дерева складна
  return $ Left "Loading compressed files requires tree serialization (not fully implemented)"

-- Проста версія збереження (без серіалізації дерева)
saveCompressedSimple :: FilePath -> CompressedFile -> IO ()
saveCompressedSimple filePath (CompressedFile origSize compSize bitCount' _ data') = do
  -- Зберігаємо тільки розміри та дані (дерево потрібно передавати окремо)
  let header = BS.concat [
        Binary.encode origSize
      , Binary.encode compSize
      , Binary.encode bitCount'
      ]
  BS.writeFile filePath $ BS.concat [header, data']

-- Версія збереження з деревом у окремому файлі
saveCompressedWithTree :: FilePath -> FilePath -> CompressedFile -> IO ()
saveCompressedWithTree dataPath treePath (CompressedFile origSize compSize bitCount' tree' data') = do
  -- Зберігаємо дерево окремо (як текст для простоти)
  writeFile treePath $ show tree'
  -- Зберігаємо дані
  let header = BS.concat [
        Binary.encode origSize
      , Binary.encode compSize
      , Binary.encode bitCount'
      ]
  BS.writeFile dataPath $ BS.concat [header, data']

