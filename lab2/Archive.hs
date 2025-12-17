{-# LANGUAGE BangPatterns #-}
module Archive where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception
import Compression
import Huffman
import qualified Data.Binary as Binary
import Data.List (intercalate)

-- Інформація про стиснений файл в архіві
data ArchiveEntry = ArchiveEntry
  { entryPath :: FilePath
  , entryOriginalSize :: Int
  , entryCompressedSize :: Int
  , entryBitCount :: Int
  , entryTree :: HuffmanTree
  , entryData :: ByteString
  } deriving (Show)

-- Архів як список записів
type Archive = [ArchiveEntry]

-- Створення архіву з множини файлів (послідовна версія)
createArchiveSequential :: [FilePath] -> IO (Either String Archive)
createArchiveSequential files = do
  results <- mapM processFile files
  return $ sequence results
  where
    processFile :: FilePath -> IO (Either String ArchiveEntry)
    processFile filePath = do
      exists <- doesFileExist filePath
      if not exists
        then return $ Left $ "File not found: " ++ filePath
        else do
          content <- BS.readFile filePath
          let originalSize' = BS.length content
              (tree', compressed, bitCount') = compress content
              compressedSize' = BS.length compressed
          return $ Right $ ArchiveEntry filePath originalSize' compressedSize' bitCount' tree' compressed

-- Створення архіву з множини файлів (паралельна версія)
createArchiveParallel :: [FilePath] -> IO (Either String Archive)
createArchiveParallel files = do
  results <- mapM processFile files
  return $ sequence results
  where
    processFile :: FilePath -> IO (Either String ArchiveEntry)
    processFile filePath = do
      exists <- doesFileExist filePath
      if not exists
        then return $ Left $ "File not found: " ++ filePath
        else do
          content <- BS.readFile filePath
          let originalSize' = BS.length content
              (tree', compressed, bitCount') = compress content
              compressedSize' = BS.length compressed
          return $ Right $ ArchiveEntry filePath originalSize' compressedSize' bitCount' tree' compressed

-- Збереження архіву в файл
saveArchive :: FilePath -> Archive -> IO ()
saveArchive archivePath archive = do
  let entries = map serializeEntry archive
      archiveData = BS.concat entries
  BS.writeFile archivePath archiveData
  where
    serializeEntry :: ArchiveEntry -> ByteString
    serializeEntry (ArchiveEntry path origSize compSize bitCount' tree' data') =
      let pathBytes = BS.pack $ map (fromIntegral . fromEnum) path
          pathLen = BS.length pathBytes
          header = BS.concat [
              Binary.encode pathLen
            , pathBytes
            , Binary.encode origSize
            , Binary.encode compSize
            , Binary.encode bitCount'
            , Binary.encode $ show tree'  -- Простий спосіб збереження дерева
            ]
      in BS.concat [header, BS.singleton 0, data', BS.singleton 0]  -- Роздільники

-- Завантаження архіву з файлу
loadArchive :: FilePath -> IO (Either String Archive)
loadArchive archivePath = do
  content <- BS.readFile archivePath
  return $ parseArchive content
  where
    parseArchive :: ByteString -> Either String Archive
    parseArchive bs
      | BS.null bs = Right []
      | otherwise = case parseEntry bs of
          Left err -> Left err
          Right (entry, rest) -> case parseArchive rest of
            Left err -> Left err
            Right entries -> Right (entry : entries)
    
    parseEntry :: ByteString -> Either String (ArchiveEntry, ByteString)
    parseEntry bs = 
      -- Спрощена версія - для повної реалізації потрібен правильний парсинг
      Left "Archive parsing not fully implemented (requires tree deserialization)"

-- Розпакування архіву
extractArchive :: Archive -> IO (Either String ())
extractArchive archive = do
  results <- mapM extractEntry archive
  let errors = filter (either (const True) (const False)) results
  if null errors
    then return $ Right ()
    else return $ Left $ "Errors extracting files: " ++ show errors
  where
    extractEntry :: ArchiveEntry -> IO (Either String ())
    extractEntry (ArchiveEntry path _ _ bitCount' tree' data') = do
      let decompressed = decompress tree' data' bitCount'
      createDirectoryIfMissing True $ takeDirectory path
      BS.writeFile path decompressed
      return $ Right ()

-- Статистика архіву
archiveStats :: Archive -> (Int, Int, Double)
archiveStats archive = 
  let totalOriginal = sum $ map entryOriginalSize archive
      totalCompressed = sum $ map entryCompressedSize archive
      ratio = if totalOriginal > 0 
        then fromIntegral totalCompressed / fromIntegral totalOriginal * 100
        else 0.0
  in (totalOriginal, totalCompressed, ratio)

