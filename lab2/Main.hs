{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import System.Directory
import Control.Concurrent
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List
import Archive
import Compression
import Benchmark
import Huffman

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["compress", archivePath] -> do
      putStrLn "Вкажіть файли для стиснення (завершіть введенням пустого рядка):"
      files <- getFiles
      if null files
        then putStrLn "Не вказано файлів для стиснення"
        else compressFiles archivePath files
    
    ["compress-parallel", archivePath] -> do
      putStrLn "Вкажіть файли для стиснення (завершіть введенням пустого рядка):"
      files <- getFiles
      if null files
        then putStrLn "Не вказано файлів для стиснення"
        else compressFilesParallel archivePath files
    
    ["benchmark"] -> do
      putStrLn "Вкажіть файли для бенчмарку (завершіть введенням пустого рядка):"
      files <- getFiles
      if null files
        then putStrLn "Не вказано файлів для бенчмарку"
        else comparePerformance files
    
    ["extract", archivePath, outputDir] -> do
      extractArchiveFromFile archivePath outputDir
    
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "Використання:"
  putStrLn "  compress <архів>              - стиснути файли (послідовно)"
  putStrLn "  compress-parallel <архів>      - стиснути файли (паралельно)"
  putStrLn "  benchmark                      - порівняти продуктивність"
  putStrLn "  extract <архів> <директорія>   - розпакувати архів"
  putStrLn ""
  putStrLn "Приклад:"
  putStrLn "  ./huffman-compression compress archive.huf"
  putStrLn "  ./huffman-compression compress-parallel archive.huf"
  putStrLn "  ./huffman-compression benchmark"

getFiles :: IO [FilePath]
getFiles = do
  line <- getLine
  if null line
    then return []
    else do
      exists <- doesFileExist line
      if exists
        then do
          rest <- getFiles
          return (line : rest)
        else do
          putStrLn $ "Файл не знайдено: " ++ line
          getFiles

-- Послідовне стиснення файлів
compressFiles :: FilePath -> [FilePath] -> IO ()
compressFiles archivePath files = do
  putStrLn $ "Стиснення " ++ show (length files) ++ " файлів (послідовно)..."
  result <- createArchiveSequential files
  case result of
    Left err -> putStrLn $ "Помилка: " ++ err
    Right archive -> do
      saveArchive archivePath archive
      let (origSize, compSize, ratio) = archiveStats archive
      putStrLn "Архів успішно створено!"
      putStrLn $ "Оригінальний розмір: " ++ show origSize ++ " байт"
      putStrLn $ "Стиснений розмір: " ++ show compSize ++ " байт"
      putStrLn $ "Коефіцієнт стиснення: " ++ show ratio ++ "%"

-- Паралельне стиснення файлів з використанням стратегій
compressFilesParallel :: FilePath -> [FilePath] -> IO ()
compressFilesParallel archivePath files = do
  putStrLn $ "Стиснення " ++ show (length files) ++ " файлів (паралельно)..."
  
  -- Використовуємо паралельну стратегію для обробки файлів
  results <- mapM processFileParallel files `using` parList rdeepseq
  
  let archive = catMaybes results
  if length archive == length files
    then do
      saveArchive archivePath archive
      let (origSize, compSize, ratio) = archiveStats archive
      putStrLn "Архів успішно створено!"
      putStrLn $ "Оригінальний розмір: " ++ show origSize ++ " байт"
      putStrLn $ "Стиснений розмір: " ++ show compSize ++ " байт"
      putStrLn $ "Коефіцієнт стиснення: " ++ show ratio ++ "%"
    else putStrLn "Помилка при створенні архіву"
  where
    processFileParallel :: FilePath -> IO (Maybe ArchiveEntry)
    processFileParallel filePath = do
      exists <- doesFileExist filePath
      if not exists
        then do
          putStrLn $ "Файл не знайдено: " ++ filePath
          return Nothing
        else do
          content <- BS.readFile filePath
          let originalSize' = BS.length content
              (tree', compressed, bitCount') = compress content
              compressedSize' = BS.length compressed
          return $ Just $ ArchiveEntry 
            { entryPath = filePath
            , entryOriginalSize = originalSize'
            , entryCompressedSize = compressedSize'
            , entryBitCount = bitCount'
            , entryTree = tree'
            , entryData = compressed
            }

-- Використовуємо Control.Concurrent для справжньої багатопотоковості
compressFilesThreaded :: FilePath -> [FilePath] -> IO ()
compressFilesThreaded archivePath files = do
  putStrLn $ "Стиснення " ++ show (length files) ++ " файлів (багатопотоково)..."
  
  numCapabilities <- getNumCapabilities
  putStrLn $ "Доступно потоків: " ++ show numCapabilities
  
  -- Створюємо канал для результатів
  resultsChan <- newChan
  let chunkSize = max 1 (length files `div` numCapabilities)
      chunks = chunksOf chunkSize files
  
  -- Запускаємо потоки для обробки чанків
  mapM_ (forkIO . processChunk resultsChan) chunks
  
  -- Збираємо результати
  archive <- collectResults resultsChan (length files)
  
  saveArchive archivePath archive
  let (origSize, compSize, ratio) = archiveStats archive
  putStrLn "Архів успішно створено!"
  putStrLn $ "Оригінальний розмір: " ++ show origSize ++ " байт"
  putStrLn $ "Стиснений розмір: " ++ show compSize ++ " байт"
  putStrLn $ "Коефіцієнт стиснення: " ++ show ratio ++ "%"
  where
    processChunk :: Chan (Maybe ArchiveEntry) -> [FilePath] -> IO ()
    processChunk chan chunk = do
      results <- mapM processFile chunk
      mapM_ (writeChan chan) results
    
    processFile :: FilePath -> IO (Maybe ArchiveEntry)
    processFile filePath = do
      exists <- doesFileExist filePath
      if not exists
        then return Nothing
        else do
          content <- BS.readFile filePath
          let originalSize' = BS.length content
              (tree', compressed, bitCount') = compress content
              compressedSize' = BS.length compressed
          return $ Just $ ArchiveEntry 
            { entryPath = filePath
            , entryOriginalSize = originalSize'
            , entryCompressedSize = compressedSize'
            , entryBitCount = bitCount'
            , entryTree = tree'
            , entryData = compressed
            }
    
    collectResults :: Chan (Maybe ArchiveEntry) -> Int -> IO [ArchiveEntry]
    collectResults chan count = do
      results <- replicateM count (readChan chan)
      return $ catMaybes results
    
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

extractArchiveFromFile :: FilePath -> FilePath -> IO ()
extractArchiveFromFile archivePath outputDir = do
  putStrLn $ "Розпакування архіву " ++ archivePath
  -- Для повної реалізації потрібен правильний парсинг архіву
  putStrLn "Розпакування потребує повної реалізації парсингу архіву"

