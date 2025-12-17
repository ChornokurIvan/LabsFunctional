{-# LANGUAGE BangPatterns #-}
module Benchmark where

import System.CPUTime
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Time.Clock
import Archive
import Compression

-- Вимірювання часу виконання
timeIt :: NFData a => IO a -> IO (Double, a)
timeIt action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let timeInSeconds = fromIntegral (end - start) / 1e12
  return (timeInSeconds, result)

-- Бенчмарк послідовного стиснення
benchmarkSequential :: [FilePath] -> IO (Double, Either String Archive)
benchmarkSequential files = timeIt $ createArchiveSequential files

-- Бенчмарк паралельного стиснення
benchmarkParallel :: [FilePath] -> IO (Double, Either String Archive)
benchmarkParallel files = timeIt $ createArchiveParallel files

-- Порівняння продуктивності
comparePerformance :: [FilePath] -> IO ()
comparePerformance files = do
  putStrLn "=== Бенчмарк стиснення файлів ==="
  putStrLn $ "Кількість файлів: " ++ show (length files)
  putStrLn ""
  
  putStrLn "Послідовне стиснення..."
  (seqTime, seqResult) <- benchmarkSequential files
  putStrLn $ "Час: " ++ show seqTime ++ " секунд"
  
  putStrLn ""
  putStrLn "Паралельне стиснення..."
  (parTime, parResult) <- benchmarkParallel files
  putStrLn $ "Час: " ++ show parTime ++ " секунд"
  
  putStrLn ""
  putStrLn "=== Результати ==="
  putStrLn $ "Послідовне: " ++ show seqTime ++ " сек"
  putStrLn $ "Паралельне: " ++ show parTime ++ " сек"
  
  if seqTime > 0
    then do
      let speedup = seqTime / parTime
      let efficiency = speedup / fromIntegral (length files) * 100
      putStrLn $ "Прискорення: " ++ show speedup ++ "x"
      putStrLn $ "Ефективність: " ++ show efficiency ++ "%"
    else putStrLn "Неможливо обчислити прискорення"
  
  case (seqResult, parResult) of
    (Right seqArchive, Right parArchive) -> do
      let (origSize, compSize, ratio) = archiveStats seqArchive
      putStrLn ""
      putStrLn "=== Статистика стиснення ==="
      putStrLn $ "Оригінальний розмір: " ++ show origSize ++ " байт"
      putStrLn $ "Стиснений розмір: " ++ show compSize ++ " байт"
      putStrLn $ "Коефіцієнт стиснення: " ++ show ratio ++ "%"
    _ -> putStrLn "Помилка при створенні архіву"

