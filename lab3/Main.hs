module Main where

import System.Environment
import System.IO
import Text.Printf (printf)
import Types
import TextProcessing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> processTextFile filePath
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "Використання:"
  putStrLn "  text-processor <шлях_до_файлу>"
  putStrLn ""
  putStrLn "Приклад:"
  putStrLn "  text-processor textbook.txt"

processTextFile :: FilePath -> IO ()
processTextFile filePath = do
  putStrLn $ "Обробка файлу: " ++ filePath
  putStrLn ""
  
  result <- readTextFromFile filePath
  case result of
    Left err -> putStrLn $ "Помилка: " ++ err
    Right text -> do
      putStrLn "=== Статистика тексту ==="
      let (totalWords, uniqueWords, totalSentences, avgRatio) = textStatistics text
      putStrLn $ "Всього слів: " ++ show totalWords
      putStrLn $ "Унікальних слів: " ++ show uniqueWords
      putStrLn $ "Пропозицій: " ++ show totalSentences
      putStrLn $ "Середня частка голосних: " ++ printf "%.2f%%" (avgRatio * 100)
      putStrLn ""
      
      putStrLn "=== Слова, відсортовані за зростанням частки голосних ==="
      let sortedWords = sortWordsByVowelRatio (textWords text)
      printWordsWithRatios sortedWords
      putStrLn ""
      
      putStrLn "=== Топ-20 слів з найменшою часткою голосних ==="
      let top20 = take 20 sortedWords
      printWordsWithRatios top20
      putStrLn ""
      
      putStrLn "=== Топ-20 слів з найбільшою часткою голосних ==="
      let top20Max = take 20 $ reverse sortedWords
      printWordsWithRatios top20Max
      putStrLn ""
      
      putStrLn "=== Унікальні слова, відсортовані за часткою голосних ==="
      let uniqueSorted = sortUniqueWordsByVowelRatio (textWords text)
      putStrLn $ "Кількість унікальних слів: " ++ show (length uniqueSorted)
      printWordsWithRatios uniqueSorted

