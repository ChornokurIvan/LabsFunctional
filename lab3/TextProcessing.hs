{-# LANGUAGE OverloadedStrings #-}
module TextProcessing where

import System.IO
import Data.Char (isLetter, isSpace, isPunctuation, toLower)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Exception (try, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types

-- Нормалізація пробілів: заміна табуляцій та послідовностей пробілів одним пробілом
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map normalizeChar
  where
    normalizeChar '\t' = ' '
    normalizeChar c = c

-- Розбиття тексту на слова
splitIntoWords :: String -> [String]
splitIntoWords text = 
  let normalized = normalizeSpaces text
      words' = words normalized
  in filter (not . null) words'

-- Витягнення слів з тексту
extractWords :: String -> [Word]
extractWords text = 
  let wordsList = splitIntoWords text
  in map makeWord wordsList

-- Розбиття тексту на пропозиції
splitIntoSentences :: String -> [String]
splitIntoSentences text = 
  let normalized = normalizeSpaces text
      -- Розбиваємо по крапці, знаку питання, знаку оклику
      sentences = splitBySentenceEnd normalized
  in filter (not . all isSpace) sentences
  where
    splitBySentenceEnd :: String -> [String]
    splitBySentenceEnd [] = []
    splitBySentenceEnd str = 
      case break (`elem` ".!?") str of
        (before, []) -> [before]
        (before, end:after) -> 
          let sentence = before ++ [end]
              rest = dropWhile isSpace after
          in sentence : splitBySentenceEnd rest

-- Парсинг пропозиції
parseSentence :: String -> Sentence
parseSentence str = 
  let wordsList = extractWords str
      puncts = extractPunctuation str
  in makeSentence wordsList puncts
  where
    extractPunctuation :: String -> [PunctuationMark]
    extractPunctuation = map makePunctuationMark . filter isPunctuation

-- Парсинг абзацу
parseParagraph :: String -> Paragraph
parseParagraph str = 
  let sentences = splitIntoSentences str
  in Paragraph (map parseSentence sentences)

-- Парсинг тексту з файлу
parseText :: String -> Text
parseText content = 
  let normalized = normalizeSpaces content
      paragraphs = splitIntoParagraphs normalized
      allWords = extractWords normalized
  in Text (map parseParagraph paragraphs) allWords
  where
    splitIntoParagraphs :: String -> [String]
    splitIntoParagraphs str = 
      let lines' = lines str
          paragraphs = groupParagraphs lines'
      in filter (not . all isSpace) paragraphs
      where
        groupParagraphs :: [String] -> [String]
        groupParagraphs [] = []
        groupParagraphs lines' = 
          let (para, rest) = break null lines'
              paraText = unlines para
              rest' = dropWhile null rest
          in if null paraText 
             then groupParagraphs rest'
             else paraText : groupParagraphs rest'

-- Читання тексту з файлу
readTextFromFile :: FilePath -> IO (Either String Text)
readTextFromFile filePath = do
  result <- try (readFile filePath) :: IO (Either IOException String)
  case result of
    Left err -> return $ Left $ "Помилка читання файлу: " ++ show err
    Right content -> return $ Right $ parseText content

-- Сортування слів за зростанням частки голосних
sortWordsByVowelRatio :: [Word] -> [Word]
sortWordsByVowelRatio = sortBy (comparing vowelRatio)

-- Сортування слів за зростанням частки голосних (з урахуванням унікальності)
sortUniqueWordsByVowelRatio :: [Word] -> [Word]
sortUniqueWordsByVowelRatio words = 
  let uniqueWords = removeDuplicates words
  in sortWordsByVowelRatio uniqueWords
  where
    removeDuplicates :: [Word] -> [Word]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (\w -> wordText w /= wordText x) xs)

-- Форматування слова з інформацією про частку голосних
formatWordWithRatio :: Word -> String
formatWordWithRatio word = 
  let text = wordText word
      ratio = vowelRatio word
      vowelCount = countVowels word
      letterCount = wordLetterCount word
  in printf "%s (голосних: %d/%d, частка: %.2f%%)" text vowelCount letterCount (ratio * 100)

-- Виведення списку слів з інформацією
printWordsWithRatios :: [Word] -> IO ()
printWordsWithRatios words = do
  mapM_ (putStrLn . formatWordWithRatio) words

-- Статистика тексту
textStatistics :: Text -> (Int, Int, Int, Double)
textStatistics (Text paragraphs allWords) = 
  let totalWords = length allWords
      uniqueWords = length $ removeDuplicates allWords
      totalSentences = sum $ map (length . paragraphSentences) paragraphs
      avgVowelRatio = if totalWords > 0
        then sum (map vowelRatio allWords) / fromIntegral totalWords
        else 0.0
  in (totalWords, uniqueWords, totalSentences, avgVowelRatio)
  where
    removeDuplicates :: [Word] -> [Word]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (\w -> wordText w /= wordText x) xs)

import Text.Printf (printf)
import Control.Exception (try, IOException)

