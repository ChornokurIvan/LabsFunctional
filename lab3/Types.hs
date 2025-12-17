{-# LANGUAGE DeriveShow #-}
module Types where

import Data.Char (isLetter, isSpace, isPunctuation, toLower, isDigit)

-- Тип Символ
data Symbol = Letter Char      -- Літера
            | Digit Char       -- Цифра
            | Punctuation Char -- Розділовий знак
            | Space            -- Пробіл
            | Tab              -- Табуляція
            | Newline          -- Новий рядок
            | Other Char       -- Інший символ
            deriving (Eq, Show)

-- Конвертація Char в Symbol
charToSymbol :: Char -> Symbol
charToSymbol c
  | isLetter c = Letter c
  | isDigit c = Digit c
  | isPunctuation c = Punctuation c
  | c == ' ' = Space
  | c == '\t' = Tab
  | c == '\n' || c == '\r' = Newline
  | isSpace c = Space
  | otherwise = Other c

-- Конвертація Symbol в Char
symbolToChar :: Symbol -> Maybe Char
symbolToChar (Letter c) = Just c
symbolToChar (Digit c) = Just c
symbolToChar (Punctuation c) = Just c
symbolToChar Space = Just ' '
symbolToChar Tab = Just '\t'
symbolToChar Newline = Just '\n'
symbolToChar (Other c) = Just c

-- Тип Слово
data Word = Word
  { wordText :: String      -- Текст слова
  , wordSymbols :: [Symbol] -- Символи слова
  } deriving (Eq, Show)

-- Створення слова з рядка
makeWord :: String -> Word
makeWord str = Word str (map charToSymbol str)

-- Отримання тільки літер зі слова
wordLetters :: Word -> [Char]
wordLetters (Word _ symbols) = 
  [c | Letter c <- symbols]

-- Кількість букв у слові
wordLetterCount :: Word -> Int
wordLetterCount = length . wordLetters

-- Тип Розділовий знак
data PunctuationMark = PunctuationMark
  { punctChar :: Char
  , punctType :: PunctType
  } deriving (Eq, Show)

data PunctType = Comma      -- Кома
               | Period     -- Крапка
               | Semicolon  -- Крапка з комою
               | Colon      -- Двокрапка
               | Question   -- Знак питання
               | Exclamation -- Знак оклику
               | Quote      -- Лапки
               | Dash       -- Тире
               | OtherPunct -- Інший розділовий знак
               deriving (Eq, Show)

-- Створення розділового знака
makePunctuationMark :: Char -> PunctuationMark
makePunctuationMark c = PunctuationMark c (classifyPunctuation c)
  where
    classifyPunctuation ',' = Comma
    classifyPunctuation '.' = Period
    classifyPunctuation ';' = Semicolon
    classifyPunctuation ':' = Colon
    classifyPunctuation '?' = Question
    classifyPunctuation '!' = Exclamation
    classifyPunctuation '"' = Quote
    classifyPunctuation '\'' = Quote
    classifyPunctuation '-' = Dash
    classifyPunctuation '—' = Dash
    classifyPunctuation _ = OtherPunct

-- Тип Пропозиція
data Sentence = Sentence
  { sentenceWords :: [Word]              -- Слова пропозиції
  , sentencePunctuation :: [PunctuationMark] -- Розділові знаки
  , sentenceStructure :: SentenceStructure    -- Структура пропозиції
  } deriving (Eq, Show)

data SentenceStructure = SimpleSentence    -- Проста пропозиція
                       | ComplexSentence   -- Складна пропозиція
                       | QuestionSentence  -- Питальна пропозиція
                       | ExclamatorySentence -- Оклична пропозиція
                       deriving (Eq, Show)

-- Створення пропозиції
makeSentence :: [Word] -> [PunctuationMark] -> Sentence
makeSentence words puncts = Sentence words puncts (classifySentence puncts)
  where
    classifySentence [] = SimpleSentence
    classifySentence (PunctuationMark _ Question : _) = QuestionSentence
    classifySentence (PunctuationMark _ Exclamation : _) = ExclamatorySentence
    classifySentence _ = SimpleSentence

-- Тип Абзац
data Paragraph = Paragraph
  { paragraphSentences :: [Sentence]
  } deriving (Eq, Show)

-- Тип Текст
data Text = Text
  { textParagraphs :: [Paragraph]
  , textWords :: [Word]  -- Всі слова тексту
  } deriving (Eq, Show)

-- Голосні букви (українська та англійська)
vowels :: [Char]
vowels = "аеиіоуєюяАЕИІОУЄЮЯaeiouAEIOU"

-- Перевірка, чи є символ голосною
isVowel :: Char -> Bool
isVowel c = toLower c `elem` map toLower vowels

-- Підрахунок голосних у слові
countVowels :: Word -> Int
countVowels word = length $ filter isVowel $ wordLetters word

-- Частка голосних у слові (від 0.0 до 1.0)
vowelRatio :: Word -> Double
vowelRatio word =
  let letterCount = wordLetterCount word
      vowelCount = countVowels word
  in if letterCount > 0
       then fromIntegral vowelCount / fromIntegral letterCount
       else 0.0

