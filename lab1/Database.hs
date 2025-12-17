{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.Result
import Data.Text (Text, pack, unpack)
import Data.Time (Day, TimeOfDay, defaultTimeLocale, parseTimeM, formatTime)
import Data.Maybe (Maybe, fromMaybe)
import Control.Exception (catch, SomeException)
import Models

-- Параметри підключення до БД
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo
  { connectHost = "localhost"
  , connectPort = 3306
  , connectUser = "root"
  , connectPassword = "password"
  , connectDatabase = "sports_faculty"
  , connectOptions = []
  , connectPath = ""
  , connectSSL = Nothing
  }

-- Створення всіх таблиць БД
createTables :: Connection -> IO ()
createTables conn = do
  createStudentsTable conn
  createTeachersTable conn
  createSectionsTable conn
  createSectionScheduleTable conn
  createParticipantsTable conn
  createCompetitionsTable conn
  createCompetitionParticipantsTable conn
  createCompetitionScheduleTable conn
  putStrLn "Всі таблиці успішно створено!"

-- Таблиця студентів
createStudentsTable :: Connection -> IO ()
createStudentsTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS students (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    surname VARCHAR(100) NOT NULL,
    group_name VARCHAR(50) NOT NULL,
    year INT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця students створена"

-- Таблиця викладачів
createTeachersTable :: Connection -> IO ()
createTeachersTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS teachers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    surname VARCHAR(100) NOT NULL,
    department VARCHAR(100) NOT NULL,
    position VARCHAR(100) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця teachers створена"

-- Таблиця спортивних секцій
createSectionsTable :: Connection -> IO ()
createSectionsTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS sections (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    capacity INT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця sections створена"

-- Таблиця розкладу секцій
createSectionScheduleTable :: Connection -> IO ()
createSectionScheduleTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS section_schedule (
    id INT AUTO_INCREMENT PRIMARY KEY,
    section_id INT NOT NULL,
    day_of_week INT NOT NULL CHECK (day_of_week BETWEEN 1 AND 7),
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    location VARCHAR(200) NOT NULL,
    FOREIGN KEY (section_id) REFERENCES sections(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця section_schedule створена"

-- Таблиця учасників секцій
createParticipantsTable :: Connection -> IO ()
createParticipantsTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS participants (
    id INT AUTO_INCREMENT PRIMARY KEY,
    entity_type ENUM('StudentType', 'TeacherType') NOT NULL,
    entity_id INT NOT NULL,
    section_id INT NOT NULL,
    join_date DATE NOT NULL,
    FOREIGN KEY (section_id) REFERENCES sections(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE KEY unique_participant (entity_type, entity_id, section_id)
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця participants створена"

-- Таблиця змагань
createCompetitionsTable :: Connection -> IO ()
createCompetitionsTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS competitions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    type VARCHAR(100) NOT NULL,
    date DATE NOT NULL,
    location VARCHAR(200) NOT NULL,
    status VARCHAR(50) NOT NULL DEFAULT 'planned',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця competitions створена"

-- Таблиця учасників змагань
createCompetitionParticipantsTable :: Connection -> IO ()
createCompetitionParticipantsTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS competition_participants (
    id INT AUTO_INCREMENT PRIMARY KEY,
    competition_id INT NOT NULL,
    entity_type ENUM('StudentType', 'TeacherType') NOT NULL,
    entity_id INT NOT NULL,
    result TEXT,
    place INT,
    FOREIGN KEY (competition_id) REFERENCES competitions(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця competition_participants створена"

-- Таблиця розкладу змагань
createCompetitionScheduleTable :: Connection -> IO ()
createCompetitionScheduleTable conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS competition_schedule (
    id INT AUTO_INCREMENT PRIMARY KEY,
    competition_id INT NOT NULL,
    event_name VARCHAR(200) NOT NULL,
    event_time TIME NOT NULL,
    event_date DATE NOT NULL,
    FOREIGN KEY (competition_id) REFERENCES competitions(id) ON DELETE CASCADE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4"
  putStrLn "Таблиця competition_schedule створена"

-- Функції для конвертації типів

timeOfDayToText :: TimeOfDay -> Text
timeOfDayToText = pack . formatTime defaultTimeLocale "%H:%M:%S"

textToTimeOfDay :: Text -> Maybe TimeOfDay
textToTimeOfDay = parseTimeM True defaultTimeLocale "%H:%M:%S" . unpack

dayToText :: Day -> Text
dayToText = pack . formatTime defaultTimeLocale "%Y-%m-%d"

textToDay :: Text -> Maybe Day
textToDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

entityTypeToText :: EntityType -> Text
entityTypeToText StudentType = "StudentType"
entityTypeToText TeacherType = "TeacherType"

textToEntityType :: Text -> Maybe EntityType
textToEntityType "StudentType" = Just StudentType
textToEntityType "TeacherType" = Just TeacherType
textToEntityType _ = Nothing

