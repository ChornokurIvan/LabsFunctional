{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.MySQL.Simple
import Data.Text (Text, pack, unpack)
import Data.Time (Day, TimeOfDay, fromGregorian, TimeOfDay(..))
import Data.Maybe (Maybe(..))
import System.IO
import Control.Exception (catch, SomeException)
import Models
import Database
import Operations

main :: IO ()
main = do
  putStrLn "=== Інформаційна система 'Спорт на факультеті' ==="
  putStrLn ""
  
  -- Підключення до БД
  conn <- connect defaultConnectInfo `catch` (\e -> do
    putStrLn "Помилка підключення до БД. Перевірте налаштування в Database.hs"
    print (e :: SomeException)
    error "Не вдалося підключитися до БД")
  
  -- Створення таблиць (якщо не існують)
  putStrLn "Ініціалізація бази даних..."
  createTables conn
  putStrLn ""
  
  -- Головне меню
  mainMenu conn
  
  close conn
  putStrLn "Дякуємо за використання системи!"

mainMenu :: Connection -> IO ()
mainMenu conn = do
  putStrLn "\n=== ГОЛОВНЕ МЕНЮ ==="
  putStrLn "1. Робота зі студентами"
  putStrLn "2. Робота з викладачами"
  putStrLn "3. Робота зі спортивними секціями"
  putStrLn "4. Робота з розкладом секцій"
  putStrLn "5. Робота з учасниками секцій"
  putStrLn "6. Робота зі змаганнями"
  putStrLn "7. Робота з учасниками змагань"
  putStrLn "8. Робота з розкладом змагань"
  putStrLn "0. Вихід"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> studentMenu conn
    "2" -> teacherMenu conn
    "3" -> sectionMenu conn
    "4" -> sectionScheduleMenu conn
    "5" -> participantMenu conn
    "6" -> competitionMenu conn
    "7" -> competitionParticipantMenu conn
    "8" -> competitionScheduleMenu conn
    "0" -> return ()
    _ -> do
      putStrLn "Невірний вибір!"
      mainMenu conn

-- Меню для студентів
studentMenu :: Connection -> IO ()
studentMenu conn = do
  putStrLn "\n=== РОБОТА ЗІ СТУДЕНТАМИ ==="
  putStrLn "1. Додати студента"
  putStrLn "2. Переглянути всіх студентів"
  putStrLn "3. Знайти студента за ID"
  putStrLn "4. Оновити студента"
  putStrLn "5. Видалити студента"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Ім'я: "
      name <- getLine
      putStr "Прізвище: "
      surname <- getLine
      putStr "Група: "
      group <- getLine
      putStr "Курс: "
      yearStr <- getLine
      let year = read yearStr :: Int
      let student = Student 0 (pack name) (pack surname) (pack group) year
      result <- insertStudent conn student
      case result of
        Just id -> putStrLn $ "Студента додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання студента"
      studentMenu conn
    "2" -> do
      students <- getAllStudents conn
      mapM_ (\s -> putStrLn $ show s) students
      studentMenu conn
    "3" -> do
      putStr "Введіть ID: "
      idStr <- getLine
      let id = read idStr :: Int
      student <- getStudentById conn id
      case student of
        Just s -> putStrLn $ show s
        Nothing -> putStrLn "Студента не знайдено"
      studentMenu conn
    "4" -> do
      putStr "Введіть ID студента для оновлення: "
      idStr <- getLine
      let id = read idStr :: Int
      student <- getStudentById conn id
      case student of
        Just old -> do
          putStr "Нове ім'я: "
          name <- getLine
          putStr "Нове прізвище: "
          surname <- getLine
          putStr "Нова група: "
          group <- getLine
          putStr "Новий курс: "
          yearStr <- getLine
          let year = read yearStr :: Int
          let updated = Student id (pack name) (pack surname) (pack group) year
          success <- updateStudent conn updated
          if success then putStrLn "Студента оновлено" else putStrLn "Помилка оновлення"
        Nothing -> putStrLn "Студента не знайдено"
      studentMenu conn
    "5" -> do
      putStr "Введіть ID для видалення: "
      idStr <- getLine
      let id = read idStr :: Int
      success <- deleteStudent conn id
      if success then putStrLn "Студента видалено" else putStrLn "Помилка видалення"
      studentMenu conn
    "0" -> mainMenu conn
    _ -> studentMenu conn

-- Меню для викладачів
teacherMenu :: Connection -> IO ()
teacherMenu conn = do
  putStrLn "\n=== РОБОТА З ВИКЛАДАЧАМИ ==="
  putStrLn "1. Додати викладача"
  putStrLn "2. Переглянути всіх викладачів"
  putStrLn "3. Знайти викладача за ID"
  putStrLn "4. Оновити викладача"
  putStrLn "5. Видалити викладача"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Ім'я: "
      name <- getLine
      putStr "Прізвище: "
      surname <- getLine
      putStr "Кафедра: "
      dept <- getLine
      putStr "Посада: "
      pos <- getLine
      let teacher = Teacher 0 (pack name) (pack surname) (pack dept) (pack pos)
      result <- insertTeacher conn teacher
      case result of
        Just id -> putStrLn $ "Викладача додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання викладача"
      teacherMenu conn
    "2" -> do
      teachers <- getAllTeachers conn
      mapM_ (\t -> putStrLn $ show t) teachers
      teacherMenu conn
    "3" -> do
      putStr "Введіть ID: "
      idStr <- getLine
      let id = read idStr :: Int
      teacher <- getTeacherById conn id
      case teacher of
        Just t -> putStrLn $ show t
        Nothing -> putStrLn "Викладача не знайдено"
      teacherMenu conn
    "4" -> do
      putStr "Введіть ID викладача для оновлення: "
      idStr <- getLine
      let id = read idStr :: Int
      teacher <- getTeacherById conn id
      case teacher of
        Just old -> do
          putStr "Нове ім'я: "
          name <- getLine
          putStr "Нове прізвище: "
          surname <- getLine
          putStr "Нова кафедра: "
          dept <- getLine
          putStr "Нова посада: "
          pos <- getLine
          let updated = Teacher id (pack name) (pack surname) (pack dept) (pack pos)
          success <- updateTeacher conn updated
          if success then putStrLn "Викладача оновлено" else putStrLn "Помилка оновлення"
        Nothing -> putStrLn "Викладача не знайдено"
      teacherMenu conn
    "5" -> do
      putStr "Введіть ID для видалення: "
      idStr <- getLine
      let id = read idStr :: Int
      success <- deleteTeacher conn id
      if success then putStrLn "Викладача видалено" else putStrLn "Помилка видалення"
      teacherMenu conn
    "0" -> mainMenu conn
    _ -> teacherMenu conn

-- Меню для секцій
sectionMenu :: Connection -> IO ()
sectionMenu conn = do
  putStrLn "\n=== РОБОТА ЗІ СПОРТИВНИМИ СЕКЦІЯМИ ==="
  putStrLn "1. Додати секцію"
  putStrLn "2. Переглянути всі секції"
  putStrLn "3. Знайти секцію за ID"
  putStrLn "4. Оновити секцію"
  putStrLn "5. Видалити секцію"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Назва: "
      name <- getLine
      putStr "Опис: "
      desc <- getLine
      putStr "Місткість: "
      capStr <- getLine
      let cap = read capStr :: Int
      let section = Section 0 (pack name) (pack desc) cap
      result <- insertSection conn section
      case result of
        Just id -> putStrLn $ "Секцію додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання секції"
      sectionMenu conn
    "2" -> do
      sections <- getAllSections conn
      mapM_ (\s -> putStrLn $ show s) sections
      sectionMenu conn
    "3" -> do
      putStr "Введіть ID: "
      idStr <- getLine
      let id = read idStr :: Int
      section <- getSectionById conn id
      case section of
        Just s -> putStrLn $ show s
        Nothing -> putStrLn "Секцію не знайдено"
      sectionMenu conn
    "4" -> do
      putStr "Введіть ID секції для оновлення: "
      idStr <- getLine
      let id = read idStr :: Int
      section <- getSectionById conn id
      case section of
        Just old -> do
          putStr "Нова назва: "
          name <- getLine
          putStr "Новий опис: "
          desc <- getLine
          putStr "Нова місткість: "
          capStr <- getLine
          let cap = read capStr :: Int
          let updated = Section id (pack name) (pack desc) cap
          success <- updateSection conn updated
          if success then putStrLn "Секцію оновлено" else putStrLn "Помилка оновлення"
        Nothing -> putStrLn "Секцію не знайдено"
      sectionMenu conn
    "5" -> do
      putStr "Введіть ID для видалення: "
      idStr <- getLine
      let id = read idStr :: Int
      success <- deleteSection conn id
      if success then putStrLn "Секцію видалено" else putStrLn "Помилка видалення"
      sectionMenu conn
    "0" -> mainMenu conn
    _ -> sectionMenu conn

-- Меню для розкладу секцій
sectionScheduleMenu :: Connection -> IO ()
sectionScheduleMenu conn = do
  putStrLn "\n=== РОБОТА З РОЗКЛАДОМ СЕКЦІЙ ==="
  putStrLn "1. Додати розклад"
  putStrLn "2. Переглянути всі розклади"
  putStrLn "3. Оновити розклад"
  putStrLn "4. Видалити розклад"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "ID секції: "
      secIdStr <- getLine
      let secId = read secIdStr :: Int
      putStr "День тижня (1-7): "
      dayStr <- getLine
      let day = read dayStr :: Int
      putStr "Час початку (HH:MM:SS): "
      startStr <- getLine
      let startTime = read startStr :: TimeOfDay
      putStr "Час закінчення (HH:MM:SS): "
      endStr <- getLine
      let endTime = read endStr :: TimeOfDay
      putStr "Місце проведення: "
      loc <- getLine
      let schedule = SectionSchedule 0 secId day startTime endTime (pack loc)
      result <- insertSectionSchedule conn schedule
      case result of
        Just id -> putStrLn $ "Розклад додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання розкладу"
      sectionScheduleMenu conn
    "2" -> do
      schedules <- getAllSectionSchedules conn
      mapM_ (\s -> putStrLn $ show s) schedules
      sectionScheduleMenu conn
    "0" -> mainMenu conn
    _ -> sectionScheduleMenu conn

-- Меню для учасників секцій
participantMenu :: Connection -> IO ()
participantMenu conn = do
  putStrLn "\n=== РОБОТА З УЧАСНИКАМИ СЕКЦІЙ ==="
  putStrLn "1. Додати учасника"
  putStrLn "2. Переглянути всіх учасників"
  putStrLn "3. Видалити учасника"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Тип (Student/Teacher): "
      typeStr <- getLine
      let entType = if typeStr == "Student" then StudentType else TeacherType
      putStr "ID студента/викладача: "
      entIdStr <- getLine
      let entId = read entIdStr :: Int
      putStr "ID секції: "
      secIdStr <- getLine
      let secId = read secIdStr :: Int
      putStr "Дата приєднання (YYYY-MM-DD): "
      dateStr <- getLine
      let date = read dateStr :: Day
      let participant = Participant 0 entType entId secId date
      result <- insertParticipant conn participant
      case result of
        Just id -> putStrLn $ "Учасника додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання учасника"
      participantMenu conn
    "2" -> do
      participants <- getAllParticipants conn
      mapM_ (\p -> putStrLn $ show p) participants
      participantMenu conn
    "3" -> do
      putStr "Введіть ID для видалення: "
      idStr <- getLine
      let id = read idStr :: Int
      success <- deleteParticipant conn id
      if success then putStrLn "Учасника видалено" else putStrLn "Помилка видалення"
      participantMenu conn
    "0" -> mainMenu conn
    _ -> participantMenu conn

-- Меню для змагань
competitionMenu :: Connection -> IO ()
competitionMenu conn = do
  putStrLn "\n=== РОБОТА ЗІ ЗМАГАННЯМИ ==="
  putStrLn "1. Додати змагання"
  putStrLn "2. Переглянути всі змагання"
  putStrLn "3. Оновити змагання"
  putStrLn "4. Видалити змагання"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "Назва: "
      name <- getLine
      putStr "Тип: "
      typeStr <- getLine
      putStr "Дата (YYYY-MM-DD): "
      dateStr <- getLine
      let date = read dateStr :: Day
      putStr "Місце проведення: "
      loc <- getLine
      putStr "Статус (planned/ongoing/completed): "
      status <- getLine
      let competition = Competition 0 (pack name) (pack typeStr) date (pack loc) (pack status)
      result <- insertCompetition conn competition
      case result of
        Just id -> putStrLn $ "Змагання додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання змагання"
      competitionMenu conn
    "2" -> do
      competitions <- getAllCompetitions conn
      mapM_ (\c -> putStrLn $ show c) competitions
      competitionMenu conn
    "0" -> mainMenu conn
    _ -> competitionMenu conn

-- Меню для учасників змагань
competitionParticipantMenu :: Connection -> IO ()
competitionParticipantMenu conn = do
  putStrLn "\n=== РОБОТА З УЧАСНИКАМИ ЗМАГАНЬ ==="
  putStrLn "1. Додати учасника змагання"
  putStrLn "2. Переглянути всіх учасників"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "ID змагання: "
      compIdStr <- getLine
      let compId = read compIdStr :: Int
      putStr "Тип (Student/Teacher): "
      typeStr <- getLine
      let entType = if typeStr == "Student" then StudentType else TeacherType
      putStr "ID студента/викладача: "
      entIdStr <- getLine
      let entId = read entIdStr :: Int
      putStr "Результат (опціонально): "
      res <- getLine
      let result = if null res then Nothing else Just (pack res)
      putStr "Місце (опціонально): "
      placeStr <- getLine
      let place = if null placeStr then Nothing else Just (read placeStr :: Int)
      let compPart = CompetitionParticipant 0 compId entType entId result place
      result <- insertCompetitionParticipant conn compPart
      case result of
        Just id -> putStrLn $ "Учасника додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання учасника"
      competitionParticipantMenu conn
    "2" -> do
      compParts <- getAllCompetitionParticipants conn
      mapM_ (\cp -> putStrLn $ show cp) compParts
      competitionParticipantMenu conn
    "0" -> mainMenu conn
    _ -> competitionParticipantMenu conn

-- Меню для розкладу змагань
competitionScheduleMenu :: Connection -> IO ()
competitionScheduleMenu conn = do
  putStrLn "\n=== РОБОТА З РОЗКЛАДОМ ЗМАГАНЬ ==="
  putStrLn "1. Додати подію до розкладу"
  putStrLn "2. Переглянути всі події"
  putStrLn "0. Назад"
  putStr "Виберіть опцію: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      putStr "ID змагання: "
      compIdStr <- getLine
      let compId = read compIdStr :: Int
      putStr "Назва події: "
      eventName <- getLine
      putStr "Час (HH:MM:SS): "
      timeStr <- getLine
      let eventTime = read timeStr :: TimeOfDay
      putStr "Дата (YYYY-MM-DD): "
      dateStr <- getLine
      let eventDate = read dateStr :: Day
      let compSchedule = CompetitionSchedule 0 compId (pack eventName) eventTime eventDate
      result <- insertCompetitionSchedule conn compSchedule
      case result of
        Just id -> putStrLn $ "Подію додано з ID: " ++ show id
        Nothing -> putStrLn "Помилка додавання події"
      competitionScheduleMenu conn
    "2" -> do
      schedules <- getAllCompetitionSchedules conn
      mapM_ (\s -> putStrLn $ show s) schedules
      competitionScheduleMenu conn
    "0" -> mainMenu conn
    _ -> competitionScheduleMenu conn

