{-# LANGUAGE OverloadedStrings #-}
module Operations where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.Result
import Data.Text (Text, pack, unpack)
import Data.Time (Day, TimeOfDay)
import Data.Maybe (Maybe, fromMaybe)
import Control.Exception (catch, SomeException)
import Models
import Database

-- CRUD операції для Student

insertStudent :: Connection -> Student -> IO (Maybe Int)
insertStudent conn student = do
  catch (do
    execute conn (insertQuery student) 
      (studentName student, studentSurname student, studentGroup student, studentYear student)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateStudent :: Connection -> Student -> IO Bool
updateStudent conn student = do
  catch (do
    execute conn (updateQuery student)
      (studentName student, studentSurname student, studentGroup student, studentYear student, studentId student)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteStudent :: Connection -> Int -> IO Bool
deleteStudent conn id = do
  catch (do
    execute conn "DELETE FROM students WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllStudents :: Connection -> IO [Student]
getAllStudents conn = do
  students <- query_ conn "SELECT id, name, surname, group_name, year FROM students"
  return students

getStudentById :: Connection -> Int -> IO (Maybe Student)
getStudentById conn id = do
  students <- query conn "SELECT id, name, surname, group_name, year FROM students WHERE id = ?" (Only id)
  case students of
    [s] -> return $ Just s
    _ -> return Nothing

-- CRUD операції для Teacher

insertTeacher :: Connection -> Teacher -> IO (Maybe Int)
insertTeacher conn teacher = do
  catch (do
    execute conn (insertQuery teacher)
      (teacherName teacher, teacherSurname teacher, teacherDepartment teacher, teacherPosition teacher)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateTeacher :: Connection -> Teacher -> IO Bool
updateTeacher conn teacher = do
  catch (do
    execute conn (updateQuery teacher)
      (teacherName teacher, teacherSurname teacher, teacherDepartment teacher, teacherPosition teacher, teacherId teacher)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteTeacher :: Connection -> Int -> IO Bool
deleteTeacher conn id = do
  catch (do
    execute conn "DELETE FROM teachers WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllTeachers :: Connection -> IO [Teacher]
getAllTeachers conn = do
  teachers <- query_ conn "SELECT id, name, surname, department, position FROM teachers"
  return teachers

getTeacherById :: Connection -> Int -> IO (Maybe Teacher)
getTeacherById conn id = do
  teachers <- query conn "SELECT id, name, surname, department, position FROM teachers WHERE id = ?" (Only id)
  case teachers of
    [t] -> return $ Just t
    _ -> return Nothing

-- CRUD операції для Section

insertSection :: Connection -> Section -> IO (Maybe Int)
insertSection conn section = do
  catch (do
    execute conn (insertQuery section)
      (sectionName section, sectionDescription section, sectionCapacity section)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateSection :: Connection -> Section -> IO Bool
updateSection conn section = do
  catch (do
    execute conn (updateQuery section)
      (sectionName section, sectionDescription section, sectionCapacity section, sectionId section)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteSection :: Connection -> Int -> IO Bool
deleteSection conn id = do
  catch (do
    execute conn "DELETE FROM sections WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllSections :: Connection -> IO [Section]
getAllSections conn = do
  sections <- query_ conn "SELECT id, name, description, capacity FROM sections"
  return sections

getSectionById :: Connection -> Int -> IO (Maybe Section)
getSectionById conn id = do
  sections <- query conn "SELECT id, name, description, capacity FROM sections WHERE id = ?" (Only id)
  case sections of
    [s] -> return $ Just s
    _ -> return Nothing

-- CRUD операції для SectionSchedule

insertSectionSchedule :: Connection -> SectionSchedule -> IO (Maybe Int)
insertSectionSchedule conn schedule = do
  catch (do
    execute conn (insertQuery schedule)
      (sectionId schedule, dayOfWeek schedule, startTime schedule, endTime schedule, location schedule)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateSectionSchedule :: Connection -> SectionSchedule -> IO Bool
updateSectionSchedule conn schedule = do
  catch (do
    execute conn (updateQuery schedule)
      (sectionId schedule, dayOfWeek schedule, startTime schedule, endTime schedule, location schedule, scheduleId schedule)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteSectionSchedule :: Connection -> Int -> IO Bool
deleteSectionSchedule conn id = do
  catch (do
    execute conn "DELETE FROM section_schedule WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllSectionSchedules :: Connection -> IO [SectionSchedule]
getAllSectionSchedules conn = do
  schedules <- query_ conn "SELECT id, section_id, day_of_week, start_time, end_time, location FROM section_schedule"
  return schedules

-- CRUD операції для Participant

insertParticipant :: Connection -> Participant -> IO (Maybe Int)
insertParticipant conn participant = do
  catch (do
    execute conn (insertQuery participant)
      (entityTypeToText $ entityType participant, entityId participant, sectionId participant, joinDate participant)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateParticipant :: Connection -> Participant -> IO Bool
updateParticipant conn participant = do
  catch (do
    execute conn (updateQuery participant)
      (entityTypeToText $ entityType participant, entityId participant, sectionId participant, joinDate participant, participantId participant)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteParticipant :: Connection -> Int -> IO Bool
deleteParticipant conn id = do
  catch (do
    execute conn "DELETE FROM participants WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllParticipants :: Connection -> IO [Participant]
getAllParticipants conn = do
  participants <- query_ conn "SELECT id, entity_type, entity_id, section_id, join_date FROM participants" :: IO [(Int, Text, Int, Int, Day)]
  return $ map convertParticipant participants
  where
    convertParticipant (id, et, eid, sid, jd) = 
      Participant id (fromMaybe StudentType $ textToEntityType et) eid sid jd

-- CRUD операції для Competition

insertCompetition :: Connection -> Competition -> IO (Maybe Int)
insertCompetition conn competition = do
  catch (do
    execute conn (insertQuery competition)
      (competitionName competition, competitionType competition, competitionDate competition, competitionLocation competition, competitionStatus competition)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateCompetition :: Connection -> Competition -> IO Bool
updateCompetition conn competition = do
  catch (do
    execute conn (updateQuery competition)
      (competitionName competition, competitionType competition, competitionDate competition, competitionLocation competition, competitionStatus competition, competitionId competition)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteCompetition :: Connection -> Int -> IO Bool
deleteCompetition conn id = do
  catch (do
    execute conn "DELETE FROM competitions WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllCompetitions :: Connection -> IO [Competition]
getAllCompetitions conn = do
  competitions <- query_ conn "SELECT id, name, type, date, location, status FROM competitions"
  return competitions

-- CRUD операції для CompetitionParticipant

insertCompetitionParticipant :: Connection -> CompetitionParticipant -> IO (Maybe Int)
insertCompetitionParticipant conn compPart = do
  catch (do
    execute conn (insertQuery compPart)
      (competitionId compPart, entityTypeToText $ entityType compPart, entityId compPart, result compPart, place compPart)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateCompetitionParticipant :: Connection -> CompetitionParticipant -> IO Bool
updateCompetitionParticipant conn compPart = do
  catch (do
    execute conn (updateQuery compPart)
      (competitionId compPart, entityTypeToText $ entityType compPart, entityId compPart, result compPart, place compPart, compParticipantId compPart)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteCompetitionParticipant :: Connection -> Int -> IO Bool
deleteCompetitionParticipant conn id = do
  catch (do
    execute conn "DELETE FROM competition_participants WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllCompetitionParticipants :: Connection -> IO [CompetitionParticipant]
getAllCompetitionParticipants conn = do
  compParts <- query_ conn "SELECT id, competition_id, entity_type, entity_id, result, place FROM competition_participants" :: IO [(Int, Int, Text, Int, Maybe Text, Maybe Int)]
  return $ map convertCompPart compParts
  where
    convertCompPart (id, cid, et, eid, res, pl) = 
      CompetitionParticipant id cid (fromMaybe StudentType $ textToEntityType et) eid res pl

-- CRUD операції для CompetitionSchedule

insertCompetitionSchedule :: Connection -> CompetitionSchedule -> IO (Maybe Int)
insertCompetitionSchedule conn compSchedule = do
  catch (do
    execute conn (insertQuery compSchedule)
      (competitionId compSchedule, eventName compSchedule, eventTime compSchedule, eventDate compSchedule)
    lastId <- lastInsertId conn
    return $ Just lastId)
    (\e -> do
      print (e :: SomeException)
      return Nothing)

updateCompetitionSchedule :: Connection -> CompetitionSchedule -> IO Bool
updateCompetitionSchedule conn compSchedule = do
  catch (do
    execute conn (updateQuery compSchedule)
      (competitionId compSchedule, eventName compSchedule, eventTime compSchedule, eventDate compSchedule, compScheduleId compSchedule)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

deleteCompetitionSchedule :: Connection -> Int -> IO Bool
deleteCompetitionSchedule conn id = do
  catch (do
    execute conn "DELETE FROM competition_schedule WHERE id = ?" (Only id)
    return True)
    (\e -> do
      print (e :: SomeException)
      return False)

getAllCompetitionSchedules :: Connection -> IO [CompetitionSchedule]
getAllCompetitionSchedules conn = do
  schedules <- query_ conn "SELECT id, competition_id, event_name, event_time, event_date FROM competition_schedule"
  return schedules

