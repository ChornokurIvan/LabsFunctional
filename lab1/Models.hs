{-# LANGUAGE OverloadedStrings #-}
module Models where

import Data.Text (Text)
import Data.Time (Day, TimeOfDay)
import Data.Maybe (Maybe)

-- Типи даних для сутностей системи

data Student = Student
  { studentId :: Int
  , studentName :: Text
  , studentSurname :: Text
  , studentGroup :: Text
  , studentYear :: Int
  } deriving (Eq, Show)

data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Text
  , teacherSurname :: Text
  , teacherDepartment :: Text
  , teacherPosition :: Text
  } deriving (Eq, Show)

data Section = Section
  { sectionId :: Int
  , sectionName :: Text
  , sectionDescription :: Text
  , sectionCapacity :: Int
  } deriving (Eq, Show)

data SectionSchedule = SectionSchedule
  { scheduleId :: Int
  , sectionId :: Int
  , dayOfWeek :: Int  -- 1-7 (Monday-Sunday)
  , startTime :: TimeOfDay
  , endTime :: TimeOfDay
  , location :: Text
  } deriving (Eq, Show)

data Participant = Participant
  { participantId :: Int
  , entityType :: EntityType
  , entityId :: Int  -- ID студента або викладача
  , sectionId :: Int
  , joinDate :: Day
  } deriving (Eq, Show)

data EntityType = StudentType | TeacherType deriving (Eq, Show)

data Competition = Competition
  { competitionId :: Int
  , competitionName :: Text
  , competitionType :: Text
  , competitionDate :: Day
  , competitionLocation :: Text
  , competitionStatus :: Text  -- "planned", "ongoing", "completed"
  } deriving (Eq, Show)

data CompetitionParticipant = CompetitionParticipant
  { compParticipantId :: Int
  , competitionId :: Int
  , entityType :: EntityType
  , entityId :: Int
  , result :: Maybe Text
  , place :: Maybe Int
  } deriving (Eq, Show)

data CompetitionSchedule = CompetitionSchedule
  { compScheduleId :: Int
  , competitionId :: Int
  , eventName :: Text
  , eventTime :: TimeOfDay
  , eventDate :: Day
  } deriving (Eq, Show)

-- Класи для роботи з сутностями

-- Клас для сутностей, які можуть бути збережені в БД
class DatabaseEntity a where
  tableName :: a -> Text
  insertQuery :: a -> Text
  updateQuery :: a -> Text
  deleteQuery :: a -> Text
  selectQuery :: a -> Text

-- Клас для сутностей, які можна відображати
class Showable a where
  display :: a -> Text

-- Клас для сутностей, які можна редагувати
class Editable a where
  createNew :: a -> a
  updateFields :: a -> a -> a

-- Екземпляри для Student
instance DatabaseEntity Student where
  tableName _ = "students"
  insertQuery _ = "INSERT INTO students (name, surname, group_name, year) VALUES (?, ?, ?, ?)"
  updateQuery _ = "UPDATE students SET name = ?, surname = ?, group_name = ?, year = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM students WHERE id = ?"
  selectQuery _ = "SELECT id, name, surname, group_name, year FROM students"

instance Showable Student where
  display s = studentName s <> " " <> studentSurname s <> " (Група: " <> studentGroup s <> ", Курс: " <> show (studentYear s) <> ")"

instance Editable Student where
  createNew s = s { studentId = 0 }
  updateFields old new = new { studentId = studentId old }

-- Екземпляри для Teacher
instance DatabaseEntity Teacher where
  tableName _ = "teachers"
  insertQuery _ = "INSERT INTO teachers (name, surname, department, position) VALUES (?, ?, ?, ?)"
  updateQuery _ = "UPDATE teachers SET name = ?, surname = ?, department = ?, position = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM teachers WHERE id = ?"
  selectQuery _ = "SELECT id, name, surname, department, position FROM teachers"

instance Showable Teacher where
  display t = teacherName t <> " " <> teacherSurname t <> " (" <> teacherPosition t <> ", " <> teacherDepartment t <> ")"

instance Editable Teacher where
  createNew t = t { teacherId = 0 }
  updateFields old new = new { teacherId = teacherId old }

-- Екземпляри для Section
instance DatabaseEntity Section where
  tableName _ = "sections"
  insertQuery _ = "INSERT INTO sections (name, description, capacity) VALUES (?, ?, ?)"
  updateQuery _ = "UPDATE sections SET name = ?, description = ?, capacity = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM sections WHERE id = ?"
  selectQuery _ = "SELECT id, name, description, capacity FROM sections"

instance Showable Section where
  display s = sectionName s <> " (Місць: " <> show (sectionCapacity s) <> ")"

instance Editable Section where
  createNew sec = sec { sectionId = 0 }
  updateFields old new = new { sectionId = sectionId old }

-- Екземпляри для SectionSchedule
instance DatabaseEntity SectionSchedule where
  tableName _ = "section_schedule"
  insertQuery _ = "INSERT INTO section_schedule (section_id, day_of_week, start_time, end_time, location) VALUES (?, ?, ?, ?, ?)"
  updateQuery _ = "UPDATE section_schedule SET section_id = ?, day_of_week = ?, start_time = ?, end_time = ?, location = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM section_schedule WHERE id = ?"
  selectQuery _ = "SELECT id, section_id, day_of_week, start_time, end_time, location FROM section_schedule"

instance Showable SectionSchedule where
  display s = "Розклад секції ID " <> show (sectionId s) <> " (День: " <> show (dayOfWeek s) <> ", Місце: " <> location s <> ")"

instance Editable SectionSchedule where
  createNew ss = ss { scheduleId = 0 }
  updateFields old new = new { scheduleId = scheduleId old }

-- Екземпляри для Participant
instance DatabaseEntity Participant where
  tableName _ = "participants"
  insertQuery _ = "INSERT INTO participants (entity_type, entity_id, section_id, join_date) VALUES (?, ?, ?, ?)"
  updateQuery _ = "UPDATE participants SET entity_type = ?, entity_id = ?, section_id = ?, join_date = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM participants WHERE id = ?"
  selectQuery _ = "SELECT id, entity_type, entity_id, section_id, join_date FROM participants"

instance Showable Participant where
  display p = "Учасник ID " <> show (entityId p) <> " (Тип: " <> show (entityType p) <> ", Секція: " <> show (sectionId p) <> ")"

instance Editable Participant where
  createNew part = part { participantId = 0 }
  updateFields old new = new { participantId = participantId old }

-- Екземпляри для Competition
instance DatabaseEntity Competition where
  tableName _ = "competitions"
  insertQuery _ = "INSERT INTO competitions (name, type, date, location, status) VALUES (?, ?, ?, ?, ?)"
  updateQuery _ = "UPDATE competitions SET name = ?, type = ?, date = ?, location = ?, status = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM competitions WHERE id = ?"
  selectQuery _ = "SELECT id, name, type, date, location, status FROM competitions"

instance Showable Competition where
  display c = competitionName c <> " (" <> competitionType c <> ", Статус: " <> competitionStatus c <> ")"

instance Editable Competition where
  createNew comp = comp { competitionId = 0 }
  updateFields old new = new { competitionId = competitionId old }

-- Екземпляри для CompetitionParticipant
instance DatabaseEntity CompetitionParticipant where
  tableName _ = "competition_participants"
  insertQuery _ = "INSERT INTO competition_participants (competition_id, entity_type, entity_id, result, place) VALUES (?, ?, ?, ?, ?)"
  updateQuery _ = "UPDATE competition_participants SET competition_id = ?, entity_type = ?, entity_id = ?, result = ?, place = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM competition_participants WHERE id = ?"
  selectQuery _ = "SELECT id, competition_id, entity_type, entity_id, result, place FROM competition_participants"

instance Showable CompetitionParticipant where
  display cp = "Учасник змагання ID " <> show (competitionId cp) <> " (Тип: " <> show (entityType cp) <> ", Місце: " <> maybe "N/A" show (place cp) <> ")"

instance Editable CompetitionParticipant where
  createNew cp = cp { compParticipantId = 0 }
  updateFields old new = new { compParticipantId = compParticipantId old }

-- Екземпляри для CompetitionSchedule
instance DatabaseEntity CompetitionSchedule where
  tableName _ = "competition_schedule"
  insertQuery _ = "INSERT INTO competition_schedule (competition_id, event_name, event_time, event_date) VALUES (?, ?, ?, ?)"
  updateQuery _ = "UPDATE competition_schedule SET competition_id = ?, event_name = ?, event_time = ?, event_date = ? WHERE id = ?"
  deleteQuery _ = "DELETE FROM competition_schedule WHERE id = ?"
  selectQuery _ = "SELECT id, competition_id, event_name, event_time, event_date FROM competition_schedule"

instance Showable CompetitionSchedule where
  display cs = "Подія: " <> eventName cs <> " (Змагання ID: " <> show (competitionId cs) <> ")"

instance Editable CompetitionSchedule where
  createNew cs = cs { compScheduleId = 0 }
  updateFields old new = new { compScheduleId = compScheduleId old }

