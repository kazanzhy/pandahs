module Demo where

----------------------------------------------------------------------------------------------------------------
import Data.Time.Clock
import Data.Time.Format
import System.Locale

(&) x f = f x

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString e = e & timestamp & timeToString ++ ": " ++ e & logLevel & logLevelToString ++ ": " ++ e & message
----------------------------------------------------------------------------------------------------------------
data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2= person2 {lastName = person1 & lastName}

x & f = f x
----------------------------------------------------------------------------------------------------------------
data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p @ (Person {firstName = fn}) = p {firstName = if length fn > 2 then head fn : "." else fn}
----------------------------------------------------------------------------------------------------------------

