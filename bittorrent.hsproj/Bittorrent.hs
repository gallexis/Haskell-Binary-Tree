{-# LANGUAGE OverloadedStrings #-}

-- This attoparsec module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or ISO-8859-15.
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Control.Applicative
import Data.Word
import Data.Time


-----------------------
------ SETTINGS -------
-----------------------

-- | File where the log is stored.
logFile :: FilePath
logFile = "sellings.log"

-----------------------
-------- TYPES --------
-----------------------

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving Show


data LogEntry =
  LogEntry { -- A local time contains the date and the time of the day.
             -- For example: 2013-06-29 11:16:23.
             entryTime :: LocalTime
           , entryIP   :: IP
           , entryProduct   :: Product
             } deriving Show
             
-- | Type synonym of a list of log entries.
type Log = [LogEntry] 


-----------------------
------- PARSING -------
-----------------------
timeParser :: Parser LocalTime
timeParser = do
  d  <- count 2 digit
  char '-'
  mm <- count 2 digit
  char '-'
  y  <- count 4 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                } 

productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)
 
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4
  

logEntryParser :: Parser LogEntry
logEntryParser = do
    date <- timeParser
    char ' '
    ip <- parseIP
    char ' '
    product <- productParser
    return $  LogEntry{ entryTime = date,
                        entryIP   = ip,         
                        entryProduct = product }
    

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

main :: IO ()
main = B.readFile logFile >>= print . parseOnly logParser
