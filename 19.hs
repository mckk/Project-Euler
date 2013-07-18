import Data.Time
import Data.Time.Calendar.WeekDate

start = fromGregorian 1901 1 1

end = fromGregorian 2000 12 31

days n = n : days (addDays 1 n)

listofdays = takeWhile (< end) (days start)

isSunday d = 7 == day
  where
    (_, _, day) = toWeekDate d

isFirst d = 1 == day
  where
    (_, _, day) = toGregorian d

res = length $ filter (\x -> isSunday x && isFirst x) listofdays