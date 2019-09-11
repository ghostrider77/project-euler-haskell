data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Enum)


nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay d = succ d


months :: [Int]
months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


calcNrSundaysInAYear :: Bool -> Day -> (Int, Day)
calcNrSundaysInAYear isLeapYear firstDay = foldl processMonths (0, firstDay) $ zip months [1..]
    where
        processMonths :: (Int, Day) -> (Int, Int) -> (Int, Day)
        processMonths (sundays, day) (month, ix) =
            let daysInMonth = if ix == 2 && isLeapYear then month + 1 else month
                nrSundays = if day == Sunday then sundays + 1 else sundays
                firstDayInNextMonth = foldl (\d _ -> nextDay d) day [1..daysInMonth]
            in (nrSundays, firstDayInNextMonth)


countSundays :: Int -> Int -> Int
countSundays startYear endYear = go startYear Tuesday 0
    where
        go :: Int -> Day -> Int -> Int
        go currentYear firstDayInYear nrSundays
            | currentYear > endYear = nrSundays
            | otherwise =
                let isLeapYear = currentYear `mod` 4 == 0 && currentYear `mod` 400 /= 0
                    (nrSundaysInCurrentYear, firstDayInNextYear) = calcNrSundaysInAYear isLeapYear firstDayInYear
                in go (currentYear + 1) firstDayInNextYear (nrSundays + nrSundaysInCurrentYear)


main :: IO()
main = do
    print $ countSundays 1901 2000
