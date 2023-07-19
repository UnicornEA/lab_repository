module Lib where

-- | Реализовать функцию @highAndLow@, принимающую на вход строку чисел, разделенных пробелами.
--
-- * Все числа записаны верно, дополнительная проверка не требуется.
-- * Во входной строке всегда будет хотябы одно число.
-- * Строка вывода должна состоять из двух чисел, разделенных пробелами, причем первым идет наибольшее значение.
--
-- Пример:
--
-- prop> highAndLow "1 2 3 4 5" = "5 1"
-- prop> highAndLow "1 2 -3 4 5" = "5 -3"
-- prop> highAndLow "1 9 3 4 -5" = "9 -5"
highAndLow :: String -> String
highAndLow = error "todo: highAndLow"

mymax (x:xs) = foldr max x xs
  where max a b | a > b = a
                | otherwise = b

mymin (x:xs) = foldr min x xs
  where min a b | a < b = a
                | otherwise = b


toStr aList = unwords ( map show( aList)) :: String
toInt aList = map read (words aList) :: [Int]

maxAndMin (x:xs) = toStr ([mymax(x:xs), mymin(x:xs)] )
getMaxAndMin aList= maxAndMin( toInt aList)

-- | Реализовать функцию, которая берет массив слов, объединяет их в предложение и возвращает предложение.
--
-- * Можно игнорировать любую необходимость очистки слов или добавления знаков препинания, но необходимо добавить пробелы между каждыми словами.
-- * В начале и конце предложения пробелов быть не должно!
--
-- Пример:
--
-- prop> smash ["hello", "world", "this", "is", "great"]  = "hello world this is great"
--smash :: [String] -> String
--smash = error "todo: smash"

smash [] = ""
smash [a,b] = a ++" " ++ b
smash aList = head aList ++ " " ++ smash (tail aList)





-- | Реализовать функцию квадратной суммы, чтобы она возводила в квадрат каждое переданное в нее число, а затем суммировала результаты.
--
-- Пример:
--
-- prop> squareSum [1,2,2] = 9
-- prop> squareSum [1,2,3] = 14
--squareSum :: [Integer] -> Integer
--squareSum = error "todo: squareSum"

squareSum [] = 0
squareSum (x:xs) = (x^2) + squareSum xs





-- | Реализовать функцию, которая переворачивает переданную в нее строку
--
-- Пример:
--
-- prop> myReverse "world" = "dlrow"
-- prop> myReverse "this is my srting" = "gnitrs ym si siht"
--myReverse :: String -> String
--myReverse = error "todo myReverse"

myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs ) ++ [x]





-- | Реализовать функцию, которая вычисляет среднее значение.
--
-- * Для пустого массива должно возвращаться @0@
--
-- Пример:
--
-- prop> avg [] = 0
-- prop> avg [1,1,1] = 1
-- prop> avg [1,2,3] = 2
-- prop> avg [1,2,3,4] = 2.5
--avg :: [Float] -> Float
--avg = error "todo: avg"

avg [] = 0
avg aList = summ aList / fromIntegral(length aList)
  where summ [] = 0
        summ(x:xs) =x + summ xs




-- | Реализовать функцию, которая принимает на вход целое неотрицательное число @n@ и
-- возвращает список всех степеней двойки с показателями от @0@ до @n@(включительно)
--
-- Пример:
--
-- prop> powersOfTwo 0 = [1]
-- prop> powersOfTwo 1 = [1,2]
-- prop> powersOfTwo 2 = [1,2,4]
--powersOfTwo :: Int -> [Int]
--powersOfTwo = error "todo: powersOfTwo"


pOfTwo 0 = [1]
pOfTwo n = (2^n) : pOfTwo (n - 1) 
powersOfTwo n = reverse (pOfTwo n)






-- | Дан массив челых чисел.
-- Вернуть массив, где первый элемент - количество положительных чисел, а второй элемент - сумма отрицательных чисел.
--
-- * Если входной массив пуст, вернуть пустой массив.
--
-- Пример:
--
-- prop> countPositivesSumNegatives [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -11, -12, -13, -14, -15] = [10, -65]
--countPositivesSumNegatives :: [Int] -> [Int]
--countPositivesSumNegatives = error "todo: countPositivesSumNegatives"

sumNegative [] = 0
sumNegative (x:xs) =  if x < 0
              then x + (sumNegative xs)
              else sumNegative xs
countEven [] = 0
countEven (x:xs) = if x> 0 
               then 1+countEven xs
               else countEven xs


countEvensumNegative (x:xs) = [countEven (x:xs), sumNegative (x:xs)]




-- | Реализовать фунцкию, которая для неотрицательного числа возвращает его цифры в массиве в обратном порядке
--
-- Пример:
--
-- prop> digitize 348597 = [7,9,5,8,4,3]
--digitize :: Int -> [Int]
--digitize = error "todo: digitize"





-- | Реализовать функцию, которая для положительного числа вернет количество его делителей.
--
-- * Число, подаваемое на вход функции не превосходит @500000@
--
-- Пример:
--
-- prop> divisors 4  = 3 -- 1, 2, 4
-- prop> divisors 5  = 2 -- 1, 5
-- prop> divisors 12 = 6 -- 1, 2, 3, 4, 6, 12
-- prop> divisors 30 = 8 -- 1, 2, 3, 5, 6, 10, 15, 30
--divisors :: Int -> Int
--divisors = error "todo: divisors"

divisors n = length (listMod n)
listMod n = filter (\x -> n `mod` x == 0)[1..n]

-- | Реализовать функцию, которая для заданного массива и граничного значения проверяет, что все элементы массива не превосходят граничное значение.
--
-- Пример:
--
-- prop> smallEnough [66, 101] 200                                       = True
-- prop> smallEnough [78, 117, 110, 99, 104, 117, 107, 115] 100          = False
-- prop> smallEnough [101, 45, 75, 105, 99, 107] 107                     = True
-- prop> smallEnough [80, 117, 115, 104, 45, 85, 112, 115] 120           = True
-- prop> smallEnough [1, 1, 1, 1, 1, 2] 1                                = False
-- prop> smallEnough [78, 33, 22, 44, 88, 9, 6] 87                       = False
-- prop> smallEnough [1, 2, 3, 4, 5, 6, 7, 8, 9] 10                      = True
-- prop> smallEnough [12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12] 12 = True
--smallEnough :: [Int] -> Int -> Bool
--smallEnough = error "todo: smallEnough"

lenghtList (x:xs) = length (x:xs)
listTrue [] _ = 0
listTrue (x:xs) n = if x <= n 
                then 1+listTrue xs n
                else listTrue xs n

smallEnough (x:xs) n = if lenghtList (x:xs) == listTrue (x:xs) n
                       then True
                       else False

-- | Реализовать функцию, которая для двух заданых чисел @a@ и @b@ и операции возвращает результат операции для двух чисел.
--
-- * Числа @a@ и @b@ всегда будут положительными
-- * Число @a@ всегда первое число в операции, а @b@ всегда второе.
-- * Операцией может быть: сложение, вычитание, умножение, деление
--
-- Пример:
--
-- prop> arithmetic 5 2 Add      = 7
-- prop> arithmetic 8 2 Subtract = 6
-- prop> arithmetic 5 2 Multiply = 10
-- prop> arithmetic 8 2 Divide   = 4
arithmetic :: Double  -> Double -> Operation -> Double 
arithmetic = error "todo: smallEnough"

-- | Операция определяется как:
data Operation = Add | Divide | Multiply | Subtract deriving (Eq)

-- | Реализовать функцию, которая для заданного числа @n@ возвращает сумму все чисел не превосходящих @n@, которые кратны 3 или 5
--
-- Пример:
--
-- prop> findSum 5  = 8  -- (3 + 5)
-- prop> findSum 10 = 33 -- (3 + 5 + 6 + 9 + 10)
findSum :: Int -> Int
findSum = error "todo: findSum"

-- | Реализовать функцию, которая принимает на вход список строк и возвращает каждую строку с ее номером
--
-- * Нумерация начинается с @1@
-- * Формат 'n: string'
-- 
-- prop> number [] = []
-- prop> number ["a", "b", "c"] = ["1: a", "2: b", "3: c"]
number :: [String] -> [String]
number = error "todo: number"

-- | Реализовать функцию, которая проверяет на равенство количество символов "x" и "o" в строке.
--
-- * Функция не должна учитывать регистр.
-- * В строке могут содержаться любые символы.
--
-- prop> xo "ooxx"    = true
-- prop> xo "xooxx"   = false
-- prop> xo "ooxXm"   = true
-- prop> xo "zpzpzpp" = true
-- prop> xo "zzoo"    = false
xo :: String -> Bool
xo str = error "todo: xo"

-- | Реализовать функцию, которая принимает на вход присок строк и возвращает только те, в которых ровно 4 символа.
--
-- Пример:
--
-- prop> friend ["Ryan", "Kieran", "Mark"] = ["Ryan", "Mark"]
-- prop> friend ["This", "IS", "enough", "TEst", "CaSe"] = ["This", "TEst", "CaSe"]
friend :: [String] -> [String]
friend = error "todo: friend"

-- | Реализовать функцию, которая для данного слова возвращает символ, находящийся посередине.
--
-- * Если длина символа четная, вернуть 2 средних символа.
--
-- Пример:
--
-- prop> getMiddle "test"    = "es"
-- prop> getMiddle "testing" = "t"
-- prop> getMiddle "middle"  = "dd"
-- prop> getMiddle "A"       = "A"
getMiddle :: String -> String
getMiddle s = error "todo: getMiddle"

-- | Реализовать функцию, которая в строке находит длину самого короткого слова
--
-- * В строке всегда есть слова.
--
-- Пример:
--
-- prop> findShortest "bitcoin take over the world maybe who knows perhaps"                = 3
-- prop> findShortest "turns out random test cases are easier than writing out basic ones" = 3
-- prop> findShortest "lets talk about javascript the best language"                       = 3
-- prop> findShortest "i want to travel the world writing code one day"                    = 1
-- prop> findShortest "Lets all go on holiday somewhere very cold"                         = 2
-- prop> findShortest "Let's travel abroad shall we"                                       = 2
findShortest :: String -> Int
findShortest s = error "todo: find_shortest"

-- | Реализовать функцию, которая порверяет отсортирован ли список и как он отсортирован.
--
-- * В списке всегда есть значения.
-- * "yes, ascending" - если числа в списке отсортированы по возрастанию
-- * "yes, descending" - если числа в списке отсортированы по убыванию
-- * "no" - иначе
-- 
-- Пример:
--
-- prop> isSortedAndHow [1,2,3,4,5] = "yes, ascending"
-- prop> isSortedAndHow [1,2,2,4,5] = "yes, ascending"
-- prop> isSortedAndHow [5,4,3,2,1] = "yes, descending"
-- prop> isSortedAndHow [5,4,3,3,1] = "yes, descending"
-- prop> isSortedAndHow [1,2,1,2,1] = "no"
isSortedAndHow :: String -> Int
isSortedAndHow s = error "todo: find_shortest"

-- | Реализовать функцию, которая проверяет, что в заданной строке нет повторяющихся символов
--
-- * В пустой строке повторений нет.
-- * Регистр букв не учитывается.
-- * В строке содержатся только буквы.
-- 
-- Пример:
--
-- prop> isIsogram "Dermatoglyphics" = true
-- prop> isIsogram "aba"             = false
-- prop> isIsogram "moOse"           = false
isIsogram :: String -> Bool
isIsogram = error "todo: isIsogram"

-- | Реализовать функцию, которая удаляет наименьшее значение из списка
--
-- * Если таких значений несколько нужно удалить только первое.
-- * Нельзя изменять порядко элементов в списке
-- * Для пустого списка вернуть пустой список.
-- 
-- Пример:
--
-- prop> removeSmallest [1,2,3,4,5] = [2,3,4,5]
-- prop> removeSmallest [5,3,2,1,4] = [5,3,2,4]
-- prop> removeSmallest [2,2,1,2,1] = [2,2,2,1]
removeSmallest :: [Int] -> [Int]
removeSmallest xs = error "todo: removeSmallest"