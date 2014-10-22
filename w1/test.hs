main = do
    putStrLn ("AA")
    putStrLn . show . sum $ [1..12]
    let
        f x = do
            (2 * x)
    putStrLn . show . f $ 20
    let
        s [] = []
        s (x:xs) = [[x]] ++ s xs
    putStrLn . show . s $ [1..12]
    let
        qsort [] = []
        qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
                       where
                         smaller = [a | a <- xs , a <= x]
                         bigger = [a | a <- xs , a > x]
    putStrLn . show . qsort $ [1, 2, 44, 23, 12]
    let
      product [] = 1
      product (x:xs) = x * product xs
    putStrLn . show . product $ [2, 3, 4]
    let
        qsort_reversed [] = []
        qsort_reversed (x:xs) = qsort_reversed bigger ++ [x] ++ qsort_reversed smaller
                       where
                         smaller = [a | a <- xs , a <= x]
                         bigger = [a | a <- xs , a > x]
    putStrLn . show . qsort_reversed $ [1, 2, 44, 23, 12]

