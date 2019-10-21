double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs where
    a = 10
    xs = [1,2,3,4,5]


last2 xs = xs !! (length xs - 1)

shuffle [] = []
shuffle xs = tail xs ++ [head xs]

f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
    where
        ys = [a | a <- xs, a <= x] 
        zs = [b | b <- xs, b > x]

test xs = [double a | a <- xs, a <= 1]


abc [] = []
abc (x:xs) = f ys ++ [x] ++ f zs
    where
    ys = [a | a <- xs, a < x] 
    zs = [b | b <- xs, b > x]