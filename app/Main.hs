data Term a =
    Constant a |
    LinearTerm [a] a |
    IsolatedVariable |
    Max [Term a] |
    Min [Term a]
    deriving Show

-- scale :: Num a => a -> Term a -> Term a
-- scale f (Constant a) = Constant (f * a)
-- scale f (LinearTerm as a) = LinearTerm (map (*f) as) a

-- neg :: Num a => Term a -> Term a
-- neg = scale (-1)

-- add :: Num a => Term a -> Term a -> Term a
-- add (Constant a) (Constant b) = Constant (a + b)
-- add (Constant a) (LinearTerm bs b) = LinearTerm bs (a + b)
-- add (LinearTerm as a) (Constant b) = LinearTerm as (a + b)
-- add (LinearTerm as a) (LinearTerm bs b)
--     | length as /= length bs = error "Can only add linear terms with equal amount of coefficients"
--     | otherwise = LinearTerm (zipWith (+) as bs) (a + b)

-- sub :: Num a => Term a -> Term a -> Term a
-- sub a b = a `add` neg b


data Inequality a = Inequality (Term a) (Term a)
    deriving Show

normalize :: Num a => Inequality a -> Inequality a
normalize (Inequality (LinearTerm as a) (LinearTerm bs b)) =
    Inequality (LinearTerm (zipWith (-) as bs) 0) (Constant (b - a))
normalize (Inequality (Constant a) (Constant b)) =
    Inequality (LinearTerm [] 0) (Constant (b - a))
normalize (Inequality (LinearTerm as a) (Constant b)) =
    Inequality (LinearTerm as 0) (Constant (b - a))
normalize (Inequality (Constant a) (LinearTerm bs b)) =
    Inequality (LinearTerm (map negate bs) 0) (Constant (b - a))



isolateFirst :: (Ord a, Fractional a) => Inequality a -> Inequality a
isolateFirst (Inequality (LinearTerm (a1 : as) 0) (Constant b))
    | a1 > 0 = Inequality IsolatedVariable (LinearTerm as' b')
    | a1 == 0 = Inequality (LinearTerm as 0) (Constant b)
    | a1 < 0 = Inequality (LinearTerm as' b') IsolatedVariable
    where
        as' = map (negate . (/a1)) as
        b' = b / a1

reduce :: [Inequality a] -> [Inequality a]
reduce is =
    let
        (gts, zs, lts) = foldr collect ([], [], []) is
    in
        Inequality IsolatedVariable (Min gts) : Inequality (Max lts) IsolatedVariable : zs
    where
        collect (Inequality IsolatedVariable t) (gts, zs, lts) = (t : gts, zs, lts)
        collect (Inequality t IsolatedVariable) (gts, zs, lts) = (gts, zs, t : lts)
        collect i (gts, zs, lts) = (gts, i : zs, lts)

solve :: (Ord a, Fractional a) => [Inequality a] -> [Inequality a]
solve = reduce . map isolateFirst

eliminate :: [Inequality a] -> [Inequality a]
eliminate ((Inequality IsolatedVariable gt) : (Inequality lt IsolatedVariable) : is) =
    Inequality lt gt : is

expand :: Num a => [Inequality a] -> [Inequality a]
expand ((Inequality (Max lts) (Min gts)) : is) =
    [normalize (Inequality lt gt) | lt <- lts, gt <- gts] ++ is


project :: (Ord a, Fractional a) => [Inequality a] -> [Inequality a]
project = expand . eliminate . solve


x :: [Inequality Double]
x =
    [
        Inequality (LinearTerm [-1, 0] 0) (Constant 0),
        Inequality (LinearTerm [1, 2] 0) (Constant 6),
        Inequality (LinearTerm [-1, -1] 0) (Constant (-2)),
        Inequality (LinearTerm [1, -1] 0) (Constant 3),
        Inequality (LinearTerm [0, -1] 0) (Constant 0)
    ]

main :: IO ()
main = do
    print "Hello World"