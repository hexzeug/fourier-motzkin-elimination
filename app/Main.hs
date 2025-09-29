data Term =
    Constant Rational |
    LinearTerm [Rational] Rational |
    IsolatedVariable |
    Max [Term] |
    Min [Term]
    deriving Show

data Equation = Equation Term Term
    deriving Show

data Inequality = Inequality Term Term
    deriving Show

data Optimum = Minimum | Maximum
    deriving (Show, Read, Eq)

isolateLast :: Equation -> Equation
isolateLast (Equation (LinearTerm as 0) IsolatedVariable) =
    Equation (LinearTerm as' 0) IsolatedVariable
    where
        as' = map (/ negate (last as)) (init as ++ [-1])

normalize :: Inequality -> Inequality
normalize (Inequality (LinearTerm as a) (LinearTerm bs b)) =
    Inequality (LinearTerm (zipWith (-) as bs) 0) (Constant (b - a))
normalize (Inequality (Constant a) (Constant b)) =
    Inequality (LinearTerm [] 0) (Constant (b - a))
normalize (Inequality (LinearTerm as a) (Constant b)) =
    Inequality (LinearTerm as 0) (Constant (b - a))
normalize (Inequality (Constant a) (LinearTerm bs b)) =
    Inequality (LinearTerm (map negate bs) 0) (Constant (b - a))



isolateFirst :: Inequality -> Inequality
isolateFirst (Inequality (LinearTerm (a1 : as) 0) (Constant b))
    | a1 > 0 = Inequality IsolatedVariable (LinearTerm as' b')
    | a1 == 0 = Inequality (LinearTerm as 0) (Constant b)
    | a1 < 0 = Inequality (LinearTerm as' b') IsolatedVariable
    where
        as' = map (negate . (/a1)) as
        b' = b / a1

reduce :: [Inequality] -> [Inequality]
reduce is =
    let
        (gts, zs, lts) = foldr collect ([], [], []) is
    in
        Inequality IsolatedVariable (Min gts) : Inequality (Max lts) IsolatedVariable : zs
    where
        collect (Inequality IsolatedVariable t) (gts, zs, lts) = (t : gts, zs, lts)
        collect (Inequality t IsolatedVariable) (gts, zs, lts) = (gts, zs, t : lts)
        collect i (gts, zs, lts) = (gts, i : zs, lts)

solveForFirst :: [Inequality] -> [Inequality]
solveForFirst = reduce . map isolateFirst

eliminate :: [Inequality] -> [Inequality]
eliminate ((Inequality IsolatedVariable gt) : (Inequality lt IsolatedVariable) : is) =
    Inequality lt gt : is

expand :: [Inequality] -> [Inequality]
expand ((Inequality (Max lts) (Min gts)) : is) =
    [normalize (Inequality lt gt) | lt <- lts, gt <- gts] ++ is

evaluate :: Term -> Rational
evaluate (Constant a) = a
evaluate (Max ts) = maximum (map evaluate ts)
evaluate (Min ts) = minimum (map evaluate ts)

substitute :: [Rational] -> Term -> Term
substitute xs (LinearTerm as a) = Constant (a + sum (zipWith (*) xs as))
substitute xs (Max ts) = Max (map (substitute xs) ts)
substitute xs (Min ts) = Min (map (substitute xs) ts)

optimizeLast :: Optimum -> [Inequality] -> [Rational]
optimizeLast opt (Inequality (LinearTerm [] 0) _ : _) = []
optimizeLast opt is =
    let
        solutionInFirst = solveForFirst is
        xs = (optimizeLast opt . expand . eliminate) solutionInFirst
        ((Inequality _ upperBound) : (Inequality lowerBound _) : _) = solutionInFirst
    in
        case opt of
            Minimum -> (evaluate . substitute xs) lowerBound : xs
            Maximum -> (evaluate . substitute xs) upperBound : xs


x :: [Inequality]
x =
    [
        Inequality (LinearTerm [-1, 0] 0) (Constant 0),
        Inequality (LinearTerm [1, 2] 0) (Constant 6),
        Inequality (LinearTerm [-1, -1] 0) (Constant (-2)),
        Inequality (LinearTerm [1, -1] 0) (Constant 3),
        Inequality (LinearTerm [0, -1] 0) (Constant 0)
    ]

y :: [Inequality]
y =
    [
        Inequality (LinearTerm [1, 1] 0) (Constant 3),
        Inequality (LinearTerm [-1, 2] 0) (Constant 3),
        Inequality (LinearTerm [1, -1] 0) (Constant 0),
        Inequality (LinearTerm [-1, 0] 0) (Constant 0)
    ]

main :: IO ()
main = do
    print "Hello World"