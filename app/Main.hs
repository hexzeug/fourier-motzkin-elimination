data Term =
    Constant Rational |
    LinearTerm [Rational] Rational |
    IsolatedVariable |
    Max [Term] |
    Min [Term]
    deriving Show

data Inequality = Inequality Term Term
    deriving Show

data Optimum = Minimum | Maximum | MinInt | MaxInt
    deriving (Show, Read, Eq)

isolateLast :: Term -> Term
isolateLast (LinearTerm as a) =
    let
        an = negate $ last as
        as' = init as ++ [-1]
    in
        LinearTerm (map (/ an) as') (a / an)

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

substituteLast :: Term -> Term -> Term
substituteLast (LinearTerm as 0) (LinearTerm bs 0) =
    let
        bs' = init bs ++ [0]
        b = last bs
    in
        LinearTerm (zipWith (+) (map (*b) as) bs') 0

substituteLasts :: Term -> [Inequality] -> [Inequality]
substituteLasts t =
    map subLast
    where
        subLast (Inequality l g) = Inequality (substituteLast t l) g

optimizeLast :: Optimum -> [Inequality] -> Maybe [Rational]
optimizeLast _ (Inequality (LinearTerm [] 0) _ : _) = Just []
optimizeLast o is =
    let
        solutionInFirst = solveForFirst is
    in case (optimizeLast o . expand . eliminate) solutionInFirst of
        Nothing -> Nothing
        Just xs ->
            let
                ((Inequality _ upperBound) : (Inequality lowerBound _) : _) = solutionInFirst
                lo = (evaluate . substitute xs) lowerBound
                hi = (evaluate . substitute xs) upperBound
                loi = fromIntegral (ceiling lo) :: Rational
                hii = fromIntegral (floor hi) :: Rational
            in
                if hi < lo
                    then Nothing
                    else case o of
                        Minimum -> Just (lo : xs)
                        Maximum -> Just (hi : xs)
                        MinInt -> if loi > hi then Nothing else Just (loi : xs)
                        MaxInt -> if hii < lo then Nothing else Just (hii : xs)

optimizeTerm :: Optimum -> Term -> [Inequality] -> Maybe [Rational]
optimizeTerm o t is =
    let
        t' = isolateLast t
        is' = substituteLasts t' is
    in
        case optimizeLast o is' of
            Nothing -> Nothing
            Just xs ->
                let
                    l = evaluate . substitute xs $ t'
                in
                    Just (init xs ++ [l])

exampleTerm :: Term
exampleTerm = LinearTerm [350, 300] 0

exampleConstraints :: [Inequality]
exampleConstraints =
    [
        Inequality (LinearTerm [18, 12] 0) (Constant 3132),
        Inequality (LinearTerm [1, 1] 0) (Constant 200),
        Inequality (LinearTerm [6, 8] 0) (Constant 1440),
        Inequality (LinearTerm [-1, 0] 0) (Constant 0),
        Inequality (LinearTerm [0, -1] 0) (Constant 0)
    ]

main :: IO ()
main = do
    print $ map fromRational <$> optimizeTerm MaxInt exampleTerm exampleConstraints