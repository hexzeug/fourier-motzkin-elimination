data NormalizedInequality a = NormalizedInequality [a] a
    deriving Show

addInequalities :: Num a => NormalizedInequality a -> NormalizedInequality a -> NormalizedInequality a
addInequalities (NormalizedInequality as1 b1) (NormalizedInequality as2 b2) = NormalizedInequality (zipWith (+) as1 as2) (b1 + b2)

data IsolatedInequality a = LEq [a] a | GEq [a] a | Zero (NormalizedInequality a)
    deriving Show

isolateFirst :: (Fractional a, Ord a) => NormalizedInequality a -> IsolatedInequality a
isolateFirst (NormalizedInequality (0 : as) b) = Zero (NormalizedInequality as b)
isolateFirst (NormalizedInequality (a1 : as) b) =
    if a1 > 0
        then GEq as' b'
        else LEq as' b'
    where
        b' = b / a1
        as' = map (negate . flip (/) a1) as

partition :: (Fractional a, Ord a) => [NormalizedInequality a] -> ([NormalizedInequality a], [NormalizedInequality a], [NormalizedInequality a])
partition [] = ([], [], [])
partition (x : xs) =
    case isolateFirst x of
        GEq as b -> (NormalizedInequality (map negate as) b : gs, zs, ls) 
        Zero z -> (gs, z : zs, ls)
        LEq as b -> (gs, zs, NormalizedInequality as (-b) : ls)
    where
        (gs, zs, ls) = partition xs

combine :: Num a => ([NormalizedInequality a], [NormalizedInequality a]) -> [NormalizedInequality a]
combine (gs, ls) =
    [addInequalities g l | g <- gs, l <- ls]

project :: (Fractional a, Ord a) => [NormalizedInequality a] -> [NormalizedInequality a]
project xs =
    let (gs, zs, ls) = partition xs
    in zs ++ combine (gs, ls)

x :: [NormalizedInequality Double]
x =
    [
        NormalizedInequality [-1, 0] 0,
        NormalizedInequality [1, 2] 6,
        NormalizedInequality [-1, -1] (-2),
        NormalizedInequality [1, -1] 3,
        NormalizedInequality [0, -1] 0
    ]

main :: IO ()
main = do
    print "Hello World"