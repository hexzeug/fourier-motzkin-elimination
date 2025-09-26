data NormalizedInequality a = NormalizedInequality [a] a
instance Show a => Show (NormalizedInequality a) where
    show :: Show a => NormalizedInequality a -> String
    show (NormalizedInequality coeffs b) = show coeffs ++ " <= " ++ show b

data IsolatedInequality a = GEq [a] a | LEq [a] a | Zero
instance Show a => Show (IsolatedInequality a) where
    show :: Show a => IsolatedInequality a -> String
    show (GEq coeffs b) = show coeffs ++ " + " ++ show b ++ " <=" 
    show (LEq coeffs b) = "<= " ++ show coeffs ++ " + " ++ show b
    show Zero = "Zero"

isolateFirst :: (Fractional a, Ord a) => NormalizedInequality a -> IsolatedInequality a
isolateFirst (NormalizedInequality (0 : as) b) = Zero
isolateFirst (NormalizedInequality (a1 : as) b) =
    if a1 < 0
        then GEq as' b'
        else LEq as' b'
    where
        b' = b / a1
        as' = map (negate . flip (/) a1) as

main :: IO ()
main = do
    print "Hello World"