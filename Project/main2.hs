
type Tape a = [a]
type TapeZipper a = (Tape a, Tape a, Tape a)

up :: TapeZipper a -> TapeZipper a
up (a, b, c:cx) = (a ++ b, [c], cx)

down :: TapeZipper a -> TapeZipper a
down (a,b,c) = (init a , [last a] , b ++ c)

tape :: TapeZipper [Int]
tape = (replicate 2 [1,2,3,4,5], [[1,2,3,4,5]], replicate 2 [1,2,3,4,5])
-- tape = (repeat [False..],[False..], repeat [False..])


--tape      [123] [123] [123] -> x

--tape2     [123] [123] [123] -> y

-- tape3    [123][123][123] [123][123][123] [123][123][123] -> xy

-- tape4    [123] [123][123][123] [123] -> xy

-- tape5    [123..](<x) [123..](<y) [123](curr) [123..](y>) [123..](x>) -> xy
-- tape5    a           b           c           d           e
-- up5      a+c         b           e           d           e-1
-- down5    a-1         b           a           d           c+e

-- left5    a           b+c         d           d-1         e
-- right5   a           b-1         b           d+c         e

