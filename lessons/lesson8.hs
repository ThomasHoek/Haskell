type Tape a = [a]
type TapeZipper a = (Tape a, Tape a)

right :: TapeZipper a -> TapeZipper a
right (bs, x:xs) = (x:bs, xs)

left :: TapeZipper a -> TapeZipper a
left (b:bs, xs) = (bs, b:xs)

tape :: TapeZipper Int
tape = ([], [1,2,3,4,5,6,7,8])



data DataPoint l v = Data { _label :: l
                          , _value :: v
                          } deriving Show

dp :: DataPoint String Int
dp = Data "Foo" 42

data Lens s t a b = Lens { view :: s -> a
                         , set :: s -> b -> t }

-- type Lens' s t a b = (s -> a, s -> b -> t) -- Zonder records:r
-- type Lens'' s t a b = s -> (a, b -> t)     -- Equivalent

label :: Lens (DataPoint a b) (DataPoint c b) a c
label = Lens _label $ \(Data l v) l' -> Data l' v

value :: Lens (DataPoint a b) (DataPoint a c) b c
value = Lens _value $ \(Data l v) v' -> Data l v'

over :: Lens s t a b -> (a -> b) -> s -> t
over lens f dp = let old = (view lens) dp
                 in (set lens) dp (f old)
    
                 

-- view label       dp  -- 42
-- set  label "Bar" dp  -- DataPoint "Bar" 42
-- over value (+5)  dp  -- DataPoint "Foo" 47