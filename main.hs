newtype SignalStrength = SignalStrength Int deriving (Eq, Show)

maybeSignal :: Int -> Maybe SignalStrength
maybeSignal s
  | s `elem` [0..15] = Just(SignalStrength s)
  | otherwise = Nothing

newSignal :: Int -> SignalStrength
newSignal i = case maybeSignal i of
  Just s -> s
  Nothing -> minSignal

maxSignal = SignalStrength 15
minSignal = SignalStrength 0

(|-*) :: SignalStrength -> SignalStrength -> SignalStrength
SignalStrength a |-* SignalStrength b = SignalStrength $ max (a - b) 0

(|-) :: SignalStrength -> SignalStrength -> SignalStrength
SignalStrength a |- SignalStrength b = SignalStrength $ a * fromEnum (a >= b)

(=|-) :: SignalStrength -> SignalStrength -> SignalStrength
SignalStrength a =|- SignalStrength b = SignalStrength $ max a b

inverter :: SignalStrength -> SignalStrength
inverter (SignalStrength a) = SignalStrength x
  where 
  x = case a of
    0 -> 15
    _ -> 0

repeater :: SignalStrength -> SignalStrength
repeater (SignalStrength a) = SignalStrength x
  where
  x = case a of
    0 -> 0
    _ -> 15

min :: SignalStrength -> SignalStrength -> SignalStrength
min a b = maxSignal |-* m
  where
  m = (maxSignal |-* a) =|- (maxSignal |-* b) 

add :: SignalStrength -> SignalStrength -> (SignalStrength, SignalStrength)
add a b = (maxSignal |-* i5,i3)
  where
    i = (i1 |-* b) =|- (b |-* i1)
    i1 = maxSignal |-* a 
    i2 = maxSignal |-* i
    i3 = inverter (i1 |- b)
    i4 = i |-* newSignal 1
    i5 = maxSignal |-* Main.min i4 i3 |-* Main.min (inverter i3) i2

printout :: SignalStrength -> SignalStrength -> IO()
printout a b = do
  print(add a b)

main = do printout (newSignal 3) (newSignal 15)