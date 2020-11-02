type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 origin dest temp = []
hanoi 1 origin dest temp = [(origin, dest)]
hanoi discs origin dest temp = concat [x, y, z]
  where
    x = hanoi (discs - 1) origin temp dest
    y = hanoi 1 origin dest temp
    z = hanoi (discs - 1) temp dest origin

