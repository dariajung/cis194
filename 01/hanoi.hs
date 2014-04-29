type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 source dest spare = [(source, dest)]
hanoi disk source dest spare = (hanoi (disk - 1) source spare dest) ++ (hanoi 0 source dest spare) ++ (hanoi (disk - 1) spare dest source)
   
