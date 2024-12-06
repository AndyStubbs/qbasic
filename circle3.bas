' PRESS NUMBER LOCK
' THEN 8 4 6 2 TO MOVE
' PRESS Q TO END

SCREEN 9
X2 = 250
Y2 = 150
C = 0
X = 50
Y = 50
R = 25
A = 1
MVE2$ = "D"
COLOR C, 1
DO
KY$ = INKEY$
IF KY$ = "6" THEN MVE$ = "R"
IF MVE$ = "R" THEN A = A + .2
IF KY$ = "4" THEN MVE$ = "L"
IF MVE$ = "L" THEN A = A - .2
IF KY$ = "8" THEN MVE2$ = "U"
IF KY$ = "2" THEN MVE2$ = "D"
IF MVE2$ = "D" THEN
X = (COS(A) * R)
Y = (SIN(A) * R)
END IF
IF MVE2$ = "U" THEN
X2 = (X / 15) + X2
Y2 = (Y / 15) + Y2
MVE2$ = "D"
END IF
MVE$ = ""
C = 15
IF C > 15 THEN C = 14
COLOR C, 1
LINE (X2, Y2)-(X + X2, Y + Y2)
LINE (X2 + (X * -1), Y2 + (Y * -1))-(X2 + X, Y2 + Y)
W1 = 0: DO UNTIL W1 = 20: W1 = W1 + 1: LOOP

COLOR 0, 1
LINE (X2, Y2)-(X + X2, Y + Y2)
LINE (X2 + (X * -1), Y2 + (Y * -1))-(X2 + X, Y2 + Y)

LOOP UNTIL KY$ = "Q"

