'MAKE SURE NUMBER LOCK IS PRESSED
'8 4 6 2 TO MOVE
'Q TO END

DIM X AS INTEGER
DIM Y AS INTEGER

SCREEN 9
X2 = 250
Y2 = 150
X = 50
Y = 50
R = 25
A = 1
MVE2$ = "D"
COLOR 14, 1
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
X2 = (X / 2) + X2
Y2 = (Y / 2) + Y2
MVE2$ = "D"
END IF
MVE$ = ""

COLOR 14, 1

P1X = X2 + X
P1Y = Y2 + Y
P2X = X2
P2Y = Y2
P3X = X2 + X + (COS(A + 2.5) * R + 4.5)
P3Y = Y2 + Y + (SIN(A + 2.5) * R + 4.5)
P4X = X2 + X + (COS(A - 2.5) * R + 4.5)
P4Y = Y2 + Y + (SIN(A - 2.5) * R + 4.5)
P5X = X2 + X + (COS(A + 4.5) * R)
P5Y = Y2 + Y + (SIN(A + 4.5) * R)


COLOR 14, 1
LOCATE 1, 1
PRINT A, "          "
LINE (P2X, P2Y)-(P1X, P1Y)
LINE (P1X, P1Y)-(P3X, P3Y)
LINE (P3X, P3Y)-(P2X, P2Y)
LINE (P4X, P4Y)-(P2X, P2Y)
LINE (P4X, P4Y)-(P1X, P1Y)
W1 = 0: DO UNTIL W1 = 10: W1 = W1 + 1: LOOP

COLOR 0, 1
LINE (P2X, P2Y)-(P1X, P1Y)
LINE (P1X, P1Y)-(P3X, P3Y)
LINE (P3X, P3Y)-(P2X, P2Y)
LINE (P4X, P4Y)-(P2X, P2Y)
LINE (P4X, P4Y)-(P1X, P1Y)

LOOP UNTIL KY$ = "Q"
