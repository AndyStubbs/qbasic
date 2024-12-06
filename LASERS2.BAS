SCREEN 7
COLOR 4, 0
X = 150
XX = 150
Z = 150
ZZ = 150

H = X + 50
HH = X - 50
L = XX + 50
LL = XX - 50

DO

KY$ = INKEY$
IF KY$ = "4" THEN T = 1
IF KY$ = "6" THEN T = 2
IF KY$ = "8" THEN G = 1
IF KY$ = "2" THEN G = 2
IF KY$ = "5" THEN T = 0: G = 0
IF KY$ = "Q" THEN END

IF T = 1 THEN X = X - 1
IF T = 2 THEN X = X + 1
IF G = 1 THEN XX = XX - 1
IF G = 2 THEN XX = XX + 1

IF X > H THEN T = 1
IF XX > L THEN G = 1
IF X < HH THEN T = 2
IF XX < LL THEN G = 2


LINE (X, XX)-(Z, ZZ)


IF F = 50 THEN F = 0: CLS
F = F + 1
DO
Y = Y + 1
LOOP UNTIL Y >= 10: Y = 0
LOOP





