SCREEN 7
COLOR 4
T% = 8
DO
LINE (X, XX)-(150, 100)

IF XX <= 0 AND FAT = 0 THEN FAT = 2
IF XX <= 0 AND FAT = 1 THEN FAT = 3
IF XX <= 0 AND FAT = 4 THEN
FAT = 2
RANDOMIZE TIMER
CHAT = INT(RND * 15)
CHAP = INT(RND * 15)
IF CHAT = 0 THEN CHAT = 6
COLOR CHAT
END IF
IF SAT = 0 THEN X = X + 1
IF X > 305 AND SAT = 0 THEN SAT = 1
IF SAT = 1 THEN X = X - 1
IF SAT = 1 AND X <= 0 THEN SAT = 0


IF FAT = 2 THEN XX = XX + 1
IF FAT = 3 THEN XX = XX + 1
DO
Y = Y + 1
Y = Y + 1
Y = Y + 3
LOOP UNTIL Y >= 1: Y = 0
IF XX > 200 AND FAT = 2 THEN FAT = 1
IF XX > 200 AND FAT = 3 THEN FAT = 4
IF FAT = 1 THEN XX = XX - 1
IF FAT = 4 THEN XX = XX - 1
LOOP WHILE INKEY$ = ""

