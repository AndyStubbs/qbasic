SCREEN 7
COLOR 4
A = 35
AA = 0
T% = 8
DO
PSET (X, XX)
DRAW "R2"
IF XX <= 0 AND FAT = 0 THEN FAT = 2
IF XX <= 0 AND FAT = 1 THEN FAT = 3
IF XX <= 0 AND FAT = 4 THEN
FAT = 2
RANDOMIZE TIMER
A = A + 35: AA = AA + 35
IF A > 300 THEN AA = 0: A = 35
CHAT = INT(RND * 15)
CHAP = INT(RND * 15)
IF CHAT = 0 THEN CHAT = 6
COLOR CHAT
END IF
IF SAT = 0 THEN X = X + 1
IF X > A AND SAT = 0 THEN SAT = 1:
IF SAT = 1 THEN X = X - 1
IF SAT = 1 AND X <= AA THEN SAT = 0


IF FAT = 2 THEN XX = XX + 1
IF FAT = 3 THEN XX = XX + 1
DO
Y = Y + 1
Y = Y + 1
Y = Y + 3
LOOP UNTIL Y = 100: Y = 0
IF XX > 200 AND FAT = 2 THEN FAT = 1
IF XX > 200 AND FAT = 3 THEN FAT = 4
IF FAT = 1 THEN XX = XX - 1
IF FAT = 4 THEN XX = XX - 1
LOOP WHILE INKEY$ = ""

