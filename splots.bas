SCREEN 9
COLOR 14, 0
C = 1
DO UNTIL INKEY$ <> ""
RANDOMIZE TIMER
'CIRCLE (X, Y), 50
LINE (XX, YY)-(X, Y)
X = RND * 1000
Y = RND * 400
T = T + 1
IF C > 15 THEN C = 1
COLOR C
IF T > 300 THEN
Z = Z + 1
T = 0: C = C + 1
XX = RND * 400
YY = RND * 250
END IF
IF Z = 5 THEN CLS : Z = 0
LOOP
