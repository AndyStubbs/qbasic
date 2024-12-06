    SCREEN 7
   DRAW "ND5 U5 R L2 R U D5 NF NG"
   
10
    PAINT (1, 1), 5
    DIM Box%(1 TO 200)
    x1% = 0: x2% = 10: y1% = 0: y2% = 10
    LINE (x1%, y1%)-(x1%, y2%), 1
    GET (x1%, y1%)-(x1%, y2%), Box%
    DO
        PAINT (1, 1), 5
        PUT (x1%, y1%), Box%, XOR
        x1% = RND * 300
        y1% = RND * 180
        PUT (x1%, y1%), Box%
    X = X + 5
    Z = Z + 5
    PRINT
    IF X = 200 AND Z < 1000 THEN PLAY "L60C": X = 0
    IF Z > 1000 THEN T = T + 1: SOUND (90), .2
    IF Z > 1000 THEN COLOR 4: CIRCLE (150, 100), T: CIRCLE (151, 100), T
    IF T > 200 THEN COLOR 14: CIRCLE (150, 100), A: CIRCLE (151, 100), A
    IF T > 200 THEN A = A + 1
    IF A > 200 THEN COLOR 0: CIRCLE (150, 100), B: CIRCLE (151, 100), B
    IF A > 200 THEN B = B + 1
    IF B > 200 THEN
    CLS
    Z = 0: X = 0: T = 0: A = 0: B = 0
    END IF
    LOOP WHILE INKEY$ = ""
    
    

