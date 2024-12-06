DECLARE SUB cool2 ()
SCREEN 13
CLS
DIM x(100), xch(100), y(100), ych(100) AS INTEGER
DIM SHARED cool%(300, 5)
DIM S(100), A2(100)



RANDOMIZE TIMER
FOR i = 0 TO 99
S(i) = RND * 6
NEXT
COLOR 14
DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
   
    IF S(i) > 5 OR S(i) = 0 THEN
        x(i) = INT(RND * 60) + 130
        y(i) = INT(RND * 50) + 75
        X1 = 160: x2 = x(i)
        Y1 = 100: y2 = y(i)
        d = (INT(SQR(ABS(Y1 - y2) ^ 2 + ABS(X1 - x2) ^ 2)) / 5) + 1
        xch(i) = ((X1 - x2) \ d) * -1
        ych(i) = ((Y1 - y2) \ d) * -1
        IF xch < 2 THEN xch = xch * 10
        IF ych < 2 THEN ych = ych * 10
        S(i) = 1
    END IF
    x(i) = x(i) + xch(i)
    y(i) = y(i) + ych(i)
    S(i) = S(i) + .07

    PSET (x(i), y(i))
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1)
    END IF
   
    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i))
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1)
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
       
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF
   
    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF
   
    END IF
    CNT = CNT + 1
   
    i = i + 1
LOOP

DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    IF y(i) < 0 THEN ych(i) = ych(i) * -1
    IF y(i) > 200 THEN ych(i) = ych(i) * -1
    IF x(i) < 0 THEN xch(i) = xch(i) * -1
    IF x(i) > 320 THEN xch(i) = xch(i) * -1
  
    y(i) = y(i) - ych(i)
    x(i) = x(i) - xch(i)
    PSET (x(i), y(i))
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1): ych(i) = ych(i)
    END IF
 
    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i)): ych(i) = ych(i)
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1)
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
     
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF
 
    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF
 
    END IF
    i = i + 1
LOOP
COLOR 15

CNT = 0
DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    IF y(i) > 200 THEN
        y(i) = 0
        x(i) = INT(RND * 320)
    END IF
    IF x(i) < 0 THEN
        x(i) = 320
        y(i) = INT(RND * 200)
    END IF
    
    IF ych(i) >= 0 THEN ych(i) = ych(i) * -1
    IF ych(i) = 0 THEN ych(i) = 1
    y(i) = y(i) - ych(i)
    x(i) = x(i) - xch(i)
    xch(i) = INT(RND * 2): CNT = 0
    CNT = CNT + 1
    PSET (x(i), y(i)): ych(i) = 1
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1): ych(i) = 2
    END IF
  
    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i))
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1): ych(i) = 3
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
      
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF
  
    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF
  
    END IF
    i = i + 1
LOOP

DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    IF y(i) < 0 THEN
        y(i) = 200
        x(i) = INT(RND * 320)
    END IF
    IF x(i) > 320 THEN
        x(i) = 0
        y(i) = INT(RND * 200)
    END IF
    y(i) = y(i) - ych(i)
    x(i) = x(i) + INT(RND * 2)
    ych(i) = INT(RND * 5)
    PSET (x(i), y(i)): ych(i) = 1
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1): ych(i) = 2
    END IF
 
    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i))
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1): ych(i) = 3
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
     
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF
 
    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF
 
    END IF
    i = i + 1

LOOP
COLOR 7

pi = 3.141592654#
A = 0
DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    A = A + (pi / 180) / 5
    xch(i) = COS(A) * 90 + 160
    ych(i) = SIN(A) * 90 + 100
    IF y(i) > 200 THEN
        y(i) = 0
        x(i) = INT(RND * 320)
    END IF
    IF x(i) > 320 THEN
        x(i) = 0
        y(i) = INT(RND * 200)
    END IF
    IF y(i) < ych(i) THEN y(i) = y(i) + 5 ELSE y(i) = y(i) - 5
    IF x(i) < xch(i) THEN x(i) = x(i) + 5 ELSE x(i) = x(i) - 5
    ych(i) = INT(RND * 5)
    PSET (x(i), y(i))
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1)
    END IF

    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i))
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1)
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
    
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF

    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF

    END IF
    i = i + 1

LOOP

A = 0
FOR i = 0 TO 99
    A2(i) = RND * 6
NEXT
1
IF CNT2 = 0 THEN ACH = .05
IF CNT2 = 1 THEN ACH = .1
IF CNT2 = 2 THEN ACH = .2
IF CNT2 = 3 THEN ACH = .3
IF CNT2 = 4 THEN ACH = 1

DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    A2(i) = A2(i) + ACH
    xch(i) = COS(A2(i)) * 90 + 160
    ych(i) = SIN(A2(i)) * 90 + 100
    IF y(i) > 200 THEN
        y(i) = 0
        x(i) = INT(RND * 320)
    END IF
    IF x(i) > 320 THEN
        x(i) = 0
        y(i) = INT(RND * 200)
    END IF
    IF y(i) < ych(i) THEN y(i) = y(i) + 5 ELSE y(i) = y(i) - 5
    IF x(i) < xch(i) THEN x(i) = x(i) + 5 ELSE x(i) = x(i) - 5
    ych(i) = INT(RND * 5)
    PSET (x(i), y(i))
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1)
    END IF

    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i))
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1)
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
   
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF

    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF

    END IF
    i = i + 1

LOOP
CNT2 = CNT2 + 1
IF CNT2 < 5 THEN GOTO 1

CNT2 = 0

COLOR 8

ACH = pi / 180
DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    A2(i) = A2(i) + ACH
    xch(i) = COS(A2(i)) * 2 * i + 160
    ych(i) = SIN(A2(i)) * 2 * i + 100
    IF y(i) < ych(i) THEN y(i) = y(i) + 5 ELSE y(i) = y(i) - 5
    IF x(i) < xch(i) THEN x(i) = x(i) + 5 ELSE x(i) = x(i) - 5
    ych(i) = INT(RND * 5)
    PSET (x(i), y(i))
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1)
        PSET (x(i) + 1, y(i))
        PSET (x(i), y(i) + 1)
    END IF

    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i))
        PSET (x(i) - 1, y(i))
        PSET (x(i), y(i) + 1)
        PSET (x(i), y(i) - 1)
    END IF
    IF i < 99 THEN
        PSET (x(i + 1), y(i + 1)), 0
        IF S(i + 1) > 2 AND S(i + 1) < 3 THEN
            PSET (x(i + 1) + 1, y(i + 1) + 1), 0
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
        END IF
  
        IF S(i + 1) > 3 THEN
            PSET (x(i + 1) + 1, y(i + 1)), 0
            PSET (x(i + 1) - 1, y(i + 1)), 0
            PSET (x(i + 1), y(i + 1) + 1), 0
            PSET (x(i + 1), y(i + 1) - 1), 0
        END IF

    END IF
    IF i = 99 THEN
        PSET (x(0), y(0)), 0
        IF S(0) > 2 AND S(0) < 3 THEN
            PSET (x(0) + 1, y(0) + 1), 0
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
        END IF
        IF S(0) > 3 THEN
            PSET (x(0) + 1, y(0)), 0
            PSET (x(0) - 1, y(0)), 0
            PSET (x(0), y(0) + 1), 0
            PSET (x(0), y(0) - 1), 0
        END IF

    END IF
    i = i + 1

LOOP
COLOR 8

ACH = (pi / 180) * 2
DO UNTIL INKEY$ <> ""
    IF i > 99 THEN i = 0
    A2(i) = A2(i) + ACH
    xch(i) = COS(A2(i)) * 2 * i + 160
    ych(i) = SIN(A2(i)) * 2 * i + 100
    IF y(i) < ych(i) THEN y(i) = y(i) + 5 ELSE y(i) = y(i) - 5
    IF x(i) < xch(i) THEN x(i) = x(i) + 5 ELSE x(i) = x(i) - 5
    ych(i) = INT(RND * 5)
    PSET (x(i), y(i)), 15
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1), 15
        PSET (x(i) + 1, y(i)), 15
        PSET (x(i), y(i) + 1), 15
    END IF

    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i)), 15
        PSET (x(i) - 1, y(i)), 15
        PSET (x(i), y(i) + 1), 15
        PSET (x(i), y(i) - 1), 15
    END IF
    i = i + 1

LOOP

ACH = (pi / 180) * 2
ve = 0
DO UNTIL ve > 15000
    ve = ve + 1
    IF i > 99 THEN i = 0
    A2(i) = A2(i) + ACH
    xch(i) = COS(A2(i)) * 2 * i + 160
    ych(i) = SIN(A2(i)) * 2 * i + 100
    IF y(i) < ych(i) THEN y(i) = y(i) + 5 ELSE y(i) = y(i) - 5
    IF x(i) < xch(i) THEN x(i) = x(i) + 5 ELSE x(i) = x(i) - 5
    ych(i) = INT(RND * 5)
    PSET (x(i), y(i)), i
    IF S(i) > 2 AND S(i) < 3 THEN
        PSET (x(i) + 1, y(i) + 1), i
        PSET (x(i) + 1, y(i)), i
        PSET (x(i), y(i) + 1), i
    END IF

    IF S(i) > 3 THEN
        PSET (x(i) + 1, y(i)), i
        PSET (x(i) - 1, y(i)), i
        PSET (x(i), y(i) + 1), i
        PSET (x(i), y(i) - 1), i
    END IF
    i = i + 1

LOOP
red = 1
green = 1
blue = 1
i = 0
ve = 0
DO UNTIL ve > 500
    'PRINT ve
    ve = ve + 1
    red = red + 1
    IF red > 63 THEN red = 0
     WAIT &H3DA, 8
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
    i = i + 1
    IF i > 256 THEN i = 0
LOOP
i = 0
red = 1
green = 1
blue = 1
ve = 0
DO UNTIL ve > 15000
    ve = ve + 1
    i = i + 1
    blue = blue + 1
    IF blue > 63 THEN blue = 0
     'WAIT &H3DA, 8
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
    IF i > 255 THEN i = 0
LOOP
 i = 0
red = 1
green = 1
blue = 1
ve = 0
DO UNTIL ve > 15000
    ve = ve + 1
    i = i + 1
    green = green + 1
    IF green > 63 THEN green = 0
     'WAIT &H3DA, 8
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
    IF i > 255 THEN i = 0
LOOP
i = 0
red = 1
green = 1
blue = 1
ve = 0
DO UNTIL ve > 15000
    ve = ve + 1
    i = i + 1
    blue = blue + 1
    red = blue
    IF blue > 63 THEN blue = 0
     'WAIT &H3DA, 8
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
    IF i > 255 THEN i = 0
LOOP
i = 0
red = 1
green = 1
blue = 1


red = 1
green = 1
blue = 1



FOR i = 0 TO 256
    blue = (i / 4)
   
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
NEXT
i = 0
red = 0
d = 1
x = 127
x2 = 129
d = 1
d = (63 / 100)
green = 1
blue = 1
red = 1
c = 128
d2 = 1
ve = 0
DO UNTIL ve > 700
    ve = ve + 1
    'FOR 2 = 0 TO 1500: NEXT
    WAIT &H3DA, 8
    blue = blue + d
    IF blue > 62 THEN d = d * -1
    IF blue < 1 THEN d = d * -1
    x = x + d2
    x2 = x2 - d2
    IF x > 200 THEN d2 = d2 * -1
 
    OUT &H3C8, x
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
 
    OUT &H3C8, x2
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)

LOOP
'i = 0
FOR i = 256 TO 0 STEP -1
    blue = 0
    WAIT &H3DA, 8
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
NEXT

CALL cool2

PUT (122, 87), cool%(0, 0), XOR
PUT (150, 87), cool%(0, 1), XOR

FOR i = 63 TO 0 STEP -1
    FOR i2 = 0 TO 10000: NEXT
    WAIT &H3DA, 8
    OUT &H3C8, 15
    OUT &H3C9, INT(i)
    OUT &H3C9, INT(i)
    OUT &H3C9, INT(i)
NEXT

SUB cool2
CLS
COLOR 15
PRINT
PRINT "THE END"
GET (0, 15)-(25, 7), cool%(0, 0)
GET (30, 15)-(55, 7), cool%(0, 1)
CLS


x = 25
y = 50
x2 = 25
y2 = 25

x4 = 280
y4 = 120
x3 = 280
y3 = 120
bcnt = 0
PUT (x3, y3), cool%(0, 1), XOR
PUT (x, y), cool%(0, 0), XOR
FOR i = 0 TO 60
    FOR i2 = 0 TO 1000: NEXT
    WAIT &H3DA, 8
    OUT &H3C8, 15
    OUT &H3C9, INT(i)
    OUT &H3C9, INT(i)
    OUT &H3C9, INT(i)
NEXT
PUT (x3, y3), cool%(0, 1), XOR
PUT (x, y), cool%(0, 0), XOR

bcnt2 = 0
pi = 3.141592654#
ba = 90
ba2 = 90
FOR i = 1 TO 1000
e$ = INKEY$
NEXT

DO UNTIL INKEY$ <> ""
    bcnt = bcnt + 1
    IF bcnt > 2 AND x2 < 100 THEN
        x2 = x2 + 2
        y2 = y2 + 2
        bcnt = 0
    END IF
    IF x < 120 THEN
        ba = ba + (pi / 180) * 5
        x = (COS(ba) * 25) + x2
        y = (SIN(ba) * 25) + y2
    END IF
  
    IF x >= 120 THEN
        bcnt2 = bcnt2 + 1
        IF bcnt2 > 2 AND x3 > 120 AND mark <> 1 THEN
            x3 = x3 - 10
            x4 = x3
            y4 = y3 - 30
            bcnt2 = 0
        END IF
        IF x3 = 120 THEN mark = 1
        IF mark = 1 AND ba2 <= 94.01419 THEN
            'PRINT ba2
            ba2 = ba2 + (pi / 180) * 10
            x3 = (COS(ba2) * 30) + x4
            y3 = (SIN(ba2) * 30) + y4
        END IF
        IF mark = 1 AND ba2 >= 94.01419 THEN y3 = y
    END IF
  
    PUT (x3, y3), cool%(0, 1), XOR
    PUT (x, y), cool%(0, 0), XOR
    'PSET (x3, y3), 15
    WAIT &H3DA, 8
    'PSET (x3, y3), 0
    PUT (x, y), cool%(0, 0), XOR
    PUT (x3, y3), cool%(0, 1), XOR

LOOP

END SUB

