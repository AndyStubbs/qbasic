DIM hx(500) AS INTEGER
DIM hy(500) AS INTEGER
DIM x(2000) AS INTEGER
DIM y(2000) AS INTEGER
DIM fx(10000) AS INTEGER
DIM fy(10000) AS INTEGER
DIM rx(500) AS INTEGER
DIM ry(500) AS INTEGER
DIM px(500) AS INTEGER
DIM py(500) AS INTEGER
DIM od(500) AS INTEGER
DIM mvt(500) AS INTEGER
DIM mve(500) AS INTEGER
DIM sts(500) AS INTEGER

RANDOMIZE TIMER

tbugs = 150

'tmr = 2000 - (tbugs * 157)
SCREEN 13

FOR i = 0 TO 9900
  fx(i) = INT(RND * 319)
  fy(i) = INT(RND * 199)
  ft% = ft% + 1
NEXT

FOR i = 0 TO 400
  rox% = INT(RND * 319)
  roy% = INT(RND * 199)
  h% = 0
  FOR i2 = 0 TO 200
    IF fx(i2) = rox% AND fy(i2) = roy% THEN h% = 1
  NEXT
  IF h% = 0 THEN rx(i) = rox%: ry(i) = roy%: rt% = rt% + 1
NEXT

FOR i = 0 TO 499
  sts(i) = 1
  mve(i) = 1
  hx(i) = INT(RND * 319)
  hy(i) = INT(RND * 199)
  x(i) = INT(RND * 319)
  y(i) = INT(RND * 199)
  px(i) = x(i): py(i) = y(i)
  od(i) = INT(RND * 7) + 1
NEXT
'tmr! = TIMER

DO UNTIL INKEY$ <> ""
  ct = ct + 1
  IF ct = 15 THEN
    FOR i = 0 TO tbugs - 1
      mve(i) = sts(i)
    NEXT
    ct = 0
  END IF

  FOR i = 0 TO tbugs - 1
    IF mve(i) = 0 THEN
      m = INT(RND * 15)
      IF m = 0 THEN d = 1
      IF m = 1 THEN d = 2
      IF m = 2 THEN d = 3
      IF m = 3 THEN d = 4
      IF m = 4 THEN d = 5
      IF m = 5 THEN d = 6
      IF m = 6 THEN d = 7
      IF m = 7 THEN d = 8
      IF m = 8 THEN mvt(i) = 1
      IF m > 8 THEN d = od(i)
      IF m < 8 THEN mvt(i) = 0
    END IF
   
    IF mve(i) = 1 THEN
      mvt(i) = 0
      IF x(i) < hx(i) THEN d = 1
      IF x(i) > hx(i) THEN d = 2
      IF y(i) < hy(i) THEN d = 3
      IF y(i) > hy(i) THEN d = 4
      IF y(i) < hy(i) AND x(i) < hx(i) THEN d = 5
      IF y(i) < hy(i) AND x(i) > hx(i) THEN d = 6
      IF y(i) > hy(i) AND x(i) < hx(i) THEN d = 7
      IF y(i) > hy(i) AND x(i) > hx(i) THEN d = 8
      IF x(i) > hx(i) - 20 AND x(i) < hx(i) + 20 THEN
        IF y(i) > hy(i) - 20 AND y(i) < hy(i) + 20 THEN sts(i) = 2
      END IF
    END IF
   
    IF mve(i) = 2 THEN
      m = INT(RND * 150)
      IF m = 0 THEN d = 1
      IF m = 1 THEN d = 2
      IF m = 2 THEN d = 3
      IF m = 3 THEN d = 4
      IF m = 4 THEN d = 5
      IF m = 5 THEN d = 6
      IF m = 6 THEN d = 7
      IF m = 7 THEN d = 8
      IF m = 8 THEN mvt(i) = 1
      IF m > 8 THEN d = od(i)
      IF m < 8 THEN mvt(i) = 0
    END IF
   
    IF d = 1 AND x(i) < 320 AND mvt(i) = 0 THEN
      IF POINT(x(i) + 2, y(i)) = 0 THEN x(i) = x(i) + 1 ELSE mve(i) = 0
    END IF
   
    IF d = 2 AND x(i) > 0 AND mvt(i) = 0 THEN
      IF POINT(x(i) - 2, y(i)) = 0 THEN x(i) = x(i) - 1 ELSE mve(i) = 0
    END IF
   
    IF d = 3 AND y(i) < 200 AND mvt(i) = 0 THEN
      IF POINT(x(i), y(i) + 2) = 0 THEN y(i) = y(i) + 1 ELSE mve(i) = 0
    END IF
   
    IF d = 4 AND y(i) > 0 AND mvt(i) = 0 THEN
      IF POINT(x(i), y(i) - 2) = 0 THEN y(i) = y(i) - 1 ELSE mve(i) = 0
    END IF
   
    IF d = 5 AND x(i) < 320 AND y(i) < 200 AND mvt(i) = 0 THEN
      IF POINT(x(i) + 2, y(i) + 2) = 0 THEN x(i) = x(i) + 1: y(i) = y(i) + 1 ELSE mve(i) = 0
    END IF
   
    IF d = 6 AND x(i) > 0 AND y(i) < 200 AND mvt(i) = 0 THEN
      IF POINT(x(i) - 2, y(i) + 2) = 0 THEN x(i) = x(i) - 1: y(i) = y(i) + 1 ELSE mve(i) = 0
    END IF
   
    IF d = 7 AND y(i) > 0 AND x(i) < 320 AND mvt(i) = 0 THEN
      IF POINT(x(i) + 2, y(i) - 2) = 0 THEN y(i) = y(i) - 1: x(i) = x(i) + 1 ELSE mve(i) = 0
    END IF
   
    IF d = 8 AND x(i) > 0 AND y(i) > 0 AND mvt(i) = 0 THEN
      IF POINT(x(i) - 2, y(i) - 2) = 0 THEN x(i) = x(i) - 1: y(i) = y(i) - 1 ELSE mve(i) = 0
    END IF
   
    PSET (px(i), py(i)), 0
   
    'R
    IF od(i) = 1 THEN
      PSET (px(i) - 1, py(i)), 0
      PSET (px(i) + 1, py(i)), 0
    END IF
    IF d = 1 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i) - 1, y(i)), 4
      PSET (x(i) + 1, y(i)), 1
    END IF
   
    'L
    IF od(i) = 2 THEN
      PSET (px(i) - 1, py(i)), 0
      PSET (px(i) + 1, py(i)), 0
    END IF
    IF d = 2 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i) - 1, y(i)), 1
      PSET (x(i) + 1, y(i)), 4
    END IF

    'D
    IF od(i) = 3 THEN
      PSET (px(i), py(i) - 1), 0
      PSET (px(i), py(i) + 1), 0
    END IF
    IF d = 3 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i), y(i) - 1), 4
      PSET (x(i), y(i) + 1), 1
    END IF

    'U
    IF od(i) = 4 THEN
      PSET (px(i), py(i) - 1), 0
      PSET (px(i), py(i) + 1), 0
    END IF
    IF d = 4 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i), y(i) - 1), 1
      PSET (x(i), y(i) + 1), 4
    END IF
   
    'DR
    IF od(i) = 5 THEN
      PSET (px(i) - 1, py(i) - 1), 0
      PSET (px(i) + 1, py(i) + 1), 0
    END IF
    IF d = 5 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i) - 1, y(i) - 1), 4
      PSET (x(i) + 1, y(i) + 1), 1
    END IF
   
    'DL
    IF od(i) = 6 THEN
      PSET (px(i) - 1, py(i) + 1), 0
      PSET (px(i) + 1, py(i) - 1), 0
    END IF
    IF d = 6 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i) - 1, y(i) + 1), 1
      PSET (x(i) + 1, y(i) - 1), 4
    END IF
   
    'UR
    IF od(i) = 7 THEN
      PSET (px(i) + 1, py(i) - 1), 0
      PSET (px(i) - 1, py(i) + 1), 0
    END IF
    IF d = 7 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i) + 1, y(i) - 1), 1
      PSET (x(i) - 1, y(i) + 1), 4
    END IF
   
    'UL
    IF od(i) = 8 THEN
      PSET (px(i) - 1, py(i) - 1), 0
      PSET (px(i) + 1, py(i) + 1), 0
    END IF
    IF d = 8 THEN
      PSET (x(i), y(i)), 4
      PSET (x(i) - 1, y(i) - 1), 1
      PSET (x(i) + 1, y(i) + 1), 4
    END IF
   
    
    'FOR i2 = 0 TO 500: NEXT
    px(i) = x(i): py(i) = y(i): od(i) = d
  NEXT
 
  FOR i = 0 TO ft%
    PSET (fx(i), fy(i)), 2
  NEXT
  FOR i = 0 TO rt%
    PSET (rx(i), ry(i)), 8
  NEXT
  
'cnt = cnt + 1
'IF cnt > 1000 THEN
'  tmr2! = TIMER
'  LOCATE 1, 1
'  PRINT tmr2! - tmr!; "         "
'  tmr! = TIMER
'  cnt = 0
'END IF
LOOP


