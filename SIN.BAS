pi = 3.141592654#

SCREEN 12
COLOR (2)
z = 25
c = 1
DO
LOCATE 1, 1
PRINT c

x = x + 1
IF c > 6 THEN c = 1
IF x > 700 THEN COLOR (c): z = z + 25: x = 0: c = c + 1

y = SIN(x * (pi / 180))

IF y >= 0 THEN PSET (x, y * 20 + 80)
IF y < 0 THEN PSET (x, y * 20 + 80)
COLOR (c + 1)
IF y >= 0 THEN PSET (x, y * 20 + 120)
IF y < 0 THEN PSET (x, y * 20 + 120)
COLOR (c + 2)
IF y >= 0 THEN PSET (x, y * 20 + 160)
IF y < 0 THEN PSET (x, y * 20 + 160)
COLOR (c + 3)
IF y >= 0 THEN PSET (x, y * 20 + 200)
IF y < 0 THEN PSET (x, y * 20 + 200)
COLOR (c + 4)
IF y >= 0 THEN PSET (x, y * 20 + 240)
IF y < 0 THEN PSET (x, y * 20 + 240)
COLOR (c + 5)
IF y >= 0 THEN PSET (x, y * 20 + 280)
IF y < 0 THEN PSET (x, y * 20 + 280)
COLOR (c + 6)
IF y >= 0 THEN PSET (x, y * 20 + 320)
IF y < 0 THEN PSET (x, y * 20 + 320)
COLOR (c + 7)
IF y >= 0 THEN PSET (x, y * 20 + 360)
IF y < 0 THEN PSET (x, y * 20 + 360)
COLOR (c + 8)
IF y >= 0 THEN PSET (x, y * 20 + 400)
IF y < 0 THEN PSET (x, y * 20 + 400)



'FOR i = 1 TO 5: NEXT
LOOP UNTIL INKEY$ <> ""


                                                                      
                                                                          

