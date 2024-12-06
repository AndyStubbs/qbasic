pi = 3.141592654#

SCREEN 13
COLOR (2)
z = 20
c = 1
DO
LOCATE 1, 1
PRINT c

x = x + 1
IF c > 6 THEN c = 1
IF x > 1400 THEN COLOR (c): z = z + 20: x = 1: c = c + 1

y = SIN(x * (pi / 180))
IF y >= 0 THEN PSET (x / 15, y * 10 + 100)
IF y < 0 THEN PSET (x / 15, y * 10 + 100)
COLOR (c + 1)

LOOP UNTIL INKEY$ <> ""


