SCREEN 9
COLOR 14, 0
C = 1
RANDOMIZE TIMER
X = RND * 640
Y = RND * 335
QQY = RND * 335
QQX = RND * 640
QQQY = RND * 335
QQQX = RND * 640
QQQQY = RND * 335
QQQQX = RND * 640

CIRCLE (640, 335), 1

DO UNTIL INKEY$ <> ""
IF T = 0 THEN X = X + 10
IF T = 1 THEN X = X - 10
IF X > 640 THEN T = 1
IF X < 0 THEN T = 0

IF Z = 0 THEN Y = Y + 10
IF Z = 1 THEN Y = Y - 10
IF Y > 335 THEN Z = 1
IF Y < 0 THEN Z = 0

IF Q = 0 THEN QY = QY + 10
IF Q = 1 THEN QY = QY - 10
IF QY > 335 THEN Q = 1
IF QY < 0 THEN Q = 0

IF QT = 0 THEN QX = QX + 10
IF QT = 1 THEN QX = QX - 10
IF QX > 640 THEN QT = 1
IF QX < 0 THEN QT = 0

IF QQ = 0 THEN QQY = QQY + 10
IF QQ = 1 THEN QQY = QQY - 10
IF QQY > 335 THEN QQ = 1
IF QQY < 0 THEN QQ = 0

IF QQT = 0 THEN QQX = QQX + 10
IF QQT = 1 THEN QQX = QQX - 10
IF QQX > 640 THEN QQT = 1
IF QQX < 0 THEN QQT = 0

IF QQQ = 0 THEN QQQY = QQQY + 10
IF QQQ = 1 THEN QQQY = QQQY - 10
IF QQQY > 335 THEN QQQ = 1
IF QQQY < 0 THEN QQQ = 0

IF QQQT = 0 THEN QQQX = QQQX + 10
IF QQQT = 1 THEN QQQX = QQQX - 10
IF QQQX > 640 THEN QQQT = 1
IF QQQX < 0 THEN QQQT = 0

IF QQQQ = 0 THEN QQQQY = QQQQY + 10
IF QQQQ = 1 THEN QQQQY = QQQQY - 10
IF QQQQY > 335 THEN QQQQ = 1
IF QQQQY < 0 THEN QQQQ = 0

IF QQQQT = 0 THEN QQQQX = QQQQX + 10
IF QQQQT = 1 THEN QQQQX = QQQQX - 10
IF QQQQX > 640 THEN QQQQT = 1
IF QQQQX < 0 THEN QQQQT = 0

DO: W1 = W1 + 1
LOOP UNTIL W1 = 1000: W1 = 0
CLS

LINE (X, Y)-(QX, QY)
LINE (QX, QY)-(QQX, QQY)
LINE (QQX, QQY)-(QQQX, QQQY)
LINE (QQQX, QQQY)-(QQQQX, QQQQY)
LINE (QQQQX, QQQQY)-(X, Y)
LOOP
