CLS
COLOR 4, 1

PRINT "PRESS A - G FOR MUSIC"
PRINT "PRESS N TO CHANGE SETTINGS"
PRINT "PRESS Q TO QUIT"

DO
KY$ = UCASE$(INKEY$)

IF KY$ = "N" THEN
PRINT "Q - QUIT"

INPUT "F - FORGROUNG B - BACKGROUND MUSIC ", MT$
IF MT$ = "B" THEN PLAY "MB": song$ = song$ + "MB": PRINT "MB";
IF MT$ = "F" THEN PLAY "MF": song$ = song$ + "MF": PRINT "MF";
PRINT
END IF

IF KY$ = "P" THEN
PRINT
INPUT "PAUSE (0-64) ", PAUSE
IF PAUSE > 64 THEN PAUSE = 64
IF PAUSE <= 5 THEN P$ = "P1"
IF PAUSE > 5 THEN P$ = "P5"
IF PAUSE > 10 THEN P$ = "P10"
IF PAUSE > 15 THEN P$ = "P15"
IF PAUSE > 20 THEN P$ = "P20"
IF PAUSE > 25 THEN P$ = "P25"
IF PAUSE > 30 THEN P$ = "P30"
IF PAUSE > 35 THEN P$ = "P35"
IF PAUSE > 40 THEN P$ = "P40"
IF PAUSE > 45 THEN P$ = "P45"
IF PAUSE > 50 THEN P$ = "P50"
IF PAUSE > 55 THEN P$ = "P55"
IF PAUSE > 60 THEN P$ = "P60"
IF PAUSE > 64 THEN P$ = "P64"
song$ = song$ + P$
PRINT P$;
END IF

IF KY$ = "O" THEN
PRINT
INPUT "OCTANE ", O
IF O > 6 THEN O = 6
IF O = 1 THEN OT$ = "O1": PRINT "O1"; : PLAY "O1"
IF O = 2 THEN OT$ = "O2": PRINT "O2"; : PLAY "O2"
IF O = 3 THEN OT$ = "O3": PRINT "O3"; : PLAY "O3"
IF O = 4 THEN OT$ = "O4": PRINT "O4"; : PLAY "O4"
IF O = 5 THEN OT$ = "O5": PRINT "O5"; : PLAY "O5"
IF O = 6 THEN OT$ = "O6": PRINT "O6"; : PLAY "O6"
song$ = song$ + OT$
END IF

IF KY$ = "T" THEN
PRINT
INPUT "TEMPO ", T
IF T > 255 THEN T = 255
IF T < 32 THEN T = 32
T2$ = "T" + STR$(T)
PRINT T2$;
PLAY T2$
song$ = song$ + T2$
END IF

IF KY$ = "M" THEN PLAY song$
IF KY$ <> "" AND KY$ < "H" THEN
PRINT KY$;
song$ = song$ + KY$
PLAY KY$
END IF


LOOP UNTIL KY$ = "Q"
PLAY song$
OPEN "MUSIC2.BAS" FOR APPEND AS 1
WRITE #1, song$
CLOSE 1





