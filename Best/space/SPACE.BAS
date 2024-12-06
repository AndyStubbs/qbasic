DECLARE SUB vsccrn (dmg2%)

'This Game uses:
'===========================================================================
' Subject: NEW ASSEMBly% MULTIKEY FUNCTION    Date: 12-20-97 (10:33)
'  Author: Joe Huber, Jr.                    Code: QB, QBasic, PDS
'  Origin: Milo Sedlacek                   Packet: KEYBOARD.ABC
'===========================================================================

DECLARE SUB mstars ()
DECLARE SUB initstars ()
DECLARE SUB keytest ()
DECLARE FUNCTION MULTIKEY (T)

DIM enemy$(15), boss$, espd%(150), ep%(150), ex%(150), ey%(150), etype%(150), enum%, entype%
DIM esx%(15), esy%(15), ep2%(15), esd%(150), espd2%(15), tme5%(15)
DIM tme6%(15), estyp%(15), esan%(15), expy%(15), expx%(15), anme%(15)

DIM eshot%(16, 16), eshot2%(16), xch%(15, 15), ych%(15, 15), x1%(15, 16), y1%(15, 16)
DIM esdmg%(16)

DIM I, tme, tme2, tme3, tme4 AS INTEGER
DIM SHARED stc(90), sty(90), stx(90), cnt, stars, x, y, st, st2, tmr AS INTEGER
DIM SHARED eship%(300, 6, 6)
DIM SHARED bship%(300, 6)
DIM explode%(300, 5)
DIM bx(20), by(20), px(20), py(20), fire(20) AS INTEGER
DIM back%(300, 5)

x1%(0, 6) = -1
CLS
SCREEN 13
dmg% = 100

enemy$(0) = "wship"
enemy$(1) = "w2ship"
enemy$(2) = "bomb"
enemy$(3) = "boss"

entype% = 4
enum% = 14

espd%(0) = 6
etype%(0) = 0
ep%(0) = 1
ex%(0) = 5
ey%(0) = 130

espd%(1) = 5
ep%(1) = 2
ex%(1) = 10
ey%(1) = 80

etype%(2) = 1
espd%(2) = 5
ep%(2) = 3
ex%(2) = 10
ey%(2) = 60

etype%(2) = 1
espd%(3) = 6
ep%(3) = 4
ex%(3) = 20
ey%(3) = 100

espd%(4) = 2
ep%(4) = 5
ex%(4) = 30
ey%(4) = 15

espd%(5) = 3
ep%(5) = 6
ex%(5) = 40
ey%(5) = 130

espd%(6) = 4
ep%(6) = 7
ex%(6) = 50
ey%(6) = 100

espd%(7) = 5
ep%(7) = 1
ex%(7) = 60
ey%(7) = 130

etype%(8) = 2
espd%(8) = 8
ep%(8) = 1
ex%(8) = 70
ey%(8) = 10

espd%(9) = 8
etype%(9) = 2
ep%(9) = 1
ex%(9) = 80
ey%(9) = 50

espd%(10) = 8
etype%(10) = 2
ep%(10) = 1
ex%(10) = 90
ey%(10) = 80

espd%(11) = 8
etype%(11) = 2
ep%(11) = 1
ex%(11) = 100
ey%(11) = 140

espd%(12) = 8
etype%(12) = 2
ep%(12) = 1
ex%(12) = 105
ey%(12) = 20

espd%(13) = 6
etype%(13) = 3
ep%(13) = 0
ex%(13) = 150
ey%(13) = 60


FOR I = 0 TO entype% - 1
    DEF SEG = VARSEG(eship%(0, 0, I))
    BLOAD enemy$(I) + LTRIM$(STR$(0)) + ".pcf", VARPTR(eship%(0, 0, I))
    DEF SEG               ' Restore default BASIC segment.
   
    DEF SEG = VARSEG(eship%(0, 1, I))
    BLOAD enemy$(I) + LTRIM$(STR$(1)) + ".pcf", VARPTR(eship%(0, 1, I))
    DEF SEG               ' Restore default BASIC segment.
   
    DEF SEG = VARSEG(eship%(0, 2, I))
    BLOAD enemy$(I) + LTRIM$(STR$(2)) + ".pcf", VARPTR(eship%(0, 2, I))
    DEF SEG               ' Restore default BASIC segment.
   
    DEF SEG = VARSEG(eship%(0, 3, I))
    BLOAD "m" + enemy$(I) + LTRIM$(STR$(0)) + ".pcf", VARPTR(eship%(0, 3, I))
    DEF SEG               ' Restore default BASIC segment.
  
    DEF SEG = VARSEG(eship%(0, 4, I))
    BLOAD "m" + enemy$(I) + LTRIM$(STR$(1)) + ".pcf", VARPTR(eship%(0, 4, I))
    DEF SEG               ' Restore default BASIC segment.
  
    DEF SEG = VARSEG(eship%(0, 5, I))
    BLOAD "m" + enemy$(I) + LTRIM$(STR$(2)) + ".pcf", VARPTR(eship%(0, 5, I))
    DEF SEG               ' Restore default BASIC segment.
NEXT

'END
FOR I = 0 TO 5
    IF I < 3 THEN filen$ = "bship": i2 = I ELSE filen$ = "mbship": i2 = I - 3
    DEF SEG = VARSEG(bship%(0, I))
    BLOAD filen$ + LTRIM$(STR$(i2)) + ".pcf", VARPTR(bship%(0, I))
    DEF SEG               ' Restore default BASIC segment.
    IF I < 5 THEN
        DEF SEG = VARSEG(explode%(0, I))
        BLOAD "fire" + LTRIM$(STR$(I)) + ".pcf", VARPTR(explode%(0, I))
        DEF SEG               ' Restore default BASIC segment.
    END IF
NEXT

RANDOMIZE TIMER

z = MULTIKEY(-1)
tmr = 15
stars = 40
CALL initstars
st = 0
x = 10
y = 90
GET (x, y)-(x + (bship%(0, 0) \ 8), y + bship%(1, 0) + 1), back%(0, 0)

FOR I = 0 TO entype% - 1
    GET (x, y)-(x + (eship%(0, 0, I) \ 8), y + eship%(1, 0, I)), back%(0, I + 1)
NEXT

DO WHILE MULTIKEY(1) = 0
    tme = tme + 1
    tme3 = tme3 + 1
    IF tme3 > tmr * 25 THEN tme4 = tme4 + 1: tme3 = 0
    IF enum2% > 5 THEN enum2% = 0
    FOR I = 0 TO enum% - 1
        IF tme4 = ex%(I) THEN
            ebd% = -1
            ebd2% = -1
            ex%(I) = -1
            esdmg%(enum2%) = 1
            eshot%(enum2%, eshot2%(enum2%)) = 0
            estyp%(enum2%) = etype%(I)
            IF estyp%(enum2%) = 1 THEN esdmg%(enum2%) = 5
            IF estyp%(enum2%) = 2 THEN esdmg%(enum2%) = 20
            IF estyp%(enum2%) = 3 THEN esdmg%(enum2%) = 80
            esd%(enum2%) = 1
            esx%(enum2%) = 300
            esy%(enum2%) = ey%(I)
            ep2%(enum2%) = ep%(I)
            espd2%(enum2%) = espd%(I)
            tme5%(enum2%) = 0
            enum2% = enum2% + 1
            enum4% = enum4% + 1
        END IF
    NEXT
   
    IF esd%(enum3%) = 1 THEN
        tme5%(enum3%) = tme5%(enum3%) + 1
        IF espd2%(enum3%) = tme5%(enum3%) THEN
            SELECT CASE ep2%(enum3%)
                CASE 0:
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 290 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 150 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) + ebd%
                    esy%(enum3%) = esy%(enum3%) + ebd2%
                    IF esx%(enum3%) < 240 THEN ebd% = 1
                    IF esx%(enum3%) > 269 THEN ebd% = -1
                    IF esy%(enum3%) > 140 THEN ebd2% = -1
                    IF esy%(enum3%) < 20 THEN ebd2% = 1
                  
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 280 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 1:
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 2:
                    tme6%(enum3%) = tme6%(enum3%) + 1
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    IF esy%(enum3%) > 20 THEN
                        esy%(enum3%) = esy%(enum3%) - 1
                        tme6%(enum3%) = 0
                    END IF
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 3:
                    tme6%(enum3%) = tme6%(enum3%) + 1
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    IF esy%(enum3%) < 180 THEN
                        esy%(enum3%) = esy%(enum3%) + 1
                        tme6%(enum3%) = 0
                    END IF
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 4:
                    tme6%(enum3%) = tme6%(enum3%) + 1
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    IF esx%(enum3%) > 250 AND esy%(enum3%) > 20 THEN
                        esy%(enum3%) = esy%(enum3%) - 1
                        tme6%(enum3%) = 0
                    END IF
                    IF esx%(enum3%) < 150 THEN
                        esy%(enum3%) = esy%(enum3%) + 1
                        tme6%(enum3%) = 0
                    END IF
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 5:
                    tme6%(enum3%) = tme6%(enum3%) + 1
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    IF esx%(enum3%) > 250 AND esy%(enum3%) < 180 THEN
                        esy%(enum3%) = esy%(enum3%) + 1
                        tme6%(enum3%) = 0
                    END IF
                    IF esx%(enum3%) < 150 THEN
                        esy%(enum3%) = esy%(enum3%) - 1
                        tme6%(enum3%) = 0
                    END IF
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 6:
                    tme6%(enum3%) = tme6%(enum3%) + 1
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    IF tme6%(enum3%) > 2 THEN
                        esy%(enum3%) = esy%(enum3%) - 1
                        tme6%(enum3%) = 0
                    END IF
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF
                CASE 7:
                    tme6%(enum3%) = tme6%(enum3%) + 1
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                    END IF
                    END IF
                    esx%(enum3%) = esx%(enum3%) - 1
                    IF tme6%(enum3%) > 2 THEN
                        esy%(enum3%) = esy%(enum3%) + 1
                        tme6%(enum3%) = 0
                    END IF
                    tme5%(enum3%) = 0
                    IF esx%(enum3%) > 5 AND esx%(enum3%) < 300 THEN
                    IF esy%(enum3%) > 5 AND esy%(enum3%) < 160 THEN
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), OR
                        PUT (esx%(enum3%), esy%(enum3%)), eship%(0, esan%(enum3%), estyp%(enum3%)), AND
                    END IF
                    END IF

            END SELECT
        END IF
    END IF
    rcnt% = rcnt% + 1
    IF rcnt% > 30 THEN
        esan%(enum3%) = esan%(enum3%) + 1
        IF esan%(enum3%) > 2 THEN esan%(enum3%) = 0
        rcnt% = 0
    END IF
   
    IF estyp%(enum3%) = 3 AND esd%(enum3%) = 1 AND esx%(enum3%) < 280 THEN
        IF lscnt% > 15 AND lsx%(0) > 0 THEN
            lscnt% = 0: LINE (lsx%(0), lsy%(0))-(lsx%(1), lsy%(1)), 0
            lsx%(0) = -1
        END IF
        lscnt% = lscnt% + 1
        lscnt2% = lscnt2% + 1
        IF lscnt2% > 2000 THEN lscnt2% = 0
        yummy% = INT(RND * 30)
        IF yummy% = 2 AND lsx%(0) <= 0 AND lscnt2% < 500 THEN
            dmg% = dmg% - 1
            LINE (x + 15, y + 5)-(esx%(enum3%), esy%(enum3%)), 4
            lsx%(0) = x + 15: lsx%(1) = esx%(enum3%)
            lsy%(0) = y + 5: lsy%(1) = esy%(enum3%)
        END IF
    END IF
  
   
'    IF eshot%(enum3%, eshot2%(enum3%)) > 4 THEN eshot%(enum3%, eshot2%(enum3%)) = 0
        yummy% = INT(RND * 5)
        IF yummy% = 2 AND estyp%(enum3%) <> 2 AND x1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) <= 0 AND esd%(enum3%) = 1 AND esx%(enum3%) > x THEN
            x1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) = esx%(enum3%): x2% = x
            y1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) = esy%(enum3%): y2% = y
            'CIRCLE (x1%(enum3%, eshot%(enum3%, eshot2%(enum3%))), y1%(enum3%, eshot%(enum3%, eshot2%(enum3%)))), 7
            d% = INT((SQR(ABS(y1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) - y2%) ^ 2 + ABS(x1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) - x2%) ^ 2)) / 5) + 1
            xch%(enum3%, eshot%(enum3%, eshot2%(enum3%))) = (x1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) - x2%) \ d%
            ych%(enum3%, eshot%(enum3%, eshot2%(enum3%))) = (y1%(enum3%, eshot%(enum3%, eshot2%(enum3%))) - (y2% + INT(RND * 100) - 50)) \ d%
            eshot%(enum3%, eshot2%(enum3%)) = 1
        END IF
   
    IF x1%(enum3%, eshot2%(enum3%)) > 1 AND x1%(enum3%, eshot2%(enum3%)) < 320 THEN
    IF y1%(enum3%, eshot2%(enum3%)) > 1 AND y1%(enum3%, eshot2%(enum3%)) < 180 THEN
        DEF SEG = &HA000
        POKE x1%(enum3%, eshot2%(enum3%)) + (y1%(enum3%, eshot2%(enum3%)) * 320&), 0
        POKE (x1%(enum3%, eshot2%(enum3%)) + 1) + ((y1%(enum3%, eshot2%(enum3%)) - 1) * 320&), 0
        POKE (x1%(enum3%, eshot2%(enum3%)) + 1) + ((y1%(enum3%, eshot2%(enum3%)) + 1) * 320&), 0
        POKE (x1%(enum3%, eshot2%(enum3%)) + 1) + (y1%(enum3%, eshot2%(enum3%)) * 320&), 0
        POKE (x1%(enum3%, eshot2%(enum3%)) + 2) + (y1%(enum3%, eshot2%(enum3%)) * 320&), 0
        DEF SEG
    END IF
    END IF
    IF x1%(enum3%, eshot2%(enum3%)) <> 0 THEN
        x1%(enum3%, eshot2%(enum3%)) = x1%(enum3%, eshot2%(enum3%)) - xch%(enum3%, eshot2%(enum3%))
        y1%(enum3%, eshot2%(enum3%)) = y1%(enum3%, eshot2%(enum3%)) - ych%(enum3%, eshot2%(enum3%))
    END IF
    'CIRCLE (x1%(enum3%, eshot2%(enum3%)), y1%(enum3%, eshot2%(enum3%))), 10, 0
    IF x1%(enum3%, eshot2%(enum3%)) > 1 AND x1%(enum3%, eshot2%(enum3%)) < 320 THEN
    IF y1%(enum3%, eshot2%(enum3%)) > 1 AND y1%(enum3%, eshot2%(enum3%)) < 180 THEN
        DEF SEG = &HA000
        POKE x1%(enum3%, eshot2%(enum3%)) + (y1%(enum3%, eshot2%(enum3%)) * 320&), 2
        POKE (x1%(enum3%, eshot2%(enum3%)) + 1) + ((y1%(enum3%, eshot2%(enum3%)) - 1) * 320&), 2
        POKE (x1%(enum3%, eshot2%(enum3%)) + 1) + ((y1%(enum3%, eshot2%(enum3%)) + 1) * 320&), 2
        POKE (x1%(enum3%, eshot2%(enum3%)) + 1) + (y1%(enum3%, eshot2%(enum3%)) * 320&), 10
        POKE (x1%(enum3%, eshot2%(enum3%)) + 2) + (y1%(enum3%, eshot2%(enum3%)) * 320&), 2
        DEF SEG
    END IF
    END IF
    'CIRCLE (x1%(enum3%, eshot2%(enum3%)), y1%(enum3%, eshot2%(enum3%))), 10, 2
   
    IF x1%(enum3%, eshot2%(enum3%)) > x AND x1%(enum3%, eshot2%(enum3%)) < x + 20 THEN
    IF y1%(enum3%, eshot2%(enum3%)) > y AND y1%(enum3%, eshot2%(enum3%)) < y + 20 THEN
        FOR I = 0 TO 6
            CIRCLE (x + 10, y + 5), I, 4
            CIRCLE (x + 11, y + 5), I, 4
            IF I < 4 THEN CIRCLE (x + 10, y + 5), I, 14
            IF I < 4 THEN CIRCLE (x + 11, y + 5), I, 14
            IF I < 2 THEN CIRCLE (x + 10, y + 5), I, 15
            IF I < 2 THEN CIRCLE (x + 11, y + 5), I, 15
        NEXT
        dmg% = dmg% - 1
        IF dmg% <= 0 THEN GOTO 1
    END IF
    END IF
    'END IF
    'LOCATE 1, 1
    'PRINT excnt%
    IF st2 > 0 THEN
        IF px(st2) > esx%(enum3%) - 5 AND px(st2) < esx%(enum3%) + eship%(0, 1, estyp%(enum3%)) / 8 THEN
        IF py(st2) > esy%(enum3%) - 5 AND py(st2) < esy%(enum3%) + eship%(1, 1, estyp%(enum3%)) THEN
            esdmg%(enum3%) = esdmg%(enum3%) - 1
            FOR I = 0 TO 4
                CIRCLE (esx%(enum3%) + 5, esy%(enum3%) + 5), I, 4
                CIRCLE (esx%(enum3%) + 4, esy%(enum3%) + 5), I, 4
            NEXT
            IF esdmg%(enum3%) < 0 THEN
                esstyp% = estyp%(enum3%)
                esd%(enum3%) = 0
                PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
                expy%(excnt%) = esy%(enum3%)
                expx%(excnt%) = esx%(enum3%)
                excnt% = excnt% + 1
                IF excnt% > 8 THEN excnt% = 0
                esx%(enum3%) = -100: esy%(enum3%) = -100
            END IF
        END IF
        END IF
    END IF
   
    IF x > esx%(enum3%) - 15 AND x < esx%(enum3%) + eship%(0, 1, estyp%(enum3%)) / 8 AND esx%(enum3%) > 15 THEN
    IF y > esy%(enum3%) - 15 AND y < esy%(enum3%) + eship%(1, 1, estyp%(enum3%)) THEN
        dmg% = dmg% - 10
        IF estyp%(enum3%) <> 3 THEN
            IF estyp%(enum3%) = 2 THEN dmg% = dmg% - 20
            esd%(enum3%) = 0
            PUT (esx%(enum3%), esy%(enum3%)), back%(0, estyp%(enum3%) + 1), PSET
            expy%(excnt%) = esy%(enum3%)
            expx%(excnt%) = esx%(enum3%)
            excnt% = excnt% + 1
            IF excnt% > 8 THEN excnt% = 0
            esx%(enum3%) = -100: esy%(enum3%) = -100
        END IF
    END IF
    END IF
    IF expx%(excnt2%) > 1 THEN
        'CIRCLE (expx%(excnt2%), expy%(excnt2%)), 6, 4
        IF excnt3% = 15 THEN
            IF expx%(excnt2%) > 5 AND expx%(excnt2%) < 280 THEN
                IF anme%(excnt2%) < 5 THEN PUT (expx%(excnt2%) - 5, expy%(excnt2%)), explode%(0, anme%(excnt2%)), PSET
                IF anme%(excnt2%) = 6 THEN
                    PUT (expx%(excnt2%) - 5, expy%(excnt2%)), back%(0, 0), PSET
                    expx%(excnt2%) = 0
                    kills% = kills% + 1
                    IF esstyp% = 3 THEN mission% = 1: GOTO 1
                END IF
            END IF
            anme%(excnt2%) = anme%(excnt2%) + 1
            excnt3% = 0
        END IF
        excnt3% = excnt3% + 1
    END IF
   
    excnt2% = excnt2% + 1
    IF excnt2% > 8 THEN excnt2% = 0
    eshot2%(enum3%) = eshot2%(enum3%) + 1
    IF eshot2%(enum3%) > 5 THEN eshot2%(enum3%) = 0
           
    'LOCATE 1, 1
    'PRINT enum%; "    "
    'PRINT enum4%; "    "
    enum3% = enum3% + 1
    IF enum3% > 5 THEN enum3% = 0

    'LOCATE 1, 1
    'PRINT tme5
    IF st > 10 THEN st = 1
    tme2 = tme2 + 1
    IF tme2 > 30000 THEN tme2 = 0
    IF tme > tmr THEN
        IF MULTIKEY(72) = 1 THEN y = y - 1
        IF MULTIKEY(75) = 1 THEN x = x - 1
        IF MULTIKEY(77) = 1 THEN x = x + 1
        IF MULTIKEY(80) = 1 THEN y = y + 1
        IF MULTIKEY(57) = 1 AND tme2 > tmr * 50 THEN
            fire(st) = 1: px(st) = x + 40: py(st) = y + 7
            st = st + 1: tme2 = 0
        END IF
        tme = 0
    END IF
    st2 = st2 + 1
    IF st2 > 10 THEN st2 = 0
   
    IF fire(st2) = 1 THEN
        px(st2) = px(st2) + 1
        
        IF st2 <> 0 THEN
                'PSET (px(st2), py(st2)), 4
            IF px(st2) > 1 AND px(st2) < 320 THEN
                DEF SEG = &HA000
                POKE px(st2) + (py(st2) * 320&), 4
                POKE (px(st2) - 6) + (py(st2) * 320&), 4
                POKE (px(st2) - 1) + (py(st2) * 320&), 4
                POKE (px(st2) - 3) + (py(st2) * 320&), 40
                POKE (px(st2) - 4) + (py(st2) * 320&), 40
                POKE (px(st2) - 5) + (py(st2) * 320&), 42
                POKE (px(st2) - 3) + ((py(st2) + 1) * 320&), 40
                POKE (px(st2) - 4) + ((py(st2) + 1) * 320&), 40
                POKE (px(st2) - 5) + ((py(st2) + 1) * 320&), 42
                POKE (px(st2) - 3) + ((py(st2) - 1) * 320&), 40
                POKE (px(st2) - 4) + ((py(st2) - 1) * 320&), 40
                POKE (px(st2) - 5) + ((py(st2) - 1) * 320&), 42
                DEF SEG
            END IF
        END IF
        IF st2 < 20 THEN
            IF px(st2 + 1) > 1 AND px(st2 + 1) < 320 THEN
                'PSET (px(st2 + 1), py(st2 + 1)), 0
                DEF SEG = &HA000
                POKE px(st2 + 1) + (py(st2 + 1) * 320&), 0
                POKE (px(st2 + 1) - 6) + (py(st2 + 1) * 320&), 0
                POKE (px(st2 + 1) - 1) + (py(st2 + 1) * 320&), 0
                POKE (px(st2 + 1) - 3) + (py(st2 + 1) * 320&), 0
                POKE (px(st2 + 1) - 4) + (py(st2 + 1) * 320&), 0
                POKE (px(st2 + 1) - 5) + (py(st2 + 1) * 320&), 0
                POKE (px(st2 + 1) - 3) + ((py(st2 + 1) + 1) * 320&), 0
                POKE (px(st2 + 1) - 4) + ((py(st2 + 1) + 1) * 320&), 0
                POKE (px(st2 + 1) - 5) + ((py(st2 + 1) + 1) * 320&), 0
                POKE (px(st2 + 1) - 3) + ((py(st2 + 1) - 1) * 320&), 0
                POKE (px(st2 + 1) - 4) + ((py(st2 + 1) - 1) * 320&), 0
                POKE (px(st2 + 1) - 5) + ((py(st2 + 1) - 1) * 320&), 0
                DEF SEG
            END IF
        END IF
    END IF

    IF x < 10 THEN x = 10
    IF x > 300 - (bship%(0, 0) \ 8) THEN x = 300 - (bship%(0, 0) \ 8)
    IF y < 10 THEN y = 10
    IF y > 180 - bship%(1, 0) THEN y = 180 - (bship%(1, 0))
   
    atme = atme + 1
    IF atme = tmr * 80 THEN
        a = a + 1
        IF a > 2 THEN a = 0
        atme = 0
        PUT (x, y), bship%(0, a + 3), AND
        PUT (x, y), bship%(0, a), OR
   END IF
   
    'WAIT &H3DA, 8:
    FOR I = 1 TO 100: NEXT
    IF x <> mx OR y <> my THEN PUT (mx, my), back%(0, 0), PSET
   
    CALL mstars
    IF x <> mx OR y <> my THEN
        'GET (x - 1, y - 1)-(x + (bship%(0, 0) \ 8) + 1, y + bship%(1, 0) + 1), back%(0, 0)
        PUT (x, y), bship%(0, a + 3), AND
        PUT (x, y), bship%(0, a), OR
    END IF
    mx = x: my = y
    IF dmg% < 0 THEN 1
    IF dmg% <> dmg3% THEN CALL vsccrn(dmg%)
    dmg3% = dmg%

LOOP
1
z = MULTIKEY(-2)
CLS
SCREEN 12
IF mission% <> 1 THEN PRINT "YOUR DEAD!"
IF mission% = 1 THEN
    PRINT "MISSION COMPLETE"
    PRINT "----------------"
    scr% = kills% * 10
    PRINT "KILLS: "; kills%; "* 10"
    IF kills% >= enum% THEN
        PRINT "BONUS:"; kills% * 10; "* 5"
        scr% = scr% * 5
    END IF
    PRINT "SCORE: "; scr%
END IF
PRINT
PRINT "THANKS FOR PLAYING MY GAME."
DO UNTIL INKEY$ <> "": LOOP
SYSTEM

SUB initstars
FOR I = 0 TO stars
    stx(I) = INT(RND * 320)
    sty(I) = INT(RND * 180)
    stc(I) = INT(RND * 4) + 32
NEXT

END SUB

SUB keytest

SCREEN 0
CLS

z = MULTIKEY(-1)
DO
x = 1
y = 1

 FOR I = 1 TO 128
 
  TEST = MULTIKEY(I)
  LOCATE y, x
  PRINT USING "## =###"; TEST; I

  IF y < 23 THEN
   y = y + 1
  ELSE
   y = 1
   x = x + 9
  END IF

 NEXT I

LOOP WHILE MULTIKEY(1) = 0

z = MULTIKEY(-2)

END

END SUB

SUB mstars

IF stc(cnt) = 32 THEN spd = 55 \ tmr
IF stc(cnt) = 33 THEN spd = 50 \ tmr
IF stc(cnt) = 34 THEN spd = 35 \ tmr
IF stc(cnt) = 35 THEN spd = 20 \ tmr

fff = 1
IF stx(cnt) > x - 5 AND stx(cnt) < x + (bship%(0, 0) \ 8) + 5 THEN
    IF sty(cnt) > y - 5 AND sty(cnt) < y + bship%(1, 0) + 5 THEN
        fff = 0
        PSET (stx(cnt), sty(cnt)), POINT(stx(cnt) - 1, sty(cnt))
        'stx(cnt) = 320: sty(cnt) = INT(RND * 200)
    END IF
END IF

stx(cnt) = stx(cnt) - spd

IF stx(cnt) >= 0 AND stx(cnt) <= 320 AND fff = 1 THEN
    DEF SEG = &HA000
    POKE (stx(cnt) + spd) + (sty(cnt) * 320&), 0
    DEF SEG
   
    DEF SEG = &HA000
    POKE stx(cnt) + (sty(cnt) * 320&), stc(cnt)
    DEF SEG
END IF
cnt = cnt + 1
IF cnt > stars THEN cnt = 0

IF stx(cnt) < 10 THEN
    PSET (stx(cnt), sty(cnt)), 0
    stx(cnt) = 320: sty(cnt) = INT(RND * 180)
END IF

END SUB

FUNCTION MULTIKEY (T)

STATIC kbcontrol%(), kbmatrix%(), Firsttime, StatusFlag

IF Firsttime = 0 THEN          'Initalize
 DIM kbcontrol%(128)
 DIM kbmatrix%(128)
 code$ = ""
 code$ = code$ + "E91D00E93C00000000000000000000000000000000000000000000000000"
 code$ = code$ + "00001E31C08ED8BE24000E07BF1400FCA5A58CC38EC0BF2400B85600FAAB"
 code$ = code$ + "89D8ABFB1FCB1E31C08EC0BF2400BE14000E1FFCFAA5A5FB1FCBFB9C5053"
 code$ = code$ + "51521E560657E460B401A8807404B400247FD0E088C3B700B0002E031E12"
 code$ = code$ + "002E8E1E100086E08907E4610C82E661247FE661B020E6205F075E1F5A59"
 code$ = code$ + "5B589DCF"
 DEF SEG = VARSEG(kbcontrol%(0))
 FOR I% = 0 TO 155                     ' Load ASM
     d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
     POKE VARPTR(kbcontrol%(0)) + I%, d%
 NEXT I%
 I& = 16       ' I think this stuff connects the interrupt with kbmatrix%()
 N& = VARSEG(kbmatrix%(0)): l& = N& AND 255: h& = ((N& AND &HFF00) \ 256): POKE I&, l&: POKE I& + 1, h&: I& = I& + 2
 N& = VARPTR(kbmatrix%(0)): l& = N& AND 255: h& = ((N& AND &HFF00) \ 256): POKE I&, l&: POKE I& + 1, h&: I& = I& + 2
 DEF SEG
 Firsttime = 1
END IF

SELECT CASE T
 CASE -1
  IF StatusFlag = 0 THEN
   DEF SEG = VARSEG(kbcontrol%(0))
   CALL ABSOLUTE(0)                     ' Run interrupt
   DEF SEG
   StatusFlag = 1
  END IF
 CASE -2
  IF StatusFlag = 1 THEN
   DEF SEG = VARSEG(kbcontrol%(0))      ' Turn off interrupt
   CALL ABSOLUTE(3)
   DEF SEG
   StatusFlag = 0
  END IF
 CASE 1 TO 128
  MULTIKEY = kbmatrix%(T)               ' Return status
 CASE ELSE
  MULTIKEY = 0                          ' User Supidity Error
END SELECT

END FUNCTION

SUB vsccrn (dmg2%)
    LINE (1, 182)-(319, 199), , B
    PAINT (5, 195), 1, 15
    LINE (10, 185)-((dmg2% * 3) + 10, 195), 4, BF

END SUB

