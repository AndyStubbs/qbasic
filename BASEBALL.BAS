' ANDY'S BASEBALL GAME
' A TWO PLAYER ONLY GAME
' PICTHES : 8 - SLOWBALL , 5 - MEDIUM PICTH, 2 - FASTBALL
' SPACEBAR SWINGS

DECLARE SUB R1 ()
DECLARE SUB R2 ()
DECLARE SUB R3 ()
DECLARE SUB R4 ()
DECLARE SUB R5 ()
DECLARE SUB R6 ()
DECLARE SUB TN ()
DECLARE SUB SWING2 ()
DECLARE SUB INTRO8 ()
DECLARE SUB SLL ()
DECLARE SUB INTRO4 ()
DECLARE SUB INTRO5 ()
DECLARE SUB INTRO3 ()
DECLARE SUB INTRO1 ()
DECLARE SUB INTRO2 ()
DECLARE SUB SETPB ()
DECLARE SUB RFIELD ()
DECLARE SUB SETP ()
DECLARE SUB GR ()
DECLARE SUB RN ()
DECLARE SUB GFIELD ()
DECLARE SUB OUTFIELD ()
DECLARE SUB BLL ()
DECLARE SUB GLL ()
DECLARE SUB SWING ()
DECLARE SUB DIAMOND ()
DECLARE SUB BFIELD ()
DECLARE SUB B3 ()
DECLARE SUB T5 ()
DECLARE SUB T6 ()
DECLARE SUB B5 ()
DECLARE SUB B6 ()
DECLARE SUB B1 ()
DECLARE SUB B2 ()
DECLARE SUB B4 ()
DECLARE SUB T1 ()
DECLARE SUB T2 ()
DECLARE SUB T3 ()
DECLARE SUB T4 ()
IN = 1
GOTO 5700
1 OT = 0
10 SCREEN 7
PRINT "INNING:"; IN
PRINT "HOME:"; H; " VISITORS:"; V
PRINT "STRIKE:"; STK; "OUT:"; OT;
RN0$ = "T"
11 IF RN1$ = "T" THEN 14
12 IF RN2$ = "T" THEN 15
13 IF RN3$ = "T" THEN 16 ELSE 17
14  PSET (240, 90)
CALL RN
IF RN2$ = "T" THEN 15 ELSE GOTO 17
IF RN3$ = "T" THEN 16 ELSE 17
15 PSET (153, 20)
CALL RN
IF RN3$ = "T" THEN 16 ELSE GOTO 17
16 PSET (90, 100)
CALL RN
17 IF STK = 3 THEN PRINT "OUT": STK = 0: OT = OT + 1
18 IF OT >= 3 THEN 2600
19 SCREEN 7
20 COLOR 15, (2)
25 PSET (160, 100)
30 CALL DIAMOND
40 PSET (159, 89)
50 CALL BFIELD
60 PSET (152, 96)
70 CALL BLL
80 PSET (149, 165)
90 CALL T1
100 PSET (94, 74)
110 CALL BFIELD
120 PSET (227, 80)
130 CALL BFIELD
140 PSET (185, 40)
150 CALL BFIELD
160 PSET (134, 40)
170 CALL BFIELD
180 B = 153
190 BB = 87
200 G = 153
210 GG = 87
DO
220 Z$ = INKEY$
IF Z$ = "Q" THEN END
230 IF Z$ = "2" THEN S = .3: SS = .03: PPP = 20
240 IF Z$ = "5" THEN S = .2:  SS = .02: PPP = 10
245 IF Z$ = "8" THEN S = .1: SS = .01: PPP = 1
LOOP UNTIL Z$ = "2" OR Z$ = "5" OR Z$ = "8"
PSET (152, 96)
CALL GLL
250 B = 152
260 DO
270 Y$ = INKEY$
275 IF Y$ = "B" THEN PSET (149, 165): CALL T6: GOTO 285
280 IF Y$ = " " THEN CALL SWING ELSE 290
285 IF BB >= 150 THEN 400 ELSE GOTO 380
290 PSET (B, BB)
300 CALL BLL
310 PSET (B, GG)
320 CALL GLL
321 B = B + SS
330 BB = BB + S
340 GG = GG + S
350 LOOP UNTIL BB >= 180
360 PSET (159, 180)
370 CALL BLL
380 CLS
385 PRINT "STRIKE": STK = STK + 1
390 GOTO 10
400 STR = 50: HIT = 50
401 RANDOMIZE TIMER
402 RT = INT(RND * 500)
403 IF RT < 100 + HIT THEN BAT$ = "HIT": GOTO 408
404 RT = INT(RND * 300)
IF Y$ = "B" THEN BAT$ = "GROUNDOUT": SPD = .5: L = 80: GOTO 410
405 IF RT < 150 THEN BAT$ = "POPOUT" ELSE BAT$ = "GROUNDOUT": SPD = .5: L = 80: GOTO 410
408 SPD = 2
409 L = 0
410 IF BB <= 165 THEN PP = 5: QQ$ = "L": GOTO 500
411 IF BB <= 170 THEN PP = 20: QQ$ = "M": GOTO 600
420 IF BB >= 170 THEN PP = 5: QQ$ = "R": GOTO 700
500 DO
510 BB = BB - SPD
513 PSET (B, BB)
515 CALL BLL
PSET (B, BB)
CALL GLL
520 B = B - SPD
530 LOOP UNTIL BB <= L
531 D = STR + HIT + PP + PPP
532 RANDOMIZE TIMER
533 DT = INT(RND * 1200)
534 IF DT < D THEN 1000
550 GOTO 1500
600 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL GLL
610 BB = BB - SPD
620 LOOP UNTIL BB <= L
D = STR + HIT + PP + PPP
RANDOMIZE TIMER
DT = INT(RND * 1200)
IF DT < D THEN 1000
630 GOTO 1500
700 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL GLL
710 BB = BB - SPD
720 B = B + SPD
730 LOOP UNTIL BB <= L
D = STR + HIT + PP + PPP
RANDOMIZE TIMER
DT = INT(RND * 1200)
IF DT < D THEN 1000
750 GOTO 1500
1000 CLS
CALL BFIELD
PSET (100, 100)
CALL BFIELD
PSET (280, 100)
CALL BFIELD
1001 IF BAT$ = "GROUNDOUT" THEN 1500 ELSE 1002
1002 CALL OUTFIELD
1003 FT = INT(RND * 500)
1004 IF RN1$ = "T" THEN V = V + 1: RN1$ = "F"
1005 IF RN2$ = "T" THEN V = V + 1: RN2$ = "F"
1006 IF RN3$ = "T" THEN V = V + 1: RN3$ = "F"
1007 V = V + 1
1010 IF FT < 250 THEN FT = FT + 250
1020 PRINT "A"; FT; "FT HOME RUN!"
1021 IF FT > 450 THEN BAA = 2: BAAA = 10
1022 IF FT < 300 THEN BAA = .3: BAAA = 50 ELSE BAA = .6: BAAA = 40
1023 BB = 180
1040 B = 159
1061 IF QQ$ = "L" THEN 1085
1062 IF QQ$ = "M" THEN 1185
1063 IF QQ$ = "R" THEN 1285
1085 DO
1090 PSET (B, BB)
1100 CALL BLL
1101 PSET (B, BB)
1102 CALL GLL
1110 BB = BB - BAA
1115 B = B - BAA
1120 LOOP UNTIL BB <= BAAA
1140 GOTO 1400
1185 DO
1190 PSET (B, BB)
1200 CALL BLL
1201 PSET (B, BB)
1202 CALL GLL
1210 BB = BB - BAA
1220 LOOP UNTIL BB <= BAAA
1240 GOTO 1400
1285 DO
1290 PSET (B, BB)
1300 CALL BLL
1301 PSET (B, BB)
1302 CALL GLL
1310 BB = BB - BAA
1315 B = B + BAA
1320 LOOP UNTIL BB <= BAAA
1340 GOTO 1400
1400 DO
1440 ZORK = ZORK + 1
1450 LOOP UNTIL ZORK = 2000
1460 ZORK = 0
1470 CLS
1480 GOTO 10
1500 IF BAT$ = "GROUNDOUT" THEN 1505 ELSE GOTO 1700
1505 CLS : CALL BLL
1510 CALL SETP
1511 PSET (237, 90)
1520 CALL BFIELD
1530 PSET (227, 80)
1540 CALL GFIELD
1541 R = 149: RR = 165
1545 DO
1550 T$ = INKEY$
1560 IF T$ = "6" THEN 1610
1570 IF T$ = "8" THEN 2200
1580 IF T$ = "4" THEN 2300
1585 IF T$ = "2" THEN 2400
1586 PSET (R, RR)
1587 CALL RN
1591 PSET (R, RR)
1592 CALL GR
1593 R = R + 2
1594 RR = RR - 1.5
1595 LOOP UNTIL RR <= 95
 IF RN3$ = "T" THEN V = V + 1: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
1597 PSET (R, RR)
1598 CALL RN
1600 CLS : GOTO 10
1610
 IF RN3$ = "T" THEN V = V + 1: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN OT = OT: STK = 0: RN1$ = "F"
1615 DO
1620 IF B > 237 THEN B = B - 1 ELSE B = B + 1
1630 IF BB > 105 THEN BB = BB - 2 ELSE BB = BB + 2
1640 PSET (B, BB)
1650 CALL BLL
1652 PSET (B, BB)
1653 CALL GLL
1660 LOOP UNTIL B >= 237
1670 PRINT "OUT": OT = OT + 1: STK = 0
1680 CLS
1690 GOTO 10
1700 IF BAT$ = "POPOUT" THEN 1710 ELSE 2500
1710 CLS
1720 CALL OUTFIELD
1730 PSET (50, 100)
1740 CALL BFIELD
1750 PSET (150, 120)
1760 CALL BFIELD
1770 PSET (220, 100)
1780 CALL BFIELD
1790 B = 159: BB = 180
1800 IF QQ$ = "L" THEN 1805
1801 IF QQ$ = "M" THEN 1900
1802 IF QQ$ = "R" THEN 1970
1805
1806 F = 50: FF = 100
1807 DO
1810 B = B - .8
1815 BB = BB - .8
1820 IF F > B THEN F = F - .7 ELSE F = F + .7
1825 IF FF > BB THEN FF = FF - .7 ELSE FF = FF + .7
1826 PSET (B, BB)
1828 CALL BLL
1830 PSET (B, BB)
1835 CALL GLL
1840 PSET (F, FF)
1845 CALL BFIELD
1850 PSET (F, FF)
1855 CALL GFIELD
1860 LOOP UNTIL F >= B
1870 GOTO 2100
1900 F = 150: FF = 120
1910 DO
1915 BB = BB - .8
1920 IF F > B THEN F = F - .7 ELSE F = F + .7
1925 IF FF > BB THEN FF = FF - .7 ELSE FF = FF + .7
1926 PSET (B, BB)
1928 CALL BLL
1930 PSET (B, BB)
1935 CALL GLL
1940 PSET (F, FF)
1945 CALL BFIELD
1950 PSET (F, FF)
1955 CALL GFIELD
1960 LOOP UNTIL FF >= BB
1965 GOTO 2100
1970 F = 220: FF = 100
2010 DO
2011 B = B + .8
2015 BB = BB - .8
2020 IF F > B THEN F = F - .7 ELSE F = F + .7
2025 IF FF > BB THEN FF = FF - .7 ELSE FF = FF + .7
2026 PSET (B, BB)
2028 CALL BLL
2030 PSET (B, BB)
2035 CALL GLL
2040 PSET (F, FF)
2045 CALL BFIELD
2050 PSET (F, FF)
2055 CALL GFIELD
2060 LOOP UNTIL FF >= BB
2065 GOTO 2100
2100 OT = OT + 1: STK = 0
2110 DO UNTIL PIG = 200
2120 PIG = PIG + 1
2130 LOOP
2140 CLS : GOTO 10
2200 DO
2210 IF B > 185 THEN B = B - .5 ELSE B = B + .5
2230 IF BB > 40 THEN BB = BB - .5 ELSE BB = BB + .5
2240 PSET (B, BB)
2250 CALL BLL
2252 PSET (B, BB)
2253 CALL GLL
2260 LOOP UNTIL BB <= 40
2070
 
 IF RN3$ = "T" THEN V = V + 1: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN OT = OT + 1: STK = 0: RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"

2280 CLS : GOTO 10
2300
 
 IF RN3$ = "T" THEN V = V + 1: RN3$ = "F"
 IF RN2$ = "T" THEN OT = OT + 1: STK = 0: RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
2310 DO
2320 IF B > 74 THEN B = B - 1 ELSE B = B + 1
2330 IF BB > 94 THEN BB = BB - 2 ELSE BB = BB + 2
2340 PSET (B, BB)
2350 CALL BLL
2352 PSET (B, BB)
2353 CALL GLL
2360 LOOP UNTIL B <= 94
2370
2380 CLS : GOTO 10
2400
 IF RN3$ = "T" THEN OT = OT + 1: STK = 0: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
2410 DO
2420 IF B > 159 THEN B = B - 1 ELSE B = B + 1
2430 IF BB < 165 THEN BB = BB + 1 ELSE BB = BB - 1
2440 PSET (B, BB)
2450 CALL BLL
2452 PSET (B, BB)
2453 CALL GLL
2460 LOOP UNTIL BB >= 165
2480 CLS : GOTO 10

2500 CLS
CALL OUTFIELD
PSET (70, 100)
CALL BFIELD
PSET (170, 90)
CALL BFIELD
PSET (280, 100)
CALL BFIELD
 BB = 180
 B = 159
 IF QQ$ = "L" THEN 2510
 IF QQ$ = "M" THEN 2520
 IF QQ$ = "R" THEN 2530
2510 DO
PSET (B, BB)
CALL BLL
PSET (B, BB)
CALL GLL
BB = BB - .5
B = B - .5
LOOP UNTIL BB <= 100
GOTO 2540
2520 DO
PSET (B, BB)
CALL BLL
PSET (B, BB)
CALL GLL
BB = BB - .5
LOOP UNTIL BB <= 100
GOTO 2540
2530 DO
PSET (B, BB)
CALL BLL
PSET (B, BB)
CALL GLL
BB = BB - .5
B = B + .5
LOOP UNTIL BB <= 100
2540 DO
GGGG = GGGG + 1
LOOP UNTIL GGGG = 100
GGGG = 0
IF RN3$ = "T" THEN V = V + 1: RN3$ = "F"
IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
IF RN0$ = "T" THEN RN1$ = "T"

CLS : GOTO 10
2600 OT = 0: CLS
     RN1$ = "F"
     RN2$ = "F"
     RN3$ = "F"
2610 CLS
3010 SCREEN 7
PRINT "INNING:"; IN
PRINT "HOME:"; H; "VISITORS:"; V
PRINT "STRIKE:"; STK; "OUT:"; OT;
RN0$ = "T"
3011 IF RN1$ = "T" THEN 3014
3012 IF RN2$ = "T" THEN 3015
3013 IF RN3$ = "T" THEN 3016 ELSE 3017
3014  PSET (240, 90)
CALL TN
IF RN2$ = "T" THEN 3015 ELSE GOTO 3017
IF RN3$ = "T" THEN 3016 ELSE 3017
3015 PSET (153, 20)
CALL TN
IF RN3$ = "T" THEN 3016 ELSE GOTO 3017
3016 PSET (90, 100)
CALL TN
3017 IF STK = 3 THEN PRINT "OUT": STK = 0: OT = OT + 1
3018 IF OT >= 3 THEN 5600
3019 SCREEN 7
3020 COLOR 15, (2)
3025 PSET (160, 100)
3030 CALL DIAMOND
3040 PSET (159, 89)
3050 CALL RFIELD
3060 PSET (152, 96)
3070 CALL BLL
3080 PSET (149, 165)
3090 CALL R1
3100 PSET (94, 74)
3110 CALL RFIELD
3120 PSET (227, 80)
3130 CALL RFIELD
3140 PSET (185, 40)
3150 CALL RFIELD
3160 PSET (134, 40)
3170 CALL RFIELD
3180 B = 153
3190 BB = 87
3200 G = 153
3210 GG = 87
DO
3220 Z$ = INKEY$
IF Z$ = "Q" THEN END
3230 IF Z$ = "2" THEN S = .3: SS = .03: PPP = 20
3240 IF Z$ = "5" THEN S = .2:  SS = .02: PPP = 10
3245 IF Z$ = "8" THEN S = .1: SS = .01: PPP = 1
LOOP UNTIL Z$ = "2" OR Z$ = "5" OR Z$ = "8"
PSET (152, 96)
CALL GLL
3250 B = 152
3260 DO
3270 Y$ = INKEY$
3275 IF Y$ = "B" THEN PSET (149, 165): CALL R6: GOTO 3285
3280 IF Y$ = " " THEN CALL SWING2 ELSE 3290
3285 IF BB >= 150 THEN 3400 ELSE GOTO 3380
3290 PSET (B, BB)
3300 CALL BLL
3310 PSET (B, GG)
3320 CALL GLL
3321 B = B + SS
3330 BB = BB + S
3340 GG = GG + S
3350 LOOP UNTIL BB >= 180
3360 PSET (159, 180)
3370 CALL BLL
3380 CLS
3385 PRINT "STRIKE": STK = STK + 1
3390 GOTO 2610
3400 STR = 50: HIT = 50
3401 RANDOMIZE TIMER
3402 RT = INT(RND * 500)
3403 IF RT < 100 + HIT THEN BAT$ = "HIT": GOTO 3408
3404 RT = INT(RND * 300)
IF Y$ = "B" THEN BAT$ = "GROUNDOUT": SPD = .5: L = 80: GOTO 3410
3405 IF RT < 200 THEN BAT$ = "POPOUT" ELSE BAT$ = "GROUNDOUT": SPD = .5: L = 80: GOTO 3410
3408 SPD = 2
3409 L = 0
3410 IF BB <= 165 THEN PP = 5: QQ$ = "L": GOTO 3500
3411 IF BB <= 170 THEN PP = 20: QQ$ = "M": GOTO 3600
3420 IF BB >= 170 THEN PP = 5: QQ$ = "R": GOTO 3700
3500 DO
3510 BB = BB - SPD
3513 PSET (B, BB)
3515 CALL BLL
 PSET (B, BB)
CALL GLL
3520 B = B - SPD
3530 LOOP UNTIL BB <= L
3531 D = STR + HIT + PP + PPP
3532 RANDOMIZE TIMER
3533 DT = INT(RND * 1200)
3534 IF DT < D THEN 4000
3550 GOTO 4500
3600 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL GLL
3610 BB = BB - SPD
3620 LOOP UNTIL BB <= L
D = STR + HIT + PP + PPP
RANDOMIZE TIMER
DT = INT(RND * 1200)
IF DT < D THEN 4000
3630 GOTO 4500
3700 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL GLL
3710 BB = BB - SPD
3720 B = B + SPD
3730 LOOP UNTIL BB <= L
D = STR + HIT + PP + PPP
RANDOMIZE TIMER
DT = INT(RND * 1200)
IF DT < D THEN 4000
3750 GOTO 4500
4000 CLS
CALL RFIELD
PSET (100, 100)
CALL RFIELD
PSET (280, 100)
CALL RFIELD
4001 IF BAT$ = "GROUNDOUT" THEN 4500 ELSE 4002
4002 CALL OUTFIELD
4003 FT = INT(RND * 500)
4004 IF RN1$ = "T" THEN H = H + 1: RN1$ = "F"
4005 IF RN2$ = "T" THEN H = H + 1: RN2$ = "F"
4006 IF RN3$ = "T" THEN H = H + 1: RN3$ = "F"
4007 H = H + 1
4010 IF FT < 250 THEN FT = FT + 250
4020 PRINT "A"; FT; "FT HOME RUN!"
4021 IF FT > 450 THEN BAA = 2: BAAA = 10
4022 IF FT < 300 THEN BAA = .3: BAAA = 50 ELSE BAA = .6: BAAA = 40
4023 BB = 180
4040 B = 159
4061 IF QQ$ = "L" THEN 4085
4062 IF QQ$ = "M" THEN 4185
4063 IF QQ$ = "R" THEN 4285
4085 DO
4090 PSET (B, BB)
4100 CALL BLL
4101 PSET (B, BB)
4102 CALL GLL
4110 BB = BB - BAA
4115 B = B - BAA
4120 LOOP UNTIL BB <= BAAA
4140 GOTO 4400
4185 DO
4190 PSET (B, BB)
4200 CALL BLL
4201 PSET (B, BB)
4202 CALL GLL
4210 BB = BB - BAA
4220 LOOP UNTIL BB <= BAAA
4240 GOTO 4400
4285 DO
4290 PSET (B, BB)
4300 CALL BLL
4301 PSET (B, BB)
4302 CALL GLL
4310 BB = BB - BAA
4315 B = B + BAA
4320 LOOP UNTIL BB <= BAAA
4340 GOTO 4400
4400 DO
4440 ZORK = ZORK + 1
4450 LOOP UNTIL ZORK = 2000
4460 ZORK = 0
4470 CLS
4480 GOTO 2610
4500 IF BAT$ = "GROUNDOUT" THEN 4505 ELSE GOTO 4700
4505 CLS : CALL BLL
4510 CALL SETPB
4511 PSET (237, 90)
4520 CALL RFIELD
4530 PSET (227, 80)
4540 CALL GFIELD
4541 R = 149: RR = 165
4545 DO
4550 T$ = INKEY$
4560 IF T$ = "6" THEN 4610
4570 IF T$ = "8" THEN 5200
4580 IF T$ = "4" THEN 5300
4585 IF T$ = "2" THEN 5400
4586 PSET (R, RR)
4587 CALL TN
4591 PSET (R, RR)
4592 CALL GR
4593 R = R + 2
4594 RR = RR - 1.5
4595 LOOP UNTIL RR <= 95
 IF RN3$ = "T" THEN H = H + 1: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
4597 PSET (R, RR)
4598 CALL RN
4600 CLS : GOTO 2610
4610
 IF RN3$ = "T" THEN H = H + 1: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN OT = OT: STK = 0: RN1$ = "F"
4615 DO
4620 IF B > 237 THEN B = B - 1 ELSE B = B + 1
4630 IF BB > 105 THEN BB = BB - 2 ELSE BB = BB + 2
4640 PSET (B, BB)
4650 CALL BLL
4652 PSET (B, BB)
4653 CALL GLL
4660 LOOP UNTIL B >= 237
4670 PRINT "OUT": OT = OT + 1: STK = 0
4680 CLS
4690 GOTO 2610
4700 IF BAT$ = "POPOUT" THEN 4710 ELSE 5500
4710 CLS
4720 CALL OUTFIELD
4730 PSET (50, 100)
4740 CALL RFIELD
4750 PSET (150, 120)
4760 CALL RFIELD
4770 PSET (220, 100)
4780 CALL RFIELD
4790 B = 159: BB = 180
4800 IF QQ$ = "L" THEN 4805
4801 IF QQ$ = "M" THEN 4900
4802 IF QQ$ = "R" THEN 4970
4805
4806 F = 50: FF = 100
4807 DO
4810 B = B - .8
4815 BB = BB - .8
4820 IF F > B THEN F = F - .7 ELSE F = F + .7
4825 IF FF > BB THEN FF = FF - .7 ELSE FF = FF + .7
4826 PSET (B, BB)
4828 CALL BLL
4830 PSET (B, BB)
4835 CALL GLL
4840 PSET (F, FF)
4845 CALL RFIELD
4850 PSET (F, FF)
4855 CALL GFIELD
4860 LOOP UNTIL F >= B
4870 GOTO 5100
4900 F = 150: FF = 120
4910 DO
4915 BB = BB - .8
4920 IF F > B THEN F = F - .7 ELSE F = F + .7
4925 IF FF > BB THEN FF = FF - .7 ELSE FF = FF + .7
4926 PSET (B, BB)
4928 CALL BLL
4930 PSET (B, BB)
4935 CALL GLL
4940 PSET (F, FF)
     CALL RFIELD
4950 PSET (F, FF)
4955 CALL GFIELD
4960 LOOP UNTIL FF >= BB
4965 GOTO 5100
4970 F = 220: FF = 100
5010 DO
5011 B = B + .8
5015 BB = BB - .8
5020 IF F > B THEN F = F - .7 ELSE F = F + .7
5025 IF FF > BB THEN FF = FF - .7 ELSE FF = FF + .7
5026 PSET (B, BB)
5028 CALL BLL
5030 PSET (B, BB)
5035 CALL GLL
5040 PSET (F, FF)
5045 CALL RFIELD
5050 PSET (F, FF)
5055 CALL GFIELD
5060 LOOP UNTIL FF >= BB
5065 GOTO 5100
5100 OT = OT + 1: STK = 0
5110 DO UNTIL PIG = 200
5120 PIG = PIG + 1
5130 LOOP
5140 CLS : GOTO 2610
5200 DO
5210 IF B > 185 THEN B = B - .5 ELSE B = B + .5
5230 IF BB > 40 THEN BB = BB - .5 ELSE BB = BB + .5
5240 PSET (B, BB)
5250 CALL BLL
5252 PSET (B, BB)
5253 CALL GLL
5260 LOOP UNTIL BB <= 40
5270
 IF RN3$ = "T" THEN H = H + 1: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN OT = OT + 1: STK = 0: RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
 
5280 CLS : GOTO 2610
5300
 IF RN3$ = "T" THEN H = H + 1: RN3$ = "F"
 IF RN2$ = "T" THEN OT = OT + 1: STK = 0: RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
 
5310 DO
5320 IF B > 74 THEN B = B - 1 ELSE B = B + 1
5330 IF BB > 94 THEN BB = BB - 2 ELSE BB = BB + 2
5340 PSET (B, BB)
5350 CALL BLL
5352 PSET (B, BB)
5353 CALL GLL
5360 LOOP UNTIL B <= 94
5370
5380 CLS : GOTO 2610
5400
 IF RN3$ = "T" THEN OT = OT + 1: STK = 0: RN3$ = "F"
 IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
 IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
 IF RN0$ = "T" THEN RN1$ = "T"
5410 DO
5420 IF B > 159 THEN B = B - 1 ELSE B = B + 1
5430 IF BB < 165 THEN BB = BB + 1 ELSE BB = BB - 1
5440 PSET (B, BB)
5450 CALL BLL
5452 PSET (B, BB)
5453 CALL GLL
5460 LOOP UNTIL BB >= 165
5480 CLS : GOTO 2610
5500 CLS
CALL OUTFIELD
PSET (70, 100)
CALL RFIELD
PSET (170, 90)
CALL RFIELD
PSET (280, 100)
CALL RFIELD
 BB = 180
 B = 159
 IF QQ$ = "L" THEN 5510
 IF QQ$ = "M" THEN 5520
 IF QQ$ = "R" THEN 5530
5510 DO
PSET (B, BB)
CALL BLL
PSET (B, BB)
CALL GLL
BB = BB - .5
B = B - .5
LOOP UNTIL BB <= 100
GOTO 5540
5520 DO
PSET (B, BB)
CALL BLL
PSET (B, BB)
CALL GLL
BB = BB - .5
LOOP UNTIL BB <= 100
GOTO 5540
5530 DO
PSET (B, BB)
CALL BLL
PSET (B, BB)
CALL GLL
BB = BB - .5
B = B + .5
LOOP UNTIL BB <= 100
5540 DO
GGGG = GGGG + 1
LOOP UNTIL GGGG = 100
GGGG = 0
IF RN3$ = "T" THEN H = H + 1: RN3$ = "F"
IF RN2$ = "T" THEN RN3$ = "T": RN2$ = "F"
IF RN1$ = "T" THEN RN2$ = "T": RN1$ = "F"
IF RN0$ = "T" THEN RN1$ = "T"
CLS : GOTO 2610
5600 RN1$ = "F"
RN2$ = "F"
RN3$ = "F"
IN = IN + 9
IF IN >= 9 THEN
CLS
PRINT "SCORE:  "; " HOME:"; H; "-"; " VISITORS:"; V
DO UNTIL INKEY$ <> "": LOOP
IF H <> V THEN END
END IF
CLS
GOTO 1
5700
 SCREEN 7
 COLOR 4, 2
 PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
 PRINT TAB(14); "ANDY'S BASEBALL"
 PRINT TAB(14); " G   A   M   E  !"
 PSET (100, 70)
 DRAW "D18"
 DRAW "R140"
 DRAW "U18"
 DRAW "L140"
 PSET (0, 0)
 DRAW "R319"
 DRAW "D190"
 DRAW "L319"
 DRAW "U190"
 PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : INPUT "DO YOU WANT TO WATCH THE INTRO"; D$
 IF D$ = "YES" THEN 5710
 IF D$ = "Y" THEN 5710 ELSE CLS : GOTO 1
5710
CLS
PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
SCREEN 10
COLOR 3, 5
PRINT TAB(17); "ANDY'S BASEBALL GAME!"
SCREEN 7
 COLOR 4, 2
 CLS
 PSET (160, 100)
 CALL DIAMOND
 PSET (159, 89)
 CALL BFIELD
 PSET (152, 96)
 CALL BLL
 PSET (149, 165)
 CALL T1
 PSET (94, 74)
 CALL BFIELD
 PSET (227, 80)
 CALL BFIELD
 PSET (185, 40)
 CALL BFIELD
 PSET (134, 40)
 CALL BFIELD
 B = 153
 BB = 87
 G = 153
 GG = 87
 DO UNTIL RATS = 1000
 RATS = RATS + 1
 LOOP
 RATS = 0
PSET (152, 96)
CALL GLL
 B = 152
 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, GG)
 CALL GLL
 B = B + .03
 BB = BB + .3
 GG = GG + .3
 LOOP UNTIL BB >= 170
 CALL SWING

 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL GLL
 BB = BB - .5
 LOOP UNTIL BB <= 10

 CLS
CALL BFIELD
PSET (100, 100)
CALL BFIELD
PSET (280, 100)
CALL BFIELD

 CALL OUTFIELD
 RANDOMIZE TIMER
 FT = INT(RND * 50000)
 IF FT < 40000 THEN FT = FT + 40000
 PRINT "A"; FT; "FT HOME RUN!"

 BAA = .08: BAAA = 40
 BB = 180
 B = 159
 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL GLL
 BB = BB - BAA
 LOOP UNTIL BB <= BAAA

 DO UNTIL RUFF = 1000
 RUFF = RUFF + 1
 LOOP

 CLS
 SCREEN 7
 COLOR 1, 0
 CALL INTRO2
 PSET (260, 90)
 CALL INTRO1
 PSET (100, 95)
 CALL INTRO3
 PRINT "I CAN'T SHAKE HIM!"
 DO UNTIL SEA = 7900
 SEA = SEA + 1
 LOOP
 SEA = 0
 LZ = 108
 ZO = 100
 PSET (260, 90)
 CALL INTRO8
 PSET (260, 80)
 CALL INTRO1
 PRINT "I CAN'T SHAKE HIM!"
 DO
 PSET (LZ, 95)
 CALL INTRO4
 PSET (LZ, 95)
 CALL INTRO5
 LZ = LZ + .5
 LOOP UNTIL LZ = 350
 LZ = 108
 G = G + 1
 PSET (260, 80)
 CALL INTRO8
 PSET (260, 100)
 CALL INTRO1
 COLOR 15, 0
 B = 0
 BB = 200
 DO
 PSET (B, BB)
 CALL BLL
 PSET (B, BB)
 CALL SLL
 B = B + .1
 BB = BB - .1
 LOOP UNTIL BB <= 100
DO
COLOR 4, 0
CIRCLE (104, 100), 1
CIRCLE (104, 100), 2
CIRCLE (104, 100), 3
CIRCLE (104, 100), 4
CIRCLE (104, 100), 5
CIRCLE (104, 100), 6
CIRCLE (104, 100), 7
CIRCLE (104, 100), 8
CIRCLE (104, 100), 9
DO UNTIL FIRE = 900
FIRE = FIRE + 1
LOOP
FIRE = 0
COLOR 0, 0
CIRCLE (104, 100), 1
CIRCLE (104, 100), 2
CIRCLE (104, 100), 3
CIRCLE (104, 100), 4
CIRCLE (104, 100), 5
CIRCLE (104, 100), 6
CIRCLE (104, 100), 7
CIRCLE (104, 100), 8
CIRCLE (104, 100), 9
DO UNTIL FIRE = 100
FIRE = FIRE + 1
LOOP
FIRE = 0
HOT = HOT + 1
LOOP UNTIL HOT = 20
PRINT "NICE HIT!"
CLS
GOTO 1

SUB B1
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "L4"
DRAW "D2"
DRAW "U10"
DRAW "D8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB B2
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "H8"
DRAW "F8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB B3
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "L7"
DRAW "R7"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB B4
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "G8"
DRAW "E8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E4"
END SUB

SUB B5
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "D10"
DRAW "U7"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"


END SUB

SUB B6
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "R13"
DRAW "L10"
DRAW "U2"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB BFIELD
DRAW "C3"
DRAW "R2"
DRAW "F2"
DRAW "L6"
DRAW "E1"
DRAW "R3"
DRAW "D2"
DRAW "C6"
DRAW "R2"
DRAW "D1"
DRAW "G1"
DRAW "L4"
DRAW "H1"
DRAW "U1"
DRAW "R6"
DRAW "D1"
DRAW "L5"
DRAW "D2"
'---------
DRAW "C1"
DRAW "D5"
DRAW "R4"
DRAW "U5"
DRAW "L3"
DRAW "D4"
DRAW "R1"
DRAW "U4"
DRAW "R1"
DRAW "D1"
DRAW "R5"
DRAW "U3"
DRAW "C6"
DRAW "R1"
DRAW "U1"
DRAW "R1"
DRAW "U2"
DRAW "L3"
DRAW "D2"
DRAW "R2"
DRAW "U1"
DRAW "L1"
DRAW "D3"
DRAW "C1"
DRAW "D2"
DRAW "L12"
DRAW "R7"
DRAW "D3"
DRAW "D2"
DRAW "C3"
DRAW "D4"
DRAW "R1"
DRAW "U4"
DRAW "L3"
DRAW "D4"
DRAW "L1"
DRAW "U4"

END SUB

SUB BLL
DRAW "C17"
DRAW "R2"
DRAW "L1"
DRAW "U1"
DRAW "D2"

END SUB

SUB DIAMOND
DRAW "C6"
DRAW "L6"
DRAW "G4"
DRAW "D4"
DRAW "F4"
DRAW "R12"
DRAW "E4"
DRAW "U4"
DRAW "H4"
DRAW "L5"
PSET (158, 106)
DRAW "R4"
PSET (147, 180)
DRAW "C6"
DRAW "R20"
DRAW "U10"
DRAW "R7"
DRAW "D10"
DRAW "L7"
DRAW "R7"
DRAW "U10"
DRAW "E64"
DRAW "H75"
DRAW "L5"
DRAW "G75"
DRAW "F64"
DRAW "D10"
DRAW "R7"
DRAW "U10"
DRAW "L7"
PSET (85, 105)
DRAW "L4"
DRAW "D2"
DRAW "R4"
DRAW "U1"
DRAW "L4"
PSET (240, 105)
DRAW "L4"
DRAW "D2"
DRAW "R4"
DRAW "U1"
DRAW "L4"
PSET (162, 30)
DRAW "L4"
DRAW "D2"
DRAW "R4"
DRAW "U1"
DRAW "L4"
PSET (162, 175)
DRAW "L4"
DRAW "D2"
DRAW "R4"
DRAW "U1"
DRAW "L4"
DRAW "E2"

END SUB

SUB GFIELD
DRAW "C2"
DRAW "R2"
DRAW "F2"
DRAW "L6"
DRAW "E1"
DRAW "R3"
DRAW "D2"
DRAW "C2"
DRAW "R2"
DRAW "D1"
DRAW "G1"
DRAW "L4"
DRAW "H1"
DRAW "U1"
DRAW "R6"
DRAW "D1"
DRAW "L5"
DRAW "D2"
'---------
DRAW "C2"
DRAW "D5"
DRAW "R4"
DRAW "U5"
DRAW "L3"
DRAW "D4"
DRAW "R1"
DRAW "U4"
DRAW "R1"
DRAW "D1"
DRAW "R5"
DRAW "U3"
DRAW "C2"
DRAW "R1"
DRAW "U1"
DRAW "R1"
DRAW "U2"
DRAW "L3"
DRAW "D2"
DRAW "R2"
DRAW "U1"
DRAW "L1"
DRAW "D3"
DRAW "C2"
DRAW "D2"
DRAW "L12"
DRAW "R7"
DRAW "D3"
DRAW "D2"
DRAW "C2"
DRAW "D4"
DRAW "R1"
DRAW "U4"
DRAW "L3"
DRAW "D4"
DRAW "L1"
DRAW "U4"

END SUB

SUB GLL
DRAW "C2"
DRAW "R2"
DRAW "L1"
DRAW "U1"
DRAW "D2"

END SUB

SUB GR
DRAW "C2"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C2"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C2"
DRAW "L1"
DRAW "D2"
DRAW "L4"
DRAW "R11"
DRAW "L11"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C2"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"
END SUB

SUB INTRO1
DRAW "C7"
DRAW "R9"
DRAW "L9"
DRAW "F2"
DRAW "R5"
DRAW "F1"
DRAW "G1"
DRAW "L5"
DRAW "G2"
DRAW "R9"
DRAW "L9"
DRAW "U5"
DRAW "C4"
DRAW "L1"
DRAW "R2"
DRAW "C7"
DRAW "D3"
DRAW "L1"
DRAW "C4"
DRAW "L1"
DRAW "R2"
DRAW "C7"
DRAW "U1"
DRAW "R7"


END SUB

SUB INTRO2
COLOR 14
FOR I = 1 TO 200
B% = INT(RND * 320)
A% = INT(RND * 320)
PSET (B%, A%)
NEXT I
END SUB

SUB INTRO3
DRAW "C1"
DRAW "R7"
DRAW "F5"
DRAW "G5"
DRAW "L7"
DRAW "H5"
DRAW "E5"
DRAW "F1"
DRAW "C8"
DRAW "F3"
DRAW "E3"
DRAW "G4"
DRAW "R8"
DRAW "L7"
DRAW "G4"
DRAW "E4"
DRAW "L8"
DRAW "R8"
DRAW "F4"
END SUB

SUB INTRO4
DRAW "C2"
DRAW "R10"
DRAW "D1"
DRAW "L10"
END SUB

SUB INTRO5
DRAW "C0"
DRAW "R10"
DRAW "D1"
DRAW "L10"
END SUB

SUB INTRO8
DRAW "C0"
DRAW "R9"
DRAW "L9"
DRAW "F2"
DRAW "R5"
DRAW "F1"
DRAW "G1"
DRAW "L5"
DRAW "G2"
DRAW "R9"
DRAW "L9"
DRAW "U5"
DRAW "C0"
DRAW "L1"
DRAW "R2"
DRAW "C0"
DRAW "D3"
DRAW "L1"
DRAW "C0"
DRAW "L1"
DRAW "R2"
DRAW "C0"
DRAW "U1"
DRAW "R7"

END SUB

SUB OUTFIELD
PSET (310, 100)
DRAW "C6"
DRAW "D70"
DRAW "G40"
DRAW "E40"
DRAW "U70"
DRAW "H40"
DRAW "L220"
DRAW "G40"
DRAW "D70"
DRAW "F30"
END SUB

SUB R1
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "D2"
DRAW "U10"
DRAW "D8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB R2
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "H8"
DRAW "F8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB R3
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "L7"
DRAW "R7"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB R4
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "G8"
DRAW "E8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"


END SUB

SUB R5
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "D10"
DRAW "U7"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB R6
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "R13"
DRAW "L10"
DRAW "U2"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"


END SUB

SUB RFIELD
DRAW "C4"
DRAW "R2"
DRAW "F2"
DRAW "L6"
DRAW "E1"
DRAW "R3"
DRAW "D2"
DRAW "C6"
DRAW "R2"
DRAW "D1"
DRAW "G1"
DRAW "L4"
DRAW "H1"
DRAW "U1"
DRAW "R6"
DRAW "D1"
DRAW "L5"
DRAW "D2"
'---------
DRAW "C7"
DRAW "D5"
DRAW "R4"
DRAW "U5"
DRAW "L3"
DRAW "D4"
DRAW "R1"
DRAW "U4"
DRAW "R1"
DRAW "D1"
DRAW "R5"
DRAW "U3"
DRAW "C6"
DRAW "R1"
DRAW "U1"
DRAW "R1"
DRAW "U2"
DRAW "L3"
DRAW "D2"
DRAW "R2"
DRAW "U1"
DRAW "L1"
DRAW "D3"
DRAW "C7"
DRAW "D2"
DRAW "L12"
DRAW "R7"
DRAW "D3"
DRAW "D2"
DRAW "C4"
DRAW "D4"
DRAW "R1"
DRAW "U4"
DRAW "L3"
DRAW "D4"
DRAW "L1"
DRAW "U4"


END SUB

SUB RN
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "L4"
DRAW "R11"
DRAW "L11"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"
END SUB

SUB SETP
 PSET (160, 100)
 CALL DIAMOND
 PSET (159, 89)
 CALL BFIELD
 PSET (152, 96)
 CALL BLL
 PSET (149, 165)
 CALL T1
 PSET (94, 74)
 CALL BFIELD
 PSET (227, 80)
 CALL BFIELD
 PSET (185, 40)
 CALL BFIELD
 PSET (134, 40)
 CALL BFIELD

END SUB

SUB SETPB
 PSET (160, 100)
 CALL DIAMOND
 PSET (159, 89)
 CALL RFIELD
 PSET (152, 96)
 CALL BLL
 PSET (149, 165)
 CALL T1
 PSET (94, 74)
 CALL RFIELD
 PSET (227, 80)
 CALL RFIELD
 PSET (185, 40)
 CALL RFIELD
 PSET (134, 40)
 CALL RFIELD
END SUB

SUB SLL
DRAW "C0"
DRAW "R2"
DRAW "L1"
DRAW "U1"
DRAW "D2"

END SUB

SUB SWING
PSET (149, 165)
CALL T1
DO UNTIL H = 100
H = H + 1
LOOP
PSET (149, 165)
CALL B1
PSET (149, 165)
CALL T2
DO UNTIL B = 100
B = B + 1
LOOP
PSET (149, 165)
CALL B2
PSET (149, 165)
CALL T3
DO UNTIL C = 100
C = C + 1
LOOP
PSET (149, 165)
CALL B3
PSET (149, 165)
CALL T4
DO UNTIL D = 100
D = D + 1
LOOP
PSET (149, 165)
CALL B4
PSET (149, 165)
CALL T5
DO UNTIL E = 100
E = E + 1
LOOP
PSET (149, 165)
CALL B5
PSET (149, 165)
CALL T6
DO UNTIL F = 100
F = F + 1
LOOP
PSET (149, 165)
CALL B6
PSET (149, 165)
CALL T2
END SUB

SUB SWING2
PSET (149, 165)
CALL R1
DO UNTIL H = 100
H = H + 1
LOOP
PSET (149, 165)
CALL B1
PSET (149, 165)
CALL R2
DO UNTIL B = 100
B = B + 1
LOOP
PSET (149, 165)
CALL B2
PSET (149, 165)
CALL R3
DO UNTIL C = 100
C = C + 1
LOOP
PSET (149, 165)
CALL B3
PSET (149, 165)
CALL R4
DO UNTIL D = 100
D = D + 1
LOOP
PSET (149, 165)
CALL B4
PSET (149, 165)
CALL R5
DO UNTIL E = 100
E = E + 1
LOOP
PSET (149, 165)
CALL B5
PSET (149, 165)
CALL R6
DO UNTIL F = 100
F = F + 1
LOOP
PSET (149, 165)
CALL B6
PSET (149, 165)
CALL R2

END SUB

SUB T1
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "D2"
DRAW "U10"
DRAW "D8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB T2
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "H8"
DRAW "F8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB T3
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "L7"
DRAW "R7"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB T4
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "L4"

DRAW "G8"
DRAW "E8"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB T5
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "D10"
DRAW "U7"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB T6
DRAW "C4"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C7"
DRAW "L1"
DRAW "D2"
DRAW "R13"
DRAW "L10"
DRAW "U2"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C4"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

SUB TN
DRAW "C3"
DRAW "R5"
DRAW "L6"
DRAW "U1"
DRAW "E1"
DRAW "R2"
DRAW "F1"
DRAW "L3"
DRAW "D2"
DRAW "C6"
DRAW "D1"
DRAW "F1"
DRAW "R1"
DRAW "E1"
DRAW "U1"
DRAW "L2"
DRAW "D1"
DRAW "R2"
DRAW "L2"
DRAW "D2"

'-----------

DRAW "C1"
DRAW "L1"
DRAW "D2"
DRAW "L4"
DRAW "R11"
DRAW "L11"
DRAW "R4"
DRAW "D3"
DRAW "R3"
DRAW "U5"
DRAW "L2"
DRAW "D5"
DRAW "R1"
DRAW "U5"
DRAW "D6"
DRAW "C3"
DRAW "R1"
DRAW "F3"
DRAW "L1"
DRAW "H3"
DRAW "L1"
DRAW "G3"
DRAW "L1"
DRAW "E3"

END SUB

