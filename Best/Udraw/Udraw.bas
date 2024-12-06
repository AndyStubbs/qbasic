'$DYNAMIC
'$STATIC
DECLARE SUB help ()
DECLARE SUB readpal ()
DECLARE SUB loadp ()
DECLARE SUB savep ()
DECLARE SUB about ()
DECLARE SUB getspics2 ()
DECLARE SUB putspics2 ()
DECLARE SUB menus2 ()
DECLARE SUB mtools ()
DECLARE SUB tools ()
DECLARE FUNCTION inputf$ ()
DECLARE SUB bNEW ()
DECLARE SUB cNEW ()
DECLARE SUB load2 ()
DECLARE SUB load ()
DECLARE SUB save4 ()
DECLARE SUB save3 ()
DECLARE SUB save2 ()
DECLARE SUB over ()
DECLARE SUB default ()
DECLARE SUB save ()
DECLARE SUB CHSIZE ()
DECLARE SUB quit ()
DECLARE SUB new ()
DECLARE SUB initfont ()
DECLARE SUB locatef (printX%, printY%)
DECLARE SUB printf (words$)
DECLARE SUB menus ()
DECLARE SUB putspics ()
DECLARE SUB getspics ()
DECLARE SUB init ()
DECLARE SUB mypal ()
DECLARE SUB drawboxes ()
DECLARE SUB viewscrn ()
CLS
SCREEN 13

'declare registers for call interupt
TYPE RegType
    ax AS INTEGER
    bx AS INTEGER
    cx AS INTEGER
    dx AS INTEGER
    bp AS INTEGER
    si AS INTEGER
    di AS INTEGER
    flags AS INTEGER
END TYPE

TYPE PalType
    red AS INTEGER
    green AS INTEGER
    blue AS INTEGER
END TYPE

TYPE MyType
    changeX AS INTEGER
    changeY AS INTEGER
    startX2 AS INTEGER
    startY2 AS INTEGER
    startX AS INTEGER
    startY AS INTEGER
    amountX AS INTEGER
    amountY AS INTEGER
END TYPE


'dimmed variables
DIM SHARED pal(256) AS PalType
DIM SHARED font(100, 40) AS INTEGER
DIM SHARED mfont(100, 40) AS INTEGER
DIM SHARED LocPrintX%, LocPrintY%
DIM SHARED inregs AS RegType, outregs AS RegType
DIM SHARED setdown AS MyType
DIM SHARED chcC(100) AS INTEGER
DIM SHARED cman(256) AS INTEGER
DIM SHARED mx, my, mb, myc, size, cnt, chc, check, c, c2, tool, sel, slc AS INTEGER
DIM SHARED big(16400) AS INTEGER
DIM SHARED pics8(34, 100) AS INTEGER
DIM SHARED pics16(130, 43) AS INTEGER
DIM SHARED pics32(514, 13) AS INTEGER
DIM SHARED undo(514, 1) AS INTEGER
DIM SHARED back(800) AS INTEGER
DIM SHARED copie(514) AS INTEGER
DIM SHARED mcopie(514) AS INTEGER
DIM SHARED mtool AS INTEGER
DIM SHARED menu$, menu2$, item$, item2$, filename$, filename2$, fst$(500)
DIM SHARED i, i3, i4, i2 AS INTEGER
GET (0, 0)-(31, 31), undo(0, 0)
GET (0, 0)-(31, 31), undo(0, 1)

'init mouse
inregs.ax = 0
CALL interrupt(&H33, inregs, outregs)
IF outregs.ax = 0 THEN CALL over

'set mouse x
'inregs.ax = 7
'inregs.cx = 630
'CALL interrupt(&H33, inregs, outregs)

'set mouse y
'inregs.ax = 8
'inregs.cx = 198
'CALL interrupt(&H33, inregs, outregs)

'starting variables
c = 1 'forecolor
chc = 1  'active window
tool = 1   'active tool
menu2$ = "DUMB"

FOR i = 15 TO 255
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
    red = 0: blue = 0: green = 0
NEXT

CALL initfont
CLS
size = 8
CALL init
CALL getspics
size = 16
CALL init
CALL getspics
size = 48
CALL init
CALL getspics

size = 32
OUT &H3C8, 15
OUT &H3C9, INT(63)
OUT &H3C9, INT(63)
OUT &H3C9, INT(63)
GET (1, 1)-(2, 2), copie(0)
GET (1, 1)-(2, 2), mcopie(0)

CALL init
CALL getspics
CALL mypal
med$ = "0"
'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse

'main program loop
DO UNTIL ky$ = CHR$(27)
    IF c <> lastc THEN
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs)
       
        i = 0: i2 = 15
        FOR mc = 0 TO 255
            IF mc = lastc THEN LINE (i, i2)-(i + 3, i2 + 5), mc, BF
            i2 = i2 + 5
            IF i2 > 197 THEN i = i + 4: i2 = 15
        NEXT
       
        i = 0: i2 = 15
        FOR mc = 0 TO 255
            IF mc = c THEN LINE (i, i2)-(i + 3, i2 + 5), 15, B
            i2 = i2 + 5
            IF i2 > 197 THEN i = i + 4: i2 = 15
        NEXT
        
        lastc = c
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
    END IF
    ky$ = UCASE$(INKEY$)
    IF ky$ = CHR$(0) + "H" THEN
       'up
       cnt = 0
       inregs.ax = 2
       CALL interrupt(&H33, inregs, outregs) 'to hide mouse
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + 2 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 2
                            thisC = POINT(i3, i4)
                            
                            PSET (i3, i4 - 1), thisC
                            IF i4 - 1 < mmy + 2 THEN PSET (i3, mmy + size + 2), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    mx = 0
                    CALL drawboxes
                END IF
                
            NEXT
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
    END IF
    IF ky$ = CHR$(0) + "K" THEN
       'left
       cnt = 0
       inregs.ax = 2
       CALL interrupt(&H33, inregs, outregs) 'to hide mouse
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + 2 TO mmx + size + 2
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 1
                            thisC = POINT(i3, i4)
                            PSET (i3 - 1, i4), thisC
                            IF i3 - 1 < mmx + 2 THEN PSET (mmx + size + 2, i4), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    mx = 0
                    CALL drawboxes
              
                END IF
               
            NEXT
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
       
    END IF
    IF ky$ = CHR$(0) + "P" THEN
       'down
       cnt = 0
       inregs.ax = 2
       CALL interrupt(&H33, inregs, outregs) 'to hide mouse
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + 2 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + size + 1 TO mmy + 1 STEP -1
                            thisC = POINT(i3, i4)
                            PSET (i3, i4 + 1), thisC
                            IF i4 + 1 > mmy + size + 1 THEN PSET (i3, mmy + 1), thisC
                       
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    mx = 0
                    CALL drawboxes
               
                END IF
               
            NEXT
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
      
    END IF
    IF ky$ = CHR$(0) + "M" THEN
       'right
       cnt = 0
       inregs.ax = 2
       CALL interrupt(&H33, inregs, outregs) 'to hide mouse
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + size + 1 TO mmx + 1 STEP -1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + size + 1 TO mmy + 2 STEP -1
                            thisC = POINT(i3, i4)
                            PSET (i3 + 1, i4), thisC
                            IF i3 + 1 > mmx + size + 1 THEN PSET (mmx + 1, i4), thisC
                       
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    mx = 0
                    CALL drawboxes
                 
                END IF
              
            NEXT
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
      
    END IF
    IF ky$ = "G" THEN
        LINE (190, 5)-(250, 15), 0, BF
        CALL locatef(190, 5)
        printf ("MODE ")
        printf (med$)
    END IF
    IF ky$ = "G" AND med$ = "0" THEN med$ = "1": GOTO 12
    IF ky$ = "G" AND med$ = "1" THEN med$ = "0"
12
    IF ky$ = "C" THEN tool = 7
    IF ky$ = "P" THEN tool = 8
    IF ky$ = "O" THEN
        CALL cNEW
        CALL loadp
        CALL bNEW
    END IF
   
    IF ky$ = "V" THEN
        CALL cNEW
        CALL savep
        CALL bNEW
    END IF
    IF ky$ = "N" THEN
        CALL cNEW
        CALL new
        CALL bNEW
    END IF
    IF ky$ = "S" THEN
        CALL cNEW
        CALL save
        CALL bNEW
    END IF
    IF ky$ = "L" THEN
        CALL cNEW
        CALL load
        CALL bNEW
    END IF
    IF ky$ = "Q" THEN
        CALL cNEW
        CALL quit
        CALL bNEW
    END IF
    IF ky$ = "D" THEN
        CALL cNEW
        CALL default
        CALL bNEW
    END IF
    IF ky$ = "M" THEN
        CALL cNEW
        CALL mypal
        CALL bNEW
    END IF
    IF ky$ = "T" THEN
        CALL cNEW
        CALL tools
        CALL bNEW
    END IF
    IF ky$ = "Z" THEN
        CALL cNEW
        CALL CHSIZE
        CALL bNEW
    END IF
    IF ky$ = "A" THEN
        CALL cNEW
        CALL about
        CALL bNEW
    END IF
    IF ky$ = "U" THEN
        mx = -1
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        CALL putspics2
        CALL getspics
        check = chc
        CALL drawboxes
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
        FOR i = 0 TO 514
            undo(i, 0) = undo(i, 1)
        NEXT
        CALL getspics2
    END IF
   
    IF ky$ = "+" AND size < 32 THEN
        chc = 1
        'hide mouse
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
        CALL getspics
        size = size * 2
        CLS
        CALL init
        CALL putspics
        mx = -1
        check = 1
        CALL drawboxes
       
        'show mouse
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
    END IF
    IF ky$ = "-" AND size > 8 THEN
        chc = 1
        'hide mouse
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        CALL getspics
        size = size \ 2
        CLS
        CALL init
        CALL putspics
        mx = -1
        check = 1
        CALL drawboxes
        'show mouse
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
   
    END IF
    CALL viewscrn  'lines for program.
   
    'draws toolbar with selected color
    FOR i = 0 TO 5
        IF tool = i + 1 AND i < 7 THEN oc = 7 ELSE oc = 15
        IF tool = i + 7 THEN oc2 = 7 ELSE oc2 = 15
        LINE (33 + (i * 48), 146)-(76 + (i * 48), 170), oc, B
        LINE (33 + (i * 48), 172)-(76 + (i * 48), 197), oc2, B
    NEXT
   
    ' test print
   
    'LOCATE 4, 2
    'PRINT menu$, menu2$
    'if clicked on menus
   
    IF mnb = 1 AND mb = 0 THEN
        FOR i = 0 TO 514
            undo(i, 0) = undo(i, 1)
        NEXT
       
        CALL getspics2
       
        mnb = 0
    END IF
    
    IF mb = 1 THEN
        IF my > 2 AND my < 13 THEN
            IF mx > 4 AND mx < 155 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs) 'to hide mouse
               
                menu2$ = "DUMB"
                item2$ = "DUMB"
                GET (32, 16)-(159, 143), big(0)
                CALL getspics
                CALL menus
           
            END IF
        END IF
    END IF
   
   
    ' gets status for mouse.
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx
    cnt2 = cnt2 + 1
   
    IF PixS = 4 AND mb = 0 THEN
        cnt2 = cnt2 + 1
        IF cnt2 > 100 THEN PixS = 0: cnt2 = 0
        px = 0
        py = 0
        check = chc
        CALL drawboxes
    END IF
  
    IF mb = 0 AND PixS = 1 THEN PixS = 2
   
    'puts drawing tool on drawing screen
    IF mb = 0 AND mx > 31 AND mx < 160 AND my > 15 AND my < 144 AND PixS > 1 THEN
        IF mx <> pmx OR my <> pmy THEN
            VIEW (31, 16)-(160, 144)
            'hide mouse
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
            PUT (1, 0), big(0), PSET
            IF tool <> 7 THEN LINE (px * (128 \ size) - 31, py * (128 \ size) - 16)-(mx - 31, my - 16), c
            IF tool = 7 AND pixS2 <> 1 THEN
                LINE (px * (128 \ size) - 31, py * (128 \ size) - 16)-(mx - 31, my - 16), 15, B
            END IF
            IF tool = 7 AND pixS2 = 1 THEN
                LINE (selx1 * (128 \ size) - 31, Sely1 * (128 \ size) - 16)-(selx2 - 31, Sely2 - 16), 15, B
            END IF
           
            IF tool = 4 THEN
                LINE (px * (128 \ size) - 31, py * (128 \ size) - 16)-(mx - 31, my - 16), c, B
            END IF
            IF tool = 3 THEN
                x1 = px * (128 \ size) - 31
                y1 = py * (128 \ size) - 16
                x2 = mx - 31
                y2 = my - 16
                dman = INT(SQR(ABS(y1 - y2) ^ 2 + ABS(x1 - x2) ^ 2))
                CIRCLE (x1, y1), dman, c
            END IF
            'show mouse
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
            VIEW (0, 0)-(319, 199)
        END IF
        ppx = px: ppy = py: pmx = mx: pmy = my
    END IF
   
    IF tool = 12 THEN
        cnt = 0
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + 2 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 2
                            thisC = POINT(i3, i4)
                            mi4 = (i4 * -1) + (mmy + (size / 2) + 2) * 2
                            PSET (i3 - (mmx - 100), mi4), thisC
                            IF i4 = mmy + 2 THEN si4 = mi4
                            IF i4 = mmy + size + 2 THEN ei4 = mi4
                            IF i3 = mmx + 2 THEN si3 = i3 - (mmx - 100)
                            IF i3 = mmx + size + 1 THEN ei3 = i3 - (mmx - 100)
                        NEXT
                    NEXT
                    GET (si3, si4)-(ei3, ei4), back(0)
                    PUT (mmx + 2, mmy + 2), back(0), PSET
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    mx = 0: mb = 0
                    CALL drawboxes
                    tool = ptool
                END IF
            NEXT
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
    END IF

    IF tool = 11 THEN
        cnt = 0
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + 2 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 2
                            thisC = POINT(i3, i4)
                            mi3 = (i3 * -1) + 100 + (mmx + (size / 2) + 2)
                            PSET (mi3, i4), thisC
                            IF i3 = mmx + 2 THEN si3 = mi3
                            IF i3 = mmx + size + 1 THEN ei3 = mi3
                        NEXT
                    NEXT
                    GET (si3, mmy + 2)-(ei3, mmy + size + 2), back(0)
                    PUT (mmx + 2, mmy + 2), back(0), PSET
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    mx = 0: mb = 0
                    CALL drawboxes
                    tool = ptool
                END IF
            NEXT
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
    END IF

    ' if clicked on draw screen
    IF (mb = 1 OR mb = 2) AND mx > 31 AND mx < 160 AND my > 16 AND my < 144 THEN
        mnb = 1
        ' hide mouse
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        'scales big to small
        mx2 = mx \ (128 \ size)
        my2 = my \ (128 \ size)
        IF tool = 10 AND mb = 1 THEN
            'draws on selected side box
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    IF chc = cnt AND mb = 1 THEN
                        OUT &H3C7, c
                        red = INP(&H3C9)
                        green = INP(&H3C9)
                        blue = INP(&H3C9)
                        red = red / 2
                        green = green / 2
                        blue = blue / 2
                        FOR i3 = 0 TO 254
                            OUT &H3C7, i3
                            red2 = INP(&H3C9)
                            green2 = INP(&H3C9)
                            blue2 = INP(&H3C9)
                            test = (red - red2) ^ 2 + (green - green2) ^ 2 + (blue - blue2) ^ 2
                            IF test < 38 THEN cman(countr) = i3: countr = countr + 1
                        NEXT
                        FOR ghost = 0 TO 100
                            hat = INT(RND * 6) - 3
                            hat2 = INT(RND * 6) - 3
                            IF hat > 1 AND hat2 > 1 THEN hat2 = 0
                            IF hat < -1 AND hat2 < -1 THEN hat2 = 0
                            setX = mx2 + hat
                            setY = my2 + hat2
                            mx3 = setX * (128 \ size): my3 = setY * (128 \ size)
                            IF cman(gbot) = 0 THEN cman(gbot) = c
                            IF mx3 > 31 AND mx3 < 159 AND my3 > 15 AND my3 < 144 THEN
                            PSET (setX + setdown.startX + (i * setdown.changeX), setY + setdown.startY + (i2 * setdown.changeY)), cman(gbot)
                            LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), cman(gbot), BF
                            gbot = gbot + 1
                            IF gbot > countr THEN gbot = 0
                            END IF
                        NEXT
                        FOR i7 = 0 TO 255
                            cman(i7) = 0
                        NEXT
                        gbot = 0
                        countr = 0
                    END IF
                NEXT
            NEXT
        END IF
       
        IF tool = 9 AND mb = 1 THEN
            'draws on selected side box
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    IF chc = cnt AND mb = 1 THEN
                        OUT &H3C7, c
                        red = INP(&H3C9)
                        green = INP(&H3C9)
                        blue = INP(&H3C9)
                   
                        IF red >= green AND red >= blue THEN blue = red: green = red
                        IF green >= red AND green >= blue THEN blue = green: red = green
                        IF blue >= green AND blue >= red THEN red = blue: green = blue
                        many = 999
                        FOR i3 = 0 TO 254
                            OUT &H3C7, i3
                            red2 = INP(&H3C9)
                            green2 = INP(&H3C9)
                            blue2 = INP(&H3C9)
                            test = (red - red2) ^ 2 + (green - green2) ^ 2 + (blue - blue2) ^ 2
                            IF test < 90 THEN cman(countr) = i3: countr = countr + 1:
                        NEXT
                        FOR ghost = 0 TO 100
                            hat = INT(RND * 6) - 3
                            hat2 = INT(RND * 6) - 3
                            IF hat > 1 AND hat2 > 1 THEN hat2 = 0
                            IF hat < -1 AND hat2 < -1 THEN hat2 = 0
                            setX = mx2 + hat
                            setY = my2 + hat2
                            mx3 = setX * (128 \ size): my3 = setY * (128 \ size)
                            IF cman(gbot) = 0 THEN cman(gbot) = cman(1)
                            IF mx3 > 31 AND mx3 < 159 AND my3 > 15 AND my3 < 144 THEN
                            PSET (setX + setdown.startX + (i * setdown.changeX), setY + setdown.startY + (i2 * setdown.changeY)), cman(gbot)
                            LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), cman(gbot), BF
                            gbot = gbot + 1
                            IF gbot > countr THEN gbot = 0
                            END IF
                        NEXT
                        FOR i7 = 0 TO 255
                            cman(i7) = 0
                        NEXT
                        gbot = 0
                        countr = 0
                    END IF
                NEXT
            NEXT
        END IF
       
       
        IF tool = 8 AND mb <> 2 THEN
            cnt = 0
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    mmx = setdown.startX + (i * setdown.changeX)
                    mmy = setdown.startY + (i2 * setdown.changeY)
                    cnt = cnt + 1
                    IF cnt = chc THEN
                        lx1 = mx / (128 / size) + mmx
                        ly1 = my / (128 / size) + mmy
                        LOCATE 1, 1
                        IF mx - (copie(0) / 16) * (128 / size) < 32 THEN
                            lx1 = 32 / (128 / size) + mmx + (copie(0) / 16)
                        END IF
                        IF mx + (copie(0) / 16) * (128 / size) > 160 THEN
                            lx1 = 160 / (128 / size) + mmx - (copie(0) / 16)
                        END IF
                        IF my - (copie(1) / 2) * (128 / size) < 17 THEN
                            ly1 = 17 / (128 / size) + mmy + (copie(1) / 2)
                        END IF
                        IF my + (copie(1) / 2) * (128 / size) > 145 THEN
                            ly1 = 145 / (128 / size) + mmy - (copie(1) / 2)
                        END IF
                       
                        IF copie(0) / 8 <= size AND copie(1) <= size THEN
                            PUT (lx1 - (copie(0) / 16), ly1 - (copie(1) / 2)), mcopie(0), AND
                            PUT (lx1 - (copie(0) / 16), ly1 - (copie(1) / 2)), copie(0), OR
                        END IF
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                    END IF
                NEXT
            NEXT
            check = chc
            CALL drawboxes
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs)
        END IF
        IF tool = 6 THEN
            c3 = c2
            IF c2 = 0 THEN c3 = c
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    IF chc = cnt AND mb = 1 THEN
                        vx1 = setdown.startX2 + (i * setdown.changeX)
                        vx2 = setdown.startX2 + (i * setdown.changeX) + size
                        vy1 = setdown.startY2 + (i2 * setdown.changeY)
                        vy2 = setdown.startY2 + (i2 * setdown.changeY) + size
                        VIEW (vx1, vy1)-(vx2 + 1, vy2 + 1)
                        LINE (0, 0)-(size + 3, size + 3), c, B
                        cnt = cnt + 1
                        pntx = mx2 - 25 \ (128 \ size)
                        pnty = my2 - 10 \ (128 \ size)
                        PAINT (pntx, pnty), c3, c
                        
                        VIEW (0, 0)-(319, 199)
                        check = chc
                        CALL drawboxes
                    END IF
                NEXT
            NEXT
        END IF
        IF tool <> 1 AND mb = 2 THEN
            'draws on selected side box
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    c = POINT(mx, my)
                NEXT
            NEXT
        END IF
       
        IF tool = 1 THEN
            'draws on selected side box
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    IF chc = cnt AND mb = 1 THEN PSET (mx2 + setdown.startX + (i * setdown.changeX), my2 + setdown.startY + (i2 * setdown.changeY)), c
                    IF chc = cnt AND mb = 2 AND med$ <> "1" THEN
                        c = POINT(mx2 + setdown.startX + (i * setdown.changeX), my2 + setdown.startY + (i2 * setdown.changeY))
                    END IF
                    IF chc = cnt AND mb = 2 AND med$ = "1" THEN PSET (mx2 + setdown.startX + (i * setdown.changeX), my2 + setdown.startY + (i2 * setdown.changeY)), 0
                NEXT
            NEXT
            'draws big on main drawing.
            mx3 = mx2 * (128 \ size): my3 = my2 * (128 \ size)
            IF mb = 1 THEN LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), c, BF
            IF mb = 2 AND med$ = "1" THEN LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), 0, BF
        END IF
       
        IF tool = 5 AND mb = 1 THEN
            'draws on selected side box
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    IF chc = cnt AND mb = 1 THEN
                        OUT &H3C7, c
                        red = INP(&H3C9)
                        green = INP(&H3C9)
                        blue = INP(&H3C9)
                        FOR i3 = 0 TO 254
                            OUT &H3C7, i3
                            red2 = INP(&H3C9)
                            green2 = INP(&H3C9)
                            blue2 = INP(&H3C9)
                            test = (red - red2) ^ 2 + (green - green2) ^ 2 + (blue - blue2) ^ 2
                            IF test < 38 THEN cman(countr) = i3: countr = countr + 1
                        NEXT
                        FOR ghost = 0 TO 100
                            hat = INT(RND * 6) - 3
                            hat2 = INT(RND * 6) - 3
                            IF hat > 1 AND hat2 > 1 THEN hat2 = 0
                            IF hat < -1 AND hat2 < -1 THEN hat2 = 0
                            setX = mx2 + hat
                            setY = my2 + hat2
                            mx3 = setX * (128 \ size): my3 = setY * (128 \ size)
                            IF cman(gbot) = 0 THEN cman(gbot) = c
                            IF mx3 > 31 AND mx3 < 159 AND my3 > 15 AND my3 < 144 THEN
                            PSET (setX + setdown.startX + (i * setdown.changeX), setY + setdown.startY + (i2 * setdown.changeY)), cman(gbot)
                            LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), cman(gbot), BF
                            gbot = gbot + 1
                            IF gbot > countr THEN gbot = 0
                            END IF
                        NEXT
                        FOR i7 = 0 TO 255
                            cman(i7) = 0
                        NEXT
                        gbot = 0
                        countr = 0
                    END IF
                NEXT
            NEXT
        END IF
       
       
        IF (tool = 2 OR tool = 3 OR tool = 4 OR tool = 7) AND mb <> 2 THEN
            'draws a line on selected side box
            cnt = 0
            FOR i = 0 TO setdown.amountX
                FOR i2 = 0 TO setdown.amountY
                    cnt = cnt + 1
                    IF chc = cnt THEN
                        IF PixS = 0 THEN
                            GET (32, 16)-(159, 143), big(0)
                            px = mx2: ppx = 0: ppy = 0
                            py = my2: pmx = 0: pmy = 0
                            PixS = 1
                        END IF
                        IF PixS = 3 THEN
                            'CALL getspics
                            IF tool = 7 THEN
                                selx1 = px: Sely1 = py
                                selx2 = mx: Sely2 = my
                                lx1 = px + setdown.startX + (i * setdown.changeX)
                                lx2 = mx2 + setdown.startX + (i * setdown.changeX)
                                ly1 = py + setdown.startY + (i2 * setdown.changeY)
                                ly2 = my2 + setdown.startY + (i2 * setdown.changeY)
                                IF lx1 < lx2 THEN SWAP lx1, lx2
                                IF ly1 < ly2 THEN SWAP ly1, ly2
                                GET (lx1, ly1)-(lx2, ly2), copie(0)
                                FOR biI = lx2 TO lx1
                                    FOR biI2 = ly2 TO ly1
                                        gc = POINT(biI, biI2)
                                        IF gc = 0 THEN PSET (biI, biI2), 255 ELSE PSET (biI, biI2), 0
                                    NEXT
                                NEXT
                                GET (lx1, ly1)-(lx2, ly2), mcopie(0)
                                PUT (lx2, ly2), copie(0), PSET
                                PixS = 4: px = 0: py = 0
                           
                            END IF
                            IF tool = 4 THEN
                                lx1 = px + setdown.startX + (i * setdown.changeX)
                                lx2 = mx2 + setdown.startX + (i * setdown.changeX)
                                ly1 = py + setdown.startY + (i2 * setdown.changeY)
                                ly2 = my2 + setdown.startY + (i2 * setdown.changeY)
                                LINE (lx1, ly1)-(lx2, ly2), c, B
                                PixS = 4: px = 0: py = 0
                            END IF
                           
                            IF tool = 2 THEN
                                LINE (px + setdown.startX + (i * setdown.changeX), py + setdown.startY + (i2 * setdown.changeY))-(mx2 + setdown.startX + (i * setdown.changeX), my2 + setdown.startY + (i2 * setdown.changeY)), c
                                PixS = 4: px = 0: py = 0
                            END IF
                            IF tool = 3 THEN
                                vx1 = setdown.startX2 + (i * setdown.changeX)
                                vx2 = setdown.startX2 + (i * setdown.changeX) + size
                                vy1 = setdown.startY2 + (i2 * setdown.changeY)
                                vy2 = setdown.startY2 + (i2 * setdown.changeY) + size
                                
                                VIEW (vx1, vy1)-(vx2 + 1, vy2 + 1)
                                x1 = px - 29 \ (128 \ size)
                                y1 = py - 14 \ (128 \ size)
                                x2 = mx2 - 29 \ (128 \ size)
                                y2 = my2 - 14 \ (128 \ size)
                                dman = INT(SQR(ABS(y1 - y2) ^ 2 + ABS(x1 - x2) ^ 2))
                                CIRCLE (x1 + 1, y1 + 1), dman, c
                               
                                PixS = 4: px = 0: py = 0
                                VIEW (0, 0)-(319, 199)
                            END IF
                        END IF
                        IF PixS = 2 THEN PixS = 3
                    END IF
                NEXT
            NEXT
        END IF
       
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
   
    END IF
    'if clicked on color bar
    IF mx < 31 AND my > 15 THEN
        
        IF mb = 1 THEN
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs)
            i = 0: i2 = 15
            FOR mc = 0 TO 255
                IF mc = lastc THEN LINE (i, i2)-(i + 3, i2 + 5), mc, BF
                i2 = i2 + 5
                IF i2 > 197 THEN i = i + 4: i2 = 15
            NEXT
           
            c = POINT(mx, my - 1)
           
            i = 0: i2 = 15
            FOR mc = 0 TO 255
                IF mc = c THEN LINE (i, i2)-(i + 3, i2 + 5), 15, B
                i2 = i2 + 5
                IF i2 > 197 THEN i = i + 4: i2 = 15
            NEXT
            lastc = c

            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
            IF mb2 = 0 THEN
                ktimer! = TIMER
                mb2 = 1
            END IF
            IF mb2 = 2 THEN
                IF TIMER - ktimer! < .25 THEN
                    CALL menus2
                END IF
                mb2 = 0
            END IF
        END IF
        IF mb = 2 THEN c2 = POINT(mx, my - 1)
        IF mb = 0 AND mb2 = 1 THEN mb2 = 2
    END IF
   
    'if clicked on small side boxes
    IF mb = 1 AND mx > 160 AND my > 15 AND my < 144 THEN
        mnb = 1
        PixS = 0:
        check = -1
        CALL drawboxes
    END IF
   
    'if clicked on toolbar
    IF mb = 1 AND mx > 31 AND my > 144 THEN
        mnb = 1
        PixS = 0:
        check = chc
        CALL drawboxes
        FOR i = 0 TO 5
            IF mx > 33 + (i * 48) AND my > 146 AND mx < 76 + (i * 48) AND my < 170 THEN
                ptool = tool
                tool = i + 1
            END IF
            IF mx > 33 + (i * 48) AND my > 172 AND mx < 76 + (i * 48) AND my < 196 THEN
                ptool = tool
                tool = (i + 7)
            END IF
        NEXT
    END IF
LOOP
CALL over

SUB about
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse

LINE (30, 25)-(280, 140), 0, BF
LINE (30, 25)-(280, 140), 15, B
LINE (30, 40)-(280, 40), 15, B
LINE (150, 126)-(168, 136), 15, B
PSET (185, 36)
PSET (184, 36)
PSET (185, 35)
PSET (184, 35)

CALL locatef(112, 30)
printf (" UDRAW V2 0")
LOCATE 7, 16
PRINT "Programmed"
LOCATE 9, 20
PRINT "by"
LOCATE 11, 16
PRINT "Andy Stubbs"

LOCATE 13, 7
PRINT "For updates and information"
LOCATE 14, 16
PRINT "please goto:"
LOCATE 15, 5
PRINT "http://members.xoom.com/Andy50/"
LOCATE 17, 20
PRINT "OK"
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse

DO UNTIL tme$ <> ""

    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx
   
    IF mx > 150 AND mx < 168 AND my > 126 AND my < 136 AND mb = 1 THEN
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs)
        LINE (150, 126)-(168, 136), 7, BF
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
        tme$ = "D"
    END IF
LOOP
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

END SUB

SUB bNEW
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse

CLS
CALL init
CALL putspics
mx = -1
check = chc
CALL drawboxes
menu2$ = "DUMB"
item2$ = "DUMB"
item$ = ""
'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse

END SUB

SUB CHSIZE
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
menu2$ = menu$
  
LINE (90, 50)-(275, 110), 0, BF
LINE (90, 50)-(275, 110), 15, B
LINE (90, 65)-(275, 65), 15

CALL locatef(140, 55)
printf ("CHANGE SIZE")
LOCATE 10, 18
PRINT ":"; 8; 16; 32
CALL locatef(107, 72)
printf ("SIZE")
CALL locatef(130, 92)
printf ("OK")
CALL locatef(190, 92)
printf ("CANCEL")

LINE (128, 90)-(146, 100), , B
LINE (188, 90)-(238, 100), , B

'LINE (150, 70)-(159, 80), 15, B
'LINE (174, 70)-(191, 80), 15, B
'LINE (206, 70)-(223, 80), 15, B
'LINE (238, 70)-(256, 80), 15, B

'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse

menu2$ = menu$

DO UNTIL tme$ <> ""
 
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx
       
    '(188, 90)-(238, 100)
    '(128, 90)-(146, 100)
  
    'LINE (150, 70)-(159, 80), 15, B
    'LINE (174, 70)-(191, 80), 15, B
    'LINE (206, 70)-(223, 80), 15, B
    'LINE (238, 70)-(256, 80), 15, B

IF mb = 1 THEN
    IF my > 70 AND my < 80 THEN
        IF mx > 140 AND mx < 256 THEN
            'hide mouse
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
           
            LINE (150, 70)-(159, 80), 0, B
            LINE (174, 70)-(191, 80), 0, B
            LINE (206, 70)-(223, 80), 0, B
            sze = 0
       END IF

        IF mx > 196 AND mx < 229 THEN
            LINE (206, 70)-(223, 80), 15, B
            sze = 32
        END IF
       
        IF mx > 164 AND mx < 197 THEN
            LINE (174, 70)-(191, 80), 15, B
            sze = 16
        END IF
      
        IF mx > 140 AND mx < 165 THEN
            LINE (150, 70)-(159, 80), 15, B
            sze = 8
        END IF
        IF mx > 140 AND mx < 256 THEN
            'show mouse
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
        END IF
    END IF
    IF mx > 128 AND mx < 146 THEN
        IF my > 90 AND my < 100 THEN
            'hide mouse
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
            menu2$ = menu$
            
            LINE (128, 90)-(146, 100), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
            IF sze <> 0 THEN size = sze: chc = 1
            'show mouse
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
            tme$ = "done"
        END IF
    END IF
   
    IF mx > 188 AND mx < 238 THEN
        IF my > 90 AND my < 100 THEN
            'hide mouse
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
            menu2$ = menu$
             
            LINE (188, 90)-(238, 100), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
             
            'show mouse
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
            tme$ = "done"
        END IF
    END IF
END IF

LOOP

END SUB

SUB cNEW
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
              
menu2$ = "DUMB"
item2$ = "DUMB"
GET (32, 16)-(159, 143), big(0)
CALL getspics

'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse

END SUB

SUB default
DEF SEG = VARSEG(pal(0))
BLOAD "default.pal", VARPTR(pal(0))
DEF SEG
FOR i = 0 TO 255
    OUT &H3C8, i
    OUT &H3C9, pal(i).red
    OUT &H3C9, pal(i).green
    OUT &H3C9, pal(i).blue
NEXT i

END SUB

SUB drawboxes
     
       IF size = 32 THEN
            cnt = 0
         
            'scan boxes
            FOR i = 0 TO 3
                FOR i2 = 0 TO 2
                    cnt = cnt + 1
                    'move box picture to main screen
                    IF mx > 163 + (i * 39) AND my > 15 + (i2 * 45) OR check = cnt THEN
                    IF mx < 198 + (i * 40) AND my < 50 + (i2 * 45) OR check = cnt THEN
                        chc = cnt
                        mmx = 163 + (i * 39)
                        mmy = 15 + (i2 * 45)
                        inregs.ax = 2
                        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                     
                        FOR i3 = 2 TO 33
                            FOR i4 = 2 TO 33
                                myc = POINT(mmx + i3, mmy + i4)
                                'PSET (i3 + 2, i4 + 2), myc
                                mx3 = (i3 + 6) * (128 \ size): my3 = (i4 + 2) * (128 \ size)
                                LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), myc, BF
                            NEXT
                        NEXT
                     
                        inregs.ax = 1
                        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                 
                    END IF
                    END IF
                NEXT
            NEXT
     
        END IF
 
        IF size = 16 THEN
            cnt = 0
            'scan boxes
            FOR i = 0 TO 6
                FOR i2 = 0 TO 5
                    cnt = cnt + 1
                    'move boxe picture to main screen
                    IF mx > 165 + (i * 22) AND my > 15 + (i2 * 21) OR check = cnt THEN
                    IF mx < 184 + (i * 22) AND my < 34 + (i2 * 21) OR check = cnt THEN
                        chc = cnt
                        mmx = 165 + (i * 22)
                        mmy = 15 + (i2 * 21)
                        inregs.ax = 2
                        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                     
                        FOR i3 = 2 TO 17
                            FOR i4 = 2 TO 17
                                myc = POINT(mmx + i3, mmy + i4)
                                'PSET (i3 + 2, i4 + 2), myc
                                mx3 = (i3 + 2) * (128 \ size): my3 = (i4) * (128 \ size)
                                LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), myc, BF
                            NEXT
                        NEXT
                     
                        inregs.ax = 1
                        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                 
                    END IF
                    END IF
                NEXT
            NEXT
     
        END IF
      
      
        IF size = 8 THEN
            cnt = 0
          
            'scan boxes
            FOR i = 0 TO 10
                FOR i2 = 0 TO 8
                    cnt = cnt + 1
                    'move boxe picture to main screen
                    IF mx > 165 + (i * 14) AND my > 15 + (i2 * 14) OR check = cnt THEN
                    IF mx < 176 + (i * 14) AND my < 26 + (i2 * 14) OR check = cnt THEN
                        chc = cnt
                        mmx = 165 + (i * 14)
                        mmy = 15 + (i2 * 14)
                        inregs.ax = 2
                        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                      
                        FOR i3 = 2 TO 9
                            FOR i4 = 2 TO 9
                                myc = POINT(mmx + i3, mmy + i4)
                                'PSET (i3 + 2, i4 + 2), myc
                                mx3 = i3 * (128 \ size): my3 = (i4 - 1) * (128 \ size)
                                LINE (mx3, my3)-(mx3 + (128 \ size) - 1, my3 + (128 \ size) - 1), myc, BF
                            NEXT
                        NEXT
                      
                        inregs.ax = 1
                        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                  
                    END IF
                    END IF
                NEXT
            NEXT
      
        END IF

END SUB

SUB getspics
   
    cnt = 0
         
    'getting boxes
    FOR i = 0 TO setdown.amountX
        FOR i2 = 0 TO setdown.amountY
            mmx = setdown.startX2 + (i * setdown.changeX)
            mmy = setdown.startY2 + (i2 * setdown.changeY)
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                     
            'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
            SELECT CASE size
                CASE 32:
                    GET (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), pics32(0, cnt)
                CASE 16:
                    GET (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), pics16(0, cnt)
                CASE 8
                    GET (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), pics8(0, cnt)
            END SELECT
            cnt = cnt + 1
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                 
        NEXT
    NEXT
     
END SUB

SUB getspics2
    cnt = 0
        
    'getting boxes
    FOR i = 0 TO setdown.amountX
        FOR i2 = 0 TO setdown.amountY
            mmx = setdown.startX2 + (i * setdown.changeX)
            mmy = setdown.startY2 + (i2 * setdown.changeY)
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
            cnt = cnt + 1
                    
            'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
            IF chc = cnt THEN
            SELECT CASE size
                CASE 32:
                    GET (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), undo(0, 1)
                CASE 16:
                    GET (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), undo(0, 1)
                CASE 8
                    GET (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), undo(0, 1)
            END SELECT
            END IF
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                
        NEXT
    NEXT

END SUB

SUB help
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse

LINE (15, 20)-(288, 180), 0, BF
LINE (15, 20)-(278, 180), 15, B

LINE (278, 20)-(288, 180), 15, B
LINE (278, 170)-(288, 170), 15
LINE (278, 30)-(288, 30), 15

LINE (278, 30)-(283, 20), 15
LINE (288, 30)-(283, 20), 15

LINE (278, 170)-(283, 180), 15
LINE (288, 170)-(283, 180), 15

LOCATE 22, 19
PRINT "OK"
LINE (142, 166)-(160, 176), 15, B
FOR i = 0 TO 499
    fst$(i) = ""
NEXT

fst$(0) = "              HELP             "

fst$(2) = "Welcome to my drawing program. "
fst$(4) = "This program is designed mainly"
fst$(5) = "for the editing of sprites. The"
fst$(6) = "maximum size you can edit is 32"
fst$(7) = "pixels.                        "
fst$(9) = "This program was made in QB4.5 "
fst$(10) = "and uses the screen mode 13h.  "
fst$(11) = "You cannot load the .PCF files "
fst$(12) = "in any other screen mode but   "
fst$(13) = "the .PCB files will load in any"
fst$(14) = "screen mode. For more help on  "
fst$(15) = "how to load these files in your"
fst$(16) = "programs please read the readme"
fst$(17) = "file.                          "

fst$(19) = "All of the menu items have     "
fst$(20) = "shortcut keys for them. For a  "
fst$(21) = "complete list of all the       "
fst$(22) = "shortcut keys that this program"
fst$(23) = "uses read the readme.txt file. "

fst$(25) = "You draw by clicking on the box"
fst$(26) = "in the center. That draws the  "
fst$(27) = "selected tool on your picture. "
fst$(29) = "The arrow keys will move your  "
fst$(30) = "picture around.                "

fst$(32) = "To quickly edit the palette    "
fst$(33) = "just double click on the color "
fst$(34) = "you want to edit. Then select  "
fst$(35) = "the RGB values you want for    "
fst$(36) = "that color.                    "

fst$(38) = "A quick way to change the      "
fst$(39) = "picture size is to press the:  "
fst$(40) = "'+' and '-' keys. This program "
fst$(41) = "lets you work on pictures sizes"
fst$(42) = "of 8, 16, and 32 all at the    "
fst$(43) = "same time, so don't worry about"
fst$(44) = "changing the size because when "
fst$(45) = "you come back to that size your"
fst$(46) = "picture will be restored.      "

fst$(52) = "To select a color off of the   "
fst$(53) = "picture just right click on it."

fst$(48) = "To change the frame you are    "
fst$(49) = "currently working on just click"
fst$(50) = "on the boxes on the right.     "

fst$(56) = "If it's in mode 1 then right   "
fst$(57) = "click on the picture to get a  "
fst$(58) = "color. If its in mode 0 then   "
fst$(59) = "right click to erase a pixel.  "
fst$(60) = "To change the mode press the G "
fst$(61) = "key.                           "

fst$(63) = "For more help please check out "
fst$(64) = "readme.txt file.               "

inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)
tme$ = ""
gi = 0
pi = 1
DO UNTIL tme$ <> ""
    IF gi < 0 THEN gi = 0
    IF gi > 50 THEN gi = 50
    IF gi <> pi THEN
        pi = gi
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        FOR i = gi TO gi + 15
            LOCATE i - gi + 5, 4
            IF fst$(i) <> "" THEN PRINT fst$(i)
            IF fst$(i) = "" THEN PRINT "                               "
        NEXT
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
    END IF
   
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx
    IF mb = 1 THEN
        'LINE (278, 170)-(288, 170), 15
        'LINE (278, 30)-(288, 30), 15
        IF mx > 278 AND mx < 288 AND my > 30 AND my < 90 THEN
            gi = gi - 5
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP
        END IF
        IF mx > 278 AND mx < 288 AND my < 170 AND my >= 90 THEN
            gi = gi + 5
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP
        END IF
       
        IF mx > 278 AND mx < 288 AND my > 170 AND my < 180 THEN
            gi = gi + 1
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP
        END IF
        IF mx > 278 AND mx < 288 AND my < 30 AND my > 20 THEN
            gi = gi - 1
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP
        END IF
       
        IF mx > 142 AND mx < 160 AND my > 166 AND my < 176 THEN
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs)
            LINE (142, 166)-(160, 176), 7, BF
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
            tme$ = "D"
        END IF
    END IF
LOOP
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

END SUB

SUB init

SELECT CASE size
    CASE 8:
            setdown.changeX = 14: setdown.changeY = 14: setdown.amountX = 10
            setdown.amountY = 8: setdown.startX = 165: setdown.startY = 16
            setdown.startX2 = 165: setdown.startY2 = 15
    CASE 16:
            setdown.changeX = 22: setdown.changeY = 21: setdown.amountX = 6
            setdown.amountY = 5: setdown.startX = 163: setdown.startY = 15
            setdown.startX2 = 165: setdown.startY2 = 15
    CASE 32:
            setdown.changeX = 39: setdown.changeY = 45: setdown.amountX = 3
            setdown.amountY = 2: setdown.startX = 157: setdown.startY = 13
            setdown.startX2 = 163: setdown.startY2 = 15
END SELECT

mc = 0
'draw color bar
'FOR i = 0 TO 4
'    FOR i2 = 4.5 TO 54
'        LINE (i * 6, i2 * 3.6)-(i * 6 + 5, i2 * 3.6 + 3), mc, BF
'        mc = mc + 1
'    NEXT
'NEXT
i = 0: i2 = 15
FOR mc = 0 TO 255
    LINE (i, i2)-(i + 3, i2 + 5), mc, BF
    i2 = i2 + 5
    IF i2 > 197 THEN i = i + 4: i2 = 15
NEXT


'draw toolbar
LINE (31, 144)-(319, 199), 0, BF
LINE (31, 144)-(319, 199), , B
PAINT (32, 145), 0, 15

FOR i = 0 TO 5
    DEF SEG = VARSEG(back(0))
    BLOAD "tool" + LTRIM$(STR$(i)) + ".pcf", VARPTR(back(0))
    DEF SEG
    PUT (40 + (i * 48), 146), back(0), PSET
    DEF SEG = VARSEG(back(0))
    BLOAD "tool" + LTRIM$(STR$(i + 6)) + ".pcf", VARPTR(back(0))
    DEF SEG
    PUT (40 + (i * 48), 168), back(0), PSET
   
    LINE (33 + (i * 48), 146)-(76 + (i * 48), 170), , B
    LINE (33 + (i * 48), 172)-(76 + (i * 48), 197), , B
NEXT
   
    CALL locatef(8, 5)
    printf ("FILE")
    CALL locatef(51, 5)
    printf ("EDIT")
    CALL locatef(91, 5)
    printf ("PAL")
    CALL locatef(122, 5)
    printf ("HELP")

    CALL locatef(190, 5)
    printf ("MODE 1")
END SUB

SUB initfont
CLS
SCREEN 13
PRINT
PRINT " A B C D E F G H I J K L M"
PRINT
PRINT " N O P Q R S T U V W X Y Z "
PRINT
PRINT " 0 1 2 3 4 5 6 7 8 9"

FOR i = 0 TO 12
    'LINE (8 + (i * 16), 8)-(14 + (i * 16), 14), , B
    GET (8 + (i * 16), 8)-(14 + (i * 16), 14), font(0, i)
NEXT

FOR i = 0 TO 13
    'LINE (8 + (i * 16), 24)-(14 + (i * 16), 30), , B
    GET (8 + (i * 16), 24)-(14 + (i * 16), 30), font(0, (i + 13))
NEXT

FOR i = 0 TO 9
    'LINE (8 + (i * 16), 40)-(14 + (i * 16), 46), , B
    GET (8 + (i * 16), 40)-(14 + (i * 16), 46), font(0, (i + 27))
    'PRINT i + 27
NEXT
FOR i = 0 TO 240
    FOR i2 = 0 TO 90
        IF POINT(i, i2) = 0 THEN PSET (i, i2), 255 ELSE PSET (i, i2), 0
    NEXT
NEXT

FOR i = 0 TO 12
    'LINE (8 + (i * 16), 8)-(14 + (i * 16), 14), , B
    GET (8 + (i * 16), 8)-(14 + (i * 16), 14), mfont(0, i)
NEXT

FOR i = 0 TO 14
    'LINE (8 + (i * 16), 24)-(14 + (i * 16), 30), , B
    GET (8 + (i * 16), 24)-(14 + (i * 16), 30), mfont(0, (i + 13))
NEXT

FOR i = 0 TO 9
    'LINE (8 + (i * 16), 40)-(14 + (i * 16), 46), , B
    GET (8 + (i * 16), 40)-(14 + (i * 16), 46), mfont(0, (i + 27))
NEXT

END SUB

FUNCTION inputf$
kb$ = ""
DO UNTIL kb$ = CHR$(13)
  
   kb$ = ""
   DO UNTIL kb$ <> ""
        kb$ = INKEY$
        
        FOR bi = 0 TO 35
            SELECT CASE bi
                CASE 0: car$ = "A"
                CASE 1: car$ = "B"
                CASE 2: car$ = "C"
                CASE 3: car$ = "D"
                CASE 4: car$ = "E"
                CASE 5: car$ = "F"
                CASE 6: car$ = "G"
                CASE 7: car$ = "H"
                CASE 8: car$ = "I"
                CASE 9: car$ = "J"
                CASE 10: car$ = "K"
                CASE 11: car$ = "L"
                CASE 12: car$ = "M"
                CASE 13: car$ = "N"
                CASE 14: car$ = "O"
                CASE 15: car$ = "P"
                CASE 16: car$ = "Q"
                CASE 17: car$ = "S"
                CASE 18: car$ = "T"
                CASE 19: car$ = "U"
                CASE 20: car$ = "V"
                CASE 21: car$ = "W"
                CASE 22: car$ = "X"
                CASE 23: car$ = "Y"
                CASE 24: car$ = "Z"
                CASE 25: car$ = "0"
                CASE 26: car$ = "1"
                CASE 27: car$ = "2"
                CASE 28: car$ = "3"
                CASE 29: car$ = "4"
                CASE 30: car$ = "5"
                CASE 31: car$ = "6"
                CASE 32: car$ = "7"
                CASE 33: car$ = "8"
                CASE 34: car$ = "9"
                CASE 35: car$ = "R"
            END SELECT
            IF kb$ = CHR$(8) AND LEN(word$) > 0 THEN
                word$ = LEFT$(word$, LEN(word$) - 1)
                kb$ = ""
                LINE (160, 70)-(270, 80), 0, BF
                LINE (160, 70)-(270, 80), 15, B
                CALL locatef(162, 72)
                printf (UCASE$(word$))
            END IF

            IF UCASE$(kb$) = car$ AND LEN(word$) <= 6 THEN
                printf (UCASE$(kb$))
                word$ = word$ + kb$
            END IF
        NEXT
   LOOP
LOOP
kb$ = ""
inputf$ = word$
END FUNCTION

SUB load
plano$ = "NO"
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
menu2$ = menu$
filename2$ = "NOTREADYYET"
FOR i = 0 TO 499
    fst$(i) = ""
NEXT

SHELL "dir *.pc? > lst.txt /o:n"
OPEN "lst.txt" FOR INPUT AS #1

DO WHILE NOT EOF(1)
    INPUT #1, fst$
    fst$ = UCASE$(fst$)
    x = INSTR(fst$, " PCF")
    IF x > 0 THEN
        fst$(aie) = RTRIM$(LEFT$(fst$, x)) + ".PCF"
        aie = aie + 1
    END IF
    x = INSTR(fst$, " PCB")
    IF x > 0 THEN
        fst$(aie) = RTRIM$(LEFT$(fst$, x)) + ".PCB"
        aie = aie + 1
    END IF
LOOP

CLOSE #1

LINE (45, 25)-(265, 165), 0, BF
LINE (45, 25)-(265, 165), 15, B

LINE (170, 42)-(170, 165)
LINE (180, 42)-(180, 165)
LINE (170, 42)-(180, 42)
LINE (170, 52)-(180, 52)
LINE (170, 52)-(175, 42)
LINE (180, 52)-(175, 42)
LINE (170, 155)-(180, 155)
LINE (170, 155)-(175, 165)
LINE (180, 155)-(175, 165)

LINE (45, 42)-(265, 42)

LINE (185, 133)-(260, 145), 15, B
LINE (185, 149)-(260, 161), 15, B


LOCATE 18, 28
PRINT "OK"

LOCATE 20, 26
PRINT "CANCEL"

LOCATE 5, 8
PRINT "FILES:"
gi = 0
FOR i = gi TO gi + 13
    LOCATE (i + 7) - gi, 8
    PRINT fst$(i)
NEXT

inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP

DO UNTIL tme$ <> ""
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = outregs.cx \ 2
    my = outregs.dx
    mb = outregs.bx
   
    IF mb = 1 THEN
      
        IF mx > 45 AND mx < 170 THEN
            IF my > 42 AND my < 265 THEN
                FOR i = 0 TO 13
                    IF (my - 48) \ 8 = i THEN filename$ = fst$(i + gi)
                NEXT
                IF filename$ <> filename2$ THEN
                    LINE (100, 30)-(245, 40), 0, BF
                    LINE (190, 50)-(254, 114), 0, BF
                    x = INSTR(filename$, ".PCF")
                    IF x > 0 THEN
                        DEF SEG = VARSEG(back(0))
                        BLOAD filename$, VARPTR(back(0))
                        DEF SEG
                        PUT (190, 50), back(0), PSET
                    END IF
                END IF
                LOCATE 5, 14
                PRINT filename$;
                filename2$ = filename$
            END IF
        END IF
      
        IF mx > 170 AND mx < 180 THEN
            IF my > 52 AND my < 101 THEN
                IF gi > 0 THEN gi = gi - 7
                IF gi < 0 THEN gi = 0
            END IF
            IF my > 100 AND my < 155 THEN
                IF gi < 1480 THEN gi = gi + 7
                IF gi > 1480 THEN gi = 1480
            END IF
          
            IF my < 165 AND my > 155 AND gi < 1499 THEN
                gi = gi + 1
            END IF
          
            IF my < 52 AND my > 42 AND gi > 0 THEN
                gi = gi - 1
            END IF
          
            LINE (46, 46)-(169, 164), 0, BF
            FOR i = gi TO gi + 13
                LOCATE (i + 7) - gi, 8
                PRINT fst$(i)
            NEXT
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP
        END IF
        IF mx > 185 AND mx < 260 THEN
            IF my > 133 AND my < 145 THEN
                tme$ = "OK"
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (185, 133)-(260, 145), 7, BF
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
            END IF
            IF my > 149 AND my < 161 THEN
                tme$ = "DONE"
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (185, 149)-(260, 161), 7, BF
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
            END IF
      
        END IF
    END IF

LOOP

IF tme$ = "OK" THEN
    x = INSTR(filename$, ".PCB")
    IF x > 0 THEN
        CALL load2
    END IF
    x = 0

    x = INSTR(filename$, ".PCF")
    IF x > 0 THEN
        DEF SEG = VARSEG(back(0))
        BLOAD filename$, VARPTR(back(0))
        DEF SEG
       
        IF back(1) > 0 AND back(1) < 9 AND size = 8 THEN
            FOR i = 0 TO 34
                pics8(i, chc - 1) = back(i)
            NEXT
        END IF
       
        IF back(1) > 0 AND back(1) < 17 AND size = 16 THEN
            FOR i = 0 TO 130
                pics16(i, chc - 1) = back(i)
            NEXT
        END IF
       
        IF back(1) > 0 AND back(1) < 33 AND size = 32 THEN
            FOR i = 0 TO 514
                pics32(i, chc - 1) = back(i)
            NEXT
        END IF
       

    END IF
END IF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

END SUB

SUB load2
    OPEN filename$ FOR INPUT AS #1
        INPUT #1, type$
        IF type$ = "***pcb***" THEN
            INPUT #1, sze
            FOR i = 0 TO 255
                INPUT #1, ir
                INPUT #1, ig
                INPUT #1, ib
                IF plano$ = "NO" THEN
                    OUT &H3C8, i
                    OUT &H3C9, INT(ir)
                    OUT &H3C9, INT(ig)
                    OUT &H3C9, INT(ib)
                END IF
            NEXT i
            IF sze = 8 THEN
                FOR i = 0 TO 33
                    FOR i2 = 0 TO 99
                        INPUT #1, pics8(i, i2)
                    NEXT
                NEXT
            END IF
            IF sze = 16 THEN
                FOR i = 0 TO 129
                    FOR i2 = 0 TO 42
                        INPUT #1, pics16(i, i2)
                    NEXT
                NEXT
            END IF
            IF sze = 32 THEN
                FOR i = 0 TO 513
                    FOR i2 = 0 TO 12
                        INPUT #1, pics32(i, i2)
                    NEXT
                NEXT
            END IF
        END IF
    CLOSE #1
    size = sze

END SUB

SUB loadp
menu2$ = menu$
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs)
FOR i = 0 TO 499
    fst$(i) = ""
NEXT

SHELL "dir *.pal > lst.txt"
OPEN "lst.txt" FOR INPUT AS #1

DO WHILE NOT EOF(1)
    INPUT #1, fst$
    fst$ = UCASE$(fst$)
    x = INSTR(fst$, " PAL")
    IF x > 0 THEN
        fst$(aie) = RTRIM$(LEFT$(fst$, x)) + ".PAL"
        aie = aie + 1
    END IF
LOOP

CLOSE #1

LINE (45, 25)-(265, 165), 0, BF
LINE (45, 25)-(265, 165), 15, B

LINE (170, 42)-(170, 165)
LINE (180, 42)-(180, 165)

LINE (170, 42)-(180, 42)
LINE (170, 52)-(180, 52)
LINE (170, 155)-(180, 155)

LINE (45, 42)-(265, 42)
LINE (170, 52)-(180, 52)
LINE (170, 52)-(175, 42)
LINE (180, 52)-(175, 42)
LINE (170, 155)-(180, 155)
LINE (170, 155)-(175, 165)
LINE (180, 155)-(175, 165)

LINE (185, 133)-(260, 145), 15, B
LINE (185, 149)-(260, 161), 15, B

LOCATE 18, 28
PRINT "OK"

LOCATE 20, 26
PRINT "CANCEL"

LOCATE 5, 8
PRINT "FILE:"

gi = 0
FOR i = gi TO gi + 13
    LOCATE (i + 7) - gi, 8
    PRINT fst$(i)
NEXT

inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP

DO UNTIL tme$ <> ""
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = outregs.cx \ 2
    my = outregs.dx
    mb = outregs.bx
   
    IF mb = 1 THEN
      
        IF mx > 45 AND mx < 170 THEN
            IF my > 42 AND my < 265 THEN
                FOR i = 0 TO 13
                    IF (my - 48) \ 8 = i THEN filename$ = fst$(i + gi)
                NEXT
                IF filename$ <> filename2$ THEN
                    LINE (100, 30)-(265, 40), 0, BF
                    DEF SEG = VARSEG(pal(0))
                    BLOAD filename$, VARPTR(pal(0))
                    DEF SEG
                    reds = 0: blues = 0: greens = 0
                    FOR i = 0 TO 255
                        reds = reds + pal(i).red
                        blues = blues + pal(i).blue
                        greens = greens + pal(i).green
                    NEXT i
                    CALL readpal
                    LOCATE 5, 14
                    PRINT filename$;
                    filename2$ = filename$
                    LOCATE 8, 25
                    COLOR cr
                    PRINT "R:"; reds
                    LOCATE 9, 25
                    COLOR cg
                    PRINT "G:"; greens
                    LOCATE 10, 25
                    COLOR cb
                    PRINT "B:"; blues
                    COLOR 15
                END IF
            END IF
        END IF
      
        IF mx > 170 AND mx < 180 THEN
            IF my > 52 AND my < 101 THEN
                IF gi > 0 THEN gi = gi - 7
                IF gi < 0 THEN gi = 0
            END IF
            IF my > 100 AND my < 155 THEN
                IF gi < 1480 THEN gi = gi + 7
                IF gi > 1480 THEN gi = 1480
            END IF
          
            IF my < 165 AND my > 155 AND gi < 1499 THEN
                gi = gi + 1
            END IF
          
            IF my < 52 AND my > 42 AND gi > 0 THEN
                gi = gi - 1
            END IF
          
            LINE (46, 46)-(169, 164), 0, BF
            FOR i = gi TO gi + 13
                LOCATE (i + 7) - gi, 8
                PRINT fst$(i)
            NEXT
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .1: LOOP
        END IF
        IF mx > 185 AND mx < 260 THEN
            IF my > 133 AND my < 145 THEN
                tme$ = "OK"
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (185, 133)-(260, 145), 7, BF
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
            END IF
            IF my > 149 AND my < 161 THEN
                tme$ = "DONE"
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (185, 149)-(260, 161), 7, BF
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
            END IF
      
        END IF
    END IF

LOOP

IF tme$ = "OK" AND filename$ <> "" THEN
    DEF SEG = VARSEG(pal(0))
    BLOAD filename$, VARPTR(pal(0))
    DEF SEG

    FOR i = 0 TO 255
        OUT &H3C8, i
        OUT &H3C9, pal(i).red
        OUT &H3C9, pal(i).green
        OUT &H3C9, pal(i).blue
    NEXT i
    CALL readpal
 
END IF

mx = 0: my = 0
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

END SUB

SUB locatef (printX%, printY%)
LocPrintX% = printX%
LocPrintY% = printY%
END SUB

SUB menus
'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse
DO UNTIL mx > 250 OR my > 80 OR item$ <> ""
IF mb = 1 THEN

    IF my < 16 THEN
   
        IF mx > 4 AND mx < 43 THEN
            menu$ = "FILE"
        END IF
        IF mx > 47 AND mx < 83 THEN
            menu$ = "EDIT"
        END IF
        IF mx > 89 AND mx < 115 THEN
            menu$ = "PAL"
        END IF
        IF mx > 119 AND mx < 155 THEN
            menu$ = "HELP"
        END IF
    END IF
    IF my > 16 THEN
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
       
        IF my > 16 AND my < 30 AND item$ <> item2$ THEN
            IF menu$ = "FILE" AND mx > 4 AND mx < 110 THEN
                LINE (8, 20)-(108, 27), 7, BF
                item$ = "NEW"
            END IF
            IF menu$ = "EDIT" AND mx > 47 AND mx < 150 THEN
                LINE (50, 20)-(150, 27), 7, BF
                item$ = "COPY"
            END IF
            IF menu$ = "PAL" AND mx > 89 AND mx < 220 THEN
                LINE (92, 20)-(220, 27), 7, BF
                item$ = "DEFAULT"
            END IF
            IF menu$ = "HELP" AND mx > 119 AND mx < 200 THEN
                LINE (122, 20)-(202, 27), 7, BF
                item$ = "HELP"
            END IF
            IF item$ <> "" THEN item2$ = item$
        END IF
       
        IF my > 30 AND my < 40 AND item$ <> item2$ THEN
            IF menu$ = "FILE" AND mx > 4 AND mx < 110 THEN
                LINE (8, 30)-(108, 37), 7, BF
                item$ = "SAVE"
            END IF
            IF menu$ = "EDIT" AND mx > 47 AND mx < 150 THEN
                LINE (50, 30)-(150, 37), 7, BF
                item$ = "PASTE"
            END IF
            IF menu$ = "PAL" AND mx > 89 AND mx < 220 THEN
                LINE (92, 30)-(220, 37), 7, BF
                item$ = "MYPAL"
            END IF
            IF menu$ = "HELP" AND mx > 119 AND mx < 200 THEN
                LINE (122, 30)-(202, 37), 7, BF
                item$ = "ABOUT"
            END IF
            IF item$ <> "" THEN item2$ = item$
        END IF
       
        IF my > 40 AND my < 50 AND item$ <> item2$ THEN
            IF menu$ = "FILE" AND mx > 4 AND mx < 110 THEN
                LINE (8, 40)-(108, 47), 7, BF
                item$ = "LOAD"
            END IF
            IF menu$ = "EDIT" AND mx > 47 AND mx < 150 THEN
                LINE (50, 40)-(150, 47), 7, BF
                item$ = "SIZE"
            END IF
            IF menu$ = "PAL" AND mx > 89 AND mx < 220 THEN
                LINE (92, 40)-(220, 47), 7, BF
                item$ = "SAVEP"
            END IF
            IF item$ <> "" THEN item2$ = item$
        END IF
       
        IF my > 50 AND my < 60 AND item$ <> item2$ THEN
            IF menu$ = "FILE" AND mx > 4 AND mx < 110 THEN
                LINE (8, 50)-(108, 57), 7, BF
                item$ = "QUIT"
            END IF
            IF menu$ = "EDIT" AND mx > 47 AND mx < 150 THEN
                LINE (50, 50)-(150, 57), 7, BF
                item$ = "TOOLS"
            END IF
            IF menu$ = "PAL" AND mx > 89 AND mx < 220 THEN
                LINE (92, 50)-(220, 57), 7, BF
                item$ = "LOADP"
            END IF
            IF item$ <> "" THEN item2$ = item$
        END IF
        IF my > 60 AND my < 70 AND item$ <> item2$ THEN
            IF menu$ = "EDIT" AND mx > 47 AND mx < 150 THEN
                LINE (50, 60)-(150, 67), 7, BF
                item$ = "UNDO"
            END IF
           
            IF item$ <> "" THEN item2$ = item$
        END IF

        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
   
    END IF
   
       
    IF menu2$ <> menu$ THEN
        item2$ = "DUMB"
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
       
        'LINE (4, 13)-(225, 63), 0, BF
        CLS
        CALL init
        mx = -1
        check = chc
        CALL drawboxes
        CALL viewscrn
        CALL putspics
        PUT (32, 16), big, PSET
       
        IF menu$ = "FILE" THEN
            LINE (4, 13)-(113, 63), 15, B
            LINE (5, 14)-(112, 62), 0, BF
            CALL locatef(10, 20)
            printf ("NEW        N")
            CALL locatef(10, 40)
            printf ("LOAD       L")
            CALL locatef(10, 30)
            printf ("SAVE       S")
            CALL locatef(10, 50)
            printf ("QUIT       Q")

        END IF
        IF menu$ = "EDIT" THEN
            LINE (47, 13)-(155, 72), 15, B
            LINE (48, 14)-(154, 71), 0, BF
            CALL locatef(50, 20)
            printf ("COPY       C")
            CALL locatef(50, 30)
            printf ("PASTE      P")
            CALL locatef(50, 40)
            printf ("SIZE       Z")
            CALL locatef(50, 50)
            printf ("TOOLS      T")
            CALL locatef(50, 60)
            printf ("UNDO       U")

        END IF
        IF menu$ = "PAL" THEN
            LINE (89, 13)-(225, 63), 15, B
            LINE (90, 14)-(224, 62), 0, BF
            CALL locatef(92, 20)
            printf ("DEFAULT PAL    D")
            CALL locatef(92, 30)
            printf ("RESTORE MYPAL  M")
            CALL locatef(92, 40)
            printf ("SAVE PAL       V")
            CALL locatef(92, 50)
            printf ("LOAD PAL       O")
       
        END IF
        IF menu$ = "HELP" THEN
            LINE (119, 13)-(205, 43), 15, B
            LINE (120, 14)-(204, 42), 0, BF
            CALL locatef(122, 20)
            printf ("HELP     H")
            CALL locatef(122, 30)
            printf ("ABOUT    A")
        END IF
        'show mouse
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs) 'to show mouse
        menu2$ = menu$
    END IF
    IF item$ = "HELP" THEN CALL help
    IF item$ = "LOADP" THEN CALL loadp
    IF item$ = "SAVEP" THEN CALL savep
    IF item$ = "ABOUT" THEN CALL about
    IF item$ = "COPY" THEN tool = 7
    IF item$ = "PASTE" THEN tool = 8
    IF item$ = "QUIT" THEN CALL quit
    IF item$ = "LOAD" THEN CALL load
    IF item$ = "SIZE" THEN CALL CHSIZE
    IF item$ = "NEW" THEN CALL new
    IF item$ = "SAVE" THEN CALL save
    IF item$ = "TOOLS" THEN CALL tools
    IF item$ = "UNDO" THEN
        mx = -1
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs) 'to hide mouse
        CALL putspics2
        CALL getspics
        check = chc
        CALL drawboxes
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
        FOR i = 0 TO 514
            undo(i, 0) = undo(i, 1)
        NEXT
        CALL getspics2
    END IF
    IF item$ = "MYPAL" THEN
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
        menu2$ = menu$
        CALL mypal
    END IF
    IF item$ = "DEFAULT" THEN
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
        CALL default
    END IF
END IF

inregs.ax = 3
CALL interrupt(&H33, inregs, outregs)
mx = (outregs.cx / 2)
my = outregs.dx
mb = outregs.bx

LOOP
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse

CLS
CALL init
CALL putspics
mx = -1
check = chc
CALL drawboxes
menu2$ = "DUMB"
item2$ = "DUMB"
item$ = ""
'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse

END SUB

SUB menus2
CALL getspics
IF my < 45 THEN my = 45
IF my > 145 THEN my = 145

inregs.ax = 2
CALL interrupt(&H33, inregs, outregs)

OUT &H3C7, c
red = INP(&H3C9)
green = INP(&H3C9)
blue = INP(&H3C9)

LINE (mx + 5, my - 25)-(mx + 90, my + 49), 0, BF
LINE (mx + 5, my - 25)-(mx + 90, my + 49), 15, B
LINE (mx + 5, my - 15)-(mx + 90, my - 15)
LINE (mx + 36, my + 23)-(mx + 54, my + 33), , B
LINE (mx + 21, my + 36)-(mx + 71, my + 46), , B

LINE (mx + 61, my - 12)-(mx + 78, my - 2), , B
LINE (mx + 61, my - 12)-(mx + 56, my - 7)
LINE (mx + 61, my - 2)-(mx + 56, my - 7)
LINE (mx + 78, my - 12)-(mx + 83, my - 7)
LINE (mx + 78, my - 2)-(mx + 83, my - 7)

LINE (mx + 61, my - 1)-(mx + 78, my + 9), , B
LINE (mx + 61, my - 1)-(mx + 56, my + 4)
LINE (mx + 61, my + 9)-(mx + 56, my + 4)
LINE (mx + 78, my - 1)-(mx + 83, my + 4)
LINE (mx + 78, my + 9)-(mx + 83, my + 4)

LINE (mx + 61, my + 10)-(mx + 78, my + 20), , B
LINE (mx + 61, my + 10)-(mx + 56, my + 15)
LINE (mx + 61, my + 20)-(mx + 56, my + 15)
LINE (mx + 78, my + 10)-(mx + 83, my + 15)
LINE (mx + 78, my + 20)-(mx + 83, my + 15)

CALL locatef(mx + 25, my - 23)
printf ("COLOR")
CALL locatef(mx + 15, my - 10)
printf ("RED  ")
printf (RTRIM$(STR$(red)))
CALL locatef(mx + 15, my + 1)
printf ("GREEN")
printf (RTRIM$(STR$(green)))
CALL locatef(mx + 15, my + 12)
printf ("BLUE ")
printf (RTRIM$(STR$(blue)))

CALL locatef(mx + 38, my + 25)
printf ("OK")

CALL locatef(mx + 23, my + 38)
printf ("CANCEL")

red2 = red
green2 = green
blue2 = blue

inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)
mex = mx
mey = my

DO UNTIL tme$ <> ""
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx
    IF mb = 0 THEN tmr50! = .4
    IF mb = 1 THEN
        IF mx > mex + 56 AND mx < mex + 61 THEN
            IF my > mey - 12 AND my < mey - 2 THEN
                IF red > 0 AND TIMER - ktimer! > tmr50! THEN
                    tmr50! = (tmr50! / 3)
                    ktimer! = TIMER
                    inregs.ax = 2
                    CALL interrupt(&H33, inregs, outregs)
                    red = red - 1
                    LINE (mex + 61, mey - 12)-(mex + 78, mey - 2), 0, BF
                    LINE (mex + 61, mey - 12)-(mex + 78, mey - 2), 15, B
                    CALL locatef(mex + 55, mey - 10)
                    printf (RTRIM$(STR$(red)))
                    OUT &H3C8, c
                    OUT &H3C9, red
                    OUT &H3C9, green
                    OUT &H3C9, blue
                    inregs.ax = 1
                    CALL interrupt(&H33, inregs, outregs)
                END IF
            END IF
            IF my > mey - 1 AND my < mey + 9 THEN
                IF green > 0 AND TIMER - ktimer! > tmr50! THEN
                    tmr50! = (tmr50! / 3)
                    ktimer! = TIMER
                    inregs.ax = 2
                    CALL interrupt(&H33, inregs, outregs)
                    green = green - 1
                    LINE (mex + 61, mey - 1)-(mex + 78, mey + 9), 0, BF
                    LINE (mex + 61, mey - 1)-(mex + 78, mey + 9), 15, B
                    CALL locatef(mex + 55, mey + 1)
                    printf (RTRIM$(STR$(green)))
                    OUT &H3C8, c
                    OUT &H3C9, red
                    OUT &H3C9, green
                    OUT &H3C9, blue
                    inregs.ax = 1
                    CALL interrupt(&H33, inregs, outregs)
                END IF
                  
            END IF
            IF my > mey + 10 AND my < mey + 20 THEN
                IF blue > 0 AND TIMER - ktimer! > tmr50! THEN
                    tmr50! = (tmr50! / 3)
                    ktimer! = TIMER
                    inregs.ax = 2
                    CALL interrupt(&H33, inregs, outregs)
                    blue = blue - 1
                    LINE (mex + 61, mey + 10)-(mex + 78, mey + 20), 0, BF
                    LINE (mex + 61, mey + 10)-(mex + 78, mey + 20), 15, B
                    CALL locatef(mex + 55, mey + 12)
                    printf (RTRIM$(STR$(blue)))
                    OUT &H3C8, c
                    OUT &H3C9, red
                    OUT &H3C9, green
                    OUT &H3C9, blue
                    inregs.ax = 1
                    CALL interrupt(&H33, inregs, outregs)
                END IF
               
            END IF
       
        END IF
        IF mx > mex + 78 AND mx < mex + 83 THEN
            IF my > mey - 12 AND my < mey - 2 THEN
                IF red < 63 AND TIMER - ktimer! > tmr50! THEN
                    tmr50! = (tmr50! / 3)
                    ktimer! = TIMER
                    inregs.ax = 2
                    CALL interrupt(&H33, inregs, outregs)
                    red = red + 1
                    LINE (mex + 61, mey - 12)-(mex + 78, mey - 2), 0, BF
                    LINE (mex + 61, mey - 12)-(mex + 78, mey - 2), 15, B
                    CALL locatef(mex + 55, mey - 10)
                    printf (RTRIM$(STR$(red)))
                    OUT &H3C8, c
                    OUT &H3C9, red
                    OUT &H3C9, green
                    OUT &H3C9, blue
                    inregs.ax = 1
                    CALL interrupt(&H33, inregs, outregs)
                END IF
            END IF
            IF my > mey - 1 AND my < mey + 9 THEN
                IF green < 63 AND TIMER - ktimer! > tmr50! THEN
                    tmr50! = (tmr50! / 3)
                    ktimer! = TIMER
                    inregs.ax = 2
                    CALL interrupt(&H33, inregs, outregs)
                    green = green + 1
                    LINE (mex + 61, mey - 1)-(mex + 78, mey + 9), 0, BF
                    LINE (mex + 61, mey - 1)-(mex + 78, mey + 9), 15, B
                    CALL locatef(mex + 55, mey + 1)
                    printf (RTRIM$(STR$(green)))
                    OUT &H3C8, c
                    OUT &H3C9, red
                    OUT &H3C9, green
                    OUT &H3C9, blue
                    inregs.ax = 1
                    CALL interrupt(&H33, inregs, outregs)
                END IF
                 
            END IF
            IF my > mey + 10 AND my < mey + 20 THEN
                IF blue < 63 AND TIMER - ktimer! > tmr50! THEN
                    tmr50! = (tmr50! / 3)
                    ktimer! = TIMER
                    inregs.ax = 2
                    CALL interrupt(&H33, inregs, outregs)
                    blue = blue + 1
                    LINE (mex + 61, mey + 10)-(mex + 78, mey + 20), 0, BF
                    LINE (mex + 61, mey + 10)-(mex + 78, mey + 20), 15, B
                    CALL locatef(mex + 55, mey + 12)
                    printf (RTRIM$(STR$(blue)))
                    OUT &H3C8, c
                    OUT &H3C9, red
                    OUT &H3C9, green
                    OUT &H3C9, blue
                    inregs.ax = 1
                    CALL interrupt(&H33, inregs, outregs)
                END IF
              
            END IF
       
        END IF
       
        IF mx > mex + 36 AND mx < mex + 54 THEN
            IF my > mey + 23 AND my < mey + 33 THEN
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (mex + 36, mey + 23)-(mex + 54, mey + 33), 7, BF
                tme$ = "done"
            END IF
        END IF
       
        IF mx > mex + 21 AND mx < mex + 71 THEN
            IF my > mey + 36 AND my < mey + 46 THEN
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (mex + 21, mey + 36)-(mex + 71, mey + 46), 7, BF
                tme$ = "done"
                red = red2: green = green2: blue = blue2
                OUT &H3C8, c
                OUT &H3C9, red
                OUT &H3C9, green
                OUT &H3C9, blue
            END IF
        END IF
   
    END IF
LOOP

tmr50! = TIMER
DO WHILE tmr50! + .3 > TIMER: LOOP
mb = 0
CLS
CALL init
CALL putspics
mx = -1
check = chc
CALL drawboxes

inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)

END SUB

SUB mtools
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse

    CLS
    CALL init
    CALL putspics
    mx = -1
    check = chc
    CALL drawboxes
    cnt = 0
    IF mtool = 7 THEN
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                countr = 999
                IF cnt = chc THEN
                    FOR i3 = mmx + 1 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 1
                            thisC = POINT(i3, i4)
                            IF thisC > 0 THEN
                                OUT &H3C7, thisC
                                red = INP(&H3C9)
                                green = INP(&H3C9)
                                blue = INP(&H3C9)
                                IF red >= green AND red >= blue THEN blue = red
                                IF green >= red AND green >= blue THEN blue = green
                                red = 0: green = 0
                                
                                FOR myi = 1 TO 254
                                    OUT &H3C7, myi
                                    red2 = INP(&H3C9)
                                    green2 = INP(&H3C9)
                                    blue2 = INP(&H3C9)
                                    test = (red2 + green2) + (blue - blue2) ^ 2
                                    IF test < 15 OR test < countr THEN thisC = myi: countr = test
                                NEXT
                           
                            END IF
                            PSET (i3, i4), thisC
                            IF i3 + 1 > mmx + size + 1 THEN PSET (mmx + 1, i4), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    countr = 0
                    mx = 0
                    CALL drawboxes
              
                END IF
           
            NEXT
        NEXT
    END IF
    IF mtool = 6 THEN
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                countr = 999
                IF cnt = chc THEN
                    FOR i3 = mmx + 1 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 1
                            thisC = POINT(i3, i4)
                            IF thisC > 0 THEN
                                OUT &H3C7, thisC
                                red = INP(&H3C9)
                                green = INP(&H3C9)
                                blue = INP(&H3C9)
                                IF red >= green AND red >= blue THEN green = red
                                IF blue >= green AND blue >= red THEN green = blue
                                red = 0: blue = 0
                               
                                FOR myi = 1 TO 254
                                    OUT &H3C7, myi
                                    red2 = INP(&H3C9)
                                    green2 = INP(&H3C9)
                                    blue2 = INP(&H3C9)
                                    test = (red2 + blue2) + (green - green2) ^ 2
                                    IF test < 15 OR test < countr THEN thisC = myi: countr = test
                                NEXT
                          
                            END IF
                            PSET (i3, i4), thisC
                            IF i3 + 1 > mmx + size + 1 THEN PSET (mmx + 1, i4), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    countr = 0
                    mx = 0
                    CALL drawboxes
             
                END IF
          
            NEXT
        NEXT
    END IF
   
    IF mtool = 5 THEN
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                countr = 999
                IF cnt = chc THEN
                    FOR i3 = mmx + 1 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 1
                            thisC = POINT(i3, i4)
                            IF thisC > 0 THEN
                                OUT &H3C7, thisC
                                red = INP(&H3C9)
                                green = INP(&H3C9)
                                blue = INP(&H3C9)
                                IF red >= green AND red >= blue THEN blue = 0: green = 0
                                IF green >= red AND green >= blue THEN blue = 0: red = green: green = 0
                                IF blue >= green AND blue >= red THEN red = blue: green = 0: blue = 0
                              
                                FOR myi = 1 TO 254
                                    OUT &H3C7, myi
                                    red2 = INP(&H3C9)
                                    green2 = INP(&H3C9)
                                    blue2 = INP(&H3C9)
                                    test = (green2 + blue2) + (red - red2) ^ 2
                                    IF test < 15 OR test < countr THEN thisC = myi: countr = test
                                NEXT
                         
                            END IF
                            PSET (i3, i4), thisC
                            IF i3 + 1 > mmx + size + 1 THEN PSET (mmx + 1, i4), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    countr = 0
                    mx = 0
                    CALL drawboxes
            
                END IF
         
            NEXT
        NEXT
    END IF
   
    IF mtool = 4 THEN
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                countr = 999
                IF cnt = chc THEN
                    FOR i3 = mmx + 1 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 1
                            thisC = POINT(i3, i4)
                            IF thisC > 0 THEN
                                OUT &H3C7, thisC
                                red = INP(&H3C9)
                                green = INP(&H3C9)
                                blue = INP(&H3C9)
                                IF red >= green AND red >= blue THEN blue = red: green = red
                                IF green >= red AND green >= blue THEN blue = green: red = green
                                IF blue >= green AND blue >= red THEN red = blue: green = blue
                             
                                FOR myi = 1 TO 254
                                    OUT &H3C7, myi
                                    red2 = INP(&H3C9)
                                    green2 = INP(&H3C9)
                                    blue2 = INP(&H3C9)
                                    test = (red - red2) * (red - red2) + (green - green2) * (green - green2) + (blue - blue2) * (blue - blue2)
                                   
                                    'test = ABS(green2 - blue2) + ABS(green2 - red2) + ABS(red2 - blue2) + ABS(red2 - green2) + ABS(blue2 - red2) + ABS(blue2 - green2)
                                    IF test < 20 THEN thisC = myi: countr = test
                                NEXT
                        
                            END IF
                            PSET (i3, i4), thisC
                            IF i3 + 1 > mmx + size + 1 THEN PSET (mmx + 1, i4), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    countr = 0
                    mx = 0
                    CALL drawboxes
           
                END IF
        
            NEXT
        NEXT
    END IF
  
   

    IF mtool < 4 THEN
       cnt = 0
       FOR i = 0 TO setdown.amountX
            FOR i2 = 0 TO setdown.amountY
                mmx = setdown.startX2 + (i * setdown.changeX)
                mmy = setdown.startY2 + (i2 * setdown.changeY)
                cnt = cnt + 1
                IF cnt = chc THEN
                    FOR i3 = mmx + 1 TO mmx + size + 1
                        'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
                        FOR i4 = mmy + 2 TO mmy + size + 1
                            thisC = POINT(i3, i4)
                            IF thisC > 0 THEN
                                OUT &H3C7, thisC
                                red = INP(&H3C9)
                                green = INP(&H3C9)
                                blue = INP(&H3C9)
                                countr = 999
                                IF mtool = 1 THEN
                                    red = red / 2
                                    blue = blue / 2
                                    green = green / 2
                                END IF
                                IF mtool = 2 THEN
                                    red = red * 2
                                    blue = blue * 2
                                    green = green * 2
                                END IF
                                IF mtool = 3 THEN
                                    red = 63 - red
                                    green = 63 - green
                                    blue = 63 - blue
                                END IF
                                FOR myi = 1 TO 254
                                    OUT &H3C7, myi
                                    red2 = INP(&H3C9)
                                    green2 = INP(&H3C9)
                                    blue2 = INP(&H3C9)
                                    test = (red - red2) * (red - red2) + (green - green2) * (green - green2) + (blue - blue2) * (blue - blue2)
                                    IF test < countr + 1 THEN thisC = myi: countr = test
                               
                                NEXT
                               
                            END IF
                            PSET (i3, i4), thisC
                            IF i3 + 1 > mmx + size + 1 THEN PSET (mmx + 1, i4), thisC
                        NEXT
                    NEXT
                    LINE (mmx + 1, mmy + 1)-(mmx + size + 2, mmy + size + 2), 0, B
                    check = chc
                    countr = 0
                    mx = 0
                    CALL drawboxes
               
                END IF
            
            NEXT
        NEXT
        mtool = 1
    END IF

CALL getspics
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)

END SUB

SUB mypal
    red = 4
    blue = 4
    green = 4

FOR i = 16 TO 254
    OUT &H3C8, i
    OUT &H3C9, INT(red)
    OUT &H3C9, INT(green)
    OUT &H3C9, INT(blue)
    IF i < 30 THEN
        red = red + 4
    END IF
    IF i > 29 AND i < 45 THEN
        red = 4
        blue = blue + 4
    END IF
    IF i > 44 AND i < 60 THEN
        red = 4
        blue = 4
        green = green + 4
    END IF
    IF i = 60 THEN red = 4: green = 4: blue = 4
    IF i > 60 AND i < 75 THEN
        red = red + 4
        green = green + 4
        blue = blue + 4
    END IF
    IF i = 75 THEN red = 4: green = 4: blue = 4
    IF i > 75 AND i < 90 THEN
        red = 4
        green = green + 4
        blue = blue + 4
    END IF
    IF i = 90 THEN red = 4: green = 4: blue = 4
    IF i > 90 AND i < 105 THEN
        red = red + 4
        green = 4
        blue = blue + 4
    END IF
    IF i = 105 THEN red = 4: green = 4: blue = 4
    IF i > 105 AND i < 120 THEN
        red = red + 4
        green = green + 4
        blue = 4
    END IF
    IF i = 120 THEN red = 4: green = 4: blue = 4
    IF i > 120 AND i < 135 THEN
        red = red + 4
        green = green + 4
        blue = blue + 2
    END IF
    IF i = 135 THEN red = 4: green = 4: blue = 4
    IF i > 135 AND i < 150 THEN
        red = red + 4
        green = green + 2
        blue = blue + 4
    END IF
    IF i = 150 THEN red = 4: green = 4: blue = 4
    IF i > 150 AND i < 165 THEN
        red = red + 2
        green = green + 4
        blue = blue + 4
    END IF
    IF i = 165 THEN red = 10: green = 4: blue = 4
    IF i > 165 AND i < 180 THEN
        red = red + 3.8
        green = green + 2.1
        blue = blue + 1.9
    END IF
    IF i = 180 THEN red = 4: green = 4: blue = 4
    IF i > 180 AND i < 195 THEN
        red = red + 4
        green = green + 2.3
        blue = blue + 2
    END IF
    IF i = 195 THEN red = 4: green = 4: blue = 4
    IF i > 195 AND i < 210 THEN
        red = red + 3
        green = 4
        blue = 4
    END IF
    IF i = 210 THEN red = 4: green = 4: blue = 4
    IF i > 210 AND i < 225 THEN
        red = red + 1
        green = green + 2
        blue = blue + 1
    END IF
    IF i = 225 THEN red = 4: green = 4: blue = 4
    IF i > 225 AND i < 240 THEN
        red = red + 3
        green = green + 2
        blue = blue + 1
    END IF
    IF i = 240 THEN red = 4: green = 4: blue = 4
    IF i > 240 AND i < 255 THEN
        red = red + 1
        green = green + 2
        blue = blue + 3
    END IF

NEXT

END SUB

SUB new
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
menu2$ = menu$
  
LINE (81, 50)-(195, 101), 15, B
LINE (82, 51)-(194, 100), 0, BF
CALL locatef(87, 55)
printf ("CLEAR PICTURE")
LINE (81, 50)-(195, 65), 15, B

CALL locatef(95, 80)
LINE (93, 78)-(111, 88), 15, B
printf ("OK")
CALL locatef(130, 80)
LINE (128, 78)-(178, 88), 15, B
printf ("CANCEL")

'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse
menu2$ = menu$

DO UNTIL tme$ <> ""
  
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx

    IF mb = 1 THEN
        IF mx > 93 AND mx < 111 THEN
            IF my > 78 AND my < 88 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                menu2$ = menu$
                LINE (93, 78)-(111, 88), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
               
                CLS
                CALL getspics
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs) 'to show mouse
                tme$ = "done"
               
                tme$ = "done"
            END IF
        END IF
        IF mx > 128 AND mx < 178 THEN
            IF my > 78 AND my < 88 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                menu2$ = menu$
              
                LINE (128, 78)-(178, 88), 15, BF
              
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs) 'to show mouse
                tme$ = "done"
            END IF
        END IF
  
    END IF
LOOP

    
END SUB

SUB over
SYSTEM

END SUB

SUB printf (words$)

FOR i = 0 TO LEN(words$) - 1
    fat2$ = RIGHT$(LEFT$(words$, i + 1), 1)
    SELECT CASE fat2$
        CASE "A": MyFontSet = 0
        CASE "B": MyFontSet = 1
        CASE "C": MyFontSet = 2
        CASE "D": MyFontSet = 3
        CASE "E": MyFontSet = 4
        CASE "F": MyFontSet = 5
        CASE "G": MyFontSet = 6
        CASE "H": MyFontSet = 7
        CASE "I": MyFontSet = 8
        CASE "J": MyFontSet = 9
        CASE "K": MyFontSet = 10
        CASE "L": MyFontSet = 11
        CASE "M": MyFontSet = 12
        CASE "N": MyFontSet = 13
        CASE "O": MyFontSet = 14
        CASE "P": MyFontSet = 15
        CASE "Q": MyFontSet = 16
        CASE "R": MyFontSet = 17
        CASE "S": MyFontSet = 18
        CASE "T": MyFontSet = 19
        CASE "U": MyFontSet = 20
        CASE "V": MyFontSet = 21
        CASE "W": MyFontSet = 22
        CASE "X": MyFontSet = 23
        CASE "Y": MyFontSet = 24
        CASE "Z": MyFontSet = 25
        CASE " ": MyFontSet = 26
        CASE "0": MyFontSet = 27
        CASE "1": MyFontSet = 28
        CASE "2": MyFontSet = 29
        CASE "3": MyFontSet = 30
        CASE "4": MyFontSet = 31
        CASE "5": MyFontSet = 32
        CASE "6": MyFontSet = 33
        CASE "7": MyFontSet = 34
        CASE "8": MyFontSet = 35
        CASE "9": MyFontSet = 36
    END SELECT
    PUT (LocPrintX%, LocPrintY%), mfont(0, MyFontSet), AND
    PUT (LocPrintX%, LocPrintY%), font(0, MyFontSet), OR
    LocPrintX% = LocPrintX% + 8
    IF LocPrintX% > 312 THEN
        LocPrintX% = 0: LocPrintY% = LocPrintY% + 8
    END IF
    IF LocPrintY% > 192 THEN LocPrintY% = 0

NEXT

END SUB

SUB putspics
    cnt = 0
        
    'scan boxes
    FOR i = 0 TO setdown.amountX
        FOR i2 = 0 TO setdown.amountY
            mmx = setdown.startX2 + (i * setdown.changeX)
            mmy = setdown.startY2 + (i2 * setdown.changeY)
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                    
            'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
            SELECT CASE size
                CASE 32:
                    PUT (mmx + 2, mmy + 2), pics32(0, cnt)
                CASE 16:
                    PUT (mmx + 2, mmy + 2), pics16(0, cnt)
                CASE 8
                    PUT (mmx + 2, mmy + 2), pics8(0, cnt)
            END SELECT
            cnt = cnt + 1
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
                
        NEXT
    NEXT

END SUB

SUB putspics2
    cnt = 0
       
    'scan boxes
    FOR i = 0 TO setdown.amountX
        FOR i2 = 0 TO setdown.amountY
            mmx = setdown.startX2 + (i * setdown.changeX)
            mmy = setdown.startY2 + (i2 * setdown.changeY)
            inregs.ax = 2
            CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                   
            'LINE (mmx + 2, mmy + 2)-(mmx + size + 1, mmy + size + 1), 5, BF
            cnt = cnt + 1
           
            IF cnt = chc AND undo(1, 0) = size THEN
            SELECT CASE size
                CASE 32:
                    PUT (mmx + 2, mmy + 2), undo(0, 0), PSET
                CASE 16:
                    PUT (mmx + 2, mmy + 2), undo(0, 0), PSET
                CASE 8
                    PUT (mmx + 2, mmy + 2), undo(0, 0), PSET
            END SELECT
            END IF
            inregs.ax = 1
            CALL interrupt(&H33, inregs, outregs) 'to show mouse
               
        NEXT
    NEXT

END SUB

SUB quit
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
menu2$ = menu$
   
LINE (81, 50)-(195, 101), 15, B
LINE (82, 51)-(194, 100), 0, BF
CALL locatef(90, 55)
printf ("QUIT PROGRAM")
LINE (81, 50)-(195, 65), 15, B

CALL locatef(95, 80)
LINE (93, 78)-(111, 88), 15, B
printf ("OK")
CALL locatef(130, 80)
LINE (128, 78)-(178, 88), 15, B
printf ("CANCEL")

'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs) 'to show mouse
menu2$ = menu$

DO UNTIL tme$ <> ""
   
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx
 
    IF mb = 1 THEN
        IF mx > 93 AND mx < 111 THEN
            IF my > 78 AND my < 88 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                menu2$ = menu$
               
                LINE (93, 78)-(111, 88), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
                CALL over
            END IF
        END IF
        IF mx > 128 AND mx < 178 THEN
            IF my > 78 AND my < 88 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs) 'to hide mouse
                menu2$ = menu$
               
                LINE (128, 78)-(178, 88), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
               
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs) 'to show mouse
                tme$ = "done"
            END IF
        END IF
   
    END IF
LOOP

END SUB

SUB readpal

FOR i = 0 TO 255
    OUT &H3C7, i
    pal(i).red = INP(&H3C9)
    pal(i).green = INP(&H3C9)
    pal(i).blue = INP(&H3C9)
NEXT i

END SUB

SUB save
filename$ = ""
filename2$ = ""

'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
menu2$ = menu$
filename2$ = "NOTREADYYET"
LINE (90, 50)-(275, 150), 0, BF
LINE (90, 50)-(275, 150), 15, B
LINE (90, 65)-(275, 65), 15

CALL locatef(135, 55)
printf ("SAVE PICTURE")
CALL locatef(100, 72)
printf ("SAVE AS")
CALL locatef(115, 87)
printf ("SAVE FRAME")
CALL locatef(115, 99)
printf ("SAVE ALL FRAMES")
CALL locatef(115, 111)
printf ("SAVE FILE")
CALL locatef(115, 123)
printf ("SAVE MASK")

CALL locatef(130, 138)
printf ("OK")
CALL locatef(190, 138)
printf ("CANCEL")

LINE (128, 136)-(146, 146), , B
LINE (188, 136)-(238, 146), , B

CIRCLE (100, 90), 4
CIRCLE (100, 102), 4
CIRCLE (100, 114), 4
LINE (96, 123)-(104, 130), 15, B
LINE (160, 70)-(270, 80), 8, BF

sel = 3
CIRCLE (100, 90), 1, 0
PSET (100, 90), 0
CIRCLE (100, 102), 1, 0
PSET (100, 102), 0
CIRCLE (100, 114), 1, 15
PSET (100, 114), 15

'show mouse
inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)
'END
DO UNTIL tme$ <> ""
    IF filename$ = filename2$ OR sel2 <> sel THEN
        sel2 = sel
        IF sel = 3 THEN filename2$ = "pcb"
        IF sel < 3 THEN filename2$ = "pcf"
        inregs.ax = 2
        CALL interrupt(&H33, inregs, outregs)
       
        LINE (160, 70)-(270, 80), 0, BF
        LINE (160, 70)-(270, 80), 15, B
       
        CALL locatef(162, 72)
        printf (UCASE$(filename$))
        PSET (LocPrintX% + 2, LocPrintY% + 6)
        PSET (LocPrintX% + 3, LocPrintY% + 6)
        PSET (LocPrintX% + 2, LocPrintY% + 5)
        PSET (LocPrintX% + 3, LocPrintY% + 5)
        printf (" ")
        printf (UCASE$(filename2$))
        
        inregs.ax = 1
        CALL interrupt(&H33, inregs, outregs)
   
    END IF

    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = (outregs.cx / 2)
    my = outregs.dx
    mb = outregs.bx

    IF mb = 1 THEN
       
        IF mx > 90 AND mx < 257 THEN
            '(160, 70)-(270, 80)
            IF my > 70 AND my <= 80 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
               
                LINE (160, 70)-(270, 80), 0, BF
                LINE (160, 70)-(270, 80), 15, B
                LINE (160, 70)-(270, 80), 0, BF
                LINE (160, 70)-(270, 80), 15, B

                CALL locatef(162, 72)
               
                filename$ = inputf$
                filename2$ = filename$
                IF filename$ = "" THEN LINE (160, 70)-(270, 80), 8, BF
                
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)

            END IF

            IF my > 118 AND my <= 131 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
               
                IF slc = 0 THEN slc = 1 ELSE slc = 0
                IF slc = 1 THEN
                    LINE (96, 123)-(104, 130), 15, BF
                END IF
                IF slc = 0 THEN
                    LINE (96, 123)-(104, 130), 0, BF
                    LINE (96, 123)-(104, 130), 15, B
                END IF
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
               
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .9: LOOP
            END IF

            IF my > 108 AND my <= 118 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                
                CIRCLE (100, 90), 1, 0
                PSET (100, 90), 0
                CIRCLE (100, 102), 1, 0
                PSET (100, 102), 0
                CIRCLE (100, 114), 1, 15
                PSET (100, 114), 15
             
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
                sel = 3
            END IF
           
            IF my > 95 AND my <= 108 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
              
                CIRCLE (100, 90), 1, 0
                PSET (100, 90), 0
                CIRCLE (100, 102), 1, 15
                PSET (100, 102), 15
                CIRCLE (100, 114), 1, 0
                PSET (100, 114), 0
              
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
                sel = 2
            END IF
           
            IF my > 85 AND my <= 95 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
               
                CIRCLE (100, 90), 1, 15
                PSET (100, 90), 15
                CIRCLE (100, 102), 1, 0
                PSET (100, 102), 0
                CIRCLE (100, 114), 1, 0
                PSET (100, 114), 0
               
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
                sel = 1
            END IF

        END IF
        IF my > 136 AND my < 146 THEN
            'cancel
            IF mx > 188 AND mx < 238 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
               
                LINE (188, 136)-(238, 146), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
                tme$ = "done"

                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
           
            END IF
           
            'ok
            IF mx > 128 AND mx < 146 THEN
                'hide mouse
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
              
                LINE (128, 136)-(146, 146), 15, BF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
               
                tme$ = "done"
                
                IF filename$ <> "" THEN
                    
                    CALL save2
                    CALL save3
                    CALL save4
                END IF
              
                'show mouse
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
          
            END IF
       
        END IF
    END IF

LOOP

END SUB

SUB save2
                   
                    IF sel = 1 THEN
                        filename$ = filename$ + "." + filename2$
                        'bsave(filename$,pics8),size * size
                      
                        IF size = 32 THEN
                            DEF SEG = VARSEG(pics32(0, chc - 1))
                            BSAVE filename$, VARPTR(pics32(0, chc - 1)), (size * size) + size
                            DEF SEG
                            IF slc = 1 THEN
                                CLS
                                PUT (10, 10), pics32(0, chc - 1)
                                FOR i = 9 TO 80
                                    FOR i2 = 9 TO 80
                                        IF POINT(i, i2) = 0 THEN PSET (i, i2), 255 ELSE PSET (i, i2), 0
                                    NEXT
                                NEXT
                                GET (10, 10)-(42, 42), back(0)
                                DEF SEG = VARSEG(back(0))
                                BSAVE "m" + filename$, VARPTR(back(0)), (size * size) + size
                                DEF SEG
                        
                            END IF
                    
                        END IF
                      
                        IF size = 16 THEN
                            DEF SEG = VARSEG(pics16(0, chc - 1))
                            BSAVE filename$, VARPTR(pics16(0, chc - 1)), (size * size) + size
                            DEF SEG
                            IF slc = 1 THEN
                                CLS
                                PUT (10, 10), pics16(0, chc - 1)
                                FOR i = 9 TO 80
                                    FOR i2 = 9 TO 80
                                        IF POINT(i, i2) = 0 THEN PSET (i, i2), 255 ELSE PSET (i, i2), 0
                                    NEXT
                                NEXT
                                GET (10, 10)-(26, 26), back(0)
                                DEF SEG = VARSEG(back(0))
                                BSAVE "m" + filename$, VARPTR(back(0)), (size * size) + size
                                DEF SEG
                         
                            END IF
                     
                        END IF
                      
                        IF size = 8 THEN
                            DEF SEG = VARSEG(pics8(0, chc - 1))
                            BSAVE filename$, VARPTR(pics8(0, chc - 1)), (size * size) + size
                            DEF SEG
                            IF slc = 1 THEN
                                CLS
                                PUT (10, 10), pics8(0, chc - 1)
                                FOR i = 9 TO 80
                                    FOR i2 = 9 TO 80
                                        IF POINT(i, i2) = 0 THEN PSET (i, i2), 255 ELSE PSET (i, i2), 0
                                    NEXT
                                NEXT
                                GET (10, 10)-(18, 18), back(0)
                                DEF SEG = VARSEG(back(0))
                                BSAVE "m" + filename$, VARPTR(back(0)), (size * size) + size
                                DEF SEG
                          
                            END IF
                      
                        END IF

                    END IF

END SUB

SUB save3
IF sel = 2 THEN
    CLS
    cnt = 0
    FOR i = 0 TO setdown.amountX * (size + 1) STEP size + 1
        FOR i2 = 0 TO setdown.amountY * (size + 1) STEP size + 1
                               
            IF size = 8 THEN mane = ((pics8(1, cnt) ^ 2) / 2) + 2
            IF size = 16 THEN mane = ((pics16(1, cnt) ^ 2) / 2) + 2
            IF size = 32 THEN mane = ((pics32(1, cnt) ^ 2) / 2) + 2
            FOR e = 2 TO mane
                IF size = 8 THEN IF pics8(e, cnt) <> 0 THEN eb = eb + e
                IF size = 16 THEN IF pics16(e, cnt) <> 0 THEN eb = eb + e
                IF size = 32 THEN IF pics32(e, cnt) <> 0 THEN eb = eb + e
            NEXT
                           
            IF eb > 1 THEN
                IF size = 8 THEN PUT (mi, mi2), pics8(0, cnt), PSET
                IF size = 16 THEN PUT (mi, mi2), pics16(0, cnt), PSET
                IF size = 32 THEN PUT (mi, mi2), pics32(0, cnt), PSET
                mi = mi + size + 1
                IF mi > 200 THEN mi2 = mi2 + size + 1: mi = 0
                eb = 0
            END IF
            cnt = cnt + 1
        NEXT
    NEXT
    cnt = 0
    FOR i2 = 0 TO 200 STEP size + 1
        FOR i = 0 TO 200 STEP size + 1
            fx = 999: sx = 0
            fy = 999: sy = 0
            FOR e = 0 TO size
                FOR e2 = 0 TO size
                    fc = POINT(i + e, i2 + e2)
                    IF fc > 0 THEN
                        IF fx > e + i THEN fx = e + i
                        IF sx < e + i THEN sx = e + i
                        IF fy > e2 + i2 THEN fy = e2 + i2
                        IF sy < e2 + i2 THEN sy = e2 + i2
                    END IF
                NEXT
            NEXT
            IF fx <> 999 THEN
                file$ = filename$ + LTRIM$(STR$(cnt)) + ".pcf"
                GET (fx, fy)-(sx, sy), back(0)
                DEF SEG = VARSEG(back(0))
                BSAVE file$, VARPTR(back(0)), (size * size) + 48
                DEF SEG
                IF slc = 1 THEN
                    FOR e = fx TO sx
                        FOR e2 = fy TO sy
                            IF POINT(e, e2) = 0 THEN PSET (e, e2), 255 ELSE PSET (e, e2), 0
                        NEXT
                    NEXT
                    GET (fx, fy)-(sx, sy), back(0)
                    DEF SEG = VARSEG(back(0))
                    BSAVE "m" + file$, VARPTR(back(0)), (size * size) + 48
                    DEF SEG
                END IF
                cnt = cnt + 1
            END IF
        NEXT
    NEXT
END IF
END SUB

SUB save4

IF sel = 3 THEN
    CLS
    cnt = 0
    file$ = filename$ + ".pcb"
   
    OPEN file$ FOR OUTPUT AS #1
   
        WRITE #1, "***pcb***"
        WRITE #1, size
        FOR i = 0 TO 255
            OUT &H3C7, i
            PRINT #1, INP(&H3C9);
            PRINT #1, INP(&H3C9);
            PRINT #1, INP(&H3C9);
        NEXT i
       
        IF size = 8 THEN
            FOR i = 0 TO 33
                FOR i2 = 0 TO 99
                    PRINT #1, pics8(i, i2);
                NEXT
            NEXT
        END IF
       
        IF size = 16 THEN
            FOR i = 0 TO 129
                FOR i2 = 0 TO 42
                    PRINT #1, pics16(i, i2);
                NEXT
            NEXT
        END IF
       
        IF size = 32 THEN
            FOR i = 0 TO 513
                FOR i2 = 0 TO 12
                    PRINT #1, pics32(i, i2);
                NEXT
            NEXT
        END IF
       
        CLOSE #1
END IF

END SUB

SUB savep

menu2$ = menu$
LINE (80, 20)-(220, 120), 0, BF
LINE (80, 20)-(220, 120), 15, B
LOCATE 4, 12
PRINT "SAVE PALETTE AS:"
LOCATE 6, 12
INPUT filename$
IF INSTR(filename$, ".") = 0 THEN filename$ = filename$ + ".pal"

IF LEN(filename$) <= 4 OR LEN(filename$) > 12 THEN filename$ = "YourPal.pal"
LOCATE 8, 12
PRINT "FILE SAVED AS:";
LOCATE 10, 12
PRINT filename$

FOR i = 0 TO 255
    OUT &H3C7, i
    pal(i).red = INP(&H3C9)
    pal(i).green = INP(&H3C9)
    pal(i).blue = INP(&H3C9)
NEXT i

DEF SEG = VARSEG(pal(0))
BSAVE filename$, VARPTR(pal(0)), 1563
DEF SEG

DO UNTIL INKEY$ <> "": LOOP

END SUB

SUB tools
'hide mouse
inregs.ax = 2
CALL interrupt(&H33, inregs, outregs) 'to hide mouse
menu2$ = menu$
  
LINE (45, 25)-(265, 165), 0, BF
LINE (45, 25)-(265, 165), 15, B

LINE (170, 42)-(170, 165)
LINE (180, 42)-(180, 165)

LINE (170, 42)-(180, 42)
LINE (170, 52)-(180, 52)
LINE (170, 155)-(180, 155)

LINE (45, 42)-(265, 42)

LINE (185, 133)-(260, 145), 15, B
LINE (185, 149)-(260, 161), 15, B

LOCATE 18, 28
PRINT "OK"

LOCATE 20, 26
PRINT "CANCEL"

LOCATE 5, 8
PRINT "TOOLS: "

FOR i = 0 TO 499
    fst$(i) = ""
NEXT

fst$(0) = "DARKEN"
fst$(1) = "BRIGHTEN"
fst$(2) = "INVERSE"
fst$(3) = "GREYSCALE"
fst$(4) = "REDSCALE"
fst$(5) = "GREENSCALE"
fst$(6) = "BLUESCALE"

gi = 0
FOR i = gi TO gi + 13
    LOCATE (i + 7) - gi, 8
    PRINT fst$(i)
NEXT

inregs.ax = 1
CALL interrupt(&H33, inregs, outregs)
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

DO UNTIL tme$ <> ""
    inregs.ax = 3
    CALL interrupt(&H33, inregs, outregs)
    mx = outregs.cx \ 2
    my = outregs.dx
    mb = outregs.bx
  
    IF mb = 1 THEN
     
        IF mx > 45 AND mx < 170 THEN
            IF my > 42 AND my < 265 THEN
                FOR i = 0 TO 13
                    IF (my - 48) \ 8 = i THEN filename$ = fst$(i + gi)
                NEXT
                IF filename$ <> filename2$ THEN
                    LINE (100, 30)-(245, 40), 0, BF
                    LINE (190, 50)-(254, 114), 0, BF
                END IF
                LOCATE 5, 14
                PRINT filename$;
                filename2$ = filename$
            END IF
        END IF
     
        IF mx > 170 AND mx < 180 THEN
            IF my > 52 AND my < 101 THEN
                IF gi > 0 THEN gi = gi - 7
                IF gi < 0 THEN gi = 0
            END IF
            IF my > 100 AND my < 155 THEN
                IF gi < 1480 THEN gi = gi + 7
                IF gi > 1480 THEN gi = 1480
            END IF
         
            IF my < 165 AND my > 155 AND gi < 1499 THEN
                gi = gi + 1
            END IF
         
            IF my < 52 AND my > 42 AND gi > 0 THEN
                gi = gi - 1
            END IF
         
            LINE (46, 46)-(169, 164), 0, BF
            FOR i = gi TO gi + 13
                LOCATE (i + 7) - gi, 8
                PRINT fst$(i)
            NEXT
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP
        END IF
        IF mx > 185 AND mx < 260 THEN
            IF my > 133 AND my < 145 THEN
                tme$ = "OK"
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (185, 133)-(260, 145), 7, BF
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
            END IF
            IF my > 149 AND my < 161 THEN
                tme$ = "DONE"
                inregs.ax = 2
                CALL interrupt(&H33, inregs, outregs)
                LINE (185, 149)-(260, 161), 7, BF
                inregs.ax = 1
                CALL interrupt(&H33, inregs, outregs)
            END IF
     
        END IF
    END IF

LOOP

IF tme$ = "OK" THEN
      
IF filename$ = "BRIGHTEN" THEN mtool = 2
IF filename$ = "DARKEN" THEN mtool = 1
IF filename$ = "INVERSE" THEN mtool = 3
IF filename$ = "GREYSCALE" THEN mtool = 4
IF filename$ = "REDSCALE" THEN mtool = 5
IF filename$ = "BLUESCALE" THEN mtool = 7
IF filename$ = "GREENSCALE" THEN mtool = 6

IF filename$ <> "" THEN CALL mtools
    
END IF
                tmr50! = TIMER
                DO WHILE TIMER < tmr50! + .3: LOOP

END SUB

SUB viewscrn
  
    LINE (300, 0)-(319, 13), 15, B
    LINE (31, 144)-(319, 199), , B
    LINE (0, 0)-(160, 15), , B
    LINE (31, 15)-(160, 144), , B
    LINE (301, 1)-(318, 12), c2, BF   'shows selected color 2
    LINE (281, 1)-(299, 12), c, BF 'shows selected color
    LINE (280, 0)-(300, 13), 15, B
 
    ' draws menu
    LINE (4, 2)-(43, 13), 15, B
    LINE (47, 2)-(85, 13), 15, B
    LINE (89, 2)-(115, 13), 15, B
    LINE (119, 2)-(155, 13), 15, B

    ' draws boxes for48 x48
    IF size = 48 THEN
        FOR i = 0 TO 2
            chcC(i) = 15
            IF chc = i THEN chcC(i - 1) = 14
        NEXT
        cnt = 0
  
        FOR i = 0 TO 1
            LINE (163 + (i * 78), 35)-(230 + (i * 78), 102), chcC(cnt), B
            cnt = cnt + 1
        NEXT
    END IF
 
    ' draws boxes for 32 x 32
    IF size = 32 THEN
        FOR i = 0 TO 12
            chcC(i) = 15
            IF chc = i THEN chcC(i - 1) = 14
        NEXT
        cnt = 0
   
        FOR i = 0 TO 3
            FOR i2 = 0 TO 2
                LINE (163 + (i * 39), 15 + (i2 * 45))-(198 + (i * 39), 50 + (i2 * 45)), chcC(cnt), B
                cnt = cnt + 1
            NEXT
        NEXT
    END IF

    ' draws boxes for 16 x 16
    IF size = 16 THEN
        FOR i = 0 TO 42
            chcC(i) = 15
            IF chc = i THEN chcC(i - 1) = 14
        NEXT
        cnt = 0
    
        FOR i = 0 TO 6
            FOR i2 = 0 TO 5
                LINE (165 + (i * 22), 15 + (i2 * 21))-(184 + (i * 22), 34 + (i2 * 21)), chcC(cnt), B
                cnt = cnt + 1
            NEXT
        NEXT
    END IF
 
    'draws side boxes for 8 x 8
    IF size = 8 THEN
       FOR i = 0 TO 99
            chcC(i) = 15
            IF chc = i THEN chcC(i - 1) = 14
        NEXT
        cnt = 0
     
        FOR i = 0 TO 10
            FOR i2 = 0 TO 8
                LINE (165 + (i * 14), 15 + (i2 * 14))-(176 + (i * 14), 26 + (i2 * 14)), chcC(cnt), B
                cnt = cnt + 1
            NEXT
        NEXT
    END IF


END SUB

