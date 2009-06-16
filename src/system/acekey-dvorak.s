;ACE-128/64 Dvorak keymatrix, by Craig Bruce - 07-June-1995
;
;this file is in ACE-assembler format.

org $2000

;The mapping of keyscan matrix positions to keys is as follows (for the 
;QWERTY layout):
;
;    \                           COLUMNS:
;ROWS:\
;      \    0       1       2       3       4       5       6       7
;       +-------+-------+-------+-------+-------+-------+-------+-------+ code
;   0   | DELETE| RETURN| RIGHT |  F7   |  F1   |  F3   |  F5   | DOWN  | (0)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   1   |   3   |   W   |   A   |   4   |   Z   |   S   |   E   |L-SHIFT| (8)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   2   |   5   |   R   |   D   |   6   |   C   |   F   |   T   |   X   | (16)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   3   |   7   |   Y   |   G   |   8   |   B   |   H   |   U   |   V   | (24)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   4   |   9   |   I   |   J   |   0   |   M   |   K   |   O   |   N   | (32)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   5   |   +   |   P   |   L   |   -   |   .   |   :   |   @   |   ,   | (40)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   6   |   \   |   *   |   ;   | HOME  |R-SHIFT|   =   |   ^   |   /   | (48)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   7   |   1   |   _   |CONTROL|   2   | SPACE |COMMODR|   Q   | STOP  | (56)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   8   | HELP  |   8   |   5   |  TAB  |   2   |   4   |   7   |   1   | (64)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;   9   |  ESC  |   +   |   -   |  LF   | ENTER |   6   |   9   |   3   | (72)
;       +-------+-------+-------+-------+-------+-------+-------+-------+
;  10   |  ALT  |   0   |   .   |  UP   | DOWN  | LEFT  | RIGHT |NO-SCRL| (80)
;       +-------+-------+-------+-------+-------+-------+-------+-------+

;Note that the C128's keyboard has all eleven rows (0-10) whereas the C64's
;keyboard only has the first eight (0-7).  The entires with $00 mean that
;pressing the associated key does nothing.  There are multiple key matrices
;to produce the codes for use with various "shift" keys.

;The layout of the standard C128/64 QWERTY keyboard is as follows:
;
;    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;    | _ | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 | + | - | \ |hom|del|
;    +---------------------------------------------------------------+
;    |ctrl | Q | W | E | R | T | Y | U | I | O | P | @ | * | ^ |restr|
;    +---------------------------------------------------------------+
;    |stp|shl| A | S | D | F | G | H | J | K | L | : | ; | = | return|
;    +---------------------------------------------------------------+
;    | C=|shift| Z | X | C | V | B | N | M | , | . | / |shift| v |-> |
;    +---+-----+---+---+---+---+---+---+---+---+---+---+-----+---+---+
;
;This is translated into the following Dvorak layout in this keymatrix:
;
;    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;    | _ | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 | + | - | \ |hom|del|
;    +---------------------------------------------------------------+
;    |ctrl | @ | , | . | P | Y | F | G | C | R | L | / | * | ^ |restr|
;    +---------------------------------------------------------------+
;    |stp|shl| A | O | E | U | I | D | H | T | N | S | ; | = | return|
;    +---------------------------------------------------------------+
;    | C=|shift| : | Q | J | K | X | B | M | W | V | Z |shift| v |-> |
;    +---+-----+---+---+---+---+---+---+---+---+---+---+-----+---+---+
;
;A couple of keys are different from the standard Dvorak layout:
;
; the (2) key above should be (2) normally and (@) when shifted
; the (6) key above should be (6) normally and (^) when shifted
; the (8) key above should be (8) normally and (*) when shifted
; the (9) key above should be (9) normally and (() when shifted
; the (0) key above should be (0) normally and ()) when shifted
; the (+) key above should be ([) normally and ({) when shifted
; the (-) key above should be (=) normally and (+) when shifted
; the (@) key above should be (') normally and (") when shifted
; the (*) key above should be (]) normally and (}) when shifted
; the (;) key above should be (-) normally and (_) when shifted
; the (:) key above should be (;) normally and (:) when shifted
;
;In other words, the Dvorak layout should be:
;
;      ! @ # $ % ^ & * ( ) { +         Layout differs from the
;      1 2 3 4 5 6 7 8 9 0 [ =         ANSI standard because
;       " < > P Y F G C R L ? }        the following substitution
;       ' , . p y f g c r l / ]        was made:
;        A O E U I D H T N S _
;        a o e u i d h t n s -                 [  =>  {
;         : Q J K X B M W V Z                  ]  =>  [
;         ; q j k x b m w v z
;
;Here is the standard lawyershit that applies exclusively the keyboard
;diagram included immediately above:
;
;   Copyright 1991 by The MITRE Corporation.
;   Permission to use, copy, modify, and distribute this
;   software and its documentation for any purpose and without
;   fee is hereby granted, provided that the above copyright
;   notice appear in all copies.  The MITRE Corporation
;   makes no representations about the suitability of this
;   software for any purpose.  It is provided "as is" without
;   express or implied warranty.
;
;Anyhow, the key translation martix for the Dvorak keyboard becomes:
;
;    \                           COLUMNS:
;ROWS:\
;      \    0       1       2       3       4       5       6       7
;       +-------+-------+-------+-------+-------+-------+-------+-------+ code
;   0   | DELETE| RETURN| RIGHT |  F7   |  F1   |  F3   |  F5   | DOWN  | (0)
;   1   |   3   |   ,   |   A   |   4   |   :   |   O   |   .   |L-SHIFT| (8)
;   2   |   5   |   P   |   E   |   6   |   J   |   U   |   Y   |   Q   | (16)
;   3   |   7   |   F   |   I   |   8   |   X   |   D   |   G   |   K   | (24)
;   4   |   9   |   C   |   H   |   0   |   M   |   T   |   R   |   B   | (32)
;   5   |   +   |   L   |   N   |   -   |   V   |   S   |   /   |   W   | (40)
;   6   |   \   |   *   |   ;   | HOME  |R-SHIFT|   =   |   ^   |   Z   | (48)
;   7   |   1   |   _   |CONTROL|   2   | SPACE |COMMODR|   @   | STOP  | (56)
;   8   | HELP  |   8   |   5   |  TAB  |   2   |   4   |   7   |   1   | (64)
;   9   |  ESC  |   +   |   -   |  LF   | ENTER |   6   |   9   |   3   | (72)
;  10   |  ALT  |   0   |   .   |  UP   | DOWN  | LEFT  | RIGHT |NO-SCRL| (80)
;       +-------+-------+-------+-------+-------+-------+-------+-------+

keymapNormal = *
   db $14,$0d,$1d,$88,$85,$86,$87,$11  ;row 0
   db $33,$2c,$41,$34,$3a,$4f,$2e,$01  ;row 1
   db $35,$50,$45,$36,$4a,$55,$59,$51  ;row 2
   db $37,$46,$49,$38,$58,$44,$47,$4b  ;row 3
   db $39,$43,$48,$30,$4d,$54,$52,$42  ;row 4
   db $2b,$4c,$4e,$2d,$56,$53,$2f,$57  ;row 5
   db $5c,$2a,$3b,$13,$01,$3d,$5e,$5a  ;row 6
   db $31,$5f,$04,$32,$20,$02,$40,$03  ;row 7
   db $04,$38,$35,$09,$32,$34,$37,$31  ;row 8
   db $1b,$2b,$2d,$0a,$0d,$36,$39,$33  ;row 9
   db $08,$30,$2e,$91,$11,$9d,$1d,$00  ;row 10

keymapShift = *
   db $94,$8d,$9d,$8c,$89,$8a,$8b,$91  ;row 0
   db $23,$3c,$c1,$24,$5b,$cf,$3e,$01  ;row 1
   db $25,$d0,$c5,$26,$ca,$d5,$d9,$d1  ;row 2
   db $27,$c6,$c9,$28,$d8,$c4,$c7,$cb  ;row 3
   db $29,$c3,$c8,$30,$cd,$d4,$d2,$c2  ;row 4
   db $db,$cc,$ce,$dd,$d6,$d3,$3f,$d7  ;row 5
   db $dc,$c0,$5d,$93,$01,$3d,$de,$da  ;row 6
   db $21,$df,$04,$22,$20,$02,$40,$83  ;row 7
   db $84,$38,$35,$02,$32,$34,$37,$31  ;row 8
   db $0e,$2b,$2d,$07,$8d,$36,$39,$33  ;row 9
   db $08,$30,$2e,$16,$17,$06,$0b,$00  ;row 10

keymapCommodore = *
   db $08,$0d,$15,$8f,$80,$82,$8e,$0f  ;row 0
   db $96,$3c,$a1,$97,$bb,$af,$3e,$01  ;row 1
   db $98,$b0,$a5,$99,$aa,$b5,$b9,$b1  ;row 2
   db $9a,$a6,$a9,$9b,$b8,$a4,$a7,$ab  ;row 3
   db $29,$a3,$a8,$30,$ad,$b4,$b2,$a2  ;row 4
   db $2b,$ac,$ae,$2d,$b6,$b3,$3f,$b7  ;row 5
   db $bc,$2a,$bd,$93,$01,$3d,$be,$ba  ;row 6
   db $81,$bf,$04,$95,$5f,$02,$a0,$83  ;row 7
   db $84,$38,$35,$18,$32,$34,$37,$31  ;row 8
   db $1b,$2b,$2d,$07,$8d,$36,$39,$33  ;row 9
   db $08,$30,$2e,$0c,$0f,$10,$15,$00  ;row 10

keymapControl = *
   db $08,$01,$1a,$8f,$80,$82,$8e,$17  ;row 0
   db $1c,$2c,$e1,$9f,$fb,$ef,$2e,$01  ;row 1
   db $9c,$f0,$e5,$1e,$ea,$f5,$f9,$f1  ;row 2
   db $1f,$e6,$e9,$9e,$f8,$e4,$e7,$eb  ;row 3
   db $12,$e3,$e8,$92,$ed,$f4,$f2,$e2  ;row 4
   db $2b,$ec,$ee,$2d,$f6,$f3,$2f,$f7  ;row 5
   db $fc,$2a,$fd,$13,$01,$3d,$fe,$fa  ;row 6
   db $90,$ff,$04,$05,$20,$02,$e0,$03  ;row 7
   db $04,$8c,$87,$18,$89,$8a,$88,$85  ;row 8
   db $1b,$84,$8f,$0a,$00,$8b,$80,$86  ;row 9
   db $08,$82,$2e,$16,$17,$19,$1a,$00  ;row 10

keymapAlternate = *
   db $14,$0d,$1d,$88,$85,$86,$87,$11  ;row 0
   db $33,$2c,$61,$34,$7b,$6f,$2e,$01  ;row 1
   db $35,$70,$65,$36,$6a,$75,$79,$71  ;row 2
   db $37,$66,$69,$38,$78,$64,$67,$6b  ;row 3
   db $39,$63,$68,$30,$6d,$74,$72,$62  ;row 4
   db $2b,$6c,$6e,$2d,$76,$73,$2f,$77  ;row 5
   db $7c,$2a,$7d,$13,$01,$3d,$7e,$7a  ;row 6
   db $31,$7f,$04,$32,$20,$02,$60,$03  ;row 7
   db $04,$38,$35,$09,$32,$34,$37,$31  ;row 8
   db $1b,$2b,$2d,$0a,$0d,$36,$39,$33  ;row 9
   db $08,$30,$2e,$91,$11,$9d,$1d,$00  ;row 10

keymapCaps = *
   db $14,$0d,$1d,$88,$85,$86,$87,$11  ;row 0
   db $33,$2c,$c1,$34,$3a,$cf,$2e,$01  ;row 1
   db $35,$d0,$c5,$36,$ca,$d5,$d9,$d1  ;row 2
   db $37,$c6,$c9,$38,$d8,$c4,$c7,$cb  ;row 3
   db $39,$c3,$c8,$30,$cd,$d4,$d2,$c2  ;row 4
   db $2b,$cc,$ce,$2d,$d6,$d3,$2f,$d7  ;row 5
   db $5c,$2a,$3b,$13,$01,$3d,$5e,$da  ;row 6
   db $31,$5f,$04,$32,$20,$02,$40,$03  ;row 7
   db $04,$38,$35,$09,$32,$34,$37,$31  ;row 8
   db $1b,$2b,$2d,$0a,$0d,$36,$39,$33  ;row 9
   db $08,$30,$2e,$91,$11,$9d,$1d,$00  ;row 10

keymapShiftComm = *
   db $00,$00,$16,$00,$00,$00,$00,$17  ;row 0
   db $00,$00,$02,$00,$10,$00,$0b,$01  ;row 1
   db $00,$07,$1b,$00,$19,$00,$18,$00  ;row 2
   db $00,$00,$16,$00,$00,$0e,$00,$1a  ;row 3
   db $00,$00,$04,$00,$17,$09,$02,$06  ;row 4
   db $17,$0a,$0b,$16,$00,$15,$00,$0c  ;row 5
   db $00,$00,$15,$00,$01,$00,$00,$0f  ;row 6
   db $00,$1b,$04,$00,$00,$00,$00,$00  ;row 7
   db $04,$38,$35,$09,$32,$34,$37,$31  ;row 8
   db $1b,$2b,$2d,$0a,$0d,$36,$39,$33  ;row 9
   db $00,$30,$2e,$91,$11,$9d,$1d,$00  ;row 10
