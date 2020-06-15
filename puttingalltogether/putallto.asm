	processor 6502
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and 
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg.u Variables
    org $80
        
HeroXPos            byte	; player 0 x-position
HeroYPos		    byte	; player 0 y-position
EnemyXPos	        byte	; player 1 x-position
EnemyYPos	        byte	; player 1 y-position
Score               byte    ; 2-digit score stored as BCD
Timer               byte    ; 2-digit timer stored as BCD
Temp                byte    ; auxiliary varible to store temp score values
OnesDigitOffset     word    ; lookup table offset for the score ones digit
TensDigitOffset     word    ; lookup table offset for the score tens digit
HeroSpritePtr       word    ; pointer to player0 sprite lookup table
ColorHeroPtr        word    ; pointer to player0 color lookup table
EnemySpritePtr      word    ; pointer to player1 sprite lookup table
ColorEnemyPtr       word    ; pointer to player1 color lookup table
HeroAnimationOffset byte    ; player0 sprite frame offset for animation
Random              byte    ; random number generated to set enemy position
ScoreSprite         byte    ; store the sprite bit pattern for the score
TimerSprite         byte    ; store the sprite bit pattern for the timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare constans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HERO_HEIGHT     = 9 ; player0 sprite height
ENEMY_HEIGHT    = 9 ; player1 sprite height
DIGITS_HEIGHT   = 5 ; Scoreboard digit height (#rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000
        
Reset:
	CLEAN_START	; call macro to reset memory address and registers     
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta HeroYPos	; HeroYPos = 10

    lda #60
    sta HeroXPos	; HeroXPos = 68

    lda #83
    sta EnemyYPos   ; EnemyYPos = 83

    lda #54
    sta EnemyXPos   ; EnemyXPos = 62

    lda #%11010100
    sta Random      ; Random = $D4

    lda #0
    sta Score       ; score = 0
    sta Timer       ; timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<Hero
    sta HeroSpritePtr       ; lo-byte pointer for hero sprite lookup table 
    lda #>Hero
    sta HeroSpritePtr+1     ; hi-byte pointer for hero sprite lookup table

    lda #<ColorHero
    sta ColorHeroPtr        ; lo-byte pointer for hero color lookup table 
    lda #>ColorHero
    sta ColorHeroPtr+1      ; hi-byte pointer for hero color lookup table

    lda #<Enemy
    sta EnemySpritePtr      ; lo-byte pointer for enemy sprite lookup table 
    lda #>Enemy
    sta EnemySpritePtr+1    ; hi-byte pointer for enemy sprite lookup table

    lda #<ColorEnemy
    sta ColorEnemyPtr       ; lo-byte pointer for enemy color lookup table 
    lda #>ColorEnemy
    sta ColorEnemyPtr+1     ; hi-byte pointer for enemy color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VSYNC	    ; turn on VBLANK
    sta VBLANK	    ; turn on VSYNC

    repeat 3
        sta WSYNC	; display 3 recommended lines of VSYNC
    repend

    lda #0
    sta VSYNC	    ; turn off VSYNC

    repeat 33
        sta WSYNC	; display 33 recommended lines of VBLANK
    repend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda HeroXPos
    ldy #0
    jsr SetObjectXPos           ; set player0 horizontal position

    lda EnemyXPos
    ldy #1
    jsr SetObjectXPos           ; set player1 horizontal position

    jsr CalculateDigitOffset    ; calculate the scoreboard digit lookup table offset

    sta WSYNC
    sta HMOVE                   ; apply the horizontal offsets previously set


    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0              ; clear TIA registers before each new frame
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1            
    sta CTRLPF
    sta COLUBK          ; reset the TIA registers before displaying the score
    
    lda #$1E            ; set scoreboard color to yellow
    sta COLUPF              

    ldx #DIGITS_HEIGHT  ; start X counter with 5 (height of digits)

.ScoreDigitLoop:
    ; SCORE
    ldy TensDigitOffset     ; get the tens digit offset for the score
    lda Digits,Y            ; load the bit pattern from lookup table
    and #%11110000          ; mask/remove the graphics for the ones digit    
    sta ScoreSprite         ; save the score tens digit pattern in a variable

    ldy OnesDigitOffset     ; get the ones digit offset for the score
    lda Digits,Y            ; load the bit pattern from lookup table
    and #%00001111          ; mask/remove the graphics for the tens digit
    ora ScoreSprite         ; merge it with the saved tens digit sprite (or)
    sta ScoreSprite         ; and save it

    sta WSYNC               ; wait for the end of scanline
    sta PF1                 ; update the playfield to display the Score sprite

    ; TIMER
    ldy TensDigitOffset+1   ; get the left digit offset for the Timer
    lda Digits,Y            ; load the digit pattern from the lookup table
    and #%11110000          ; mask/remove the graphics for the tens digit
    sta TimerSprite         ; save the score tens digit pattern in a variable

    ldy OnesDigitOffset+1   ; get the ones digit offset for the Timer
    lda Digits,Y            ; load the digit pattern from lookup table
    and #%00001111          ; mask/remove the graphics for the tens digit
    ora TimerSprite         ; merge with the saved tens digit graphics
    sta TimerSprite         ; and save it

    
    jsr Sleep12Cycles       ; wastes some cycles

    sta PF1                 ; update the playfield for Timer display

    ldy ScoreSprite         ; preload for the next scanline
    sta WSYNC               ; wait for next scanline

    sty PF1                 ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1   ; increment all digits for the next line of data

    jsr Sleep12Cycles       ;waste some cycles

    dex                     ; X--
    sta PF1                   
    bne .ScoreDigitLoop     ; if dex != 0, then branch to ScoreDigitLooping

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining visible scanlines of our main game (2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLines:
    lda #$84
    sta COLUBK	    ; set color background to blue

    lda #$C2
    sta COLUPF	    ; set playfield/grass color to green

    lda #%11111111
    sta PF0

    lda #%11111000
    sta PF1

    lda 0
    sta PF2

    lda #%00000001
    sta CTRLPF

    ldx #85	                ; x counts the number of remaining scanlines        
.GameLineLoop:
.AreWeInsideHeroSprite:
    txa                     ; transfer X to A
    sec                     ; make sure the carry flag is set before subtraction
    sbc HeroYPos            ; subtract sprite Y-coordinate
    cmp HERO_HEIGHT         ; are we inside the sprite height bounds?
    bcc .DrawSpriteP0       ; if the result < SpriteHeight, call the draw routine
    lda #0                  ; else, set lookup index to zero

.DrawSpriteP0:
    clc                     ; clear carry flag before addition
    adc HeroAnimationOffset ; jump to the correct sprite frame address in memory

    tay                     ; load Y so we can work with the pointer
    lda (HeroSpritePtr),Y   ; load player0 bitmap data from lookup table
    sta WSYNC               ; wait for scanline
    sta GRP0                ; set graphics for player0
    lda (ColorHeroPtr),Y    ; load player color from lookup table
    sta COLUP0              ; set color of player 0

.AreWeInsideEnemySprite:
    txa                     ; transfer X to A
    sec                     ; make sure the carry flag is set before subtraction
    sbc EnemyYPos           ; subtract sprite Y-coordinate
    cmp ENEMY_HEIGHT        ; are we inside the sprite height bounds?
    bcc .DrawSpriteP1       ; if the result < SpriteHeight, call the draw routine
    lda #0                  ; else, set lookup index to zero

.DrawSpriteP1:
    tay                     ; load Y so we can work with the pointer

    lda #%00000101
    sta NUSIZ1              ;   stretch player1 sprite

    lda (EnemySpritePtr),Y  ; load player0 bitmap data from lookup table
    sta WSYNC               ; wait for scanline
    sta GRP1                ; set graphics for player0
    lda (ColorEnemyPtr),Y   ; load player color from lookup table
    sta COLUP1              ; set color of player 0


    dex		; X--
    bne .GameLineLoop	    ; repeat next main game scanline until finished


    lda #0
    sta HeroAnimationOffset ; reset jet animation frame to zero each frame
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK		; turn VBLANK on again
    repeat 30
        sta WSYNC	; display 30 recommended lines for VBLANK Overscan      
    repend

    lda #0
    sta VBLANK		; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000          ; player0 joystick up
    bit SWCHA
    bne CheckP0Down         ; if bit pattern doesnt match, bypass up block
    inc HeroYPos
    lda #0                  ; 0
    sta HeroAnimationOffset ; set animation offset to the second frame

CheckP0Down:
    lda #%00100000          ; player0 joystick down
    bit SWCHA
    bne CheckP0Left         ; if bit pattern doesnt match, bypass down block
    dec HeroYPos
    lda #0                  ; 0
    sta HeroAnimationOffset ; set animation offset to the second frame

CheckP0Left:
    lda #%01000000          ; player0 joystick left
    bit SWCHA
    bne CheckP0Right        ; if bit pattern doesnt match, bypass left block
    dec HeroXPos
    lda HERO_HEIGHT         ; 9
    sta HeroAnimationOffset ; set animation offset to the second frame

CheckP0Right:
    lda #%10000000          ; player0 joystick right
    bit SWCHA
    bne EndInputCheck       ; if bit pattern doesnt match, bypass right block
    inc HeroXPos
    lda HERO_HEIGHT
    sta HeroAnimationOffset ; set animation offset to the second frame

EndInputCheck:              ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update enemy position for the next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda EnemyYPos
    clc
    cmp #0                  ; compare enemy y-position with 0
    bmi .ResetEnemyPosition ; if it is < 0, then y-position to the top
    dec EnemyYPos           ; else, decrement enemy y-position for next frame
    jmp EndPositionUpdate

.ResetEnemyPosition:
    jsr GetRandomEnemyPosition  ; call subroutine for random x-position

EndPositionUpdate:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000          ; CXPPMM bit 7 detects P0 and P1 collision
    bit CXPPMM              ; check CXPPMM bit 7 with the above pattern
    bne .CollisionP0P1      ; collision between P0 and P1 happened
    jmp CheckCollisionP0PF  ; skip to the next check
.CollisionP0P1:
    jsr GameOver    ; call Game Over subroutine

CheckCollisionP0PF:
    lda #%10000000  ; CXP0FB bit 7 detects P0 and PF collision
    bit CXP0FB      ; check CXP0FB bit 7 with the above pattern
    bne .CollisionP0PF ; if collision P0 and PF happened
    jmp EndCollisionCheck
.CollisionP0PF:
    jsr GameOver    ; call Game Over subroutine

EndCollisionCheck:  ; fallback
    sta CXCLR       ; clear all collision flags before next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jmp StartFrame  ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target object x-coordinate position in pixels of our object
;; Y is the object type:
;;
;; 0: player0
;; 1: player1
;; 2: missile0
;; 3: missile1
;; 4: ball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC           ; start a fresh new scanline
    sec                 ; make sure carry-flag is set before subtraction
.Div15Loop
    sbc #15             ; subtract 15 from accumulator
    bcs .Div15Loop      ; loop until carry-flag is set
    eor #7              ; handle offset range from -8 to 7
    asl                 ; four shift lefts to get only the top 4 bits
    asl
    asl
    asl
    sta HMP0,Y          ; store the fine offset to the correct HMxx
    sta RESP0,Y         ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta COLUBK

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback shift register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number
;; Divide the random value by 4 to limit the size of the result to match river.
;; Add 30 to compensate for the left green playfield.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomEnemyPosition subroutine

    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random          ; performs a series of shifts and bit operations

    lsr
    lsr                 ; divide the value by 4 with 2 right shifts

    sta EnemyXPos       ; save it to the variable EnemyXPos

    lda #30
    adc EnemyXPos       ; adds 30 + EnemyXPos to compensate for left PF
    sta EnemyXPos       ; and sets the new value to the enemy x-position

    lda #96
    sta EnemyYPos

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of the variable Score and Timer into
;; the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiply by 5.
;;   - we can use left shifts to perform multiplication by 2.
;;   - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since its already times 16, we need to divide it
;; by 16 and then multiply by 5.
;;   - we can use right shifts to perform division by 2
;;   - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine

    ldx #1                  ; X register is the loop counter
.PrepareScoreLoop           ; this will loop twice, first X=1, and then X=0

    lda Score,X             ; load A with Timer (X=1) or Score (X=0)
    and #%00001111          ; remove the tens digit by masking 4 bits 00001111
    sta Temp                ; save the value of A into Temp

    asl                     ; shift left (it is now N*2)
    asl                     ; shift left (it is now N*4)
    adc Temp                ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X   ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score,X             ; load A with the timer (X=1) or Score (X=0)
    and #%11110000          ; remove ones digit by masking 4 bits 11110000
    lsr                     ; shift right (it is now N/2)
    lsr                     ; shift right (it is now N/4)
    sta Temp                ; save the value of A into Temp
    lsr                     ; shift right (it is now N/8)
    lsr                     ; shift right (it is now N/16)
    adc Temp                ; add the value saved in Temp (N/16+N/4)    
    sta TensDigitOffset,X   ; store A in TensDigitOffset+1 or TensDigitOffset

    dex                     ; X--
    bpl .PrepareScoreLoop   ; while X >= 0, loop to pass a second time

    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
Hero:
    .byte #%00000000;$40
    .byte #%00100010;$1C
    .byte #%01111111;$0A
    .byte #%00111110;$0E
    .byte #%00011100;$0E
    .byte #%00011100;$04
    .byte #%00001000;$B4
    .byte #%00001000;$0E
    .byte #%00001000;$06
HeroTurn:
    .byte #%00000000;$40
    .byte #%00001000;$1C
    .byte #%00111110;$0A
    .byte #%00011100;$0E
    .byte #%00011100;$0E
    .byte #%00011100;$04
    .byte #%00001000;$B4
    .byte #%00001000;$0E
    .byte #%00001000;$06
Enemy:
    .byte #%00000000;$40
    .byte #%00001000;$40
    .byte #%00001000;$40
    .byte #%00101010;$0E
    .byte #%00111110;$40
    .byte #%01111111;$40
    .byte #%00101010;$40
    .byte #%00001000;$40
    .byte #%00011100;$40
    
ColorHero:
    .byte #$00;
    .byte #$1C;
    .byte #$0A;
    .byte #$0E;
    .byte #$0E;
    .byte #$04;
    .byte #$B4;
    .byte #$0E;
    .byte #$06;
ColorHeroTurn:
    .byte #$00;
    .byte #$1C;
    .byte #$0A;
    .byte #$0E;
    .byte #$0E;
    .byte #$04;
    .byte #$B4;
    .byte #$0E;
    .byte #$06;
ColorEnemy:
    .byte #$00;
    .byte #$40;
    .byte #$40;
    .byte #$0E;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete my ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC	; move to position $FCCC
    word Reset	; write 2 bytes with the program reset address
    word Reset	; write 2 bytes with the interruption vector