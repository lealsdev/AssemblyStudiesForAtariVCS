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
        
HeroXPos        byte	    ; player 0 x-position
HeroYPos		byte	    ; player 0 y-position
EnemyXPos	    byte	    ; player 1 x-position
EnemyYPos	    byte	    ; player 1 y-position
HeroSpritePtr   word        ; pointer to player0 sprite lookup table
ColorHeroPtr    word        ; pointer to player0 color lookup table
EnemySpritePtr  word        ; pointer to player1 sprite lookup table
ColorEnemyPtr   word        ; pointer to player1 color lookup table
HeroAnimationOffset byte    ; player0 sprite frame offset for animation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare constans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HERO_HEIGHT     = 9 ; player0 sprite height
ENEMY_HEIGHT    = 9 ; player1 sprite height

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
    sta HeroXPos	; HeroXPos = 60

    lda #83
    sta EnemyYPos   ; EnemyYPos = 83

    lda #54
    sta EnemyXPos   ; EnemyXPos = 54

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
;; Calculations and tasks performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda HeroXPos
    ldy #0
    jsr SetObjectXPos       ; set player0 horizontal position

    lda EnemyXPos
    ldy #1
    jsr SetObjectXPos       ; set player1 horizontal position

    sta WSYNC
    sta HMOVE               ; apply the horizontal offsets previously set

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

    repeat 37
        sta WSYNC	;display 37 recommended lines of VBLANK
    repend
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 96 visible scanlines of our main game (2-line kernel)
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

    ldx #96	        ; x counts the number of remaining scanlines        
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
    lda #96
    sta EnemyYPos

EndPositionUpdate:


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
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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