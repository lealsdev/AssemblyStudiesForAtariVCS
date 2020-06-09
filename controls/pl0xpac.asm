	processor 6502
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with register mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an uninitialized segment at $80 for var declaration.
;; We have memory from $80 to $FF to work with, minus a few at
;; the end if we use the stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg.u Variables
    org $80
P0XPos	 byte		; sprite X coordinate
P0Height byte		; sprite x height
P0Direction byte	; turn left or right

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code segment starting at $F000.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg code
    org $F000
        
Reset:
	CLEAN_START	; macro to clean memory and TIA
        
    ldx #$80
    stx COLUBK	; blue background color
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #10
    sta P0XPos	; initialize player X coordinate
    
    lda #14
    sta P0Height
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by configuring VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
	lda #2
    sta VSYNC	; turn VBLANK on
    sta VBLANK	; turn VSYNC on
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 3 vertical lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        sta WSYNC  	; first three VSYNC scanlines
    REPEND
    lda #0
    sta VSYNC      	; turn VSYNC off


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set player horizontal position while in VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda P0XPos	; load register A with desired X position
	and #$7F	; AND position with $7F to fix range
    sta WSYNC	; wait for next scanline
    sta HMCLR	; clear old horizontal position values
        
    sec		; set carry flag before subtraction
DivideLoop:
	sbc #15		; subtract 15 from the accumulator
    bcs DivideLoop	; loop while carry flag is still set

    eor #7		; adjust the remainder in A between -8 and 7
    asl		; shift left by 4, as HMP0 uses only 4 bits
    asl
    asl
    asl
    
    sta HMP0	; set fine position
    sta RESP0	; reset 15-step brute position
    sta WSYNC	; wait for next scanline
    sta HMOVE      	; apply the fine position offset
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the remaining 35 lines of VBLANK (37 - 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 35
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK     	; turn VBLANK off
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	REPEAT 130
        sta WSYNC	; wait for 160 empty scanlines
    REPEND
    
    ldy P0Height		; counter to draw 17 rows of player0 bitmap        
DrawBitmap:

	lda #%01000000
    cmp P0Direction
    bne SetRightDirection
    
    lda P0BitmapLeft,Y	; set player0 walking to left
    jmp ContinueDrawing
        
SetRightDirection:
    lda P0BitmapRight,Y	; set player0 walking to right
        
ContinueDrawing:
    sta GRP0	; set graphics for player 0 slice
    
    lda P0Color,Y	; load player color from lookup table
    sta COLUP0	; set color for player 0 slice
    
    sta WSYNC	; wait for next scanline
    
    dey
    bne DrawBitmap	; repeat next scanline until finished
    
    lda #0
    sta GRP0       ; disable P0 bitmap graphics
            
    REPEAT 45
        sta WSYNC  ; wait for remaining 15 empty scanlines
    REPEND        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Overscan:
    lda #2
    sta VBLANK     ; turn VBLANK on again for overscan
    REPEAT 30
        sta WSYNC
    REPEND
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joystick input test for P0 up/down/left/right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    
    dec P0XPos
    sta P0Direction		; saves the input to set the character direction

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput
    
    inc P0XPos
    sta P0Direction		; saves the input to set the character direction

NoInput:
    ; fallback when no input was performed


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player graphics bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0BitmapLeft:
    byte #%00000000
    byte #%00011000
    byte #%00111100
    byte #%01111110
    byte #%11111111
    byte #%00001111
    byte #%00001111
    byte #%11111111
    byte #%11111111
    byte #%11101111
    byte #%11101111
    byte #%01111110
    byte #%00111100
    byte #%00011000
        
P0BitmapRight:
    byte #%00000000
    byte #%00011000
    byte #%00111100
    byte #%01111110
    byte #%11111111
    byte #%11110000
    byte #%11110000
    byte #%11111111
    byte #%11111111
    byte #%11110111
    byte #%11110111
    byte #%01111110
    byte #%00111100
    byte #%00011000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    byte #$00
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C
    byte #$1C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset