    processor 6502
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include files with definitions and macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000

Start:
    CLEAN_START
    
    lda #$80
    sta COLUBK		; blue background color
    
    lda #$1E
    sta COLUPF		; yellow playfield color
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the recommended black scanlines (VSYNC and VBLANK)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VSYNC
    sta VBLANK

    ldx #0
    repeat 3
        stx WSYNC
    repend
    
    stx VSYNC
    
    repeat 37
    	stx WSYNC
    repend
    
    stx VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the CTRLPF register to allow playfield reflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
    ldx #%00000001
    stx CTRLPF			; CTRLPF register

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Skip 7 scanlines with no PF set
    ldx #0
    stx PF0
    stx PF1
    stx PF2    
    repeat 7
    	stx WSYNC
    repend
    
; Set the PF0 to 1100 (LSB first) and PF1-PF2 as 1111 1111    
    ldx #%11000000
    stx PF0
    
    ldx #%11111111
    stx PF1
    stx PF2
    
    repeat 7
    	stx WSYNC
    repend
    
; Set the next 164 lines only with
; - PF0 1100
; - PF1 0000
; - PF2 1000
    ldx #%11000000
    stx PF0
    
    ldx #0
    stx PF1    
    
    ldx #%10000000
    stx PF2
    
    repeat 164
    	stx WSYNC
    repend
    
; Set the PF0 to 1100 (LSB first) and PF1-PF2 as 1111 1111      
    ldx #%11000000
    stx PF0
    
    ldx #%11111111
    stx PF1
    
    ldx #%11111111
    stx PF2
    
    repeat 7
    	stx WSYNC
    repend
    
; Skip 7 vertical lines with no PF set
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    
    repeat 7
    	stx WSYNC
    repend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK overscan lines to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK    	
    repeat 30
    	stx WSYNC
    repend
    
    lda #0
    sta VBLANK
    
    
    jmp DrawFrame
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start