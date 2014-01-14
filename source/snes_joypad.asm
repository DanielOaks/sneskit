;----------------------------------------------------
; joypad reading codes (for two joypads)
; by mukunda
;----------------------------------------------------

.include "snes.inc"

.globalzp joy1_held, joy1_down, joy2_prev, joy2_down
.global joyRead

.zeropage

joy1_prev:	.res 2
joy1_held:	.res 2
joy1_down:	.res 2
joy2_prev:	.res 2
joy2_held:	.res 2
joy2_down:	.res 2

.code
.a8
.i16

;---------------------------------------------------------------------
.macro readjoy prev, held, down, reg
;---------------------------------------------------------------------
	lda	reg		; read joypad register
	bit	#0Fh		; catch non-joypad input
	beq	:+		; (bits 0-3 should be zero)
	lda	#0		;-------------------------------------
:	sta	held		; store 'held' state
	eor	prev		; compute 'down' state from bits that
	and	held		; have changed from 0 to 1
	sta	down		;
.endmacro			;
;---------------------------------------------------------------------

;*********************************************************************
; note: set bit0 of NMITIMEN before using
; this function
; note: call this function during vblank
;
; this function should not be called at the
; beginning of vblank because a few scanlines
; are required by the hardware to read the joypads
;
; (copy your oam buffer first or something)
;*********************************************************************
joyRead:
;*********************************************************************
	ldy	joy1_held	; copy joy states
	sty	joy1_prev	;
	ldy	joy2_held	;
	sty	joy2_prev	;-------------------------------------
:	lda	REG_SLHV	; wait until past lines 224,225
	lda	REG_OPHCT	;
	cmp	#224		;
	beq	:-		;
	cmp	#225		;
	beq	:-		;-------------------------------------
:	lda	REG_HVBJOY	; wait until joypads are ready
	lsr			;
	bcs	:-		;-------------------------------------
	rep	#20h		; read joypads
	;-------------------------------------------------------------
	readjoy	joy1_prev, joy1_held, joy1_down, REG_JOY1L
	readjoy	joy2_prev, joy2_held, joy2_down, REG_JOY2L
	;-------------------------------------------------------------
	sep	#20h		; return
	rts			;
;---------------------------------------------------------------------
