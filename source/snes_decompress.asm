;-------------------------------------------------------------
; decompression routines
; by mukunda
;-------------------------------------------------------------

.include "snes.inc"

.global DecompressDataVram
.global CopyPalette
.importzp memptr, m0, m1, m2, m3, m4, m5, m6

.code
.a8
.i16

;=========================================================================
.macro copy_vbyte
;=========================================================================
.scope
	rep	#21h			; a = m0 - y
	tya
	iny
	lsr
	sta	REG_VMADDL		; set vram address
	lda	m0
	inc	m0
	bcs	@readH			;
@readL:
	lsr
	tax
	sep	#20h			;
	lda	REG_VMDATALREAD		;
	stx	REG_VMADDL
	bcs	@writeH
@writeL:
	sta	REG_VMDATAL
	dex
	;bne	0
	bra	@copyexit

@readH:
:	lsr
	tax
	sep	#20h			;
	lda	REG_VMDATAHREAD		;
:
@copyexit:
.endscope
.endmacro
	
;=========================================================================
.macro store_vbyte
;=========================================================================
	bcs	:+			; write A to vram (carry = H/L)
	sta	REG_VMDATAL		;
	bra	:++			;
:	sta	REG_VMDATAH		;
:					;
.endmacro				;

;=========================================================================
.macro prep_store
;=========================================================================
	rep	#20h			; setup vram address (to target)
	lda	m0
	inc	m0
	lsr				;
	sta	REG_VMADDL		;
	sep	#20h			;
.endmacro				;

;=========================================================================
.macro prep_storef
;=========================================================================
	rep	#20h			; setup vram address (to target)
	lda	m0			;
	inc	m0			;
	lsr				;
	sta	REG_VMADDL		;
	sep	#20h			;
.endmacro				;

;-------------------------------------------------------------------------
; x = source address
; y = vram address
; a = bank number
;=========================================================================
DecompressDataVram:
;=========================================================================
	sta	memptr+2		; memptr = source address
	stx	memptr			;---------------------------------
	sty	m0			; m0 = vram target
					;---------------------------------
	ldx	m4			; preserve m4,m5,m6
	phx				;
	ldx	m5			;
	phx				;
	ldx	m6			;
	phx				;---------------------------------
					; setup VRAM access (increment on readlow)
	lda	#%00000000		;
	sta	REG_VMAIN		;
					;---------------------------------
	ldy	#0			; y = 0 (source index)
					;---------------------------------
	lda	[memptr]		; test compression type
	and	#0F0h			;
	cmp	#10h			; 1x = LZ77	
	beq	@LZ77source		;
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@ddv_exit:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	plx
	stx	m6
	plx
	stx	m5
	plx
	stx	m4
	rts

;=========================================================================
@LZ77source:
;=========================================================================

	iny				; x = byte 1,2 (data length)
	rep	#20h			;
	lda	[memptr], y		;
	tax				;
	sep	#20h			;
	
	iny
	iny
	iny

;------------------------------------------------------------------------	
@LZ77_DecompressLoop:
;------------------------------------------------------------------------
	lda	[memptr], y		; m5 = cflags
	iny				;
	sta	m5			;
	
	lda	#8			; m6 = bit counter
	sta	m6			;
	
@next_bit:
	asl	m5			; test bit
	bcs	@lz_byte		;
	
;----------------------------------------
@raw_byte:
;----------------------------------------
	prep_store
	lda	[memptr], y		; copy one byte
	iny
	
	store_vbyte
	
	dex
	beq	@ddv_exit; @next_block
	dec	m6
	bne	@next_bit
	bra	@LZ77_DecompressLoop

;---------------------------------------------------------------------
@lz_byte:
;---------------------------------------------------------------------
	rep	#20h			; read 2 bytes
	lda	[memptr], y		;
	iny				;
	iny				;
					;-----------------------------
	phy				; preserve y
					;-----------------------------
	sep	#20h			; y = target - disp - 1
	pha				;	
	and	#0Fh			;
	xba				;
	rep	#20h			;
	sec				;
	sbc	m0			;
	eor	#0FFFFh			;
	tay				;-----------------------------
	sep	#20h			; a = count (top 4 bits + 3)
	pla				;
	lsr				;
	lsr				;
	lsr				;
	lsr				;
	clc				;
	adc	#3			;-----------------------------
	sta	m4			; m4 = count (16bit)
	stz	m4+1			;-----------------------------
	rep	#20h			; m4 = count > x ? x : count
	cpx	m4			;
	bcs	:+			;
	stx	m4			;
	sec				;-----------------------------
:	txa				; push "x - count"
	sbc	m4			;
	pha				; 
	sep	#20h			;
	
;---------------------------------------------------------------------
@copyloop:
;---------------------------------------------------------------------

	rep	#21h			; copy ONE byte..........
	tya				;
	iny				;
	lsr				;
	sta	REG_VMADDL		;
	lda	m0			;
	inc	m0			;
	bcs	@copy_readH		;
@copy_readL:				;
	lsr				;
	tax				;
	sep	#20h			;
	lda	REG_VMDATALREAD		;
	stx	REG_VMADDL		;
	bcs	@copy_writeH		;
@copy_writeL:				;
	sta	REG_VMDATAL		;
	dec	m4			;
	bne	@copyloop		;
	bra	@copyexit		;
					;
@copy_readH:				;
	lsr				;
	tax				;
	sep	#20h			;
	lda	REG_VMDATAHREAD		;
	stx	REG_VMADDL		;
	bcc	@copy_writeL		;
@copy_writeH:				;
	sta	REG_VMDATAH		;
	
	dec	m4
	bne	@copyloop
@copyexit:
	
	plx
	ply
	cpx	#0

@next_block:
	
	beq	@ddv_exit2		; exit on end of data
	dec	m6
	beq	:+
	jmp	@next_bit
:	jmp	@LZ77_DecompressLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@ddv_exit2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	plx
	stx	m6
	plx
	stx	m5
	plx
	stx	m4
	rts
	
;****************************************************************
; Copy data from memory/rom to cgram
;
; A = source bank
; B = palette index
; Y = source address
; X = number of colours
;****************************************************************
	.global CopyPalette

CopyPalette:
	sta	memptr+2	; set bank#
	sty	memptr+0	; set source addr
	xba			;
	sta	REG_CGADD	; set cg address
	
	ldy	#0		;
:	lda	[memptr], y	; copy x colours
	iny			;
	sta	REG_CGDATA	;
	lda	[memptr], y	;
	iny			;
	sta	REG_CGDATA	;
				;
	dex			;
	bne	:-		;
	
	rts
