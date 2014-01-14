;----------------------------------------------------------------------
; snes init code
; by neviksti/mukunda
;----------------------------------------------------------------------

.IMPORT __HDATA_LOAD__
.IMPORT __HDATA_RUN__
.IMPORT __HDATA_SIZE__


.include "snes.inc"

.global _start
.import main

.A8
.I16
.CODE

;----------------------------------------------------------------------
_start:
;----------------------------------------------------------------------
	sei				; disable interrupts
	clc				; switch to native mode
	xce				;
	rep	#38h			; mem/A/X/Y = 16bit
					; decimal mode off
	ldx	#1FFFh			; setup stack pointer
	txs				;
	lda	#0000h			; direct page = 0000h
	tcd				;
	sep	#20h			; 8bit A/mem
	lda	#80h			; data bank = 80h
	pha				;
	plb				;
	lda	$FFD5			; get map mode
	lsr				; 21/31 jump to bank C0
	bcs	:+			; 20/30 jump to bank 80
	jml	$800000+_histart	; (for switchable speed)
:	jml	$C00000+_histart	;
	
;----------------------------------------------------------------------
_histart:
;----------------------------------------------------------------------
	
	lda	$FFD5			; if map_mode & 10h
	bit	#10h			; switch to hi-speed mode
	beq	:+			;
	lda	#1			;
	sta	REG_MEMSEL		;
:					;

	lda	#8Fh			; enter forced blank
	sta	2100h			;

	ldx	#2101h			; regs $2101-$210C
:	stz	00h,x			; set Sprite,Character,Tile sizes to lowest, 
	inx				; and set addresses to $0000
	cpx	#210Dh			;
	bne	:-			;

:	stz	00h,x			; regs $210D-$2114
	stz	00h,x			; Set all BG scroll values to $0000
	inx				;
	cpx	#2115h			;
	bne	:-			;

	lda	#80h			; Initialize VRAM transfer mode to word-access, increment by 1
	sta	2115h			;

	stz	2116h			; VRAM address = $0000
	stz	2117h			;

	stz	211Ah			; clear Mode7 setting

	ldx	#211Bh			; regs $211B-$2120

:	stz	00h,x			; clear out the Mode7 matrix values
	stz	00h,x			;
	inx				;
	cpx	#2121h			;
	bne	:-			;

	ldx	#2123h			; regs $2123-$2133
:	stz	00h,x			; turn off windows, main screens, sub screens, color addition,
	inx				; fixed color = $00, no super-impose (external synchronization),
	cpx	#2134h			; no interlaced mode, normal resolution
	bne	:-			;

	stz	213Eh			; might not be necesary, but selects PPU master/slave mode
	stz	4200h			; disable timers, NMI,and auto-joyread
	lda	#0FFh
	sta	4201h			; programmable I/O write port, 
					; initalize to allow reading at in-port

	stz	420Bh			; turn off all general DMA channels
	stz	420Ch			; turn off all H-MA channels

	lda	4210h			; NMI status, reading resets

;----------------------------------------------------------------------
; clear VRAM
;----------------------------------------------------------------------
	lda	#80h		; set vram port to word access
	sta	2115h		;
	ldx	#1809h		; clear vram with dma
	stx	4300h		; dma mode: fixed source, WORD to $2118/9
	ldx	#0000h		;
	stx	2116h		; VRAM port address to $0000
	stx	0000h		; Set $00:0000 to $0000 (assumes scratchpad ram)
	stx	4302h		; Set source address to $xx:0000
	lda	#00h
	sta	4304h		; Set source bank to $00
	ldx	#0
	stx	4305h		; Set transfer size to 64k bytes
	lda	#01h
	sta	420Bh		; Initiate transfer

	stz	2119h		; clear the last byte of the VRAM
;----------------------------------------------------------------------
; clear palette
;----------------------------------------------------------------------
	stz	2121h
	ldx	#0100h
	
:	stz	$2122
	stz	$2122
	dex
	bne	:-
;----------------------------------------------------------------------

;------------------------------------------------------------------------------------------
; init OAM
;------------------------------------------------------------------------------------------

	stz	2102h			; sprites initialized to be off the screen, 
	stz	2103h			; palette 0, character 0
	ldx	#0080h
	lda	#0F0h

:	sta	2104h			; X = 240
	sta	2104h			; Y = 240
	stz	2104h			; character = $00
	stz	2104h			; set priority=0, no flips
	dex
	bne	:-

	ldx	#0020h
:	stz	2104h			; size bit=0, x MSB = 0
	dex
	bne	:-

;------------------------------------------------------------------------------------------
; erase WRAM
;------------------------------------------------------------------------------------------

	stz	2181h			; WRAM address = 0
	stz	2182h			;
	stz	2183h			;

	ldx	#8008h			; Set DMA mode to fixed source, BYTE to $2180
	stx	4300h 		        ; source = wram_fill_byte
	ldx	#wram_fill_byte		; transfer size = full 64k
	stx	4302h         		; 
	lda	#^wram_fill_byte	;
	sta	4304h  		        ;
	ldx	#0000h			;
	stx	4305h			;
	lda	#01h			;
	sta	420Bh			; start transfer (lower 64k)
	nop
	nop
	sta	420Bh			; transfer again (higher 64k)
	
;------------------------------------------------------------------------------------------
; load DATA segment
;------------------------------------------------------------------------------------------

	lda	#^__HDATA_RUN__&1	; copy to __HDATA_RUN__
	sta	REG_WMADDH		;
	ldx	#.LOWORD(__HDATA_RUN__)	;
	stx	REG_WMADDL		;

	ldx	#8000h			; dma increment source, copy to 2180
	stx	REG_DMAP0		;
	ldx	#.LOWORD(__HDATA_LOAD__); copy from __HDATA_LOAD__
	stx	REG_A1T0L		;
	lda	#^__HDATA_LOAD__	;
	sta	REG_A1B0		;
	ldx	#__HDATA_SIZE__		; skip if data segment is empty
	beq	_empty_data_segment	;
	stx	REG_DAS0L		;
	lda	#01h			;
	sta	REG_MDMAEN		;
_empty_data_segment:			;
	
	jmp	main

wram_fill_byte:
.byte	$00


