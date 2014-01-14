; graphic decompression functions

.ifndef __SNES_DECOMPRESS__
__SNES_DECOMPRESS__ = 1

;-------------------------------------------------------------------------
; x = source address
; y = vram address
; a = bank number
;-------------------------------------------------------------------------
.import DecompressDataVram

;****************************************************************
; Copy data from memory/rom to cgram
;
; A = source bank
; B = palette index
; Y = source address
; X = number of colours
;****************************************************************
.import CopyPalette

.macro DoCopyPalette source, index, colors
	lda	#index
	xba
	lda	#^source
	ldy	#.LOWORD(source)
	ldx	#colors
	jsr	CopyPalette
.endmacro

.macro DoDecompressDataVram source, dest
	ldx	#.LOWORD(source)
	ldy	#dest
	lda	#^source
	jsr	DecompressDataVram
.endmacro

.endif
