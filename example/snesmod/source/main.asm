;***************************************************
; snesmod example
;***************************************************

; NOTE: PRESS A TO MAKE SOUND!
;
; ALSO NOTE THAT SOUND EFFECTS USE UP CHANNEL 8
; PLEASE ENSURE THAT THE MUSIC DOES NOT USE CHANNEL 8
; FOR IMPORTANT SECTIONS BECAUSE IT WILL GET OVERWRITTEN
;
; NOTE THAT THIS MUSIC USES THE HIHAT IN CHANNEL 8 SO
; IT IS HIGHLY NOTICABLE THAT IT IS BEING CUT OFF
; FOR SOUND EFFECTS

.include "snes.inc"
.include "snes_joypad.inc"
.include "snesmod.inc"
.include "soundbank.inc"

.global _nmi, main

;===============================================================
	.zeropage
;===============================================================

;...insert some zeropage variables

bgcolor:	.res 2

;===============================================================
	.code
;===============================================================

; HERE IS AN EXAMPLE SOUND TABLE
; THE SOUND TABLE DEFINES THE SOUNDS THAT WILL BE USED
; AS STREAMED SOUND EFFECTS

; THE SOUND EFFECT DATA MUST BE IN BRR FORMAT
; THIS CAN BE DONE WITH THE SNESBRR TOOL BY DMV47

;==============================================================================
SoundTable:
;==============================================================================
SND_TEST = 0
	.byte	4				; DEFAULT PITCH (1..6) (hz = PITCH*2000)
	.byte	8				; DEFAULT PANNING (0..15)
	.byte	15				; DEFAULT VOLUME (0..15)
	.word	(TEST66_DATA_END-TEST66_DATA)/9	; NUMBER OF BRR CHUNKS IN SAMPLE (BYTES/9)
	.word	.LOWORD(TEST66_DATA)		; ADDRESS OF BRR SAMPLE
	.byte	^TEST66_DATA			; ADDRESS BANK
;------------------------------------------------------------------------------

;------------------------------
; INCLUDE BRR DATA INTO PROGRAM
;------------------------------
TEST66_DATA:
.incbin "../sound/tada.brr" ;tada sound, converted with snesbrr.exe
TEST66_DATA_END:
	

	.a8
	.i16

;---------------------------------------------------------------
; program entry point
;===============================================================
main:
;===============================================================
	lda	#0Fh			; enable screen
	sta	REG_INIDISP		;
	
	
;---------------------------------------------------------------
	jsr	spcBoot			; BOOT SNESMOD
;---------------------------------------------------------------
	lda	#^__SOUNDBANK__		; give soundbank
	jsr	spcSetBank		; (the soundbank must 
					; have dedicated bank(s))
;---------------------------------------------------------------
	ldx	#MOD_POLLEN8		; load module into SPC
	jsr	spcLoad			;
;---------------------------------------------------------------
	lda	#39			; allocate around 10K of sound ram (39 256-byte blocks)
	jsr	spcAllocateSoundRegion	;
	; now the module size must be restricted to 10K less than the
	; maximum allowed
;---------------------------------------------------------------
	lda	#^SoundTable|80h	; set sound table address
	ldy	#.LOWORD(SoundTable)	;
	jsr	spcSetSoundTable	;
;---------------------------------------------------------------
	ldx	#0			; play module starting at position 0
	jsr	spcPlay			;
;---------------------------------------------------------------

	LDx	#150			; lower the music volume a bit (150/255)
	jsr	spcSetModuleVolume	;

	lda	#81h			; enable IRQ, joypad
	sta	REG_NMITIMEN
	
main_loop:

	lda	joy1_down		; on keypress A:
	bit	#JOYPAD_A		;
	beq	@nkeypress_a		;
					;
	spcPlaySoundM SND_TEST		; play sound using all default parameters
					;
@nkeypress_a:				;

	jsr	spcProcess		; update SPC

	wai
	
	rep	#20h			; increment bgcolor
	inc	bgcolor			;
	lda	bgcolor			;
	sep	#20h			;
	stz	REG_CGADD		;
	sta	REG_CGDATA		;
	xba				;
	sta	REG_CGDATA		;
	
	bra	main_loop
	
	
;---------------------------------------------------------------
; NMI irq handler
;===============================================================
_nmi:
;===============================================================
	rep	#30h			; push a,x,y
	pha				;
	phx				;
	phy				;-----------------------
	sep	#20h			; 8bit akku
					;
;---------------------------------------
;TODO: insert vblank code
;---------------------------------------
	jsr	joyRead
					;
	lda	REG_TIMEUP		; read from REG_TIMEUP (?)
					;
	rep	#30h			; pop a,x,y
	ply				;
	plx				;
	pla				;
	rti				; return
	
;===============================================================
	.segment "HDATA"
	.segment "HRAM"
	.segment "HRAM2"
	.segment "XCODE"
;===============================================================