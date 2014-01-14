.include "snes.inc"

.export spcBoot
.export spcSetBank
.export spcLoad
.export spcTest
.export spcPlay
.export spcStop
.export spcReadStatus
.export spcReadPosition
.export spcGetCues

.export spcSetModuleVolume
.export spcFadeModuleVolume
.export spcLoadEffect
.export spcEffect

.export spcFlush
.export spcProcess

.export spcSetSoundTable
.export spcAllocateSoundRegion
.export spcPlaySound
.export spcPlaySoundV
.export spcPlaySoundEx

.import CART_HEADER

;----------------------------------------------------------------------
; soundbank defs
;----------------------------------------------------------------------

.ifdef HIROM
SB_SAMPCOUNT	=0000h
SB_MODCOUNT	=0002h
SB_MODTABLE	=0004h
SB_SRCTABLE	=0184h
.else
SB_SAMPCOUNT	=8000h
SB_MODCOUNT	=8002h
SB_MODTABLE	=8004h
SB_SRCTABLE	=8184h
.endif

;----------------------------------------------------------------------
; spc commands
;----------------------------------------------------------------------

CMD_LOAD	=00h
CMD_LOADE	=01h
CMD_VOL		=02h
CMD_PLAY	=03h
CMD_STOP	=04h
CMD_MVOL	=05h
CMD_FADE	=06h
CMD_RES		=07h
CMD_FX		=08h
CMD_TEST	=09h
CMD_SSIZE	=0Ah

;----------------------------------------------------------------------

; process for 5 scanlines
PROCESS_TIME = 5
INIT_DATACOPY =13

;======================================================================
.zeropage
;======================================================================

spc_ptr:	.res 3
spc_v:		.res 1
spc_bank:	.res 1

spc1:		.res 2
spc2:		.res 2

spc_fread:	.res 1
spc_fwrite:	.res 1

; port record [for interruption]
spc_pr:		.res 4

digi_src:	.res 3
digi_src2:	.res 3

SoundTable:	.res 3

;======================================================================
.bss
;======================================================================

spc_fifo:	.res 256	; 128-byte command fifo
spc_sfx_next:	.res 1
spc_q:		.res 1

digi_init:	.res 1
digi_pitch:	.res 1
digi_vp:	.res 1
digi_remain:	.res 2
digi_active:	.res 1
digi_copyrate:	.res 1
;spc_fread:	.res 1		;
;spc_fwrite:	.res 1		;

;======================================================================
.segment "RODATA"
;======================================================================

.import SNESMOD_SPC
.import SNESMOD_SPC_end

SPC_BOOT = 0400h ; spc entry/load address

;======================================================================
.code
;======================================================================

.i16
.a8

;**********************************************************************
;* upload driver
;*
;* disable time consuing interrupts during this function
;**********************************************************************
spcBoot:			
;----------------------------------------------------------------------
:	ldx	REG_APUIO0	; wait for 'ready signal from SPC
	cpx	#0BBAAh		;
	bne	:-		;--------------------------------------
	stx	REG_APUIO1	; start transfer:
	ldx	#SPC_BOOT	; port1 = !0
	stx	REG_APUIO2	; port2,3 = transfer address
	lda	#0CCh		; port0 = 0CCh
	sta	REG_APUIO0	;--------------------------------------
:	cmp	REG_APUIO0	; wait for SPC
	bne	:-		;
;----------------------------------------------------------------------
; ready to transfer
;----------------------------------------------------------------------
	lda	f:SNESMOD_SPC	; read first byte
	xba			;
	lda	#0		;
	ldx	#1		;
	bra	sb_start	;
;----------------------------------------------------------------------
; transfer data
;----------------------------------------------------------------------
sb_send:
;----------------------------------------------------------------------
	xba			; swap DATA into A
	lda	f:SNESMOD_SPC, x; read next byte
	inx			; swap DATA into B
	xba			;--------------------------------------
:	cmp	REG_APUIO0	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; increment counter (port0 data)
;----------------------------------------------------------------------
sb_start:
;----------------------------------------------------------------------
	rep	#20h		; write port0+port1 data
	sta	REG_APUIO0	;
	sep	#20h		;--------------------------------------
	cpx	#SNESMOD_SPC_end-SNESMOD_SPC	; loop until all bytes transferred
	bcc	sb_send				;
;----------------------------------------------------------------------
; all bytes transferred
;----------------------------------------------------------------------
:	cmp	REG_APUIO0	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; add 2 or so...
	ina			;--------------------------------------
				; mask data so invalid 80h message wont get sent
	stz	REG_APUIO1	; port1=0
	ldx	#SPC_BOOT	; port2,3 = entry point
	stx	REG_APUIO2	;
	sta	REG_APUIO0	; write P0 data
				;--------------------------------------
:	cmp	REG_APUIO0	; final sync
	bne	:-		;--------------------------------------
	stz	REG_APUIO0
	
	stz	spc_v		; reset V
	stz	spc_q		; reset Q
	stz	spc_fwrite	; reset command fifo
	stz	spc_fread	;
	stz	spc_sfx_next	;
	
	stz	spc_pr+0
	stz	spc_pr+1
	stz	spc_pr+2
	stz	spc_pr+3
;----------------------------------------------------------------------
; driver installation successful
;----------------------------------------------------------------------
	rts			; return
;----------------------------------------------------------------------

;**********************************************************************
; set soundbank bank number (important...)
;
;**********************************************************************
spcSetBank:
	sta	spc_bank
	rts
	
; increment memory pointer by 2
.macro incptr
.scope
	iny
	iny
	
.ifndef HIROM
	bmi	_catch_overflow
	inc	spc_ptr+2
	ldy	#8000h
.else
	bne	_catch_overflow
	inc	spc_ptr+2
.endif

_catch_overflow:
.endscope
.endmacro

;**********************************************************************
; upload module to spc
;
; x = module_id
; modifies, a,b,x,y
;
; this function takes a while to execute
;**********************************************************************
spcLoad:
;----------------------------------------------------------------------

	phx				; flush fifo!
	jsr	spcFlush		;
	plx				;
	
	phx
	ldy	#SB_MODTABLE
	sty	spc2
	jsr	get_address
	rep	#20h
	lda	[spc_ptr], y	; X = MODULE SIZE
	tax
	
	incptr
	
	lda	[spc_ptr], y	; read SOURCE LIST SIZE
	
	incptr
	
	sty	spc1		; pointer += listsize*2
	asl			;
	adc	spc1		;
.ifndef HIROM
	bmi	:+		;
	ora	#8000h		;
.else
	bcc	:+
.endif
	inc	spc_ptr+2	;
:	tay			;
	
	sep	#20h		;
	lda	spc_v		; wait for spc
	pha			;
:	cmp	REG_APUIO1	;
	bne	:-		;------------------------------
	lda	#CMD_LOAD	; send LOAD message
	sta	REG_APUIO0	;
	pla			;
	eor	#80h		;
	ora	#01h		;
	sta	spc_v		;
	sta	REG_APUIO1	;------------------------------
:	cmp	REG_APUIO1	; wait for spc
	bne	:-		;------------------------------
	jsr	do_transfer
	
	;------------------------------------------------------
	; transfer sources
	;------------------------------------------------------
	
	plx
	ldy	#SB_MODTABLE
	sty	spc2
	jsr	get_address
	incptr
	
	rep	#20h		; x = number of sources
	lda	[spc_ptr], y	;
	tax			;
	
	incptr
	
transfer_sources:
	
	lda	[spc_ptr], y	; read source index
	sta	spc1		;
	
	incptr
	
	phy			; push memory pointer
	sep	#20h		; and counter
	lda	spc_ptr+2	;
	pha			;
	phx			;
	
	jsr	transfer_source
	
	plx			; pull memory pointer
	pla			; and counter
	sta	spc_ptr+2	;
	ply			;
	
	dex
	bne	transfer_sources
@no_more_sources:

	stz	REG_APUIO0	; end transfers
	lda	spc_v		;
	eor	#80h		;
	sta	spc_v		;
	sta	REG_APUIO1	;-----------------
:	cmp	REG_APUIO1	; wait for spc
	bne	:-		;-----------------
	sta	spc_pr+1
	stz	spc_sfx_next	; reset sfx counter
	
	
	rts
	
;--------------------------------------------------------------
; spc1 = source index
;--------------------------------------------------------------
transfer_source:
;--------------------------------------------------------------
	
	ldx	spc1
	ldy	#SB_SRCTABLE
	sty	spc2
	jsr	get_address
	
	lda	#01h		; port0=01h
	sta	REG_APUIO0	;
	rep	#20h		; x = length (bytes->words)
	lda	[spc_ptr], y	;
	incptr			;
	ina			;
	lsr			;
	tax			;
	lda	[spc_ptr], y	; port2,3 = loop point
	sta	REG_APUIO2
	incptr
	sep	#20h
	
	lda	spc_v		; send message
	eor	#80h		;	
	ora	#01h		;
	sta	spc_v		;
	sta	REG_APUIO1	;-----------------------
:	cmp	REG_APUIO1	; wait for spc
	bne	:-		;-----------------------
	cpx	#0
	beq	end_transfer	; if datalen != 0
	bra	do_transfer	; transfer source data
	
;--------------------------------------------------------------
; spc_ptr+y: source address
; x = length of transfer (WORDS)
;--------------------------------------------------------------
transfer_again:
	eor	#80h		;
	sta	REG_APUIO1	;
	sta	spc_v		;
	incptr			;
:	cmp	REG_APUIO1	;
	bne	:-		;
;--------------------------------------------------------------
do_transfer:
;--------------------------------------------------------------

	rep	#20h		; transfer 1 word
	lda	[spc_ptr], y	;
	sta	REG_APUIO2	;
	sep	#20h		;
	lda	spc_v		;
	dex			;
	bne	transfer_again	;
	
	incptr

end_transfer:
	lda	#0		; final word was transferred
	sta	REG_APUIO1	; write p1=0 to terminate
	sta	spc_v		;
:	cmp	REG_APUIO1	;
	bne	:-		;
	sta	spc_pr+1
	rts

;--------------------------------------------------------------
; spc2 = table offset
; x = index
;
; returns: spc_ptr = 0,0,bank, Y = address
get_address:
;--------------------------------------------------------------

	lda	spc_bank	; spc_ptr = bank:SB_MODTABLE+module_id*3
	sta	spc_ptr+2	;
	rep	#20h		;
	stx	spc1		;
	txa			;
	asl			;
	adc	spc1		;
	adc	spc2		;
	sta	spc_ptr		;
	
	lda	[spc_ptr]	; read address
	pha			;
	sep	#20h		;
	ldy	#2		;
	lda	[spc_ptr],y	; read bank#
	
	clc			; spc_ptr = long address to module
	adc	spc_bank	;
	sta	spc_ptr+2	;
	ply			;
	stz	spc_ptr
	stz	spc_ptr+1
	rts			;
	
;**********************************************************************
;* x = id
;*
;* load effect into memory
;**********************************************************************
spcLoadEffect:
;----------------------------------------------------------------------
	ldy	#SB_SRCTABLE	; get address of source
	sty	spc2		;
	jsr	get_address	;--------------------------------------
	lda	spc_v		; sync with SPC
:	cmp	REG_APUIO1	;
	bne	:-		;--------------------------------------
	lda	#CMD_LOADE	; write message
	sta	REG_APUIO0	;--------------------------------------
	lda	spc_v		; dispatch message and wait
	eor	#80h		;
	ora	#01h		;
	sta	spc_v		;
	sta	REG_APUIO1	;
:	cmp	REG_APUIO1	;
	bne	:-		;--------------------------------------
	rep	#20h		; x = length (bytes->words)
	lda	[spc_ptr], y	;
	ina			;
	lsr			;
	incptr			;
	tax			;--------------------------------------
	incptr			; skip loop
	sep	#20h		;--------------------------------------
	jsr	do_transfer	; transfer data
				;--------------------------------------
	lda	spc_sfx_next	; return sfx index
	inc	spc_sfx_next	;
	rts			;
	
;**********************************************************************
; a = id
; spc1 = params
;**********************************************************************
QueueMessage:
	sei				; disable IRQ in case user 
					; has spcProcess in irq handler
			
	sep	#10h			; queue data in fifo
	ldx	spc_fwrite		;
	sta	spc_fifo, x		;
	inx				;
	lda	spc1			;
	sta	spc_fifo, x		;
	inx				;
	lda	spc1+1			;
	sta	spc_fifo, x		;
	inx				;
	stx	spc_fwrite		;
	rep	#10h			;
	cli				;
	rts				;

;**********************************************************************
; flush fifo (force sync)
;**********************************************************************
spcFlush:
;----------------------------------------------------------------------
	lda	spc_fread		; call spcProcess until
	cmp	spc_fwrite		; fifo becomes empty
	beq	@exit			;
	jsr	spcProcessMessages	;
	bra	spcFlush		;
@exit:	rts				;
	
	
;**********************************************************************
; process spc messages for x time
;**********************************************************************
spcProcess:
;----------------------------------------------------------------------

	lda	digi_active
	beq	:+
	jsr	spcProcessStream
:

spcProcessMessages:

	sep	#10h			; 8-bit index during this function
	lda	spc_fwrite		; exit if fifo is empty
	cmp	spc_fread		;
	beq	@exit			;------------------------------
	ldy	#PROCESS_TIME		; y = process time
;----------------------------------------------------------------------
@process_again:
;----------------------------------------------------------------------
	lda	spc_v			; test if spc is ready
	cmp	REG_APUIO1		;
	bne	@next			; no: decrement time
					;------------------------------
	ldx	spc_fread		; copy message arguments
	lda	spc_fifo, x		; and update fifo read pos
	sta	REG_APUIO0		;
	sta	spc_pr+0
	inx				;
	lda	spc_fifo, x		;
	sta	REG_APUIO2		;
	sta	spc_pr+2
	inx				;
	lda	spc_fifo, x		;
	sta	REG_APUIO3		;
	sta	spc_pr+3
	inx				;
	stx	spc_fread		;------------------------------
	lda	spc_v			; dispatch message
	eor	#80h			;
	sta	spc_v			;
	sta	REG_APUIO1		;------------------------------
	sta	spc_pr+1
	lda	spc_fread		; exit if fifo has become empty
	cmp	spc_fwrite		;
	beq	@exit			;
;----------------------------------------------------------------------
@next:
;----------------------------------------------------------------------
	lda	REG_SLHV		; latch H/V and test for change
	lda	REG_OPVCT		;------------------------------
	cmp	spc1			; we will loop until the VCOUNT
	beq	@process_again		; changes Y times
	sta	spc1			;
	dey				;
	bne	@process_again		;
;----------------------------------------------------------------------
@exit:
;----------------------------------------------------------------------
	rep	#10h			; restore 16-bit index
	rts				;
	
;**********************************************************************
; x = starting position
;**********************************************************************
spcPlay:
;----------------------------------------------------------------------
	txa				; queue message: 
	sta	spc1+1			; id -- xx
	lda	#CMD_PLAY		;
	jmp	QueueMessage		;
	
spcStop:
	lda	#CMD_STOP
	jmp	QueueMessage

;-------test function-----------;
spcTest:			;#
	lda	spc_v		;#
:	cmp	REG_APUIO1	;#
	bne	:-		;#
	xba			;#
	lda	#CMD_TEST	;#
	sta	REG_APUIO0	;#
	xba			;#
	eor	#80h		;#
	sta	spc_v		;#
	sta	REG_APUIO1	;#
	rts			;#
;--------------------------------#
; ################################

;**********************************************************************
; read status register
;**********************************************************************
spcReadStatus:
	ldx	#5			; read PORT2 with stability checks
	lda	REG_APUIO2		; 
@loop:					;
	cmp	REG_APUIO2		;
	bne	spcReadStatus		;
	dex				;
	bne	@loop			;
	rts				;
	
;**********************************************************************
; read position register
;**********************************************************************
spcReadPosition:
	ldx	#5			; read PORT3 with stability checks
	lda	REG_APUIO2		;
@loop:					;
	cmp	REG_APUIO2		;
	bne	spcReadPosition		;
	dex				;
	bne	@loop			;
	rts				;

;**********************************************************************
spcGetCues:
;**********************************************************************
	lda	spc_q
	sta	spc1
	jsr	spcReadStatus
	and	#0Fh
	sta	spc_q
	sec
	sbc	spc1
	bcs	:+
	adc	#16
:	rts

;**********************************************************************
; x = volume
;**********************************************************************
spcSetModuleVolume:
;**********************************************************************
	txa				;queue:
	sta	spc1+1			; id -- vv
	lda	#CMD_MVOL		;
	jmp	QueueMessage		;

;**********************************************************************
; x = target volume
; y = speed
;**********************************************************************
spcFadeModuleVolume:
;**********************************************************************
	txa				;queue:
	sta	spc1+1			; id xx yy
	tya				;
	sta	spc1			;
	lda	#CMD_FADE
	jmp	QueueMessage

;**********************************************************************
;* a = v*16 + p
;* x = id
;* y = pitch (0-15, 8=32khz)
;**********************************************************************
spcEffect:
;----------------------------------------------------------------------
	sta	spc1			; spc1.l = "vp"
	sty	spc2			; spc1.h = "sh"
	txa				;
	asl				;
	asl				;
	asl				;
	asl				;
	ora	spc2			;
	sta	spc1+1			;------------------------------
	lda	#CMD_FX			; queue FX message
	jmp	QueueMessage		;
;----------------------------------------------------------------------

;======================================================================
;
; STREAMING
;
;======================================================================

;======================================================================
spcSetSoundTable:
;======================================================================
	sty	SoundTable
	sta	SoundTable+2
	rts

;======================================================================
spcAllocateSoundRegion:
;======================================================================
; a = size of buffer
;----------------------------------------------------------------------
	pha				; flush command queue
	jsr	spcFlush		;
					;
	lda	spc_v			; wait for spc
:	cmp	REG_APUIO1		;
	bne	:-			;
;----------------------------------------------------------------------
	pla				; set parameter
	sta	REG_APUIO3		;
;----------------------------------------------------------------------
	lda	#CMD_SSIZE		; set command
	sta	REG_APUIO0		;
	sta	spc_pr+0		;
;----------------------------------------------------------------------
	lda	spc_v			; send message
	eor	#128			;
	sta	REG_APUIO1		;
	sta	spc_v			;
	sta	spc_pr+1		;
;----------------------------------------------------------------------
	rts

;----------------------------------------------------------------------
; a = index of sound
;======================================================================
spcPlaySound:
;======================================================================
	xba
	lda	#128
	xba
	ldx	#-1
	ldy	#-1
	jmp	spcPlaySoundEx
	
;======================================================================
spcPlaySoundV:
;======================================================================
	xba
	lda	#128
	xba
	ldx	#-1
	jmp	spcPlaySoundEx
	
;----------------------------------------------------------------------
; a = index
; b = pitch
; y = vol
; x = pan
;======================================================================
spcPlaySoundEx:
;======================================================================
	sep	#10h			; push 8bit vol,pan on stack
	phy				;
	phx				;
;----------------------------------------------------------------------------
	rep	#30h			; um
	pha				; 
;----------------------------------------------------------------------------
	and	#0FFh			; y = sound table index 
	asl				;
	asl				;
	asl				;
	tay				;
;----------------------------------------------------------------------------
	pla				; a = rate
	xba				;
	and	#255			; clear B
	sep	#20h			;
;----------------------------------------------------------------------------
	cmp	#0			; if a < 0 then use default
	bmi	@use_default_pitch	; otherwise use direct	
	sta	digi_pitch		;
	bra	@direct_pitch		;
@use_default_pitch:			;
	lda	[SoundTable], y		;
	sta	digi_pitch		;
@direct_pitch:				;
;----------------------------------------------------------------------------
	tax				; set transfer rate
	lda	digi_rates, x		;
	sta	digi_copyrate		;
;----------------------------------------------------------------------------
	iny				; [point to PAN]
	pla				; if pan <0 then use default
	bmi	@use_default_pan	; otherwise use direct
	sta	spc1
	bra	@direct_pan
@use_default_pan:
	lda	[SoundTable], y
	sta	spc1
@direct_pan:
;----------------------------------------------------------------------------
	iny				; [point to VOL]
	pla				; if vol < 0 then use default
	bmi	@use_default_vol	; otherwise use direct
	bra	@direct_vol
@use_default_vol:
	lda	[SoundTable], y
@direct_vol:
;----------------------------------------------------------------------------
	asl				; vp = (vol << 4) | pan
	asl				;
	asl				;		
	asl				;
	ora	spc1			;
	sta	digi_vp			;
;----------------------------------------------------------------------------
	iny				; [point to LENGTH]
	rep	#20h			; copy length
	lda	[SoundTable], y		;
	sta	digi_remain		;
;----------------------------------------------------------------------------
	iny				; [point to SOURCE]
	iny				;
	lda	[SoundTable], y		; copy SOURCE also make +2 copy
	iny				;
	iny				;
	sta	digi_src		;
	ina				;
	ina				;
	sta	digi_src2		;
	sep	#20h			;
	lda	[SoundTable], y		;
	sta	digi_src+2		;
	sta	digi_src2+2		;
;----------------------------------------------------------------------------
	lda	#1			; set flags
	sta	digi_init		;
	sta	digi_active		; 
;----------------------------------------------------------------------------
	rts
	
;============================================================================
spcProcessStream:
;============================================================================
	rep	#20h			; test if there is data to copy
	lda	digi_remain		;
	bne	:+			;
	sep	#20h			;
	stz	digi_active		;
	rts				;
:	sep	#20h			;
;-----------------------------------------------------------------------
	lda	spc_pr+0		; send STREAM signal
	ora	#128			;
	sta	REG_APUIO0		;
;-----------------------------------------------------------------------
:	bit	REG_APUIO0		; wait for SPC
	bpl	:-			;
;-----------------------------------------------------------------------
	stz	REG_APUIO1		; if digi_init then:
	lda	digi_init		;   clear digi_init
	beq	@no_init		;   set newnote flag
	stz	digi_init		;   copy vp
	lda	digi_vp			;   copy pan
	sta	REG_APUIO2		;   copy pitch
	lda	digi_pitch		;
	sta	REG_APUIO3		;
	lda	#1			;
	sta	REG_APUIO1		;
	lda	digi_copyrate		; copy additional data
	clc				;
	adc	#INIT_DATACOPY		;
	bra	@newnote		;
@no_init:				;
;-----------------------------------------------------------------------
	lda	digi_copyrate		; get copy rate
@newnote:
	rep	#20h			; saturate against remaining length
	and	#0FFh			; 
	cmp	digi_remain		;
	bcc	@nsatcopy		;
	lda	digi_remain		;
	stz	digi_remain		;
	bra	@copysat		;
@nsatcopy:				;
;-----------------------------------------------------------------------
	pha				; subtract amount from remaining
	sec				;
	sbc	digi_remain		;
	eor	#0FFFFH			;
	ina				;
	sta	digi_remain		;
	pla				;
@copysat:				;
;-----------------------------------------------------------------------
	sep	#20h			; send copy amount
	sta	REG_APUIO0		;
;-----------------------------------------------------------------------
	sep	#10h			; spc1 = nn*3 (amount of tribytes to copy)
	tax				; x = vbyte
	sta	spc1			;
	asl				;
	clc				;
	adc	spc1			;
	sta	spc1			;
	ldy	#0			;
;-----------------------------------------------------------------------


@next_block:
		
	lda	[digi_src2], y
	sta	spc2
	rep	#20h			; read 2 bytes
	lda	[digi_src], y		;
:	cpx	REG_APUIO0		;-sync with spc
	bne	:-			;
	inx				; increment v
	sta	REG_APUIO2		; write 2 bytes
	sep	#20h			;
	lda	spc2			; copy third byte
	sta	REG_APUIO1		;
	stx	REG_APUIO0		; send data
	iny				; increment pointer
	iny				;
	iny				;
	dec	spc1			; decrement block counter
	bne	@next_block		;
;-----------------------------------------------------------------------
:	cpx	REG_APUIO0		; wait for spc
	bne	:-			;
;-----------------------------------------------------------------------	
	lda	spc_pr+0		; restore port data
	sta	REG_APUIO0		;
	lda	spc_pr+1		;
	sta	REG_APUIO1		;
	lda	spc_pr+2		;
	sta	REG_APUIO2		;
	lda	spc_pr+3		;
	sta	REG_APUIO3		;
;-----------------------------------------------------------------------
	tya				; add offset to source
	rep	#31h			;
	and	#255			;
	adc	digi_src		;
	sta	digi_src		;
	ina				;
	ina				;
	sta	digi_src2		;
	sep	#20h			;
;-----------------------------------------------------------------------
	rts
	
digi_rates:
	.byte	0, 3, 5, 7, 9, 11, 13

;;----------------------------------------------------------------------
;spcProcessDigital:
;;----------------------------------------------------------------------
;	lda	spc_pr+0		; send STREAM signal
;	ora	#128			;
;	sta	REG_APUIO0		;
;;----------------------------------------------------------------------
;:	cmp	REG_APUIO0		; wait for SPC
;	bne	:-			;
;;----------------------------------------------------------------------
;	lda	REG_APUIO1		; get chunk counter
;	bne	:+			; if 0 then ragequit
;	lda	spc_pr+0		; [restore p0]
;	sta	REG_APUIO0		;
;	rts				;
;:					;
;;----------------------------------------------------------------------
;	sep	#30h			;
;	stz	REG_MEMSEL		; switch to SlowROM
;	tax				;
;	ldy	#0			;
;;----------------------------------------------------------------------
;; critical routine following
;;
;; some instructions: bytes,cyc (estimate) microseconds
;; stz io       : 3,4 (8*3+6,		30) 1.397us
;; sep/rep IMM8 : 2,3 (8*2+6,		22) 1.024us
;; sta8 io      : 3,4 (8*3+6,		30) 1.397us
;; iny          : 1,2 (8+6,		14) 0.651us
;; lda8 []+y    : 2,7 (8*2+8*3+8,	40) 2.235us
;; sta16 io     : 3,5 (8*3+6+6,		36) 1.676us
;; nop          : 1,2 (8+6,		14) 0.651us
;;----------------------------------------------------------------------
;	sei				;
;	stz	REG_MEMSEL		; switch to SlowROM
;	stz	REG_APUIO0		; send start signal
;	
;	;--------------------------------------------------------------
;	; ~13us until start
;	;--------------------------------------------------------------
;	
;			;	bne	_sr_wait_for_snes	0#		+2
;			;	nop				1.953125	+2
;			;	cmp	x, #0			3.90625		+2
;			;	beq	_sr_skip		5.859375	+2
;			;	mov	y, stream_write		7.8125		+3
;			;	clrc				10.7421875	+2
;			;SPC:					12.6953125
;	
;	
;	; (TWEAK)
;	nop			; 0#
;	nop			; 0.65186012944079713181543046049262
;	nop			; 1.3037202588815942636308609209846
;	nop			; 1.9555803883223913954462913814766
;	nop			; 2.6074405177631885272617218419686
;	nop			; 3.2593006472039856590771523024606
;	nop			; 3.9111607766447827908925827629526
;	nop			; 4.5630209060855799227080132234446
;	nop			; 5.2148810355263770545234436839366
;	nop			; 5.8667411649671741863388741444286
;	nop			; 6.5186012944079713181543046049206
;	nop			; 7.1704614238487684499697350654126
;	nop			; 7.8223215532895655817851655259046
;	nop			; 8.4741816827303627136005959863966
;				; 9.126041812171159845416026446888
;				
;			;SPC:
;			;	byte1 mov a, dp	; 0#
;			;	write		; 2.9296875
;			;	byte2		; 8.7790626 (PER BYTE)
;			; 17.5581252	b2
;			; 26.3371878	b3
;			; 35.1162504	b4
;			; 43.895313	b5
;			; 52.6743756	b6
;			; 61.4534382	b7
;			; 70.2325008	b8
;			; 79.0115634	b9
;			; 87.790626	b10
;			; 96.5696886	b11
;			; 105.3487512	b12
;			; 114.1278138	b13
;			; 122.9068764	b14
;			; 131.685939	b15
;			; 140.4650016	b16
;			; 149.2440642	b17
;			
;.macro dm_copy_byte target, trail ;84 +14*trail cycles
;	lda	[digital_src], y
;	iny
;	sta	REG_APUIO0
;	.repeat trail-1
;		nop
;	.endrep
;.endmacro
;	
;@cpy_next_chunk:
;	dm_copy_byte REG_APUIO0, 8 ;8.7790626 : 9.12604181217116
;	dm_copy_byte REG_APUIO1, 7 ;17.5581252 : 17.6002234949015
;	dm_copy_byte REG_APUIO2, 8 ;26.3371878 : 26.7262653070727
;	dm_copy_byte REG_APUIO3, 7 ;35.1162504 : 35.2004469898031
;	dm_copy_byte REG_APUIO0, 8 ;43.895313 : 44.3264888019742
;	dm_copy_byte REG_APUIO1, 7 ;52.6743756 : 52.8006704847046
;	dm_copy_byte REG_APUIO2, 8 ;61.4534382 : 61.9267122968758
;	dm_copy_byte REG_APUIO3, 7 ;70.2325008 : 70.4008939796061
;	dm_copy_byte REG_APUIO0, 8 ;79.0115634 : 79.5269357917772
;	dm_copy_byte REG_APUIO1, 7 ;87.790626 : 88.0011174745075
;	dm_copy_byte REG_APUIO2, 8 ;96.5696886 : 97.1271592866786
;	dm_copy_byte REG_APUIO3, 7 ;105.3487512 : 105.601340969409
;	dm_copy_byte REG_APUIO0, 8 ;114.1278138 : 114.72738278158
;	dm_copy_byte REG_APUIO1, 7 ;122.9068764 : 123.20156446431
;	dm_copy_byte REG_APUIO2, 8 ;131.685939 : 132.327606276482
;	dm_copy_byte REG_APUIO3, 7 ;140.4650016 : 140.801787959212
;	dm_copy_byte REG_APUIO0, 7 ;149.2440642 : 149.275969641942
;	dm_copy_byte REG_APUIO1, 8 ;158.0231268 : 158.402011454113
;	
;:	cpx	REG_APUIO0 	; sync point
;	bne	:-		;
;	
;	dex
;	beq	@cpy_complete
;	jmp	@cpy_next_chunk
;@cpy_complete:
;
;	lda	spc_pr+0
;	sta	REG_APUIO0
;	lda	spc_pr+1
;	sta	REG_APUIO1
;	lda	spc_pr+2
;	sta	REG_APUIO2
;	lda	spc_pr+3
;	sta	REG_APUIO3
;
;	rep	#10h
;	
;----------------------------------------------------------------------
;	lda	CART_HEADER + 0D5h - 0B0h	; restore rom speed
;	and	#1				;
;	sta	REG_MEMSEL			;
;----------------------------------------------------------------------
;	
;	cli
;	rts

;**********************************************************************
; stop
;
; this is a blocking function
;**********************************************************************
;spcDisableDigital:
;;----------------------------------------------------------------------
;	jsr	spcFlush		; flush existing messages
;;----------------------------------------------------------------------
;	lda	spc_v			; wait for spc
;:	cmp	REG_APUIO1		;
;	bne	:-			;
;;----------------------------------------------------------------------
;	lda	#CMD_DDS		; send DDS message
;	sta	REG_APUIO0		;
;	lda	spc_v			;
;	eor	#128			;
;	sta	spc_v			;
;	sta	REG_APUIO1		;
;;----------------------------------------------------------------------
;:	cmp	REG_APUIO1		; wait for spc
;	bne	:-			;
;;----------------------------------------------------------------------
;	sta	spc_pr+1
;	rts

;**********************************************************************
; start streaming
;
; this is a blocking function
;**********************************************************************
;spcEnableDigital:
;----------------------------------------------------------------------
	;jsr	spcFlush		; flush existing messages
;----------------------------------------------------------------------
;	lda	spc_v			; wait for spc
;:	cmp	REG_APUIO1		;
;	bne	:-			;
;----------------------------------------------------------------------
;	lda	#CMD_EDS		; send EDS message
;	sta	REG_APUIO0		;	
;	lda	spc_v			;
;	eor	#128			;
;	sta	spc_v			;
;	sta	REG_APUIO1		;
;----------------------------------------------------------------------	
;:	cmp	REG_APUIO1		; wait for spc
;	bne	:-			;
;----------------------------------------------------------------------
;	sta	spc_pr+1
;	stz	digital_len
;	stz	digital_len+1
;	rts				;
