;**********************************************************************
; snes cartridge header
;
; customize as needed
;**********************************************************************

;----------------------------------------------------------------------
.segment "HEADER"
;----------------------------------------------------------------------

.import _start
.import _nmi

;----------------------------------------------------------------------
; expansion ram size
;----------------------------------------------------------------------
XRAM_NONE		=00H
XRAM_16KBIT		=01H
XRAM_64KBIT		=02H
XRAM_256KBIT		=03H
XRAM_512KBIT		=04H
XRAM_1MBIT		=05H

;----------------------------------------------------------------------
; map mode
;----------------------------------------------------------------------
MODE_20			=20H	; mode 20, 2.68 mhz (LoROM)
MODE_21			=21H	; mode 21, 2.68 mhz (HiROM)
MODE_23			=23H	; mode 23, 2.68 mhz (?)
MODE_25			=25H	; mode 25, 2.68 mhz (?)
MODE_20_FAST		=30H	; mode 20, 3.58 mhz (LoROM, FastROM)
MODE_21_FAST		=31H	; mode 21, 3.58 mhz (HiROM, FastROM)
MODE_25_FAST		=35H	; mode 25, 3.58 mhz (?)

;----------------------------------------------------------------------
; cartridge type
;----------------------------------------------------------------------
CART_ROM		=00H	; ROM Only
CART_ROM_RAM		=01H	; ROM+RAM
CART_ROM_RAM_BATT	=02H	; ROM+RAM+BATTERY

;----------------------------------------------------------------------
; destination code
;----------------------------------------------------------------------
DEST_JAPAN		=00H
DEST_USA_CANADA		=01H
DEST_EUROPE		=02H
DEST_SCANDANAVIA	=03H
DEST_FRENCH_EUROPE	=06H
DEST_DUTCH		=07H
DEST_SPANISH		=08H
DEST_GERMAN		=09H
DEST_ITALIAN		=0AH
DEST_CHINESE		=0BH
DEST_KOREAN		=0DH
DEST_COMMON		=0EH
DEST_CANADA		=0FH
DEST_BRAZIL		=10H
DEST_AUSTRALIA		=11H
DEST_OTHERX		=12H
DEST_OTHERY		=13H
DEST_OTHERZ		=14H

;----------------------------------------------------------------------
; ROM registration data
;----------------------------------------------------------------------
.byte	'0', '0'		; B0 - maker code
.byte	"GAME"			; B2 - game code
.byte	0, 0, 0, 0, 0, 0, 0	; B6 - reserved
.byte	XRAM_NONE		; BD - expansion ram size
.byte	00h			; BE - special version
.byte	00h			; BF - cartridge sub-number
.byte	"SNES TEMPLATE        "	; C0 - game title
.byte	MODE_20		      ;	; D5 - map mode
.byte	CART_ROM		; D6 - cartridge type (ROM only)
.byte	00h			; D7 - ROM size (set with sneschk)
.byte	XRAM_NONE		; D8 - RAM size (no ram)
.byte	DEST_USA_CANADA		; D9 - destination code (usa/canada) (NTSC)
.byte	33h			; DA - fixed byte
.byte	00h			; DB - mask ROM version
.word	0000h			; DC - complement check (set with sneschk)
.word	0000h			; DE - check sum (set with sneschk)
;----------------------------------------------------------------------
; native mode vectors
;----------------------------------------------------------------------
VEC_E0:	.word	0		; -
VEC_E2:	.word	0		; -
VEC_E4:	.word	0		; -
VEC_E6:	.word	0		; BRK
VEC_E8:	.word	0		; ABORT
VEC_EA:	.word	_nmi		; NMI
VEC_EC:	.word	0		; RESET
VEC_EE:	.word	0		; IRQ
;----------------------------------------------------------------------
; emulation mode vectors
;----------------------------------------------------------------------
VEC_F0:	.word	0		; -
VEC_F2:	.word	0		; -
VEC_F4:	.word	0		; COP
VEC_F6:	.word	0		; -
VEC_F8:	.word	0		; ABORT
VEC_FA:	.word	0		; NMI
VEC_FC:	.word	_start		; RES (entry point)
VEC_FE:	.word	0		; IRQ/BRK
;----------------------------------------------------------------------
