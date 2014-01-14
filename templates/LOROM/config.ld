#-------------------------------------------------------------------------
# SNES MODE 20H (LoROM) Memory Specification
#-------------------------------------------------------------------------

#*************************************************************************
MEMORY {
#*************************************************************************

#-------------------------------------------------------------------------
# ZROM used for program code (32k)
# really at bank 80h but use 0 to avoid asm errors
#-------------------------------------------------------------------------
	ZROM:	start = $8000, size = $7FB0, fill = yes;
	CHEAD:	start = $FFB0, size = 80, fill = yes;

#-------------------------------------------------------------------------
# insert rom sections for program data
#-------------------------------------------------------------------------
	ROM1:	start = $818000, size = $8000, fill = yes;
#	ROM2:	start = $828000, size = $8000, fill = yes;
	
#-------------------------------------------------------------------------
# 128K memory
#-------------------------------------------------------------------------

	ZPAGE:	start = $0000, size = 256;	# 8k loram
	LORAM:	start = $0100, size = 7936;	#
	HIRAM1:	start = $7E2000, size = 57344;	# 56k hiram1
	HIRAM2:	start = $7F0000, size = 65536;	# 64k hiram2
}

#*************************************************************************
SEGMENTS {
#*************************************************************************

#-------------------------------------------------------------------------
# standard segments
#-------------------------------------------------------------------------
	CODE:		load = ZROM, type = ro;		# 32k code
	HEADER:		load = CHEAD, type = ro;	# cartridge header
	ZEROPAGE:	load = ZPAGE, type = zp;	# zero page
	BSS:		load = LORAM, type = bss;	# 8k (minus zpage) bss
	RODATA:		load = ROM1, type = ro;		# some rodata...
	
#-------------------------------------------------------------------------
# Note: when using HRAM or HRAM2 or HDATA you must use FAR addressing:
#
# .segment "HRAM": far
#
# the segment word 'data' can't be used because i can't figure out how to
# override the addressing mode
#
#-------------------------------------------------------------------------
	
	HRAM:		load = HIRAM1, type = bss;	# 56K+64K HRAM
	HRAM2:		load = HIRAM2, type = bss;	# 
	
#-------------------------------------------------------------------------
# select load and run segments for HDATA
# default: load in ROM1, run in 64k HIRAM2
#-------------------------------------------------------------------------

	HDATA:		load = ROM1, run = HIRAM2, type = rw, define = yes;
	
#-------------------------------------------------------------------------
# custom segments (insert as needed)
#-------------------------------------------------------------------------

#	GRAPHICS:	load = ROM1, type = ro;	
#	SOUNDBANK:	load = ROM2, type = ro;

}
