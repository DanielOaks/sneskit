#-------------------------------------------------------------------------
# SNES MODE 21H (HiROM) Memory Specification
#-------------------------------------------------------------------------

#*************************************************************************
MEMORY {
#*************************************************************************

#-------------------------------------------------------------------------
# HROM used for extended program code (32k)
#   ( do NOT access HROM normally! )
# CROM used for common program code (32k) (shared across banks)
# these are really at bank C0 but use 0 to avoid asm errors
#-------------------------------------------------------------------------
	HROM:	start = $0000, size = $8000, fill = yes;
	CROM:	start = $8000, size = $7FB0, fill = yes;
	CHEAD:	start = $FFB0, size = $50, fill = yes;

#-------------------------------------------------------------------------
# insert rom sections for program data
#-------------------------------------------------------------------------
	ROM1:	start = $C10000, size = $10000, fill = yes;
	ROM2:	start = $C20000, size = $10000, fill = yes;
	
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
	CODE:		load = CROM, type = ro;		# 32k code
	XCODE:		load = HROM, type = ro;		# 32k xcode
	HEADER:		load = CHEAD, type = ro;	# cartridge header
	ZEROPAGE:	load = ZPAGE, type = zp;	# zero page
	BSS:		load = LORAM, type = bss;	# 8k (minus zpage) bss
	RODATA:		load = ROM1, type = ro;		# some rodata...
	
#-------------------------------------------------------------------------
# Remember to use FAR addressing or similar compensation when
# accessing data that is not mapped to bank 00H/80H
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
	SOUNDBANK:	load = ROM2, type = ro; # THE SOUNDBANK NEEDS DEDICATED BANK(S)

}
