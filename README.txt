********************
SNES DEVELOPMENT KIT
********************

JULY 2009 EDITION

PLEASE ADD "SNESKIT" TO YOUR ENVIRONMENT WITH THE PATH
TO THE SNESKIT FOLDER
____________________________________________________________

YOU NEED TO POPULATE THE KIT
____________________________________________________________

CC65
--
Download a cc65 package from cc65.org and place in /cc65
The binaries should then be at /cc65/bin

ADDITIONAL DOCUMENTATION
--
You can add these to your /docs folder:
 * book1.pdf - SNES programming manual!!
   http://romhacking.net/docs/226/
 * Programmanual.pdf - (if 65816primer.txt isn't enough)
   www.westerndesigncenter.com/wdc/datasheets/Programmanual.pdf
 * w65c816s.pdf - 65816 datasheet
   [missing link]

EMULATORS
--
Populate your /emu folder with these recommended emulators
 * ZSNES (very FAST) www.zsnes.com
 * Snes9x DEBUG VERSION (very DEBUGGER) http://romhacking.net/utils/241/
 * BSNES (very ACCURATE) www.byuu.com
 * SNESGT (very ???) http://www.zophar.net/snes/snesgt.html

 * BSNES +DEBUGGER
   (i just found out about this one and haven't tried it yet)
   http://romhacking.net/utils/273/
 
 * ZSNES +DEBUGGER
   (another one i haven't tried...)
   [missing link]

OTHER TOOLS
--
 * VSNES - SUPER useful for viewing data (like VRAM) 
   in an emulator savestate!
   http://romhacking.net/utils/274/
 * SPCTool - useful for spc development
   http://spcsets.caitsith2.net/spctool/


____________________________________________________________

GETTING STARTED
____________________________________________________________

Two templates are provided in /template. One is for LOROM
mapping mode, the other is for HIROM mapping mode! The 
templates contain a HEADER.ASM which should be modified
to suit your purposes--it contains start vectors, mapping
mode, game TITLE, cartridge speed (append _FAST to map mode 
for HISPEEDS), and NTSC/PAL can be selected by changing the 
region.

a .pnproj file is included for Programmers Notepad 2!

*** SYNTAX HIGHLIGHTING ***

You can customize Programmers Notepad to have syntax
highlighting for 65816. Goto:
 Tools->Options->Schemes->Advanced
and select Assembler from the Scheme menu.
Goto "Keywords" and replace the x86 CPU instructions with this:

adc and asl bcc bcs beq bit bmi bne bpl bra 
clc cli cmp cpa cpx cpy dea dec dex dey eor 
ina inc inx iny jml jmp jsr lda ldx ldy lsr 
nop ora pha phb php phx phy pla plb plp plx 
ply rep rol ror rti rts sbc sec sei sep sta 
stx sty stz tax tay tcd tcs tsc txa txs txy 
tya tyx wai xba xce

REGISTERS with this:

a x y

DIRECTIVES with this:

.a16 .a8 .asciiz .bss .byte .code .define 
.else .elseif .endif .endmacro .endrep 
.endrepeat .endscope .export .exportzp 
.global .globalzp .i16 .i8 .if .ifdef .ifndef 
.import .importzp .include .local .macro 
.repeat .res .rodata .scope .segment .word 
.zeropage

Also change tabsize to 8! which is optimal for assembly 
coding!!

I may have forgotten some of the instructions/directives
so if you see one that isn't highlighted you can easily
add it to the list! and push Sort to reorganize them! :D

You can then customize the Styles/More options according
to your taste!! Like I made numbers bold, and directives
bold and red!! (see pn2_65816.png)

____________________________________________________________

SNESMOD
____________________________________________________________

To use SNESMOD, the SNESMOD source files must be added
to the startfiles in the makefile. see the SNESMOD example.

snesmod.asm sm_spc.asm

____________________________________________________________

SNESGRIT
____________________________________________________________

This is a graphics converter for SNES which is a modified 
version of the "GBA Raster Image Transmogrifier" by Cearn, 
which is distributed under the GPL. Please contact me 
(mukunda@mukunda.com) if you would like to obtain the 
[modified] source code.

____________________________________________________________

IRC
____________________________________________________________

Join #snesdev on EFNet!
