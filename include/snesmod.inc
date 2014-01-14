.ifndef __SNESMOD_INC__
.define __SNESMOD_INC__

;*************************************************************************
;* spcBoot
;*
;* boots the spc700 with sm-spc
;* call once at startup
;*************************************************************************
.import spcBoot

;*************************************************************************
;* spcSetBank
;*
;* params:
;*   a = bank
;*
;* set soundbank origin
;* soundbank must have dedicated bank(s)
;*************************************************************************
.import spcSetBank

;*************************************************************************
;* spcLoad
;*
;* params:
;*   x = module_id
;*
;* load module into sm-spc
;* this function may take some time to execute
;*************************************************************************
.import spcLoad

;*************************************************************************
;* spcLoadEffect
;*
;* params:
;*   x = sfx_id
;*
;* load sound effect into sm-spc
;* this function may take some time to execute
;*************************************************************************
.import spcLoadEffect

;*************************************************************************
;* spcPlay
;*
;* params:
;*   x = starting position
;*
;* play module
;*
;* note: this simply queues a message, use spcFlush if you want
;* to wait until the message is processed
;*
;* another note: there may be a significant startup time from
;* after the message is processed to when the song starts playing...
;* to sync the program with the song start use spcFlush and then
;* wait until SPC_P of the status register is set.
;*************************************************************************
.import spcPlay
.import spcStop

;*************************************************************************
;* spcReadStatus
;*
;* returns:
;*   a = status register
;*
;* read status register from sm-spc
;*************************************************************************
.import spcReadStatus

;*************************************************************************
;* spcGetCues
;*
;* returns:
;*   a = (0..15) number of cues that occured since last call
;*
;* get number of cues that have passed (pattern effect SF1)
;*************************************************************************
.import spcGetCues

;*************************************************************************
;* spcSetModuleVolume
;*
;* params:
;*   x = volume (0..255)
;*
;* set the module playback volume
;*************************************************************************
.import spcSetModuleVolume

;*************************************************************************
;* spcFadeModuleVolume
;*
;* params:
;*   x = target volume
;*   y = fade speed (volume(0..255) += y every 32ms)
;*
;* fade the module volume towards the target
;*************************************************************************
.import spcFadeModuleVolume

;*************************************************************************
;* spcFlush
;*
;* Flush message queue (force sync)
;*************************************************************************
.import spcFlush

;*************************************************************************
;* spcProcess
;*
;* Process messages
;*
;* This function will try to give messages to the spc until a few
;* scanlines pass
;*
;* this function MUST be called every frame if you are using
;* streamed sounds
;*************************************************************************
.import spcProcess

;*************************************************************************
;* spcTest
;*
;* diagnostic function
;*************************************************************************
.import spcTest

;*************************************************************************
;* spcEffect
;*
;* Play sound effect (load with spcLoadEffect)
;*
;* a = volume(0..15) AND panning(0..15) (v*16+p)
;* x = effect index (0-15)
;* y = pitch (0-15, 8=32khz)
;*************************************************************************
.import spcEffect

;*************************************************************************
;* spcSetSoundTable
;*
;* set the address of the SOUND TABLE
;*
;* y = address of sound table
;* a = bank of sound table
;*************************************************************************
.import spcSetSoundTable

;*************************************************************************
;* spcAllocateSoundRegion
;*
;* set the size of the sound region
;* (this must be big enough to hold your longest/largest sound)
;*
;* this function will STOP module playback too
;*
;* a = size of sound region (a*256 bytes)
;*************************************************************************
.import spcAllocateSoundRegion

;*************************************************************************
; SOUND HEADER (8 BYTES)
;
; byte: default PITCH (1..6) (hz = PITCH * 2000)
; byte: default PANNING (0..15)
; byte: default VOLUME (0..15)
; word: number of BRR chunks (BYTELEN/9) (max 4kilobytes??)
; word: address of BRR data
; byte: bank of BRR data
;*************************************************************************

;*************************************************************************
;* spcPlaySound
;*
;* Play sound from memory (using default arguments)
;*
;* a = index in sound table
;*************************************************************************
.import spcPlaySound

;*************************************************************************
;* spcPlaySoundV
;*
;* Play sound from memory with custom VOLUME
;*
;* a = index in sound table
;* x = volume (0..15)
;*************************************************************************
.import spcPlaySoundV

;*************************************************************************
;* spcPlaySoundEx
;*
;* Play sound from snes memory with custom args
;*
;* a = index in sound table
;* b = pitch of sound (0..6, >128=use default)
;* y = volume of sound (0..15, >32768=use default)
;* x = panning of sound (0..15, >32768=use default)
;*************************************************************************
.import spcPlaySoundEx

;*************************************************************************
;* status register definitions
;*
;* F = module fade in progress
;* E = end of module reached
;* P = module is playing (0 means it is not playing or it is preparing)
;*************************************************************************
SPC_F	=80h
SPC_E	=40h
SPC_P	=20h

.macro spcPlaySoundM index
	lda	#index
	jsr	spcPlaySound
.endmacro

.endif
