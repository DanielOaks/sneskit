;----------------------------------------------------
; joypad reading codes
;----------------------------------------------------

.ifndef __SNES_JOYPAD__
__SNES_JOYPAD__ = 1

; joypad reading function
.import joyRead

; held = 16bit key status
; down = 16bit key changing 0->1
.importzp joy1_held, joy1_down, joy2_prev, joy2_down

.endif
