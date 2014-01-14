;----------------------------
; some zero page variables
;----------------------------

.globalzp memptr, m0, m1, m2, m3, m4, m5, m6, m7

.zeropage

memptr:	.res 3  	;
m0:	.res 2		;
m1:	.res 2		;
m2:	.res 2		;
m3:	.res 2		;

m4:	.res 2		;
m5:	.res 2		;
m6:	.res 2		;
m7:	.res 2		;

;---------------------------------------
; suggested standard:
;
; a,b,x,y,memptr,m0,m1,m2,m3 may be
; modified freely and m4-m7 be preserved 
; across functions
;
;also:
; a=8bit, i=16bit
;---------------------------------------