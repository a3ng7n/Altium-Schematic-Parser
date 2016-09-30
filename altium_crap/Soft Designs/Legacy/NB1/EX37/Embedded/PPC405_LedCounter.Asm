;-------------------------------------------------------------------------------
.define DataA          '%R5'
.define PortBase       '%R20'
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
    .section .text
    .global _START
    .global _START405
_START:
_START405:
;-------------------------------------------------------------------------------
MainStart:
    lis   PortBase,0xff00            ; high 16 bits of value
    ori   PortBase, PortBase,0x0000  ; low 16 bits of value

    li    DataA, 0
CountOnLeds_Loop:
    addi  DataA,DataA,1
    stw   DataA,0(PortBase)

    b     CountOnLeds_Loop
;-------------------------------------------------------------------------------

    nop
Test:
    b $-0xFFFC



    .endsec
    .end

