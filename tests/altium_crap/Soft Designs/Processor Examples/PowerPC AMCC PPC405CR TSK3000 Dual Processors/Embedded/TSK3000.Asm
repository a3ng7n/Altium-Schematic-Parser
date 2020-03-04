;-------------------------------------------------------------------------------
.define call     'bgezal $0,'
.define ret      'jr $ra'
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
.define MemPointer          '$2'
.define LoopCounter         '$3'
.define DataA               '$5'
.define DataB               '$6'
.define DataC               '$7'
.define DataD               '$8'
.define DataE               '$9'
.define DataF               '$10'
.define DataG               '$11'
.define DataH               '$12'
.define BaseAddressIO       '$22'
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;Fixed Memory Spaces
MemoryMap_Low           .equ  0x00000000   ;  2K x 32-bit =   8K Bytes
MemoryMap_Main          .equ  0x01000000   ;256K x 32-bit =   1M Bytes
MemoryMap_IO            .equ  0xFF000000   ; IO Space - 16-bit space 0xFFFF_0000 to 0xFFFF_FFFF
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
MemoryLength_Low        .equ    4 * 1024
MemoryLength_Main       .equ    1 * 1024 * 1024
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
RelocatedCodeAddress    .equ  MemoryMap_Main + 0x20000
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
    .global _START
    .section .text
_START:
    b     RelocateCode

RelocateCode_Start:
;-------------------------------------------------------------------------------
    li    BaseAddressIO,MemoryMap_IO
    li    MemPointer,0
    li    DataE, 0x0
    li    MemPointer, MemoryMap_Main + 0x20
    sw    DataE,0(MemPointer)

CountOnLeds_Loop:
    li    DataE,0x0
    lw    DataE,0(MemPointer)
    addi  DataE,DataE,1
    sw    DataE,0(MemPointer)
    srl   DataF,DataE,8
    srl   DataG,DataE,16
    srl   DataH,DataE,24

    sb    DataH,3(BaseAddressIO)  ; First most signicant 8-bits
    sb    DataG,2(BaseAddressIO)  ;
    sb    DataF,1(BaseAddressIO)  ;
    sb    DataE,0(BaseAddressIO)  ; Last least signicant 8-bits

    b     CountOnLeds_Loop

    nop
    break  ;Should never get here
    nop
    nop
;-------------------------------------------------------------------------------
RelocateCode_End:

;-------------------------------------------------------------------------------
RelocateCode:
    la    DataA, RelocateCode_Start
    li    DataB, RelocatedCodeAddress
    la    DataC, RelocateCode_End-4
RelocateCode_Loop:
    lw    DataD,0(DataA)
    nop
    sw    DataD,0(DataB)
    addi  DataA,4
    addi  DataB,4
    bne   DataA,DataC,RelocateCode_Loop

    li    DataB,RelocatedCodeAddress
    jr    DataB
;-------------------------------------------------------------------------------



