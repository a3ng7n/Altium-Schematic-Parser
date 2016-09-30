;-------------------------------------------------------------------------------
; The sole purpose of the boot program is to switch on the memory at 0x000_0000
; so that the real application (which starts from this address) can begin.
; To did this we switch use bank1 of the 405CR's EBC (External Bus Controller)
; The rest of the memory configuration happens in the main application code.
;
; This program is actually implemented as a simple case statement in the VHDL
; wrapper for the 405CR. This should realy be using true FPGA distributed memory
; but currently we do not suppport this with our memory components.
; Using a block memory would be too wastefull since we have only 12 instrucions.
;-------------------------------------------------------------------------------

.include "PPC405CR_Constants.Asm"

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
.define MemPointer          '%R2'
.define LoopCounter         '%R3'
.define DataA               '%R5'
.define DataB               '%R6'
.define DataC               '%R7'
.define DataD               '%R8'
.define DataE               '%R9'
.define DataF               '%R10'
.define DataG               '%R11'
.define DataH               '%R12'
.define DataI               '%R13'
.define BaseAddressIO       '%R22'
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;Fixed Memory Spaces
MemoryMap_Low           .equ  0x00000000   ;  2K x 32-bit =   8K Bytes
MemoryMap_Main          .equ  0x01000000   ;  256K x 32-bit =   1M Bytes
MemoryMap_IO            .equ  0xFF000000   ; 0x01200000 IO Space - 16-bit space 0xFF00_0000 to 0xFFFF_FFFF
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
MemoryLength_Low        .equ    4 * 1024
MemoryLength_Main       .equ    1 * 1024 * 1024
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
RelocatedCodeAddress    .equ  MemoryMap_Main + 0x10000
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
    .section .text
    .global _START
    .global _START405
_START:
_START405:
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Configure Bank2 for Memory
; EBC Access Parameters - Bank2
;-------------------------------------------------------------------------------
    li      %R4,EBC0_B2AP
    mtdcr   PeripheralControl_Address,%R4
    lis     %R4    ,@MSH(EBC_WishboneBus)
    ori     %R4,%R4,@LSH(EBC_WishboneBus)
    mtdcr   PeripheralControl_Data,%R4

;-------------------------------------------------------------------------------
; EMC Configuration Registers  - Bank2
;-------------------------------------------------------------------------------
    li      %R4,EBC0_B2CR
    mtdcr   PeripheralControl_Address,%R4
    lis     %R4    ,@MSH(EBC_16MB_32Bit_RAM_010)
    ori     %R4,%R4,@LSH(EBC_16MB_32Bit_RAM_010)
    mtdcr   PeripheralControl_Data,%R4
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Configure Bank0 for Peripheral
; EBC Access Parameters - Bank0
;-------------------------------------------------------------------------------
    li      %R4,EBC0_B0AP
    mtdcr   PeripheralControl_Address,%R4
    lis     %R4    ,@MSH(EBC_WishboneBus)
    ori     %R4,%R4,@LSH(EBC_WishboneBus)
    mtdcr   PeripheralControl_Data,%R4

;-------------------------------------------------------------------------------
; EMC Configuration Registers  - Bank0
;-------------------------------------------------------------------------------
    li      %R4,EBC0_B0CR
    mtdcr   PeripheralControl_Address,%R4
    lis     %R4    ,@MSH(EBC_16MB_32Bit_RAM_FFF)
    ori     %R4,%R4,@LSH(EBC_16MB_32Bit_RAM_FFF)
    mtdcr   PeripheralControl_Data,%R4
;-------------------------------------------------------------------------------

    b RelocateCode

RelocateCode_Start:
    lis   BaseAddressIO, @MSH(MemoryMap_IO)
    ori   BaseAddressIO, BaseAddressIO, @LSH(MemoryMap_IO)
    li    MemPointer,0
    li    DataE, 0x0
    lis   MemPointer, @MSH( MemoryMap_Main+0x10 )
    ori   MemPointer,MemPointer, @LSH( MemoryMap_Main+0x10 )
    stw   DataE,0(MemPointer)

CountOnLeds_Loop:
    li    DataE,0x0
    la   DataE,0(MemPointer)        ;???
    addi  DataE,DataE,1
    ;stw   DataE,0(MemPointer)
    addi  MemPointer, MemPointer, 1
    srwi   DataF,DataE,8
    srwi   DataG,DataE,16
    srwi   DataH,DataE,24

    stb    DataH,3(BaseAddressIO)  ; First most significant 8-bits
    stb    DataG,2(BaseAddressIO)  ;
    stb    DataF,1(BaseAddressIO)  ;
    stb    DataE,0(BaseAddressIO)  ; Last least significant 8-bits

    b     CountOnLeds_Loop

    nop
    nop  ;Should never get here
    nop
    nop
;-------------------------------------------------------------------------------
RelocateCode_End:

;-------------------------------------------------------------------------------
RelocateCode:
    li  DataA, RelocateCode_Start
    lis DataB, @MSH( RelocatedCodeAddress )
    ori DataB,DataB, @LSH( RelocatedCodeAddress )
    li  DataC, RelocateCode_End-4
    li  DataD, 0
RelocateCode_Loop:
    lmw  DataD,0(DataA)
    nop
    stw   DataD,0(DataB)
    addi  DataA,DataA,4
    addi  DataB,DataB,4
    cmpw  DataA, DataC
    bne   RelocateCode_Loop

    lis DataB, @MSH( RelocatedCodeAddress )
    ori DataB,DataB, @LSH( RelocatedCodeAddress )
    mtctr DataB
    bctrl
;-------------------------------------------------------------------------------
    .endsec
    .end



