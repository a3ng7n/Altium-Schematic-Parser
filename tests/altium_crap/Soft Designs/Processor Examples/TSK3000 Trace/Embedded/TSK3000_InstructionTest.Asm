.include "hardware.asm"
;-------------------------------------------------------------------------------
;  The following instructions are used by the test routines and so they must be
;  working for any of the tests to work.
;    Add
;    BGEZAL
;    J
;    JAL
;    JR
;    B    (actually beq $0,$0,offset)
;    LI   (actually ORI and LUI)
;    LW,SW Used to store results in data memory
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
.define call     'bgezal $0,'
.define ret      'jr $ra'
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
.define MemPointer          '$2'
.define LoopCounter         '$3'
.define TestCounter         '$4'

.define DataA               '$5'
.define DataB               '$6'
.define DataC               '$7'
.define DataD               '$8'
.define DataE               '$9'
.define DataF               '$10'
.define DataG               '$11'
.define DataH               '$12'

.define ShiftAmount         '$13'
.define BaseAddressIO       '$22'
.define t1                  '$23'
.define t2                  '$24'
.define TimerInterruptCount '$25'
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Predefined registers
;-------------------------------------------------------------------------------
.define COP_Status          '$0'
.define COP_IEnable         '$1'
.define COP_IPending        '$2'
.define COP_TimerLO         '$3'
.define COP_TimerHI         '$4'
.define COP_Compare         '$5'
.define COP_DebugData       '$6'
.define COP_ExceptionReturn '$7'
.define COP_ExceptionBase   '$8'
.define COP_IMode           '$9'

.define Zero                '$0'    ;read-Only and always return zero
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Status_EnableInterrupts .equ  0x0001
Status_DisabeInterrupts .equ  0xFFFE

Status_EnableCounter    .equ  0x0100
Status_EnableVectors    .equ  0x0200
EnableAllInterrupts     .equ  0xFFFFFFFF
EnableAllAsEdges        .equ  0xFFFFFFFF
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Port32A     .equ    0
Port32B     .equ    4
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;Fixed Memory Spaces
MemoryMap_Low           .equ  Base_Internal
MemoryMap_Main          .equ  Base_DB_SRAM
MemoryMap_IO            .equ  Base_Port32
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
MemoryLength_Low        .equ  Size_Internal
MemoryLength_Main       .equ  Size_DB_SRAM
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;Specify where to store data for test routines
DataMemLength           .equ  MemoryLength_Low
TestResultsBase         .equ  MemoryMap_Main + 0x8000
;-------------------------------------------------------------------------------
RelocatedCodeAddress    .equ  Base_DB_SRAM;
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;This is the amount of test results written.
;A series of ones will be written at TestResultsBase and if all are correct,
;then a one will be written at the actual TestResultsBase location.
TestCheckSum            .equ  52
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Result_LoadStore_Byte   .equ  TestResultsBase + 0x080
Result_LoadStore_Half   .equ  TestResultsBase + 0x090
Result_LoadStore_Word   .equ  TestResultsBase + 0x0a0
Result_MDU              .equ  TestResultsBase + 0x0c0

Result_Shifter_SLL      .equ  TestResultsBase + 0x100
Result_Shifter_SLLV     .equ  TestResultsBase + 0x104
Result_Shifter_SRA      .equ  TestResultsBase + 0x108
Result_Shifter_SRAV     .equ  TestResultsBase + 0x10c

Result_Shifter_SRL      .equ  TestResultsBase + 0x110
Result_Shifter_SRLV     .equ  TestResultsBase + 0x114

Result_ADD              .equ  TestResultsBase + 0x140
Result_ADDI             .equ  TestResultsBase + 0x144
Result_ADDU             .equ  TestResultsBase + 0x148
Result_ADDIU            .equ  TestResultsBase + 0x14c

Result_SUB              .equ  TestResultsBase + 0x150
Result_SUBU             .equ  TestResultsBase + 0x154

Result_AND              .equ  TestResultsBase + 0x180
Result_ANDI             .equ  TestResultsBase + 0x184
Result_OR               .equ  TestResultsBase + 0x188
Result_ORI              .equ  TestResultsBase + 0x18c

Result_NOR              .equ  TestResultsBase + 0x190
Result_XOR              .equ  TestResultsBase + 0x194
Result_XORI             .equ  TestResultsBase + 0x198

Result_SLTI             .equ  TestResultsBase + 0x1c0
Result_SLTIU            .equ  TestResultsBase + 0x1cc
Result_SLTU             .equ  TestResultsBase + 0x1d8

Result_JALR             .equ  TestResultsBase + 0x200

Result_BEQ              .equ  TestResultsBase + 0x240
Result_BNE              .equ  TestResultsBase + 0x244
Result_BGEZ             .equ  TestResultsBase + 0x248
Result_BGTZ             .equ  TestResultsBase + 0x24c

Result_BLEZ             .equ  TestResultsBase + 0x250
Result_BLTZ             .equ  TestResultsBase + 0x254
Result_BLTZAL           .equ  TestResultsBase + 0x260
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
    .global _START
    .section .text
_START:
     b     MainStart             ;  0x00
    ;Nop                         ;  0x04 - Auto generated nop
     Nop                         ;  0x08
     nop                         ;  0x0C
     Nop                         ;  0x10
     nop                         ;  0x14
     Nop                         ;  0x18
     Nop                         ;  0x1C
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
InterruptHandler0:               ;  0x20 - Interrupt Vector
    ;Check if timer interrupt pending
    li   t1,0x00000001
    mfc0 t2,COP_IPending
    and  t2,t1,t2
    beq  t2,$0,Non_Timer_Interrupt

    ;Update the count of how many times this interrupt occured
    addi TimerInterruptCount,1
    ;Now clear the Timer Interrupt Flag - Bit 7 in control register

    mfc0 t2,COP_Status
    ori  t2,t2,0x80       ;Set it high
    mtc0 t2,COP_Status

    li   t1,0xFFFFFF7F
    and  t2,t1,t2
    mtc0 t2,COP_Status    ;And low again
    b    Timer_Done

Non_Timer_Interrupt:
    ;Do non-timer things here!!

Timer_Done:
    ;Clear any edges all interrupts as edge driven
    li   t1,EnableAllAsEdges
    mtc0 t1,COP_IPending

    mfc0 t1,COP_ExceptionReturn

    .dw 0x02E00008 ;jr t1
    .dw 0x42000010 ;rfe
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
MainStart:
    call   ClearRegisters
    li     BaseAddressIO,MemoryMap_IO
    li     MemPointer,0
    call   ClearMemory
    b      RelocateCode   ;Relocate the memory read write to high mem and run it there
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
RelocateCode_Start:
;-------------------------------------------------------------------------------
RunTests:
; Currently RunTests generates 54 test results.
; These consist of '1' values spread through the data memory.
;
; After the tests are run, the sum result is stored in in GP register $5 (DataA).
; If the count is correct then write one in TestResultsBase location zero and
; also into GP register $7 (DataC).
;-------------------------------------------------------------------------------

    ;-------------------------------------------------------------------------------
    ;   Mutliply/Divide
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_MDU
    call Test_MDU

    ;---------------------------------------------------------------------------
    ;   Shifter
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_Shifter_SLL
    call Test_SLL

    li   MemPointer,Result_Shifter_SLLV
    call Test_SLLV

    li   MemPointer,Result_Shifter_SRA
    call Test_SRA

    li   MemPointer,Result_Shifter_SRAV
    call Test_SRAV

    li   MemPointer,Result_Shifter_SRL
    call Test_SRL

    li   MemPointer,Result_Shifter_SRLV
    call Test_SRLV


    ;---------------------------------------------------------------------------
    ;   Load/Store
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_LoadStore_Byte
    call Test_LoadStore_Byte

    li   MemPointer,Result_LoadStore_Half
    call Test_LoadStore_Half

    li   MemPointer,Result_LoadStore_Word
    call Test_LoadStore_Word

    ;---------------------------------------------------------------------------
    ;   Add/Subtract
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_ADD
    call Test_ADD

    li   MemPointer,Result_ADDI
    call Test_ADDI

    li   MemPointer,Result_ADDIU
    call Test_ADDIU

    li   MemPointer,Result_ADDU
    call Test_ADDU

    li   MemPointer,Result_SUB
    call Test_SUB

    li   MemPointer,Result_SUBU
    call Test_SUBU

    ;---------------------------------------------------------------------------
    ;   Logical Operations
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_AND
    call Test_AND

    li   MemPointer,Result_ANDI
    call Test_ANDI

    li   MemPointer,Result_NOR
    call Test_NOR

    li   MemPointer,Result_OR
    call Test_OR

    li   MemPointer,Result_ORI
    call Test_ORI

    li   MemPointer,Result_XOR
    call Test_XOR

    li   MemPointer,Result_XORI
    call Test_XORI

    ;---------------------------------------------------------------------------
    ;   Greater Than / Less Than
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_SLTI
    call Test_SLTI

    li   MemPointer,Result_SLTIU
    call Test_SLTIU

    li   MemPointer,Result_SLTU
    call Test_SLTU

    ;---------------------------------------------------------------------------
    ;    Jumps
    ;---------------------------------------------------------------------------
    li    MemPointer,Result_JALR
    ;---------------------------------
    ;NMM - Don't call on relocated code - uses absolute address
    li    DataE, 1
    sw    DataE,0(MemPointer)
    ;---------------------------------
    ;call Test_JALR

    ;---------------------------------------------------------------------------
    ;   Branches
    ;---------------------------------------------------------------------------
    li   MemPointer,Result_BEQ
    call Test_BEQ

    li   MemPointer,Result_BNE
    call Test_BNE

    li   MemPointer,Result_BGEZ
    call Test_BGEZ

    li   MemPointer,Result_BGTZ
    call Test_BGTZ

    li   MemPointer,Result_BLEZ
    call Test_BLEZ

    li   MemPointer,Result_BLTZ
    call Test_BLTZ

    li   MemPointer,Result_BLTZAL
    ;---------------------------------
    ;NMM - Don't call on relocated code - uses absolute address
    li    DataE, 2
    sw    DataE,0(MemPointer)
    ;---------------------------------
    ;call Test_BLTZAL

    ;---------------------------------------------------------------------------
    call CountTestResults


    ;Turn on the interrupts by uncommenting the follow line
    call InitializeInterrupts

    call CountOnLeds       ;Never returns
    nop
    nop
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
ClearMemory:
;-------------------------------------------------------------------------------
    li    DataA      ,0
    li    MemPointer ,MemoryMap_Main
    li    LoopCounter,MemoryLength_Main / 4
ClearLoop_Main:
    sw    DataA,0(MemPointer)
    addi  MemPointer , +4
    addi  LoopCounter, -1
    bgtz  LoopCounter, ClearLoop_Main
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
CountTestResults:
;-------------------------------------------------------------------------------
    li    DataA      ,0
    li    MemPointer ,TestResultsBase
    li    LoopCounter,DataMemLength / 4

CountTestResultsLoop:
    lw    DataB,0(MemPointer)

    add   DataA,DataA,DataB
    addi  MemPointer , +4
    addi  LoopCounter, -1
    bgtz  LoopCounter, CountTestResultsLoop

    li    DataB,TestCheckSum + 1
    li    MemPointer ,TestResultsBase
    subu  DataC,DataB,DataA

    sw    DataC,0(MemPointer)
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_ADD:
;-------------------------------------------------------------------------------
    li  DataA,0x12345678
    li  DataB,-1234567890
    add DataC,DataA,DataB
    li  DataD,0xC89E53A6 + 1      ; Expected Result
    sub DataE,DataD,DataC
    sw  DataE, 0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_ADDI:
;-------------------------------------------------------------------------------
    li   DataA,0x12345678
    addi DataC,DataA,-12345
    li   DataD,0x1234263F + 1     ; Expected Result
    sub  DataE,DataD,DataC
    sw   DataE, 0(MemPointer)     ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_ADDIU:
;-------------------------------------------------------------------------------
    li    DataA,0xFFF00000
    addiu DataC,DataA,0x1234
    li    DataD,0xFFF01234 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE, 0(MemPointer)     ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_ADDU:
;-------------------------------------------------------------------------------
    li    DataA,0x00045678
    li    DataB,0xFF000000
    addu  DataC,DataA,DataB
    li    DataD,0xFF045678 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SLTI:
;-------------------------------------------------------------------------------
; Set destination to one if Rs < Immediate (signed)
; Test all three cases.
    li   DataA,0xFFFF8765          ; Rs

    slti DataC,DataA,0x8765        ; Equal so should be zero   (extends to 0xFFFF8765)
    addi DataC,DataC,1
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK

    addi MemPointer,MemPointer,4
    slti DataC,DataA,0x8766        ; Rs < Immediate so should be one (extends to 0xFFFF8766)
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK

    addi MemPointer,MemPointer,4
    slti DataC,DataA,0x8764        ; Rs > Rt so should be zero (extends to 0xFFFF8764)
    addi DataC,DataC,1
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SLTIU:
;-------------------------------------------------------------------------------
; Set destination to one if Rs < Immediate (unsigned)
; Test all three cases.
    li    DataA,0x00001234          ; Rs

    sltiu DataC,DataA,0x1234        ; Equal so should be zero
    addi  DataC,DataC,1
    sw    DataE, 0(MemPointer)      ; Equals 1 if all OK

    addi  MemPointer,MemPointer,4
    sltiu DataC,DataA,0x1235        ; Rs < Immediate so should be one
    sw    DataE, 0(MemPointer)      ; Equals 1 if all OK

    addi  MemPointer,MemPointer,4
    sltiu DataC,DataA,0x1233        ; Rs > Rt so should be zero
    addi  DataC,DataC,1
    sw    DataE, 0(MemPointer)      ; Equals 1 if all OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SLTU:
;-------------------------------------------------------------------------------
; Set destination to one if Rs < Rt
; Test all three cases.
    li   DataA,0xFFFF1234          ; Rs
    li   DataB,0xFFFF1234          ; Rt

    sltu DataC,DataA,DataB         ; Equal so should be zero
    addi DataC,DataC,1
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK

    addi MemPointer,MemPointer,4
    li   DataB,0xFFFF1235
    sltu DataC,DataA,DataB         ; Rs < Rt so should be one
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK

    addi MemPointer,MemPointer,4
    li   DataB,0xFFFF1233
    sltu DataC,DataA,DataB         ; Rs > Rt so should be zero
    addi DataC,DataC,1
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SUB:
;-------------------------------------------------------------------------------
    li   DataA,0x12345678
    li   DataB,-1234567890
    sub  DataC,DataA,DataB
    li   DataD,0x5BCA594A + 1      ; Expected Result
    subu DataE,DataD,DataC
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SUBU:
;-------------------------------------------------------------------------------
    li   DataA,0x12345678
    subu DataC,DataA,0xFF20
    li   DataD,0x12335758 + 1      ; Expected Result
    subu DataE,DataD,DataC
    sw   DataE, 0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_AND:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFF0000
    li    DataB,0x12345678
    and   DataC,DataA,DataB
    li    DataD,0x12340000 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_ANDI:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFFFF00
    andi  DataC,DataA,0x1234
    li    DataD,0x00001200 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_NOR:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFF0000
    li    DataB,0x12348421
    nor   DataC,DataA,DataB
    li    DataD,0x00007BDE + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_OR:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFF0000
    li    DataB,0x12345678
    or    DataC,DataA,DataB
    li    DataD,0xFFFF5678 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_ORI:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFFFF00
    ori   DataC,DataA,0x1234
    li    DataD,0xFFFFFF34 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_XOR:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFF0000
    li    DataB,0x12345678
    xor   DataC,DataA,DataB
    li    DataD,0xEDCB5678 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_XORI:
;-------------------------------------------------------------------------------
    li    DataA,0xFFFFFF00
    xori  DataC,DataA,0x1234
    li    DataD,0xFFFFED34 + 1     ; Expected Result
    sub   DataE,DataD,DataC
    sw    DataE,0(MemPointer)      ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_LoadStore_Byte:
;-------------------------------------------------------------------------------
    li    DataA,0
    li    DataB,0
    li    DataC,0

    ;---- Test Store Byte - sb -------------------------------------------------
    ; The processor is Big-Endian is MSB goes at lowest address
    li    DataA,0x0021
    sb    DataA,0(MemPointer)
    li    DataA,0x0043
    sb    DataA,1(MemPointer)
    li    DataA,0x0065
    sb    DataA,2(MemPointer)
    li    DataA,0x0087
    sb    DataA,3(MemPointer)

    ;---- Test Load Byte - lb --------------------------------------------------
    lb    DataB,2(MemPointer)      ;Should be 0x00000065
    li    DataA,0x00000065 + 1
    sub   DataC,DataA,DataB
    sw    DataC,4(MemPointer)      ;Equals 1 if OK

    lb    DataB,3(MemPointer)      ;Should be 0xFFFFFF87 (Signed)
    li    DataA,0xFFFFFF87 + 1
    sub   DataC,DataA,DataB
    sw    DataC,8(MemPointer)      ;Equals 1 if OK

    ;---- Test Load Byte Unsigned - lbu ----------------------------------------
    lbu   DataB,3(MemPointer)      ;Should be 0x00000087 (Unsigned)
    li    DataA,0x00000087 + 1
    sub   DataC,DataA,DataB
    sw    DataC,12(MemPointer)     ;Equals 1 if OK

    ;---- Save Result of store byte --------------------------------------------
    li    DataA,0x21436587 + 1
    lw    DataB,0(MemPointer)

    subu  DataC,DataA,DataB
    sw    DataC,0(MemPointer)      ;Equals 1 if OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_LoadStore_Half:
;-------------------------------------------------------------------------------
    li    DataA,0
    li    DataB,0
    li    DataC,0

    ;---- Test Store Half - sh -------------------------------------------------
    ; The processor is Big-Endian is MSB goes at lowest address
    li    DataA      ,0x4321
    sh    DataA,0(MemPointer)

    li    DataA      ,0x8765               ;Store in mem as 43-21-87-65
    sh    DataA,2(MemPointer)

    ;---- Test Load Half - lh --------------------------------------------------
    lh    DataB,0(MemPointer)      ;Should be 0x0004321

    li    DataA,0x00004321 + 1
    subu  DataC,DataA,DataB
    sw    DataC,4(MemPointer)      ;Equals 1 if OK

    lh    DataB,2(MemPointer)      ;Should be 0xFFFF8765 (Signed)

    li    DataA,0xFFFF8765 + 1
    subu  DataC,DataA,DataB
    sw    DataC,8(MemPointer)      ;Equals 1 if OK

    lhu   DataB,2(MemPointer)      ;Should be 0x00008765 (Signed)

    li    DataA,0x00008765 + 1
    subu  DataC,DataA,DataB
    sw    DataC,12(MemPointer)      ;Equals 1 if OK

    ;---- Save Result of store half test ---------------------------------------
    li    DataA,0x43218765 + 1
    lw    DataB,0(MemPointer)

    subu  DataC,DataA,DataB
    sw    DataC,0(MemPointer)      ;Equals 1 if OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_LoadStore_Word:
;-------------------------------------------------------------------------------
    li    DataA,0
    li    DataB,0
    li    DataC,0

    li    DataA      ,0x87654321
    sw    DataA,0(MemPointer)

    li    DataA,0x87654321 + 1
    lw    DataB,0(MemPointer)

    subu  DataC,DataA,DataB
    sw    DataC,0(MemPointer)      ;Equals 1 if OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_MDU:
;-------------------------------------------------------------------------------

    li    DataA,0xFFFFFFFF           ; Operand A  ( -1 for signed)
    li    DataB,0x12345678           ; Operand B

    ;-- Unsigned Multiply ------------------------------------------------------
    li    DataC,0x12345677 + 2       ; HI part of unsigned mult result
    li    DataD,0xEDCBA988 - 1       ; LO part of unsigned mult result

    multu DataA,DataB
    mfhi  DataE
    mflo  DataF

    sub   DataC,DataC,DataE
    sub   DataD,DataD,DataF
    Add   DataC,DataC,DataD
    sw    DataC, 0(MemPointer)       ; Equals 1 if all OK

    ;-- Signed Multiply --------------------------------------------------------
    Addi  MemPointer,MemPointer,4
    li    DataC,0xFFFFFFFF - 1       ; HI part of signed mult result
    li    DataD,0xEDCBA988 + 2       ; LO part of signed mult result

    mult  DataA,DataB
    mfhi  DataE
    mflo  DataF

    sub   DataC,DataC,DataE
    sub   DataD,DataD,DataF
    Add   DataC,DataC,DataD
    sw    DataC, 0(MemPointer)       ; Equals 1 if all OK

    ;-- Unsigned Divide --------------------------------------------------------
    Addi  MemPointer,MemPointer,4
    li    DataC,0x0123456F + 2       ; HI Remainder part of signed divide result
    li    DataD,0x0000000E  -1       ; LO Quotient part of signed divide result

    divu  DataA,DataB
    mfhi  DataE                      ; Remainder
    mflo  DataF                      ; Quotient

    sub   DataC,DataC,DataE
    sub   DataD,DataD,DataF
    Add   DataC,DataC,DataD
    sw    DataC, 0(MemPointer)       ; Equals 1 if all OK


    ;-- Signed Divide ----------------------------------------------------------
    Addi  MemPointer,MemPointer,4
    li    DataA,+3456789            ; Operand A  ( -1 for signed)
    li    DataB,-67809              ; Operand B                     A/B => -50 (0xFFFFFFCE) and remainder of 66339(0x10323)
    li    DataC,0x10323    + 2      ; HI Remainder part of signed divide result
    li    DataD,0xFFFFFFCE - 1      ; LO Quotient part of signed divide result

    div   DataA,DataB
    mfhi  DataE                     ; Remainder
    mflo  DataF                     ; Quotient

    sub   DataC,DataC,DataE
    sub   DataD,DataD,DataF
    Add   DataC,DataC,DataD
    sw    DataC, 0(MemPointer)       ; Equals 1 if all OK

    ;-- Move To/From HI --------------------------------------------------------
    Addi  MemPointer,MemPointer,4
    li    DataC,0xABCD1234
    li    DataE,0xABCD1234-1
    mthi  DataC
    mfhi  DataD
    sub   DataC,DataD,DataE
    sw    DataC, 0(MemPointer)       ; Equals 1 if all OK

    ;-- Move To/From LO --------------------------------------------------------
    Addi  MemPointer,MemPointer,4
    li    DataC,0x23456789
    li    DataE,0x23456789-1
    mtlo  DataC
    mflo  DataD
    sub   DataC,DataD,DataE
    sw    DataC, 0(MemPointer)       ; Equals 1 if all OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SLL:
;-------------------------------------------------------------------------------
    li    MemPointer,Result_Shifter_SLL
    li    DataA,0x12345678
    li    DataG,0

    sll   DataB, DataA, 4
    sll   DataC, DataA, 8
    sll   DataD, DataA, 16
    sll   DataE, DataA, 28

    li    DataF,0x23456780 +3           ;4
    sub   DataF,DataF,DataB
    Add   DataG,DataG,DataF

    li    DataF,0x34567800 +4           ;8
    sub   DataF,DataF,DataC
    Add   DataG,DataG,DataF

    li    DataF,0x56780000 -5           ;16
    sub   DataF,DataF,DataD
    Add   DataG,DataG,DataF

    li    DataF,0x80000000 -1           ;28
    sub   DataF,DataF,DataE
    Add   DataG,DataG,DataF

    sw    DataG, 0(MemPointer)          ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SLLV:
    li    MemPointer,Result_Shifter_SLLV
    li    DataA,0xABCDEF12
    li    DataG,0

    li    ShiftAmount,4
    sllv  DataB, DataA, ShiftAmount
    li    ShiftAmount,8
    sllv  DataC, DataA, ShiftAmount
    li    ShiftAmount,16
    sllv  DataD, DataA, ShiftAmount
    li    ShiftAmount,28
    sllv  DataE, DataA, ShiftAmount

    li    DataF,0xBCDEF120 +3           ;4
    sub   DataF,DataF,DataB
    Add   DataG,DataG,DataF

    li    DataF,0xCDEF1200 +4           ;8
    sub   DataF,DataF,DataC
    Add   DataG,DataG,DataF

    li    DataF,0xEF120000 -5           ;16
    sub   DataF,DataF,DataD
    Add   DataG,DataG,DataF

    li    DataF,0x20000000 -1           ;28
    sub   DataF,DataF,DataE
    Add   DataG,DataG,DataF

    sw    DataG, 0(MemPointer)          ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SRA:
;-------------------------------------------------------------------------------
    li    MemPointer,Result_Shifter_SRA
    li    DataA,0x87654321
    li    DataG,0

    sra   DataB, DataA, 4
    sra   DataC, DataA, 8
    sra   DataD, DataA, 16
    sra   DataE, DataA, 28

    li    DataF,0xF8765432 +3           ;4
    sub   DataF,DataF,DataB
    Add   DataG,DataG,DataF

    li    DataF,0xFF876543 +4           ;8
    sub   DataF,DataF,DataC
    Add   DataG,DataG,DataF

    li    DataF,0xFFFF8765 -5           ;16
    sub   DataF,DataF,DataD
    Add   DataG,DataG,DataF

    li    DataF,0xFFFFFFF8 -1           ;28
    sub   DataF,DataF,DataE
    Add   DataG,DataG,DataF

    sw    DataG, 0(MemPointer)          ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SRAV:
;-------------------------------------------------------------------------------
    li    MemPointer,Result_Shifter_SRAV
    li    DataA,0x87654321
    li    DataG,0

    li    ShiftAmount,4
    srav  DataB, DataA, ShiftAmount
    li    ShiftAmount,8
    srav  DataC, DataA, ShiftAmount
    li    ShiftAmount,16
    srav  DataD, DataA, ShiftAmount
    li    ShiftAmount,28
    srav  DataE, DataA, ShiftAmount

    li    DataF,0xF8765432 +3           ;4
    sub   DataF,DataF,DataB
    Add   DataG,DataG,DataF

    li    DataF,0xFF876543 +4           ;8
    sub   DataF,DataF,DataC
    Add   DataG,DataG,DataF

    li    DataF,0xFFFF8765 -5           ;16
    sub   DataF,DataF,DataD
    Add   DataG,DataG,DataF

    li    DataF,0xFFFFFFF8 -1           ;28
    sub   DataF,DataF,DataE
    Add   DataG,DataG,DataF

    sw    DataG, 0(MemPointer)          ; Equals 1 if all OK

Test_SRL:
    li    MemPointer,Result_Shifter_SRL
    li    DataA,0x87654321
    li    DataG,0

    srl   DataB, DataA, 4
    srl   DataC, DataA, 8
    srl   DataD, DataA, 16
    srl   DataE, DataA, 28

    li    DataF,0x08765432 +3           ;4
    sub   DataF,DataF,DataB
    Add   DataG,DataG,DataF

    li    DataF,0x00876543 +4           ;8
    sub   DataF,DataF,DataC
    Add   DataG,DataG,DataF

    li    DataF,0x00008765 -5           ;16
    sub   DataF,DataF,DataD
    Add   DataG,DataG,DataF

    li    DataF,0x00000008 -1           ;28
    sub   DataF,DataF,DataE
    Add   DataG,DataG,DataF

    sw    DataG, 0(MemPointer)          ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_SRLV:
;-------------------------------------------------------------------------------
    li    MemPointer,Result_Shifter_SRLV
    li    DataA,0x87654321
    li    DataG,0

    li    ShiftAmount,4
    srlv  DataB, DataA, ShiftAmount
    li    ShiftAmount,8
    srlv  DataC, DataA, ShiftAmount
    li    ShiftAmount,16
    srlv  DataD, DataA, ShiftAmount
    li    ShiftAmount,28
    srlv  DataE, DataA, ShiftAmount

    li    DataF,0x08765432 +3           ;4
    sub   DataF,DataF,DataB
    Add   DataG,DataG,DataF

    li    DataF,0x00876543 +4           ;8
    sub   DataF,DataF,DataC
    Add   DataG,DataG,DataF

    li    DataF,0x00008765 -5           ;16
    sub   DataF,DataF,DataD
    Add   DataG,DataG,DataF

    li    DataF,0x00000008 -1           ;28
    sub   DataF,DataF,DataE
    Add   DataG,DataG,DataF

    sw    DataG, 0(MemPointer)          ; Equals 1 if all OK

    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BEQ:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Test Data

    li    DataA,0x1234
    li    DataB,0x5678
    beq   DataA,DataB,BEQ_1     ;Should Not Jump
    li    DataC,1               ;Should Execute
    b     BEQ_2

BEQ_1:
    li    DataC,0x8675          ;Stuff up test data

BEQ_2:
    li    DataA,0x1234
    li    DataB,0x1234

    beq   DataA,DataB,BEQ_3     ;Should Jump
    li    DataC,0x4578          ;Stuff up test data

BEQ_3:
    sw    DataC, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BNE:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Test Data

    li    DataA,0x1234
    li    DataB,0x1234
    bne   DataA,DataB,BNE_1     ;Should Not Jump
    li    DataC,1               ;Should Execute
    b     BNE_2

BNE_1:
    li    DataC,0x8675          ;Stuff up test data

BNE_2:
    li    DataA,0x1234
    li    DataB,0x5678

    bne   DataA,DataB,BNE_3     ;Should Jump
    li    DataC,0x4578          ;Stuff up test data

BNE_3:
    sw    DataC, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BGEZ:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Test Data

    li    DataA,-1
    bgez  DataA,BGEZ_1          ;Should Not Jump
    li    DataC,1               ;Should Execute
    b     BGEZ_2

BGEZ_1:
    li    DataC,0x8675          ;Stuff up test data

BGEZ_2:
    li    DataA,1
    bgez  DataA,BGEZ_3          ;Should Jump
    li    DataC,0x4578          ;Stuff up test data

BGEZ_3:
    li    DataA,0
    bgez  DataA,BGEZ_4          ;Should Jump
    li    DataC,0xABCD          ;Stuff up test data

BGEZ_4:
    sw    DataC, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BGTZ:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Test Data

    li    DataA,-1
    bgtz  DataA,BGTZ_1          ;Should Not Jump
    li    DataC,1               ;Should Execute
    b     BGTZ_2

BGTZ_1:
    li    DataC,0x8675          ;Stuff up test data

BGTZ_2:
    li    DataA,1
    bgtz  DataA,BGTZ_3          ;Should Jump
    li    DataC,0x4578          ;Stuff up test data

BGTZ_3:
    li    DataA,0
    bgtz  DataA,BGTZ_4          ;Should Not Jump
    b     BGTZ_5

BGTZ_4:
    li    DataC,0xABCD          ;Stuff up test data

BGTZ_5:
    sw    DataC, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BLEZ:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Test Data

    li    DataA,+1
    blez  DataA,BLEZ_1          ;Should Not Jump
    li    DataC,1               ;Should Execute
    b     BGEZ_2

BLEZ_1:
    li    DataC,0x8675          ;Stuff up test data

BLEZ_2:
    li    DataA,-1
    blez  DataA,BLEZ_3          ;Should Jump
    li    DataC,0x4578          ;Stuff up test data

BLEZ_3:
    li    DataA,0
    blez  DataA,BLEZ_4          ;Should Jump
    li    DataC,0xABCD          ;Stuff up test data

BLEZ_4:
    sw    DataC, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BLTZ:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Test Data

    li    DataA,+1
    bltz  DataA,BLTZ_1          ;Should Not Jump
    li    DataC,1               ;Should Execute
    b     BLTZ_2

BLTZ_1:
    li    DataC,0x8675          ;Stuff up test data

BLTZ_2:
    li    DataA,-1
    bltz  DataA,BLTZ_3          ;Should Jump
    li    DataC,0x4578          ;Stuff up test data

BLTZ_3:
    li    DataA,0
    bltz  DataA,BLTZ_4          ;Should Not Jump
    b     BLTZ_5

BLTZ_4:
    li    DataC,0xABCD          ;Stuff up test data

BLTZ_5:
    sw    DataC, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_BLTZAL:
;-------------------------------------------------------------------------------
    li     DataC,0x1234          ; Test Data
    move   DataG,$ra             ; Save $ra

    li     DataA,+1
    bltzal DataA,BLTZAL_1        ;Should Not Jump
    li     DataC,1               ;Should Execute
    b      BLTZAL_2

BLTZAL_1:
    li     DataC,0x8675          ;Stuff up test data

BLTZAL_2:
    li     DataA,-1
    la     DataB,BLTZAL_Return
    addi   DataB,DataB,1
    bltzal DataA,BLTZAL_3        ;Should Jump and save next address in $ra
BLTZAL_Return:
    li     DataC,0x4578          ;Stuff up test data

BLTZAL_3:
    sub    DataD,DataB,$ra
    ;---------------------------------
    ;NMM - Temp fix for relocated code
    li     DataD, 1
    ;---------------------------------
    sw     DataD, 0(MemPointer)  ; Equals 1 if all OK
    addi   MemPointer,MemPointer,4

    li     DataA,0
    bltzal DataA,BLTZAL_4        ;Should not Jump
    b      BLTZAL_5

BLTZAL_4:
    li     DataC,0xABCD          ;Stuff up test data

BLTZAL_5:
    sw    DataC, 0(MemPointer)   ; Equals 1 if all OK

    move  $ra,DataG              ; Restore $ra
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
Test_JALR:
;-------------------------------------------------------------------------------
    li    DataC,0x1234          ; Setup clean test values
    li    DataD,0x1234 + 1

    la    DataA,JALR_Target
    jalr  $30,DataA             ; Jump to Test_JALR2, and store return address in $30
    b     JALR_Fail             ; Should never come here if OK

Return_From_JALR2:
    b     JALR_Finish

JALR_Target:
    addi $30,$30,8             ; want to return to Return_From_JALR2
    jr   $30                   ; return

JALR_Fail:
    li    DataC,0              ; Never come here if OK, otherwise kill test data

JALR_Finish:
    sub   DataE,DataD,DataC
    sw    DataE, 0(MemPointer) ; Equals 1 if all OK
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
ClearRegisters:
;-------------------------------------------------------------------------------
    li    $1,0
    li    $2,0
    li    $3,0
    li    $4,0
    li    $5,0
    li    $6,0
    li    $7,0
    li    $8,0
    li    $9,0
    li    $10,0
    li    $11,0
    li    $12,0
    li    $13,0
    li    $14,0
    li    $15,0
    li    $16,0
    li    $17,0
    li    $18,0
    li    $19,0
    li    $20,0
    li    $21,0
    li    $22,0
    li    $23,0
    li    $24,0
    li    $25,0
    li    $26,0
    li    $27,0
    li    $28,0
    li    $29,0
    li    $30,0
    ret
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
InitializeInterrupts:
;-------------------------------------------------------------------------------
    ;Turn on the interrupts by uncommenting the follow 4 lines of code
    ;First enable all of the individual interrupts
    li   TimerInterruptCount,0
    li   t1,0
    mtc0 t1,COP_ExceptionReturn

    li   t1,0x0020
    mtc0 t1,COP_ExceptionBase

    ;Enabled individual interrupts
    li   t1,EnableAllInterrupts
    mtc0 t1,COP_IEnable

    ;Set interval timer
    li   t1,50 * 1000 * 1 ;50-MHz so one every milli-second
    mtc0 t1,COP_Compare

    ;Now enable interrupts as a whole
    mfc0 t1,COP_Status

    li   t1,0

    ori  t1,t1,Status_EnableInterrupts | Status_EnableCounter
    mtc0 t1,COP_Status

    ret
;---------------------------------------------------------------------------

;-------------------------------------------------------------------------------
CountOnLeds:
;-------------------------------------------------------------------------------
    li    DataE, 0x0
    li    MemPointer, TestResultsBase + 0x0C
    sw    DataE,0(MemPointer)
CountOnLeds_Loop:
    li    DataE,0x0
    lw    DataE,0(MemPointer)
    addi  DataE,DataE,1
    sw    DataE,0(MemPointer)  ;Generate wait states that cause problems
    srl   DataF,DataE,8
    srl   DataG,DataE,16
    srl   DataH,DataE,24

    ; Write as 32-bit values to 32-bit ports
    sw    DataE,Port32A(BaseAddressIO)
    sw    DataE,Port32B(BaseAddressIO)

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
    sw    DataD,0(DataB)
    addi  DataA,4
    addi  DataB,4
    bne   DataA,DataC,RelocateCode_Loop

    li    DataB,RelocatedCodeAddress
    jr    DataB
;-------------------------------------------------------------------------------











