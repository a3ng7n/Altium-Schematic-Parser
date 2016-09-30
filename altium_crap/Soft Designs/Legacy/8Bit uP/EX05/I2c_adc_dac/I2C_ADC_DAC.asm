
; I2CM I2C Controller Register Initialization Values

CONTROL_Enable     .equ  001h  ; Control Register Values
CONTROL_IEN        .equ  002h
CONTROL_IACK       .equ  004h
CONTROL_WR         .equ  008h
CONTROL_RD         .equ  010h
CONTROL_STO        .equ  020h
CONTROL_STA        .equ  040h
CONTROL_NACK       .equ  080h

CLK_0_VAL          .equ  018h  ; Clock Prescale Register Values
CLK_1_VAL          .equ  000h

; I2CM Controller Register Addresses

I2CM_Control       .equ  000h
I2CM_Status        .equ  001h
I2CM_CLK_0         .equ  002h
I2CM_CLK_1         .equ  003h
I2CM_WR_DAT        .equ  004h
I2CM_RD_DAT        .equ  005h

; I2CM External Control Signals

WRITE              .equ  010h
READ               .equ  020h

; Other Symbols
CLEAR              .equ  000h

; there are 2 I2C devices on the Nanoboard a ADC: MAX1037 - addr: C8 (C9 read)
;                                          a DAC: MAX5841 - addr: BA (BB read)
;
; ADC I2C Addresses
;   MAX1037_ADC_WR  @ 0C8h
;   MAX1037_ADC_RD  @ 0C9h
;   MAX1039_ADC_WR  @ 0CAh
;   MAX1039_ADC_RD  @ 0CBh
;
; DAC I2C Addresses
;   MAX5841M_DAC_WR @ 0BAh
;   MAX5841L_DAC_WR @ 07Ah
;
; For more information on the device: http://pdfserv.maxim-ic.com/en/ds/MAX5841.pdf

; ADC I2C Addresses
MAX1037_ADC_WR     .equ  0C8h
MAX1037_ADC_RD     .equ  0C9h

; DAC I2C Addresses
MAX5841M_DAC_WR    .equ  0BAh


    .global __start

    .Section Text, Code, at( 0H )
__start:
START:
    LJMP    INIT

    .Section Text, Code, at( 3H )
    LJMP    INT0SUBR

    .Section Text, Code, at( 50H )
INIT:

; Initialize I2CM Registers:

    ; reset control and write data register
    MOV P2, #CLEAR
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #WRITE | I2CM_WR_DAT
    MOV P1, #CLEAR

    ; write to clk0
    MOV P2, #CLK_0_VAL
    MOV P1, #WRITE | I2CM_CLK_0
    MOV P1, #CLEAR

    ; write to clk1
    MOV P2, #CLK_1_VAL
    MOV P1, #WRITE | I2CM_CLK_1
    MOV P1, #CLEAR


DAC_INIT:
;--------------------------------------------------;
;   Initialize I2C DAC

    MOV A,  #MAX5841M_DAC_WR
    LCALL   WRITE_ADDRESS

;   Control register of MAX5841 ( + 4 data bits)
;   Setting C3-C0 to '1' and D9-D6 to '0' will uses extended mode for power down registers
    MOV A,  #0F0h
    MOV B,  #CLEAR
    LCALL   WRITE_DATA

;   PD regsiters (clear the power-down registers)
;   Powering up device. DAC output A and B restored to previous value
    MOV A,  #03Ch
    MOV B,  #CONTROL_STO
    LCALL   WRITE_DATA
;--------------------------------------------------;



ADC_INIT:
;--------------------------------------------------;
;   Initialize the I2C ADC

    MOV A,  #MAX1037_ADC_WR
    LCALL   WRITE_ADDRESS

;   Setup register of MAX1037
;   Setting the setup register to default settings
    MOV A,  #80h
    MOV B,  #CLEAR
    LCALL   WRITE_DATA

;   Setting the configuration register to SCAN mode '11' coverting channels selected by CS3-CS0
    MOV A,  #61h
    MOV B,  #CONTROL_STO
    LCALL   WRITE_DATA
;--------------------------------------------------;


    MOV R0, #CLEAR ;  Initialize Saw-Wave counter registers
    MOV R1, #CLEAR


SAW_TOOTH_WAVE:
;--------------------------------------------------;
;   Main Program Loop - creates a SAW output on DAC

;   Write Address
    MOV A, #MAX5841M_DAC_WR
    LCALL  WRITE_ADDRESS

;   Control register of MAX5841 ( + 4 data bits)
    MOV A, R1
    ORL A, #080h
    MOV B, #CLEAR
    LCALL  WRITE_DATA

;   WRITE the DAC Output Voltage
    MOV A, R0
    MOV B, #CONTROL_STO
    LCALL  WRITE_DATA

;   Keep ramping up
    lcall ADC_IN
    CJNE R0, #0FFh, INC_R0
    CJNE R1, #00Fh, INC_R1
    MOV R0, #CLEAR
    MOV R1, #CLEAR
    LJMP SAW_TOOTH_WAVE

INC_R1:
    ;LCALL ADC_IN
    INC R1
    MOV R0, #CLEAR
    LJMP SAW_TOOTH_WAVE

INC_R0:
    INC R0
    LJMP SAW_TOOTH_WAVE
;--------------------------------------------------;




ADC_IN:
;--------------------------------------------------;
;   Receive ADC input voltage from CH0
    MOV R3, A
    MOV R4, B

    MOV A,  #MAX1037_ADC_RD
    MOV P2, A
    MOV P1, #WRITE | I2CM_WR_DAT
    MOV P1, #CLEAR

    ; set the enable, ien, start and wr bit in the I2CM control register
    MOV P2, #CONTROL_Enable | CONTROL_IEN | CONTROL_STA | CONTROL_WR
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #CLEAR

    ; wait for interrupt
    LCALL WAIT_INT
    LCALL CHECK_ACK
    LCALL CLEAR_INT
    LCALL CHECK_INT_CLEARED

;   Read Result from I2CM
    MOV A,  #CLEAR
    MOV B,  #CONTROL_STO
    LCALL   READ_DATA
    MOV A, R3
    MOV B, R4
    RET
;--------------------------------------------------;








WRITE_ADDRESS:
;---------------------------------------------------
;       WRITE the address of the I2C Device
;---------------------------------------------------
; This routine writes the address of the I2C device
; To the I2C bus for reading or writing data.
; The Desired address is passed to the routine using A

    MOV P2, A
    MOV P1, #WRITE | I2CM_WR_DAT
    MOV P1, #CLEAR

    ; set the enable, ien, start and wr bit in the I2CM control register
    MOV P2, #CONTROL_Enable | CONTROL_IEN | CONTROL_STA | CONTROL_WR
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #CLEAR

    ; wait for interrupt
    LCALL WAIT_INT
    LCALL CHECK_ACK
    LCALL CLEAR_INT
    LCALL CHECK_INT_CLEARED
    RET


WRITE_DATA:
;---------------------------------------------------
;       WRITE data to the I2C Device
;---------------------------------------------------
; This routine writes data to the I2C device. The data byte to be written
; is passed to the routine using A. This routine is usually preceeded by
; a WRITE_ADDRESS routine call. B = Zero for normal write, = CONTROL_STO for write with stop condition.

    MOV P2, A
    MOV P1, #WRITE | I2CM_WR_DAT
    MOV P1, #CLEAR

    ; set the enable, ien and wr bit in the I2CM control register
    ORL B,  #CONTROL_Enable | CONTROL_IEN | CONTROL_WR
    MOV P2, B
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #CLEAR

    ; wait for interrupt
    LCALL WAIT_INT
    LCALL CHECK_ACK
    LCALL CLEAR_INT
    LCALL CHECK_INT_CLEARED
    RET


READ_DATA:
;--------------------------------------------------
;       Read data from the I2C Device
;--------------------------------------------------
; This routine reads received data from the RD_DAT register in the
; I2CM Master controller. This routine is usually preceeded by a WRITE_ADDRESS
; routine call. The read byte is returned in A. B = 0 for normal, = CONTROL_STO for stop.

    ; set the enable, ien and rd bit in the I2CM control register
    ORL B,  #CONTROL_Enable | CONTROL_IEN | CONTROL_RD | CONTROL_NACK
    MOV P2, B
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #CLEAR

    ; wait for interrupt
    LCALL WAIT_INT
    LCALL CLEAR_INT
    LCALL CHECK_INT_CLEARED

    MOV P1, #I2CM_RD_DAT
    ORL P1, #READ
    MOV A,  P2
    MOV P1, #CLEAR
    MOV P3, A

    MOV B,  #CLEAR
    ORL B,  #CONTROL_Enable | CONTROL_IEN | CONTROL_RD
    MOV P2, B
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #CLEAR

    ; wait for interrupt
    LCALL WAIT_INT
    LCALL CLEAR_INT
    LCALL CHECK_INT_CLEARED

   RET











WAIT_INT:
    MOV P1, #READ | I2CM_Status
    MOV A, P2
    MOV P1, #CLEAR
    RRC A
    JNC WAIT_INT
    RET

CHECK_INT_CLEARED:
    MOV P1, #READ | I2CM_Status
    MOV A, P2
    MOV P1, #CLEAR
    RRC A
    JC CHECK_INT_CLEARED
    RET

CLEAR_INT:
    MOV P2, #007h
    MOV P1, #WRITE | I2CM_Control
    MOV P1, #CLEAR | I2CM_Control
    MOV P1, #CLEAR
    RET

CHECK_ACK:
    ; read the [config] status register
    MOV P1, #READ | I2CM_Status
    MOV A, P2
    MOV P1, #CLEAR
    CLR C
    RRC A
    RRC A
    JNC CHECK_ACK
    RET


;-------------------------------------------------------------------
;   Interrupt subroutine
;-------------------------------------------------------------------
INT0SUBR:
    CLR  EA             ; disables all interrupts
    SETB EA             ; enable all interrupts
    RETI


   .END

