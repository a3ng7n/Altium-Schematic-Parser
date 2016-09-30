;-------------------------------------------------------------------------------
;  TSK165_PWM.ASM (C)2005 Altium Ltd.
;-------------------------------------------------------------------------------
; This example demonstrates the use of the TSK165 Microcontroller with the
; WB_PWM8 and in the EP1S10F672C6ES device on the Parallax
; Stratix 672 SmartPack
;-------------------------------------------------------------------------------


; Equates:
;-------------------------------------------------------------------------------
; Port A is used for remote control via LiveDesign using the 8-Bit Digital IO.
; Ports B and C are used for the data and control signals that drive the two
; WB_PWM8 components.
LD_IO     .equ       PORTA
PW_DATA   .equ       PORTB
PW_CONT   .equ       PORTC

; Wishbone Control Bits
STB1      .equ       0x80 ; PWM1 strobe
STB2      .equ       0x40 ; PWM2 strobe
CYC       .equ       0x20 ; Bus Cycle flag
WE        .equ       0x10 ; Write Enable
RD_SEL    .equ       0x08 ; 0 = read from PWM1, 1 = read from PWM2
PWM_RST   .equ       0x04 ; Use this to reset both PWM devices.
ADR1      .equ       0x02 ; Address Bits
ADR0      .equ       0x01

STB1_b    .equ       7
STB2_b    .equ       6

; PWM Register Addresses
PWCON     .equ       0x03 ; Control Register
PWPHI     .equ       0x02 ; Pre-Scaler Reload Registers
PWPLO     .equ       0x01
PWMRG     .equ       0x00 ; PWM Value Register

; PWCON Control Bit Masks
PWIN      .equ       0x7F ; PWM Interrupt
PRIN      .equ       0xBF ; Pre-Scaler Interrupt

PWI       .equ       0x80
PRI       .equ       0x40
PFL1      .equ       0x20 ; Spare bits
PFL0      .equ       0x10
PWIE      .equ       0x08 ; PWM Interrupt Enable
PRIE      .equ       0x04 ; Pre-Scaler Interrupt Enable
PIEN      .equ       0x02 ; Global Interrupt Enable
PWEN      .equ       0x01 ; PWM Peripheral Enable

PWINT     .equ       7
PRINT     .equ       6

; PWM Interrupts
PW1INT    .equ       6
PW2INT    .equ       7

; Reserved Variables:
;-------------------------------------------------------------------------------
          .section   variables,   data
PWM1_UCNT .dsb       1
PWM2_UCNT .dsb       1
RLOAD_L1  .dsb       1
RLOAD_L2  .dsb       1
TEMPVAR   .dsb       1
FLAGS     .dsb       1
PWM1_STAT .dsb       1
PWM2_STAT .dsb       1

INC_DEC1  .equ       0
INC_DEC2  .equ       1

; Main Program:
;-------------------------------------------------------------------------------
          .section   mainprogram, code

INIT:     MOVLW      0x01
          MOVWF      PW_DATA
          MOVLW      STB1 | STB2 | CYC | WE | PWPHI
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          MOVLW      PWEN | PWIE | PIEN ;| PRIE
          MOVWF      PW_DATA
          MOVLW      STB1 | STB2 | CYC | WE | PWCON ; Start both PWMs
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          GJMP       MAIN



; Interrupt Service Routines (Software based Interrupts) for PWM 1
;-------------------------------------------------------------------------------
PWI1:     MOVF       PW_DATA, W
          ANDLW      PWIN
          MOVWF      PW_DATA
          MOVLW      STB1 | CYC | WE | PWCON ; Clear PWM1 Interrupt
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          DECFSZ     RLOAD_L1, F
          RETLW      0x00
          MOVF       PWM1_STAT, W
          MOVWF      RLOAD_L1
          MOVF       PWM1_UCNT, W
          MOVWF      PW_DATA
          MOVLW      STB1 | CYC | WE | PWMRG ; Reload the new PWM value
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          BTFSC      FLAGS, INC_DEC1
          GOTO       DOWN1
          INCFSZ     PWM1_UCNT, F            ; We count down and then we count
          GOTO       RETURN1                 ; up, fading in and out.
          BSF        FLAGS, INC_DEC1
DOWN1:    DECFSZ     PWM1_UCNT, F
          GOTO       RETURN1
          BCF        FLAGS, INC_DEC1
RETURN1:  RETLW      0x00

PRI1:     MOVF       PW_DATA, W
          ANDLW      PRIN
          MOVWF      PW_DATA
          MOVLW      STB1 | CYC | WE | PWCON ; Clear PWM1 Prescaler Interrupt
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          MOVF       LD_IO, W
          ANDLW      0x0F                    ; Use Low Nibble for Pre-scaler
          MOVWF      PWM1_STAT
          MOVWF      LD_IO
          RETLW      0x00

INT1:     MOVLW      STB1 | CYC | PWCON      ; Read PWCON register
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          BTFSC      PW_DATA, PWINT
          CALL       PWI1
          BTFSC      PW_DATA, PRINT
          CALL       PRI1
          RETLW      0x00



; Interrupt Service Routines (Software based Interrupts) for PWM 2
;-------------------------------------------------------------------------------
PWI2:     MOVF       PW_DATA, W
          ANDLW      PWIN
          MOVWF      PW_DATA
          MOVLW      STB2 | CYC | WE | PWCON ; Clear PWM1 Interrupt
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          DECFSZ     RLOAD_L2, F
          RETLW      0
          MOVF       PWM2_STAT, W
          MOVWF      RLOAD_L2
          MOVF       PWM2_UCNT, W
          MOVWF      PW_DATA
          MOVLW      STB2 | CYC | WE | PWMRG ; Reload the new PWM value
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          BTFSC      FLAGS, INC_DEC2
          GOTO       DOWN2
          INCFSZ     PWM2_UCNT, F            ; We count down and then we count
          GOTO       RETURN2                 ; up, fading in and out.
          BSF        FLAGS, INC_DEC2
DOWN2:    DECFSZ     PWM2_UCNT, F
          GOTO       RETURN2
          BCF        FLAGS, INC_DEC2
RETURN2:  RETLW      0x00

PRI2:     MOVF       PW_DATA, W
          ANDLW      PRIN
          MOVWF      PW_DATA
          MOVLW      STB2 | CYC | WE | PWCON ; Clear PWM2 Prescaler Interrupt
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          MOVF       LD_IO, W                ; Use High Nibble for Pre-scaler
          ANDLW      0xF0
          MOVWF      TEMPVAR
          SWAPF      TEMPVAR, W
          MOVWF      PWM2_STAT
          MOVWF      LD_IO
          RETLW      0x00

INT2:     MOVLW      STB2 | CYC | RD_SEL | PWCON
          MOVWF      PW_CONT
          MOVLW      0x00
          MOVWF      PW_CONT
          BTFSC      PW_DATA, PWINT
          CALL       PWI2
          BTFSC      PW_DATA, PRINT
          CALL       PRI2
          RETLW      0x00



; Main Program Loop - Checks for Interrupt Status on both PWM devices
;-------------------------------------------------------------------------------
MAIN:     BTFSC      PW_CONT, PW1INT         ;
          CALL       INT1
          BTFSC      PW_CONT, PW2INT         ;
          CALL       INT2
          GJMP       MAIN
