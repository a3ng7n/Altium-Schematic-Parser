;...............................................................................
MainCount                   .equ     3FH
CountInner                  .equ     40H
CountOuter                  .equ     41H
CountInnerS                 .equ     42H
CountInnerLCD               .equ     43H

CurDispAddr                 .equ     46H
CharacterToSent             .equ     4BH

CanIRStatus                 .equ     35H
CanD1R                      .equ     36H
CanIDR                      .equ     37H
P1FD1R                      .equ     38H
;...............................................................................

    .Section Text, Code
;    .offset 0000H
MainProc:
;...............................................................................
    LJMP    INIT

;...............................................................................

;    .offset 0003H
    LJMP    INT0SUBT

;    .offset 0050H
INIT:
    MOV   DPH,#00H
    MOV   DPL,#00H
    MOV   PSW,#00H

    MOV   MainCount, #00H
    CLR   EA      ; Disables all interrupts
    
;...............................................................................
MainLoop :
;...............................................................................

    LCALL WAIT_LCD
    
    LCALL   InitCAN         ; Initialize a CAN module
    SETB    EX0             ; Enable int0 interrupt
    SETB    EA              ; Enable all interrupts

TransmitVCan:


    LCALL   DISPLAY_MESSAGE_TX
    LCALL   DISPLAY_MESSAGE_RX
    LCALL   SetFrameToTransmit
    LCALL   TransmitFrame
    LCALL   VeryLongDelay
    
    MOV     A, #00AH
    CJNE    A, MainCount, TransmitVCan
    MOV     MainCount, #000H
    SJMP    TransmitVCan
;...............................................................................

;...............................................................................
InitCAN:
;...............................................................................
    ; LOAD 01 to the CR = MODE register
    MOV     DPH,#00H
    MOV     DPL,#00H
    MOV     A, #01H
    MOVX    @DPTR, A
    
RST_MOD: 
    MOV     DPL, #00H
    MOVX    A, @DPTR
    JNB     ACC.0, RST_MOD  
    
    ; LOAD FF to the ACCMASK0 register
    MOV     DPL, #014H
    MOV     A,#0FFH
    MOVX    @DPTR,A 
    ; LOAD FF to the ACCMASK1 register
    MOV     DPL, #015H
    MOV     A,#0FFH
    MOVX    @DPTR,A
    ; LOAD FF to the ACCMASK2 register
    MOV     DPL, #016H
    MOV     A,#0FFH
    MOVX    @DPTR,A
    ; LOAD FF to the ACCMASK3 register
    MOV     DPL, #017H
    MOV     A,#0FFH
    MOVX    @DPTR,A
    ; LOAD 9F to the CDR register
    MOV     DPL,#1FH
    MOV     A, #0C0H
    MOVX    @DPTR, A
    ; LOAD 06 to the BTR0 register
    MOV     DPL,#06H
    MOV     A, #09H ; 40 MHz 125kb/s
    MOVX    @DPTR, A
    ; LOAD 07 to the BTR1 register
    MOV     DPL,#07H
    MOV     A, #1CH ; 40 MHz 125kb/s
    MOVX    @DPTR, A    
    ; LOAD 1A to the OCR register
    MOV     DPL,#08H
    MOV     A, #01AH
    MOVX    @DPTR, A
    ; LOAD 04 to the CR = MODE register
    MOV     DPL,#00H
    MOV     A, #00H ;
    MOVX    @DPTR, A
    
    ; LOAD 8F to the CDR register
    MOV     DPL,#1FH
    MOV     A, #0C0H
    MOVX    @DPTR, A
    ; Set acceptance mask register 0
    MOV     DPL,#14H
    MOV     A, #0FFH
    MOVX    @DPTR, A
    ; Set acceptance mask register 1
    MOV     DPL,#15H
    MOV     A, #0FFH
    MOVX    @DPTR, A
    ; Set acceptance mask register 2
    MOV     DPL,#16H
    MOV     A, #0FFH
    MOVX    @DPTR, A
    ; Set acceptance mask register 3
    MOV     DPL,#17H
    MOV     A, #0FFH
    MOVX    @DPTR, A
    ; Enable Receive interrupt
    MOV     DPL,#04H
    CLR     A
    ORL     A, #01H
    MOVX    @DPTR, A
    ; LOAD 02 to the CR = MODE register "Listen Only Mode"
    MOV     DPL,#00H
    MOV     A, #00H
    MOVX    @DPTR, A
    
OP_MOD: 
    MOV     DPL, #00H
    MOVX    A, @DPTR
    JB      ACC.0, OP_MOD
    


    RET
;...............................................................................

;...............................................................................
INT0SUBT:
;...............................................................................
    CLR   EA      ; Disables all interrupts
    PUSH    DPL
    PUSH    DPH
    PUSH    ACC
    ;read IR register
    MOV     DPH,#00H
    MOV     DPL,#03H
    MOVX    A, @DPTR
    MOV     CanIRStatus, A
    MOV     A, CanIRStatus
    ANL     A, #01H
    JZ      RFRETIR
ReadRecData:    
    ;if receive interrupt active
    ; read data from receive buffer and put data1 into CanD1R register
    LCALL   CanReadReceiveBuffers
    MOV     A, CanIDR
    XRL     A, #55H
    JNZ     RFRETI
    MOV     P1FD1R, CanD1R
RFRETI:
    ; release receive buffer
    MOV     DPL,#01H
    MOV     A, #04H
    MOVX    @DPTR, A
RFRETIR:
    MOV     DPL,#02H
    MOVX    A, @DPTR
    ANL     A, #01H
    JNZ     ReadRecData
    POP     ACC
    POP     DPH
    POP     DPL
    LCALL   DISPLAY_MESSAGE_RX
    CLR     IE0
    SETB    EA  ; Enable all interrupts
    RETI
;...............................................................................

;...............................................................................
SetFrameToTransmit:
;...............................................................................

CHK_TR1:    
    MOV     DPH,#00H
    MOV     DPL,#02H
    MOVX    A,@DPTR
    JNB ACC.2,CHK_TR1
    
    ; LOAD 03 to the TXB0 register
    MOV     DPH,#00H
    MOV     DPL,#10H
    MOV     A, #03H
    MOVX    @DPTR, A
    ; LOAD 55 to the TXB1 register
    MOV     DPL,#11H
    MOV     A, #55H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXB2 register
    MOV     DPL,#12H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD0 register
    MOV     DPL,#13H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 55 to the TXD1 register
    MOV     DPL,#14H
    MOV     A, MainCount
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD2 register
    MOV     DPL,#15H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD3 register
    MOV     DPL,#16H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD4 register
    MOV     DPL,#17H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD5 register
    MOV     DPL,#18H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD6 register
    MOV     DPL,#19H
    MOV     A, #00H
    MOVX    @DPTR, A
    ; LOAD 00 to the TXD7 register
    MOV     DPL,#1AH
    MOV     A, #00H
    MOVX    @DPTR, A
    INC     MainCount
    RET
;...............................................................................

;...............................................................................
TransmitFrame:
;...............................................................................
    ; LOAD 01 to the CMR register
    MOV     DPH,#00H
    MOV     DPL,#01H
    MOV     A, #01H
    MOVX    @DPTR, A
    RET
;...............................................................................
;...............................................................................
CanReadReceiveBuffers:
;...............................................................................
    ; read data from RXD1 register
    MOV     DPH,#00H
    MOV     DPL ,#14H
    MOVX    A, @DPTR
    MOV     CanD1R, A
    ; read a part of identifier from a RXB1 register
    MOV     DPL ,#11H
    MOVX    A, @DPTR
    MOV     CanIDR, A
    RET
;...............................................................................
;...............................................................................
DISPLAY_MESSAGE_RX:
    LCALL WAIT_LCD
    ; set-up the 'strobe' bit for the LCD driver
    MOV P0, #052h  ; 'R'
    MOV P3, #0F0h
    MOV P3, #0D0h
    MOV P3, #0F0h
    LCALL WAIT_LCD

    MOV P0, #065h  ; 'e'
    MOV P3, #0F1h
    MOV P3, #0D1h
    MOV P3, #0F1h
    LCALL WAIT_LCD
        
    MOV P0, #063h  ; 'c'
    MOV P3, #0F2h
    MOV P3, #0D2h
    MOV P3, #0F2h
    LCALL WAIT_LCD
        
    MOV P0, #065h  ; 'e'
    MOV P3, #0F3h
    MOV P3, #0D3h
    MOV P3, #0F3h
    LCALL WAIT_LCD
        
    MOV P0, #069h  ; 'i'
    MOV P3, #0F4h
    MOV P3, #0D4h
    MOV P3, #0F4h
    LCALL WAIT_LCD
        
    MOV P0, #076h  ; 'v'
    MOV P3, #0F5h
    MOV P3, #0D5h
    MOV P3, #0F5h
    LCALL WAIT_LCD
        
    MOV P0, #065h  ; 'e'
    MOV P3, #0F6h
    MOV P3, #0D6h
    MOV P3, #0F6h
    LCALL WAIT_LCD
        
    MOV P0, #03Ah  ; ':'
    MOV P3, #0F7h
    MOV P3, #0D7h
    MOV P3, #0F7h
    LCALL WAIT_LCD
        
    MOV P0, #020h  ; ' '
    MOV P3, #0F8h
    MOV P3, #0D8h
    MOV P3, #0F8h
    LCALL WAIT_LCD
        
    MOV P0, #020h  ; ' '
    MOV P3, #0F9h
    MOV P3, #0D9h
    MOV P3, #0F9h
    LCALL WAIT_LCD
        
    MOV A, P1FD1R
    ORL A, #030h
    MOV P0, A    ; the value of the counter
    MOV P3, #0FAh
    MOV P3, #0DAh
    MOV P3, #0FAh
    JNC  WAIT_LCD        
        
    RET

WAIT_LCD:
    MOV A, P3
    CLR C
    RRC A
    JNC  WAIT_LCD
    RET

DISPLAY_MESSAGE_TX:
    LCALL WAIT_LCD
    ; set-up the 'strobe' bit for the LCD driver
    MOV P0, #054h   ; 'T'
    MOV P3, #0E0h
    MOV P3, #0C0h
    MOV P3, #0E0h
    LCALL WAIT_LCD

    MOV P0, #072h   ; 'r'
    MOV P3, #0E1h
    MOV P3, #0C1h
    MOV P3, #0E1h
    LCALL WAIT_LCD
        
    MOV P0, #061h   ; 'a'
    MOV P3, #0E2h
    MOV P3, #0C2h
    MOV P3, #0E2h
    LCALL WAIT_LCD
        
    MOV P0, #06Eh   ; 'n'
    MOV P3, #0E3h
    MOV P3, #0C3h
    MOV P3, #0E3h
    LCALL WAIT_LCD
        
    MOV P0, #073h   ; 's'
    MOV P3, #0E4h
    MOV P3, #0C4h
    MOV P3, #0E4h
    LCALL WAIT_LCD
        
    MOV P0, #06Dh   ; 'm'
    MOV P3, #0E5h
    MOV P3, #0C5h
    MOV P3, #0E5h
    LCALL WAIT_LCD
        
    MOV P0, #069h   ; 'i'
    MOV P3, #0E6h
    MOV P3, #0C6h
    MOV P3, #0E6h
    LCALL WAIT_LCD
        
    MOV P0, #074h   ; 't'
    MOV P3, #0E7h
    MOV P3, #0C7h
    MOV P3, #0E7h
    LCALL WAIT_LCD
        
    MOV P0, #03Ah   ; ':'
    MOV P3, #0E8h
    MOV P3, #0C8h
    MOV P3, #0E8h
    LCALL WAIT_LCD
        
    MOV P0, #020h   ; ' '
    MOV P3, #0E9h
    MOV P3, #0C9h
    MOV P3, #0E9h
    LCALL WAIT_LCD
        
    MOV A, MainCount
    ORL A, #030h
    MOV P0, A       ; the value of the counter
    MOV P3, #0EAh
    MOV P3, #0CAh
    MOV P3, #0EAh
    LCALL  WAIT_LCD
        
    RET

;...............................................................................

;...............................................................................
LongDelay:
;...............................................................................
    MOV   CountOuter, #0FFH
LoopOuter:
       MOV   CountInner, #0FFH
LoopInner:
         DEC   CountInner
         MOV A, CountInner
       JNZ   LoopInner
       DEC  CountOuter
       MOV A, CountOuter
    JNZ   LoopOuter
    RET
;...............................................................................

;...............................................................................
VeryLongDelay:
;...............................................................................
    LCALL    LongDelay
    LCALL    LongDelay
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay
    LCALL    LongDelay
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay
    LCALL    LongDelay
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    LCALL    LongDelay  ; 40MHz ; comment this line if 16 MHz clock
    RET
;...............................................................................

    .END
