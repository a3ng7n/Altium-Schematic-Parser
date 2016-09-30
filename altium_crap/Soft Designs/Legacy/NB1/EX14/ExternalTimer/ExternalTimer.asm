;...............................................................................
PORT_BASE     .equ     0x4000
PORTA         .equ     PORT_BASE + 0
PORTB         .equ     PORT_BASE + 1
PORTC         .equ     PORT_BASE + 2
PORTD         .equ     PORT_BASE + 3
;..............................................................................

;..............................................................................
TIMER_BASE    .equ     0x8000
TCON_ID       .equ     TIMER_BASE + 0
TMOD_ID       .equ     TIMER_BASE + 1
TL0_ID        .equ     TIMER_BASE + 2
TL1_ID        .equ     TIMER_BASE + 3
TH0_ID        .equ     TIMER_BASE + 4
TH1_ID        .equ     TIMER_BASE + 5
;..............................................................................

;..............................................................................
KeyPadInput   .equ     PORTA    ;Input  Bits[3..0] = Key ID
                                ;       Bit 4      = Status
KeyPadControl .equ     PORTA    ;Output Bit 0      = Reset
;..............................................................................

;..............................................................................
LCD_Data      .equ     PORTB    ;
LCD_Status    .equ     PORTB    ;Input  Bit 0      = Busy
LCD_Address   .equ     PORTC    ;Output Bits[3..0] = Offset on Line
                                ;       Bit 4      = Line No.
LCD_Control   .equ     PORTD    ;Output Bit 0      = Strobe
;..............................................................................

;..............................................................................
CurrentChar   .equ     0x0075
CounterChar   .equ     0x0076
CurrentMsgO   .equ     0x0077
CurrentPos    .equ     0x0078
DoubleChar    .equ     0x0079

KeyPadChar    .equ     0x007B
FPressDet     .equ     0x007C

ExtTCounter   .equ     0x007D
IntTCounter   .equ     0x007F

TimerRelEn    .equ     0x0080
;..............................................................................

;..............................................................................
; bit positions
BP0           .equ     0x00
BP1           .equ     0x01
BP2           .equ     0x02
BP3           .equ     0x03
BP4           .equ     0x04
BP5           .equ     0x05
BP6           .equ     0x06
BP7           .equ     0x07
;..............................................................................


;...............................................................................
    .Section Text1, Code, at( 0000H )
        JP      MainProc
;...............................................................................

;...............................................................................
    .Section Text3, Code, at( 0066H )
        JP      NonMaskableInt
;...............................................................................

;...............................................................................
    .Section Text4, Code, at( 0300H )
;...............................................................................
MainProc:
;...............................................................................
    LD      SP,0x1FFF
    CALL    ResetKeypad

    LD      A,0x00
    LD      (LCD_Control),A

    LD      A,0x00
    LD      (FPressDet),A
    LD      (ExtTCounter),A

    LD      A,0xFF
    LD      (IntTCounter),A

    LD      E,0x00
    CALL    Msg_To_Display_Base_On_E
    JP      MainLoop
;...............................................................................

;...............................................................................
MainLoop:
;...............................................................................
    CALL    ScanKeyPadForFirstChar
    LD      A,(FPressDet)
    BIT     BP0,A
    JP      Z,MainLoop
    CALL    ClearLCD
    LD      E,0x01
    CALL    Msg_To_Display_Base_On_E
    CALL    Send_Char_On_Pos_17
    CALL    Send_Char_On_Pos_18
    CALL    Timer0Mode1Init
    CALL    Timer0Reload
    CALL    Timer0Start
    MainLoopF:
    CALL    CheckKeyPadCharFor1or2or3or4
    JP      MainLoopF
;...............................................................................

;...............................................................................
NonMaskableInt:
;...............................................................................
    PUSH    BC
    PUSH    AF
    PUSH    DE
    PUSH    IX
    LD      A,(TimerRelEn)
    CP      0x00
    JP      NZ,NonMaskableIntReloadDis
    CALL    Timer0Reload
NonMaskableIntReloadDis:
    CALL    Timer0Start
    LD      A,(IntTCounter)
    DEC     A
    JP      NZ,NonMaskableIntRET
    LD      (IntTCounter),A
    CALL    DecCounter
    CALL    Send_Char_On_Pos_17
    CALL    Send_Char_On_Pos_18
    POP     IX
    POP     DE
    POP     AF
    POP     BC
    RETN
NonMaskableIntRET:
    LD      (IntTCounter),A
    POP     IX
    POP     DE
    POP     AF
    POP     BC
    RETN
;...............................................................................

;...............................................................................
Timer0Mode1Init:
;...............................................................................
    PUSH    AF
    LD      A,0x00
    LD      (TimerRelEn),A
    LD      A,0x01
    LD      (TMOD_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0Mode1InitExtClk:
;...............................................................................
    PUSH    AF
    LD      A,0x00
    LD      (TimerRelEn),A
    LD      A,0x0D
    LD      (TMOD_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0Mode2Init:
;...............................................................................
    PUSH    AF
    LD      A,0x01
    LD      (TimerRelEn),A
    LD      A,0x02
    LD      (TMOD_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0Mode2InitExtClk:
;...............................................................................
    PUSH    AF
    LD      A,0x01
    LD      (TimerRelEn),A
    LD      A,0x0E
    LD      (TMOD_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0Reload:
;...............................................................................    
    PUSH    AF
    LD      A,0x77
    LD      (TL0_ID),A
    LD      A,0xFE
    LD      (TH0_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0ReloadMode2:
;...............................................................................
    PUSH    AF
    LD      A,0x01
    LD      (TL0_ID),A
    LD      A,0x01
    LD      (TH0_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0Start:
;...............................................................................
    PUSH    AF
    LD      A,0x10
    LD      (TCON_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
Timer0Stop:
;...............................................................................
    PUSH    AF
    LD      A,0x00
    LD      (TCON_ID),A
    POP      AF
    RET
;...............................................................................

;...............................................................................
ResetKeypad:
;...............................................................................
    PUSH    AF
    LD      A,0x01
    LD      (KeyPadControl),A
    LD      A,0x00
    LD      (KeyPadControl),A
    POP     AF
    RET
;...............................................................................

;...............................................................................
ScanKeyPadForFirstChar:
;...............................................................................
    PUSH    AF
    LD      A,(KeyPadInput)
    LD      (KeyPadChar),A
    BIT     BP4,A
    JP      Z,ScanKeyPadForFirstCharRET
    CALL    ResetKeypad
    LD      A,0x01
    LD      (FPressDet),A
    ScanKeyPadForFirstCharRET:
    POP      AF
    RET
;...............................................................................

;...............................................................................
CheckKeyPadCharFor1or2or3or4:
;...............................................................................
    PUSH    AF
    LD      A,(KeyPadInput)
    LD      (KeyPadChar),A
    BIT      BP4,A
    JP       Z,CheckKeyPadCharFor1or2or3or4RET
    CALL     ResetKeypad
    AND      A,0x0F
    CP       0x00
    JP       Z,CheckKeyPadCharFor1
    CP       0x01
    JP       Z,CheckKeyPadCharFor2
    CP       0x02
    JP       Z,CheckKeyPadCharFor3
    CP       0x04
    JP       Z,CheckKeyPadCharFor4
    POP      AF
    RET
CheckKeyPadCharFor1:
    CALL     ClearLCD
    LD       A,0x00
    LD       (ExtTCounter),A
    LD       A,0xFF
    LD       (IntTCounter),A
    LD       E,0x01
    CALL     Msg_To_Display_Base_On_E
    CALL     Send_Char_On_Pos_17
    CALL     Send_Char_On_Pos_18
    CALL     Timer0Stop
    CALL     Timer0Mode1Init
    CALL     Timer0Reload
    CALL     Timer0Start
    POP      AF
    RET
CheckKeyPadCharFor2:
    CALL     ClearLCD
    LD       A,0x00
    LD       (ExtTCounter),A
    LD       A,0xFF
    LD       (IntTCounter),A
    LD       E,0x02
    CALL     Msg_To_Display_Base_On_E
    CALL     Send_Char_On_Pos_17
    CALL     Send_Char_On_Pos_18
    CALL     Timer0Stop
    CALL     Timer0Mode1InitExtClk
    CALL     Timer0Reload
    CALL     Timer0Start
    POP      AF
    RET
CheckKeyPadCharFor3:
    CALL     ClearLCD
    LD       A,0x00
    LD       (ExtTCounter),A
    LD       A,0xFF
    LD       (IntTCounter),A
    LD       E,0x03
    CALL     Msg_To_Display_Base_On_E
    CALL     Send_Char_On_Pos_17
    CALL     Send_Char_On_Pos_18
    CALL     Timer0Stop
    CALL     Timer0Mode2Init
    CALL     Timer0ReloadMode2
    CALL     Timer0Start
    POP      AF
    RET
CheckKeyPadCharFor4:
    CALL     ClearLCD
    LD       A,0x00
    LD       (ExtTCounter),A
    LD       A,0xFF
    LD       (IntTCounter),A
    LD       E,0x04
    CALL     Msg_To_Display_Base_On_E
    CALL     Send_Char_On_Pos_17
    CALL     Send_Char_On_Pos_18
    CALL     Timer0Stop
    CALL     Timer0Mode2InitExtClk
    CALL     Timer0ReloadMode2
    CALL     Timer0Start
CheckKeyPadCharFor1or2or3or4RET:
    POP      AF
    RET
;...............................................................................

;...............................................................................
WaitOnLCD:
    PUSH     AF
WaitOnLCD1:
    LD       A,(LCD_Status)
    AND      A,0x01
    CP       0x00
    JP       NZ,WaitOnLCD1
    POP      AF
    RET
;...............................................................................

;...............................................................................
ClearLCD:
;...............................................................................
    PUSH    BC
    LD      IX,Lookup_Space
    LD      B,0x20
    ClearLCD_Next_Pos:
    LD      A,(IX+0)
    LD      (CurrentChar),A
    LD      A,B
    LD      (CurrentPos),A
    CALL    Send_Char_On_Spec_Pos_Of_LCD
    DEC     B
    JP      NZ,ClearLCD_Next_Pos
    ClearLCD_RET:
    POP     BC
    RET
;...............................................................................

;...............................................................................
DecCounter:
    LD       A,(ExtTCounter)
    INC      A
    LD       B,A
    AND      A,0x0F
    CP       0x0A
    JP       NZ,DecCounterJRET
    LD       A,(ExtTCounter)
    AND      A,0xF0
    RRCA
    RRCA
    RRCA
    RRCA
    INC      A
    RLCA
    RLCA
    RLCA
    RLCA
    CP       0xA0
    JP       NZ,DecCounterRET
    LD       A,0x00
DecCounterRET:
    LD       (ExtTCounter),A
    RET
DecCounterJRET:
    LD       A,B
    LD       (ExtTCounter),A
    RET
;...............................................................................

;...............................................................................

;...............................................................................
Send_Char_On_Pos_17:
;...............................................................................
    PUSH    BC
    LD      IX,Lookup_Hex
    LD      A,0x10
    LD      (CurrentPos),A
    LD      A,(ExtTCounter)
    RRCA
    RRCA
    RRCA
    RRCA
    AND     A,0x0F
    LD      C,A
    LD      B,0
    ADD     IX,BC
    LD      A,(IX+0)
    LD      (CurrentChar),A
    CALL    Send_Char_On_Spec_Pos_Of_LCD
    POP     BC
    RET
;...............................................................................

;...............................................................................
Send_Char_On_Pos_18:
;...............................................................................
    PUSH    BC
    LD      IX,Lookup_Hex
    LD      A,0x11
    LD      (CurrentPos),A
    LD      A,(ExtTCounter)
    AND     A,0x0F
    LD      C,A
    LD      B,0
    ADD     IX,BC
    LD      A,(IX+0)
    LD      (CurrentChar),A
    CALL    Send_Char_On_Spec_Pos_Of_LCD
    POP     BC
    RET
;...............................................................................

;...............................................................................
Msg_To_Display_Base_On_E:
;...............................................................................
    PUSH    DE
    LD      IX,Lookup_Messages_Offset
    CALL    Read_Next_Character_From_Table
    LD      A,(CurrentChar)
    LD      (CurrentMsgO),A
    POP     DE
    CALL    Send_Msg_On_LCD
    RET
;...............................................................................

;...............................................................................
Read_Next_Character_From_Table:
;...............................................................................
    LD      D,0
    ADD     IX,DE
    LD      A,(IX+0)
    LD      (CurrentChar),A
    RET
;...............................................................................

;...............................................................................
Send_Msg_On_LCD:
;...............................................................................
    PUSH    BC
    LD      IX,Lookup_Messages_Length
    CALL    Read_Next_Character_From_Table
    LD      A,(CurrentChar)
    LD      B,A
    LD      C,A
    LD      A,(CurrentMsgO)
    LD      E,A
    NextCharToDispaly:
    LD      IX,Lookup_Messages
    CALL    Read_Next_Character_From_Table
    LD      A,C
    SUB     B
    CALL    WaitOnLCD
    LD      (LCD_Address),A
    LD      A,(CurrentChar)
    LD      (LCD_Data),A
    LD      A,0x01
    LD      (LCD_Control),A
    LD      A,0x00
    LD      (LCD_Control),A
    INC     E
    DEC     B
    JP      NZ,NextCharToDispaly
    POP     BC
    RET
;...............................................................................

;...............................................................................
Send_Char_On_Spec_Pos_Of_LCD:
;...............................................................................
    CALL    WaitOnLCD
    LD      A,(CurrentPos)
    LD      (LCD_Address),A
    LD      A,(CurrentChar)
    LD      (LCD_Data),A
    LD      A,0x01
    LD      (LCD_Control),A
    LD      A,0x00
    LD      (LCD_Control),A
    RET
;...............................................................................

;...............................................................................
Lookup_Messages_Length:
;...............................................................................
    .db 0x1F
    .db 0x0E
    .db 0x0E
    .db 0x0E
    .db 0x0E
;...............................................................................

;...............................................................................
Lookup_Messages_Offset:
;...............................................................................
    .db 0x00
    .db 0x1F
    .db 0x2D
    .db 0x3B
    .db 0x49
;...............................................................................

;...............................................................................
Lookup_Messages:
;...............................................................................
    .db 'W'
    .db 'i'
    .db 's'
    .db 'h'
    .db 'b'
    .db 'o'
    .db 'n'
    .db 'e'
    .db ' '
    .db 'e'
    .db 'x'
    .db 'a'
    .db 'm'
    .db 'p'
    .db 'l'
    .db 'e'
    .db 'E'
    .db 'x'
    .db 't'
    .db 'e'
    .db 'r'
    .db 'n'
    .db 'a'
    .db 'l'
    .db ' '
    .db 'T'
    .db 'i'
    .db 'm'
    .db 'e'
    .db 'r'
    .db '.'
    ;0x1FH
    .db 'M'
    .db 'o'
    .db 'd'
    .db 'e'
    .db '1'
    .db ' '
    .db 'T'
    .db 'i'
    .db 'm'
    .db 'e'
    .db 'r'
    .db ' '
    .db ' '
    .db ':'
    ;0x2DH
    .db 'M'
    .db 'o'
    .db 'd'
    .db 'e'
    .db '1'
    .db ' '
    .db 'C'
    .db 'o'
    .db 'u'
    .db 'n'
    .db 't'
    .db 'e'
    .db 'r'
    .db ':'
    ;0x3BH
    .db 'M'
    .db 'o'
    .db 'd'
    .db 'e'
    .db '2'
    .db ' '
    .db 'T'
    .db 'i'
    .db 'm'
    .db 'e'
    .db 'r'
    .db ' '
    .db ' '
    .db ':'
    ;0x49H
    .db 'M'
    .db 'o'
    .db 'd'
    .db 'e'
    .db '2'
    .db ' '
    .db 'C'
    .db 'o'
    .db 'u'
    .db 'n'
    .db 't'
    .db 'e'
    .db 'r'
    .db ':'
;...............................................................................

;...............................................................................
Lookup_Space:
;...............................................................................
    .db ' '
;...............................................................................

;...............................................................................
Lookup_Hex:
;...............................................................................
    .db '0'
    .db '1'
    .db '2'
    .db '3'
    .db '4'
    .db '5'
    .db '6'
    .db '7'
    .db '8'
    .db '9'
    .db 'A'
    .db 'B'
    .db 'C'
    .db 'D'
    .db 'E'
    .db 'F'
;...............................................................................


END
