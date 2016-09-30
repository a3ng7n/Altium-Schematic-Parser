{..............................................................................}
{ Summary Conversion of Schematic Types To Strings and Vice Versa              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Const
    CR = #13+#10;
{..............................................................................}

{..............................................................................}
Function BooleanToStr(Bool: Boolean): String;
Begin
    If Bool Then Result := 'TRUE'
            Else Result := 'FALSE';
End;
{..............................................................................}

{..............................................................................}
Function StrToBoolean(Str: String): Boolean;
Begin
    Str := UpperCase(Str);
    If Str = 'TRUE' Then Result := True
                    Else Result := False;
End;
{..............................................................................}

{..............................................................................}
Function PinElectricalToStr(PinElectrical: TPinElectrical): String;
Begin
    Case PinElectrical Of
        eElectricInput         : Result := 'INPUT'          ;
        eElectricIO            : Result := 'IO'             ;
        eElectricOutput        : Result := 'OUTPUT'         ;
        eElectricOpenCollector : Result := 'OPEN COLLECTOR' ;
        eElectricPassive       : Result := 'PASSIVE'        ;
        eElectricHiZ           : Result := 'HIZ'            ;
        eElectricOpenEmitter   : Result := 'EMITTER'        ;
        eElectricPower         : Result := 'POWER'          ;
    Else
        Result := 'UNKNOW PIN ELECTRICAL';
    End;
End;
{..............................................................................}

{..............................................................................}
Function StrToPinElectrical(Str: String): TPinElectrical;
Begin
    Str := UpperCase(Str);
    If Str = 'INPUT'          Then Result := eElectricInput         Else
    If Str = 'IO'             Then Result := eElectricIO            Else
    If Str = 'OUTPUT'         Then Result := eElectricOutput        Else
    If Str = 'OPEN COLLECTOR' Then Result := eElectricOpenCollector Else
    If Str = 'PASSIVE'        Then Result := eElectricPassive       Else
    If Str = 'HIZ'            Then Result := eElectricHiZ           Else
    If Str = 'EMITTER'        Then Result := eElectricOpenEmitter   Else
    If Str = 'POWER'          Then Result := eElectricPower         Else
                                   Result := eElectricPassive;
End;
{..............................................................................}   

{..............................................................................}
Function RotationBy90ToStr(RotationBy90: TRotationBy90): String;
Begin
    Case RotationBy90 Of
        eRotate0   : Result := '0 DEGREES'   ;
        eRotate90  : Result := '90 DEGREES'  ;
        eRotate180 : Result := '180 DEGREES' ;
        eRotate270 : Result := '270 DEGREES' ;
    Else
        Result := 'UNKNOW ROTATION BY 90';
    End;
End;
{..............................................................................}

{..............................................................................}
Function StrToRotationBy90(Str: String): TRotationBy90;
Begin
    Str := UpperCase(Str);
    If Str = '0 DEGREES'   Then Result := eRotate0   Else
    If Str = '90 DEGREES'  Then Result := eRotate90  Else
    If Str = '180 DEGREES' Then Result := eRotate180 Else
    If Str = '270 DEGREES' Then Result := eRotate270 Else
                                Result := eRotate0;
End;
{..............................................................................}

{..............................................................................}
Function IeeeSymbolToStr(IeeeSymbol: TIeeeSymbol): String;
Begin
    Case IeeeSymbol Of
        eNoSymbol                : Result := 'NO SYMBOL'                 ;
        eDot                     : Result := 'DOT'                       ;
        eRightLeftSignalFlow     : Result := 'RIGHT LEFT SIGNAL FLOW'    ;
        eClock                   : Result := 'CLOCK'                     ;
        eActiveLowInput          : Result := 'ACTIVE LOW INPUT'          ;
        eAnalogSignalIn          : Result := 'ANALOG SIGNAL IN'          ;
        eNotLogicConnection      : Result := 'NOT LOGIC CONNECTION'      ;
        eShiftRight              : Result := 'SHIFT RIGHT'               ;
        ePostPonedOutput         : Result := 'POST PONED OUTPUT'         ;
        eOpenCollector           : Result := 'OPEN COLLECTOR'            ;
        eHiz                     : Result := 'HIZ'                       ;
        eHighCurrent             : Result := 'HIGH CURRENT'              ;
        ePulse                   : Result := 'PULSE'                     ;
        eSchmitt                 : Result := 'SCHMITT'                   ;
        eDelay                   : Result := 'DELAY'                     ;
        eGroupLine               : Result := 'GROUPLINE'                 ;
        eGroupBin                : Result := 'GROUPBIN'                  ;
        eActiveLowOutput         : Result := 'ACTIVE LOW OUTPUT'         ;
        ePiSymbol                : Result := 'PI SYMBOL'                 ;
        eGreaterEqual            : Result := 'GREATER EQUAL'             ;
        eLessEqual               : Result := 'LESS EQUAL'                ;
        eSigma                   : Result := 'SIGMA'                     ;
        eOpenCollectorPullUp     : Result := 'OPEN COLLECTOR PULL UP'    ;
        eOpenEmitter             : Result := 'OPEN EMITTER'              ;
        eOpenEmitterPullUp       : Result := 'OPEN EMITTER PULL UP'      ;
        eDigitalSignalIn         : Result := 'DIGITAL SIGNAL IN'         ;
        eAnd                     : Result := 'AND'                       ;
        eInvertor                : Result := 'INVERTOR'                  ;
        eOr                      : Result := 'OR'                        ;
        eXor                     : Result := 'XOR'                       ;
        eShiftLeft               : Result := 'SHIFT LEFT'                ;
        eInputOutput             : Result := 'INPUT OUTPUT'              ;
        eOpenCircuitOutput       : Result := 'OPEN CIRCUIT OUTPUT'       ;
        eLeftRightSignalFlow     : Result := 'LEFT RIGHT SIGNAL FLOW'    ;
        eBidirectionalSignalFlow : Result := 'BIDIRECTIONAL SIGNAL FLOW' ;
    Else
        Result := 'UNKNOW IEEE SYMBOL';
    End;
End;
{..............................................................................}

{..............................................................................}
Function StrToIeeeSymbol(Str: String): TIeeeSymbol;
Begin
    Str := UpperCase(Str);
    If Str = 'NO SYMBOL'                 Then Result := eNoSymbol                Else
    If Str = 'DOT'                       Then Result := eDot                     Else
    If Str = 'RIGHT LEFT SIGNAL FLOW'    Then Result := eRightLeftSignalFlow     Else
    If Str = 'CLOCK'                     Then Result := eClock                   Else
    If Str = 'ACTIVE LOW INPUT'          Then Result := eActiveLowInput          Else
    If Str = 'ANALOG SIGNAL IN'          Then Result := eAnalogSignalIn          Else
    If Str = 'NOT LOGIC CONNECTION'      Then Result := eNotLogicConnection      Else
    If Str = 'SHIFT RIGHT'               Then Result := eShiftRight              Else
    If Str = 'POST PONED OUTPUT'         Then Result := ePostPonedOutput         Else
    If Str = 'OPEN COLLECTOR'            Then Result := eOpenCollector           Else
    If Str = 'HIZ'                       Then Result := eHiz                     Else
    If Str = 'HIGH CURRENT'              Then Result := eHighCurrent             Else
    If Str = 'PULSE'                     Then Result := ePulse                   Else
    If Str = 'SCHMITT'                   Then Result := eSchmitt                 Else
    If Str = 'DELAY'                     Then Result := eDelay                   Else
    If Str = 'GROUPLINE'                 Then Result := eGroupLine               Else
    If Str = 'GROUPBIN'                  Then Result := eGroupBin                Else
    If Str = 'ACTIVE LOW OUTPUT'         Then Result := eActiveLowOutput         Else
    If Str = 'PI SYMBOL'                 Then Result := ePiSymbol                Else
    If Str = 'GREATER EQUAL'             Then Result := eGreaterEqual            Else
    If Str = 'LESS EQUAL'                Then Result := eLessEqual               Else
    If Str = 'SIGMA'                     Then Result := eSigma                   Else
    If Str = 'OPEN COLLECTOR PULL UP'    Then Result := eOpenCollectorPullUp     Else
    If Str = 'OPEN EMITTER'              Then Result := eOpenEmitter             Else
    If Str = 'OPEN EMITTER PULL UP'      Then Result := eOpenEmitterPullUp       Else
    If Str = 'DIGITAL SIGNAL IN'         Then Result := eDigitalSignalIn         Else
    If Str = 'AND'                       Then Result := eAnd                     Else
    If Str = 'INVERTOR'                  Then Result := eInvertor                Else
    If Str = 'OR'                        Then Result := eOr                      Else
    If Str = 'XOR'                       Then Result := eXor                     Else
    If Str = 'SHIFT LEFT'                Then Result := eShiftLeft               Else
    If Str = 'INPUT OUTPUT'              Then Result := eInputOutput             Else
    If Str = 'OPEN CIRCUIT OUTPUT'       Then Result := eOpenCircuitOutput       Else
    If Str = 'LEFT RIGHT SIGNAL FLOW'    Then Result := eLeftRightSignalFlow     Else
    If Str = 'BIDIRECTIONAL SIGNAL FLOW' Then Result := eBidirectionalSignalFlow Else
                                              Result := eNoSymbol;
End;
{..............................................................................}

