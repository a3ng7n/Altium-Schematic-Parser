{..............................................................................}
{ Summary Cycle through available cursor types                                }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    PCBSystemOptions : IPCB_SystemOptions;
Begin
    PCBSystemOptions := PCBServer.SystemOptions;
    If PCBSystemOptions = Nil Then Exit;

    If PcbSystemOptions.BoardCursorType = eCurShapeCross90 Then
        PcbSystemOptions.BoardCursorType := eCurShapeBigCross
    Else If PcbSystemOptions.BoardCursorType = eCurShapeBigCross Then
        PcbSystemOptions.BoardCursorType := eCurShapeCross45
    Else
        PcbSystemOptions.BoardCursorType := eCurShapeCross90;
End.
{..............................................................................}

{..............................................................................}
