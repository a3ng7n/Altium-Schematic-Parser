//..............................................................................
// Summary  Demonstration of a query script
//          to execute in the Filter panel's query window.
//
//          Query scripts need to be installed in the
//          Installed Projects list from
//          DXP » Preferences » DXP System » Scripting System
//
// Copyright (c) 2004 by Altium Limited
//..............................................................................

//..............................................................................
Function HighlightPadsWithZeroHoleSize : Boolean;
Begin
    Result := False;

    // Check if PCB document exists, if not, exit.
    If UpperCase(Client.CurrentView.OwnerDocument.Kind) <> 'PCB' Then Exit;

    Result := IsComponentPad and (Holesize = 0);
End;
//..............................................................................

//..............................................................................
