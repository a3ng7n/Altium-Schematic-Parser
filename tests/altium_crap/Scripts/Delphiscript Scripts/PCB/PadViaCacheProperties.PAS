{..................................................................................................}
{ Summary Demonstrate use of TPadCache record and GetObjectAtCursor method                         }
{         for Pad and Via Objects                                                                  }
{                                                                                                  }
{ Load PadViaExamples.PcbDoc from Examples » Scripts » DelphiScript » PCB folder                   }
{ before running the script.                                                                       }
{ Copyright (c) 2004 by Altium Limited                                                             }
{..................................................................................................}

{..................................................................................................}

{..................................................................................................}
Function GetCacheState (Value : TCacheState) : String;
Begin
    Result := '?';
    If Value = eCacheInvalid Then Result := 'Invalid';
    If Value = eCacheValid   Then Result := 'Valid';
    If Value = eCacheManual  Then Result := '''Manual''';
End;
{..................................................................................................}

{..................................................................................................}
Function GetPlaneConnectionStyle (Value : TPlaneConnectionStyle) : String;
Begin
    Result := 'Unknown';
    If Value = ePlaneNoConnect     Then Result := 'No Connect';
    If Value = ePlaneReliefConnect Then Result := 'Relief';
    If Value = ePlaneDirectConnect Then Result := 'Direct';
End;
{..................................................................................................}

{..................................................................................................}
Function IntegerToHexString(I : Integer) : TDynamicString;
Var
    AValue : TDynamicString;
    J      : Integer;
Begin
    If I = 0 Then
        AValue := '0'
    Else
        AValue := '';

    While I > 0 Do
    Begin
        J := I Mod 16;
        I := I Div 16;

        Case J Of
            0..9 : AValue := IntToStr(J) + AValue;
            10   : AValue := 'A' + AValue;
            11   : AValue := 'B' + AValue;
            12   : AValue := 'C' + AValue;
            13   : AValue := 'D' + AValue;
            14   : AValue := 'E' + AValue;
            15   : AValue := 'F' + AValue;
        End;
    End;
    Result := AValue;
End;
{..................................................................................................}

{..................................................................................................}
//Pads and vias share common properties, thus the common properties are cached.
Procedure CachedProperties;
Var
    Board       : IPCB_Board;
    Handle      : IPCB_Primitive;
    PadCache    : TPadCache;

    L           : TLayer;
    PlanesArray : TPlanesConnectArray;

    CPL         : Integer; // decimal value for CPL
    CPL_Hex     : String;  // hex value for CPL
    LS          : AnsiString;
Begin

    // Board.GetObjectAtCursor puts the PCB Editor into Interactive mode, 
    // ie a crosshair cursor appears and the 'Choose a pad or via' message
    // on the status bar of DXP.

    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    Handle := Board.GetObjectAtCursor(MkSet(ePadObject,eViaObject), 
                                      AllLayers, 
                                      'Choose a pad or via');
    While Handle <> 0 Do
    Begin
        PadCache := Handle.Cache;
        If (Handle.GetState_ObjectId = ePadObject) Then
            LS := 'Cached properties of selected pad:'
        Else // (If the object clicked on is not a pad, it must be a via instead.)
            LS := 'Cached properties of selected via:';

        // CCSV - Plane Connection Style valid ?
        LS := LS + #13 + #13 + 'CCSV: ' + GetCacheState(PadCache.PlaneConnectionStyleValid);
        If (PadCache.PlaneConnectionStyleValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Plane Connection Style: ' + GetPlaneConnectionStyle(PadCache.PlaneConnectionStyle)
        Else
            LS := LS + #13 + '{ Plane Connection Style: ' + GetPlaneConnectionStyle(PadCache.PlaneConnectionStyle) + ' }';

        // CPLV - Plane Layers (List) valid ?
        LS := LS + #13 + #13 + 'CPLV: ' + GetCacheState(PadCache.PlanesValid);

        (*
        // Transfer Pad.Cache's Planes field (Word type) to the Planes variable (TPlanesConnectArray type).
        PlanesArray := PadCache.Planes;
        // Calculate the decimal value of the 'CPL' number.
        CPL := 0;
        For L := kMaxInternalPlane DownTo kMinInternalPlane Do
        Begin
            // Planes is a TPlanesConnectArray and each internal plane has a boolean value.
            // at the moment PlanesArray[L] is always true which is not TRUE!
            If (PlanesArray[L] = True) Then
                CPL := (2 * CPL) + 1
            Else
                CPL := 2 * CPL;
        End;

        // Calculate the hexadecimal value of the 'CPL' number.
        CPL_Hex := IntegerToHexString(CPL);
        If (PadCache.PlanesValid <> eCacheInvalid) Then
        Begin
            LS := LS + #13 +   'Power Planes Connection Code (Decimal): ' + IntToStr(CPL);
            LS := LS + #13 +   'Power Planes Connection Code (Base 16): ' + CPL_Hex;
        End
        Else
        Begin
            LS := LS + #13 + '{ Power Planes Connection Code (Decimal): ' + IntToStr(CPL) + ' }';
            LS := LS + #13 + '{ Power Planes Connection Code (Base 16): ' + CPL_Hex + ' }';
        End;
        *)

        // CCWV - Relief Conductor Width valid ?
        LS := LS + #13 + #13 + 'CCWV: ' + GetCacheState(PadCache.ReliefConductorWidthValid);
        If (PadCache.ReliefConductorWidthValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Relief Conductor Width: ' + CoordUnitToString(PadCache.ReliefConductorWidth,eImperial)
        Else
            LS := LS + #13 + '{ Relief Conductor Width: ' + CoordUnitToString(PadCache.ReliefConductorWidth,eImperial) + ' }';

        // CENV - Relief Entries valid ?
        LS := LS + #13 + #13 + 'CENV: ' + GetCacheState(PadCache.ReliefEntriesValid);
        If (PadCache.ReliefEntriesValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Relief Entries: ' + IntToStr(PadCache.ReliefEntries)
        Else
            LS := LS + #13 + '{ Relief Entries: ' + IntToStr(PadCache.ReliefEntries) + ' }';

        // CAGV - Relief Air Gap Valid ?
        LS := LS + #13 + #13 + 'CAGV: ' + GetCacheState(PadCache.ReliefAirGapValid);
        If (PadCache.ReliefAirGapValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Relief Air Gap: ' + CoordUnitToString(PadCache.ReliefAirGap,eImperial)
        Else
            LS := LS + #13 + '{ Relief Air Gap: ' + CoordUnitToString(PadCache.ReliefAirGap,eImperial) + ' }';

        // CPRV - Power Plane Relief Expansion Valid ?
        LS := LS + #13 + #13 + 'CPRV: ' + GetCacheState(PadCache.PowerPlaneReliefExpansionValid);
        If (PadCache.PowerPlaneReliefExpansionValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Power Plane Relief Expansion: ' + CoordUnitToString(PadCache.PowerPlaneReliefExpansion,eImperial)
        Else
            LS := LS + #13 + '{ Power Plane Relief Expansion: ' + CoordUnitToString(PadCache.PowerPlaneReliefExpansion,eImperial) + ' }';

        // CPCV - Power Plane Clearance Valid ?
        LS := LS + #13 + #13 + 'CPCV: ' + GetCacheState(PadCache.PowerPlaneClearanceValid);
        If (PadCache.PowerPlaneClearanceValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Power Plane Clearance: ' + CoordUnitToString(PadCache.PowerPlaneClearance,eImperial)
        Else
            LS := LS + #13 + '{ Power Plane Clearance: ' + CoordUnitToString(PadCache.PowerPlaneClearance,eImperial) + ' }';

        // CSEV - Solder Mask Expansion Valid ?
        LS := LS + #13 + #13 + 'CSEV: ' + GetCacheState(PadCache.SolderMaskExpansionValid);
        If (PadCache.SolderMaskExpansionValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Solder Mask Expansion: ' + CoordUnitToString(PadCache.SolderMaskExpansion,eImperial)
        Else
            LS := LS + #13 + '{ Solder Mask Expansion: ' + CoordUnitToString(PadCache.SolderMaskExpansion,eImperial) + ' }';

        // CPEV - Paste Mask Expansion Valid ?
        LS := LS + #13 + #13 + 'CPEV: ' + GetCacheState(PadCache.PasteMaskExpansionValid);
        If (PadCache.PasteMaskExpansionValid <> eCacheInvalid) Then
            LS := LS + #13 + 'Paste Mask Expansion: ' + CoordUnitToString(PadCache.PasteMaskExpansion,eImperial)
        Else
            LS := LS + #13 + '{ Paste Mask Expansion: ' + CoordUnitToString(PadCache.PasteMaskExpansion,eImperial) + ' }';

        // Display the results
        ShowInfo(LS);

        // Continue the loop - ie user can click on another pad or via object.
        Handle := Board.GetObjectAtCursor(MkSet(ePadObject, eViaObject), AllLayers, 'Choose a pad or via');
    End;
End;
