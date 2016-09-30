{..............................................................................}
{ Summary Fetch existing violation objects and generate a report outlining     }
{         different violations.                                                }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function ConvertRuleLayerKindToString(K : TRuleLayerKind) : String;
Begin
    Result := '';
    Case K Of
       eRuleLayerKind_SameLayer     : Result := 'Same Layer';
       eRuleLayerKind_AdjacentLayer : Result := 'AdjacentLayer';
    End;
End;
{..............................................................................}

{..............................................................................}
Function ConvertRuleKindToString(R : TRuleKind) : String;
Begin
    Result := '';
    Case R Of
        eRule_Clearance                 : Result := 'Clearance';
        eRule_ParallelSegment           : Result := 'Parallel Segment';
        eRule_MaxMinWidth               : Result := 'Max min Width';
        eRule_MaxMinLength              : Result := 'Max min length';
        eRule_MatchedLengths            : Result := 'Matched lengths';
        eRule_DaisyChainStubLength      : Result := 'Daisy Chain Stub Length';
        eRule_PowerPlaneConnectStyle    : Result := 'Power Plane Connect Style';
        eRule_RoutingTopology           : Result := 'Routing Topology';
        eRule_RoutingPriority           : Result := 'Routing Priority';
        eRule_RoutingLayers             : Result := 'Routing Layers';
        eRule_RoutingCornerStyle        : Result := 'Routing Corner Style';
        eRule_RoutingViaStyle           : Result := 'Routing Via Style';
        eRule_PowerPlaneClearance       : Result := 'Power Plane Clearance';
        eRule_SolderMaskExpansion       : Result := 'Solder Mask Expansion';
        eRule_PasteMaskExpansion        : Result := 'Paste Mask Expansion';
        eRule_ShortCircuit              : Result := 'Short Circuit';
        eRule_BrokenNets                : Result := 'Broken Nets';
        eRule_ViasUnderSMD              : Result := 'Vias Under SMD';
        eRule_MaximumViaCount           : Result := 'Maximum Via Count';
        eRule_MinimumAnnularRing        : Result := 'Minimum Annular Ring';
        eRule_PolygonConnectStyle       : Result := 'Polygon Connect Style';
        eRule_AcuteAngle                : Result := 'Acute Angle';
        eRule_ConfinementConstraint     : Result := 'Confinement Constraint';
        eRule_SMDToCorner               : Result := 'SMD to corner';
        eRule_ComponentClearance        : Result := 'Component Clearance';
        eRule_ComponentRotations        : Result := 'Component Rotations';
        eRule_PermittedLayers           : Result := 'Permitted Layers';
        eRule_NetsToIgnore              : Result := 'Nets To Ignore';
        eRule_SignalStimulus            : Result := 'Signal Stimulus';
        eRule_Overshoot_FallingEdge     : Result := 'Overshoot Falling Edge';
        eRule_Overshoot_RisingEdge      : Result := 'Overshoot Rising Edge';
        eRule_Undershoot_FallingEdge    : Result := 'Undershoot Falling Edge';
        eRule_Undershoot_RisingEdge     : Result := 'Undershoot Rising Edge';
        eRule_MaxMinImpedance           : Result := 'Max Min Impedance';
        eRule_SignalTopValue            : Result := 'Signal Top Value';
        eRule_SignalBaseValue           : Result := 'Signal Base Value';
        eRule_FlightTime_RisingEdge     : Result := 'Flight Time Rising Edge';
        eRule_FlightTime_FallingEdge    : Result := 'Flight Time Falling Edge';
        eRule_LayerStack                : Result := 'Layer Stack';
        eRule_MaxSlope_RisingEdge       : Result := 'Max Slope Rising Edge';
        eRule_MaxSlope_FallingEdge      : Result := 'Max Slope Falling Edge';
        eRule_SupplyNets                : Result := 'Supply Nets';
        eRule_MaxMinHoleSize            : Result := 'Max Min Hole Size';
        eRule_TestPointStyle            : Result := 'Test Point Style';
        eRule_TestPointUsage            : Result := 'Test Point Usage';
        eRule_UnconnectedPin            : Result := 'Unconnected Pin';
        eRule_SMDToPlane                : Result := 'SMD To Plane';
        eRule_SMDNeckDown               : Result := 'SMD Neck Down';
        eRule_LayerPair                 : Result := 'Layer Pair';
        eRule_FanoutControl             : Result := 'FanOut Control';
    End;
End;
{..............................................................................}

{..............................................................................}
Function ConvertLayerToString(L : TLayer) : String;
Begin
    Result := Layer2String(L);
End;
{..............................................................................}

{..............................................................................}
Function ConvertObjectKindToString(O : TObjectId) : String;
Begin
    Result := '';
    Case O Of
        eNoObject           : Result := 'Any Object';
        eArcObject          : Result := 'Arc';
        ePadObject          : Result := 'Pad';
        eViaObject          : Result := 'Via';
        eTrackObject        : Result := 'Track';
        eTextObject         : Result := 'Text';
        eFillObject         : Result := 'Fill';
        eConnectionObject   : Result := 'Connection';
        eNetObject          : Result := 'Net';
        eComponentObject    : Result := 'Component';
        ePolyObject         : Result := 'Polygon';
        eDimensionObject    : Result := 'Dimension';
        eCoordinateObject   : Result := 'Coordinate';
        eClassObject        : Result := 'Class';
        eRuleObject         : Result := 'Rule';
        eFromToObject       : Result := 'FromTo';
        eViolationObject    : Result := 'Violation';
        eEmbeddedObject     : Result := 'Embedded';
        eTraceObject        : Result := 'Trace';
        eSpareViaObject     : Result := 'Spare Via';
        eBoardObject        : Result := 'Board';
        eBoardOutlineObject : Result := 'Board Outline';
    End;
End;
{..............................................................................}

{..............................................................................}
Function ConvertNetScopeToString(N : TNetScope) : String;
Begin
    Result := '';
    Case N Of
        eNetScope_DifferentNetsOnly : Result := 'Different Nets only';
        eNetScope_SameNetOnly       : Result := 'Same Net only';
        eNetScope_AnyNet            : Result := 'Any Net';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure SearchAndReportViolations;
Var
    Board         : IPCB_Board;
    Iterator      : IPCB_BoardIterator;
    Violation     : IPCB_Violation;
    ViolationData : TStringList;
    Rule          : IPCB_Rule;
    PCBObject     : IPCB_Primitive;
    S             : TDynamicString;
    Document      : IServerDocument;
    FileName      : TPCBString;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil then Exit;

    // Create an iterator to look for violation objects only.
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eViolationObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Create a TSTringList object to store violation data.
    ViolationData := TStringList.Create;


    // search for violations
    Violation := Iterator.FirstPCBObject;
    While Violation <> Nil Do
    Begin
        ViolationData.Add('Violation Name: ' + Violation.Name + '  Description: ' + Violation.Description);

        // Report design rule associated with the currently found violation object
        Rule := Violation.Rule;
        ViolationData.Add('  Rule Name: '        + Rule.Name                                    + ', ' +
                          '  Rule Kind: '        + ConvertRuleKindToString     (Rule.RuleKind)  + ', ' +
                          '  NetScope : '        + ConvertNetScopeToString     (Rule.NetScope)  + ', ' +
                          '  Rule Layer Kind : ' + ConvertRuleLayerKindToString(Rule.LayerKind) + ', ' +
                          '  Scope 1 : '         + Rule.Scope1Expression                        + ', ' +
                          '  Scope 2 : '         + Rule.Scope2Expression);

        // Report first pcb object associated with a unary/binary design rule.
        PCBObject := Violation.Primitive1;

        //PCBObject.GetState_Kind always returns an eAnyObject, ObjectKind routine is not implemented!
        ViolationData.Add('   ' +
                          '   Object 1: ' + ConvertObjectKindToString(PCBObject.ObjectId) + ', ' +
                          '   Layer  1: ' + ConvertLayerToString     (PCBObject.Layer)
                          );

        // Report second pcb object associated with a binary design rule.
        // however there are unary and binary rules, thus, for unary rules,
        // there will only be one object in violation associated with the violation
        PCBObject := Violation.Primitive2;
        If PCBObject <> Nil Then
        Begin
            ViolationData.Add('   ' +
                              '   Object 2: ' + ConvertObjectKindToString(PCBObject.ObjectId) + ', '+
                              '   Layer  2: ' + ConvertLayerToString     (PCBObject.Layer)
                              );
        End;

        // insert a blank line..
        ViolationData.Add('');

        Violation := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);


    FileName := ChangeFileExt(Board.FileName,'.vio');
    ViolationData.SaveToFile(Filename);

    //Display violation details on a text document
    If ViolationData.Count > 0 Then
    Begin
        // open and display the textfile
        Document := Client.OpenDocument('Text', Filename);
        If Document <> Nil Then
            Client.ShowDocument(Document);
    End;
    ViolationData.Free;
End;
{..............................................................................}

{..............................................................................}
