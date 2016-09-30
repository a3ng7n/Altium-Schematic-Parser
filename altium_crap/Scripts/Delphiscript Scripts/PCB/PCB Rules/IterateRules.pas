{..............................................................................}
{ Summary Iterates Rules from the current PCB document.                        }
{                                                                              }
{ Copyright (c) 2008 by Altium Limited                                         }
{                                                                              }
{ RuleFactory used to create new rule objects                                  }
{ IPCB_Rule interface object represents a rule object on a PCB document        }
{..............................................................................}

{..............................................................................}
Function RuleKindToString (ARuleKind : TRuleKind) : String;
Begin
    Result := '';

    Case ARuleKind Of
        eRule_Clearance                : Result := 'Clearance';
        eRule_ParallelSegment          : Result := 'ParallelSegment';
        eRule_MaxMinWidth              : Result := 'Width';
        eRule_MaxMinLength             : Result := 'Length';               
        eRule_MatchedLengths           : Result := 'MatchedLengths';       
        eRule_DaisyChainStubLength     : Result := 'StubLength';           
        eRule_PowerPlaneConnectStyle   : Result := 'PlaneConnect';         
        eRule_RoutingTopology          : Result := 'RoutingTopology';      
        eRule_RoutingPriority          : Result := 'RoutingPriority';      
        eRule_RoutingLayers            : Result := 'RoutingLayers';        
        eRule_RoutingCornerStyle       : Result := 'RoutingCorners';       
        eRule_RoutingViaStyle          : Result := 'RoutingVias';          
        eRule_PowerPlaneClearance      : Result := 'PlaneClearance';       
        eRule_SolderMaskExpansion      : Result := 'SolderMaskExpansion';  
        eRule_PasteMaskExpansion       : Result := 'PasteMaskExpansion';   
        eRule_ShortCircuit             : Result := 'ShortCircuit';         
        eRule_BrokenNets               : Result := 'UnRoutedNet';          
        eRule_ViasUnderSMD             : Result := 'ViasUnderSMD';         
        eRule_MaximumViaCount          : Result := 'MaximumViaCount';      
        eRule_MinimumAnnularRing       : Result := 'MinimumAnnularRing';   
        eRule_PolygonConnectStyle      : Result := 'PolygonConnect';       
        eRule_AcuteAngle               : Result := 'AcuteAngle';           
        eRule_ConfinementConstraint    : Result := 'RoomDefinition';       
        eRule_SMDToCorner              : Result := 'SMDToCorner';          
        eRule_ComponentClearance       : Result := 'ComponentClearance';  
        eRule_ComponentRotations       : Result := 'ComponentOrientations';
        eRule_PermittedLayers          : Result := 'PermittedLayers';      
        eRule_NetsToIgnore             : Result := 'NetsToIgnore';         
        eRule_SignalStimulus           : Result := 'SignalStimulus';       
        eRule_Overshoot_FallingEdge    : Result := 'OvershootFalling';     
        eRule_Overshoot_RisingEdge     : Result := 'OvershootRising';      
        eRule_Undershoot_FallingEdge   : Result := 'UndershootFalling';    
        eRule_Undershoot_RisingEdge    : Result := 'UndershootRising';     
        eRule_MaxMinImpedance          : Result := 'MaxMinImpedance';      
        eRule_SignalTopValue           : Result := 'SignalTopValue';       
        eRule_SignalBaseValue          : Result := 'SignalBaseValue';      
        eRule_FlightTime_RisingEdge    : Result := 'FlightTimeRising';     
        eRule_FlightTime_FallingEdge   : Result := 'FlightTimeFalling';    
        eRule_LayerStack               : Result := 'LayerStack';           
        eRule_MaxSlope_RisingEdge      : Result := 'SlopeRising';          
        eRule_MaxSlope_FallingEdge     : Result := 'SlopeFalling';         
        eRule_SupplyNets               : Result := 'SupplyNets';           
        eRule_MaxMinHoleSize           : Result := 'HoleSize';             
        eRule_TestPointStyle           : Result := 'Testpoint';            
        eRule_TestPointUsage           : Result := 'TestPointUsage';       
        eRule_UnconnectedPin           : Result := 'UnConnectedPin';       
        eRule_SMDToPlane               : Result := 'SMDToPlane';           
        eRule_SMDNeckDown              : Result := 'SMDNeckDown';          
        eRule_LayerPair                : Result := 'LayerPairs';           
        eRule_FanoutControl            : Result := 'FanoutControl';        
        eRule_MaxMinHeight             : Result := 'Height';               
        eRule_DifferentialPairsRouting : Result := 'DiffPairsRouting';    
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure IterateRules;
Var
    Board         : IPCB_Board;
    Rule          : IPCB_Rule;
    BoardIterator : IPCB_BoardIterator;
    Rpt           : TStringList;
    FileName      : TPCBString;
    Document      : IServerDocument;
    Count         : Integer;
    I             : Integer;
    J             : Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Retrieve the iterator
    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eRuleObject));

    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Count := 0;
    Rpt   := TStringList.Create;

    // Search for Rule and for each rule found
    // get its attributes and put them in a TStringList
    // to be saved as a text file...
    Rule := BoardIterator.FirstPCBObject;
    While (Rule <> Nil) Do
    Begin
        Inc(Count);
        Rpt.Add(IntToStr(Count) + ': ' + Rule.Name + ', UniqueId: ' +  Rule.UniqueId +
                ', RuleType: ' + RuleKindToString(Rule.RuleKind));
        Rule := BoardIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(BoardIterator);
    Rpt.Insert(0,'Rules Information for the ' + ExtractFileName(Board.FileName) + ' document.');
    Rpt.Insert(1,'----------------------------------------------------------');
    Rpt.Insert(2,'');

    // Display the Rules report
    FileName := ChangeFileExt(Board.FileName,'.rul');
    Rpt.SaveToFile(Filename);
    Rpt.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);
 End;
{..............................................................................}

{..............................................................................}

(*
TRuleType
TScopeType

IPCB_Rule
    IPCB_ClearanceConstraint
    IPCB_ParallelSegmentConstraint
    IPCB_MaxMinWidthConstraint
    IPCB_MaxMinLengthConstraint
    IPCB_MatchedNetLengthsConstraint
    IPCB_DaisyChainStubLengthConstraint
    IPCB_PowerPlaneConnectStyleRule
    IPCB_PolygonConnectStyleRule
    IPCB_RoutingTopologyRule
    IPCB_RoutingPriorityRule
    IPCB_RoutingLayersRule
    IPCB_RoutingCornerStyleRule
    IPCB_RoutingViaStyleRule
    IPCB_PowerPlaneClearanceRule
    IPCB_SolderMaskExpansionRule
    IPCB_PasteMaskExpansionRule
    IPCB_ShortCircuitConstraint
    IPCB_BrokenNetRule
    IPCB_ViasUnderSMDConstraint
    IPCB_MaximumViaCountRule
    IPCB_MinimumAnnularRing
    IPCB_AcuteAngle
    IPCB_ConfinementConstraint
    IPCB_ComponentClearanceConstraint
    IPCB_ComponentRotationsRule
    IPCB_PermittedLayersRule
    IPCB_SignalStimulus
    IPCB_MaxOvershootFall
    IPCB_MaxOvershootRise
    IPCB_MaxUndershootFall
    IPCB_MaxUndershootRise
    IPCB_RuleMaxMinImpedance
    IPCB_RuleMinSignalTopValue
    IPCB_RuleMaxSignalBaseValue
    IPCB_RuleFlightTime_RisingEdge
    IPCB_RuleFlightTime_FallingEdge
    IPCB_RuleMaxSlopeRisingEdge
    IPCB_RuleMaxSlopeFallingEdge
    IPCB_NetsToIgnoreRule
    IPCB_SMDToCornerConstraint
    IPCB_RuleSupplyNets
    IPCB_MaxMinHoleSizeConstraint
    IPCB_TestPointStyleRule
    IPCB_TestPointUsage
    IPCB_UnConnectedPinRule
    IPCB_SMDToPlaneConstraint
    IPCB_SMDNeckDownConstraint
    IPCB_LayerPairsRule
    IPCB_FanoutControlRule
    IPCB_MaxMinHeightConstraint
    IPCB_DifferentialPairsRoutingRule
*)
