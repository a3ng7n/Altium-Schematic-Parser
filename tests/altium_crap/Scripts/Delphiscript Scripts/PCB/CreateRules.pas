{..............................................................................}
{ Summary Create 2 different design rules.                                     }
{                                                                              }
{ 1. CreateAMaxMinRule procedure                                               }
{ 2. CreateARoomDefinitionRule procedure                                       }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CreateAMaxMinRule;
Var
    PCBBoard   : IPCB_Board;
    P          : IPCB_MaxMinWidthConstraint;
    SystemUnit : TUnit;
    MaxLimit   : TCoord;
    MinLimit   : TCoord;
    L          : TLayer;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    // Create a Max Min Width Routing rule
    P := PCBServer.PCBRuleFactory(eRule_MaxMinWidth);

    // Set values
    P.NetScope  := eNetScope_AnyNet;
    P.LayerKind := eRuleLayerKind_SameLayer;

    // Define values for the Min Max Width Constraint rule
    SystemUnit := eImperial; // SystemUnit - eImperial or eMetric
    StringToCoordUnit('10mils', MinLimit, SystemUnit);
    StringToCoordUnit('20mils', MaxLimit, SystemUnit);

    For L := MinLayer To MaxLayer Do
    Begin
        P.MaxWidth    [L] := MaxLimit;
        P.MinWidth    [L] := MinLimit;
        P.FavoredWidth[L] := MaxLimit;
    End;

    P.Name    := 'Custom Width Rule';
    P.Comment := 'Custom Width Rule';

    // Add the rule into the Board
    PCBBoard.AddPCBObject(P);
End;
{..............................................................................}

{..............................................................................}
Procedure CreateARoomDefinitionRule;
Var
    PCBBoard   : IPCB_Board;
    P          : IPCB_ConfinementConstraint;
    CoordRect  : TCoordRect;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    P := PCBServer.PCBRuleFactory(eRule_ConfinementConstraint);

    // Set values
    P.NetScope  := eNetScope_AnyNet;
    P.LayerKind := eRuleLayerKind_SameLayer;

    CoordRect := TCoordRect; // special function to initialize a record type variable.
    CoordRect.X1 := MilsToCoord(2000);
    CoordRect.Y1 := MilsToCoord(2000);
    CoordRect.X2 := MilsToCoord(4000);
    CoordRect.Y2 := MilsToCoord(4000);
    P.BoundingRect := CoordRect;

    // Top or Bottom layer only
    P.ConstraintLayer := eBottomLayer;

    // TConfinementStyle
    P.Kind := eConfineOut;

    P.Name    := 'Custom Room Defn Rule';
    P.Comment := 'Custom room definition (confinement constraint) rule.';

    PCBBoard.AddPCBObject(P);
End;
