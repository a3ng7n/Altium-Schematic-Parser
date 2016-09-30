{..............................................................................}
{ Summary Fetch and modify width Rules from the current PCB document.          }
{                                                                              }
{ Copyright (c) 2008 by Altium Limited                                         }
{                                                                              }
{ IPCB_Rule interface object represents a rule object on a PCB document        }
{..............................................................................}

{..............................................................................}
Procedure FetchAndModifyWidthConstraintsForTheCurrentPCB;
Var
    Board         : IPCB_Board;
    BoardIterator : IPCB_BoardIterator;
    RuleWidth     : IPCB_MaxMinWidthConstraint;
    L             : TLayer;
    Rpt           : TStringList;
    FileName      : TPCBString;
    Document      : IServerDocument;
    Count         : Integer;
Begin
    // Retrieve the current PCB
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Retrieve the PCB iterator
    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eRuleObject));

    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Count := 0;
    Rpt   := TStringList.Create;

    RuleWidth := BoardIterator.FirstPCBObject;
    While (RuleWidth <> Nil) Do
    Begin
        If RuleWidth.RuleKind = eRule_MaxMinWidth Then
        Begin
             Inc(Count);
            // width values are in TCoord values.
            L := eTopLayer;
            // remember property names with [] sets are not supported in DelphiScript
            // thus we use property name()

            // MinWidth[Const L : TLayer] in PCB API is translated as
            // MinWidth(L) where L is a TLayer type.
            If RuleWidth.MinWidth(L) < MilsToCoord(20) Then
               RuleWidth.MinWidth(L) := MilsToCoord(10);

            If RuleWidth.MaxWidth(L) < MilsToCoord(20) Then
               RuleWidth.MaxWidth(L) := MilsToCoord(10);

            If RuleWidth.FavoredWidth(L) < MilsToCoord(20) Then
               RuleWidth.FavoredWidth(L) := MilsToCoord(10);

            Rpt.Add(IntToStr(Count) + ': ' + RuleWidth.Name + ', UniqueId: ' +  RuleWidth.UniqueId);
        End;

        RuleWidth := BoardIterator.NextPCBObject;
    End;

    Board.BoardIterator_Destroy(BoardIterator);
    Rpt.Insert(0,'Width Rules Modification Information for the ' + ExtractFileName(Board.FileName) + ' document.');
    Rpt.Insert(1,'----------------------------------------------------------');
    Rpt.Insert(2,'');

    // Display the Rules report
    FileName := ChangeFileExt(Board.FileName,'.widthrul');
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
    IPCB_MaxMinWidthConstraint
*)
