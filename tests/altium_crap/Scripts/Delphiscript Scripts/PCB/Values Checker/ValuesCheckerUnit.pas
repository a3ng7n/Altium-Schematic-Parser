{..............................................................................}
{ Summary Check for PCB objects with illegal values                            }
{         Before outputting into Camtastic                                     }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CheckObjectValues;
Begin
    formObjectValueChecker.ShowModal;
End;
{..............................................................................}

{..............................................................................}
Procedure TformObjectValueChecker.bOKClick(Sender: TObject);
Var
    PCB_Board        : IPCB_Board;
    ObjectsBadValues : TStringList;
    S                : TDynamicString;
    ReportDocument   : IServerDocument;
Begin
    PCB_Board       := PCBServer.GetCurrentPCBBoard;
    If PCB_Board     = Nil Then Close;

    BeginHourGlass;

    ObjectsBadValues := TStringList.Create;

    // Execute the following checkers
    If XPcbZeroRadiiArcs.Checked Then
        CheckArcRadii(PCB_Board,ObjectsBadValues);

    If XPcbZeroDiameterPads.Checked Then
       CheckPadDiameters(PCB_Board,0,ObjectsBadValues);

    If XPcbZeroDiameterVias.Checked Then
       CheckViaDiameters(PCB_Board,0,ObjectsBadValues);

    If XPcbPadDiameters.Checked Then
       CheckPadDiameters(PCB_Board, IntToStr(MEPadDiameter.Text), ObjectsBadValues);

    If XPcbViaDiameters.Checked Then
       CheckViaDiameters(PCB_Board, IntToStr(MEViaDiameter.Text), ObjectsBadValues);


    If XPcbNegativeValues.Checked Then
        CheckNegativeValues(PCB_Board,ObjectsBadValues);

    OutputErrorReport(PCB_Board,ObjectsBadValues);

    ObjectsBadValues.Free;
    EndHourGlass;
    
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure TformObjectValueChecker.bCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}

