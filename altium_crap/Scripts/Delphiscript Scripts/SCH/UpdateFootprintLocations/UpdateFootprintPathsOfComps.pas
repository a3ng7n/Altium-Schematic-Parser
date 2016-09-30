{..............................................................................}
{ Summary A Schematic Component has a Footprint (PCB Model) property and its   }
{         Library Path property. There are situations when components have     }
{        the same footprint name but come from different libraries.            }
{                                                                              }
{         This UpdateFootprintPathsOfComp script allows you to choose a        }
{         footprint name from the currently open schematic and then choose     }
{         one of available footprint locations. The locations are based on     }
{         existing components footprint' locations. Then when dialog is        }
{         closed, all the components that have the same footprint name         }
{         have their locations updated with the specified location.            }
{                                                                              }
{                                                                              }
{         Make sure you have libraries installed for the Sch Components        }
{         so that footprint library locations (paths) can appear!              }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
// Global Variables...
Var
    CurrentSheet       : ISch_Sheet;
    TheLocation        : TPCBString;
    TheFootprintName   : TPCBString;
{..............................................................................}

{..............................................................................}
Procedure TForm_UpdatePath.XPBitBtnCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_UpdatePath.XPBitBtnOKClick(Sender: TObject);
Var
    Iterator           : ISch_Iterator;
    Component          : ISch_Component;
    ImplIterator       : ISch_Iterator;
    SchImplementation  : ISch_Implementation;
    Location           : TPCBString;
    TempList           : TStringList;
    I                  : Integer;
Begin
    If TheFootPrintName = '' Then Exit;

    SchServer.ProcessControl.PreProcess(CurrentSheet, '');
    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            ImplIterator := Component.SchIterator_Create;
            ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));
            Try
                SchImplementation := ImplIterator.FirstSchObject;
                While SchImplementation <> Nil Do
                Begin
                    // check which PCB model is used for the Sch Component.
                    If SchImplementation.ModelType = 'PCBLIB' Then
                        If SchImplementation.ModelName = TheFootprintName Then
                        Begin
                            // assumption, the DatafileLink[0] with index 0 is the
                            // current pcb model - there can be different PCB models
                            // for the same schematic component but only one can be the current one.
                            SchServer.RobotManager.SendMessage(SchImplementation.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                            SchImplementation.DatafileLink[0].Location := TheLocation;
                            SchServer.RobotManager.SendMessage(SchImplementation.I_ObjectAddress, c_BroadCast, SCHM_EndModify, c_NoEventData);
                        End;

                    SchImplementation := ImplIterator.NextSchObject;
                End;
            Finally
                Component.SchIterator_Destroy(ImplIterator);
            End;
             Component := Iterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(Iterator);
    End;
    SchServer.ProcessControl.PostProcess(CurrentSheet, '');
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchModelPaths(Dummy : Integer = 0);
Var
    Iterator           : ISch_Iterator;
    Component          : ISch_Component;
    ImplIterator       : ISch_Iterator;
    SchImplementation  : ISch_Implementation;
    Location           : TPCBString;
    TempList           : TStringList;
    I                  : Integer;
Begin
    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    //remove duplicates in a sorted TListBox
    TempList := TStringList.Create;
    TempList.Sorted     := True;
    TempList.Duplicates := dupIgnore;

    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            ImplIterator := Component.SchIterator_Create;
            ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));
            Try
                SchImplementation := ImplIterator.FirstSchObject;
                While SchImplementation <> Nil Do
                Begin
                    // check which PCB model is used for the Sch Component.
                    If SchImplementation.ModelType = 'PCBLIB' Then
                        If SchImplementation.ModelName = TheFootprintName Then
                        Begin
                            // Get locations for the footprint name.
                            // assumption, the DatafileLink[0] with index 0 is the
                            // current pcb model - there can be different PCB models
                            // for the same schematic component but only one can be the current one.
                            Location := SchImplementation.DatafileLink[0].Location;
                            If Location = '' Then Location := 'Any';
                            TempList.Add(Location);
                        End;

                    SchImplementation := ImplIterator.NextSchObject;
                End;
            Finally
                Component.SchIterator_Destroy(ImplIterator);
            End;
             Component := Iterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(Iterator);
    End;

    // Clear out ListBox_DiffPaths
    Form_UpdatePath.ListBox_DiffPaths.Items.Clear;

    // Add into Combox with new stringlist.
    For I := 0 to TempList.Count - 1 Do
        Form_UpdatePath.ListBox_DiffPaths.Items.Add(TempList.Strings[i]);

    TempList.Free;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchModelNames(Dummy : Integer = 0);
Var
    Iterator           : ISch_Iterator;
    Component          : ISch_Component;
    ImplIterator       : ISch_Iterator;
    SchImplementation  : ISch_Implementation;
    TempList           : TStringList;
    I                  : Integer;
Begin
    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    //remove duplicates in a sorted TListBox
    TempList := TStringList.Create;
    TempList.Sorted     := True;
    TempList.Duplicates := dupIgnore;

    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            ImplIterator := Component.SchIterator_Create;
            ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));
            Try
                SchImplementation := ImplIterator.FirstSchObject;
                While SchImplementation <> Nil Do
                Begin
                    // check which PCB model is used for the Sch Component.
                    If SchImplementation.ModelType = 'PCBLIB' Then
                        If SchImplementation.IsCurrent Then
                            TempList.Add(SchImplementation.ModelName);

                    SchImplementation := ImplIterator.NextSchObject;
                End;
            Finally
                Component.SchIterator_Destroy(ImplIterator);
            End;
             Component := Iterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(Iterator);
    End;

    // Clear out combobox component
    Form_UpdatePath.ComboBox_FootprintNames.Items.Clear;

    // Add into Combox with new stringlist.
    For I := 0 to TempList.Count - 1 Do
        Form_UpdatePath.ComboBox_FootprintNames.Items.Add(TempList.Strings[i]);

    TempList.Free;
    Form_UpdatePath.ShowModal;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_UpdatePath.ComboBox_FootprintNamesClick(Sender: TObject);
Begin
    TheFootprintName := ComboBox_FootprintNames.Text;
    If TheFootprintName <> '' Then FetchModelPaths;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_UpdatePath.ListBox_DiffPathsClick(Sender: TObject);
Begin
    If ListBox_DiffPaths.Items.Count < 1 Then Exit;
    If ListBox_DiffPaths.ItemIndex = -1 Then Exit;

    TheLocation := ListBox_DiffPaths.Items(ListBox_DiffPaths.ItemIndex);
    lFootPrintLocation.Caption := 'New Path: ' + TheLocation;
    If TheLocation = 'Any' Then TheLocation := '';
End;
{..............................................................................}

{..............................................................................}
Procedure RunScript;
Begin
    Client.StartServer('SCH');
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then
    Begin
        ShowError('This is not a schematic document.');
        Exit;
    End;

    FetchModelNames;

    // User has to click on a model name from the list first before
    // the model locations (paths) can be listed.

    // You then click on the Location from the Locations Listbox.
    // Click ok to update the schematic with the specified location for these components.
End;
{..............................................................................}

{..............................................................................}
End.

