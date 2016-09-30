{..............................................................................}
{ Summary Pins Importer script.
{         The ImportPinsForm is the main form.                                 }
{         You need a Sch Pins Data CSV file to import and create new Pins      }
{         onto a SchLib document                                               }
{                                                                              }
{ To use the script:                                                           }
{  1/ Execute the ImportPins procedure and the Pins Importer dialog appears    }
{  2/ Click on browse button to load in the CSV file of schematic pins data.   }
{  3/ Click on the Update Mapping button to refresh the links between          }
{     text fields and pin properties, then click on Import button to generate  }
{     a list of different pins on a Sch library page.                          }
{                                                                              } 
{ Version 1.1 Location problem resolved for DXP 2004 SP2 onwards               }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Interface
Type
  TImportPinsForm = class(TForm)
    ButtonBrowse        : TButton;
    ButtonUpdateMapping : TButton;
    ButtonChangeMapping : TButton;
    ButtonImport        : TButton;
    ListView            : TListView;
    OpenDialog          : TOpenDialog;
    Edit                : TEdit;
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonUpdateMappingClick(Sender: TObject);
    procedure ButtonChangeMappingClick(Sender: TObject);
    procedure ButtonImportClick(Sender: TObject);
  End;

Var
    ImportPinsForm : TImportPinsForm;
    SchDoc         : ISch_Document;
{..............................................................................}

{..............................................................................}
Procedure TImportPinsForm.ButtonBrowseClick(Sender: TObject);
Begin
    If OpenDialog.Execute Then Edit.Text := OpenDialog.FileName;
End;
{..............................................................................}

{..............................................................................}
Procedure AddListViewItem(ItemIndex: Integer; ItemCaption: String);
Var
    i : Integer ;
Begin
    ListView.Items.Add;
    ListView.Items[ItemIndex].Caption := ItemCaption;
    For i := 0 To FormChangeMapping.ComboBox.Items.Count-1 Do
        If UpperCase(ItemCaption) = UpperCase(FormChangeMapping.ComboBox.Items[i]) Then
        Begin
            ListView.Items[ItemIndex].SubItems.Add(FormChangeMapping.ComboBox.Items[i]);
            ListView.Items[ItemIndex].Checked := True;
            Exit;
        End;
    ListView.Items[ItemIndex].SubItems.Add('');
End;
{..............................................................................}

{..............................................................................}
procedure TImportPinsForm.ButtonUpdateMappingClick(Sender: TObject);
Var
    StrList     : TStringList ;
    ValuesCount : Integer     ;
    i, j        : Integer     ;
Begin
    If Edit.Text = '' Then Exit;

    StrList := TStringList.Create;
    Try
        StrList.LoadFromFile(Edit.Text);
        ListView.Clear;

        ValuesCount := 1 ;
        j           := 1 ;
        For i := 1 To Length(StrList[0]) Do
            If (Copy(StrList[0], i, 1) = ',') Then
            Begin
                AddListViewItem(ValuesCount-1, Copy(StrList[0], j, i-j));
                j := i+1;
                Inc(ValuesCount);
            End;
        If ValuesCount > 1 Then
            AddListViewItem(ValuesCount-1, Copy(StrList[0], j, Length(StrList[0])+1-j));
    Finally
        StrList.Free;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TImportPinsForm.ButtonChangeMappingClick(Sender: TObject);
Begin
    If ListView.ItemIndex = -1 Then
    Begin
        ShowMessage('Select a text field to map');
        Exit;
    End;

    FormChangeMapping.LabelTextField.Caption := ListView.Items[ListView.ItemIndex].Caption;
    If FormChangeMapping.ShowModal = mrOk Then
    Begin
        ListView.Items[ListView.ItemIndex].SubItems.Strings[0]:=(FormChangeMapping.ComboBox.Text);
        ListView.Items[ListView.ItemIndex].Checked := FormChangeMapping.CheckBox.Checked;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TImportPinsForm.ButtonImportClick(Sender: TObject);
Var
    SchPin           : ISch_Pin      ;
    ValuesCount      : Integer       ;
    i, j, k, l       : Integer       ;
    TxtFieldValue    : String        ;
    PinProperty      : String        ;
    StrList          : TStringList   ;

    Location         : TLocation     ;
    PinLocX, PinLocY : Integer       ;
    PinLocMapped     : Boolean       ;
Begin
    // check if file exists or not
    If Not(FileExists(Edit.Text)) or (Edit.Text = '') Then
    Begin
        ShowWarning('The Pin Data CSV format file doesnt exist!');
        Exit;
    End;

    StrList := TStringList.Create;
    Try
        StrList.LoadFromFile(Edit.Text);

        For j := 1 To StrList.Count-1 Do
        Begin
            SchPin       := SchServer.SchObjectFactory(ePin,eCreate_GlobalCopy);
            PinLocMapped := False;

            For i := 0 To ListView.Items.Count-1 Do
            Begin
                If ListView.Items[i].Checked Then
                Begin
                    TxtFieldValue := '';
                    ValuesCount   := 1 ;
                    k             := 1 ;
                    For l := 1 To Length(StrList[j]) Do
                        If (Copy(StrList[j], l, 1) = ',') Then
                        Begin
                            If ValuesCount = i+1 Then
                            Begin
                                TxtFieldValue := Copy(StrList[j], k, l-k);
                                k := l+1;
                                Inc(ValuesCount);
                                Break;
                            End;
                            k := l+1;
                            Inc(ValuesCount);
                        End;
                    If ValuesCount = i+1 Then TxtFieldValue := Copy(StrList[j], k, Length(StrList[j])+1-k);

                    PinProperty := UpperCase(ListView.Items[i].SubItems.Strings[0]);

                    If PinProperty = 'DISPLAY NAME'      Then SchPin.Name             :=                    TxtFieldValue  Else
                    If PinProperty = 'SHOW DISPLAY NAME' Then SchPin.ShowName         := StrToBoolean      (TxtFieldValue) Else
                    If PinProperty = 'DESIGNATOR'        Then SchPin.Designator       :=                    TxtFieldValue  Else
                    If PinProperty = 'ELECTRICAL TYPE'   Then SchPin.Electrical       := StrToPinElectrical(TxtFieldValue) Else
                    If PinProperty = 'DESCRIPTION'       Then SchPin.Description      :=                    TxtFieldValue  Else
                    If PinProperty = 'PIN IS HIDDEN'     Then SchPin.IsHidden         := StrToBoolean      (TxtFieldValue) Else
                    If PinProperty = 'HIDDEN NET NAME'   Then SchPin.HiddenNetName    :=                    TxtFieldValue  Else
                    If PinProperty = 'INNER SYMBOL'      Then SchPin.Symbol_Inner     := StrToIeeeSymbol   (TxtFieldValue) Else
                    If PinProperty = 'INNER EDGE SYMBOL' Then SchPin.Symbol_InnerEdge := StrToIeeeSymbol   (TxtFieldValue) Else
                    If PinProperty = 'OUTER SYMBOL'      Then SchPin.Symbol_Outer     := StrToIeeeSymbol   (TxtFieldValue) Else
                    If PinProperty = 'OUTER EDGE SYMBOL' Then SchPin.Symbol_OuterEdge := StrToIeeeSymbol   (TxtFieldValue) Else
                    If PinProperty = 'LENGTH'            Then SchPin.PinLength        := StrToInt          (TxtFieldValue) Else
                    If PinProperty = 'ORIENTATION'       Then SchPin.Orientation      := StrToRotationBy90 (TxtFieldValue) Else
                    If PinProperty = 'LOCATION X'        Then Begin
                                                                  PinLocX := MilsToCoord(10 * StrToInt(TxtFieldValue));
                                                                  PinLocMapped := True;
                                                              End Else
                    If PinProperty = 'LOCATION Y'        Then Begin
                                                                  PinLocY := MilsToCoord(10 * StrToInt(TxtFieldValue));
                                                                  PinLocMapped := True;
                                                              End ;
                End;
            End;

            // SP2 onwards need to do it differently with Location property 
            If PinLocMapped Then
            Begin
                //SchPin.Location := Point(PinLocX, PinLocY)
                Location := SchPin.GetState_Location;
                Location.X := PinLocX;
                Location.Y := PinLocY;
                SchPin.SetState_Location(Location);
            End
            Else
            Begin
                //SchPin.Location := Point(0, (j-1)*10);
                Location := SchPin.GetState_Location;
                Location.X := 0;
                Location.Y := (j-1)*10;
                SchPin.SetState_Location(Location);
            End;

            SchDoc.RegisterSchObjectInContainer(SchPin);
        End;
    Finally
        StrList.Free;
    End;

    SchDoc.GraphicallyInvalidate;

    ResetParameters;
    AddStringParameter('Action', 'All');
    RunProcess('Sch:Zoom');

    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure RunImportPins;
Begin
    If SchServer = Nil Then Exit;
    SchDoc := SchServer.GetCurrentSchDocument;
    If SchDoc = Nil Then Exit;

    // check if it is a schematic library document
    If Not SchDoc.IsLibrary Then Exit;

    ImportPinsForm.ShowModal;
End;
{..............................................................................}

{..............................................................................}
