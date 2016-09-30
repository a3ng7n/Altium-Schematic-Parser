{..............................................................................}
{ Summary: MillExporter - MMExportDlg                                          }
{   Control the settings for exporting a PCB to be routed on a milling machine.}
{                                                                              }
{ Written by Marty Hauff                                                       }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}
Var
  MMExportForm : TMMExportForm;
  TMMExportForm_IniFilename : TPCBString;
  RunExporter : Boolean;


{..............................................................................}
procedure TMMExportForm.TEdit_PosReal_OnKeyPress(Sender: TObject; var Key: Char);
begin
    case Key of
//    BackSpace, '.', '0'..'9'
      8, 46, 48..57 :
      begin
         if (Key = 46) and (Pos('.', Sender.Text) > 0) then
            Key := 0;
      end;
      else
         Key := 0;
   end;
end;

procedure TMMExportForm.TEdit_PosInt_OnKeyPress(Sender: TObject; var Key: Char);
begin
    case Key of
//    BackSpace, '0'..'9'
      8, 48..57 : Key := Key;
      else
         Key := 0;
   end;
end;
{..............................................................................}

{..............................................................................}
procedure TMMExportForm_SetIniFilename (Filename: TPCBString);
begin
   TMMExportForm_IniFilename := Filename;
end;


procedure TMMExportForm_ReadIniFileToForm(dummy : Integer);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(TMMExportForm_IniFilename);
    CutterDiameter.Text := IniFile.ReadFloat('Defaults','CutterDiameter',0.25);
    CutterDepth.Text := IniFile.ReadFloat('Defaults','CutterDepth',0.1);
    editDrillDepth.Text := IniFile.ReadFloat('Defaults','DrillDepth',3);
    PassHeight.Text := IniFile.ReadFloat('Defaults','PassHeight',10);
    XYFeedRate.Text := IniFile.ReadFloat('Defaults','XYFeedRate',5);
    ZFeedRate.Text := IniFile.ReadFloat('Defaults','ZFeedRate',5);
    editMillRPM.Text := IniFile.ReadFloat('Defaults','MillRPM',10000);
    editDrillRPM.Text := IniFile.ReadFloat('Defaults','DrillRPM',10000);
    IniFile.Free;
end;

procedure TMMExportForm_WriteFormToIniFile(dummy : Integer);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(TMMExportForm_IniFilename);
    IniFile.WriteString('Defaults','CutterDiameter',CutterDiameter.Text);
    IniFile.WriteString('Defaults','CutterDepth',CutterDepth.Text);
    IniFile.WriteString('Defaults','DrillDepth',editDrillDepth.Text);
    IniFile.WriteString('Defaults','PassHeight',PassHeight.Text);
    IniFile.WriteString('Defaults','XYFeedRate',XYFeedRate.Text);
    IniFile.WriteString('Defaults','ZFeedRate',ZFeedRate.Text);
    IniFile.WriteString('Defaults','MillRPM',editMillRPM.Text);
    IniFile.WriteString('Defaults','DrillRPM',editDrillRPM.Text);
    IniFile.Free;
end;

procedure TMMExportForm.btnExportClick(Sender: TObject);
begin
//    ShowMessage('Exporting data');
    TMMExportForm_WriteFormToIniFile(0);
    RunExporter := true;
    Close;
end;

procedure TMMExportForm.btnExitClick(Sender: TObject);
begin
    RunExporter := false;
    Close;
end;

procedure TMMExportForm.EnableDrillClick(Sender: TObject);
begin
    DrillOption.Enabled := EnableDrill.GetChecked;
end;

procedure TMMExportForm.btnMachineConfigClick(Sender: TObject);
begin
//    MMName.SetItemIndex(SetupMachineConfig(MMName.ItemIndex));
   TMMSetupForm_SetupMachineConfig(MMName.ItemIndex);
   MMSetupForm.ShowModal;
   MMExportForm.MMName.Items := MMSetupForm.MMName.Items;
   MMExportForm.MMName.SetItemIndex(MMSetupForm.MMName.ItemIndex);
end;

{..............................................................................}
//This is effectively the user's portion of the constructor
procedure TMMExportForm.UserCreate(Sender: TObject);
Var
    FileName : TPCBString;

begin
    //Read the MillExport defaults from the ini file.
    FileName := SpecialFolder_AltiumApplicationData + '\MillExport.ini';
    TMMExportForm_SetIniFilename(FileName);
    TMMExportForm_ReadIniFileToForm(0);

    //Read the list of available mill setups
    FileName := SpecialFolder_AltiumApplicationData + '\MillSetup.ini';
    TMMSetupForm_SetIniFilename(FileName);
    TMMSetupForm_SetupMachineConfig(0);
    MMExportForm.MMName.Items := MMSetupForm.MMName.Items;
    MMExportForm.MMName.SetItemIndex(MMSetupForm.MMName.ItemIndex);

//    MMExportForm.DrillOption.Items.Add('none');
    MMExportForm.DrillOption.Items.Add('spot');
    MMExportForm.DrillOption.Items.Add('full');
    MMExportForm.DrillOption.SetItemIndex(0);

    TMMExportForm_ReadIniFileToForm(0);
    RunExporter := false;
end;
