{..............................................................................}
{ Summary: MillExporter - MMSetupDlg                                           }
{   Setup the default values for a milling machine to be used by MillExporter. }
{                                                                              }
{ Written by Marty Hauff                                                       }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}
Var
  MMSetupForm : TMMSetupForm;
  TMMSetupForm_IniFileName : TPCBString;
  MillModel : string;

{..............................................................................}
procedure TMMSetupForm.TEdit_PosReal_OnKeyPress(Sender: TObject; var Key: Char);
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

procedure TMMSetupForm.TEdit_PosInt_OnKeyPress(Sender: TObject; var Key: Char);
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
procedure TMMSetupForm_SetIniFilename (Filename: TPCBString);
var
    IniFile : TIniFile;
begin
   TMMSetupForm_IniFilename := Filename;
   If (FileExists(TMMSetupForm_IniFilename) = false) then
   begin    //Create a default file
      IniFile := TIniFile.Create(FileName);
      IniFile.WriteString('EGX-350','Model','EGX-350');
      IniFile.WriteString('EGX-350','Description','Desktop Engraver');
      IniFile.WriteString('EGX-350','Manufacturer','Roland');
      IniFile.WriteString('EGX-350','CommandSet','RML');
      IniFile.WriteString('EGX-350','CutterDiameter','0.25');
      IniFile.WriteString('EGX-350','MaxX','305');
      IniFile.WriteString('EGX-350','MaxY','230');
      IniFile.WriteString('EGX-350','MaxZ','40');
      IniFile.WriteString('EGX-350','MinXYSpeed','0.1');
      IniFile.WriteString('EGX-350','MaxXYSpeed','60');
      IniFile.WriteString('EGX-350','MinZSpeed','0.1');
      IniFile.WriteString('EGX-350','MaxZSpeed','30');
      IniFile.WriteString('EGX-350','MinSpindleSpeed','5000');
      IniFile.WriteString('EGX-350','MaxSpindleSpeed','20000');
      IniFile.WriteString('EGX-350','XMargin','5');
      IniFile.WriteString('EGX-350','YMargin','5');

      IniFile.WriteString('MDX-15','Model','MDX-15');
      IniFile.WriteString('MDX-15','Description','Desktop 3D Scanning and Milling Machine');
      IniFile.WriteString('MDX-15','Manufacturer','Roland');
      IniFile.WriteString('MDX-15','CommandSet','RML');
      IniFile.WriteString('MDX-15','CutterDiameter','0.25');
      IniFile.WriteString('MDX-15','MaxX','152.4');
      IniFile.WriteString('MDX-15','MaxY','101.6');
      IniFile.WriteString('MDX-15','MaxZ','60.5');
      IniFile.WriteString('MDX-15','MinXYSpeed','0.1');
      IniFile.WriteString('MDX-15','MaxXYSpeed','15');
      IniFile.WriteString('MDX-15','MinZSpeed','0.1');
      IniFile.WriteString('MDX-15','MaxZSpeed','15');
      IniFile.WriteString('MDX-15','MinSpindleSpeed','6500');
      IniFile.WriteString('MDX-15','MaxSpindleSpeed','6500');
      IniFile.WriteString('MDX-15','XMargin','5');
      IniFile.WriteString('MDX-15','YMargin','5');

      IniFile.WriteString('MDX-20','Model','MDX-20');
      IniFile.WriteString('MDX-20','Description','Desktop 3D Scanning and Milling Machine');
      IniFile.WriteString('MDX-20','Manufacturer','Roland');
      IniFile.WriteString('MDX-20','CommandSet','RML');
      IniFile.WriteString('MDX-20','CutterDiameter','0.25');
      IniFile.WriteString('MDX-20','MaxX','203.2');
      IniFile.WriteString('MDX-20','MaxY','152.4');
      IniFile.WriteString('MDX-20','MaxZ','60.5');
      IniFile.WriteString('MDX-20','MinXYSpeed','0.1');
      IniFile.WriteString('MDX-20','MaxXYSpeed','15');
      IniFile.WriteString('MDX-20','MinZSpeed','0.1');
      IniFile.WriteString('MDX-20','MaxZSpeed','15');
      IniFile.WriteString('MDX-20','MinSpindleSpeed','6500');
      IniFile.WriteString('MDX-20','MaxSpindleSpeed','6500');
      IniFile.WriteString('MDX-20','XMargin','5');
      IniFile.WriteString('MDX-20','YMargin','5');

      IniFile.WriteString('MDX-40','Model','MDX-40');
      IniFile.WriteString('MDX-40','Description','3D Milling Machine');
      IniFile.WriteString('MDX-40','Manufacturer','Roland');
      IniFile.WriteString('MDX-40','CommandSet','RML');
      IniFile.WriteString('MDX-40','CutterDiameter','0.25');
      IniFile.WriteString('MDX-40','MaxX','305');
      IniFile.WriteString('MDX-40','MaxY','305');
      IniFile.WriteString('MDX-40','MaxZ','105');
      IniFile.WriteString('MDX-40','MinXYSpeed','0.1');
      IniFile.WriteString('MDX-40','MaxXYSpeed','50');
      IniFile.WriteString('MDX-40','MinZSpeed','0.1');
      IniFile.WriteString('MDX-40','MaxZSpeed','30');
      IniFile.WriteString('MDX-40','MinSpindleSpeed','4500');
      IniFile.WriteString('MDX-40','MaxSpindleSpeed','15000');
      IniFile.WriteString('MDX-40','XMargin','5');
      IniFile.WriteString('MDX-40','YMargin','5');

      IniFile.Free;
   end;
end;

Procedure TMMSetupForm_ReadIniFileToForm(Name : string);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(TMMSetupForm_IniFilename);
    MMSetupForm.Model.Text := IniFile.ReadString(Name,'Model','');
    MMSetupForm.Manufacturer.Text := IniFile.ReadString(Name,'Manufacturer','');
    MMSetupForm.CommandSet.Text := IniFile.ReadString(Name,'CommandSet','RML');
    MMSetupForm.Description.Text := IniFile.ReadString(Name,'Description','');
    MMSetupForm.CutterDiameter.Text := IniFile.ReadFloat(Name, 'CutterDiameter',0.25);

    MMSetupForm.MaxX.Text := IniFile.ReadFloat(Name,'MaxX',100.0);
    MMSetupForm.MaxY.Text := IniFile.ReadFloat(Name,'MaxY',100.0);
    MMSetupForm.MaxZ.Text := IniFile.ReadFloat(Name,'MaxZ',10.0);

    MMSetupForm.MinXYSpeed.Text := IniFile.ReadFloat(Name,'MinXYSpeed',0.1);
    MMSetupForm.MaxXYSpeed.Text := IniFile.ReadFloat(Name,'MaxXYSpeed',10);
    MMSetupForm.MinZSpeed.Text := IniFile.ReadFloat(Name,'MinZSpeed',0.1);
    MMSetupForm.MaxZSpeed.Text := IniFile.ReadFloat(Name,'MaxZSpeed',10);
    MMSetupForm.MinSpindleSpeed.Text := IniFile.ReadInteger(Name,'MinSpindleSpeed',1000);
    MMSetupForm.MaxSpindleSpeed.Text := IniFile.ReadInteger(Name,'MaxSpindleSpeed',10000);
    MMSetupForm.XMargin.Text := IniFile.ReadFloat(Name,'XMargin',5.0);
    MMSetupForm.YMargin.Text := IniFile.ReadFloat(Name,'YMargin',5.0);
    IniFile.Free;
End;

Procedure TMMSetupForm_WriteFormToIniFile(MillModel : string);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(TMMSetupForm_IniFilename);
    IniFile.WriteString(MillModel,'Model',MMSetupForm.Model.Text);
    IniFile.WriteString(MillModel,'CommandSet',MMSetupForm.CommandSet.Text);
    IniFile.WriteString(MillModel,'Description',MMSetupForm.Description.Text);
    IniFile.WriteFloat(MillModel,'CutterDiameter',MMSetupForm.CutterDiameter.Text);
    IniFile.WriteFloat(MillModel,'MaxX',MMSetupForm.MaxX.Text);
    IniFile.WriteFloat(MillModel,'MaxY',MMSetupForm.MaxY.Text);
    IniFile.WriteFloat(MillModel,'MaxZ',MMSetupForm.MaxZ.Text);
    IniFile.WriteFloat(MillModel,'MinXYSpeed',MMSetupForm.MinXYSpeed.Text);
    IniFile.WriteFloat(MillModel,'MaxXYSpeed',MMSetupForm.MaxXYSpeed.Text);
    IniFile.WriteFloat(MillModel,'MinZSpeed',MMSetupForm.MinZSpeed.Text);
    IniFile.WriteFloat(MillModel,'MaxZSpeed',MMSetupForm.MaxZSpeed.Text);
    IniFile.WriteInteger(MillModel,'MinSpindleSpeed',MMSetupForm.MinSpindleSpeed.Text);
    IniFile.WriteInteger(MillModel,'MaxSpindleSpeed',MMSetupForm.MaxSpindleSpeed.Text);
    IniFile.WriteFloat(MillModel, 'XMargin',MMSetupForm.XMargin.Text);
    IniFile.WriteFloat(MillModel, 'YMargin',MMSetupForm.YMargin.Text);
    IniFile.Free;
End;
{..............................................................................}

{..............................................................................}
procedure TMMSetupForm.SaveClick(Sender: TObject);
Var
    RecordName : String;
    idx : Integer;
begin
    RecordName := MMSetupForm.MMName.Text;
    if (RecordName[strlen(RecordName)] = '*') then
       RecordName := Copy(RecordName, 1, strlen(RecordName)-1);

    idx := MMSetupForm.MMName.ItemIndex;
    MMSetupForm.MMName.Items[idx] := RecordName;
    MMSetupForm.MMName.SetItemIndex(idx);
//    MMSetupForm.MMName.Text := RecordName;
    TMMSetupForm_WriteFormToIniFile(RecordName);
    ShowMessage('Model data for ''' + RecordName + ''' has been saved');
end;

procedure TMMSetupForm.bnExitClick(Sender: TObject);
begin
   Close;
end;
{..............................................................................}

{..............................................................................}
procedure TMMSetupForm.NewClick(Sender: TObject);
Var
   NewMillName : string;
    IniFile : TIniFile;
begin
   IniFile := TIniFile.Create(TMMSetupForm_IniFilename);
   if (InputQuery('Add New Mill','Name',NewMillName) <> 0) then
   begin
      //if name doesn't exist already then
      if (IniFile.SectionExists(NewMillName)) then
        ShowMessage('An entry with that name already exists')
      else
      begin
        MMSetupForm.MMName.SetItemIndex(MMSetupForm.MMName.Items.Add(NewMillName + '*'));
        TMMSetupForm_ReadIniFileToForm(NewMillName);
      end;
      IniFile.Free;
   end
end;

procedure TMMSetupForm.DeleteClick(Sender: TObject);
Var
   ButtonPressed : Integer;
   RecordName : String;
   IniFile : TIniFile;
begin
   RecordName := MMSetupForm.MMName.Text;
   ButtonPressed := MessageDlg('Entirely remove settings for ''' + RecordName + '''',mtWarning, mbOKCancel,0);

   if (ButtonPressed = mrOK) then
   begin
      IniFile := TIniFile.Create(TMMSetupForm_IniFilename);
      IniFile.EraseSection(RecordName);
      ShowMessage('''' + RecordName + ''' deleted');
      IniFile.Free;
      MMSetupForm.MMName.Items.Delete(MMSetupForm.MMName.ItemIndex);
      MMSetupForm.MMName.SetItemIndex(0);
      TMMSetupForm_ReadIniFileToForm(MMSetupForm.MMName.Items[0]);
   end;
end;


procedure TMMSetupForm.MMNameChange(Sender: TObject);
begin
    TMMSetupForm_ReadIniFileToForm(MMSetupForm.MMName.Text);
end;
{..............................................................................}

{..............................................................................}
function TMMSetupForm_SetupMachineConfig(Idx : Integer) : Integer;
Var
    Sections : TStrings;
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(TMMSetupForm_IniFileName);
    Sections := TStringList.Create;
    IniFile.ReadSections(Sections);
    MMSetupForm.MMName.SetItems(Sections);
    MMSetupForm.MMName.SetItemIndex(Idx);
    IniFile.Free;

    TMMSetupForm_ReadIniFileToForm(MMSetupForm.MMName.Text);
    Result := MMName.ItemIndex;
end;
{..............................................................................}

