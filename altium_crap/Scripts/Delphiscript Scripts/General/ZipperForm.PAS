{..............................................................................}
{ Summary A Zipper that zips up the files of a project.                        }
{                                                                              }
{ Version 2.0                                                                  }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure RunZipper;
Begin
    FormZipper.XPProjectFileNameEdit.Filter := 'PCB Project (*.PrjPcb)|*.PrjPcb|FPGA Project (*.PrjFpg)|*.PrjPcb|Core Project (*.PrjCor)|*.PrjCor|Integrated Library (*.LibPkg)|*.LibPkg|Embedded Project (*.PrjEmb)|*.PrjPcb|Script Project (*.PrjScr)|*.PrjScr';
    FormZipper.ShowModal;
End;
{..............................................................................}

{..............................................................................}
Function FetchGeneratedFiles(AnOutputPath : TString; AnAttr : Integer; ASL : TStringList);
Begin
    // There are subfolders in an Generated Output folder
    // Normally Out is the name of the generated output folder.
    FindFiles(AnOutputPath+'\','*.*',AnAttr,True,ASL);
End;
{..............................................................................}

{..............................................................................}
Procedure ExecuteZipper(AProjectFileName   : String;
                        AZipFileName       : String;
                        AFiles             : TCheckListBox;
                        ZipProjectFileName : Boolean;
                        ZipGeneratedFiles  : Boolean;
                        ZipFilePaths       : Boolean);
Var
    Zip            : TXceedZip;
    I              : Integer;
    Workspace      : IWorkspace;
    Project        : IProject;
    OutputPath     : String;
    GeneratedFiles : TStringList;
Begin
    // If there are no files to zip, then exit
    If AFiles.Count < 1 Then Exit;

    //Create the Zip file
    Try
        Zip := TXCeedZip.Create(AZipFilename);

        // Setup Zipper, dont want to generate a temporary folder/file
        Zip.UseTempFile := False;

        // keep paths for those archived files or not.
        If ZipFilepaths Then Zip.PreservePaths := True
                        Else Zip.PreservePaths := False;


        // Check which entries are enabled and add these in the zip file
        For I := 0 to AFiles.Count - 1 Do
            If AFiles.Checked[I] Then
                Zip.AddFilesToProcess(ExtractFilePath(AProjectFileName) + AFiles.Items[I]);


        // If Project File enabled then include the Project itself in the zip file.
        If ZipProjectFileName Then Zip.AddFilesToProcess(AProjectFileName);


        // If output files enabled, then drill down to the default output folder
        // to obtain the generated files and put them in the zip file.
        If ZipGeneratedFiles Then
        Begin
            Workspace := GetWorkspace;
            Project := Workspace.DM_GetProjectFromPath(AProjectFileName);
            If Project <> Nil Then
            Begin
                OutputPath := Project.DM_GetOutputPath;

                //Fetch generated output files in the Project's Generated folder
                Try
                    GeneratedFiles := TStringList.Create;
                    FetchGeneratedFiles(OutputPath, faAnyFile, GeneratedFiles);
                    If GeneratedFiles.Count > 0 Then
                        For I := 0 to GeneratedFiles.Count - 1 Do
                            Zip.AddFilesToProcess(GeneratedFiles.Strings[I]);
                Finally
                    GeneratedFiles.Free;
                End;
            End;
        End;

        Zip.Zip;
    Finally
        Zip.Free;
    End;
    Close;
End;
{..............................................................................}

{..............................................................................}
procedure TformZipper.bCloseClick(Sender: TObject);
begin
    Close;
end;
{..............................................................................}

{..............................................................................}
procedure TformZipper.ZipClick(Sender: TObject);
Var
    FileName      : String;
    ProjectReader : TIniFile;
    I             : Integer;
Begin
    If XPProjectFileNameEdit.Text = '' Then
    Begin
        ShowWarning('Project is not selected.');
        Exit;
    End;

    If CheckListBoxProjectFiles.Items.Count > 0 Then
        ExecuteZipper(XPProjectFileNameEdit.Text,
                      XPZipFileNameEdit.Text,
                      CheckListBoxProjectFiles,
                      cbIncludeProjectFile.Checked,
                      cbIncludeGeneratedFiles.Checked,
                      cbIncludePathsInZipFile.Checked);
End;
{..............................................................................}

{..............................................................................}
procedure TformZipper.bEnableAllClick(Sender: TObject);
var
    I : Integer;
begin
    // enabling the entries
    For I := 0 to CheckListBoxProjectFiles.Items.Count - 1 Do
        CheckListBoxProjectFiles.Checked[I] := True;
end;
{..............................................................................}

{..............................................................................}
procedure TformZipper.bClearAllClick(Sender: TObject);
var
    I : Integer;
begin
    // disabling the entries
    For I := 0 to CheckListBoxProjectFiles.Items.Count - 1 Do
        CheckListBoxProjectFiles.Checked[I] := False;
end;
{..............................................................................}

{..............................................................................}
procedure TformZipper.XPProjectFileNameEditChange(Sender: TObject);
Var
    I             : Integer;
    ProjectReader : TIniFile;
    FileName      : String;
begin
    // populate the CheckListBoxProjectFiles listbox
    // A Design Prj has [DocumentN] sections and grab the
    // DocumentPath statement within

    Try
        ProjectReader := TIniFile.Create(XPProjectFileNameEdit.Text);
        I := 1;
        Repeat
            Filename := ProjectReader.ReadString('Document'+IntToStr(I),'DocumentPath','');
            If FileName <> '' Then
                CheckListBoxProjectFiles.Items.Add(FileName);

            Inc(I);
        Until Filename = '';
    Finally
        ProjectReader.Free;
    End;

    If CheckListBoxProjectFiles.Items.Count = 0 Then
    Begin
        ShowError('No files found!');
        Exit;
    End;

    // by default enable all files in the design project for zipping
    For I := 0 to CheckListBoxProjectFiles.Items.Count - 1 Do
        CheckListBoxProjectFiles.Checked[I] := True;

    // by default filename of the design project but with ZIP extension
    XPZipFileNameEdit.Text := ChangeFileExt(XPProjectFileNameEdit.Text,'.ZIP');
end;
{..............................................................................}

{..............................................................................}

