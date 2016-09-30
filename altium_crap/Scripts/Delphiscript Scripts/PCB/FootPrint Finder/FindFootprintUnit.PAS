{..............................................................................}
{ Summary Look for a footprint in a PCB Library (*.PCBLIB)                      }
{ Version 1.0                                                                  }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure FindFootprintInAPCBLIB(FileName : String);
Var
    CurrentLib        : IPCB_Library;
    FootprintIterator : IPCB_LibraryIterator;
    Footprint         : IPCB_LibComponent;
    Board             : IPCB_Board;
Begin
    CurrentLib := PCBServer.GetPCBLibraryByPath(FileName);
    If CurrentLib = Nil Then Exit;

    FootprintIterator := CurrentLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;
    Try
        Footprint := FootprintIterator.FirstPCBObject;
        While Footprint <> Nil Do
        Begin
           If Footprint.Name = FormFootprintFinder.eFootprintName.Text Then
               FormFootprintFinder.lbFootprintLibraries.Items.Add(ExtractFileName(FileName));

           Footprint := FootprintIterator.NextPCBObject;
        End;
    Finally
        CurrentLib.LibraryIterator_Destroy(FootprintIterator);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure OpenAPCBLib(PCBLibPath : String);
Var
    Document : IServerDocument;
Begin
    Document := Client.OpenDocument('PCBLIB',PCBLibPath);
    If Document <> Nil Then
        FindFootprintInAPCBLIB(Document.FileName);

    Client.CloseDocument(Document);
End;
{..............................................................................}

{..............................................................................}
Procedure TFormFootprintFinder.bSearchClick(Sender: TObject);
Var
    PCBLIBFiles      : TStringList;
    I                : Integer;
    Path             : WideString;
    LookInSubFolders : Boolean;
    FileMask         : WideString;
Begin
     Path := XPDirectoryEdit1.Text;
     Try
         PCBLIBFiles := TStringList.Create;

         // collect file paths in the PCBLibFiles
         FileMask         := '*.PCBLIB';
         LookInSubFolders := False;
         FindFiles(Path+'\',FileMask,faAnyFile,LookInSubFolders,PCBLibFiles);

         // If PCBLibs found, then look for footprints in these PCBLIBs
         If PCBLIBFiles.Count > 0 Then
             For I := 0 to PCBLIBFiles.Count - 1 Do
             Begin
                 lNumberOfLibs.Caption := IntToStr(I + 1) + ' of ' + IntToStr(PCBLIBFiles.Count) + ' Files';
                 lFootprint.Caption    := 'Searching in ' + ExtractFileName(PCBLIBFiles.Strings[I]);
                 OpenAPCBLib(PCBLIBFiles.Strings[I]);
             End;
     Finally
         PCBLIBFiles.Free;
     End;

     lFootprint.Caption := 'Search Complete!';
End;
{..............................................................................}

{..............................................................................}
Procedure TFormFootprintFinder.bCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}

