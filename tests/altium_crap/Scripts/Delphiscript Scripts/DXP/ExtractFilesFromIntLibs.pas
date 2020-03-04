{..............................................................................}
{ Summary Extract source library files out of an integrated library            }
{ Version 1.0                                                                  }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Program ExtractSourceLibsFromIntLibs;
Var
   SourceFolder : String;
   FilesList    : TStringList;
   i            : Integer;
Begin
    If IntegratedLibraryManager = Nil then Exit;

    If (InputQuery('Extract IntLib Files','Enter folder containing IntLib files',SourceFolder)) Then
    Begin
        If (SourceFolder <> '') Then
            If (SourceFolder[Length(SourceFolder)] <> '\') Then
                SourceFolder := SourceFolder + '\';

        If (DirectoryExists(SourceFolder)) Then
        Begin
           Try
                  FilesList            := TStringList.Create;
                  FilesList.Sorted     := True;
                  FilesList.Duplicates := dupIgnore;

                  // FindFiles function is a built in function from Scripting...
                  FindFiles(SourceFolder,'*.IntLib',faAnyFile,False,FilesList);

                  For i := 0 To FilesList.Count - 1 Do
                       IntegratedLibraryManager.ExtractSources(FilesList.Strings[i]);

           Finally
                      FilesList.Free;
           End;
        End;
    End;
End.
{..............................................................................}

{..............................................................................}

