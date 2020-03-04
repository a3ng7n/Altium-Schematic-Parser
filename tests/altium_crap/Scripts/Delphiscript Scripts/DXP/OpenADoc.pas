{..............................................................................}
{ Summary Demo how to open a text document using Client's Open Document.       }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure OpenAndShowATextDocument;
Var
    Filename       : TString;
    ReportDocument : IServerDocument;
Begin
    If Client = Nil Then Exit;

    // Opens an autoexec.bat text file but dont focus it.
    FileName := 'C:\Autoexec.bat';
    ReportDocument := Client.OpenDocumentShowOrHide('Text',FileName,False);
    If ReportDocument <> Nil Then
         Client.ShowDocumentDontFocus(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
