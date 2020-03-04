{..............................................................................}
{ Summary Set a document dirty, so it gets saved when Save command is invoked. } 
{ Copyright (c) 2003 by Altium Limited                                         } 
{..............................................................................}

{..............................................................................}
Procedure SetDocumentDirty;
Var
    AView           : IServerDocumentView;
    AServerDocument : IServerDocument;
Begin
    If Client = Nil Then Exit;
    // Grab the current document view using the Client's Interface.
    AView := Client.GetCurrentView;

    // Grab the server document which stores views by extracting the ownerdocument field.
    AServerDocument := AView.OwnerDocument;

    // Set the document dirty.
    AServerDocument.Modified := True;
End;
{..............................................................................}