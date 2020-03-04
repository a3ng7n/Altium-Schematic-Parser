{..............................................................................}
{ Summary Creates and fetches new embedded objects on a current PCB document.  }
{ Copyright (c) 2005 by Altium Limited                                         }  
{..............................................................................}

{..............................................................................}
Procedure CreateEmbeddedObjects;
Var
    Board      : IPCB_Board;
    EmbdObject : IPCB_Embedded;
Begin
    // Check if PCB board exists
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowWarning('This document is not a PCB document!');
        Exit;
    End;

    // Embedded object #1 created.
    EmbdObject             := PCBServer.PCBObjectFactory(eEmbeddedObject, eNoDimension, eCreate_Default);
    EmbdObject.Name        := 'Number One';
    EmbdObject.Description := 'Embedded object number 1 can store many chars.';
    Board.AddPCBObject(EmbdObject);

    // Embedded object #2 created.
    EmbdObject             := PCBServer.PCBObjectFactory(eEmbeddedObject, eNoDimension, eCreate_Default);
    EmbdObject.Name        := 'Number Two';
    EmbdObject.Description := 'Embedded object number 2 can store many chars.';
    Board.AddPCBObject(EmbdObject);
End;
{..............................................................................}

{..............................................................................}
Procedure RetrieveEmbeddedObjects;
Var
    Board    : IPCB_Board;
    Iterator : IPCB_BoardIterator;
    Embd     : IPCB_Embedded;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowWarning('This document is not a PCB document!');
        Exit;
    End;

    Iterator := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eEmbeddedObject));
    Iterator.AddFilter_LayerSet (AllLayers);
    Iterator.AddFilter_Method   (eProcessAll);

    Embd   := Iterator.FirstPCBObject;
    While Embd <> Nil Do
    Begin
        ShowInfo('Embedded object''s name field : ' + Embd.Name + #13#10 +
                 'Embedded description field    : ' + Embd.Description);

        Embd := Iterator.NextPCBObject;
    End;
    PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(Iterator);

End;
{..............................................................................}

{..............................................................................}
