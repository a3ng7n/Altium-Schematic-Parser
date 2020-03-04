{..............................................................................}
{ Summary Demo the use of the library iterator.                                }
{         Displays the number of child objects for each footprint found in a   }
{         PCB library                                                          }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure LookInsideFootprints;
Var
    CurrentLib        : IPCB_Library;
    AObject           : IPCB_Primitive;

    FootprintIterator : IPCB_LibraryIterator;
    Iterator          : IPCB_GroupIterator;

    Footprint         : IPCB_LibComponent;
    FirstTime         : Boolean;
    NoOfPrims         : Integer;
    S                 : TString;
Begin
    CurrentLib := PCBServer.GetCurrentPCBLibrary;
    If CurrentLib = Nil Then
    Begin
        ShowMessage('This is not a PCB Library document');
        Exit;
    End;

    // For each page of library is a footprint
    FootprintIterator := CurrentLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;

    S         := '';
    FirstTime := True;
    Try
        // Within each page, fetch primitives of the footprint
        // A footprint is a IPCB_LibComponent inherited from
        // IPCB_Group which is a container object that stores primitives.
        Footprint := FootprintIterator.FirstPCBObject;
        While Footprint <> Nil Do
        Begin
           If FirstTime Then
           Begin
              S := S + ExtractFileName(Footprint.Board.FileName) + #13;
              S := S + ' Current Footprint : ' + CurrentLib.CurrentComponent.Name + #13 + #13;
           End;

           S := S + Footprint.Name;

           Iterator := Footprint.GroupIterator_Create;
           Iterator.SetState_FilterAll;

           // Counts number of prims for each Footprint as a IPCB_LibComponent
           NoOfPrims := 0;
           AObject := Iterator.FirstPCBObject;
           While (AObject <> Nil) Do
           Begin
               // counts child objects or primitives
               // for each footprint.
               Inc(NoOfPrims);

               // do what you want with the AObject.
               AObject := Iterator.NextPCBObject;
           End;

           S := S + ' has ' + IntToStr(NoOfPrims) + ' Primitives.' + #13;
           FirstTime := False;
           Footprint.GroupIterator_Destroy(Iterator);
           Footprint := FootprintIterator.NextPCBObject;
        End;
    Finally
        CurrentLib.LibraryIterator_Destroy(FootprintIterator);
    End;

    ShowMessage(S);
End;
{..............................................................................}

{..............................................................................}
