{..............................................................................}
{ Summary Component Replacer Example                                           }
{                                                                              }
{ Finds the selected component on the schematic and replaces it with           }
{ 2N3904 from the Miscellaneous Devices.IntLib at the same location            }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure ReplaceSelectedComponent;
Var
    CurrentSheet  : ISch_Document;
    Iterator      : ISch_Iterator;
    Comp          : ISch_Component;
    Loc           : TLocation;
    X             : String;
    Y             : String;
Begin
    If SchServer = Nil Then Exit;
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;

    Try
        SchServer.ProcessControl.PreProcess(CurrentSheet, '');

        // Set up iterator to look for Component objects only
        Iterator := CurrentSheet.SchIterator_Create;
        If Iterator = Nil Then Exit;

        Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
        Try
            Comp := Iterator.FirstSchObject;
            While Comp <> Nil Do
            Begin
                If Comp.Selection Then Break;

                Comp := Iterator.NextSchObject;
            End;

        Finally
            Currentsheet.SchIterator_Destroy(iterator);
        End;

        If Comp = Nil Then Exit;

        // extract component's location and other params if necessary then delete it.
        Loc := Comp.Location;
        X := IntToStr(Loc.X);
        Y := IntToStr(Loc.Y);

        CurrentSheet.RemoveSchObject(Comp);
        CurrentSheet.GraphicallyInvalidate;

        // place a new component on where the old component used to be.
        IntegratedLibraryManager.PlaceLibraryComponent(
            '2N3904',
            'Miscellaneous Devices.IntLib',
            'Orientation=1|Location.X=' + X + '|Location.Y='+Y);
    Finally
        SchServer.ProcessControl.PostProcess(CurrentSheet, '');
    End;

    CurrentSheet.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
