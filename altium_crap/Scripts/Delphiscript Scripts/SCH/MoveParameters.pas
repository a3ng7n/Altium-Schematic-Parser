{..............................................................................}
{ Summary Demo how to move parameters of a component                           }
{         by changing the Location property                                    }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure MoveParameters;
Var
    CurrentSch : ISch_Sheet;
    Iterator   : ISch_Iterator;
    PIterator  : ISch_Iterator;
    AComponent : ISch_Component;
    Parameter  : ISch_Parameter;
    Location   : TLocation;
Begin
    // CHeck if schematic server exists or not.
    If SchServer = Nil Then Exit;

    // Obtain the current schematic document interface.
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    // Look for components only
    Iterator := CurrentSch.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Try
        AComponent := Iterator.FirstSchObject;
        While AComponent <> Nil Do
        Begin
            Try
                PIterator := AComponent.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                Parameter := PIterator.FirstSchObject;
                While Parameter <> Nil Do
                Begin
                    //Parameter.MoveByXY(MilsToCoord(1000),MilsToCoord(2000));
                    Location := Parameter.GetState_Location;
                    Location.X := Location.X + MilsToCoord(100);
                    Location.Y := Location.X + MilsToCoord(100);
                    Parameter.SetState_Location(Location);

                    Parameter := PIterator.NextSchObject;
                End;
            Finally
                AComponent.SchIterator_Destroy(PIterator);
            End;
            AComponent := Iterator.NextSchObject;
        End;
    Finally
        CurrentSch.SchIterator_Destroy(Iterator);
    End;
End;
{..............................................................................}

{..............................................................................}
