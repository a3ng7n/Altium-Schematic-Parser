{..............................................................................}
{ Summary Deleting PCB Objects and Updating the Undo System                    }
{         The correct way to delete PCB Objects                                }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{ Version 1.1 The way tracks are removed are changed.                          }
{..............................................................................}

{..............................................................................}
Procedure RemoveTracksOnTopLayer;
var
    CurrentPCBBoard : IPCB_Board;
    Iterator        : IPCB_BoardIterator;
    Track           : IPCB_Track;
    TrackList       : TInterfaceList;
    I               : Integer;
Begin
    CurrentPCBBoard := PCBServer.GetCurrentPCBBoard;
    If CurrentPCBBoard = Nil Then Exit;

    Iterator := CurrentPCBBoard.BoardIterator_Create;
    If Iterator = Nil Then Exit;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject));
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer));


    // Fetch the first track from the board.
    // If the first track is not found, the iteration process
    // is cancelled.
    // Tracks are stored in a TInterfacelist that are to be deleted later...
    TrackList := TInterfaceList.Create;

    Try
        Track := Iterator.FirstPCBObject;
        While Track <> Nil Do
        Begin
            TrackList.Add(Track);
            Track := Iterator.NextPCBObject;
        End;
    Finally
        CurrentPCBBoard.BoardIterator_Destroy(Iterator);
    End;

    // Remove objects from the board. 
    // TInterfaceList object is updated automatically
    Try
        PCBServer.PreProcess;
        For I := 0 to TrackList.Count - 1 Do
        Begin
            Track := TrackList.items[i];
            CurrentPCBBoard.RemovePCBObject(Track);
        End;
    Finally
        PCBServer.PostProcess;
        TrackList.Free;
    End;


    // Refresh the PCB document.
    CurrentPCBBoard.ViewManager_FullUpdate;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..................................................................................................}

{..................................................................................................}
