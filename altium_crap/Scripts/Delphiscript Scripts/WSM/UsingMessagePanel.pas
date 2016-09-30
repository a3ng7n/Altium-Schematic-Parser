{..............................................................................}
{ Summary Inserting text into DXP Message Panel                                }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure InsertMessagesIntoMessagePanel;
var
    WSM         : IWorkSpace;
    MM          : IMessagesManager;
    ImageIndex  : Integer;
    F           : Boolean;
Begin
    // Obtain the Workspace Manager interface
    WSM := GetWorkSpace;
    If WSM = Nil Then Exit;

    // Obtain the Messages Panel interface
    MM := WSM.DM_MessagesManager;
    If MM = Nil Then Exit;

    // Tick Icon for the lines in the Message panel
    ImageIndex := 3;

    // Clear out messages from the Message panel...
    MM.ClearMessages;
    MM.BeginUpdate;

    F := False;
    MM.AddMessage({MessageClass             } 'MessageClass 1',
                  {MessageText              } 'MessageText 1',
                  {MessageSource            } 'DXP Message',
                  {MessageDocument          } 'MessageDocument1',
                  {MessageCallBackProcess   } '',
                  {MessageCallBackParameters} '',
                  ImageIndex,
                  F);

    MM.AddMessage({MessageClass             } 'MessageClass 2',
                  {MessageText              } 'MessageText 2',
                  {MessageSource            } 'DXP Message 2',
                  {MessageDocument          } 'MessageDocument 2',
                  {MessageCallBackProcess   } '',
                  {MessageCallBackParameters} '',
                  ImageIndex,
                  F);

    MM.EndUpdate;

    // Display the Messages panel in DXP.
    WSM.DM_ShowMessageView;
End;
{..............................................................................}

{..............................................................................}
(*
Image Index values for the DM_AddMessage method...

Tick                      =  3;
Cross                     =  4;

Folder_NoError            = 6;
Folder_Warning            = 7;
Folder_Error              = 8;
Folder_Fatal              = 9;

Marker_NoError            = 107;
Marker_Warning            = 108;
Marker_Error              = 109;
Marker_Fatal              = 110;

ProjectGroup              = 54;
ProjectGroup2             = 55;
PcbLayer                  = 51;
EmptySection              =  9;
CamJob                    = 67;

BoardProject              = 56;
FpgaProject               = 57;
EmbeddedProject           = 58;
IntegratedLibrary         = 59;
FreeDocumentsProject      = 6;
*)
