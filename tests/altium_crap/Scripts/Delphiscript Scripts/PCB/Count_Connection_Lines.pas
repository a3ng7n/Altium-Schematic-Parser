{..............................................................................}
{ Summary                                                                      }
{ Demo How to find and list all connection lines in the PCB                    }
{      and display the connection line data on the Messages panel.             }
{                                                                              }
{ Version 1.0                                                                  }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure DisplayResultsInMessagePanel(Results : TStringList);
Var
    WSM         : IWorkSpace;
    MM          : IMessagesManager;
    ImageIndex  : Integer;
    F           : Boolean;
    I           : Integer;
Begin
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
    For I := 0 to Results.Count - 1 Do
    Begin
        MM.AddMessage({MessageClass             } 'Connection Line: ' + IntToStr(I + 1),
                      {MessageText              } Results.Strings[I],
                      {MessageSource            } '',
                      {MessageDocument          } '',
                      {MessageCallBackProcess   } '',
                      {MessageCallBackParameters} '',
                      ImageIndex,
                      F);
    End;
    MM.EndUpdate;

    // Display the Messages panel in Altium Designer.
    WSM.DM_ShowMessageView;
End;
{..............................................................................}

{..............................................................................}
Procedure CountAndFindConnections;
Var
    Board            : IPCB_Board;
    Connect          : IPCB_Connection;
    Iterator         : IPCB_BoardIterator;
    ConnectionList   : TStringList;
    ConnectNumber    : Integer;
    Document         : IServerDocument;
    FileName         : TPCBString;
Begin
    ConnectNumber       := 0;

    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    ConnectionList := TStringList.Create;
    // retrieve the iterator
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eConnectionObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    Try
        // Search and count connection lines
        Connect := Iterator.FirstPCBObject;
        While (Connect <> Nil) Do
        Begin
            Inc(ConnectNumber);

            // Makes a string list of connections
            ConnectionList.Add('Connection Line Owner Net:  ' + Connect.Net.Name + ' ' +
            'X1:' + IntToStr(CoordToMils(Connect.X1)) + ' ' +
            'Y1:' + IntToStr(CoordToMils(Connect.Y1)) + ' ' +
            'X2:' + IntToStr(CoordToMils(Connect.X2)) + ' ' +
            'Y2:' + IntToStr(CoordToMils(Connect.Y2)));

            // Fetch the next connection line object
            Connect := Iterator.NextPCBObject;
        End;

        // Free the iterator from memory
        Board.BoardIterator_Destroy(Iterator);

        // List the total of connection lines found from the current PCB
        ConnectionList.Add(#13 + 'Total Connection Lines:  ' + IntToStr(ConnectNumber));

        // Get the filename of the current PCB and change its file extension to TXT
        FileName := ChangeFileExt(Board.FileName,'.txt');

        // Save list of connection lines to a file
        ConnectionList.SaveToFile(Filename);

        // Display the results in the Message panel.
        DisplayResultsInMessagePanel(ConnectionList);
    Finally
        ConnectionList.Free;
    End;
End;
{..............................................................................}

{..............................................................................}
