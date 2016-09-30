{..............................................................................}
{ Summary Reports the DXP Version                                              }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function DXPVersion : WideString;
Var
    ClientModule : IClient;
    ServerRecord : IServerRecord;
    Version      : WideString;
Begin
    ClientModule := Client;
    If ClientModule = Nil Then Exit;

    //The IServerRecord interface encapsulates the details
    // of a server's installation file

    //We are interested in the DXP's Client Module
    // and fetch the product version.
    ServerRecord := ClientModule.GetServerRecordByName('CLIENT');
    Version := ServerRecord.GetVersion;

    ShowMessage(Version);
End;
{..............................................................................}

{..............................................................................}

