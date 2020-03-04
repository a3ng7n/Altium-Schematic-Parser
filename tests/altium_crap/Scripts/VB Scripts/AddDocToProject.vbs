'..............................................................................
' Summary Demo of inserting a schematic sheet in a new PCB Project
'         There needs to be a sheet1.schdoc file in C:\ folder
' Copyright (c) 2003 by Altium Limited
'..............................................................................

'..............................................................................

Dim Workspace
Dim Document
Dim SheetFileName

' Need to start up Schematic Server first.
Client.StartServer("SCH")

' Create a blank project.
Call Client.SendMessage("WorkspaceManager:OpenObject","Kind=PcbProject | ObjectKind=NewAnything",1024,Null)

SheetFileName = "C:\Sheet1.SchDoc"
Set Document  = Client.OpenDocument("Sch", SheetFileName)

If Not (Document Is Nothing) Then
    'Add this schematic sheet in the newly created PCB project in DXP.
    Set Workspace = GetWorkspace
    If Not (Workspace Is Nothing) Then
       Workspace.DM_FocusedProject.DM_AddSourceDocument(Document.FileName)
    End If

    Client.ShowDocument(Document)
End If


