/*..............................................................................*/
/* Summary Demo of inserting a schematic sheet in a new PCB Project             */
/*         There needs to be a sheet1.schdoc file in C:\ folder                 */
/* Copyright (c) 2003 by Altium Limited                                         */
/*..............................................................................*/
/*                                                                              */
/*..............................................................................*/

var Workspace;
var Document;
var SheetFileName;

// Need to start up Schematic Server first.
Client.StartServer("SCH")

// Create a blank project.
Client.SendMessage("WorkspaceManager:OpenObject","Kind=PcbProject | ObjectKind=NewAnything",1024,null)

SheetFileName = "C:\\Sheet1.SchDoc"
Document      = Client.OpenDocument("Sch", SheetFileName)

if(Document != null)
{
    //Add this schematic sheet in the newly created PCB project in DXP.
    Workspace = GetWorkspace
    if(Workspace != null)
       Workspace.DM_FocusedProject.DM_AddSourceDocument(Document.FileName)


    Client.ShowDocument(Document)
}


