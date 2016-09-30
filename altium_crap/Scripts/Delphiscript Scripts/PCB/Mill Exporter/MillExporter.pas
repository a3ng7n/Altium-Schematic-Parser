{..............................................................................}
{ Summary: MillExporter                                                        }
{   Export a PCB design into a format that can be milled by a CNC Milling      }
{ machine.  Currently only Roland RML milling language is supported.           }
{                                                                              }
{ To use this script:                                                          }
{ 1) DXP >> Preferences >> Scripting System >> Global Projects                 }
{ 2) Install this project                                                      }
{ 3) With a PCB document open, DXP >> Run Script ... RunMillExporter           }
{ 4) Fill out the appropriate information in the dialog box and click Export   }
{ 5) A unique file will be created for each layer to be milled as well as each }
{ drill size to be drilled                                                     }
{                                                                              }
{ NOTE: MillExport.ini and MillSetup.ini will be saved in the Altium Designer  }
{       application data area:                                                 }
{ i.e. C:\Documents and Settings\user\Application Data\AltiumDesignerSummer08  }
{                                                                              }
{ Written by Marty Hauff                                                       }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}
Const
    //Constants used in the messages panel
   IMG_Wire       = 1;
   IMG_Component    = 2;
   IMG_Tick      = 3;
   IMG_Cross     = 4;
   IMG_OutputToFile = 67;
   IMG_OpenDocument = 68;
   IMG_GreenSquare = 71;

Var
   Board        : IPCB_Board;       //The Board
   RefPoint     : TCoordPoint;
   OffsetPoint  : TCoordPoint;
   RouterModel  : string;           //The target milling machine
   ReportLog    : TStringList;      //The Report log
   WSM          : IWorkSpace;       //Used for reporting errors to the Messages Panel
   MsgMgr      : IMessagesManager;

//General Options
   DrillHoles : string;     //Permissable values (none, spot, full)
   OriginPadName : string;  //Name of free pad on PCB document to be used as the 0,0 reference point.

//Mill specific Options
   MaxX : Integer;          //Maximum X value that can be milled (units 0.01mm)
   MaxY : Integer;          //Maximum Y value that can be milled (units 0.01mm)
   MaxZ : Integer;          //Maximum possible height of milling head (units 0.01mm)
//   Z1 : Integer;            //Drill/Mill Depth of 'PD' commands (units 0.01mm)
//   Z2 : Integer;            //Drill/Mill Height of 'PU' commands (units 0.01mm)
   XYSpeed : Integer;       //Milling speed in X,Y direction (units 0.01mm)
   ZSpeed : Integer;        //Milling speed in Z direction (units 0.01mm)
   MillRPM : Integer;       //Milling head spead (units RPM)
   MaxRPM : Integer;        //Maximum permissible head speed for the selected model
   MinRPM : Integer;        //Minimum permissible head speed for the selected model
   MillDepth : Integer;     //Milling Depth when head in the 'Pen Down' position (units 0.01mm)
   ClearDepth : Integer;    //Depth of head when in the 'Pen Up' position (units 0.01mm)
   DrillDepth : Integer;    //Depth that holes are to be drilled to when performing 'full' hole drilling

{..............................................................................}
Procedure WriteReport(Msg : string);
Begin
    ReportLog.Add (Msg);
{  MsgMgr.BeginUpdate;
MsgMgr.AddMessage('MessageClass 1',
                Msg,
                     'Export to RML',
                     Board.FileName,
                     '',
                     '',
                     3,
                     false);
   MsgMgr.EndUpdate; }
End;
{..............................................................................}

{..............................................................................}
Procedure WriteReportAndExit(Msg : string);
Var
    FileName      : WideString;
    Document      : IServerDocument;
Begin
    if (Msg <> '') then
       ReportLog.Add(Msg);

    FileName := ChangeFileExt(Board.FileName, '.rpt');
    ReportLog.SaveToFile(Filename);
    ReportLog.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);
//    Client.ShowDocument(Client.LastActiveDocumentOfType('PCB'));
//  Exit;
End;
{..............................................................................}

{..............................................................................}
Function CoordToRMLUnit (C: TReal) : Integer;
Begin
     Result := Int(CoordToMMs (C) * 100);
End;
{..............................................................................}

{..............................................................................}
Function XCoordToRMLUnit (C: TReal) : Integer;
Begin
     Result := CoordToRMLUnit(abs((C + OffsetPoint.X) - RefPoint.X));
End;
{..............................................................................}

{..............................................................................}
Function YCoordToRMLUnit (C: TReal) : Integer;
Begin
     Result := CoordToRMLUnit(abs((C + OffsetPoint.Y) - RefPoint.Y));
End;
{..............................................................................}

{..............................................................................}
Procedure RML_RouteRegion(Contour : IPCB_Contour; StringList : TStringList);
Var
    I : Integer;
    Commands : string;
Begin
    Commands := '^PU' + IntToStr(XCoordToRMLUnit(Contour.X[0])) + ',' + IntToStr(YCoordToRMLUnit(Contour.Y[0])) + ';';
    For I := 1 To Contour.Count Do
    Begin
      Commands := Commands + '^PD' + IntToStr(XCoordToRMLUnit(Contour.X[I])) + ',' + IntToStr(YCoordToRMLUnit(Contour.Y[I])) + ';';
    End;
    StringList.Add(Commands);
End;
{..............................................................................}

{..............................................................................}
Procedure RML_ExportPreRouteCommands(StringList : TStringList);
Var
   Commands : string;
Begin
    WriteReport('Exporting pre route commands');
        Commands := chr(03) + ';';              //Not quite sure why we start with this
        Commands := Commands + '^IN;';          //Initialize
        Commands := Commands + '!MC1;';         //Set the spindle motor to a rotateable state
        Commands := Commands + '^PA;';          //Plot Absolute
        StringList.Add(Commands);

        Commands := '!RC' + IntToStr(MillRPM) + ';';    //Set the motor speed
        Commands := Commands + '^VS' + IntToStr(XYSpeed) + ';';        //Set the tool XY speed
        Commands := Commands + '!VZ' + IntToStr(ZSpeed) + ';';        //Set the Z axis tool speed
//        Commands := Commands + '!PZ-25,100;';   //Set Z1=-0.25mm, Z2=1.0mm
        Commands := Commands + '!PZ-' + IntToStr(MillDepth) + ',' + IntToStr(ClearDepth) + ';';   //Set the 'Pen Down' and 'Pen Up' depths as an offset from the Zero depth.
        StringList.Add(Commands);
End;
{..............................................................................}

{..............................................................................}
Procedure RML_ExportPreDrillCommands(StringList : TStringList);
Var
   Commands : string;
Begin
    WriteReport('Exporting pre drill commands');
        Commands := chr(03) + ';';              //Not quite sure why we start with this
        Commands := Commands + '^IN;';          //Initialize
        Commands := Commands + '!MC1;';         //Set the spindle motor to a rotateable state
        Commands := Commands + '^PA;';          //Plot Absolute
        StringList.Add(Commands);

        Commands := '!RC' + IntToStr(MillRPM) + ';';    //Set the motor speed
        Commands := Commands + '^VS' + IntToStr(XYSpeed) + ';';        //Set the tool XY speed
        Commands := Commands + '!VZ' + IntToStr(ZSpeed) + ';';        //Set the Z axis tool speed
//        Commands := Commands + '!PZ-25,50;';   //Set Z1=-0.25mm, Z2=1.0mm
        Commands := Commands + '!PZ-' + IntToStr(DrillDepth) + ',' + IntToStr(ClearDepth) + ';';   //Set the 'Pen Down' and 'Pen Up' depths as an offset from the Zero depth.
        StringList.Add(Commands);
End;
{..............................................................................}

{..............................................................................}
Function GetPolygonObjectByName(PolygonName : string) : IPCB_Polygon;
Var
    BoardIterator : IPCB_BoardIterator;

Begin
    // Retrieve the iterator
    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Result := BoardIterator.FirstPCBObject;
    if (Result = nil) then Exit;

    while ((Result <> Nil) and (Result.Name <> PolygonName)) Do
    Begin
       Result := BoardIterator.NextPCBObject;
    End;
    if (Result <> Nil) then
    begin
        If (Result.Name <> PolygonName) then
            Result := nil;
    End;
    Board.BoardIterator_Destroy(BoardIterator);
End;
{..............................................................................}

{..............................................................................}
Function GetPadObjectByName(PadName : string) : IPCB_Pad;
Var
    BoardIterator : IPCB_BoardIterator;

Begin
    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(ePadObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Result := BoardIterator.FirstPCBObject;
    while ((Result <> Nil) and (Result.Name <> PadName)) Do
    Begin
       Result := BoardIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(BoardIterator);
End;
{..............................................................................}

{..............................................................................}
Procedure SetPolygonToBoardOutline(Polygon : IPCB_Polygon);
Var
    I       : Integer;
    RepourMode : TPolygonRepourMode;

Begin
    WriteReport('Resizing polygon ''' + Polygon.Name + ''' to board outline');

    //Save the current Polygon repour setting
    RepourMode := PCBServer.SystemOptions.PolygonRepour;
    //Update so that Polygons always repour - avoids polygon repour yes/no dialog box popping up.
    PCBServer.SystemOptions.PolygonRepour := eAlwaysRepour;

//    Polygon := GetPolygonObjectByName(PolygonName);
    if (Polygon = Nil) then
       WriteReportAndExit('ERROR: Can''t find polygon ''' + Polygon.Name + '''');

    PCBServer.PreProcess;
    PCBServer.SendMessageToRobots(Polygon.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

    Polygon.PointCount := Board.BoardOutline.PointCount;
    For I := 0 To Board.BoardOutline.PointCount Do
    Begin
       Polygon.Segments[I] := Board.BoardOutline.Segments[I];
    End;

    PCBServer.SendMessageToRobots(Polygon.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
    PCBServer.PostProcess;
    //Revert back to previous user polygon repour option.
    PCBServer.SystemOptions.PolygonRepour := RepourMode;

    Polygon.Rebuild;
    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
End;
{..............................................................................}

{..............................................................................}
Procedure SetRefPointForLayer (Layer : TLayer);
Var
    Pad           : IPCB_Primitive;
    PadName       : string;
    I             : Integer;

Begin
    WriteReport('SetRefPointForLayer(' + cLayerStrings[Layer] + ')');
//    if (OriginPadName = 'auto') then
//    begin
        //Find the origin point by finding the minX and MinY values of the board outline.
        RefPoint.X := Board.BoardOutline.Segments[0].VX;
        RefPoint.Y := Board.BoardOutline.Segments[0].VY;
        For I := 1 To Board.BoardOutline.PointCount Do
        Begin
           if (Board.BoardOutline.Segments[I].VX < RefPoint.X) then
               RefPoint.X := Board.BoardOutline.Segments[I].VX;
           if (Board.BoardOutline.Segments[I].VY < RefPoint.Y) then
               RefPoint.Y := Board.BoardOutline.Segments[I].VY;
        End;
{    End
    Else
    Begin
        Pad := GetPadObjectByName(OriginPadName);
        If Pad = Nil then
        Begin
             WriteReportAndExit('ERROR: Could not find reference Pad: ''' + OriginPadName + '''');
        End;

        RefPoint.Y := Pad.Y;
        RefPoint.X := Pad.X;
    end;
 }
    If (Layer = eBottomLayer) Then
       RefPoint.X := RefPoint.X + MMsToCoord(MaxX/100);

    WriteReport(' ... using point ' + IntToStr(CoordToMMs(RefPoint.X)) + ',' + IntToStr(CoordToMMs(RefPoint.Y)) + ' as ZeroPoint');

End;
{..............................................................................}

{..............................................................................}
Procedure UpdateLocalVariables(dummy : Integer);
// NOTE: Need to remove mill dependent hard coded values - i.e. mm -> RML units.
Begin
    MsgMgr.BeginUpdate;

    MsgMgr.AddMessage('[Notice]','Loading settings for ' + RouterModel,'RML Generator','','','',IMG_OpenDocument, false);
    WriteReport('Loading settings for: ''' + RouterModel + '''');

//Options
{    MsgMgr.AddMessage('[Notice]','Loading general settings','RML Generator',IniFileName,'','',IMG_OpenDocument, false); }

{    OriginPadName := IniFile.ReadString('Options','OriginPadName','auto');
    MsgMgr.AddMessage('[Notice]','   OriginPadName: ' + OriginPadName,'RML Generator',IniFileName,'','',IMG_GreenSquare, false);
    WriteReport('OriginPadName: ' + OriginPadName); }
    if (MMExportForm.EnableDrill.GetChecked = true) then
       DrillHoles := MMExportForm.DrillOption.Text
    else
       DrillHoles := 'none';
    MsgMgr.AddMessage('[Notice]','   DrillHoles: ' + DrillHoles,'RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('DrillHoles: ' + DrillHoles);

//    MaxX := StrToInt(IniFile.ReadString(RouterModel,'MaxX',0));
    MaxX := MMSetupForm.MaxX.Text * 100;
    MsgMgr.AddMessage('[Notice]','   MaxX: ' + FloatToStr(MaxX/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('MaxX: ' + IntToStr(MaxX));

//    MaxY := StrToInt(IniFile.ReadString(RouterModel,'MaxY',0));
    MaxY := MMSetupForm.MaxY.Text * 100;
    MsgMgr.AddMessage('[Notice]','   MaxY: ' + FloatToStr(MaxY/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('MaxY: ' + IntToStr(MaxY));

//    MaxZ := StrToInt(IniFile.ReadString(RouterModel,'MaxZ',0));
    MaxZ := MMSetupForm.MaxZ.Text * 100;
    MsgMgr.AddMessage('[Notice]','   MaxZ: ' + FloatToStr(MaxZ/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('MaxZ: ' + IntToStr(MaxZ));
{
//    Z1 := StrToInt(IniFile.ReadString(RouterModel,'Z1',0));
    Z1 := MMExportForm.CutterDepth.Text * 100;
    MsgMgr.AddMessage('[Notice]','   Z1: ' + FloatToStr(Z1/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('Z1: ' + IntToStr(Z1));

//    Z2 := StrToInt(IniFile.ReadString(RouterModel,'Z2',0));
    Z2 := MMExportForm.PassHeight.Text * 100;
    MsgMgr.AddMessage('[Notice]','   Z2: ' + FloatToStr(Z2/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('Z2: ' + IntToStr(Z2));
}
//    XYSpeed := StrToInt(IniFile.ReadString(RouterModel,'XYSpeed',0));
    XYSpeed := MMExportForm.XYFeedRate.Text;
    MsgMgr.AddMessage('[Notice]','   XYSpeed: ' + FloatToStr(XYSpeed) + 'mm/sec','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('XYSpeed: ' + IntToStr(XYSpeed));

//    ZSpeed := StrToInt(IniFile.ReadString(RouterModel,'ZSpeed',0));
    ZSpeed := MMExportForm.ZFeedRate.Text;
    MsgMgr.AddMessage('[Notice]','   ZSpeed: ' + FloatToStr(ZSpeed) + 'mm/sec','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('ZSpeed: ' + IntToStr(ZSpeed));

//    MillRPM := StrToInt(IniFile.ReadString(RouterModel,'MillRPM',0));
    MillRPM := MMExportForm.editMillRPM.Text;
    MsgMgr.AddMessage('[Notice]','   MillRPM: ' + IntToStr(MillRPM) + 'rpm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('MillRPM: ' + IntToStr(MillRPM));

//    MillDepth := StrToInt(IniFile.ReadString(RouterModel,'MillDepth',0));
    MillDepth := MMExportForm.CutterDepth.Text * 100;
    MsgMgr.AddMessage('[Notice]','   MillDepth: ' + FloatToStr(MillDepth/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('MillDepth: ' + IntToStr(MillDepth));

//    ClearDepth := StrToInt(IniFile.ReadString(RouterModel,'ClearDepth',0));
    ClearDepth := MMExportForm.PassHeight.Text * 100;
    MsgMgr.AddMessage('[Notice]','   ClearDepth: ' + FloatToStr(ClearDepth/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('ClearDepth: ' + IntToStr(ClearDepth));

//    DrillDepth := IniFile.ReadString(RouterModel,'DrillDepth',0);
    DrillDepth := MMExportForm.editDrillDepth.Text * 100;
    MsgMgr.AddMessage('[Notice]','   DrillDepth: ' + FloatToStr(DrillDepth/100) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('DrillDepth: ' + IntToStr(DrillDepth));

    OffsetPoint.X := MMsToCoord(strtofloat(MMSetupForm.Xmargin.Text));
    OffsetPoint.Y := MMsToCoord(strtofloat(MMSetupForm.Ymargin.Text));
    MsgMgr.AddMessage('[Notice]','   Offset(X,Y): ' + FloatToStr(CoordToMMS(OffsetPoint.X)) + ',' + FloatToStr(CoordToMMS(OffsetPoint.Y)) + 'mm','RML Generator','','','',IMG_GreenSquare, false);
    WriteReport('Offset(X,Y): ' + FloatToStr(CoordToMMS(OffsetPoint.X)) + ',' + FloatToStr(CoordToMMS(OffsetPoint.Y)) + 'mm');

    WriteReport('');
    MsgMgr.EndUpdate;
End;
{..............................................................................}

{..............................................................................}
procedure RML_ExportPolygonOutline (Polygon : IPCB_Polygon; RMLOut : TStringList);
var
    RegionIterator : IPCB_GroupIterator;
    Region        : IPCB_Region;
    I             : Integer;

Begin
    WriteReport('RML_ExportPolygonOutline(' + Polygon.Name + ')');

    //Start running through the regions and write out their vertices
    RegionIterator := Polygon.GroupIterator_Create;
    RegionIterator.AddFilter_ObjectSet(MkSet(eRegionObject));
    Region := RegionIterator.FirstPCBObject;

    WriteReport('Exporting polygon contour');
    While (Region  <> Nil) Do
    Begin
       RML_RouteRegion(Region.MainContour, RMLOut);
       For I := 0 To Region.HoleCount - 1 Do
       Begin
          RML_RouteRegion(Region.Holes[I], RMLOut);
       End;
       Region := RegionIterator.NextPCBObject;
    End;
    Polygon.GroupIterator_Destroy (RegionIterator);
End;
{..............................................................................}

{..............................................................................}
Procedure RML_ExportDrillFiles (dummy : integer);
Var
    RegionIterator : IPCB_GroupIterator;
    Region        : IPCB_Region;
    BoardIterator : IPCB_BoardIterator;
    HoleItem      : IPCB_Primitive;
    HoleCount     : Integer;
    FileName      : TPCBString;
    Document      : IServerDocument;
    I             : Integer;
    Commands      : string;
    HoleSizeList : TList;
    HoleCountList : TList;
    DrillCommands : TList;

Begin
//    Board := PCBServer.GetCurrentPCBBoard;
//    If Board = Nil Then Exit;

     //Required because scripting system can't cope with TCoordPoint types
//    RefPoint := TCoordPoint;

    MsgMgr.BeginUpdate;
    MsgMgr.AddMessage('[Start Output]','Start output generation','Export RML Drill Files','','','',IMG_OutputToFile, false);
    MsgMgr.EndUpdate;
    WriteReport('ExportRMLDrillFiles');
    SetRefPointForLayer(eTopLayer);

    HoleSizeList := TList.Create;
    HoleCountList := TList.Create;
    DrillCommands := TList.Create;
    WriteReport('Scanning board for holes');
    BoardIterator := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(ePadObject, eViaObject));
    BoardIterator.AddFilter_LayerSet(AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    HoleItem := BoardIterator.FirstPCBObject;
    HoleCount := 0;
    while (HoleItem <> Nil) Do
    Begin
       If HoleItem.holesize <> 0 then
       Begin
          Inc(HoleCount);
          I := HoleSizeList.IndexOf(HoleItem.holesize);
          if (I < 0) then
          begin
              HoleSizeList.Add(HoleItem.holesize);
              I := HoleSizeList.Count - 1;
              DrillCommands.Add(TStringList.Create);        //add a new string list for the drill files
              HoleCountList.Add(1);
              RML_ExportPreDrillCommands(DrillCommands.Items[I]);
          end;
          DrillCommands.Items[I].Add('^PU' + IntToStr(XCoordToRMLUnit(HoleItem.X)) + ',' + IntToStr(YCoordToRMLUnit(HoleItem.Y)) + ';'
                    + '^PD' + IntToStr(XCoordToRMLUnit(HoleItem.X)) + ',' + IntToStr(YCoordToRMLUnit(HoleItem.Y)) + ';');
          HoleCountList.Items[I] := HoleCountList.Items[I] + 1;
       End;
       HoleItem := BoardIterator.NextPCBObject;
    End;

    //Save the different drill files
    for I := 0 to HoleSizeList.Count - 1 Do
    Begin
       WriteReport('HoleSizeList[' + IntToStr(I) + '] is ' + FloatToStr(CoordToMMs(HoleSizeList.Items[I])) + 'mm');
      MsgMgr.AddMessage('[Notice]', IntToStr(HoleCountList.Items[I]) + ' holes found at apperture: ' + FloatToStr(CoordToMMs(HoleSizeList.Items[I])) + 'mm', 'Export RML Drill Files','','','',IMG_GreenSquare, false);
       DrillCommands.Items[I].Add('^PU0,0;');

    // Display the Exported RML commands
       FileName := ChangeFileExt(Board.FileName,'_' + RouterModel + '_' + 'Drill_' + FloatToStr(CoordToMMs(HoleSizeList.Items[I])) + 'mm' + '.mdt');
      MsgMgr.AddMessage('[Generated File]','Drill file saved and added to project', 'Export RML Drill Files', ExtractFileName(FileName),'','',IMG_OutputToFile, false);
       WriteReport('Saving Drill File: ' + Filename);
       DrillCommands.Items[i].SaveToFile(Filename);
       DrillCommands.Items[i].Free;

       GetWorkspace.DM_FocusedProject.DM_AddSourceDocument(Filename);
       Document  := Client.OpenDocument('Text', FileName);
       If Document <> Nil Then
          Client.ShowDocument(Document);
       Client.ShowDocument(Client.LastActiveDocumentOfType('PCB'));

    End;
    Board.BoardIterator_Destroy(BoardIterator);
    HoleSizeList.Free;
    HoleCountList.Free;

    MsgMgr.AddMessage('[Finished Output]','Finished output generation','Export RML Drill Files','','','',IMG_OutputToFile, false);
    MsgMgr.EndUpdate;

End;
{..............................................................................}

{..............................................................................}
Function AddMillClearanceRule(PolygonName : string) : IPCB_ClearanceConstraint;
Begin
   //Create a new Clearance Rule
   Result := PCBServer.PCBRuleFactory(eRule_Clearance);
   PCBServer.SystemOptions.PolygonRepour := eAlwaysRepour;
   PCBServer.PreProcess;
   PCBServer.SendMessageToRobots(Result.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

   Result.Name := '__MillClearanceRule';
   Result.Scope1Expression := 'InNamedPolygon(''' + PolygonName + ''')';
   Result.Scope2Expression := 'All';
   Result.Gap := MMsToCoord(StrToFloat(MMSetupForm.CutterDiameter.Text)/2);
   Board.AddPCBObject(Result);
   PCBServer.SendMessageToRobots(Result.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
   PCBServer.PostProcess;
End;
{..............................................................................}

{..............................................................................}
Function MakeNewPolygon(PolygonName : string; Layer : TLayer) : IPCB_Polygon;
Var
    I       : Integer;
    RepourMode : TPolygonRepourMode;

Begin
    WriteReport('MakeNewPolygon: ''' + PolygonName + ''' on Layer ''' + cLayerStrings[Layer] + '''');

    //Save the current Polygon repour setting
    RepourMode := PCBServer.SystemOptions.PolygonRepour;
    //Update so that Polygons always repour - avoids polygon repour yes/no dialog box popping up.
    PCBServer.SystemOptions.PolygonRepour := eAlwaysRepour;

    Result := PCBServer.PCBObjectFactory(ePolyObject, eNoDimension, eCreate_Default);
    Board.AddPCBObject(Result);

    PCBServer.PreProcess;
    PCBServer.SendMessageToRobots(Result.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

    Result.Name := PolygonName;
    Result.Layer := Layer;
    Result.PolyHatchStyle := ePolySolid;
    Result.RemoveIslandsByArea := false;
    Result.RemoveNarrowNecks := false;
    Result.ArcApproximation := MilsToCoord(0.5);
    Result.RemoveDead := false;
    Result.PourOver := ePolygonPourOver_None;
    Result.Net := Nil;

    Result.PointCount := Board.BoardOutline.PointCount;
    For I := 0 To Board.BoardOutline.PointCount Do
    Begin
       Result.Segments[I] := Board.BoardOutline.Segments[I];
    End;

    PCBServer.SendMessageToRobots(Result.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
    PCBServer.PostProcess;
    //Revert back to previous user polygon repour option.
    PCBServer.SystemOptions.PolygonRepour := RepourMode;

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
End;
{..............................................................................}

{..............................................................................}
procedure RML_ExportDrillData(dummy : Integer);
begin
end;

{..............................................................................}
procedure RML_ExportMillLayerData (Layer : TLayer; RMLOut : TStringList);
var
    BoardIterator : IPCB_BoardIterator;
    MillPolygonName : string;
    DeletePolygonWhenDone : IPCB_Polygon;
    DeletePolyClearanceRuleWhenDone : IPCB_ClearanceConstraint;
    Polygon : IPCB_Polygon;
    Hole           : IPCB_Primitive;
    Commands      : string;

begin
    WriteReport('RML_ExportMillLayerData');

    DeletePolygonWhenDone := nil;
    DeletePolyClearanceRuleWhenDone := nil;

    SetRefPointForLayer(Layer);
    RML_ExportPreRouteCommands(RMLOut);

    if ((Layer = eTopLayer) and (DrillHoles = 'spot')) then
    begin
       WriteReport('Spot drilling holes');
       BoardIterator := Board.BoardIterator_Create;
       BoardIterator.AddFilter_ObjectSet(MkSet(ePadObject, eViaObject));
       BoardIterator.AddFilter_LayerSet(AllLayers);
       BoardIterator.AddFilter_Method(eProcessAll);

       Hole := BoardIterator.FirstPCBObject;
       Commands := '';
       While (Hole <> Nil) Do
       Begin
           If Hole.HoleSize > 0 then
           begin
                Commands := Commands + '^PU' + IntToStr(XCoordToRMLUnit(Hole.X)) + ',' + IntToStr(YCoordToRMLUnit(Hole.Y)) + ';';
                Commands := Commands + '^PD' + IntToStr(XCoordToRMLUnit(Hole.X)) + ',' + IntToStr(YCoordToRMLUnit(Hole.Y)) + ';';
           end;
           Hole := BoardIterator.NextPCBObject;
       End;
       RMLOut.Add(Commands);
       Board.BoardIterator_Destroy(BoardIterator);
    end;

    //Create a temporary polygon that will cover the entire board
    MillPolygonName := '__TempOutlinePolygon';
    DeletePolyClearanceRuleWhenDone := AddMillClearanceRule(MillPolygonName);
    Polygon := MakeNewPolygon(MillPolygonName, Layer);
    DeletePolygonWhenDone := Polygon;

    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    BoardIterator.AddFilter_LayerSet(MkSet(Layer));
    BoardIterator.AddFilter_Method(eProcessAll);

    Polygon := BoardIterator.FirstPCBObject;
    while (Polygon <> Nil) Do
    Begin
       RML_ExportPolygonOutline (Polygon, RMLOut);
       Polygon := BoardIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(BoardIterator);

    if (DeletePolygonWhenDone <> nil) then
       Board.RemovePCBObject(DeletePolygonWhenDone);
    if (DeletePolyClearanceRuleWhenDone <> nil) then
       Board.RemovePCBObject(DeletePolyClearanceRuleWhenDone);
end;
{..............................................................................}

{..............................................................................}
procedure ExportMillLayerData(Layer : TLayer; Data : TStringList);
begin
   //if export language is RML then
   RML_ExportMillLayerData(Layer, Data);
end;
{..............................................................................}

{..............................................................................}
procedure ExportMillFiles(dummy : Integer);
var
    Data          : TStringList;
    FileName      : TPCBString;
    Document      : IServerDocument;
    Layer         : TLayer;

begin
    WriteReport('ExportMillFiles');

    if (MMExportForm.EnableTopLayer.GetChecked = true) then
    begin
       Layer := eTopLayer;
       Data := TStringList.Create;
       ExportMillLayerData(Layer, Data);
       FileName := ChangeFileExt(Board.FileName,'_' + RouterModel + '_' + cLayerStrings[Layer] + '.mdt');
       Data.SaveToFile(Filename);
       WriteReport(cLayerStrings[Layer] + ' routing file written to ' + FileName);
       Data.Free;

       GetWorkspace.DM_FocusedProject.DM_AddSourceDocument(Filename);
       Document  := Client.OpenDocument('Text', FileName);
       If Document <> Nil Then
           Client.ShowDocument(Document);
       Client.ShowDocument(Client.LastActiveDocumentOfType('PCB'));
    end;

    if (MMExportForm.EnableBottomLayer.GetChecked = true) then
    begin
       Layer := eBottomLayer;
       Data := TStringList.Create;
       ExportMillLayerData(Layer, Data);
       FileName := ChangeFileExt(Board.FileName,'_' + RouterModel + '_' + cLayerStrings[Layer] + '.mdt');
       Data.SaveToFile(Filename);
       WriteReport(cLayerStrings[Layer] + ' routing file written to ' + FileName);
       Data.Free;

       GetWorkspace.DM_FocusedProject.DM_AddSourceDocument(Filename);
       Document  := Client.OpenDocument('Text', FileName);
       If Document <> Nil Then
           Client.ShowDocument(Document);
       Client.ShowDocument(Client.LastActiveDocumentOfType('PCB'));
    end;

    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
end;
{..............................................................................}

{..............................................................................}
procedure ExportDrillFiles(dummy : Integer);
begin
     //If export language is RML then
     RML_ExportDrillFiles(0);
end;
{..............................................................................}

{..............................................................................}
Procedure RunMillExporter;
var
    I       : Integer;
    BoardIterator : IPCB_BoardIterator;

Begin
    // Obtain the Workspace Manager interface
    WSM := GetWorkSpace;
    If WSM = Nil Then Exit;

    // Obtain the Messages Panel interface
    MsgMgr := WSM.DM_MessagesManager;
    If MsgMgr = Nil Then Exit;

    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

     //Required because scripting system can't cope with TCoordPoint types
    RefPoint := TCoordPoint;
    OffsetPoint := TCoordPoint;

    MMExportForm.ShowModal;
    if (RunExporter = true) then
    begin
       // Clear out messages from the Message panel...
       MsgMgr.ClearMessages;
       MsgMgr.BeginUpdate;
       MsgMgr.AddMessage('[Notice]','Exporting design to RML','RML Generator',ExtractFileName(Board.FileName),'','',IMG_GreenSquare, false);
       MsgMgr.EndUpdate;

       // Display the Messages panel in DXP.
       WSM.DM_ShowMessageView;

       ReportLog := TStringList.Create;

       //Set the router model and read in the machine settings
       RouterModel := MMExportForm.MMName.Text;
       TMMSetupForm_ReadIniFileToForm(RouterModel);

       UpdateLocalVariables(0);

       MsgMgr.BeginUpdate;
       MsgMgr.AddMessage('[Notice]','Targeting: ' + RouterModel,'RML Generator','','','',IMG_GreenSquare, false);
       MsgMgr.EndUpdate;

       if (DrillHoles = 'full') then
          ExportDrillFiles(0);

       ExportMillFiles(0);

       WriteReportAndExit('SUCCESS');
   end;
End;

