Const
     c_CirCadDataFile      = 'CIRCAD Version 4.0 -- Data File.';
     c_CirCadEndOfFile     = 'END OF FILE';

     c_SetupSection        = '-g';
     c_LayerTableSection   = '-y';
     c_WorkspaceSetting    = 'x';
     c_GridSetting         = 'd';
     c_BusLayerName        = 'Busses';

     // Object Code
     c_InvalidObjectCode     = -1;
     c_LineObjectCode        = 1;
     c_ArcObjectCode         = 2;
     c_PinObjectCode         = 3;
     c_TextObjectCode        = 4;
     c_ComponentObjectCode   = 6;
     c_FilledSolidObjectCode = 7;

     // Object Type
     c_InvalidType      = 0;
     c_SchLineType      = 1;
     c_SchArcType       = 2;
     c_BusType          = 3;
     c_SchPolygonType   = 4;
     c_SchComponentType = 5;
     c_SchPinType       = 6;
     c_SchLabelType     = 7;
     c_SchWireType      = 8;
     c_SchJunctionType  = 9;
     c_SchComponentPinType = 10;
     c_SchDotType          = 11;

     c_ObjectCodeIndex     = 0;
     c_LayerNumIndex       = 1;

     // Line Segment format:
     // Code Layer Width X1 Y1 X2 Y2 Signal
     c_LineWidthIndex      = 2;
     c_LineOneEndXIndex    = 3;
     c_LineOneEndYIndex    = 4;
     c_LineOtherEndXIndex  = 5;
     c_LineOtherEndYIndex  = 6;
     c_LineSignalIndex     = 7;

     // Arc format
     // Code Layer Width X Y Radius Start Sweep Ellipse Angle Signal
     c_ArcWidthIndex           = 2;
     c_ArcLocationXIndex       = 3;
     c_ArcLocationYIndex       = 4;
     c_ArcRadiusIndex          = 5;
     c_ArcStartAngleIndex      = 6;
     c_ArcSweepAngleIndex      = 7;
     c_ArcEllipticalRatioIndex = 8;
     c_ArcRotationIndex        = 9;
     c_ArcNetNameIndex         = 10;

     // Text format:
     // Code Layer Height X Y Length Angle Anchor Font Data
     c_TextHeightIndex     = 2;
     c_TextLocationXIndex  = 3;
     c_TextLocationYIndex  = 4;
     c_TextLengthIndex     = 5;
     c_TextRotationIndex   = 6;
     c_TextAnchorIndex     = 7;
     c_TextFontNameIdIndex = 8;
     c_TextDataIndex       = 9;

     // Polygon point format:
     // Code Layer Width X Y
     c_PolygonPointXIndex = 3;
     c_PolygonPointYIndex = 4;

     // Pin/Pad/Via format
     // Code Layer Type X Y Width Height Hole Angle Designator Signal
     c_PinTypeIndex       = 2;
     c_PinLocationXIndex  = 3;
     c_PinLocationYIndex  = 4;
     c_PinWidthIndex      = 5;
     c_PinHeightIndex     = 6;
     c_PinHoleIndex       = 7;
     c_PinAngleIndex      = 8;
     c_PinDesignatorIndex = 9;
     c_PinSignalIndex     = 10;

     // Pin Type definition
     c_SchematicPin   = 0;
     c_SchematicDot   = 7;

     // Component header format:
     // Code Flag Rotation X1 Y1 X2 Y2 Parameters
     c_ComponentCodeIndex        = 0;
     c_ComponentFlagIndex        = 1;
     c_ComponentRotationIndex    = 2;
     c_ComponentLLXIndex         = 3;
     c_ComponentLLYIndex         = 4;
     c_ComponentURXIndex         = 5;
     c_ComponentURYIndex         = 6;
     c_ComponentParameterIndex   = 7;

     // Component Parameter format:
     // Name<delimiter>Refdes<delimiter>Value<delimiter>ModelName
     c_ComponentNameIndex       = 0;
     c_ComponentRefdesIndex     = 1;
     c_ComponentValueIndex      = 2;
     c_ComponentModifierIndex   = 3;
     c_ComponentModelIndex      = 4;

     // Component End Line format:
     // Code ComponentObjectCode Flag X Y
     c_ComponentLocationXIndex = 3;
     c_ComponentLocationYIndex = 4;

     // Delimiter
     c_ComponentParamDelimiter  = 127;
     c_FieldDelimiter           = ' ';
     c_SpaceDelimiter           = 32;

     // Layer format:
     // color,plot,type,number,name
     c_LayerNumberIndex = 3;
     c_LayerNameIndex   = 4;

Var
    g_BusLayerNum     : Integer;
    g_WorkspaceWidth  : TCoord;
    g_WorkspaceHeight : TCoord;

///////////////////////////////////////////////////////////////////////////////
Function GetObjectCode(Str : TDynamicString) : Integer;
Var
    i : Integer;
Begin
    Result := c_InvalidObjectCode;
    If (Length(Str) > 0) And
       (Str[1] >= Chr('0')) And
       (Str[1] <= Chr('9')) Then
    Begin
        Result := StrToInt(Str[1]);
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function ReadPolygonPoints(StrList   : TStringList) : Boolean;
Var
   tokens : TStringList;
   code   : Integer;
   layer  : Integer;
   width  : TCoord;
   str    : TDynamicString;
Begin
     Result := True;
     tokens := TStringList.Create;
     While GetNextLine(str) Do
     Begin
          If str = c_CirCadEndOfFile Then
              Break;
          TokenizeString(str, c_SpaceDelimiter, tokens, -1);
          If tokens.Count > (c_PolygonPointYIndex + 1) Then
              Break;
          code := StrToInt(tokens[c_ObjectCodeIndex]);
          If code <> c_LineObjectCode Then
              Break;
          If tokens.Count <= c_LineWidthIndex Then
              Break;
          StrToCoord(tokens[c_LineWidthIndex], width);
          If width <> 0 Then
              Break;
          StrList.Add(str);
     End;
     g_LookAhead := str;
     tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function ParseLine(Str  : TDynamicString;
                   Data : TStringList ) : Integer;
Var
    object_type : Integer;
    tokens      : TStringList;
    signal      : Boolean;
    layer_num   : Integer;
    width       : TCoord;
Begin
    object_type := c_InvalidType;

    tokens := TStringList.Create;
    TokenizeString(Str, c_SpaceDelimiter, tokens, -1);
    If tokens.Count > c_LineOtherEndYIndex Then
    Begin
        layer_num := StrToInt(tokens[c_LayerNumIndex]);
        StrToCoord(tokens[c_LineWidthIndex], width);
    End;

    signal := False;
    If tokens.Count > c_LineSignalIndex Then
        signal := True;

    If (layer_num = g_BusLayerNum) Then
        object_type := c_BusType
    Else If (signal) Then
        object_type := c_SchWireType
    Else If (width = 0) Then
    Begin
        ReadPolygonPoints(Data);
        object_type := c_SchPolygonType;
    End
    Else
        object_type := c_SchLineType;

    tokens.Free;
    Result := object_type;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure InitSettingTable(SettingTable : TStringList);
Var
   str_list      : TStringList;
Begin
     str_list := TStringList.Create;
     SettingTable.AddObject(c_SetupSection, str_list);
     str_list := TStringList.Create;
     SettingTable.AddObject(c_LayerTableSection, str_list);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function ValidateFile(const InputFile : String) : Boolean;
Var
   in_file : Text;
   str     : String;
Begin
     Result := False;
     AssignFile(in_file, InputFile);
     Reset(in_file);
     ReadLn(in_file, str);
     If (str <> Null) Then
     Begin
         str := Trim(str);
         If (str = c_CirCadDataFile) Then
         Begin
              Result := True;
         End;
     End;
     CloseFile(in_file);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function CreateOutputFileName(const InputFile : TDynamicString) : TDynamicString;
Var
   new_extension : TDynamicString;
   filename      : TDynamicString;
   prefix        : TDynamicString;
   file_ext      : TDynamicString;
   i             : Integer;
Begin
     Result := '';
     If Client = Nil Then
        Exit;
     new_extension := Client.GetDefaultExtensionForDocumentKind(cDocKind_Sch);
     If Length(new_extension) > 0 Then
     Begin
          filename := ExtractFileName(InputFile);
          file_ext := ExtractFileExt(InputFile);

          i := Pos('.', filename);
          If i > 0 Then
             filename := Copy(filename, 1, i - 1);

          // Scan the string...looking for the first digit
          prefix := '';
          For i := 1 To Length(file_ext) Do
          Begin
              If (file_ext[i] >= Chr('0')) And (file_ext[i] <= Chr('9')) Then
              Begin
                  prefix := Copy(file_ext, i, Length(file_ext) - i + 1);
                  filename := filename + '_' + prefix;
                  Break;
              End;
          End;
          Result := filename + '.' + new_extension;
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure SaveDocument(      Document   : IServerDocument;
                       const SaveFormat : TDynamicString);
Begin
     If Client = Nil Then
        Exit;
     Document.Modified := True;
     Client.ShowDocument(Document);
     Document.Focus;

     ResetParameters;
     AddStringParameter('SaveMode', 'Standard');
     AddStringParameter('ObjectKind', 'Document');
     AddStringParameter('FileFormat', SaveFormat);
     RunProcess('WorkspaceManager:SaveObject');
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function CreateDocument (Const FilePath   : TDynamicString;
                         Const DocKind    : TDynamicString;
                         Const SaveFormat : TDynamicString) : IServerDocument;
Var
   document : IServerDocument;
   doc_name : TDynamicString;
   out_file : File;
Begin
     Result := Nil;
     If Client = Nil Then
        Exit;
     document := Client.GetDocumentByPath(FilePath);
     If document <> Nil Then
        Client.CloseDocument(document);
     doc_name := ExtractFileName(FilePath);
     document := Client.OpenNewDocument(DocKind, doc_name, doc_name, False);
     If document = Nil Then
        Exit;
     document.SetFileName(FilePath);

     // Create file
     AssignFile(out_file, FilePath);
     ReWrite(out_file);
     CloseFile(out_file);

     SaveDocument(document, SaveFormat);

     Result := document;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function SetupGridSpacing(const Items    : TStringList;
                                SchDoc   : ISch_Document) : Boolean;
Var
   grid_spacing : TCoord;
Begin
     Result := False;
     If (Items.Count > 0) And
        StrToCoord(Items[0], grid_spacing) Then
     Begin
          SchDoc.VisibleGridSize := grid_spacing;
     End;
End;

///////////////////////////////////////////////////////////////////////////////
Function GetWorkspaceSize(const Items    : TStringList;
                          var   Width    : TCoord;
                          var   Height   : TCoord) : Boolean;
Var
   x1 : TCoord;
   y1 : TCoord;
   x2 : TCoord;
   y2 : TCoord;
Begin
     Result := False;
     If (Items.Count = 4) And
        StrToCoord(Items[0], x1) And
        StrToCoord(Items[1], y1) And
        StrToCoord(Items[2], x2) And
        StrToCoord(Items[3], y2) Then
     Begin
          Width  := x1 + x2;
          Height := y1 + y2;
          If (Width > 0) And (Height > 0) Then
          Begin
               Result := True;
          End;
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function SetupSchDocument(      SchDoc       : ISch_Document;
                          const SettingTable : TStringList) : Boolean;
Var
   str    : TDynamicString;
   items  : TStringList;
   width  : TCoord;
   height : TCoord;
Begin
     Result := False;

     g_WorkspaceWidth  := SchDoc.SheetSizeX;
     g_WorkspaceHeight := SchDoc.SheetSizeY;

     items := TStringList.Create;
     If LookupSetting(SettingTable, c_SetupSection, c_WorkspaceSetting, str) Then
     Begin
          items.Clear;
          Tokenize(str, ',', items);
          If GetWorkSpaceSize(items, width, height) Then
          Begin
              g_WorkspaceWidth  := width;
              g_WorkspaceHeight := height;
          End;
     End;

     SchDoc.BorderOn         := False;
     SchDoc.ReferenceZonesOn := False;
     SchDoc.TitleBlockOn     := False;
     SchDoc.UseCustomSheet   := True;
     SchDoc.CustomX          := g_WorkspaceWidth;
     SchDoc.CustomY          := g_WorkspaceHeight;
     SchDoc.UpdateDocumentProperties;

     If LookupSetting(SettingTable, c_SetupSection, c_GridSetting, str) Then
     Begin
          items.Clear;
          Tokenize(str, ',', items);
          SetupGridSpacing(items, SchDoc);
     End;
     items.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetBusLayerNum(const SettingTable) : Integer;
Var
    index   : Integer;
    i       : Integer;
    tokens  : TStringList;
    setting : TStringList;
    str     : TDynamicString;
Begin
    Result := -1;
    If SettingTable.Find(c_LayerTableSection, index) Then
    Begin
        tokens := TStringList.Create;

        setting := SettingTable.Objects[index];
        For i := 0 To (setting.Count - 1) Do
        Begin
            str := setting[i];
            tokens.Clear;
            Tokenize(str, ':', tokens);
            If (tokens.Count > 1) Then
            Begin
                str := tokens[1];
                tokens.Clear;
                Tokenize(str, ',', tokens);
                If (tokens.Count > c_LayerNameIndex) Then
                Begin
                    If (tokens[c_LayerNameIndex] = c_BusLayerName) Then
                    Begin
                        Result := StrToInt(tokens[c_LayerNumberIndex]);
                        Break;
                    End;
                End;
            End;
        End;
        tokens.Free;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetPenWidth(const Width : TCoord) : TSize;
Begin
    If (Width <= MilsToCoord(10)) Then
        Result := eSmall
    Else If (Width >= MilsToCoord(20)) And (Width <= MilsToCoord(30)) Then
        Result := eMedium
    Else If (Width >= MilsToCoord(30)) Then
        Result := eLarge
    Else
        Result := eSmall;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetOrientation(Angle : Double) : TRotationBy90;
Begin
    If      Angle = 90.0  Then Result := eRotate90
    Else If Angle = 180.0 Then Result := eRotate180
    Else If Angle = 270.0 Then Result := eRotate270
    Else                       Result := eRotate0;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetTextJustification(Anchor : Integer) : TTextJustification;
Var
    just : TTextJustification;
Begin
    Case Anchor Of
        0  : just := eJustify_BottomLeft;
        1  : just := eJustify_BottomCenter;
        2  : just := eJustify_BottomRight;
        3  : just := eJustify_BottomLeft;  // For now, bottom offset -> bottom left
        4  : just := eJustify_CenterLeft;
        5  : just := eJustify_Center;
        6  : just := eJustify_CenterRight;
        7  : just := eJustify_CenterLeft;  // For now, center offset -> center left
        8  : just := eJustify_TopLeft;
        9  : just := eJustify_TopCenter;
        10 : just := eJustify_TopRight;
        11 : just := eJustify_TopLeft;      // For now, top offset -> top left
        12 : just := eJustify_BottomLeft;   // For now, base left  -> bottom left
        13 : just := eJustify_BottomCenter; // For now, base center -> bottom center
        14 : just := eJustify_BottomRight;  // For now, base right  -> bottom right
        15 : just := eJustify_BottomLeft;   // For now, base offset -> bottom left
        Else just := eJustify_BottomLeft;
    End;
    Result := just;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetFontId(Height : Double;
                   NameId : Integer) : TFontID;
Var
    font_size : Integer;
    font_name : TDynamicString;
Begin
    font_size := Round(160 * Height);
    font_name := 'Times New Roman';
    Result    := SchServer.FontManager.GetFontID(font_size,
                                                 0,         // rotation
                                                 False,     // underline
                                                 False,     // italic
                                                 False,     // Bold
                                                 False,     // StrikeOut
                                                 font_name);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure ReadUntilPrimaryDataSection(    InFile    : Text;
                                      var LookAhead : TDynamicString);
Var
    str : TDynamicString;
Begin
    LookAhead := '';
    While (Not Eof(InFile)) Do
    Begin
        ReadLn(InFile, str);
        If (Not (str = Null)) And (Length(str) > 0) Then
        Begin
            If (str[1] >= Chr('0')) And (str[1] <= Chr('9')) Then
            Begin
                LookAhead := str;
                Break;
            End;
        End;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateLine(const Data : TStringList;
                       var   Obj  : ISch_GraphicalObject) : Boolean;
Var
   tokens : TStringList;
   width  : TCoord;
   x1     : TCoord;
   x2     : TCoord;
   y1     : TCoord;
   y2     : TCoord;
   line   : ISch_Line;
Begin
    Result := False;
    Obj    := Nil;

    If Data.Count = 0 Then
        Exit;

    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If (tokens.Count >= c_LineOtherEndYIndex) Then
    Begin
         StrToCoord(tokens[c_LineWidthIndex], width);
         StrToCoord(tokens[c_LineOneEndXIndex], x1);
         StrToCoord(tokens[c_LineOneEndYIndex], y1);
         StrToCoord(tokens[c_LineOtherEndXIndex], x2);
         StrToCoord(tokens[c_LineOtherEndYIndex], y2);

         //line := SchServer.SchObjectFactory(eLine, eCreate_Default);
         line := SchServer.SchObjectFactory(eLine, eCreate_GlobalCopy);
         If line <> Nil Then
         Begin
              line.Corner    := Point(x1, y1);
              line.Location  := Point(x2, y2);
              line.LineWidth := GetPenWidth(width);

              Obj    := Line;
              Result := True;
         End;

    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateWire(const Data : TStringList;
                       var   Obj  : ISch_GraphicalObject)
                       : Boolean;
Var
    wire        : ISch_Wire;
    tokens      : TStringList;
    width       : TCoord;
    x1          : TCoord;
    x2          : TCoord;
    y1          : TCoord;
    y2          : TCoord;
    signal_name : TStringList;
Begin
    Result := False;
    Obj    := Nil;

    If Data.Count = 0 Then
        Exit;

    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If (tokens.Count > c_LineSignalIndex) Then
    Begin
         StrToCoord(tokens[c_LineWidthIndex], width);
         StrToCoord(tokens[c_LineOneEndXIndex], x1);
         StrToCoord(tokens[c_LineOneEndYIndex], y1);
         StrToCoord(tokens[c_LineOtherEndXIndex], x2);
         StrToCoord(tokens[c_LineOtherEndYIndex], y2);
         signal_name := tokens[c_LineSignalIndex];

         wire := SchServer.SchObjectFactory(eWire, eCreate_Default);
         If wire <> Nil Then
         Begin
              wire.VerticesCount := 2;
              wire.Vertex[1] := Point(x1, y1);
              wire.Vertex[2] := Point(x2, y2);
              wire.LineWidth := GetPenWidth(width);

              Obj    := wire;
              Result := True;
         End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateBus(const Data : TStringList;
                      var   Obj  : ISch_GraphicalObject) : Boolean;
Var
   tokens : TStringList;
   width  : TCoord;
   x1     : TCoord;
   x2     : TCoord;
   y1     : TCoord;
   y2     : TCoord;
   bus    : ISch_Bus;
Begin
    Result := False;
    Obj    := Nil;

    If Data.Count = 0 Then
        Exit;

    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If (tokens.Count >= c_LineOtherEndYIndex) Then
    Begin
         StrToCoord(tokens[c_LineWidthIndex], width);
         StrToCoord(tokens[c_LineOneEndXIndex], x1);
         StrToCoord(tokens[c_LineOneEndYIndex], y1);
         StrToCoord(tokens[c_LineOtherEndXIndex], x2);
         StrToCoord(tokens[c_LineOtherEndYIndex], y2);

         bus := SchServer.SchObjectFactory(eBus, eCreate_Default);
         If bus <> Nil Then
         Begin
              bus.VerticesCount := 2;
              bus.Vertex[1]     := Point(x1, y1);
              bus.Vertex[2]     := Point(x2, y2);
              bus.LineWidth     := GetPenWidth(width);

              Obj    := bus;
              Result := True;
         End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateText(const Data : StringList;
                       var   Obj  : ISch_GraphicalObject) : Boolean;
Var
    height       : Double;
    x            : TCoord;
    y            : TCoord;
    len          : Double;
    angle        : Double;
    anchor       : Integer;
    font_name_id : Integer;
    value        : TDynamicString;
    tokens       : TStringList;
    sch_label    : ISch_label;
Begin
    Result := False;
    Obj    := Nil;

    If Data.Count = 0 Then
        Exit;

    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, c_TextDataIndex);
    If (tokens.Count >= c_TextDataIndex) Then
    Begin
        height := StrToFloat(tokens[c_TextHeightIndex]);
        StrToCoord(tokens[c_TextLocationXIndex], x);
        StrToCoord(tokens[c_TextLocationYIndex], y);
        len          := StrToFloat(tokens[c_TextLengthIndex]);
        angle        := StrToFloat(tokens[c_TextRotationIndex]);
        anchor       := StrToInt(tokens[c_TextAnchorIndex]);
        font_name_id := StrToInt(tokens[c_TextFontNameIdIndex]);
        value        := tokens[c_TextDataIndex];

        sch_label := SchServer.SchObjectFactory(eLabel, eCreate_GlobalCopy);
        If sch_label <> Nil Then
        Begin
            sch_label.Location      := Point(x, y);
            sch_label.Text          := value;
            sch_label.Justification := GetTextJustification(anchor);
            sch_label.FontId        := GetFontId(height, font_name_id);
            sch_label.Orientation   := GetOrientation(angle);

            Obj    := sch_label;
            Result := True;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateArc(const Data : TStringList;
                      var   Arc  : ISch_GraphicalObject) : Boolean;
Var
   width         : TCoord;
   x             : TCoord;
   y             : TCoord;
   radius        : TCoord;
   start_angle   : Double;
   sweep_angle   : Double;
   ellipse_ratio : Double;
   rotation      : Double;
   tokens        : TStringList;
Begin
    Result := False;
    Arc    := Nil;

    If Data.Count = 0 Then
        Exit;

    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If (tokens.Count >= c_ArcRotationIndex) Then
    Begin
        StrToCoord(tokens[c_ArcWidthIndex], width);
        StrToCoord(tokens[c_ArcLocationXIndex], x);
        StrToCoord(tokens[c_ArcLocationYIndex], y);
        StrToCoord(tokens[c_ArcRadiusIndex], radius);
        start_angle    := StrToFloat(tokens[c_ArcStartAngleIndex]);
        sweep_angle    := StrToFloat(tokens[c_ArcSweepAngleIndex]);
        ellipse_ratio  := StrToFloat(tokens[c_ArcEllipticalRatioIndex]);
        rotation       := StrToFloat(tokens[c_ArcRotationIndex]);

        If (ellipse_ratio = 0) Then
            Arc := SchServer.SchObjectFactory(eArc, eCreate_Default)
        Else
            Arc := SchServer.SchObjectFactory(eEllipticalArc, eCreate_Default);

        If Arc <> Nil Then
        Begin
            Arc.Location   := Point(x, y);
            Arc.Radius     := radius;
            Arc.StartAngle := start_angle;
            Arc.EndAngle   := (start_angle + sweep_angle);
            Arc.LineWidth  := GetPenWidth(width);

            If ellipse_ratio > 0 Then
                Arc.SecondaryRadius := radius * ellipse_ratio;
            Arc.RotateBy90(Arc.Location, GetOrientation(rotation));

            Result := True;
        End;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSchPolygon(const Data : TStringList;
                             var   Obj  : ISch_GraphicalObject) : Boolean;
Var
    x         : TCoord;
    y         : TCoord;
    i         : Integer;
    point_num : Integer;
    tokens    : TStringList;
    polygon   : ISch_Polygon;
Begin
    Result  := False;
    Obj     := Nil;

    point_num := Data.Count - 1;  // exclude the header line
    If (point_num < 3 ) Then
        Exit;  // At least 3 points

    polygon := SchServer.SchObjectFactory(ePolygon, eCreate_Default);
    If (polygon <> Nil) Then
    Begin
        polygon.VerticesCount := point_num;
        polygon.LineWidth     := eZeroSize;
        polygon.IsSolid       := True;

        tokens := TStringList.Create;
        For i := 1 To point_num Do
        Begin
           // Code Layer Width X Y
           TokenizeString(Data[i], c_SpaceDelimiter, tokens, -1);
           If tokens.Count > c_PolygonPointYIndex Then
           Begin
               StrToCoord(tokens[c_PolygonPointXIndex], x);
               StrToCoord(tokens[c_PolygonPointYIndex], y);

               polygon.Vertex[i] := Point(x, y);
           End;
        End;
        tokens.Free;

        Obj    := polygon;
        Result := True;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetPinEndPoint(const PinLocation   : TLocation;
                        const ComponentData : TStringList;
                        var   OtherEnd      : TLocation)
                        : Boolean;
Var
    tokens : TStringList;
    i      : Integer;
    object_code : Integer;
    x1 : TCoord;
    x2 : TCoord;
    y1 : TCoord;
    y2 : TCoord;
Begin
    Result := False;
    tokens := TStringList.Create;
    For i := 0 To (ComponentData.Count - 1) Do
    Begin
        TokenizeString(ComponentData[i], c_SpaceDelimiter, tokens, -1);
        If tokens.Count > c_ObjectCodeIndex Then
        Begin
            object_code := StrToInt(tokens[c_ObjectCodeIndex]);
            If object_code = c_LineObjectCode Then
            Begin
                If tokens.Count > c_LineOtherEndYIndex Then
                Begin
                    StrToCoord(tokens[c_LineOneEndXIndex], x1);
                    StrToCoord(tokens[c_LineOneEndYIndex], y1);
                    StrToCoord(tokens[c_LineOtherEndXIndex], x2);
                    StrToCoord(tokens[c_LineOtherEndYIndex], y2);

                    If (x1 = PinLocation.x) And (y1 = PinLocation.y) Then
                    Begin
                        OtherEnd := Point(x2, y2);
                        Result   := True;
                        Break;
                    End;

                    If (x2 = PinLocation.x) And (y2 = PinLocation.y) Then
                    Begin
                        OtherEnd := Point(x1, y1);
                        Result   := True;
                        Break;
                    End;
                End;
            End;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetLineOrientation(const Pt1 : TLocation;
                            const Pt2 : TLocation;
                            var   Orientation : TRotationBy90;
                            var   Len         : TCoord)
                            : Boolean;
Var
    dx : TCoord;
    dy : TCoord;
Begin
    Result := True;

    dx := Pt2.x - Pt1.x;
    dy := Pt2.y - Pt1.y;

    If ((dx = 0)  and  (dy = 0)) Or
       ((dx <> 0) and  (dy <> 0)) Then
    Begin
        Result := False;
        Exit;
    End;

    If dx = 0 Then
    Begin
        Len := Abs(dy);
        If dy > 0 Then
            Orientation := eRotate90
        Else
            Orientation := eRotate270;
    End
    Else If dy = 0 Then
    Begin
        Len := Abs(dx);
        If dx > 0 Then
            Orientation := eRotate0
        Else
            Orientation := eRotate180;
    End
    Else
    Begin
        Result := False;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSchComponentPin(const Data          : TStringList;
                                  const ComponentData : TStringList;
                                  var   Obj           : ISch_GraphicalObject;
                                  var   PinDes        : TDynamicString)  : Boolean;
Var
    x      : TCoord;
    y      : TCoord;
    width  : TCoord;
    height : TCoord;
    tokens : TStringList;
    pt     : TLocation;
    loc    : TLocation;
    ori    : TRotationBy90;
    len    : TCoord;
    pin    : ISch_Pin;
Begin
    Result := False;
    If Data.Count = 0 Then
        Exit;
    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If tokens.Count > c_PinDesignatorIndex Then
    Begin
        StrToCoord(tokens[c_PinLocationXIndex], x);
        StrToCoord(tokens[c_PinLocationYIndex], y);
        StrToCoord(tokens[c_PinWidthIndex], width);
        StrToCoord(tokens[c_PinHeightIndex], height);

        pin := SchServer.SchObjectFactory(ePin, eCreate_Default);
        If (pin <> Nil) Then
        Begin
            PinDes := tokens[c_PinDesignatorIndex];
            pin.Designator := tokens[c_PinDesignatorIndex];
            pin.Name       := tokens[c_PinDesignatorIndex];  // For now - Use the pin desigator
            pin.ShowName   := False;
            pin.ShowDesignator := True;
            pin.Width      := width;

            ori      := eRotate0;
            len      := 0;
            loc      := Point(x,y);

            If GetPinEndPoint(loc, ComponentData, pt) Then
            Begin
                If GetLineOrientation(pt, loc, ori, len) Then
                Begin
                    loc := pt;
                End;
            End;

            pin.Location    := loc;
            pin.PinLength   := len;
            pin.Orientation := ori;

            pin.IsHidden    := False;
            pin.SwapId_Pin  := '0';
            pin.SwapId_Part := '0';
            pin.Electrical  := eElectricPassive;

            Obj    := pin;
            Result := True;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSchComponentLabel(const Data : TStringList;
                                    var   Obj  : ISch_GraphicalObject) : Boolean;
Var
    tokens    : TStringList;
    text_data : TDynamicString;
Begin
    Result := False;
    Obj    := Nil;
    If (Data.Count = 0) Then
        Exit;
    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If tokens.Count > c_TextDataIndex Then
    Begin
        text_data := tokens[c_TextDataIndex];
        If text_data[1] <> '&' Then
        Begin
            Result := TranslateText(Data, Obj);
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSchematicDot(const Data : TStringList;
                               var   Obj  : ISch_GraphicalObject) : Boolean;
Var
    x      : TCoord;
    y      : TCoord;
    width  : TCoord;
    height : TCoord;
    tokens : TStringList;
    ellipse : ISch_Ellipse;
Begin
    Result := False;
    If Data.Count = 0 Then
        Exit;
    tokens := TStringList.Create;
    Tokenize(Data[0], c_FieldDelimiter, tokens);
    If tokens.Count > c_PinDesignatorIndex Then
    Begin
        StrToCoord(tokens[c_PinLocationXIndex], x);
        StrToCoord(tokens[c_PinLocationYIndex], y);
        StrToCoord(tokens[c_PinWidthIndex], width);
        StrToCoord(tokens[c_PinHeightIndex], height);

        ellipse := SchServer.SchObjectFactory(eEllipse, eCreate_Default);
        If (ellipse <> Nil) Then
        Begin
            ellipse.Location        := Point(x, y);
            ellipse.Radius          := Round(width / 2);
            ellipse.SecondaryRadius := Round(height / 2);
            ellipse.IsSolid         := True;
            ellipse.LineWidth       := eZeroSize;

            Obj    := ellipse;
            Result := True;
        End;
    End;
End;

///////////////////////////////////////////////////////////////////////////////
Function TranslatePin(const Data : TStringList;
                      var   Pin  : TSch_Pin) : Boolean;
Var
    x      : TCoord;
    y      : TCoord;
    width  : TCoord;
    height : TCoord;
    tokens : TStringList;
Begin
    Result := False;
    If Data.Count = 0 Then
        Exit;
    tokens := TStringList.Create;
    Tokenize(Data[0], c_FieldDelimiter, tokens);
    If tokens.Count > c_PinDesignatorIndex Then
    Begin
        StrToCoord(tokens[c_PinLocationXIndex], x);
        StrToCoord(tokens[c_PinLocationYIndex], y);
        StrToCoord(tokens[c_PinWidthIndex], width);
        StrToCoord(tokens[c_PinHeightIndex], height);

        Pin := SchServer.SchObjectFactory(ePin, eCreate_GlobalCopy);
        If (Pin <> Nil) Then
        Begin
            Pin.Designator := tokens[c_PinDesignatorIndex];
            Pin.Name       := tokens[c_PinDesignatorIndex];  // For now - Use the pin desigator
            Pin.ShowName   := False;
            Pin.ShowDesignator := True;
            Pin.Width      := width;
            Pin.Location   := Point(x, y);
            Pin.PinLength  := 0;  // MilsToCoord(10);
            Pin.IsHidden   := False;
            Pin.SwapId_Pin  := '0';
            Pin.SwapId_Part := '0';
            Pin.Electrical  := eElectricPassive;

            Result         := True;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateJunction(const Data : TStringList;
                           var   Obj  : ISch_GraphicalObject) : Boolean;
Var
    x        : TCoord;
    y        : TCoord;
    width    : TCoord;
    height   : TCoord;
    tokens   : TStringList;
    junction : ISch_Junction;
Begin
    Result := True;
    Obj    := Nil;
    Exit;

    If Data.Count = 0 Then
        Exit;
    tokens := TStringList.Create;
    Tokenize(Data[0], c_FieldDelimiter, tokens);
    If tokens.Count > c_PinDesignatorIndex Then
    Begin
        StrToCoord(tokens[c_PinLocationXIndex], x);
        StrToCoord(tokens[c_PinLocationYIndex], y);
        StrToCoord(tokens[c_PinWidthIndex], width);
        StrToCoord(tokens[c_PinHeightIndex], height);

        junction := SchServer.SchObjectFactory(eJunction, eCreate_Default);
        If (junction <> Nil) Then
        Begin
            junction.Location := Point(x, y);
            //junction.Size     := GetPenWidth(width);
            junction.Size     := eSmall;

            Obj    := junction;
            Result := True;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetComponentParameter(const Data : TStringList) : TDynamicString;
Var
    tokens : TStringList;
Begin
    Result := '';
    If Data.Count = 0 Then
        Exit;
    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);    // the first line is the component header
    If tokens.Count > c_ComponentParameterIndex Then
    Begin
        Result := tokens[c_ComponentParameterIndex];
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
Function ReadComponent(StrList : TStringList) : Boolean;
Var
    code  : Integer;
    str   : TDynamicString;
Begin
     Result := False;
     While GetNextLine(str) Do
     Begin
          code := GetObjectCode(str);
          If code <> c_InvalidObjectCode Then
          Begin
              StrList.Add(str);
              If (code = 0) Then
              Begin
                 Result := True;
                 Break;
              End;
          End;
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function DetectPinObject(Str : TDynamicString) : Integer;
Var
    pin_type    : Integer;
    object_type : Integer;
    tokens      : TStringList;
Begin
    tokens := TStringList.Create;
    object_type := c_InvalidType;
    TokenizeString(Str, c_SpaceDelimiter, tokens, -1);
    If tokens.Count > c_PinTypeIndex Then
    Begin
        pin_type := StrToInt(tokens[c_PinTypeIndex]);
        Case pin_type Of
             c_SchematicPin :
                object_type := c_SchPinType;
             c_SchematicDot :
                object_type := c_SchDotType;
        End;
    End;
    Result := object_type;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetNextObjectData(var ObjectType : Integer;
                               Data       : TStringList) : Boolean;
Var
    width  : TCoord;
    str    : TDynamicString;
    tokens : TStringList;
    code   : Integer;
    layer_num : Integer;
Begin
    Result     := False;
    ObjectType := c_InvalidType;

    While (ObjectType = c_InvalidType) And GetNextLine(str) Do
    Begin
        Data.Clear;
        If (str = c_CirCadEndOfFile) Then
            Break;
        code := GetObjectCode(str);
        Data.Add(str);
        Case code Of
            c_LineObjectCode :
                ObjectType := ParseLine(str, Data);
            c_ArcObjectCode :
                ObjectType := c_SchArcType;
            c_TextObjectCode :
                ObjectType := c_SchLabelType;
            c_PinObjectCode :
                ObjectType := DetectPinObject(str);
            c_ComponentObjectCode :
            Begin
                If ReadComponent(Data) Then
                Begin
                    ObjectType := c_SchComponentType;
                End;
            End;
        End;
    End;

    If ObjectType <> c_InvalidType Then
        Result := True;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetComponentProperty(const ParameterStr : TDynamicString;
                                    PropType     : Integer)  : TDynamicString;
Var
    tokens : TStringList;
Begin
    Result := '';
    tokens := TStringList.Create;
    TokenizeString(ParameterStr, c_ComponentParamDelimiter, tokens, -1);
    If tokens.Count > PropType Then
        Result := tokens[PropType];
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetComponentLocation(const Data     : TStringList;
                              var   Location : TLocation) : Boolean;
Var
    tokens : TStringList;
    x      : TCoord;
    y      : TCoord;
Begin
    Result := False;
    If Data.Count = 0 Then
        Exit;
    tokens := TStringList.Create;
    Tokenize(Data[Data.Count - 1], c_FieldDelimiter, tokens);
    If tokens.Count > c_ComponentLocationYIndex Then
    Begin
        StrToCoord(tokens[c_ComponentLocationXIndex], x);
        StrToCoord(tokens[c_ComponentLocationYIndex], y);
        Location := Point(x, y);
        Result := True;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetPowerSignalName(const ComponentData : TStringList) : TDynamicString;
Var
    tokens      : TStringList;
    object_code : Integer;
    i           : Integer;
Begin
    Result := '';
    tokens := TStringList.Create;
    For i := 0 To (ComponentData.Count - 1) Do
    Begin
        TokenizeString(ComponentData[i], c_SpaceDelimiter, tokens, -1);
        If tokens.Count > c_ObjectCodeIndex Then
        Begin
            object_code := StrToInt(tokens[c_ObjectCodeIndex]);
            If object_code = c_PinObjectCode Then
            Begin
                If tokens.Count > c_PinSignalIndex Then
                Begin
                    Result := tokens[c_PinSignalIndex];
                    Break;
                End;
            End;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslatePowerComponent(const Data     : TStringList;
                                       Name     : TDynamicString;
                                       Angle    : Double;
                                       Location : TLocation;
                                 var   Obj      : ISch_GraphicalObject) : Boolean;
Var
    power_obj : ISch_PowerObject;
    rotation  : TRotationBy90;
    net_name  : TDynamicString;
Begin
    Result := False;
    Obj    := Nil;

    power_obj := SchServer.SchObjectFactory(ePowerObject, eCreate_Default);
    If (power_obj <> Nil) Then
    Begin
        power_obj.Location    := Location;
        power_obj.ShowNetName := True;   // To do....read for the setup parameter for display power name
        net_name              := GetPowerSignalName(Data);
        power_obj.Text        := net_name;

        If      (net_name = 'GND')   Then   power_obj.Style := ePowerGndPower
        Else If (net_name = 'EARTH') Then   power_obj.Style := ePowerGndEarth
        Else If (name     = '$$PWR') Then   power_obj.Style := ePowerBar
        Else                                power_obj.Style := ePowerGndSignal;

        If   (power_obj.Style = ePowerBar) Then  angle := angle + 90
        Else                                     angle := angle + 270.0;

        If angle >= 360 Then
           angle := angle - 360;
        rotation := GetOrientation(Angle);

        power_obj.RotateBy90(Location, rotation);

        Obj    := power_obj;
        Result := True;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSignalComponent(const Data : TStringList;
                                        Name : TDynamicString;
                                        Location : TLocation;
                                  var   Obj  : ISch_GraphicalObject)  : Boolean;
Var
    llx       : TCoord;
    lly       : TCoord;
    urx       : TCoord;
    ury       : TCoord;
    direction : Integer;   // 0 = right, 1 = left, 2 = top, 3 = bottom
    len       : TCoord;
    width     : TCoord;
    height    : TCoord;
    tokens    : TStringList;
    net_name  : TDynamicString;
    port      : ISch_Port;
Begin
    Result := False;
    Obj    := Nil;
    If (Data.Count = 0) Then
        Exit;
    tokens := TStringList.Create;
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, -1);
    If (tokens.Count > c_ComponentParameterIndex) Then
    Begin
        StrToCoord(tokens[c_ComponentLLXIndex], llx);
        StrToCoord(tokens[c_ComponentLLYIndex], lly);
        StrToCoord(tokens[c_ComponentURXIndex], urx);
        StrToCoord(tokens[c_ComponentURYIndex], ury);
        net_name  := GetPowerSignalName(Data);

        width  := Abs(urx - llx);
        height := Abs(ury - lly);

        If width > height then
        Begin
            // Horizontal orientation
            len := width;
            If  (location.X > ((llx + urx)/2)) Then  direction := 0  // right
            Else                                     direction := 1; // left
        End
        Else
        Begin
            len := height;
            // Vertical
            If  (location.Y > ((lly + ury)/2)) Then  direction := 2  // top
            Else                                     direction := 3; // bottom
        End;

        port := SchServer.SchObjectFactory(ePort, eCreate_Default);
        If (port <> Nil) Then
        Begin
            port.Name  := net_name;
            port.Width := len;

            If direction = 0 Then  // right
            Begin
                port.Location  := Point(Location.X - len, Location.Y);
                port.Style     := ePortRight;
                Port.Alignment := eLeftAlign;   // text alignment
            End
            Else If direction = 1 Then // left
            Begin
                port.Location  := Location;
                port.Style     := ePortLeft;
                Port.Alignment := eLeftAlign;   // text alignment
            End
            Else If direction = 2 Then // top
            Begin
                port.Location  := Point(Location.X, Location.Y - len);
                port.Style     := ePortTop;
                Port.Alignment := eRightAlign;   // text alignment
            End
            Else If direction = 3 Then // bottom
            Begin
                port.Location  := Location;
                port.Style     := ePortBottom;
                Port.Alignment := eRightAlign;   // text alignment
            End;

            Obj    := port;
            Result := True;
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetLabelValue(const InputStr : TDynamicString) : TDynamicString;
Var
    tokens      : TStringList;
    object_code : Integer;
Begin
    Result := '';
    tokens := TStringList.Create;
    TokenizeString(InputStr, c_SpaceDelimiter, tokens, -1);
    If tokens.Count > c_ObjectCodeIndex Then
    Begin
        object_code := StrToInt(tokens[c_ObjectCodeIndex]);
        If object_code = c_TextObjectCode Then
        Begin
            If tokens.Count > c_TextDataIndex Then
                Result := tokens[c_TextDataIndex];
        End;
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetLabelIndex(const ComponentData : TStringList;
                       const LabelValue    : TDynamicString;
                       var   Index         : Integer) : Boolean;
Var
    i      : Integer;
Begin
    Result := False;
    For i := 0 To (ComponentData.Count - 1) Do
    Begin
        If (GetLabelValue(ComponentData[i]) = LabelValue) Then
        Begin
            Index  := i;
            Result := True;
            Break;
        End;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure UpdateComponentLabel(const Str        : TDynamicString;
                                     CompLabel  : ISch_Label);
Var
    x            : TCoord;
    y            : TCoord;
    angle        : Double;
    anchor       : Integer;
    tokens       : TStringList;
Begin
    tokens := TStringList.Create;
    TokenizeString(Str, c_SpaceDelimiter, tokens, c_TextDataIndex);
    If (tokens.Count >= c_TextDataIndex) Then
    Begin
        StrToCoord(tokens[c_TextLocationXIndex], x);
        StrToCoord(tokens[c_TextLocationYIndex], y);
        angle        := StrToFloat(tokens[c_TextRotationIndex]);
        anchor       := StrToInt(tokens[c_TextAnchorIndex]);

        CompLabel.Location      := Point(x, y);
        CompLabel.Justification := GetTextJustification(anchor);
        CompLabel.Orientation   := GetOrientation(angle);
    End;
    tokens.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateNormalComponent(const Data     : TStringList;
                                        Name     : TDynanicString;
                                        Refdes   : TDynamicString;
                                        Value    : TDynamicString;
                                        Model    : TDynamicString;
                                        Location : TLocation;
                                  var   Obj      : ISch_GraphicalObject) : Boolean;
Var
    component   : ISch_Component;
    sub_obj     : ISch_GraphicalObject;
    object_type : Integer;
    i           : Integer;
    str_list    : TStringList;
    pin_des     : TDynamicString;
    pin_map     : TDynamicString;
    impl_obj    : ISch_Implementation;
Begin
    Result := False;
    Obj    := Nil;

    pin_map := '';
    pin_des := '';

    component := SchServer.SchObjectFactory(eSchComponent, eCreate_Default);
    If component <> Nil Then
    Begin
        component.LibReference  := Name;
        component.Location      := Location;
        component.PartCount     := 1;
        component.CurrentPartId := 1;

        // Designator
        component.Designator.Location     := Location;
        component.Designator.Text         := Refdes;
        component.Designator.Autoposition := True;
        component.Designator.IsHidden     := False;
        If GetLabelIndex(Data, '&1', i) Then
            UpdateComponentLabel(Data[i], Component.Designator);

        // Comment
        component.Comment.Location        := location;
        component.Comment.Text            := value;
        component.Comment.Autoposition    := True;
        If GetLabelIndex(Data, '&2', i) Then
        Begin
            UpdateComponentLabel(Data[i], Component.Comment);
            component.Comment.IsHidden  := False;
        End
        Else
            component.Comment.IsHidden  := True;

        g_Index   := 1;
        g_Source  := c_StringListSource;
        g_StrList := Data;

        str_list := TStringList.Create;
        While (GetNextObjectData(object_type, str_list)) Do
        Begin
            Case object_type Of
                c_SchLineType    : TranslateLine(str_list, sub_obj);
                c_SchArcType     : TranslateArc(str_list, sub_obj);
                c_SchPolygonType : TranslateSchPolygon(str_list, sub_obj);
                c_SchLabelType   : TranslateSchComponentLabel(str_list, sub_obj);
                c_SchPinType     :
                Begin
                    If TranslateSchComponentPin(str_list, Data, sub_obj, pin_des) Then
                    Begin
                        If pin_map <> '' Then
                            pin_map := pin_map + ',';
                        pin_map := pin_map + '(' + pin_des + ':' + pin_des + ')';
                    End;
                End
                Else
                    sub_obj := Nil;
            End;

            If (sub_obj <> Nil) Then
            Begin
                sub_obj.OwnerPartId := component.CurrentPartId;
                sub_obj.Selection   := False;
                component.AddSchObject(sub_obj);
            End;

{
            If (Model <> '') Then
            Begin
                impl_obj := SchServer.SchObjectFactory(eImplementation, eCreate_Default);
                If (impl_obj <> Nil) Then
                Begin
                    impl_obj.ModelName    := Model;
                    impl_obj.ModelType    := cModelType_PCB;
                    impl_obj.MapAsString  := pin_map;

                    component.AddSchObject(impl_obj);
                End;
            End;
 }

        End;
        str_list.Free;

        component.UpdatePrimitivesAccessibility;

        Obj    := component;
        Result := True;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSchComponent(const Data : TStringList;
                               var   Obj  : ISch_GraphicalObject)  : Boolean;
Var
    name       : TDynamicString;
    parameter  : TDynamicString;
    refdes     : TDynamicString;
    value      : TDynamicString;
    model      : TDynamicString;
    angle      : Double;
    location   : TLocation;
    tokens     : TStringList;
    x          : TCoord;
    y          : TCoord;
Begin
    Result := True;
    Obj    := Nil;

    name   := '';
    refdes := '';
    value  := '';
    model  := '';

    If (Data.Count = 0) Then
        Exit;

    tokens := TStringList.Create;
    // Parse the header
    TokenizeString(Data[0], c_SpaceDelimiter, tokens, c_ComponentParameterIndex);

    If (tokens.Count > c_ComponentParameterIndex) Then
    Begin
        angle     := StrToFloat(tokens[c_ComponentRotationIndex]);
        parameter := tokens[c_ComponentParameterIndex];
        tokens.Clear;
        TokenizeString(parameter, c_ComponentParamDelimiter, tokens, -1);
        If (tokens.Count > c_ComponentNameIndex)   Then  name   := tokens[c_ComponentNameIndex];
        If (tokens.Count > c_ComponentRefdesIndex) Then  refdes := tokens[c_ComponentRefdesIndex];
        If (tokens.Count > c_ComponentValueIndex)  Then  value  := tokens[c_ComponentValueIndex];
    End;
    // Parse the end line for the location
    TokenizeString(Data[Data.Count - 1], c_SpaceDelimiter, tokens, -1);
    If tokens.Count > c_ComponentLocationYIndex Then
    Begin
        StrToCoord(tokens[c_ComponentLocationXIndex], x);
        StrToCoord(tokens[c_ComponentLocationYIndex], y);
        location := Point(x, y);
    End;
    tokens.Free;

    name := UpperCase(name);

    If (name = '') Then
        Exit;
    //If name = '$$BOX' Then
    //    Exit;
    //If name = '$$TTL' Then
    //    Exit;
    If name = '$$BUS' Then
        Exit;

    If      ((name = '$$GND') Or (name = '$$PWR')) Then Result := TranslatePowerComponent(Data, name, angle, location, Obj)
    Else If (name = '$$SIG')                       Then Result := TranslateSignalComponent(Data, name, location, Obj)
    Else                                                Result := TranslateNormalComponent(Data, name, refdes, value, model, location, Obj);

End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateObject(      ObjectType : Integer;
                         const Data       : TStringList;
                         var   Obj        : ISch_GraphicalObject) : Boolean;
Begin
    Case ObjectType Of
        c_SchLineType      :  Result := TranslateLine(Data, Obj);
        c_SchArcType       :  Result := TranslateArc(Data, Obj);
        c_SchComponentType :  Result := TranslateSchComponent(Data, Obj);
        c_SchLabelType     :  Result := TranslateText(Data, Obj);
        c_SchPolygonType   :  Result := TranslateSchPolygon(Data, Obj);
        c_BusType          :  Result := TranslateBus(Data, Obj);
        c_SchWireType      :  Result := TranslateWire(Data, Obj);
        Else
        Begin
            Obj    := Nil;
            Result := True;
        End;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslatePrimaryData(      SchDoc    : ISch_Document;
                              const Inputfile : TDynamicString) : Boolean;
Var
   in_file     : Text;
   object_type : Integer;
   obj         : ISch_GraphicalObject;
   log_msg     : TDynamicString;
   data        : TStringList;
   str         : TDynamicString;
   bound_rect  : TCoordRect;
Begin
     Result := True;

     AssignFile(in_file, InputFile);
     Reset(in_file);

     
     ReadUntilPrimaryDataSection(in_file, str);

     g_LookAhead := str;
     g_InFile    := in_file;
     g_Source    := c_InputFileSource;

     data := TStringList.Create;

     While GetNextObjectData(object_type, data) Do
     Begin
         str := g_LookAhead;  // save the lookahead here

         Result := TranslateObject(object_type, data, obj);
         If Not Result Then
         Begin
             log_msg := 'Error translating';
             Log(log_msg);
             LogUnderLine(log_msg);
             LogStringList(data);
             Break;
         End;

         If obj <> nil Then
         Begin
              SchDoc.AddSchObject(obj);

              bound_rect := obj.BoundingRectangle_Full;
              If bound_rect.right > g_WorkspaceWidth Then
                 g_WorkspaceWidth := bound_rect.right;
              If bound_rect.top > g_WorkspaceHeight Then
                 g_WorkspaceHeight := bound_rect.top;
         End;

         g_LookAhead := str;
         g_InFile    := in_file;
         g_Source    := c_InputFileSource;
     End;

     data.Free;
     CloseFile(in_file);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TranslateSch(const InputFile    : TDynamicString;
                      const OutputDir    : TDynamicString;
                      var   OutputFile   : TDynamicString) : Boolean;
Var
   setting_table : TStringList;
   sch_document  : ISch_Document;
   document      : IServerDocument;
   filename      : TDynamicString;
Begin
     Result := True;
     Try
          If Not ValidateFile(InputFile) Then
          Begin
               Log('Input file is not a CirCad data file.');
               Exit;
          End;

          filename := CreateOutputFileName(InputFile);
          If filename = '' Then
          Begin
               Log('Cannot construct output file name');
               Exit;
          End;

          OutputFile := IncludeTrailingPathDelimiter(OutputDir) + filename;

          document := CreateDocument(OutputFile, cDocKind_Sch, 'BINARY');
          If document = nil Then
          Begin
               Log('Cannot create documment ' + OutputFile);
               Exit;
          End;

          setting_table            := TStringList.Create;
          setting_table.Sorted     := True;
          setting_table.Duplicates := dupIgnore;

          InitSettingTable(setting_table);
          ReadDesignSettings(InputFile, setting_table);

          g_BusLayerNum := GetBusLayerNum(setting_table);

          sch_document := SchServer.GetSchDocumentByPath(OutputFile);
          SetupSchDocument(sch_document, setting_table);
          TranslatePrimaryData(sch_document, InputFile);

          sch_document.CustomX := g_WorkspaceWidth;
          sch_document.CustomY := g_WorkspaceHeight;
          sch_document.UpdateDocumentProperties;

          SaveDocument(document, 'BINARY');

          DestroyStringList(setting_table);
          Result := True;
     Except
           Result := False;
           Log('Exception thrown');
     End;
End;
///////////////////////////////////////////////////////////////////////////////

Procedure TranslateCirCadFile;
Begin
    TranslatorDialog.ShowModal;
End;

