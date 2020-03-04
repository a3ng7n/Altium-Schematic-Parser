{..............................................................................}
{ Summary Generate Parameters with Pin Net Info for pins of components from    }
{         a focussed sch or selected schs of a PCB Project.                    }
{                                                                              }
{ Version 2.0                                                                  }
{ November 15th 2006                                                           }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    PinsData  : TStringList;
{..............................................................................}

{..............................................................................}
Procedure CreateAParameterObject(ASchDoc : ISch_Document; APinNetName : String; APin : ISch_Pin; AComponent : ISch_Component; AColor : TColor; AnOffset : Integer; Name : String; NameShow : Boolean; AssignParamToPin : Boolean);
Var
    SchParameter : ISch_Parameter;
    PinLocation  : TLocation;
    CompLocation : TLocation;
    PinLocX      : Integer;
    PinLocY      : Integer;
Begin
    SchParameter := SchServer.SchObjectFactory(eParameter,eCreate_GlobalCopy);
    If SchParameter = Nil Then Exit;

    // Check location of pin in respect to component. if left side, offset -, if right side offset +
    PinLocation := APin.GetState_Location;
    PinLocX     := PinLocation.X;
    PinLocY     := PinLocation.Y;

    PinLocation.X  := PinLocX;
    PinLocation.Y  := PinLocY;
    SchParameter.SetState_Location(PinLocation);

    SchParameter.Name     := Name;
    SchParameter.ShowName := NameShow;
    SchParameter.Text     := APinNetName;
    SchParameter.IsHidden := False;
    SchParameter.Color    := AColor;

    //TRotationBy90 for Orientation property.
    If APin.Orientation = eRotate0   Then PinLocX  := PinLocX + DxpsToCoord(AnOffset);
    If APin.Orientation = eRotate180 Then PinLocX  := PinLocX - DxpsToCoord(AnOffset);
    If APin.Orientation = eRotate90  Then PinLocY  := PinLocY + DxpsToCoord(AnOffset);
    If APin.Orientation = eRotate270 Then PinLocY  := PinLocY - DxpsToCoord(AnOffset);

    SchParameter.Orientation := APin.Orientation;

    If AssignParamToPin = True Then
    Begin
        APin.AddSchObject(SchParameter);
        SchServer.RobotManager.SendMessage(APin.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, SchParameter.I_ObjectAddress);
    End
    Else
    Begin
        AComponent.AddSchObject(SchParameter);
        SchServer.RobotManager.SendMessage(AComponent.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, SchParameter.I_ObjectAddress);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchPinsOfComponent(ASchDoc : ISch_Document; AComponent : ISch_Component; Pinsdata : TStringList; AColor : TColor; AnOffset : Integer; Name : String; NameShow : Boolean; AssignParamToPin : Boolean);
Var
    APin          : ISch_Pin;
    SchParameter  : ISch_Parameter;
    PinIterator   : ISch_Iterator;
    I, Index      : Integer;
    PinFullString : String;
    PinDesignator : String;
    PinNetName    : String;
    PinLocation   : TLocation;
Begin
    PinIterator := AComponent.SchIterator_Create;
    PinIterator.AddFilter_ObjectSet(MkSet(ePin));
    Try
        APin := PinIterator.FirstSchObject;
        While APin <> Nil Do
        Begin
            I := 0;
            While I < Pinsdata.Count Do
            Begin
                PinFullString := Pinsdata.Strings[i];
                // PinDesignator needs to only have the PinNumber string but
                // it has the PinNumber : FlattenednetName string
                Index := Pos(':',PinFullString);
                PinDesignator := Copy(PinFullString,0,Index-2); // exclude the whitespace and : chars
                PinNetName    := Copy(PinFullString,Index + 2,Length(PinFullString));

                // Put a parameter with net info next to each pin of a component
                If APin.Designator = PinDesignator Then
                Begin
                    CreateAParameterObject(ASchDoc, PinNetName, APin, AComponent, AColor, AnOffset, Name, NameShow, AssignParamToPin);
                    Pinsdata.Delete[I];
                End
                Else
                    Inc(I);
            End;
            APin := PinIterator.NextSchObject;
        End;
    Finally
        AComponent.SchIterator_Destroy(PinIterator);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateParametersForPins(CurrentSch : ISch_Document; CompInfo : TStringList; AColor : TColor; AnOffset : Integer; Name : String; NameShow : Boolean; AssignParamToPin : Boolean);
Var
    Iterator             : ISch_Iterator;
    AComponent           : ISch_Component;
    I                    : Integer;
    APartID              : Integer;
    CompDesignator       : String;
    AComponentDesignator : String;
    PinsData             : TStringList;
Begin
    // Go through all components on the schematic sheet and fetch pins of each comp
    Iterator := CurrentSch.SchIterator_Create;
    If Iterator = Nil Then Exit;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Try
        // for each component, get its pins and their connectivity info
        // need to account for multi part components. how to detect multi part components?
        AComponent := Iterator.FirstSchObject;
        While AComponent <> Nil Do
        Begin
            If AComponent.IsMultiPartComponent Then
                AComponentDesignator := AComponent.FullPartDesignator(AComponent.GetState_CurrentPartID)
            Else
                AComponentDesignator := AComponent.Designator.Text;

            I := 0;
            While I < CompInfo.Count Do
            Begin
                CompDesignator := CompInfo.Strings[i];
                If CompDesignator = AComponentDesignator Then
                Begin
                     PinsData  := CompInfo.Objects[i];
                     FetchPinsOfComponent(CurrentSch, AComponent, PinsData, AColor, AnOffset, Name, NameShow, AssignParamToPin);
                     CompInfo.Delete(i);
                 End
                 Else
                     Inc(I);
            End;
            AComponent := Iterator.NextSchObject;
        End;
    Finally
        CurrentSch.SchIterator_Destroy(Iterator);
    End;

    // Refresh schematic doc that has new parameter objects
    CurrentSch.GraphicallyInvalidate;
    ResetParameters;
    AddStringParameter('Action', 'Document');
    RunProcess('Sch:Zoom');
End;
{..............................................................................}

{..............................................................................}
Procedure FetchNetsOfComponents(Doc : IDocument; CompInfo : TStringList);
Var
    I, J, K      : Integer;
    Comp         : Component;
    MultiPartCnt : Integer;
    MultiPart    : IPart;
    Pin          : IPin;
Begin
    For J := 0 to Doc.DM_ComponentCount - 1 Do
    Begin
        Comp     := Doc.DM_Components(J);

        If Comp.DM_SubPartCount = 1 Then
        Begin
            PinsData := TStringList.Create;
            For K := 0 to Comp.DM_PinCount - 1 Do
            Begin
                Pin := Comp.DM_Pins(K);
                PinsData.Add(Pin.DM_PinNumber + ' : ' + Pin.DM_FlattenedNetName);
            End;
            CompInfo.AddObject(Comp.DM_FullLogicalDesignator, PinsData);
        End
        Else If Comp.DM_SubPartCount > 1 Then
        Begin
            For MultiPartCnt := 0 to Comp.DM_SubPartCount - 1 Do
            Begin
                PinsData  := TStringList.Create;
                MultiPart := Comp.DM_SubParts(MultiPartCnt);
                For K := 0 to MultiPart.DM_PinCount - 1 Do
                Begin
                    Pin := MultiPart.DM_Pins(K);
                    If Pin.DM_FlattenedNetName <> '?' Then
                        PinsData.Add(Pin.DM_PinNumber + ' : ' + Pin.DM_FlattenedNetName);
                End;
                CompInfo.AddObject(MultiPart.DM_FullLogicalDesignator, PinsData);
            End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchNetsFromFocussedDocumentOnlyAndGenerateParameters(ACompInfo : TStringList; AProject : IProject; ParameterColor : TColor; ParameterOffset : Integer; ParameterName : String; ParameterNameShow : Boolean; AssignParamToPin : Boolean);
Var
    I, K, J     : Integer;
    Doc         : IDocument;
    FocussedDoc : IDocument;
    CurrentSch  : ISch_Document;
Begin
     FocussedDoc := GetWorkspace.DM_FocusedDocument;
     If FocussedDoc = Nil Then Exit;

     // obtain the physical document thats the same as the focussed document
     // need the physical document for net information...
     For J := 0 to AProject.DM_PhysicalDocumentCount - 1 Do
     Begin
         Doc := AProject.DM_PhysicalDocuments(J);
         If Doc.DM_FullPath = FocussedDoc.DM_FullPath Then Break;
     End;
     If Doc.DM_FullPath <> FocussedDoc.DM_FullPath Then Exit;

     // get nets from the Doc - IDocument type
     FetchNetsOfComponents    (Doc, ACompInfo);

     CurrentSch := SchServer.GetCurrentSchDocument;
     If CurrentSch = Nil Then Exit;
     Try
         SchServer.ProcessControl.PreProcess(CurrentSch, '');
         GenerateParametersForPins(CurrentSch, ACompInfo, ParameterColor, ParameterOffset, ParameterName, ParameterNameShow, AssignParamToPin);
     Finally
         SchServer.ProcessControl.PostProcess(CurrentSch, '');
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchNetsFromDocumentsAndGenerateParameters(ADocumentList : TStringList; ACompInfo : TStringList; AProject : IProject; ParameterColor : TColor; ParameterOffset : Integer; ParameterName : String; ParameterNameShow : Boolean; AssignParamToPin : Boolean);
Var
    I, K, J     : Integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SchDocument : IServerDocument;
Begin
    // selected or all documents within the project
    For J := 0 to AProject.DM_PhysicalDocumentCount - 1 Do
    Begin
        Doc := AProject.DM_PhysicalDocuments(J);
        For I := 0 to ADocumentList.Count - 1 Do
        Begin
            If Doc.DM_Filename = ADocumentList.Strings[i] Then
            Begin
                FetchNetsOfComponents(Doc, ACompInfo);

                // Open this doc and focus it.
                SchDocument := Client.OpenDocument('Sch', Doc.DM_FullPath);
                If SchDocument <> Nil Then
                Begin
                    Client.ShowDocument(SchDocument);

                    CurrentSch := SchServer.GetCurrentSchDocument;
                    If CurrentSch = Nil Then Exit;
                    Try
                        SchServer.ProcessControl.PreProcess(CurrentSch, '');
                        GenerateParametersForPins(CurrentSch, ACompInfo, ParameterColor, ParameterOffset, ParameterName, ParameterNameShow, AssignParamToPin);
                    Finally
                        SchServer.ProcessControl.PostProcess(CurrentSch, '');
                    End;
                End;
            End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchComponentNetInfo(DocumentScope : Integer;  ADocumentList : TStringList; ParameterColor : TColor; ParameterOffset : Integer; ParameterName : String; ParameterNameShow : Boolean; AssignParamToPin : Boolean);
Var
    ACompInfo   : TStringList;
    Project     : IProject;
Begin
    BeginHourGlass;

    // Check if schematic server exists or not.
    If SchServer = Nil Then Exit;
    ACompInfo := TStringList.Create;

    // do a compile so the logical documents get expanded into physical documents.
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;
    Project.DM_Compile;

    If DocumentScope = 0 Then
        FetchNetsFromFocussedDocumentOnlyAndGenerateParameters(ACompInfo, Project, ParameterColor, ParameterOffset, ParameterName, ParameterNameShow, AssignParamToPin)
    Else
        FetchNetsFromDocumentsAndGenerateParameters(ADocumentList, ACompInfo, Project, ParameterColor, ParameterOffset, ParameterName, ParameterNameShow, AssignParamToPin);

    ACompInfo.Free;
    EndHourGlass;
End;
{..............................................................................}

{..............................................................................}
Procedure RunConnectivityInfoGenerator;
Var
    SchDoc : ISch_Document;
Begin
    If SchServer = Nil Then Exit;
    SchDoc := SchServer.GetCurrentSchDocument;
    If SchDoc = Nil Then Exit;

    FormParamMaker.ShowModal;
End;
{..............................................................................}

{..............................................................................}
End.


// Connectivity Script Feature/Issue Information.
// =============================================

// Known Issues
// ============
// if parameters attached to its associated component, parameters are not rotated.
// Cannot deal with multi channel designs - designators are expanded dynamically.
// if script has an error, you click on stop but the dialog is still there. have to close ALt Des and try again.
// Dialog is always centered on monitor one no matter where Alt Des is. Changing the Dialog form's position has no effect.


// Version 3
// =========
// Deal with Multi channel designs.
// Check for parameter duplicates
// Optimisation of fetch and generate parameters operations.
