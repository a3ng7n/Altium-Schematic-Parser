'--------------------------------------------------------------------------------------
' $Summary PCB Layer Sets - Demonstrate the use of macros in changing PCB Layers
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub Main
    LayerSet = ""  
    R = RunDialog(LayerSet)
    If  R <> 0 Then
        Select Case LayerSet
          Case 0
           TopLayers
          Case 1
           BottomLayers
          Case 2
           MechanicalLayers           
          End Select
    End IF
End Sub
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Function RunDialog(ByRef Scheme As String) As String
   ' Dialog box definition

   Dim ColorListBoxSource$ (3)
       ColorListBoxSource$ (0) = "Top Layers"
       ColorListBoxSource$ (1) = "Bottom Layers"
       ColorListBoxSource$ (2) = "Mechanical Layers"

Begin Dialog DLGTYPE 100,100, 168, 65, "Please Choose a Layer Set"
  Text 5,13,41,10, ""
  OKButton 120,8,37,12
  CancelButton 120,24,37,12
  ListBox 8,4,105,55, ColorListboxSource$(), .ColorListBox
End Dialog

   Dim DlgBox As DlgType
   Dim Result As String
   
   Result = Dialog(DlgBox) ' call the dialog box
   Scheme = DlgBox.ColorListBox
   RunDialog = Result
End Function
'--------------------------------------------------------------------------------------
 
'--------------------------------------------------------------------------------------
Sub TopLayers              
  Call ResetParameters
  Call AddStringParameter ("TopSignal"         , "True"  )
  Call AddStringParameter ("Mid1"              , "False" )
  Call AddStringParameter ("Mid2"              , "False" )
  Call AddStringParameter ("Mid3"              , "False" )
  Call AddStringParameter ("Mid4"              , "False" )  
  Call AddStringParameter ("Mid5"              , "False" )  
  Call AddStringParameter ("Mid6"              , "False" )  
  Call AddStringParameter ("Mid7"              , "False" )  
  Call AddStringParameter ("Mid8"              , "False" )  
  Call AddStringParameter ("Mid9"              , "False" )  
  Call AddStringParameter ("Mid10"             , "False" )  
  Call AddStringParameter ("Mid11"             , "False" )  
  Call AddStringParameter ("Mid12"             , "False" )  
  Call AddStringParameter ("Mid13"             , "False" )  
  Call AddStringParameter ("Mid14"             , "False" )  
  Call AddStringParameter ("Mid15"             , "False" )  
  Call AddStringParameter ("Mid16"             , "False" )  
  Call AddStringParameter ("Mid17"             , "False" )  
  Call AddStringParameter ("Mid18"             , "False" )  
  Call AddStringParameter ("Mid19"             , "False" )  
  Call AddStringParameter ("Mid20"             , "False" )  
  Call AddStringParameter ("Mid21"             , "False" )  
  Call AddStringParameter ("Mid22"             , "False" )  
  Call AddStringParameter ("Mid23"             , "False" )  
  Call AddStringParameter ("Mid24"             , "False" )  
  Call AddStringParameter ("Mid25"             , "False" )  
  Call AddStringParameter ("Mid26"             , "False" )  
  Call AddStringParameter ("Mid27"             , "False" )  
  Call AddStringParameter ("Mid28"             , "False" )  
  Call AddStringParameter ("Mid29"             , "False" )  
  Call AddStringParameter ("Mid30"             , "False" )  
  Call AddStringParameter ("BottomSignal"      , "False" )  
  Call AddStringParameter ("TopOverlay"        , "True"  )  
  Call AddStringParameter ("BottomOverlay"     , "False" )  
  Call AddStringParameter ("TopPaste"          , "True"  )  
  Call AddStringParameter ("BottomPaste"       , "False" )  
  Call AddStringParameter ("TopSolder"         , "True"  )  
  Call AddStringParameter ("BottomSolder"      , "False" )  
  Call AddStringParameter ("Plane1"            , "False" )  
  Call AddStringParameter ("Plane2"            , "False" )  
  Call AddStringParameter ("Plane3"            , "False" )  
  Call AddStringParameter ("Plane4"            , "False" )  
  Call AddStringParameter ("Plane5"            , "False" )  
  Call AddStringParameter ("Plane6"            , "False" )  
  Call AddStringParameter ("Plane7"            , "False" )  
  Call AddStringParameter ("Plane8"            , "False" )  
  Call AddStringParameter ("Plane9"            , "False" )  
  Call AddStringParameter ("Plane10"           , "False" )  
  Call AddStringParameter ("Plane11"           , "False" )  
  Call AddStringParameter ("Plane12"           , "False" )  
  Call AddStringParameter ("Plane13"           , "False" )  
  Call AddStringParameter ("Plane14"           , "False" )  
  Call AddStringParameter ("Plane15"           , "False" )  
  Call AddStringParameter ("Plane16"           , "False" )  
  Call AddStringParameter ("DrillGuide"        , "False" )  
  Call AddStringParameter ("KeepOut"           , "False" )  
  Call AddStringParameter ("Mechanical1"       , "False" )  
  Call AddStringParameter ("Mechanical2"       , "False" )  
  Call AddStringParameter ("Mechanical3"       , "False" )  
  Call AddStringParameter ("Mechanical4"       , "False" )
  Call AddStringParameter ("Mechanical5"       , "False" )
  Call AddStringParameter ("Mechanical6"       , "False" )
  Call AddStringParameter ("Mechanical7"       , "False" )
  Call AddStringParameter ("Mechanical8"       , "False" )
  Call AddStringParameter ("Mechanical9"       , "False" )
  Call AddStringParameter ("Mechanical10"      , "False" )
  Call AddStringParameter ("Mechanical11"      , "False" )  
  Call AddStringParameter ("Mechanical12"      , "False" )  
  Call AddStringParameter ("Mechanical13"      , "False" )  
  Call AddStringParameter ("Mechanical14"      , "False" )
  Call AddStringParameter ("Mechanical15"      , "False" )
  Call AddStringParameter ("Mechanical16"      , "False" )
  Call AddStringParameter ("DrillDrawing"      , "False" )  
  Call AddStringParameter ("MultiLayer"        , "True"  )
  
  Call RunProcess ("Pcb:DocumentPreferences")

End Sub
'--------------------------------------------------------------------------------------
 

'--------------------------------------------------------------------------------------
Sub BottomLayers
  Call ResetParameters
  Call AddStringParameter ("TopSignal"         , "False" )
  Call AddStringParameter ("Mid1"              , "False" ) 
  Call AddStringParameter ("Mid2"              , "False" )
  Call AddStringParameter ("Mid3"              , "False" )
  Call AddStringParameter ("Mid4"              , "False" )  
  Call AddStringParameter ("Mid5"              , "False" )  
  Call AddStringParameter ("Mid6"              , "False" )  
  Call AddStringParameter ("Mid7"              , "False" )  
  Call AddStringParameter ("Mid8"              , "False" )  
  Call AddStringParameter ("Mid9"              , "False" )  
  Call AddStringParameter ("Mid10"             , "False" )  
  Call AddStringParameter ("Mid11"             , "False" )  
  Call AddStringParameter ("Mid12"             , "False" )  
  Call AddStringParameter ("Mid13"             , "False" )  
  Call AddStringParameter ("Mid14"             , "False" )  
  Call AddStringParameter ("Mid15"             , "False" )  
  Call AddStringParameter ("Mid16"             , "False" )  
  Call AddStringParameter ("Mid17"             , "False" )  
  Call AddStringParameter ("Mid18"             , "False" )  
  Call AddStringParameter ("Mid19"             , "False" )  
  Call AddStringParameter ("Mid20"             , "False" )  
  Call AddStringParameter ("Mid21"             , "False" )  
  Call AddStringParameter ("Mid22"             , "False" )  
  Call AddStringParameter ("Mid23"             , "False" )  
  Call AddStringParameter ("Mid24"             , "False" )  
  Call AddStringParameter ("Mid25"             , "False" )  
  Call AddStringParameter ("Mid26"             , "False" )  
  Call AddStringParameter ("Mid27"             , "False" )  
  Call AddStringParameter ("Mid28"             , "False" )  
  Call AddStringParameter ("Mid29"             , "False" )  
  Call AddStringParameter ("Mid30"             , "False" )  
  Call AddStringParameter ("BottomSignal"      , "True"  )  
  Call AddStringParameter ("TopOverlay"        , "False" )  
  Call AddStringParameter ("BottomOverlay"     , "True"  )  
  Call AddStringParameter ("TopPaste"          , "False" )  
  Call AddStringParameter ("BottomPaste"       , "True"  )  
  Call AddStringParameter ("TopSolder"         , "False" )  
  Call AddStringParameter ("BottomSolder"      , "True"  )  
  Call AddStringParameter ("Plane1"            , "False" )  
  Call AddStringParameter ("Plane2"            , "False" )  
  Call AddStringParameter ("Plane3"            , "False" )  
  Call AddStringParameter ("Plane4"            , "False" )  
  Call AddStringParameter ("Plane5"            , "False" )  
  Call AddStringParameter ("Plane6"            , "False" )  
  Call AddStringParameter ("Plane7"            , "False" )  
  Call AddStringParameter ("Plane8"            , "False" )  
  Call AddStringParameter ("Plane9"            , "False" )  
  Call AddStringParameter ("Plane10"           , "False" )  
  Call AddStringParameter ("Plane11"           , "False" )  
  Call AddStringParameter ("Plane12"           , "False" )  
  Call AddStringParameter ("Plane13"           , "False" )  
  Call AddStringParameter ("Plane14"           , "False" )  
  Call AddStringParameter ("Plane15"           , "False" )  
  Call AddStringParameter ("Plane16"           , "False" )  
  Call AddStringParameter ("DrillGuide"        , "False" )  
  Call AddStringParameter ("KeepOut"           , "False" )  
  Call AddStringParameter ("Mechanical1"       , "False" )  
  Call AddStringParameter ("Mechanical2"       , "False" )  
  Call AddStringParameter ("Mechanical3"       , "False" )  
  Call AddStringParameter ("Mechanical4"       , "False" )  
  Call AddStringParameter ("Mechanical5"       , "False" )
  Call AddStringParameter ("Mechanical6"       , "False" )
  Call AddStringParameter ("Mechanical7"       , "False" )
  Call AddStringParameter ("Mechanical8"       , "False" )
  Call AddStringParameter ("Mechanical9"       , "False" )
  Call AddStringParameter ("Mechanical10"      , "False" )
  Call AddStringParameter ("Mechanical11"      , "False" )  
  Call AddStringParameter ("Mechanical12"      , "False" )  
  Call AddStringParameter ("Mechanical13"      , "False" )  
  Call AddStringParameter ("Mechanical14"      , "False" )
  Call AddStringParameter ("Mechanical15"      , "False" )
  Call AddStringParameter ("Mechanical16"      , "False" )
  Call AddStringParameter ("DrillDrawing"      , "False" )  
  Call AddStringParameter ("MultiLayer"        , "True"  )  
  
  Call RunProcess ("Pcb:DocumentPreferences")

End Sub
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub MechanicalLayers
  Call ResetParameters
  Call AddStringParameter ("TopSignal"         , "False" )
  Call AddStringParameter ("Mid1"              , "False" ) 
  Call AddStringParameter ("Mid2"              , "False" )
  Call AddStringParameter ("Mid3"              , "False" )
  Call AddStringParameter ("Mid4"              , "False" )  
  Call AddStringParameter ("Mid5"              , "False" )  
  Call AddStringParameter ("Mid6"              , "False" )  
  Call AddStringParameter ("Mid7"              , "False" )  
  Call AddStringParameter ("Mid8"              , "False" )  
  Call AddStringParameter ("Mid9"              , "False" )  
  Call AddStringParameter ("Mid10"             , "False" )  
  Call AddStringParameter ("Mid11"             , "False" )  
  Call AddStringParameter ("Mid12"             , "False" )  
  Call AddStringParameter ("Mid13"             , "False" )  
  Call AddStringParameter ("Mid14"             , "False" )  
  Call AddStringParameter ("Mid15"             , "False" )  
  Call AddStringParameter ("Mid16"             , "False" )  
  Call AddStringParameter ("Mid17"             , "False" )  
  Call AddStringParameter ("Mid18"             , "False" )  
  Call AddStringParameter ("Mid19"             , "False" )  
  Call AddStringParameter ("Mid20"             , "False" )  
  Call AddStringParameter ("Mid21"             , "False" )  
  Call AddStringParameter ("Mid22"             , "False" )  
  Call AddStringParameter ("Mid23"             , "False" )  
  Call AddStringParameter ("Mid24"             , "False" )  
  Call AddStringParameter ("Mid25"             , "False" )  
  Call AddStringParameter ("Mid26"             , "False" )  
  Call AddStringParameter ("Mid27"             , "False" )  
  Call AddStringParameter ("Mid28"             , "False" )  
  Call AddStringParameter ("Mid29"             , "False" )  
  Call AddStringParameter ("Mid30"             , "False" )  
  Call AddStringParameter ("BottomSignal"      , "False" )  
  Call AddStringParameter ("TopOverlay"        , "False" )  
  Call AddStringParameter ("BottomOverlay"     , "False" )  
  Call AddStringParameter ("TopPaste"          , "False" )  
  Call AddStringParameter ("BottomPaste"       , "False" )  
  Call AddStringParameter ("TopSolder"         , "False" )  
  Call AddStringParameter ("BottomSolder"      , "False" )  
  Call AddStringParameter ("Plane1"            , "False" )  
  Call AddStringParameter ("Plane2"            , "False" )  
  Call AddStringParameter ("Plane3"            , "False" )  
  Call AddStringParameter ("Plane4"            , "False" )  
  Call AddStringParameter ("Plane5"            , "False" )  
  Call AddStringParameter ("Plane6"            , "False" )  
  Call AddStringParameter ("Plane7"            , "False" )  
  Call AddStringParameter ("Plane8"            , "False" )  
  Call AddStringParameter ("Plane9"            , "False" )  
  Call AddStringParameter ("Plane10"           , "False" )  
  Call AddStringParameter ("Plane11"           , "False" )  
  Call AddStringParameter ("Plane12"           , "False" )  
  Call AddStringParameter ("Plane13"           , "False" )  
  Call AddStringParameter ("Plane14"           , "False" )  
  Call AddStringParameter ("Plane15"           , "False" )  
  Call AddStringParameter ("Plane16"           , "False" )  
  Call AddStringParameter ("DrillGuide"        , "True"  )  
  Call AddStringParameter ("KeepOut"           , "True"  )  
  Call AddStringParameter ("Mechanical1"       , "True"  )  
  Call AddStringParameter ("Mechanical2"       , "True"  )  
  Call AddStringParameter ("Mechanical3"       , "True"  )  
  Call AddStringParameter ("Mechanical4"       , "True"  )  
  Call AddStringParameter ("Mechanical5"       , "True"  )
  Call AddStringParameter ("Mechanical6"       , "True"  )
  Call AddStringParameter ("Mechanical7"       , "True"  )
  Call AddStringParameter ("Mechanical8"       , "True"  )
  Call AddStringParameter ("Mechanical9"       , "True"  )
  Call AddStringParameter ("Mechanical10"      , "True"  )
  Call AddStringParameter ("Mechanical11"      , "True"  )  
  Call AddStringParameter ("Mechanical12"      , "True"  )  
  Call AddStringParameter ("Mechanical13"      , "True"  )  
  Call AddStringParameter ("Mechanical14"      , "True"  )
  Call AddStringParameter ("Mechanical15"      , "True"  )
  Call AddStringParameter ("Mechanical16"      , "True"  )
  Call AddStringParameter ("DrillDrawing"      , "True"  )  
  Call AddStringParameter ("MultiLayer"        , "False" )  
  
  Call RunProcess ("Pcb:DocumentPreferences")

End Sub
'--------------------------------------------------------------------------------------

