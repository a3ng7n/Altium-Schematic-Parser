'--------------------------------------------------------------------------------------
' $Summary PCB Color Scheme - Demonstrate the use of a script in changing PCB Colors
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub Main
    Scheme = ""  
    R = RunDialog(Scheme)     
    If  R <> 0 Then
        Select Case Scheme
          Case 0
           SetSchemeDefaults
          Case 1
           SetSchemeClassic          
          End Select
    End IF
End Sub
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Function RunDialog(ByRef Scheme As String) As String
   ' Dialog box definition

   Dim ColorListBoxSource$ (2)
       ColorListBoxSource$ (0) = "Defaults (Version 3)"
       ColorListBoxSource$ (1) = "Classic  (Version 2)"

Begin Dialog DLGTYPE 100,100, 168, 65, "Please Choose a PCB Color Scheme"
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
Sub SetSchemeDefaults
  
  Call ResetParameters
  Call AddStringParameter ("TopSignalColor"         , "255"       )
  Call AddStringParameter ("Mid1Color"              , "128"       ) 
  Call AddStringParameter ("Mid2Color"              , "32768"     )
  Call AddStringParameter ("Mid3Color"              , "65280"     )
  Call AddStringParameter ("Mid4Color"              , "8388608"   )
  Call AddStringParameter ("Mid5Color"              , "16776960"  )
  Call AddStringParameter ("Mid6Color"              , "8388736"   )
  Call AddStringParameter ("Mid7Color"              , "16711935"  )
  Call AddStringParameter ("Mid8Color"              , "32896"     )
  Call AddStringParameter ("Mid9Color"              , "65535"     )
  Call AddStringParameter ("Mid10Color"             , "8421504"   )
  Call AddStringParameter ("Mid11Color"             , "32768"     )
  Call AddStringParameter ("Mid12Color"             , "8388736"   )
  Call AddStringParameter ("Mid13Color"             , "8421376"   )
  Call AddStringParameter ("Mid14Color"             , "12632256"  )
  Call AddStringParameter ("Mid15Color"             , "128"       ) 
  Call AddStringParameter ("Mid16Color"             , "32768"     )
  Call AddStringParameter ("Mid17Color"             , "65280"     )
  Call AddStringParameter ("Mid18Color"             , "8388608"   )
  Call AddStringParameter ("Mid19Color"             , "16776960"  )
  Call AddStringParameter ("Mid20Color"             , "8388736"   )
  Call AddStringParameter ("Mid21Color"             , "16711935"  )
  Call AddStringParameter ("Mid22Color"             , "32896"     )
  Call AddStringParameter ("Mid23Color"             , "65535"     )
  Call AddStringParameter ("Mid24Color"             , "8421504"   )
  Call AddStringParameter ("Mid25Color"             , "32768"     )
  Call AddStringParameter ("Mid26Color"             , "8388736"   )
  Call AddStringParameter ("Mid27Color"             , "8421376"   )
  Call AddStringParameter ("Mid28Color"             , "12632256"  )
  Call AddStringParameter ("Mid29Color"             , "128"       )
  Call AddStringParameter ("Mid30Color"             , "32768"     )
  Call AddStringParameter ("BottomSignalColor"      , "16711680"  )
  Call AddStringParameter ("TopOverlayColor"        , "32768"     )
  Call AddStringParameter ("BottomOverlayColor"     , "7585984"   )
  Call AddStringParameter ("TopPasteColor"          , "8388736"   )
  Call AddStringParameter ("BottomPasteColor"       , "128"       )
  Call AddStringParameter ("TopSolderColor"         , "3162822"   )
  Call AddStringParameter ("BottomSolderColor"      , "7307173"   )
  Call AddStringParameter ("Plane1Color"            , "32768"     )
  Call AddStringParameter ("Plane2Color"            , "128"       )
  Call AddStringParameter ("Plane3Color"            , "8388736"   )
  Call AddStringParameter ("Plane4Color"            , "8421376"   )
  Call AddStringParameter ("Plane5Color"            , "32768"     )
  Call AddStringParameter ("Plane6Color"            , "128"       )
  Call AddStringParameter ("Plane7Color"            , "8388736"   )
  Call AddStringParameter ("Plane8Color"            , "8421376"   )
  Call AddStringParameter ("Plane9Color"            , "32768"     )
  Call AddStringParameter ("Plane10Color"           , "128"       )
  Call AddStringParameter ("Plane11Color"           , "8388736"   )
  Call AddStringParameter ("Plane12Color"           , "8421376"   )
  Call AddStringParameter ("Plane13Color"           , "32768"     )
  Call AddStringParameter ("Plane14Color"           , "128"       )
  Call AddStringParameter ("Plane15Color"           , "8388736"   )
  Call AddStringParameter ("Plane16Color"           , "8421376"   )
  Call AddStringParameter ("DrillGuideColor"        , "128"       )
  Call AddStringParameter ("KeepOutColor"           , "8388736"   )
  Call AddStringParameter ("Mechanical1Color"       , "8388736"   )
  Call AddStringParameter ("Mechanical2Color"       , "8421376"   )
  Call AddStringParameter ("Mechanical3Color"       , "32768"     )
  Call AddStringParameter ("Mechanical4Color"       , "0"         )
  Call AddStringParameter ("Mechanical5Color"       , "8388736"   )
  Call AddStringParameter ("Mechanical6Color"       , "8421376"   )
  Call AddStringParameter ("Mechanical7Color"       , "32768"     )
  Call AddStringParameter ("Mechanical8Color"       , "0"         )
  Call AddStringParameter ("Mechanical9Color"       , "8388736"   )
  Call AddStringParameter ("Mechanical10Color"      , "8421376"   )
  Call AddStringParameter ("Mechanical11Color"      , "32768"     )
  Call AddStringParameter ("Mechanical12Color"      , "0"         )
  Call AddStringParameter ("Mechanical13Color"      , "8388736"   )
  Call AddStringParameter ("Mechanical14Color"      , "8421376"   )
  Call AddStringParameter ("Mechanical15Color"      , "32768"     )
  Call AddStringParameter ("Mechanical16Color"      , "0"         )
  Call AddStringParameter ("DrillDrawingColor"      , "3408013"   )
  Call AddStringParameter ("MultiLayerColor"        , "8421504"   )
  Call AddStringParameter ("ConnectLayerColor"      , "7709086"   )
  Call AddStringParameter ("BackgroundColor"        , "0"         )
  Call AddStringParameter ("DRCErrorColor"          , "65280"     )
  Call AddStringParameter ("SelectionColor"         , "65535"     )
  Call AddStringParameter ("VisibleGrid1Color"      , "12632256"  )
  Call AddStringParameter ("VisibleGrid2Color"      , "11913679"  )
  Call AddStringParameter ("PadHoleColor"           , "6899487"   )
  Call AddStringParameter ("ViaHoleColor"           , "9279142"   )
  
  Call RunProcess ("Pcb:SetupPreferences")

End Sub
'--------------------------------------------------------------------------------------
 
'--------------------------------------------------------------------------------------
Sub SetSchemeClassic          
  
  Call ResetParameters
  Call AddStringParameter ("TopSignalColor"         , "255"       )                   
  Call AddStringParameter ("Mid1Color"              , "128"       )                   
  Call AddStringParameter ("Mid2Color"              , "32768"     )                 
  Call AddStringParameter ("Mid3Color"              , "65280"     )                 
  Call AddStringParameter ("Mid4Color"              , "8388608"   )               
  Call AddStringParameter ("Mid5Color"              , "16776960"  )              
  Call AddStringParameter ("Mid6Color"              , "8388736"   )               
  Call AddStringParameter ("Mid7Color"              , "16711935"  )              
  Call AddStringParameter ("Mid8Color"              , "32896"     )                 
  Call AddStringParameter ("Mid9Color"              , "65535"     )                 
  Call AddStringParameter ("Mid10Color"             , "8421504"   )               
  Call AddStringParameter ("Mid11Color"             , "16777215"  )              
  Call AddStringParameter ("Mid12Color"             , "8388736"   )               
  Call AddStringParameter ("Mid13Color"             , "8421376"   )               
  Call AddStringParameter ("Mid14Color"             , "12632256"  )              
  Call AddStringParameter ("Mid15Color"             , "128"       )                   
  Call AddStringParameter ("Mid16Color"             , "32768"     )                 
  Call AddStringParameter ("Mid17Color"             , "65280"     )                 
  Call AddStringParameter ("Mid18Color"             , "8388608"   )               
  Call AddStringParameter ("Mid19Color"             , "16776960"  )              
  Call AddStringParameter ("Mid20Color"             , "8388736"   )               
  Call AddStringParameter ("Mid21Color"             , "16711935"  )              
  Call AddStringParameter ("Mid22Color"             , "32896"     )                 
  Call AddStringParameter ("Mid23Color"             , "65535"     )                 
  Call AddStringParameter ("Mid24Color"             , "8421504"   )               
  Call AddStringParameter ("Mid25Color"             , "16777215"  )              
  Call AddStringParameter ("Mid26Color"             , "8388736"   )               
  Call AddStringParameter ("Mid27Color"             , "8421376"   )               
  Call AddStringParameter ("Mid28Color"             , "12632256"  )              
  Call AddStringParameter ("Mid29Color"             , "128"       )                   
  Call AddStringParameter ("Mid30Color"             , "32768"     )                 
  Call AddStringParameter ("BottomSignalColor"      , "16711680"  )              
  Call AddStringParameter ("TopOverlayColor"        , "65535"     )                 
  Call AddStringParameter ("BottomOverlayColor"     , "32896"     )                 
  Call AddStringParameter ("TopPasteColor"          , "8421504"   )               
  Call AddStringParameter ("BottomPasteColor"       , "128"       )                   
  Call AddStringParameter ("TopSolderColor"         , "8388736"   )               
  Call AddStringParameter ("BottomSolderColor"      , "16711935"  )              
  Call AddStringParameter ("Plane1Color"            , "32768"     )                 
  Call AddStringParameter ("Plane2Color"            , "128"       )                   
  Call AddStringParameter ("Plane3Color"            , "8388736"   )               
  Call AddStringParameter ("Plane4Color"            , "8421376"   )               
  Call AddStringParameter ("Plane5Color"            , "32768"     )                 
  Call AddStringParameter ("Plane6Color"            , "128"       )                   
  Call AddStringParameter ("Plane7Color"            , "8388736"   )               
  Call AddStringParameter ("Plane8Color"            , "8421376"   )               
  Call AddStringParameter ("Plane9Color"            , "32768"     )                 
  Call AddStringParameter ("Plane10Color"           , "128"       )                   
  Call AddStringParameter ("Plane11Color"           , "8388736"   )               
  Call AddStringParameter ("Plane12Color"           , "8421376"   )               
  Call AddStringParameter ("Plane13Color"           , "32768"     )                 
  Call AddStringParameter ("Plane14Color"           , "128"       )                   
  Call AddStringParameter ("Plane15Color"           , "8388736"   )               
  Call AddStringParameter ("Plane16Color"           , "8421376"   )               
  Call AddStringParameter ("DrillGuideColor"        , "128"       )                   
  Call AddStringParameter ("KeepOutColor"           , "16711935"  )              
  Call AddStringParameter ("Mechanical1Color"       , "16711935"  )              
  Call AddStringParameter ("Mechanical2Color"       , "8388736"   )               
  Call AddStringParameter ("Mechanical3Color"       , "32768"     )                 
  Call AddStringParameter ("Mechanical4Color"       , "32896"     )                 
  Call AddStringParameter ("Mechanical5Color"       , "16711935"  )              
  Call AddStringParameter ("Mechanical6Color"       , "8388736"   )               
  Call AddStringParameter ("Mechanical7Color"       , "32768"     )                 
  Call AddStringParameter ("Mechanical8Color"       , "32896"     )                 
  Call AddStringParameter ("Mechanical9Color"       , "16711935"  )              
  Call AddStringParameter ("Mechanical10Color"      , "8388736"   )               
  Call AddStringParameter ("Mechanical11Color"      , "32768"     )                 
  Call AddStringParameter ("Mechanical12Color"      , "32896"     )                 
  Call AddStringParameter ("Mechanical13Color"      , "16711935"  )              
  Call AddStringParameter ("Mechanical14Color"      , "8388736"   )               
  Call AddStringParameter ("Mechanical15Color"      , "32768"     )                 
  Call AddStringParameter ("Mechanical16Color"      , "0"         )                 
  Call AddStringParameter ("DrillDrawingColor"      , "2752767"   )               
  Call AddStringParameter ("MultiLayerColor"        , "12632256"  )              
  Call AddStringParameter ("ConnectLayerColor"      , "7709086"   )               
  Call AddStringParameter ("BackgroundColor"        , "0"         )                     
  Call AddStringParameter ("DRCErrorColor"          , "65280"     )                 
  Call AddStringParameter ("SelectionColor"         , "16777215"  )              
  Call AddStringParameter ("VisibleGrid1Color"      , "6049101"   )                 
  Call AddStringParameter ("VisibleGrid2Color"      , "9473425"   )               
  Call AddStringParameter ("PadHoleColor"           , "15461320"  )               
  Call AddStringParameter ("ViaHoleColor"           , "11599871"  )                   
  
  Call RunProcess ("Pcb:SetupPreferences")

End Sub
'--------------------------------------------------------------------------------------
