'----------------------------------------------------------------------------------
' $Summary Circuit Wizard - Demonstrate the use of macros in placeing/connecting parts together.
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------

' Declare your constants here
Const DEFAULT_RES = "200 ohm"
Const DEFAULT_CAP = "100 uf"

'--------------------------------------------------------------------------------------
Sub Main
    Dim X As Integer, Y As Integer, R As Integer
    Dim Capacitance As String, Resistance As String

    R = RunDialog(Resistance, Capacitance)
    If  R <> 0 Then
      If GetClickPosition(X, Y) <> 0 Then
         Call PlaceFilter(X, Y, Capacitance, Resistance)
      End IF
    End IF
End Sub
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Function RunDialog(ByRef Res As String, ByRef Cap As String) As Integer
   ' Dialog box definition
   Begin Dialog DlgType 100,100,180, 50, "Filter Wizard"
         Text             5, 13, 41, 10, "Capacitance:"
         TextBox         54, 12, 50, 12, .Capacitance
         Text             5, 28, 41, 10, "Resistance:"
         TextBox         54, 27, 50, 12, .Resistance
         OKButton       120, 12, 50, 12
         CancelButton   120, 28, 50, 12
   End Dialog

   Dim DlgBox As DlgType
   Dim Result As Integer

   DlgBox.Capacitance = DEFAULT_CAP
   DlgBox.Resistance  = DEFAULT_RES
   Result = Dialog(DlgBox) ' call the dialog box
   Res = DlgBox.Resistance
   Cap = DlgBox.Capacitance
   RunDialog = Result
End Function
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Function GetClickPosition(ByRef X As Integer, ByRef Y As Integer) As Integer
    Dim AResult As Integer

    Call ResetParameters
    Call RunProcess         ("Sch:AskForXYLocation")
    Call GetIntegerParameter("LocationX", X)
    Call GetIntegerParameter("LocationY", Y)
    Call GetIntegerParameter("Result"   , AResult)
    GetClickPosition = AResult
End Function
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub PlaceFilter(X As Integer, Y As Integer,  Cap As String, Res As String)
   Call ResetParameters
   Call AddStringParameter ("Library","Miscellaneous Devices.IntLib")
   Call AddStringParameter ("LibReference", "Res1" )
   Call AddStringParameter ("ModelType", "SIM" )
   Call AddStringParameter ("ModelParameterName0", "Value" )
   Call AddStringParameter ("ModelParameterValue0", Res )
   Call AddStringParameter ("Designator", "R1"     )
   Call AddIntegerParameter("Location.X", X + 40   )
   Call AddIntegerParameter("Location.Y", Y + 10   )
   Call AddIntegerParameter("Orientation", 0       )
   Call RunProcess         ("IntegratedLibrary:PlaceLibraryComponent")

   Call ResetParameters
   Call AddStringParameter ("Library","Miscellaneous Devices.IntLib")
   Call AddStringParameter ("LibReference", "Cap"  )
   Call AddStringParameter ("ModelType", "SIM" )
   Call AddStringParameter ("ModelParameterName0", "Value" )
   Call AddStringParameter ("ModelParameterValue0", Cap )
   Call AddStringParameter ("Designator", "C1"     )
   Call AddIntegerParameter("Location.X", X + 110  )
   Call AddIntegerParameter("Location.Y", Y - 40   )
   Call AddIntegerParameter("Orientation", 1       )
   Call RunProcess         ("IntegratedLibrary:PlaceLibraryComponent")

   Call ResetParameters
   Call AddColorParameter  ("Color", 255,0,0       )
   Call AddIntegerParameter("Location1.X", X       )
   Call AddIntegerParameter("Location1.Y", Y       )
   Call AddIntegerParameter("Location2.X", X + 30  )
   Call AddIntegerParameter("Location2.Y", Y       )
   Call RunProcess         ("Sch:PlaceWire"        )

   Call ResetParameters
   Call AddColorParameter  ("Color", 255,0,0       )
   Call AddIntegerParameter("Location1.X", X + 70  )
   Call AddIntegerParameter("Location1.Y", Y       )
   Call AddIntegerParameter("Location2.X", X + 150 )
   Call AddIntegerParameter("Location2.Y", Y       )
   Call RunProcess         ("Sch:PlaceWire"        )

   Call ResetParameters
   Call AddColorParameter  ("Color", 255,0,0       )
   Call AddIntegerParameter("Location1.X", X + 120 )
   Call AddIntegerParameter("Location1.Y", Y       )
   Call AddIntegerParameter("Location2.X", X + 120 )
   Call AddIntegerParameter("Location2.Y", Y - 20  )
   Call RunProcess         ("Sch:PlaceWire"        )

   Call ResetParameters
   Call AddIntegerParameter("Location.X" , X + 120 )
   Call AddIntegerParameter("Location.Y" , Y - 50  )
   Call AddIntegerParameter("Orientation", 3       )
   Call AddStringParameter ("S"          ,"GND"    )
   Call AddIntegerParameter("Style", 4             )
   Call RunProcess         ("Sch:PlacePowerPort"   )
End Sub
'--------------------------------------------------------------------------------------
