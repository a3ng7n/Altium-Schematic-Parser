'--------------------------------------------------------------------------------------
' $Summary Place Schematic Port Object
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub Main
   Call ResetParameters
   Call AddIntegerParameter("Location.X", 100        )
   Call AddIntegerParameter("Location.Y", 100        )
   Call AddIntegerParameter("Style"     , 2          )
   Call AddIntegerParameter("IOType"    , 3          )
   Call AddIntegerParameter("Alignment" , 0          )
   Call AddIntegerParameter("Width"     , 100        )
   Call AddStringParameter ("Name"      , "Test Port")
   Call AddLongIntParameter("AreaColor" , &HFFFFFF   )
   Call AddLongIntParameter("TextColor" , &H000000   )
   Call RunProcess         ("Sch:PlacePort"          )
End Sub
'--------------------------------------------------------------------------------------
