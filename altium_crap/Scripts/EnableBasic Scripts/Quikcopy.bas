'--------------------------------------------------------------------------------------
' $Summary Quick Copy - Select objects, then Copy and Paste.
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub Main
    Call  ResetParameters
    
    ' In Enable Basic, Boolean type is not supported. Therefore, the procedure
    ' "AddBooleanParameter" cannot be called.  Use "AddStringParameter" instead
    ' as shown.
    
    ' Call  AddBooleanParameter("SelectionReference",False)
    Call  AddStringParameter("SelectionReference","False")
    
    Call  RunProcess("Sch:SetupPreferences")
    Call  ResetParameters
    Call  AddStringParameter("Action","All")
    Call  RunProcess("Sch:Deselect")
    Call  ResetParameters
    Call  RunProcess("Sch:ToggleSelection")
    Call  RunProcess("Sch:Copy")
    Call  AddStringParameter("Action","All")
    Call  RunProcess("Sch:Deselect")
    Call  ResetParameters
    Call  RunProcess("Sch:Paste")
    Call  AddStringParameter("Action","All")
    Call  RunProcess("Sch:Deselect")
End Sub    
'--------------------------------------------------------------------------------------
                                                                                       
