'--------------------------------------------------------------------------------------
' $Summary Clear Inside - Delete objects within an area defined by user.
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub Main
    Call ResetParameters
    Call AddStringParameter("Action","All")
    Call RunProcess("Sch:Deselect")
    Call ResetParameters
    Call AddStringParameter("Action","InsideArea")
    Call RunProcess("Sch:Select")
    Call ResetParameters
    Call RunProcess("Sch:Clear")
End Sub
'--------------------------------------------------------------------------------------
                                                                                       
