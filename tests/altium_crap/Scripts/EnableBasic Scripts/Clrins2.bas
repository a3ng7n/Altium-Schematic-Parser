'--------------------------------------------------------------------------------------
' $Summary Clear Inside - Delete objects within an area defined by user. Confirm before deleting.             
' Copyright (c) 2004 by Altium Limited
'
' Enable Basic script For DXP 2004 SP2 
'--------------------------------------------------------------------------------------

'--------------------------------------------------------------------------------------
Sub Main
    Dim Result As Integer 

    Call ResetParameters
    Call AddStringParameter("Action","All")
    Call RunProcess("Sch:Deselect")
    Call ResetParameters
    Call AddStringParameter("Action","InsideArea")
    Call RunProcess("Sch:Select")
    Call ResetParameters
    Call ConfirmNoYes("Confirm Delete",Result)


    If Result = 1 Then
       RunProcess("Sch:Clear")
    Else
       Call AddStringParameter("Action","All")
       RunProcess("Sch:Deselect")
    End IF
End Sub
'--------------------------------------------------------------------------------------

