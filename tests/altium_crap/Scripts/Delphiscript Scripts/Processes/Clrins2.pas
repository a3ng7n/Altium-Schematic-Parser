{..............................................................................}
{ Summary Clear Inside - Delete objects within an area defined by user.        }
{ Confirm before deleting. Use on a schematic document.                        }
{ Copyright (c) 2003 by Altium Limited                                         }     
{..............................................................................}

{..............................................................................}
Var
    Value : Integer;
Begin
    ResetParameters;
    AddStringParameter('Action','All');
    RunProcess('Sch:Deselect');
    ResetParameters;
    AddStringParameter('Action','InsideArea');
    RunProcess('Sch:Select');
    ResetParameters;
     
    Value := ConfirmNoYes('Confirm Delete');
    If Value = True Then
    Begin
       RunProcess('Sch:Clear');
    End
    Else
    Begin
       AddStringParameter('Action','All');
       RunProcess('Sch:Deselect');
    End;
End.
{..............................................................................}

{..............................................................................}

