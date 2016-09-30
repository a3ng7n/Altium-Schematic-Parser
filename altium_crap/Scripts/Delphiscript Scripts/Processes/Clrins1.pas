{..............................................................................}
{ Summary Clear Inside - Delete objects within an area defined by user.        }
{ Copyright (c) 2003 by Altium Limited                                         }  
{..............................................................................}

{..............................................................................}
Begin
    ResetParameters;
    AddStringParameter('Action','All');
    RunProcess('Sch:Deselect');
    ResetParameters;
    AddStringParameter('Action','InsideArea');
    RunProcess('Sch:Select');
    ResetParameters;
    RunProcess('Sch:Clear');
End.
{..............................................................................}
                                                                                       
