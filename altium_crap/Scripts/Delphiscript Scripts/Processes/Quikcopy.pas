{..............................................................................}
{ Summary Quick Copy - Select objects, then Copy and Paste.                    }
{ Copyright (c) 2003 by Altium Limited                                         }  
{..............................................................................}

{..............................................................................}
Begin
    ResetParameters;
    
    AddStringParameter('SelectionReference','False');
    
    RunProcess('Sch:SetupPreferences');
    ResetParameters;
    AddStringParameter('Action','All');
    RunProcess('Sch:Deselect');
    ResetParameters;
    RunProcess('Sch:ToggleSelection');
    RunProcess('Sch:Copy');
    AddStringParameter('Action','All');
    RunProcess('Sch:Deselect');
    ResetParameters;
    RunProcess('Sch:Paste');
    AddStringParameter('Action','All');
    RunProcess('Sch:Deselect');
End.
{..............................................................................}

{..............................................................................}
                                                                                       
