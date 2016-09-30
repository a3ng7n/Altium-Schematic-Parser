{..............................................................................}
{ Summary Demo the use of Client's RunCommonDialog process                     } 
{ Copyright (c) 2003 by Altium Limited                                         }   
{..............................................................................}

{..............................................................................}

{..............................................................................}
Procedure RunADialogProcess;
Var
    S     : String;
    Value : Integer;
Begin
    // available parameters for Dialog: Color or FileOpenSave Names
    ResetParameters;
    AddStringParameter('Dialog','Color'); // color dialog
    AddStringParameter('Color', '0');     // black color
    RunProcess('Client:RunCommonDialog');

    //Result value obtained from the RunCommonDialog's Ok or Cancel buttons.
    GetStringParameter('Result',S);
    If (S = 'True') Then
    Begin
        GetStringParameter('Color',S);
        ShowInfo('New color is ' + S);
    End;
End;
{..............................................................................}

{..............................................................................}