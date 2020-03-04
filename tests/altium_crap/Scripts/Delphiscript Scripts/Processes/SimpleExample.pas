{..............................................................................}
{ Summary SimpleExample - Demonstrate the use of AddIntegerParameter           }
{                          and GetIntegerParameter                             }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    Value : Integer;
Begin
    ResetParameters;
    AddIntegerParameter('A', 1);
    GetIntegerParameter('A', Value);
    If Value = 1 Then
       ShowInfo('Ok')
    Else
       ShowError('Failed');
End.
{..............................................................................}

{..............................................................................}
