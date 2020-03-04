{..............................................................................}
{ Summary A simple hello world - an introduction to DelphiScript language.     }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure HelloWorld;
Begin
    ShowMessage('Hello world!');
End;
{..............................................................................}

{..............................................................................}
Procedure ShowAParametricMessage(S : String);
Var
    DefaultMessage;
Begin
    DefaultMessage := 'Hello World!';

    If S = '' Then ShowMessage(DefaultMessage)
              Else ShowMessage(S);
End;
{..............................................................................}

{..............................................................................}


