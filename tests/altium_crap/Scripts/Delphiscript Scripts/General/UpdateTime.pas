{..............................................................................}
{ Summary Demo the use of TTimer component and the Time function               }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

Interface

Type
  TTimerForm = class(TForm)
    Timer1 : TTimer;
    Label1 : TLabel;
    procedure Timer1Timer(Sender: TObject);
  End;

Var
  TimerForm: TTimerForm;

Implementation

{$R *.dfm}
{..............................................................................}

{..............................................................................}
Procedure TTimerForm.Timer1Timer(Sender: TObject);
Var
  DateTime : TDateTime;
  str      : string;
Begin
    // Store the current date and time
    DateTime := Time;

    // Convert the time into a string
    str := TimeToStr(DateTime);

    // Display the time on the form's caption
    Label1.Caption := str;
End;
{..............................................................................}

{..............................................................................}
Procedure RunTimerExample;
Begin
    TimerForm.showmodal;
End;
{..............................................................................}

{..............................................................................}
End.
