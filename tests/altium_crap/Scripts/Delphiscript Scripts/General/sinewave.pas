{..............................................................................}
{ Summary Generate and plot a sine wave on a form (of a dialog)                }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Interface

Type
  TSineWaveForm = class(TForm)
    bDraw  : TButton;
    bClose : TButton;

    Procedure bDrawClick(Sender: TObject);
    Procedure bCloseClick(Sender: TObject);
    Procedure Form1Create(Sender: TObject);
  Private
    Function CreateShape(Dummy : Integer) : TShape;
  Public
    Shape       : TShape;
  End;

Var
  SineWaveForm       : TSineWaveForm;

Implementation

{$R *.DFM}
{..............................................................................}

{..............................................................................}
Procedure TSineWaveform.bDrawClick(Sender: TObject);
Var
    x1             : integer;
    y,y1,y3,y5     : integer;

    Step           : integer;
    NumberOfCycles : Integer;
    CycleLength    : Integer;
    Shape          : TShape;
    StartX         : Integer;
    FinalX         : Integer;
Begin
    // starting point for the sine wave
    StartX         := 10;

    // final point for the sine wave and the width of dialog
    // taken into consideration
    FinalX         := Self.Width - 30;
    NumberOfCycles := 1;

    // length of one wave cycle
    CycleLength    := Round((FinalX-StartX) / NumberOfCycles);

    // distance between data points is 5 pixels long
    Step           := 5;

    x1 := StartX;
    While x1 <= FinalX do
    Begin
       // three sine waves (with increasing periods) generated and added
       // together as a rudimentary square wave
       y1 := round(50 *(sin(NumberofCycles * (2 * pi *  (x1 / CycleLength))) + 1));
       y3 := round((50 / 3) *(sin(NumberofCycles * (2 * pi *  (3 * x1 / CycleLength))) + 1));
       y5 := round((50 / 5) *(sin(NumberofCycles * (2 * pi *  (5 * x1 / CycleLength))) + 1));

       y := y1 + y3 + y5;

       // create the data point and plot it on the dialog.
       Shape := CreateShape(0);
       Shape.Left := x1;
       Shape.Top  := y;
       Shape.Show;

       // go to the next data point of the sine wave
       x1 := x1 + Step;
    End;

    // refresh and draw the sine wave points.
   Self.Invalidate;
End;
{..............................................................................}

{..............................................................................}
Function TSineWaveform.CreateShape(Dummy : Integer = 0) : TShape;
Begin
    Result := TShape.Create(Nil);

    Result.Brush.Color := 0;
    Result.Brush.style := bsSolid;

    Result.Pen.Color := 65000;
    Result.pen.Mode  := pmCopy;
    Result.pen.Style := psSolid;
    Result.Pen.Width := 2;

    Result.Shape   := stCircle;
    Result.Visible := true;
    Result.Width   := 10;
    Result.Height  := 10;
    Result.Parent  := self;
End;
{..............................................................................}

{..............................................................................}
Procedure TSineWaveform.bCloseClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
procedure DrawSine;
Begin
    SineWaveform.Showmodal;
End;
{..............................................................................}

{..............................................................................}
End.
