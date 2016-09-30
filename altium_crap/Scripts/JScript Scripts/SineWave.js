/*..............................................................................*/
/* Summary Generate and plot a sine wave on a form                              */
/* Copyright (c) 2003 by Altium Limited                                         */
/*..............................................................................*/

/*..............................................................................*/
function bDrawClick(Sender)
{
    var i;
    var x1;
    var y,y1,y3,y5;

    var Step;
    var NumberOfCycles;
    var CycleLength;
    var Shape;
    var StartX;
    var FinalX;
    var pi = 3.1415926;


    StartX         = 10;

    // width of dialog
    FinalX         = Width - 50;
    NumberOfCycles = 1;

    // length of one wave cycle
    CycleLength    = (FinalX-StartX) / NumberOfCycles;

    // distance between data points is fixed at 5
    Step           = 5;

    x1 = StartX;
    i = 1;
    while(x1 <= FinalX)
    {
       y1 = 50 *(Math.sin(NumberOfCycles * (2 * pi *  (x1 / CycleLength))) + 1);
       y3 = (50 / 3) *(Math.sin(NumberOfCycles * (2 * pi *  (3 * x1 / CycleLength))) + 1);
       y5 = (50 / 5) *(Math.sin(NumberOfCycles * (2 * pi *  (5 * x1 / CycleLength))) + 1);

       y = y1 + y3 + y5;

       Shape = CreateShape(0);
       Shape.Left = x1;
       Shape.Top  = y;
       Shape.Show();

       x1 = x1 + Step;
       i  = i + 1;
    }
}
//..............................................................................

//..............................................................................
function CreateShape(Dummy)
{
    var Result = TShape.Create(SineWaveForm);

    Result.Brush.Color = 0;
    Result.Brush.style = bsSolid;

    Result.Pen.Color = 65000;
    Result.pen.Mode  = pmCopy;
    Result.pen.Style = psSolid;
    Result.Pen.Width = 2;

    Result.Shape   = stCircle;
    Result.Visible = true;
    Result.Width   = 10;
    Result.Height  = 10;
    Result.Parent  = SineWaveForm;

    return Result;
}
//..............................................................................

//..............................................................................
function bCloseClick(Sender)
{
    Close();
}
//..............................................................................

//..............................................................................
function DrawSine()
{
    ShowModal();
}
//..............................................................................

//..............................................................................
