'..............................................................................
' Summary Generate and plot a sine wave on a form
' Copyright (c) 2003 by Altium Limited
'..............................................................................

Sub bDrawClick(Sender)
    Dim i
    Dim x1
    Dim y,y1,y3,y5

    Dim Step
    Dim NumberOfCycles
    Dim CycleLength
    Dim Shape
    Dim StartX
    Dim FinalX
    Dim pi

    pi     = 3.1415926
    StartX = 10

    ' width of dialog
    FinalX         = Width - 50
    NumberOfCycles = 1

    ' length of one wave cycle
    CycleLength    = Round((FinalX-StartX) / NumberOfCycles)

    ' distance between data points is fixed at 5
    Step           = 5

    x1 = StartX
    i = 1
    While x1 <= FinalX
       y1 = 50 *(sin(NumberofCycles * (2 * pi *  (x1 / CycleLength))) + 1)
       y3 = (50 / 3) *(sin(NumberofCycles * (2 * pi *  (3 * x1 / CycleLength))) + 1)
       y5 = (50 / 5) *(sin(NumberofCycles * (2 * pi *  (5 * x1 / CycleLength))) + 1)

       y = y1 + y3 + y5

       Shape = CreateShape(0)
       Shape.Left = x1
       Shape.Top  = y
       Shape.Show

       x1 = x1 + Step
       i  = i + 1
    WEnd

End Sub
'..............................................................................

'..............................................................................
Function CreateShape(Dummy)
    Dim Result
    Result = TShape.Create(SineWaveForm)

    Result.Brush.Color = 0
    Result.Brush.style = bsSolid

    Result.Pen.Color = 65000
    Result.pen.Mode  = pmCopy
    Result.pen.Style = psSolid
    Result.Pen.Width = 2

    Result.Shape   = stCircle
    Result.Visible = true
    Result.Width   = 10
    Result.Height  = 10
    Result.Parent  = SineWaveForm
    CreateShape = Result
End Function
'..............................................................................

'..............................................................................
Sub bCloseClick(Sender)
    Close
End Sub
'..............................................................................

'..............................................................................
Sub DrawSine
    SineWaveform.showmodal
End Sub
'..............................................................................

'..............................................................................
