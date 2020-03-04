{..............................................................................}
{ Summary Demo the use of Delphi Script graphics to build a mandelbrot set     }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}
unit MandelBrot;

Interface

Type
  TMandelBrotForm1 = class(TForm)
    bDraw: TButton;
    bQuit: TButton;
    procedure bDrawClick(Sender: TObject);
    procedure bQuitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Var
  MandelBrotForm1: TMandelBrotForm1;

Implementation

{$R *.DFM}
{..............................................................................}

{..............................................................................}
Procedure DrawMandelbrot(ACanvas: TCanvas; X, Y, au, bu: Double; X2, Y2: Integer); 
Var 
  c1, c2, z1, z2, tmp : Double; 
  i, j, Count         : Integer; 
Begin 
    c2 := bu; 
    For i := 10 to X2 do  
    Begin 
        c1 := au;
        For j := 0 to Y2 do  
        Begin 
            z1 := 0; 
            z2 := 0; 
            Count := 0;
           {
           count is deep of iteration of the mandelbrot set 
           If |z| >=2 then z is not a member of a mandelset
           } 

           While (((z1 * z1 + z2 * z2 < 4) and (Count <= 90))) do  
           Begin
               tmp := z1;
               z1 := z1 * z1 - z2 * z2 + c1;
               z2 := 2 * tmp * z2 + c2;
               Inc(Count);
           End;

           //The color-palette depends on TColor(n*count mod t)
           ACanvas.Pixels[j + 10, i] := (16 * Count mod 255);
           c1 := c1 + X;
           Application.processmessages;
        End;
        c2 := c2 + Y;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure DrawAMandelBrot;
Begin
    MandelBrotForm1.ShowModal;
End;
{..............................................................................}

{..............................................................................}
Procedure TMandelBrotForm1.bDrawClick(Sender: TObject);
Var
  Right, Left, Bottom, Top : Integer;
  au, ao                   : Integer;
  dX, dY, bo, bu           : Double;
Begin
    // Initialize Mandelbrot
    Left   := 0;
    Right  := 200;
    Top    := 0;
    Bottom := 205;
    ao     := 1;
    au     := -2;
    bo     := 1.5;
    bu     := -1.5;

    // Direct scaling cause of speed
    dX := (ao - au) / (Right - Left);
    dY := (bo - bu) / (Bottom - Top);
    DrawMandelbrot(Self.Canvas, dX, dY, au, bu, Right, Bottom);
End;
{..............................................................................}

{..............................................................................}
Procedure TMandelBrotForm1.bQuitClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
End.
