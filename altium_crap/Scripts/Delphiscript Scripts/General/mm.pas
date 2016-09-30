{..............................................................................}
{ Summary Demo the use of TMediaPlayer component                               }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

Interface

Type
  TMMDemo = class(TForm)
    MediaPlayer1 : TMediaPlayer;
    Animate1     : TAnimate;
    MediaPlayer1 : TMediaPlayer;
    Animate1     : TAnimate;
    procedure Animate1Stop(Sender: TObject);
    procedure Animate1Start(Sender: TObject);
  End;

Var
  MMDemo: TMMDemo;

Implementation

{$R *.DFM}
{..............................................................................}

{..............................................................................}
Procedure RunMediaPlayer;
Begin
    MMDemo.ShowModal;
End;
{..............................................................................}

{..............................................................................}
procedure TMMDemo.MMDemoCreate(Sender: TObject);
begin
    MediaPlayer1.FileName := SpecialFolder_DesignExamples + 'Scripts\Delphiscript Scripts\General\Protel.avi';
    MediaPlayer1.AutoOpen := True;
    MediaPlayer1.Open;
    MediaPlayer1.AutoEnable := True;
end;


End.
