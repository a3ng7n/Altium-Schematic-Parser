{..............................................................................}
{ Summary A Tic Tac Toe game...                                                }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

Type

  TfrMain = class(TForm)
    lblCell0      : TLabel;
    lblCell1      : TLabel;
    lblCell2      : TLabel;
    lblCell3      : TLabel;
    lblCell4      : TLabel;
    lblCell5      : TLabel;
    lblCell6      : TLabel;
    lblCell7      : TLabel;
    lblCell8      : TLabel;
    gbScoreBoard  : TGroupBox;
    rgPlayFirst   : TRadioGroup;
    lblX          : TLabel;
    lblMinus      : TLabel;
    lblO          : TLabel;
    lblXScore     : TLabel;
    lblColon      : TLabel;
    lblOScore     : TLabel;
    btnNewGame    : TButton;
    btnResetScore : TButton;
    
    procedure FormCreate(Sender: TObject);
    procedure lblCell0Click(Sender: TObject);
    procedure btnNewGameClick(Sender: TObject);
    procedure btnResetScoreClick(Sender: TObject);
    Procedure btnExitClick(Sender: TObject);
  private
    procedure InitPlayGround(Dummy   : Integer);
    function  GamePlay      (xo_Move : Integer) : integer;
    function  CheckWin      (iPos    : TXOPosArray) : integer;
  End;
 
{..............................................................................}

{..............................................................................}
Var
  frMain    : TfrMain;
  
  iXPos     : array [1..3, 1..3] of Integer;
  iOPos     : array [1..3, 1..3] of Integer;
  
  sPlaySign : String;
  bGameOver : Boolean;
  iMove     : Integer;
  iXScore   : Integer;
  iOScore   : Integer;
  
{..............................................................................}

{..............................................................................}
Procedure TfrMain.InitPlayGround(Dummy : Integer);
Var
    i, j, k: integer;
Begin
    for i := 1 to 3 do
    begin
        for j := 1 To 3 do
        begin
            k := (i - 1) * 3 + j - 1; // 0 .. 8
            TLabel(FindComponent('lblCell' + IntToStr(k))).Caption := '';
            iXPos[i, j] := 0;
           iOPos[i, j] := 0;
        end;
    end;

    if rgPlayFirst.ItemIndex = 0 then sPlaySign := 'X';
    if rgPlayFirst.ItemIndex = 1 then sPlaySign := 'O';

    bGameOver := False;
    iMove     := 0;
End;
{..............................................................................}

{..............................................................................}
Procedure TfrMain.FormCreate(Sender: TObject);
Begin
    iXScore := 0;
    iOScore := 0;

    InitPlayGround(0);
End;
{..............................................................................}

{..............................................................................}
Function TfrMain.CheckWin(iPos : TXOPosArray) : Integer;
Var
    iScore : Integer;
    i      : Integer;
    j      : Integer;
Begin
    Result := -1;

    //in rows?
   iScore := 0;
   For i := 1 to 3 do
   Begin
      iScore := 0;
      Inc(Result);
      
      For j := 1 To 3 Do 
          Inc(iScore, iPos[i,j]);
      
      If iScore = 3 Then Exit
   End;//for i
    
    
   //top-left bottom-right diagonal?
   iScore := 0;
   Inc(Result);
   For i := 1 to 3 Do 
       Inc(iScore, iPos[i,i]);
       
   If iScore = 3 Then Exit;
    
    
   //top-right bottom-left diagonal?
   iScore := 0;
   Inc(Result);
   
   For i := 1 to 3 Do 
       Inc(iScore, iPos[i,4-i]);
       
   If iScore = 3 Then Exit;
    
   //columns?
   For i := 1 to 3 Do
   Begin
      iScore := 0;
      Inc(Result);
      For j := 1 to 3 Do 
          Inc(iScore, iPos[j,i]);
          
      If iScore = 3 Then Exit;
   End;//for i
    
   Result := -1;
End;
{..............................................................................}

{..............................................................................}
Function TfrMain.GamePlay(xo_Move : Integer):integer;
Var
    x    : integer;
    y    : integer;
    iWin : integer;
Begin
    Result := -1;

    Inc(iMove);
    x := (xo_Move Div 3) + 1;
    y := (xo_Move Mod 3) + 1;

    If sPlaySign = 'O' Then
    Begin
      iOPos[x,y] := 1;
      iWin := CheckWin(iOPos);
    End
    Else
    Begin
       iXPos[x,y] := 1;
       iWin := CheckWin(iXPos);
    End;

    TLabel(FindComponent('lblCell' + IntToStr(xo_Move))).Caption := sPlaySign;

    Result := iWin;

    If iWin >= 0 then
    Begin
        bGameOver := True;
        //mark victory

        If sPlaySign = 'X' then
        Begin
           iXScore := iXScore + 1;
           lblXScore.Caption := IntToStr(iXScore);
        End
        Else
        Begin
           iOScore := iOScore + 1;
           lblOScore.Caption := IntToStr(iOScore);
        End;

        ShowMessage(sPlaySign + ' - Wins!');
    End;

    If (iMove = 9) And (bGameOver = False) Then
    Begin
        ShowMessage('It''s a Draw!');
        bGameOver := True
    End;

    If sPlaySign = 'O' Then
       sPlaySign := 'X'
    Else
       sPlaySign := 'O';
End;
{..............................................................................}

{..............................................................................}
Procedure TfrMain.lblCell0Click(Sender: TObject);
Var
    iWin        : integer;
    CellIndex   : Integer;
    l           : integer;
    controlname : string;
    newname     : string;
Begin
    If bGameOver = True Then Exit;
    
    If TLabel(Sender).Caption <> '' then
    Begin
        ShowMessage('Cell ocupied!');
        Exit;
    End;

   ControlName := TLabel(Sender).Name;
   L           := IntToStr(length(ControlName));
   NewName     := ControlName[L];;
   CellIndex   := StrToInt(NewName);
   iWin        := GamePlay(CellIndex);
End;
{..............................................................................}

{..............................................................................}
Procedure TfrMain.btnNewGameClick(Sender: TObject);
Begin
    If bGameOver = False then
    Begin
        If MessageDlg('End the current game?', mtConfirmation, mbOKCancel,0) = mrCancel then Exit;
    End;

    InitPlayGround(0);
End;
{..............................................................................}

{..............................................................................}
Procedure TfrMain.btnResetScoreClick(Sender: TObject);
Begin
    If MessageDlg('Reset the scores?', mtConfirmation, mbOKCancel,0) = mrCancel then Exit;

    iXScore := 0;
    iOScore := 0;
    lblXScore.Caption := IntToStr(iXScore);
    lblOScore.Caption := IntToStr(iOScore);
End;
{..............................................................................}

{..............................................................................}
Procedure TfrMain.btnExitClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure RunTicTacGame;
Begin
    frMain.Showmodal;
End;
{..............................................................................}

{..............................................................................}
End.
