{..............................................................................}
{ Summary Open a text file, convert it and save it as a new text file.         }
{ Copyright (c) 2003 by Altium Limited                                         } 
{..............................................................................}
Interface

Type
  TTextForm = class(TForm)
    rgConvert      : TRadioGroup;
    bOpen          : TButton;
    bSave          : TButton;
    eOriginalFile  : TEdit;
    eConvertedFile : TEdit;
    OpenDialog1    : TOpenDialog;
    SaveDialog1    : TSaveDialog;
    bConvert       : TButton;
    bCancel        : TButton;
    Bevel1         : TBevel;
    procedure Button1Click(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bConvertClick(Sender: TObject);
  public
    Procedure ConvertToUpperCase(Dummy : Integer);
    Procedure ConvertSymbols    (Dummy : Integer);
  End;

Var
  TextForm: TTextForm;

Implementation

{$R *.DFM}
{..............................................................................}

{..............................................................................}
Procedure TTextForm.ConvertToUpperCase(Dummy : Integer);
var
    InputFile  : TextFile;
    OutputFile : TextFile;
    I          : Integer;
    Line       : String;
Begin
    AssignFile(OutputFile,eConvertedFile.Text);
    Rewrite(OutputFile);

    AssignFile(InputFIle,eOriginalFIle.Text);
    Reset(InputFile);

    Try
        while not EOF(InputFile) do
        begin
            readln(InputFile,Line);
            If Not VarIsNull(line) Then
            Begin
                For I := 1 to Length(Line) Do
                Begin
                    Line[I] := UpperCase(Line[I]);
                End;
                Writeln(Outputfile, Line);
            End
            Else
              Writeln(outputfile, '');
        end;

    Finally
        CloseFile(InputFile);
        CloseFile(OutputFile);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TTextForm.ConvertSymbols(Dummy : Integer);
Var
    InputFile  : TextFile;
    OutputFile : TextFile;
    I          : Integer;
    line       : string;

Begin
    AssignFile(InputFile,eOriginalFile.Text);
    Reset(InputFile);

    AssignFile(OutputFile,eConvertedFile.Text);
    Rewrite(OutputFile);
    Try
        While not EOF(InputFile) do
        begin
            Readln(InputFile, line);

            If Not VarIsNull(line) Then
            Begin
                For I := 1 to Length(Line) Do
                Begin
                    If Line[I] > Chr(127) Then Line[I] := '#';
                End;
                Writeln(outputfile, line);
            End
            Else
              Writeln(outputfile, '');
        End;
    Finally
        CloseFile(InputFile);
        CloseFile(OutputFile);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TTextForm.bOpenClick(Sender: TObject);
Begin
    If OpenDialog1.Execute Then
        eOriginalFile.Text := OpenDialog1.Filename;
End;
{..............................................................................}

{..............................................................................}
Procedure TTextForm.bSaveClick(Sender: TObject);
Begin
    If SaveDialog1.Execute Then
        eConvertedFile.Text := SaveDialog1.Filename;
End;
{..............................................................................}

{..............................................................................}
Procedure TTextForm.bConvertClick(Sender: TObject);
Var
    Document : IServerDocument;
Begin
    If eOriginalFile.Text  = '' Then Exit;
    If eConvertedFile.Text = '' Then Exit;

    Case rgConvert.ItemIndex of
        0 : ConvertToUpperCase(0);
        1 : ConvertSymbols(0);
    End;

    // show the new converted text file.
    Document := Client.OpenDocument('Text',SaveDialog1.FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);

    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure TTextForm.bCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure RunTextConverter;
Begin
    TextForm.ShowModal;
End;
{..............................................................................}

{..............................................................................}

End.
