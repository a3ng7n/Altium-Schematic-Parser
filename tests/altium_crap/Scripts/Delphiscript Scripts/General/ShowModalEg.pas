{..............................................................................}
{ Summary Demo the use of ShowModal property of a script form                  }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}
unit FormShowModal;

interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

Type
  TFormShowModalEg = class(TForm)
    bOk: TButton;
    bCancel: TButton;
    procedure bCancelClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Var
  FormShowModalEg: TFormShowModalEg;

Implementation

{$R *.DFM}
{..............................................................................}

{..............................................................................}
Procedure TFormShowModalEg.bCancelClick(Sender: TObject);
Begin
    ModalResult   := mrCancel;
    Sender.Cancel := True;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormShowModalEg.bOkClick(Sender: TObject);
Begin
    ModalResult    := mrOk;
    Sender.Default := True;
End;
{..............................................................................}

{..............................................................................}
Procedure RunShowModalExample;
Begin
    // Form's Visible property must be false for ShowModal to work properly.
    //   FormStyle := fsNormal; FormKind := fkNone;  
    FormShowModalEg.ShowModal;

    // FormShowModalEg dialog disappears before the ShowMessage appears.
    If FormShowModalEg.ModalResult = mrOk     Then ShowMessage('mrOk');
    If FormShowModalEg.ModalResult = mrCancel Then ShowMessage('mrCancel');
End;
{..............................................................................}

{..............................................................................}
End.
