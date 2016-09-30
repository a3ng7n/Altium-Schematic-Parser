{..............................................................................}
{ Summary PCB Hole Size Editor version 1.0                                     }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Unit EHSForm;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

Type
  TEditHoleSizeForm = class(TForm)
    GroupBox1     : TGroupBox;
    eEditHoleSize : TEdit;
    bOk           : TButton;
    bCancel       : TButton;
    procedure bOkClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  End;
{..............................................................................}

{..............................................................................}
Var
  EditHoleSizeForm : TEditHoleSizeForm;
{..............................................................................}

{..............................................................................}
Implementation

{$R *.DFM}
{..............................................................................}

{..............................................................................}
Procedure TEditHoleSizeForm.bOkClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure TEditHoleSizeForm.bCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
End.
