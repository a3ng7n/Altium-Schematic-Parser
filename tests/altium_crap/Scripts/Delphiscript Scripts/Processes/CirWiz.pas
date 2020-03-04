{..............................................................................}
{ Summary Generate a simple filter circuit and place on sch sheet              }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

Interface

Type
  TFilterForm = class(TForm)
    Label1         : TLabel;
    txtCapacitance : TEdit;
    Label2         : TLabel;
    txtResistance  : TEdit;
    btnOk          : TButton;
    btnCancel      : TButton;
    procedure btnOkClick(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  End;

Var
   FilterForm : TFilterForm;

Implementation
{$R *.DFM}

{..............................................................................}
Const
    DEFAULT_RES = '200 ohm';
    DEFAULT_CAP = '100 uf';
{..............................................................................}

{..............................................................................}
Procedure RunCircuitWizard;
Begin
    FilterForm.Showmodal;
End;
{..............................................................................}

{..............................................................................}
Function GetClickPosition(Var X, Y : Integer) : Integer;
Begin
    ResetParameters;
    RunProcess         ('Sch:AskForXYLocation');
    GetIntegerParameter('LocationX', X);
    GetIntegerParameter('LocationY', Y);
    GetIntegerParameter('Result'   , Result);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceFilter(X, Y : Integer; Const Cap, Res : String);
Begin
   ResetParameters;
   AddStringParameter ('Library','Miscellaneous Devices.IntLib');
   AddStringParameter ('LibReference', 'Res1' );
   AddStringParameter ('ModelType','SIM' );
   AddStringParameter ('ModelParameterName0', 'Value' );
   AddStringParameter ('ModelParameterValue0', Res );
   AddStringParameter ('Designator', 'R1'     );
   AddIntegerParameter('Location.X', X + 40   );
   AddIntegerParameter('Location.Y', Y + 10   );
   AddIntegerParameter('Orientation', 0       );
   RunProcess         ('IntegratedLibrary:PlaceLibraryComponent');

   ResetParameters;
   AddStringParameter ('Library','Miscellaneous Devices.IntLib');
   AddStringParameter ('LibReference', 'Cap'  );
   AddStringParameter ('ModelType','SIM' );
   AddStringParameter ('ModelParameterName0', 'Value' );
   AddStringParameter ('ModelParameterValue0', Cap );
   AddStringParameter ('Designator', 'C1'     );
   AddIntegerParameter('Location.X', X + 110  );
   AddIntegerParameter('Location.Y', Y - 40   );
   AddIntegerParameter('Orientation', 1       );
   RunProcess         ('IntegratedLibrary:PlaceLibraryComponent');

   ResetParameters;
   AddColorParameter  ('Color', 255,0,0       );
   AddIntegerParameter('Location1.X', X       );
   AddIntegerParameter('Location1.Y', Y       );
   AddIntegerParameter('Location2.X', X + 30  );
   AddIntegerParameter('Location2.Y', Y       );
   RunProcess         ('Sch:PlaceWire'        );

   ResetParameters;
   AddColorParameter  ('Color', 255,0,0       );
   AddIntegerParameter('Location1.X', X + 70  );
   AddIntegerParameter('Location1.Y', Y       );
   AddIntegerParameter('Location2.X', X + 150 );
   AddIntegerParameter('Location2.Y', Y       );
   RunProcess         ('Sch:PlaceWire'        );

   ResetParameters;
   AddColorParameter  ('Color', 255,0,0       );
   AddIntegerParameter('Location1.X', X + 120 );
   AddIntegerParameter('Location1.Y', Y       );
   AddIntegerParameter('Location2.X', X + 120 );
   AddIntegerParameter('Location2.Y', Y - 20  );
   RunProcess         ('Sch:PlaceWire'        );

   ResetParameters;
   AddIntegerParameter('Location.X' , X + 120 );
   AddIntegerParameter('Location.Y' , Y - 50  );
   AddIntegerParameter('Orientation', 3       );
   AddStringParameter ('S'          ,'GND'    );
   AddIntegerParameter('Style', 4             );
   RunProcess         ('Sch:PlacePowerPort'   );
End;
{..............................................................................}

{..............................................................................}
Procedure TFilterForm.btnOkClick(Sender: TObject);
Var
    X, Y : Integer;
Begin
    If GetClickPosition(X, Y) <> 0 Then
          PlaceFilter(X, Y, txtCapacitance.Text, txtResistance.Text);
    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure TFilterForm.Form1Create(Sender: TObject);
Begin
    txtCapacitance.Text := DEFAULT_CAP;
    txtResistance.Text  := DEFAULT_RES;
End;
{..............................................................................}

{..............................................................................}
Procedure TFilterForm.btnCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
End.
