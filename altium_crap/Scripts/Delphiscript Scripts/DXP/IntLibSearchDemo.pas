{..............................................................................}
{ Summary Demo the use of several search methods in Integrated Library Manager }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure Search;
Var
    IntMan         : IIntegratedLibraryManager;
    LibPath        : String;
    InstLibCount   : Integer;
    I              : Integer;
    InstLibs       : String;
    FoundLocation  : String;
    AFootprintName : String;
    InIntLib       : Boolean;

    InstPath       : WideString;
    ModelType      : String;
    S              : WideString;
Begin
    IntMan := IntegratedLibraryManager;
    If IntMan = Nil Then Exit;

    //Integrated Library Manager is dependent on the library files installed in the Available Libraries dialog.
    // Install a standard PCBLIB file
    IntMan.InstallLibrary('C:\Program Files\Altium Designer 6\Examples\Reference Designs\4 Port Serial Interface\Libraries\4 Port Serial Interface.PcbLib');

    // Install a standard INTLIB file
    IntMan.InstallLibrary('C:\Program Files\Altium Designer 6\Library\Xilinx\Xilinx Spartan-3E.IntLib');

     // Look for a footprint in 4 Port Serial Interface.PCBLIB
    ModelType      := 'PCBLIB';
    InIntLib       := False;
    AFootprintName := 'DIP14';
    IntMan.FindDatafileInStandardLibs (AFootprintName, 'PCBLIB', '', InIntLib, FoundLocation);
    ShowMessage(FoundLocation);

    //Look for a footprint in a Xilinx Spartan-3E.IntLib
    ModelType      := 'PCBLIB';
    AFootprintName := 'TQ144';
    InIntLib       := True;
    IntMan.FindDatafileInStandardLibs (AFootprintName, 'PCBLIB', '', InIntLib, FoundLocation);
    ShowMessage(FoundLocation);

    //Look for a 3d model in Xilinx Spartan-3E.IntLib
    ModelType      := 'PCB3DLIB';
    AFootprintName := 'XC3S100E-TQ144';
    InIntLib       := True;
    IntMan.FindDatafileInStandardLibs (AFootprintName, 'PCB3DLIB', '', InIntLib, FoundLocation);
    ShowMessage(FoundLocation);

     // Un-install files
     IntMan.UnInstallLibrary('C:\Program Files\Altium Designer 6\Examples\Reference Designs\4 Port Serial Interface\Libraries\4 Port Serial Interface.PcbLib');
     IntMan.UnInstallLibrary('C:\Program Files\Altium Designer 6\Library\Xilinx\Xilinx Spartan-3E.IntLib');
End;
{..............................................................................}

{..............................................................................}

