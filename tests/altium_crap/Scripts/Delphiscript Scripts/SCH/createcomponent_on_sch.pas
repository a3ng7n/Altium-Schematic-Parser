{..............................................................................}
{ Summary Demo how to place a component                                        }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure PlaceAPartProgrammatically;
Begin
    If SchServer = Nil Then Exit;
    If SchServer.GetCurrentSchDocument = Nil Then Exit;

    If IntegratedLibraryManager = Nil Then Exit;

    // Integrated Library object model is used to place a
    // component from the library onto the scehamtic sheet.
    IntegratedLibraryManager.PlaceLibraryComponent(
        'Res2',
        'Miscellaneous Devices.IntLib',
        'ModelType=SIM|ModelParameterName0=Value|' +
        'ModelParameterValue0=1K|Orientation=1|Location.X=10000000|Location.Y=20000000');

     // Refresh screen
    SchServer.GetCurrentSchDocument.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
