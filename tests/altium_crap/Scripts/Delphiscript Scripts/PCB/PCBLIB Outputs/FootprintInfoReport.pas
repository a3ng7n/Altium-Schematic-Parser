// Footprint Report
Procedure ReportFootprintInfo;
Var
    CurrentLib        : IPCB_Library;
    FootprintIterator : IPCB_LibraryIterator;
    Footprint         : IPCB_LibComponent;
    FootprintList     : TStringList;
    ReportDocument    : IServerDocument;
    Filename          : TString;
    S                 : TString;
    I                 : Integer;
Begin
    CurrentLib := PCBServer.GetCurrentPCBLibrary;
    If CurrentLib = Nil Then Exit;

    Filename := ExtractFilePath(CurrentLib.Board.FileName) + 'PCBLib_Report.csv';
    S := '';
    FootprintList := TStringList.Create;

    FootprintIterator := CurrentLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;
    Try
        Footprint := FootprintIterator.FirstPCBObject;
        While Footprint <> Nil Do
        Begin
            // Determine which units are in use. at the mo it is the other way around!!!
            If CurrentLib.Board.DisplayUnit = eMetric Then
                S := footprint.name + ',' + FloatToStr(CoordToMils(Footprint.Height)) + ',' + Footprint.Description
            Else
                S := footprint.name + ',' + FloatToStr(CoordToMMs(Footprint.Height)) + ',' + Footprint.Description;

            FootprintList.Add(S);
            Footprint := FootprintIterator.NextPCBObject;
        End;
     Finally
        CurrentLib.LibraryIterator_Destroy(FootprintIterator);
        FootprintList.SaveToFile(FileName);
        FootprintList.Free;
    End;

    //Display and save report.
    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;



