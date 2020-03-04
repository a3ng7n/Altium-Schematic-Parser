Sub ViaCreation
    Dim  Board
    Dim  Via 

    Set Board = PCBServer.GetCurrentPCBBoard
    If Board is Nothing Then Exit Sub

    ' Create a Via object
    Via           = PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default)
    Via.X         = MilsToCoord(7500)
    Via.Y         = MilsToCoord(7500)
    Via.Size      = MilsToCoord(50)
    Via.HoleSize  = MilsToCoord(20)
    Via.LowLayer  = eTopLayer
    Via.HighLayer = eBottomLayer

    ' Put this via in the Board object
    Board.AddPCBObject(Via)
End Sub
