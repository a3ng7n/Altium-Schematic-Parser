function ViaCreation() {
var Board; //IPCB_Board;
var Via;   //IPCB_Via;

    Board = PCBServer.GetCurrentPCBBoard;
    if (Board != Null)
    {
        /* Create a Via object */
        Via           = PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default);
        Via.X         = MilsToCoord(7500);
        Via.Y         = MilsToCoord(7500);
        Via.Size      = MilsToCoord(50);
        Via.HoleSize  = MilsToCoord(20);
        Via.LowLayer  = eTopLayer;
        Via.HighLayer = eBottomLayer;

        /* Put this via in the Board object*/
        Board.AddPCBObject(Via);
    }
}
