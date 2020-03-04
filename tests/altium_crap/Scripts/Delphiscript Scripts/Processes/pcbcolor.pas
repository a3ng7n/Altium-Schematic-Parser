{..............................................................................}
{ Summary PCB Color Scheme - Demo changing PCB Colors for a PCB document       }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure SetSchemeDefaults;
Begin  
  ResetParameters;
  AddStringParameter ('TopSignalColor'         , '255'       );
  AddStringParameter ('Mid1Color'              , '128'       ); 
  AddStringParameter ('Mid2Color'              , '32768'     );
  AddStringParameter ('Mid3Color'              , '65280'     );
  AddStringParameter ('Mid4Color'              , '8388608'   );
  AddStringParameter ('Mid5Color'              , '16776960'  );
  AddStringParameter ('Mid6Color'              , '8388736'   );
  AddStringParameter ('Mid7Color'              , '16711935'  );
  AddStringParameter ('Mid8Color'              , '32896'     );
  AddStringParameter ('Mid9Color'              , '65535'     );
  AddStringParameter ('Mid10Color'             , '8421504'   );
  AddStringParameter ('Mid11Color'             , '32768'     );
  AddStringParameter ('Mid12Color'             , '8388736'   );
  AddStringParameter ('Mid13Color'             , '8421376'   );
  AddStringParameter ('Mid14Color'             , '12632256'  );
  AddStringParameter ('Mid15Color'             , '128'       ); 
  AddStringParameter ('Mid16Color'             , '32768'     );
  AddStringParameter ('Mid17Color'             , '65280'     );
  AddStringParameter ('Mid18Color'             , '8388608'   );
  AddStringParameter ('Mid19Color'             , '16776960'  );
  AddStringParameter ('Mid20Color'             , '8388736'   );
  AddStringParameter ('Mid21Color'             , '16711935'  );
  AddStringParameter ('Mid22Color'             , '32896'     );
  AddStringParameter ('Mid23Color'             , '65535'     );
  AddStringParameter ('Mid24Color'             , '8421504'   );
  AddStringParameter ('Mid25Color'             , '32768'     );
  AddStringParameter ('Mid26Color'             , '8388736'   );
  AddStringParameter ('Mid27Color'             , '8421376'   );
  AddStringParameter ('Mid28Color'             , '12632256'  );
  AddStringParameter ('Mid29Color'             , '128'       );
  AddStringParameter ('Mid30Color'             , '32768'     );
  AddStringParameter ('BottomSignalColor'      , '16711680'  );
  AddStringParameter ('TopOverlayColor'        , '32768'     );
  AddStringParameter ('BottomOverlayColor'     , '7585984'   );
  AddStringParameter ('TopPasteColor'          , '8388736'   );
  AddStringParameter ('BottomPasteColor'       , '128'       );
  AddStringParameter ('TopSolderColor'         , '3162822'   );
  AddStringParameter ('BottomSolderColor'      , '7307173'   );
  AddStringParameter ('Plane1Color'            , '32768'     );
  AddStringParameter ('Plane2Color'            , '128'       );
  AddStringParameter ('Plane3Color'            , '8388736'   );
  AddStringParameter ('Plane4Color'            , '8421376'   );
  AddStringParameter ('Plane5Color'            , '32768'     );
  AddStringParameter ('Plane6Color'            , '128'       );
  AddStringParameter ('Plane7Color'            , '8388736'   );
  AddStringParameter ('Plane8Color'            , '8421376'   );
  AddStringParameter ('Plane9Color'            , '32768'     );
  AddStringParameter ('Plane10Color'           , '128'       );
  AddStringParameter ('Plane11Color'           , '8388736'   );
  AddStringParameter ('Plane12Color'           , '8421376'   );
  AddStringParameter ('Plane13Color'           , '32768'     );
  AddStringParameter ('Plane14Color'           , '128'       );
  AddStringParameter ('Plane15Color'           , '8388736'   );
  AddStringParameter ('Plane16Color'           , '8421376'   );
  AddStringParameter ('DrillGuideColor'        , '128'       );
  AddStringParameter ('KeepOutColor'           , '8388736'   );
  AddStringParameter ('Mechanical1Color'       , '8388736'   );
  AddStringParameter ('Mechanical2Color'       , '8421376'   );
  AddStringParameter ('Mechanical3Color'       , '32768'     );
  AddStringParameter ('Mechanical4Color'       , '0'         );
  AddStringParameter ('Mechanical5Color'       , '8388736'   );
  AddStringParameter ('Mechanical6Color'       , '8421376'   );
  AddStringParameter ('Mechanical7Color'       , '32768'     );
  AddStringParameter ('Mechanical8Color'       , '0'         );
  AddStringParameter ('Mechanical9Color'       , '8388736'   );
  AddStringParameter ('Mechanical10Color'      , '8421376'   );
  AddStringParameter ('Mechanical11Color'      , '32768'     );
  AddStringParameter ('Mechanical12Color'      , '0'         );
  AddStringParameter ('Mechanical13Color'      , '8388736'   );
  AddStringParameter ('Mechanical14Color'      , '8421376'   );
  AddStringParameter ('Mechanical15Color'      , '32768'     );
  AddStringParameter ('Mechanical16Color'      , '0'         );
  AddStringParameter ('DrillDrawingColor'      , '3408013'   );
  AddStringParameter ('MultiLayerColor'        , '8421504'   );
  AddStringParameter ('ConnectLayerColor'      , '7709086'   );
  AddStringParameter ('BackgroundColor'        , '0'         );
  AddStringParameter ('DRCErrorColor'          , '65280'     );
  AddStringParameter ('SelectionColor'         , '65535'     );
  AddStringParameter ('VisibleGrid1Color'      , '12632256'  );
  AddStringParameter ('VisibleGrid2Color'      , '11913679'  );
  AddStringParameter ('PadHoleColor'           , '6899487'   );
  AddStringParameter ('ViaHoleColor'           , '9279142'   );
  
  RunProcess ('Pcb:SetupPreferences');
End;
{..............................................................................}

{..............................................................................}
Procedure SetSchemeClassic;
Begin  
  ResetParameters;
  AddStringParameter ('TopSignalColor'         , '255'       );                   
  AddStringParameter ('Mid1Color'              , '128'       );                   
  AddStringParameter ('Mid2Color'              , '32768'     );                 
  AddStringParameter ('Mid3Color'              , '65280'     );                 
  AddStringParameter ('Mid4Color'              , '8388608'   );               
  AddStringParameter ('Mid5Color'              , '16776960'  );              
  AddStringParameter ('Mid6Color'              , '8388736'   );               
  AddStringParameter ('Mid7Color'              , '16711935'  );              
  AddStringParameter ('Mid8Color'              , '32896'     );                 
  AddStringParameter ('Mid9Color'              , '65535'     );                 
  AddStringParameter ('Mid10Color'             , '8421504'   );               
  AddStringParameter ('Mid11Color'             , '16777215'  );              
  AddStringParameter ('Mid12Color'             , '8388736'   );               
  AddStringParameter ('Mid13Color'             , '8421376'   );               
  AddStringParameter ('Mid14Color'             , '12632256'  );              
  AddStringParameter ('Mid15Color'             , '128'       );                   
  AddStringParameter ('Mid16Color'             , '32768'     );                 
  AddStringParameter ('Mid17Color'             , '65280'     );                 
  AddStringParameter ('Mid18Color'             , '8388608'   );               
  AddStringParameter ('Mid19Color'             , '16776960'  );              
  AddStringParameter ('Mid20Color'             , '8388736'   );               
  AddStringParameter ('Mid21Color'             , '16711935'  );              
  AddStringParameter ('Mid22Color'             , '32896'     );                 
  AddStringParameter ('Mid23Color'             , '65535'     );                 
  AddStringParameter ('Mid24Color'             , '8421504'   );               
  AddStringParameter ('Mid25Color'             , '16777215'  );              
  AddStringParameter ('Mid26Color'             , '8388736'   );               
  AddStringParameter ('Mid27Color'             , '8421376'   );               
  AddStringParameter ('Mid28Color'             , '12632256'  );              
  AddStringParameter ('Mid29Color'             , '128'       );                   
  AddStringParameter ('Mid30Color'             , '32768'     );                 
  AddStringParameter ('BottomSignalColor'      , '16711680'  );              
  AddStringParameter ('TopOverlayColor'        , '65535'     );                 
  AddStringParameter ('BottomOverlayColor'     , '32896'     );                 
  AddStringParameter ('TopPasteColor'          , '8421504'   );               
  AddStringParameter ('BottomPasteColor'       , '128'       );                   
  AddStringParameter ('TopSolderColor'         , '8388736'   );               
  AddStringParameter ('BottomSolderColor'      , '16711935'  );              
  AddStringParameter ('Plane1Color'            , '32768'     );                 
  AddStringParameter ('Plane2Color'            , '128'       );                   
  AddStringParameter ('Plane3Color'            , '8388736'   );               
  AddStringParameter ('Plane4Color'            , '8421376'   );               
  AddStringParameter ('Plane5Color'            , '32768'     );                 
  AddStringParameter ('Plane6Color'            , '128'       );                   
  AddStringParameter ('Plane7Color'            , '8388736'   );               
  AddStringParameter ('Plane8Color'            , '8421376'   );               
  AddStringParameter ('Plane9Color'            , '32768'     );                 
  AddStringParameter ('Plane10Color'           , '128'       );                   
  AddStringParameter ('Plane11Color'           , '8388736'   );               
  AddStringParameter ('Plane12Color'           , '8421376'   );               
  AddStringParameter ('Plane13Color'           , '32768'     );                 
  AddStringParameter ('Plane14Color'           , '128'       );                   
  AddStringParameter ('Plane15Color'           , '8388736'   );               
  AddStringParameter ('Plane16Color'           , '8421376'   );               
  AddStringParameter ('DrillGuideColor'        , '128'       );                   
  AddStringParameter ('KeepOutColor'           , '16711935'  );              
  AddStringParameter ('Mechanical1Color'       , '16711935'  );              
  AddStringParameter ('Mechanical2Color'       , '8388736'   );               
  AddStringParameter ('Mechanical3Color'       , '32768'     );                 
  AddStringParameter ('Mechanical4Color'       , '32896'     );                 
  AddStringParameter ('Mechanical5Color'       , '16711935'  );              
  AddStringParameter ('Mechanical6Color'       , '8388736'   );               
  AddStringParameter ('Mechanical7Color'       , '32768'     );                 
  AddStringParameter ('Mechanical8Color'       , '32896'     );                 
  AddStringParameter ('Mechanical9Color'       , '16711935'  );              
  AddStringParameter ('Mechanical10Color'      , '8388736'   );               
  AddStringParameter ('Mechanical11Color'      , '32768'     );                 
  AddStringParameter ('Mechanical12Color'      , '32896'     );                 
  AddStringParameter ('Mechanical13Color'      , '16711935'  );              
  AddStringParameter ('Mechanical14Color'      , '8388736'   );               
  AddStringParameter ('Mechanical15Color'      , '32768'     );                 
  AddStringParameter ('Mechanical16Color'      , '0'         );                 
  AddStringParameter ('DrillDrawingColor'      , '2752767'   );               
  AddStringParameter ('MultiLayerColor'        , '12632256'  );              
  AddStringParameter ('ConnectLayerColor'      , '7709086'   );               
  AddStringParameter ('BackgroundColor'        , '0'         );                     
  AddStringParameter ('DRCErrorColor'          , '65280'     );                 
  AddStringParameter ('SelectionColor'         , '16777215'  );              
  AddStringParameter ('VisibleGrid1Color'      , '6049101'   );                 
  AddStringParameter ('VisibleGrid2Color'      , '9473425'   );               
  AddStringParameter ('PadHoleColor'           , '15461320'  );               
  AddStringParameter ('ViaHoleColor'           , '11599871'  );                   
                                                               
  RunProcess ('Pcb:SetupPreferences'); 
End;                                                          
{..............................................................................}

{..............................................................................}                                                              
                                                              
                                                              
                                                              
                                                              
                                                              
                                                              
                                                              
                                                              
                                                              
