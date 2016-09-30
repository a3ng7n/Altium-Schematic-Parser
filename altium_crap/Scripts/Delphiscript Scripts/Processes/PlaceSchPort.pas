{..............................................................................}
{ Summary Place a Schematic Port Object                                        }
{ Copyright (c) 2003 by Altium Limited                                         }  
{..............................................................................}

{..............................................................................}

{..............................................................................}
Begin
   ResetParameters;
   AddIntegerParameter('Location.X', 20000000   );
   AddIntegerParameter('Location.Y', 20000000   );
   AddIntegerParameter('Style'     , 2          );
   AddIntegerParameter('IOType'    , 3          );
   AddIntegerParameter('Alignment' , 0          );
   AddIntegerParameter('Width'     , 10000000   );
   AddStringParameter ('Name'      , 'Test Port');
   AddIntegerParameter('AreaColor' , $FFFFFF    );
   AddIntegerParameter('TextColor' , $000000    );
   RunProcess         ('Sch:PlacePort'          );
End.
{..............................................................................}

{..............................................................................}

