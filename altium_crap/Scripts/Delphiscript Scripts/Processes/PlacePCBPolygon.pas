{..............................................................................}
{ Summary Place a PCB Polygon Object                                           }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Begin

   // A square polygon on Top layer is placed at 5000,5000 mils
   ResetParameters;
   AddStringParameter('Location.X', '5000');
   AddStringParameter('Location.Y', '5000');

   // Setup properties for this polygon
   AddStringParameter('PourOver','True');
   AddStringParameter('RemoveDead','False');
   AddStringParameter('GridSize','10');      
   AddStringParameter('TrackWidth','12');
   AddStringParameter('HatchStyle','90Degree');
   AddStringParameter('Name','GND');   
   AddStringParameter('Layer','Top');
   AddStringParameter('PourOver','True');            
   AddStringParameter('PolygonType','Polygon');
   AddStringParameter('Selected','True');
            
   // Polygon Vertices
   AddStringParameter('Kind0','0');
   AddStringParameter('Vx0','1000');
   AddStringParameter('Vy0','1000');
      
   AddStringParameter('Kind1','0');
   AddStringParameter('Vx1','2500');
   AddStringParameter('Vy1','1000');
   
   AddStringParameter('Kind2','0');
   AddStringParameter('Vx2','2500');
   AddStringParameter('Vy2','2500');
   
   AddStringParameter('Kind3','0');
   AddStringParameter('Vx3','1000');
   AddStringParameter('Vy3','2500');
   
   AddStringParameter('Kind4','0');
   AddStringParameter('Vx4','1000');
   AddStringParameter('Vy4','1000');
   
   RunProcess('PCB:PlacePolygonPlane');
End.
{..............................................................................}

{..............................................................................}

