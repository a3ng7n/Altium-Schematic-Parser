{..............................................................................}
{ Summary Change Mapping Dialog to map fields from a CSV text file             }
{         Used by the main Import Pin dialog                                   }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure TFormChangeMapping.FormChangeMappingCreate(Sender: TObject);
Begin
    ComboBox.Items.Text := 'Display Name'      + #13 + #10 +  // SchPin.Name
                           'Show Display Name' + #13 + #10 +  // SchPin.ShowName
                           'Designator'        + #13 + #10 +  // SchPin.Designator
                           'Electrical Type'   + #13 + #10 +  // SchPin.Electrical
                           'Description'       + #13 + #10 +  // SchPin.Description
                           'Pin is Hidden'     + #13 + #10 +  // SchPin.IsHidden
                           'Hidden Net Name'   + #13 + #10 +  // SchPin.HiddenNetName
                           'Inner Symbol'      + #13 + #10 +  // SchPin.Symbol_Inner
                           'Inner Edge Symbol' + #13 + #10 +  // SchPin.Symbol_InnerEdge
                           'Outer Symbol'      + #13 + #10 +  // SchPin.Symbol_Outer
                           'Outer Edge Symbol' + #13 + #10 +  // SchPin.Symbol_OuterEdge
                           'Length'            + #13 + #10 +  // SchPin.PinLength
                           'Orientation'       + #13 + #10 +  // SchPin.Orientation
                           'Location X'        + #13 + #10 +  // SchPin.Location - x
                           'Location Y'        + #13 + #10 +  // SchPin.Location - y
                           '';
End;
{..............................................................................}

{..............................................................................}





