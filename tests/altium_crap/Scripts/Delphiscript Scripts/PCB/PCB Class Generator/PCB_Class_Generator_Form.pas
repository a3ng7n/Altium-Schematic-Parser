{..............................................................................}
{ Summary: Add nets connected to selected objects to a net class.              }
{ Authors: Matt Berggren, David Parker                                         }
{ Copyright (c) 2008 by Altium Limited                                         }
{ Usage:                                                                       }
{  1. First select one or more pads. If the pads are part of footprints, use   }
{ the shift key to only select the pads (not the entire footprint).            }
{  2. Run this script                                                          }
{  3. Follow the prompts and either create a new net class or add the selected }
{  objects to an existing net class.                                           }
{  4. Click OK to complete the operation                                       }
{..............................................................................}
Var                                                                             //Global Variables
   Board         :  IPCB_Board;                                                 //The interface to the current PCB as returned by the PCBServer
   BoardIterator :  IPCB_BoardIterator;                                         //Used for iterating through a list of PCB objects
   ClassList     :  TStringList;                                                //A list of strings
   NetClass      :  IPCB_ObjectClass;                                           //An interface to PCB classes
   ClassIterator :  IPCB_BoardIterator;                                         //Used for iterating through a list of PCB classes

{..............................................................................}

{..............................................................................}
{ CollectNetNameClasses                                                        }
{ Summary: Add the names of all existing net classes to ClassList and the net  }
{  class combo box in the NetClassGenerator Form                               }
{                                                                              }
{ Parameters: none                                                             }
{ Modifies: ClassList, formNetClassGenerator.comboBoxClassList.items           }
{ Returns: nothing                                                             }
{..............................................................................}
Procedure CollectNetClasses(Dummy);                                             //Use 'Dummy' as a parameter to prevent this procedure from external visibility - i.e. from DXP >> Run Script
Var
    i : Integer;                                                                //index into formNetClassGenerator comboBoxClassList
Begin

   ClassIterator := Board.BoardIterator_Create;                                 //Create a new iterator - by default, the filter will be set to eNoObject and eNoLayer
   ClassIterator.SetState_FilterAll;                                            // so free the filter up
   ClassIterator.AddFilter_ObjectSet(MkSet(eClassObject));                      //Use the MkSet function to create a single element set [eClassObject].  Add this set as a filter
   NetClass := ClassIterator.FirstPCBObject;                                    //Retrieve the first item in the Iterator;
   i := 0;                                                                      //Initialize the index counter
   While NetClass <> Nil Do                                                     //While NetClass points to something meaningful
   Begin
       Inc(i);                                                                  //Increment the index counter;
       If NetClass.MemberKind = eClassMemberKind_Net Then                       //We are only interested in 'Net' Classes so skip other (Component, FromTo, Pad, etc.) classes
       Begin                                                                            //Here because we have found a Net class
           formNetClassGenerator.comboBoxClassList.items.AddObject(NetClass.Name, i);   //So add the name of the net class to the combo box
           ClassList.Add(NetClass.Name);                                                // and also add its name to the ClassList string list.
       End;
       NetClass := ClassIterator.NextPCBObject;                                 //Move on to the next object in the iterator
   End;
   Board.BoardIterator_Destroy(ClassIterator);                                  //We don't need the iterator anymore so free its memory.
End;
{..............................................................................}

{..............................................................................}
{ CreateClasses                                                                }
{ Summary: This is the main entry point for this script.                       }
{ Initialize the contents of the net class combo box and run the main form.    }
{                                                                              }
{ Parameters: none                                                             }
{ Modifies:                                                                    }
{ Returns: nothing                                                             }
{..............................................................................}
Procedure CreateClasses;                                                        //The main procedure must not have any parameters
Begin
    Board := PCBServer.GetCurrentPCBBoard;                                      //Retrieve the interface to the current PCB
    If Board = Nil Then Exit;                                                   //We can't assume that a PCB document is open so check just to make sure

    ClassList := TStringList.Create;                                            //Create the blank ClassList string list

    CollectNetClasses(Nil);                                                     //Call CollectNetClasses to populate ClassList and the combo box.  Note that Nil is passed as the Dummy parameter

    formNetClassGenerator.ShowModal;                                            //Run the main form modally
End;
{..............................................................................}

{..............................................................................}
{ AddNetNamesToNetClass                                                        }
{ Summary: Add the net name attached to all selected objects to a net class    }
{                                                                              }
{ Parameters:                                                                  }
{    ANetClassName: String, - the name of the netclass that nets are to be     }
{                             added to                                         }
{ Modifies: Nets in NetClass ANetClassName                                     }
{ Returns: nothing                                                             }
{..............................................................................}
Procedure AddNetNamesToNetClass(ANetClassName : String);                        //ANetClassName is the name of the NetClass that net names will be added to
Var
   ASetOfObjects : TObjectSet;                                                  //The set of objects that can have net attributes attached to them
   PCBObject     : IPCB_Object;                                                 //The interface to an individual PCB object
   Iterator      : IPCB_BoardIterator;                                          //Used to iterate through a group of PCB objects
Begin
    ASetOfObjects := MkSet(eArcObject,eTrackObject, eViaObject, ePadObject, eRegionObject);       //Create a set of objects that can have net attributes attached to them
    Iterator := Board.BoardIterator_Create;                                     //Create the PCB Object iterator - no need to call SetState_FilterAll because we will explicitly set the layers and objects in the following lines
    Iterator.AddFilter_ObjectSet(ASetOfObjects);                                //Filter on only those objects that can have net attributes attached to them
    Iterator.AddFilter_LayerSet(AllLayers);                                     //Filter across all layers

    ClassIterator    := Board.BoardIterator_Create;                             //Create a new iterator - by default, the filter will be set to eNoObject and eNoLayer
    ClassIterator.SetState_FilterAll;                                           // so free the filter up
    ClassIterator.AddFilter_ObjectSet(MkSet(eClassObject));                     //Filter only on the Class Objects

    NetClass := ClassIterator.FirstPCBObject;                                   //Retrieve the first NetClass (we hope) item in the Iterator;
    While NetClass <> Nil Do                                                    //Loop while NetClass points to something meaningful
    Begin
        If NetClass.MemberKind = eClassMemberKind_Net Then                      //We are only interested in 'Net' Classes so skip other (Component, FromTo, Pad, etc.) classes
        Begin
            If NetClass.Name = ANetClassName then                               //If we have found the NetClass with the name specified by ANetClassName
            Begin
                  PCBObject := Iterator.FirstPCBObject;                         //Start iterating through all of the PCB objects in the PCB Object iterator
                  While PCBObject <> Nil Do                                     //While PCBObject points to something meaningful
                  Begin
                       If ((PCBObject.Net <> Nil) && (PCBObject.Selected))  Then //If the PCBObject's net name is not empty and the PCBObject was part of the user's selection
                            NetClass.AddMemberByName(PCBObject.Net.Name);       //Add the PCBObject's net name to the NetClass

                       PCBObject := Iterator.NextPCBObject;                     //And move on to the next PCB object in the iterator
                  End;
                  Break;                                                        //We've found what we were looking for (ANetClassName) so jump out of the while loop
            End;
        End;
        NetClass := ClassIterator.NextPCBObject;                                //Move on to the next object in the iterator.
    End;
    Board.BoardIterator_Destroy(Iterator);                                      //OK, we're done with the PCB object iterator so free its memory
    Board.BoardIterator_Destroy(ClassIterator);                                 // and free the ClassIterator too.
End;
{..............................................................................}

{..............................................................................}
{ TformNetClassGenerator.buttonOKClick                                         }
{ Summary: Called in response to a user clicking the OK button, this procedure }
{  looks for user selected net class, adds a new one if it can't be found, and }
{  adds the nets of the selected objects to the net class.                     }
{                                                                              }
{ Parameters: none                                                             }
{ Modifies: Nets in the NetClass selected by the user                          }
{ Returns: nothing                                                             }
{..............................................................................}
Procedure TformNetClassGenerator.buttonOKClick(Sender: TObject);                //buttonOKClick is an event handler that will be called when the OK button is clicked
Var                                                                             // Sender is the Control which initiated the message (in this case it will be the OK button)
   i             : Integer;                                                     //Index used for looping through items in ClassList
Begin
   i := 0;                                                                      //Start our search from the first item in ClassList
   while (i < ClassList.Count) Do                                               // and be prepared to loop through all items in ClassList
   Begin
      If ClassList.Strings[i] = comboBoxClassList.text then                     //If the NetClass defined by the user already exists (i.e. was found in ClassList)
         Break                                                                  // then no need to keep looking
      Else                                                                      //Otherwise
         Inc(i);                                                                // Move on to check the next one in the list
   End;

   if (i = ClassList.Count) then                                                //If we got this far without finding the NetClass in ClassList
   Begin                                                                        //then we need to create a new NetClass
      PCBServer.PreProcess;                                                     //Need to run this before creating anything new on the PCB
      NetClass := PCBServer.PCBClassFactoryByClassMember(eClassMemberKind_Net); //Use the ClassFactory to create a new NetClass object
      NetClass.SuperClass := False;                                             //We don't want a superclass
      NetClass.Name := comboBoxClassList.text;                                  //Give it the name defined by the user
      Board.AddPCBObject(NetClass);                                             // and add it to the PCB
      PCBServer.PostProcess;                                                    //Finalize things with this procedure after creating anything new on the PCB
   End;

   AddNetNamesToNetClass(comboBoxClassList.text);                               //Add the list of net names to the NetClass
   ShowInfo('Nets were successfully added to Net Class:' + comboBoxClassList.text);   //Let the user know that their request was successful
   formNetClassGenerator.close;                                                 // and close the modal form to exit.
End;
{..............................................................................}

