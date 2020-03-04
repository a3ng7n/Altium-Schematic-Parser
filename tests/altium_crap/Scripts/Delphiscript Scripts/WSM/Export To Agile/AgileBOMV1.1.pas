{..............................................................................}
{ Title:       Special Variant BOM Reports: Agile and Engineering formats      }
{ Type:        Delphi Script                                                   }
{ Used On:     Altium Projects (*.PrjPcb)                                      }
{ Filename:    AgileBOM.pas                                                    }
{ Author:      Jeff Condit                                                     }
{ Copyright:   (c) 2008 by Altium Limited                                      }
{ Description: This script searches the project for assembly variant informa-  }
{              tion and formats a report of it.  When finished, the report is  }
{              saved in the project directory with an *.VarRpt extension and   }
{              then displayed.  There are 2 passes; one to determine the neces-}
{              sary field widths, and the second to format the report.         }
{ Revision:    1.0 10/03/2008 JC Initial coding                                }
{              1.1 10/09/2008 JC Minor Adjustments after review:               }
{                                Default LibRef header changed to COHR P/N     }
{                                Added BOMTitle and TitleStr for substitution  }
{                                Auto-compile if flattened doc unavailbale     }
{                                Stopped long fields from misaligning comp list}
{                                Default to DNI parameter if it exists         }
{                                Add BOMAgileSuppress for null last columns    }
{                                Add BOMAgileNoQuotes for fields w/o commas    }
{..............................................................................}

{..............................................................................}
// GLOBAL VARIABLES SECTION
Var
    CurrWorkSpace         : IWorkSpace;          // An Interface handle to the current workspace
    CurrProject           : IProject;            // An Interface handle to the current Project
    ProjectVariant        : IProjectVariant;     // An Interface handle to the current Variant
    ProjectVariantCount   : Integer;             // Number of Variants
    VarDescStr            : String;              // A string for the Variant Description
    FlattenedDoc          : IDocument;           // An Interface handle to the "flattened" document
    CompCount             : Integer;             // A count of the number of components in document
    CurrComponent         : IComponent;          // An Interface handle to the current Component
    LibRefWid             : Integer;             // Width of Library Reference in CompList Record
    DescriptionWid        : Integer;             // Width of Description in CompList Record
    FootprintWid          : Integer;             // Width of Footprint in CompList Record
    PhysicalDesWid        : Integer;             // Width of Physical Designator in CompList Record
    StuffedWid            : Integer;             // Width of Stuffed flag in CompList Record
    LibRefStart           : Integer;             // Width of Library Reference in CompList Record
    DescriptionStart      : Integer;             // Start of Description in CompList Record
    FootprintStart        : Integer;             // Start of Footprint in CompList Record
    PhysicalDesStart      : Integer;             // Start of Physical Designator in CompList Record
    StuffedStart          : Integer;             // Start of Stuffed flag in CompList Record
    LibRefEnd             : Integer;             // End of Library Reference in CompList Record
    DescriptionEnd        : Integer;             // End of Description in CompList Record
    FootprintEnd          : Integer;             // End of Footprint in CompList Record
    PhysicalDesEnd        : Integer;             // End of Physical Designator in CompList Record
    StuffedEnd            : Integer;             // End of Stuffed flag in CompList Record
    StuffedPhysDesColWid  : Integer;             // Width of Stuffed Designator Column in Report
    NoStuffPhysDesColWid  : Integer;             // Width of Not Stuffed Designator Column in Report
    CompList              : TStringList;         // Component Data List
    CompListCount         : Integer;             // A count of strings in CompList
    CompanyNam            : String;              // A string for the Company Name
    AssyNumber            : String;              // A string for the Assembly Number
    RevNumber             : String;              // A string for the Revision Number
    TitleStr              : String;              // A string for the design's Title
    DateStr               : TDynamicString;      // A string for the date
    TimeStr               : TDynamicString;      // A string for the time
    SuppressNulLastCol    : Boolean;             // Flag to suppress the last column if empty
    SuppressQuoteWoCommas : Boolean;             // Flag to suppress quoting columns w/o commas
    StuffedPhysDesList    : Array[1..999] of String; // An Array of Stuffed PhysDes Lines
    StuffedPhysDesString  : String;                  // A long string of Stuffed PhysDes Lines
    StuffedPhysDesCount   : Integer;                 // The Number of lines needed for Stuffed Designators
    NoStuffPhysDesList    : Array[1..999] of String; // An Array of Un-Stuffed PhysDes Lines
    NoStuffPhysDesString  : String;                  // A long string of Un-Stuffed PhysDes Lines
    NoStuffPhysDesCount   : Integer;                 // The Number of lines needed for Un-Stuffed Designators
    ParameterName         : String;                  // The name of the component paramater to match teh variant by
    VariantName           : String;                  // The name of the variant to use
    UseParameters         : Boolean;                 // Use a component parameter to define the variant to use
    UseVariants           : Boolean;                 // Use variants to define the variant to use
    FullyPopulated        : Boolean;                 // Fully populate the BOM
    CreateAgileBOM        : Boolean;                 // Create a BOM in agile format
    CreateEngineeringBOM  : Boolean;                 // Create a engineering BOM
    AddToProject          : Boolean;                 // True if run from DXP>>Run Script ; False if run from OutJob
    OpenOutputs           : Boolean;                 // System parameter supplied by OutJob : open generated files
    TargetFileName        : String;                  // System parameter supplied by OutJob : desired output file name, if any
    TargetFolder          : String;                  // System parameter supplied by OutJob : desired output folder
    TargetPrefix          : String;                  // System parameter supplied by OutJob : desired output file name prefix, if any

// REPORT CONFIGURATION VARIABLES
    HeaderStr1            : String;              // Header line 1
    HeaderStr2            : String;              // Header line 2
    HeaderStr3            : String;              // Header line 3
    HeaderStr4            : String;              // Header line 4
    HeaderStr5            : String;              // Header line 5
    HeaderStr6            : String;              // Header line 6
    DetailStr1            : String;              // Detail First Line 1
    LineFormat            : String;              // Format of Line
    MaxCols               : Integer;             // Maximum Number of Columns
    LastCol               : Integer;             // Last Column Defined
    ColWidth              : Array[1..99] of Integer; // Column Widths
    ColStart              : Array[1..99] of Integer; // Starting Column
    ColEnd                : Array[1..99] of Integer; // Ending Column
    ColType               : Array[1..99] of String;  // Column Type
{...............................................................................}

{...............................................................................}
Function GetOutputFileNameWithExtension(Ext : String) : String;
Begin
    If TargetFolder = '' Then
        TargetFolder := CurrProject.DM_GetOutputPath;
    If TargetFileName = '' Then
        TargetFileName := CurrProject.DM_ProjectFileName;
    Result := AddSlash(TargetFolder) + TargetPrefix + ChangeFileExt(TargetFileName, Ext);
End;
{...............................................................................}

{...............................................................................}
procedure GetState_Controls;
Begin
    ParameterName         := ParametersComboBox.Items[ ParametersComboBox.ItemIndex ];
    VariantName           := VariantsComboBox  .Items[VariantsComboBox.ItemIndex];
    UseParameters         := UseParametersRadioButton    .Checked;
    UseVariants           := UseVariantsRadioButton      .Checked;
    FullyPopulated        := FullyPopulatedRadioButton   .Checked;
    CreateAgileBOM        := CreateAgileBOMCheckBox      .Checked;
    CreateEngineeringBOM  := CreateEngineeringBOMCheckBox.Checked;
End;
{...............................................................................}

{...............................................................................}
procedure SetState_Controls;
Begin
    ParametersComboBox          .ItemIndex := ParametersComboBox.Items.IndexOf(ParameterName);
    VariantsComboBox            .ItemIndex := VariantsComboBox  .Items.IndexOf(VariantName);
    UseParametersRadioButton    .Checked   := UseParameters;
    UseVariantsRadioButton      .Checked   := UseVariants;
    FullyPopulatedRadioButton   .Checked   := FullyPopulated;
    CreateAgileBOMCheckBox      .Checked   := CreateAgileBOM;
    CreateEngineeringBOMCheckBox.Checked   := CreateEngineeringBOM;
End;
{...............................................................................}

{...............................................................................}
{ FUNCTION Stuffed( PhysicalDesignator : String ) determines whether the speci- }
{ fied Physical Designator represents a Stuffed part in the current Variant (or }
{ Parameter equivalent).                                                        }
{...............................................................................}
Function Stuffed(
    PhysicalDesignator    : String
    )                     : Boolean;
Var
    ComponentVariation    : IComponentVariation; // An Interface handle to a varied component
    CompCount             : Integer;             // The Number of Components in Flattened Document
    CompIndex             : Integer;             // An Index for pulling out components
    ParmCount             : Integer;             // The Number of Parameters in Component
    ParmIndex             : Integer;             // An Index for pulling out Parameters
    DNIParmName           : String;              // The Do Not Install Parameter Name for which we are searching
    CurrParm              : IParameter;          // An interface handle to a Parameter
    CompFound             : Boolean;             // Whether we are done scanning components
    ParmFound             : Boolean;             // Whether we are done scanning components
Begin
    // NOTE: Only one of the following three radio buttons will be checked:

    // First, see if it is fully populated
    If (FullyPopulated = True) Then Result := True;

    // Second, see if we are using a component Parameter
    If UseParameters Then
    Begin
        // Determine the Parameter Name for which we are looking
        DNIParmName := ParameterName;

        // Set the default answer to Not Stuffed if component cannot be found
        Result := False;

        // Clear the found flags
        CompFound := False;
        ParmFound := False;

        // Determine the number of compoents in the flattened document
        CompCount := FlattenedDoc.DM_ComponentCount;

        // Begin walking through components to find it so we can look at it's parameter names
        For CompIndex := 0 To CompCount - 1 Do
        Begin
            // Fetch a Component
            CurrComponent := FlattenedDoc.DM_Components[ CompIndex ];

            // Terminate continued searching after we find the right physical designator
            If (CurrComponent.DM_PhysicalDesignator = PhysicalDesignator) Then
            Begin
                // The right component has been found, so cease searching more components
                CompFound := True;
                CompIndex := CompCount;

                // Set the default answer to Stuffed if the component has been found
                Result := True;

                // Determine how many parameters it has
                ParmCount := CurrComponent.DM_ParameterCount;

                // Begin walking through parameters to find the specified one
                For ParmIndex := 0 To ParmCount - 1 Do
                Begin
                    // Fetch one of the component's Parameters
                    CurrParm := CurrComponent.DM_Parameters( ParmIndex );

                    // Terminate Continued searching after we find the right one
                    If (CurrParm.DM_Name = DNIParmName) Then
                    Begin
                        // The right Parameter has been found, so cease searching any more
                        ParmFound := True;
                        ParmIndex := ParmCount;

                        // Set the default answer to Do Not Stuffed unless certain velues exist
                        Result := False;

                        // Check for specific values which will allow this part to be stuffed
                        If ((CurrParm.DM_Value = '*') Or (CurrParm.DM_Value = ''))
                        Then Result := True;
                    End; // If (CurrParm.DM_Name := DNIParmName)
                End; // For ParmIndex := 0 To ParmCount - 1 Do
            End; // If (CurrComponent.DM_PhysicalDesignator = PhysicalDesignator) else
        End; // CompIndex := 0 To CompCount - 1 Do
    End; // If (UseParametersRadioButton.Checked = True) Then

    // Last, see if we are using a Variant
    If UseVariants Then
    Begin
        // If there is no Project Variant then the part is assumed stuffed
        If (ProjectVariant = Nil) Then Result := True
        Else
            Begin
                // Now that we have the valid Project Variant we must look for this component
                ComponentVariation := ProjectVariant.DM_FindComponentVariationByDesignator( PhysicalDesignator );

                // If no Component Variation for this designator, then component is assumed stuffed
                If (ComponentVariation = Nil)
                Then Result := True
                // Otherwise, it is fitted unless its VariationKind is eVariation_NotFitted
                Else Result := ComponentVariation.DM_VariationKind <> eVariation_NotFitted;
            End; // If (ProjectVariant = Nil) Else
    End; // If UseVariantsRadioButton.Checked = True)
End; // Function Stuffed( PhysicalDesignator : String ) : Boolean;
{...............................................................................}

{...............................................................................}
{ PROCEDURE FetchComponents( Dummy: Integer ) reads the Flattened schematic     }
{ sheet created by compiling the project to extract a list of all components and}
{ associated information necessary for the report.  This information is sorted, }
{ counted, and stored in a list.                                                }
{...............................................................................}
procedure FetchComponents( Dummy: Integer );
Var
    CompIndex             : Integer;             // An Index for pullin out components
    PhysCompCount         : Integer;             // A count of the number of components in document
    PhysComponent         : IComponent;          // An Interface handle to the current Component
    PhysCompIndex         : Integer;             // An index for pulling out Physical Parts
    CurrPart              : IPart;               // An Interface handle to the current Part of a Component
    CurrSortStr           : String;              // A String to hold the sorting field
    CurrPhysDesStr        : String;              // A String to hold the current Physical Designator
    CurrFootprintStr      : String;              // A String to hold the current Footprint
    CurrDescrStr          : String;              // A String to hold the current Description
    CurrLibRefStr         : String;              // A String to hold the current Library Reference
    CurrStuffedStr        : String;              // A String to hold the Stuffed flag for the currrent component
    FormatStr             : String;              // A String into which one part's data can be formatted
    TempStrg              : String;              // A temporary string for formatting
    TempChar              : String;              // A temporary string for one Char when formatting
    SortLetters           : String;              // A string for letters of the designator prefix
    SortNumbers           : String;              // A string for numbers of the designator
    SortRest              : String;              // A string for the rest of the designator
    SortLetrWid           : Integer;             // Width of the sort letters column
    SortNumbWid           : Integer;             // Width of the sort Numbers column
    SortRestWid           : Integer;             // Width of the sort Rest column
    SortWid               : Integer;             // Width of the entire sort field
    SortStart             : Integer;             // Start of the entire sort field
    SortCount             : Integer;             // Number of characters in the Physical Designator
    SortEnd               : Integer;             // End of the entire sort field
    LetrDone              : Boolean;             // Flag for processing designator prefix letters
    NumbDone              : Boolean;             // Flag for processing designator prefix numbers
    RestDone              : Boolean;             // Flag for processing the rest of the designator characters
    SortIndex             : Integer;             // Temporary Index for chars in the Designator
    CompCount             : Integer;             // The Number of Components Flattened Document
    CompListIndex         : Integer;             // An index for strings in CompList
Begin
    // Establish fields in Component List
    SortLetrWid := 10;
    SortNumbWid := 10;
    SortRestWid := 50;
    SortWid := SortLetrWid + SortNumbWid + SortRestWid;
    LibRefWid := 30;
    DescriptionWid := 50;
    FootprintWid := 50;
    PhysicalDesWid := 30;
    StuffedWid := 1;
    LibRefStart := 1;
    SortStart := LibRefStart + LibRefWid;
    DescriptionStart := SortStart + SortWid;
    FootprintStart := DescriptionStart + DescriptionWid;
    PhysicalDesStart := FootprintStart + FootprintWid;
    StuffedStart := PhysicalDesStart + PhysicalDesWid;
    LibRefEnd := SortStart - 1;
    SortEnd := DescriptionStart - 1;
    DescriptionEnd := FootprintStart - 1;
    FootprintEnd := PhysicalDesStart - 1;
    PhysicalDesEnd := StuffedStart - 1;
    StuffedEnd := StuffedStart + StuffedWid - 1;

    // Fetch the Flattened schematic sheet document.  This is a fictitious document
    // generated when the project is compiled containing all components from all
    // sheets.  This is much more useful for making lists of everything than rummaging
    // through all the sheets one at a time.  This sheet is not graphical in that
    // it cannot be viewed like a schematic, but you can view what's in it by using
    // the Navigator panel.
    FlattenedDoc := CurrProject.DM_DocumentFlattened;

    // If we couldn't get the flattened sheet, then most likely the project has
    // not been compiled recently
    If (FlattenedDoc = Nil) Then
    Begin
        // First try compiling the project
        AddStringParameter( 'Action', 'Compile' );
        AddStringParameter( 'ObjectKind', 'Project' );
        RunProcess( 'WorkspaceManager:Compile' );

        // Try Again to open the flattened document
        FlattenedDoc := CurrProject.DM_DocumentFlattened;
        If (FlattenedDoc = Nil) Then
        Begin
            ShowMessage('NOTICE: Compile the Project before Running this report.');
            Close;
            Exit;
        End; // If (FlattenedDoc = Nil) Then
    End; // If (FlattenedDoc = Nil) Then

    // Now that we have the flattened document, check how many components are in it
    CompCount := FlattenedDoc.DM_ComponentCount;

    // Set Up Progress Bar
    ProgressBar.Min      := 0;
    ProgressBar.Max      := CompCount - 1;
    ProgressBar.Step     := 1;
    ProgressBar.Position := 0;
    ProgressBar.Visible  := True;

    // Walk through every component one-by-one pulling out needed information and
    // writing it into the component string list
    For CompIndex := 0 To CompCount - 1 Do
    Begin
        CurrComponent := FlattenedDoc.DM_Components[ CompIndex ];

        // Update Progress Bar
        ProgressBar.Position := CompIndex;

        // Much information about the component can only be retrieved by examining
        // one the its sub-parts (Strange, but true.)
        CurrPart         := CurrComponent.DM_SubParts[0];

        // Create the Sort String
        // Initialize the sort strings and flags
        SortLetters := '';
        SortNumbers := '';
        SortRest := '';
        LetrDone := False;
        NumbDone := False;
        RestDone := False;

        // Fetch the physical designator string so we can parse it
        TempStrg := Trim( CurrPart.DM_PhysicalDesignator );
        SortCount := Length( Tempstrg );

        // Now iterate through its characters
        For SortIndex := 1 To SortCount Do
        Begin
            // Fetch the next character
            TempChar := Copy( TempStrg, SortIndex, 1 );

            // If processing prefix letters:
            If (LetrDone = False)
            Then
                Begin
                    // See if it is a letter and will fit
                    If ( ( ((TempChar >= 'A') And (TempChar <= 'Z'))
                        Or ((TempChar >= 'a') And (TempChar <= 'z')) )
                      And (Length( SortLetters ) < SortLetrWid) )
                    Then SortLetters := SortLetters + TempChar
                    Else LetrDone := True;
                End; // If (LetrDone = False)

            // If processing the prefix numbers:
            If ((LetrDone = True) And (NumbDone = False))
            Then
                Begin
                    // See if it is a number and will fit
                    If ( ((TempChar >= '0') And (TempChar <= '9'))
                      And (Length( SortNumbers ) < SortNumbWid) )
                    Then SortNumbers := SortNumbers + TempChar
                    Else NumbDone := True;
                End; // If ((LetrDone = True) And (NumbDone = False))

            // If processing the rest of the physical designator:
            If ((LetrDone = True) And (NumbDone = True) And (RestDone = False))
            Then
                Begin
                    // See if there is room for the rest of it
                    If (Length( SortRest ) < SortRestWid)
                    Then SortRest := SortRest + TempChar
                    Else RestDone := True;
                End; // If ((LetrDone = True) And (NumbDone = True) And (RestDone = False))
        End; // For (SortIndex := 1 To SortWid Do

        // Now create the sort formatted string
        TempStrg :=
            SortLetters
          + StringOfChar( ' ', SortLetrWid - Length( SortLetters ) )
          + StringOfChar( '0', SortNumbWid - Length( SortNumbers ) )
          + SortNumbers
          + SortRest
          + StringOfChar( ' ', SortRestWid - Length( SortRest    ) );

        // Exactly fill the Sorting field
        If ( Length( TempStrg ) > SortWid) // If too long
        Then TempStrg := Copy( TempStrg, 1, SortWid );
        CurrSortStr := TempStrg + StringOfChar( ' ', SortWid - Length( TempStrg ) ); // Pad if needed

        // Exactly fill the Library Reference field
        TempStrg := Trim( CurrPart.DM_LibraryReference );
        If ( Length( TempStrg ) > LibRefWid) // If too long
        Then TempStrg := Copy( TempStrg, 1, LibRefWid );
        CurrLibRefStr := TempStrg + StringOfChar( ' ', LibRefWid - Length( TempStrg ) ); // Pad if needed

        // Exactly fill the Description field
        TempStrg := Trim( CurrPart.DM_Description );
        If ( Length( TempStrg ) > DescriptionWid) // If too long
        Then TempStrg :=  Copy( TempStrg, 1, DescriptionWid );
        CurrDescrStr := TempStrg + StringOfChar( ' ', DescriptionWid - Length( TempStrg ) ); // Pad if needed

        // Exactly fill the Footprint field
        TempStrg := Trim( CurrPart.DM_Footprint );
        If ( Length( TempStrg ) > FootprintWid) // If too long
        Then TempStrg :=  Copy( TempStrg, 1, FootprintWid );
        CurrFootprintStr := TempStrg + StringOfChar( ' ', FootprintWid - Length( TempStrg ) ); // Pad if needed

        // Exactly fill the Physical Designator field
        TempStrg := Trim( CurrPart.DM_PhysicalDesignator );
        If ( Length( TempStrg ) > PhysicalDesWid) // If too long
        Then TempStrg :=  Copy( TempStrg, 1, PhysicalDesWid );
        CurrPhysDesStr := TempStrg + StringOfChar( ' ', PhysicalDesWid - Length( TempStrg ) ); // Pad if needed

        // Exactly fill the Stuffed flag field
        If Stuffed( CurrPart.DM_PhysicalDesignator )
        Then CurrStuffedStr   := 'Y'
        Else CurrStuffedStr   := 'N';

        // Format the collected data
        FormatStr :=
            CurrLibRefStr
          + CurrSortStr
          + CurrDescrStr
          + CurrFootprintStr
          + CurrPhysDesStr
          + CurrStuffedStr;

        // Add it to the string list
        CompList.Add := FormatStr;
    End; // For CompIndex := 0 To CompCount - 1 Do

    // Sort the string list (ASCII Ordered left to right)
    CompList.Sort;

    // Record how many records ar in the list for later processing control
    CompListCount := CompList.Count;

    // Hide Progress Bar
    ProgressBar.Visible  := False;

End; // procedure FetchComponents( Dummy: Integer );
{...............................................................................}

{...............................................................................}
{ PROCEDURE LoadParameterNames( Dummy: Integer ) reads the Flattened schematic  }
{ sheet created by compiling the project to extract a list parameter names and  }
{ adds them the the Parameter Drop-Down control for choosing by the user.       }
{...............................................................................}
procedure LoadParameterNames( Dummy: Integer );
Var
    CompCount             : Integer;             // The Number of Components in the Flattened Document
    CompIndex             : Integer;             // An Index for pulling out components
    ParmCount             : Integer;             // The Number of Parameters in a Component
    ParmIndex             : Integer;             // An Index for pulling out parameters
    CurrParm              : IParameter;          // An interface handle to the current parameter
    NameCount             : Integer;             // The Number of parameter Names found
    NameIndex             : Integer;             // An Index for pulling out parameter Names
    DNIIndex              : Integer;             // The Index for the parameter named "DNI"
    NameList              : TStringList;         // Exapndable parameter Name list
Begin

    // Fetch the Flattened schematic sheet document.  This is a fictitious document
    // generated when the project is compiled containing all components from all
    // sheets.  This is much more useful for making lists of everything than rummaging
    // through all the sheets one at a time.  This sheet is not graphical in that
    // it cannot be viewed like a schematic, but you can view what's in it by using
    // the Navigator panel.
    FlattenedDoc := CurrProject.DM_DocumentFlattened;

    // If we couldn't get the flattened sheet, then most likely the project has
    // not been compiled recently
    If (FlattenedDoc = Nil) Then
    Begin
        // First try compiling the project
        AddStringParameter( 'Action', 'Compile' );
        AddStringParameter( 'ObjectKind', 'Project' );
        RunProcess( 'WorkspaceManager:Compile' );

        // Try Again to open the flattened document
        FlattenedDoc := CurrProject.DM_DocumentFlattened;
        If (FlattenedDoc = Nil) Then
        Begin
            ShowMessage('NOTICE: Compile the Project before Running this report.');
            Close;
            Exit;
        End; // If (FlattenedDoc = Nil) Then
    End; // If (FlattenedDoc = Nil) Then

    // Create an expandable string list to hold parameter Names
    NameList := TStringList.Create;

    // Let the TStringList automatically sort itself and remove duplicates as it is created
    NameList.Sorted := True;
    NameList.Duplicates := dupIgnore;

    // Now that we have the flattened document, check how many components are in it
    CompCount := FlattenedDoc.DM_ComponentCount;

    // Walk through every component one-by-one so we can look at it's parameter names
    For CompIndex := 0 To CompCount - 1 Do
    Begin
        // Fetch a Component
        CurrComponent := FlattenedDoc.DM_Components[ CompIndex ];

        // Determine how many parameters it has
        ParmCount := CurrComponent.DM_ParameterCount;

        // Walk through every parameter of this component one-by-one
        For ParmIndex := 0 To ParmCount - 1 Do
        Begin
            // Fetch one of the component's Parameters
            CurrParm := CurrComponent.DM_Parameters( ParmIndex );

            // Add it's name to the string list
            NameList.Add := CurrParm.DM_Name;
        End; // For ParmIndex := 0 To ParmCount - 1 Do

    End; // CompIndex := 0 To CompCount - 1 Do

    // Determine how many records are in the parameter list
    NameCount := NameList.Count;

    // Add all parameter names in this list to the Parameters Combo-Box
    // and also default to the parameter named DNI if it exists
    DNIIndex := 0;
    For NameIndex := 0 To NameCount - 1 Do
    Begin
        If (NameList.Strings[ NameIndex ] = 'DNI') Then DNIIndex := NameIndex;
        ParametersComboBox.Items.Add( NameList.Strings[ NameIndex ] );
    End;

    // Choose the entry to start with
    ParametersComboBox.ItemIndex := DNIIndex;

    // Free up the Parameter Name List as we no longer need it
    NameList.Free;
End;
{...............................................................................}

{...............................................................................}
{ FUNCTION HasNoCommas( TestStrg : String ) : Boolean returns True if there are }
{ no commas in the string.                                                      }
{...............................................................................}
function HasNoCommas( TestStrg : String ) : Boolean;
Var
    TempStrg              : String;  // A temporary string
Begin
    TempStrg := StringReplace( TestStrg, ',', '', MkSet( rfReplaceAll, rfIgnoreCase ) );
    If (Length( TempStrg ) <> Length( TestStrg ) )
    Then Result := False
    Else Result := True;
End;
{...............................................................................}

{...............................................................................}
{ PROCEDURE SetupProjectVariant( Dummy: Integer ) finds and sets-up the Project }
{ Variant with a description matching that chosen in the Variants Combo-Box.    }
{...............................................................................}
procedure SetupProjectVariant( Dummy: Integer );
Var
    ProjVarIndex          : Integer;             // Index for iterating through variants
    TempVariant           : IProjectVariant;     // A temporary Handle for a ProjectVariant
Begin
    // Determine how many ProjectVariants are defined within this focussed Project
    ProjectVariantCount := CurrProject.DM_ProjectVariantCount;
    ProjectVariant := Nil;

    // Find the Project Variant matching the Variants Combo-Box
    For ProjVarIndex := 0 To ProjectVariantCount - 1 Do
    Begin
        // Fetch the currently indexed project Assembly Variant
        TempVariant := CurrProject.DM_ProjectVariants[ ProjVarIndex ];

        // See if the Description matches that in the Variants Combo-Box
        If (VariantName = TempVariant.DM_Description)
        Then ProjectVariant := CurrProject.DM_ProjectVariants[ ProjVarIndex ];
    End;
End;
{...............................................................................}

{...............................................................................}
{ Function PerformSubstitutions( CurrString : String ) is used to insert vari-  }
{ ables such as dat and time into strings at the location of predefined markers.}
{ Markers supported so far are: <DATE>, <TIME>, <COMPANY>, <ASSY>, and <REV>.   }
{ Date and time are read from the system when the OK button is clicked.  Other  }
{ values are determined when the report is configured, usually from project par-}
{ ameters.                                                                      }
{...............................................................................}
Function PerformSubstitutions( CurrString : String ) : String;
Begin
    // Search for and replace all occurrances of special strings regardless of case
    CurrString := StringReplace( CurrString, '<DATE>',    DateStr,    MkSet( rfReplaceAll, rfIgnoreCase ) );
    CurrString := StringReplace( CurrString, '<TIME>',    TimeStr,    MkSet( rfReplaceAll, rfIgnoreCase ) );
    CurrString := StringReplace( CurrString, '<COMPANY>', CompanyNam, MkSet( rfReplaceAll, rfIgnoreCase ) );
    CurrString := StringReplace( CurrString, '<TITLE>',   TitleStr,   MkSet( rfReplaceAll, rfIgnoreCase ) );
    CurrString := StringReplace( CurrString, '<ASSY>',    AssyNumber, MkSet( rfReplaceAll, rfIgnoreCase ) );
    CurrString := StringReplace( CurrString, '<REV>',     RevNumber,  MkSet( rfReplaceAll, rfIgnoreCase ) );

    // Return the modified string
    Result := CurrString;
End;
{...............................................................................}

{...............................................................................}
{ PROCEDURE procedure ClearPhysDesStrings( Dummy: Integer ) clears the PhysDes  }
{ Strings so there are no entries.                                              }
{...............................................................................}
procedure ClearPhysDesStrings( Dummy: Integer );
Begin
    StuffedPhysDesString := '';
    NoStuffPhysDesString := '';
End; // procedure ClearPhysDesStrings( Dummy: Integer )
{...............................................................................}

{...............................................................................}
{ PROCEDURE procedure AddToStuffedPhysDesString( PhysDes: String ) adds another }
{ Designator into the StuffedPhysDesString for later reporting.                 }
{...............................................................................}
procedure AddToStuffedPhysDesString( PhysDes: String );
Begin
    If (Length( StuffedPhysDesString ) = 0)
    Then StuffedPhysDesString := PhysDes
    Else StuffedPhysDesString := StuffedPhysDesString + ',' + PhysDes;
End; // procedure AddToStuffedPhysDesString( PhysDes: String );
{...............................................................................}

{...............................................................................}
{ PROCEDURE procedure AddToNoStuffPhysDesString( PhysDes: String ) adds another }
{ Designator into the NoStuffPhysDesString for later reporting.                 }
{...............................................................................}
procedure AddToNoStuffPhysDesString( PhysDes: String );
Begin
    If (Length( NoStuffPhysDesString ) = 0)
    Then NoStuffPhysDesString := 'DNI: ' + PhysDes
    Else NoStuffPhysDesString := NoStuffPhysDesString + ',' + PhysDes;
End; // procedure AddToNoStuffPhysDesString( PhysDes: String );
{...............................................................................}

{...............................................................................}
{ PROCEDURE ConfigureAgileReport( Dummy: Integer ) parses all headers and con-  }
{ figuration strings.  From that, it configures all the variables necessary to  }
{ generate the Agile report.  Control is done through adding Project >> Project }
{ Options >> Parameters.  The following are supported:                          }
{ BOMAssyNum       -- Usually required to get the AssyNumber in the Header      }
{ BOMAgileHeader1  -- Usually not required                                      }
{ BOMAgileDetail1  -- Usually not required                                      }
{ BOMAgileSuppress -- Usually not required (Suppresses last col if empty)       }
{ BOMAgileNoQuotes -- Usually not required (Suppresses quoting empty columns    }
{...............................................................................}
procedure ConfigureAgileReport( Dummy: Integer );
Var
    ParmCount             : Integer;             // The number of parameters
    ParmIndex             : Integer;             // An index for the current parameter
    CurrParm              : IParameter;          // The current parameter
Begin
    // First, setup the project assembly variant matching the Variants Combo-Box
    SetupProjectVariant( 0 );

    // Set up the default strings
    HeaderStr1 := 'Parent,Child,Qty,Find Num,Ref Des,BoM Notes';
    AssyNumber := '<1234567890ABC>';
    DetailStr1 := '<ASSY>,D120031,0,0,,Lead-Free Processing Specification';

    // Initialize default controls
    SuppressNulLastCol := False;
    SuppressQuoteWoCommas := False;

    // So we can use an alternate method: Scan the project file
    ParmCount := CurrProject.DM_ParameterCount;
    For ParmIndex := 0 To ParmCount - 1 Do
    Begin
        CurrParm := CurrProject.DM_Parameters( ParmIndex );
        If (CurrParm.DM_Name = 'BOMAssyNum')       Then AssyNumber := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMAgileHeader1')  Then HeaderStr1 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMAgileDetail1')  Then DetailStr1 := CurrParm.DM_Value;

        // Use BOMAgileSuppress to suppress the last column when empty
        If (CurrParm.DM_Name = 'BOMAgileSuppress')
        Then If (Copy( CurrParm.DM_Value, 1, 1) <> 'N') Then SuppressNulLastCol := True;

        // Use BOMAgileNoQuotes to suppress quoting fields w/o commas
        If (CurrParm.DM_Name = 'BOMAgileNoQuotes')
        Then If (Copy( CurrParm.DM_Value, 1, 1) <> 'N') Then SuppressQuoteWoCommas := True;

    End;

    // Now substitute variables into the header strings
    DetailStr1 := PerformSubstitutions( DetailStr1 );
End; // procedure ConfigureAgileReport( Dummy: Integer );
{...............................................................................}

{...............................................................................}
{ PROCEDURE: CreateAgileReport( Dummy: Integer ) creates, formats, and writes an}
{ Agile formatted BOM out to a file in the project directory, adds it to the    }
{ project under Generated >> Text Documents, and opens it up for display.  The  }
{ file is named the same as the project except with an extension of *.AgileBOM  }
{...............................................................................}
procedure CreateAgileReport( Dummy: Integer );
Var
    LineIndex             : Integer;             // An Index for pulling out components
    CompListIndex         : Integer;             // An index for strings in CompList
    AgileReport           : TStringList;         // Expandable String List for Report
    AgileReportFileName   : String;              // String containing the name of the report file
    ReportDocument        : IServerDocument;     // An Interface to the Report Document
    FormattedLine         : String;              // A String for formatting on report line
    StuffQty              : Integer;             // Stuffed Quantity of the current part
    CurrStrg              : String;              // A string for the current component record
    PrevStrg              : String;              // A string for the previous component record
    TempStr2              : String;              // A temporary string for column formatting
    CurrPhysDes           : String;              // A string for the current Physical Designator
    PrevPhysDes           : String;              // A string for the previous Physical Designator
    PhysDesIndex          : Integer;             // An Index for wrapped PhysDes Arrays
    MaxPhysDes            : Integer;             // The Max Count of Wrapped Designator lines
    CurrLibRef            : String;              // A string for the current Library Reference
    PrevLibRef            : String;              // A string for the current Library Reference
    LibRefCount           : Integer;             // A count of the number of Library References outputted
    FindNumScale          : Integer;             // A scaling factor for counting up
    ColIndex              : Integer;             // An index for the current column
    IsStuffed             : Boolean;             // A flag indicating whether or not a component is stuffed
Begin
    // Configure all columns and variables from project parameters or defaults
    ConfigureAgileReport( 0 );

    // Create a string list into which we can write the report
    AgileReport := TStringList.Create;

    // Initalize the Library Reference counter for Agile Report
    LibRefCount := 0;   // First "Find" number
    FindNumScale := 10; // Increment between "Find" numbers

    // Add the already-formatted header and detail lines into the report (if any)
    If (Length( Trim( HeaderStr1 ) ) > 0) Then AgileReport.Add( HeaderStr1 );
    If (Length( Trim( DetailStr1 ) ) > 0) Then
    Begin
        AgileReport.Add( DetailStr1 );
        LibRefCount := LibRefCount + 1;
    End;

    // Clear out the lists of Stuffed and Non-Stuffed Physical Designators
    ClearPhysDesStrings( 0 );

    // Set Up Progress Bar
    ProgressBar.Min      := 0;
    ProgressBar.Max      := CompListCount - 1;
    ProgressBar.Step     := 1;
    ProgressBar.Position := 0;
    ProgressBar.Visible  := True;

    // Format a line for each line in the component list
    For CompListIndex := 0 To CompListCount - 1 Do
    Begin
        // Update Progress Bar
        ProgressBar.Position := CompListIndex;

        // Save the record, designator, and library reference from the previous pass
        PrevStrg := CurrStrg;
        PrevLibRef := CurrLibRef;
        PrevPhysDes := CurrPhysDes;

        // Get the record, designator, and library reference for the current pass
        CurrStrg := CompList.Strings[ CompListIndex ];
        CurrLibRef  := Trim( Copy( CurrStrg, LibRefStart, LibRefWid ) );
        CurrPhysDes := Trim( Copy( CurrStrg, PhysicalDesStart, PhysicalDesWid ) );

        // First Pass only: Init previous variable to match current variables
        If (CompListIndex = 0)
        Then
            Begin
                PrevStrg := CurrStrg;
                PrevLibRef := CurrLibRef;
                PrevPhysDes := CurrPhysDes;
            End; // If (CompListIndex = 0) Then

        //  If Library Reference is unchanged, we need to consolidate it with previous records
        If (PrevLibRef = CurrLibRef)
        Then
            Begin
                // Determine if Current component is stuffed or not
                If (Copy( CurrStrg, StuffedStart, StuffedWid ) = 'Y')
                Then IsStuffed := True
                Else IsStuffed := False;

                // If so, Increment the Stuffed Quantity
                If IsStuffed Then StuffQty := StuffQty + 1;

                // Consolidate Here
                If IsStuffed
                Then AddToStuffedPhysDesString( CurrPhysDes )
                Else AddToNoStuffPhysDesString( CurrPhysDes );
            End
        Else // Otherwise, we are now on a new part and so need to print the previous record
            Begin
                // Formatted the line string to be outputSuppressQuoteWoCommas
                FormattedLine := '';

                // Format output of the assembly number
                TempStr2 := AssyNumber;
                If ((SuppressQuoteWoCommas = True) And (HasNoCommas( TempStr2 ) = True))
                Then FormattedLine := FormattedLine        + TempStr2
                Else FormattedLine := FormattedLine + '"'  + TempStr2 + '"';

                // Format output of the library reference
                TempStr2 := Trim( Copy( PrevStrg, LibRefStart, LibRefWid ) );
                If ((SuppressQuoteWoCommas = True) And (HasNoCommas( TempStr2 ) = True))
                Then FormattedLine := FormattedLine + ','  + TempStr2
                Else FormattedLine := FormattedLine + ',"' + TempStr2 + '"';

                // Format output of the stuffed quantity
                FormattedLine := FormattedLine + ',' + IntToStr( StuffQty );

                // Format output of the find number
                FormattedLine := FormattedLine + ',' + IntToStr( LibRefCount * FindNumScale );

                // Format output of the stuffed designators list
                TempStr2 := Trim( StuffedPhysDesString );
                If ((SuppressQuoteWoCommas = True) And (HasNoCommas( TempStr2 ) = True))
                Then FormattedLine := FormattedLine + ','  + TempStr2
                Else FormattedLine := FormattedLine + ',"' + TempStr2 + '"';

                // Format output of the non-stuffed designators list
                TempStr2 := Trim( NoStuffPhysDesString );
                If ( (Length( TempStr2 ) > 0) Or (SuppressNulLastCol = False) ) Then
                Begin
                    If ((SuppressQuoteWoCommas = True) And (HasNoCommas( TempStr2 ) = True))
                    Then FormattedLine := FormattedLine + ','  + TempStr2
                    Else FormattedLine := FormattedLine + ',"' + TempStr2 + '"';
                End;

                // Add this formatted line to the report
                AgileReport.Add( FormattedLine );
                LibRefCount := LibRefCount + 1;

                // Clear out the wrapped lists of Stuffed and Non-Stuffed Physical Designators
                ClearPhysDesStrings( 0 );

                // Having printed the previous group we need to reset the stuffed quantity counter
                // Determine if Current component is stuffed or not
                If (Copy( CurrStrg, StuffedStart, StuffedWid ) = 'Y')
                Then IsStuffed := True
                Else IsStuffed := False;

                If IsStuffed
                Then StuffQty := 1
                Else StuffQty := 0;

                If IsStuffed
                Then AddToStuffedPhysDesString( CurrPhysDes )
                Else AddToNoStuffPhysDesString( CurrPhysDes );

            End; // If (PrevLibRef = CurrLibRef) Else
    End; // For CompListIndex := 0 To CompListCount - 1 Do

    // Hide Progress Bar
    ProgressBar.Visible := False;

    // All components have been reported, so name the Agile BOM Report file
    AgileReportFileName := GetOutputFileNameWithExtension('.AgileBOM');

    // Save the report to the specified file name and eliminate the string list
    AgileReport.SaveToFile( AgileReportFileName );
    AgileReport.Free;

    If AddToProject Then
        // Add the report file to the current project (under Generated >> Text Documents)
        CurrProject.DM_AddGeneratedDocument( AgileReportFileName );

    If OpenOutputs Then
    Begin
        // Try to open the report document again using the OpenDocument process of the Client server
        ReportDocument := Client.OpenDocument( 'Text', AgileReportFileName );

        //  If we get it opened up again then show it to the users
        If (ReportDocument <> Nil) Then Client.ShowDocument( ReportDocument );
    End;

End; // procedure CreateAgileReport( Dummy: Integer );
{...............................................................................}

{...............................................................................}
{ PROCEDURE procedure ClearPhysDesLists( Dummy: Integer ) clears the PhysDes    }
{ Lists so there are no entries.                                                }
{...............................................................................}
procedure ClearPhysDesLists( Dummy: Integer );
Begin
    StuffedPhysDesCount := 1;
    StuffedPhysDesList[ StuffedPhysDesCount ] := '';
    NoStuffPhysDesCount := 1;
    NoStuffPhysDesList[ NoStuffPhysDesCount ] := '';
End; // procedure ClearPhysDesLists( Dummy: Integer );
{...............................................................................}

{...............................................................................}
{ PROCEDURE procedure AddToStuffedPhysDesList( PhysDes: String ) adds another   }
{ Designator into the StuffedPhysDesList for later reporting.                   }
{...............................................................................}
procedure AddToStuffedPhysDesList( PhysDes: String );
Var
    TrialLen                   : Integer;
Begin
    TrialLen :=
        Length( StuffedPhysDesList[ StuffedPhysDesCount ] )
      + 1 // for preceeding comma
      + Length( PhysDes );
    If (TrialLen <= StuffedPhysDesColWid)
    Then // It will fit so put it in.  Omit comma at beginning of field
        Begin
            If (Length( StuffedPhysDesList[ StuffedPhysDesCount ] ) = 0)
            Then StuffedPhysDesList[ StuffedPhysDesCount ] :=
                 StuffedPhysDesList[ StuffedPhysDesCount ] + PhysDes
            Else StuffedPhysDesList[ StuffedPhysDesCount ] :=
                 StuffedPhysDesList[ StuffedPhysDesCount ] + ',' + PhysDes;
        End
    Else  // It won't fit so put it in the next record (sans comma)
        Begin
            StuffedPhysDesCount := StuffedPhysDesCount + 1;
            StuffedPhysDesList[ StuffedPhysDesCount ] := PhysDes;
        End;
End; // procedure AddToStuffedPhysDesList( PhysDes: String );
{...............................................................................}

{...............................................................................}
{ PROCEDURE procedure AddToNoStuffPhysDesList( PhysDes: String ) adds another   }
{ Designator into the NoStuffPhysDesList for later reporting.                   }
{...............................................................................}
procedure AddToNoStuffPhysDesList( PhysDes: String );
Var
    TrialLen                   : Integer;
Begin
    TrialLen :=
        Length( NoStuffPhysDesList[ NoStuffPhysDesCount ] )
      + 1 // for preceeding comma
      + Length( PhysDes );
    If (TrialLen <= NoStuffPhysDesColWid)
    Then // It will fit so put it in.  Omit comma at beginning of field
        Begin
            If (Length( NoStuffPhysDesList[ NoStuffPhysDesCount ] ) = 0)
            Then NoStuffPhysDesList[ NoStuffPhysDesCount ] :=
                 NoStuffPhysDesList[ NoStuffPhysDesCount ] + PhysDes
            Else NoStuffPhysDesList[ NoStuffPhysDesCount ] :=
                 NoStuffPhysDesList[ NoStuffPhysDesCount ] + ',' + PhysDes;
        End
    Else  // It won't fit so put it in the next record (sans comma)
        Begin
            NoStuffPhysDesCount := NoStuffPhysDesCount + 1;
            NoStuffPhysDesList[ NoStuffPhysDesCount ] := PhysDes;
        End;
End; // procedure AddToNoStuffPhysDesList( PhysDes: String );
{...............................................................................}

{...............................................................................}
{ PROCEDURE ConfigureEngineeringReport( Dummy: Integer ) parses all headers and }
{ configurations strings.  Form that, it configures all the variables necessary }
{ to generate the engineering report.  Control is done through adding Project >>}
{ Project Options >> Parameters.  The following are supported:                  }
{ BOMCompany      -- Usually required to get the company name inthe header      }
{ BOMAssyNum      -- Usually required to get the AssyNumber in the Header       }
{ BOMRevision     -- Usually required to get the Revision in the Header         }
{ BOMTitle        -- Usually required to get the design's Title in the Header   }
{ BOMEngrgHeader1 -- Usually not required                                       }
{ BOMEngrgHeader2 -- Usually not required                                       }
{ BOMEngrgHeader3 -- Usually not required                                       }
{ BOMEngrgHeader4 -- Usually not required                                       }
{ BOMEngrgHeader5 -- Usually not required                                       }
{ BOMEngrgHeader6 -- Usually not required                                       }
{ BOMEngrgLineFormat -- Usually not required                                    }
{...............................................................................}
procedure ConfigureEngineeringReport( Dummy: Integer );
Var
    OptionsStorage : IOptionsStorage;
    ParmCount             : Integer;             // The number of parameters
    ParmIndex             : Integer;             // An index for the current parameter
    CurrParm              : IParameter;          // The current parameter
    CharCount             : Integer;             // The number of chars in the line format string
    CharIndex             : Integer;             // An index for the current char
    CurrChar              : String;              // One character from the line format string
    PrevChar              : String;              // The previous character
    ColIndex              : Integer;             // An index for the current column
Begin
    // First, setup the project assembly variant matching the Variants Combo-Box
    SetupProjectVariant( 0 );

    // Set up the default header and line format strings
    HeaderStr1 := '<COMPANY> ENGINEERING BOM FOR PWA: <ASSY>-<REV>   TITLE: <TITLE>';
    HeaderStr2 := 'PROPRIETARY AND CONFIDENTIAL, <COMPANY>';
    HeaderStr3 := 'CREATED:  <DATE>  <TIME>';
    HeaderStr4 := '';
    HeaderStr5 := 'FIND  COHR P/N         DESCRIPTION             QUAN  REFERENCE            DNI REFERENCE        FOOTPRINT';
    HeaderStr6 := '----  ---------------  ----------------------  ----  -------------------  -------------------  ---------------------------------';
    LineFormat := 'LLLLBBPPPPPPPPPPPPPPPPBDDDDDDDDDDDDDDDDDDDDDDDDQQQQBBSSSSSSSSSSSSSSSSSSSBBNNNNNNNNNNNNNNNNNNNBBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
    CompanyNam := 'ALTIUM, INC.';
    AssyNumber := '<1234567890ABC>';

    // So we use an alternate method: Scan the project file
    ParmCount := CurrProject.DM_ParameterCount;
    For ParmIndex := 0 To ParmCount - 1 Do
    Begin
        CurrParm := CurrProject.DM_Parameters( ParmIndex );
        If (CurrParm.DM_Name = 'BOMCompany')      Then CompanyNam := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMAssyNum')      Then AssyNumber := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMRevision')     Then RevNumber  := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMTitle')        Then TitleStr   := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMEngrgHeader1') Then HeaderStr1 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMEngrgHeader2') Then HeaderStr2 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMEngrgHeader3') Then HeaderStr3 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMEngrgHeader4') Then HeaderStr4 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMEngrgHeader5') Then HeaderStr5 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMEngrgHeader6') Then HeaderStr6 := CurrParm.DM_Value;
        If (CurrParm.DM_Name = 'BOMLineFormat')   Then LineFormat := CurrParm.DM_Value;
    End;

    // Now substitute variables into the header strings
    HeaderStr1 := PerformSubstitutions( HeaderStr1 );
    HeaderStr2 := PerformSubstitutions( HeaderStr2 );
    HeaderStr3 := PerformSubstitutions( HeaderStr3 );
    HeaderStr4 := PerformSubstitutions( HeaderStr4 );
    HeaderStr5 := PerformSubstitutions( HeaderStr5 );
    HeaderStr6 := PerformSubstitutions( HeaderStr6 );

    // Now its time to parse the line format string
    // Set up column width defaults for each field
    ColIndex := 1;
    MaxCols := 99;
    ColWidth[ ColIndex ] := 0;
    ColType[ ColIndex ] := Copy( LineFormat, 1, 1 );
    CharCount := Length( LineFormat );
    CurrChar := Copy( LineFormat, CharIndex, 1 );
    For CharIndex := 1 To CharCount Do
    Begin
        PrevChar := CurrChar;
        CurrChar := Copy( LineFormat, CharIndex, 1 );
        If (ColIndex <= MaxCols) Then
        Begin
            If (CurrChar = PrevChar) Then
                Begin
                    ColWidth[ ColIndex ] := ColWidth[ ColIndex ] + 1;
                    // Record the widths of the Stuffed and Non-stuffed Designator report columns
                    If (ColType[ ColIndex ] = 'S') Then StuffedPhysDesColWid := ColWidth[ ColIndex ];
                    If (ColType[ ColIndex ] = 'N') Then NoStuffPhysDesColWid := ColWidth[ ColIndex ];
                End
            Else
                Begin
                    // First, record the widths of the Stuffed and Non-stuffed Designator report columns
                    If (ColType[ ColIndex ] = 'S') Then StuffedPhysDesColWid := ColWidth[ ColIndex ];
                    If (ColType[ ColIndex ] = 'N') Then NoStuffPhysDesColWid := ColWidth[ ColIndex ];

                    // Then switch to the next new column
                    ColIndex := ColIndex + 1;
                    ColWidth[ ColIndex ] := 1;
                    ColType[ ColIndex ] := Copy( LineFormat, CharIndex, 1 );
                End;
        End;
    End;
    LastCol := ColIndex;

    // Calculate all start and end columns for each field
    ColStart[ 1 ] := 1;
    ColEnd[   1 ] := ColStart[ 1 ] + ColWidth[ 1 ];
    For ColIndex := 2 To LastCol Do
    Begin
        ColStart[ ColIndex ] := ColStart[ ColIndex - 1 ] + ColWidth[ ColIndex - 1 ];
        ColEnd[   ColIndex ] := ColStart[ ColIndex     ] + ColWidth[ ColIndex     ];
    End;

End; // procedure ConfigureEngineeringReport( Dummy: Integer );
{...............................................................................}

{...............................................................................}
{ PROCEDURE: CreateEngineeringReport( Dummy: Integer ) creates, formats, and    }
{ writes the Engineering BOM out to a file in the project directory, adds it to }
{ the project under Generated >> Text Documents, & opens it up for display.  The}
{ file is named the same as the project except with an extension of *.EngrBOM   }
{...............................................................................}
procedure CreateEngineeringReport( Dummy: Integer );
Var
    LineIndex             : Integer;             // An Index for pulling out components
    CompListIndex         : Integer;             // An index for strings in CompList
    EngineeringReport     : TStringList;         // Expandable String List for Report
    EngrgReportFileName   : String;              // String containing the name of the report file
    ReportDocument        : IServerDocument;     // An Interface to the Report Document
    FormattedLine         : String;              // A String for formatting on report line
    StuffQty              : Integer;             // Stuffed Quantity of the current part
    CurrStrg              : String;              // A string for the current component record
    PrevStrg              : String;              // A string for the previous component record
    TempStr2              : String;              // A temporary string for column formatting
    CurrPhysDes           : String;              // A string for the current Physical Designator
    PrevPhysDes           : String;              // A string for the previous Physical Designator
    PhysDesIndex          : Integer;             // An Index for wrapped PhysDes Arrays
    MaxPhysDes            : Integer;             // The Max Count of Wrapped Designator lines
    CurrLibRef            : String;              // A string for the current Library Reference
    PrevLibRef            : String;              // A string for the current Library Reference
    LibRefCount           : Integer;             // A count of the number of Library References outputted
    FindNumScale          : Integer;             // A scaling factor for counting up
    ColIndex              : Integer;             // An index for the current column
    IsStuffed             : Boolean;             // A flag indicating whether or not a component is stuffed
Begin
    // Configure all columns and variables from project parameters or defaults
    ConfigureEngineeringReport( 0 );

    // Create a string list into which we can write the report
    EngineeringReport := TStringList.Create;

    // Add the already-formatted header lines into the report
    EngineeringReport.Add( HeaderStr1 );
    EngineeringReport.Add( HeaderStr2 );
    EngineeringReport.Add( HeaderStr3 );
    EngineeringReport.Add( HeaderStr4 );
    // If the last header line or pair of header lines are blank, they will be omitted
    If ((Length( Trim( HeaderStr6 ) ) > 0) Or (Length( Trim( HeaderStr6 ) ) > 0))
    Then EngineeringReport.Add( HeaderStr5 );
    If (Length( Trim( HeaderStr6 ) ) > 0)
    Then EngineeringReport.Add( HeaderStr6 );

    // Initalize the Library Reference counter for Engineering Report
    LibRefCount := 1;
    FindNumScale := 1;

    // Clear out the wrapped lists of Stuffed and Non-Stuffed Physical Designators
    ClearPhysDesLists( 0 );

    // Set Up Progress Bar
    ProgressBar.Min      := 0;
    ProgressBar.Max      := CompListCount - 1;
    ProgressBar.Step     := 1;
    ProgressBar.Position := 0;
    ProgressBar.Visible  := True;

    // Format a line for each line in the component list
    For CompListIndex := 0 To CompListCount - 1 Do
    Begin
        // Update Progress Bar
        ProgressBar.Position := CompListIndex;

        // Save the record, designator, and library reference from the previous pass
        PrevStrg := CurrStrg;
        PrevLibRef := CurrLibRef;
        PrevPhysDes := CurrPhysDes;

        // Get the record, designator, and library reference for the current pass
        CurrStrg := CompList.Strings[ CompListIndex ];
        CurrLibRef  := Trim( Copy( CurrStrg, LibRefStart, LibRefWid ) );
        CurrPhysDes := Trim( Copy( CurrStrg, PhysicalDesStart, PhysicalDesWid ) );

        // First Pass only: Init previous variable to match current variables
        If (CompListIndex = 0)
        Then
            Begin
                PrevStrg := CurrStrg;
                PrevLibRef := CurrLibRef;
                PrevPhysDes := CurrPhysDes;
            End; // If (CompListIndex = 0) Then

        //  If the library reference is the same, we need to consolidate it with
        // previous records
        If (PrevLibRef = CurrLibRef)
        Then
            Begin
                // Determine if Current component is stuffed or not
                If (Copy( CurrStrg, StuffedStart, StuffedWid ) = 'Y')
                Then IsStuffed := True
                Else IsStuffed := False;

                // If so, Increment the Stuffed Quantity
                If IsStuffed Then StuffQty := StuffQty + 1;

                // Consolidate Here
                If IsStuffed
                Then AddToStuffedPhysDesList( CurrPhysDes )
                Else AddToNoStuffPhysDesList( CurrPhysDes );
            End
        Else // Otherwise, we need to print the previous record
            Begin
                // Clear the current formatted line string
                FormattedLine := '';

                // Append each column into it per the formatting definitions
                For ColIndex := 1 To LastCol Do
                Begin

                    // Handle blank columns (B)
                    If ColType[ ColIndex ] = 'B' Then
                    Begin
                        TempStr2 := StringOfChar( ' ', ColWidth[ ColIndex ] );
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'B' Then

                    // Handle Line number columns (L)
                    If ColType[ ColIndex ] = 'L' Then
                    Begin
                        TempStr2 := IntToStr( LibRefCount * FindNumScale );
                        TempStr2 := StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) ) + TempStr2;
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'L' Then

                    // Handle Part number (LibraryRef) columns (P)
                    If ColType[ ColIndex ] = 'P' Then
                    Begin
                        If (ColWidth[ ColIndex ] < LibRefWid)
                        Then TempStr2 := Copy( PrevStrg, LibRefStart, ColWidth[ ColIndex ] )
                        Else TempStr2 := Copy( PrevStrg, LibRefStart, LibRefWid );
                        TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'P' Then

                    // Handle Description columns (D)
                    If ColType[ ColIndex ] = 'D' Then
                    Begin
                        If (ColWidth[ ColIndex ] < DescriptionWid)
                        Then TempStr2 := Copy( PrevStrg, DescriptionStart, ColWidth[ ColIndex ] )
                        Else TempStr2 := Copy( PrevStrg, DescriptionStart, DescriptionWid );
                        TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'D' Then

                    // Handle stuffed Quantity columns (Q)
                    If ColType[ ColIndex ] = 'Q' Then
                    Begin
                        TempStr2 := IntToStr( StuffQty );
                        TempStr2 := StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) ) + TempStr2;
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'Q' Then

                    // Handle Stuffed designator list columns (S)
                    If ColType[ ColIndex ] = 'S' Then
                    Begin
                        TempStr2 := StuffedPhysDesList[ 1 ];
                        TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'S' Then

                    // Handle Non-stuffed designator list columns (N)
                    If ColType[ ColIndex ] = 'N' Then
                    Begin
                        TempStr2 := NoStuffPhysDesList[ 1 ];
                        TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'N' Then

                    // Handle Footprint columns (F)
                    If ColType[ ColIndex ] = 'F' Then
                    Begin
                        If (ColWidth[ ColIndex ] < FootprintWid)
                        Then TempStr2 := Copy( PrevStrg, FootprintStart, ColWidth[ ColIndex ] )
                        Else TempStr2 := Copy( PrevStrg, FootprintStart, FootprintWid );
                        TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                        FormattedLine := FormattedLine + TempStr2;
                    End; // If ColType[ ColIndex ] = 'F' Then

                End; // For ColIndex := 1 To LastCol Do

                // Add this formatted line to the report
                EngineeringReport.Add( FormattedLine );
                LibRefCount := LibRefCount + 1;

                // Output wrapped followup lines as follows:

                // Determine the last designator records we need to access from the arrays
                If (StuffedPhysDesCount > NoStuffPhysDesCount)
                Then MaxPhysDes := StuffedPhysDesCount
                Else MaxPhysDes := NoStuffPhysDesCount;

                // The first rows were already used in the formatted line
                // Now we output the rest of the lines with these two columns only
                For PhysDesIndex := 2 To MaxPhysDes Do
                Begin
                    // Clear the current formatted line string
                    FormattedLine := '';

                    // Append each column into it per the formatting definitions
                    For ColIndex := 1 To LastCol Do
                    Begin

                        // Handle Stuffed designator list columns (S)
                        If ColType[ ColIndex ] = 'S' Then
                        Begin
                            If (PhysDesIndex <= StuffedPhysDesCount)
                            Then TempStr2 := StuffedPhysDesList[ PhysDesIndex ]
                            Else TempStr2 := ' ';
                            TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                            FormattedLine := FormattedLine + TempStr2;
                        End; // If ColType[ ColIndex ] = 'S' Then

                        // Handle Non-stuffed designator list columns (N)
                        If ColType[ ColIndex ] = 'N' Then
                        Begin
                            If (PhysDesIndex <= NoStuffPhysDesCount)
                            Then TempStr2 := NoStuffPhysDesList[ PhysDesIndex ]
                            Else TempStr2 := ' ';
                            TempStr2 := Tempstr2 + StringOfChar( ' ', ColWidth[ ColIndex ] - Length( TempStr2 ) );
                            FormattedLine := FormattedLine + TempStr2;
                        End; // If ColType[ ColIndex ] = 'N' Then

                        // Handle everything else with blanks
                        If ((ColType[ ColIndex ] <> 'S') And (ColType[ ColIndex ] <> 'N')) Then
                        Begin
                            TempStr2 := StringOfChar( ' ', ColWidth[ ColIndex ] );
                            FormattedLine := FormattedLine + TempStr2;
                        End; // If ((ColType[ ColIndex ] <> 'S') And (ColType[ ColIndex ] <> 'N')) Then
                    End; // For ColIndex := 1 To LastCol Do

                    // Add this extra formatted line to the report
                    EngineeringReport.Add( FormattedLine );

                End; // For ( PhysDesIndex := 2 To MaxPhysDes Do

                // Clear out the wrapped lists of Stuffed and Non-Stuffed Physical Designators
                ClearPhysDesLists( 0 );

                // Having printed the previous group we need to reset the stuffed quantity counter
                // Determine if Current component is stuffed or not
                If (Copy( CurrStrg, StuffedStart, StuffedWid ) = 'Y')
                Then IsStuffed := True
                Else IsStuffed := False;
                If IsStuffed
                Then StuffQty := 1
                Else StuffQty := 0;
                If IsStuffed
                Then AddToStuffedPhysDesList( CurrPhysDes )
                Else AddToNoStuffPhysDesList( CurrPhysDes );

            End; // If (PrevLibRef = CurrLibRef) Else
    End; // For CompListIndex := 0 To CompListCount - 1 Do

    // Hide Progress Bar
    ProgressBar.Visible := False;

    // All components have been reported, so name the Engineering BOM Report file
    EngrgReportFileName := GetOutputFileNameWithExtension('.EngrBOM');

    // Save the report to the specified file name and eliminate the string list
    EngineeringReport.SaveToFile( EngrgReportFileName );
    EngineeringReport.Free;

    If AddToProject Then
        // Add the report file to the current project (under Generated >> Text Documents)
        CurrProject.DM_AddGeneratedDocument( EngrgReportFileName );

    If OpenOutputs Then
    Begin
        // Try to open the report document again using the OpenDocument process of the Client server
        ReportDocument := Client.OpenDocument( 'Text', EngrgReportFileName );
        //  If we get it opened up again then show it to the users
        If (ReportDocument <> Nil) Then Client.ShowDocument( ReportDocument );
    End;

End; // procedure CreateEngineeringReport( Dummy: Integer );
{...............................................................................}

{...............................................................................}
{ PROCEDURE: ReWriteActionLabel( Dummy: Integer ) examines the states of the    }
{ various controls on the Form and from this information constructs a string    }
{ describing what would be done if the OK button is clicked.  This string is    }
{ displayed on the Form in the Action Label just above the Cancel & OK buttons. }
{...............................................................................}
procedure ReWriteActionLabel( Dummy: Integer );
Var
    // Define a set of strings for defining pieces of a sentence
    StringA : String;
    StringB : String;
    StringC : String;
    StringD : String;
    StringE : String;
    StringF : String;
    StringG : String;
Begin
    GetState_Controls;

    // They are all initialized to a single space
    StringA := ' ';
    StringB := ' ';
    StringC := ' ';
    StringD := ' ';
    StringE := ' ';
    StringF := ' ';
    StringG := ' ';

    // Strings A & B say which BOM(s) will be run based on the two BOM Creation check-boxes
    // If both are unchecked, no BOMs will be created
    If ((CreateAgileBOM = False) And (CreateEngineeringBOM = False))
    Then
        Begin
            // If both are unchecked, no BOMs will be created
            StringA := 'No ';
            StringB := 's';
        End;
    // If only the CreateAgileBOM check-box is checked, only the Agile BOM will be created
    If ((CreateAgileBOM = True ) And (CreateEngineeringBOM = False))
    Then
        Begin
            StringA := 'only Agile ';
            StringB := '';
        End;
    // If only the CreateEngineeringBOM check-box is checked, only the Engineering BOM will be created
    If ((CreateAgileBOM = False) And (CreateEngineeringBOM = True ))
    Then
        Begin
            StringA := 'only Engineering ';
            StringB := '';
        End;
    // If both are checked, both the Agile and Engineering BOMs will be created
    If ((CreateAgileBOM = True ) And (CreateEngineeringBOM = True ))
    Then
        Begin
            StringA := 'both Agile and Engineering ';
            StringB := 's';
        End;

    // The rest of the strings say which Parameter of Variant will define the Variant reported
    // If the UseParameter Radio-Button is selected the Variant reported will be
    // defined by the Parameter selected in the Parameters Combo-Box
    If UseParameters
    Then
        Begin
            StringC := 'Variant';
            StringD := ' using ';
            StringE := ' Parameter: ';
            StringF := ParameterName;
            StringG := '.';
        End;
    // If the UseVariants Radio-Button is selected the Variant reported will be
    // defined by the Variant selected in the Variants Combo-Box
    If UseVariants
    Then
        Begin
            StringC := 'Variant';
            StringD := ' using';
            StringE := ' Variant: ';
            StringF := VariantName;
            StringG := '.';
        End;
    // If the FullyPopulated Radio-Button is selected, no Variant will be used
    // and the fully populated board will be reported
    If FullyPopulated
    Then
        Begin
            StringC := 'Fully Populated';
            StringD := '.';
            StringE := ' ';
            StringF := ' ';
        End;

    // Finally, all the strings are concatenated to produce the Action Label
    // displayed just above the OK and Cancel buttons
    ActionLabel.Caption := 'Create ' + StringA + StringC + ' BOM' + StringB + StringD + StringE + StringF + StringG;
End;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: InitializeProject( Dummy: Integer ) several things.  First, it     }
{ makes sure the Workspace and Focussed Project can be opened and if not exits  }
{ the script.  Second, it loads the variant names for this project into the Var-}
{ iants Combo-Box and select the first one. And third, it re-writes the Action  }
{ label on the Form describing what action will be done when the OK button is   }
{ clicked.                                                                      }
{...............................................................................}
Procedure InitializeProject( Dummy: Integer );
Var
    ProjVarIndex          : Integer;             // Index for iterating through variants
Begin
    // Make sure the current Workspace opens or else quit this script
    CurrWorkSpace := GetWorkSpace;
    If (CurrWorkSpace = Nil) Then Exit;

    // Make sure the currently focussed Project in this Workspace opens or else
    // quit this script
    CurrProject := CurrWorkspace.DM_FocusedProject;
    If CurrProject = Nil Then Exit;

    // Determine how many Assembly Variants are defined within this focussed Project
    ProjectVariantCount := CurrProject.DM_ProjectVariantCount;

    // Process each Project Assembly Variant sequentially
    For ProjVarIndex := 0 To ProjectVariantCount - 1 Do
    Begin
        // Fetch the currently indexed project Assembly Variant
        ProjectVariant := CurrProject.DM_ProjectVariants[ ProjVarIndex ];
        VariantsComboBox.Items.Add( ProjectVariant.DM_Description );
    End;

    // Choose first variant to start with
    VariantsComboBox.ItemIndex := 0;
    ProjectVariant := CurrProject.DM_ProjectVariants[ 0 ];

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
End;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: Initialize does things necessary for the Form to operate properly, }
{ such as making sure the Workspace and Project open, loading the Combo-Boxes   }
{ with the proper choices, and initializing other settings.                     }
{...............................................................................}
procedure Initialize;
begin
    // Open Workspace, Project, Get Variants, etc.
    InitializeProject( 0 );

    // Add Parameters to Parameters ComboBox
    LoadParameterNames( 0 );

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.UseParametersRadioButtonClick(Sender: TObject) is an }
{ event handler for the UseParameters Radio-Button.  When the user clicks on it }
{ this procedure is called.                                                     }
{...............................................................................}
procedure TAgileBOMForm.UseParametersRadioButtonClick(Sender: TObject);
begin
    // When clicked, this radio button become checked and all the others unchecked
    UseParametersRadioButton.Checked := True;
    UseVariantsRadioButton.Checked := False;
    FullyPopulatedRadioButton.Checked := False;

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.ParametersComboBoxChange(Sender: TObject) is an event}
{ handler for the Parameters Combo-Box.  When the user clicks on it, this pro-  }
{ cedure is called.                                                             }
{...............................................................................}
procedure TAgileBOMForm.ParametersComboBoxChange(Sender: TObject);
begin
    // When clicked, the Parameters Combo-Box is automatically activated for selecting.
    // However, we should also check the associated UseParameters radio button
    // and uncheck all the others
    UseParametersRadioButton.Checked := True;
    UseVariantsRadioButton.Checked := False;
    FullyPopulatedRadioButton.Checked := False;

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.UseVariantsRadioButtonClick(Sender: TObject) is an   }
{ event handler for the UseVariants Radio-Button.  When the user clicks on it   }
{ this procedure is called.                                                     }
{...............................................................................}
procedure TAgileBOMForm.UseVariantsRadioButtonClick(Sender: TObject);
begin
    // When clicked, this radio button become checked and all the others unchecked
    UseVariantsRadioButton.Checked := True;
    UseParametersRadioButton.Checked := False;
    FullyPopulatedRadioButton.Checked := False;

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.VariantsComboBoxChange(Sender: TObject) is an event  }
{ handler for the Variants Combo-Box.  When the user clicks on it, this proced- }
{ ure is called.                                                                }
{...............................................................................}
procedure TAgileBOMForm.VariantsComboBoxChange(Sender: TObject);
begin
    // When clicked, the Variants Combo-Box is automatically activated for selecting.
    // However, we should also check the associated UseVariants radio button
    // and uncheck all the others
    UseVariantsRadioButton.Checked := True;
    UseParametersRadioButton.Checked := False;
    FullyPopulatedRadioButton.Checked := False;

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.FullyPopulatedRadioButtonClick(Sender: TObject) is an}
{ event handler for the FullyPopulated Radio-Button.  When the user clicks on it}
{ this procedure is called.                                                     }
{...............................................................................}
procedure TAgileBOMForm.FullyPopulatedRadioButtonClick(Sender: TObject);
begin
    // When clicked, this radio button become checked and all the others unchecked
    FullyPopulatedRadioButton.Checked := True;
    UseParametersRadioButton.Checked := False;
    UseVariantsRadioButton.Checked := False;

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.CreateAgileBOMCheckBoxClick(Sender: TObject) is an   }
{ event handler for the CreateAgileBOM Check-Box.  When the user clicks on it   }
{ this procedure is called.                                                     }
{...............................................................................}
procedure TAgileBOMForm.CreateAgileBOMCheckBoxClick(Sender: TObject);
begin
    // This procedure toggles CreateAgileBOMCheckBox.Checked automatically

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: TAgileBOMForm.CreateEngineeringBOMCheckBoxClick(Sender: TObject) is}
{ an event handler for the CreateEngineeringBOM Check-Box.  When the user clicks}
{ on it this procedure is called.                                               }
{...............................................................................}
procedure TAgileBOMForm.CreateEngineeringBOMCheckBoxClick(Sender: TObject);
begin
    // This procedure toggles CreateEngineeringBOMCheckBox.Checked automatically

    // Based on current settings on the form, re-write the description of what
    // action will be done when the OK button is pressed
    ReWriteActionLabel( 0 );
end;
{...............................................................................}

{...............................................................................}
Procedure Generate_BOM;
Begin
    // Fetch the current Date and Time for later use in the report
    DateStr := GetCurrentDateString();
    TimeStr := GetCurrentTimeString();

    // Create a list within which components can be gathered and sorted
    CompList := TStringList.Create;

    // Locate components and write salient information on each into the list
    FetchComponents( 0 );

    // If the Engineering BOM is checked for creation, create it
    If CreateEngineeringBOM
    Then CreateEngineeringReport( 0 );

    // If the Agile BOM is checked for creation, create it
    If CreateAgileBOM
    Then CreateAgileReport( 0 );

    // Now that we are done writing reports, Free up the list we created earlier
    CompList.Free;
End;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: SetState_FromParameters decodes a parameter string and sets all    }
{ settings accordingly.                                                         }
{...............................................................................}
Procedure SetState_FromParameters(AParametersList : String);
Var
    S : String;
Begin
    InitializeProject( 0 );

    ParameterName        := '';
    VariantName          := '';
    UseParameters        := False;
    UseVariants          := False;
    FullyPopulated       := False;
    CreateAgileBOM       := False;
    CreateEngineeringBOM := False;
    OpenOutputs          := True;
    AddToProject         := True;
    TargetFolder         := '';
    TargetFileName       := '';
    TargetPrefix         := '';

    If GetState_Parameter(AParametersList, 'ParameterName'       , S) Then ParameterName        := S;
    If GetState_Parameter(AParametersList, 'VariantName'         , S) Then VariantName          := S;
    If GetState_Parameter(AParametersList, 'UseParameters'       , S) Then UseParameters        := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'UseVariants'         , S) Then UseVariants          := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'FullyPopulated'      , S) Then FullyPopulated       := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'CreateAgileBOM'      , S) Then CreateAgileBOM       := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'CreateEngineeringBOM', S) Then CreateEngineeringBOM := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'OpenOutputs'         , S) Then OpenOutputs          := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'AddToProject'        , S) Then AddToProject         := StringsEqual(S, 'True');
    If GetState_Parameter(AParametersList, 'TargetFileName'      , S) Then TargetFileName       := S;
    If GetState_Parameter(AParametersList, 'TargetFolder'        , S) Then TargetFolder         := S;
    If GetState_Parameter(AParametersList, 'TargetPrefix'        , S) Then TargetPrefix         := S;

    SetState_Controls;
End;
{...............................................................................}

{...............................................................................}
{ FUNCTION: GetState_FromParameters encodes all settings as a parameter string. }
{...............................................................................}
Function GetState_FromParameters : String;
Begin
    GetState_Controls;

    Result := '';
    Result := Result +       'ParameterName='        + ParameterName;
    Result := Result + '|' + 'VariantName='          + VariantName;
    Result := Result + '|' + 'UseParameters='        + BoolToStr(UseParameters       , True);
    Result := Result + '|' + 'UseVariants='          + BoolToStr(UseVariants         , True);
    Result := Result + '|' + 'FullyPopulated='       + BoolToStr(FullyPopulated      , True);
    Result := Result + '|' + 'CreateAgileBOM='       + BoolToStr(CreateAgileBOM      , True);
    Result := Result + '|' + 'CreateEngineeringBOM=' + BoolToStr(CreateEngineeringBOM, True);
End;
{...............................................................................}

{...............................................................................}
{ MAIN ENTRY POINT: This procedure is the main entry point for this script.  It }
{ has no parameters and hence is listed when you select a script to be run.     }
{ It shows the form with default settings, and, if the user clicks OK,          }
{ generates an output file with the desired settings.                           }
{...............................................................................}
Procedure AgileBOM;
Begin
    Initialize;
    OpenOutputs  := True;
    AddToProject := True;
    If AgileBOMForm.ShowModal = mrOK Then
    Begin
        GetState_Controls;
        Generate_BOM;
        Close;
    End;
End;
{...............................................................................}

{...............................................................................}
{ PROCEDURE: Generate is the entry point when running a Script Output from an   }
{ OutJob document.  It generates an output file without showing the form.  The  }
{ settings to use are supplied from OutJob as a parameter string.               }
{...............................................................................}
Procedure Generate(Parameters : String);
Begin
    SetState_FromParameters(Parameters);
    Generate_BOM;
End;
{...............................................................................}

{...............................................................................}
{ FUNCTION: Configure is the entry point for the right-click Configure command  }
{ in an OutJob document.  It shows the form with the supplied settings (encoded }
{ as a parameter string), and, if the user clicks OK, it returns the new        }
{ settings.  These new settings will be saved by OutJob, and applied in         }
{ subsequent invocations of the Generate procedure.                             }
{...............................................................................}
Function Configure(Parameters : String) : String;
Begin
    Result := '';
    Initialize;
    SetState_FromParameters(Parameters);
    If AgileBOMForm.ShowModal = mrOK Then
    Begin
        Result := GetState_FromParameters;
        Close;
    End;
End;
{...............................................................................}

{...............................................................................}
{ FUNCTION: PredictOutputFileNames is an entry point called from Boards view.   }
{ It should returns the full path names of all files that will be generated by  }
{ the Generate procedure, without actually generating them.  The file names     }
{ should be returned via the Result string, separated by '|' characters.        }
{...............................................................................}
Function PredictOutputFileNames(Parameters : String) : String;
Var
    OutputFileNames : TStringList;
Begin
    SetState_FromParameters(Parameters);
    OutputFileNames                 := TStringList.Create;
    OutputFileNames.Delimiter       := '|';
    OutputFileNames.StrictDelimiter := True;
    If CreateEngineeringBOM Then
        OutputFileNames.Add(GetOutputFileNameWithExtension('.EngrBOM'));
    If CreateAgileBOM Then
        OutputFileNames.Add(GetOutputFileNameWithExtension('.AgileBOM'));
    Result := OutputFileNames.DelimitedText;
    OutputFileNames.Free;
End;
{...............................................................................}

