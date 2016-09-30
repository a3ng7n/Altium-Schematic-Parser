{..............................................................................}
{ Summary Demo the use of Schematic Font Manager interface.                    }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Interface
Type
  TForm_FontsEditor = class(TForm)
    tvObjects     : TTreeView;
    ImageList1    : TImageList;
    btnSelectAll  : TButton;
    btnClearAll   : TButton;
    txtFont       : TEdit;
    Label1        : TLabel;
    btnSelectFont : TButton;
    Label2        : TLabel;
    txtRotation   : TEdit;
    btnSet        : TButton;
    FontDialog    : TFontDialog;
    Button3       : TButton;
    procedure tvObjectsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnSetClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
  End;

{..............................................................................}
var
  Form_FontsEditor: TForm_FontsEditor;
{..............................................................................}

Implementation

{$R *.DFM}

Const
    iiChecked = 1;
Var    
    SchFont : TFont;
{..............................................................................}

{..............................................................................}
Function IsObjectOk(Obj : ISch_BasicContainer) : Boolean;
Var
    ObjectID : TObjectID;
Begin 
    ObjectID := Obj.ObjectId;
    Case ObjectID Of
       eLabel,
       eTextFrame,
       eNetLabel,
       ePowerObject,
       eCrossSheetConnector,
       eParameter,
       eSheetFileName,
       eSheetName,
       eDesignator : 
          Result := True
    Else
       Result := False;
    End;
End;        
{..............................................................................}

{..............................................................................}
Function BuildFontStyleString(Bold, Italic, Underline, StrikeOut : Boolean) : String;
Begin
    Result := '';
    If Bold Then
       Result := Result + ' Bold';        
    If Italic Then
       Result := Result + ' Italic';
    If Underline Then
       Result := Result + ' Underline';
    If StrikeOut Then
       Result := Result + ' StrikeOut';
End;
{..............................................................................}

{..............................................................................}
Function GetFontString(FontManager : ISch_FontManager; FontID : TFontID) : String;
Var
    Size      : Integer;
    Rotation  : Integer;
    Underline : Boolean;
    Italic    : Boolean;                     
    Bold      : Boolean; 
    StrikeOut : Boolean;
    FontName  : String;
Begin
    FontManager.GetFontSpec(FontID, Size, Rotation, Underline, Italic, Bold, StrikeOut, FontName);
    Result := Format('%s %d %d deg%s', [FontName, Size, Rotation, BuildFontStyleString(Bold, Italic, Underline, StrikeOut)]);
End;
{..............................................................................}

{..............................................................................}
Function FindNode(ObjectID : TObjectID) : TTreeNode;
Var
    List     : TList;
Begin
    Result := tvObjects.Items.GetFirstNode;
    While Result <> Nil Do
    Begin
       List := Result.Data;
       If List[0].ObjectID = ObjectID Then
          Exit;
       Result := Result.GetNextSibling;   
    End;
    Result := Nil;
End;
{..............................................................................}

{..............................................................................}
Function GetObjectText(Obj : ISchBasicContainer; FontManager : ISch_FontManager) : String;
Begin
    Result := Format('%s (%s)', [Obj.GetState_Text, GetFontString(FontManager, Obj.FontID)]);
End;
{..............................................................................}

{..............................................................................}
Procedure AddObject(Obj : ISch_BasicContainer; FontManager : ISch_FontManager);
Var
    Node : TTreeNode;
    ObjectID : TObjectID;
    List : TList;
Begin
    ObjectID := Obj.ObjectID;
    Node := FindNode(ObjectID);
    If Node = Nil Then
    Begin
       List := TList.Create;
       Node := tvObjects.Items.Add(Nil, GetStateString_ObjectId(ObjectID));
       Node.Data := List;
    End
    Else
       List := Node.Data;
    
    List.Add(Obj);
    Node := tvObjects.Items.AddChild(Node, GetObjectText(Obj, FontManager));
    Node.Data := Obj;
End;
{..............................................................................}

{..............................................................................}
Procedure CollectInfo(Dummy : Integer = 0);;
Var
    Server      : ISchServer;
    Document    : ISch_Document;
    Iterator    : ISch_Iterator;
    Obj         : ISch_BasicContainer;
    FontManager : ISch_FontManager;
Begin
    Server := SchServer;
    If Server = Nil Then
    Begin
       ShowError('No SchServer started');
       Exit;
    End;
    Document := SchServer.GetCurrentSchDocument;
    If Document = Nil Then
    Begin
       ShowError('Current document is not SCH document');                
       Exit;
    End;
    
    FontManager := SchServer.FontManager;
    
    Iterator := Document.SchIterator_Create;
    Obj := Iterator.FirstSchObject;
    While Obj <> Nil Do
    Begin
       If IsObjectOk(Obj) Then
          AddObject(Obj, FontManager);
       Obj := Iterator.NextSchObject;
    End;
End;
{..............................................................................}

{..............................................................................}
Function BuildFontStylesText(Font : TFont) : String;
Var
    FontStyle : TFontStyles;
Begin
    FontStyle := Font.Style;
    Result := BuildFontStyleString(InSet(fsBold, FontStyle), InSet(fsItalic, FontStyle), InSet(fsUnderline, FontStyle), InSet(fsStrikeOut, FontStyle));
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateFont(Dummy : Integer = 0);;
Begin
    Form_FontsEditor.txtFont.Text := Format('%s %d%s',[SchFont.Name, SchFont.Size, BuildFontStylesText(SchFont)]);
End;
{..............................................................................}

{..............................................................................}
Procedure InitFont(Dummy : Integer = 0);
Begin
    SchFont := TFont.Create;
    SchFont.Assign(Form_FontsEditor.Font);
    UpdateFont;
End;
{..............................................................................}

{..............................................................................}
Procedure RunDialog;
Begin
    CollectInfo;
    InitFont;
    Form_FontsEditor.ShowModal;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_FontsEditor.tvObjectsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
    Pt       : TPoint;
    HitTests : THitTests;
    Node     : TTreeNode;
    ARect    : TRect;
Begin
    Pt := Mouse.CursorPos;
    Pt := tvObjects.ScreenToClient(Pt);
    HitTests := tvObjects.GetHitTestInfoAt(Pt.X, Pt.Y);
    If InSet(htOnIcon, HitTests) Then
    Begin
        Node := tvObjects.GetNodeAt(Pt.X, Pt.Y);
        If Node.SelectedIndex = 0 Then
            Node.SelectedIndex := 1
        Else
            Node.SelectedIndex := 0;
        Node.ImageIndex := Node.SelectedIndex;
        ARect := Node.DisplayRect(False);
        tvObjects.Invalidate;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure AddChildItems(Node : TTreeNode; ForceAdd : Boolean; List : TList);
Begin
    Node := Node.GetFirstChild;
    While Node <> Nil Do
    Begin
       If ForceAdd Or (Node.ImageIndex = iiChecked) Then
          List.Add(Node);
       Node := Node.GetNextSibling;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure BuildObjectList(List : TList);
Var
    Node : TTreeNode;
Begin
    Node := tvObjects.Items.GetFirstNode;
    While Node <> Nil Do
    Begin
       AddChildItems(Node, Node.ImageIndex = iiChecked, List);
       Node := Node.GetNextSibling;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateObjects(List : TList; FontID : TFontID; FontManager : ISch_FontManager);
Var
    I    : Integer;
    Node : TTreeNode;
    Obj  : ISch_BasicContainer;
Begin
    For I := 0 To List.Count - 1 Do
    Begin
       Node := List[I];
       Obj := Node.Data;
       Obj.FontID := FontID;
       Node.Text := GetObjectText(Obj, FontManager);
       Obj.GraphicallyInvalidate;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_FontsEditor.btnSetClick(Sender: TObject);
Var
    Rotation    : Integer;
    Server      : ISchServer;
    FontManager : ISch_FontManager;
    FontID      : Integer;
    List        : TList;
Begin
    Rotation := StrToIntDef(txtRotation.Text, -1);
    If Rotation < 0 Then
    Begin
       ShowError('Rotation have to be valid positive value.');
       Exit;
    End;
    
    List := TList.Create;
    Try
       BuildObjectList(List);
        
       Server := SchServer;
       FontManager := SchServer.FontManager;
       FontID := FontManager.GetFontID(SchFont.Size, Rotation, InSet(fsUnderline, SchFont.Style), 
          InSet(fsItalic, SchFont.Style), InSet(fsBold, SchFont.Style), InSet(fsStrikeOut, SchFont.Style), SchFont.Name);
       UpdateObjects(List, FontID, FontManager);                            
    Finally
       List.Free;
    End;                  
End;
{..............................................................................}

{..............................................................................}
Procedure SelectClear(Index : Integer);
Var
    Node : TTreeNode;
begin
    Node := tvObjects.Items.GetFirstNode;
    While Node <> Nil Do
    Begin
       Node.ImageIndex := Index;
       Node.SelectedIndex := Node.ImageIndex;
       Node := Node.GetNext;
    End;
    tvObjects.Invalidate;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_FontsEditor.btnSelectAllClick(Sender: TObject);
Begin
    SelectClear(iiChecked);       
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_FontsEditor.btnSelectFontClick(Sender: TObject);
Begin
    FontDialog.Font.Assign(SchFont);
    If FontDialog.Execute Then
    Begin
       SchFont.Assign(FontDialog.Font);
       UpdateFont;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm_FontsEditor.btnClearAllClick(Sender: TObject);
Begin
    SelectClear(0);       
End;
{..............................................................................}

{..............................................................................}
End.
