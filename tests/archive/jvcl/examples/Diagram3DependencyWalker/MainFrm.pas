unit MainFrm;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvDiagramShape, Dialogs, ComCtrls, Menus, ImgList, StdCtrls, ExtCtrls,
  ActnList, IniFiles, PersistSettings, DepWalkConsts;

type
  // (p3) interposer class for TListBox that implements IPersistSettings (for the skiplist)
  TListBox = class(StdCtrls.TListBox, IUnknown, IPersistSettings)
  private
    {IPersistSettings}
    procedure Load(Storage: TCustomIniFile);
    procedure Save(Storage: TCustomIniFile);
  end;

  TfrmMain = class(TForm, IUnknown, IPersistSettings)
    StatusBar1: TStatusBar;
    mmMain: TMainMenu;
    File1: TMenuItem;
    SelectFiles1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    dlgSelectFiles: TOpenDialog;
    il32: TImageList;
    Clear1: TMenuItem;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    lbSkipList: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    popSkipList: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Edit1: TMenuItem;
    Arrange1: TMenuItem;
    N2: TMenuItem;
    Skiplist1: TMenuItem;
    Add2: TMenuItem;
    Delete2: TMenuItem;
    alMain: TActionList;
    acSelectFiles: TAction;
    acExit: TAction;
    acSortName: TAction;
    acSortLinksTo: TAction;
    acSortLinksFrom: TAction;
    acInvertSort: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClear: TAction;
    acAbout: TAction;
    byName1: TMenuItem;
    byLinksTo1: TMenuItem;
    LinksFrom1: TMenuItem;
    N3: TMenuItem;
    InvertSort1: TMenuItem;
    popDiagram: TPopupMenu;
    acUnitStats: TAction;
    Statistics1: TMenuItem;
    Delete3: TMenuItem;
    N4: TMenuItem;
    acDelDiagram: TAction;
    acPrint: TAction;
    Print1: TMenuItem;
    acFind: TAction;
    Find1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SbMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure acSelectFilesExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acArrangeAction(Sender: TObject);
    procedure acInvertSortExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acUnitStatsExecute(Sender: TObject);
    procedure acDelDiagramExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
  private
    { Private declarations }
    FPrintFormat:TPrintFormat;
    FFileShapes: TStringlist;
    FLeft, FTop: integer;
    sb: TScrollBox;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure Clear;
    procedure CreatePrintOut(Strings: TStrings; AFormat:TPrintFormat=pfText);
    function GetFileShape(const Filename: string): TJvBitmapShape;
    procedure ParseUnits(Files, Errors: TStrings);
    procedure ParseUnit(const Filename: string; Errors: TStrings);
    function GetUses(const Filename: string; AUses: TStrings; var ErrorMessage: string): boolean;
    procedure Connect(StartShape, EndShape: TJvCustomDiagramShape);
    procedure LoadSkipList;
    procedure SaveSkipList;
    function InSkipList(const Filename: string): boolean;
    procedure Arrange(AList: TList);
    procedure CreateScrollBox(AParent: TWinControl);
    procedure DoShapeClick(Sender: TObject);
    procedure SortItems(ATag: integer; AList: TList;
      InvertedSort: boolean);

    {IPersistSettings}
    procedure Load(Storage: TCustomIniFile);
    procedure Save(Storage: TCustomIniFile);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  JCLParseUses, Clipbrd, StatsFrm, ShellAPI, PrintFrm;

const
  FStartX = 50;
  FStartY = 50;
  FOffsetX = 75;
  FOffsetY = 50;

{$R *.dfm}

type
  // (p3) class that changes and restores the screen cursor automatically
  TChangeCursor = class(TInterfacedObject)
  private
    FOldCursor: TCursor;
  public
    constructor Create(NewCursor: TCursor);
    destructor Destroy; override;
  end;

{ TChangeCursor }

constructor TChangeCursor.Create(NewCursor: TCursor);
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := NewCursor;
end;

destructor TChangeCursor.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;

function WaitCursor: IUnknown;
begin
  Result := TChangeCursor.Create(crHourGlass);
end;

function ChangeCursor(NewCursor: TCursor): IUnknown;
begin
  Result := TChangeCursor.Create(NewCursor);
end;

function YesNo(const ACaption,AMsg:string):boolean;
begin
  Result := MessageBox(GetFocus,PChar(AMsg),PChar(ACaption),
    MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = IDYES;
end;

procedure SuspendRedraw(AControl: TWinControl; Suspend: boolean);
begin
  AControl.Perform(WM_SETREDRAW, Ord(not Suspend), 0);
  if not Suspend then
    RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

// (p3) copy Strings.Objects to TList

procedure CopyObjects(Strings: TStrings; AList: TList);
var
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do
    AList.Add(Strings.Objects[i]);
end;

// (p3) returns the number of links that are connected to AShape

function GetNumLinksTo(AShape: TJvCustomDiagramShape): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).EndConn.Shape = AShape) then
      Inc(Result);
end;

// (p3) returns the number of links that are connected from AShape

function GetNumLinksFrom(AShape: TJvCustomDiagramShape): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).StartConn.Shape = AShape) then
      Inc(Result);
end;

// (p3) retrievs the shapes that AShape is connected to and store their name and pointers in Strings

procedure UsesUnits(AShape: TJvCustomDiagramShape; Strings: TStrings; const Ext: string = '.pas');
var i: integer;
begin
  Strings.Clear;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).StartConn.Shape = AShape) then
      with TJvConnector(AShape.Parent.Controls[i]).EndConn do
        Strings.AddObject(ChangeFileExt(Shape.Caption.Text, Ext), Shape);
end;

// (p3) retrievs the shapes that connects to AShape and store their name and pointers in Strings

procedure UsedByUnits(AShape: TJvCustomDiagramShape; Strings: TStrings; const Ext: string = '.pas');
var i: integer;
begin
  Strings.Clear;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).EndConn.Shape = AShape) then
      with TJvConnector(AShape.Parent.Controls[i]).StartConn do
        Strings.AddObject(ChangeFileExt(Shape.Caption.Text, Ext), Shape);
end;

// (p3) returns the first selected shape that isn't a TJvTextShape or a TJvConnector
// (NOTE: I'm relying on that TJvTextShape has a nil Caption and TJvConnectors cannot be selected)

function GetFirstSelectedShape(Parent: TWInControl): TJvCustomDiagramShape;
var i: integer;
begin
  for i := 0 to Parent.ControlCount - 1 do
    if (Parent.Controls[i] is TJvCustomDiagramShape) and TJvCustomDiagramShape(Parent.Controls[i]).Selected and
    // don't be fooled by captions (they are also TJvCustomDiagramShape):
    not (TJvCustomDiagramShape(Parent.Controls[i]).Caption = nil) then
    begin
      Result := TJvCustomDiagramShape(Parent.Controls[i]);
      Exit;
    end;
  Result := nil;
end;

function NameCompare(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(
    TJvCustomDiagramShape(Item1).Caption.Text,
    TJvCustomDiagramShape(Item2).Caption.Text);
end;

function InvertNameCompare(Item1, Item2: Pointer): integer;
begin
  Result := -NameCompare(Item1, Item2);
end;

function MinLinksToCompare(Item1, Item2: Pointer): integer;
begin
  Result := GetNumLinksTo(Item1) - GetNumLinksTo(Item2);
  if Result = 0 then
    Result := GetNumLinksFrom(Item1) - GetNumLinksFrom(Item2);
  if Result = 0 then
    NameCompare(Item1, Item2);
end;

function MaxLinksToCompare(Item1, Item2: Pointer): integer;
begin
  Result := -MinLinksToCompare(Item1, Item2);
end;

function MinLinksFromCompare(Item1, Item2: Pointer): integer;
begin
  Result := GetNumLinksFrom(Item1) - GetNumLinksFrom(Item2);
  if Result = 0 then
    Result := GetNumLinksTo(Item1) - GetNumLinksTo(Item2);
  if Result = 0 then
    NameCompare(Item1, Item2);
end;

function MaxLinksFromCompare(Item1, Item2: Pointer): integer;
begin
  Result := -MinLinksFromCompare(Item1, Item2);
end;

{ TListBox }

procedure TListBox.Load(Storage: TCustomIniFile);
begin
  if Storage.SectionExists(Name) then
  begin
    Sorted := false;
    Storage.ReadSection(Name, Items);
    Sorted := true;
  end;
end;

procedure TListBox.Save(Storage: TCustomIniFile);
var i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Storage.WriteString(Name, Items[i], '');
end;

{ TfrmMain }

procedure TfrmMain.DoShapeClick(Sender: TObject);
begin
  if Sender is TJvBitmapShape then
  begin
    TJvBitmapShape(Sender).BringToFront;
    TJvBitmapShape(Sender).Caption.BringToFront;
  end
  else if Sender is TJvTextShape then
  begin
    TJvTextShape(Sender).BringToFront;
    TJvBitmapShape(TJvTextShape(Sender).Tag).BringToFront;
  end;
end;
// (p3) returns an existing or new shape
// Filename is considered unique

function TfrmMain.GetFileShape(const Filename: string): TJvBitmapShape;
var
  i: integer;
  AFilename: string;
begin
  AFilename := ChangeFileExt(ExtractFilename(Filename), '');
  i := FFileShapes.IndexOf(AFilename);
  if i < 0 then
  begin
    Result := TJvBitmapShape.Create(self);
    Result.Images := il32;
    Result.ImageIndex := 0;
    Result.Hint := AFilename;
    Result.ShowHint := True;
    Result.OnClick := DoShapeClick;
    Result.PopupMenu := popDiagram;
    Result.Top := FTop;
    Result.Left := FLeft;
    Result.Parent := sb;
    Result.Caption := TJvTextShape.Create(self);
    Result.Caption.Parent := sb;
    Result.Caption.Enabled := false;
    //    Result.Caption.OnClick := DoShapeClick;
    Result.Caption.Tag := integer(Result);
    Result.Caption.Text := AFilename;
    Result.Caption.AlignCaption(taLeftJustify);
    Result.BringToFront;
    i := FFileShapes.AddObject(AFilename, Result);
  end;
  Result := TJvBitmapShape(FFileShapes.Objects[i]);
end;

// (p3) connects two shapes with a single head arrow pointing towards EndShape

procedure TfrmMain.Connect(StartShape, EndShape: TJvCustomDiagramShape);
var
  arr: TJvSingleHeadArrow;
begin
  arr := TJvSingleHeadArrow.Create(self);
  with arr do
  begin
    LineColour := clBtnShadow;
    // Set the start connection
    StartConn.Side := csRight;
    StartConn.Offset := StartShape.Height div 2;
    StartConn.Shape := StartShape;
    // Set the end connection
    EndConn.Side := csLeft;
    EndConn.Offset := EndShape.Height div 2;
    EndConn.Shape := EndShape;
    // Ensure the size is correct
    SetBoundingRect;
    Parent := sb;
    SendToBack;
  end;
end;

// (p3) Builds a list of all units used by Filename and adds the unit names to AUses
// returns true if no errors, any exception message is added to ErrorMessage but th eprocessing
// is not aborted

function TfrmMain.GetUses(const Filename: string; AUses: TStrings; var ErrorMessage: string): boolean;
var
  UL: TUsesList;
  i: integer;
  P: PChar;
begin
  Result := true;
  try
    with TMemoryStream.Create do
    try
      LoadFromFile(Filename);
      AUses.Clear;
      P := PChar(Memory);
      with TUnitGoal.Create(P) do
      try
        UL := UsesIntf;
        for i := 0 to UL.Count - 1 do
          if not InSkipList(UL.Items[i]) then
            AUses.Add(UL.Items[i]);
        UL := UsesImpl;
        for i := 0 to UL.Count - 1 do
          if not InSkipList(UL.Items[i]) then
            AUses.Add(UL.Items[i]);
      finally
        Free;
      end;
    finally
      Free;
    end;
  except
    on E: Exception do
    begin
      Result := false;
      ErrorMessage := E.Message;
    end;
  end;
end;

// (p3) reads a single file's uses. Creates, connects and positions the shapes as necessary

procedure TfrmMain.ParseUnit(const Filename: string; Errors: TStrings);
var
  AUses: TStringlist;
  FS: TJvBitmapShape;
  i: integer;
  AFilename, ErrMsg: string;
begin
  AFilename := ChangeFileExt(ExtractFileName(Filename), '');
  if InSkipList(AFilename) then
    Exit;
  AUses := TStringlist.Create;
  FTop := FStartY;
  try
    if not GetUses(Filename, AUses, ErrMsg) then
      Errors.Add(Format('%s: %s', [AFilename, ErrMsg]));
    // add the actual file
    FS := GetFileShape(AFilename);
    Inc(FLeft,FOffsetX);
    for i := 0 to AUses.Count - 1 do
    begin
      //add the used unit and connect to the parsed file
      Connect(FS, GetFileShape(ChangeFileExt(ExtractFileName(AUses[i]), '')));
      Inc(FTop, FOffsetY);
    end;
  finally
    AUses.Free;
  end;
  Application.ProcessMessages;
end;

// (p3) reads a list of filenames and calls ParseUnit for each

procedure TfrmMain.ParseUnits(Files, Errors: TStrings);
var
  i: integer;
begin
  WaitCursor;
  SuspendRedraw(sb, true);
  try
    for i := 0 to Files.Count - 1 do
    begin
      StatusBar1.Panels[0].Text := Files[i];
      StatusBar1.Update;
      if i > 0 then
        Inc(FLeft, FOffsetX);
      ParseUnit(Files[i], Errors);
    end;
  finally
    SuspendRedraw(sb, false);
  end;
  StatusBar1.Panels[0].Text := Format('Done (%d units parsed, %d units in diagram)',
    [Files.Count, FFileShapes.Count]);
end;

// (p3) removes all shapes and links

procedure TfrmMain.Clear;
// var i: integer;
begin
  WaitCursor;
  FFileShapes.Clear;
  TJvCustomDiagramShape.DeleteAllShapes(sb);
  FLeft := FStartX;
  FTop := FStartY;
  StatusBar1.Panels[0].Text := '  Ready';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFileShapes := TStringlist.Create;
  FFileShapes.Sorted := true;
  FFileShapes.Duplicates := dupError;
  FLeft := FStartX;
  FTop := FStartY;
  LoadSettings;
  CreateScrollBox(Panel1);
end;

procedure TfrmMain.LoadSkipList;
var
  i: integer;
begin
  // NB! not used: see LoadSettings
  if FileExists(ExtractFilePath(Application.Exename) + 'SkipList.txt') then
  begin
    lbSkipList.Sorted := false;
    lbSkipList.Items.LoadFromFile(ExtractFilePath(Application.Exename) + 'SkipList.txt');
    for i := lbSkipList.Items.Count - 1 downto 0 do
    begin
      lbSkipList.Items[i] := ExtractFileName(ChangeFileExt(lbSkipList.Items[i], ''));
      if lbSkipList.Items[i] = '' then
        lbSkipList.Items.Delete(i);
    end;
    lbSkipList.Sorted := true;
  end;
end;

procedure TfrmMain.SaveSkipList;
begin
  // NB! not used: see SaveSettings
  lbSkipList.Items.SaveToFile(ExtractFilePath(Application.Exename) + 'SkipList.txt');
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //  Clear;
  SaveSettings;
  FFileShapes.Free;
end;

function TfrmMain.InSkipList(const Filename: string): boolean;
begin
  Result := (lbSkipList.Items.IndexOf(ChangeFileExt(ExtractFileName(Filename), '')) > -1);
end;

// (p3) arranges the shapes in AList into a grid of rows and columns
// tries to make the grid as "square" as possible (Rows = Cols)

procedure TfrmMain.Arrange(AList: TList);
var
  Cols, i: integer;
  FS: TJvCustomDiagramShape;
begin
  if AList.Count = 0 then
    Exit;
  Cols := round(sqrt(AList.Count));
  FLeft := 0;
  FTop := 0;
  for i := 0 to AList.Count - 1 do
  begin
    if (i mod Cols = 0) then // new row
    begin
      FLeft := FStartX;
      Inc(FTop, FOffsetY);
    end;
    FS := TJvCustomDiagramShape(AList[i]);
    FS.SetBounds(FLeft, FTop, FS.Width, FS.Height);
    Inc(FLeft, FOffsetX);
  end;
end;

procedure TfrmMain.SbMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := true;
  with sb do
    if (ssShift in Shift) and (HorzScrollBar.IsScrollBarVisible) then
      HorzScrollBar.Position := HorzScrollBar.Position - WheelDelta
    else if (VertScrollBar.IsScrollBarVisible) then
      VertScrollBar.Position := VertScrollBar.Position - WheelDelta;
end;

procedure TfrmMain.CreateScrollBox(AParent: TWinControl);
begin
  sb.Free;
  sb := TScrollBox.Create(self);
  sb.HorzScrollBar.Smooth := True;
  sb.VertScrollBar.Smooth := True;
  sb.Align := alClient;
  sb.BorderStyle := bsNone;
  sb.TabStop := True;
  sb.OnMouseWheel := SbMouseWheel;
  sb.Parent := AParent;
  sb.Color := clWindow;
end;

procedure TfrmMain.acSelectFilesExecute(Sender: TObject);
var
  Errors: TStringlist; // S: string;
begin
  ForceCurrentDirectory := true;
  if dlgSelectFiles.Execute then
  begin
    Errors := TStringlist.Create;
    try
      ParseUnits(dlgSelectFiles.Files, Errors);
      if Errors.Count > 0 then
      begin
        ShowMessageFmt('Errors were encountered:'#13#10#13#10'%s', [Errors.Text]);
        // copy to clipboard as well
        Clipboard.SetTextBuf(PChar(Errors.Text));
      end;
    finally
      Errors.Free;
    end;
  end;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SortItems(ATag: integer; AList: TList; InvertedSort: boolean);
begin
  case ATag of
    0:
      if InvertedSort then
        AList.Sort(InvertNameCompare)
      else
        AList.Sort(NameCompare);
    1:
      if InvertedSort then
        AList.Sort(MaxLinksToCompare)
      else
        AList.Sort(MinLinksToCompare);
    2:
      if InvertedSort then
        AList.Sort(MaxLinksFromCompare)
      else
        AList.Sort(MinLinksFromCompare);
  else
    Exit; // no sorting
  end;
end;

procedure TfrmMain.CreatePrintOut(Strings: TStrings; AFormat:TPrintFormat=pfText);
var
  i, j, ATag: integer;
  UsedByStrings, UsesStrings: TStringlist;
  AList: TList;
  AShape: TJvBitmapShape;
begin
  UsedByStrings := TStringlist.Create;
  UsesStrings := TStringlist.Create;
  AList := TList.Create;
  try
    Strings.Clear;
    // (p3) use same sorting as in the current view (defaults to "by Name"):
    CopyObjects(FFileShapes, AList);
    if acSortName.Checked then
      ATag := acSortName.Tag
    else if acSortLinksTo.Checked then
      ATag := acSortLinksTo.Tag
    else if acSortLinksFrom.Checked then
      ATag := acSortLinksFrom.Tag
    else
      ATag := -1; // no need to sort: FFileShapes already sorted by name
    SortItems(ATag, AList, acInvertSort.Checked);
    for i := 0 to AList.Count - 1 do
    begin
      AShape := TJvBitmapShape(AList[i]);
      UsesUnits(AShape, UsesStrings,'');
      UsedByUnits(AShape, UsedByStrings,'');
      case AFormat of
      pfText:
      begin
        Strings.Add(AShape.Caption.Text);
        Strings.Add('  uses:');
        if UsesStrings.Count < 1 then
          Strings.Add('    (none)')
        else
          for j := 0 to UsesStrings.Count - 1 do
            Strings.Add('    ' + UsesStrings[j]);
        Strings.Add('  used by:');
        if UsedByStrings.Count < 1 then
          Strings.Add('    (none)')
        else
          for j := 0 to UsedByStrings.Count - 1 do
            Strings.Add('    ' + UsedByStrings[j]);
      end;
      pfHTML:
      begin
        Strings.Add(Format('<h3>%s:</h3>',[AShape.Caption.Text]));
        if UsesStrings.Count > 0 then
          Strings.Add('<b>uses:</b>');
        Strings.Add('<ul>');
        for j := 0 to UsesStrings.Count - 1 do
          Strings.Add('<li>' + UsesStrings[j]);
        Strings.Add('</ul>');
        if UsedByStrings.Count > 0 then
          Strings.Add('<b>used by:</b>');
        Strings.Add('<ul>');
        for j := 0 to UsedByStrings.Count - 1 do
          Strings.Add('<li>' + UsedByStrings[j]);
        Strings.Add('</ul>');
      end;
      pfXML:
      begin
        Strings.Add(Format('<UNIT Name="%s">',[AShape.Caption.Text]));
          for j := 0 to UsesStrings.Count - 1 do
            Strings.Add(Format('<USES Name="%s" />',[UsesStrings[j]]));
          for j := 0 to UsedByStrings.Count - 1 do
            Strings.Add(Format('<USEDBY Name="%s" />',[UsedByStrings[j]]));
        Strings.Add('</UNIT>');
      end;
      end;  // case
    end;
    // insert headers and footers:
    case AFormat of
      pfXML:
      begin
        Strings.Insert(0,'<?xml version="1.0" encoding="UTF-8" standalone="yes"?><DependencyWalker>');
        Strings.Add('</DependencyWalker>');
      end;
      pfHTML:
      begin
        Strings.Insert(0,'<html><head><title>Dependency Walker</title></head>');
        Strings.Insert(1,'<body style="font-family:verdana,arial,sans serif,helvetica"><h1>Dependency Walker</h1><hr>');
//        Strings.Insert(2,'<h2>Legend:</h2><ul type="disc"><li> denotes "uses"</ul><ul type="square"><li>denotes "used by"</ul>');

        Strings.Add('</body></html>');
      end;
    end; //
  finally
    UsedByStrings.Free;
    UsesStrings.Free;
    AList.Free;
  end;
end;


procedure TfrmMain.acArrangeAction(Sender: TObject);
var
  AList: TList;
begin
  WaitCursor;
  SuspendRedraw(sb, true);
  // (p3) reset checked here so it will be easier to check wich one is used as radio-item
  AList := TList.Create;
  try
    acSortName.Checked := false;
    acSortLinksTo.Checked := false;
    acSortLinksFrom.Checked := false;
    sb.HorzScrollBar.Position := 0;
    sb.VertScrollBar.Position := 0;
    CopyObjects(FFileShapes, AList);
    SortItems((Sender as TAction).Tag, Alist, acInvertSort.Checked);
    Arrange(AList);
  finally
    SuspendRedraw(sb, false);
    AList.Free;
  end;
  TAction(Sender).Checked := true;
end;

procedure TfrmMain.acInvertSortExecute(Sender: TObject);
begin
  acInvertSort.Checked := not acInvertSort.Checked;
end;

procedure TfrmMain.acAddExecute(Sender: TObject);
var
  S: string;
begin
  S := '';
  if InputQuery('Add unit to skiplist', 'Unit name:', S) and (S <> '') and not InSkipList(S) then
    lbSkipList.Items.Add(ChangeFileExt(ExtractFilename(S), ''));
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
var
  i: integer;
begin
  if not YesNo('Confirm delete','Delete selected items?') then Exit;
  with lbSkipList do
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  ShowMessage('Dependency Walker Demo - part of JVCL (http://jvcl.sourceforge.net)');
end;

procedure TfrmMain.acClearExecute(Sender: TObject);
begin
  if YesNo('Confirm delete','Clear diagram?') then
    Clear;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acDelete.Enabled := lbSkipList.SelCount > 0;
  acClear.Enabled := sb.ControlCount > 0;
  // (p3) this might be too slow on large sets of shapes so comment it out
  //if it gets too sluggish
  acDelDiagram.Enabled := GetFirstSelectedShape(sb) <> nil;
  acUnitStats.Enabled := acDelDiagram.Enabled;
end;

procedure TfrmMain.Load(Storage: TCustomIniFile);
begin
  Top := Storage.ReadInteger(ClassName, 'Top', Top);
  Left := Storage.ReadInteger(ClassName, 'Left', Left);
  Width := Storage.ReadInteger(ClassName, 'Width', Width);
  Height := Storage.ReadInteger(ClassName, 'Height', Height);
  acInvertSort.Checked := Storage.ReadBool(ClassName, 'InvertSort', false);
  FPrintFormat := TPrintFormat(Storage.ReadInteger(ClassName,'Print Format',0));
end;

procedure TfrmMain.Save(Storage: TCustomIniFile);
begin
  if not IsZoomed(Handle) and not IsIconic(Application.Handle) then
  begin
    Storage.WriteInteger(ClassName, 'Top', Top);
    Storage.WriteInteger(ClassName, 'Left', Left);
    Storage.WriteInteger(ClassName, 'Width', Width);
    Storage.WriteInteger(ClassName, 'Height', Height);
  end;
  Storage.WriteBool(ClassName, 'InvertSort', acInvertSort.Checked);
  Storage.WriteInteger(ClassName,'Print Format',Ord(FPrintFormat));
end;

procedure TfrmMain.LoadSettings;
var Ini: TIniFile;
begin
//  LoadSkipList;
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    PersistSettings.LoadComponents(self, Ini);
  finally
    Ini.Free;
  end;
end;

procedure TfrmMain.SaveSettings;
var Ini: TIniFile;
begin
//  SaveSkipList;
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    PersistSettings.SaveComponents(self, Ini);
  finally
    Ini.Free;
  end;
end;

procedure TfrmMain.acUnitStatsExecute(Sender: TObject);
var
  AShape: TJvCustomDiagramShape;
  UsedByStrings, UsesStrings: TStringlist;
begin
  AShape := GetFirstSelectedShape(sb);
  if AShape = nil then
    Exit;
  // (p3) collect the stats for the file
  // since we can't guarantee that the file can be found
  // on the system, only collect what we know explicitly (name, links):
  UsedByStrings := TStringlist.Create;
  UsesStrings := TStringlist.Create;
  try
    UsesUnits(AShape, UsesStrings);
    UsedByUnits(AShape, UsedByStrings);
    if UsedByStrings.Count < 1 then
      UsedByStrings.Add('(none)');
    if UsesStrings.Count < 1 then
      UsesStrings.Add('(none)');
    TfrmUnitStats.Execute(ChangeFileExt(AShape.Caption.Text, '.pas'), UsedByStrings, UsesStrings);
  finally
    UsedByStrings.Free;
    UsesStrings.Free;
  end;
end;

procedure TfrmMain.acDelDiagramExecute(Sender: TObject);
var AShape: TJvCustomDiagramShape;
  i: integer;
begin
  // (p3) Can't use TJvCustomDiagramShape.DeleteSelecetdShapes here since
  // we need to remove the item from the FFileShapes list as well:
  AShape := GetFirstSelectedShape(sb);
  if (AShape <> nil) and YesNo('Confirm delete',Format('Remove "%s" from diagram?',[AShape.Caption.Text])) then
  begin
    i := FFileShapes.IndexOfObject(AShape);
    if i > -1 then
      FFileShapes.Delete(i);
    AShape.Free;
  end;
end;

procedure TfrmMain.acPrintExecute(Sender: TObject);
const
  cFormatExt:array[TPrintFormat] of PChar = ('.txt','.htm','.xml');
var
  S: TStringlist;
  AFileName:string;
begin
  S := TStringlist.Create;
  try
    if not TfrmPrint.Execute(FPrintFormat) then Exit;
    WaitCursor;
    CreatePrintOut(S,FPrintFormat);
    if S.Count > 0 then
    begin
      AFilename := ExtractFilePath(Application.Exename) + 'DependencyWalker' + cFormatExt[FPrintFormat];
      S.SaveToFile(AFilename);
      // show in default viewer: let user decide whether to print or not after viewing
      ShellExecute(Handle, 'open', PChar(AFilename), nil, nil, SW_SHOWNORMAL);
    end;
  finally
    S.Free;
  end;
end;


procedure TfrmMain.acFindExecute(Sender: TObject);
var S:string;i:integer;
begin
  S := '';
  if InputQuery('Find','Name:',S) and (S <> '') then
  begin
    i := FFileShapes.IndexOf(S);
    if i < 0 then
      ShowMessageFmt('"%s" not found!',[S])
    else
    begin
      TJvCustomDiagramShape(FFileShapes.Objects[i]).Selected := true;
      // (p3) the caption (mostly) extends further to the right than the image,
      // so scroll the caption to make as much of the shape as possible visible
      sb.ScrollInView(TJvCustomDiagramShape(FFileShapes.Objects[i]).Caption);
    end;
  end;
end;

end.

