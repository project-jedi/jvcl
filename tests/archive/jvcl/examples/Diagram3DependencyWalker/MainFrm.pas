{$I JVCL.INC}
unit MainFrm;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvDiagramShape, Dialogs, ComCtrls, Menus, ImgList, StdCtrls, ExtCtrls,
  ActnList, PersistSettings, DepWalkConsts, ToolWin, Buttons, PersistForm;

type
  (*
    // (p3) interposer class for TListBox that implements IPersistSettings (for the skiplist)
    TListBox = class(StdCtrls.TListBox, IUnknown, IPersistSettings)
    private
      {IPersistSettings}
      procedure Load(Storage: TCustomIniFile);
      procedure Save(Storage: TCustomIniFile);
    end;
   *)
  TfrmMain = class(TfrmPersistable)
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
    New1: TMenuItem;
    vertSplitter: TSplitter;
    pnlDiagram: TPanel;
    pnlSkipList: TPanel;
    lbSkipList: TListBox;
    pnlDiagramTitle: TPanel;
    pnlSkipListTitle: TPanel;
    popSkipList: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Edit1: TMenuItem;
    mnuSort: TMenuItem;
    N2: TMenuItem;
    Skiplist1: TMenuItem;
    Add2: TMenuItem;
    Delete2: TMenuItem;
    alMain: TActionList;
    acOpen: TAction;
    acExit: TAction;
    acSortName: TAction;
    acSortLinksTo: TAction;
    acSortLinksFrom: TAction;
    acInvertSort: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acNew: TAction;
    acAbout: TAction;
    byName1: TMenuItem;
    byLinksTo1: TMenuItem;
    LinksFrom1: TMenuItem;
    N3: TMenuItem;
    InvertSort1: TMenuItem;
    popShape: TPopupMenu;
    acUnitStats: TAction;
    Statistics1: TMenuItem;
    Delete3: TMenuItem;
    N4: TMenuItem;
    acDelShape: TAction;
    acReport: TAction;
    Print1: TMenuItem;
    acFind: TAction;
    Find1: TMenuItem;
    cbToolbar: TCoolBar;
    tbStandard: TToolBar;
    tbSelectFiles: TToolButton;
    tbNew: TToolButton;
    ToolButton3: TToolButton;
    tbAddSkip: TToolButton;
    tbDelSkip: TToolButton;
    Actions: TImageList;
    tbReport: TToolButton;
    ToolButton7: TToolButton;
    tbFind: TToolButton;
    tbUnitStats: TToolButton;
    tbAbout: TToolButton;
    ToolButton11: TToolButton;
    tbDelShape: TToolButton;
    ToolButton13: TToolButton;
    acAddToSkipList: TAction;
    Addtoskiplist1: TMenuItem;
    View1: TMenuItem;
    acViewStatusBar: TAction;
    acViewSkipList: TAction;
    SpeedButton1: TSpeedButton;
    StatusBar2: TMenuItem;
    Skiplist2: TMenuItem;
    acViewToolBar: TAction;
    Toolbar1: TMenuItem;
    N6: TMenuItem;
    acRefresh: TAction;
    sb: TScrollBox;
    acSaveBMP: TAction;
    acCopy: TAction;
    popDiagram: TPopupMenu;
    CopyDiagramtoClipboard1: TMenuItem;
    CopyDiagramtoClipboard2: TMenuItem;
    N5: TMenuItem;
    SaveImage1: TMenuItem;
    N7: TMenuItem;
    dlgSaveImage: TSaveDialog;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    acSaveDiagram: TAction;
    acOpenDiagram: TAction;
    acParseUnit: TAction;
    Parseunit1: TMenuItem;
    N8: TMenuItem;
    acOptions: TAction;
    Options1: TMenuItem;
    N10: TMenuItem;
    Shapes1: TMenuItem;
    Addtoskiplist2: TMenuItem;
    Statistics2: TMenuItem;
    Delete4: TMenuItem;
    ParseUnit2: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    acUnitView: TAction;
    ViewSource1: TMenuItem;
    ViewSource2: TMenuItem;
    acSortIntfImpl: TAction;
    byINterfaceImplementation1: TMenuItem;
    pnlStats: TPanel;
    Panel2: TPanel;
    SpeedButton2: TSpeedButton;
    horzSplitter: TSplitter;
    reStatistics: TRichEdit;
    acViewDetails: TAction;
    Statistics3: TMenuItem;
    acNoSort: TAction;
    none1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sbMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure acOpenExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acArrangeAction(Sender: TObject);
    procedure acInvertSortExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acUnitStatsExecute(Sender: TObject);
    procedure acDelShapeExecute(Sender: TObject);
    procedure acReportExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acAddToSkipListExecute(Sender: TObject);
    procedure acViewStatusBarExecute(Sender: TObject);
    procedure acViewSkipListExecute(Sender: TObject);
    procedure acViewToolBarExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acSaveBMPExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acSaveDiagramExecute(Sender: TObject);
    procedure acOpenDiagramExecute(Sender: TObject);
    procedure sbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure acParseUnitExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acUnitViewExecute(Sender: TObject);
    procedure sbExit(Sender: TObject);
    procedure acViewDetailsExecute(Sender: TObject);
    procedure acNoSortExecute(Sender: TObject);
  private
    { Private declarations }
    FFocusRectAnchor: TPoint;
    FFocusRect: TRect;
    FDrawing: boolean;

    FPrintFormat: TPrintFormat;
    FFileShapes, FLoadedFiles, FSearchPaths: TStringlist;
    FInitialDir: string;
    FLeft, FTop: integer;
    FOffsetX, FOffsetY: integer;
    FReload: boolean;
    FIntfLineColor, FImplLineColor, FIntfSelColor, FImplSelColor: TColor;

    function GetPersistStorage: TPersistStorage;
    procedure LoadSettings;
    procedure SaveSettings;
    function FindUnit(const Filename: string; const DefaultExt: string = '.pas'): string;
    procedure GetSearchPaths;
    procedure Clear(ClearAll: boolean);
    procedure CreatePrintOut(Strings: TStrings; AFormat: TPrintFormat = pfText);
    function GetFileShape(const Filename: string; var IsNew: boolean): TJvBitmapShape;
    procedure ParseUnits(Files, Errors: TStrings);
    procedure ParseUnit(const Filename: string; Errors: TStrings);
    function GetUses(const Filename: string; AUsesIntf, AUsesImpl: TStrings; var ErrorMessage: string): boolean;
    procedure Connect(StartShape, EndShape: TJvCustomDiagramShape; IsInterface: boolean);
    procedure LoadSkipList;
    procedure SaveSkipList;
    function InSkipList(const Filename: string): boolean;
    procedure Arrange(AList: TList);
    procedure DoShapeClick(Sender: TObject);
    procedure DoShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SortItems(ATag: integer; AList: TList; InvertedSort: boolean);

    procedure CreateDiagramBitmap(Bmp: TBitmap);
    procedure HighlightConnectors(AShape: TJvCustomDiagramShape);
    procedure DoBeginFocusRect(Sender: TObject; ARect: TRect; Button: TMouseButton; Shift: TShiftState; var Allow: boolean);
    procedure DoEndFocusRect(Sender: TObject; ARect: TRect; Button: TMouseButton; Shift: TShiftState);
    procedure DoFocusingRect(Sender: TObject; ARect: TRect; Shift: TShiftState; var Continue: boolean);
    procedure SetSelected(const Value: TJvCustomDiagramShape);
    procedure ShowInlineStats(AShape: TJvCustomDiagramShape);
    function GetSelected: TJvCustomDiagramShape;
  protected
    procedure Load(Storage: TPersistStorage); override;
    procedure Save(Storage: TPersistStorage); override;
  public
    { Public declarations }
    property Selected: TJvCustomDiagramShape read GetSelected write SetSelected;
  end;

var
  frmMain: TfrmMain;

implementation
uses
  JCLParseUses,
  DepWalkUtils,
  Clipbrd,
  IniFiles,
  StatsFrm,
  ShellAPI,
  PrintFrm,
  Registry,
{$IFNDEF COMPILER6_UP}
  JvFunctions,
{$ENDIF}
  OptionsFrm;

{$R *.dfm}

(*
{ TListBox }

procedure TListBox.Load(Storage: TCustomIniFile);
begin
  Exit;
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
  Exit;
  Storage.EraseSection(Name);
  for i := 0 to Items.Count - 1 do
    Storage.WriteString(Name, Items[i], '');
end;

*)

// utility functions

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

procedure UsesUnits(AShape: TJvCustomDiagramShape; Strings: TStrings; const Ext: string = cPascalExt);
var i: integer;
begin
  Strings.Clear;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).StartConn.Shape = AShape) then
      with TJvConnector(AShape.Parent.Controls[i]).EndConn do
        Strings.AddObject(ChangeFileExt(Shape.Caption.Text, Ext), Shape);
end;

// (p3) retrieves the shapes that connects to AShape and store their name and pointers in Strings

procedure UsedByUnits(AShape: TJvCustomDiagramShape; Strings: TStrings; const Ext: string = cPascalExt);
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

// TList sorting functions:

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

function SortIntfCompare(Item1, Item2: Pointer): integer;
begin
  Result := TJvBitmapShape(Item1).ImageIndex - TJvBitmapShape(Item2).ImageIndex;
end;

function SortImplCompare(Item1, Item2: Pointer): integer;
begin
  Result := -SortIntfCompare(Item1, Item2);
end;

{ TfrmMain }

{ IPersistSettings }

procedure TfrmMain.Load(Storage: TPersistStorage);
begin
  // DO NOT LOCALIZE!
  if not FReload then
    inherited;
  FReload := true;
  acInvertSort.Checked := Storage.ReadBool(ClassName, 'InvertSort', false);
  FInitialDir := Storage.ReadString(ClassName, 'InitialDir', '');
  pnlSkipList.Width := Storage.ReadInteger(ClassName, 'vertSplitter', pnlSkipList.Width);
  pnlStats.Height := Storage.ReadInteger(ClassName, 'horzSplitter', pnlStats.Height);
  StatusBar1.Top := ClientHeight;
  if not acViewStatusBar.Checked = Storage.ReadBool(ClassName, acViewStatusBar.Name, acViewStatusBar.Checked) then
    acViewStatusBar.Execute; // toggle to other state
  if not acViewToolbar.Checked = Storage.ReadBool(ClassName, acViewToolbar.Name, acViewToolbar.Checked) then
    acViewToolbar.Execute;
  if not acViewSkipList.Checked = Storage.ReadBool(ClassName, acViewSkipList.Name, acViewSkipList.Checked) then
    acViewSkipList.Execute;
  if not acViewDetails.Checked = Storage.ReadBool(ClassName, acViewDetails.Name, acViewDetails.Checked) then
    acViewDetails.Execute;
  FOffsetX := Storage.ReadInteger('Options', 'ShapeWidth', 100);
  FOffsetY := Storage.ReadInteger('Options', 'ShapeHeight', 100);
  FIntfLineColor := Storage.ReadInteger('Options', 'IntfColor', clBlack);
  FIntfSelColor := Storage.ReadInteger('Options', 'IntfSelColor', clRed);
  FImplLineColor := Storage.ReadInteger('Options', 'ImplColor', clBtnShadow);
  FImplSelColor := Storage.ReadInteger('Options', 'ImplSelColor', clBlue);
end;

procedure TfrmMain.Save(Storage: TPersistStorage);
begin
  inherited;
  Storage.WriteBool(ClassName, 'InvertSort', acInvertSort.Checked);
  Storage.WriteString(ClassName, 'InitialDir', FInitialDir);
  Storage.WriteInteger(ClassName, 'vertSplitter', pnlSkipList.Width);
  Storage.WriteInteger(ClassName, 'horzSplitter', pnlStats.Height);
  Storage.WriteBool(ClassName, acViewStatusBar.Name, acViewStatusBar.Checked);
  Storage.WriteBool(ClassName, acViewToolbar.Name, acViewToolbar.Checked);
  Storage.WriteBool(ClassName, acViewSkipList.Name, acViewSkipList.Checked);
  Storage.WriteBool(ClassName, acViewDetails.Name, acViewDetails.Checked);

end;

// main form utility functions

// (p3) highlights the connectors (arrows) going to and from AShape

procedure TfrmMain.HighlightConnectors(AShape: TJvCustomDiagramShape);
var i: integer; C: TJvConnector; Changed: boolean;
begin
  Changed := false;
  for i := 0 to AShape.Parent.ControlCount - 1 do
  begin
    if AShape.Parent.Controls[i] is TJvConnector then
    begin
      C := TJvConnector(AShape.Parent.Controls[i]);
      if (C.StartConn.Shape = AShape) or (C.EndConn.Shape = AShape) then
      begin
        Changed := true;
        if C.LineColor = FIntfLineColor then
          C.LineColor := FIntfSelColor
        else if C.LineColor = FImplLineColor then
          C.LineColor := FImplSelColor
        else
          Changed := false;
        if Changed then
          C.Invalidate;
      end
      else // reset to standard color
      begin
        Changed := true;
        if C.LineColor = FIntfSelColor then
          C.LineColor := FIntfLineColor
        else if C.LineColor = FImplSelColor then
          C.LineColor := FImplLineColor
        else
          Changed := false;
        if Changed then
          C.Invalidate;
      end;
    end;
  end;
  if Changed then
  begin
    AShape.Parent.Repaint;
    //    AShape.BringToFront;
  end;
end;

// (p3) returns an existing or new shape
// Filename is checked against unique list

function TfrmMain.GetFileShape(const Filename: string; var IsNew: boolean): TJvBitmapShape;
var
  i: integer;
  AFilename: string;
begin
  AFilename := FindUnit(Filename);
  i := FFileShapes.IndexOf(AFilename);
  IsNew := false;
  if i < 0 then
  begin
    IsNew := true;
    Result := TJvBitmapShape.Create(self);
    Result.Images := il32;
    Result.ImageIndex := cUnitUsedImageIndex; // always set "used" as default
    Result.Hint := AFilename;
    Result.ShowHint := True;
    Result.OnClick := DoShapeClick;
    Result.OnDblClick := acParseUnitExecute;
    Result.OnMouseDown := DoShapeMouseDown;

    Result.PopupMenu := popShape;
    Result.Top := FTop;
    Result.Left := FLeft;
    Result.Parent := sb;
    Result.Caption := TJvTextShape.Create(self);
    Result.Caption.Parent := sb;
    Result.Caption.Enabled := false;
    Result.Caption.Tag := integer(Result);
    Result.Caption.Text := ChangeFileExt(ExtractFilename(AFilename), '');
    Result.AlignCaption(taLeftJustify);
    Result.BringToFront;
    i := FFileShapes.AddObject(AFilename, Result);
  end;
  Result := TJvBitmapShape(FFileShapes.Objects[i]);
end;

// (p3) connects two shapes with a single head arrow pointing towards EndShape
// colors differently depending on if it's interface link or an implementation link

procedure TfrmMain.Connect(StartShape, EndShape: TJvCustomDiagramShape; IsInterface: boolean);
var
  arr: TJvSingleHeadArrow;
begin
  arr := TJvSingleHeadArrow.Create(self);
  with arr do
  begin
    if IsInterface then
      LineColor := FIntfLineColor
    else
      LineColor := FImplLineColor;
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

function TfrmMain.GetUses(const Filename: string; AUsesIntf, AUsesImpl: TStrings; var ErrorMessage: string): boolean;
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
      AUsesIntf.Clear;
      AUSesImpl.Clear;
      P := PChar(Memory);
      with TUnitGoal.Create(P) do
      try
        UL := UsesIntf;
        for i := 0 to UL.Count - 1 do
          if not InSkipList(UL.Items[i]) then
            AUsesIntf.Add(UL.Items[i]);
        UL := UsesImpl;
        for i := 0 to UL.Count - 1 do
          if not InSkipList(UL.Items[i]) then
            AUsesImpl.Add(UL.Items[i]);
      finally
        Free;
      end;
    finally
      Free;
    end;
  except
    on E: EFOpenError do
    begin
      Result := false;
      ErrorMessage := E.Message + #13#10 + SCheckPaths;
    end;
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
  AUsesIntf, AUsesImpl: TStringlist;
  FS: TJvBitmapShape;
  i: integer;
  AFilename, ErrMsg: string;
  b, IsNew: boolean;
begin
  AFilename := FindUnit(Filename);
  if InSkipList(AFilename) then
    Exit;
  AUsesIntf := TStringlist.Create;
  AUsesImpl := TStringlist.Create;
  try
    b := GetUses(AFilename, AUsesIntf, AUsesImpl, ErrMsg);
    if not b and (Errors <> nil) then
      Errors.Add(Format('%s: %s', [AFilename, ErrMsg]));
    FS := GetFileShape(AFilename, IsNew);
    if b then
      FS.ImageIndex := cUnitParsedImageIndex; // this is a parsed file
    if IsNew then
    begin
      Inc(FLeft, FOffsetX);
      FLoadedFiles.Add(AFilename);
    end;
    for i := 0 to AUsesIntf.Count - 1 do
    begin
      //add the used unit and connect to the parsed file
      Connect(FS, GetFileShape(AUsesIntf[i], IsNew), true);
      if IsNew then
        Inc(FTop, FOffsetY);
    end;
    for i := 0 to AUsesImpl.Count - 1 do
    begin
      //add the used unit and connect to the parsed file
      Connect(FS, GetFileShape(AUsesImpl[i], IsNew), false);
      if IsNew then
        Inc(FTop, FOffsetY);
    end;
  finally
    AUsesIntf.Free;
    AUsesImpl.Free;
  end;
  Application.ProcessMessages;
end;

// (p3) reads a list of filenames and calls ParseUnit for each

procedure TfrmMain.ParseUnits(Files, Errors: TStrings);
var
  i, aCount: integer;
begin
  WaitCursor;
  SuspendRedraw(sb, true);
  try
    for i := 0 to Files.Count - 1 do
    begin
      StatusBar1.Panels[0].Text := Files[i];
      StatusBar1.Update;
      aCount := FFileShapes.Count;
      FTop := cStartY;
      ParseUnit(Files[i], Errors);
      if aCount < FFileShapes.Count then
        Inc(FLeft, FOffsetX);
    end;
  finally
    SuspendRedraw(sb, false);
  end;
  StatusBar1.Panels[0].Text := Format(SParsedStatusFmt, [Files.Count, FFileShapes.Count]);
end;

// (p3) tries to find Filename and return it's full path and filename
// if it fails, the original Filename is returned instead

function TfrmMain.FindUnit(const Filename: string; const DefaultExt: string = '.pas'): string;
var i: integer;
begin
  Result := ExpandUNCFileName(Filename);
  if FileExists(Result) then
    Exit;
  Result := ChangeFileExt(Result, DefaultExt);
  if FileExists(Result) then
    Exit;
  Result := ExtractFilePath(dlgSelectFiles.FileName) + ExtractFileName(Result);
  if FileExists(Result) then
    Exit;

  if FSearchPaths = nil then
    GetSearchPaths;
  Result := ExtractFileName(Result);
  for i := 0 to FSearchPaths.Count - 1 do
    if FileExists(IncludeTrailingPathDelimiter(FSearchPaths[i]) + Result) then
    begin
      Result := IncludeTrailingPathDelimiter(FSearchPaths[i]) + Result;
      Exit;
    end;

  Result := Filename;
end;

// (p3) removes all shapes and links

procedure TfrmMain.Clear(ClearAll: boolean);
// var i: integer;
begin
  WaitCursor;
  FreeAndNil(FSearchPaths);
  FFileShapes.Clear;
  if ClearAll then
    FLoadedFiles.Clear;
  TJvCustomDiagramShape.DeleteAllShapes(sb);
  FLeft := cStartX;
  FTop := cStartY;
//  Selected := nil;
  StatusBar1.Panels[0].Text := SStatusReady;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetStorageHandler(GetPersistStorage);
  FFileShapes := TStringlist.Create;
  FLoadedFiles := TStringlist.Create;
  FFileShapes.Sorted := true;
  FFileShapes.Duplicates := dupError;
  FLeft := cStartX;
  FTop := cStartY;
  LoadSettings;
end;

procedure TfrmMain.LoadSkipList;
var
  //  i: integer;
  AFilename: string;
begin
  AFilename := ExtractFilePath(Application.Exename) + 'SkipList.txt';
  if FileExists(AFilename) then
  begin
    lbSkipList.Sorted := false;
    lbSkipList.Items.LoadFromFile(AFilename);
    {    for i := lbSkipList.Items.Count - 1 downto 0 do
        begin
          lbSkipList.Items[i] := ExtractFileName(ChangeFileExt(lbSkipList.Items[i], ''));
          if lbSkipList.Items[i] = '' then
            lbSkipList.Items.Delete(i);
        end; }
    lbSkipList.Sorted := true;
  end;
end;

procedure TfrmMain.SaveSkipList;
begin
  lbSkipList.Items.SaveToFile(ExtractFilePath(Application.Exename) + 'SkipList.txt');
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
    if (i mod Cols = 0) then // new row or first row
    begin
      FLeft := cStartX;
      if i = 0 then
        Inc(FTop, cStartY) // first row
      else
        Inc(FTop, FOffsetY);
    end;
    FS := TJvCustomDiagramShape(AList[i]);
    FS.SetBounds(FLeft, FTop, FS.Width, FS.Height);
    Inc(FLeft, FOffsetX);
  end;
  Dec(FLeft, FOffsetX);
end;

function iff(Condition: boolean; TrueValue, FalseValue: integer): integer;
begin
  if Condition then
    Result := TrueValue
  else
    Result := FalseValue;
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
    3:
      if InvertedSort then
        AList.Sort(SortImplCompare)
      else
        AList.Sort(SortIntfCompare);
  else
    Exit; // no sorting
  end;
end;

procedure TfrmMain.CreatePrintOut(Strings: TStrings; AFormat: TPrintFormat = pfText);
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
      UsesUnits(AShape, UsesStrings, '');
      UsedByUnits(AShape, UsedByStrings, '');
      case AFormat of
        pfText:
          begin
            Strings.Add(AShape.Caption.Text);
            Strings.Add('  ' + SUsesColon);
            if UsesStrings.Count < 1 then
              Strings.Add('    ' + SNone)
            else
              for j := 0 to UsesStrings.Count - 1 do
                Strings.Add('    ' + UsesStrings[j]);
            Strings.Add('  ' + SUsedByColon);
            if UsedByStrings.Count < 1 then
              Strings.Add('    ' + SNone)
            else
              for j := 0 to UsedByStrings.Count - 1 do
                Strings.Add('    ' + UsedByStrings[j]);
          end;
        pfHTML:
          begin
            Strings.Add(Format('<h3>%s:</h3>', [AShape.Caption.Text]));
            if UsesStrings.Count > 0 then
              Strings.Add(Format('<b>%s</b>', [SUsesColon]));
            Strings.Add('<ul>');
            for j := 0 to UsesStrings.Count - 1 do
              Strings.Add('<li>' + UsesStrings[j]);
            Strings.Add('</ul>');
            if UsedByStrings.Count > 0 then
              Strings.Add(Format('<b>%s</b>', [SUsedByColon]));
            Strings.Add('<ul>');
            for j := 0 to UsedByStrings.Count - 1 do
              Strings.Add('<li>' + UsedByStrings[j]);
            Strings.Add('</ul>');
          end;
        pfXML:
          begin
            // DO NOT LOCALIZE!
            Strings.Add(Format('<UNIT Name="%s">', [AShape.Caption.Text]));
            for j := 0 to UsesStrings.Count - 1 do
              Strings.Add(Format('<USES Name="%s" />', [UsesStrings[j]]));
            for j := 0 to UsedByStrings.Count - 1 do
              Strings.Add(Format('<USEDBY Name="%s" />', [UsedByStrings[j]]));
            Strings.Add('</UNIT>');
          end;
      end; // case
    end;
    // insert headers and footers:
    case AFormat of
      pfXML:
        begin
          // DO NOT LOCALIZE!
          Strings.Insert(0, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><DependencyWalker>');
          Strings.Add('</DependencyWalker>');
        end;
      pfHTML:
        begin
          // DO NOT LOCALIZE!
          Strings.Insert(0, Format('<html><head><title>%s</title><link rel="stylesheet" href="DependencyWalker.css" type="text/css"></head>', [SDependencyWalkerTitle]));
          Strings.Insert(1, Format('<body><h1>%s</h1><hr>', [SDependencyWalkerTitle]));
          Strings.Add('</body></html>');
        end;
    end; //
  finally
    UsedByStrings.Free;
    UsesStrings.Free;
    AList.Free;
  end;
end;

procedure TfrmMain.LoadSettings;
begin
  LoadSkipList;
  AutoLoad(self);
  Application.HintShortCuts := true;
end;

procedure TfrmMain.SaveSettings;
begin
  SaveSkipList;
  AutoSave(self);
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

// (p3) probably not the most effective code in the world but it does seem to work...

procedure PaintScrollBox(sb: TScrollBox; Canvas: TCanvas);
var sbPos: TPoint;
  tmpPos: integer;
begin
  sbPos.X := sb.HorzScrollBar.Position;
  sbPos.Y := sb.VertScrollBar.Position;
  try
    sb.HorzScrollBar.Position := 0;
    sb.VertScrollBar.Position := 0;
    while true do
    begin
      while true do
      begin
        sb.PaintTo(Canvas.Handle, sb.HorzScrollBar.Position, sb.VertScrollBar.Position);
        tmpPos := sb.VertScrollBar.Position;
        sb.VertScrollBar.Position := sb.VertScrollBar.Position + sb.ClientHeight;
        if sb.VertScrollBar.Position = tmpPos then
          Break;
      end;
      sb.VertScrollBar.Position := 0;
      tmpPos := sb.HorzScrollBar.Position;
      sb.HorzScrollBar.Position := sb.HorzScrollBar.Position + sb.ClientWidth;
      if sb.HorzScrollBar.Position = tmpPos then
        Break;
    end;
  finally
    sb.HorzScrollBar.Position := sbPos.X;
    sb.VertScrollBar.Position := sbPos.Y;
  end;
end;

procedure TfrmMain.CreateDiagramBitmap(Bmp: TBitmap);
begin
  // add some extra pixels around the edges...
  bmp.Width := Max(sb.ClientWidth, sb.HorzScrollBar.Range) + 10;
  bmp.Height := Max(sb.ClientHeight, sb.VertScrollBar.Range) + 10;
  bmp.Canvas.Brush.Color := sb.Color;
  bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
  PaintScrollBox(sb, bmp.Canvas);
end;

procedure TfrmMain.GetSearchPaths;
var ini: TCustomIniFile;
begin
  FreeAndNil(FSearchPaths);
  FSearchPaths := TStringlist.Create;
  ini := GetStorage;
  try
    ini.ReadSection('Library Paths', FSearchPaths);
  finally
    ini.Free;
  end;
end;

// (p3) create and return the type of TPersistStorage we are currently using

// main form event handlers (normal, run-time assigned) and actions

// (p3) bring the Shape to the front so we can see it

procedure TfrmMain.DoShapeClick(Sender: TObject);
begin
  TJvBitmapShape(Sender).BringToFront;
  TJvBitmapShape(Sender).Caption.BringToFront;
end;

// (p3) highlight the shapes connectors when it is selected

procedure TfrmMain.DoShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    HighLightConnectors(Sender as TJvCustomDiagramShape);
  ShowInlineStats(Sender as TJvCustomDiagramShape);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //  Clear;
  SaveSettings;
  FFileShapes.Free;
  FLoadedFiles.Free;
  FreeAndNil(FSearchPaths);
end;

procedure TfrmMain.sbMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := true;
  with sb do
    if (ssShift in Shift) and (HorzScrollBar.IsScrollBarVisible) then
      HorzScrollBar.Position := HorzScrollBar.Position - iff(ssCtrl in Shift, WheelDelta * 3, WheelDelta)
    else if (VertScrollBar.IsScrollBarVisible) then
      VertScrollBar.Position := VertScrollBar.Position - iff(ssCtrl in Shift, WheelDelta * 3, WheelDelta);
end;

procedure TfrmMain.acOpenExecute(Sender: TObject);
var
  Errors: TStringlist; // S: string;
begin
  ForceCurrentDirectory := true;
  dlgSelectFiles.InitialDir := FInitialDir;
  if dlgSelectFiles.Execute then
  begin
    FInitialDir := ExtractFilePath(dlgSelectFiles.Filename);
    Errors := TStringlist.Create;
    try
      ParseUnits(dlgSelectFiles.Files, Errors);
      if Errors.Count > 0 then
      begin
        ShowMessageFmt(SParseErrorsFmt, [Errors.Text]);
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

procedure TfrmMain.acArrangeAction(Sender: TObject);
var
  AList: TList;
begin
  WaitCursor;
  SuspendRedraw(sb, true);
  TJvCustomDiagramShape.UnselectAllShapes(sb);
  Selected := nil;
  AList := TList.Create;
  try
    FLeft := cStartX;
    FTop := cStartY;
    // (p3) reset here so it will be easier to check wich one is used as radio-item
    // (actions doesn't support RadioItem functionality but menus do):
    acSortName.Checked := false;
    acSortLinksTo.Checked := false;
    acSortLinksFrom.Checked := false;
    acSortIntfImpl.Checked := false;

    sb.HorzScrollBar.Position := 0;
    sb.VertScrollBar.Position := 0;
    CopyObjects(FFileShapes, AList);
    SortItems((Sender as TAction).Tag, AList, acInvertSort.Checked);
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
  if InputQuery(SAddSkipListTitle, SAddSkipListCaption, S) and (S <> '') and not InSkipList(S) then
    lbSkipList.Items.Add(ChangeFileExt(ExtractFilename(S), ''));
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
var
  i: integer;
begin
  if not YesNo(SConfirmDelete, SDelSelItemsPrompt) then
    Exit;
  with lbSkipList do
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  ShowMessage(SAboutText);
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
begin
  if YesNo(SConfirmClear, SClearDiagramPrompt) then
  begin
    Clear(true);
    LoadSettings;
  end;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acDelete.Enabled := lbSkipList.SelCount > 0;
  acNew.Enabled := sb.ControlCount > 0;
  acFind.Enabled := acNew.Enabled;
  acReport.Enabled := acNew.Enabled;
  acCopy.Enabled := acNew.Enabled;
  acSaveBMP.Enabled := acCopy.Enabled;
  mnuSort.Enabled := sb.ControlCount > 1;

  acDelShape.Enabled := Selected <> nil;
  acUnitStats.Enabled := acDelShape.Enabled;
  acAddToSkipList.Enabled := acDelShape.Enabled;
  acParseUnit.Enabled := acDelShape.Enabled;
  acUnitView.Enabled := acDelShape.Enabled;
end;

procedure TfrmMain.acUnitStatsExecute(Sender: TObject);
var
  AShape: TJvCustomDiagramShape;
  i: integer;
  S: string;
  UsedByStrings, UsesStrings: TStringlist;
begin
  AShape := Selected;
  if AShape = nil then
    AShape := TJvCustomDiagramShape(popShape.PopupComponent);
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
      UsedByStrings.Add(SNone);
    if UsesStrings.Count < 1 then
      UsesStrings.Add(SNone);
    i := FFileShapes.IndexOfObject(AShape);
    if i > -1 then
      S := FFileShapes[i]
    else
      S := ChangeFileExt(AShape.Caption.Text, cPascalExt);
    TfrmUnitStats.Execute(S, UsedByStrings, UsesStrings);
  finally
    UsedByStrings.Free;
    UsesStrings.Free;
  end;
end;

procedure TfrmMain.acDelShapeExecute(Sender: TObject);
var AShape: TJvCustomDiagramShape;
  i: integer;
begin
  // (p3) Can't use TJvCustomDiagramShape.DeleteSelectedShapes here since
  // we need to remove the item from the FFileShapes list as well:
  AShape := Selected;
  if (AShape <> nil) and YesNo(SConfirmDelete, Format(SDelSelItemFmt, [AShape.Caption.Text])) then
  begin
    repeat
      i := FFileShapes.IndexOfObject(AShape);
      if i > -1 then
        FFileShapes.Delete(i);
      AShape.Free;
      AShape := Selected;
    until AShape = nil;
  end;
end;

procedure TfrmMain.acReportExecute(Sender: TObject);
const
  // DO NOT LOCALIZE!
  cFormatExt: array[TPrintFormat] of PChar = ('.txt', '.htm', '.xml');
var
  S: TStringlist;
  AFileName: string;
  Ini: TPersistStorage;
begin
  if not TfrmPrint.Execute then
    Exit;
  Ini := GetStorage;
  try
    FPrintFormat := TPrintFormat(Ini.ReadInteger('Printing', 'Print Format', Ord(FPrintFormat)));
  finally
    Ini.Free;
  end;

  WaitCursor;
  S := TStringlist.Create;
  try
    CreatePrintOut(S, FPrintFormat);
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
var S: string;
  i: integer;
begin
  S := '';
  if InputQuery(SFindTitle, SFindNameColon, S) and (S <> '') then
  begin
    i := FFileShapes.IndexOf(S);
    if i < 0 then
      ShowMessageFmt(SFindNotFoundFmt, [S])
    else
    begin
      TJvCustomDiagramShape(FFileShapes.Objects[i]).Selected := true;
      // (p3) the caption (mostly) extends further to the right than the image,
      // so scroll the caption to make as much of the shape as possible visible
      sb.ScrollInView(TJvCustomDiagramShape(FFileShapes.Objects[i]).Caption);
    end;
  end;
end;

procedure TfrmMain.acAddToSkipListExecute(Sender: TObject);
var ASHape: TJvCustomDiagramShape;
begin
  AShape := Selected;
  if AShape <> nil then
  begin
    lbSkipList.Items.Add(ChangeFileExt(ExtractFilename(AShape.Caption.Text), ''));
    acDelShape.Execute;
  end;
end;

procedure TfrmMain.acViewStatusBarExecute(Sender: TObject);
begin
  acViewStatusBar.Checked := not acViewStatusBar.Checked;
  StatusBar1.Visible := acViewStatusBar.Checked;
end;

procedure TfrmMain.acViewSkipListExecute(Sender: TObject);
begin
  acViewSkipList.Checked := not acViewSkipList.Checked;
  pnlSkipList.Visible := acViewSkipList.Checked;
  vertSplitter.Visible := acViewSkipList.Checked;
  if pnlSkipList.Visible then
    vertSplitter.Left := pnlSkipList.Left;
end;

procedure TfrmMain.acViewToolBarExecute(Sender: TObject);
begin
  acViewToolBar.Checked := not acViewToolBar.Checked;
  cbToolbar.Visible := acViewToolBar.Checked;
end;

procedure TfrmMain.acViewDetailsExecute(Sender: TObject);
begin
  acViewDetails.Checked := not acViewDetails.Checked;
  pnlStats.Visible := acViewDetails.Checked;
  horzSplitter.Visible := pnlStats.Visible;
  if pnlStats.Visible then
    horzSplitter.Top := pnlStats.Top - 1;
end;

procedure TfrmMain.acRefreshExecute(Sender: TObject);
begin
  sb.Invalidate;
end;

procedure TfrmMain.acSaveBMPExecute(Sender: TObject);
var b: TBitmap;
begin
  if dlgSaveImage.Execute then
  begin
    b := TBitmap.Create;
    try
      CreateDiagramBitmap(b);
      b.SaveToFile(dlgSaveImage.Filename);
    finally
      b.Free;
    end;
    ShellExecute(Handle, 'open', PChar(dlgSaveImage.Filename), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmMain.acCopyExecute(Sender: TObject);
var
  AFormat: Word;
  b: TBitmap;
  AData: Cardinal;
  APalette: HPALETTE;
begin
  b := TBitmap.Create;
  try
    CreateDiagramBitmap(b);
    b.SaveToClipboardFormat(AFormat, AData, APalette);
    Clipboard.SetAsHandle(AFormat, AData);
  finally
    b.Free;
  end;
end;

procedure TfrmMain.acSaveDiagramExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if Execute then
      TJvCustomDiagramShape.SaveToFile(Filename, sb);
  finally
    Free;
  end;
end;

procedure TfrmMain.acOpenDiagramExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if Execute then
    begin
      FFileShapes.Clear;
      FLoadedFiles.Clear;
      TJvCustomDiagramShape.LoadFromFile(Filename, sb);
      // TODO: update FFileShapes list with new items
      // NB! loading a saved diagram looses the info about interface/implementation uses!
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.sbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SetCaptureControl(sb);
  // (p3) unselect any selected shape
  Selected := nil;
  if sb.CanFocus then
    sb.SetFocus;
  FDrawing := false;
  if Button = mbLeft then
  begin
    // initiate a focus rect
    FFocusRectAnchor.X := X;
    FFocusRectAnchor.Y := Y;
    FFocusRect := Rect(FFocusRectAnchor.X, FFocusRectAnchor.Y, 0, 0);
    DoBeginFocusRect(sb, FFocusRect, Button, Shift, FDrawing);
  end;
end;

procedure Swap(var Val1, Val2: integer);
var tmp: integer;
begin
  tmp := Val1;
  Val1 := Val2;
  Val2 := tmp;
end;

function NormalizedRect(ALeft, ATop, ARight, ABottom: integer): TRect;
begin
  if ALeft > ARight then
    Swap(ALeft, ARight);
  if ATop > ABottom then
    Swap(ATop, ABottom);
  Result := Rect(ALeft, ATop, ARight, ABottom);
end;

procedure TfrmMain.sbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var DC: HDC;
begin
  inherited;
  if not FDrawing then
    Exit;
  DC := GetDC(sb.Handle);
  try
    // erase previous rect
    DrawFocusRect(DC, FFocusRect);
    FFocusRect := NormalizedRect(FFocusRectAnchor.X, FFocusRectAnchor.Y, X, Y);
    // draw new rect
    DoFocusingRect(sb, FFocusRect, Shift, FDrawing);
    if FDrawing then
      DrawFocusRect(DC, FFocusRect);
  finally
    ReleaseDC(sb.Handle, DC);
  end;
end;

procedure TfrmMain.sbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var DC: HDC;
begin
  inherited;
  if FDrawing then
  begin
    DC := GetDC(sb.Handle);
    try
      // erase last focus rect
      DrawFocusRect(DC, FFocusRect);
      DoEndFocusRect(sb, FFocusRect, Button, Shift);
    finally
      ReleaseDC(sb.Handle, DC);
    end;
    ReleaseCapture;
  end;
  FDrawing := false;
end;

procedure TfrmMain.DoBeginFocusRect(Sender: TObject; ARect: TRect;
  Button: TMouseButton; Shift: TShiftState; var Allow: boolean);
begin
  Allow := sb.ControlCount > 0;
end;

procedure GetControlsInRect(AParent: TWinControl; ARect: TRect; PartialOK: boolean; AList: TList);
var i: integer;
begin
  for i := 0 to AParent.ControlCount - 1 do
    with AParent.Controls[i] do
      if PtInRect(ARect, Point(Left, Top)) then
      begin
        if PartialOK or PtInRect(ARect, Point(Left + Width, Top + Height)) then
          AList.Add(AParent.Controls[i]);
      end
      else if PartialOK and PtInRect(ARect, Point(Left + Width, Top + Height)) then
        AList.Add(AParent.Controls[i])
end;

procedure TfrmMain.DoFocusingRect(Sender: TObject; ARect: TRect; Shift: TShiftState; var Continue: boolean);
var AList: TList; i: integer;
begin
  AList := TList.Create;
  if not (ssShift in Shift) then
    TJvCustomDiagramShape.UnselectAllShapes(sb);
  try
    GetControlsInRect(sb, ARect, true, AList);
    for i := 0 to AList.Count - 1 do
      if TObject(AList[i]) is TJvBitmapShape then
        TJvBitmapShape(TObject(AList[i])).Selected := true;
    TJvBitmapShape.SetMultiSelected(sb, AList.Count > 1);
  finally
    AList.Free;
  end;
end;

procedure TfrmMain.DoEndFocusRect(Sender: TObject; ARect: TRect; Button: TMouseButton; Shift: TShiftState);
var AList: TList; i: integer;
begin
  AList := TList.Create;
  if not (ssShift in Shift) then
    TJvCustomDiagramShape.UnselectAllShapes(sb);
  try
    GetControlsInRect(sb, ARect, true, AList);
    for i := 0 to AList.Count - 1 do
      if TObject(AList[i]) is TJvBitmapShape then
        TJvBitmapShape(TObject(AList[i])).Selected := true;
    TJvBitmapShape.SetMultiSelected(sb, AList.Count > 1);
  finally
    AList.Free;
  end;
end;

procedure TfrmMain.sbExit(Sender: TObject);
begin
  inherited;
  FDrawing := false;
end;

// (p3) do a recursive parse of a unit

procedure TfrmMain.acParseUnitExecute(Sender: TObject);
var Errors: TStringList; i, aCount: integer;AShape:TJvCustomDiagramShape;
begin
  WaitCursor;
  AShape := Selected;
  i := FFileShapes.IndexOfObject(AShape);
  if i < 0 then
  begin
    if AShape <> nil then
      ShowMessageFmt(SFileNotFoundFmt, [AShape.Caption.Text])
    else
      ShowMessage(SUnitNotFound);
    Exit;
  end;
  Errors := TStringlist.Create;
  try
    FTop := cStartY;
    aCount := FFileShapes.Count;
    Inc(FLeft, FOffsetX); // start new row
    ParseUnit(FFileShapes[i], Errors);
    if Errors.Count > 0 then
    begin
      ShowMessageFmt(SParseErrorsFmt, [Errors.Text]);
      // copy to clipboard as well
      Clipboard.SetTextBuf(PChar(Errors.Text));
    end;
    if aCount = FFileShapes.Count then // nothing happened, so reset FLeft
      Dec(FLeft, FOffsetX);
  finally
    Errors.Free;
  end;
end;

procedure TfrmMain.acOptionsExecute(Sender: TObject);
begin
  if TfrmOptions.Execute then
  begin
    FreeAndNil(FSearchPaths);
    if sb.ControlCount = 0 then
      LoadSettings
    else
      ShowMessage(SRestartForNewOptions);
  end;
end;

procedure TfrmMain.acUnitViewExecute(Sender: TObject);
var AFilename: string;
begin
  AFilename := FindUnit(Selected.Caption.Text);
  if FileExists(AFilename) then
    ShellExecute(Handle, 'open', PChar(AFilename), nil, nil, SW_SHOWNORMAL)
  else
    ShowMessageFmt(SFileNotFoundFmt, [AFilename]);
end;

function TfrmMain.GetPersistStorage: TPersistStorage;
begin
  Result := TPersistStorage(TMemIniFile.Create(ChangeFileExt(Application.ExeName, cIniFileExt)));
  // ...could just as well have been:
//  Result := TPersistStorage(TRegistryIniFile.Create('\Software\JEDI\JVCL\Demos\Dependency Walker'));
end;

procedure SetRESelText(RE: TRichEdit; AColor: TColor; AStyle: TFontStyles; const AText: string);
begin
  RE.SelAttributes.Color := AColor;
  RE.SelAttributes.Style := AStyle;
  RE.SelText := AText;
end;

procedure TfrmMain.ShowInlineStats(AShape: TJvCustomDiagramShape);
var
  i: integer;
  S: string;
  UsedByStrings, UsesStrings: TStringlist;
begin
  reStatistics.Lines.Clear;
  if AShape <> nil then
  begin
    // (p3) collect the stats for the file
    // since we can't guarantee that the file can be found
    // on the system, only collect what we know explicitly (name, links):
    UsedByStrings := TStringlist.Create;
    UsesStrings := TStringlist.Create;
    try
      UsesUnits(AShape, UsesStrings);
      UsedByUnits(AShape, UsedByStrings);
      if UsedByStrings.Count < 1 then
        UsedByStrings.Add(SNone);
      if UsesStrings.Count < 1 then
        UsesStrings.Add(SNone);
      i := FFileShapes.IndexOfObject(AShape);
      if i > -1 then
        S := FFileShapes[i]
      else
        S := ChangeFileExt(AShape.Caption.Text, cPascalExt);
      SetRESelText(reStatistics, clNavy, [fsBold], S + ':'#13#10#13#10);
      SetRESelText(reStatistics, clBlack, [fsBold], 'uses:' + #13#10);
      for i := 0 to UsesStrings.Count - 1 do
        SetRESelText(reStatistics, clBlack, [], #9 + UsesStrings[i] + #13#10);
      SetRESelText(reStatistics, clBlack, [fsBold], 'used by:'#13#10);
      for i := 0 to UsedByStrings.Count - 1 do
        SetRESelText(reStatistics, clBlack, [], #9 + UsedByStrings[i] + #13#10);
    finally
      UsedByStrings.Free;
      UsesStrings.Free;
    end;
  end;
  // scroll to top:
  reStatistics.SelStart := 0;
  SendMessage(reStatistics.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TfrmMain.SetSelected(const Value: TJvCustomDiagramShape);
begin
  if Value <> nil then
  begin
    Value.Selected := true;
    ShowInlineStats(Value);
  end
  else
    TJvCustomDiagramShape.UnselectAllShapes(sb);
end;

procedure TfrmMain.acNoSortExecute(Sender: TObject);
var Errors: TStringlist;
begin
  acSortName.Checked := false;
  acSortLinksTo.Checked := false;
  acSortLinksFrom.Checked := false;
  acSortIntfImpl.Checked := false;
  acNoSort.Checked := true;
  Clear(false);
  Errors := TStringlist.Create;
  try
    ParseUnits(FLoadedFiles, Errors);
    if Errors.Count > 0 then
    begin
      ShowMessageFmt(SParseErrorsFmt, [Errors.Text]);
      // copy to clipboard as well
      Clipboard.SetTextBuf(PChar(Errors.Text));
    end;
  finally
    Errors.Free;
  end;
end;

function TfrmMain.GetSelected: TJvCustomDiagramShape;
var i: integer;
begin
  Result := nil;
  for i := 0 to sb.ControlCount - 1 do
    if (sb.Controls[i] is TJvBitmapShape) and TJvBitmapShape(sb.Controls[i]).Selected then
    begin
      Result := TJvCustomDiagramShape(sb.Controls[i]);
      Exit;
    end;
end;

end.

