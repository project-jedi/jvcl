unit MainFrm;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvDiagramShape, Dialogs, ComCtrls, Menus, ImgList, StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
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
    byname1: TMenuItem;
    byLinksTo1: TMenuItem;
    byLinksFrom1: TMenuItem;
    byLinksToinverted1: TMenuItem;
    byLinksFrominverted1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SelectFiles1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Clear1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure SbMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SelectArrangeClick(Sender: TObject);
  private
    { Private declarations }
    FFileShapes: TStringlist;
    FLeft, FTop: integer;
    sb: TScrollBox;
    procedure Clear;
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
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  JCLParseUses, Clipbrd;
const
  FStartX = 50;
  FStartY = 50;
  FOffsetX = 100;
  FOffsetY = 50;

{$R *.dfm}

type
  TWaitCursor = class(TInterfacedObject)
  private
    FOldCursor: TCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TWaitCursor }

constructor TWaitCursor.Create;
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

destructor TWaitCursor.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;

function WaitCursor: IUnknown;
begin
  Result := TWaitCursor.Create;
end;

procedure SuspendRedraw(AControl: TWinControl; Suspend: boolean);
begin
  AControl.Perform(WM_SETREDRAW, Ord(not Suspend), 0);
  if not Suspend then
    RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

procedure CopyObjects(Strings: TStrings; AList: TList);
var i: integer;
begin
  for i := 0 to Strings.Count - 1 do
    AList.Add(Strings.Objects[i]);
end;

function GetNumLinksTo(AShape: TJvCustomDiagramShape): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).EndConn.Shape = AShape) then
      Inc(Result);
end;

function GetNumLinksFrom(AShape: TJvCustomDiagramShape): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to AShape.Parent.ControlCount - 1 do
    if (AShape.Parent.Controls[i] is TJvConnector) and
      (TJvConnector(AShape.Parent.Controls[i]).StartConn.Shape = AShape) then
      Inc(Result);
end;

function NameCompare(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(
    TJvCustomDiagramShape(Item1).Caption.Text,
    TJvCustomDiagramShape(Item2).Caption.Text);
end;

function MinLinksToCompare(Item1, Item2: Pointer): integer;
begin
  Result := GetNumLinksTo(Item1) - GetNumLinksTo(Item2);
end;

function MinLinksFromCompare(Item1, Item2: Pointer): integer;
begin
  Result := GetNumLinksFrom(Item1) - GetNumLinksFrom(Item2);
end;

function MaxLinksToCompare(Item1, Item2: Pointer): integer;
begin
  Result := GetNumLinksTo(Item2) - GetNumLinksTo(Item1);
end;

function MaxLinksFromCompare(Item1, Item2: Pointer): integer;
begin
  Result := GetNumLinksFrom(Item2) - GetNumLinksFrom(Item1);
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

function TfrmMain.GetFileShape(const Filename: string): TJvBitmapShape;
var i: integer; AFilename: string;
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
    Result.Top := FTop;
    Result.Left := FLeft;
    Result.Parent := sb;
    Result.Caption := TJvTextShape.Create(self);
    Result.Caption.Parent := sb;
    Result.Caption.Enabled := false;
    Result.Caption.OnClick := DoShapeClick;
    Result.Caption.Tag := integer(Result);
    Result.Caption.Text := AFilename;
    Result.Caption.AlignCaption(taLeftJustify);
    Result.BringToFront;
    i := FFileShapes.AddObject(AFilename, Result);
  end;
  Result := TJvBitmapShape(FFileShapes.Objects[i]);
end;

procedure TfrmMain.Connect(StartShape, EndShape: TJvCustomDiagramShape);
var arr: TJvSingleHeadArrow;
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

function TfrmMain.GetUses(const Filename: string; AUses: TStrings; var ErrorMessage: string): boolean;
var UL: TUsesList; i: integer; P: PChar;
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

procedure TfrmMain.ParseUnit(const Filename: string; Errors: TStrings);
var AUses: TStringlist; FS: TJvBitmapShape; i: integer;
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
    if AUses.Count > 0 then
      Inc(FLeft, FOffsetX);
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

procedure TfrmMain.ParseUnits(Files, Errors: TStrings);
var i: integer;
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
  StatusBar1.Panels[0].Text := Format('Done (%d units parsed, %d diagram units available)',
    [Files.Count, FFileShapes.Count]);
end;

procedure TfrmMain.Clear;
// var i: integer;
begin
  WaitCursor;
  FFileShapes.Clear;
  // this is faster than freeing explicitly:
  CreateScrollBox(Panel1);
{  for i := ComponentCount - 1 downto 0 do
    if Components[i] is TJvBitmapShape then
      TJvBitmapShape(Components[i]).Free; // this will free both the caption and the connector(s)
}
  FLeft := FStartX;
  FTop := FStartY;
  StatusBar1.Panels[0].Text := '  Ready';
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('Dependency Walker Demo - part of JVCL (http://jvcl.sourceforge.net)');
end;

procedure TfrmMain.SelectFiles1Click(Sender: TObject);
var Errors: TStringlist; // S: string;
begin
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFileShapes := TStringlist.Create;
  FFileShapes.Sorted := true;
  FFileShapes.Duplicates := dupError;
  FLeft := FStartX;
  FTop := FStartY;
  LoadSkipList;
  CreateScrollBox(Panel1);
end;

procedure TfrmMain.LoadSkipList;
var i: integer;
begin
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
  lbSkipList.Items.SaveToFile(ExtractFilePath(Application.Exename) + 'SkipList.txt');
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  Clear;
  SaveSkipList;
  FFileShapes.Free;
end;

procedure TfrmMain.Clear1Click(Sender: TObject);
begin
  Clear;
end;

function TfrmMain.InSkipList(const Filename: string): boolean;
begin
  Result := (lbSkipList.Items.IndexOf(ChangeFileExt(ExtractFileName(Filename), '')) > -1);
end;

procedure TfrmMain.Add1Click(Sender: TObject);
var S: string;
begin
  S := '';
  if InputQuery('Add unit', 'Name:', S) and (S <> '') and not InSkipList(S) then
    lbSkipList.Items.Add(ChangeFileExt(ExtractFilename(S), ''));
end;

procedure TfrmMain.Delete1Click(Sender: TObject);
var i: integer;
begin
  with lbSkipList do
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
end;

procedure TfrmMain.Arrange(AList: TList);
var Cols, i: integer; FS: TJvCustomDiagramShape;
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

procedure TfrmMain.SelectArrangeClick(Sender: TObject);
var AList: TList;
begin
  WaitCursor;
  SuspendRedraw(sb, true);
  AList := TList.Create;
  try
    sb.HorzScrollBar.Position := 0;
    sb.VertScrollBar.Position := 0;
    CopyObjects(FFileShapes, AList);
    case TMenuItem(Sender).Tag of
      1:
        AList.Sort(MinLinksToCompare);
      2:
        AList.Sort(MinLinksFromCompare);
      3:
        AList.Sort(MaxLinksToCompare);
      4:
        AList.Sort(MaxLinksFromCompare);
    else
      AList.Sort(NameCompare);
    end;
    Arrange(AList);
  finally
    SuspendRedraw(sb, false);
    AList.Free;
  end;
  TMenuItem(Sender).Checked := true;
end;

end.

