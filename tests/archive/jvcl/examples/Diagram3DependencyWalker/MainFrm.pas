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
    ScrollBox1: TScrollBox;
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
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SelectFiles1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Clear1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Arrange1Click(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    FFileShapes: TStringlist;
    FLeft, FTop: integer;
    procedure Clear;
    function GetFileShape(const Filename: string): TJvBitmapShape;
    procedure ParseUnits(Files, Errors: TStrings);
    procedure ParseUnit(const Filename: string; Errors: TStrings);
    function GetUses(const Filename: string; AUses: TStrings; var ErrorMessage: string): boolean;
    procedure Connect(StartShape, EndShape: TJvCustomDiagramShape);
    procedure LoadSkipList;
    procedure SaveSkipList;
    function InSkipList(const Filename: string): boolean;
    procedure DoShapeClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  JCLParseUses;

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

{ TfrmMain }

procedure TfrmMain.DoShapeClick(Sender:TObject);
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
var i: integer;AFilename:string;
begin
  AFilename := ChangeFileExt(ExtractFilename(Filename),'');
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
    Result.Parent := ScrollBox1;
    Result.Caption := TJvTextShape.Create(self);
    Result.Caption.Parent := ScrollBox1;
    Result.Caption.Enabled := false;
    Result.Caption.OnClick := DoShapeClick;
    Result.Caption.Tag := integer(Result);
    Result.Caption.Text := AFilename;
    Result.Caption.AlignCaption(taLeftJustify);
    Result.BringToFront;
    i := FFileShapes.AddObject(AFilename,Result);
  end;
  Result := TJvBitmapShape(FFileShapes.Objects[i]);
end;

procedure TfrmMain.Connect(StartShape, EndShape: TJvCustomDiagramShape);
var arr:TJvSingleHeadArrow;
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
    Parent := ScrollBox1;
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
  FTop := 10;
  try
    if not GetUses(Filename, AUses, ErrMsg) then
      Errors.Add(Format('%s: %s', [AFilename, ErrMsg]));
    // add the actual file
    FS := GetFileShape(AFilename);
    if AUses.Count > 0 then
      Inc(FLeft, 100);
    for i := 0 to AUses.Count - 1 do
    begin
      //add the used unit and connect to the parsed file
      Connect(FS, GetFileShape(ChangeFileExt(ExtractFileName(AUses[i]),'')));
      Inc(FTop, 50);
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
  for i := 0 to Files.Count - 1 do
  begin
    StatusBar1.Panels[0].Text := Files[i];
    StatusBar1.Update;
    if i > 0 then
      Inc(FLeft, 100);
    ParseUnit(Files[i], Errors);
  end;
  StatusBar1.Panels[0].Text := Format('Done (%d units parsed, %d diagram units available)',
    [Files.Count,FFileShapes.Count]);
end;

procedure TfrmMain.Clear;
var i: integer;
begin
  WaitCursor;
  FFileShapes.Clear;
  for i := ComponentCount - 1 downto 0 do
    if Components[i] is TJvBitmapShape then
      TJvBitmapShape(Components[i]).Free; // this will free both the caption and the connector(s)
  FLeft := 10;
  FTop := 10;
  StatusBar1.Panels[0].Text := '  Ready';
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('Dependency Walker Demo');
end;

procedure TfrmMain.SelectFiles1Click(Sender: TObject);
var Errors: TStringlist;
begin
  if dlgSelectFiles.Execute then
  begin
    Errors := TStringlist.Create;
    try
      ParseUnits(dlgSelectFiles.Files, Errors);
      if Errors.Count > 0 then
        ShowMessageFmt('Errors were encountered:'#13#10#13#10'%s', [Errors.Text]);
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
  FLeft := 10;
  FTop := 10;
  LoadSkipList;
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

procedure TfrmMain.Arrange1Click(Sender: TObject);
var X, Y, Cols, i: integer; FS: TJvCustomDiagramShape;
begin
  WaitCursor;
  ScrollBox1.HorzScrollBar.Position := 0;
  ScrollBox1.VertScrollBar.Position := 0;
  if FFileShapes.Count < 2 then
    Exit;
  Cols := round(sqrt(FFileShapes.Count));
  X := 10;
  Y := -40;
  for i := 0 to FFileShapes.Count - 1 do
  begin
    if (i mod Cols = 0) then // new row
    begin
      X := 10;
      Inc(Y, 50);
    end;
    FS := TJvCustomDiagramShape(FFileShapes.Objects[i]);
    FS.SetBounds(X, Y, FS.Width, FS.Height);
    Inc(X, 100);
  end;
end;

procedure TfrmMain.ScrollBox1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := true;
  with ScrollBox1 do
    if (ssShift in Shift) and (HorzScrollBar.IsScrollBarVisible) then
      HorzScrollBar.Position := HorzScrollBar.Position + WheelDelta
    else if (VertScrollBar.IsScrollBarVisible) then
      VertScrollBar.Position := VertScrollBar.Position + WheelDelta;
end;

end.

