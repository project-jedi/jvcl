{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDriveCtrls.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Components to replace the TDriveComboBox from Borland that also adds a TDriveListBox.
  Uses the system Iconlist to display drive icons.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvDriveCtrls;

interface

uses
  Windows, Messages, Classes, Graphics, Controls,
  FileCtrl, StdCtrls,
  JvComboBox, JvListBox, JvSearchFiles, JvTypes, JVCLVer;

type
  // redclare so user don't have to add JvTypes to uses manually
  TJvDriveType = JvTypes.TJvDriveType;
  TJvDriveTypes = JvTypes.TJvDriveTypes;

const
  dtStandard: TJvDriveTypes = [dtFixed, dtRemote, dtCDROM];

type
  TJvDirectoryListBox = class;

  TJvDriveCombo = class(TJvCustomComboBox)
  private
    FDrives: TStringList;
    FImages: TImageList;
    FImageWidth: Integer;
    FImageSize: TJvImageSize;
    FItemIndex: Integer;
    FOffset: Integer;
    FDrive: Char;
    FDriveTypes: TJvDriveTypes;
    FSmall: Integer;
    FLarge: Integer;
    FDisplayName: string;
    FDirList: TJvDirectoryListBox;
    procedure ResetItemHeight;
    procedure SetImageSize(Value: TJvImageSize);
    procedure SetOffset(Value: Integer);
  protected
    procedure FontChanged; override;
    procedure CreateWnd; override;
    procedure SetDrive(Value: Char);
    procedure SetDriveTypes(Value: TJvDriveTypes);
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure BuildList; virtual;
    procedure Change; override;
    property Items stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh; virtual;
  published
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property Drive: Char read FDrive write SetDrive stored False;
    property DriveTypes: TJvDriveTypes read FDriveTypes write SetDriveTypes;
    property Offset: Integer read FOffset write SetOffset;
    property ImageSize: TJvImageSize read FImageSize write SetImageSize;
    property DisplayName: string read FDisplayName;
    property Color;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  TJvDriveList = class(TJvCustomListBox)
  private
    FDrives: TStringList;
    FImages: TImageList;
    FImageWidth: Integer;
    FImageSize: TJvImageSize;
    FItemIndex: Integer;
    FOffset: Integer;
    FDrive: Char;
    FDriveTypes: TJvDriveTypes;
    FSmall: Integer;
    FLarge: Integer;
    FImageAlign: TJvImageAlign;
    FOnChange: TNotifyEvent;
    procedure SetImageAlign(Value: TJvImageAlign);
    procedure ResetItemHeight;
    procedure SetImageSize(Value: TJvImageSize);
    procedure SetOffset(Value: Integer);
    function GetDrives(Index: Integer): string;
    function GetDriveCount: Integer;
  protected
    procedure Resize; override;
    procedure FontChanged; override;
    procedure SetDrive(Value: Char);
    procedure SetDriveTypes(Value: TJvDriveTypes);
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure BuildList; virtual;
    procedure Change; dynamic;
    property Items stored False;
    property Offset: Integer read FOffset write SetOffset;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Refresh;
    property Drives[Index: Integer]: string read GetDrives;
    property DriveCount: Integer read GetDriveCount;
  published
    property MultiSelect;
    property ScrollBars default ssNone;
    property ImageAlign: TJvImageAlign read FImageAlign write SetImageAlign default iaCentered;
    property Drive: Char read FDrive write SetDrive stored False;
    property DriveTypes: TJvDriveTypes read FDriveTypes write SetDriveTypes;
    property ImageSize: TJvImageSize read FImageSize write SetImageSize;
    property Align;
    property BorderStyle;
    property Color;
    property Sorted;
    property Tag;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  TJvFileListBox = class(TFileListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FImages: TImageList;
    FForceFileExtensions: Boolean;
    FSearchFiles: TJvSearchFiles;
    procedure SetForceFileExtensions(const Value: Boolean);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure ReadFileNames; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyFilePath(const EditText: string); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Directory stored False;
    property FileName stored False;
    // set this property to True to force the display of filename extensions for all files even if
    // the user has activated the Explorer option "Don't show extensions for known file types"
    property ForceFileExtensions: Boolean read FForceFileExtensions write SetForceFileExtensions;
    property Columns;
    property BorderStyle;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  TJvDriveChangeError = procedure(Sender: TObject; var NewDrive: Char) of object;

  TJvDirectoryListBox = class(TJvCustomListBox)
  private
    FFileList: TJvFileListBox;
    FDriveCombo: TJvDriveCombo;
    FDirLabel: TLabel;
    FInSetDir: Boolean;
    FPreserveCase: Boolean;
    FCaseSensitive: Boolean;
    FAutoExpand: Boolean;
    { (rb) Probably better to switch the values in FDisplayNames and the values
           in Items, see comment at TJvCustomListBox.LBAddString }
    FDisplayNames: TStringList;
    FOnDriveChangeError: TJvDriveChangeError;
    FShowAllFolders: Boolean;
    function GetDrive: Char;
    procedure SetFileList(Value: TJvFileListBox);
    procedure SetDirLabel(Value: TLabel);
    procedure SetDirLabelCaption;
    procedure SetDrive(Value: Char);
    procedure DriveChange(NewDrive: Char);
    procedure SetDir(const NewDirectory: string);
    procedure SetDirectory(const NewDirectory: string); virtual;
    procedure ResetItemHeight;
    procedure SetDriveCombo(const Value: TJvDriveCombo);
    procedure SetShowAllFolders(const Value: Boolean);
  protected
    FImages: TImageList;
    FDirectory: string;
    FOnChange: TNotifyEvent;
    procedure FontChanged; override;
    procedure Change; virtual;
    procedure DblClick; override;
    procedure ReadBitmaps; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    function ReadDirectoryNames(const ParentDirectory: string;
      DirectoryList: TStrings): Integer;
    procedure BuildList; virtual;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click; override;
    function DoDriveChangeError(var NewDrive: Char): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    function GetItemPath(Index: Integer): string;
    procedure OpenCurrent;
    property Drive: Char read GetDrive write SetDrive stored False;
    procedure Update; reintroduce;
    property PreserveCase: Boolean read FPreserveCase;
    property CaseSensitive: Boolean read FCaseSensitive;
  published
    property Align;
    property AutoExpand: Boolean read FAutoExpand write FAutoExpand default True;
    property BorderStyle;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property Color;
    property Directory: string read FDirectory write SetDirectory;
    property DirLabel: TLabel read FDirLabel write SetDirLabel;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FileList: TJvFileListBox read FFileList write SetFileList;
    property DriveCombo: TJvDriveCombo read FDriveCombo write SetDriveCombo;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    { No need to store the items, image indexes aren't stored thus need to call
      BuildList anyway }
    property Items stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAllFolders: Boolean read FShowAllFolders write SetShowAllFolders default False;
    property ShowHint;
    property ScrollBars default ssNone;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDriveChangeError: TJvDriveChangeError read FOnDriveChangeError write FOnDriveChangeError;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  ShellAPI, SysUtils, Math, Forms, ImgList,
  JvJCLUtils, JvJVCLUtils, JvConsts;

const
  cDirPrefix = #32;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

function IsValidDriveType(DriveTypes: TJvDriveTypes; DriveType: UINT): Boolean;
const
  cDriveMasks: array[TJvDriveType] of UINT =
  (DRIVE_UNKNOWN, DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_REMOTE, DRIVE_CDROM, DRIVE_RAMDISK);
var
  I: TJvDriveType;
begin
  Result := True;
  for I := Low(TJvDriveType) to High(TJvDriveType) do
    if (I in DriveTypes) and (DriveType = cDriveMasks[I]) then
      Exit;
  Result := False;
end;

//=== { TJvDriveCombo } ======================================================

constructor TJvDriveCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLarge := GetSystemMetrics(SM_CXICON);
  FSmall := GetSystemMetrics(SM_CXSMICON);

  FDrives := TStringList.Create;
  FDriveTypes := dtStandard;

  if FImageSize = isSmall then
    FImages := TImageList.CreateSize(FSmall, FSmall)
  else
    FImages := TImageList.CreateSize(FLarge, FLarge);

  FImages.DrawingStyle := dsTransparent;
  FImageWidth := FImages.Width;
  FImages.ShareImages := True;

  FItemIndex := 0;
  FOffset := 4;
  Color := clWindow;
  Style := csOwnerDrawFixed;
  ResetItemHeight;
end;

destructor TJvDriveCombo.Destroy;
begin
  FDrives.Free;
  FImages.Free;
  inherited Destroy;
end;

procedure TJvDriveCombo.BuildList;
var
  Info: TSHFileInfo;
  S: string;
  Options: Integer;
  Drv: Char;
  LastErrorMode: Cardinal;
  Tmp: array[0..104] of Char; // 4 chars ('C:\#0') * 26 possible drives + 1 terminating #0 = 105 chars
  P: PChar;
begin
  Drv := Drive;
  Items.Clear;
  FDrives.Clear;
  Options := SHGFI_SYSICONINDEX;
  if FImageSize = isSmall then
    Options := Options or SHGFI_SMALLICON
  else
    Options := Options or SHGFI_LARGEICON;

  FImages.Handle := SHGetFileInfo('', 0, Info, SizeOf(TSHFileInfo), Options);
  FImages.ShareImages := True;
  LastErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    FillChar(Tmp[0], SizeOf(Tmp), #0);
    GetLogicalDriveStrings(SizeOf(Tmp), Tmp);
    P := Tmp;
    while P^ <> #0 do
    begin
      S := P;
      Inc(P, 4);
      if IsValidDriveType(DriveTypes, GetDriveType(PChar(S))) then
      begin
        SHGetFileInfo(PChar(S), 0, Info, SizeOf(TSHFileInfo), SHGFI_DISPLAYNAME or Options);
        Items.AddObject(Trim(Info.szDisplayName), TObject(Info.iIcon));
        FDrives.Add(S[1]);
      end
    end;
    Drive := Drv;
    Update;
  finally
    SetErrorMode(LastErrorMode);
  end;
end;

procedure TJvDriveCombo.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
  if FDrive = #0 then
  begin
    if FDrives.IndexOf(GetCurrentDir[1]) > 0 then
      Drive := GetCurrentDir[1]
    else
    if FDrives.Count > 0 then
      Drive := FDrives[0][1];
  end;
end;

procedure TJvDriveCombo.Refresh;
begin
  BuildList;
end;

procedure TJvDriveCombo.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
      Include(State, odSelected);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TJvDriveCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Offset, I: Integer;
begin
  //  inherited;
  with Canvas do
  begin
    Offset := FImageWidth + FOffset + FOffset;
    if FImages.Count > 0 then
    begin
      I := Integer(Items.Objects[Index]);
      FImages.Draw(Canvas, Rect.Left + FOffset, Rect.Top, I);
      Rect.Left := Rect.Left + Offset;
      Rect.Right := Rect.Left + Canvas.TextWidth(Items[Index]) + 6;
    end;
    FillRect(Rect);
    if odSelected in State then
      DrawFocusRect(Rect);
    Inc(Rect.Left, 3);
    DrawText(Canvas, Items[Index], -1, Rect,
      DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  end;
end;

procedure TJvDriveCombo.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHeight;
end;

procedure TJvDriveCombo.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvDriveCombo.ResetItemHeight;
var
  NewHeight: Integer;
begin
  NewHeight := GetItemHeight(Font);
  if NewHeight < FImages.Height then
    NewHeight := FImages.Height;
  ItemHeight := NewHeight;
end;

procedure TJvDriveCombo.SetDriveTypes(Value: TJvDriveTypes);
begin
  FDriveTypes := Value;
  if FDriveTypes = [] then
    FDriveTypes := [dtFixed];
  BuildList;
  Change;
  // Drive := FDrive;
end;

procedure TJvDriveCombo.SetDrive(Value: Char);
var
  I, J: Integer;
begin
  J := 0;
  if FItemIndex <> -1 then
    J := FItemIndex;

  Value := UpCase(Value);
  if FDrive <> Value then
  begin
    I := FDrives.IndexOf(Value);
    if I > -1 then
    begin
      FDrive := Value;
      FItemIndex := I;
      ItemIndex := I;
      if FDirList <> nil then
        FDirList.DriveChange(FDrive);
      Change;
    end;
  end
  else
    ItemIndex := J;
end;

procedure TJvDriveCombo.SetImageSize(Value: TJvImageSize);
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;

    if Items.Count > 0 then
      Items.Clear;
    if Assigned(FImages) then
      FImages.Free;

    if Value = isSmall then
      FImages := TImageList.CreateSize(FSmall, FSmall)
    else
      FImages := TImageList.CreateSize(FLarge, FLarge);

    FImages.DrawingStyle := dsTransparent;
    FImages.ShareImages := True;
    FImageWidth := FImages.Width;
    ResetItemHeight;
    RecreateWnd;
    BuildList;
    Change;
  end;
end;

procedure TJvDriveCombo.SetOffset(Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Refresh;
  end;
end;

procedure TJvDriveCombo.Change;
  function FirstChar(const S: string): Char;
  begin
    if Length(S) > 0 then
      Result := S[1]
    else
      Result := #0;
  end;
begin
  if ItemIndex <> -1 then
    FItemIndex := ItemIndex
  else
    FItemIndex := 0;
  if (FItemIndex >= 0)  and (FItemIndex < FDrives.Count) then
    Drive := FirstChar(FDrives[FItemIndex]);
  if (ItemIndex > -1) and (ItemIndex < Items.Count) then
    FDisplayName := Items[ItemIndex]
  else
    FDisplayName := '';
  inherited Change;
end;

procedure TJvDriveCombo.CNCommand(var Msg: TWMCommand);
begin
  inherited;
  case Msg.NotifyCode of
    {    CBN_EDITCHANGE:
          Change;}
    CBN_SELCHANGE:
      Change;
  end;
end;

//=== { TJvDriveList } =======================================================

constructor TJvDriveList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLarge := GetSystemMetrics(SM_CXICON);
  FSmall := GetSystemMetrics(SM_CXSMICON);

  FDrives := TStringList.Create;
  FDriveTypes := dtStandard;
  FImageAlign := iaCentered;
  ScrollBars := ssNone;

  if FImageSize = isSmall then
    FImages := TImageList.CreateSize(FSmall, FSmall)
  else
    FImages := TImageList.CreateSize(FLarge, FLarge);

  FImages.DrawingStyle := dsTransparent;
  FImageWidth := FImages.Width;
  FImages.ShareImages := True;

  FItemIndex := 0;
  Color := clWindow;
  SetBounds(0, 0, FImageWidth * 6 + 16, 97);
  FOffset := 4;
  Style := lbOwnerDrawFixed;
  ResetItemHeight;
end;

destructor TJvDriveList.Destroy;
begin
  FDrives.Free;
  FImages.Free;
  inherited Destroy;
end;

procedure TJvDriveList.BuildList;
var
  Info: TSHFileInfo;
  S: string;
  Options: Integer;
  Drv: Char;
  Tmp: array[0..104] of Char;
  P: PChar;
  LastErrorMode: Cardinal;
begin
  Drv := Drive;
  if Items.Count > 0 then
  begin
    Items.Clear;
    FDrives.Clear;
  end;

  Options := SHGFI_SYSICONINDEX;
  if FImageSize = isSmall then
    Options := Options or SHGFI_SMALLICON
  else
    Options := Options or SHGFI_LARGEICON;

  FImages.Handle := SHGetFileInfo('', 0, Info, SizeOf(TSHFileInfo), Options);
  FImages.ShareImages := True;
  FillChar(Tmp[0], SizeOf(Tmp), #0);
  LastErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    GetLogicalDriveStrings(SizeOf(Tmp), Tmp);
    P := Tmp;
    while P^ <> #0 do
    begin
      S := P;
      Inc(P, 4);
      if IsValidDriveType(DriveTypes, GetDriveType(PChar(S))) then
      begin
        SHGetFileInfo(PChar(S), 0, Info, SizeOf(TSHFileInfo), SHGFI_DISPLAYNAME or Options);
        Items.AddObject(Trim(Info.szDisplayName), TObject(Info.iIcon));
        FDrives.Add(S[1]);
      end;
    end;
    Drive := Drv;
    Update;
  finally
    SetErrorMode(LastErrorMode);
  end;
end;

procedure TJvDriveList.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
  if Drive = #0 then
    if FDrives.IndexOf(GetCurrentDir[1]) > 0 then
      Drive := GetCurrentDir[1]
    else
    if FDrives.Count > 0 then
      Drive := FDrives[0][1];
end;

procedure TJvDriveList.Refresh;
begin
  BuildList;
end;

procedure TJvDriveList.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
      Include(State, odSelected);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);

    Canvas.Handle := 0;
  end;
end;

procedure TJvDriveList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  HOffset, I: Integer;
  tmpCol: TColor;
  tmpR: TRect;
begin
  with Canvas do
  begin
    tmpCol := Canvas.Brush.Color;
    Canvas.Brush.Color := Self.Color;
    FillRect(Rect);
    Canvas.Brush.Color := tmpCol;
    if FImageAlign = iaCentered then
    begin
      HOffset := (Rect.Right - Rect.Left) div 2 - FImageWidth div 2;
      if FImages.Count > 0 then
      begin
        I := Integer(Items.Objects[Index]);
        FImages.Draw(Canvas, HOffset, Rect.Top, I);
      end;
      InflateRect(Rect, 1, -6);
      tmpR := Rect;
      DrawText(Canvas, Items[Index], -1, tmpR,
        DT_SINGLELINE or DT_BOTTOM or DT_CENTER or DT_NOPREFIX or DT_CALCRECT);
      Rect.Top := tmpR.Bottom - CanvasMaxTextHeight(Canvas);
      Rect.Left := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(PChar(Items[Index])) div 2;
      Rect.Right := Rect.Left + Canvas.TextWidth(PChar(Items[Index]));
      DrawText(Canvas, Items[Index], -1, Rect, DT_SINGLELINE or DT_CENTER or DT_NOPREFIX);
    end
    else
    begin
      if FImages.Count > 0 then
      begin
        I := Integer(Items.Objects[Index]);
        FImages.Draw(Canvas, Rect.Left + FOffset * 2, Rect.Top + FOffset * 2, I);
      end;
      tmpR := Rect;
      DrawText(Canvas, Items[Index], -1, tmpR,
        DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_NOPREFIX or DT_CALCRECT);
      Rect.Top := tmpR.Bottom - CanvasMaxTextHeight(Canvas);
      Rect.Bottom := Rect.Top + CanvasMaxTextHeight(Canvas);
      Rect.Left := FImageWidth + FOffset * 3;
      Rect.Right := Rect.Left + Canvas.TextWidth(PChar(Items[Index]));
      DrawText(Canvas, Items[Index], -1, Rect, DT_SINGLELINE or DT_TOP or DT_NOPREFIX);
    end;
  end;
  if odFocused in State then
    DrawFocusRect(Canvas.Handle, Rect);
end;

procedure TJvDriveList.MeasureItem(Index: Integer; var Height: Integer);
begin
  if FImageAlign = iaCentered then
    Height := FImageWidth + GetItemHeight(Font)
  else
    Height := Max(GetItemHeight(Font), FImageWidth);
end;

procedure TJvDriveList.SetImageAlign(Value: TJvImageAlign);
begin
  if FImageAlign <> Value then
  begin
    FImageAlign := Value;
    Invalidate;
  end;
end;

procedure TJvDriveList.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvDriveList.ResetItemHeight;
begin
  ItemHeight := GetItemHeight(Font) + FImageWidth + 8;
end;

procedure TJvDriveList.SetDriveTypes(Value: TJvDriveTypes);
begin
  FDriveTypes := Value;
  if FDriveTypes = [] then
    FDriveTypes := [dtFixed];
  BuildList;
end;

procedure TJvDriveList.SetDrive(Value: Char);
var
  I, J: Integer;
begin
  J := 0;
  if FItemIndex <> -1 then
    J := FItemIndex;

  Value := UpCase(Value);
  if (FDrive <> Value) and (Value <> #0) then
  begin
    I := FDrives.IndexOf(Value);
    if I > -1 then
    begin
      FDrive := Value;
      FItemIndex := I;
      ItemIndex := I;
    end;
  end
  else
    ItemIndex := J;
end;

procedure TJvDriveList.SetImageSize(Value: TJvImageSize);
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    if Items.Count > 0 then
      Items.Clear;
    if Assigned(FImages) then
      FImages.Free;

    if Value = isSmall then
      FImages := TImageList.CreateSize(FSmall, FSmall)
    else
      FImages := TImageList.CreateSize(FLarge, FLarge);

    FImages.DrawingStyle := dsTransparent;
    FImages.ShareImages := True;
    FImageWidth := FImages.Width;
    ResetItemHeight;
    RecreateWnd;
    BuildList;
    Change;
  end;
end;

procedure TJvDriveList.SetOffset(Value: Integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Refresh;
  end;
end;

procedure TJvDriveList.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TJvDriveList.Change;
begin
  if ItemIndex <> -1 then
    FItemIndex := ItemIndex;
  Drive := FDrives[FItemIndex][1];
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDriveList.CNCommand(var Msg: TWMCommand);
begin
  inherited;
  case Msg.NotifyCode of
    {    CBN_EDITCHANGE:
          Change;}
    CBN_SELCHANGE:
      Change;
  end;
end;

//=== { TJvDirectoryListBox } ================================================

function AddPathBackslash(Path: string): string;
begin
  Result := Path;
  if (Length(Path) > 1) and (AnsiLastChar(Path) <> '\') then
    Result := Path + '\';
end;

function DirLevel(const PathName: string): Integer; { counts '\' in path }
var
  P: PChar;
begin
  Result := 0;
  P := AnsiStrScan(PChar(PathName), '\');
  while P <> nil do
  begin
    Inc(Result);
    Inc(P);
    P := AnsiStrScan(P, '\');
  end;
end;

function ConcatPaths(const Path, S: string): string;
begin
  if Path = '' then
  begin
    Result := AddPathBackslash(S);
    Exit;
  end;
  if AnsiLastChar(Path)^ <> '\' then
    Result := Path + '\' + S
  else
    Result := Path + S;
end;

constructor TJvDirectoryListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 145;
  Style := lbOwnerDrawFixed;
  Sorted := False;
  ScrollBars := ssNone;
  FAutoExpand := True;
  FImages := TImageList.Create(Self);
  FImages.ShareImages := True;
  FDisplayNames := TStringList.Create;
  ReadBitmaps;
  GetDir(0, FDirectory);
  ResetItemHeight;
end;

destructor TJvDirectoryListBox.Destroy;
begin
  FDisplayNames.Free;
  inherited Destroy;
end;

function TJvDirectoryListBox.DoDriveChangeError(var NewDrive: Char): Boolean;
begin
  Result := Assigned(FOnDriveChangeError);
  if Result then
    FOnDriveChangeError(Self, NewDrive);
end;

procedure TJvDirectoryListBox.DriveChange(NewDrive: Char);
var
  VolFlags, MLength: DWORD;
  TmpDrive: Char;
begin
  if UpCase(NewDrive) <> UpCase(Drive) then
  begin
    if NewDrive <> #0 then
    begin
      if not SetCurrentDir(NewDrive + ':') then
      begin
        TmpDrive := NewDrive;
        if DoDriveChangeError(NewDrive) and (NewDrive <> TmpDrive) then
        begin
          DriveChange(NewDrive)
        end
        else
        if TmpDrive <> Drive then
          DriveChange(Drive); // ...if not, revert
      end;
      FDirectory := GetCurrentDir; { store correct directory name }
      GetVolumeInformation(PChar(NewDrive + ':\'), nil, 0, nil, MLength, VolFlags, nil, 0);
      FPreserveCase := VolFlags and (FS_CASE_IS_PRESERVED or FS_CASE_SENSITIVE) <> 0;
      FCaseSensitive := (VolFlags and FS_CASE_SENSITIVE) <> 0;
    end;
    if not FInSetDir then
    begin
      BuildList;
      Change;
    end;
  end;
end;

procedure TJvDirectoryListBox.SetFileList(Value: TJvFileListBox);
begin
  if FFileList <> nil then
    FFileList.FDirList := nil;
  FFileList := Value;
  if FFileList <> nil then
  begin
    FFileList.FreeNotification(Self);
    FFileList.Directory := Directory;
  end;
end;

procedure TJvDirectoryListBox.SetDirLabel(Value: TLabel);
begin
  FDirLabel := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  SetDirLabelCaption;
end;

procedure TJvDirectoryListBox.SetDir(const NewDirectory: string);
begin
  if DirectoryExists(FDirectory) then
    SetCurrentDir(FDirectory);
  SetCurrentDir(NewDirectory); { exception raised if invalid dir }
  FDirectory := GetCurrentDir; { store correct directory name }
  BuildList;
  Change;
end;

procedure TJvDirectoryListBox.OpenCurrent;
begin
  Directory := GetItemPath(ItemIndex);
end;

procedure TJvDirectoryListBox.Update;
begin
  BuildList;
  Change;
end;

function TJvDirectoryListBox.ReadDirectoryNames(const ParentDirectory: string;
  DirectoryList: TStrings): Integer;
const
  cAttr: array [Boolean] of Integer = (faDirectory,
    {$IFDEF VCL} faReadOnly or faHidden or faSysFile or faArchive or {$ENDIF} faDirectory);
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  Result := 0;
  DirectoryList.BeginUpdate;
  Status := FindFirst(ConcatPaths(ParentDirectory, AllFilePattern), cAttr[ShowAllFolders], SearchRec);
  try
    while Status = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          DirectoryList.Add(ConcatPaths(ParentDirectory, SearchRec.Name));
          Inc(Result);
        end;
      end;
      Status := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
    DirectoryList.EndUpdate;
  end;
end;

procedure TJvDirectoryListBox.BuildList;
const
  CFlagsDir = SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SELECTED or SHGFI_OPENICON
    or SHGFI_DISPLAYNAME;
  CFlagsSubDirs = SHGFI_ICON or SHGFI_SMALLICON or SHGFI_DISPLAYNAME;
var
  TempPath: string;
  DirName: string;
  BackSlashPos: Integer;
  I: Integer;
  Siblings: TStringList;
  NewSelect: Integer;
  tmpFolder: string;
  psfi: TSHFileInfo;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    FDisplayNames.Clear;

    TempPath := Directory;
    tmpFolder := '';

    if Length(TempPath) > 0 then
    begin
      if AnsiLastChar(TempPath)^ <> '\' then
      begin
        BackSlashPos := AnsiPos('\', TempPath);
        while BackSlashPos <> 0 do
        begin
          DirName := Copy(TempPath, 1, BackSlashPos - 1);
          tmpFolder := ConcatPaths(tmpFolder, DirName);
          Delete(TempPath, 1, BackSlashPos);
          SHGetFileInfo(PChar(tmpFolder), 0, psfi, SizeOf(TSHFileInfo), CFlagsDir);
          Items.AddObject(tmpFolder, TObject(psfi.iIcon));
          FDisplayNames.Add(psfi.szDisplayName);
          BackSlashPos := AnsiPos('\', TempPath);
        end;
      end;
      // add the selected dir:
      SHGetFileInfo(PChar(Directory), 0, psfi, SizeOf(TSHFileInfo), CFlagsDir);
      Items.AddObject(Directory, TObject(psfi.iIcon));
      FDisplayNames.Add(psfi.szDisplayName);
    end;
    NewSelect := Items.Count - 1;

    Siblings := TStringList.Create;
    try
      Siblings.Sorted := True;
      { read all the subdir names into Siblings }
      ReadDirectoryNames(Directory, Siblings);
      for I := 0 to Siblings.Count - 1 do
      begin
        SHGetFileInfo(PChar(Siblings[I]), 0, psfi, SizeOf(TSHFileInfo), CFlagsSubDirs);
        Items.AddObject(Siblings[I], TObject(psfi.iIcon));
        FDisplayNames.Add(psfi.szDisplayName);
      end;
    finally
      Siblings.Free;
    end;
  finally
    Items.EndUpdate;
  end;
  if HandleAllocated then
    ItemIndex := NewSelect;
end;

procedure TJvDirectoryListBox.ReadBitmaps;
var
  psfi: TSHFileInfo;
begin
  FImages.Handle := SHGetFileInfo('', 0, psfi, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FImages.ShareImages := True;
  FImages.DrawingStyle := dsTransparent;
end;

procedure TJvDirectoryListBox.DblClick;
begin
  OpenCurrent;
  inherited DblClick;
end;

procedure TJvDirectoryListBox.Change;
begin
  if FFileList <> nil then
    FFileList.Directory := Directory;
  if FDriveCombo <> nil then
    FDriveCombo.Drive := Drive;
  SetDirLabelCaption;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDirectoryListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(Lo(itemState));
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
    begin
      Canvas.FillRect(rcItem);
      //if odFocused in State then
      //  DrawFocusRect(hDC, rcItem);
    end;
    Canvas.Handle := 0;
  end;
end;

procedure TJvDirectoryListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  BmpWidth: Integer;
  DirOffset: Integer;
  S: string;
  RectText: TRect;
begin
  with Canvas do
  begin
    FillRect(Rect);

    BmpWidth := FImages.Width;
    if Index = 0 then
      DirOffset := Rect.Left + 2
    else
      DirOffset := Rect.Left + (DirLevel(Items[Index]) + 1) * 4 + 2;
    FImages.Draw(Canvas, DirOffset, (Rect.Top + Rect.Bottom - FImages.Height) div 2,
      Integer(Items.Objects[Index]));

    S := FDisplayNames[Index];

    RectText := Rect;
    RectText.Left := RectText.Left + DirOffset + FImages.Width + 2;
    RectText.Right := RectText.Left + TextWidth(S) + 4;

    TextOut(Rect.Left + BmpWidth + DirOffset + 4, Rect.Top + 2, S);
    if odFocused in State then
      DrawFocusRect(RectText);
  end;
end;

function TJvDirectoryListBox.GetItemPath(Index: Integer): string;
begin
  Result := '';
  if Index < Items.Count then
    Result := Items[Index];
  Exit;
end;

procedure TJvDirectoryListBox.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
  ItemIndex := DirLevel(Directory);
end;

procedure TJvDirectoryListBox.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
end;

procedure TJvDirectoryListBox.ResetItemHeight;
var
  NewHeight: Integer;
begin
  NewHeight := GetItemHeight(Font);
  if NewHeight < (FImages.Height + 1) then
    NewHeight := FImages.Height + 1;
  ItemHeight := NewHeight;
end;

function TJvDirectoryListBox.GetDrive: Char;
begin
  Result := FDirectory[1];
end;

procedure TJvDirectoryListBox.SetDrive(Value: Char);
begin
  if UpCase(Value) <> UpCase(Drive) then
    SetDirectory(Format('%s:', [Value]));
end;

procedure TJvDirectoryListBox.SetDirectory(const NewDirectory: string);
var
  NewDrive: string;
begin
  { When reading from the stream, always set the directory; if we don't do this
    the image indexes aren't initialized }
  if (Length(NewDirectory) = 0) or
    ((AnsiCompareText(NewDirectory, Directory) = 0) and not (csReading in ComponentState)) then
    Exit;
  NewDrive := ExtractFileDrive(NewDirectory);
  if Length(NewDrive) <> 2 then // we only support single Char drives (no UNC's)
    Exit;
  //  ProcessPath(NewDirectory, NewDrive, DirPart, FilePart);
  try
    if Drive <> NewDrive[1] then
    begin
      FInSetDir := True;
      if FDriveCombo <> nil then
        FDriveCombo.Drive := NewDrive[1]
      else
        DriveChange(NewDrive[1]);
    end;
  finally
    FInSetDir := False;
  end;
  if not DirectoryExists(NewDirectory) then
    SetDir(GetCurrentDir) // we have to do this because we might have changed drive
  else
    SetDir(NewDirectory);
end;

procedure TJvDirectoryListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Word(Key) = VK_RETURN then
    OpenCurrent;
end;

procedure TJvDirectoryListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FFileList then
      FFileList := nil
    else
    if AComponent = FDriveCombo then
      FDriveCombo := nil
    else
    if AComponent = FDirLabel then
      FDirLabel := nil;
  end;
end;

procedure TJvDirectoryListBox.SetDirLabelCaption;
var
  DirWidth: Integer;
begin
  if FDirLabel <> nil then
  begin
    DirWidth := Width;
    if not FDirLabel.AutoSize then
      DirWidth := FDirLabel.Width;
    FDirLabel.Caption := MinimizeName(Directory, FDirLabel.Canvas, DirWidth);
  end;
end;

procedure TJvDirectoryListBox.SetDriveCombo(const Value: TJvDriveCombo);
begin
  if FDriveCombo <> nil then
    FDriveCombo.FDirList := nil;
  FDriveCombo := Value;
  if FDriveCombo <> nil then
  begin
    FDriveCombo.FDirList := Self;
    FDriveCombo.Drive := Drive;
    FDriveCombo.FreeNotification(Self);
  end;
end;

procedure TJvDirectoryListBox.Click;
begin
  if FAutoExpand then
    OpenCurrent;
  inherited Click;
end;

procedure TJvDirectoryListBox.SetShowAllFolders(const Value: Boolean);
begin
  if FShowAllFolders <> Value then
  begin
    FShowAllFolders := Value;
    BuildList;
  end;
end;

//=== { TJvFileListBox } =====================================================

constructor TJvFileListBox.Create(AOwner: TComponent);
var
  shi: TSHFileInfo;
begin
  inherited Create(AOwner);
  FImages := TImageList.CreateSize(16, 16);
  FImages.ShareImages := True;
  FImages.Handle := SHGetFileInfo('', 0, shi, SizeOf(shi), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FImages.DrawingStyle := dsTransparent;

  FSearchFiles := TJvSearchFiles.Create(Self);
  FSearchFiles.Options := [soAllowDuplicates,
    soSearchDirs, soSearchFiles, soStripDirs];
  FSearchFiles.DirOption := doExcludeSubDirs;
  FSearchFiles.FileParams.FileMaskSeperator := ';';
  FSearchFiles.FileParams.SearchTypes := [stAttribute, stFileMask];
  FSearchFiles.FileParams.Attributes.IncludeAttr := 0;
  { No filter on drives }
  FSearchFiles.DirParams.SearchTypes := [];
  FSearchFiles.ErrorResponse := erIgnore;
end;

destructor TJvFileListBox.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

procedure TJvFileListBox.ReadFileNames;
var
  shinf: SHFILEINFO;
  I, J: Integer;
  Flags: Cardinal;
  AttrIndex: TFileAttr;
  AttrWord: DWORD;
  SaveCursor: TCursor;
const
  SHGFI_OVERLAYINDEX = $00000040;
  {TFileAttr = (ftReadOnly, ftHidden, ftSystem, ftVolumeID, ftDirectory,
    ftArchive, ftNormal);}
  Attributes: array[TFileAttr] of Word = (FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_HIDDEN,
    FILE_ATTRIBUTE_SYSTEM, 0 {faVolumeID}, 0 {faDirectory}, FILE_ATTRIBUTE_ARCHIVE,
    FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_NORMAL {faNormal});
  CAllAttributes = FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN or
    FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_NORMAL;
begin
  AttrWord := 0;
  if HandleAllocated then
  begin
    { Set attribute flags based on values in FileType }
    for AttrIndex := Low(TFileAttr) to High(TFileAttr) do
      if AttrIndex in FileType then
        AttrWord := AttrWord or Attributes[AttrIndex];
    SetCurrentDir(FDirectory); { go to the directory we want }
    Clear; { clear the list }

    SaveCursor := Screen.Cursor;
    try
      FSearchFiles.RootDirectory := GetCurrentDir;
      FSearchFiles.FileParams.FileMask := FMask;
      { CAllAttributes is used to ensure that we do not filter out some new
        Attributes, such as FILE_ATTRIBUTE_NOT_CONTENT_INDEXED etc }
      FSearchFiles.FileParams.Attributes.ExcludeAttr := not AttrWord and CAllAttributes;
      if ftDirectory in FileType then
        FSearchFiles.Options := FSearchFiles.Options + [soSearchDirs]
      else
        FSearchFiles.Options := FSearchFiles.Options - [soSearchDirs];

      FSearchFiles.Search;

      { Overlay included to display linked folders or files etc. }
      Flags := SHGFI_SYSICONINDEX or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_DISPLAYNAME;
      if GetShellVersion >= $00050000 then
        Flags := Flags or SHGFI_OVERLAYINDEX;

      { First add directories.. }
      with FSearchFiles.Directories do
        for J := 0 to Count - 1 do
        begin
          { Note that the strings in FSearchFiles.Directories do not include a path }
          SHGetFileInfo(PChar(Strings[J]), 0, shinf, SizeOf(shinf), Flags);
          if FForceFileExtensions then
            I := Items.Add(cDirPrefix + Strings[J])
          else
            I := Items.Add(cDirPrefix + string(shinf.szDisplayName));
          Items.Objects[I] := TObject(shinf.iIcon);
          if I = 100 then
            Screen.Cursor := crHourGlass;
        end;

      { ..then add files }
      with FSearchFiles.Files do
        for J := 0 to Count - 1 do
        begin
          SHGetFileInfo(PChar(Strings[J]), 0, shinf, SizeOf(shinf), Flags);
          if FForceFileExtensions then
            I := Items.Add(Strings[J])
          else
            I := Items.Add(shinf.szDisplayName);
          Items.Objects[I] := TObject(shinf.iIcon);
          if I = 100 then
            Screen.Cursor := crHourGlass;
        end;
    finally
      Screen.Cursor := SaveCursor;
    end;
    Change;
  end;
end;

procedure TJvFileListBox.ApplyFilePath(const EditText: string);
begin
  if (EditText <> '') and
    (AnsiCompareFileName(ExtractFilePath(FileName), ExtractFilePath(EditText)) <> 0) then
  begin
    inherited ApplyFilePath(EditText);
    ReadFileNames;
  end;
end;

procedure TJvFileListBox.SetForceFileExtensions(const Value: Boolean);
begin
  if FForceFileExtensions <> Value then
  begin
    FForceFileExtensions := Value;
    ReadFileNames;
  end;
end;

procedure TJvFileListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(Lo(itemState));
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    //    if odFocused in State then DrawFocusRect(hDC, rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TJvFileListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Offset: Integer;
  tmpR: TRect;
  ImageIndex: Integer;
  OverlayIndex: Integer;
begin
  with Canvas do
  begin
    //    FillRect(Rect);
    Offset := 2;
    tmpR := Rect;
    if ShowGlyphs then
    begin
      ImageIndex := Integer(Items.Objects[Index]);
      OverlayIndex := (ImageIndex shr 24) - 1;
      if OverlayIndex >= 0 then
        FImages.DrawOverlay(Canvas, Rect.Left + 2, (Rect.Top + Rect.Bottom - FImages.Height) div 2,
          ImageIndex and $00FFFFFF, OverlayIndex)
      else
        FImages.Draw(Canvas, Rect.Left + 2, (Rect.Top + Rect.Bottom - FImages.Height) div 2,
          ImageIndex);
      Offset := FImages.Width + 6;
    end;

    // Use Trim because directories have a space as prefix, so that
    // the directory names appear above the files.
    tmpR.Left := tmpR.Left + Offset - 2;
    tmpR.Right := tmpR.Left + TextWidth(Trim(Items[Index])) + 4;
    FillRect(tmpR);
    TextOut(Rect.Left + Offset, Rect.Top, Trim(Items[Index]));

    if odFocused in State then
      DrawFocusRect(tmpR);
  end;
end;

function TJvDriveList.GetDrives(Index: Integer): string;
begin
  Result := FDrives[Index];
end;

function TJvDriveList.GetDriveCount: Integer;
begin
  Result := FDrives.Count;
end;

end.

