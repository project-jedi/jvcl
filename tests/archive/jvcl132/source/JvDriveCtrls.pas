{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDriveCtrls.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

{ Components to replace the TDriveComboBox from Borland that also adds a TDriveListBox.
    Uses the system Iconlist to display driveicons. }

unit JvDriveCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  FileCtrl, Dialogs, StdCtrls, ShellApi, ImgList, JvComponent, JvCtrls, JVCLVer;

type
  TJvDriveType = (dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk);
  TJvDriveTypes = set of TJvDriveType;

const
  dtStandard: TJvDriveTypes = [dtFixed, dtRemote, dtCDROM];

type
  TJvDirectoryListBox = class;
  TJvImageSize = (isSmall, isLarge);
  TJvImageAlign = (iaLeft, iaCentered);

  TJvDriveCombo = class(TJvCustomComboBox)
  private
    { Private declarations }
    FDrives: TStrings;
    FImages: TImagelist;
    FImageWidth: integer;
    FImageSize: TJvImageSize;
    FItemIndex: integer;
    FOffset: integer;
    FDrive: char;
    FDriveTypes: TJvDriveTypes;
    FSmall, FLarge: integer;
    FDisplayName: string;
    FDirList: TJvDirectoryListBox;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ResetItemHeight;
    procedure SetJvImageSize(Value: TJvImageSize);
    procedure SetOffset(Value: integer);
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure SetDrive(Value: char);
    procedure SetJvDriveTypes(Value: TJvDriveTypes);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure BuildList; virtual;
    procedure Change; override;
    property Items stored false;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh; virtual;
  published
    { Published declarations }
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property Drive: char read FDrive write SetDrive stored false;
    property DriveTypes: TJvDriveTypes read FDriveTypes write SetJvDriveTypes;
    property Offset: integer read FOffset write SetOffset;
    property ImageSize: TJvImageSize read FImageSize write SetJvImageSize;
    property DisplayName: string read FDisplayName;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
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

  { TDriveList }

  TJvDriveList = class(TJvCustomListBox)
  private
    { Private declarations }
    FDrives: TStrings;
    FImages: TImagelist;
    FImageWidth: integer;
    FImageSize: TJvImageSize;
    FItemIndex: integer;
    FOffset: integer;
    FDrive: char;
    FDriveTypes: TJvDriveTypes;
    FSmall, FLarge: integer;
    FImageAlign: TJvImageAlign;
    FOnChange: TNotifyEvent;
    procedure SetJvImageAlign(Value: TJvImageAlign);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ResetItemHeight;
    procedure SetJvImageSize(Value: TJvImageSize);
    procedure SetOffset(Value: integer);
    procedure WMSize(var Message: TWMNoParams); message WM_SIZE;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure SetDrive(Value: char);
    procedure SetJvDriveTypes(Value: TJvDriveTypes);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure BuildList; virtual;
    procedure Change; dynamic;
    property Items stored false;
    property Offset:integer read FOffset write SetOffset;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
  published
    { Published declarations }
    property MultiSelect;
    property ImageAlign: TJvImageAlign read FImageAlign write SetJvImageAlign default iaCentered;
    property Drive: char read FDrive write SetDrive stored false;
    property DriveTypes: TJvDriveTypes read FDriveTypes write SetJvDriveTypes;
    property ImageSize: TJvImageSize read FImageSize write SetJvImageSize;
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Sorted;
    property Tag;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
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

  { TJvFileListBox }
  TJvFileListBox = class(TFileListBox)
  private
    { Private declarations }
    FAboutJVCL: TJVCLAboutInfo;
    FImages: TImageList;
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure ReadFilenames; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Directory stored false;
    property Filename stored false;
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

  { TJvDirectoryListBox }

  TJvDirectoryListBox = class(TJvCustomListBox)
  private
    FFileList: TJvFileListBox;
    FDriveCombo: TJvDriveCombo;
    FDirLabel: TLabel;
    FInSetDir: Boolean;
    FPreserveCase: Boolean;
    FCaseSensitive: Boolean;
    FAutoExpand: boolean;
    function GetDrive: char;
    procedure SetFileListBox(Value: TJvFileListBox);
    procedure SetDirLabel(Value: TLabel);
    procedure SetDirLabelCaption;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetDrive(Value: char);
    procedure DriveChange(NewDrive: Char);
    procedure SetDir(const NewDirectory: string);
    procedure SetDirectory(const NewDirectory: string); virtual;
    procedure ResetItemHeight;
    procedure SetDriveCombo(const Value: TJvDriveCombo);
    procedure SetAutoExpand(const Value: boolean);
  protected
    FImages: TImageList;
    FDirectory: string;
    FOnChange: TNotifyEvent;
    procedure Change; virtual;
    procedure DblClick; override;
    procedure ReadBitmaps; virtual;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    function ReadDirectoryNames(const ParentDirectory: string;
      DirectoryList: TStringList): Integer;
    procedure BuildList; virtual;
    procedure KeyPress(var Key: Char); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetItemPath(Index: Integer): string;
    procedure OpenCurrent;
    property Drive: Char read GetDrive write SetDrive stored false;
    procedure Update;  reintroduce; 
    property PreserveCase: Boolean read FPreserveCase;
    property CaseSensitive: Boolean read FCaseSensitive;
  published
    property Align;
    property AutoExpand: boolean read FAutoExpand write SetAutoExpand default true;
    property BorderStyle;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;

    property Color;
    property Ctl3D;
    property Directory: string read FDirectory write SetDirectory stored false;
    property DirLabel: TLabel read FDirLabel write SetDirLabel;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FileList: TJvFileListBox read FFileList write SetFileListBox;
    property DriveCombo: TJvDriveCombo read FDriveCombo write SetDriveCombo;

    property Font;
    property IntegralHeight;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
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
const
  cDirPrefix = #32;

function GetItemHeight(Font: TFont): Integer;
var DC: HDC; SaveFont: HFont; Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

{ TJvDriveCombo }

constructor TJvDriveCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLarge := GetSystemMetrics(SM_CXICON);
  FSmall := GetSystemMetrics(SM_CXSMICON);

  FDrives := TStringList.Create;
  FDriveTypes := dtStandard;

  if FImageSize = isSmall then
    FImages := TImagelist.CreateSize(FSmall, FSmall)
  else
    FImages := TImagelist.CreateSize(FLarge, FLarge);

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
  Options: integer;
  ADrive: char;
  LastErrorMode: Cardinal;
  tmp: array[0..104] of char; // 4 chars ('C:\#0') * 26 possible drives + 1 terminating #0 = 105 chars
  P: PChar;
begin
  Items.Clear;
  FDrives.Clear;
  Options := SHGFI_SYSICONINDEX;
  if FImageSize = isSmall then
    Options := Options or SHGFI_SMALLICON
  else
    Options := Options or SHGFI_LARGEICON;

  FImages.Handle := SHGetFileInfo('', 0, Info, SizeOf(TShFileInfo), Options);
  FImages.ShareImages := true;
  ADrive := Drive;
  LastErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    FillChar(tmp[0], sizeof(tmp), #0);
    GetLogicalDriveStrings(sizeof(tmp), tmp);
    P := tmp;
    while P^ <> #0 do
    begin
      S := string(P);
      Inc(P, 4);
      SHGetFileInfo(PChar(S), 0, Info, SizeOf(TShFileInfo), SHGFI_DISPLAYNAME or Options);
      Items.AddObject(Trim(Info.szDisplayName), TObject(Info.iIcon));
      FDrives.Add(S[1]);
    end;
    SetDrive(ADrive);
    Update;
  finally
    SetErrorMode(LastErrorMode);
  end;
end;

procedure TJvDriveCombo.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
  if (csDesigning in ComponentState) then
    SetDrive('C');
end;

procedure TJvDriveCombo.Refresh;
begin
  BuildList;
end;

procedure TJvDriveCombo.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State, odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State, odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State, odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State, odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State, odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State, odGrayed);
    if bool(itemState and ODS_SELECTED) then
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
  Offset, i: Integer;
begin
  //  inherited;
  with Canvas do
  begin
    Offset := FImageWidth + FOffset + FOffset;
    if FImages.Count > 0 then
    begin
      i := integer(Items.Objects[Index]);
      FImages.Draw(Canvas, Rect.Left + FOffset, Rect.Top, i);
      Rect.Left := Rect.Left + Offset;
      Rect.Right := Rect.Left + Canvas.TextWidth(Items[Index]) + 6;
    end;
    FillRect(Rect);
    if odSelected in State then
      DrawFocusRect(Rect);
    Inc(Rect.Left, 3);
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, Rect,
      DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  end;
end;

procedure TJvDriveCombo.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHeight;
end;

procedure TJvDriveCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvDriveCombo.ResetItemHeight;
var
  nuHeight: Integer;
begin
  nuHeight := GetItemHeight(Font);
  if nuHeight < FImages.Height then
    nuHeight := FImages.Height;
  ItemHeight := nuHeight;
end;

procedure TJvDriveCombo.SetJvDriveTypes(Value: TJvDriveTypes);
begin
  FDriveTypes := Value;
  if FDriveTypes = [] then
    FDriveTypes := [dtFixed];
  BuildList;
  // SetDrive(FDrive);
end;

procedure TJvDriveCombo.SetDrive(Value: char);
var i, j: integer;
begin
  j := 0;
  if FItemIndex <> -1 then
    j := FItemIndex;

  Value := UpCase(Value);
  if FDrive <> Value then
  begin
    i := FDrives.IndexOf(Value);
    if i > -1 then
    begin
      FDrive := Value;
      FItemIndex := i;
      ItemIndex := i;
      if FDirList <> nil then
        FDirList.DriveChange(FDrive);
      Change;
      Exit;
    end;
  end
  else
    ItemIndex := j;
end;

procedure TJvDriveCombo.SetJvImageSize(Value: TJvImageSize);
begin
  if FImageSize <> Value then
  begin
    FIMageSize := Value;

    if Items.Count > 0 then
      Items.Clear;
    if Assigned(FImages) then
      FImages.Free;

    if Value = isSmall then
      FImages := TImagelist.CreateSize(FSmall, FSmall)
    else
      FImages := TImagelist.CreateSize(FLarge, FLarge);

    FImages.DrawingStyle := dsTransparent;
    FImages.ShareImages := True;
    FImageWidth := FImages.Width;
    ResetItemHeight;
    RecreateWnd;
    BuildList;
    Change;
  end;
end;

procedure TJvDriveCombo.SetOffset(Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Refresh;
  end;
end;

procedure TJvDriveCombo.Change;
begin
  if ItemIndex <> -1 then
    FItemIndex := ItemIndex;
  SetDrive(FDrives[FItemIndex][1]);
  if (ItemIndex > -1) and (ItemIndex < Items.Count) then
    FDisplayName := Items[ItemIndex]
  else
    FDisplayName := '';
  inherited;
end;

procedure TJvDriveCombo.CNCommand(var Message: TWMCommand);
begin
  inherited;
  case Message.NotifyCode of
    {    CBN_EDITCHANGE:
          Change;}
    CBN_SELCHANGE:
      Change;
  end;
end;

{ TJvDriveList }

constructor TJvDriveList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLarge := GetSystemMetrics(SM_CXICON);
  FSmall := GetSystemMetrics(SM_CXSMICON);

  FDrives := TStringList.Create;
  FDriveTypes := dtStandard;
  FImageAlign := iaCentered;

  if FImageSize = isSmall then
    FImages := TImagelist.CreateSize(FSmall, FSmall)
  else
    FImages := TImagelist.CreateSize(FLarge, FLarge);

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
  Info: TSHFileInfo; S: string;
  Options: integer;
  ADrive: char;
  tmp: array[0..104] of char;
  P: PChar;
  LastErrorMode: Cardinal;
begin
  if Items.Count > 0 then
  begin
    FItemIndex := 0;
    Items.Clear;
    FDrives.Clear;
  end;

  Options := SHGFI_SYSICONINDEX;
  if FImageSize = isSmall then
    Options := Options or SHGFI_SMALLICON
  else
    Options := Options or SHGFI_LARGEICON;

  FImages.Handle := SHGetFileInfo('', 0, Info, SizeOf(TShFileInfo), Options);
  FImages.ShareImages := true;
  FillChar(tmp[0], sizeof(tmp), #0);
  ADrive := Drive;
  LastErrorMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    GetLogicalDriveStrings(sizeof(tmp), tmp);
    P := tmp;
    while P^ <> #0 do
    begin
      S := string(P);
      Inc(P, 4);
      SHGetFileInfo(PChar(S), 0, Info, SizeOf(TShFileInfo), SHGFI_DISPLAYNAME or Options);
      Items.AddObject(Trim(Info.szDisplayName), TObject(Info.iIcon));
      FDrives.Add(S[1]);
    end;
    SetDrive(ADrive);
    Update;
  finally
    SetErrorMode(LastErrorMode);
  end;
end;

procedure TJvDriveList.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
  if (csDesigning in ComponentState) then
    SetDrive(GetCurrentDir[1]);
end;

procedure TJvDriveList.Refresh;
begin
  BuildList;
end;

procedure TJvDriveList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State, odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State, odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State, odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State, odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State, odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State, odGrayed);
    if bool(itemState and ODS_SELECTED) then
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

procedure TJvDriveList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  HOffset, i: Integer;
  tmpCol: TColor;
  tmpR: TRect;
begin
  with Canvas do
  begin
    tmpCol := Canvas.Brush.Color;
    Canvas.Brush.Color := self.COlor;
    FillRect(Rect);
    Canvas.Brush.Color := tmpCol;
    if FImageAlign = iaCentered then
    begin
      HOffset := (Rect.Right - Rect.Left) div 2 - FImageWidth div 2;
      if FImages.Count > 0 then
      begin
        i := integer(Items.Objects[Index]);
        FImages.Draw(Canvas, HOffset, Rect.Top, i);
      end;
      InflateRect(Rect, 1, -6);
      tmpR := Rect;
      DrawText(Canvas.Handle, PChar(Items[Index]), -1, tmpR,
        DT_SINGLELINE or DT_BOTTOM or DT_CENTER or DT_NOPREFIX or DT_CALCRECT);
      Rect.Top := tmpR.Bottom - Canvas.TextHeight('Wq');
      Rect.Left := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(PChar(Items[Index])) div 2;
      Rect.Right := Rect.Left + Canvas.TextWidth(PChar(Items[Index]));
      DrawText(Canvas.Handle, PChar(Items[Index]), -1, Rect,
        DT_SINGLELINE or DT_CENTER or DT_NOPREFIX);
    end
    else
    begin
      if FImages.Count > 0 then
      begin
        i := integer(Items.Objects[Index]);
        FImages.Draw(Canvas, Rect.Left + FOffset * 2, Rect.Top + FOffset * 2, i);
      end;
      tmpR := Rect;
      DrawText(Canvas.Handle, PChar(Items[Index]), -1, tmpR,
        DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_NOPREFIX or DT_CALCRECT);
      Rect.Top := tmpR.Bottom - Canvas.TextHeight('Wq');
      Rect.Bottom := Rect.Top + Canvas.TextHeight('Wq');
      Rect.Left := FImageWidth + FOffset * 3;
      Rect.Right := Rect.Left + Canvas.TextWidth(PChar(Items[Index]));
      DrawText(Canvas.Handle, PChar(Items[Index]), -1, Rect,
        DT_SINGLELINE or DT_TOP or DT_NOPREFIX);
    end;
  end;
  if odFocused in State then
    DrawFocusRect(Canvas.Handle, Rect);
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

procedure TJvDriveList.MeasureItem(Index: Integer; var Height: Integer);
begin
  if FImageAlign = iaCentered then
    Height := FImageWidth + GetItemHeight(Font)
  else
    Height := Max(GetItemHeight(Font), FImageWidth);
end;

procedure TJvDriveList.SetJvImageAlign(Value: TJvImageAlign);
begin
  if FImageAlign <> Value then
  begin
    FImageAlign := Value;
    Invalidate;
  end;
end;

procedure TJvDriveList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvDriveList.ResetItemHeight;
begin
  ItemHeight := GetItemHeight(Font) + FImageWidth + 8;
end;

procedure TJvDriveList.SetJvDriveTypes(Value: TJvDriveTypes);
begin
  FDriveTypes := Value;
  if FDriveTypes = [] then
    FDriveTypes := [dtFixed];
  BuildList;
end;

procedure TJvDriveList.SetDrive(Value: char);
var i, j: integer;
begin
  j := 0;
  if FItemIndex <> -1 then
    j := FItemIndex;

  Value := UpCase(Value);
  if FDrive <> Value then
  begin
    i := FDrives.IndexOf(Value);
    if i > -1 then
    begin
      FDrive := Value;
      FItemIndex := i;
      ItemIndex := i;
      Exit;
    end;
  end
  else
    ItemIndex := j;
end;

procedure TJvDriveList.SetJvImageSize(Value: TJvImageSize);
begin
  if FImageSize <> Value then
  begin
    FImageSize := Value;
    if Items.Count > 0 then
      Items.Clear;
    if Assigned(FImages) then
      FImages.Free;

    if Value = isSmall then
      FImages := TImagelist.CreateSize(FSmall, FSmall)
    else
      FImages := TImagelist.CreateSize(FLarge, FLarge);

    FImages.DrawingStyle := dsTransparent;
    FImages.ShareImages := True;
    FImageWidth := FImages.Width;
    ResetItemHeight;
    RecreateWnd;
    BuildList;
    Change;
  end;
end;

procedure TJvDriveList.SetOffset(Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    Refresh;
  end;
end;

procedure TJvDriveList.WMSize(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TJvDriveList.Change;
begin
  if ItemIndex <> -1 then
    FItemIndex := ItemIndex;
  SetDrive(FDrives[FItemIndex][1]);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDriveList.CNCommand(var Message: TWMCommand);
begin

  inherited;
  case Message.NotifyCode of
    {    CBN_EDITCHANGE:
          Change;}
    CBN_SELCHANGE:
      Change;
  end;
end;

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
    Result := AddPathBackSlash(S);
    Exit;
  end;
  if AnsiLastChar(Path)^ <> '\' then
    Result := Path + '\' + S
  else
    Result := Path + S;
end;

{ TJvDirectoryListBox }

constructor TJvDirectoryListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 145;
  Style := lbOwnerDrawFixed;
  Sorted := False;
  FAutoExpand := true;
  FImages := TImagelist.Create(self);
  FImages.ShareImages := true;
  ReadBitmaps;
  GetDir(0, FDirectory);
  ResetItemHeight;
end;

destructor TJvDirectoryListBox.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDirectoryListBox.DriveChange(NewDrive: Char);
var OldMode: Cardinal;
begin
  if (UpCase(NewDrive) <> UpCase(Drive)) then
  begin
    if NewDrive <> #0 then
    begin
      OldMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
      try
        ChDir(NewDrive + ':');
        GetDir(0, FDirectory); { store correct directory name }
      finally
        SetErrorMode(OldMode);
      end;
    end;
    if not FInSetDir then
    begin
      BuildList;
      Change;
    end;
  end;
end;

procedure TJvDirectoryListBox.SetFileListBox(Value: TJvFileListBox);
begin
  if FFileList <> nil then
    FFileList.FDirList := nil;
  FFileList := Value;
  if FFileList <> nil then
    FFileList.FreeNotification(Self);
end;

procedure TJvDirectoryListBox.SetDirLabel(Value: TLabel);
begin
  FDirLabel := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  SetDirLabelCaption;
end;

procedure TJvDirectoryListBox.SetDir(const NewDirectory: string);
var OldMode: Cardinal;
begin
  OldMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    if DirectoryExists(FDirectory) then
      ChDir(FDirectory);
    ChDir(NewDirectory); { exception raised if invalid dir }
    GetDir(0, FDirectory); { store correct directory name }
  finally
    SetErrorMode(OldMode);
  end;
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
  DirectoryList: TStringList): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  Result := 0;
  Status := FindFirst(ConcatPaths(ParentDirectory, '*.*'), faDirectory, SearchRec);
  try
    while Status = 0 do
    begin
      if (SearchRec.Attr and faDirectory = faDirectory) then
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
  end;
end;

procedure TJvDirectoryListBox.BuildList;
var
  TempPath: string;
  DirName: string;
  BackSlashPos: Integer;
  I: Integer;
  Siblings: TStringList;
  NewSelect: Integer;
  tmpFolder: string;
  psfi: TShFileInfo;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    TempPath := Directory;
    tmpFolder := '';
    if (Length(TempPath) > 0) then
    begin
      if AnsiLastChar(TempPath)^ <> '\' then
      begin
        BackSlashPos := AnsiPos('\', TempPath);
        while BackSlashPos <> 0 do
        begin
          DirName := Copy(TempPath, 1, BackSlashPos - 1);
          tmpFolder := ConcatPaths(tmpFolder, DirName);
          Delete(TempPath, 1, BackSlashPos);
          ShGetFileInfo(PChar(tmpFolder), 0, psfi, sizeof(TSHFileInfo), SHGFI_ICON or SHGFI_SMALLICON or SHGFI_OPENICON);
          Items.AddObject(tmpFolder, TObject(psfi.iIcon));
          BackSlashPos := AnsiPos('\', TempPath);
        end;
      end;
      // add the selected dir:
      ShGetFileInfo(PChar(Directory), 0, psfi, sizeof(TSHFileInfo), SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SELECTED or SHGFI_OPENICON);
      Items.AddObject(Directory, TObject(psfi.iIcon));
    end;
    NewSelect := Items.Count - 1;

    Siblings := TStringList.Create;
    try
      Siblings.Sorted := True;
      { read all the subdir names into Siblings }
      ReadDirectoryNames(Directory, Siblings);
      for i := 0 to Siblings.Count - 1 do
      begin
        ShGetFileInfo(PChar(Siblings[i]), 0, psfi, sizeof(TSHFileInfo), SHGFI_ICON or SHGFI_SMALLICON);
        Items.AddObject(Siblings[i], TObject(psfi.iIcon));
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
var psfi: TShFileInfo;
begin
  FImages.Handle := ShGetFileInfo('', 0, psfi, sizeof(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FImages.ShareImages := true;
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

function ExtractLastFolder(const S: string): string;
var P: PChar;
begin
  Result := S;
  if Length(S) <= 3 then
    Exit;
  P := PChar(S);
  Inc(P, Length(S) - 1);
  while P^ <> '\' do
    Dec(P);
  Inc(p);
  Result := P;
end;

procedure TJvDirectoryListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
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

procedure TJvDirectoryListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  bmpWidth: Integer;
  dirOffset: Integer;
  S: string;
  psfi: TSHFileInfo;
  tmpR: TRect;
begin
  with Canvas do
  begin
    tmpR := Rect;
    bmpWidth := FImages.Width;
    if Index = 0 then
      dirOffset := Rect.Left + 2
    else
      dirOffset := Rect.Left + (DirLevel(Items[Index]) + 1) * 4 + 2;
    FImages.Draw(Canvas, dirOffset, (Rect.Top + Rect.Bottom - FImages.Height) div 2,
      integer(Items.Objects[Index]));
    ShGetFileInfo(PChar(Items[Index]), 0, psfi, sizeof(TSHFileInfo), SHGFI_DISPLAYNAME);
    S := psfi.szDisplayName; // (Items[Index]);

    tmpR.Left := tmpR.Left + dirOffset + FImages.Width + 2;
    tmpR.Right := tmpR.Left + TextWidth(S) + 4;
    FillRect(tmpR);
    TextOut(Rect.Left + bmpWidth + dirOffset + 4, Rect.Top + 2, S);
    if odFocused in State then
      DrawFocusRect(tmpR);
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

procedure TJvDirectoryListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

procedure TJvDirectoryListBox.ResetItemHeight;
var
  nuHeight: Integer;
begin
  nuHeight := GetItemHeight(Font);
  if nuHeight < (FImages.Height + 1) then
    nuHeight := FImages.Height + 1;
  ItemHeight := nuHeight;
end;

function TJvDirectoryListBox.GetDrive: char;
begin
  Result := FDirectory[1];
end;

procedure TJvDirectoryListBox.SetDrive(Value: char);
begin
  if (UpCase(Value) <> UpCase(Drive)) then
    SetDirectory(Format('%s:', [Value]));
end;

procedure TJvDirectoryListBox.SetDirectory(const NewDirectory: string);
var
  DirPart: string;
  FilePart: string;
  NewDrive: Char;
begin
  if Length(NewDirectory) = 0 then
    Exit;
  if (AnsiCompareText(NewDirectory, Directory) = 0) then
    Exit;
  ProcessPath(NewDirectory, NewDrive, DirPart, FilePart);
  try
    if Drive <> NewDrive then
    begin
      FInSetDir := True;
      if (FDriveCombo <> nil) then
        FDriveCombo.Drive := Drive
      else
        DriveChange(NewDrive);
    end;
  finally
    FInSetDir := False;
  end;
  SetDir(DirPart);
end;

procedure TJvDirectoryListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Word(Key) = VK_RETURN) then
    OpenCurrent;
end;

procedure TJvDirectoryListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FFileList) then
      FFileList := nil
    else if (AComponent = FDriveCombo) then
      FDriveCombo := nil
    else if (AComponent = FDirLabel) then
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

constructor TJvFileListBox.Create(AOwner: TComponent);
var shi: TSHFileInfo;
begin
  inherited Create(AOwner);
  FImages := TImageList.CreateSize(16, 16);
  FImages.ShareImages := True;
  FImages.Handle := SHGetFileInfo('', 0, shi, sizeof(shi), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FImages.Drawingstyle := dsTransparent;
end;

destructor TJvFileListBox.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

procedure TJvFileListBox.ReadFileNames;
var
  AttrIndex: TFileAttr;
  I: Integer;
  MaskPtr: PChar;
  Ptr: PChar;
  AttrWord: Word;
  FileInfo: TSearchRec;
  SaveCursor: TCursor;
const
  Attributes: array[TFileAttr] of Word = (faReadOnly, faHidden, faSysFile,
    faVolumeID, faDirectory, faArchive, 0);
var shi: TSHFileInfo; OldMode: Cardinal;
begin
  AttrWord := DDL_READWRITE;
  if HandleAllocated then
  begin
    { Set attribute flags based on values in FileType }
    for AttrIndex := ftReadOnly to ftArchive do
      if AttrIndex in FileType then
        AttrWord := AttrWord or Attributes[AttrIndex];
    OldMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
    try
      ChDir(FDirectory); { go to the directory we want }
    finally
      SetErrorMode(OldMode);
    end;
    Clear; { clear the list }

    I := 0;
    SaveCursor := Screen.Cursor;
    try
      MaskPtr := PChar(FMask);
      while MaskPtr <> nil do
      begin
        Ptr := StrScan(MaskPtr, ';');
        if Ptr <> nil then
          Ptr^ := #0;
        if FindFirst(MaskPtr, AttrWord, FileInfo) = 0 then
        begin
          repeat
            if (ftNormal in FileType) or (FileInfo.Attr and AttrWord <> 0) then
              if (FileInfo.Attr and faDirectory <> 0) then
              begin
                if (FileInfo.Name[1] <> '.') then
                begin
                  SHGetFileInfo(PChar(AddPathBackSlash(FDirectory) + FileInfo.Name), 0, shi, sizeof(shi), SHGFI_SYSICONINDEX or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_DISPLAYNAME);
                  I := Items.Add(Format(cDirPrefix + '%s', [shi.szDisplayName]));
                  Items.Objects[I] := TObject(shi.iIcon);
                end;
              end
              else if (FileInfo.Name[1] <> '.') then
              begin
                SHGetFileInfo(PChar(AddPathBackSlash(FDirectory) + FileInfo.Name), 0, shi, sizeof(shi), SHGFI_SYSICONINDEX or SHGFI_ICON or SHGFI_SMALLICON or SHGFI_DISPLAYNAME);
                I := Items.Add(shi.szDisplayName);
                Items.Objects[I] := TObject(shi.iIcon);
              end;
            if I = 100 then
              Screen.Cursor := crHourGlass;
          until FindNext(FileInfo) <> 0;
          FindClose(FileInfo);
        end;
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          Inc(Ptr);
        end;
        MaskPtr := Ptr;
      end;
    finally
      Screen.Cursor := SaveCursor;
    end;
    Change;
  end;
end;

procedure TJvFileListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
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
  offset: Integer;
  tmpR: TRect;
begin
  with Canvas do
  begin
    //    FillRect(Rect);
    offset := 2;
    tmpR := Rect;
    if ShowGlyphs then
    begin
      FImages.Draw(Canvas, Rect.Left + 2, (Rect.Top + Rect.Bottom - FImages.Height) div 2, integer(Items.Objects[index]));
      Offset := FImages.Width + 6;
    end;
    tmpR.Left := tmpR.Left + offset - 2;
    tmpR.Right := tmpR.Left + TextWidth(Items[Index]) + 4;
    FillRect(tmpR);
    TextOut(Rect.Left + offset, Rect.Top, trim(Items[Index]));

    if odFocused in State then
      DrawFocusRect(tmpR);
  end;
end;

procedure TJvDirectoryListBox.SetDriveCombo(const Value: TJvDriveCombo);
begin
  if FDriveCombo <> nil then
    FDriveCombo.FDirList := nil;
  FDriveCombo := Value;
  if FDriveCombo <> nil then
  begin
    FDriveCombo.FDirList := self;
    FDriveCombo.Drive := Drive;
    FDriveCombo.FreeNotification(Self);
  end;
end;

procedure TJvDirectoryListBox.Click;
begin
  if FAutoExpand then
    OpenCurrent;
  inherited;
end;

procedure TJvDirectoryListBox.SetAutoExpand(const Value: boolean);
begin
  FAutoExpand := Value;
end;

end.

