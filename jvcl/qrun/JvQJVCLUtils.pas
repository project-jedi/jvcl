{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJVCLUtils.PAS, released on 2002-09-24.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQJVCLUtils;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  {$IFDEF MSWINDOWS}
  Windows, Messages, ShellAPI, Registry,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Classes, 
  Qt, QWinCursors, QWindows, 
  QForms, QGraphics, QControls, QStdCtrls, QExtCtrls, QMenus,
  QDialogs, QComCtrls, QImgList, QGrids, IniFiles,
  {$IFNDEF NO_JCL}
  JclBase,
  {$ENDIF !NO_JCL}
  JvQJCLUtils, JvQAppStorage, JvQTypes, JvQFinalize;



function Icon2Bitmap(Ico: TIcon): TBitmap;
function Bitmap2Icon(Bmp: TBitmap): TIcon;




// bitmap manipulation functions
// NOTE: Dest bitmap must be freed by caller!
// get red channel bitmap
procedure GetRBitmap(var Dest: TBitmap; const Source: TBitmap);
// get green channel bitmap
procedure GetGBitmap(var Dest: TBitmap; const Source: TBitmap);
// get blue channel bitmap
procedure GetBBitmap(var Dest: TBitmap; const Source: TBitmap);
// get monochrome bitmap
procedure GetMonochromeBitmap(var Dest: TBitmap; const Source: TBitmap);
// get hue bitmap (h part of hsv)
procedure GetHueBitmap(var Dest: TBitmap; const Source: TBitmap);
// get saturation bitmap (s part of hsv)
procedure GetSaturationBitmap(var Dest: TBitmap; const Source: TBitmap);
// get value bitmap (V part of HSV)
procedure GetValueBitmap(var Dest: TBitmap; const Source: TBitmap);



{$IFDEF MSWINDOWS}

type
  TJvWallpaperStyle = (wpTile, wpCenter, wpStretch);

// set the background wallpaper (two versions)
procedure SetWallpaper(const Path: string); overload;
procedure SetWallpaper(const Path: string; Style: TJvWallpaperStyle); overload;

(* (rom) to be deleted. Use ScreenShot from JCL
{$IFDEF VCL}
// screen capture functions
function CaptureScreen(IncludeTaskBar: Boolean = True): TBitmap; overload;
function CaptureScreen(Rec: TRect): TBitmap; overload;
function CaptureScreen(WndHandle: Longword): TBitmap; overload;
{$ENDIF VCL}
*)

{$ENDIF MSWINDOWS}

procedure RGBToHSV(R, G, B: Integer; var H, S, V: Integer);

{ from JvVCLUtils }

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
{ Windows resources (bitmaps and icons) VCL-oriented routines }
procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY, DstW,
  DstH: Integer; SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
function MakeBitmap(ResID: PChar): TBitmap;
function MakeBitmapID(ResID: Word): TBitmap;
function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
function CreateTwoColorsBrushPattern(Color1, Color2: TColor): TBitmap;
function CreateDisabledBitmap_NewStyle(FOriginal: TBitmap; BackColor: TColor):
  TBitmap;
function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighLightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor):
  TBitmap;
procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap; Cols, Rows,
  Index: Integer);
function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighLightColor, GrayColor: TColor;
  DrawHighlight: Boolean);

function MakeIcon(ResID: PChar): TIcon;
function MakeIconID(ResID: Word): TIcon;
function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;


{ Execute executes other program and waiting for it
  terminating, then return its Exit Code }
function Execute(const CommandLine, WorkingDirectory: string): Integer;

// launches the specified CPL file
// format: <Filename> [,@n] or [,,m] or [,@n,m]
// where @n = zero-based index of the applet to start (if there is more than one
// m is the zero-based index of the tab to display


function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
function PaletteColor(Color: TColor): Longint;
procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);

procedure Delay(MSecs: Longint);
procedure CenterControl(Control: TControl);

procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
function GetAveCharSize(Canvas: TCanvas): TPoint;

{ Gradient filling routine }

type
  TFillDirection = (fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft);

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);

procedure StartWait;
procedure StopWait;
function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
function GetNextFreeCursorIndex(StartHint: Integer; PreDefined: Boolean):
  Integer;
function WaitCursor: IInterface;
function ScreenCursor(ACursor: TCursor): IInterface;
{$IFDEF MSWINDOWS}
// loads the more modern looking drag cursors from OLE32.DLL
function LoadOLEDragCursors: Boolean;
// set some default cursor from JVCL
{$ENDIF MSWINDOWS}
procedure SetDefaultJVCLCursors;


function ScreenWorkArea: TRect;

{ Grid drawing }

type
  TVertAlignment = (vaTopJustify, vaCenterJustify, vaBottomJustify);

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean; ARightToLeft:
  Boolean = False);
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean); overload;
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean);
overload;
procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);



type
  TJvDesktopCanvas = class(TQtCanvas)
  protected
    procedure CreateHandle; override;
  public
    procedure SetOrigin(X, Y: Integer);
  end;


  { end from JvVCLUtils }

  { begin JvUtils }
  {**** other routines - }
  { FindByTag returns the control with specified class,
    ComponentClass, from WinContol.Controls property,
    having Tag property value, equaled to Tag parameter }
function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass;
  const Tag: Integer): TComponent;
{ ControlAtPos2 equal to TWinControl.ControlAtPos function,
  but works better }
function ControlAtPos2(Parent: TWinControl; X, Y: Integer): TControl;
{ RBTag searches WinControl.Controls for checked
  RadioButton and returns its Tag property value }
function RBTag(Parent: TWinControl): Integer;
{ FindFormByClass returns first form with specified
  class, FormClass, owned by Application global variable }
function FindFormByClass(FormClass: TFormClass): TForm;
function FindFormByClassName(const FormClassName: string): TForm;
{ AppMinimized returns True, if Application is minimized }
function AppMinimized: Boolean;
function IsForegroundTask: Boolean;


{ returns the sum of pc.Left, pc.Width and piSpace}
function ToRightOf(const pc: TControl; piSpace: Integer = 0): Integer;
{ sets the top of pc to be in the middle of pcParent }
procedure CenterHeight(const pc, pcParent: TControl);
procedure CenterHor(Parent: TControl; MinLeft: Integer; Controls: array of TControl);
procedure EnableControls(Control: TWinControl; const Enable: Boolean);
procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: Integer; const Enable: Boolean);
procedure ExpandWidth(Parent: TControl; MinWidth: Integer; Controls: array of TControl);
function PanelBorder(Panel: TCustomPanel): Integer;
function Pixels(Control: TControl; APixels: Integer): Integer;

type
  TMenuAnimation = (maNone, maRandom, maUnfold, maSlide);

procedure ShowMenu(Form: TForm; MenuAni: TMenuAnimation);

{$IFDEF MSWINDOWS}
{ TargetFileName - if FileName is ShortCut returns filename ShortCut linked to }
function TargetFileName(const FileName: TFileName): TFileName;
{ return filename ShortCut linked to }
function ResolveLink(const HWND: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
{$ENDIF MSWINDOWS}

type
  TProcObj = procedure of object;

procedure ExecAfterPause(Proc: TProcObj; Pause: Integer);

{ end JvUtils }

{ begin JvAppUtils}
function GetDefaultSection(Component: TComponent): string;
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;
  TPlacementOption = (fpState, fpSize, fpLocation, fpActiveControl);
  TPlacementOptions = set of TPlacementOption;
  TPlacementOperation = (poSave, poRestore);

var
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;

function GetDefaultIniRegKey: string;
function FindForm(FormClass: TFormClass): TForm;
function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
function ShowDialog(FormClass: TFormClass): Boolean;
function InstantiateForm(FormClass: TFormClass; var Reference): TForm;

procedure SaveFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage; Options: TPlacementOptions);
procedure RestoreFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage; Options: TPlacementOptions);

procedure SaveMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage);
procedure RestoreMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage);
procedure RestoreGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage);
procedure SaveGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage);

function StrToIniStr(const Str: string): string;
function IniStrToStr(const Str: string): string;

// Ini Utilitie Functions
// Added by RDB

function FontStylesToString(Styles: TFontStyles): string;
function StringToFontStyles(const Styles: string): TFontStyles;

function RectToStr(Rect: TRect): string;
function StrToRect(const Str: string; const Def: TRect): TRect;
function PointToStr(P: TPoint): string;
function StrToPoint(const Str: string; const Def: TPoint): TPoint;

{
function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  Default: Longint): Longint;
procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
procedure IniReadSections(IniFile: TObject; Strings: TStrings);
procedure IniEraseSection(IniFile: TObject; const Section: string);
procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
}



{ Internal using utilities }

procedure InternalSaveFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage;
  const StorePath: string; Options: TPlacementOptions = [fpState, fpSize, fpLocation]);
procedure InternalRestoreFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage;
  const StorePath: string; Options: TPlacementOptions = [fpState, fpSize, fpLocation]);
procedure InternalSaveGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage; const StorePath: string);
procedure InternalRestoreGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage; const StorePath: string);
procedure InternalSaveMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage; const StorePath: string);
procedure InternalRestoreMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage; const StorePath: string);

{ end JvAppUtils }
{ begin JvGraph }
type
  TMappingMethod = (mmHistogram, mmQuantize, mmTrunc784, mmTrunc666,
    mmTripel, mmGrayscale);

function GetBitmapPixelFormat(Bitmap: TBitmap): TPixelFormat;


procedure TileImage(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
function ZoomImage(ImageW, ImageH, MaxW, MaxH: Integer; Stretch: Boolean): TPoint;

type
  TJvGradientOptions = class(TPersistent)
  private
    FStartColor: TColor;
    FEndColor: TColor;
    FDirection: TFillDirection;
    FStepCount: Byte;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetStartColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetDirection(Value: TFillDirection);
    procedure SetStepCount(Value: Byte);
    procedure SetVisible(Value: Boolean);
  protected
    procedure Changed; dynamic;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(Canvas: TCanvas; Rect: TRect);
  published
    property Direction: TFillDirection read FDirection write SetDirection default fdTopToBottom;
    property EndColor: TColor read FEndColor write SetEndColor default clGray;
    property StartColor: TColor read FStartColor write SetStartColor default clSilver;
    property StepCount: Byte read FStepCount write SetStepCount default 64;
    property Visible: Boolean read FVisible write SetVisible default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
{ end JvGraph }

type
  // equivalent of TPoint, but that can be a published property for BCB
  TJvPoint = class(TPersistent)
  private
    FY: Longint;
    FX: Longint;
    FOnChange: TNotifyEvent;
    procedure SetX(Value: Longint);
    procedure SetY(Value: Longint);
  protected
    procedure DoChange;
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: Longint read FX write SetX;
    property Y: Longint read FY write SetY;
  end;

  // equivalent of TRect, but that can be a published property for BCB
  TJvRect = class(TPersistent)
  private
    FTopLeft: TJvPoint;
    FBottomRight: TJvPoint;
    FOnChange: TNotifyEvent;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetBottom(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetRight(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetBottomRight(Value: TJvPoint);
    procedure SetTopLeft(Value: TJvPoint);
    procedure PointChange(Sender: TObject);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TopLeft: TJvPoint read FTopLeft write SetTopLeft;
    property BottomRight: TJvPoint read FBottomRight write SetBottomRight;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Right: Integer read GetRight write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
  end;

{ begin JvCtrlUtils }

//------------------------------------------------------------------------------
// ToolBarMenu
//------------------------------------------------------------------------------

procedure JvCreateToolBarMenu(AForm: TForm; AToolBar: TToolBar;
  AMenu: TMainMenu = nil);

//------------------------------------------------------------------------------
// ListView functions
//------------------------------------------------------------------------------

type
  PJvLVItemStateData = ^TJvLVItemStateData;
  TJvLVItemStateData = record
    Caption: string;
    Data: Pointer;
    Focused: Boolean;
    Selected: Boolean;
  end;

{ listview functions }
function ConvertStates(const State: Integer): TItemStates;

function ChangeHasDeselect(const peOld, peNew: TItemStates): Boolean;
function ChangeHasSelect(const peOld, peNew: TItemStates): Boolean;

function ChangeHasDefocus(const peOld, peNew: TItemStates): Boolean;
function ChangeHasFocus(const peOld, peNew: TItemStates): Boolean;

function GetListItemColumn(const pcItem: TListItem; piIndex: Integer): string;

procedure JvListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean = False; Headers: Boolean = True);

function JvListViewSafeSubItemString(Item: TListItem; SubItemIndex: Integer): string;

procedure JvListViewSortClick(Column: TListColumn;
  AscendingSortImage: Integer = -1; DescendingSortImage: Integer = -1);

procedure JvListViewCompare(ListView: TListView; Item1, Item2: TListItem;
  var Compare: Integer);

procedure JvListViewSelectAll(ListView: TListView; Deselect: Boolean = False);

function JvListViewSaveState(ListView: TListView): TJvLVItemStateData;

function JvListViewRestoreState(ListView: TListView; Data: TJvLVItemStateData;
  MakeVisible: Boolean = True; FocusFirst: Boolean = False): Boolean;



//------------------------------------------------------------------------------
// MessageBox
//------------------------------------------------------------------------------

function JvMessageBox(const Text, Caption: string; Flags: DWORD): Integer; overload;
function JvMessageBox(const Text: string; Flags: DWORD): Integer; overload;

{ end JvCtrlUtils }

procedure UpdateTrackFont(TrackFont, Font: TFont; TrackOptions: TJvTrackFontOptions);
// Returns the size of the image
// used for checkboxes and radiobuttons.
// Originally from Mike Lischke
function GetDefaultCheckBoxSize: TSize;

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;

{$IFDEF MSWINDOWS}
// AllocateHWndEx works like Classes.AllocateHWnd but does not use any virtual memory pages
function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): Windows.HWND;
// DeallocateHWndEx works like Classes.DeallocateHWnd but does not use any virtual memory pages
procedure DeallocateHWndEx(Wnd: Windows.HWND);

function JvMakeObjectInstance(Method: TWndMethod): Pointer;
procedure JvFreeObjectInstance(ObjectInstance: Pointer);
{$ENDIF  MSWINDOWS}



function GetAppHandle: HWND;
// DrawArrow draws a standard arrow in any of four directions and with the specifed color.
// Rect is the area to draw the arrow in and also defines the size of the arrow
// Note that this procedure might shrink Rect so that it's width and height is always
// the same and the width and height are always even, i.e calling with
// Rect(0,0,12,12) (odd) is the same as calling with Rect(0,0,11,11) (even)
// Direction defines the direction of the arrow. If Direction is akLeft, the arrow point is
// pointing to the left
procedure DrawArrow(Canvas: TCanvas; Rect: TRect; Color: TColor = clBlack; Direction: TAnchorKind = akBottom);

function IsPositiveResult(Value: TModalResult): Boolean;
function IsNegativeResult(Value: TModalResult): Boolean;
function IsAbortResult(const Value: TModalResult): Boolean;
function StripAllFromResult(const Value: TModalResult): TModalResult;
// returns either BrightColor or DarkColor depending on the luminance of AColor
// This function gives the same result (AFAIK) as the function used in Windows to
// calculate the desktop icon text color based on the desktop background color
function SelectColorByLuminance(AColor, DarkColor, BrightColor: TColor): TColor;

implementation

uses
  SysConst, 
  {$IFDEF MSWINDOWS}
  CommCtrl, MMSystem, ShlObj, ActiveX,
  {$ENDIF MSWINDOWS} 
  QConsts, 
  Math,
  JclSysInfo,
  JvQConsts, JvQProgressUtils, JvQResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvConsts.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvConsts.res}
{$ENDIF LINUX}

const
  sUnitName = 'JvJVCLUtils';
  {$IFDEF MSWINDOWS}
  RC_ControlRegistry = 'Control Panel\Desktop';
  RC_WallPaperStyle = 'WallpaperStyle';
  RC_WallpaperRegistry = 'Wallpaper';
  RC_TileWallpaper = 'TileWallpaper';
  RC_RunCpl = 'rundll32.exe shell32,Control_RunDLL ';
  {$ENDIF MSWINDOWS}

function GetAppHandle: HWND;
begin  
  Result := Application.AppWidget; 
end;

type
  TWaitCursor = class(TInterfacedObject, IInterface)
  private
    FCursor: TCursor;
  public
    constructor Create(ACursor: TCursor);
    destructor Destroy; override;
  end;

constructor TWaitCursor.Create(ACursor: TCursor);
begin
  inherited Create;
  FCursor := Screen.Cursor;
  Screen.Cursor := ACursor;
end;

destructor TWaitCursor.Destroy;
begin
  Screen.Cursor := FCursor;
  inherited Destroy;
end;


type
  TIconAccessProtected = class(TIcon);

function Icon2Bitmap(Ico: TIcon): TBitmap;
begin
  Result := TBitmap.Create;
  TIconAccessProtected(Ico).AssignTo(Result);
end;

function Bitmap2Icon(Bmp: TBitmap): TIcon;
begin
  Result := TIcon.Create;
  Result.Assign(Bmp);
end;




procedure RGBToHSV(R, G, B: Integer; var H, S, V: Integer);


var
  QC: QColorH;
begin
  QC := QColor_create(R, G, B);
  QColor_getHsv(QC, @H, @S, @V);
  QColor_destroy(QC);
end;


(* (rom) to be deleted. Use ScreenShot from JCL
{$IFDEF VCL}

function CaptureScreen(Rec: TRect): TBitmap;
const
  NumColors = 256;
var
  R: TRect;
  C: TCanvas;
  LP: PLogPalette;
  TmpPalette: HPALETTE;
  Size: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := Rec.Right - Rec.Left;
  Result.Height := Rec.Bottom - Rec.Top;
  R := Rec;
  C := TCanvas.Create;
  try
    C.Handle := GetDC(HWND_DESKTOP);
    Result.Canvas.CopyRect(Rect(0, 0, Rec.Right - Rec.Left, Rec.Bottom -
      Rec.Top), C, R);
    Size := SizeOf(TLogPalette) + (Pred(NumColors) * SizeOf(TPaletteEntry));
    LP := AllocMem(Size);
    try
      LP^.palVersion := $300;
      LP^.palNumEntries := NumColors;
      GetSystemPaletteEntries(C.Handle, 0, NumColors, LP^.palPalEntry);
      TmpPalette := CreatePalette(LP^);
      Result.Palette := TmpPalette;
      DeleteObject(TmpPalette);
    finally
      FreeMem(LP, Size);
    end
  finally
    ReleaseDC(HWND_DESKTOP, C.Handle);
    C.Free;
  end;
end;

function CaptureScreen(IncludeTaskBar: Boolean): TBitmap;
var
  R: TRect;
begin
  if IncludeTaskBar then
    R := Rect(0, 0, Screen.Width, Screen.Height)
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, Pointer(@R), 0);
  Result := CaptureScreen(R);
end;

function CaptureScreen(WndHandle: Longword): TBitmap;
var
  R: TRect;
  WP: TWindowPlacement;
begin
  if GetWindowRect(WndHandle, R) then
  begin
    GetWindowPlacement(WndHandle, @WP);
    if IsIconic(WndHandle) then
      ShowWindow(WndHandle, SW_RESTORE);
    BringWindowToTop(WndHandle);
    Result := CaptureScreen(R);
    SetWindowPlacement(WndHandle, @WP);
  end
  else
    Result := nil;
end;
{$ENDIF VCL}
*)

{$IFDEF MSWINDOWS}

procedure SetWallpaper(const Path: string);
begin
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE);
end;

procedure SetWallpaper(const Path: string; Style: TJvWallpaperStyle);
begin
  with TRegistry.Create do
  begin
    OpenKey(RC_ControlRegistry, False);
    case Style of
      wpTile:
        begin
          WriteString(RC_TileWallpaper, '1');
          WriteString(RC_WallPaperStyle, '0');
        end;
      wpCenter:
        begin
          WriteString(RC_TileWallpaper, '0');
          WriteString(RC_WallPaperStyle, '0');
        end;
      wpStretch:
        begin
          WriteString(RC_TileWallpaper, '0');
          WriteString(RC_WallPaperStyle, '2');
        end;
    end;
    WriteString(RC_WallpaperRegistry, Path);
    Free;
  end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

{$ENDIF MSWINDOWS}

procedure GetRBitmap(var Dest: TBitmap; const Source: TBitmap);
var
  I, J: Integer;
  Line: PJvRGBArray;
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.PixelFormat := pf24bit;
  for J := Dest.Height - 1 downto 0 do
  begin
    Line := Dest.ScanLine[J];
    for I := Dest.Width - 1 downto 0 do
    begin
      Line[I].rgbGreen := 0;
      Line[I].rgbBlue := 0;
    end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;

procedure GetBBitmap(var Dest: TBitmap; const Source: TBitmap);
var
  I, J: Integer;
  Line: PJvRGBArray;
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.PixelFormat := pf24bit;
  for J := Dest.Height - 1 downto 0 do
  begin
    Line := Dest.ScanLine[J];
    for I := Dest.Width - 1 downto 0 do
    begin
      Line[I].rgbRed := 0;
      Line[I].rgbGreen := 0;
    end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;

procedure GetGBitmap(var Dest: TBitmap; const Source: TBitmap);
var
  I, J: Integer;
  Line: PJvRGBArray;
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.PixelFormat := pf24bit;
  for J := Dest.Height - 1 downto 0 do
  begin
    Line := Dest.ScanLine[J];
    for I := Dest.Width - 1 downto 0 do
    begin
      Line[I].rgbRed := 0;
      Line[I].rgbBlue := 0;
    end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;

procedure GetMonochromeBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.Monochrome := True;
end;

procedure GetHueBitmap(var Dest: TBitmap; const Source: TBitmap);
var
  I, J, H, S, V: Integer;
  Line: PJvRGBArray;
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.PixelFormat := pf24bit;
  for J := Dest.Height - 1 downto 0 do
  begin
    Line := Dest.ScanLine[J];
    for I := Dest.Width - 1 downto 0 do
      with Line[I] do
      begin
        RGBToHSV(rgbRed, rgbGreen, rgbBlue, H, S, V);
        rgbRed := H;
        rgbGreen := H;
        rgbBlue := H;
      end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;

procedure GetSaturationBitmap(var Dest: TBitmap; const Source: TBitmap);
var
  I, J, H, S, V: Integer;
  Line: PJvRGBArray;
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.PixelFormat := pf24bit;
  for J := Dest.Height - 1 downto 0 do
  begin
    Line := Dest.ScanLine[J];
    for I := Dest.Width - 1 downto 0 do
      with Line[I] do
      begin
        RGBToHSV(rgbRed, rgbGreen, rgbBlue, H, S, V);
        rgbRed := S;
        rgbGreen := S;
        rgbBlue := S;
      end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;

procedure GetValueBitmap(var Dest: TBitmap; const Source: TBitmap);
var
  I, J, H, S, V: Integer;
  Line: PJvRGBArray;
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.PixelFormat := pf24bit;
  for J := Dest.Height - 1 downto 0 do
  begin
    Line := Dest.ScanLine[J];
    for I := Dest.Width - 1 downto 0 do
      with Line[I] do
      begin
        RGBToHSV(rgbRed, rgbGreen, rgbBlue, H, S, V);
        rgbRed := V;
        rgbGreen := V;
        rgbBlue := V;
      end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;



// (rom) a thread to wait would be more elegant, also JCL function available

function Execute(const CommandLine, WorkingDirectory: string): Integer;
{$IFDEF MSWINDOWS}
var
  R: Boolean;
  ProcessInformation: TProcessInformation;
  StartupInfo: TStartupInfo;
  ExCode: Cardinal;
begin
  Result := 0;
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_SHOW;
  end;
  R := CreateProcess(
    nil, // Pointer to name of executable module
    PChar(CommandLine), // Pointer to command line string
    nil, // Pointer to process security attributes
    nil, // Pointer to thread security attributes
    False, // handle inheritance flag
    0, // creation flags
    nil, // Pointer to new environment block
    PChar(WorkingDirectory), // Pointer to current directory name
    StartupInfo, // Pointer to STARTUPINFO
    ProcessInformation); // Pointer to PROCESS_INFORMATION
  if R then
    while (GetExitCodeProcess(ProcessInformation.hProcess, ExCode) and
      (ExCode = STILL_ACTIVE)) do
      Application.ProcessMessages
  else
    Result := GetLastError;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
begin
  if WorkingDirectory = '' then
    Result := Libc.system(PChar(Format('cd "%s" ; %s',
      [GetCurrentDir, CommandLine])))
  else
    Result := Libc.system(PChar(Format('cd "%s" ; %s',
      [WorkingDirectory, CommandLine])));
end;
{$ENDIF LINUX}



{ imported from VCLFunctions }

procedure CenterHeight(const pc, pcParent: TControl);
begin
  pc.Top := //pcParent.Top +
    ((pcParent.Height - pc.Height) div 2);
end;

function ToRightOf(const pc: TControl; piSpace: Integer): Integer;
begin
  if pc <> nil then
    Result := pc.Left + pc.Width + piSpace
  else
    Result := piSpace;
end;

{ compiled from ComCtrls.pas's implmentation section }

function HasFlag(A, B: Integer): Boolean;
begin
  Result := (A and B) <> 0;
end;

function ConvertStates(const State: Integer): TItemStates;
begin
  Result := []; 
end;

function ChangeHasSelect(const peOld, peNew: TItemStates): Boolean;
begin
  Result := (not (isSelected in peOld)) and (isSelected in peNew);
end;

function ChangeHasDeselect(const peOld, peNew: TItemStates): Boolean;
begin
  Result := (isSelected in peOld) and (not (isSelected in peNew));
end;

function ChangeHasFocus(const peOld, peNew: TItemStates): Boolean;
begin
  Result := (not (IsFocused in peOld)) and (IsFocused in peNew);
end;

function ChangeHasDefocus(const peOld, peNew: TItemStates): Boolean;
begin
  Result := (IsFocused in peOld) and (not (IsFocused in peNew));
end;

function GetListItemColumn(const pcItem: TListItem; piIndex: Integer): string;
begin
  if pcItem = nil then
  begin
    Result := '';
    Exit;
  end;

  if (piIndex < 0) or (piIndex > pcItem.SubItems.Count) then
  begin
    Result := '';
    Exit;
  end;

  if piIndex = 0 then
    Result := pcItem.Caption
  else
    Result := pcItem.SubItems[piIndex - 1];
end;

{from JvVCLUtils }

{ Bitmaps }



type
  TPrivateControl = class(TComponent)
  protected
    FVisible: Boolean;
  end;

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  Pixmap: QPixmapH;
  DestDev: QPaintDeviceH;
  pdm: QPaintDeviceMetricsH;
  OrigVisible: Boolean;
begin
  if (Control = nil) or (Control.Parent = nil) then
    Exit;
  Dest.Start;
  try
    DestDev := QPainter_device(Dest.Handle);
    with Control.Parent do
      ControlState := ControlState + [csPaintCopy];
    try
      pdm := QPaintDeviceMetrics_create(DestDev);
      try
        Pixmap := QPixmap_create(Control.Width, Control.Height,
          QPaintDeviceMetrics_depth(pdm), QPixmapOptimization_DefaultOptim);
      finally
        QPaintDeviceMetrics_destroy(pdm);
      end;
      OrigVisible := TPrivateControl(Control).FVisible;
      TPrivateControl(Control).FVisible := False; // do not draw the Control itself
      try
        QPixmap_grabWidget(Pixmap, Control.Parent.Handle, Control.Left,
          Control.Top, Control.Width, Control.Height);
        Qt.bitBlt(DestDev, 0, 0, Pixmap, 0, 0, Control.Width,
          Control.Height, Qt.RasterOp_CopyROP, True);
      finally
        TPrivateControl(Control).FVisible := OrigVisible;
        QPixmap_destroy(Pixmap);
      end;
    finally
      with Control.Parent do
        ControlState := ControlState - [csPaintCopy];
    end;
  finally
    Dest.Stop;
  end;
end;




function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
begin
  Result := TBitmap.Create;
  try
    if Module <> 0 then
    begin
      if LongRec(ResID).Hi = 0 then
        Result.LoadFromResourceID(Module, LongRec(ResID).Lo)
      else
        Result.LoadFromResourceName(Module, StrPas(ResID));
    end
    else
    begin 
        ResourceNotFound(ResID);
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function MakeBitmap(ResID: PChar): TBitmap;
begin
  Result := MakeModuleBitmap(HInstance, ResID);
end;

function MakeBitmapID(ResID: Word): TBitmap;
begin
  Result := MakeModuleBitmap(HInstance, MakeIntResource(ResID));
end;

procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap;
  Cols, Rows, Index: Integer);
var
  CellWidth, CellHeight: Integer;
begin
  if (Source <> nil) and (Dest <> nil) then
  begin
    if Cols <= 0 then
      Cols := 1;
    if Rows <= 0 then
      Rows := 1;
    if Index < 0 then
      Index := 0;
    CellWidth := Source.Width div Cols;
    CellHeight := Source.Height div Rows;
    with Dest do
    begin
      Width := CellWidth;
      Height := CellHeight;
    end;
    if Source is TBitmap then
    begin
      Dest.Canvas.CopyRect(Bounds(0, 0, CellWidth, CellHeight),
        TBitmap(Source).Canvas, Bounds((Index mod Cols) * CellWidth,
        (Index div Cols) * CellHeight, CellWidth, CellHeight));
      Dest.TransparentColor := TBitmap(Source).TransparentColor;
    end
    else
    begin
      Dest.Canvas.Brush.Color := clSilver;
      Dest.Canvas.FillRect(Bounds(0, 0, CellWidth, CellHeight));
      Dest.Canvas.Draw(-(Index mod Cols) * CellWidth,
        -(Index div Cols) * CellHeight, Source);
    end;
    Dest.Transparent := Source.Transparent;
  end;
end;

{ Transparent bitmap }




procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, Srch: Integer; Dummy: Integer;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: QPixmapH;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: QPixmapH;
  MemDC, BackDC, ObjectDC, SaveDC: QPainterH;
begin
  { Create some DCs to hold temporary data }
  BackDC := CreateCompatibleDC(DstDC);
  ObjectDC := CreateCompatibleDC(DstDC);
  MemDC := CreateCompatibleDC(DstDC);
  SaveDC := CreateCompatibleDC(DstDC);
  { Create a bitmap for each DC }
  bmAndObject := CreateBitmap(SrcW, Srch, 1, 1, nil);
  bmAndBack := CreateBitmap(SrcW, Srch, 1, 1, nil);
  bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave := CreateCompatibleBitmap(DstDC, SrcW, Srch);
  { Each DC must select a bitmap object to store pixel data }
  bmBackOld := SelectObject(BackDC, bmAndBack);
  bmObjectOld := SelectObject(ObjectDC, bmAndObject);
  bmMemOld := SelectObject(MemDC, bmAndMem);
  bmSaveOld := SelectObject(SaveDC, bmSave);
  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, Srch, SrcDC, SrcX, SrcY, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, Srch, SaveDC, 0, 0, SRCCOPY);
  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, Srch, ObjectDC, 0, 0, NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, Srch, SRCAND);
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, Srch, BackDC, 0, 0, SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, Srch, SRCPAINT);
  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SRCCOPY);
  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
end;


procedure DrawTransparentBitmapRect(DC: HDC; Bitmap: HBITMAP; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; TransparentColor: TColorRef);
var
  hdcTemp: HDC;
begin
  hdcTemp := CreateCompatibleDC(DC);
  try
    SelectObject(hdcTemp, Bitmap);
    with SrcRect do
      StretchBltTransparent(DC, DstX, DstY, DstW, DstH, hdcTemp,
        Left, Top, Right - Left, Bottom - Top, 0, TransparentColor);
  finally
    DeleteDC(hdcTemp);
  end;
end;

procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBITMAP;
  DstX, DstY: Integer; TransparentColor: TColorRef);
var
  BM: tagBITMAP;
begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  DrawTransparentBitmapRect(DC, Bitmap, DstX, DstY, BM.bmWidth, BM.bmHeight,
    Rect(0, 0, BM.bmWidth, BM.bmHeight), TransparentColor);
end;

procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
  TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
  SrcW, Srch: Integer);
var
  CanvasChanging: TNotifyEvent;
begin
  if DstW <= 0 then
    DstW := Bitmap.Width;
  if DstH <= 0 then
    DstH := Bitmap.Height;
  if (SrcW <= 0) or (Srch <= 0) then
  begin
    SrcX := 0;
    SrcY := 0;
    SrcW := Bitmap.Width;
    Srch := Bitmap.Height;
  end; 
  Dest.Start; 
  if not Bitmap.Monochrome then
    SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
  CanvasChanging := Bitmap.Canvas.OnChanging;
  Bitmap.Canvas.Lock;
  try
    Bitmap.Canvas.OnChanging := nil; 
    Bitmap.Canvas.Start; 
    if TransparentColor = clNone then
    begin
      StretchBlt(Dest.Handle, DstX, DstY, DstW, DstH, Bitmap.Canvas.Handle,
        SrcX, SrcY, SrcW, Srch, Cardinal(Dest.CopyMode));
    end
    else
    begin
      if TransparentColor = clDefault then
        TransparentColor := Bitmap.Canvas.Pixels[0, Bitmap.Height - 1];
      if Bitmap.Monochrome then
        TransparentColor := clWhite
      else
        TransparentColor := ColorToRGB(TransparentColor);  
      StretchBltTransparent(Dest.Handle, DstX, DstY, DstW, DstH,
        Bitmap.Canvas.Handle, SrcX, SrcY, SrcW, Srch,
        0, TransparentColor); 
    end; 
    Bitmap.Canvas.Stop; 
  finally
    Bitmap.Canvas.OnChanging := CanvasChanging;
    Bitmap.Canvas.Unlock; 
    Dest.Stop; 
  end;
end;

procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; Bitmap: TBitmap;
  TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
      DstX, DstY, DstW, DstH, Left, Top, Right - Left, Bottom - Top);
end;

procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
      DstX, DstY, Right - Left, Bottom - Top, Left, Top, Right - Left,
      Bottom - Top);
end;

procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor, DstX, DstY,
    Bitmap.Width, Bitmap.Height, 0, 0, Bitmap.Width, Bitmap.Height);
end;

{ CreateDisabledBitmap. Creating TBitmap object with disable button glyph
  image. You must destroy it outside by calling TBitmap.Free method. }

function CreateDisabledBitmap_NewStyle(FOriginal: TBitmap; BackColor: TColor):
  TBitmap;
var
  MonoBmp: TBitmap;
  R: TRect;
  DestDC, SrcDC: HDC;
begin
  R := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    Result.Canvas.Brush.Color := BackColor;
    Result.Canvas.FillRect(R);

    MonoBmp := TBitmap.Create;
    try
      MonoBmp.Width := FOriginal.Width;
      MonoBmp.Height := FOriginal.Height;
      MonoBmp.Canvas.Brush.Color := clWhite;
      MonoBmp.Canvas.FillRect(R);
      DrawBitmapTransparent(MonoBmp.Canvas, 0, 0, FOriginal, BackColor);
      MonoBmp.Monochrome := True;

      SrcDC := MonoBmp.Canvas.Handle;
      { Convert Black to clBtnHighlight }
      Result.Canvas.Brush.Color := clBtnHighlight;
      DestDC := Result.Canvas.Handle;
      SetTextColor(DestDC, clWhite);
      SetBkColor(DestDC, clBlack);
      BitBlt(DestDC, 1, 1, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0,
        ROP_DSPDxax);
      { Convert Black to clBtnShadow }
      Result.Canvas.Brush.Color := clBtnShadow;
      DestDC := Result.Canvas.Handle;
      SetTextColor(DestDC, clWhite);
      SetBkColor(DestDC, clBlack);
      BitBlt(DestDC, 0, 0, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0,
        ROP_DSPDxax);
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighLightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
var
  MonoBmp: TBitmap;
  IRect: TRect;
begin
  IRect := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    MonoBmp := TBitmap.Create;
    try
      with MonoBmp do
      begin
        Width := FOriginal.Width;
        Height := FOriginal.Height;
        Canvas.CopyRect(IRect, FOriginal.Canvas, IRect); 
        Canvas.Brush.Color := OutlineColor;
        if Monochrome then
        begin
          Canvas.Font.Color := clWhite;
          Monochrome := False;
          Canvas.Brush.Color := clWhite;
        end;
        Monochrome := True;
      end;
      with Result.Canvas do
      begin
        Brush.Color := BackColor;
        FillRect(IRect); 
        MonoBmp.Canvas.Start;
        Start;
        try 
          if DrawHighlight then
          begin
            Brush.Color := HighLightColor;
            SetTextColor(Handle, clBlack);
            SetBkColor(Handle, clWhite);
            BitBlt(Handle, 1, 1, RectWidth(IRect), RectHeight(IRect),
              MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
          end;
          Brush.Color := ShadowColor;
          SetTextColor(Handle, clBlack);
          SetBkColor(Handle, clWhite);
          BitBlt(Handle, 0, 0, RectWidth(IRect), RectHeight(IRect),
            MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax); 
        finally
          Stop;
          MonoBmp.Canvas.Stop;
        end; 
      end;
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor):
  TBitmap;
begin
  Result := CreateDisabledBitmapEx(FOriginal, OutlineColor,
    clBtnFace, clBtnHighlight, clBtnShadow, True);
end;

{ ChangeBitmapColor. This function create new TBitmap object.
  You must destroy it outside by calling TBitmap.Free method. }

function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
var
  R: TRect;
begin
  Result := TBitmap.Create;
  try
    with Result do
    begin
      Height := Bitmap.Height;
      Width := Bitmap.Width;
      R := Bounds(0, 0, Width, Height);
      Canvas.Brush.Color := NewColor;
      Canvas.FillRect(R);  
      DrawBitmapTransparent(Canvas, 0, 0, Bitmap, Color); 
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighLightColor, GrayColor: TColor;
  DrawHighlight: Boolean);
var
  Bmp: TBitmap;
  SaveColor: TColor;
begin
  SaveColor := Canvas.Brush.Color;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Images.Width;
    Bmp.Height := Images.Height;
    with Bmp.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Images.Width, Images.Height));  
      Images.Draw(Bmp.Canvas, 0, 0, Index, itMask); 
    end;
    Bmp.Monochrome := True;
    if DrawHighlight then
    begin
      Canvas.Brush.Color := HighLightColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X + 1, Y + 1, Images.Width,
        Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    Canvas.Brush.Color := GrayColor;
    SetTextColor(Canvas.Handle, clWhite);
    SetBkColor(Canvas.Handle, clBlack);
    BitBlt(Canvas.Handle, X, Y, Images.Width,
      Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

{ Brush Pattern }

function CreateTwoColorsBrushPattern(Color1, Color2: TColor): TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := 8;
  Result.Height := 8;
  with Result.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color1;
    FillRect(Rect(0, 0, Result.Width, Result.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
          Pixels[X, Y] := Color2; { on even/odd rows }
  end;
end;

{ Icons }

function MakeIcon(ResID: PChar): TIcon;
begin
  Result := MakeModuleIcon(HInstance, ResID);
end;

function MakeIconID(ResID: Word): TIcon;
begin
  Result := MakeModuleIcon(HInstance, MakeIntResource(ResID));
end;

function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
begin
  Result := TIcon.Create;  
  try
    Result.LoadFromResourceName(HInstance, ResID);
  except
    Result.Free;
    Result := nil;
  end; 
end;

{ Create TBitmap object from TIcon }

function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
var
  IWidth, IHeight: Integer;
begin
  IWidth := Icon.Width;
  IHeight := Icon.Height;
  Result := TBitmap.Create;
  try
    Result.Width := IWidth;
    Result.Height := IHeight;
    with Result.Canvas do
    begin
      Brush.Color := BackColor;
      FillRect(Rect(0, 0, IWidth, IHeight));
      Draw(0, 0, Icon);
    end;
    Result.TransparentColor := BackColor;
    Result.Transparent := True;
  except
    Result.Free;
    raise;
  end;
end;

function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;

var
  Bmp: TBitmap;

begin
  with TImageList.CreateSize(Bitmap.Width, Bitmap.Height) do
  try
    if TransparentColor = clDefault then
      TransparentColor := Bitmap.TransparentColor; 
    AddMasked(Bitmap, TransparentColor);
    Result := TIcon.Create;
    try  
      Bmp := TBitmap.Create;
      try
        GetBitmap(0, Bmp);
        Result.Assign(Bmp);
      finally
        Bmp.Free;
      end; 
    except
      Result.Free;
      raise;
    end;
  finally
    Free;
  end;
end;

type
  TCustomControlAccessProtected = class(TCustomControl);




procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
var
  Canvas: TJvDeskTopCanvas;
  I: integer;
begin
  Canvas := TJvDeskTopCanvas.Create;
  with Canvas do
    try
      StartPaint;
      try
        for I := 1 to Width do
        begin
          DrawFocusRect(ScreenRect);
          InflateRect(ScreenRect, -1, -1);
        end;
      finally
        StopPaint;
      end;
    finally
      Free;
    end;
end;

procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
var
  Canvas: TJvDeskTopCanvas;
  R: TRect;
begin
  Canvas := TJvDeskTopCanvas.Create;
  with Canvas do
    try
      StartPaint;
      try
        R := Rect(RectOrg.X, RectOrg.Y, RectEnd.X, RectEnd.Y);
        QWindows.InvertRect(Handle, R);
      finally
        StopPaint;
      end;
    finally
      Free;
    end;
end;



function PointInPolyRgn(const P: TPoint; const Points: array of TPoint):
  Boolean;
type
  PPoints = ^TPoints;
  TPoints = array [0..0] of TPoint;
var
  Rgn: HRGN;
begin
  Rgn := CreatePolygonRgn(PPoints(@Points)^, High(Points) + 1, WINDING);
  try
    Result := PtInRegion(Rgn, P.X, P.Y);
  finally
    DeleteObject(Rgn);
  end;
end;

function PaletteColor(Color: TColor): Longint;
begin
  Result := ColorToRGB(Color) or PaletteMask;
end;



procedure Delay(MSecs: Longint);
var
  FirstTickCount, Now: Longint;
begin
  FirstTickCount := GetTickCount;
  repeat
    Application.ProcessMessages;
    { allowing access to other controls, etc. }
    Now := GetTickCount;
  until (Now - FirstTickCount >= MSecs) or (Now < FirstTickCount);
end;

procedure CenterControl(Control: TControl);
var
  X, Y: Integer;
begin
  X := Control.Left;
  Y := Control.Top;
  if Control is TForm then
  begin
    with Control do
    begin
      if (TForm(Control).FormStyle = fsMDIChild) and
        (Application.MainForm <> nil) then
      begin
        X := (Application.MainForm.ClientWidth - Width) div 2;
        Y := (Application.MainForm.ClientHeight - Height) div 2;
      end
      else
      begin
        X := (Screen.Width - Width) div 2;
        Y := (Screen.Height - Height) div 2;
      end;
    end;
  end
  else
  if Control.Parent <> nil then
  begin
    with Control do
    begin
      Parent.HandleNeeded;
      X := (Parent.ClientWidth - Width) div 2;
      Y := (Parent.ClientHeight - Height) div 2;
    end;
  end;
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  with Control do
    SetBounds(X, Y, Width, Height);
end;

procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
var
  R: TRect;
  AutoScroll: Boolean;
begin
  AutoScroll := AForm.AutoScroll;
  AForm.Hide;
  TCustomControlAccessProtected(AForm).DestroyHandle;
  with AForm do
  begin  
    BorderStyle := fbsNone; 
    BorderIcons := [];
    Parent := AControl;
  end;
  AControl.DisableAlign;
  try
    if Align <> alNone then
      AForm.Align := Align
    else
    begin
      R := AControl.ClientRect;
      AForm.SetBounds(R.Left + AForm.Left, R.Top + AForm.Top, AForm.Width,
        AForm.Height);
    end;
    AForm.AutoScroll := AutoScroll;
    AForm.Visible := Show;
  finally
    AControl.EnableAlign;
  end;
end;





function ScreenWorkArea: TRect;
begin
  {$IFDEF MSWINDOWS}
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
  {$ENDIF MSWINDOWS}
    with Screen do
      Result := Bounds(0, 0, Width, Height);
end;

{ Standard Windows MessageBox function }

function MsgBox(const Caption, Text: string; Flags: Integer): Integer;


var
  Mbs: TMessageButtons;
  Def: TMessageButton;
  Style: TMessageStyle;
  DefFlags: Integer;
begin
  Mbs := [];
  DefFlags := Flags and $00000F00;
  case Flags and $0000000F of
    MB_OK:
      begin
        Mbs := [smbOk];
        Def := smbOk;
      end;
    MB_OKCANCEL:
      begin
        Mbs := [smbOk, smbCancel];
        Def := smbOk;
        if DefFlags <> MB_DEFBUTTON1 then
          Def := smbCancel;
      end;
    MB_ABORTRETRYIGNORE:
      begin
        Mbs := [smbAbort, smbRetry, smbIgnore];
        Def := smbAbort;
        case DefFlags of
          MB_DEFBUTTON2:
            Def := smbRetry;
          MB_DEFBUTTON3:
            Def := smbIgnore;
        end;
      end;
    MB_YESNOCANCEL:
      begin
        Mbs := [smbYes, smbNo, smbCancel];
        Def := smbYes;
        case DefFlags of
          MB_DEFBUTTON2:
            Def := smbNo;
          MB_DEFBUTTON3:
            Def := smbCancel;
        end;
      end;
    MB_YESNO:
      begin
        Mbs := [smbYes, smbNo];
        Def := smbYes;
        if DefFlags <> MB_DEFBUTTON1 then
          Def := smbNo;
      end;
    MB_RETRYCANCEL:
      begin
        Mbs := [smbRetry, smbCancel];
        Def := smbRetry;
        if DefFlags <> MB_DEFBUTTON1 then
          Def := smbCancel;
      end;
  else
    Mbs := [smbOk];
    Def := smbOk;
  end;

  case Flags and $000000F0 of
    MB_ICONWARNING:
      Style := smsWarning;
    MB_ICONERROR:
      Style := smsCritical;
  else
    Style := smsInformation;
  end;

  case Application.MessageBox(Text, Caption, Mbs, Style, Def) of
    smbOk:
      Result := IDOK;
    smbCancel:
      Result := IDCANCEL;
    smbAbort:
      Result := IDABORT;
    smbRetry:
      Result := IDRETRY;
    smbIgnore:
      Result := IDIGNORE;
    smbYes:
      Result := IDYES;
    smbNo:
      Result := IDNO;
  else
    Result := IDOK;
  end;
end;



{ Gradient fill procedure - displays a gradient beginning with a chosen        }
{ color and ending with another chosen color. Based on TGradientFill           }
{ component source code written by Curtis White, cwhite att teleport dott com. }

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  StartRGB: array [0..2] of Byte; { Start RGB values }
  RGBDelta: array [0..2] of Integer;
  { Difference between start and end RGB values }
  ColorBand: TRect; { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBRUSH;
  TmpColor: TColor;
begin  
  Canvas.Start; 
  try
    if (StartColor = clNone) and (EndColor = clNone) then
      Exit;
    if not (IsRectEmpty(ARect) and (GetMapMode(Canvas.Handle) = MM_TEXT)) then
    begin  
      StartColor := ColorFromColormap(StartColor);
      EndColor := ColorFromColormap(EndColor); 
      if Direction in [fdBottomToTop, fdRightToLeft] then
      begin
        // just swap the colors
        TmpColor := StartColor;
        StartColor := EndColor;
        EndColor := TmpColor;
        if Direction = fdBottomToTop then
          Direction := fdTopToBottom
        else
          Direction := fdLeftToRight;
      end;
      if (Colors < 2) or (StartColor = EndColor) then
      begin
        Brush := CreateSolidBrush(ColorToRGB(StartColor));
        FillRect(Canvas.Handle, ARect, Brush);
        DeleteObject(Brush);
        Exit;
      end;
          { Set the Red, Green and Blue colors }
      StartRGB[0] := GetRValue(StartColor);
      StartRGB[1] := GetGValue(StartColor);
      StartRGB[2] := GetBValue(StartColor);
          { Calculate the difference between begin and end RGB values }
      RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
      { Calculate the color band's coordinates }
      ColorBand := ARect;
      if Direction = fdTopToBottom then
      begin
        Colors := Max(2, Min(Colors, RectHeight(ARect)));
        Delta := RectHeight(ARect) div Colors;
      end
      else
      begin
        Colors := Max(2, Min(Colors, RectWidth(ARect)));
        Delta := RectWidth(ARect) div Colors;
      end;
      with Canvas.Pen do
      begin { Set the pen style and mode }
        Style := psSolid;
        Mode := pmCopy;
      end;
      { Perform the fill }
      if Delta > 0 then
      begin
        for I := 0 to Colors - 1 do
        begin
          if Direction = fdTopToBottom then
          { Calculate the color band's top and bottom coordinates }
          begin
            ColorBand.Top := ARect.Top + I * Delta;
            ColorBand.Bottom := ColorBand.Top + Delta;
          end
          { Calculate the color band's left and right coordinates }
          else
          begin
            ColorBand.Left := ARect.Left + I * Delta;
            ColorBand.Right := ColorBand.Left + Delta;
          end;
        { Calculate the color band's color }
          Brush := CreateSolidBrush(RGB(
            StartRGB[0] + MulDiv(I, RGBDelta[0], Colors - 1),
            StartRGB[1] + MulDiv(I, RGBDelta[1], Colors - 1),
            StartRGB[2] + MulDiv(I, RGBDelta[2], Colors - 1)));
          FillRect(Canvas.Handle, ColorBand, Brush);
          DeleteObject(Brush);
        end;
      end;
      if Direction = fdTopToBottom then
        Delta := RectHeight(ARect) mod Colors
      else
        Delta := RectWidth(ARect) mod Colors;
      if Delta > 0 then
      begin
        if Direction = fdTopToBottom then
        { Calculate the color band's top and bottom coordinates }
        begin
          ColorBand.Top := ARect.Bottom - Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end
        else
        { Calculate the color band's left and right coordinates }
        begin
          ColorBand.Left := ARect.Right - Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
        Brush := CreateSolidBrush(EndColor);
        FillRect(Canvas.Handle, ColorBand, Brush);
        DeleteObject(Brush);
      end;
    end; //  if Not (IsRectEmpty(ARect) and ...
  finally  
    Canvas.Stop; 
  end;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array [0..51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a')); 
  Canvas.Start; 
  GetTextExtentPoint32(Canvas.Handle, Buffer, 52, TSize(Result)); 
  Canvas.Stop; 
  Result.X := Result.X div 52;
end;

{ Cursor routines }

{$IFDEF MSWINDOWS}
function LoadAniCursor(Instance: THandle; ResID: PChar): HCURSOR;
{ Unfortunately I don't know how we can load animated cursor from
  executable resource directly. So I write this routine using temporary
  file and LoadCursorFromFile function. }
var
  S: TFileStream;
  Path, FileName: array[0..MAX_PATH] of Char;
  RSrc: HRSRC;
  Res: THandle;
  Data: Pointer;
begin
  Integer(Result) := 0;
  RSrc := FindResource(Instance, ResID, RT_ANICURSOR);
  if RSrc <> 0 then
  begin
    OSCheck(GetTempPath(MAX_PATH, Path) <> 0);
    OSCheck(GetTempFileName(Path, 'ANI', 0, FileName) <> 0);
    try
      Res := LoadResource(Instance, RSrc);
      try
        Data := LockResource(Res);
        if Data <> nil then
        try
          S := TFileStream.Create(StrPas(FileName), fmCreate);
          try
            S.WriteBuffer(Data^, SizeOfResource(Instance, RSrc));
          finally
            S.Free;
          end;
          Result := LoadCursorFromFile(FileName);
        finally
          UnlockResource(Res);
        end;
      finally
        FreeResource(Res);
      end;
    finally
      Windows.DeleteFile(FileName);
    end;
  end;
end;
{$ENDIF MSWINDOWS}

function GetNextFreeCursorIndex(StartHint: Integer; PreDefined: Boolean):
  Integer;
begin
  Result := StartHint;
  if PreDefined then
  begin
    if Result >= crSizeAll then Result := crSizeAll - 1;
  end
  else
  if Result <= crDefault then
    Result := crDefault + 1;
  while (Screen.Cursors[Result] <> Screen.Cursors[crDefault]) do
  begin
    if PreDefined then
      Dec(Result)
    else
      Inc(Result);
    if (Result < Low(TCursor)) or (Result > High(TCursor)) then
      raise EOutOfResources.CreateRes(@SOutOfResources);
  end;
end;

function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
var
  Handle: HCURSOR;
begin
  Handle := LoadCursor(Instance, ResID); 
  if Integer(Handle) = 0 then
    ResourceNotFound(ResID);
  try
    Result := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[Result] := Handle;
  except 
    raise;
  end;
end;

// (rom) changed to var
var
  WaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

const
  FWaitCursor: TCursor = crHourGlass;

procedure StartWait;
begin
  if WaitCount = 0 then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := FWaitCursor;
  end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then
  begin
    Dec(WaitCount);
    if WaitCount = 0 then
      Screen.Cursor := SaveCursor;
  end;
end;

function WaitCursor: IInterface;
begin
  Result := ScreenCursor(crHourGlass);
end;

function ScreenCursor(ACursor: TCursor): IInterface;
begin
  Result := TWaitCursor.Create(ACursor);
end;

{$IFDEF MSWINDOWS}

var
  OLEDragCursorsLoaded: Boolean = False;

function LoadOLEDragCursors: Boolean;
const
  cOle32DLL: PChar = 'ole32.dll';
var
  Handle: Cardinal;
begin
  if OLEDragCursorsLoaded then
  begin
    Result := True;
    Exit;
  end;
  OLEDragCursorsLoaded := True;

  Result := False;
  if Screen <> nil then
  begin
    Handle := GetModuleHandle(cOle32DLL);
    if Handle = 0 then
      Handle := LoadLibraryEx(cOle32DLL, 0, LOAD_LIBRARY_AS_DATAFILE);
    if Handle <> 0 then // (p3) don't free the lib handle!
    try
      Screen.Cursors[crNoDrop] := LoadCursor(Handle, PChar(1));
      Screen.Cursors[crDrag] := LoadCursor(Handle, PChar(2));
      Screen.Cursors[crMultiDrag] := LoadCursor(Handle, PChar(3));
      Screen.Cursors[crMultiDragLink] := LoadCursor(Handle, PChar(4));
      Screen.Cursors[crDragAlt] := LoadCursor(Handle, PChar(5));
      Screen.Cursors[crMultiDragAlt] := LoadCursor(Handle, PChar(6));
      Screen.Cursors[crMultiDragLinkAlt] := LoadCursor(Handle, PChar(7));
      Result := True;
    except
    end;
  end;
end;

{$ENDIF MSWINDOWS}

procedure SetDefaultJVCLCursors;
begin
  if Screen <> nil then
  begin
    // dynamically assign the first available cursor id to our cursor defines
    crMultiDragLink := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crMultiDragLink] := Screen.Cursors[crMultiDrag];
    crDragAlt := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crDragAlt] := Screen.Cursors[crDrag];
    crMultiDragAlt := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crMultiDragAlt] := Screen.Cursors[crMultiDrag];
    crMultiDragLinkAlt := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crMultiDragLinkAlt] := Screen.Cursors[crMultiDrag];
    { begin RxLib }
    crHand := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crHand] := LoadCursor(HInstance, 'JV_HANDCUR');
    crDragHand := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'JV_DRAGCUR');
    { end RxLib }
  end;
end;

{ Grid drawing }

// (rom) changed to var
var
  DrawBitmap: TBitmap = nil;

procedure UsesBitmap;
begin
  if DrawBitmap = nil then
  begin
    DrawBitmap := TBitmap.Create;
    AddFinalizeObjectNil(sUnitName, TObject(DrawBitmap));
  end;
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean;
  ARightToLeft: Boolean = False);
const
  AlignFlags: array [TAlignment] of Integer =
   (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
    DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
    DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  WrapFlags: array [Boolean] of Integer = (0, DT_WORDBREAK);


begin
  ACanvas.TextRect(ARect, ARect.Left + DX, ARect.Top + DY,
    Text, AlignFlags[Alignment] or WrapFlags[WordWrap]);
end;


procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean); overload;
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify:
      H := MinOffs;
    vaCenterJustify:
      with TCustomControlAccessProtected(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top - Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    begin
      with TCustomControlAccessProtected(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top - Canvas.TextHeight('W'));
    end;
  end;
  WriteText(TCustomControlAccessProtected(Control).Canvas, ARect, MinOffs,
    H, S, Align, WordWrap, ARightToLeft);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean); overload;
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter, ARightToLeft);
end;

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean); overload;
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify:
      H := MinOffs;
    vaCenterJustify:
      with TCustomControlAccessProtected(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top - Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    begin
      with TCustomControlAccessProtected(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top - Canvas.TextHeight('W'));
    end;
  end;
  WriteText(TCustomControlAccessProtected(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment); overload;
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign, Align = taCenter);
end;

procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);
begin
  Rect.Top := (Rect.Bottom + Rect.Top - Bmp.Height) div 2;
  Rect.Left := (Rect.Right + Rect.Left - Bmp.Width) div 2;
  TCustomControlAccessProtected(Control).Canvas.Draw(Rect.Left, Rect.Top, Bmp);
end;

//=== { TJvDesktopCanvas } ===================================================


procedure TJvDesktopCanvas.CreateHandle;
begin
  inherited CreateHandle;
  QtHandle := GetDesktopWindow;
end;




procedure TJvDesktopCanvas.SetOrigin(X, Y: Integer);
var
  FOrigin: TPoint;
begin 
  StartPaint; 
  SetWindowOrgEx(Handle, -X, -Y, @FOrigin); 
  StopPaint; 
end;

// (rom) moved to file end to minimize W- switch impact at end of function

{ end JvVCLUtils }
{ begin JvUtils }

function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass;
  const Tag: Integer): TComponent;
var
  I: Integer;
begin
  for I := 0 to WinControl.ControlCount - 1 do
  begin
    Result := WinControl.Controls[I];
    if (Result is ComponentClass) and (Result.Tag = Tag) then
      Exit;
  end;
  Result := nil;
end;

function ControlAtPos2(Parent: TWinControl; X, Y: Integer): TControl;
var
  I: Integer;
  P: TPoint;
begin
  P := Point(X, Y);
  for I := Parent.ControlCount - 1 downto 0 do
  begin
    Result := Parent.Controls[I];
    with Result do
      if PtInRect(BoundsRect, P) then
        Exit;
  end;
  Result := nil;
end;

function RBTag(Parent: TWinControl): Integer;
var
  RB: TRadioButton;
  I: Integer;
begin
  RB := nil;
  with Parent do
    for I := 0 to ControlCount - 1 do
      if (Controls[I] is TRadioButton) and
        (Controls[I] as TRadioButton).Checked then
      begin
        RB := Controls[I] as TRadioButton;
        Break;
      end;
  if RB <> nil then
    Result := RB.Tag
  else
    Result := 0;
end;

function FindFormByClass(FormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Application.ComponentCount - 1 do
    if Application.Components[I].ClassName = FormClass.ClassName then
    begin
      Result := Application.Components[I] as TForm;
      Break;
    end;
end;

function FindFormByClassName(const FormClassName: string): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Application.ComponentCount - 1 do
    if Application.Components[I].ClassName = FormClassName then
    begin
      Result := Application.Components[I] as TForm;
      Break;
    end;
end;

function AppMinimized: Boolean;
begin
  Result := IsIconic(GetAppHandle);
end;

{$IFDEF MSWINDOWS}

{ Check if this is the active Windows task }
{ Copied from implementation of FORMS.PAS  }
type
  PCheckTaskInfo = ^TCheckTaskInfo;
  TCheckTaskInfo = record
    FocusWnd: Windows.HWND;
    Found: Boolean;
  end;

function CheckTaskWindow(Window: Windows.HWND; Data: Longint): WordBool; stdcall;
begin
  Result := True;
  if PCheckTaskInfo(Data)^.FocusWnd = Window then
  begin
    Result := False;
    PCheckTaskInfo(Data)^.Found := True;
  end;
end;

function IsForegroundTask: Boolean;
var
  Info: TCheckTaskInfo;
begin
  Info.FocusWnd := Windows.GetActiveWindow;
  Info.Found := False;
  Windows.EnumThreadWindows(GetCurrentThreadID, @CheckTaskWindow, Longint(@Info));
  Result := Info.Found;
end;

{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
function IsForegroundTask: Boolean;
begin
  Result := Application.Active;
end;
{$ENDIF LINUX}



procedure CenterHor(Parent: TControl; MinLeft: Integer; Controls: array of TControl);
var
  I: Integer;
begin
  for I := Low(Controls) to High(Controls) do
    Controls[I].Left := Max(MinLeft, (Parent.Width - Controls[I].Width) div 2);
end;

procedure EnableControls(Control: TWinControl; const Enable: Boolean);
var
  I: Integer;
begin
  for I := 0 to Control.ControlCount - 1 do
    Control.Controls[I].Enabled := Enable;
end;

procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: Integer; const Enable: Boolean);
var
  I: Integer;
begin
  for I := 0 to MenuItem.Count - 1 do
    if MenuItem[I].Tag <> Tag then
      MenuItem[I].Enabled := Enable;
end;

procedure ExpandWidth(Parent: TControl; MinWidth: Integer; Controls: array of TControl);
var
  I: Integer;
begin
  for I := Low(Controls) to High(Controls) do
    Controls[I].Width := Max(MinWidth, Parent.ClientWidth - 2 * Controls[I].Left);
end;

function PanelBorder(Panel: TCustomPanel): Integer;
begin
  Result := TPanel(Panel).BorderWidth;
  if TPanel(Panel).BevelOuter <> bvNone then
    Inc(Result, TPanel(Panel).BevelWidth);
  if TPanel(Panel).BevelInner <> bvNone then
    Inc(Result, TPanel(Panel).BevelWidth);
end;

function Pixels(Control: TControl; APixels: Integer): Integer;
var
  Form: TForm;
begin
  Result := APixels;
  if Control is TForm then
    Form := TForm(Control)
  else
    Form := TForm(GetParentForm(Control));
  if Form.Scaled then
    Result := Result * Form.PixelsPerInch div 96;
end;

procedure ShowMenu(Form: TForm; MenuAni: TMenuAnimation);
var
  I: Integer;
  H: Integer;
  W: Integer;
begin
  case MenuAni of
    maNone:
      Form.Show;
    maRandom:
      ;
    maUnfold:
      begin
        H := Form.Height;
        Form.Height := 0;
        Form.Show;
        for I := 0 to H div 10 do
          if Form.Height < H then
            Form.Height := Form.Height + 10;
      end;
    maSlide:
      begin
        H := Form.Height;
        W := Form.Width;
        Form.Height := 0;
        Form.Width := 0;
        Form.Show;
        for I := 0 to Max(H div 5, W div 5) do
        begin
          if Form.Height < H then
            Form.Height := Form.Height + 5;
          if Form.Width < W then
            Form.Width := Form.Width + 5;
        end;
        //      CS_SAVEBITS
      end;
  end;
end;

{$IFDEF MSWINDOWS}

function TargetFileName(const FileName: TFileName): TFileName;
begin
  Result := FileName;
  if SameFileName(ExtractFileExt(FileName), '.lnk') then
    if ResolveLink(GetAppHandle, FileName, Result) <> 0 then
      raise EJVCLException.CreateResFmt(@RsECantGetShortCut, [FileName]);
end;

function ResolveLink(const HWND: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
var
  psl: IShellLink;
  WLinkFile: array [0..MAX_PATH] of WideChar;
  wfd: TWin32FindData;
  ppf: IPersistFile;
  wnd: Windows.HWND;
begin  
  wnd := QWidget_winId(HWND); 
  Pointer(psl) := nil;
  Pointer(ppf) := nil;
  Result := CoInitialize(nil);
  if Succeeded(Result) then
  begin
    // Get a Pointer to the IShellLink interface.
    Result := CoCreateInstance(CLSID_ShellLink, nil,
      CLSCTX_INPROC_SERVER, IShellLink, psl);
    if Succeeded(Result) then
    begin

      // Get a Pointer to the IPersistFile interface.
      Result := psl.QueryInterface(IPersistFile, ppf);
      if Succeeded(Result) then
      begin
        StringToWideChar(LinkFile, WLinkFile, SizeOf(WLinkFile) - 1);
        // Load the shortcut.
        Result := ppf.Load(WLinkFile, STGM_READ);
        if Succeeded(Result) then
        begin
          // Resolve the link.
          Result := psl.Resolve(wnd, SLR_ANY_MATCH);
          if Succeeded(Result) then
          begin
            // Get the path to the link target.
            SetLength(FileName, MAX_PATH);
            Result := psl.GetPath(PChar(FileName), MAX_PATH, wfd,
              SLGP_UNCPRIORITY);
            if not Succeeded(Result) then
              Exit;
            SetLength(FileName, Length(PChar(FileName)));
          end;
        end;
        // Release the Pointer to the IPersistFile interface.
        ppf._Release;
      end;
      // Release the Pointer to the IShellLink interface.
      psl._Release;
    end;
    CoUninitialize;
  end;
  Pointer(psl) := nil;
  Pointer(ppf) := nil;
end;

{$ENDIF MSWINDOWS}

var
  ProcList: TList = nil;

type
  TJvProcItem = class(TObject)
  private
    FProcObj: TProcObj;
  public
    constructor Create(AProcObj: TProcObj);
  end;

constructor TJvProcItem.Create(AProcObj: TProcObj);
begin
  inherited Create;
  FProcObj := AProcObj;
end;

procedure TmrProc(hwnd: HWND; uMsg: Integer; idEvent: Integer; dwTime: Integer); stdcall;
var
  Pr: TProcObj;
begin
  if ProcList[idEvent] <> nil then
  begin
    Pr := TJvProcItem(ProcList[idEvent]).FProcObj;
    TJvProcItem(ProcList[idEvent]).Free;
  end
  else
    Pr := nil;
  ProcList.Delete(idEvent);
  KillTimer(hwnd, idEvent);
  if ProcList.Count <= 0 then
  begin
    ProcList.Free;
    ProcList := nil;
  end;
  if Assigned(Pr) then
    Pr;
end;

procedure ExecAfterPause(Proc: TProcObj; Pause: Integer);
var
  Num: Integer;
  I: Integer;
begin
  if ProcList = nil then
    ProcList := TList.Create;
  Num := -1;
  for I := 0 to ProcList.Count - 1 do
    if @TJvProcItem(ProcList[I]).FProcObj = @Proc then
    begin
      Num := I;
      Break;
    end;
  if Num <> -1 then
    KillTimer(GetAppHandle, Num)
  else
    Num := ProcList.Add(TJvProcItem.Create(Proc));
  SetTimer(GetAppHandle, Num, Pause, @TmrProc);
end;

{ end JvUtils }

{ begin JvApputils }

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then
  begin
    if Component is TCustomForm then
      Result := Component.ClassName
    else
    begin
      Result := Component.Name;
      if Component is TControl then
      begin
        F := GetParentForm(TControl(Component));
        if F <> nil then
          Result := F.ClassName + Result
        else
        begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else
      begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else
    Result := '';
end;

function GetDefaultIniName: string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result := OnGetDefaultIniName
  else
    {$IFDEF LINUX}
    Result := GetEnvironmentVariable('HOME') + PathDelim +
      '.' + ExtractFileName(Application.ExeName);
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
    {$ENDIF MSWINDOWS}
end;

function GetDefaultIniRegKey: string;
begin
  if RegUseAppTitle and (Application.Title <> '') then
    Result := Application.Title
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  if DefCompanyName <> '' then
    Result := DefCompanyName + '\' + Result;
  Result := 'Software\' + Result;
end;

function FindForm(FormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is FormClass then
    begin
      Result := Screen.Forms[I];
      Break;
    end;
  end;
end;

function InternalFindShowForm(FormClass: TFormClass;
  const Caption: string; Restore: Boolean): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is FormClass then
      if (Caption = '') or (Caption = Screen.Forms[I].Caption) then
      begin
        Result := Screen.Forms[I];
        Break;
      end;
  end;
  if Result = nil then
  begin
    Application.CreateForm(FormClass, Result);
    if Caption <> '' then
      Result.Caption := Caption;
  end;
  with Result do
  begin
    if Restore and (WindowState = wsMinimized) then
      WindowState := wsNormal;
    Show;
  end;
end;

function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
begin
  Result := InternalFindShowForm(FormClass, Caption, True);
end;

function ShowDialog(FormClass: TFormClass): Boolean;
var
  Dlg: TForm;
begin
  Application.CreateForm(FormClass, Dlg);
  try
    Result := Dlg.ShowModal in [mrOk, mrYes];
  finally
    Dlg.Free;
  end;
end;

function InstantiateForm(FormClass: TFormClass; var Reference): TForm;
begin
  if TForm(Reference) = nil then
    Application.CreateForm(FormClass, Reference);
  Result := TForm(Reference);
end;

// (rom) use StrStringToEscaped, StrEscapedToString from JclStrings.pas

function StrToIniStr(const Str: string): string;
var
  N: Integer;
begin
  Result := Str;
  repeat
    N := Pos(CrLf, Result);
    if N > 0 then
      Result := Copy(Result, 1, N - 1) + '\n' + Copy(Result, N + 2, Length(Result));
  until N = 0;
  repeat
    N := Pos(#10#13, Result);
    if N > 0 then
      Result := Copy(Result, 1, N - 1) + '\n' + Copy(Result, N + 2, Length(Result));
  until N = 0;
end;

function IniStrToStr(const Str: string): string;
var
  N: Integer;
begin
  Result := Str;
  repeat
    N := Pos('\n', Result);
    if N > 0 then
      Result := Copy(Result, 1, N - 1) + CrLf + Copy(Result, N + 2, Length(Result));
  until N = 0;
end;

{ The following strings should not be localized }
const
  siFlags = 'Flags';
  siShowCmd = 'ShowCmd';
  siMinMaxPos = 'MinMaxPos';
  siNormPos = 'NormPos';
  siPixels = 'PixelsPerInch';
  siMDIChild = 'MDI Children';
  siListCount = 'Count';
  siItem = 'Item%d';

(*
function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadString(Section, Ident, Default)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      Result := TCustomIniFile(IniFile).ReadString(Section, Ident, Default)
    else
      Result := Default;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
var
  S: string;
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
  {$ENDIF MSWINDOWS}
  begin
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
      ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TCustomIniFile then
      TCustomIniFile(IniFile).WriteString(Section, Ident, S);
  end;
end;

function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  Default: Longint): Longint;
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      Result := TCustomIniFile(IniFile).ReadInteger(Section, Ident, Default)
    else
      Result := Default;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      TCustomIniFile(IniFile).WriteInteger(Section, Ident, Value);
end;

function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      Result := TCustomIniFile(IniFile).ReadBool(Section, Ident, Default)
    else
      Result := Default;
end;

procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteBool(Section, Ident, Value)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      TCustomIniFile(IniFile).WriteBool(Section, Ident, Value);
end;

procedure IniEraseSection(IniFile: TObject; const Section: string);
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).EraseSection(Section)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      TCustomIniFile(IniFile).EraseSection(Section);
end;

procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
begin
  {$IFDEF MSWINDOWS}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).DeleteKey(Section, Ident)
  else
  {$ENDIF MSWINDOWS}
    if IniFile is TCustomIniFile then
      TCustomIniFile(IniFile).DeleteKey(Section, Ident);
end;

procedure IniReadSections(IniFile: TObject; Strings: TStrings);
begin
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).ReadSections(Strings)
  {$IFDEF MSWINDOWS}
  else
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).ReadSections(Strings);
  {$ENDIF MSWINDOWS}
end;
*)

{$HINTS OFF}
type
  {*******************************************************}
  { !! ATTENTION Nasty implementation                     }
  {*******************************************************}
  {                                                       }
  { This class definition was copied from FORMS.PAS.      }
  { It is needed to access some private fields of TForm.  }
  {                                                       }
  { Any changes in the underlying classes may cause       }
  { errors in this implementation!                        }
  {                                                       }
  {*******************************************************}

  TJvHackForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState; { !! }
  end;

  TComponentAccessProtected = class(TComponent);
{$HINTS ON}

function CrtResString: string;
begin
  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN),
    GetSystemMetrics(SM_CYSCREEN)]);
end;

function ReadPosStr(AppStorage: TJvCustomAppStorage; const Path: string): string;
begin
  if AppStorage.ValueStored(Path + CrtResString) then
    Result := AppStorage.ReadString(Path + CrtResString)
  else
    Result := AppStorage.ReadString(Path);
end;

procedure WritePosStr(AppStorage: TJvCustomAppStorage; const Path, Value: string);
begin
  AppStorage.WriteString(Path + CrtResString, Value);
  AppStorage.WriteString(Path, Value);
end;

procedure InternalSaveMDIChildren(MainForm: TForm;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  I: Integer;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.CreateRes(@SNoMDIForm);
  AppStorage.DeleteSubTree(AppStorage.ConcatPaths([StorePath, siMDIChild]));
  if MainForm.MDIChildCount > 0 then
  begin
    AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, siMDIChild,
      siListCount]), MainForm.MDIChildCount);
    for I := 0 to MainForm.MDIChildCount - 1 do
      AppStorage.WriteString(AppStorage.ConcatPaths([StorePath, siMDIChild,
        Format(siItem, [I])]), MainForm.MDIChildren[I].ClassName);
  end;
end;

procedure InternalRestoreMDIChildren(MainForm: TForm;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  I: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.CreateRes(@SNoMDIForm);
  StartWait;
  try
    Count := AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath, siMDIChild,
      siListCount]), 0);
    if Count > 0 then
    begin
      for I := 0 to Count - 1 do
      begin
        FormClass :=
          TFormClass(GetClass(AppStorage.ReadString(AppStorage.ConcatPaths([StorePath,
          siMDIChild, Format(siItem, [I])]), '')));
        if FormClass <> nil then
          InternalFindShowForm(FormClass, '', False);
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure SaveMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage);
begin
  InternalSaveMDIChildren(MainForm, AppStorage, '');
end;

procedure RestoreMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage);
begin
  InternalRestoreMDIChildren(MainForm, AppStorage, '');
end;

procedure InternalSaveFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage;
  const StorePath: string; Options: TPlacementOptions = [fpState, fpSize, fpLocation]);
var
  Placement: TWindowPlacement;
begin
  if Options = [fpActiveControl] then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if (Form = Application.MainForm) and AppMinimized then
      ShowCmd := SW_SHOWMINIMIZED; 
    if fpState in Options then
      AppStorage.WriteInteger(StorePath + '\' + siShowCmd, ShowCmd);
    if [fpSize, fpLocation] * Options <> [] then
    begin
      AppStorage.WriteInteger(StorePath + '\' + siFlags, Flags);
      AppStorage.WriteInteger(StorePath + '\' + siPixels, Screen.PixelsPerInch);
      WritePosStr(AppStorage, StorePath + '\' + siMinMaxPos, Format('%d,%d,%d,%d',
        [ptMinPosition.X, ptMinPosition.Y, ptMaxPosition.X, ptMaxPosition.Y]));
      WritePosStr(AppStorage, StorePath + '\' + siNormPos, Format('%d,%d,%d,%d',
        [rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Right,
         rcNormalPosition.Bottom]));
    end;
  end;
end;

procedure InternalRestoreFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage;
  const StorePath: string; Options: TPlacementOptions = [fpState, fpSize, fpLocation]);
const
  Delims = [',', ' '];
var
  PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound: Boolean;
begin
  if Options = [fpActiveControl] then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if not IsWindowVisible(Form.Handle) then
      ShowCmd := SW_HIDE;
    if [fpSize, fpLocation] * Options <> [] then
    begin
      DataFound := False;
      AppStorage.ReadInteger(StorePath + '\' + siFlags, Flags);
      PosStr := ReadPosStr(AppStorage, StorePath + '\' + siMinMaxPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        if fpLocation in Options then
        begin
          ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
          ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        end;
        if fpSize in Options then
        begin
          ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
          ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
        end;
      end;
      PosStr := ReadPosStr(AppStorage, StorePath + '\' + siNormPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        if fpLocation in Options then
        begin
          rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Left);
          rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Top);
        end;
        if fpSize in Options then
        begin
          rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Left + Width);
          rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Top + Height);
        end;
      end;
      DataFound := DataFound and (Screen.PixelsPerInch = AppStorage.ReadInteger(
        StorePath + '\' + siPixels, Screen.PixelsPerInch));
      if DataFound then
      begin  
          if not (BorderStyle in [fbsSizeable, fbsSizeToolWin]) then 
            rcNormalPosition := Rect(rcNormalPosition.Left,
              rcNormalPosition.Top, rcNormalPosition.Left + Width, rcNormalPosition.Top + Height);
        if rcNormalPosition.Right > rcNormalPosition.Left then
        begin
          if (Position in [poScreenCenter, poDesktopCenter]) and
            not (csDesigning in ComponentState) then
          begin
            TComponentAccessProtected(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              TComponentAccessProtected(Form).SetDesigning(False);
            end;
          end;
          SetWindowPlacement(Handle, @Placement);
        end;
      end;
    end;
    if fpState in Options then
    begin
      WinState := wsNormal;
      { default maximize MDI main form }
      if ((Application.MainForm = Form) or
        (Application.MainForm = nil)) and ((FormStyle = fsMDIForm) or
        ((FormStyle = fsNormal) and (Position = poDefault))) then
        WinState := wsMaximized;
      ShowCmd := AppStorage.ReadInteger(StorePath + '\' + siShowCmd, SW_HIDE);
      case ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE:
          WinState := wsMaximized;
      end; 
        WindowState := WinState;
    end;
    Update;
  end;
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, Format(siItem, [I])]),
      TDrawGrid(Grid).ColWidths[I]);
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[I] :=
      AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath,
        Format(siItem, [I])]), TDrawGrid(Grid).ColWidths[I]);
end;

procedure RestoreGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage);
begin
  InternalRestoreGridLayout(Grid, AppStorage, GetDefaultSection(Grid));
end;

procedure SaveGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage);
begin
  InternalSaveGridLayout(Grid, AppStorage, GetDefaultSection(Grid));
end;

procedure SaveFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage; Options: TPlacementOptions);
begin
  InternalSaveFormPlacement(Form, AppStorage, GetDefaultSection(Form), Options);
end;

procedure RestoreFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage; Options: TPlacementOptions);
begin
  InternalRestoreFormPlacement(Form, AppStorage, GetDefaultSection(Form), Options);
end;


{ end JvAppUtils }
{ begin JvGraph }
// (rom) moved here to make JvMaxMin obsolete

function MaxFloat(const Values: array of Extended): Extended;
var
  I: Cardinal;
begin
  Result := Values[Low(Values)];
  for I := Low(Values) + 1 to High(Values) do
    if Values[I] > Result then
      Result := Values[I];
end;

procedure InvalidBitmap;
begin
  raise EInvalidGraphic.CreateRes(@SInvalidBitmap);
end;

function WidthBytes(I: Longint): Longint;
begin
  Result := ((I + 31) div 32) * 4;
end;

function PixelFormatToColors(PixelFormat: TPixelFormat): Integer;
begin
  case PixelFormat of
    pf1bit:
      Result := 2; 
    pf8bit:
      Result := 256;
  else
    Result := 0;
  end;
end;



{ Quantizing }
{ Quantizing procedures based on free C source code written by
  Joe C. Oliphant, CompuServe 71742, 1451, joe_oliphant att csufresno dott edu }

const
  MAX_COLORS = 4096;

type
  TTriple = array [0..2] of Byte;

  PQColor = ^TQColor;
  TQColor = record
    RGB: TTriple;
    NewColorIndex: Byte;
    Count: Longint;
    PNext: PQColor;
  end;

  PQColorArray = ^TQColorArray;
  TQColorArray = array [0..MAX_COLORS - 1] of TQColor;

  PQColorList = ^TQColorList;
  TQColorList = array [0..MaxListSize - 1] of PQColor;

  PNewColor = ^TNewColor;
  TNewColor = record
    RGBMin: TTriple;
    RGBWidth: TTriple;
    NumEntries: Longint;
    Count: Longint;
    QuantizedColors: PQColor;
  end;

  PNewColorArray = ^TNewColorArray;
  TNewColorArray = array [Byte] of TNewColor;

procedure PInsert(ColorList: PQColorList; Number: Integer; SortRGBAxis: Integer);
var
  Q1, Q2: PQColor;
  I, J: Integer;
  Temp: PQColor;
begin
  for I := 1 to Number - 1 do
  begin
    Temp := ColorList^[I];
    J := I - 1;
    while J >= 0 do
    begin
      Q1 := Temp;
      Q2 := ColorList^[J];
      if Q1^.RGB[SortRGBAxis] - Q2^.RGB[SortRGBAxis] > 0 then
        Break;
      ColorList^[J + 1] := ColorList^[J];
      Dec(J);
    end;
    ColorList^[J + 1] := Temp;
  end;
end;

procedure PSort(ColorList: PQColorList; Number: Integer; SortRGBAxis: Integer);
var
  Q1, Q2: PQColor;
  I, J, N, Nr: Integer;
  Temp, Part: PQColor;
begin
  if Number < 8 then
  begin
    PInsert(ColorList, Number, SortRGBAxis);
    Exit;
  end;
  Part := ColorList^[Number div 2];
  I := -1;
  J := Number;
  repeat
    repeat
      Inc(I);
      Q1 := ColorList^[I];
      Q2 := Part;
      N := Q1^.RGB[SortRGBAxis] - Q2^.RGB[SortRGBAxis];
    until N >= 0;
    repeat
      Dec(J);
      Q1 := ColorList^[J];
      Q2 := Part;
      N := Q1^.RGB[SortRGBAxis] - Q2^.RGB[SortRGBAxis];
    until N <= 0;
    if I >= J then
      Break;
    Temp := ColorList^[I];
    ColorList^[I] := ColorList^[J];
    ColorList^[J] := Temp;
  until False;
  Nr := Number - I;
  if I < Number div 2 then
  begin
    PSort(ColorList, I, SortRGBAxis);
    PSort(PQColorList(@ColorList^[I]), Nr, SortRGBAxis);
  end
  else
  begin
    PSort(PQColorList(@ColorList^[I]), Nr, SortRGBAxis);
    PSort(ColorList, I, SortRGBAxis);
  end;
end;

function DivideMap(NewColorSubdiv: PNewColorArray; ColorMapSize: Integer;
  var NewColormapSize: Integer; LPSTR: Pointer): Integer;
var
  I, J: Integer;
  MaxSize, Index: Integer;
  NumEntries, MinColor, MaxColor: Integer;
  Sum, Count: Longint;
  QuantizedColor: PQColor;
  SortArray: PQColorList;
  SortRGBAxis: Integer;
begin
  Index := 0;
  SortRGBAxis := 0;
  while ColorMapSize > NewColormapSize do
  begin
    MaxSize := -1;
    for I := 0 to NewColormapSize - 1 do
    begin
      for J := 0 to 2 do
      begin
        if (NewColorSubdiv^[I].RGBWidth[J] > MaxSize) and
          (NewColorSubdiv^[I].NumEntries > 1) then
        begin
          MaxSize := NewColorSubdiv^[I].RGBWidth[J];
          Index := I;
          SortRGBAxis := J;
        end;
      end;
    end;
    if MaxSize = -1 then
    begin
      Result := 1;
      Exit;
    end;
    SortArray := PQColorList(LPSTR);
    J := 0;
    QuantizedColor := NewColorSubdiv^[Index].QuantizedColors;
    while (J < NewColorSubdiv^[Index].NumEntries) and
      (QuantizedColor <> nil) do
    begin
      SortArray^[J] := QuantizedColor;
      Inc(J);
      QuantizedColor := QuantizedColor^.PNext;
    end;
    PSort(SortArray, NewColorSubdiv^[Index].NumEntries, SortRGBAxis);
    for J := 0 to NewColorSubdiv^[Index].NumEntries - 2 do
      SortArray^[J]^.PNext := SortArray^[J + 1];
    SortArray^[NewColorSubdiv^[Index].NumEntries - 1]^.PNext := nil;
    NewColorSubdiv^[Index].QuantizedColors := SortArray^[0];
    QuantizedColor := SortArray^[0];
    Sum := NewColorSubdiv^[Index].Count div 2 - QuantizedColor^.Count;
    NumEntries := 1;
    Count := QuantizedColor^.Count;
    Dec(Sum, QuantizedColor^.PNext^.Count);
    while (Sum >= 0) and (QuantizedColor^.PNext <> nil) and
      (QuantizedColor^.PNext^.PNext <> nil) do
    begin
      QuantizedColor := QuantizedColor^.PNext;
      Inc(NumEntries);
      Inc(Count, QuantizedColor^.Count);
      Dec(Sum, QuantizedColor^.PNext^.Count);
    end;
    MaxColor := (QuantizedColor^.RGB[SortRGBAxis]) shl 4;
    MinColor := (QuantizedColor^.PNext^.RGB[SortRGBAxis]) shl 4;
    NewColorSubdiv^[NewColormapSize].QuantizedColors := QuantizedColor^.PNext;
    QuantizedColor^.PNext := nil;
    NewColorSubdiv^[NewColormapSize].Count := Count;
    Dec(NewColorSubdiv^[Index].Count, Count);
    NewColorSubdiv^[NewColormapSize].NumEntries :=
      NewColorSubdiv^[Index].NumEntries - NumEntries;
    NewColorSubdiv^[Index].NumEntries := NumEntries;
    for J := 0 to 2 do
    begin
      NewColorSubdiv^[NewColormapSize].RGBMin[J] :=
        NewColorSubdiv^[Index].RGBMin[J];
      NewColorSubdiv^[NewColormapSize].RGBWidth[J] :=
        NewColorSubdiv^[Index].RGBWidth[J];
    end;
    NewColorSubdiv^[NewColormapSize].RGBWidth[SortRGBAxis] :=
      NewColorSubdiv^[NewColormapSize].RGBMin[SortRGBAxis] +
      NewColorSubdiv^[NewColormapSize].RGBWidth[SortRGBAxis] -
      MinColor;
    NewColorSubdiv^[NewColormapSize].RGBMin[SortRGBAxis] := MinColor;
    NewColorSubdiv^[Index].RGBWidth[SortRGBAxis] :=
      MaxColor - NewColorSubdiv^[Index].RGBMin[SortRGBAxis];
    Inc(NewColormapSize);
  end;
  Result := 1;
end;

function Quantize(const Bmp: TBitmapInfoHeader; gptr, Data8: Pointer;
  var ColorCount: Integer; var OutputColormap: TRGBPalette): Integer;
type
  PWord = ^Word;
var
  P: PByteArray;
  LineBuffer, Data: Pointer;
  LineWidth: Longint;
  TmpLineWidth, NewLineWidth: Longint;
  I, J: Longint;
  Index: Word;
  NewColormapSize, NumOfEntries: Integer;
  Mems: Longint;
  cRed, cGreen, cBlue: Longint;
  LPSTR, Temp, Tmp: Pointer;
  NewColorSubdiv: PNewColorArray;
  ColorArrayEntries: PQColorArray;
  QuantizedColor: PQColor;
begin
  LineWidth := WidthBytes(Longint(Bmp.biWidth) * Bmp.biBitCount);
  Mems := (Longint(SizeOf(TQColor)) * (MAX_COLORS)) +
    (Longint(SizeOf(TNewColor)) * 256) + LineWidth +
    (Longint(SizeOf(PQColor)) * (MAX_COLORS));
  LPSTR := AllocMemo(Mems);
  try
    Temp := AllocMemo(Longint(Bmp.biWidth) * Longint(Bmp.biHeight) *
      SizeOf(Word));
    try
      ColorArrayEntries := PQColorArray(LPSTR);
      NewColorSubdiv := PNewColorArray(HugeOffset(LPSTR,
        Longint(SizeOf(TQColor)) * (MAX_COLORS)));
      LineBuffer := HugeOffset(LPSTR, (Longint(SizeOf(TQColor)) * (MAX_COLORS))
        +
        (Longint(SizeOf(TNewColor)) * 256));
      for I := 0 to MAX_COLORS - 1 do
      begin
        ColorArrayEntries^[I].RGB[0] := I shr 8;
        ColorArrayEntries^[I].RGB[1] := (I shr 4) and $0F;
        ColorArrayEntries^[I].RGB[2] := I and $0F;
        ColorArrayEntries^[I].Count := 0;
      end;
      Tmp := Temp;
      for I := 0 to Bmp.biHeight - 1 do
      begin
        HMemCpy(LineBuffer, HugeOffset(gptr, (Bmp.biHeight - 1 - I) *
          LineWidth), LineWidth);
        P := LineBuffer;
        for J := 0 to Bmp.biWidth - 1 do
        begin
          Index := (Longint(P^[2] and $F0) shl 4) +
            Longint(P^[1] and $F0) + (Longint(P^[0] and $F0) shr 4);
          Inc(ColorArrayEntries^[Index].Count);
          P := HugeOffset(P, 3);
          PWord(Tmp)^ := Index;
          Tmp := HugeOffset(Tmp, 2);
        end;
      end;
      for I := 0 to 255 do
      begin
        NewColorSubdiv^[I].QuantizedColors := nil;
        NewColorSubdiv^[I].Count := 0;
        NewColorSubdiv^[I].NumEntries := 0;
        for J := 0 to 2 do
        begin
          NewColorSubdiv^[I].RGBMin[J] := 0;
          NewColorSubdiv^[I].RGBWidth[J] := 255;
        end;
      end;
      I := 0;
      while I < MAX_COLORS do
      begin
        if ColorArrayEntries^[I].Count > 0 then
          Break;
        Inc(I);
      end;
      QuantizedColor := @ColorArrayEntries^[I];
      NewColorSubdiv^[0].QuantizedColors := @ColorArrayEntries^[I];
      NumOfEntries := 1;
      Inc(I);
      while I < MAX_COLORS do
      begin
        if ColorArrayEntries^[I].Count > 0 then
        begin
          QuantizedColor^.PNext := @ColorArrayEntries^[I];
          QuantizedColor := @ColorArrayEntries^[I];
          Inc(NumOfEntries);
        end;
        Inc(I);
      end;
      QuantizedColor^.PNext := nil;
      NewColorSubdiv^[0].NumEntries := NumOfEntries;
      NewColorSubdiv^[0].Count := Longint(Bmp.biWidth) * Longint(Bmp.biHeight);
      NewColormapSize := 1;
      DivideMap(NewColorSubdiv, ColorCount, NewColormapSize,
        HugeOffset(LPSTR, Longint(SizeOf(TQColor)) * (MAX_COLORS) +
        Longint(SizeOf(TNewColor)) * 256 + LineWidth));
      if NewColormapSize < ColorCount then
      begin
        for I := NewColormapSize to ColorCount - 1 do
          FillChar(OutputColormap[I], SizeOf(TRGBQuad), 0);
      end;
      for I := 0 to NewColormapSize - 1 do
      begin
        J := NewColorSubdiv^[I].NumEntries;
        if J > 0 then
        begin
          QuantizedColor := NewColorSubdiv^[I].QuantizedColors;
          cRed := 0;
          cGreen := 0;
          cBlue := 0;
          while QuantizedColor <> nil do
          begin
            QuantizedColor^.NewColorIndex := I;
            Inc(cRed, QuantizedColor^.RGB[0]);
            Inc(cGreen, QuantizedColor^.RGB[1]);
            Inc(cBlue, QuantizedColor^.RGB[2]);
            QuantizedColor := QuantizedColor^.PNext;
          end;
          with OutputColormap[I] do
          begin
            rgbRed := (Longint(cRed shl 4) or $0F) div J;
            rgbGreen := (Longint(cGreen shl 4) or $0F) div J;
            rgbBlue := (Longint(cBlue shl 4) or $0F) div J;
            rgbReserved := 0;
            if (rgbRed <= $10) and (rgbGreen <= $10) and (rgbBlue <= $10) then
              FillChar(OutputColormap[I], SizeOf(TRGBQuad), 0); { clBlack }
          end;
        end;
      end;
      TmpLineWidth := Longint(Bmp.biWidth) * SizeOf(Word);
      NewLineWidth := WidthBytes(Longint(Bmp.biWidth) * 8);
      FillChar(Data8^, NewLineWidth * Bmp.biHeight, #0);
      for I := 0 to Bmp.biHeight - 1 do
      begin
        LineBuffer := HugeOffset(Temp, (Bmp.biHeight - 1 - I) * TmpLineWidth);
        Data := HugeOffset(Data8, I * NewLineWidth);
        for J := 0 to Bmp.biWidth - 1 do
        begin
          PByte(Data)^ := ColorArrayEntries^[PWord(LineBuffer)^].NewColorIndex;
          LineBuffer := HugeOffset(LineBuffer, 2);
          Data := HugeOffset(Data, 1);
        end;
      end;
    finally
      FreeMemo(Temp);
    end;
  finally
    FreeMemo(LPSTR);
  end;
  ColorCount := NewColormapSize;
  Result := 0;
end;

{
  Procedures to truncate to lower bits-per-pixel, grayscale, tripel and
  histogram conversion based on freeware C source code of GBM package by
  Andy Key (nyangau att interalpha dott co dott uk). The home page of GBM
  author is at http://www.interalpha.net/customer/nyangau/.
}

{ Truncate to lower bits per pixel }

type
  TTruncLine = procedure(Src, Dest: Pointer; CX: Integer);

  { For 6Rx6Gx6B, 7Rx8Gx4B palettes etc. }

const
  Scale04: array [0..3] of Byte = (0, 85, 170, 255);
  Scale06: array [0..5] of Byte = (0, 51, 102, 153, 204, 255);
  Scale07: array [0..6] of Byte = (0, 43, 85, 128, 170, 213, 255);
  Scale08: array [0..7] of Byte = (0, 36, 73, 109, 146, 182, 219, 255);

  { For 6Rx6Gx6B, 7Rx8Gx4B palettes etc. }

var
  TruncTablesInitialized: Boolean = False;
  TruncIndex04: array [Byte] of Byte;
  TruncIndex06: array [Byte] of Byte;
  TruncIndex07: array [Byte] of Byte;
  TruncIndex08: array [Byte] of Byte;

  { These functions initialises this module }

procedure InitTruncTables;

  function NearestIndex(Value: Byte; const Bytes: array of Byte): Byte;
  var
    B, I: Byte;
    Diff, DiffMin: Word;
  begin
    Result := 0;
    B := Bytes[0];
    DiffMin := Abs(Value - B);
    for I := 1 to High(Bytes) do
    begin
      B := Bytes[I];
      Diff := Abs(Value - B);
      if Diff < DiffMin then
      begin
        DiffMin := Diff;
        Result := I;
      end;
    end;
  end;

var
  I: Integer;
begin
  if not TruncTablesInitialized then
  begin
    TruncTablesInitialized := True;
    // (rom) secured because it is called in initialization section
    // (ahuser) moved from initialization section to "on demand" initialization
    try
      { For 7 Red X 8 Green X 4 Blue palettes etc. }
      for I := 0 to 255 do
      begin
        TruncIndex04[I] := NearestIndex(Byte(I), Scale04);
        TruncIndex06[I] := NearestIndex(Byte(I), Scale06);
        TruncIndex07[I] := NearestIndex(Byte(I), Scale07);
        TruncIndex08[I] := NearestIndex(Byte(I), Scale08);
      end;
    except
    end;
  end;
end;

procedure Trunc(const Header: TBitmapInfoHeader; Src, Dest: Pointer;
  DstBitsPerPixel: Integer; TruncLineProc: TTruncLine);
var
  SrcScanline, DstScanline: Longint;
  Y: Integer;
begin
  SrcScanline := (Header.biWidth * 3 + 3) and not 3;
  DstScanline := ((Header.biWidth * DstBitsPerPixel + 31) div 32) * 4;
  for Y := 0 to Header.biHeight - 1 do
    TruncLineProc(HugeOffset(Src, Y * SrcScanline),
      HugeOffset(Dest, Y * DstScanline), Header.biWidth);
end;

{ return 6Rx6Gx6B palette
  This function makes the palette for the 6 red X 6 green X 6 blue palette.
  216 palette entrys used. Remaining 40 Left blank.
}

procedure TruncPal6R6G6B(var Colors: TRGBPalette);
var
  I, R, G, B: Byte;
begin
  FillChar(Colors, SizeOf(TRGBPalette), $80);
  I := 0;
  for R := 0 to 5 do
    for G := 0 to 5 do
      for B := 0 to 5 do
      begin
        Colors[I].rgbRed := Scale06[R];
        Colors[I].rgbGreen := Scale06[G];
        Colors[I].rgbBlue := Scale06[B];
        Colors[I].rgbReserved := 0;
        Inc(I);
      end;
end;

{ truncate to 6Rx6Gx6B one line }

procedure TruncLine6R6G6B(Src, Dest: Pointer; CX: Integer);
var
  X: Integer;
  R, G, B: Byte;
begin
  InitTruncTables;
  for X := 0 to CX - 1 do
  begin
    B := TruncIndex06[Byte(Src^)];
    Src := HugeOffset(Src, 1);
    G := TruncIndex06[Byte(Src^)];
    Src := HugeOffset(Src, 1);
    R := TruncIndex06[Byte(Src^)];
    Src := HugeOffset(Src, 1);
    PByte(Dest)^ := 6 * (6 * R + G) + B;
    Dest := HugeOffset(Dest, 1);
  end;
end;

{ truncate to 6Rx6Gx6B }

procedure Trunc6R6G6B(const Header: TBitmapInfoHeader;
  const Data24, Data8: Pointer);
begin
  Trunc(Header, Data24, Data8, 8, TruncLine6R6G6B);
end;

{ return 7Rx8Gx4B palette
  This function makes the palette for the 7 red X 8 green X 4 blue palette.
  224 palette entrys used. Remaining 32 Left blank.
  Colours calculated to match those used by 8514/A PM driver.
}

procedure TruncPal7R8G4B(var Colors: TRGBPalette);
var
  I, R, G, B: Byte;
begin
  FillChar(Colors, SizeOf(TRGBPalette), $80);
  I := 0;
  for R := 0 to 6 do
    for G := 0 to 7 do
      for B := 0 to 3 do
      begin
        Colors[I].rgbRed := Scale07[R];
        Colors[I].rgbGreen := Scale08[G];
        Colors[I].rgbBlue := Scale04[B];
        Colors[I].rgbReserved := 0;
        Inc(I);
      end;
end;

{ truncate to 7Rx8Gx4B one line }

procedure TruncLine7R8G4B(Src, Dest: Pointer; CX: Integer);
var
  X: Integer;
  R, G, B: Byte;
begin
  InitTruncTables;
  for X := 0 to CX - 1 do
  begin
    B := TruncIndex04[Byte(Src^)];
    Src := HugeOffset(Src, 1);
    G := TruncIndex08[Byte(Src^)];
    Src := HugeOffset(Src, 1);
    R := TruncIndex07[Byte(Src^)];
    Src := HugeOffset(Src, 1);
    PByte(Dest)^ := 4 * (8 * R + G) + B;
    Dest := HugeOffset(Dest, 1);
  end;
end;

{ truncate to 7Rx8Gx4B }

procedure Trunc7R8G4B(const Header: TBitmapInfoHeader;
  const Data24, Data8: Pointer);
begin
  Trunc(Header, Data24, Data8, 8, TruncLine7R8G4B);
end;

{ Grayscale support }

procedure GrayPal(var Colors: TRGBPalette);
var
  I: Byte;
begin
  FillChar(Colors, SizeOf(TRGBPalette), 0);
  for I := 0 to 255 do
    FillChar(Colors[I], 3, I);
end;

procedure GrayScale(const Header: TBitmapInfoHeader; Data24, Data8: Pointer);
var
  SrcScanline, DstScanline: Longint;
  Y, X: Integer;
  Src, Dest: PByte;
  R, G, B: Byte;
begin
  SrcScanline := (Header.biWidth * 3 + 3) and not 3;
  DstScanline := (Header.biWidth + 3) and not 3;
  for Y := 0 to Header.biHeight - 1 do
  begin
    Src := Data24;
    Dest := Data8;
    for X := 0 to Header.biWidth - 1 do
    begin
      B := Src^;
      Src := HugeOffset(Src, 1);
      G := Src^;
      Src := HugeOffset(Src, 1);
      R := Src^;
      Src := HugeOffset(Src, 1);
      Dest^ := Byte(Longint(Word(R) * 77 + Word(G) * 150 + Word(B) * 29) shr 8);
      Dest := HugeOffset(Dest, 1);
    end;
    Data24 := HugeOffset(Data24, SrcScanline);
    Data8 := HugeOffset(Data8, DstScanline);
  end;
end;

{ Tripel conversion }

procedure TripelPal(var Colors: TRGBPalette);
var
  I: Byte;
begin
  FillChar(Colors, SizeOf(TRGBPalette), 0);
  for I := 0 to $40 do
  begin
    Colors[I].rgbRed := I shl 2;
    Colors[I + $40].rgbGreen := I shl 2;
    Colors[I + $80].rgbBlue := I shl 2;
  end;
end;

procedure Tripel(const Header: TBitmapInfoHeader; Data24, Data8: Pointer);
var
  SrcScanline, DstScanline: Longint;
  Y, X: Integer;
  Src, Dest: PByte;
  R, G, B: Byte;
begin
  SrcScanline := (Header.biWidth * 3 + 3) and not 3;
  DstScanline := (Header.biWidth + 3) and not 3;
  for Y := 0 to Header.biHeight - 1 do
  begin
    Src := Data24;
    Dest := Data8;
    for X := 0 to Header.biWidth - 1 do
    begin
      B := Src^;
      Src := HugeOffset(Src, 1);
      G := Src^;
      Src := HugeOffset(Src, 1);
      R := Src^;
      Src := HugeOffset(Src, 1);
      case ((X + Y) mod 3) of
        0: Dest^ := Byte(R shr 2);
        1: Dest^ := Byte($40 + (G shr 2));
        2: Dest^ := Byte($80 + (B shr 2));
      end;
      Dest := HugeOffset(Dest, 1);
    end;
    Data24 := HugeOffset(Data24, SrcScanline);
    Data8 := HugeOffset(Data8, DstScanline);
  end;
end;

{ Histogram/Frequency-of-use method of color reduction }

const
  MAX_N_COLS = 2049;
  MAX_N_HASH = 5191;

function Hash(R, G, B: Byte): Word;
begin
  Result := Word(Longint(Longint(R + G) * Longint(G + B) * Longint(B + R)) mod MAX_N_HASH);
end;

type
  PFreqRecord = ^TFreqRecord;
  TFreqRecord = record
    B: Byte;
    G: Byte;
    R: Byte;
    Frequency: Longint;
    Nearest: Byte;
  end;

  PHist = ^THist;
  THist = record
    ColCount: Longint;
    Rm: Byte;
    Gm: Byte;
    BM: Byte;
    Freqs: array [0..MAX_N_COLS - 1] of TFreqRecord;
    HashTable: array [0..MAX_N_HASH - 1] of Word;
  end;

function CreateHistogram(R, G, B: Byte): PHist;
{ create empty histogram }
begin
  GetMem(Result, SizeOf(THist));
  with Result^ do
  begin
    Rm := R;
    Gm := G;
    BM := B;
    ColCount := 0;
  end;
  FillChar(Result^.HashTable, MAX_N_HASH * SizeOf(Word), 255);
end;

procedure ClearHistogram(var Hist: PHist; R, G, B: Byte);
begin
  with Hist^ do
  begin
    Rm := R;
    Gm := G;
    BM := B;
    ColCount := 0;
  end;
  FillChar(Hist^.HashTable, MAX_N_HASH * SizeOf(Word), 255);
end;

procedure DeleteHistogram(var Hist: PHist);
begin
  FreeMem(Hist, SizeOf(THist));
  Hist := nil;
end;

function AddToHistogram(var Hist: THist; const Header: TBitmapInfoHeader;
  Data24: Pointer): Boolean;
{ add bitmap data to histogram }
var
  Step24: Integer;
  HashColor, Index: Word;
  Rm, Gm, BM, R, G, B: Byte;
  X, Y, ColCount: Longint;
begin
  Step24 := ((Header.biWidth * 3 + 3) and not 3) - Header.biWidth * 3;
  Rm := Hist.Rm;
  Gm := Hist.Gm;
  BM := Hist.BM;
  ColCount := Hist.ColCount;
  for Y := 0 to Header.biHeight - 1 do
  begin
    for X := 0 to Header.biWidth - 1 do
    begin
      B := Byte(Data24^) and BM;
      Data24 := HugeOffset(Data24, 1);
      G := Byte(Data24^) and Gm;
      Data24 := HugeOffset(Data24, 1);
      R := Byte(Data24^) and Rm;
      Data24 := HugeOffset(Data24, 1);
      HashColor := Hash(R, G, B);
      repeat
        Index := Hist.HashTable[HashColor];
        if (Index = $FFFF) or ((Hist.Freqs[Index].R = R) and
          (Hist.Freqs[Index].G = G) and (Hist.Freqs[Index].B = B)) then
          Break;
        Inc(HashColor);
        if HashColor = MAX_N_HASH then
          HashColor := 0;
      until False;
      { Note: loop will always be broken out of }
      { We don't allow HashTable to fill up above half full }
      if Index = $FFFF then
      begin
        { Not found in Hash table }
        if ColCount = MAX_N_COLS then
        begin
          Result := False;
          Exit;
        end;
        Hist.Freqs[ColCount].Frequency := 1;
        Hist.Freqs[ColCount].B := B;
        Hist.Freqs[ColCount].G := G;
        Hist.Freqs[ColCount].R := R;
        Hist.HashTable[HashColor] := ColCount;
        Inc(ColCount);
      end
      else
      begin
        { Found in Hash table, update index }
        Inc(Hist.Freqs[Index].Frequency);
      end;
    end;
    Data24 := HugeOffset(Data24, Step24);
  end;
  Hist.ColCount := ColCount;
  Result := True;
end;

procedure PalHistogram(var Hist: THist; var Colors: TRGBPalette;
  ColorsWanted: Integer);
{ work out a palette from Hist }
var
  I, J: Longint;
  MinDist, Dist: Longint;
  MaxJ, MinJ: Longint;
  DeltaB, DeltaG, DeltaR: Longint;
  MaxFreq: Longint;
begin
  I := 0;
  MaxJ := 0;
  MinJ := 0;
  { Now find the ColorsWanted most frequently used ones }
  while (I < ColorsWanted) and (I < Hist.ColCount) do
  begin
    MaxFreq := 0;
    for J := 0 to Hist.ColCount - 1 do
      if Hist.Freqs[J].Frequency > MaxFreq then
      begin
        MaxJ := J;
        MaxFreq := Hist.Freqs[J].Frequency;
      end;
    Hist.Freqs[MaxJ].Nearest := Byte(I);
    Hist.Freqs[MaxJ].Frequency := 0; { Prevent later use of Freqs[MaxJ] }
    Colors[I].rgbBlue := Hist.Freqs[MaxJ].B;
    Colors[I].rgbGreen := Hist.Freqs[MaxJ].G;
    Colors[I].rgbRed := Hist.Freqs[MaxJ].R;
    Colors[I].rgbReserved := 0;
    Inc(I);
  end;
  { Unused palette entries will be medium grey }
  while I <= 255 do
  begin
    Colors[I].rgbRed := $80;
    Colors[I].rgbGreen := $80;
    Colors[I].rgbBlue := $80;
    Colors[I].rgbReserved := 0;
    Inc(I);
  end;
  { For the rest, find the closest one in the first ColorsWanted }
  for I := 0 to Hist.ColCount - 1 do
  begin
    if Hist.Freqs[I].Frequency <> 0 then
    begin
      MinDist := 3 * 256 * 256;
      for J := 0 to ColorsWanted - 1 do
      begin
        DeltaB := Hist.Freqs[I].B - Colors[J].rgbBlue;
        DeltaG := Hist.Freqs[I].G - Colors[J].rgbGreen;
        DeltaR := Hist.Freqs[I].R - Colors[J].rgbRed;
        Dist := Longint(DeltaR * DeltaR) + Longint(DeltaG * DeltaG) +
          Longint(DeltaB * DeltaB);
        if Dist < MinDist then
        begin
          MinDist := Dist;
          MinJ := J;
        end;
      end;
      Hist.Freqs[I].Nearest := Byte(MinJ);
    end;
  end;
end;

procedure MapHistogram(var Hist: THist; const Header: TBitmapInfoHeader;
  Data24, Data8: Pointer);
{ map bitmap data to Hist palette }
var
  Step24: Integer;
  Step8: Integer;
  HashColor, Index: Longint;
  Rm, Gm, BM, R, G, B: Byte;
  X, Y: Longint;
begin
  Step24 := ((Header.biWidth * 3 + 3) and not 3) - Header.biWidth * 3;
  Step8 := ((Header.biWidth + 3) and not 3) - Header.biWidth;
  Rm := Hist.Rm;
  Gm := Hist.Gm;
  BM := Hist.BM;
  for Y := 0 to Header.biHeight - 1 do
  begin
    for X := 0 to Header.biWidth - 1 do
    begin
      B := Byte(Data24^) and BM;
      Data24 := HugeOffset(Data24, 1);
      G := Byte(Data24^) and Gm;
      Data24 := HugeOffset(Data24, 1);
      R := Byte(Data24^) and Rm;
      Data24 := HugeOffset(Data24, 1);
      HashColor := Hash(R, G, B);
      repeat
        Index := Hist.HashTable[HashColor];
        if (Hist.Freqs[Index].R = R) and (Hist.Freqs[Index].G = G) and
          (Hist.Freqs[Index].B = B) then
          Break;
        Inc(HashColor);
        if HashColor = MAX_N_HASH then
          HashColor := 0;
      until False;
      PByte(Data8)^ := Hist.Freqs[Index].Nearest;
      Data8 := HugeOffset(Data8, 1);
    end;
    Data24 := HugeOffset(Data24, Step24);
    Data8 := HugeOffset(Data8, Step8);
  end;
end;

procedure Histogram(const Header: TBitmapInfoHeader; var Colors: TRGBPalette;
  Data24, Data8: Pointer; ColorsWanted: Integer; Rm, Gm, BM: Byte);
{ map single bitmap to frequency optimised palette }
var
  Hist: PHist;
begin
  Hist := CreateHistogram(Rm, Gm, BM);
  try
    repeat
      if AddToHistogram(Hist^, Header, Data24) then
        Break
      else
      begin
        if Gm > Rm then
          Gm := Gm shl 1
        else
        if Rm > BM then
          Rm := Rm shl 1
        else
          BM := BM shl 1;
        ClearHistogram(Hist, Rm, Gm, BM);
      end;
    until False;
    { Above loop will always be exited as if masks get rough   }
    { enough, ultimately number of unique colours < MAX_N_COLS }
    PalHistogram(Hist^, Colors, ColorsWanted);
    MapHistogram(Hist^, Header, Data24, Data8);
  finally
    DeleteHistogram(Hist);
  end;
end;

{ expand to 24 bits-per-pixel }

(*
procedure ExpandTo24Bit(const Header: TBitmapInfoHeader; Colors: TRGBPalette;
  Data, NewData: Pointer);
var
  Scanline, NewScanline: Longint;
  Y, X: Integer;
  Src, Dest: Pointer;
  C: Byte;
begin
  if Header.biBitCount = 24 then
  begin
    Exit;
  end;
  Scanline := ((Header.biWidth * Header.biBitCount + 31) div 32) * 4;
  NewScanline := ((Header.biWidth * 3 + 3) and not 3);
  for Y := 0 to Header.biHeight - 1 do
  begin
    Src := HugeOffset(Data, Y * Scanline);
    Dest := HugeOffset(NewData, Y * NewScanline);
    case Header.biBitCount of
      1:
      begin
        C := 0;
        for X := 0 to Header.biWidth - 1 do
        begin
          if (X and 7) = 0 then
          begin
            C := Byte(Src^);
            Src := HugeOffset(Src, 1);
          end
          else C := C shl 1;
          PByte(Dest)^ := Colors[C shr 7].rgbBlue;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C shr 7].rgbGreen;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C shr 7].rgbRed;
          Dest := HugeOffset(Dest, 1);
        end;
      end;
      4:
      begin
        X := 0;
        while X < Header.biWidth - 1 do
        begin
          C := Byte(Src^);
          Src := HugeOffset(Src, 1);
          PByte(Dest)^ := Colors[C shr 4].rgbBlue;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C shr 4].rgbGreen;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C shr 4].rgbRed;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C and 15].rgbBlue;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C and 15].rgbGreen;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C and 15].rgbRed;
          Dest := HugeOffset(Dest, 1);
          Inc(X, 2);
        end;
        if X < Header.biWidth then
        begin
          C := Byte(Src^);
          PByte(Dest)^ := Colors[C shr 4].rgbBlue;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C shr 4].rgbGreen;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C shr 4].rgbRed;
          {Dest := HugeOffset(Dest, 1);}
        end;
      end;
      8:
      begin
        for X := 0 to Header.biWidth - 1 do
        begin
          C := Byte(Src^);
          Src := HugeOffset(Src, 1);
          PByte(Dest)^ := Colors[C].rgbBlue;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C].rgbGreen;
          Dest := HugeOffset(Dest, 1);
          PByte(Dest)^ := Colors[C].rgbRed;
          Dest := HugeOffset(Dest, 1);
        end;
      end;
    end;
  end;
end;
*)



function GetBitmapPixelFormat(Bitmap: TBitmap): TPixelFormat;
begin
  Result := Bitmap.PixelFormat;
end;

function BytesPerScanLine(PixelsPerScanline, BitsPerPixel,
  Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;



function ZoomImage(ImageW, ImageH, MaxW, MaxH: Integer; Stretch: Boolean):
  TPoint;
var
  Zoom: Double;
begin
  Result := Point(0, 0);
  if (MaxW <= 0) or (MaxH <= 0) or (ImageW <= 0) or (ImageH <= 0) then
    Exit;
  with Result do
    if Stretch then
    begin
      Zoom := MaxFloat([ImageW / MaxW, ImageH / MaxH]);
      if Zoom > 0 then
      begin
        X := Round(ImageW * 0.98 / Zoom);
        Y := Round(ImageH * 0.98 / Zoom);
      end
      else
      begin
        X := ImageW;
        Y := ImageH;
      end;
    end
    else
    begin
      X := MaxW;
      Y := MaxH;
    end;
end;

procedure TileImage(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
var
  X, Y: Integer;
  SaveIndex: Integer;
begin
  if (Image.Width = 0) or (Image.Height = 0) then
    Exit;
  SaveIndex := SaveDC(Canvas.Handle);
  try
    with Rect do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    for X := 0 to (RectWidth(Rect) div Image.Width) do
      for Y := 0 to (RectHeight(Rect) div Image.Height) do
        Canvas.Draw(Rect.Left + X * Image.Width,
          Rect.Top + Y * Image.Height, Image);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

//=== { TJvGradientOptions } =================================================

constructor TJvGradientOptions.Create;
begin
  inherited Create;
  FStartColor := clSilver;
  FEndColor := clGray;
  FStepCount := 64;
  FDirection := fdTopToBottom;
end;

procedure TJvGradientOptions.Assign(Source: TPersistent);
begin
  if Source is TJvGradientOptions then
  begin
    with TJvGradientOptions(Source) do
    begin
      Self.FStartColor := StartColor;
      Self.FEndColor := EndColor;
      Self.FStepCount := StepCount;
      Self.FDirection := Direction;
      Self.FVisible := Visible;
    end;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TJvGradientOptions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvGradientOptions.Draw(Canvas: TCanvas; Rect: TRect);
begin
  GradientFillRect(Canvas, Rect, FStartColor, FEndColor, FDirection, FStepCount);
end;

procedure TJvGradientOptions.SetStartColor(Value: TColor);
begin
  if Value <> FStartColor then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

procedure TJvGradientOptions.SetEndColor(Value: TColor);
begin
  if Value <> FEndColor then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TJvGradientOptions.SetDirection(Value: TFillDirection);
begin
  if Value <> FDirection then
  begin
    FDirection := Value;
    Changed;
  end;
end;

procedure TJvGradientOptions.SetStepCount(Value: Byte);
begin
  if Value <> FStepCount then
  begin
    FStepCount := Value;
    Changed;
  end;
end;

procedure TJvGradientOptions.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;
{ end JvGraph }

{ begin JvCtrlUtils }

//=== ToolBarMenu ============================================================

procedure JvCreateToolBarMenu(AForm: TForm; AToolBar: TToolBar;
  AMenu: TMainMenu);
var
  I, TotalWidth: Integer;
  Button: TToolButton;
begin
  if AForm.FormStyle = fsMDIForm then
    {$IFNDEF NO_JCL}
    raise EJclError.CreateResRec(@RsNotForMdi);
    {$ELSE}
    raise Exception.CreateRes(@RsNotForMdi);
    {$ENDIF !NO_JCL}
  if AMenu = nil then
    AMenu := AForm.Menu;
  if AMenu = nil then
    Exit;
  with AToolBar do
  begin
    TotalWidth := BorderWidth;  
    for I := ControlCount - 1 downto 0 do
      if Controls[I] is TToolButton then
        Controls[I].Free; 
    ShowCaptions := True;
  end;
  with AMenu do
    for I := Items.Count - 1 downto 0 do
    begin
      Button := TToolButton.Create(AToolBar);
      Button.Parent := AToolBar;
      Button.AutoSize := True;
      Button.Caption := Items[I].Caption;
      Button.Grouped := True;  
      if Items[I].Action <> nil then
        Button.Action := Items[I].Action
      else
      begin
        Button.Caption := Items[I].Caption;
        Button.Enabled := Items[I].Enabled;
        Button.ImageIndex := Items[I].ImageIndex;
        Button.OnClick := Items[I].OnClick;
      end; 
      Inc(TotalWidth, Button.Width + AToolBar.BorderWidth);
    end;
  AToolBar.Width := TotalWidth;
  AForm.Menu := nil;
end;

//=== ListView functions =====================================================

procedure JvListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean; Headers: Boolean);
var
  R, C: Integer;
  ColWidths: array of Word;
  S: string;

  procedure AddLine;
  begin
    Strings.Add(TrimRight(S));
  end;

  function StrPadRight(const S: string; Len: Integer): string;
  begin
    Result := S;
    if Len > Length(S) then
      Result := Result + MakeStr(' ', Len - Length(S))
  end;

  function StrPadLeft(const S: string; Len: Integer): string;
  begin
    Result := S;
    if Len > Length(S) then
      Result := MakeStr(' ', Len - Length(S)) + Result;
  end;

  function MakeCellStr(const Text: string; Index: Integer): string;
  begin
    with ListView.Columns[Index] do
      if Alignment = taLeftJustify then
        Result := StrPadRight(Text, ColWidths[Index] + 1)
      else
        Result := StrPadLeft(Text, ColWidths[Index]) + ' ';
  end;

begin
  SetLength(S, 256);
  with ListView do
  begin
    SetLength(ColWidths, Columns.Count);
    if Headers then
      for C := 0 to Columns.Count - 1 do
        ColWidths[C] := Length(Trim(Columns[C].Caption));
    for R := 0 to Items.Count - 1 do
      if not SelectedOnly or Items[R].Selected then
      begin
        ColWidths[0] := Max(ColWidths[0], Length(Trim(Items[R].Caption)));
        for C := 0 to Items[R].SubItems.Count - 1 do
          ColWidths[C + 1] := Max(ColWidths[C + 1],
            Length(Trim(Items[R].SubItems[C])));
      end;
    Strings.BeginUpdate;
    try
      if Headers then
        with Columns do
        begin
          S := '';
          for C := 0 to Count - 1 do
            S := S + MakeCellStr(Items[C].Caption, C);
          AddLine;
          S := '';
          for C := 0 to Count - 1 do
            S := S + StringOfChar('-', ColWidths[C]) + ' ';
          AddLine;
        end;
      for R := 0 to Items.Count - 1 do
        if not SelectedOnly or Items[R].Selected then
          with Items[R] do
          begin
            S := MakeCellStr(Caption, 0);
            for C := 0 to Min(SubItems.Count, Columns.Count - 1) - 1 do
              S := S + MakeCellStr(SubItems[C], C + 1);
            AddLine;
          end;
    finally
      Strings.EndUpdate;
    end;
  end;
end;

function JvListViewSafeSubItemString(Item: TListItem; SubItemIndex: Integer): string;
begin
  if Item.SubItems.Count > SubItemIndex then
    Result := Item.SubItems[SubItemIndex]
  else
    Result := '';
end;

procedure JvListViewSortClick(Column: TListColumn; AscendingSortImage: Integer;
  DescendingSortImage: Integer);
var
  ListView: TListView; 
begin
  ListView := TListColumns(Column.Collection).Owner as TListView;
  ListView.Columns.BeginUpdate;
  try
    with ListView.Columns do 
    if ListView.Tag and $FF = Column.Index then
      ListView.Tag := ListView.Tag xor $100
    else
      ListView.Tag := Column.Index; 
  finally
    ListView.Columns.EndUpdate;
  end;
end;

procedure JvListViewCompare(ListView: TListView; Item1, Item2: TListItem;
  var Compare: Integer);
var
  ColIndex: Integer;

  function FmtStrToInt(S: string): Integer;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(S) do
      if not (S[I] in (DigitChars + ['-'])) then
        Delete(S, I, 1)
      else
        Inc(I);
    Result := StrToInt(S);
  end;

begin
  with ListView do
  begin
    ColIndex := Tag and $FF - 1;
    if Columns[ColIndex + 1].Alignment = taLeftJustify then
    begin
      if ColIndex = -1 then
        Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
      else
        Compare := AnsiCompareText(Item1.SubItems[ColIndex],
          Item2.SubItems[ColIndex]);
    end
    else
    begin
      if ColIndex = -1 then
        Compare := FmtStrToInt(Item1.Caption) - FmtStrToInt(Item2.Caption)
      else
        Compare := FmtStrToInt(Item1.SubItems[ColIndex]) -
          FmtStrToInt(Item2.SubItems[ColIndex]);
    end;
    if Tag and $100 <> 0 then
      Compare := -Compare;
  end;
end;

procedure JvListViewSelectAll(ListView: TListView; Deselect: Boolean);
var
  I: Integer; 
  SaveOnSelectItem: TLVSelectItemEvent;
begin
  with ListView do
    if MultiSelect then
    begin
      Items.BeginUpdate;
      SaveOnSelectItem := OnSelectItem;
      WaitCursor;
      try  
        for I := 0 to Items.Count - 1 do
          Items[I].Selected := not Deselect; 
      finally
        OnSelectItem := SaveOnSelectItem;
        Items.EndUpdate;
      end;
    end;
end;

function JvListViewSaveState(ListView: TListView): TJvLVItemStateData;
var
  TempItem: TListItem;
begin
  with Result do
  begin
    Focused := Assigned(ListView.ItemFocused);
    Selected := Assigned(ListView.Selected);
    if Focused then
      TempItem := ListView.ItemFocused
    else
    if Selected then
      TempItem := ListView.Selected
    else
      TempItem := nil;
    if TempItem <> nil then
    begin
      Caption := TempItem.Caption;
      Data := TempItem.Data;
    end
    else
    begin
      Caption := '';
      Data := nil;
    end;
  end;
end;

function JvListViewRestoreState(ListView: TListView; Data: TJvLVItemStateData;
  MakeVisible: Boolean; FocusFirst: Boolean): Boolean;
var
  TempItem: TListItem;
begin
  with ListView do
  begin
    TempItem := FindCaption(0, Data.Caption, False, True, False);
    Result := TempItem <> nil;
    if Result then
    begin
      TempItem.Focused := Data.Focused;
      TempItem.Selected := Data.Selected;
    end
    else
    if FocusFirst and (Items.Count > 0) then
    begin
      TempItem := Items[0];
      TempItem.Focused := True;
      TempItem.Selected := True;
    end;
    if MakeVisible and (TempItem <> nil) then  
      TempItem.MakeVisible; 
  end;
end;



//== MessageBox ==============================================================

function JvMessageBox(const Text, Caption: string; Flags: DWORD): Integer;
begin
  Result := MsgBox(Text, Caption, Flags);
end;

function JvMessageBox(const Text: string; Flags: DWORD): Integer;
begin
  Result := MsgBox(Text, Application.Title, Flags);
end;

procedure UpdateTrackFont(TrackFont, Font: TFont; TrackOptions: TJvTrackFontOptions);
begin
  if hoFollowFont in TrackOptions then
  begin
    if not (hoPreserveCharSet in TrackOptions) then
      TrackFont.Charset := Font.Charset;
    if not (hoPreserveColor in TrackOptions) then
      TrackFont.Color := Font.Color;
    if not (hoPreserveHeight in TrackOptions) then
      TrackFont.Height := Font.Height;
    if not (hoPreserveName in TrackOptions) then
      TrackFont.Name := Font.Name;
    if not (hoPreservePitch in TrackOptions) then
      TrackFont.Pitch := Font.Pitch;
    if not (hoPreserveStyle in TrackOptions) then
      TrackFont.Style := Font.Style;
  end;
end;

{ end JvCtrlUtils }

function GetDefaultCheckBoxSize: TSize;
begin  
  Result.cx := 12;
  Result.cy := 12; 
end;

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
var
  tt: TTextMetric;
begin
  // (ahuser) Qt returns different values for TextHeight('Ay') and TextHeigth(#1..#255) 
  Canvas.Start;  // if it is called outside a paint event
  RequiredState(Canvas, [csHandleValid, csFontValid, csBrushValid]); 
  GetTextMetrics(Canvas.Handle, tt); 
  Canvas.Stop; 
  Result := tt.tmHeight;
end;

{$IFDEF MSWINDOWS}

//=== AllocateHWndEx =========================================================

const
  cUtilWindowExClass: TWndClass = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: SizeOf(TMethod);
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindowEx');

function StdWndProc(Window: Windows.HWND; Message, WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
var
  Msg: Messages.TMessage;
  WndProc: TWndMethod;
begin
  TMethod(WndProc).Code := Pointer(GetWindowLong(Window, 0));
  TMethod(WndProc).Data := Pointer(GetWindowLong(Window, 4));
  if Assigned(WndProc) then
  begin
    Msg.Msg := Message;
    Msg.WParam := WParam;
    Msg.LParam := LParam;
    Msg.Result := 0;
    WndProc(Msg);
    Result := Msg.Result;
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): Windows.HWND;
var
  TempClass: TWndClass;
  UtilWindowExClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowExClass := cUtilWindowExClass;
  UtilWindowExClass.hInstance := HInstance;
  UtilWindowExClass.lpfnWndProc := @DefWindowProc;
  if AClassName <> '' then
    UtilWindowExClass.lpszClassName := PChar(AClassName);

  ClassRegistered := Windows.GetClassInfo(HInstance, UtilWindowExClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowExClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowExClass);
  end;
  Result := Windows.CreateWindowEx(Windows.WS_EX_TOOLWINDOW, UtilWindowExClass.lpszClassName,
    '', Windows.WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);

  if Assigned(Method) then
  begin
    Windows.SetWindowLong(Result, 0, Longint(TMethod(Method).Code));
    Windows.SetWindowLong(Result, SizeOf(TMethod(Method).Code), Longint(TMethod(Method).Data));
    Windows.SetWindowLong(Result, GWL_WNDPROC, Longint(@StdWndProc));
  end;
end;

procedure DeallocateHWndEx(Wnd: Windows.HWND);
begin
  Windows.DestroyWindow(Wnd);
end;

function JvMakeObjectInstance(Method: TWndMethod): Pointer;
begin 
  Result := Classes.MakeObjectInstance(Method); 
end;

procedure JvFreeObjectInstance(ObjectInstance: Pointer);
begin
  if ObjectInstance <> nil then 
    Classes.FreeObjectInstance(ObjectInstance); 
end;

{$ENDIF MSWINDOWS}



procedure InitScreenCursors;
begin
  try
    if Screen <> nil then
    begin
      { begin RxLib }
      { (rom) deactivated  can cause problems
      Screen.Cursors[crHand] := LoadCursor(HInstance, 'JV_HANDCUR');
      Screen.Cursors[crDragHand] := LoadCursor(HInstance, 'JV_DRAGCUR');
      }
      { end RxLib } 
    end;
  except
  end;
end;

const
  Lefts = ['[', '{', '('];
  Rights = [']', '}', ')'];

{ Utilities routines }

function FontStylesToString(Styles: TFontStyles): string;
begin
  Result := '';
  if fsBold in Styles then
    Result := Result + 'B';
  if fsItalic in Styles then
    Result := Result + 'I';
  if fsUnderline in Styles then
    Result := Result + 'U';
  if fsStrikeOut in Styles then
    Result := Result + 'S';
end;

function StringToFontStyles(const Styles: string): TFontStyles;
begin
  Result := [];
  if Pos('B', UpperCase(Styles)) > 0 then
    Include(Result, fsBold);
  if Pos('I', UpperCase(Styles)) > 0 then
    Include(Result, fsItalic);
  if Pos('U', UpperCase(Styles)) > 0 then
    Include(Result, fsUnderline);
  if Pos('S', UpperCase(Styles)) > 0 then
    Include(Result, fsStrikeOut);
end;



function RectToStr(Rect: TRect): string;
begin
  with Rect do
    Result := Format('[%d,%d,%d,%d]', [Left, Top, Right, Bottom]);
end;

function StrToRect(const Str: string; const Def: TRect): TRect;
var
  S: string;
  Temp: string[10];
  I: Integer;
begin
  Result := Def;
  S := Str;
  if (S[1] in Lefts) and (S[Length(S)] in Rights) then
  begin
    Delete(S, 1, 1);
    SetLength(S, Length(S) - 1);
  end;
  I := Pos(',', S);
  if I > 0 then
  begin
    Temp := Trim(Copy(S, 1, I - 1));
    Result.Left := StrToIntDef(Temp, Def.Left);
    Delete(S, 1, I);
    I := Pos(',', S);
    if I > 0 then
    begin
      Temp := Trim(Copy(S, 1, I - 1));
      Result.Top := StrToIntDef(Temp, Def.Top);
      Delete(S, 1, I);
      I := Pos(',', S);
      if I > 0 then
      begin
        Temp := Trim(Copy(S, 1, I - 1));
        Result.Right := StrToIntDef(Temp, Def.Right);
        Delete(S, 1, I);
        Temp := Trim(S);
        Result.Bottom := StrToIntDef(Temp, Def.Bottom);
      end;
    end;
  end;
end;

function PointToStr(P: TPoint): string;
begin
  with P do
    Result := Format('[%d,%d]', [X, Y]);
end;

function StrToPoint(const Str: string; const Def: TPoint): TPoint;
var
  S: string;
  Temp: string[10];
  I: Integer;
begin
  Result := Def;
  S := Str;
  if (S[1] in Lefts) and (S[Length(Str)] in Rights) then
  begin
    Delete(S, 1, 1);
    SetLength(S, Length(S) - 1);
  end;
  I := Pos(',', S);
  if I > 0 then
  begin
    Temp := Trim(Copy(S, 1, I - 1));
    Result.X := StrToIntDef(Temp, Def.X);
    Delete(S, 1, I);
    Temp := Trim(S);
    Result.Y := StrToIntDef(Temp, Def.Y);
  end;
end;

procedure DrawArrow(Canvas: TCanvas; Rect: TRect; Color: TColor = clBlack; Direction: TAnchorKind = akBottom);
var
  I, Size: Integer;
begin
  Size := Rect.Right - Rect.Left;
  if Odd(Size) then
  begin
    Dec(Size);
    Dec(Rect.Right);
  end;
  Rect.Bottom := Rect.Top + Size;
  Canvas.Pen.Color := Color;
  case Direction of
    akLeft:
      for I := 0 to Size div 2 do
      begin
        Canvas.MoveTo(Rect.Right - I, Rect.Top + I);
        Canvas.LineTo(Rect.Right - I, Rect.Bottom - I);
      end;
    akRight:
      for I := 0 to Size div 2 do
      begin
        Canvas.MoveTo(Rect.Left + I, Rect.Top + I);
        Canvas.LineTo(Rect.Left + I, Rect.Bottom - I);
      end;
    akTop:
      for I := 0 to Size div 2 do
      begin
        Canvas.MoveTo(Rect.Left + I, Rect.Bottom - I);
        Canvas.LineTo(Rect.Right - I, Rect.Bottom - I);
      end;
    akBottom:
      for I := 0 to Size div 2 do
      begin
        Canvas.MoveTo(Rect.Left + I, Rect.Top + I);
        Canvas.LineTo(Rect.Right - I, Rect.Top + I);
      end;
  end;
end;

function IsPositiveResult(Value: TModalResult): Boolean;
begin
  Result := Value in [mrOk, mrYes, mrAll, mrYesToAll];
end;

function IsNegativeResult(Value: TModalResult): Boolean;
begin
  Result := Value in [mrNo, mrNoToAll];
end;

function IsAbortResult(const Value: TModalResult): Boolean;
begin
  Result := Value in [mrCancel, mrAbort];
end;

function StripAllFromResult(const Value: TModalResult): TModalResult;
begin
  case Value of
    mrAll:
      Result := mrOk;
    mrNoToAll:
      Result := mrNo;
    mrYesToAll:
      Result := mrYes;
  else
    Result := Value;
  end;
end;

//=== { TJvPoint } ===========================================================

procedure TJvPoint.Assign(Source: TPersistent);
begin
  if Source is TJvPoint then
  begin
    FX := TJvPoint(Source).X;
    FY := TJvPoint(Source).Y;
    DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TJvPoint.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvPoint.SetX(Value: Longint);
begin
  FX := Value;
  DoChange;
end;

procedure TJvPoint.SetY(Value: Longint);
begin
  FY := Value;
  DoChange;
end;

//=== { TJvRect } ============================================================

constructor TJvRect.Create;
begin
  inherited Create;
  FTopLeft := TJvPoint.Create;
  FBottomRight := TJvPoint.Create;
  FTopLeft.OnChange := PointChange;
  FBottomRight.OnChange := PointChange;
end;

destructor TJvRect.Destroy;
begin
  FTopLeft.Free;
  FBottomRight.Free;
  inherited Destroy;
end;

procedure TJvRect.Assign(Source: TPersistent);
begin
  if Source is TJvRect then
  begin
    TopLeft.Assign(TJvRect(Source).TopLeft);
    BottomRight.Assign(TJvRect(Source).BottomRight);
    DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TJvRect.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvRect.GetBottom: Integer;
begin
  Result := FBottomRight.Y;
end;

function TJvRect.GetLeft: Integer;
begin
  Result := FTopLeft.X;
end;

function TJvRect.GetRight: Integer;
begin
  Result := FBottomRight.X;
end;

function TJvRect.GetTop: Integer;
begin
  Result := FTopLeft.Y;
end;

procedure TJvRect.PointChange(Sender: TObject);
begin
  DoChange;
end;

procedure TJvRect.SetBottom(Value: Integer);
begin
  FBottomRight.Y := Value;
end;

procedure TJvRect.SetBottomRight(Value: TJvPoint);
begin
  FBottomRight.Assign(Value);
end;

procedure TJvRect.SetLeft(Value: Integer);
begin
  FTopLeft.X := Value;
end;

procedure TJvRect.SetRight(Value: Integer);
begin
  FBottomRight.X := Value;
end;

procedure TJvRect.SetTop(Value: Integer);
begin
  FTopLeft.Y := Value;
end;

procedure TJvRect.SetTopLeft(Value: TJvPoint);
begin
  FTopLeft.Assign(Value);
end;

function TJvRect.GetHeight: Integer;
begin
  Result := FBottomRight.Y - FTopLeft.Y;
end;

function TJvRect.GetWidth: Integer;
begin
  Result := FBottomRight.X - FTopLeft.X;
end;

procedure TJvRect.SetHeight(Value: Integer);
begin
  FBottomRight.Y := FTopLeft.Y + Value;
end;

procedure TJvRect.SetWidth(Value: Integer);
begin
  FBottomRight.X := FTopLeft.X + Value;
end;

function SelectColorByLuminance(AColor, DarkColor, BrightColor: TColor): TColor;
var
  ACol: Longint;
begin
  ACol := ColorToRGB(AColor) and $00FFFFFF;
  if ((2.99 * GetRValue(ACol) + 5.87 * GetGValue(ACol) + 1.14 * GetBValue(ACol)) > $400) then
    Result := DarkColor
  else
    Result := BrightColor;
end;

initialization
  InitScreenCursors;

finalization
  FinalizeUnit(sUnitName);

end.

