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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvJVCLUtils;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Variants,
  {$IFDEF MSWINDOWS}
  Windows, Messages, ShellAPI, Registry,
  {$ENDIF MSWINDOWS}
  Types,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF}
  SysUtils,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls, Menus,
  Dialogs, ComCtrls, ImgList, Grids, MultiMon,
  Classes, // must be after "Forms"
  JclBase,
  JvJCLUtils, JvAppStorage, JvTypes;

// Transform an icon to a bitmap
function IconToBitmap(Ico: HICON): TBitmap;
// Transform an icon to a bitmap using an image list
function IconToBitmap2(Ico: HICON; Size: Integer = 32;
  TransparentColor: TColor = clNone): TBitmap;
function IconToBitmap3(Ico: HICON; Size: Integer = 32;
  TransparentColor: TColor = clNone): TBitmap;

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


// hides / shows the a forms caption area
procedure HideFormCaption(FormHandle: THandle; Hide: Boolean);

{$IFDEF MSWINDOWS}
type
  TJvWallpaperStyle = (wpTile, wpCenter, wpStretch);

// set the background wallpaper (two versions)
procedure SetWallpaper(const Path: string); overload;
procedure SetWallpaper(const Path: string; Style: TJvWallpaperStyle); overload;
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

function CreateRotatedFont(Font: TFont; Angle: Integer): HFONT;

//1 This function validates if the control or any of it subcontrols has the focus.
function IsSubControlFocused(AControl: TWinControl): Boolean;

// launches the specified CPL file
// format: <Filename> [,@n] or [,,m] or [,@n,m]
// where @n = zero-based index of the applet to start (if there is more than one
// m is the zero-based index of the tab to display

procedure LaunchCpl(const FileName: string);

// for Win 2000 and XP
procedure ShowSafeRemovalDialog;

{
  GetControlPanelApplets retrieves information about all control panel applets in a specified folder.
  APath is the Path to the folder to search and AMask is the filename mask (containing wildcards if necessary) to use.

  The information is returned in the Strings and Images lists according to the following rules:
   The Display Name and Path to the CPL file is returned in Strings with the following format:
     '<displayname>=<Path>'
   You can access the DisplayName by using the Strings.Names array and the Path by accessing the Strings.Values array
   Strings.Objects can contain either of two values depending on if Images is nil or not:
     * If Images is nil then Strings.Objects contains the image for the applet as a TBitmap. Note that the caller (you)
     is responsible for freeing the bitmaps in this case
     * If Images <> nil, then the Strings.Objects array contains the index of the image in the Images array for the selected item.
       To access and use the ImageIndex, typecast Strings.Objects to an int:
         Tmp.Name := Strings.Name[I];
         Tmp.ImageIndex := Integer(Strings.Objects[I]);
  The function returns True if any Control Panel Applets were found (i.e Strings.Count is > 0 when returning)
}

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings;
  Images: TCustomImageList = nil): Boolean;
{ GetControlPanelApplet works like GetControlPanelApplets, with the difference that it only loads and searches one cpl file (according to AFilename).
  Note though, that some CPL's contains multiple applets, so the Strings and Images lists can contain multiple return values.
  The function returns True if any Control Panel Applets were found in AFilename (i.e if items were added to Strings)
}
function GetControlPanelApplet(const AFileName: string; Strings: TStrings;
  Images: TCustomImageList = nil): Boolean;

function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
function PaletteColor(Color: TColor): Longint;
procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);

procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);

function GetTickCount64: Int64;
procedure Delay(MSecs: Int64); // WARNING: This function calls ProcessMessages
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
function GetNextFreeCursorIndex(StartHint: Integer; PreDefined: Boolean): Integer;
function WaitCursor: IInterface;
function ScreenCursor(ACursor: TCursor): IInterface;
{$IFDEF MSWINDOWS}
// loads the more modern looking drag cursors from OLE32.DLL
function LoadOLEDragCursors: Boolean;
// set some default cursor from JVCL
{$ENDIF MSWINDOWS}
procedure SetDefaultJVCLCursors;


function LoadAniCursor(Instance: THandle; ResID: PChar): HCURSOR;

{ Windows API level routines }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, Srch: Integer;
  Palette: HPALETTE; TransparentColor: TColorRef);
procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBITMAP;
  DstX, DstY: Integer; TransparentColor: TColorRef);
function PaletteEntries(Palette: HPALETTE): Integer;
procedure ShadeRect(DC: HDC; const Rect: TRect);

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
  TJvDesktopCanvas = class(TCanvas)
  private
    FDC: HDC;
  protected
    procedure CreateHandle; override;
  public
    destructor Destroy; override;
    procedure SetOrigin(X, Y: Integer);
    procedure FreeHandle;
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

function MsgBox(const Caption, Text: string; Flags: Integer): Integer; overload;
function MsgBox(Handle: THandle; const Caption, Text: string; Flags: Integer): Integer; overload;
function MsgDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext: Integer; Control: TWinControl): Integer;
function MsgDlgDef(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpContext: Integer;
  Control: TWinControl): Integer;

(***** Utility MessageBox based dialogs *)
// returns True if user clicked Yes
function MsgYesNo(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Boolean;
// returns True if user clicked Retry
function MsgRetryCancel(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Boolean;
// returns IDABORT, IDRETRY or IDIGNORE
function MsgAbortRetryIgnore(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Integer;
// returns IDYES, IDNO or IDCANCEL
function MsgYesNoCancel(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Integer;
// returns True if user clicked OK
function MsgOKCancel(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Boolean;

// dialog without icon
procedure MsgOK(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
// dialog with info icon
procedure MsgInfo(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
// dialog with warning icon
procedure MsgWarn(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
// dialog with question icon
procedure MsgQuestion(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
// dialog with error icon
procedure MsgError(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
// dialog with custom icon (must be available in the app resource)
procedure MsgAbout(Handle: Integer; const Msg, Caption: string; const IcoName: string = 'MAINICON'; Flags: DWORD = MB_OK);

{**** Windows routines }

{ LoadIcoToImage loads two icons from resource named NameRes,
  into two image lists ALarge and ASmall}
procedure LoadIcoToImage(ALarge, ASmall: ImgList.TCustomImageList;
  const NameRes: string);

{ Works like InputQuery but displays 2 edits. If PasswordChar <> #0, the second edit's PasswordChar is set }
function DualInputQuery(const ACaption, Prompt1, Prompt2: string;
  var AValue1, AValue2: string; PasswordChar: Char = #0): Boolean;

{ Works like InputQuery but set the edit's PasswordChar to PasswordChar. If PasswordChar = #0, works exactly like InputQuery }
function InputQueryPassword(const ACaption, APrompt: string; PasswordChar: Char; var Value: string): Boolean;


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
function ResolveLink(const HWND: THandle; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
{$ENDIF MSWINDOWS}

type
  TProcObj = procedure of object;

procedure ExecAfterPause(Proc: TProcObj; Pause: Integer);

{ end JvUtils }

{ begin JvAppUtils}
function GetFirstParentForm(Control: TControl): TCustomForm;
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

function FontToString(Font: TFont): string;
function StringToFont(const Str: string): TFont;

function RectToStr(Rect: TRect): string;
function StrToRect(const Str: string; const Def: TRect): TRect;
function PointToStr(P: TPoint): string;
function StrToPoint(const Str: string; const Def: TPoint): TPoint;

procedure AppBroadcast(Msg: UINT; wParam: WPARAM; lParam: LPARAM);

procedure AppTaskbarIcons(AppOnly: Boolean);


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

function GetPaletteBitmapFormat(Bitmap: TBitmap): TPixelFormat;
procedure SetBitmapPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat;
  Method: TMappingMethod);
function BitmapToMemoryStream(Bitmap: TBitmap; PixelFormat: TPixelFormat;
  Method: TMappingMethod): TMemoryStream;
procedure GrayscaleBitmap(Bitmap: TBitmap);

function BitmapToMemory(Bitmap: TBitmap; Colors: Integer): TStream;
procedure SaveBitmapToFile(const FileName: string; Bitmap: TBitmap;
  Colors: Integer);

function ScreenPixelFormat: TPixelFormat;
function ScreenColorCount: Integer;

var
  DefaultMappingMethod: TMappingMethod = mmHistogram;


function GetWorkareaRect(Monitor: TMonitor): TRect;
function FindMonitor(Handle: HMONITOR): TMonitor;

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
  // equivalent of TPoint, but that can be a published property
  TJvPoint = class(TPersistent)
  private
    FY: Longint;
    FX: Longint;
    FOnChange: TNotifyEvent;
    procedure SetX(Value: Longint);
    procedure SetY(Value: Longint);
    function GetAsPoint: TPoint;
    procedure SetAsPoint(const Value: TPoint);
  protected
    procedure DoChange;
  public
    procedure AssignPoint(const Source: TPoint);
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(const Source: TPoint); reintroduce; overload;
    procedure CopyToPoint(var Point: TPoint);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property AsPoint: TPoint read GetAsPoint write SetAsPoint;  
  published
    property X: Longint read FX write SetX default 0;
    property Y: Longint read FY write SetY default 0;
  end;

  // equivalent of TRect, but that can be a published property
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
    procedure AssignRect(const Source: TRect);
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(const Source: TRect); reintroduce; overload;
    procedure CopyToRect(var Rect: TRect);
    property TopLeft: TJvPoint read FTopLeft write SetTopLeft;
    property BottomRight: TJvPoint read FBottomRight write SetBottomRight;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: Integer read GetLeft write SetLeft default 0;
    property Top: Integer read GetTop write SetTop default 0;
    property Right: Integer read GetRight write SetRight default 0;
    property Bottom: Integer read GetBottom write SetBottom default 0;
  end;

  TJvSize = class(TPersistent)
  private
    FWidth: Longint;
    FHeight: Longint;
    FOnChange: TNotifyEvent;
    procedure SetWidth(Value: Longint);
    procedure SetHeight(Value: Longint);
    function GetSize: TSize;
    procedure SetSize(const Value: TSize);
  protected
    procedure DoChange;
  public
    procedure AssignSize(const Source: TSize);
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(const Source: TSize); reintroduce; overload;
    procedure CopyToSize(var Size: TSize);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property AsSize: TSize read GetSize write SetSize;
  published
    property Width: Longint read FWidth write SetWidth default 0;
    property Height: Longint read FHeight write SetHeight default 0;
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


function JvListViewGetOrderedColumnIndex(Column: TListColumn): Integer;
procedure JvListViewSetSystemImageList(ListView: TListView);


//------------------------------------------------------------------------------
// MessageBox
//------------------------------------------------------------------------------

function JvMessageBox(const Text, Caption: string; Flags: DWORD): Integer; overload;
function JvMessageBox(const Text: string; Flags: DWORD): Integer; overload;

{ end JvCtrlUtils }

procedure UpdateTrackFont(TrackFont, Font: TFont; TrackOptions: TJvTrackFontOptions);
function IsHotTrackFontDfmStored(TrackFont, Font: TFont; TrackOptions: TJvTrackFontOptions): Boolean;

// Returns the size of the image
// used for checkboxes and radiobuttons.
// Originally from Mike Lischke
function GetDefaultCheckBoxSize: TSize;

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;

{$IFDEF MSWINDOWS}
// AllocateHWndEx works like Classes.AllocateHWnd but does not use any virtual memory pages
function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): THandle;
// DeallocateHWndEx works like Classes.DeallocateHWnd but does not use any virtual memory pages
procedure DeallocateHWndEx(Wnd: THandle);

function JvMakeObjectInstance(Method: TWndMethod): Pointer;
procedure JvFreeObjectInstance(ObjectInstance: Pointer);
{$ENDIF MSWINDOWS}

function GetAppHandle: THandle;
// DrawArrow draws a standard arrow in any of four directions and with the specifed color.
// Rect is the area to draw the arrow in and also defines the size of the arrow
// Note that this procedure might shrink Rect so that it's width and height is always
// the same and the width and height are always even, i.e calling with
// Rect(0,0,12,12) (odd) is the same as calling with Rect(0,0,11,11) (even)
// Direction defines the direction of the arrow. If Direction is akLeft, the arrow point is
// pointing to the left
procedure DrawArrow(Canvas: TCanvas; ARect: TRect; Color: TColor = clBlack;
  Direction: TAnchorKind = akBottom; Margin: Integer = 0); overload;

// The param X and Y is the topleft.X and topleft.Y of rect to draw arrow
procedure DrawArrow(Canvas: TCanvas; X, Y: Integer;  Size: Integer;
  Color: TColor = clBlack; Direction: TAnchorKind = akBottom); overload;


procedure DrawLine(Canvas: TCanvas; X, Y, X2, Y2: Integer);
  
function IsPositiveResult(Value: TModalResult): Boolean;
function IsNegativeResult(Value: TModalResult): Boolean;
function IsAbortResult(const Value: TModalResult): Boolean;
function StripAllFromResult(const Value: TModalResult): TModalResult;
// returns either BrightColor or DarkColor depending on the luminance of AColor
// This function gives the same result (AFAIK) as the function used in Windows to
// calculate the desktop icon text color based on the desktop background color
function SelectColorByLuminance(AColor, DarkColor, BrightColor: TColor): TColor;

// (peter3) implementation moved from JvHTControls.
type
  TJvHTMLCalcType = (htmlShow, htmlCalcWidth, htmlCalcHeight, htmlHyperLink);

procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer = 100); overload;
procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width, Height: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer = 100); overload;
function HTMLDrawText(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): string;
function HTMLDrawTextHL(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; MouseX, MouseY: Integer; SuperSubScriptRatio: Double; 
  Scale: Integer = 100): string;
function HTMLPlainText(const Text: string): string;
function HTMLTextExtent(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): TSize;
function HTMLTextWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
function HTMLTextHeight(Canvas: TCanvas; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
function HTMLPrepareText(const Text: string): string;

// This type is used to allow an easy migration from a TBitmap property to a
// TPicture property. It is, for instance, used in TJvXPButton so that users
// migrating to the JVCL can still open their applications and benefit
// automatically from the change of format. The whole point is that a TPicture
// can also contain an Icon, which could be a valid source for a button glyph.
type
  TJvPicture = class (TPicture)
  private
    procedure ReadBitmapData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  end;

// This class is here because of issue 4859. Basically, using TBitmap as a
// parameter for an event handler is the source of an ambiguity under
// C++ Builder because of Windows::TBitmap. The only solution is to replace
// TBitmap by TJvBitmap in the event handler declarations. This, however,
// forces the Delphi users to change their event handlers so that the IDE
// will not complain when opening the forms.
type
  TJvBitmap = class(TBitmap)
  end;

{
Documentation:
*************

WHAT IT IS:
   These are helper functions to register graphic formats than can
   later be recognized from a stream, thus allowing to rely on the actual
   content of a file rather than from its filename extension.
   This is used in TJvDBImage and TJvImage.

IMAGE FORMATS:
   The implementation is simple: Just register image signatures with
   RegisterGraphicSignature procedure and the methods takes care
   of the correct instantiation of the TGraphic object. The signatures
   register at unit's initialization are: BMP, WMF, EMF, ICO, JPG.
   If you got some other image library (such as GIF, PCX, TIFF, ANI or PNG),
   just register the signature:

     RegisterGraphicSignature(<string value>, <offset>, <class>)

     or

     RegisterGraphicSignature([<byte values>], <offset>, <class>)

   This means:
     When <string value> (or byte values) found at <offset> the graphic
     class to use is <class>

   For example (actual code of the initialization section):

     RegisterGraphicSignature([$D7, $CD], 0, TMetaFile); // WMF
     RegisterGraphicSignature([1, 0], 0, TMetaFile);     // EMF
     RegisterGraphicSignature('JFIF', 6, TJPEGImage);

   You can also unregister signature. IF you want use TGIFImage instead of
   TJvGIFImage, you can unregister with:

     UnregisterGraphicSignature('GIF', 0);

     or just

     UnregisterGraphicSignature(TJvGIFImage); // must add JvGIF unit in uses clause

   then:
     RegisterGraphicSignature('GIF', 0, TGIFImage); // must add GIFImage to uses clause

   If you dont like the signature registration there is a new event called
   OnGetGraphicClass. The event gets the following parameters:

    Sender: TObject;
    Stream: TMemoryStream;
    var GraphicClass: TGraphicClass)

   The memory stream containing the blob data is sent in Stream to allow the user
   to inspect the contents and figure out which graphic class is.

   The graphic class to be used must implement LoadFromStream and SaveToStream
   methods in order to work properly.
}

type
  TJvGetGraphicClassEvent = procedure(Sender: TObject; AStream: TMemoryStream;
    var GraphicClass: TGraphicClass) of object;

procedure RegisterGraphicSignature(const ASignature: AnsiString; AOffset: Integer;
  AGraphicClass: TGraphicClass); overload;
procedure RegisterGraphicSignature(const ASignature: array of Byte; AOffset: Integer;
  AGraphicClass: TGraphicClass); overload;

procedure UnregisterGraphicSignature(AGraphicClass: TGraphicClass); overload;
procedure UnregisterGraphicSignature(const ASignature: AnsiString; AOffset: Integer); overload;
procedure UnregisterGraphicSignature(const ASignature: array of Byte; AOffset: Integer); overload;

function GetGraphicClass(AStream: TStream): TGraphicClass;
function GetGraphicObject(AStream: TStream): TGraphic; overload;
function GetGraphicObject(AStream: TStream; ASender: TObject; AOnProc: TJvGetGraphicClassEvent): TGraphic; overload;

// Coordinate Space/rectangle and Transformation Functions added by dejoy

{Translates child coordinates to parent coordinates.
 not found by iterating up the chain of Parent, not equal to TControl.ClientToParent
 If AParent is nil (Delphi) or NULL (C++), the AChild control's immediate parent is used.
}
function PointChildToParent(APoint: TPoint; AChild: TControl; AParent: TWinControl = nil): TPoint;
{extended PointChildToParent to TRect}
function RectChildToParent(ARect: TRect; AChild: TControl; AParent: TWinControl = nil): TRect;
{union TControl.ClientToScreen from Point to Rect}
function RectClientToScreen(AClientRect: TRect; AControl: TControl): TRect;
{union TControl.ScreenToClient from Point to Rect}
function RectScreenToClient(AScreenRect: TRect; AControl: TControl): TRect;
{converts (maps) a rect from a control client area coordinates to another control client area coordinates}
function MapControlRect(ACtlFrom, ACtlTo: TControl; AClientRect: TRect):  TRect;
{converts (maps) a point from a control client area coordinates to another control client area coordinates}
function MapControlPoint(ACtlFrom, ACtlTo: TControl; APoint: TPoint):  TPoint;
{converts (maps) a rect from a coordinate space relative to one window to a coordinate space relative to another window}
function MapWindowRect(hWndFrom, hWndTo: HWND; ARect: TRect): TRect;
{specifies the size (in pixels) of a control's windows area.
 TControl.ClientRect likely, but including client area and noclient area.}
function GetControlRect(AControl: TControl): TRect;
{specifies the screen coordinates of a control's windows area, the size of rect is equaled to GetControlRect.}
function GetControlScreenRect(AControl: TControl): TRect;

{retrieves a handle of a device context (DC) for the client area of the control}
function GetControlDC(Control: TControl; var WindowHandle: HWND): HDC;
{retrieves the device context (DC) for the entire window,
 including title bar, menus, and scroll bars, that client area and noclient area
 }
function GetControlWindowDC(Control: TControl; var WindowHandle: HWND): HDC;
{retrieves the max rectangle of combined with all child controls of AContainer,
 expressed in the coordinate system of the AContainer control.
 equal to AContainer.BoundsRect if AContainer.ControlCount = 0.
}
function GetContainerMaxBoundsRect(AContainer: TWinControl): TRect;
{retrieves the top handle of a specified control,function GetParentForm likely. }
function GetParentWindow(ASender: TWinControl): HWnd;

function BeginClipRect(DC: HDC; AClipRect: TRect; fnMode: Integer = RGN_COPY): Integer;
function EndClipRect(DC: HDC): Integer;

function GetTopOwner(aCmp: TComponent): TComponent;
function GetTopForm(aCmp: TComponent): TCustomForm;
function IsOwnedComponent(aCmp, aOwner: TComponent): Boolean;
function IsChildWindow(const AChild, AParent: THandle): Boolean;

// This function generates a unique name for a component inside the list of all
// owner components.
// The name is generated in the login <OwnerName>_<AComponentName><Nr> or
// <OwnerName>_<ACOmponent.ClassName><Nr> when the AComponentName parameter
// is not defined. The number will be increased until the name is unique.
function GenerateUniqueComponentName(AOwner, AComponent: TComponent; const
    AComponentName: string = ''): string;

function ReplaceImageListReference(This: TComponent; NewReference: TCustomImageList;
  var VarReference: TCustomImageList; ChangeLink: TChangeLink): Boolean;
function ReplaceComponentReference(This, NewReference: TComponent; var VarReference: TComponent): Boolean;



{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Consts,
  {$IFDEF MSWINDOWS}
  CommCtrl, ShlObj, ActiveX,
  {$ENDIF MSWINDOWS}
  Math, Contnrs,
  jpeg,
  JclFileUtils,
  JvConsts, JvResources;

{$R JvConsts.res}

const
  {$IFDEF MSWINDOWS}
  RC_ControlRegistry = 'Control Panel\Desktop';
  RC_WallPaperStyle = 'WallpaperStyle';
  RC_WallpaperRegistry = 'Wallpaper';
  RC_TileWallpaper = 'TileWallpaper';
  RC_RunCpl = 'rundll32.exe shell32,Control_RunDLL ';
  {$ENDIF MSWINDOWS}

function GetAppHandle: THandle;
begin
  Result := Application.Handle;
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


function IconToBitmap(Ico: HICON): TBitmap;
var
  Pic: TPicture;
begin
  Pic := TPicture.Create;
  try
    Pic.Icon.Handle := Ico;
    Result := TBitmap.Create;
    Result.Height := Pic.Icon.Height;
    Result.Width := Pic.Icon.Width;
    Result.Canvas.Draw(0, 0, Pic.Icon);
  finally
    Pic.Free;
  end;
end;

function IconToBitmap2(Ico: HICON; Size: Integer = 32;
  TransparentColor: TColor = clNone): TBitmap;
begin
  // (p3) this seems to generate "better" bitmaps...
  with TImageList.CreateSize(Size, Size) do
  try
    Masked := True;
    BkColor := TransparentColor;
    ImageList_AddIcon(Handle, Ico);
    Result := TBitmap.Create;
    Result.PixelFormat := pf24bit;
    if TransparentColor <> clNone then
      Result.TransparentColor := TransparentColor;
    Result.Transparent := TransparentColor <> clNone;
    GetBitmap(0, Result);
  finally
    Free;
  end;
end;

function IconToBitmap3(Ico: HICON; Size: Integer = 32;
  TransparentColor: TColor = clNone): TBitmap;
var
  Icon: TIcon;
  Tmp: TBitmap;
begin
  Icon := TIcon.Create;
  Tmp := TBitmap.Create;
  try
    Icon.Handle := CopyIcon(Ico);
    Result := TBitmap.Create;
    Result.Width := Icon.Width;
    Result.Height := Icon.Height;
    Result.PixelFormat := pf24bit;
    // fill the bitmap with the transparent color
    Result.Canvas.Brush.Color := TransparentColor;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
    Result.Canvas.Draw(0, 0, Icon);
    Result.TransparentColor := TransparentColor;
    Tmp.Assign(Result);
    //    Result.Width := Size;
    //    Result.Height := Size;
    Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), Tmp);
    Result.Transparent := True;
  finally
    Icon.Free;
    Tmp.Free;
  end;
end;

procedure RGBToHSV(R, G, B: Integer; var H, S, V: Integer);
var
  Delta: Integer;
  Min, Max: Integer;

  function GetMax(I, J, K: Integer): Integer;
  begin
    if J > I then
      I := J;
    if K > I then
      I := K;
    Result := I;
  end;

  function GetMin(I, J, K: Integer): Integer;
  begin
    if J < I then
      I := J;
    if K < I then
      I := K;
    Result := I;
  end;

begin
  Min := GetMin(R, G, B);
  Max := GetMax(R, G, B);
  V := Max;
  Delta := Max - Min;
  if Max = 0 then
    S := 0
  else
    S := (255 * Delta) div Max;
  if S = 0 then
    H := 0
  else
  begin
    if R = Max then
      H := (60 * (G - B)) div Delta
    else
    if G = Max then
      H := 120 + (60 * (B - R)) div Delta
    else
      H := 240 + (60 * (R - G)) div Delta;
    if H < 0 then
      H := H + 360;
  end;
end;

{$IFDEF MSWINDOWS}

procedure SetWallpaper(const Path: string);
begin
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE);
end;

procedure SetWallpaper(const Path: string; Style: TJvWallpaperStyle);
begin
  with TRegistry.Create do
  begin
    try
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
    finally
      Free;
    end;
  end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

{$ENDIF MSWINDOWS}

type
  TGetXBitmapMode =(gxRed, gxGreen, gxBlue, gxHue, gxSaturation, gxValue);

procedure GetXBitmap(var Dest: TBitmap; const Source: TBitmap; Mode: TGetXBitmapMode);
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
    case Mode of
      gxRed:
        for I := Dest.Width - 1 downto 0 do
        begin
          Line[I].rgbGreen := 0;
          Line[I].rgbBlue := 0;
        end;
      gxGreen:
        for I := Dest.Width - 1 downto 0 do
        begin
          Line[I].rgbRed := 0;
          Line[I].rgbBlue := 0;
        end;
      gxBlue:
        for I := Dest.Width - 1 downto 0 do
        begin
          Line[I].rgbRed := 0;
          Line[I].rgbGreen := 0;
        end;
      gxHue:
        for I := Dest.Width - 1 downto 0 do
        begin
          RGBToHSV(Line[I].rgbRed, Line[I].rgbGreen, Line[I].rgbBlue, H, S, V);
          Line[I].rgbRed := H;
          Line[I].rgbGreen := H;
          Line[I].rgbBlue := H;
        end;
      gxSaturation:
        for I := Dest.Width - 1 downto 0 do
        begin
          RGBToHSV(Line[I].rgbRed, Line[I].rgbGreen, Line[I].rgbBlue, H, S, V);
          Line[I].rgbRed := S;
          Line[I].rgbGreen := S;
          Line[I].rgbBlue := S;
        end;
      gxValue:
        for I := Dest.Width - 1 downto 0 do
        begin
          RGBToHSV(Line[I].rgbRed, Line[I].rgbGreen, Line[I].rgbBlue, H, S, V);
          Line[I].rgbRed := V;
          Line[I].rgbGreen := V;
          Line[I].rgbBlue := V;
        end;
    end;
  end;
  Dest.PixelFormat := Source.PixelFormat;
end;

procedure GetRBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  GetXBitmap(Dest, Source, gxRed);
end;

procedure GetBBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  GetXBitmap(Dest, Source, gxBlue);
end;

procedure GetGBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  GetXBitmap(Dest, Source, gxGreen);
end;

procedure GetMonochromeBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  if not Assigned(Dest) then
    Dest := TBitmap.Create;
  Dest.Assign(Source);
  Dest.Monochrome := True;
end;

procedure GetHueBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  GetXBitmap(Dest, Source, gxHue);
end;

procedure GetSaturationBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  GetXBitmap(Dest, Source, gxSaturation);
end;

procedure GetValueBitmap(var Dest: TBitmap; const Source: TBitmap);
begin
  GetXBitmap(Dest, Source, gxValue);
end;


{ (rb) Duplicate of JvAppUtils.AppTaskbarIcons }

procedure HideFormCaption(FormHandle: THandle; Hide: Boolean);
begin
  if Hide then
    SetWindowLong(FormHandle, GWL_STYLE,
      GetWindowLong(FormHandle, GWL_STYLE) and not WS_CAPTION)
  else
    SetWindowLong(FormHandle, GWL_STYLE,
      GetWindowLong(FormHandle, GWL_STYLE) or WS_CAPTION);
end;


procedure LaunchCpl(const FileName: string);
begin
  // rundll32.exe shell32,Control_RunDLL ';
  RunDLL32('shell32.dll', 'Control_RunDLL', FileName, True);
  //  WinExec(PChar(RC_RunCpl + FileName), SW_SHOWNORMAL);
end;

procedure ShowSafeRemovalDialog;
begin
  LaunchCpl('HOTPLUG.DLL');
end;

const
  {$EXTERNALSYM WM_CPL_LAUNCH}
  WM_CPL_LAUNCH = (WM_USER + 1000);
  {$EXTERNALSYM WM_CPL_LAUNCHED}
  WM_CPL_LAUNCHED = (WM_USER + 1001);

  { (p3) just define enough to make the Cpl unnecessary for us (for the benefit of PE users) }
  cCplAddress = 'CPlApplet';
  CPL_INIT = 1;
  {$EXTERNALSYM CPL_INIT}
  CPL_GETCOUNT = 2;
  {$EXTERNALSYM CPL_GETCOUNT}
  CPL_INQUIRE = 3;
  {$EXTERNALSYM CPL_INQUIRE}
  CPL_EXIT = 7;
  {$EXTERNALSYM CPL_EXIT}
  CPL_NEWINQUIRE = 8;
  {$EXTERNALSYM CPL_NEWINQUIRE}

type
  TCPLApplet = function(hwndCPl: THandle; uMsg: UINT;
    lParam1, lParam2: LPARAM): Longint; stdcall;

  TCPLInfo = record
    idIcon: Integer;
    idName: Integer;
    idInfo: Integer;
    lData: LONG_PTR;
  end;

  TNewCPLInfoA = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwHelpContext: DWORD;
    lData: LONG_PTR;
    HICON: HICON;
    szName: array [0..31] of AnsiChar;
    szInfo: array [0..63] of AnsiChar;
    szHelpFile: array [0..127] of AnsiChar;
  end;
  TNewCPLInfoW = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwHelpContext: DWORD;
    lData: LONG_PTR;
    HICON: HICON;
    szName: array [0..31] of WideChar;
    szInfo: array [0..63] of WideChar;
    szHelpFile: array [0..127] of WideChar;
  end;

function GetControlPanelApplet(const AFileName: string; Strings: TStrings;
  Images: TCustomImageList = nil): Boolean;
var
  hLib: HMODULE; // Library Handle to *.cpl file
  hIco: HICON;
  CplCall: TCPLApplet; // Pointer to CPlApplet() function
  I: Longint;
  TmpCount, Count: Longint;
  S: WideString;
  // the three types of information that can be returned
  CPLInfo: TCPLInfo;
  InfoW: TNewCPLInfoW;
  InfoA: TNewCPLInfoA;
  HWND: THandle;
begin
  Result := False;
  hLib := SafeLoadLibrary(AFileName);
  if hLib = 0 then
    Exit;
  HWND := GetForegroundWindow;
  TmpCount := Strings.Count;
  Strings.BeginUpdate;
  try
    @CplCall := GetProcAddress(hLib, cCplAddress);
    if not Assigned(CplCall) then
      Exit;
    CplCall(HWND, CPL_INIT, 0, 0); // Init the *.cpl file
    try
      Count := CplCall(HWND, CPL_GETCOUNT, 0, 0);
      for I := 0 to Count - 1 do
      begin
        FillChar(InfoW, SizeOf(InfoW), 0);
        FillChar(InfoA, SizeOf(InfoA), 0);
        FillChar(CPLInfo, SizeOf(CPLInfo), 0);
        S := '';
        CplCall(HWND, CPL_NEWINQUIRE, I, LPARAM(@InfoW));
        if InfoW.dwSize = SizeOf(InfoW) then
        begin
          hIco := InfoW.HICON;
          S := WideString(InfoW.szName);
        end
        else
        begin
          if InfoW.dwSize = SizeOf(InfoA) then
          begin
            Move(InfoW, InfoA, SizeOf(InfoA));
            hIco := CopyIcon(InfoA.HICON);
            S := string(InfoA.szName);
          end
          else
          begin
            CplCall(HWND, CPL_INQUIRE, I, LPARAM(@CPLInfo));
            LoadStringA(hLib, CPLInfo.idName, InfoA.szName,
              SizeOf(InfoA.szName));
            hIco := LoadImage(hLib, PChar(CPLInfo.idIcon), IMAGE_ICON, 16, 16,
              LR_DEFAULTCOLOR);
            S := string(InfoA.szName);
          end;
        end;
        if S <> '' then
        begin
          S := Format('%s=%s,@%d', [S, AFileName, I]);
          if Images <> nil then
          begin
            hIco := CopyIcon(hIco);
            ImageList_AddIcon(Images.Handle, hIco);
            Strings.AddObject(S, TObject(Images.Count - 1));
          end
          else
            Strings.AddObject(S, IconToBitmap2(hIco, 16, clMenu));
          // (p3) not sure this is really needed...
          // DestroyIcon(hIco);
        end;
      end;
      Result := TmpCount < Strings.Count;
    finally
      CplCall(HWND, CPL_EXIT, 0, 0);
    end;
  finally
    FreeLibrary(hLib);
    Strings.EndUpdate;
  end;
end;

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings;
  Images: TCustomImageList = nil): Boolean;
var
  H: THandle;
  F: TSearchRec;
begin
  Result := False;
  if Strings = nil then
    Exit;
  H := FindFirst(IncludeTrailingPathDelimiter(APath) + AMask, faAnyFile, F);
  if Images <> nil then
  begin
    Images.Clear;
    Images.BkColor := clMenu;
  end;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    while H = 0 do
    begin
      if F.Attr and faDirectory = 0 then
        //    if (F.Name <> '.') and (F.Name <> '..') then
        GetControlPanelApplet(APath + F.Name, Strings, Images);
      H := FindNext(F);
    end;
    SysUtils.FindClose(F);
    Result := Strings.Count > 0;
  finally
    Strings.EndUpdate;
  end;
end;

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
  if HasFlag(State, LVIS_ACTIVATING) then
    Include(Result, isActivating);
  if HasFlag(State, LVIS_CUT) then
    Include(Result, isCut);
  if HasFlag(State, LVIS_DROPHILITED) then
    Include(Result, isDropHilited);
  if HasFlag(State, LVIS_FOCUSED) then
    Include(Result, IsFocused);
  if HasFlag(State, LVIS_SELECTED) then
    Include(Result, isSelected);
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
  TAccessWinControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
  ViewPortOrg: TPoint;
begin
  if (Control = nil) or (Control.Parent = nil) then
    Exit;
  Count := Control.Parent.ControlCount;
  DC := Dest.Handle;
  Control.Parent.ControlState := Control.Parent.ControlState + [csPaintCopy];
  try
    // The view port may already be set. This is especially true when
    // a control using CopyParentImage is placed inside a control that
    // calls it as well. Best example is a TJvSpeeButton in a TJvPanel,
    // both with Transparent set to True (discovered while working on
    // Mantis 3624)
    GetViewPortOrgEx(DC, ViewPortOrg);

    SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);

    ViewPortOrg.X := ViewPortOrg.X-Control.Left;
    ViewPortOrg.Y := ViewPortOrg.Y-Control.Top;

    // Copy parent control image
    SaveIndex := SaveDC(DC);
    try
      SetViewPortOrgEx(DC, ViewPortOrg.X, ViewPortOrg.Y, nil);
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
        Control.Parent.ClientHeight);
      TAccessWinControl(Control.Parent).Perform(WM_ERASEBKGND, WPARAM(DC), 0);
      TAccessWinControl(Control.Parent).PaintWindow(DC);
    finally
      RestoreDC(DC, SaveIndex);
    end;

    // Copy images of control's siblings
    // Note: while working on Mantis 3624 it was decided that there was no
    // real reason to limit this to controls derived from TGraphicControl.
    for I := 0 to Count - 1 do
    begin
      if Control.Parent.Controls[I] = Control then
        Break
      else
      if (Control.Parent.Controls[I] <> nil) then
      begin
        with Control.Parent.Controls[I] do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if IntersectRect(R, SelfR, CtlR) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SetViewPortOrgEx(DC, Left + ViewPortOrg.X, Top + ViewPortOrg.Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, DC, 0);
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;
        end;
      end;
    end;
  finally
    Control.Parent.ControlState := Control.Parent.ControlState - [csPaintCopy];
  end;
end;

function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
begin
  Result := TBitmap.Create;
  try
    if Module <> 0 then
    begin
      if DWORD_PTR(ResID) <= $FFFF then
        Result.LoadFromResourceID(Module, INT_PTR(ResID))
      else
        Result.LoadFromResourceName(Module, StrPas(ResID));
    end
    else
    begin
      Result.Handle := LoadBitmap(Module, ResID);
      if Result.Handle = 0 then
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
    Dest.Width := CellWidth;
    Dest.Height := CellHeight;
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
  SrcDC: HDC; SrcX, SrcY, SrcW, Srch: Integer; Palette: HPALETTE;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBITMAP;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBITMAP;
  MemDC, BackDC, ObjectDC, SaveDC: HDC;
  palDst, palMem, palSave, palObj: HPALETTE;
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
  { Select palette }
  palDst := 0;
  palMem := 0;
  palSave := 0;
  palObj := 0;
  if Palette <> 0 then
  begin
    palDst := SelectPalette(DstDC, Palette, True);
    RealizePalette(DstDC);
    palSave := SelectPalette(SaveDC, Palette, False);
    RealizePalette(SaveDC);
    palObj := SelectPalette(ObjectDC, Palette, False);
    RealizePalette(ObjectDC);
    palMem := SelectPalette(MemDC, Palette, True);
    RealizePalette(MemDC);
  end;
  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
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
  { Restore palette }
  if Palette <> 0 then
  begin
    SelectPalette(MemDC, palMem, False);
    SelectPalette(ObjectDC, palObj, False);
    SelectPalette(SaveDC, palSave, False);
    SelectPalette(DstDC, palDst, True);
  end;
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
    StretchBltTransparent(DC, DstX, DstY, DstW, DstH, hdcTemp,
      SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, 0, TransparentColor);
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
  if not Bitmap.Monochrome then
    SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
  CanvasChanging := Bitmap.Canvas.OnChanging;
  Bitmap.Canvas.Lock;
  try
    Bitmap.Canvas.OnChanging := nil;
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
        Bitmap.Palette, TransparentColor);
    end;
  finally
    Bitmap.Canvas.OnChanging := CanvasChanging;
    Bitmap.Canvas.Unlock;
  end;
end;

procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; Bitmap: TBitmap;
  TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
    DstX, DstY, DstW, DstH, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top);
end;

procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
    DstX, DstY, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top);
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
        HandleType := bmDDB;
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
    Result.Height := Bitmap.Height;
    Result.Width := Bitmap.Width;

    R := Bounds(0, 0, Result.Width, Result.Height);

    Result.Canvas.Brush.Color := NewColor;
    Result.Canvas.FillRect(R);
    Result.Canvas.BrushCopy( R, Bitmap, R, Color);
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
      ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
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
  Result.Canvas.Brush.Style := bsSolid;
  Result.Canvas.Brush.Color := Color1;
  Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
  for Y := 0 to 7 do
    for X := 0 to 7 do
      if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
        Result.Canvas.Pixels[X, Y] := Color2; { on even/odd rows }
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
  Result.Handle := LoadIcon(Module, ResID);
  if Result.Handle = 0 then
  begin
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
    Result.Canvas.Brush.Color := BackColor;
    Result.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
    Result.Canvas.Draw(0, 0, Icon);
    Result.TransparentColor := BackColor;
    Result.Transparent := True;
  except
    Result.Free;
    raise;
  end;
end;

function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;

begin
  with TImageList.CreateSize(Bitmap.Width, Bitmap.Height) do
  try
    if TransparentColor = clDefault then
      TransparentColor := Bitmap.TransparentColor;
    AllocBy := 1;
    AddMasked(Bitmap, TransparentColor);
    Result := TIcon.Create;
    try
      GetIcon(0, Result);
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

procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
var
  DC: Windows.HDC;
  R: TRect;
begin
  DC := Windows.GetDC(HWND_DESKTOP);
  try
    R := Rect(RectOrg.X, RectOrg.Y, RectEnd.X, RectEnd.Y);
    Windows.InvertRect(DC, R);
  finally
    Windows.ReleaseDC(HWND_DESKTOP, DC);
  end;
end;

procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
var
  DC: Windows.HDC;
  I: Integer;
begin
  DC := Windows.GetDC(HWND_DESKTOP);
  try
    for I := 1 to Width do
    begin
      Windows.DrawFocusRect(DC, ScreenRect);
      //InflateRect(ScreenRect, -1, -1);
    end;
  finally
    Windows.ReleaseDC(HWND_DESKTOP, DC);
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

function CreateRotatedFont(Font: TFont; Angle: Integer): HFONT;
var
  LogFont: TLogFont;
begin
  FillChar(LogFont, SizeOf(LogFont), 0);
  with LogFont do
  begin
    lfHeight := Font.Height;
    lfWidth := 0;
    lfEscapement := Angle * 10;
    lfOrientation := 0;
    if fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Ord(fsItalic in Font.Style);
    lfUnderline := Ord(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    lfCharSet := Byte(Font.Charset);
    if SameText(Font.Name, 'Default') then
      StrPCopy(lfFaceName, string(DefFontData.Name))
    else
      StrPCopy(lfFaceName, Font.Name);
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_TT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Font.Pitch of
      fpVariable:
        lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed:
        lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LogFont);
end;

function PaletteEntries(Palette: HPALETTE): Integer;
begin
  GetObject(Palette, SizeOf(Integer), @Result);
end;

procedure Delay(MSecs: Int64);
var
  FirstTickCount, Now: Int64;
begin
  FirstTickCount := GetTickCount64;
  repeat
    Application.ProcessMessages;
    { allowing access to other controls, etc. }
    Now := GetTickCount64;
  until (Now - FirstTickCount >= MSecs);
end;

function GetTickCount64: Int64;
var
  QFreq, QCount: Int64;
begin
   Result := GetTickCount;
   if QueryPerformanceFrequency(QFreq) then
   begin
     QueryPerformanceCounter(QCount);
     if QFreq <> 0 then
       Result := (QCount div QFreq) * 1000;
  end;
end;

procedure CenterControl(Control: TControl);
var
  X, Y: Integer;
begin
  X := Control.Left;
  Y := Control.Top;
  if Control is TForm then
  begin
    if (TForm(Control).FormStyle = fsMDIChild) and
      (Application.MainForm <> nil) then
    begin
      X := (Application.MainForm.ClientWidth - Control.Width) div 2;
      Y := (Application.MainForm.ClientHeight - Control.Height) div 2;
    end
    else
    begin
      X := (Screen.Width - Control.Width) div 2;
      Y := (Screen.Height - Control.Height) div 2;
    end;
  end
  else
  if Control.Parent <> nil then
  begin
    Control.Parent.HandleNeeded;
    X := (Control.Parent.ClientWidth - Control.Width) div 2;
    Y := (Control.Parent.ClientHeight - Control.Height) div 2;
  end;
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  Control.SetBounds(X, Y, Control.Width, Control.Height);
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
  AForm.BorderStyle := bsNone;
  AForm.BorderIcons := [];
  AForm.Parent := AControl;
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


{ ShowMDIClientEdge function has been copied from Inprise's FORMS.PAS unit,
  Delphi 4 version }

procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
var
  Style: Longint;
begin
  if ClientHandle <> 0 then
  begin
    Style := GetWindowLong(ClientHandle, GWL_EXSTYLE);
    if ShowEdge then
      if Style and WS_EX_CLIENTEDGE = 0 then
        Style := Style or WS_EX_CLIENTEDGE
      else
        Exit
    else
    if Style and WS_EX_CLIENTEDGE <> 0 then
      Style := Style and not WS_EX_CLIENTEDGE
    else
      Exit;
    SetWindowLong(ClientHandle, GWL_EXSTYLE, Style);
    SetWindowPos(ClientHandle, 0, 0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

{ Shade rectangle }

procedure ShadeRect(DC: HDC; const Rect: TRect);
const
  HatchBits: array [0..7] of Word = ($11, $22, $44, $88, $11, $22, $44, $88);
var
  Bitmap: HBITMAP;
  SaveBrush: HBRUSH;
  SaveTextColor, SaveBkColor: TColorRef;
begin
  Bitmap := CreateBitmap(8, 8, 1, 1, @HatchBits);
  SaveBrush := SelectObject(DC, CreatePatternBrush(Bitmap));
  try
    SaveTextColor := SetTextColor(DC, clWhite);
    SaveBkColor := SetBkColor(DC, clBlack);
    PatBlt(DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, $00A000C9);
    SetBkColor(DC, SaveBkColor);
    SetTextColor(DC, SaveTextColor);
  finally
    DeleteObject(SelectObject(DC, SaveBrush));
    DeleteObject(Bitmap);
  end;
end;

function ScreenWorkArea: TRect;
begin
  {$IFDEF MSWINDOWS}
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
  {$ENDIF MSWINDOWS}
  Result := Bounds(0, 0, Screen.Width, Screen.Height);
end;

{ Standard Windows MessageBox function }

function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(Text), PChar(Caption), Flags);
end;

function MsgDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons;
  HelpCtx: Longint): Word;
begin
  Result := MessageDlg(Msg, AType, AButtons, HelpCtx);
end;

function MsgBox(Handle: THandle; const Caption, Text: string; Flags: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.MessageBox(Handle, PChar(Text), PChar(Caption), Flags);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := MsgBox(Caption, Text, Flags);
  {$ENDIF UNIX}
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
  Canvas.Lock;
  try
    if (StartColor = clNone) and (EndColor = clNone) then
      Exit;
    if not (IsRectEmpty(ARect) and (GetMapMode(Canvas.Handle) = MM_TEXT)) then
    begin
      StartColor := ColorToRGB(StartColor);
      EndColor := ColorToRGB(EndColor);
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
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Mode := pmCopy;
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
    Canvas.Unlock;
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
  GetTextExtentPoint32(Canvas.Handle, Buffer, 52, TSize(Result));
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
  FileName: string;
  RSrc: HRSRC;
  Res: THandle;
  Data: Pointer;
begin
  Result := 0;
  RSrc := FindResource(Instance, ResID, RT_ANICURSOR);
  if RSrc <> 0 then
  begin
    FileName := FileGetTempName('ANI');
    try
      Res := LoadResource(Instance, RSrc);
      try
        Data := LockResource(Res);
        if Data <> nil then
        try
          S := TFileStream.Create(FileName, fmCreate);
          try
            S.WriteBuffer(Data^, SizeOfResource(Instance, RSrc));
          finally
            S.Free;
          end;
          Result := LoadCursorFromFile(PChar(FileName));
        finally
          UnlockResource(Res);
        end;
      finally
        FreeResource(Res);
      end;
    finally
      Windows.DeleteFile(PChar(FileName));
    end;
  end;
end;
{$ENDIF MSWINDOWS}

function GetNextFreeCursorIndex(StartHint: Integer; PreDefined: Boolean): Integer;
begin
  Result := StartHint;
  if PreDefined then
  begin
    if Result >= crSizeAll then
      Result := crSizeAll - 1;
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
  if Handle = 0 then
    Handle := LoadAniCursor(Instance, ResID);
  if Handle = 0 then
    ResourceNotFound(ResID);
  try
    Result := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[Result] := Handle;
  except
    DestroyCursor(Handle);
    raise;
  end;
end;

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
  cOle32DLL = 'ole32.dll';
var
  Handle: HMODULE;
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
    crHand := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crHand] := LoadCursor(HInstance, 'JvHANDCURSOR');
    crDragHand := GetNextFreeCursorIndex(crJVCLFirst, False);
    Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'JvDRAGCURSOR');
  end;
end;

{ Grid drawing }

var
  DrawBitmap: TBitmap = nil;

procedure UsesBitmap;
begin
  if DrawBitmap = nil then
    DrawBitmap := TBitmap.Create;
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

  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  B, R: TRect;
  I, Left: Integer;
begin
  UsesBitmap;
  I := ColorToRGB(ACanvas.Brush.Color);
  if not WordWrap and (Integer(GetNearestColor(ACanvas.Handle, I)) = I) and
    (Pos(Cr, Text) = 0) then
  begin { Use ExtTextOut for solid colors }
    { In BiDi, because we changed the window origin, the text that does not
    change alignment, actually gets its alignment changed. }
    if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1 -
        (ACanvas.TextWidth(Text) shr 1);
    end;
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
  end
  else
  begin { Use FillRect and DrawText for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      { Use offscreen bitmap to eliminate flicker and }
      { brush origin tics in painting / scrolling.    }
      DrawBitmap.Width := Max(DrawBitmap.Width, ARect.Right - ARect.Left);
      DrawBitmap.Height := Max(DrawBitmap.Height, ARect.Bottom - ARect.Top);
      R := Rect(DX, DY, ARect.Right - ARect.Left - 1, ARect.Bottom - ARect.Top - 1);
      B := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

      DrawBitmap.Canvas.Font := ACanvas.Font;
      DrawBitmap.Canvas.Font.Color := ACanvas.Font.Color;
      DrawBitmap.Canvas.Brush := ACanvas.Brush;
      DrawBitmap.Canvas.Brush.Style := bsSolid;
      DrawBitmap.Canvas.FillRect(B);
      SetBkMode(DrawBitmap.Canvas.Handle, Transparent);
      if (ACanvas.CanvasOrientation = coRightToLeft) then
        ChangeBiDiModeAlignment(Alignment);
      DrawText(DrawBitmap.Canvas, Text, Length(Text), R,
      //Windows.DrawText(Handle, PChar(Text), Length(Text), R,
        AlignFlags[Alignment] or RTL[ARightToLeft] or WrapFlags[WordWrap]);

      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
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
      H := Max(1, (ARect.Bottom - ARect.Top - TCustomControlAccessProtected(Control).Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    H := Max(MinOffs, ARect.Bottom - ARect.Top - TCustomControlAccessProtected(Control).Canvas.TextHeight('W'));
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
      H := Max(1, (ARect.Bottom - ARect.Top - TCustomControlAccessProtected(Control).Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    H := Max(MinOffs, ARect.Bottom - ARect.Top - TCustomControlAccessProtected(Control).Canvas.TextHeight('W'));
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

destructor TJvDesktopCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

procedure TJvDesktopCanvas.CreateHandle;
begin
  if FDC = 0 then
    FDC := GetWindowDC(GetDesktopWindow);
  Handle := FDC;
end;

procedure TJvDesktopCanvas.FreeHandle;
begin
  if FDC <> 0 then
  begin
    Handle := 0;
    ReleaseDC(GetDesktopWindow, FDC);
    FDC := 0;
  end;
end;


procedure TJvDesktopCanvas.SetOrigin(X, Y: Integer);
var
  FOrigin: TPoint;
begin
  SetWindowOrgEx(Handle, -X, -Y, @FOrigin);
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
    if PtInRect(Result.BoundsRect, P) then
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
  for I := 0 to Parent.ControlCount - 1 do
    if (Parent.Controls[I] is TRadioButton) and
      (Parent.Controls[I] as TRadioButton).Checked then
    begin
      RB := Parent.Controls[I] as TRadioButton;
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

function CheckTaskWindow(Window: HWND; Data: LPARAM): BOOL; stdcall;
begin
  Result := True;
  if PCheckTaskInfo(Data).FocusWnd = Window then
  begin
    PCheckTaskInfo(Data).Found := True;
    Result := False;
  end;
end;

function IsForegroundTask: Boolean;
var
  Info: TCheckTaskInfo;
begin
  Info.FocusWnd := Windows.GetActiveWindow;
  Info.Found := False;
  EnumThreadWindows(GetCurrentThreadId, @CheckTaskWindow, LPARAM(@Info));
  Result := Info.Found;
end;
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function IsForegroundTask: Boolean;
begin
  Result := Application.Active;
end;
{$ENDIF UNIX}

const
  NoHelp = 0; { for MsgDlg2 }
  MsgDlgCharSet: Integer = DEFAULT_CHARSET;

function MsgDlgDef1(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; UseDefButton: Boolean;
  AHelpContext: Integer; Control: TWinControl): Integer;
const
  ButtonNames: array [TMsgDlgBtn] of string =
  ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help'{$IFDEF RTL200_UP}, 'Close'{$ENDIF RTL200_UP});
var
  P: TPoint;
  I: Integer;
  Btn: TButton;
  StayOnTop: Boolean;
begin
  if AHelpContext <> 0 then
    Buttons := Buttons + [mbHelp];
  StayOnTop := False;
  with CreateMessageDialog(Msg, DlgType, Buttons) do
  try
    Font.Charset := MsgDlgCharSet;
    if (Screen.ActiveForm <> nil) and
      (Screen.ActiveForm.FormStyle = fsStayOnTop) then
    begin
      StayOnTop := True;
      SetWindowTop(Screen.ActiveForm.Handle, False);
    end;
    if ACaption <> '' then
      Caption := ACaption;
    if Control = nil then
    begin
      Left := (Screen.Width - Width) div 2;
      Top := (Screen.Height - Height) div 2;
    end
    else
    begin
      P := Point((Control.Width - Width) div 2,
        (Control.Height - Height) div 2);
      P := Control.ClientToScreen(P);
      Left := P.X;
      Top := P.Y
    end;
    if Left < 0 then
      Left := 0
    else
    if Left > Screen.Width then
      Left := Screen.Width - Width;
    if Top < 0 then
      Top := 0
    else
    if Top > Screen.Height then
      Top := Screen.Height - Height;
    HelpContext := AHelpContext;

    Btn := FindComponent(ButtonNames[DefButton]) as TButton;
    if UseDefButton and (Btn <> nil) then
    begin
      for I := 0 to ComponentCount - 1 do
        if Components[I] is TButton then
          (Components[I] as TButton).Default := False;
      Btn.Default := True;
      ActiveControl := Btn;
    end;
    Btn := FindComponent(ButtonNames[mbIgnore]) as TButton;
    if Btn <> nil then
    begin
      // Btn.Width := Btn.Width * 5 div 4; {To shift the Help button Help [translated] }
    end;
    Result := ShowModal;
  finally
    Free;
    if (Screen.ActiveForm <> nil) and StayOnTop then
      SetWindowTop(Screen.ActiveForm.Handle, True);
  end;
end;

function MsgDlgDef(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpContext: Integer;
  Control: TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, DefButton, True,
    HelpContext, Control);
end;

function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext: Integer;
  Control: TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, mbHelp, False,
    HelpContext, Control);
end;

function MsgYesNo(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Boolean;
begin
  Result := MsgBox(Handle, Caption, Msg, MB_YESNO or Flags) = IDYES;
end;

function MsgRetryCancel(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Boolean;
begin
  Result := MsgBox(Handle, Caption, Msg, MB_RETRYCANCEL or Flags) = IDRETRY;
end;

function MsgAbortRetryIgnore(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Integer;
begin
  Result := MsgBox(Handle, Caption, Msg, MB_ABORTRETRYIGNORE or Flags);
end;

function MsgYesNoCancel(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Integer;
begin
  Result := MsgBox(Handle, Caption, Msg, MB_YESNOCANCEL or Flags);
end;

function MsgOKCancel(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0): Boolean;
begin
  Result := MsgBox(Handle, Caption, Msg, MB_OKCANCEL or Flags) = IDOK;
end;

procedure MsgOK(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
begin
  MsgBox(Handle, Caption, Msg, MB_OK or Flags);
end;

procedure MsgInfo(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
begin
  MsgOK(Handle, Msg, Caption, MB_ICONINFORMATION or Flags);
end;

procedure MsgWarn(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
begin
  MsgOK(Handle, Msg, Caption, MB_ICONWARNING or Flags);
end;

procedure MsgQuestion(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
begin
  MsgOK(Handle, Msg, Caption, MB_ICONQUESTION or Flags);
end;

procedure MsgError(Handle: Integer; const Msg, Caption: string; Flags: DWORD = 0);
begin
  MsgOK(Handle, Msg, Caption, MB_ICONERROR or Flags);
end;

function FindIcon(hInstance: DWORD; const IconName: string): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := (IconName <> '') and
      (FindResourceW(hInstance, PWideChar(WideString(IconName)), PWideChar(RT_GROUP_ICON)) <> 0) or
      (FindResourceW(hInstance, PWideChar(WideString(IconName)), PWideChar(RT_ICON)) <> 0)
  else
    Result := (IconName <> '') and
      (FindResourceA(hInstance, PAnsiChar(AnsiString(IconName)), PAnsiChar(RT_GROUP_ICON)) <> 0) or
      (FindResourceA(hInstance, PAnsiChar(AnsiString(IconName)), PAnsiChar(RT_ICON)) <> 0);
end;

type
  TMsgBoxParamsRec = record
    case Boolean of
      False: (ParamsA: TMsgBoxParamsA);
      True: (ParamsW: TMsgBoxParamsW);
  end;

procedure MsgAbout(Handle: Integer; const Msg, Caption: string; const IcoName: string = 'MAINICON'; Flags: DWORD = MB_OK);
var
  Params: TMsgBoxParamsRec;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Params.ParamsW.hInstance := hInstance;
    Params.ParamsW.cbSize := SizeOf(TMsgBoxParamsW);
    Params.ParamsW.hwndOwner := Handle;
    Params.ParamsW.lpszText := PWideChar(WideString(Msg));
    Params.ParamsW.lpszCaption := PWideChar(WideString(Caption));
    Params.ParamsW.dwStyle := Flags;
    if FindIcon(hInstance, IcoName) then
    begin
      Params.ParamsW.dwStyle := Params.ParamsW.dwStyle or MB_USERICON;
      Params.ParamsW.lpszIcon := PWideChar(WideString(IcoName));
    end
    else
      Params.ParamsW.dwStyle := Params.ParamsW.dwStyle or MB_ICONINFORMATION;
    Params.ParamsW.dwContextHelpId := 0;
    Params.ParamsW.lpfnMsgBoxCallback := nil;
    Params.ParamsW.dwLanguageId := GetUserDefaultLangID;
    MessageBoxIndirectW(Params.ParamsW);
  end
  else
  begin
    Params.ParamsA.hInstance := hInstance;
    Params.ParamsA.cbSize := SizeOf(TMsgBoxParamsA);
    Params.ParamsA.hwndOwner := Handle;
    Params.ParamsA.lpszText := PAnsiChar(AnsiString(Msg));
    Params.ParamsA.lpszCaption := PAnsiChar(AnsiString(Caption));
    Params.ParamsA.dwStyle := Flags;
    if FindIcon(hInstance, IcoName) then
    begin
      Params.ParamsA.dwStyle := Params.ParamsA.dwStyle or MB_USERICON;
      Params.ParamsA.lpszIcon := PAnsiChar(AnsiString(IcoName));
    end
    else
      Params.ParamsA.dwStyle := Params.ParamsA.dwStyle or MB_ICONINFORMATION;
    Params.ParamsA.dwContextHelpId := 0;
    Params.ParamsA.lpfnMsgBoxCallback := nil;
    Params.ParamsA.dwLanguageId := GetUserDefaultLangID;
    MessageBoxIndirectA(Params.ParamsA);
  end;
end;

procedure LoadIcoToImage(ALarge, ASmall: ImgList.TCustomImageList; const NameRes: string);
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  if ALarge <> nil then
  begin
    Ico.Handle := LoadImage(HInstance, PChar(NameRes), IMAGE_ICON, 32, 32, 0);
    ALarge.AddIcon(Ico);
  end;
  if ASmall <> nil then
  begin
    Ico.Handle := LoadImage(HInstance, PChar(NameRes), IMAGE_ICON, 16, 16, 0);
    ASmall.AddIcon(Ico);
  end;
  Ico.Free;
end;

function DualInputQuery(const ACaption, Prompt1, Prompt2: string;
  var AValue1, AValue2: string; PasswordChar: Char = #0): Boolean;
var
  AForm: TForm;
  ALabel1, ALabel2: TLabel;
  AEdit1, AEdit2: TEdit;
  ASize, I: Integer;
begin
  Result := False;
  AForm := CreateMessageDialog(Prompt1, mtCustom, [mbOK, mbCancel]);
  ASize := 0;
  if AForm <> nil then
  try
    AForm.Caption := ACaption;
    ALabel1 := AForm.FindComponent('Message') as TLabel;
    for I := 0 to AForm.ControlCount - 1 do
      if AForm.Controls[I] is TButton then
        TButton(AForm.Controls[I]).Anchors := [akRight, akBottom];
    if ALabel1 <> nil then
    begin
      AEdit1 := TEdit.Create(AForm);
      AEdit1.Left := ALabel1.Left;
      AEdit1.Width := AForm.ClientWidth - AEdit1.Left * 2;
      AEdit1.Top := ALabel1.Top + ALabel1.Height + 2;
      AEdit1.Parent := AForm;
      AEdit1.Anchors := [akLeft, akTop, akRight];
      AEdit1.Text := AValue1;
      ALabel1.Caption := Prompt1;
      ALabel1.FocusControl := AEdit1;
      Inc(ASize, AEdit1.Height + 2);

      ALabel2 := TLabel.Create(AForm);
      ALabel2.Left := ALabel1.Left;
      ALabel2.Top := AEdit1.Top + AEdit1.Height + 7;
      ALabel2.Caption := Prompt2;
      ALabel2.Parent := AForm;
      Inc(ASize, ALabel2.Height + 7);

      AEdit2 := TEdit.Create(AForm);
      AEdit2.Left := ALabel1.Left;
      AEdit2.Width := AForm.ClientWidth - AEdit2.Left * 2;
      AEdit2.Top := ALabel2.Top + ALabel2.Height + 2;
      AEdit2.Parent := AForm;
      AEdit2.Anchors := [akLeft, akTop, akRight];
      AEdit2.Text := AValue1;
      if PasswordChar <> #0 then
        AEdit2.PasswordChar := PasswordChar;
      ALabel2.FocusControl := AEdit2;

      Inc(ASize, AEdit2.Height + 8);
      AForm.ClientHeight := AForm.ClientHeight + ASize;
      AForm.ClientWidth := 320;
      AForm.ActiveControl := AEdit1;
      Result := AForm.ShowModal = mrOk;
      if Result then
      begin
        AValue1 := AEdit1.Text;
        AValue2 := AEdit2.Text;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

function InputQueryPassword(const ACaption, APrompt: string; PasswordChar: Char; var Value: string): Boolean;
var
  AForm: TForm;
  ALabel: TLabel;
  AEdit: TEdit;
  ASize: Integer;
begin
  Result := False;
  AForm := CreateMessageDialog(APrompt, mtCustom, [mbOK, mbCancel]);
  if AForm <> nil then
  try
    AForm.Caption := ACaption;
    ALabel := AForm.FindComponent('Message') as TLabel;
    for ASize := 0 to AForm.ControlCount - 1 do
      if AForm.Controls[ASize] is TButton then
        TButton(AForm.Controls[ASize]).Anchors := [akRight, akBottom];
    ASize := 0;
    if ALabel <> nil then
    begin
      AEdit := TEdit.Create(AForm);
      AEdit.Left := ALabel.Left;
      AEdit.Width := AForm.ClientWidth - AEdit.Left * 2;
      AEdit.Top := ALabel.Top + ALabel.Height + 2;
      AEdit.Parent := AForm;
      AEdit.Anchors := [akLeft, akTop, akRight];
      AEdit.Text := Value;
      AEdit.PasswordChar := PasswordChar;
      ALabel.Caption := APrompt;
      ALabel.FocusControl := AEdit;
      Inc(ASize, AEdit.Height + 2);

      AForm.ClientHeight := AForm.ClientHeight + ASize;
      AForm.ClientWidth := 320;
      AForm.ActiveControl := AEdit;
      Result := AForm.ShowModal = mrOk;
      if Result then
        Value := AEdit.Text;
    end;
  finally
    AForm.Free;
  end;
end;


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

function ResolveLink(const HWND: THandle; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
var
  psl: IShellLink;
  WLinkFile: array [0..MAX_PATH] of WideChar;
  wfd: TWin32FindData;
  ppf: IPersistFile;
  wnd: Windows.HWND;
begin
  wnd := HWND;
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

procedure TmrProc(hwnd: THandle; uMsg: Integer; idEvent: Integer; dwTime: Integer); stdcall;
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

function GetFirstParentForm(Control: TControl): TCustomForm;
begin
  while not (Control is TCustomForm) and (Control.Parent <> nil) do
    Control := Control.Parent;
  if Control is TCustomForm then
    Result := TCustomForm(Control) else
    Result := nil;
end;

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
        // GetParentForm will not stop at the first TCustomForm it finds.
        // Starting with Delphi 2005, we can pass False as the second parameter
        // to stop at the FIRST parent that is a TCustomForm, but this is not
        // available in earlier versions of Delphi. Hence the creation and
        // use of GetFirstParentForm.
        // This is required to fix Mantis 3785. Indeed with GetParentForm, the
        // returned form would be the top most form.
        // Say, you have a control in Form2, with an instance of Form2 docked
        // in Form1. When loading, F would Form1, because the parent chain
        // is completely set. But when destroying, the parent chain would be
        // already broken, and F would then be Form2, thus returning a different
        // section name than the one returned when loading.
        F := GetFirstParentForm(TControl(Component));
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
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + PathDelim +
      '.' + ExtractFileName(Application.ExeName);
    {$ENDIF UNIX}
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
  if Restore and (Result.WindowState = wsMinimized) then
    Result.WindowState := wsNormal;
  Result.Show;
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

{$HINTS OFF}
type
  TComponentAccessProtected = class(TComponent);
{$HINTS ON}

function CrtResString: string;
begin
//  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN),
//    GetSystemMetrics(SM_CYSCREEN)]);

//! New Resolution Identifier, because old method did not work
//    properly for Multi-screen systems (returned only width/height of current screen)
  Result := Format('(%dx%d)', [Screen.DesktopWidth, Screen.DesktopHeight]);
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
  AppStorage.BeginUpdate;
  try
    AppStorage.DeleteSubTree(AppStorage.ConcatPaths([StorePath, siMDIChild]));
    if MainForm.MDIChildCount > 0 then
    begin
      AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, siMDIChild,
        siListCount]), MainForm.MDIChildCount);
      for I := 0 to MainForm.MDIChildCount - 1 do
        AppStorage.WriteString(AppStorage.ConcatPaths([StorePath, siMDIChild,
          Format(siItem, [I])]), MainForm.MDIChildren[I].ClassName);
    end;
  finally
    AppStorage.EndUpdate;
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
  AppStorage.BeginUpdate;
  try
    StartWait;
    try
      Count := AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath, siMDIChild, siListCount]), 0);
      if Count > 0 then
      begin
        for I := Count - 1 downto 0 do
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
  finally
    AppStorage.EndUpdate;
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
  AppStorage.BeginUpdate;
  try
    Placement.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Form.Handle, @Placement);
    if (Form = Application.MainForm) and AppMinimized then
      Placement.ShowCmd := SW_SHOWMINIMIZED;
    if (Form.FormStyle = fsMDIChild) and (Form.WindowState = wsMinimized) then
      Placement.Flags := Placement.Flags or WPF_SETMINPOSITION;
    if fpState in Options then
      AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, siShowCmd]), Placement.ShowCmd);
    if [fpSize, fpLocation] * Options <> [] then
    begin
      AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, siFlags]), Placement.Flags);
      AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, siPixels]), Screen.PixelsPerInch);
      WritePosStr(AppStorage, AppStorage.ConcatPaths([StorePath, siMinMaxPos]), Format('%d,%d,%d,%d',
        [Placement.ptMinPosition.X, Placement.ptMinPosition.Y, Placement.ptMaxPosition.X, Placement.ptMaxPosition.Y]));
      WritePosStr(AppStorage, AppStorage.ConcatPaths([StorePath, siNormPos]), Format('%d,%d,%d,%d',
        [Placement.rcNormalPosition.Left, Placement.rcNormalPosition.Top, Placement.rcNormalPosition.Right,
         Placement.rcNormalPosition.Bottom]));
    end;
  finally
    AppStorage.EndUpdate;
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
  OriginalShowCmd: UINT;

  procedure ChangePosition(APosition: TPosition);
  begin
    TComponentAccessProtected(Form).SetDesigning(True);
    try
      Form.Position := APosition;
    finally
      TComponentAccessProtected(Form).SetDesigning(False);
    end;
  end;

  function IsOnAnyMonitor(ARect: TRect) : Boolean;
  var
    BottomRight : TPoint;
  begin
    BottomRight := ARect.BottomRight;
    Dec(BottomRight.X);
    Dec(BottomRight.Y);
    Result := (Screen.MonitorFromPoint(ARect.TopLeft, mdNull) <> Nil) and
        (Screen.MonitorFromPoint(BottomRight, mdNull) <> Nil);
  end;

begin
  if Options = [fpActiveControl] then
    Exit;
  AppStorage.BeginUpdate;
  try
    Placement.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Form.Handle, @Placement);
    OriginalShowCmd := Placement.ShowCmd;
    if not IsWindowVisible(Form.Handle) then
      Placement.ShowCmd := SW_HIDE;
    if [fpSize, fpLocation] * Options <> [] then
    begin
      DataFound := False;
      Placement.Flags := AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath, siFlags]), Placement.Flags);
      PosStr := ReadPosStr(AppStorage, AppStorage.ConcatPaths([StorePath, siMinMaxPos]));
      if PosStr <> '' then
      begin
        DataFound := True;
        if fpLocation in Options then
        begin
          Placement.ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
          Placement.ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        end;
        if fpSize in Options then
        begin
          Placement.ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
          Placement.ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
        end;
      end;
      PosStr := ReadPosStr(AppStorage, AppStorage.ConcatPaths([StorePath, siNormPos]));
      if PosStr <> '' then
      begin
        DataFound := True;
        if fpLocation in Options then
        begin
          Placement.rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Form.Left);
          Placement.rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Form.Top);
        end
        else
        begin
          Placement.rcNormalPosition.Left :=  Form.Left;
          Placement.rcNormalPosition.Top :=  Form.Top;
        end;
        if fpSize in Options then
        begin
          Placement.rcNormalPosition.Right := Placement.rcNormalPosition.Left +
              StrToIntDef(ExtractWord(3, PosStr, Delims), Form.Width)-
              StrToIntDef(ExtractWord(1, PosStr, Delims), Form.Left);
          Placement.rcNormalPosition.Bottom := Placement.rcNormalPosition.Top +
              StrToIntDef(ExtractWord(4, PosStr, Delims), Form.Height)-
              StrToIntDef(ExtractWord(2, PosStr, Delims), Form.Top);
        end
        else
          if fpLocation in Options then
          begin
            Placement.rcNormalPosition.Right := Placement.rcNormalPosition.Left + Form.Width;
            Placement.rcNormalPosition.Bottom := Placement.rcNormalPosition.Top + Form.Height;
          end;
      end;
      DataFound := DataFound and (Screen.PixelsPerInch = AppStorage.ReadInteger(
        AppStorage.ConcatPaths([StorePath, siPixels]), Screen.PixelsPerInch));
      if DataFound then
      begin
        if (Placement.rcNormalPosition.Right > Placement.rcNormalPosition.Left) and
           IsOnAnyMonitor(Placement.rcNormalPosition) then
        begin
          if not (csDesigning in Form.ComponentState) then
          begin
            if (fpSize in Options) and (fpLocation in Options) then
              ChangePosition(poDesigned)
            else
            if fpSize in Options then
            begin
              if Form.Position = poDefault then
                ChangePosition(poDefaultPosOnly);
            end
            else
            if fpLocation in Options then // obsolete but better to read
              if Form.Position = poDefault then
                ChangePosition(poDefaultSizeOnly)
              else
              if Form.Position <> poDesigned then
                ChangePosition(poDesigned);
          end;
          SetWindowPlacement(Form.Handle, @Placement);
        end;
      end;
    end;
    if fpState in Options then
    begin
      WinState := wsNormal;
      { default maximize MDI main form }
      if ((Application.MainForm = Form) or
        (Application.MainForm = nil)) and ((Form.FormStyle = fsMDIForm) or
        ((Form.FormStyle = fsNormal) and (Form.Position = poDefault))) then
        WinState := wsMaximized;
      Placement.ShowCmd := AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath, siShowCmd]), OriginalShowCmd);
      case Placement.ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE:
          WinState := wsMaximized;
      end;
      if (WinState = wsMinimized) and ((Form = Application.MainForm) or
        (Application.MainForm = nil)) then
      begin
        TWindowState(Pointer(@Form.WindowState)^) := wsNormal;
        PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
      if Form.FormStyle in [fsMDIChild, fsMDIForm] then
        TWindowState(Pointer(@Form.WindowState)^) := WinState
      else
        Form.WindowState := WinState;
    end;
    Form.Update;
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  I: Longint;
begin
  AppStorage.BeginUpdate;
  try
    for I := 0 to TDrawGrid(Grid).ColCount - 1 do
      AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, Format(siItem, [I])]),
        TDrawGrid(Grid).ColWidths[I]);
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  I: Longint;
begin
  AppStorage.BeginUpdate;
  try
    for I := 0 to TDrawGrid(Grid).ColCount - 1 do
      TDrawGrid(Grid).ColWidths[I] :=
        AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath,
          Format(siItem, [I])]), TDrawGrid(Grid).ColWidths[I]);
  finally
    AppStorage.EndUpdate;
  end;
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


procedure AppBroadcast(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    SendMessage(Screen.Forms[I].Handle, Msg, wParam, lParam);
end;

procedure AppTaskbarIcons(AppOnly: Boolean);
var
  Style: Longint;
begin
  Style := GetWindowLong(Application.Handle, GWL_STYLE);
  if AppOnly then
    Style := Style or WS_CAPTION
  else
    Style := Style and not WS_CAPTION;
  SetWindowLong(Application.Handle, GWL_STYLE, Style);
  if AppOnly then
    SwitchToWindow(Application.Handle, False);
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
    pf4bit:
      Result := 16;
    pf8bit:
      Result := 256;
  else
    Result := 0;
  end;
end;



function ScreenPixelFormat: TPixelFormat;
var
  DC: HDC;
begin
  DC := CreateIC('DISPLAY', nil, nil, nil);
  try
    case GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL) of
      1:
        Result := pf1bit;
      4:
        Result := pf4bit;
      8:
        Result := pf8bit;
      15:
        Result := pf15bit;
      16:
        Result := pf16bit;
      24:
        Result := pf24bit;
      32:
        Result := pf32bit;
    else
      Result := pfDevice;
    end;
  finally
    DeleteDC(DC);
  end;
end;

function ScreenColorCount: Integer;
begin
  Result := PixelFormatToColors(ScreenPixelFormat);
end;


function GetWorkareaRect(Monitor: TMonitor): TRect;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  GetMonitorInfo(Monitor.Handle, @MonInfo);
  Result := MonInfo.rcWork;
end;

function FindMonitor(Handle: HMONITOR): TMonitor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[I].Handle = Handle then
    begin
      Result := Screen.Monitors[I];
      Break;
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
  TQColorList = array [0..{$IFDEF RTL230_UP}MaxInt div 16{$ELSE}MaxListSize{$ENDIF RTL230_UP} - 1] of PQColor;

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

procedure PInsert(ColorList: PQColorList;
  Number: Integer; SortRGBAxis: Integer);
var
  Q1, Q2: PQColor;
  I, J: Integer;
  Temp: PQColor;
begin
  for I := 1 to Number - 1 do
  begin
    Temp := ColorList[I];
    J := I - 1;
    while J >= 0 do
    begin
      Q1 := Temp;
      Q2 := ColorList[J];
      if Q1.RGB[SortRGBAxis] - Q2.RGB[SortRGBAxis] > 0 then
        Break;
      ColorList[J + 1] := ColorList[J];
      Dec(J);
    end;
    ColorList[J + 1] := Temp;
  end;
end;

procedure PSort(ColorList: PQColorList;
  Number: Integer; SortRGBAxis: Integer);
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
  Part := ColorList[Number div 2];
  I := -1;
  J := Number;
  repeat
    repeat
      Inc(I);
      Q1 := ColorList[I];
      Q2 := Part;
      N := Q1.RGB[SortRGBAxis] - Q2.RGB[SortRGBAxis];
    until N >= 0;
    repeat
      Dec(J);
      Q1 := ColorList[J];
      Q2 := Part;
      N := Q1.RGB[SortRGBAxis] - Q2.RGB[SortRGBAxis];
    until N <= 0;
    if I >= J then
      Break;
    Temp := ColorList[I];
    ColorList[I] := ColorList[J];
    ColorList[J] := Temp;
  until False;
  Nr := Number - I;
  if I < Number div 2 then
  begin
    PSort(ColorList, I, SortRGBAxis);
    PSort(PQColorList(@ColorList[I]), Nr, SortRGBAxis);
  end
  else
  begin
    PSort(PQColorList(@ColorList[I]), Nr, SortRGBAxis);
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
        if (NewColorSubdiv[I].RGBWidth[J] > MaxSize) and
          (NewColorSubdiv[I].NumEntries > 1) then
        begin
          MaxSize := NewColorSubdiv[I].RGBWidth[J];
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
    QuantizedColor := NewColorSubdiv[Index].QuantizedColors;
    while (J < NewColorSubdiv[Index].NumEntries) and
      (QuantizedColor <> nil) do
    begin
      SortArray[J] := QuantizedColor;
      Inc(J);
      QuantizedColor := QuantizedColor.PNext;
    end;
    PSort(SortArray, NewColorSubdiv[Index].NumEntries, SortRGBAxis);
    for J := 0 to NewColorSubdiv[Index].NumEntries - 2 do
      SortArray[J].PNext := SortArray[J + 1];
    SortArray[NewColorSubdiv[Index].NumEntries - 1].PNext := nil;
    NewColorSubdiv[Index].QuantizedColors := SortArray[0];
    QuantizedColor := SortArray[0];
    Sum := NewColorSubdiv[Index].Count div 2 - QuantizedColor.Count;
    NumEntries := 1;
    Count := QuantizedColor.Count;
    Dec(Sum, QuantizedColor.PNext.Count);
    while (Sum >= 0) and (QuantizedColor.PNext <> nil) and
      (QuantizedColor.PNext.PNext <> nil) do
    begin
      QuantizedColor := QuantizedColor.PNext;
      Inc(NumEntries);
      Inc(Count, QuantizedColor.Count);
      Dec(Sum, QuantizedColor.PNext.Count);
    end;
    MaxColor := (QuantizedColor.RGB[SortRGBAxis]) shl 4;
    MinColor := (QuantizedColor.PNext.RGB[SortRGBAxis]) shl 4;
    NewColorSubdiv[NewColormapSize].QuantizedColors := QuantizedColor.PNext;
    QuantizedColor.PNext := nil;
    NewColorSubdiv[NewColormapSize].Count := Count;
    Dec(NewColorSubdiv[Index].Count, Count);
    NewColorSubdiv[NewColormapSize].NumEntries := NewColorSubdiv[Index].NumEntries - NumEntries;
    NewColorSubdiv[Index].NumEntries := NumEntries;
    for J := 0 to 2 do
    begin
      NewColorSubdiv[NewColormapSize].RGBMin[J] :=
        NewColorSubdiv[Index].RGBMin[J];
      NewColorSubdiv[NewColormapSize].RGBWidth[J] :=
        NewColorSubdiv[Index].RGBWidth[J];
    end;
    NewColorSubdiv[NewColormapSize].RGBWidth[SortRGBAxis] :=
      NewColorSubdiv[NewColormapSize].RGBMin[SortRGBAxis] +
      NewColorSubdiv[NewColormapSize].RGBWidth[SortRGBAxis] -
      MinColor;
    NewColorSubdiv[NewColormapSize].RGBMin[SortRGBAxis] := MinColor;
    NewColorSubdiv[Index].RGBWidth[SortRGBAxis] := MaxColor - NewColorSubdiv[Index].RGBMin[SortRGBAxis];
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
  LineBuffer, Data: PAnsiChar;
  LineWidth: Longint;
  TmpLineWidth, NewLineWidth: Longint;
  I, J: Longint;
  Index: Word;
  NewColormapSize, NumOfEntries: Integer;
  Mems: Longint;
  cRed, cGreen, cBlue: Longint;
  LPSTR, Temp, Tmp: PAnsiChar;
  NewColorSubdiv: PNewColorArray;
  ColorArrayEntries: PQColorArray;
  QuantizedColor: PQColor;
begin
  LineWidth := WidthBytes(Longint(Bmp.biWidth) * Bmp.biBitCount);
  Mems := (Longint(SizeOf(TQColor)) * (MAX_COLORS)) +
    (Longint(SizeOf(TNewColor)) * 256) + LineWidth +
    (Longint(SizeOf(PQColor)) * (MAX_COLORS));
  LPSTR := AllocMem(Mems);
  try
    Temp := AllocMem(Longint(Bmp.biWidth) * Longint(Bmp.biHeight) * SizeOf(Word));
    try
      ColorArrayEntries := PQColorArray(LPSTR);
      NewColorSubdiv := PNewColorArray(LPSTR + Longint(SizeOf(TQColor)) * (MAX_COLORS));
      LineBuffer := LPSTR + (Longint(SizeOf(TQColor)) * (MAX_COLORS))
        +
        (Longint(SizeOf(TNewColor)) * 256);
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
        Move(Pointer(PAnsiChar(gptr) + (Bmp.biHeight - 1 - I) * LineWidth)^, LineBuffer^, LineWidth);
        P := PByteArray(LineBuffer);
        for J := 0 to Bmp.biWidth - 1 do
        begin
          Index := (Longint(P^[2] and $F0) shl 4) +
            Longint(P^[1] and $F0) + (Longint(P^[0] and $F0) shr 4);
          Inc(ColorArrayEntries^[Index].Count);
          Inc(PByte(P), 3);
          PWord(Tmp)^ := Index;
          Inc(Tmp, 2);
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
        LPSTR + Longint(SizeOf(TQColor)) * (MAX_COLORS) + Longint(SizeOf(TNewColor)) * 256 + LineWidth);
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
        LineBuffer := Temp + (Bmp.biHeight - 1 - I) * TmpLineWidth;
        Data := PAnsiChar(Data8) + I * NewLineWidth;
        for J := 0 to Bmp.biWidth - 1 do
        begin
          PByte(Data)^ := ColorArrayEntries^[PWord(LineBuffer)^].NewColorIndex;
          Inc(LineBuffer, 2);
          Inc(Data);
        end;
      end;
    finally
      FreeMem(Temp);
    end;
  finally
    FreeMem(LPSTR);
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
    TruncLineProc(PAnsiChar(Src) + Y * SrcScanline, PAnsiChar(Dest) + Y * DstScanline, Header.biWidth);
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
    Inc(PByte(Src));
    G := TruncIndex06[Byte(Src^)];
    Inc(PByte(Src));
    R := TruncIndex06[Byte(Src^)];
    Inc(PByte(Src), 1);
    PByte(Dest)^ := 6 * (6 * R + G) + B;
    Inc(PByte(Dest));
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
    Inc(PByte(Src));
    G := TruncIndex08[Byte(Src^)];
    Inc(PByte(Src));
    R := TruncIndex07[Byte(Src^)];
    Inc(PByte(Src));
    PByte(Dest)^ := 4 * (8 * R + G) + B;
    Inc(PByte(Dest));
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
      Inc(Src);
      G := Src^;
      Inc(Src);
      R := Src^;
      Inc(Src);
      Dest^ := Byte(Longint(Word(R) * 77 + Word(G) * 150 + Word(B) * 29) shr 8);
      Inc(Dest);
    end;
    Data24 := PAnsiChar(Data24) + SrcScanline;
    Data8 := PAnsiChar(Data8) + DstScanline;
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
      Inc(Src);
      G := Src^;
      Inc(Src);
      R := Src^;
      Inc(Src);
      case ((X + Y) mod 3) of
        0: Dest^ := Byte(R shr 2);
        1: Dest^ := Byte($40 + (G shr 2));
        2: Dest^ := Byte($80 + (B shr 2));
      end;
      Inc(Dest);
    end;
    Data24 := PAnsiChar(Data24) + SrcScanline;
    Data8 := PAnsiChar(Data8) + DstScanline;
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
      Inc(PByte(Data24));
      G := Byte(Data24^) and Gm;
      Inc(PByte(Data24));
      R := Byte(Data24^) and Rm;
      Inc(PByte(Data24));
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
    Inc(PByte(Data24), Step24);
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
      Inc(PByte(Data24));
      G := Byte(Data24^) and Gm;
      Inc(PByte(Data24));
      R := Byte(Data24^) and Rm;
      Inc(PByte(Data24));
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
      Inc(PByte(Data8));
    end;
    Inc(PByte(Data24), Step24);
    Inc(PByte(Data8), Step8);
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
  Src, Dest: PAnsiChar;
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
    Src := PAnsiChar(Data) + Y * Scanline;
    Dest := PAnsiChar(NewData) + Y * NewScanline;
    case Header.biBitCount of
      1:
      begin
        C := 0;
        for X := 0 to Header.biWidth - 1 do
        begin
          if (X and 7) = 0 then
          begin
            C := Byte(Src^);
            Inc(Src);
          end
          else C := C shl 1;
          PByte(Dest)^ := Colors[C shr 7].rgbBlue;
          Inc(Dest);
          PByte(Dest)^ := Colors[C shr 7].rgbGreen;
          Inc(Dest);
          PByte(Dest)^ := Colors[C shr 7].rgbRed;
          Inc(Dest);
        end;
      end;
      4:
      begin
        X := 0;
        while X < Header.biWidth - 1 do
        begin
          C := Byte(Src^);
          Inc(Src);
          PByte(Dest)^ := Colors[C shr 4].rgbBlue;
          Inc(Dest);
          PByte(Dest)^ := Colors[C shr 4].rgbGreen;
          Inc(Dest);
          PByte(Dest)^ := Colors[C shr 4].rgbRed;
          Inc(Dest);
          PByte(Dest)^ := Colors[C and 15].rgbBlue;
          Inc(Dest);
          PByte(Dest)^ := Colors[C and 15].rgbGreen;
          Inc(Dest);
          PByte(Dest)^ := Colors[C and 15].rgbRed;
          Inc(Dest);
          Inc(X, 2);
        end;
        if X < Header.biWidth then
        begin
          C := Byte(Src^);
          PByte(Dest)^ := Colors[C shr 4].rgbBlue;
          Inc(Dest);
          PByte(Dest)^ := Colors[C shr 4].rgbGreen;
          Inc(Dest);
          PByte(Dest)^ := Colors[C shr 4].rgbRed;
          {Inc(Dest);}
        end;
      end;
      8:
      begin
        for X := 0 to Header.biWidth - 1 do
        begin
          C := Byte(Src^);
          Inc(Src);
          PByte(Dest)^ := Colors[C].rgbBlue;
          Inc(Dest);
          PByte(Dest)^ := Colors[C].rgbGreen;
          Inc(Dest);
          PByte(Dest)^ := Colors[C].rgbRed;
          Inc(Dest);
        end;
      end;
    end;
  end;
end;
*)

{ DIB utility routines }

function GetPaletteBitmapFormat(Bitmap: TBitmap): TPixelFormat;
var
  PalSize: Integer;
begin
  Result := pfDevice;
  if Bitmap.Palette <> 0 then
  begin
    GetObject(Bitmap.Palette, SizeOf(Integer), @PalSize);
    if PalSize > 0 then
    begin
      if PalSize <= 2 then
        Result := pf1bit
      else
      if PalSize <= 16 then
        Result := pf4bit
      else
      if PalSize <= 256 then
        Result := pf8bit;
    end;
  end;
end;


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

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  PixelFormat: TPixelFormat);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  { Retrieve the info for the current bitmap, thus with the current bit size/PixelFormat }
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then
    InvalidBitmap
  else
  if (Bytes >= (SizeOf(DS.dsbm) + SizeOf(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(SizeOf(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, SizeOf(BI), 0);
    BI.biSize := SizeOf(BI);
    BI.biWidth := DS.dsbm.bmWidth;
    BI.biHeight := DS.dsbm.bmHeight;
  end;
  case PixelFormat of
    pf1bit:
      BI.biBitCount := 1;
    pf4bit:
      BI.biBitCount := 4;
    pf8bit:
      BI.biBitCount := 8;
    pf24bit:
      BI.biBitCount := 24;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  { Calculate the size of the image with the new bit count; better would be to
    call GetDIBits, see http://support.microsoft.com/default.aspx?scid=kb;EN-US;80080
  }
  BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) *
    Abs(BI.biHeight);
  BI.biClrUsed := 0;
  BI.biClrImportant := 0;
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
  var ImageSize: Longint; BitCount: TPixelFormat);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, BitCount);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl BI.biBitCount);
  ImageSize := BI.biSizeImage;
end;

function GetDInColors(const BI: TBitmapInfoHeader): Integer;
begin
  if (BI.biClrUsed = 0) and (BI.biBitCount <= 8) then
    Result := 1 shl BI.biBitCount
  else
    Result := BI.biClrUsed;
end;

function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
  with TBitmapInfoHeader(BitmapInfo) do
    biHeight := Abs(biHeight);
  OldPal := 0;
  DC := CreateScreenCompatibleDC;
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight,
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;

    TBitmapInfoHeader(BitmapInfo).biClrUsed := GetDInColors(TBitmapInfoHeader(BitmapInfo));
  finally
    if OldPal <> 0 then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

function DIBFromBit(Src: HBITMAP; Pal: HPALETTE; PixelFormat: TPixelFormat;
  var Length: Longint): Pointer;
var
  HeaderSize: Integer;
  ImageSize: Longint;
  FileHeader: PBitmapFileHeader;
  BI: PBitmapInfoHeader;
  Bits: Pointer;
begin
  if Src = 0 then
    InvalidBitmap;
  InternalGetDIBSizes(Src, HeaderSize, ImageSize, PixelFormat);
  Length := SizeOf(TBitmapFileHeader) + HeaderSize + ImageSize;
  Result := GlobalAllocPtr(GMEM_ZEROINIT, Length);
  try
    FileHeader := Result;
    with FileHeader^ do
    begin
      bfType := $4D42;
      bfSize := Length;
      bfOffBits := SizeOf(FileHeader^) + HeaderSize;
    end;
    BI := PBitmapInfoHeader(PAnsiChar(FileHeader) + SizeOf(FileHeader^));
    Bits := Pointer(PAnsiChar(BI) + HeaderSize);
    InternalGetDIB(Src, Pal, BI^, Bits^, PixelFormat);
  except
    GlobalFreePtr(Result);
    raise;
  end;
end;

{ Change bits per pixel in a General Bitmap }

function BitmapToMemoryStream(Bitmap: TBitmap; PixelFormat: TPixelFormat;
  Method: TMappingMethod): TMemoryStream;
var
  FileHeader: PBitmapFileHeader;
  BI, NewBI: PBitmapInfoHeader;
  Bits: Pointer;
  NewPalette: PRGBPalette;
  NewHeaderSize: Integer;
  ImageSize, Length, Len: Longint;
  P, InitData: Pointer;
  ColorCount: Integer;
  SourceBitmapFormat: TPixelFormat;
begin
  Result := nil;
  if Bitmap.Handle = 0 then
    InvalidBitmap;
  SourceBitmapFormat := GetBitmapPixelFormat(Bitmap);
  if (SourceBitmapFormat = PixelFormat) and
    (Method <> mmGrayscale) then
  begin
    Result := TMemoryStream.Create;
    try
      Bitmap.SaveToStream(Result);
      Result.Position := 0;
    except
      Result.Free;
      raise;
    end;
    Exit;
  end;
  case PixelFormat of
    pf1bit, pf4bit, pf24bit:
      begin
        P := DIBFromBit(Bitmap.Handle, Bitmap.Palette, PixelFormat, Length);
        try
          Result := TMemoryStream.Create;
          try
            Result.Write(P^, Length);
            Result.Position := 0;
          except
            Result.Free;
            raise;
          end;
        finally
          GlobalFreePtr(P);
        end;
      end;
    pf8bit:
      begin
        { pf8bit - expand to 24bit first }
        InitData := DIBFromBit(Bitmap.Handle, Bitmap.Palette, pf24bit, Len);
        try
          BI := PBitmapInfoHeader(PAnsiChar(InitData) + SizeOf(TBitmapFileHeader));
          if BI^.biBitCount <> 24 then
            raise EJVCLException.CreateRes(@RsEBitCountNotImplemented);
          Bits := Pointer(PAnsiChar(BI) + SizeOf(TBitmapInfoHeader));
          InternalGetDIBSizes(Bitmap.Handle, NewHeaderSize, ImageSize, PixelFormat);
          Length := SizeOf(TBitmapFileHeader) + NewHeaderSize;
          P := AllocMem(Length);
          try
            NewBI := PBitmapInfoHeader(PAnsiChar(P) + SizeOf(TBitmapFileHeader));
            if NewHeaderSize <= SizeOf(TBitmapInfoHeader) then
              NewPalette := nil
            else
              NewPalette := PRGBPalette(PAnsiChar(NewBI) + SizeOf(TBitmapInfoHeader));
            FileHeader := PBitmapFileHeader(P);
            InitializeBitmapInfoHeader(Bitmap.Handle, NewBI^, PixelFormat);
            if Assigned(NewPalette) then
              case Method of
                mmQuantize:
                  begin
                    ColorCount := 256;
                    Quantize(BI^, Bits, Bits, ColorCount, NewPalette^);
                    NewBI^.biClrImportant := ColorCount;
                  end;
                mmTrunc784:
                  begin
                    TruncPal7R8G4B(NewPalette^);
                    Trunc7R8G4B(BI^, Bits, Bits);
                    NewBI^.biClrImportant := 224;
                  end;
                mmTrunc666:
                  begin
                    TruncPal6R6G6B(NewPalette^);
                    Trunc6R6G6B(BI^, Bits, Bits);
                    NewBI^.biClrImportant := 216;
                  end;
                mmTripel:
                  begin
                    TripelPal(NewPalette^);
                    Tripel(BI^, Bits, Bits);
                  end;
                mmHistogram:
                  begin
                    Histogram(BI^, NewPalette^, Bits, Bits,
                      PixelFormatToColors(PixelFormat), 255, 255, 255);
                  end;
                mmGrayscale:
                  begin
                    GrayPal(NewPalette^);
                    GrayScale(BI^, Bits, Bits);
                  end;
              end;
            with FileHeader^ do
            begin
              bfType := $4D42;
              bfSize := Length;
              bfOffBits := SizeOf(FileHeader^) + NewHeaderSize;
            end;
            Result := TMemoryStream.Create;
            try
              Result.Write(P^, Length);
              Result.Write(Bits^, ImageSize);
              Result.Position := 0;
            except
              Result.Free;
              raise;
            end;
          finally
            FreeMem(P);
          end;
        finally
          GlobalFreePtr(InitData);
        end;
      end
  else
    raise EJVCLException.CreateRes(@RsEPixelFormatNotImplemented)
  end;
end;

function BitmapToMemory(Bitmap: TBitmap; Colors: Integer): TStream;
var
  PixelFormat: TPixelFormat;
begin
  if Colors <= 2 then
    PixelFormat := pf1bit
  else
  if Colors <= 16 then
    PixelFormat := pf4bit
  else
  if Colors <= 256 then
    PixelFormat := pf8bit
  else
    PixelFormat := pf24bit;
  Result := BitmapToMemoryStream(Bitmap, PixelFormat, DefaultMappingMethod);
end;

procedure SaveBitmapToFile(const FileName: string; Bitmap: TBitmap;
  Colors: Integer);
var
  Memory: TStream;
begin
  if Bitmap.Monochrome then
    Colors := 2;
  Memory := BitmapToMemory(Bitmap, Colors);
  try
    TMemoryStream(Memory).SaveToFile(FileName);
  finally
    Memory.Free;
  end;
end;

procedure SetBitmapPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat;
  Method: TMappingMethod);
var
  M: TMemoryStream;
begin
  if (Bitmap.Handle = 0) or ((GetBitmapPixelFormat(Bitmap) = PixelFormat) and (Method <> mmGrayscale)) then
    Exit;
  M := BitmapToMemoryStream(Bitmap, PixelFormat, Method);
  try
    Bitmap.LoadFromStream(M);
  finally
    M.Free;
  end;
end;

procedure GrayscaleBitmap(Bitmap: TBitmap);
begin
  SetBitmapPixelFormat(Bitmap, pf8bit, mmGrayscale);
end;

function ZoomImage(ImageW, ImageH, MaxW, MaxH: Integer; Stretch: Boolean):
  TPoint;
var
  Zoom: Double;
begin
  Result := Point(0, 0);
  if (MaxW <= 0) or (MaxH <= 0) or (ImageW <= 0) or (ImageH <= 0) then
    Exit;
  if Stretch then
  begin
    Zoom := MaxFloat([ImageW / MaxW, ImageH / MaxH]);
    if Zoom > 0 then
    begin
      Result.X := Round(ImageW * 0.98 / Zoom);
      Result.Y := Round(ImageH * 0.98 / Zoom);
    end
    else
    begin
      Result.X := ImageW;
      Result.Y := ImageH;
    end;
  end
  else
  begin
    Result.X := MaxW;
    Result.Y := MaxH;
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
    IntersectClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
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
    Self.FStartColor := TJvGradientOptions(Source).StartColor;
    Self.FEndColor := TJvGradientOptions(Source).EndColor;
    Self.FStepCount := TJvGradientOptions(Source).StepCount;
    Self.FDirection := TJvGradientOptions(Source).Direction;
    Self.FVisible := TJvGradientOptions(Source).Visible;
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
    raise EJVCLException.CreateRes(@RsENotForMdi);
  if AMenu = nil then
    AMenu := AForm.Menu;
  if AMenu = nil then
    Exit;

  TotalWidth := AToolbar.BorderWidth;
  for I := AToolbar.ButtonCount - 1 downto 0 do
    AToolbar.Buttons[I].Free;
  AToolbar.ShowCaptions := True;

  for I := AMenu.Items.Count - 1 downto 0 do
  begin
    Button := TToolButton.Create(AToolBar);
    Button.Parent := AToolBar;
    Button.AutoSize := True;
    Button.Caption := AMenu.Items[I].Caption;
    Button.Grouped := True;
    Button.MenuItem := AMenu.Items[I];
    Inc(TotalWidth, Button.Width + AToolBar.BorderWidth);
  end;

  AToolBar.Width := TotalWidth;
  AForm.Menu := nil;
end;

//=== ListView functions =====================================================

procedure JvListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean; Headers: Boolean);
var
  ColWidths: array of Word;

  procedure AddLine(const S: string);
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

var
  R, C: Integer;
  S: string;
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
          AddLine(S);
          S := '';
          for C := 0 to Count - 1 do
            S := S + StringOfChar('-', ColWidths[C]) + ' ';
          AddLine(S);
        end;
      for R := 0 to Items.Count - 1 do
        if not SelectedOnly or Items[R].Selected then
          with Items[R] do
          begin
            S := MakeCellStr(Caption, 0);
            for C := 0 to Min(SubItems.Count, Columns.Count - 1) - 1 do
              S := S + MakeCellStr(SubItems[C], C + 1);
            AddLine(S);
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
  I: Integer;
begin
  ListView := TListColumns(Column.Collection).Owner as TListView;
  ListView.Columns.BeginUpdate;
  try
    with ListView.Columns do
      for I := 0 to Count - 1 do
        Items[I].ImageIndex := -1;
    if ListView.Tag and $FF = Column.Index then
      ListView.Tag := ListView.Tag xor $100
    else
      ListView.Tag := Column.Index;
    if ListView.Tag and $100 = 0 then
      Column.ImageIndex := AscendingSortImage
    else
      Column.ImageIndex := DescendingSortImage;
  finally
    ListView.Columns.EndUpdate;
  end;
end;

procedure JvListViewCompare(ListView: TListView; Item1, Item2: TListItem;
  var Compare: Integer);

  function FmtStrToInt(S: string): Integer;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(S) do
      if not CharInSet(S[I], DigitChars + ['-']) then
        Delete(S, I, 1)
      else
        Inc(I);
    Result := StrToInt(S);
  end;

var
  ColIndex: Integer;
begin
  with ListView do
  begin
    ColIndex := Tag and $FF - 1;
    if Columns[ColIndex + 1].Alignment = taLeftJustify then
    begin
      if ColIndex = -1 then
        Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
      else
        Compare := AnsiCompareText(Item1.SubItems[ColIndex], Item2.SubItems[ColIndex]);
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
  H: THandle;
  Data: Integer;
  SaveOnSelectItem: TLVSelectItemEvent;
begin
  with ListView do
    if MultiSelect then
    begin
      Items.BeginUpdate;
      SaveOnSelectItem := OnSelectItem;
      WaitCursor;
      try
        H := Handle;
        OnSelectItem := nil;
        if Deselect then
          Data := 0
        else
          Data := LVIS_SELECTED;
        for I := 0 to Items.Count - 1 do
          ListView_SetItemState(H, I, Data, LVIS_SELECTED);
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
      TempItem.MakeVisible(True);
  end;
end;

function JvListViewGetOrderedColumnIndex(Column: TListColumn): Integer;
var
  ColumnOrder: array of Integer;
  Columns: TListColumns;
  I: Integer;
begin
  Result := -1;
  Columns := TListColumns(Column.Collection);
  SetLength(ColumnOrder, Columns.Count);
  ListView_GetColumnOrderArray(Columns.Owner.Handle, Columns.Count, PInteger(ColumnOrder));
  for I := 0 to High(ColumnOrder) do
    if ColumnOrder[I] = Column.Index then
    begin
      Result := I;
      Break;
    end;
end;

procedure JvListViewSetSystemImageList(ListView: TListView);
var
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
begin
  FillChar(FileInfo, SizeOf(FileInfo), 0);
  ImageListHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(ListView.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, LPARAM(ImageListHandle));

  FillChar(FileInfo, SizeOf(FileInfo), 0);
  ImageListHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  SendMessage(ListView.Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, LPARAM(ImageListHandle));
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

function IsHotTrackFontDfmStored(TrackFont, Font: TFont; TrackOptions: TJvTrackFontOptions): Boolean;
var
  DefFont: TFont;
begin
  if hoFollowFont in TrackOptions then
    DefFont := nil
  else
  begin
    DefFont := TFont.Create;
    Font := DefFont;
    TrackOptions := []; // compare all
  end;
  try
    Result := ((hoPreserveCharSet in TrackOptions) and (TrackFont.Charset <> Font.Charset)) or
              ((hoPreserveColor in TrackOptions) and (TrackFont.Color <> Font.Color)) or
              ((hoPreserveHeight in TrackOptions) and (TrackFont.Height <> Font.Height)) or
              ((hoPreservePitch in TrackOptions) and (TrackFont.Pitch <> Font.Pitch)) or
              ((hoPreserveStyle in TrackOptions) and (TrackFont.Style <> Font.Style)) or
              {$IFDEF COMPILER10_UP}
              ((hoPreserveOrientation in TrackOptions) and (TrackFont.Orientation <> Font.Orientation)) or
              {$ENDIF COMPILER10_UP}
              {$IFDEF COMPILER15_UP}
              ((hoPreserveQuality in TrackOptions) and (TrackFont.Quality <> Font.Quality)) or
              {$ENDIF COMPILER15_UP}
              ((hoPreserveName in TrackOptions) and (TrackFont.Name <> Font.Name));
  finally
    DefFont.Free;
  end;
end;

{ end JvCtrlUtils }

function GetDefaultCheckBoxSize: TSize;
begin
  with TBitmap.Create do
  try
    Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
    Result.cx := Width div 4;
    Result.cy := Height div 3;
  finally
    Free;
  end;
end;

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
var
  tt: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, tt);
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

function StdWndProc(Window: THandle; Message, WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
var
  Msg: Messages.TMessage;
  WndProc: TWndMethod;
begin
  TMethod(WndProc).Code := Pointer(GetWindowLongPtr(Window, 0));
  TMethod(WndProc).Data := Pointer(GetWindowLongPtr(Window, SizeOf(Pointer)));
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

function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): THandle;
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
    SetWindowLongPtr(Result, 0, LONG_PTR(TMethod(Method).Code));
    SetWindowLongPtr(Result, SizeOf(TMethod(Method).Code), LONG_PTR(TMethod(Method).Data));
    SetWindowLongPtr(Result, GWLP_WNDPROC, LONG_PTR(@StdWndProc));
  end;
end;

procedure DeallocateHWndEx(Wnd: THandle);
begin
  Windows.DestroyWindow(Wnd);
end;

function JvMakeObjectInstance(Method: TWndMethod): Pointer;
begin
  Result := MakeObjectInstance(Method);
end;

procedure JvFreeObjectInstance(ObjectInstance: Pointer);
begin
  if Assigned(ObjectInstance) then
    FreeObjectInstance(ObjectInstance);
end;

{$ENDIF MSWINDOWS}

procedure InitScreenCursors;
begin
  try
    if Screen <> nil then
    begin
      // now only available through SetDefaultJVCLCursors
      Screen.Cursors[crMultiDragLink] := Screen.Cursors[crMultiDrag];
      Screen.Cursors[crDragAlt] := Screen.Cursors[crDrag];
      Screen.Cursors[crMultiDragAlt] := Screen.Cursors[crMultiDrag];
      Screen.Cursors[crMultiDragLinkAlt] := Screen.Cursors[crMultiDrag];
    end;
  except
  end;
end;

const
  LeftBrackets = ['[', '{', '('];
  RightsBrackets = [']', '}', ')'];

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

function FontToString(Font: TFont): string;
begin
  Result := Format('%s,%d,%s,%d,%s,%d', [Font.Name, Font.Size,
    FontStylesToString(Font.Style), Ord(Font.Pitch), ColorToString(Font.Color), Font.Charset]);
end;

function StringToFont(const Str: string): TFont;
const
  Delims = [',', ';'];
var
  Pos: Integer;
  I: Byte;
  S: string;
begin
  Result := TFont.Create;
  try
    Pos := 1;
    I := 0;
    while Pos <= Length(Str) do
    begin
      Inc(I);
      S := Trim(ExtractSubstr(Str, Pos, Delims));
      case I of
        1:
          Result.Name := S;
        2:
          Result.Size := StrToIntDef(S, Result.Size);
        3:
          Result.Style := StringToFontStyles(S);
        4:
          Result.Pitch := TFontPitch(StrToIntDef(S, Ord(Result.Pitch)));
        5:
          Result.Color := StringToColor(S);
        6:
          Result.Charset := TFontCharset(StrToIntDef(S, Result.Charset));
      end;
    end;
  finally
  end;
end;


function RectToStr(Rect: TRect): string;
begin
  Result := Format('[%d,%d,%d,%d]', [Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]);
end;

function StrToRect(const Str: string; const Def: TRect): TRect;
var
  S: string;
  Temp: string{$IFNDEF RTL200_UP}[10]{$ENDIF ~RTL200_UP};
  I: Integer;
begin
  Result := Def;
  S := Str;
  if (S <> '') and CharInSet(S[1], LeftBrackets) and CharInSet(S[Length(S)], RightsBrackets) then
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
  Result := Format('[%d,%d]', [P.X, P.Y]);
end;

function StrToPoint(const Str: string; const Def: TPoint): TPoint;
var
  S: string;
  Temp: string{$IFNDEF RTL200_UP}[10]{$ENDIF ~RTL200_UP};
  I: Integer;
begin
  Result := Def;
  S := Str;
  if (S <> '') and CharInSet(S[1], LeftBrackets) and CharInSet(S[Length(Str)], RightsBrackets) then
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

procedure DrawLine(Canvas: TCanvas; X, Y, X2, Y2: Integer);
begin
  Canvas.MoveTo(X, Y);
  Canvas.LineTo(X2, Y2);
end;

procedure DrawArrow(Canvas: TCanvas; X, Y: Integer; Size: Integer;
  Color: TColor = clBlack; Direction: TAnchorKind = akBottom);
begin
  if (X >= 0) and (Y >= 0) and (Size > 0) then
    DrawArrow(Canvas, Bounds(X, Y, Size, Size), Color, Direction, 0);
end;

procedure DrawArrow(Canvas: TCanvas; ARect: TRect; Color: TColor = clBlack;
  Direction: TAnchorKind = akBottom; Margin: Integer = 0);
var
  Size: Integer;
begin
  RectNormalize(ARect);

  if Margin <> 0 then
    InflateRect(ARect, -Margin, -Margin);

  Size := RectWidth(ARect);
  if Odd(Size) then
    Dec(ARect.Right);

  RectSquare(ARect);
  Size := RectWidth(ARect);

  Canvas.Pen.Color := Color;
  case Direction of
    akLeft: {Draw from right to left}
      begin
        ARect.Right := ARect.Right - (Size div 4); {Because rect is Square, Thus origin sub  size/4 is center}
        while ARect.Top < ARect.Bottom {+ 1} do
        begin
          DrawLine(Canvas, ARect.Right, ARect.Top, ARect.Right, ARect.Bottom);
          InflateRect(ARect, -1, -1);
        end;
      end;
    akRight: {Draw  from left to right}
      begin
        ARect.Left := ARect.Left + (Size div 4); {Because rect is Square, Thus origin add  size/4 is center}
        while ARect.Top < ARect.Bottom + 1 do
        begin
          DrawLine(Canvas, ARect.Left, ARect.Top, ARect.Left, ARect.Bottom);
          InflateRect(ARect, -1, -1);
        end;
      end;
    akTop: {Draw from Bottom to top}
      begin
        ARect.Bottom := ARect.Bottom - (Size div 4); {Because rect is Square, Thus origin sub  size/4 is center}
        while ARect.Left < ARect.Right + 1 do
        begin
          DrawLine(Canvas, ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom);
          InflateRect(ARect, -1, -1);
        end;
      end;
    akBottom: {Draw from top to Bottom}
      begin
        ARect.Top := ARect.Top + (Size div 4); {Because rect is Square, Thus origin add  size/4 is center}
        while ARect.Left < ARect.Right + 1 do
        begin
          DrawLine(Canvas, ARect.Left, ARect.Top, ARect.Right, ARect.Top);
          InflateRect(ARect, -1, -1);
        end;
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

procedure TJvPoint.AssignPoint(const Source: TPoint);
begin
  X := Source.X;
  Y := Source.Y;
end;

procedure TJvPoint.Assign(const Source: TPoint);
begin
  X := Source.X;
  Y := Source.Y;
end;

procedure TJvPoint.CopyToPoint(var Point: TPoint);
begin
  Point.X := X;
  Point.Y := Y;
end;

procedure TJvPoint.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvPoint.GetAsPoint: TPoint;
begin
  Result := Point(FX, FY);
end;

procedure TJvPoint.SetAsPoint(const Value: TPoint);
begin
  if (Value.X <> FX) or (Value.Y <> FY) then
  begin
    FX := Value.X;
    FY := Value.Y;
    DoChange;
  end;
end;

procedure TJvPoint.SetX(Value: Longint);
begin
  if Value <> FX then
  begin
    FX := Value;
    DoChange;
  end;
end;

procedure TJvPoint.SetY(Value: Longint);
begin
  if Value <> FY then
  begin
    FY := Value;
    DoChange;
  end;
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

procedure TJvRect.AssignRect(const Source: TRect);
begin
  TopLeft.AssignPoint(Source.TopLeft);
  BottomRight.AssignPoint(Source.BottomRight);
end;

procedure TJvRect.Assign(const Source: TRect);
begin
  TopLeft.Assign(Source.TopLeft);
  BottomRight.Assign(Source.BottomRight);
end;

procedure TJvRect.CopyToRect(var Rect: TRect);
begin
  TopLeft.CopyToPoint(Rect.TopLeft);
  BottomRight.CopyToPoint(Rect.BottomRight);
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

{ TJvSize }

procedure TJvSize.Assign(Source: TPersistent);
begin
  if Source is TJvSize then
  begin
    FWidth := (Source as TJvSize).Width;
    FHeight := (Source as TJvSize).Height;
    DoChange;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TJvSize.AssignSize(const Source: TSize);
begin
  FWidth := Source.cx;
  FHeight := Source.cy;
  DoChange;
end;

procedure TJvSize.Assign(const Source: TSize);
begin
  FWidth := Source.cx;
  FHeight := Source.cy;
  DoChange;
end;

procedure TJvSize.CopyToSize(var Size: TSize);
begin
  Size.cx := Width;
  Size.cy := Height;
end;

procedure TJvSize.DoChange;
begin
  if Assigned(OnChange) then
   OnChange(Self);
end;

function TJvSize.GetSize: TSize;
begin
  Result.cx := FWidth;
  Result.cy := FHeight;
end;

procedure TJvSize.SetSize(const Value: TSize);
begin
  if (Value.cx <> FWidth) or (Value.cy <> FHeight) then
  begin
    FWidth := Value.cx;
    FHeight := Value.cy;
    DoChange;
  end;
end;

procedure TJvSize.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    DoChange;
  end;
end;

procedure TJvSize.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
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

const
  cBR = '<BR>';
  cHR = '<HR>';
  cTagBegin = '<';
  cTagEnd = '>';
  cLT = '<';
  cGT = '>';
  cQuote = '"';
  cCENTER = 'CENTER';
  cRIGHT = 'RIGHT';
  cHREF = 'HREF';
  cIND = 'IND';
  cCOLOR = 'COLOR';
  cBGCOLOR = 'BGCOLOR';

// moved from JvHTControls and renamed
function HTMLPrepareText(const Text: string): string;
type
  THtmlCode = record
    Html: string;
    Text: UTF8String;
  end;
const
  Conversions: array [0..6] of THtmlCode = (
    (Html: '&amp;'; Text: '&'),
    (Html: '&quot;'; Text: '"'),
    (Html: '&reg;'; Text: #$C2#$AE),
    (Html: '&copy;'; Text: #$C2#$A9),
    (Html: '&trade;'; Text: #$E2#$84#$A2),
    (Html: '&euro;'; Text: #$E2#$82#$AC),
    (Html: '&nbsp;'; Text: ' ')
  );
var
  I: Integer;
begin
  Result := Text;
  for I := Low(Conversions) to High(Conversions) do
    Result := StringReplace(Result, Conversions[I].Html, Utf8ToAnsi(Conversions[I].Text), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll, rfIgnoreCase]); // only <BR> can be new line
  Result := StringReplace(Result, cBR, sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, cHR, cHR + sLineBreak, [rfReplaceAll, rfIgnoreCase]); // fixed <HR><BR>
end;

function HTMLBeforeTag(var Str: string; DeleteToTag: Boolean = False): string;
begin
  if Pos(cTagBegin, Str) > 0 then
  begin
    Result := Copy(Str, 1, Pos(cTagBegin, Str) - 1);
    if DeleteToTag then
      Delete(Str, 1, Pos(cTagBegin, Str) - 1);
  end
  else
  begin
    Result := Str;
    if DeleteToTag then
      Str := '';
  end;
end;

function GetChar(const Str: string; Pos: Word; Up: Boolean = False): Char;
begin
  if Length(Str) >= Pos then
    Result := Str[Pos]
  else
    Result := ' ';
  if Up then
    Result := UpCase(Result);
end;

function HTMLDeleteTag(const Str: string): string;
begin
  Result := Str;
  if (GetChar(Result, 1) = cTagBegin) and (Pos(cTagEnd, Result) > 1) then
    Delete(Result, 1, Pos(cTagEnd, Result));
end;

procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
var
  H: Integer;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, Width, H, CalcType, MouseX, MouseY, MouseOnLink,
    LinkName, SuperSubScriptRatio, Scale);
  if CalcType = htmlCalcHeight then
    Width := H;
end;

type
  TScriptPosition = (spNormal, spSuperscript, spSubscript);

procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width, Height: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
const
  DefaultLeft = 0; // (ahuser) was 2
var
  vText, vM, TagPrp, Prp, TempLink: string;
  vCount: Integer;
  vStr: TStringList;
  Selected: Boolean;
  Alignment: TAlignment;
  Trans, IsLink: Boolean;
  CurLeft: Integer;
  // for begin and end
  OldFontStyles: TFontStyles;
  OldFontColor: TColor;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldAlignment: TAlignment;
  OldFont: TFont;
  OldWidth: Integer;
  // for font style
  RemFontColor,
  RemBrushColor: TColor;
  RemFontSize: Integer;
  ScriptPosition: TScriptPosition;

  function ExtractPropertyValue(const Tag: string; PropName: string): string;
  var
    I: Integer;
  begin
    Result := '';
    PropName := UpperCase(PropName);
    if Pos(PropName, UpperCase(Tag)) > 0 then
    begin
      Result := Copy(Tag, Pos(PropName, UpperCase(Tag)) + Length(PropName), Length(Tag));
     if Pos('"', Result) <> 0 then
     begin
       Result := Copy(Result, Pos('"', Result) + 1, Length(Result));
       Result := Copy(Result, 1, Pos('"', Result) - 1);
     end
     else
     if Pos('''', Result) <> 0 then
     begin
       Result := Copy(Result, Pos('''', Result) + 1, Length(Result));
       Result := Copy(Result, 1, Pos('''', Result) - 1);
     end
     else
     begin
       Result := Trim(Result);
       Delete(Result, 1, 1);
       Result := Trim(Result);
       I := 1;
       while (I < Length(Result)) and (Result[I+1] <> ' ') do
         Inc(I);
       Result := Copy(Result, 1, I);
     end;
    end;
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if Assigned(Canvas) then
      if Include then
        Canvas.Font.Style := Canvas.Font.Style + [Style]
      else
        Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

  function CalcPos(const Str: string): Integer;
  begin
    case Alignment of
      taRightJustify:
        Result := (Rect.Right - Rect.Left) - HTMLTextWidth(Canvas, Rect, State, Str, Scale);
      taCenter:
        Result := DefaultLeft + ((Rect.Right - Rect.Left) - HTMLTextWidth(Canvas, Rect, State, Str, SuperSubScriptRatio)) div 2;
    else
      Result := DefaultLeft;
    end;
    if Result <= 0 then
      Result := DefaultLeft;
  end;

  procedure Draw(const M: string);
  var
    Width, Height: Integer;
    R: TRect;
    OriginalFontSize: Integer;
  begin
    R := Rect;
    Inc(R.Left, CurLeft);
    if Assigned(Canvas) then
    begin
      OriginalFontSize := Canvas.Font.Size; 
      try
        if ScriptPosition <> spNormal then
          Canvas.Font.Size := Round(Canvas.Font.Size * SuperSubScriptRatio);
          
        Width  := Canvas.TextWidth(M);
        Height := CanvasMaxTextHeight(Canvas);

        if ScriptPosition = spSubscript then
          R.Top := R.Bottom - Height - 1;

        if IsLink and not MouseOnLink then
          if (MouseY >= R.Top) and (MouseY <= R.Top + Height) and
             (MouseX >= R.Left) and (MouseX <= R.Left + Width) and
             ((MouseY > 0) or (MouseX > 0)) then
          begin
            MouseOnLink := True;
            Canvas.Font.Color := clRed; // hover link
            LinkName := TempLink;
          end;
        if CalcType = htmlShow then
        begin
          if Trans then
            Canvas.Brush.Style := bsClear; // for transparent
          Canvas.TextOut(R.Left, R.Top, M);
        end;
        CurLeft := CurLeft + Width;
      finally
        Canvas.Font.Size := OriginalFontSize;
      end;
    end;
  end;

  procedure NewLine(Always: Boolean = False);
  begin
    if Assigned(Canvas) then
      if Always or (vCount < vStr.Count - 1) then
      begin
        Width := Max(Width, CurLeft);
        CurLeft := DefaultLeft;
        Rect.Top := Rect.Top + CanvasMaxTextHeight(Canvas);
      end;
  end;

begin
  // (p3) remove warnings
  OldFontColor := 0;
  OldBrushColor := 0;
  OldBrushStyle := bsClear;
  RemFontSize := 0;
  RemFontColor := 0;
  RemBrushColor := 0;
  OldAlignment := taLeftJustify;
  OldFont := TFont.Create;

  if Canvas <> nil then
  begin
    OldFontStyles := Canvas.Font.Style;
    OldFontColor  := Canvas.Font.Color;
    OldBrushColor := Canvas.Brush.Color;
    OldBrushStyle := Canvas.Brush.Style;
    OldAlignment  := Alignment;
    RemFontColor  := Canvas.Font.Color;
    RemBrushColor := Canvas.Brush.Color;
    RemFontSize   := Canvas.Font.size;
  end;
  try
    Alignment := taLeftJustify;
    IsLink := False;
    MouseOnLink := False;
    vText := Text;
    vStr  := TStringList.Create;
    vStr.Text := HTMLPrepareText(vText);
    LinkName := '';
    TempLink := '';
    ScriptPosition := spNormal;

    Selected := (odSelected in State) or (odDisabled in State);
    Trans := (Canvas.Brush.Style = bsClear) and not selected;

    Width := DefaultLeft;
    CurLeft := DefaultLeft;

    vM := '';
    for vCount := 0 to vStr.Count - 1 do
    begin
      vText := vStr[vCount];
      CurLeft := CalcPos(vText);
      while vText <> '' do
      begin
        vM := HTMLBeforeTag(vText, True);
        vM := StringReplace(vM, '&lt;', cLT, [rfReplaceAll, rfIgnoreCase]); // <--+ this must be here
        vM := StringReplace(vM, '&gt;', cGT, [rfReplaceAll, rfIgnoreCase]); // <--/
        if GetChar(vText, 1) = cTagBegin then
        begin
          if vM <> '' then
            Draw(vM);
          if Pos(cTagEnd, vText) = 0 then
            Insert(cTagEnd, vText, 2);
          if GetChar(vText, 2) = '/' then
          begin
            case GetChar(vText, 3, True) of
              'A':
                begin
                  IsLink := False;
                  Canvas.Font.Assign(OldFont);
                end;
              'B':
                Style(fsBold, False);
              'I':
                Style(fsItalic, False);
              'U':
                Style(fsUnderline, False);
              'S':
                begin
                  ScriptPosition := spNormal;
                  Style(fsStrikeOut, False);
                end;
              'F':
                begin
                  if not Selected then // restore old colors
                  begin
                    Canvas.Font.Color := RemFontColor;
                    Canvas.Brush.Color := RemBrushColor;
                    Canvas.Font.Size := RemFontSize;
                    Trans := True;
                  end;
                end;
            end
          end
          else
          begin
            case GetChar(vText, 2, True) of
              'A':
                begin
                  if GetChar(vText, 3, True) = 'L' then // ALIGN
                  begin
                    TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText) - 2));
                    if Pos(cCENTER, TagPrp) > 0 then
                      Alignment := taCenter
                    else
                    if Pos(cRIGHT, TagPrp) > 0 then
                      Alignment := taRightJustify
                    else
                      Alignment := taLeftJustify;
                    CurLeft := DefaultLeft;
                    if CalcType in [htmlShow, htmlHyperLink] then
                      CurLeft := CalcPos(vText);
                  end
                  else
                  begin   // A HREF
                    TagPrp := Copy(vText, 2, Pos(cTagEnd, vText) - 2);
                    if Pos(cHREF, UpperCase(TagPrp)) > 0 then
                    begin
                      IsLink := True;
                      OldFont.Assign(Canvas.Font);
                      if not Selected then
                        Canvas.Font.Color := clBlue;
                      TempLink := ExtractPropertyValue(TagPrp, cHREF);
                    end;
                  end;
                end;
              'B':
                Style(fsBold, True);
              'I':
                if GetChar(vText, 3, True) = 'N' then //IND="%d"
                begin
                  TagPrp := Copy(vText, 2, Pos(cTagEnd, vText) - 2);
                  CurLeft := StrToInt(ExtractPropertyValue(TagPrp, cIND)); // ex IND="10"
                  if odReserved1 in State then
                    CurLeft := Round((CurLeft * Scale) div 100);
                end
                else
                  Style(fsItalic, True); // ITALIC
              'U':
                Style(fsUnderline, True);
              'S':
                begin
                  if GetChar(vText, 4, True) = 'P' then
                  begin
                    ScriptPosition := spSuperscript;
                  end
                  else if GetChar(vText, 4, True) = 'B' then
                  begin
                    ScriptPosition := spSubscript;
                  end
                  else
                  begin
                    ScriptPosition := spNormal;
                    Style(fsStrikeOut, True);
                  end;
                end;
              'H':
                if (GetChar(vText, 3, True) = 'R') and Assigned(Canvas) then // HR
                begin
                  if odDisabled in State then // only when disabled
                    Canvas.Pen.Color := Canvas.Font.Color;
                  OldWidth := Canvas.Pen.Width;
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText)-2));
                  Canvas.Pen.Width := StrToIntDef(ExtractPropertyValue(TagPrp, 'SIZE'), 1); // ex HR="10"
                  if odReserved1 in State then
                    Canvas.Pen.Width := Round((Canvas.Pen.Width * Scale) div 100);
                  if CalcType = htmlShow then
                  begin
                    Canvas.MoveTo(Rect.Left, Rect.Top + CanvasMaxTextHeight(Canvas));
                    Canvas.LineTo(Rect.Right, Rect.Top + CanvasMaxTextHeight(Canvas));
                  end;
                  Rect.Top := Rect.Top + 1 + Canvas.Pen.Width;
                  Canvas.Pen.Width := OldWidth;
                  NewLine(HTMLDeleteTag(vText) <> '');
                end;
              'F':
                if (Pos(cTagEnd, vText) > 0) and (not Selected) and Assigned(Canvas) {and (CalcType in [htmlShow, htmlHyperLink])} then // F from FONT
                begin
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText) - 2));
                  RemFontColor := Canvas.Font.Color;
                  RemBrushColor := Canvas.Brush.Color;

                  if Pos(cCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cCOLOR);
                    if Prp[1] = '#' then
                      Prp[1] := '$';
                    Canvas.Font.Color := StringToColor(Prp);
                  end;
                  if Pos(cBGCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cBGCOLOR);
                    if Prp[1] = '#' then
                      Prp[1] := '$';
                    if UpperCase(Prp) = 'CLNONE' then
                      Trans := True
                    else
                    begin
                      Canvas.Brush.Color := StringToColor(Prp);
                      Trans := False;
                    end;
                  end;
                  if Pos('SIZE', TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, 'SIZE');
                    Canvas.Font.Size := StrToIntDef(Prp,2) * Canvas.Font.Size div 2;
                  end;
                end;
            end;
          end;
          vText := HTMLDeleteTag(vText);
          vM := '';
        end;
      end;
      if vM <> '' then
        Draw(vM);
      NewLine;
      vM := '';
    end;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := OldFontStyles;
      Canvas.Font.Color := OldFontColor;
      Canvas.Brush.Color := OldBrushColor;
      Canvas.Brush.Style := OldBrushStyle;
      Alignment := OldAlignment;
  {    Canvas.Font.Color := RemFontColor;
      Canvas.Brush.Color:= RemBrushColor;}
    end;
    FreeAndNil(vStr);
    FreeAndNil(OldFont);
  end;
  Width := Max(Width, CurLeft - DefaultLeft);
  Height := Rect.Top + CanvasMaxTextHeight(Canvas);
end;

function HTMLDrawText(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer): string;
var
  W: Integer;
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, W, htmlShow, 0, 0, S, St, SuperSubScriptRatio, Scale);
end;

function HTMLDrawTextHL(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; MouseX, MouseY: Integer;
  SuperSubScriptRatio: Double; Scale: Integer): string;
var
  W: Integer;
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, W, htmlShow, MouseX, MouseY, S, St, SuperSubScriptRatio, Scale);
end;

function HTMLPlainText(const Text: string): string;
var
  S: string;
begin
  Result := '';
  S := HTMLPrepareText(Text);
  while Pos(cTagBegin, S) > 0 do
  begin
    Result := Result + Copy(S, 1, Pos(cTagBegin, S)-1);
    if Pos(cTagEnd, S) > 0 then
      Delete(S, 1, Pos(cTagEnd, S))
    else
      Delete(S, 1, Pos(cTagBegin, S));
  end;
  Result := Result + S;
end;

function HTMLTextExtent(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): TSize;
var
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, Result.cx, Result.cy, htmlCalcWidth, 0, 0, S, St, SuperSubScriptRatio, Scale);
  if Result.cy = 0 then
    Result.cy := CanvasMaxTextHeight(Canvas);
  Inc(Result.cy);
end;

function HTMLTextWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
var
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, Result, htmlCalcWidth, 0, 0, S, St, SuperSubScriptRatio, Scale);
end;

function HTMLTextHeight(Canvas: TCanvas; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
var
  S: Boolean;
  St: string;
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);
  HTMLDrawTextEx(Canvas, R, [], Text, Result, htmlCalcHeight, 0, 0, S, St, SuperSubScriptRatio, Scale);
  if Result = 0 then
    Result := CanvasMaxTextHeight(Canvas);
  Inc(Result);
end;

{ TJvPicture }
procedure TJvPicture.ReadBitmapData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  Bitmap.LoadFromStream(Stream);
end;

procedure TJvPicture.DefineProperties(Filer: TFiler);
var
  SavedPosition: Integer;
  Reader: TReader;
  VType : TValueType;
  WasBitmap : Boolean;
  Count : Longint;
  NameLength: Byte;
begin
  if Filer is TReader then
  begin
    // When we are reading, we must detect if the data is a valid TPicture
    // data or just a TBitmap data. This is done by having a sneak peak at
    // what's in the reader stream. If we find a NameLength tag that is
    // greater than 63 (it's built-in limit, see TPicture.DefineProperties)
    // then it must be a TBitmap and we then tell the bitmap to load itself
    // from the Filter.
    // Note: the test must be done here, before any call to the
    // DefineBinaryProperty of the Reader. If not, then the FPropName field
    // would be put back to blank and prevent the inherited DefineProperties
    // from working correctly.
    Reader := Filer as TReader;
    WasBitmap := False;
    SavedPosition := Reader.Position;

    VType := Reader.ReadValue;
    if VType = vaBinary then
    begin
      Reader.Read(Count, SizeOf(Count));
      Reader.Read(NameLength, SizeOf(NameLength));
      WasBitmap := NameLength > 63;
    end;

    Reader.Position := SavedPosition;

    if WasBitmap then
      Filer.DefineBinaryProperty('Data', ReadBitmapData, nil, True)
    else
      inherited DefineProperties(Filer);
  end
  else
    inherited DefineProperties(Filer);
end;

//=== { TGraphicSignature } ==================================================

// Code to manage graphic's signatures.
type
  TGraphicSignature = class(TObject)
  public
    Signature: AnsiString;
    Offset: Integer;
    GraphicClass: TGraphicClass;
    constructor Create(const ASignature: AnsiString; AOffset: Integer; AGraphicClass: TGraphicClass);
    function CheckSignature(AStream: TStream): Boolean;
  end;

constructor TGraphicSignature.Create(const ASignature: AnsiString; AOffset: Integer; AGraphicClass: TGraphicClass);
begin
  inherited Create;
  Signature := ASignature;
  Offset := AOffset;
  GraphicClass := AGraphicClass;
end;

function TGraphicSignature.CheckSignature(AStream: TStream): Boolean;
var
  Buffer: AnsiString;
  Count: Integer;
  BytesRead: Integer;
begin
  Result := False;
  try
    Count := Length(Signature);
    AStream.Position := Offset;
    SetLength(Buffer, Count);
    BytesRead := AStream.Read(Buffer[1], Count);
    Result := (BytesRead = Count) and (Buffer = Signature);
  except
    // Ignore any error...
  end;
end;

var
  GraphicSignatures: TObjectList = nil;

procedure GraphicSignaturesNeeded;
begin
  if not Assigned(GraphicSignatures) then
  begin
    GraphicSignatures := TObjectList.Create;

    RegisterGraphicSignature('BM', 0, TBitmap);
    RegisterGraphicSignature([0, 0, 1, 0], 0, TIcon);
    RegisterGraphicSignature([$D7, $CD], 0, TMetafile); // WMF
    RegisterGraphicSignature([1, 0], 0, TMetafile); // EMF
    RegisterGraphicSignature('JFIF', 6, TJPEGImage);
    RegisterGraphicSignature('Exif', 6 , TJPEGImage);
    // NB! Registering these will add a requirement on having the JvMM package installed
    // Let users register these manually
    // RegisterGraphicSignature([$0A], 0, TJvPcx);
    // RegisterGraphicSignature('ACON', 8, TJvAni);
    // JvCursorImage cannot be registered because it doesn't support
    // LoadFromStream/SaveToStream but here's the signature for future reference:
    // RegisterGraphicSignature([0, 0, 2, 0], 0, TJvCursorImage);
    {$IFDEF USE_JV_GIF}
    // RegisterGraphicSignature('GIF', 0, TJvGIFImage);
    {$ENDIF USE_JV_GIF}
//    RegisterGraphicSignature('GIF', 0, TGIFGraphic);
//    RegisterGraphicSignature('PNG', 1, TPNGGraphic);
  end;
end;

procedure RegisterGraphicSignature(const ASignature: AnsiString; AOffset: Integer; AGraphicClass: TGraphicClass);
var
  GraphicSignature: TGraphicSignature;
begin
  // Avoid bad signatures
  if (ASignature = '') or (AOffset < 0) or (AGraphicClass = nil) then
    raise  EJVCLException.CreateRes(@RsEBadGraphicSignature);
  GraphicSignaturesNeeded;
  // Should raise an exception if empty signature, negative offset or null class.
  GraphicSignature := TGraphicSignature.Create(ASignature, AOffset, AGraphicClass);
  try
    GraphicSignatures.Add(GraphicSignature);
  except
    GraphicSignature.Free;
  end;
end;

procedure RegisterGraphicSignature(const ASignature: array of Byte; AOffset: Integer; AGraphicClass: TGraphicClass);
begin
  if Length(ASignature) > 0 then
    RegisterGraphicSignature(StringOf(ASignature), AOffset, AGraphicClass);
end;

procedure UnregisterGraphicSignature(AGraphicClass: TGraphicClass); overload;
var
  I: Integer;
begin
  if Assigned(GraphicSignatures) then
    for I := GraphicSignatures.Count - 1 downto 0 do
      if TGraphicSignature(GraphicSignatures[I]).GraphicClass = AGraphicClass then
        GraphicSignatures.Delete(I);
end;

procedure UnregisterGraphicSignature(const ASignature: AnsiString; AOffset: Integer);
var
  I: Integer;
begin
  if Assigned(GraphicSignatures) then
    for I := GraphicSignatures.Count - 1 downto 0 do
      with TGraphicSignature(GraphicSignatures[I]) do
        if (Signature = ASignature) and (Offset = AOffset) then
          GraphicSignatures.Delete(I);
end;

procedure UnregisterGraphicSignature(const ASignature: array of Byte; AOffset: Integer);
begin
  UnregisterGraphicSignature(StringOf(ASignature), AOffset);
end;

function GetGraphicClass(AStream: TStream): TGraphicClass;
var
  P: Integer;
  I: Integer;
  S: TGraphicSignature;
begin
  Result := nil;
  GraphicSignaturesNeeded;
  if Assigned(GraphicSignatures) then
  begin
    P := AStream.Position;
    try
      for I := 0 to GraphicSignatures.Count - 1 do
      begin
        S := TGraphicSignature(GraphicSignatures[I]);
        if S.CheckSignature(AStream) then
        begin
          Result := S.GraphicClass;
          Exit;
        end;
      end;
    finally
      AStream.Position := P;
    end;
  end;
end;

function GetGraphicObject(AStream: TStream): TGraphic;
var
  LOnProc: TJvGetGraphicClassEvent;
begin
  LOnProc := nil;
  Result := GetGraphicObject(AStream, nil, LOnProc);
end;

function GetGraphicObject(AStream: TStream; ASender: TObject; AOnProc: TJvGetGraphicClassEvent): TGraphic; overload;
var
  GraphicClass: TGraphicClass;
begin
  // Figure out which Graphic class is...
  GraphicClass := GetGraphicClass(AStream);
  // Call user event
  if Assigned(AOnProc) and (AStream is TMemoryStream) then
    AOnProc(ASender, TMemoryStream(AStream), GraphicClass);
  // If we got one, load it..
  if Assigned(GraphicClass) then
  begin
    Result := GraphicClass.Create;
    Result.LoadFromStream(AStream);
  end
  else // nope.
    Result := nil;
end;

function PointChildToParent(APoint: TPoint; AChild: TControl; AParent: TWinControl): TPoint;
var
  LParent: TWinControl;
begin
  if AChild = nil then
    raise EInvalidOperation.Create(RsEChildControlMissing);

  if AParent = nil then
    AParent := AChild.Parent;
  if AParent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [AChild.Name]);
  Result := APoint;
  LParent := AChild.Parent;
  if AParent.ContainsControl(AChild) then
  begin
    Inc(Result.X, AChild.Left);
    Inc(Result.Y, AChild.Top);
    while (LParent <> nil) do
    begin
      if (LParent.Parent <> nil) and (LParent <> AParent) then
      begin
        Inc(Result.X, LParent.Left);
        Inc(Result.Y, LParent.Top);
      end;
      if LParent = AParent then
        Break
      else
        LParent := LParent.Parent;
    end;
  end else
    raise EInvalidOperation.CreateFmt(SParentGivenNotAParent, [AChild.Name]);
end;

function RectChildToParent(ARect: TRect; AChild: TControl; AParent: TWinControl = nil): TRect;
var
  Pt1, Pt2: TPoint;
begin
  Pt1 := PointChildToParent(ARect.TopLeft, AChild, AParent);
  Pt2 := PointChildToParent(ARect.BottomRight, AChild, AParent);
  Result := Rect(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function RectClientToScreen(AClientRect: TRect; AControl: TControl): TRect;
begin
  Result.TopLeft := AControl.ClientToScreen(AClientRect.TopLeft);
  Result.BottomRight := AControl.ClientToScreen(AClientRect.BottomRight);
end;

function RectScreenToClient(AScreenRect: TRect; AControl: TControl): TRect;
begin
  Result.TopLeft := AControl.ScreenToClient(AScreenRect.TopLeft);
  Result.BottomRight := AControl.ScreenToClient(AScreenRect.BottomRight);
end;

function MapControlRect(ACtlFrom, ACtlTo: TControl; AClientRect: TRect):
  TRect;
begin
  Result := RectClientToScreen(AClientRect, ACtlFrom);
  Result := RectScreenToClient(Result, ACtlTo);
end;

function MapControlPoint(ACtlFrom, ACtlTo: TControl; APoint: TPoint):  TPoint;
begin
  Result := ACtlFrom.ClientToScreen(APoint);
  Result := ACtlTo.ScreenToClient(Result);
end;

function MapWindowRect(hWndFrom, hWndTo: HWND; ARect: TRect): TRect;
begin
  MapWindowPoints(hWndFrom, hWndTo, ARect, 2);
  Result := ARect;
end;

function GetControlRect(AControl: TControl): TRect;
begin
  Assert(AControl <> nil, 'Control is nil');

  Result := Rect(0, 0, AControl.Width, AControl.Height);
end;

function GetControlScreenRect(AControl: TControl): TRect;
begin
  Assert(AControl <> nil, 'Control is nil');

  Result := AControl.BoundsRect;
  if AControl.Parent <> nil then
    Result := RectClientToScreen(Result, AControl.Parent);
end;

function GetControlDC(Control: TControl; var WindowHandle: HWND): HDC;
var
  lWinCtl: TWinControl;
begin
  Result := 0;
  if Control = nil then
    Exit;
  if Control is TWinControl then
  begin
    lWinCtl := TWinControl(Control);
    if not lWinCtl.HandleAllocated then
      Exit;
    Result := GetDC(lWinCtl.Handle);
    WindowHandle := lWinCtl.Handle;
  end
  else
  begin
    lWinCtl := Control.Parent;
    if lWinCtl = nil then
      raise EInvalidOperation.CreateFmt(SParentRequired, [Control.Name]);

    if not lWinCtl.HandleAllocated then
      Exit;
    Result := GetDC(lWinCtl.Handle);
    WindowHandle := lWinCtl.Handle;

    SetViewportOrgEx(Result, Control.Left, Control.Top, nil);
    IntersectClipRect(Result, 0, 0, Control.Width, Control.Height);
  end;
end;

function GetControlWindowDC(Control: TControl; var WindowHandle: HWND): HDC;
var
  lWinCtl: TWinControl;
begin
  Result := 0;
  if Control = nil then
    Exit;
  if Control is TWinControl then
  begin
    lWinCtl := TWinControl(Control);
    Result := GetWindowDC(lWinCtl.Handle);
    WindowHandle := lWinCtl.Handle;
  end
  else
  begin
    Result:= GetControlDC(Control,WindowHandle);
  end;
end;

function GetContainerMaxBoundsRect(AContainer: TWinControl): TRect;
var
  I: Integer;
begin
  if (AContainer = nil) or not Assigned(AContainer) then
    Exit;

  if AContainer.ControlCount = 0 then
    Result := AContainer.BoundsRect
  else
  begin
    for I := 0 to AContainer.ControlCount - 1 do
    begin
      with AContainer.Controls[I] do
      begin
        if I = 0 then
          Result := Bounds(Left, Top, Width, Height)
        else
        begin
          if Top < Result.Top then
            Result.Top := Top;
          if Left < Result.Left then
            Result.Left := Left;
          if Left + Width > Result.Right then
            Result.Right := Left + Width;
          if Top + Height > Result.Bottom then
            Result.Bottom := Top + Height;
        end;
      end;
    end;
  end;
end;

function GetParentWindow(ASender: TWinControl): HWND;
var
  Last, P: HWND;
begin
  if ASender = nil then
    raise EInvalidOperation.Create(RsEChildControlMissing);

  Last := ASender.Handle;
  P := GetParent(ASender.Handle);
  while P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
end;

function BeginClipRect(DC: HDC; AClipRect: TRect; fnMode: Integer): Integer; 
var
  MyRgn: HRGN;
begin
  Result := RGN_ERROR;
  if not IsRectEmpty(AClipRect) then
  begin
    MyRgn := CreateRectRgnIndirect(AClipRect);
    try
      Result := ExtSelectClipRgn(DC, MyRgn, fnMode);
    finally
      DeleteObject(MyRgn);
    end;
  end;
end;

function EndClipRect(DC: HDC): Integer;
begin
  Result := SelectClipRgn(DC, 0);
end;

function GetTopOwner(aCmp: TComponent): TComponent;
begin
  if aCmp = nil then
    Result := nil
  else
  if aCmp.Owner <> nil then
    Result := GetTopOwner(aCmp.Owner)
  else
    Result := aCmp;
end;

function GetTopForm(aCmp: TComponent): TCustomForm;
begin
  if aCmp is TControl then
  begin
    while (aCmp <> nil) and not (aCmp is TCustomForm) do
      aCmp := TControl(aCmp).Parent;
  end
  else // aCmp is TComponent
    while (aCmp <> nil) and not (aCmp is TCustomForm) do
      aCmp := aCmp.Owner;

  Result := TCustomForm(aCmp);
end;

function IsOwnedComponent(aCmp, aOwner: TComponent): Boolean;
begin
  Result := False;
  if not (Assigned(aCmp) or Assigned(aOwner)) then
    Exit;

  Result := True;
  while aCmp.Owner <> nil do
  begin
    if aCmp.Owner = aOwner then
      Exit;
    aCmp := aCmp.Owner;
  end;
  Result := False;
end;

function IsChildWindow(const AChild, AParent: THandle): Boolean;
var
  LParent: HWND;
begin
  { Determines whether a window is the child (or grand^x-child) of another window }
  LParent := AChild;
  if LParent = AParent then
    Result := False // (ahuser) a parent is no a child of itself
  else
  begin
    while (LParent <> AParent) and (LParent <> NullHandle) do
      LParent := GetParent(LParent);
    Result := (LParent = AParent) and (LParent <> NullHandle);
  end;
end;

function GenerateUniqueComponentName(AOwner, AComponent: TComponent; const
    AComponentName: string = ''): string;

  function ValidateName(const AName: string): String;
  var
    I: Integer;
    Ignore : Boolean;
    C : Char;
  begin
    Ignore := True;
    Result := '';
    for I := 1 to Length(AName)  do
    begin
      C := AName[I];
      if CharInSet(C, ['A'..'Z', 'a'..'z', '_']) or
         ((Result <> '') and CharInSet(C, ['0'..'9'])) then
      begin
        Ignore := False;
        Result := Result+C;
      end
      else if Result <> '' then
      begin
        if not Ignore then
          Result := Result+'_';
        Ignore := True;
      end;
    end;
  end;

  function GenerateName(const AName: string; ANumber: Integer): string;
  begin
    Result := ValidateName (AName);
    if Assigned(AOwner) and (AOwner.Name <> '') then
      Result := AOwner.Name + '_' + Result;
    if ANumber > 0 then
      Result := Result + IntToStr(ANumber);
  end;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if AName <> '' then
      for I := 0 to AOwner.ComponentCount - 1 do
        if (AOwner.Components[I] <> AComponent) and
          (CompareText(AOwner.Components[I].Name, AName) = 0) then
        begin
          Result := False;
          Break;
        end;
  end;

var
  I: Integer;
begin
  if not Assigned(AOwner) then
    Result := ''
  else
    for I := 0 to MaxInt do
    begin
      if (AComponentName <> '') then
        Result := GenerateName(AComponentName, I)
      else
        if Assigned(AComponent) then
          Result := GenerateName(AComponent.ClassName, I)
        else
          Result := GenerateName('', I);
      if IsUnique(Result) then
        Break;
    end;
end;

function ReplaceComponentReference(This, NewReference: TComponent; var VarReference: TComponent): Boolean;
begin
  Result := (VarReference <> NewReference) and Assigned(This);
  if Result then
  begin
    if Assigned(VarReference) then
      VarReference.RemoveFreeNotification(This);
    VarReference := NewReference;
    if Assigned(VarReference) then
      VarReference.FreeNotification(This);
  end;
end;

function ReplaceImageListReference(This: TComponent; NewReference: TCustomImageList;
  var VarReference: TCustomImageList; ChangeLink: TChangeLink): Boolean;
begin
  Result := (VarReference <> NewReference) and Assigned(This);
  if Result then
  begin
    if Assigned(VarReference) then
    begin
      VarReference.RemoveFreeNotification(This);
      VarReference.UnRegisterChanges(ChangeLink);
    end;
    VarReference := NewReference;
    if Assigned(VarReference) then
    begin
      VarReference.RegisterChanges(ChangeLink);
      VarReference.FreeNotification(This);
    end;
  end;
end;

function IsSubControlFocused(AControl: TWinControl): Boolean;
var
  Form: TCustomForm;
  Ctrl: TWinControl;
begin
  Result := False;
  if AControl = nil then
    Exit;

  if AControl.Focused then
    Result := True
  else
  begin
    Form := GetParentForm(AControl);
    if Form <> nil then
    begin
      Ctrl := Form.ActiveControl;
      while Ctrl <> nil do
      begin
        if Ctrl = AControl then
        begin
          Result := True;
          Exit;
        end;
        Ctrl := Ctrl.Parent;
      end;
    end;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitScreenCursors;

finalization
  FreeAndNil(DrawBitmap);
  FreeAndNil(GraphicSignatures);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
