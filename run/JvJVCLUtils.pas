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

Last Modified: 2002-09-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvJVCLUtils;

interface
uses
  {$IFDEF COMPILER6_UP}
  RTLConsts, Variants,
  {$ENDIF}
  Windows, Forms, Graphics, Messages, Classes, Controls, StdCtrls,
  ExtCtrls, Menus, Dialogs, Registry, ComCtrls, SysUtils, ShellApi,
  ImgList, Grids, IniFiles,
  JclBase, JclSysUtils, JclStrings,
  JvAppStore, JvTypes;

{$IFNDEF COMPILER6_UP}
type
  EOSError = class(EWin32Error);
{$ENDIF}

// Transform an icon to a bitmap
function IconToBitmap(Ico: HICON): TBitmap;
// Transform an icon to a bitmap using an image list
function IconToBitmap2(Ico: HICON; Size: Integer = 32; TransparentColor: TColor = clNone): TBitmap;
function IconToBitmap3(Ico: HICON; Size: Integer = 32; TransparentColor: TColor = clNone): TBitmap;

//Open an object with the shell (url or something like that)
function OpenObject(Value: PChar): Boolean; overload;
function OpenObject(Value: string): Boolean; overload;

//Raise the last Exception
procedure RaiseLastWin32; overload;
procedure RaiseLastWin32(Text: string); overload;
//Raise the last Exception with a small comment from your part

//Same as linux function ;)
procedure PError(Text: string);

//Return the maximum of three integers
function GetMax(I, J, K: Integer): Integer;

//Return the minimum of three integers
function GetMin(I, J, K: Integer): Integer;

//Convert RGB Values to HSV
procedure RGBToHSV(r, g, b: Integer; var h, s, v: Integer);

{ GetFileVersion returns the most significant 32 bits of a file's binary
  version number. Typically, this includes the major and minor version placed
  together in one 32-bit Integer. It generally does not include the release
  or build numbers. It returns 0 if it failed. }
function GetFileVersion(const AFilename: string): Cardinal;
{$EXTERNALSYM GetFileVersion}

//Get version of Shell.dll
function GetShellVersion: Cardinal;
{$EXTERNALSYM GetShellVersion}

// set the background wallpaper (two versions)
procedure SetWallpaper(Path: string); overload;
procedure SetWallpaper(Path: string; Style: TJvWallpaperStyle); overload;

// screen capture functions
function CaptureScreen: TBitmap; overload;
function CaptureScreen(Rec: TRect): TBitmap; overload;

// CD functions
procedure OpenCdDrive;
procedure CloseCdDrive;

// bitmap manipulation functions
// NOTE: returned bitmap must be freed by caller!
// get red channel bitmap
function GetRBitmap(Value: TBitmap): TBitmap;
// get green channel bitmap
function GetGBitmap(Value: TBitmap): TBitmap;
// get blue channel bitmap
function GetBBitmap(Value: TBitmap): TBitmap;
// get monochrome bitmap
function GetMonochromeBitmap(Value: TBitmap): TBitmap;
// get hue bitmap (h part of hsv)
function GetHueBitmap(Value: TBitmap): TBitmap;
// get saturation bitmap (s part of hsv)
function GetSaturationBitmap(Value: TBitmap): TBitmap;
// get value bbitmap (v part of hsv)
function GetValueBitmap(Value: TBitmap): TBitmap;
// hides / shows the a forms caption area
procedure HideFormCaption(FormHandle: THandle; Hide: Boolean);
// launches the specified CPL file
// format: <Filename> [,@n] or [,,m] or [,@n,m]
// where @n = zero-based index of the applet to start (if there is more than one
// m is the zero-based index of the tab to display
procedure LaunchCpl(FileName: string);

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

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TCustomImageList = nil): Boolean;
{ GetControlPanelApplet works like GetControlPanelApplets, with the difference that it only loads and searches one cpl file (according to AFilename).
  Note though, that some CPL's contains multiple applets, so the Strings and Images lists can contain multiple return values.
  The function returns True if any Control Panel Applets were found in AFilename (i.e if items were added to Strings)
}
function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TCustomImageList = nil): Boolean;

// execute a program without waiting
procedure Exec(FileName, Parameters, Directory: string);
// execute a program and wait for it to finish
procedure ExecuteAndWait(FileName: string; Visibility: Integer);
{ Execute executes other program and waiting for it
  terminating, then return its Exit Code }
function Execute(const CommandLine, WorkingDirectory: string): Integer;

// returns True if Drive is accessible
function DiskInDrive(Drive: Char): Boolean;
// returns True if this is the first instance of the program that is running
function FirstInstance(const ATitle: string): Boolean;
// restores a window based on it's classname and Caption. Either can be left empty
// to widen the search
procedure RestoreOtherInstance(MainFormClassName, MainFormCaption: string);

// manipulate the traybar and start button
procedure HideTraybar;
procedure ShowTraybar;
procedure ShowStartButton;
procedure HideStartButton;

// (rom) SC_MONITORPOWER is documented as Windows 95 only
// (rom) better do some testing
// set monitor functions
procedure MonitorOn;
procedure MonitorOff;
procedure LowPower;

// send a key to the window named AppName
function SendKey(AppName: string; Key: Char): Boolean;

// associates an extension to a specific program
procedure AssociateExtension(IconPath, ProgramName, Path, Extension: string);

function GetRecentDocs: TStringList;
procedure AddToRecentDocs(const Filename: string);
// create a region from a bitmap
function RegionFromBitmap(const Image: TBitmap): HRGN;

// returns a list of all windows currently visible, the Objects property is filled with their window handle
procedure GetVisibleWindows(List: Tstrings);

// JvComponentFunctions
{-----------------------------------------------------------------------------
Comments:
  Functions pulled out of MemoEx, used in MemoEx.pas and TypedEdit.pas

  This unit has low internal cohesion (ie it contains routines that do all kinds of stuff)
  Some are very good candidates for wider reuse
  some are quite specific to the controls
  and in a larger library this unit would be broken up

  I have tried to group related functions together
}

function CharIsMoney(const Ch: Char): Boolean;

{ there is a STrToIntDef provided by Delphi, but no "safe" versions of
  StrToFloat or StrToCurr }
function StrToFloatDef(const Str: string; Def: Extended): Extended;
function StrToCurrDef(const Str: string; Def: Currency): Currency;

{ GetChangedText works out the new text given the current cursor pos & the key pressed
  It is not very useful in other contexts,
  but it is in this unit as it is needed in both MemoEx and TypedEdit }
function GetChangedText(const Text: string; SelStart, SelLength: Integer; Key: Char): string;

function MakeYear4Digit(Year, Pivot: Integer): Integer;

function StrIsInteger(const S: string): Boolean;
function StrIsFloatMoney(const Ps: string): Boolean;
function StrIsDateTime(const Ps: string): Boolean;

function PreformatDateString(Ps: string): string;

function BooleanToInteger(const Pb: Boolean): Integer;
function StringToBoolean(const Ps: string): Boolean;

function SafeStrToDateTime(const Ps: string): TDateTime;
function SafeStrToDate(const Ps: string): TDateTime;
function SafeStrToTime(const Ps: string): TDateTime;

function StrDelete(const psSub, psMain: string): string;

{ listview functions }
function ConvertStates(const State: Integer): TItemStates;

function ChangeHasDeselect(const peOld, peNew: TItemStates): Boolean;
function ChangeHasSelect(const peOld, peNew: TItemStates): Boolean;

function ChangeHasDefocus(const peOld, peNew: TItemStates): Boolean;
function ChangeHasFocus(const peOld, peNew: TItemStates): Boolean;

function GetListItemColumn(const pcItem: TListItem; piIndex: Integer): string;

{ returns the sum of pc.Left, pc.Width and piSpace}
function ToRightOf(const pc: TControl; piSpace: Integer = 0): Integer;
{ sets the top of pc to be in the middle of pcParent }
procedure CenterHeight(const pc, pcParent: TControl);
{ returns the fractional value of pcValue}
function TimeOnly(pcValue: TDateTime): TTime;
{ returns the integral value of pcValue }
function DateOnly(pcValue: TDateTime): TDate;

type
  TdtKind = (dtkDateOnly, dtkTimeOnly, dtkDateTime);

const
  { TDateTime value used to signify Null value}
  NullEquivalentDate: TDateTime = 0.0;

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
// Replacement for Win32Check to avoid platform specific warnings in D6
function OSCheck(RetVal: Boolean): Boolean;

{ Shortens a fully qualified Path name so that it can be drawn with a specified length limit.
  Same as FileCtrl.MinimizeName in functionality (but not implementation). Included here to
  not be forced to use FileCtrl unnecessarily }
function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;

{ RunDLL32 runs a function in a DLL using the utility rundll32.exe (on NT) or rundll.exe (on Win95/98)
 ModuleName is the name of the DLL to load, FuncName is the function to call and CmdLine is
 the command-line parameters (if any) to send to the function. Set WaitForCompletion to False to
 return immediately after the call.
 CmdShow should be one of the SW_SHOWXXXX constants and defaults SW_SHOWDEFAULT
 Return value:
 if WaitForCompletion is True, returns True if the wait didn't return WAIT_FAILED
 if WaitForCompletion is False, returns True if the process could be created
 To get information on why RunDLL32 might have failed, call GetLastError
 To get more info on what can actually be called using rundll32.exe, take a look at
 http://www.dx21.com/SCRIPTING/RUNDLL32/REFGUIDE.ASP?NTI=4&SI=6
}
type
  // the signature of procedures in DLL's that can be called using rundll32.exe
  TRunDLL32Proc = procedure(Handle: HWND; hInstance: HMODULE; CmdLine: PChar; CmdShow: Integer); stdcall;

function RunDLL32(const ModuleName, FuncName, CmdLine: string; WaitForCompletion: Boolean; CmdShow: Integer =
  SW_SHOWDEFAULT): Boolean;
{ RunDll32Internal does the same as RunDLL32 but does not use the RunDLL32.exe application to do it.
 Rather it loads the DLL, gets a pointer to the function in FuncName and calls it with the given parameters.
 Because of this behaviour, RunDll32Internal works slightly different from RunDLL32:
 * It doesn't return any value indicating success/failure
 * There is no WaitForCompletion parameter (but see comment below on how to circumvent this)
 * You must pass in a valid windows handle in Wnd. Note that if you pass 0, the call might fail, with no indication of why.
 * To simulate WaitForCompletion = False, pass the return value of GetDesktopWindow as the Wnd parameter,
 * To simulate WaitForCompletion = True, pass the handle of the calling window (f ex the form you are calling the procedure from)
 * If you try to call a function in a DLL that doesn't use the TRunDLL32Proc signature, your program
   might crash. Using the RunDLL32 function protects you from any problems with calling the wrong functions
   (a dialog is displayed if do something wrong)
 * RunDll32Internal is slightly faster but RunDLL32 is safer
}
procedure RunDll32Internal(Wnd: HWnd; const DLLName, FuncName, CmdLine: string; CmdShow: Integer = SW_SHOWDEFAULT);
{ GetDLLVersion loads DLLName, gets a pointer to the DLLVersion function and calls it, returning the major and minor version values
from the function. Returns False if the DLL couldn't be loaded or if GetDLLVersion couldn't be found. }
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;

{$IFNDEF COMPILER6_UP}
{ D5 compatibility functions }
procedure RaiseLastOSError;
function IncludeTrailingPathDelimiter(const APath: string): string;
function ExcludeTrailingPathDelimiter(const APath: string): string;
{$ENDIF}

{ from JvVCLUtils }

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
function CreateDisabledBitmap_NewStyle(FOriginal: TBitmap; BackColor: TColor): TBitmap;
function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor): TBitmap;
function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap; Cols, Rows,
  Index: Integer);
procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighlightColor, GrayColor: TColor; DrawHighlight: Boolean);

function MakeIcon(ResID: PChar): TIcon;
function MakeIconID(ResID: Word): TIcon;
function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;

{ Service routines }

procedure NotImplemented;
procedure ResourceNotFound(ResID: PChar);
function PointInRect(const P: TPoint; const R: TRect): Boolean;
function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
function PaletteColor(Color: TColor): Longint;
function WidthOf(R: TRect): Integer;
function HeightOf(R: TRect): Integer;
procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
procedure CopyParentImage(Control: TControl; Dest: TCanvas);
procedure Delay(MSecs: Longint);
procedure CenterControl(Control: TControl);
procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
function MakeVariant(const Values: array of Variant): Variant;
function CreateRotatedFont(Font: TFont; Angle: Integer): HFONT;
function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
function MsgDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
{$IFDEF CBUILDER}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): HWND;
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): HWND;
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF CBUILDER}
function IsForegroundTask: Boolean;
procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
function GetAveCharSize(Canvas: TCanvas): TPoint;
function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
procedure FreeUnusedOle;
procedure Beep;
function GetWindowsVersion: string;
function LoadDLL(const LibName: string): THandle;
function RegisterServer(const ModuleName: string): Boolean;

{ Gradient filling routine }

type
  TFillDirection = (fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft);

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);

{ String routines }

function GetEnvVar(const VarName: string): string;
function AnsiUpperFirstChar(const S: string): string;
function StringToPChar(var S: string): PChar;
function StrPAlloc(const S: string): PChar;
procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
function DropT(const S: string): string;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
procedure FreeMemo(var fpBlock: Pointer);
function GetMemoSize(fpBlock: Pointer): Longint;
function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean;

{ Manipulate huge pointers routines }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);

{ Standard Windows colors that are not defined by Delphi }

const
  clCream = TColor($A6CAF0);
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($FFFBF0);

const
  WaitCursor: TCursor = crHourGlass;

procedure StartWait;
procedure StopWait;
function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
function LoadAniCursor(Instance: THandle; ResID: PChar): HCURSOR;

{ Windows API level routines }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPALETTE;
  TransparentColor: TColorRef);
procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBITMAP;
  DstX, DstY: Integer; TransparentColor: TColorRef);
function PaletteEntries(Palette: HPALETTE): Integer;
function WindowClassName(Wnd: HWND): string;
function ScreenWorkArea: TRect;
procedure SwitchToWindow(Wnd: HWND; Restore: Boolean);
procedure ActivateWindow(Wnd: HWND);
procedure ShowWinNoAnimate(Handle: HWND; CmdShow: Integer);
procedure CenterWindow(Wnd: HWND);
procedure ShadeRect(DC: HDC; const Rect: TRect);
procedure KillMessage(Wnd: HWND; Msg: Cardinal);

{ Convert dialog units to pixels and backwards }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
function DialogUnitsToPixelsY(DlgUnits: Word): Word;
function PixelsToDialogUnitsX(PixUnits: Word): Word;
function PixelsToDialogUnitsY(PixUnits: Word): Word;

{ Grid drawing }

type
  TVertAlignment = (vaTopJustify, vaCenter, vaBottomJustify);

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean; ARightToLeft: Boolean = False);
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean);overload;
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean); overload;
procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);

type
  TJvScreenCanvas = class(TCanvas)
  private
    FDeviceContext: HDC;
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
function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass; const Tag: Integer): TComponent;
{ ControlAtPos2 equal to TWinControl.ControlAtPos function,
  but works better }
function ControlAtPos2(Parent: TWinControl; X, Y: Integer): TControl;
{ RBTag searches WinControl.Controls for checked
  RadioButton and returns its Tag property value }
function RBTag(Parent: TWinControl): Integer;
{ FindFormByClass returns first form with specified
  class, FormClass, owned by Application global variable }
function FindFormByClass(FormClass: TFormClass): TForm;
function FindFormByClassName(FormClassName: string): TForm;
{ AppMinimized returns True, if Application is minimized }
function AppMinimized: Boolean;
{ MessageBox is Application.MessageBox with string (not PChar) parameters.
  if Caption parameter = '', it replaced with Application.Title }
function MessageBox(const Msg: string; Caption: string;
  const Flags: Integer): Integer;
function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext: Integer; Control: TWinControl): Integer;
function MsgDlgDef(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpContext: Integer;
  Control: TWinControl): Integer;
{**** Windows routines }

{ SetWindowTop put window to top without recreating window }
procedure SetWindowTop(const Handle: HWND; const Top: Boolean);
{**** Graphic routines - }


{ LoadIcoToImage loads two icons from resource named NameRes,
  into two image lists ALarge and ASmall}
procedure LoadIcoToImage(ALarge, ASmall: TCustomImageList; const NameRes: string);

{ DefineCursor load cursor from resource, and return
  available cursor number, assigned to it }
  // (p3) - this was previously JvUtils.DefineCursor
function DefineCursor2(Identifier: PChar): TCursor;

procedure CenterHor(Parent: TControl; MinLeft: Integer; Controls: array of TControl);
procedure EnableControls(Control: TWinControl; const Enable: Boolean);
procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: Integer; const Enable: Boolean);
procedure ExpandWidth(Parent: TControl; MinWidth: Integer; Controls: array of TControl);
function PanelBorder(Panel: TCustomPanel): Integer;
function Pixels(Control: TControl; APixels: Integer): Integer;

type
  TMenuAnimation = (maNone, maRandom, maUnfold, maSlide);

procedure ShowMenu(Form: TForm; MenuAni: TMenuAnimation);

{ TargetFileName - if FileName is ShortCut returns filename ShortCut linked to }
function TargetFileName(const FileName: TFileName): TFileName;
{ return filename ShortCut linked to }
function ResolveLink(const hWnd: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;

type
  TProcObj = procedure of object;

procedure ExecAfterPause(Proc: TProcObj; Pause: Integer);
{ BrowseForFolder displays Browse For Folder dialog }
function BrowseForFolder(const Handle: HWND; const Title: string; var Folder: string): Boolean;


{ end JvUtils }

{ begin JvAppUtils}
function GetDefaultSection(Component: TComponent): string;
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;

var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;

function GetDefaultIniRegKey: string;

function FindForm(FormClass: TFormClass): TForm;
function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
function ShowDialog(FormClass: TFormClass): Boolean;
function InstantiateForm(FormClass: TFormClass; var Reference): TForm;

procedure SaveFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore;
  SaveState: Boolean = True; SavePosition: Boolean = True);
procedure RestoreFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore;
  LoadState: Boolean = True; LoadPosition: Boolean = True);

procedure SaveMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore);
procedure RestoreMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore);
procedure RestoreGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore);
procedure SaveGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore);

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

function StrToIniStr(const Str: string): string;
function IniStrToStr(const Str: string): string;

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

procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);

procedure AppTaskbarIcons(AppOnly: Boolean);

{ Internal using utilities }

procedure InternalSaveFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string; SaveState: Boolean = True; SavePosition: Boolean = True);
procedure InternalRestoreFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string; LoadState: Boolean = True; LoadPosition: Boolean = True);
procedure InternalSaveGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore;
  const StorePath: string);
procedure InternalRestoreGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore;
  const StorePath: string);
procedure InternalSaveMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string);
procedure InternalRestoreMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string);

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
procedure SaveBitmapToFile(const Filename: string; Bitmap: TBitmap;
  Colors: Integer);

function ScreenPixelFormat: TPixelFormat;
function ScreenColorCount: Integer;

procedure TileImage(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
function ZoomImage(ImageW, ImageH, MaxW, MaxH: Integer; Stretch: Boolean): TPoint;

var
  DefaultMappingMethod: TMappingMethod = mmHistogram;

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

{ begin JvFileUtil }
procedure CopyFile(const FileName, DestName: string; ProgressControl: TControl);
procedure CopyFileEx(const FileName, DestName: string;
  OverwriteReadOnly, ShellDialog: Boolean; ProgressControl: TControl);
procedure MoveFile(const FileName, DestName: TFileName);
procedure MoveFileEx(const FileName, DestName: TFileName; ShellDialog: Boolean);

{ end JvFileUtil }

{ begin JvCtrlUtils }
function IntToExtended(I: Integer): Extended;

//------------------------------------------------------------------------------
// ToolBarMenu
//------------------------------------------------------------------------------

procedure JvCreateToolBarMenu(AForm: TForm; AToolBar: TToolBar; AMenu: TMainMenu = nil);

//------------------------------------------------------------------------------
// ListView functions
//------------------------------------------------------------------------------

type
  PJvLVItemStateData = ^TJvLVItemStateData;
  TJvLVItemStateData = record
    Caption: string;
    Data: Pointer;
    Focused, Selected: Boolean;
  end;

procedure JvListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean = False; Headers: Boolean = True);

function JvListViewSafeSubItemString(Item: TListItem; SubItemIndex: Integer): string;

procedure JvListViewSortClick(Column: TListColumn; AscendingSortImage: Integer = -1;
  DescendingSortImage: Integer = -1);

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

procedure UpdateTrackFont(TrackFont,Font:TFont;TrackOptions:TJvTrackFontOptions);
// Returns the size of the image
// used for checkboxes and radiobuttons.
// Originally from Mike Lischke
function GetDefaultCheckBoxSize:TSize;

implementation

uses
  Consts, SysConst, CommCtrl, MMSystem, ShlObj, ActiveX, Math, 
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF}
  JclSysInfo,  JclGraphics,
  JvConsts, JvJCLUtils, JvProgressUtils;

resourcestring
  SWin32Error = 'Win32 Error.  Code: %d.'#10'%s';
  RsNotForMdi = 'MDI forms are not allowed';

const
  RC_ControlRegistry = 'Control Panel\Desktop';
  RC_WallpaperStyle = 'WallpaperStyle';
  RC_WallpaperRegistry = 'Wallpaper';
  RC_TileWallpaper = 'TileWallpaper';
  RC_OpenCDDrive = 'set cdaudio door open wait';
  RC_CloseCDDrive = 'set cdaudio door closed wait';
  RC_RunCpl = 'rundll32.exe shell32,Control_RunDLL ';
  RC_ShellName = 'Shell_TrayWnd';
  RC_DefaultIcon = 'DefaultIcon';

var
  ShellVersion: Integer;

{$IFNDEF COMPILER6_UP}

{ (rb) Duplicate of JclBase.RaiseLastOSError }

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function IncludeTrailingPathDelimiter(const APath: string): string;
begin
  if (Length(APath) > 0) and (APath[Length(APath)] <> '\') then
    Result := APath + '\'
  else
    Result := APath;
end;

function ExcludeTrailingPathDelimiter(const APath: string): string;
begin
  Result := APath;
  while (Length(Result) > 0) and (Result[Length(Result)] = '\') do
    SetLength(Result, Length(Result) - 1);
end;

{$ENDIF}

function IconToBitmap(Ico: HICON): TBitmap;
var
  Pic: TPicture;
begin
  Pic := TPicture.Create;
  Pic.Icon.Handle := Ico;
  Result := TBitmap.Create;
  Result.Height := Pic.Icon.Height;
  Result.Width := Pic.Icon.Width;
  Result.Canvas.Draw(0, 0, Pic.Icon);
  Pic.Free;
end;

function IconToBitmap2(Ico: HICON; Size: Integer = 32; TransparentColor: TColor = clNone): TBitmap;
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

function IconToBitmap3(Ico: HICON; Size: Integer = 32; TransparentColor: TColor = clNone): TBitmap;
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
    // fill the bitmap with the transparant color
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

function OpenObject(Value: string): Boolean;
begin
  Result := OpenObject(PChar(Value));
end;

{ (rb) Duplicate of JvFunctions.Exec }

function OpenObject(Value: PChar): Boolean;
begin
  Result := ShellExecute(0, 'open', Value, nil, nil, SW_SHOWNORMAL) > HINSTANCE_ERROR;
end;

procedure RaiseLastWin32;
begin
  PError('');
end;

procedure RaiseLastWin32(Text: string);
begin
  PError(Text);
end;

procedure PError(Text: string);
var
  LastError: Integer;
  St: string;
begin
  LastError := GetLastError;
  if LastError <> 0 then
  begin
    St := Format(SWin32Error, [LastError, SysErrorMessage(LastError)]);
    if Text <> '' then
      St := Text + ':' + St;
    raise EOSError.Create(St);
  end;
end;

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

procedure RGBToHSV(r, g, b: Integer; var h, s, v: Integer);
var
  Delta: Integer;
  Min, Max: Integer;
begin
  Min := GetMin(r, g, b);
  Max := GetMax(r, g, b);
  v := Max;
  Delta := Max - Min;
  if Max = 0 then
    s := 0
  else
    s := (255 * Delta) div Max;
  if s = 0 then
    h := 0
  else
  begin
    if r = Max then
      h := (60 * (g - b)) div Delta
    else
      if g = Max then
      h := 120 + (60 * (b - r)) div Delta
    else
      h := 240 + (60 * (r - g)) div Delta;
    if h < 0 then
      h := h + 360;
  end;
end;

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := 0;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result := FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function GetShellVersion: Cardinal;
begin
  if ShellVersion = 0 then
    ShellVersion := GetFileVersion('shell32.dll');
  Result := ShellVersion;
end;

procedure SetWallpaper(Path: string);
begin
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE);
end;

procedure SetWallpaper(Path: string; Style: TJvWallpaperStyle);
begin
  with TRegistry.Create do
  begin
    OpenKey(RC_ControlRegistry, False);
    case Style of
      wpTile:
        begin
          WriteString(RC_TileWallpaper, '1');
          WriteString(RC_Wallpaperstyle, '0');
        end;
      wpCenter:
        begin
          WriteString(RC_TileWallpaper, '0');
          WriteString(RC_Wallpaperstyle, '0');
        end;
      wpStretch:
        begin
          WriteString(RC_TileWallpaper, '0');
          WriteString(RC_Wallpaperstyle, '2');
        end;
    end;
    WriteString(RC_WallpaperRegistry, Path);
    Free;
  end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

function CaptureScreen(Rec: TRect): TBitmap;
const
  NumColors = 256;
var
  R: TRect;
  C: TCanvas;
  LP: PLogPalette;
  TmpPalette: HPalette;
  Size: Integer;
  Img: TImage; // (p3) change to Bmp?
begin
  Img := TImage.Create(nil);
  try
    Img.Width := Rec.Right - Rec.Left;
    Img.Height := Rec.Bottom - Rec.Top;
    R := Rec;
    C := TCanvas.Create;
    try
      C.Handle := GetDC(HWND_DESKTOP);
      Img.Canvas.CopyRect(Rect(0, 0, Rec.Right - Rec.Left, Rec.Bottom - Rec.Top), C, R);
      Size := SizeOf(TLogPalette) + (Pred(NumColors) * SizeOf(TPaletteEntry));
      LP := AllocMem(Size);
      try
        LP^.palVersion := $300;
        LP^.palNumEntries := NumColors;
        GetSystemPaletteEntries(C.Handle, 0, NumColors, LP^.palPalEntry);
        TmpPalette := CreatePalette(LP^);
        Img.Picture.Bitmap.Palette := TmpPalette;
        DeleteObject(TmpPalette);
      finally
        FreeMem(LP, Size);
      end
    finally
      ReleaseDC(HWND_DESKTOP, C.Handle);
      C.Free;
    end;
    Result := TBitmap.Create;
    Result.Assign(Img.Picture.Bitmap);
  finally
    Img.Free;
  end;
end;

function CaptureScreen: TBitmap;
begin
  Result := CaptureScreen(Rect(0, 0, Screen.Width, Screen.Height));
end;

{ (rb) Duplicate of JclMultimedia.OpenCloseCdDrive ?? }

procedure OpenCdDrive;
begin
  mciSendString(PChar(RC_OpenCDDrive), nil, 0, GetForegroundWindow);
end;

procedure CloseCdDrive;
begin
  mciSendString(PChar(RC_CloseCDDrive), nil, 0, GetForegroundWindow);
end;

function GetRBitmap(Value: TBitmap): TBitmap;
var
  I, J: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for J := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[J];
    rowB := Result.Scanline[J];
    for I := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[I].rgbtRed := rowRGB[I].rgbtRed;
      TRGBArray(rowB^)[I].rgbtGreen := 0;
      TRGBArray(rowB^)[I].rgbtBlue := 0;
    end;
  end;
end;

function GetBBitmap(Value: TBitmap): TBitmap;
var
  I, J: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for J := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[J];
    rowB := Result.Scanline[J];
    for I := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[I].rgbtRed := 0;
      TRGBArray(rowB^)[I].rgbtGreen := 0;
      TRGBArray(rowB^)[I].rgbtBlue := rowRGB[I].rgbtBlue;
    end;
  end;
end;

function GetGBitmap(Value: TBitmap): TBitmap;
var
  I, J: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for J := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[J];
    rowB := Result.Scanline[J];
    for I := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[I].rgbtRed := 0;
      TRGBArray(rowB^)[I].rgbtGreen := rowRGB[I].rgbtGreen;
      TRGBArray(rowB^)[I].rgbtBlue := 0;
    end;
  end;
end;

function GetHueBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, I, J: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for J := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[J];
    rowS := Result.Scanline[J];
    for I := Value.Width - 1 downto 0 do
    begin
      with rowRGB[I] do
        RGBToHSV(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[I].rgbtBlue := h;
      rowS[I].rgbtGreen := h;
      rowS[I].rgbtRed := h;
    end;
  end;
end;

function GetMonochromeBitmap(Value: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Assign(Value);
  Result.Monochrome := True;
end;

function GetSaturationBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, I, J: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for J := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[J];
    rowS := Result.Scanline[J];
    for I := Value.Width - 1 downto 0 do
    begin
      with rowRGB[I] do
        RGBToHSV(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[I].rgbtBlue := s;
      rowS[I].rgbtGreen := s;
      rowS[I].rgbtRed := s;
    end;
  end;
end;

function GetValueBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, I, J: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for J := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[J];
    rowS := Result.Scanline[J];
    for I := Value.Width - 1 downto 0 do
    begin
      with rowRGB[I] do
        RGBToHSV(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[I].rgbtBlue := v;
      rowS[I].rgbtGreen := v;
      rowS[I].rgbtRed := v;
    end;
  end;
end;

{ (rb) Duplicate of JvAppUtils.AppTaskbarIcons }

procedure HideFormCaption(FormHandle: THandle; Hide: Boolean);
begin
  if Hide then
    SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) and not WS_CAPTION)
  else
    SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) or WS_CAPTION);
end;

procedure LaunchCpl(FileName: string);
begin
  // rundll32.exe shell32,Control_RunDLL ';
  RunDLL32('shell32.dll', 'Control_RunDLL', Filename, True);
  //  WinExec(PChar(RC_RunCpl + FileName), SW_SHOWNORMAL);
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
  TCPLApplet = function(hwndCPl: THandle; uMsg: DWORD;
    lParam1, lParam2: Longint): Longint; stdcall;

  TCPLInfo = packed record
    idIcon: Integer;
    idName: Integer;
    idInfo: Integer;
    lData: Longint;
  end;

  TNewCPLInfoA = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwHelpContext: DWORD;
    lData: Longint;
    hIcon: HICON;
    szName: array [0..31] of AnsiChar;
    szInfo: array [0..63] of AnsiChar;
    szHelpFile: array [0..127] of AnsiChar;
  end;
  TNewCPLInfoW = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwHelpContext: DWORD;
    lData: Longint;
    hIcon: HICON;
    szName: array [0..31] of WideChar;
    szInfo: array [0..63] of WideChar;
    szHelpFile: array [0..127] of WideChar;
  end;

function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TCustomImageList = nil): Boolean;
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
  hWnd:THandle;
begin
  Result := False;
  hLib := SafeLoadLibrary(AFilename);
  if hLib = 0 then
    Exit;
  hWnd := GetForegroundWindow;
  TmpCount := Strings.Count;
  try
    @CplCall := GetProcAddress(hLib, PChar(cCplAddress));
    if @CplCall = nil then
      Exit;
    CplCall(hWnd, CPL_INIT, 0, 0); // Init the *.cpl file
    try
      Count := CplCall(hWnd, CPL_GETCOUNT, 0, 0);
      for I := 0 to Count - 1 do
      begin
        FillChar(InfoW, SizeOf(InfoW), 0);
        FillChar(InfoA, SizeOf(InfoA), 0);
        FillChar(CPLInfo, SizeOf(CPLInfo), 0);
        S := '';
        CplCall(hWnd, CPL_NEWINQUIRE, I, Longint(@InfoW));
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
            CplCall(hWnd, CPL_INQUIRE, I, Longint(@CPLInfo));
            LoadStringA(hLib, CPLInfo.idName, InfoA.szName, SizeOf(InfoA.szName));
            hIco := LoadImage(hLib, PChar(CPLInfo.idIcon), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
            S := string(InfoA.szName);
          end;
        end;
        if S <> '' then
        begin
          S := Format('%s=%s,@%d', [S, AFilename, I]);
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
      CplCall(hWnd, CPL_EXIT, 0, 0);
    end;
  finally
    FreeLibrary(hLib);
  end;
end;

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TCustomImageList = nil): Boolean;
var
  H: THandle;
  F: TSearchRec;
begin
  H := FindFirst(IncludeTrailingPathDelimiter(APath) + AMask, faAnyFile, F);
  if Images <> nil then
  begin
    Images.Clear;
    Images.BkColor := clMenu;
  end;
  if Strings <> nil then
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
end;

procedure Exec(FileName, Parameters, Directory: string);
var
  Operation: string;
begin
  Operation := 'open';
  ShellExecute(GetForegroundWindow, PChar(Operation), PChar(FileName), PChar(Parameters), PChar(Directory),
    SW_SHOWNORMAL);
end;

{ (rb) Duplicate of JclMiscel.WinExec32AndWait }

procedure ExecuteAndWait(FileName: string; Visibility: Integer);
var
  zAppName: array [0..512] of Char;
  zCurDir: array [0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil, zAppName, nil, nil, False, Create_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
    nil, nil, StartupInfo, ProcessInfo) then
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
end;

// (rom) a thread to wait would be more elegant, also JCL function available

function Execute(const CommandLine, WorkingDirectory: string): Integer;
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


{ (rb) Duplicate of JclFileUtils.DiskInDrive }

function DiskInDrive(Drive: Char): Boolean;
var
  DrvNum: Byte;
  EMode: Word;
begin
  DrvNum := Ord(Drive);
  if DrvNum >= Ord('a') then
    Dec(DrvNum, $20);
  EMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := DiskSize(DrvNum - $40) <> -1;
  finally
    SetErrorMode(EMode);
  end;
end;

function FirstInstance(const ATitle: string): Boolean;
var
  Mutex: THandle;
begin
  Mutex := CreateMutex(nil, False, PChar(ATitle));
  try
    Result := (Mutex <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS);
  finally
    ReleaseMutex(Mutex);
  end;
end;

procedure RestoreOtherInstance(MainFormClassName, MainFormCaption: string);
var
  OtherWnd, OwnerWnd: HWND;
begin
  OtherWnd := FindWindow(PChar(MainFormClassName), PChar(MainFormCaption));
  ShowWindow(OtherWnd, SW_SHOW); //in case the window was not visible before

  OwnerWnd := 0;
  if OtherWnd <> 0 then
    OwnerWnd := GetWindow(OtherWnd, GW_OWNER);

  if OwnerWnd <> 0 then
    OtherWnd := OwnerWnd;

  if OtherWnd <> 0 then
  begin
    { (rb) Use JvVCLUtils.SwitchToWindow }
    if IsIconic(OtherWnd) then
      ShowWindow(OtherWnd, SW_RESTORE);

    SetForegroundWindow(OtherWnd);
  end;
end;

procedure HideTraybar;
var
  Wnd: HWND;
begin
  Wnd := FindWindow(PChar(RC_ShellName), nil);
  ShowWindow(Wnd, SW_HIDE);
end;

procedure ShowTraybar;
var
  Wnd: HWND;
begin
  Wnd := FindWindow(PChar(RC_ShellName), nil);
  ShowWindow(Wnd, SW_SHOW);
end;

procedure HideStartBtn(Visible: Boolean);
var
  Tray, Child: HWND;
  C: array [0..127] of Char;
  S: string;
begin
  Tray := FindWindow(PChar(RC_ShellName), nil);
  Child := GetWindow(Tray, GW_CHILD);
  while Child <> 0 do
  begin
    if GetClassName(Child, C, SizeOf(C)) > 0 then
    begin
      S := StrPas(C);
      if UpperCase(S) = 'BUTTON' then
        if Visible then
          ShowWindow(Child, SW_SHOWNORMAL)
        else
          ShowWindow(Child, SW_HIDE);
    end;
    Child := GetWindow(Child, GW_HWNDNEXT);
  end;
end;

procedure ShowStartButton;
begin
  HideStartBtn(True);
end;

procedure HideStartButton;
begin
  HideStartBtn(False);
end;

procedure MonitorOn;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, -1);
end;

procedure MonitorOff;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, 2);
end;

procedure LowPower;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, 1);
end;

{$WARNINGS OFF}

procedure SendShift(H: HWND; Down: Boolean);
var
  vKey, ScanCode: Word;
  lParam: Longint;
begin
  vKey := VK_SHIFT;
  ScanCode := MapVirtualKey(vKey, 0);
  lParam := Longint(ScanCode) shl 16 or 1;
  if not Down then
    lParam := lParam or $C0000000;
  SendMessage(H, WM_KEYDOWN, vKey, lParam);
end;

procedure SendCtrl(H: HWND; Down: Boolean);
var
  vKey, ScanCode: Word;
  lParam: Longint;
begin
  vKey := VK_CONTROL;
  ScanCode := MapVirtualKey(vKey, 0);
  lParam := Longint(ScanCode) shl 16 or 1;
  if not Down then
    lParam := lParam or $C0000000;
  SendMessage(H, WM_KEYDOWN, vKey, lParam);
end;

function SendKey(AppName: string; Key: Char): Boolean;
var
  vKey, ScanCode: Word;
  lParam, ConvKey: Longint;
  Shift, Ctrl: Boolean;
  H: HWND;
begin
  H := FindWindow(PChar(AppName), nil);
  if H <> 0 then
  begin
    ConvKey := OemKeyScan(Ord(Key));
    Shift := (ConvKey and $00020000) <> 0;
    Ctrl := (ConvKey and $00040000) <> 0;
    ScanCode := ConvKey and $000000FF or $FF00;
    vKey := Ord(Key);
    lParam := Longint(ScanCode) shl 16 or 1;
    if Shift then
      SendShift(H, True);
    if Ctrl then
      SendCtrl(H, True);
    SendMessage(H, WM_KEYDOWN, vKey, lParam);
    SendMessage(H, WM_CHAR, vKey, lParam);
    lParam := lParam or $C0000000;
    SendMessage(H, WM_KEYUP, vKey, lParam);
    if Shift then
      SendShift(H, False);
    if Ctrl then
      SendCtrl(H, False);
    Result := True;
  end
  else
    Result := False;
end;

{$WARNINGS ON}

procedure RebuildIconCache;
var
  Dummy: DWORD;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    Longint(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, Dummy);
end;

procedure AssociateFileExtension(IconPath, ProgramName, Path, Extension: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(ProgramName, True);
    WriteString('', ProgramName);
    if IconPath <> '' then
    begin
      OpenKey(RC_DefaultIcon, True);
      WriteString('', IconPath);
    end;
    CloseKey;
    OpenKey(ProgramName, True);
    OpenKey('shell', True);
    OpenKey('open', True);
    OpenKey('command', True);
    WriteString('', '"' + Path + '" "%1"');
    Free;
  end;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + extension, True);
    WriteString('', ProgramName);
    Free;
  end;
  RebuildIconCache;
end;

procedure AssociateExtension(IconPath, ProgramName, Path, Extension: string);
begin
  AssociateFileExtension(IconPath, ProgramName, Path, Extension);
end;

function GetRecentDocs: TStringList;
var
  Path: string;
  t: TSearchRec;
  Res: Integer;
begin
  Result := TStringList.Create;
  Result.Clear;
  Path := INcludeTrailingPathDelimiter(GetRecentFolder);
  //search for all files
  Res := FindFirst(Path + '*.*', faAnyFile, t);
  try
    while Res = 0 do
    begin
      if (t.Name <> '.') and (t.Name <> '..') then
        Result.Add(Path + T.Name);
      Res := FindNext(t);
    end;
  finally
    FindClose(t);
  end;
end;

{ (rb) Duplicate of JvWinDialogs.AddToRecentDocs }

procedure AddToRecentDocs(const Filename: string);
begin
  SHAddToRecentDocs(SHARD_PATH, PChar(Filename));
end;

function RegionFromBitmap(const Image: TBitmap): HRGN;
begin
  Result := 0;
  if Assigned(Image) and not Image.Empty then
    Result := CreateRegionFromBitmap(Image, Image.Canvas.Pixels[0, 0], rmExclude);
end;

function EnumWindowsProc(Handle: THandle; lParam: TStrings): Boolean; stdcall;
var
  St: array [0..256] of Char;
  St2: string;
begin
  if IsWindowVisible(Handle) then
  begin
    GetWindowText(Handle, St, SizeOf(St));
    St2 := St;
    if St2 <> '' then
      with TStrings(lParam) do
        AddObject(St2, TObject(Handle));
  end;
  Result := True;
end;

procedure GetVisibleWindows(List: Tstrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    EnumWindows(@EnumWindowsProc, Integer(List));
  finally
    List.EndUpdate;
  end;
end;

// from JvComponentFunctions

function StrPosNoCase(const psSub, psMain: string): Integer;
begin
  Result := Pos(AnsiUpperCase(psSub), AnsiUpperCase(psMain));
end;

function StrRestOf(const Ps: string; const n: Integer): string;
begin
  Result := Copy(Ps, n, (Length(Ps) - n + 1));
end;

{!!!!!!!! use these cos the JCL one is badly broken }

{ Am using this one purely as an itnernal for StrReplace

 Replace part of a string with new text. iUpdatePos is the last update position
 i.e. the position the substr was found + the length of the replacement string + 1.
 Use 0 first time in }

function StrReplaceInstance(const psSource, psSearch, psReplace: string;
  var piUpdatePos: Integer; const pbCaseSens: Boolean): string;
var
  liIndex: Integer;
  lsCopy: string;
begin
  Result := psSource;
  if piUpdatePos >= Length(psSource) then
    Exit;
  if psSearch = '' then
    Exit;

  Result := StrLeft(psSource, piUpdatePos - 1);
  lsCopy := StrRestOf(psSource, piUpdatePos);

  if pbCaseSens then
    liIndex := Pos(psSearch, lsCopy)
  else
    liIndex := StrPosNoCase(psSearch, lsCopy);
  if liIndex = 0 then
  begin
    Result := psSource;
    piUpdatePos := Length(psSource) + 1;
    Exit;
  end;

  Result := Result + StrLeft(lsCopy, liIndex - 1);
  Result := Result + psReplace;
  piUpdatePos := Length(Result) + 1;
  Result := Result + StrRestOf(lsCopy, liIndex + Length(psSearch));
end;

function LStrReplace(const psSource, psSearch, psReplace: string;
  const pbCaseSens: Boolean): string;
var
  liUpdatePos: Integer;
begin
  liUpdatePos := 0;
  Result := psSource;
  while liUpdatePos < Length(Result) do
    Result := StrReplaceInstance(Result, psSearch, psReplace, liUpdatePos, pbCaseSens);
end;

{ if it's not a decimal point then it must be a digit, space or Currency symbol
  also always use $ for money }

function CharIsMoney(const Ch: Char): Boolean;
begin
  Result := CharIsDigit(Ch) or (Ch = AnsiSpace) or (Ch = '$') or (Ch = '-') or
    (Pos(Ch, CurrencyString) > 0);
end;

function StrToCurrDef(const Str: string; Def: Currency): Currency;
var
  lStr: string;
begin
  try
    lStr := StrStripNonNumberChars(Str);

    if lStr = '' then
      Result := Def
    else
      Result := StrToCurr(lstr);
  except
    Result := Def;
  end;
end;

function StrToFloatDef(const Str: string; Def: Extended): Extended;
var
  lStr: string;
begin
  lStr := StrStripNonNumberChars(Str);

  if lStr = '' then
    Result := Def
  else
  try
      { the string '-' fails StrToFloat, but it can be interpreted as 0  }
    if StrRight(lStr, 1) = '-' then
      lStr := lStr + '0';

      { a string that ends in a '.' such as '12.' fails StrToFloat,
       but as far as I am concerned, it may as well be interpreted as 12.0 }
    if StrRight(lStr, 1) = '.' then
      lStr := lStr + '0';
    if not TextToFloat(PChar(lStr), Result, fvExtended) then
      Result := Def;
//    Result := StrToFloat(lStr);
  except
    Result := Def;
  end;
end;

function GetChangedText(const Text: string; SelStart, SelLength: Integer; Key: Char): string;
begin
  { take the original text, replace what will be overwritten with new value }
  Result := Text;

  if SelLength > 0 then
    Delete(Result, SelStart + 1, SelLength);
  if Key <> #0 then
    Insert(Key, Result, SelStart + 1);
end;

{ "window" technique for years to translate 2 digits to 4 digits.
   The window is 100 years wide
   The pivot year is the lower edge of the window
  A pivot year of 1900 is equivalent to putting 1900 before every 2-digit year
 if pivot is 1940, then 40 is interpreted as 1940, 00 as 2000 and 39 as 2039
 The system default is 1950

 Why the reimplementation?
 JclDatetime.Make4DigitYear will fail after 2100, this won't
 note that in this implementation pivot is a 4-digit year
 I have made it accept JclDatetime.Make4DigitYear's 2 digit pivot years.
 They are expanded by adding 1900.

 It is also better in that a valid 4-digit year will pass through unchanged,
 not fail an assertion.
}

function MakeYear4Digit(Year, Pivot: Integer): Integer;
var
  Century: Integer;
begin
  if Pivot < 0 then
    raise EJVCLException.Create('JvFunctions.MakeYear4Digit: Pivot < 0');

  { map 100 to zero }
  if Year = 100 then
    Year := 0;
  if Pivot = 100 then
    Pivot := 0;

  // turn 2 digit pivot to 4 digit
  if Pivot < 100 then
    Pivot := Pivot + 1900;

  { turn 2 digit years to 4 digits }
  if (Year >= 0) and (Year < 100) then
  begin
    Century := (Pivot div 100) * 100;

    Result := Year + Century; // give the result the same century as the pivot
    if Result < Pivot then
      //  cannot be lower than the Pivot
      Result := Result + 100;
  end
  else
    Result := Year;
end;

function StrIsInteger(const S: string): Boolean;
var
  I: Integer;
  Ch: Char;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    Ch := S[I];
    if (not CharIsNumber(Ch)) or (Ch = DecimalSeparator) then //Az
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsFloatMoney(const Ps: string): Boolean;
var
  liLoop, liDots: Integer;
  Ch: Char;
begin
  Result := True;
  liDots := 0;

  for liLoop := 1 to Length(Ps) do
  begin
    { allow digits, space, Currency symbol and one decimal dot }
    Ch := Ps[liLoop];

    if Ch = DecimalSeparator then
    begin
      Inc(liDots);
      if liDots > 1 then
      begin
        Result := False;
        Break;
      end;
    end
    else
      if not CharIsMoney(Ch) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function StrIsDateTime(const Ps: string): Boolean;
const
  MIN_DATE_TIME_LEN = 6; {2Jan02 }
  MAX_DATE_TIME_LEN = 30; { 30 chars or so in '12 December 1999 12:23:23:00' }
var
  liLoop: Integer;
  Ch: Char;
  liColons, liSlashes, liSpaces, liDigits, liAlpha: Integer;
  lbDisqualify: Boolean;
begin
  if Length(Ps) < MIN_DATE_TIME_LEN then
  begin
    Result := False;
    Exit;
  end;

  if Length(Ps) > MAX_DATE_TIME_LEN then
  begin
    Result := False;
    Exit;
  end;

  lbDisqualify := False;
  liColons := 0;
  liSlashes := 0;
  liSpaces := 0;
  liDigits := 0;
  liAlpha := 0;

  for liLoop := 1 to Length(Ps) do
  begin
    Ch := Ps[liLoop];

    if Ch = ':' then
      Inc(liColons)
    else
      if Ch = AnsiForwardSlash then
      Inc(liSlashes)
    else
      if Ch = AnsiSpace then
      Inc(liSpaces)
    else
      if CharIsDigit(Ch) then
      Inc(liDigits)
    else
      if CharIsAlpha(Ch) then
      Inc(liAlpha)
    else
    begin
      // no wierd punctuation in dates!
      lbDisqualify := True;
      Break;
    end;
  end;

  Result := False;
  if not lbDisqualify then
    { a date must have colons and slashes and spaces, but not to many of each }
    if (liColons > 0) or (liSlashes > 0) or (liSpaces > 0) then
      { only 2 slashes in "dd/mm/yy" or 3 colons in "hh:mm:ss:ms" or 6 spaces "yy mm dd hh mm ss ms" }
      if (liSlashes <= 2) and (liColons <= 3) and (liSpaces <= 6) then
        { must have some digits (min 3 digits, eg in "2 jan 02", max 16 dgits in "01/10/2000 10:10:10:10"
        longest month name is 8 chars }
        if (liDigits >= 3) and (liDigits <= 16) and (liAlpha <= 10) then
          Result := True;

  { define in terms of results - if I can interpret it as a date, then I can }
  if Result then
    Result := (SafeStrToDateTime(PreformatDateString(Ps)) <> 0);
end;

function PreformatDateString(Ps: string): string;
var
  liLoop: Integer;
begin
  { turn any month names to numbers }

  { use the StrReplace in stringfunctions -
  the one in JclStrings is badly broken and brings down the app }

  for liLoop := Low(LongMonthNames) to High(LongMonthNames) do
    Ps := LStrReplace(Ps, LongMonthNames[liLoop], IntToStr(liLoop), False);

  { now that 'January' is gone, catch 'Jan' }
  for liLoop := Low(ShortMonthNames) to High(ShortMonthNames) do
    Ps := LStrReplace(Ps, ShortMonthNames[liLoop], IntToStr(liLoop), False);

  { remove redundant spaces }
  Ps := LStrReplace(Ps, AnsiSpace + AnsiSpace, AnsiSpace, False);

  Result := Ps;
end;

function BooleanToInteger(const Pb: Boolean): Integer;
begin
  // (p3) this works as well:
  // Result := Ord(Pb);
  if Pb then
    Result := 1
  else
    Result := 0;
end;

{ from my ConvertFunctions unit }

function StringToBoolean(const Ps: string): Boolean;
const
  TRUE_STRINGS: array [1..5] of string = ('True', 't', 'y', 'yes', '1');
var
  liLoop: Integer;
begin
  Result := False;

  for liLoop := Low(TRUE_STRINGS) to High(TRUE_STRINGS) do
    if AnsiSameText(Ps, TRUE_STRINGS[liLoop]) then
    begin
      Result := True;
      Break;
    end;
end;

function SafeStrToDateTime(const Ps: string): TDateTime;
begin
  try
    Result := StrToDateTime(PreformatDateString(Ps));
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

function SafeStrToDate(const Ps: string): TDateTime;
begin
  try
    Result := StrToDate(PreformatDateString(Ps));
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

function SafeStrToTime(const Ps: string): TDateTime;
begin
  try
    Result := StrToTime(Ps)
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
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

{ have to do this as it depends what the datekind of the control is}

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
begin
  Result := False;
  case pdtKind of
    dtkDateOnly:
      Result := pdtValue < 1; //if date only then anything less than 1 is considered null
    dtkTimeOnly:
      Result := Frac(pdtValue) = NullEquivalentDate; //if time only then anything without a remainder is null
    dtkDateTime:
      Result := pdtValue = NullEquivalentDate;
  end;
end;

function OSCheck(RetVal: Boolean): Boolean;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;

function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;
var
  b: array [0..MAX_PATH] of Char;
  R: TRect;
begin
  StrCopy(b, PChar(Filename));
  R := Rect(0, 0, MaxLen, Canvas.TextHeight('Wq'));
  if DrawText(Canvas.Handle, b, Length(Filename), R,
    DT_SINGLELINE or DT_MODIFYSTRING or DT_PATH_ELLIPSIS or DT_CALCRECT or DT_NOPREFIX) > 0 then
    Result := b
  else
    Result := Filename;
end;

function RunDLL32(const ModuleName, FuncName, CmdLine: string; WaitForCompletion: Boolean; CmdShow: Integer =
  SW_SHOWDEFAULT): Boolean;
var
  SI: TStartUpInfo;
  PI: TProcessInformation;
  S: string;
begin
  SI.cb := SizeOf(SI);
  GetStartupInfo(SI);
  SI.wShowWindow := CmdShow;
  S := Format('rundll32.exe %s,%s %s', [ModuleName, FuncName, CmdLine]);
  Result := CreateProcess(nil, PChar(S), nil, nil, False, 0, nil, nil, SI, PI);
  try
    if WaitForCompletion then
      Result := WaitForSingleObject(PI.hProcess, INFINITE) <> WAIT_FAILED;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

procedure RunDll32Internal(Wnd: HWnd; const DLLName, FuncName, CmdLine: string; CmdShow: Integer = SW_SHOWDEFAULT);
var
  H: THandle;
  ErrMode: Cardinal;
  P: TRunDLL32Proc;
begin
  ErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  H := LoadLibrary(PChar(DLLName));
  try
    if H <> INVALID_HANDLE_VALUE then
    begin
      P := GetProcAddress(H, PChar(FuncName));
      if Assigned(P) then
        P(Wnd, H, PChar(CmdLine), CmdShow);
    end;
  finally
    SetErrorMode(ErrMode);
    if H <> INVALID_HANDLE_VALUE then
      FreeLibrary(H);
  end;
end;

function TimeOnly(pcValue: TDateTime): TTime;
begin
  Result := Frac(pcValue);
end;

function DateOnly(pcValue: TDateTime): TDate;
begin
  Result := Trunc(pcValue);
end;

function HasFlag(a, b: Integer): Boolean;
begin
  Result := (a and b) <> 0;
end;

{ compiled from ComCtrls.pas's implmentation section }

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
    Include(Result, isFocused);
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
  Result := (not (isFocused in peOld)) and (isFocused in peNew);
end;

function ChangeHasDefocus(const peOld, peNew: TItemStates): Boolean;
begin
  Result := (isFocused in peOld) and (not (isFocused in peNew));
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

{!! from strFunctions }

function StrDeleteChars(const Ps: string; const piPos: Integer; const piCount: Integer): string;
begin
  Result := StrLeft(Ps, piPos - 1) + StrRestOf(Ps, piPos + piCount);
end;

function StrDelete(const psSub, psMain: string): string;
var
  liPos: Integer;
begin
  Result := psMain;
  if psSub = '' then
    Exit;

  liPos := StrIPos(psSub, psMain);

  while liPos > 0 do
  begin
    Result := StrDeleteChars(Result, liPos, Length(psSub));
    liPos := StrIPos(psSub, Result);
  end;
end;

type
  // (p3) from ShLwAPI
  TDLLVersionInfo = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformID: DWORD;
  end;

function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;
var
  hDLL, hr: THandle;
  pDllGetVersion: function(var Dvi: TDLLVersionInfo): Integer; stdcall;
  Dvi: TDLLVersionInfo;
begin
  hDLL := LoadLibrary(PChar(DLLName));
  if hDLL < 32 then
    hDLL := 0;
  if hDLL <> 0 then
  begin
    Result := True;
    (*  You must get this function explicitly
        because earlier versions of the DLL's
        don't implement this function.
        That makes the lack of implementation
        of the function a version marker in itself.   *)
    @pDllGetVersion := GetProcAddress(hDLL, PChar('DllGetVersion'));
    if Assigned(pDllGetVersion) then
    begin
      FillChar(Dvi, SizeOf(Dvi), #0);
      Dvi.cbSize := SizeOf(Dvi);
      hr := pDllGetVersion(Dvi);
      if hr = 0 then
      begin
        pdwMajor := Dvi.dwMajorVersion;
        pdwMinor := Dvi.dwMinorVersion;
      end;
    end
    else (*   If GetProcAddress failed, the DLL is a version previous to the one  shipped with IE 3.x. *)
    begin
      pdwMajor := 4;
      pdwMinor := 0;
    end;
    FreeLibrary(hDLL);
    Exit;
  end;
  Result := False;
end;
{from JvVCLUtils }

{ Exceptions }

procedure ResourceNotFound(ResID: PChar);
var
  S: string;
begin
  if LongRec(ResID).Hi = 0 then
    S := IntToStr(LongRec(ResID).Lo)
  else
    S := StrPas(ResID);
  raise EResNotFound.CreateFmt(SResNotFound, [S]);
end;

{ Bitmaps }

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
  Result := MakeModuleBitmap(hInstance, ResID);
end;

function MakeBitmapID(ResID: Word): TBitmap;
begin
  Result := MakeModuleBitmap(hInstance, MakeIntResource(ResID));
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

type
  TJvParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if (Control = nil) or (Control.Parent = nil) then
    Exit;
  Count := Control.Parent.ControlCount;
  DC := Dest.Handle;
  with Control.Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    with Control do
    begin
      SelfR := Bounds(Left, Top, Width, Height);
      X := -Left;
      Y := -Top;
    end;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
        Control.Parent.ClientHeight);
      with TJvParentControl(Control.Parent) do
      begin
        Perform(WM_ERASEBKGND, DC, 0);
        PaintWindow(DC);
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
    { Copy images of graphic controls }
    for I := 0 to Count - 1 do
    begin
      if Control.Parent.Controls[I] = Control then
        Break
      else
      if (Control.Parent.Controls[I] <> nil) and
        (Control.Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(Control.Parent.Controls[I]) do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SaveIndex := SaveDC(DC);
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
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
    with Control.Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

{ Transparent bitmap }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPALETTE;
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
  bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndBack := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
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
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
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
  BM: Windows.TBitmap;
begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  DrawTransparentBitmapRect(DC, Bitmap, DstX, DstY, BM.bmWidth, BM.bmHeight,
    Rect(0, 0, BM.bmWidth, BM.bmHeight), TransparentColor);
end;

procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
  TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
  SrcW, SrcH: Integer);
var
  CanvasChanging: TNotifyEvent;
begin
  if DstW <= 0 then
    DstW := Bitmap.Width;
  if DstH <= 0 then
    DstH := Bitmap.Height;
  if (SrcW <= 0) or (SrcH <= 0) then
  begin
    SrcX := 0;
    SrcY := 0;
    SrcW := Bitmap.Width;
    SrcH := Bitmap.Height;
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
        SrcX, SrcY, SrcW, SrcH, Dest.CopyMode);
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
        Bitmap.Canvas.Handle, SrcX, SrcY, SrcW, SrcH, Bitmap.Palette,
        TransparentColor);
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
      Canvas.BrushCopy(R, Bitmap, R, Color);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ CreateDisabledBitmap. Creating TBitmap object with disable button glyph
  image. You must destroy it outside by calling TBitmap.Free method. }

const
  ROP_DSPDxax = $00E20746;

function CreateDisabledBitmap_NewStyle(FOriginal: TBitmap; BackColor: TColor): TBitmap;
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
      Windows.SetTextColor(DestDC, clWhite);
      Windows.SetBkColor(DestDC, clBlack);
      BitBlt(DestDC, 1, 1, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0, ROP_DSPDxax);
      { Convert Black to clBtnShadow }
      Result.Canvas.Brush.Color := clBtnShadow;
      DestDC := Result.Canvas.Handle;
      Windows.SetTextColor(DestDC, clWhite);
      Windows.SetBkColor(DestDC, clBlack);
      BitBlt(DestDC, 0, 0, FOriginal.Width, FOriginal.Height, SrcDC, 0, 0, ROP_DSPDxax);
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
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
          Brush.Color := HighlightColor;
          SetTextColor(Handle, clBlack);
          SetBkColor(Handle, clWhite);
          BitBlt(Handle, 1, 1, WidthOf(IRect), HeightOf(IRect),
            MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
        end;
        Brush.Color := ShadowColor;
        SetTextColor(Handle, clBlack);
        SetBkColor(Handle, clWhite);
        BitBlt(Handle, 0, 0, WidthOf(IRect), HeightOf(IRect),
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

function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor): TBitmap;
begin
  Result := CreateDisabledBitmapEx(FOriginal, OutlineColor,
    clBtnFace, clBtnHighlight, clBtnShadow, True);
end;

procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighlightColor, GrayColor: TColor; DrawHighlight: Boolean);
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
      Canvas.Brush.Color := HighlightColor;
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
  Result := MakeModuleIcon(hInstance, ResID);
end;

function MakeIconID(ResID: Word): TIcon;
begin
  Result := MakeModuleIcon(hInstance, MakeIntResource(ResID));
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

{ Dialog units }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

function DialogUnitsToPixelsY(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

function PixelsToDialogUnitsX(PixUnits: Word): Word;
begin
  Result := PixUnits * 4 div LoWord(GetDialogBaseUnits);
end;

function PixelsToDialogUnitsY(PixUnits: Word): Word;
begin
  Result := PixUnits * 8 div HiWord(GetDialogBaseUnits);
end;

{ Service routines }

type
  TJvHack = class(TCustomControl);

function LoadDLL(const LibName: string): THandle;
var
  ErrMode: Cardinal;
begin
  ErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  Result := LoadLibrary(PChar(LibName));
  SetErrorMode(ErrMode);
  if Result < HINSTANCE_ERROR then
    OSCheck(False);
end;

function RegisterServer(const ModuleName: string): Boolean;
{ RegisterServer procedure written by Vladimir Gaitanoff, 2:50/430.2 }
type
  TProc = procedure;
var
  Handle: THandle;
  DllRegServ: Pointer;
begin
  Result := False;
  Handle := LoadDLL(ModuleName);
  try
    DllRegServ := GetProcAddress(Handle, 'DllRegisterServer');
    if Assigned(DllRegServ) then
    begin
      TProc(DllRegServ);
      Result := True;
    end;
  finally
    FreeLibrary(Handle);
  end;
end;

procedure Beep;
begin
  MessageBeep(0);
end;

procedure FreeUnusedOle;
begin
  FreeLibrary(GetModuleHandle('OleAut32'));
end;

procedure NotImplemented;
begin
  Screen.Cursor := crDefault;
  MessageDlg(SNotImplemented, mtInformation, [mbOk], 0);
  Abort;
end;

procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetDC(0);
  try
    R := Rect(RectOrg.X, RectOrg.Y, RectEnd.X, RectEnd.Y);
    InvertRect(DC, R);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
var
  DC: HDC;
  I: Integer;
begin
  DC := GetDC(0);
  try
    for I := 1 to Width do
    begin
      DrawFocusRect(DC, ScreenRect);
      InflateRect(ScreenRect, -1, -1);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function PointInRect(const P: TPoint; const R: TRect): Boolean;
begin
  with R do
    Result := (Left <= P.X) and (Top <= P.Y) and
      (Right >= P.X) and (Bottom >= P.Y);
end;

function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
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

procedure KillMessage(Wnd: HWND; Msg: Cardinal);
{ Delete the requested message from the queue, but throw back }
{ any WM_QUIT msgs that PeekMessage may also return.          }
{ Copied from DbGrid.pas                                      }
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.WParam);
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
    if AnsiCompareText(Font.Name, 'Default') = 0 then
      StrPCopy(lfFaceName, DefFontData.Name)
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

function PaletteEntries(Palette: HPALETTE): Integer;
begin
  GetObject(Palette, SizeOf(Integer), @Result);
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

procedure FitRectToScreen(var Rect: TRect);
var
  X, Y, Delta: Integer;
begin
  X := GetSystemMetrics(SM_CXSCREEN);
  Y := GetSystemMetrics(SM_CYSCREEN);
  with Rect do
  begin
    if Right > X then
    begin
      Delta := Right - Left;
      Right := X;
      Left := Right - Delta;
    end;
    if Left < 0 then
    begin
      Delta := Right - Left;
      Left := 0;
      Right := Left + Delta;
    end;
    if Bottom > Y then
    begin
      Delta := Bottom - Top;
      Bottom := Y;
      Top := Bottom - Delta;
    end;
    if Top < 0 then
    begin
      Delta := Bottom - Top;
      Top := 0;
      Bottom := Top + Delta;
    end;
  end;
end;

procedure CenterWindow(Wnd: HWND);
var
  R: TRect;
begin
  GetWindowRect(Wnd, R);
  R := Rect((GetSystemMetrics(SM_CXSCREEN) - R.Right + R.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - R.Bottom + R.Top) div 2,
    R.Right - R.Left, R.Bottom - R.Top);
  FitRectToScreen(R);
  SetWindowPos(Wnd, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOZORDER);
end;

procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
var
  R: TRect;
  AutoScroll: Boolean;
begin
  AutoScroll := AForm.AutoScroll;
  AForm.Hide;
  TJvHack(AForm).DestroyHandle;
  with AForm do
  begin
    BorderStyle := bsNone;
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
    SetWindowPos(ClientHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

function MakeVariant(const Values: array of Variant): Variant;
begin
  if High(Values) - Low(Values) > 1 then
    Result := VarArrayOf(Values)
  else
  if High(Values) - Low(Values) = 1 then
    Result := Values[Low(Values)]
  else
    Result := Null;
end;

{ Shade rectangle }

procedure ShadeRect(DC: HDC; const Rect: TRect);
const
  HatchBits: array [0..7] of Word = ($11, $22, $44, $88, $11, $22, $44, $88);
var
  Bitmap: HBITMAP;
  SaveBrush: HBrush;
  SaveTextColor, SaveBkColor: TColorRef;
begin
  Bitmap := CreateBitmap(8, 8, 1, 1, @HatchBits);
  SaveBrush := SelectObject(DC, CreatePatternBrush(Bitmap));
  try
    SaveTextColor := SetTextColor(DC, clWhite);
    SaveBkColor := SetBkColor(DC, clBlack);
    with Rect do
      PatBlt(DC, Left, Top, Right - Left, Bottom - Top, $00A000C9);
    SetBkColor(DC, SaveBkColor);
    SetTextColor(DC, SaveTextColor);
  finally
    DeleteObject(SelectObject(DC, SaveBrush));
    DeleteObject(Bitmap);
  end;
end;

function ScreenWorkArea: TRect;
begin
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
    with Screen do
      Result := Bounds(0, 0, Width, Height);
end;

function WindowClassName(Wnd: HWND): string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, GetClassName(Wnd, Buffer, SizeOf(Buffer) - 1));
end;

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: HWND; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := GetAnimation;
  if Animation then
    SetAnimation(False);
  ShowWindow(Handle, CmdShow);
  if Animation then
    SetAnimation(True);
end;


procedure SwitchToWindow(Wnd: HWND; Restore: Boolean);
begin
  if IsWindowEnabled(Wnd) then
  begin
    SetForegroundWindow(Wnd);
    if Restore and IsWindowVisible(Wnd) then
    begin
      if not IsZoomed(Wnd) then
        SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      SetFocus(Wnd);
    end;
  end;
end;

function GetWindowParent(Wnd: HWND): HWND;
begin
  Result := GetWindowLong(Wnd, GWL_HWNDPARENT);
end;

procedure ActivateWindow(Wnd: HWND);
begin
  if Wnd <> 0 then
  begin
    ShowWinNoAnimate(Wnd, SW_SHOW);
    SetForegroundWindow(Wnd);
  end;
end;

{$IFDEF CBUILDER}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): HWND;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): HWND;
{$ENDIF CBUILDER}
var
  BufClass, BufTitle: PChar;
begin
  Result := 0;
  if (MainFormClass = '') and (ATitle = '') then
    Exit;
  BufClass := nil;
  BufTitle := nil;
  if (MainFormClass <> '') then
    BufClass := StrPAlloc(MainFormClass);
  if (ATitle <> '') then
    BufTitle := StrPAlloc(ATitle);
  try
    Result := FindWindow(BufClass, BufTitle);
  finally
    StrDispose(BufTitle);
    StrDispose(BufClass);
  end;
end;

function WindowsEnum(Handle: HWND; Param: Longint): Bool; export; stdcall;
begin
  if WindowClassName(Handle) = 'TAppBuilder' then
  begin
    Result := False;
    PLongint(Param)^ := 1;
  end
  else
    Result := True;
end;

{$IFDEF CBUILDER}
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF CBUILDER}
var
  PrevWnd, PopupWnd, ParentWnd: HWND;
  IsDelphi: Longint;
begin
  Result := False;
  PrevWnd := FindPrevInstance(MainFormClass, ATitle);
  if PrevWnd <> 0 then
  begin
    ParentWnd := GetWindowParent(PrevWnd);
    while (ParentWnd <> GetDesktopWindow) and (ParentWnd <> 0) do
    begin
      PrevWnd := ParentWnd;
      ParentWnd := GetWindowParent(PrevWnd);
    end;
    if WindowClassName(PrevWnd) = 'TApplication' then
    begin
      IsDelphi := 0;
      EnumThreadWindows(GetWindowTask(PrevWnd), @WindowsEnum,
        LPARAM(@IsDelphi));
      if Boolean(IsDelphi) then
        Exit;
      if IsIconic(PrevWnd) then
      begin { application is minimized }
        SendMessage(PrevWnd, WM_SYSCOMMAND, SC_RESTORE, 0);
        Result := True;
        Exit;
      end
      else
        ShowWinNoAnimate(PrevWnd, SW_SHOWNOACTIVATE);
    end
    else
      ActivateWindow(PrevWnd);
    PopupWnd := GetLastActivePopup(PrevWnd);
    if (PrevWnd <> PopupWnd) and IsWindowVisible(PopupWnd) and
      IsWindowEnabled(PopupWnd) then
    begin
      SetForegroundWindow(PopupWnd);
    end
    else
      ActivateWindow(PopupWnd);
    Result := True;
  end;
end;

{ Standard Windows MessageBox function }

function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(Text), PChar(Caption), Flags);
end;

function MsgDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
begin
  Result := MessageDlg(Msg, AType, AButtons, HelpCtx);
end;

{ Gradient fill procedure - displays a gradient beginning with a chosen    }
{ color and ending with another chosen color. Based on TGradientFill       }
{ component source code written by Curtis White, cwhite@teleport.com.      }

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  StartRGB: array [0..2] of Byte; { Start RGB values }
  RGBDelta: array [0..2] of Integer; { Difference between start and end RGB values }
  ColorBand: TRect; { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBrush;
begin
  if IsRectEmpty(ARect) then
    Exit;
  if Colors < 2 then
  begin
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(Canvas.Handle, ARect, Brush);
    DeleteObject(Brush);
    Exit;
  end;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  case Direction of
    fdTopToBottom, fdLeftToRight:
      begin
        { Set the Red, Green and Blue colors }
        StartRGB[0] := GetRValue(StartColor);
        StartRGB[1] := GetGValue(StartColor);
        StartRGB[2] := GetBValue(StartColor);
        { Calculate the difference between begin and end RGB values }
        RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
        RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
        RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
      end;
    fdBottomToTop, fdRightToLeft:
      begin
        { Set the Red, Green and Blue colors }
        { Reverse of TopToBottom and LeftToRight directions }
        StartRGB[0] := GetRValue(EndColor);
        StartRGB[1] := GetGValue(EndColor);
        StartRGB[2] := GetBValue(EndColor);
        { Calculate the difference between begin and end RGB values }
        { Reverse of TopToBottom and LeftToRight directions }
        RGBDelta[0] := GetRValue(StartColor) - StartRGB[0];
        RGBDelta[1] := GetGValue(StartColor) - StartRGB[1];
        RGBDelta[2] := GetBValue(StartColor) - StartRGB[2];
      end;
  end;
  { Calculate the color band's coordinates }
  ColorBand := ARect;
  if Direction in [fdTopToBottom, fdBottomToTop] then
  begin
    Colors := Max(2, Min(Colors, HeightOf(ARect)));
    Delta := HeightOf(ARect) div Colors;
  end
  else
  begin
    Colors := Max(2, Min(Colors, WidthOf(ARect)));
    Delta := WidthOf(ARect) div Colors;
  end;
  with Canvas.Pen do
  begin { Set the pen style and mode }
    Style := psSolid;
    Mode := pmCopy;
  end;
  { Perform the fill }
  if Delta > 0 then
  begin
    for I := 0 to Colors do
    begin
      case Direction of
        { Calculate the color band's top and bottom coordinates }
        fdTopToBottom, fdBottomToTop:
          begin
            ColorBand.Top := ARect.Top + I * Delta;
            ColorBand.Bottom := ColorBand.Top + Delta;
          end;
        { Calculate the color band's left and right coordinates }
        fdLeftToRight, fdRightToLeft:
          begin
            ColorBand.Left := ARect.Left + I * Delta;
            ColorBand.Right := ColorBand.Left + Delta;
          end;
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
  if Direction in [fdTopToBottom, fdBottomToTop] then
    Delta := HeightOf(ARect) mod Colors
  else
    Delta := WidthOf(ARect) mod Colors;
  if Delta > 0 then
  begin
    case Direction of
      { Calculate the color band's top and bottom coordinates }
      fdTopToBottom, fdBottomToTop:
        begin
          ColorBand.Top := ARect.Bottom - Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
      { Calculate the color band's left and right coordinates }
      fdLeftToRight, fdRightToLeft:
        begin
          ColorBand.Left := ARect.Right - Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
    end;
    case Direction of
      fdTopToBottom, fdLeftToRight:
        Brush := CreateSolidBrush(EndColor);
    else {fdBottomToTop, fdRightToLeft }
      Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(Canvas.Handle, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
var
  I: Integer;
begin
  Result := Text;
  I := 1;
  while (I <= Length(Text)) and (Canvas.TextWidth(Result) > MaxWidth) do
  begin
    Inc(I);
    Result := Copy(Text, 1, Max(0, Length(Text) - I)) + '...';
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
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
begin
  if Size > 0 then
    Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size)
  else
    Result := nil;
end;

function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
begin
  Result := GlobalReallocPtr(fpBlock, Size,
    HeapAllocFlags or GMEM_ZEROINIT);
end;

procedure FreeMemo(var fpBlock: Pointer);
begin
  if fpBlock <> nil then
  begin
    GlobalFreePtr(fpBlock);
    fpBlock := nil;
  end;
end;

function GetMemoSize(fpBlock: Pointer): Longint;
var
  hMem: THandle;
begin
  Result := 0;
  if fpBlock <> nil then
  begin
    hMem := GlobalHandle(fpBlock);
    if hMem <> 0 then
      Result := GlobalSize(hMem);
  end;
end;

function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,fpBlock1
        MOV     EDI,fpBlock2
        MOV     ECX,Size
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,2
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

{ Manipulate huge pointers routines by Ray Lischner, The Waite Group, Inc. }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PChar(HugePtr) + Amount;
end;

procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PChar(HugePtr) - Amount;
end;

function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
begin
  Result := PChar(HugePtr) + Amount;
end;

procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);
begin
  Move(SrcPtr^, DstPtr^, Amount);
end;

procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
var
  SrcPtr, DstPtr: PChar;
begin
  SrcPtr := PChar(Base) + Src * SizeOf(Pointer);
  DstPtr := PChar(Base) + Dst * SizeOf(Pointer);
  Move(SrcPtr^, DstPtr^, Size * SizeOf(Pointer));
end;
{ String routines }

{ function GetParamStr copied from SYSTEM.PAS unit of Delphi 2.0 }

function GetParamStr(P: PChar; var Param: string): PChar;
var
  Len: Integer;
  Buffer: array [Byte] of Char;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then
      Inc(P, 2)
    else
      Break;
  end;
  Len := 0;
  while P[0] > ' ' do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

function ParamCountFromCommandLine(CmdLine: PChar): Integer;
var
  S: string;
  P: PChar;
begin
  P := CmdLine;
  Result := 0;
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then
      Break;
    Inc(Result);
  end;
end;

function ParamStrFromCommandLine(CmdLine: PChar; Index: Integer): string;
var
  P: PChar;
begin
  P := CmdLine;
  while True do
  begin
    P := GetParamStr(P, Result);
    if (Index = 0) or (Result = '') then
      Break;
    Dec(Index);
  end;
end;

procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
var
  Buffer: PChar;
  Cnt, I: Integer;
  S: string;
begin
  ExeName := '';
  Params := '';
  Buffer := StrPAlloc(CmdLine);
  try
    Cnt := ParamCountFromCommandLine(Buffer);
    if Cnt > 0 then
    begin
      ExeName := ParamStrFromCommandLine(Buffer, 0);
      for I := 1 to Cnt - 1 do
      begin
        S := ParamStrFromCommandLine(Buffer, I);
        if Pos(' ', S) > 0 then
          S := '"' + S + '"';
        Params := Params + S;
        if I < Cnt - 1 then
          Params := Params + ' ';
      end;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

function AnsiUpperFirstChar(const S: string): string;
var
  Temp: string[1];
begin
  Result := AnsiLowerCase(S);
  if S <> '' then
  begin
    Temp := Result[1];
    Temp := AnsiUpperCase(Temp);
    Result[1] := Temp[1];
  end;
end;

function StrPAlloc(const S: string): PChar;
begin
  Result := StrPCopy(StrAlloc(Length(S) + 1), S);
end;

function StringToPChar(var S: string): PChar;
begin
  Result := PChar(S);
end;

function DropT(const S: string): string;
begin
  if (UpCase(S[1]) = 'T') and (Length(S) > 1) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

{ Cursor routines }

function LoadAniCursor(Instance: THandle; ResID: PChar): HCURSOR;
{ Unfortunately I don't know how we can load animated cursor from
  executable resource directly. So I write this routine using temporary
  file and LoadCursorFromFile function. }
var
  S: TFileStream;
  Path, FileName: array [0..MAX_PATH] of Char;
  Rsrc: HRSRC;
  Res: THandle;
  Data: Pointer;
begin
  Result := 0;
  Rsrc := FindResource(Instance, ResID, RT_ANICURSOR);
  if Rsrc <> 0 then
  begin
    OSCheck(GetTempPath(MAX_PATH, Path) <> 0);
    OSCheck(GetTempFileName(Path, 'ANI', 0, FileName) <> 0);
    try
      Res := LoadResource(Instance, Rsrc);
      try
        Data := LockResource(Res);
        if Data <> nil then
        try
          S := TFileStream.Create(StrPas(FileName), fmCreate);
          try
            S.WriteBuffer(Data^, SizeOfResource(Instance, Rsrc));
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

function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
var
  Handle: HCURSOR;
begin
  Handle := LoadCursor(Instance, ResID);
  if Handle = 0 then
    Handle := LoadAniCursor(Instance, ResID);
  if Handle = 0 then
    ResourceNotFound(ResID);
  for Result := 100 to High(TCursor) do { Look for an unassigned cursor index }
    if (Screen.Cursors[Result] = Screen.Cursors[crDefault]) then
    begin
      Screen.Cursors[Result] := Handle;
      Exit;
    end;
  DestroyCursor(Handle);
  raise EOutOfResources.Create(SOutOfResources);
end;

// (rom) changed to var
var
  WaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

procedure StartWait;
begin
  if WaitCount = 0 then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := WaitCursor;
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

{ Grid drawing }

// (rom) changed to var
var
  DrawBitmap: TBitmap = nil;

procedure UsesBitmap;
begin
  if DrawBitmap = nil then
    DrawBitmap := TBitmap.Create;
end;

procedure ReleaseBitmap; far;
begin
  if DrawBitmap <> nil then
    DrawBitmap.Free;
  DrawBitmap := nil;
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean; ARightToLeft: Boolean = False);
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
    (Pos(#13, Text) = 0) then
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
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
  end
  else
  begin { Use FillRect and DrawText for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1,
          Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]
          or RTL[ARightToLeft] or WrapFlags[WordWrap]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean);
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify:
      H := MinOffs;
    vaCenter:
      with TJvHack(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    begin
      with TJvHack(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W'));
    end;
  end;
  WriteText(TJvHack(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap,
    ARightToLeft);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean);
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter, ARightToLeft);
end;

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean);
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify:
      H := MinOffs;
    vaCenter:
      with TJvHack(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
    begin
      with TJvHack(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top -
          Canvas.TextHeight('W'));
    end;
  end;
  WriteText(TJvHack(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment);
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter);
end;

procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);
begin
  Rect.Top := (Rect.Bottom + Rect.Top - Bmp.Height) div 2;
  Rect.Left := (Rect.Right + Rect.Left - Bmp.Width) div 2;
  TJvHack(Control).Canvas.Draw(Rect.Left, Rect.Top, Bmp);
end;

destructor TJvScreenCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

procedure TJvScreenCanvas.CreateHandle;
begin
  if FDeviceContext = 0 then
    FDeviceContext := GetDC(0);
  Handle := FDeviceContext;
end;

procedure TJvScreenCanvas.FreeHandle;
begin
  if FDeviceContext <> 0 then
  begin
    Handle := 0;
    ReleaseDC(0, FDeviceContext);
    FDeviceContext := 0;
  end;
end;

procedure TJvScreenCanvas.SetOrigin(X, Y: Integer);
var
  FOrigin: TPoint;
begin
  SetWindowOrgEx(Handle, -X, -Y, @FOrigin);
end;

{ Check if this is the active Windows task }
{ Copied from implementation of FORMS.PAS  }

type
  PCheckTaskInfo = ^TCheckTaskInfo;
  TCheckTaskInfo = record
    FocusWnd: HWND;
    Found: Boolean;
  end;

function CheckTaskWindow(Window: HWND; Data: Longint): WordBool;stdcall;
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
  Info.FocusWnd := GetActiveWindow;
  Info.Found := False;
  EnumThreadWindows(GetCurrentThreadID, @CheckTaskWindow, Longint(@Info));
  Result := Info.Found;
end;

function GetWindowsVersion: string;
const
  sWindowsVersion = 'Windows %s %d.%.2d.%.3d %s';
var
  Ver: TOsVersionInfo;
  Platfrm: string[4];
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do
  begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s:
        Platfrm := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then
            Platfrm := '98'
          else
            Platfrm := '95';
        end;
      VER_PLATFORM_WIN32_NT: Platfrm := 'NT';
    end;
    Result := Trim(Format(sWindowsVersion, [Platfrm, dwMajorVersion,
      dwMinorVersion, dwBuildNumber, szCSDVersion]));
  end;
end;

// (rom) moved to file end to minimize W- switch impact at end of function

{$W+}
function GetEnvVar(const VarName: string): string;
var
  S: array [0..2048] of Char;
begin
  if GetEnvironmentVariable(PChar(VarName), S, SizeOf(S) - 1) > 0 then
    Result := StrPas(S)
  else
    Result := '';
end;
{$W-}

{ end JvVCLUtils }
{ begin JvUtils }
function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass; const Tag: Integer): TComponent;
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

function FindFormByClassName(FormClassName: string): TForm;
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
  Result := IsIconic(Application.Handle);
end;

function MessageBox(const Msg: string; Caption: string; const Flags: Integer): Integer;
begin
  if Caption = '' then
    Caption := Application.Title;
  Result := Application.MessageBox(PChar(Msg), PChar(Caption), Flags);
end;

const
  NoHelp = 0; { for MsgDlg2 }
  MsgDlgCharSet: Integer = DEFAULT_CHARSET;

function MsgDlgDef1(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; UseDefButton: Boolean;
  AHelpContext: Integer; Control: TWinControl): Integer;
const
  ButtonNames: array [TMsgDlgBtn] of PChar =
    ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
     'YesToAll', 'Help');
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
    Font.CharSet := MsgDlgCharSet;
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
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, DefButton, True, HelpContext, Control);
end;

function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext: Integer;
  Control: TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, mbHelp, False, HelpContext, Control);
end;

procedure SetWindowTop(const Handle: HWND; const Top: Boolean);
const
  TopFlag: array [Boolean] of Longword = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, TopFlag[Top], 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure LoadIcoToImage(ALarge, ASmall: TCustomImageList; const NameRes: string);
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  if ALarge <> nil then
  begin
    Ico.Handle := LoadImage(hInstance, PChar(NameRes), IMAGE_ICON, 32, 32, 0);
    ALarge.AddIcon(Ico);
  end;
  if ASmall <> nil then
  begin
    Ico.Handle := LoadImage(hInstance, PChar(NameRes), IMAGE_ICON, 16, 16, 0);
    ASmall.AddIcon(Ico);
  end;
  Ico.Free;
end;

{ function DefineCursor was typed from
  book "Secrets of Delphi 2" by Ray Lischner }

function DefineCursor2(Identifier: PChar): TCursor;
var
  Handle: HCursor;
begin
  Handle := LoadCursor(hInstance, Identifier);
  if Handle = 0 then
    raise EOutOfResources.Create('Cannot load cursor resource');
  for Result := 1 to High(TCursor) do
    if Screen.Cursors[Result] = Screen.Cursors[crDefault] then
    begin
      Screen.Cursors[Result] := Handle;
      Exit;
    end;
  raise EOutOfResources.Create('Too many user-defined cursors');
end;

procedure CenterHor(Parent: TControl; MinLeft: Integer; Controls: array of TControl);
var
  I: Integer;
begin
  for I := Low(Controls) to High(Controls) do
    Controls[I].Left := Max(MinLeft, (Parent.Width - Controls[I].Width) div 2)
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
  h: Integer;
  w: Integer;
begin
  case MenuAni of
    maNone:
      Form.Show;
    maRandom:
      ;
    maUnfold:
      begin
        h := Form.Height;
        Form.Height := 0;
        Form.Show;
        for I := 0 to h div 10 do
          if Form.Height < h then
            Form.Height := Form.Height + 10;
      end;
    maSlide:
      begin
        h := Form.Height;
        w := Form.Width;
        Form.Height := 0;
        Form.Width := 0;
        Form.Show;
        for I := 0 to Max(h div 5, w div 5) do
        begin
          if Form.Height < h then
            Form.Height := Form.Height + 5;
          if Form.Width < w then
            Form.Width := Form.Width + 5;
        end;
//      CS_SAVEBITS
      end;
  end;
end;

function TargetFileName(const FileName: TFileName): TFileName;
begin
  Result := FileName;
  if CompareText(ExtractFileExt(FileName), '.lnk') = 0 then
    if ResolveLink(Application.Handle, FileName, Result) <> 0 then
      raise Exception.CreateFmt(SCantGetShortCut, [FileName]);
end;

function ResolveLink(const hWnd: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
var
  psl: IShellLink;
  WLinkFile: array [0..MAX_PATH] of WideChar;
  wfd: TWIN32FINDDATA;
  ppf: IPersistFile;
begin
  Pointer(psl) := nil;
  Pointer(ppf) := nil;
  Result := CoInitialize(nil);
  if SUCCEEDED(Result) then
  begin
    // Get a Pointer to the IShellLink interface.
    Result := CoCreateInstance(CLSID_ShellLink, nil,
      CLSCTX_INPROC_SERVER, IShellLink, psl);
    if SUCCEEDED(Result) then
    begin

      // Get a Pointer to the IPersistFile interface.
      Result := psl.QueryInterface(IPersistFile, ppf);
      if SUCCEEDED(Result) then
      begin
        StringToWideChar(LinkFile, WLinkFile, SizeOf(WLinkFile) - 1);
        // Load the shortcut.
        Result := ppf.Load(WLinkFile, STGM_READ);
        if SUCCEEDED(Result) then
        begin
          // Resolve the link.
          Result := psl.Resolve(hWnd, SLR_ANY_MATCH);
          if SUCCEEDED(Result) then
          begin
            // Get the path to the link target.
            SetLength(FileName, MAX_PATH);
            Result := psl.GetPath(PChar(FileName), MAX_PATH, wfd,
              SLGP_UNCPRIORITY);
            if not SUCCEEDED(Result) then
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
    CoUnInitialize;
  end;
  Pointer(psl) := nil;
  Pointer(ppf) := nil;
end;

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
  FProcObj := AProcObj;
end;

procedure TmrProc(hWnd: HWND; uMsg: Integer; idEvent: Integer; dwTime: Integer); stdcall;
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
  KillTimer(hWnd, idEvent);
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
    KillTimer(Application.Handle, Num)
  else
    Num := ProcList.Add(TJvProcItem.Create(Proc));
  SetTimer(Application.Handle, Num, Pause, @TmrProc);
end;

function BrowseForFolder(const Handle: HWND; const Title: string; var Folder: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Id: PItemIDList;
  FN: array [0..MAX_PATH] of Char;
begin
  with BrowseInfo do
  begin
    hwndOwner := Handle;
    pidlRoot := nil;
    pszDisplayName := FN;
    lpszTitle := PChar(Title);
    ulFlags := 0;
    lpfn := nil;
  end;
  Id := SHBrowseForFolder(BrowseInfo);
  Result := Id <> nil;
  if Result then
  begin
    SHGetPathFromIDList(Id, FN);
    Folder := FN;
  end;
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
    Result:= OnGetDefaultIniName
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.INI'));
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
      Result := Copy(Result, 1, N-1) + '\n' + Copy(Result, N+2, Length(Result));
  until N = 0;
  repeat
    N := Pos(#10#13, Result);
    if N > 0 then
      Result := Copy(Result, 1, N-1) + '\n' + Copy(Result, N+2, Length(Result));
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
      Result := Copy(Result, 1, N-1) + CrLf + Copy(Result, N+2, Length(Result));
  until N = 0;
end;

{ The following strings should not be localized }
const
  siFlags     = 'Flags';
  siShowCmd   = 'ShowCmd';
  siMinMaxPos = 'MinMaxPos';
  siNormPos   = 'NormPos';
  siPixels    = 'PixelsPerInch';
  siMDIChild  = 'MDI Children';
  siListCount = 'Count';
  siItem      = 'Item%d';

function IniReadString(IniFile: TObject; const Section, Ident,
  Default: string): string;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadString(Section, Ident, Default)
  else
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
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
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
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
  if IniFile is TCustomIniFile then
    Result := TCustomIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  Value: Longint);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).WriteInteger(Section, Ident, Value);
end;

function IniReadBool(IniFile: TObject; const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
  if IniFile is TCustomIniFile then
    Result := TCustomIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteBool(IniFile: TObject; const Section, Ident: string;
  Value: Boolean);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteBool(Section, Ident, Value)
  else
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).WriteBool(Section, Ident, Value);
end;

procedure IniEraseSection(IniFile: TObject; const Section: string);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).EraseSection(Section)
  else
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).EraseSection(Section);
end;

procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).DeleteKey(Section, Ident)
  else
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).DeleteKey(Section, Ident);
end;

procedure IniReadSections(IniFile: TObject; Strings: TStrings);
begin
  if IniFile is TCustomIniFile then
    TCustomIniFile(IniFile).ReadSections(Strings)
  else
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).ReadSections(Strings);
end;

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

  TJvNastyForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState; { !! }
  end;

  TJvHackComponent = class(TComponent);
  {$HINTS ON}

function CrtResString: string;
begin
  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN),
    GetSystemMetrics(SM_CYSCREEN)]);
end;

function ReadPosStr(AppStore: TJvCustomAppStore; const Path: string): string;
begin
  if AppStore.ValueStored(Path + CrtResString) then
    Result := AppStore.ReadString(Path + CrtResString)
  else
    Result := AppStore.ReadString(Path);
end;

procedure WritePosStr(AppStore: TJvCustomAppStore; const Path, Value: string);
begin
  AppStore.WriteString(Path + CrtResString, Value);
  AppStore.WriteString(Path, Value);
end;

procedure InternalSaveMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string);
var
  I: Integer;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(SNoMDIForm);
  AppStore.DeleteSubTree(AppStore.ConcatPaths([StorePath, siMDIChild]));
  if MainForm.MDIChildCount > 0 then
  begin
    AppStore.WriteInteger(AppStore.ConcatPaths([StorePath, siMDIChild, siListCount]),
      MainForm.MDIChildCount);
    for I := 0 to MainForm.MDIChildCount - 1 do
      AppStore.WriteString(AppStore.ConcatPaths([StorePath, siMDIChild, Format(siItem, [I])]),
        MainForm.MDIChildren[I].ClassName);
  end;
end;

procedure InternalRestoreMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string);
var
  I: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(SNoMDIForm);
  StartWait;
  try
    Count := AppStore.ReadInteger(AppStore.ConcatPaths([StorePath, siMDIChild, siListCount]), 0);
    if Count > 0 then
    begin
      for I := 0 to Count - 1 do
      begin
        FormClass := TFormClass(GetClass(AppStore.ReadString(AppStore.ConcatPaths([StorePath,
          siMDIChild, Format(siItem, [I])]), '')));
        if FormClass <> nil then
          InternalFindShowForm(FormClass, '', False);
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure SaveMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore);
begin
  InternalSaveMDIChildren(MainForm, AppStore, '');
end;

procedure RestoreMDIChildren(MainForm: TForm; const AppStore: TJvCustomAppStore);
begin
  InternalRestoreMDIChildren(MainForm, AppStore, '');
end;

procedure InternalSaveFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string; SaveState: Boolean = True; SavePosition: Boolean = True);
var
  Placement: TWindowPlacement;
begin
  if not (SaveState or SavePosition) then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if (Form = Application.MainForm) and IsIconic(Application.Handle) then
      ShowCmd := SW_SHOWMINIMIZED;
    if (FormStyle = fsMDIChild) and (WindowState = wsMinimized) then
      Flags := Flags or WPF_SETMINPOSITION;
    if SaveState then
    begin
      AppStore.WriteInteger(StorePath + '\' + siShowCmd, ShowCmd);
    end;
    if SavePosition then
    begin
      AppStore.WriteInteger(StorePath + '\' + siFlags, Flags);
      AppStore.WriteInteger(StorePath + '\' + siPixels, Screen.PixelsPerInch);
      WritePosStr(AppStore, StorePath + '\' + siMinMaxPos, Format('%d,%d,%d,%d',
        [ptMinPosition.X, ptMinPosition.Y, ptMaxPosition.X, ptMaxPosition.Y]));
      WritePosStr(AppStore, StorePath + '\' + siNormPos, Format('%d,%d,%d,%d',
        [rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Right,
        rcNormalPosition.Bottom]));
    end;
  end;
end;

procedure InternalRestoreFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore;
  const StorePath: string; LoadState: Boolean = True; LoadPosition: Boolean = True);
const
  Delims = [',', ' '];
var
  PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound: Boolean;
begin
  if not (LoadState or LoadPosition) then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if not IsWindowVisible(Form.Handle) then
      ShowCmd := SW_HIDE;
    if LoadPosition then
    begin
      DataFound := False;
      AppStore.ReadInteger(StorePath + '\' + siFlags, Flags);
      PosStr := ReadPosStr(AppStore, StorePath + '\' + siMinMaxPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
        ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
        ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
      end;
      PosStr := ReadPosStr(AppStore, StorePath + '\' + siNormPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Left);
        rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Top);
        rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Left + Width);
        rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Top + Height);
      end;
      DataFound := DataFound and (Screen.PixelsPerInch = AppStore.ReadInteger(
        StorePath + '\' + siPixels, Screen.PixelsPerInch));
      if DataFound then
      begin
        if not (BorderStyle in [bsSizeable, bsSizeToolWin]) then
          rcNormalPosition := Rect(rcNormalPosition.Left, rcNormalPosition.Top,
            rcNormalPosition.Left + Width, rcNormalPosition.Top + Height);
        if rcNormalPosition.Right > rcNormalPosition.Left then
        begin
          if (Position in [poScreenCenter, poDesktopCenter]) and
            not (csDesigning in ComponentState) then
          begin
            TJvHackComponent(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              TJvHackComponent(Form).SetDesigning(False);
            end;
          end;
          SetWindowPlacement(Handle, @Placement);
        end;
      end;
    end;
    if LoadState then
    begin
      WinState := wsNormal;
      { default maximize MDI main form }
      if ((Application.MainForm = Form) or
        (Application.MainForm = nil)) and ((FormStyle = fsMDIForm) or
        ((FormStyle = fsNormal) and (Position = poDefault))) then
        WinState := wsMaximized;
      ShowCmd := AppStore.ReadInteger(StorePath + '\' + siShowCmd, SW_HIDE);
      case ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE:
          WinState := wsMaximized;
      end;
      if (WinState = wsMinimized) and ((Form = Application.MainForm)
        or (Application.MainForm = nil)) then
      begin
        TJvNastyForm(Form).FWindowState := wsNormal;
        PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
      if FormStyle in [fsMDIChild, fsMDIForm] then
        TJvNastyForm(Form).FWindowState := WinState
      else
        WindowState := WinState;
    end;
    Update;
  end;
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore;
  const StorePath: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    AppStore.WriteInteger(AppStore.ConcatPaths([StorePath, Format(siItem, [I])]),
      TDrawGrid(Grid).ColWidths[I]);
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore;
  const StorePath: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[I] := AppStore.ReadInteger(AppStore.ConcatPaths([StorePath,
      Format(siItem, [I])]), TDrawGrid(Grid).ColWidths[I]);
end;

procedure RestoreGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore);
begin
  InternalRestoreGridLayout(Grid, AppStore, GetDefaultSection(Grid));
end;

procedure SaveGridLayout(Grid: TCustomGrid; const AppStore: TJvCustomAppStore);
begin
  InternalSaveGridLayout(Grid, AppStore, GetDefaultSection(Grid));
end;

procedure SaveFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore; SaveState,
  SavePosition: Boolean);
begin
  InternalSaveFormPlacement(Form, AppStore, GetDefaultSection(Form), SaveState, SavePosition);
end;

procedure RestoreFormPlacement(Form: TForm; const AppStore: TJvCustomAppStore; LoadState,
  LoadPosition: Boolean);
begin
  InternalRestoreFormPlacement(Form, AppStore, GetDefaultSection(Form), LoadState, LoadPosition);
end;

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;
var
  CurrentName: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to MaxInt do
  begin
    CurrentName := Format(FileNameMask, [I]);
    if not FileExists(NormalDir(Path) + CurrentName) then
    begin
      Result := CurrentName;
      Exit;
    end;
  end;
end;

procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);
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
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > Result then
      Result := Values[I];
end;

procedure InvalidBitmap; near;
begin
  raise EInvalidGraphic.Create(SInvalidBitmap);
end;

type
  PRGBPalette = ^TRGBPalette;
  TRGBPalette = array [Byte] of TRGBQuad;

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
  DC := GetDC(0);
  try
    case (GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL)) of
      1:
        Result := pf1bit;
      4:
        Result := pf4bit;
      8:
        Result := pf8bit;
      24:
        Result := pf24bit;
    else
      Result := pfDevice;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function ScreenColorCount: Integer;
begin
  Result := PixelFormatToColors(ScreenPixelFormat);
end;

{ Quantizing }
{ Quantizing procedures based on free C source code written by
  Joe C. Oliphant, CompuServe 71742, 1451, joe_oliphant@csufresno.edu }

const
  MAX_COLORS = 4096;

type
  PQColor = ^TQColor;
  TQColor = record
    RGB: array [0..2] of Byte;
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
    RGBMin, RGBWidth: array [0..2] of Byte;
    NumEntries: Longint;
    Count: Longint;
    QuantizedColors: PQColor;
  end;

  PNewColorArray = ^TNewColorArray;
  TNewColorArray = array[Byte] of TNewColor;

procedure PInsert(ColorList: PQColorList; Number: Integer;
  SortRGBAxis: Integer);
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

procedure PSort(ColorList: PQColorList; Number: Integer;
  SortRGBAxis: Integer);
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
  var NewColormapSize: Integer; lpStr: Pointer): Integer;
var
  I, J: Integer;
  MaxSize, Index: Integer;
  NumEntries, MinColor,
    MaxColor:Integer;
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
        if (NewColorSubdiv^[I].RGBwidth[J] > MaxSize) and
          (NewColorSubdiv^[I].NumEntries > 1) then
        begin
          MaxSize := NewColorSubdiv^[I].RGBwidth[J];
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
    SortArray := PQColorList(lpStr);
    J := 0;
    QuantizedColor := NewColorSubdiv^[Index].QuantizedColors;
    while (J < NewColorSubdiv^[Index].NumEntries) and
      (QuantizedColor <> nil) do
    begin
      SortArray^[J] := QuantizedColor;
      Inc(J);
      QuantizedColor := QuantizedColor^.pnext;
    end;
    PSort(SortArray, NewColorSubdiv^[Index].NumEntries, SortRGBAxis);
    for J := 0 to NewColorSubdiv^[Index].NumEntries - 2 do
      SortArray^[J]^.pnext := SortArray^[J + 1];
    SortArray^[NewColorSubdiv^[Index].NumEntries - 1]^.pnext := nil;
    NewColorSubdiv^[Index].QuantizedColors := SortArray^[0];
    QuantizedColor := SortArray^[0];
    Sum := NewColorSubdiv^[Index].Count div 2 - QuantizedColor^.Count;
    NumEntries := 1;
    Count := QuantizedColor^.Count;
    Dec(Sum, QuantizedColor^.pnext^.Count);
    while (Sum >= 0) and (QuantizedColor^.pnext <> nil) and
      (QuantizedColor^.pnext^.pnext <> nil) do
    begin
      QuantizedColor := QuantizedColor^.pnext;
      Inc(NumEntries);
      Inc(Count, QuantizedColor^.Count);
      Dec(Sum, QuantizedColor^.pnext^.Count);
    end;
    MaxColor := (QuantizedColor^.RGB[SortRGBAxis]) shl 4;
    MinColor := (QuantizedColor^.pnext^.RGB[SortRGBAxis]) shl 4;
    NewColorSubdiv^[NewColormapSize].QuantizedColors := QuantizedColor^.pnext;
    QuantizedColor^.pnext := nil;
    NewColorSubdiv^[NewColormapSize].Count := Count;
    Dec(NewColorSubdiv^[Index].Count, Count);
    NewColorSubdiv^[NewColormapSize].NumEntries :=
      NewColorSubdiv^[Index].NumEntries - NumEntries;
    NewColorSubdiv^[Index].NumEntries := NumEntries;
    for J := 0 to 2 do
    begin
      NewColorSubdiv^[NewColormapSize].RGBmin[J] :=
        NewColorSubdiv^[Index].RGBmin[J];
      NewColorSubdiv^[NewColormapSize].RGBwidth[J] :=
        NewColorSubdiv^[Index].RGBwidth[J];
    end;
    NewColorSubdiv^[NewColormapSize].RGBwidth[SortRGBAxis] :=
      NewColorSubdiv^[NewColormapSize].RGBmin[SortRGBAxis] +
      NewColorSubdiv^[NewColormapSize].RGBwidth[SortRGBAxis] -
      MinColor;
    NewColorSubdiv^[NewColormapSize].RGBmin[SortRGBAxis] := MinColor;
    NewColorSubdiv^[Index].RGBwidth[SortRGBAxis] :=
      MaxColor - NewColorSubdiv^[Index].RGBmin[SortRGBAxis];
    Inc(NewColormapSize);
  end;
  Result := 1;
end;

function Quantize(const bmp: TBitmapInfoHeader; gptr, Data8: Pointer;
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
  lpStr, Temp, Tmp: Pointer;
  NewColorSubdiv: PNewColorArray;
  ColorArrayEntries: PQColorArray;
  QuantizedColor: PQColor;
begin
  LineWidth := WidthBytes(Longint(bmp.biWidth) * bmp.biBitCount);
  Mems := (Longint(SizeOf(TQColor)) * (MAX_COLORS)) +
    (Longint(SizeOf(TNewColor)) * 256) + LineWidth +
    (Longint(sizeof(PQCOLOR)) * (MAX_COLORS));
  lpStr := AllocMemo(Mems);
  try
    Temp := AllocMemo(Longint(bmp.biWidth) * Longint(bmp.biHeight) *
      SizeOf(Word));
    try
      ColorArrayEntries := PQColorArray(lpStr);
      NewColorSubdiv := PNewColorArray(HugeOffset(lpStr,
        Longint(sizeof(TQColor)) * (MAX_COLORS)));
      LineBuffer := HugeOffset(lpStr, (Longint(sizeof(TQColor)) * (MAX_COLORS)) +
        (Longint(sizeof(TNewColor)) * 256));
      for I := 0 to MAX_COLORS - 1 do
      begin
        ColorArrayEntries^[I].RGB[0] := I shr 8;
        ColorArrayEntries^[I].RGB[1] := (I shr 4) and $0F;
        ColorArrayEntries^[I].RGB[2] := I and $0F;
        ColorArrayEntries^[I].Count := 0;
      end;
      Tmp := Temp;
      for I := 0 to bmp.biHeight - 1 do
      begin
        HMemCpy(LineBuffer, HugeOffset(gptr, (bmp.biHeight - 1 - I) *
          LineWidth), LineWidth);
        P := LineBuffer;
        for J := 0 to bmp.biWidth - 1 do
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
          NewColorSubdiv^[I].RGBmin[J] := 0;
          NewColorSubdiv^[I].RGBwidth[J] := 255;
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
          QuantizedColor^.pnext := @ColorArrayEntries^[I];
          QuantizedColor := @ColorArrayEntries^[I];
          Inc(NumOfEntries);
        end;
        Inc(I);
      end;
      QuantizedColor^.pnext := nil;
      NewColorSubdiv^[0].NumEntries := NumOfEntries;
      NewColorSubdiv^[0].Count := Longint(bmp.biWidth) * Longint(bmp.biHeight);
      NewColormapSize := 1;
      DivideMap(NewColorSubdiv, ColorCount, NewColormapSize,
        HugeOffset(lpStr, Longint(SizeOf(TQColor)) * (MAX_COLORS) +
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
            QuantizedColor := QuantizedColor^.pnext;
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
      TmpLineWidth := Longint(bmp.biWidth) * SizeOf(Word);
      NewLineWidth := WidthBytes(Longint(bmp.biWidth) * 8);
      FillChar(Data8^, NewLineWidth * bmp.biHeight, #0);
      for I := 0 to bmp.biHeight - 1 do
      begin
        LineBuffer := HugeOffset(Temp, (bmp.biHeight - 1 - I) * TmpLineWidth);
        Data := HugeOffset(Data8, I * NewLineWidth);
        for J := 0 to bmp.biWidth - 1 do
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
    FreeMemo(lpStr);
  end;
  ColorCount := NewColormapSize;
  Result := 0;
end;

{
  Procedures to truncate to lower bits-per-pixel, grayscale, tripel and
  histogram conversion based on freeware C source code of GBM package by
  Andy Key (nyangau@interalpha.co.uk). The home page of GBM author is
  at http://www.interalpha.net/customer/nyangau/.
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
  { For 7 Red X 8 Green X 4 Blue palettes etc. }
  for I := 0 to 255 do
  begin
    TruncIndex04[I] := NearestIndex(Byte(I), Scale04);
    TruncIndex06[I] := NearestIndex(Byte(I), Scale06);
    TruncIndex07[I] := NearestIndex(Byte(I), Scale07);
    TruncIndex08[I] := NearestIndex(Byte(I), Scale08);
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

procedure TruncLine6R6G6B(Src, Dest: Pointer; CX: Integer); far;
var
  X: Integer;
  R, G, B: Byte;
begin
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

procedure TruncLine7R8G4B(Src, Dest: Pointer; CX: Integer); far;
var
  X: Integer;
  R, G, B: Byte;
begin
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

procedure Grayscale(const Header: TBitmapInfoHeader; Data24, Data8: Pointer);
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
  Result := Word(Longint(Longint(R + G) * Longint(G + B) *
    Longint(B + R)) mod MAX_N_HASH);
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
    Bm: Byte;
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
    Bm := B;
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
    Bm := B;
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
  Rm, Gm, Bm, R, G, B: Byte;
  X, Y, ColCount: Longint;
begin
  Step24 := ((Header.biWidth * 3 + 3) and not 3) - Header.biWidth * 3;
  Rm := Hist.Rm;
  Gm := Hist.Gm;
  Bm := Hist.Bm;
  ColCount := Hist.ColCount;
  for Y := 0 to Header.biHeight - 1 do
  begin
    for X := 0 to Header.biWidth - 1 do
    begin
      B := Byte(Data24^) and Bm;
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
  Rm, Gm, Bm, R, G, B: Byte;
  X, Y: Longint;
begin
  Step24 := ((Header.biWidth * 3 + 3) and not 3) - Header.biWidth * 3;
  Step8 := ((Header.biWidth + 3) and not 3) - Header.biWidth;
  Rm := Hist.Rm;
  Gm := Hist.Gm;
  Bm := Hist.Bm;
  for Y := 0 to Header.biHeight - 1 do
  begin
    for X := 0 to Header.biWidth - 1 do
    begin
      B := Byte(Data24^) and Bm;
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
  Data24, Data8: Pointer; ColorsWanted: Integer; Rm, Gm, Bm: Byte);
{ map single bitmap to frequency optimised palette }
var
  Hist: PHist;
begin
  Hist := CreateHistogram(Rm, Gm, Bm);
  try
    repeat
      if AddToHistogram(Hist^, Header, Data24) then
        Break
      else
      begin
        if Gm > Rm then
          Gm := Gm shl 1
        else
        if Rm > Bm then
          Rm := Rm shl 1
        else
          Bm := Bm shl 1;
        ClearHistogram(Hist, Rm, Gm, Bm);
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
  if Header.biBitCount = 24 then begin
    Exit;
  end;
  Scanline := ((Header.biWidth * Header.biBitCount + 31) div 32) * 4;
  NewScanline := ((Header.biWidth * 3 + 3) and not 3);
  for Y := 0 to Header.biHeight - 1 do begin
    Src := HugeOffset(Data, Y * Scanline);
    Dest := HugeOffset(NewData, Y * NewScanline);
    case Header.biBitCount of
      1:
      begin
        C := 0;
        for X := 0 to Header.biWidth - 1 do begin
          if (X and 7) = 0 then begin
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
        while X < Header.biWidth - 1 do begin
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
        if X < Header.biWidth then begin
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
        for X := 0 to Header.biWidth - 1 do begin
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

function BytesPerScanline(PixelsPerScanline, BitsPerPixel,
  Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and
    not Alignment;
  Result := Result div 8;
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  PixelFormat: TPixelFormat);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then
    InvalidBitmap
  else
  if (Bytes >= (SizeOf(DS.dsbm) + SizeOf(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(SizeOf(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case PixelFormat of
    pf1bit: BI.biBitCount := 1;
    pf4bit: BI.biBitCount := 4;
    pf8bit: BI.biBitCount := 8;
    pf24bit: BI.biBitCount := 24;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
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
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) *
      (1 shl BI.biBitCount);
  ImageSize := BI.biSizeImage;
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
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight,
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
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
  Result := AllocMemo(Length);
  try
    FillChar(Result^, Length, 0);
    FileHeader := Result;
    with FileHeader^ do
    begin
      bfType := $4D42;
      bfSize := Length;
      bfOffBits := SizeOf(FileHeader^) + HeaderSize;
    end;
    BI := PBitmapInfoHeader(Longint(FileHeader) + SizeOf(FileHeader^));
    Bits := Pointer(Longint(BI) + HeaderSize);
    InternalGetDIB(Src, Pal, BI^, Bits^, PixelFormat);
  except
    FreeMemo(Result);
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
begin
  if Bitmap.Handle = 0 then
    InvalidBitmap;
  if (GetBitmapPixelFormat(Bitmap) = PixelFormat) and
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
  if not (PixelFormat in [pf1bit, pf4bit, pf8bit, pf24bit]) then
    NotImplemented
  else
  if PixelFormat in [pf1bit, pf4Bit] then
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
      FreeMemo(P);
    end;
    Exit;
  end;
  { pf8bit - expand to 24bit first }
  InitData := DIBFromBit(Bitmap.Handle, Bitmap.Palette, pf24bit, Len);
  try
    BI := PBitmapInfoHeader(Longint(InitData) + SizeOf(TBitmapFileHeader));
    if BI^.biBitCount <> 24 then
      NotImplemented; {!!!}
    Bits := Pointer(Longint(BI) + SizeOf(TBitmapInfoHeader));
    InternalGetDIBSizes(Bitmap.Handle, NewHeaderSize, ImageSize, PixelFormat);
    Length := SizeOf(TBitmapFileHeader) + NewHeaderSize;
    P := AllocMemo(Length);
    try
      FillChar(P^, Length, #0);
      NewBI := PBitmapInfoHeader(Longint(P) + SizeOf(TBitmapFileHeader));
      NewPalette := PRGBPalette(Longint(NewBI) + SizeOf(TBitmapInfoHeader));
      FileHeader := PBitmapFileHeader(P);
      InitializeBitmapInfoHeader(Bitmap.Handle, NewBI^, PixelFormat);
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
        Result.Write(Bits^, ImageSize div 3);
        Result.Position := 0;
      except
        Result.Free;
        raise;
      end;
    finally
      FreeMemo(P);
    end;
  finally
    FreeMemo(InitData);
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

procedure SaveBitmapToFile(const Filename: string; Bitmap: TBitmap;
  Colors: Integer);
var
  Memory: TStream;
begin
  if Bitmap.Monochrome then
    Colors := 2;
  Memory := BitmapToMemory(Bitmap, Colors);
  try
    TMemoryStream(Memory).SaveToFile(Filename);
  finally
    Memory.Free;
  end;
end;

procedure SetBitmapPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat;
  Method: TMappingMethod);
var
  M: TMemoryStream;
begin
  if (Bitmap.Handle = 0) or (GetBitmapPixelFormat(Bitmap) = PixelFormat) then
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

function ZoomImage(ImageW, ImageH, MaxW, MaxH: Integer; Stretch: Boolean): TPoint;
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
    for X := 0 to (WidthOf(Rect) div Image.Width) do
      for Y := 0 to (HeightOf(Rect) div Image.Height) do
        Canvas.Draw(Rect.Left + X * Image.Width,
          Rect.Top + Y * Image.Height, Image);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

//=== TJvGradientOptions ============================================================

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
  GradientFillRect(Canvas, Rect, FStartColor, FEndColor, FDirection,
    FStepCount);
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

{ begin JvFileUtil }

//=== TJvFileOperator ========================================================

type
  TFileOperation = (foCopy, foDelete, foMove, foRename);
  TFileOperFlag = (flAllowUndo, flConfirmMouse, flFilesOnly, flMultiDest,
    flNoConfirmation, flNoConfirmMkDir, flRenameOnCollision, flSilent,
    flSimpleProgress, flNoErrorUI);
  TFileOperFlags = set of TFileOperFlag;

  TJvFileOperator = class(TComponent)
  private
    FAborted: Boolean;
    FOperation: TFileOperation;
    FOptions: TFileOperFlags;
    FProgressTitle: string;
    FSource: string;
    FDestination: string;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; virtual;
    property Aborted: Boolean read FAborted;
  published
    property Destination: string read FDestination write FDestination;
    property Operation: TFileOperation read FOperation write FOperation
      default foCopy;
    property Options: TFileOperFlags read FOptions write FOptions
      default [flAllowUndo, flNoConfirmMkDir];
    property ProgressTitle: string read FProgressTitle write FProgressTitle;
    property Source: string read FSource write FSource;
  end;

constructor TJvFileOperator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [flAllowUndo, flNoConfirmMkDir];
end;

function TJvFileOperator.TaskModalDialog(DialogFunc: Pointer; var DialogData): Boolean;
type
  TDialogFunc = function(var DialogData): Integer; stdcall;
var
  ActiveWindow: HWND;
  WindowList: Pointer;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    Result := TDialogFunc(DialogFunc)(DialogData) = 0;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

function TJvFileOperator.Execute: Boolean;
const
  OperTypes: array [TFileOperation] of UINT =
    (FO_COPY, FO_DELETE, FO_MOVE, FO_RENAME);
  OperOptions: array [TFileOperFlag] of FILEOP_FLAGS =
    (FOF_ALLOWUNDO, FOF_CONFIRMMOUSE, FOF_FILESONLY, FOF_MULTIDESTFILES,
     FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR, FOF_RENAMEONCOLLISION,
     FOF_SILENT, FOF_SIMPLEPROGRESS, FOF_NOERRORUI);
var
  OpStruct: TSHFileOpStruct;
  Flag: TFileOperFlag;

  function AllocFileStr(const S: string): PChar;
  var
    P: PChar;
  begin
    Result := nil;
    if S <> '' then
    begin
      Result := StrCopy(StrAlloc(Length(S) + 2), PChar(S));
      P := Result;
      while P^ <> #0 do
      begin
        if (P^ = ';') or (P^ = '|') then
          P^ := #0;
        Inc(P);
      end;
      Inc(P);
      P^ := #0;
    end;
  end;

begin
  FAborted := False;
  FillChar(OpStruct, SizeOf(OpStruct), 0);
  with OpStruct do
  try
    if (Application.MainForm <> nil) and
      Application.MainForm.HandleAllocated then
      Wnd := Application.MainForm.Handle
    else
      Wnd := Application.Handle;
    wFunc := OperTypes[Operation];
    pFrom := AllocFileStr(FSource);
    pTo := AllocFileStr(FDestination);
    fFlags := 0;
    for Flag := Low(Flag) to High(Flag) do
      if Flag in FOptions then
        fFlags := fFlags or OperOptions[Flag];
    lpszProgressTitle := PChar(FProgressTitle);
    Result := TaskModalDialog(@SHFileOperation, OpStruct);
    FAborted := fAnyOperationsAborted;
  finally
    if pFrom <> nil then
      StrDispose(pFrom);
    if pTo <> nil then
      StrDispose(pTo);
  end;
end;

procedure CopyMoveFileShell(const FileName, DestName: string; Confirmation,
  AllowUndo, MoveFile: Boolean);
begin
  with TJvFileOperator.Create(nil) do
  try
    Source := FileName;
    Destination := DestName;
    if MoveFile then
    begin
      if AnsiCompareText(ExtractFilePath(FileName),
        ExtractFilePath(DestName)) = 0 then
        Operation := foRename
      else
        Operation := foMove;
    end
    else
      Operation := foCopy;
    if not AllowUndo then
      Options := Options - [flAllowUndo];
    if not Confirmation then
      Options := Options + [flNoConfirmation];
    if not Execute or Aborted then
      SysUtils.Abort;
  finally
    Free;
  end;
end;

procedure MoveFile(const FileName, DestName: TFileName);
var
  Destination: TFileName;
  Attr: Integer;
begin
  Destination := ExpandFileName(DestName);
  if not RenameFile(FileName, Destination) then
  begin
    Attr := FileGetAttr(FileName);
    if Attr < 0 then
      Exit;
    if (Attr and faReadOnly) <> 0 then
      FileSetAttr(FileName, Attr and not faReadOnly);
    CopyFile(FileName, Destination, nil);
    DeleteFile(FileName);
  end;
end;

procedure MoveFileEx(const FileName, DestName: TFileName;
  ShellDialog: Boolean);
begin
  if NewStyleControls and ShellDialog then
    CopyMoveFileShell(FileName, DestName, False, False, True)
  else
    MoveFile(FileName, DestName);
end;

procedure CopyFile(const FileName, DestName: string; ProgressControl: TControl);
begin
  CopyFileEx(FileName, DestName, False, False, ProgressControl);
end;

procedure CopyFileEx(const FileName, DestName: string;
  OverwriteReadOnly, ShellDialog: Boolean; ProgressControl: TControl);
const
  ChunkSize = 8192;
var
  CopyBuffer: Pointer;
  Source, Dest: Integer;
  Destination: TFileName;
  FSize, BytesCopied, TotalCopied: Longint;
  Attr: Integer;
begin
  if NewStyleControls and ShellDialog then
  begin
    CopyMoveFileShell(FileName, DestName, not OverwriteReadOnly,
      False, False);
    Exit;
  end;
  Destination := DestName;
  if HasAttr(Destination, faDirectory) then
    Destination := NormalDir(Destination) + ExtractFileName(FileName);
  GetMem(CopyBuffer, ChunkSize);
  try
    TotalCopied := 0;
    FSize := GetFileSize(FileName);
    Source := FileOpen(FileName, fmShareDenyWrite);
    if Source < 0 then
      raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
    try
      if ProgressControl <> nil then
      begin
        SetProgressMax(ProgressControl, FSize);
        SetProgressMin(ProgressControl, 0);
        SetProgressValue(ProgressControl, 0);
      end;
      ForceDirectories(ExtractFilePath(Destination));
      if OverwriteReadOnly then
      begin
        Attr := FileGetAttr(Destination);
        if (Attr >= 0) and ((Attr and faReadOnly) <> 0) then
          FileSetAttr(Destination, Attr and not faReadOnly);
      end;
      Dest := FileCreate(Destination);
      if Dest < 0 then
        raise EFCreateError.CreateFmt(SFCreateError, [Destination]);
      try
        repeat
          BytesCopied := FileRead(Source, CopyBuffer^, ChunkSize);
          if BytesCopied = -1 then
            raise EReadError.Create(SReadError);
          TotalCopied := TotalCopied + BytesCopied;
          if BytesCopied > 0 then
          begin
            if FileWrite(Dest, CopyBuffer^, BytesCopied) = -1 then
              raise EWriteError.Create(SWriteError);
          end;
          if ProgressControl <> nil then
            SetProgressValue(ProgressControl, TotalCopied);
        until BytesCopied < ChunkSize;
        FileSetDate(Dest, FileGetDate(Source));
      finally
        FileClose(Dest);
      end;
    finally
      FileClose(Source);
    end;
  finally
    FreeMem(CopyBuffer, ChunkSize);
    if ProgressControl <> nil then
      SetProgressValue(ProgressControl, 0);
  end;
end;
{ end JvFileUtil }

{ begin JvCtrlUtils }
function IntToExtended(I: Integer): Extended;
begin
  Result := I;
end;

//==============================================================================
// ToolBarMenu
//==============================================================================

procedure JvCreateToolBarMenu(AForm: TForm; AToolBar: TToolBar; AMenu: TMainMenu);
var
  I, TotalWidth: Integer;
  Button: TToolButton;
begin
  if AForm.FormStyle = fsMDIForm then
    raise EJclError.CreateResRec(@RsNotForMdi);
  if AMenu = nil then
    AMenu := AForm.Menu;
  if AMenu = nil then
    Exit;
  with AToolBar do
  begin
    TotalWidth := BorderWidth;
    for I := ButtonCount - 1 downto 0 do
      Buttons[I].Free;
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
      Button.MenuItem := Items[I];
      Inc(TotalWidth, Button.Width + AToolBar.BorderWidth);
    end;
  AToolBar.Width := TotalWidth;
  AForm.Menu := nil;
end;

//==============================================================================
// ListView functions
//==============================================================================

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
          ColWidths[C + 1] := Max(ColWidths[C + 1], Length(Trim(Items[R].SubItems[C])));
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
    Result := ''
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
var
  ColIndex: Integer;

  function FmtStrToInt(S: string): Integer;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(S) do
      if not (S[I] in ['0'..'9', '-']) then
        Delete(S, I, 1) else Inc(I);
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
        Compare := AnsiCompareText(Item1.SubItems[ColIndex], Item2.SubItems[ColIndex]);
    end
    else
    begin
      if ColIndex = -1 then
        Compare := FmtStrToInt(Item1.Caption) - FmtStrToInt(Item2.Caption)
      else
        Compare := FmtStrToInt(Item1.SubItems[ColIndex]) - FmtStrToInt(Item2.SubItems[ColIndex]);
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
      Screen.Cursor := crHourGlass;
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
        Screen.Cursor := crDefault;
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
  FillChar(FileInfo, Sizeof(FileInfo), #0);
  ImageListHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(ListView.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  FillChar(FileInfo, Sizeof(FileInfo), #0);
  ImageListHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  SendMessage(ListView.Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, ImageListHandle);
end;

//==============================================================================
// MessageBox
//==============================================================================

function JvMessageBox(const Text, Caption: string; Flags: DWORD): Integer;
begin
  with Application do
    Result := MessageBox(PChar(Text), PChar(Caption), Flags);
end;

function JvMessageBox(const Text: string; Flags: DWORD): Integer;
begin
  with Application do
    Result := MessageBox(PChar(Text), PChar(Title), Flags);
end;

procedure UpdateTrackFont(TrackFont,Font:TFont;TrackOptions:TJvTrackFontOptions);
begin
  if hoFollowFont in TrackOptions then
  begin
    if not (hoPreserveCharSet in TrackOptions) then
      TrackFont.CharSet := Font.CharSet;
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

function GetDefaultCheckBoxSize:TSize;
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

initialization
  { begin JvGraph }
  InitTruncTables;
  { end JvGraph }

{ begin JvVCLUtils }
finalization
  ReleaseBitmap;
{ end from JvVCLUtils }

end.
