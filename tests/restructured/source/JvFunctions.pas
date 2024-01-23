{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFunctions.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Anthony Steele [asteele@iafrica.com]
Peter Thörnqvist [peter3@peter3.com]

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFunctions;


interface

uses
  Windows, Graphics, Classes, Messages, Controls,
  ComCtrls, SysUtils, ShellApi, JvTypes, ImgList;

{$IFNDEF COMPILER6_UP}
type
  EOSError = class(EWin32Error);
{$ENDIF}
  //Transform an icon to a bitmap
function IconToBitmap(ico: HIcon): TBitmap;
{$EXTERNALSYM IconToBitmap}
// Transform an icon to a bitmap using an image list
function IconToBitmap2(ico: HIcon; Size: integer = 32; TransparentColor: TColor = clNone): TBitmap;
{$EXTERNALSYM IconToBitmap2}
function IconToBitmap3(ico: HIcon; Size: integer = 32; TransparentColor: TColor = clNone): TBitmap;
{$EXTERNALSYM IconToBitmap3}

//Open an object with the shell (url or something like that)
procedure OpenObject(Value: PChar); overload;
{$EXTERNALSYM OpenObject}
procedure OpenObject(Value: string); overload;
{$EXTERNALSYM OpenObject}

//Raise the last Exception
procedure RaiseLastWin32; overload;
{$EXTERNALSYM RaiseLastWin32}
procedure RaiseLastWin32(Text: string); overload;
{$EXTERNALSYM RaiseLastWin32}
//Raise the last Exception with a small comment from your part

//Same as linux function ;)
procedure PError(Text: string);
{$EXTERNALSYM PError}

//Return the maximum of three integers
function GetMax(i, j, k: Integer): Integer;
{$EXTERNALSYM GetMax}

//Return the minimum of three integers
function GetMin(i, j, k: Integer): Integer;
{$EXTERNALSYM GetMin}

//Convert RGB Values to HSV
procedure RgbToHSV(r, g, b: Integer; var h, s, v: Integer);
{$EXTERNALSYM RgbToHSV}

//Get version of Shell.dll
function GetShellVersion: Integer;
{$EXTERNALSYM GetShellVersion}

// set the background wallpaper (two versions)
procedure SetWallpaper(Path: string); overload;
procedure SetWallpaper(Path: string; Style: TWallpaperStyle); overload;

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
procedure HideFormCaption(FormHandle: THandle; Hide: boolean);
// launches the specified CPL file
// format: <Filename> [,@n] or [,,m] or [,@n,m]
// where @n = zero-based index of the applet to start (if there is more than one
// m is the zero-based index of the tab to display
procedure LaunchCpl(FileName: string);

{
  GetControlPanelApplets retrieves information about all control panel applets in a specified folder.
  APath is the path to the folder to search and AMask is the filename mask (containing wildcards if necessary) to use.

  The information is returned in the Strings and Images lists according to the following rules:
   The Display Name and Path to the CPL file is returned in Strings with the following format:
     '<displayname>=<path>'
   You can access the DisplayName by using the Strings.Names array and the Path by accessing the Strings.Values array
   Strings.Objects can contain either of two values depending on if Images is nil or not:
     * If Images is nil then Strings.Objects contains the image for the applet as a TBitmap. Note that the caller (you)
     is responsible for freeing the bitmaps in this case
     * If Images <> nil, then the Strings.Objects array contains the index of the image in the Images array for the selected item.
       To access and use the ImageIndex, typecast Strings.Objects to an int:
         tmp.Name := Strings.Name[i];
         tmp.ImageIndex := integer(Strings.Objects[i]);
  The function returns true if any Control Panel Applets were found (i.e Strings.Count is > 0 when returning)
}
{$IFNDEF D6PersonalEdition}
function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TImageList = nil): Boolean;
{ GetControlPanelApplet works like GetControlPanelApplets, with the difference that it only loads and searches one cpl file (according to AFilename).
  Note though, that some CPL's contains multiple applets, so the Strings and Images lists can contain multiple return values.
  The function returns true if any Control Panel Applets were found in AFilename (i.e if items were added to Strings)
}
function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TImageList = nil): Boolean;
{$ENDIF}
// execute a program without waiting
procedure Exec(FileName, Parameters, Directory: string);
// execute a program and wait for it to finish
procedure ExecuteAndWait(FileName: string; Visibility: Integer);
// returns true if Drive is accessible
function DiskInDrive(Drive: Char): Boolean;
// returns true if this is the first instance of the program that is running
function FirstInstance(const ATitle: string): boolean;

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
function RegionFromBitmap(Image: TBitmap): HRGN;

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

function CharIsMoney(const ch: char): boolean;

{ there is a STrToIntDef provided by Delphi, but no "safe" versions of
  StrToFloat or StrToCurr }
function StrToFloatDef(const str: string; def: extended): extended;
function StrToCurrDef(const str: string; cDef: currency): currency;

{ GetChangedText works out the new text given the current cursor pos & the key pressed
  It is not very useful in other contexts,
  but it is in this unit as it is needed in both MemoEx and TypedEdit }
function GetChangedText(const Text: string; SelStart, SelLength: integer; Key: char): string;

function MakeYear4Digit(Year, Pivot: integer): integer;

function StrIsInteger(const S: AnsiString): boolean;
function StrIsFloatMoney(const ps: string): boolean;
function StrIsDateTime(const ps: string): boolean;

function PreformatDateString(ps: string): string;

function BooleanToInteger(const pb: boolean): integer;
function StringToBoolean(const ps: AnsiString): boolean;

function SafeStrToDateTime(const ps: string): TDateTime;
function SafeStrToDate(const ps: string): TDateTime;
function SafeStrToTime(const ps: string): TDateTime;

function StrDelete(const psSub, psMain: string): string;

{ listview functions }
function ConvertStates(const State: integer): TItemStates;

function ChangeHasDeselect(const peOld, peNew: TItemStates): boolean;
function ChangeHasSelect(const peOld, peNew: TItemStates): boolean;

function ChangeHasDefocus(const peOld, peNew: TItemStates): boolean;
function ChangeHasFocus(const peOld, peNew: TItemStates): boolean;

function GetListItemColumn(const pcItem: TListItem; piIndex: integer): string;

{ returns the sum of pc.Left, pc.Width and piSpace}
function ToRightOf(const pc: TControl; piSpace: integer = 0): integer;
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
function OSCheck(RetVal: boolean): boolean;

{ Shortens a fully qualified path name so that it can be drawn with a specified length limit.
  Same as FileCtrl.MinimizeName in functionality (but not implementation). Included here to
  not be forced to use FileCtrl unnecessarily }
function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;

{ RunDLL32 runs a function in a DLL using the utility rundll32.exe (on NT) or rundll.exe (on Win95/98)
 ModuleName is the name of the DLL to load, FuncName is the function to call and CmdLine is
 the command-line parameters (if any) to send to the function. Set WaitForCompletion to false to
 return immediately after the call.
 CmdShow should be one of the SW_SHOWXXXX constants and defaults SW_SHOWDEFAULT
 Return value:
 if WaitForCompletion is true, returns true if the wait didn't return WAIT_FAILED
 if WaitForCompletion is false, returns true if the process could be created
 To get information on why RunDLL32 might have failed, call GetLastError
 To get more info on what can actually be called using rundll32.exe, take a look at
 http://www.dx21.com/SCRIPTING/RUNDLL32/REFGUIDE.ASP?NTI=4&SI=6
}
function RunDLL32(const ModuleName, FuncName, CmdLine: string; WaitForCompletion: boolean; CmdShow: integer = SW_SHOWDEFAULT): boolean;
{ RunDll32Internal does the same as RunDLL32 but does not use the RunDLL32.exe application to do it.
 Rather it loads the DLL, gets a pointer to the function in FuncName and calls it with the given parameters.
 Because of this behaviour, RunDll32Internal works slightly different from RunDLL32:
 * It doesn't return any value indicating success/failure
 * There is no WaitForCompletion parameter (but see comment below on how to circumvent this)
 * You must pass in a valid windows handle in Wnd. Note that if you pass 0, the call might fail, with no indication of why.
 * To simulate WaitForCompletion = false, pass the return value of GetDesktopWindow as the Wnd parameter,
 * To simulate WaitForCompletion = true, pass the handle of the calling window (f ex the form you are calling the procedure from)
 * If you try to call a function in a DLL that doesn't use the TRunDLL32Proc signature (declared in JvTypes), your program
   might crash. Using the RunDLL32 function protects you from any problems with calling the wrong functions
   (a dialog is displayed if do something wrong)
 * RunDll32Internal is slightly faster but RunDLL32 is safer
}
procedure RunDll32Internal(Wnd: HWnd; const DLLName, FuncName, CmdLine: string; CmdShow: integer = SW_SHOWDEFAULT);
{ GetDLLVersion loads DLLName, gets a pointer to the DLLVersion function and calls it, returning the major and minor version values
from the function. Returns false if the DLL couldn't be loaded or if GetDLLVersion couldn't be found. }
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: integer): boolean;

{$IFNDEF COMPILER6_UP}
{ D5 compatibility functions }
procedure RaiseLastOSError;
function IncludeTrailingPathDelimiter(const APath: string): string;
function ExcludeTrailingPathDelimiter(const APath: string): string;
{$ENDIF}

implementation
uses
  Forms, Registry, ExtCtrls,
{$IFDEF COMPILER6_UP}Types, {$ENDIF}MMSystem,
  ShlObj, CommCtrl, 
  {$IFNDEF D6PersonalEdition}Cpl,{$ENDIF}
  { jvcl} JvDirectories,
  { jcl } JCLStrings;

resourcestring
  SWin32Error = 'Win32 Error.  Code: %d.'#10'%s';

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
    SetLength(Result,Length(Result)-1);
end;

{$ENDIF}

{*****************************************************}

function IconToBitmap(ico: HIcon): TBitmap;
var
  i: TPicture;
begin
  i := TPicture.Create;
  i.Icon.Handle := ico;
  Result := TBitmap.Create;
  Result.Height := i.Icon.Height;
  Result.Width := i.Icon.Width;
  Result.Canvas.Draw(0, 0, i.icon);
  i.Free;
end;

function IconToBitmap2(ico: HIcon; Size: integer = 32; TransparentColor: TColor = clNone): TBitmap;
begin
  // (p3) this seems to generate "better" bitmaps...
  with TImageList.CreateSize(Size, Size) do
  try
    Masked := true;
    BkColor := TransparentColor;
    ImageList_AddIcon(Handle, ico);
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

function IconToBitmap3(ico: HIcon; Size: integer = 32; TransparentColor: TColor = clNone): TBitmap;
var
  Icon: TIcon; tmp: TBitmap;
begin
  Icon := TIcon.Create;
  tmp := TBitmap.Create;
  try
    Icon.Handle := CopyIcon(ico);
    Result := TBitmap.Create;
    Result.Width := Icon.Width;
    Result.Height := Icon.Height;
    Result.PixelFormat := pf24bit;
    // fill the bitmap with the transparant color
    Result.Canvas.Brush.Color := TransparentColor;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
    Result.Canvas.Draw(0, 0, Icon);
    Result.TransparentColor := TransparentColor;
    tmp.Assign(Result);
    //    Result.Width := Size;
    //    Result.Height := Size;
    Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), tmp);
    Result.Transparent := True;
  finally
    Icon.Free;
    tmp.Free;
  end;
end;

{*****************************************************}

procedure OpenObject(Value: string);
begin
  OpenObject(PChar(Value));
end;

{*****************************************************}

procedure OpenObject(Value: PChar);
begin
  ShellExecute(0, nil, Value, nil, nil, SW_NORMAL);
end;

{**************************************************}

procedure RaiseLastWin32;
begin
  PError('');
end;

{**************************************************}

procedure RaiseLastWin32(Text: string);
begin
  PError(Text);
end;

{**************************************************}

procedure PError(Text: string);
var
  lastError: Integer;
  st: string;
begin
  lastError := GetLastError;
  if lastError <> 0 then
  begin
    st := Format(SWin32Error, [LastError, SysErrorMessage(LastError)]);
    if (Text <> '') then
      st := Text + ':' + st;
    raise EOSError.Create(st);
  end;
end;

{**************************************************}

function GetMax(i, j, k: Integer): Integer;
begin
  if j > i then
    i := j;
  if k > i then
    i := k;
  Result := i;
end;

{**************************************************}

function GetMin(i, j, k: Integer): Integer;
begin
  if j < i then
    i := j;
  if k < i then
    i := k;
  Result := i;
end;

{**************************************************}

procedure RgbToHSV(r, g, b: Integer; var h, s, v: Integer);
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
    else if g = Max then
      h := 120 + (60 * (b - r)) div Delta
    else
      h := 240 + (60 * (r - g)) div Delta;
    if h < 0 then
      h := h + 360;
  end;
end;

{**************************************************}

function GetShellVersion: Integer;
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if ShellVersion = 0 then
  begin
    InfoSize := GetFileVersionInfoSize('shell32.dll', Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo('shell32.dll', Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            ShellVersion := FI.dwFileVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
  Result := ShellVersion;
end;

procedure SetWallpaper(Path: string);
begin
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE);
end;

{************************************************************}

procedure SetWallpaper(Path: string; Style: TWallpaperStyle);
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
  Size: Integer;
  img: TImage; // (p3) change to bmp?
begin
  img := TImage.Create(nil);
  try
    img.Width := rec.Right - rec.Left;
    img.Height := rec.Bottom - rec.Top;
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
        Img.Picture.Bitmap.Palette := CreatePalette(LP^);
      finally
        FreeMem(LP, Size);
      end
    finally
      ReleaseDC(HWND_DESKTOP, C.Handle);
      C.Free;
    end;
    Result := TBitmap.Create;
    Result.Assign(img.Picture.Bitmap);
  finally
    img.Free;
  end;
end;

{*********************************************************}

function CaptureScreen: TBitmap;
begin
  Result := CaptureScreen(Rect(0, 0, Screen.Width, Screen.Height));
end;

procedure OpenCdDrive;
begin
  mciSendString(PChar(RC_OpenCDDrive), nil, 0, Application.Handle);
end;

{*********************************************************}

procedure CloseCdDrive;
begin
  mciSendString(PChar(RC_CloseCDDrive), nil, 0, Application.Handle);
end;

function GetRBitmap(Value: TBitmap): TBitmap;
var
  i, j: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowB := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[i].rgbtRed := rowRGB[i].rgbtRed;
      TRGBArray(rowB^)[i].rgbtGreen := 0;
      TRGBArray(rowB^)[i].rgbtBlue := 0;
    end;
  end;
end;

{************************************************************}

function GetBBitmap(Value: TBitmap): TBitmap;
var
  i, j: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowB := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[i].rgbtRed := 0;
      TRGBArray(rowB^)[i].rgbtGreen := 0;
      TRGBArray(rowB^)[i].rgbtBlue := rowRGB[i].rgbtBlue;
    end;
  end;
end;

{************************************************************}

function GetGBitmap(Value: TBitmap): TBitmap;
var
  i, j: Integer;
  rowRGB, rowB: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowB := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      TRGBArray(rowB^)[i].rgbtRed := 0;
      TRGBArray(rowB^)[i].rgbtGreen := rowRGB[i].rgbtGreen;
      TRGBArray(rowB^)[i].rgbtBlue := 0;
    end;
  end;
end;

{************************************************************}

function GetHueBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, i, j: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowS := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      with rowRGB[i] do
        RgbToHsv(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[i].rgbtBlue := h;
      rowS[i].rgbtGreen := h;
      rowS[i].rgbtRed := h;
    end;
  end;
end;

{************************************************************}

function GetMonochromeBitmap(Value: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Assign(Value);
  Result.Monochrome := True;
end;

{************************************************************}

function GetSaturationBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, i, j: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowS := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      with rowRGB[i] do
        RgbToHsv(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[i].rgbtBlue := s;
      rowS[i].rgbtGreen := s;
      rowS[i].rgbtRed := s;
    end;
  end;
end;

{************************************************************}

function GetValueBitmap(Value: TBitmap): TBitmap;
var
  h, s, v, i, j: Integer;
  rowRGB, rowS: PRGBArray;
begin
  Value.PixelFormat := pf24bit;
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
  for j := Value.Height - 1 downto 0 do
  begin
    rowRGB := Value.Scanline[j];
    rowS := Result.Scanline[j];
    for i := Value.Width - 1 downto 0 do
    begin
      with rowRGB[i] do
        RgbToHsv(rgbtRed, rgbtGreen, rgbtBlue, h, s, v);
      rowS[i].rgbtBlue := v;
      rowS[i].rgbtGreen := v;
      rowS[i].rgbtRed := v;
    end;
  end;
end;

procedure HideFormCaption(FormHandle: THandle; Hide: boolean);
begin
  if Hide then
    SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) and not WS_CAPTION)
  else
    SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) or WS_CAPTION);
end;

procedure LaunchCpl(FileName: string);
begin
  // rundll32.exe shell32,Control_RunDLL ';
  RunDLL32('shell32.dll', 'Control_RunDLL', Filename, true);
  //  WinExec(PChar(RC_RunCpl + FileName), SW_SHOWNORMAL);
end;

{$IFNDEF D6PersonalEdition}
resourcestring
  RC_CplAddress = 'CPlApplet';

function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TImageList = nil): Boolean;
var
  hLib: HMODULE; // Library Handle to *.cpl file
  hIco: HICON;
  CplCall: TCPLApplet; // Pointer to CPlApplet() function
  i: LongInt;
  tmpCount, Count: LongInt;
  S: WideString;
  // the three types of information that can be returned
  CPLInfo: TCPLInfo;
  InfoW: TNewCPLInfoW;
  InfoA: TNewCPLInfoA;
begin
  Result := False;
  hLib := SafeLoadLibrary(AFilename);
  if hLib = 0 then
    Exit;
  tmpCount := Strings.Count;
  try
    @CplCall := GetProcAddress(hLib, PChar(RC_CplAddress));
    if @CplCall = nil then
      Exit;
    CplCall(GetFocus, CPL_INIT, 0, 0); // Init the *.cpl file
    try
      Count := CplCall(GetFocus, CPL_GETCOUNT, 0, 0);
      for i := 0 to Count - 1 do
      begin
        FillChar(InfoW, sizeof(InfoW), 0);
        FillChar(InfoA, sizeof(InfoA), 0);
        FillChar(CPLInfo, sizeof(CPLInfo), 0);
        S := '';
        CplCall(GetFocus, CPL_NEWINQUIRE, i, LongInt(@InfoW));
        if InfoW.dwSize = sizeof(InfoW) then
        begin
          hIco := InfoW.hIcon;
          S := WideString(InfoW.szName);
        end
        else
        begin
          if InfoW.dwSize = sizeof(InfoA) then
          begin
            Move(InfoW, InfoA, sizeof(InfoA));
            hIco := CopyIcon(InfoA.hIcon);
            S := string(InfoA.szName);
          end
          else
          begin
            CplCall(GetFocus, CPL_INQUIRE, i, LongInt(@CPLInfo));
            LoadStringA(hLib, CPLInfo.idName, InfoA.szName, sizeof(InfoA.szName));
            hIco := LoadImage(hLib, PChar(CPLInfo.idIcon), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
            S := string(InfoA.szName);
          end;
        end;
        if S <> '' then
        begin
          S := Format('%s=%s,@%d', [S, AFilename, i]);
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
      Result := tmpCount < Strings.Count;
    finally
      CplCall(GetFocus, CPL_EXIT, 0, 0);
    end;
  finally
    FreeLibrary(hLib);
  end;
end;

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TImageList = nil): Boolean;
var H: THandle; F: TSearchRec;
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
  FindClose(F);
  Result := Strings.Count > 0;
end;
{$ENDIF}

procedure Exec(FileName, Parameters, Directory: string);
var
  Operation: string;
begin
  Operation := 'open';
  ShellExecute(Application.Handle, PChar(Operation), PChar(FileName), PChar(Parameters), PChar(Directory),
    SW_SHOWNORMAL);
end;

{**************************************************}

procedure ExecuteAndWait(FileName: string; Visibility: Integer);
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
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

function FirstInstance(const ATitle: string): boolean;
var
  FHMutex: Thandle;
begin
  FHMutex := CreateMutex(nil, False, PChar(ATitle));
  try
    Result := (FHMutex <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS);
  finally
    ReleaseMutex(FHMutex);
  end;
end;

{***************************************************}

procedure HideTraybar;
var FHwnd: THandle;
begin
  FHWnd := FindWindow(PChar(RC_ShellName), nil);
  ShowWindow(FHWnd, SW_HIDE);
end;

{***************************************************}

procedure ShowTraybar;
var FHwnd: THandle;
begin
  FHWnd := FindWindow(PChar(RC_ShellName), nil);
  ShowWindow(FHWnd, SW_SHOW);
end;

{***************************************************}

procedure HideStartBtn(Visible: Boolean);
var
  Tray, Child: HWND;
  C: array[0..127] of Char;
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

{***************************************************}

procedure ShowStartButton;
begin
  HideStartBtn(True);
end;

{***************************************************}

procedure HideStartButton;
begin
  HideStartBtn(False);
end;

{*********************************************************}

procedure MonitorOn;
begin
  SendMessage(GetFocus, WM_SYSCOMMAND, SC_MONITORPOWER, -1);
end;

{*********************************************************}

procedure MonitorOff;
begin
  SendMessage(GetFocus, WM_SYSCOMMAND, SC_MONITORPOWER, 2);
end;

procedure LowPower;
begin
  SendMessage(GetFocus, WM_SYSCOMMAND, SC_MONITORPOWER, 1);
end;

{$WARNINGS OFF}

{****************************************************}

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

{****************************************************}

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

{****************************************************}

function SendKey(AppName: string; Key: Char): Boolean;
var
  vKey, ScanCode: Word;
  lParam, ConvKey: longint;
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
  bidon: DWord;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    Longint(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, bidon);
end;

{*****************************************************}

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

{*****************************************************}

procedure AssociateExtension(IconPath, ProgramName, Path, Extension: string);
begin
  AssociateFileExtension(IconPath, ProgramName, Path, Extension);
end;

function GetRecentDocs: TStringList;
var
  path: string;
  t: TSearchRec;
  res: Integer;
  FDirs: TJvDirectories;
begin
  Result := TStringList.Create;
  Result.Clear;
  FDirs := TJvDirectories.Create(nil);
  try
    path := FDirs.Recent + '\';
    //search for all files
    res := FindFirst(path + '*.*', faAnyFile, t);
    try
      while res = 0 do
      begin
        if (t.Name <> '.') and (t.Name <> '..') then
          Result.Add(Path + T.Name);
        res := FindNext(t);
      end;
    finally
      FindClose(t);
    end;
  finally
    FDirs.Free;
  end;
end;

procedure AddToRecentDocs(const Filename: string);
begin
  SHAddToRecentDocs(SHARD_PATH, PChar(Filename));
end;

function RegionFromBitmap(Image: TBitmap): HRGN;
var
  rgn1, rgn2: HRGN;
  startx, endx, x, y: Integer;
  TransparentColor: TRGBTriple;
  bmp: TBitmap;
  p: PRGBArray;
begin
  rgn1 := 0;

  bmp := TBitmap.Create;
  bmp.Assign(Image);
  bmp.PixelFormat := pf24Bit;

  if (bmp.Height > 0) and (bmp.Width > 0) then
  begin
    p := bmp.ScanLine[0];
    TransparentColor := p[0];
  end;

  for y := 0 to bmp.Height - 1 do
  begin
    x := 0;
    p := bmp.ScanLine[y];
    repeat
      while (x < bmp.Width) and (CompareMem(@p[x], @TransparentColor, 3)) do
        Inc(x);
      Inc(x);
      startx := x;
      while (x < bmp.Width) and (not (CompareMem(@p[x], @TransparentColor, 3))) do
        Inc(x);
      endx := x;

      // do we have some pixels?
      if startx < bmp.Width then
      begin
        if rgn1 = 0 then
          // Create a region to start with
          rgn1 := CreateRectRgn(startx + 1, y, endx, y + 1)
        else
        begin
          rgn2 := CreateRectRgn(startx + 1, y, endx, y + 1);
          if rgn2 <> 0 then
            CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
          DeleteObject(rgn2);
        end;
      end;
    until x >= Image.Width;
  end;

  bmp.Free;
  Result := rgn1;
end;

function EnumWindowsProc(Handle: THandle; lParam: TStrings): Boolean; stdcall;
var
  st: array[0..256] of Char;
  st2: string;
begin
  if IsWindowVisible(Handle) then
  begin
    GetWindowText(Handle, st, SizeOf(st));
    st2 := st;
    if (st2 <> '') then
      with TStrings(lParam) do
        AddObject(st2, TObject(Handle));
  end;
  Result := True;
end;

procedure GetVisibleWindows(List: Tstrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    EnumWindows(@EnumWindowsProc, integer(List));
  finally
    List.EndUpdate;
  end;
end;

// from JvComponentFunctions

{-------------------------------------------------------------------------------
  internals }

function StrPosNoCase(const psSub, psMain: AnsiString): integer;
begin
  Result := Pos(AnsiUpperCase(psSub), AnsiUpperCase(psMain));
end;

function StrRestOf(const ps: AnsiString; const n: integer): AnsiString;
begin
  Result := Copy(ps, n, (Length(ps) - n + 1));
end;

{!!!!!!!! use these cos the JCL one is badly broken }

{ Am using this one purely as an itnernal for StrReplace

 Replace part of a AnsiString with new text. iUpdatePos is the last update position
 i.e. the position the substr was found + the length of the replacement AnsiString + 1.
 Use 0 first time in }

function StrReplaceInstance(const psSource, psSearch, psReplace: AnsiString;
  var piUpdatePos: integer; const pbCaseSens: boolean): AnsiString;
var
  liIndex: integer;
  lsCopy: AnsiString;
begin
  Result := psSource;
  if piUpdatePos >= Length(psSource) then
    exit;
  if psSearch = '' then
    exit;

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
    exit;
  end;

  Result := Result + StrLeft(lsCopy, liIndex - 1);
  Result := Result + psReplace;
  piUpdatePos := Length(Result) + 1;
  Result := Result + StrRestOf(lsCopy, liIndex + Length(psSearch));
end;

function LStrReplace(const psSource, psSearch, psReplace: AnsiString;
  const pbCaseSens: boolean): AnsiString;
var
  liUpdatePos: integer;
begin
  liUpdatePos := 0;
  Result := psSource;
  while liUpdatePos < Length(Result) do
    Result := StrReplaceInstance(Result, psSearch, psReplace, liUpdatePos, pbCaseSens);
end;

{-------------------------------------------------------------------------------
  exported }

{ if it's not a decimal point then it must be a digit, space or currency symbol
  also always use $ for money }

function CharIsMoney(const ch: char): boolean;
begin
  Result := CharIsDigit(ch) or (ch = AnsiSpace) or (ch = '$') or (ch = '-') or
    (Pos(ch, CurrencyString) > 0);
end;

function StrToCurrDef(const str: string; cDef: currency): currency;
var
  lStr: string;
begin
  try
    lStr := StrStripNonNumberChars(str);

    if lStr = '' then
      Result := cDef
    else
      Result := StrToCurr(lstr);
  except
    Result := cDef;
  end;
end;

function StrToFloatDef(const str: string; def: extended): extended;
var
  lStr: string;
begin
  lStr := StrStripNonNumberChars(str);

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

    Result := StrToFloat(lStr);
  except
    Result := Def;
  end;
end;

function GetChangedText(const Text: string; SelStart, SelLength: integer; Key: char): string;
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
   The windowsill year is the lower edge of the window
  A windowsill year of 1900 is equivalent to putting 1900 before every 2-digit year
 if piWindowsillYear is 1940, then 40 is interpreted as 1940, 00 as 2000 and 39 as 2039
 The system default is 1950
}
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

function MakeYear4Digit(Year, Pivot: integer): integer;
var
  Century: integer;
begin
  Assert(Pivot >= 0);

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

function StrIsInteger(const S: AnsiString): boolean;
var
  I: integer;
  ch: char;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    ch := S[I];
    if (not CharIsNumber(ch)) or (ch = DecimalSeparator) then //Az
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsFloatMoney(const ps: string): boolean;
var
  liLoop, liDots: integer;
  ch: char;
begin
  Result := True;
  liDots := 0;

  for liLoop := 1 to Length(ps) do
  begin
    { allow digits, space, currency symbol and one decimal dot }
    ch := ps[liLoop];

    if (ch = DecimalSeparator) then
    begin
      inc(liDots);
      if liDots > 1 then
      begin
        Result := False;
        break;
      end;
    end
    else if not CharIsMoney(ch) then
    begin
      Result := False;
      break;
    end;
  end;
end;

function StrIsDateTime(const ps: string): boolean;
const
  MIN_DATE_TIME_LEN = 6; {2Jan02 }
  MAX_DATE_TIME_LEN = 30; { 30 chars or so in '12 December 1999 12:23:23:00' }
var
  liLoop: integer;
  ch: char;
  liColons, liSlashes, liSpaces, liDigits, liAlpha: integer;
  lbDisqualify: boolean;
begin
  if Length(ps) < MIN_DATE_TIME_LEN then
  begin
    Result := False;
    exit;
  end;

  if Length(ps) > MAX_DATE_TIME_LEN then
  begin
    Result := False;
    exit;
  end;

  lbDisqualify := False;
  liColons := 0;
  liSlashes := 0;
  liSpaces := 0;
  liDigits := 0;
  liAlpha := 0;

  for liLoop := 1 to Length(ps) do
  begin
    ch := ps[liLoop];

    if (ch = ':') then
      inc(liColons)
    else if (ch = AnsiForwardSlash) then
      inc(liSlashes)
    else if (ch = AnsiSpace) then
      inc(liSpaces)
    else if CharIsDigit(ch) then
      inc(liDigits)
    else if CharIsAlpha(ch) then
      inc(liAlpha)
    else
    begin
      // no weird punctuation in dates!
      lbDisqualify := True;
      break;
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
    Result := (SafeStrToDateTime(PreformatDateString(ps)) <> 0);
end;

function PreformatDateString(ps: string): string;
var
  liLoop: integer;
begin
  { turn any month names to numbers }

  { use the StrReplace in stringfunctions -
  the one in JclStrings is badly broken and brings down the app }

  for liLoop := Low(LongMonthNames) to High(LongMonthNames) do
    ps := LStrReplace(ps, LongMonthNames[liLoop], IntToStr(liLoop), False);

  { now that 'January' is gone, catch 'Jan' }
  for liLoop := Low(ShortMonthNames) to High(ShortMonthNames) do
    ps := LStrReplace(ps, ShortMonthNames[liLoop], IntToStr(liLoop), False);

  { remove redundant spaces }
  ps := LStrReplace(ps, AnsiSpace + AnsiSpace, AnsiSpace, False);

  Result := ps;
end;

function BooleanToInteger(const pb: boolean): integer;
begin
  // (p3) this works as well:
  // Result := Ord(pb);
  if pb then
    Result := 1
  else
    Result := 0;
end;

{ from my ConvertFunctions unit }

function StringToBoolean(const ps: AnsiString): boolean;
const
  TRUE_STRINGS: array[1..5] of string = ('true', 't', 'y', 'yes', '1');
var
  liLoop: integer;
begin
  Result := False;

  for liLoop := Low(TRUE_STRINGS) to High(TRUE_STRINGS) do
    if AnsiSameText(ps, TRUE_STRINGS[liLoop]) then
    begin
      Result := True;
      break;
    end;
end;

function SafeStrToDateTime(const ps: string): TDateTime;
begin
  try
    Result := StrToDateTime(PreformatDateString(ps));
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

function SafeStrToDate(const ps: string): TDateTime;
begin
  try
    Result := StrToDate(PreformatDateString(ps));
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

function SafeStrToTime(const ps: string): TDateTime;
begin
  try
    Result := StrToTime(ps)
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

function ToRightOf(const pc: TControl; piSpace: integer): integer;
begin
  if pc <> nil then
    Result := pc.Left + pc.Width + piSpace
  else
    Result := piSpace;
end;

{ have to do this as it depends what the datekind of the control is}

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
begin
  Result := false;
  case pdtKind of
    dtkDateOnly: Result := pdtValue < 1; //if date only then anything less than 1 is considered null
    dtkTimeOnly: Result := frac(pdtValue) = NullEquivalentDate; //if time only then anything without a remainder is null
    dtkDateTime: Result := pdtValue = NullEquivalentDate;
  end;
end;

function OSCheck(RetVal: boolean): boolean;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;

function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;
var b: array[0..MAX_PATH] of char; R: TRect;
begin
  StrCopy(b, PChar(Filename));
  R := Rect(0, 0, MaxLen, Canvas.TextHeight('Wq'));
  if DrawText(Canvas.Handle, b, Length(Filename), R,
    DT_SINGLELINE or DT_MODIFYSTRING or DT_PATH_ELLIPSIS or DT_CALCRECT or DT_NOPREFIX) > 0 then
    Result := b
  else
    Result := Filename;
end;

function RunDLL32(const ModuleName, FuncName, CmdLine: string; WaitForCompletion: boolean; CmdShow: integer = SW_SHOWDEFAULT): boolean;
var
  SI: TStartUpInfo;
  PI: TProcessInformation;
  S: string;
begin
  SI.cb := sizeof(SI);
  GetStartupInfo(SI);
  SI.wShowWindow := CmdShow;
  S := Format('rundll32.exe %s,%s %s', [ModuleName, FuncName, CmdLine]);
  Result := CreateProcess(nil, PChar(S), nil, nil, false, 0, nil, nil, SI, PI);
  try
    if WaitForCompletion then
      Result := WaitForSingleObject(PI.hProcess, INFINITE) <> WAIT_FAILED;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

procedure RunDll32Internal(Wnd: HWnd; const DLLName, FuncName, CmdLine: string; CmdShow: integer = SW_SHOWDEFAULT);
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
  Result := frac(pcValue);
end;

function DateOnly(pcValue: TDateTime): TDate;
begin
  Result := trunc(pcValue);
end;

{-------------------------------------------------------------------------------
  internals used below }

function HasFlag(a, b: integer): boolean;
begin
  Result := (a and b) <> 0;
end;

{-------------------------------------------------------------------------------
  listview specific stuff }

{ compiled from ComCtrls.pas's implmentation section }

function ConvertStates(const State: integer): TItemStates;
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

function ChangeHasSelect(const peOld, peNew: TItemStates): boolean;
begin
  Result := (not (isSelected in peOld)) and (isSelected in peNew);
end;

function ChangeHasDeselect(const peOld, peNew: TItemStates): boolean;
begin
  Result := (isSelected in peOld) and (not (isSelected in peNew));
end;

function ChangeHasFocus(const peOld, peNew: TItemStates): boolean;
begin
  Result := (not (isFocused in peOld)) and (isFocused in peNew);
end;

function ChangeHasDefocus(const peOld, peNew: TItemStates): boolean;
begin
  Result := (isFocused in peOld) and (not (isFocused in peNew));
end;

function GetListItemColumn(const pcItem: TListItem; piIndex: integer): string;
begin
  if pcItem = nil then
  begin
    Result := '';
    exit;
  end;

  if (piIndex < 0) or (piIndex > pcItem.SubItems.Count) then
  begin
    Result := '';
    exit;
  end;

  if piIndex = 0 then
    Result := pcItem.Caption
  else
    Result := pcItem.SubItems[piIndex - 1];
end;

{!! from strFunctions }

function StrDeleteChars(const ps: string; const piPos: integer; const piCount: integer): string;
begin
  Result := StrLeft(ps, piPos - 1) + StrRestOf(ps, piPos + piCount);
end;

function StrDelete(const psSub, psMain: string): string;
var
  liPos: integer;
begin
  Result := psMain;
  if psSub = '' then
    exit;

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
    cbSize: DWord;
    dwMajorVersion: DWord;
    dwMinorVersion: DWord;
    dwBuildNumber: DWord;
    dwPlatformID: DWord;
  end;

function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: integer): boolean;
var hDLL, hr: THandle;
  pDllGetVersion: function(var dvi: TDLLVersionInfo): integer; stdcall;
  dvi: TDLLVersionInfo;
begin
  hDLL := LoadLibrary(PChar(DLLName));
  if (hDLL < 32) then
    hDLL := 0;
  if (hDLL <> 0) then
  begin
    Result := true;
    (*  You must get this function explicitly
        because earlier versions of the DLL's
        don't implement this function.
        That makes the lack of implementation
        of the function a version marker in itself.   *)
    @pDllGetVersion := GetProcAddress(hDLL, PChar('DllGetVersion'));
    if Assigned(pDllGetVersion) then
    begin
      ZeroMemory(@dvi, sizeof(dvi));
      dvi.cbSize := sizeof(dvi);
      hr := pDllGetVersion(dvi);
      if (hr = 0) then
      begin
        pdwMajor := dvi.dwMajorVersion;
        pdwMinor := dvi.dwMinorVersion;
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
  Result := false;
end;

end.

