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
cginzel [cginzel@hotmail.com]
Remko Bonte

Last Modified: 2003-02-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFunctions;

interface
uses
  Windows, Graphics, Classes, Messages, Controls,
  ComCtrls, SysUtils, ShellApi, ImgList,
  JvTypes;

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

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TImageList = nil): Boolean;
{ GetControlPanelApplet works like GetControlPanelApplets, with the difference that it only loads and searches one cpl file (according to AFilename).
  Note though, that some CPL's contains multiple applets, so the Strings and Images lists can contain multiple return values.
  The function returns True if any Control Panel Applets were found in AFilename (i.e if items were added to Strings)
}
function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TImageList = nil): Boolean;

// execute a program without waiting
procedure Exec(FileName, Parameters, Directory: string);
// execute a program and wait for it to finish
procedure ExecuteAndWait(FileName: string; Visibility: Integer);
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

implementation

uses
  Forms, Registry, ExtCtrls,
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF}
  MMSystem,
  ShlObj, CommCtrl,
  JvDirectories,
  JclStrings, JclGraphics;

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

function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TImageList = nil): Boolean;
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

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TImageList = nil): Boolean;
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
  Dirs: TJvDirectories;
begin
  Result := TStringList.Create;
  Result.Clear;
  Dirs := TJvDirectories.Create(nil);
  try
    Path := Dirs.Recent + '\';
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
  finally
    Dirs.Free;
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

    Result := StrToFloat(lStr);
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
      // no weird punctuation in dates!
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

end.

