{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUtils.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
                       Roman Tkachev <whiteman@infa.ru>
Copyright (c) 1999, 2002 Andrei Prygounkov, Roman Tkachev
All Rights Reserved.

Contributor(s):

Last Modified: 2003-03-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : common routines

Known Issues:
  * Some russian comments were translated to english; these comments are marked
    with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvUtils;

interface

{$DEFINE INCLUDE_RAUTILSW}

uses
  Windows, Forms, Controls, Graphics, SysUtils, Classes,
  StdCtrls, ExtCtrls, Dialogs, Menus, Clipbrd,
  {$IFDEF COMPILER3_UP}
  ShlObj,
  {$ENDIF}
  {$IFDEF COMPILER3_UP}
  ActiveX,
  {$ELSE}
  Ole2,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  TypInfo,
  JvStrUtil;

{$IFNDEF COMPILER4_UP}
type
  Longword = Integer;
{$ENDIF}

type
  TTickCount = Cardinal;

{**** files routines}

{ GetWinDir returns Windows folder name }
function GetWinDir: TFileName;
{ GetTempDir returns Windows temporary folder name }
function GetTempDir: string;
{ GenTempFileName returns temporary file name on
  drive, there FileName is placed }
function GenTempFileName(FileName: string): string;
{ GenTempFileNameExt same to previous function, but
  returning filename has given extension, FileExt }
function GenTempFileNameExt(FileName: string; const FileExt: string): string;
{ ClearDir clears folder Dir }
function ClearDir(const Dir: string): Boolean;
{ DeleteDir clears and than delete folder Dir }
function DeleteDir(const Dir: string): Boolean;
{ FileEquMask returns True if file, FileName,
  is compatible with given dos file mask, Mask }
function FileEquMask(FileName, Mask: TFileName): Boolean;
{ FileEquMasks returns True if file, FileName,
  is compatible with given Masks.
  Masks must be separated with comma (';') }
function FileEquMasks(FileName, Masks: TFileName): Boolean;
procedure DeleteFiles(const Folder: TFileName; const Masks: string);
{ LZFileExpand expand file, FileSource,
  into FileDest. Given file must be compressed, using MS Compress program }
function LZFileExpand(const FileSource, FileDest: string): Boolean;
{ FileGetInfo fills SearchRec record for specified file attributes}
function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): Boolean;
{ HasSubFolder returns True, if folder APath contains other folders }
function HasSubFolder(APath: TFileName): Boolean;
{ IsEmptyFolder returns True, if there are no files or
  folders in given folder, APath}
function IsEmptyFolder(APath: TFileName): Boolean;
{ AddSlash add slash Char to Dir parameter, if needed }
procedure AddSlash(var Dir: TFileName);
{ AddSlash returns string with added slash Char to Dir parameter, if needed }
function AddSlash2(const Dir: TFileName): string;
{ AddPath returns FileName with Path, if FileName not contain any path }
function AddPath(const FileName, Path: TFileName): TFileName;
function AddPaths(const PathList, Path: string): string;
function ParentPath(const Path: TFileName): TFileName;
function FindInPath(const FileName, PathList: string): TFileName;
{$IFNDEF BCB1}
{ BrowseForFolder displays Browse For Folder dialog }
function BrowseForFolder(const Handle: HWND; const Title: string; var Folder: string): Boolean;
{$ENDIF BCB1}
{ DeleteReadOnlyFile clears R/O file attribute and delete file }
function DeleteReadOnlyFile(const FileName: TFileName): Boolean;
{ HasParam returns True, if program running with specified parameter, Param }
function HasParam(const Param: string): Boolean;
function HasSwitch(const Param: string): Boolean;
function Switch(const Param: string): string;
{ ExePath returns ExtractFilePath(ParamStr(0)) }
function ExePath: TFileName;
function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
function FileTimeToDateTime(const FT: TFileTime): TDateTime;
function MakeValidFileName(const FileName: TFileName; const ReplaceBadChar: Char): TFileName;

{**** Graphic routines }

{ TTFontSelected returns True, if True Type font
  is selected in specified device context }
function TTFontSelected(const DC: HDC): Boolean;
{ TrueInflateRect inflates rect in other method, than InflateRect API function }
function TrueInflateRect(const R: TRect; const I: Integer): TRect;

{**** Windows routines }

{ SetWindowTop put window to top without recreating window }
procedure SetWindowTop(const Handle: HWND; const Top: Boolean);

{**** other routines }

{ KeyPressed returns True, if Key VK is now pressed }
function KeyPressed(VK: Integer): Boolean;
procedure SwapInt(var Int1, Int2: Integer);
function IntPower(Base, Exponent: Integer): Integer;
function ChangeTopException(E: TObject): TObject;
function StrToBool(const S: string): Boolean;

{$IFNDEF COMPILER3_UP}
{ AnsiStrLIComp compares S1 to S2, without case-sensitivity, up to a maximum
  Length of MaxLen bytes. The compare operation is controlled by the
  current Windows locale. The return value is the same as for CompareStr. }
function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
function AnsiStrIComp(S1, S2: PChar): Integer;
{$ENDIF}
function Var2Type(V: Variant; const VarType: Integer): Variant;
function VarToInt(V: Variant): Integer;
function VarToFloat(V: Variant): Double;
function GetParameter: string;
function GetLongFileName(FileName: string): string;
function FileNewExt(const FileName, NewExt: TFileName): TFileName;
function GetComputerID: string;
function GetComputerName: string;

{**** files routines - }

{ ResSaveToFile save resource named as Name with Typ type into file FileName.
  Resource can be compressed using MS Compress program}
function ResSaveToFile(const Typ, Name: string; const Compressed: Boolean; const FileName: string): Boolean;
function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar;
  const Compressed: Boolean; const FileName: string): Boolean;
function ResSaveToString(Instance: HINST; const Typ, Name: string;
  var S: string): Boolean;
{ Execute executes other program and waiting for it
  terminating, then return its Exit Code }
function Execute(const CommandLine, WorkingDirectory: string): Integer;
{ IniReadSection read section, Section, from ini-file,
  IniFileName, into strings, Ss.
  This function reads ALL strings from specified section.
  Note: TIninFile.ReadSection function reads only strings with '=' symbol.}
function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): Boolean;
{ LoadTextFile load text file, FileName, into string }
function LoadTextFile(const FileName: TFileName): string;
procedure SaveTextFile(const FileName: TFileName; const Source: string);
{ ReadFolder reads files list from disk folder, Folder,
  that are equal to mask, Mask, into strings, FileList}
function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): Integer;
function ReadFolders(const Folder: TFileName; FolderList: TStrings): Integer;

{$IFDEF COMPILER3_UP}
{ TargetFileName - if FileName is ShortCut returns filename ShortCut linked to }
function TargetFileName(const FileName: TFileName): TFileName;
{ return filename ShortCut linked to }
function ResolveLink(const hWnd: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
{$ENDIF COMPILER3_UP}

{**** Graphic routines - }

{ LoadIcoToImage loads two icons from resource named NameRes,
  into two image lists ALarge and ASmall}
procedure LoadIcoToImage(ALarge, ASmall: TImageList; const NameRes: string);
{ RATextOut same with TCanvas.TextOut procedure, but
  can clipping drawing with rectangle, RClip. }
procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string);
{ RATextOutEx same with RATextOut function, but
  can calculate needed height for correct output }
function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string; const CalcHeight: Boolean): Integer;
{ RATextCalcHeight calculate needed height for
  correct output, using RATextOut or RATextOutEx functions }
function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): Integer;
{ Cinema draws some visual effect }
procedure Cinema(Canvas: TCanvas; rS {Source}, rD {Dest}: TRect);
{ Roughed fills rect with special 3D pattern }
procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: Boolean);
{ BitmapFromBitmap creates new small bitmap from part
  of source bitmap, SrcBitmap, with specified width and height,
  AWidth, AHeight and placed on a specified Index, Index in the
  source bitmap }
function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, Index: Integer): TBitmap;
{ TextWidth calculate text with for writing using standard desktop font }
function TextWidth(AStr: string): Integer;
{ DefineCursor load cursor from resource, and return
  available cursor number, assigned to it }
function DefineCursor(Identifer: PChar): TCursor;

{**** other routines - }
{ FindFormByClass returns first form with specified
  class, FormClass, owned by Application global variable }
function FindFormByClass(FormClass: TFormClass): TForm;
function FindFormByClassName(FormClassName: string): TForm;
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
{ Delay stop program execution to MSec msec }
procedure Delay(MSec: Longword);
procedure CenterHor(Parent: TControl; MinLeft: Integer; Controls: array of TControl);
procedure EnableControls(Control: TWinControl; const Enable: Boolean);
procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: Integer; const Enable: Boolean);
procedure ExpandWidth(Parent: TControl; MinWidth: Integer; Controls: array of TControl);
function PanelBorder(Panel: TCustomPanel): Integer;
function Pixels(Control: TControl; APixels: Integer): Integer;
procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint);
procedure Error(const Msg: string);
procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
  { example for Text parameter :
    'Item 1 <b>bold</b> <i>italic ITALIC <c:Red>red <c:Green>green <c:blue>blue </i>' }
function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;
function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;
function ItemHtPlain(const Text: string): string;
{ ClearList - clears list of TObject }
procedure ClearList(List: TList);

procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: Word);
procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: Word);

{ RTTI support }
function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
function GetPropStr(Obj: TObject; const PropName: string): string;
function GetPropOrd(Obj: TObject; const PropName: string): Integer;
function GetPropMethod(Obj: TObject; const PropName: string): TMethod;

procedure PrepareIniSection(SS: TStrings);
{ following functions are not documented because
  they are don't work properly, so don't use them }
{$IFDEF COMPILER2}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
{$ENDIF}

type
  TMenuAnimation = (maNone, maRandom, maUnfold, maSlide);

procedure ShowMenu(Form: TForm; MenuAni: TMenuAnimation);

type
  TProcObj = procedure of object;

procedure ExecAfterPause(Proc: TProcObj; Pause: Integer);

const
  NoHelp = 0; { for MsgDlg2 }
  MsgDlgCharSet: Integer = DEFAULT_CHARSET;

// (rom) from JvBandWindows to make it obsolete
function PointL(const X, Y: Longint): TPointL;
// (rom) from JvBandUtils to make it obsolete
function iif(const Test: Boolean; const ATrue, AFalse: Variant): Variant;

implementation

uses
  Math,
  JvCtlConst;

function GetWinDir: TFileName;
var
  WinDir: array [0..MAX_PATH] of Char;
begin
  WinDir[GetWindowsDirectory(WinDir, MAX_PATH)] := #0;
  Result := WinDir;
end;

function GenTempFileName(FileName: string): string;
var
  TempDir: array [0..MAX_PATH] of Char;
  TempFile: array [0..MAX_PATH] of Char;
  STempDir: TFileName;
  Res: Integer;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  if FileName <> '' then
  begin
    if Length(FileName) < 4 then
      FileName := ExpandFileName(FileName);
    if (Length(FileName) > 4) and (FileName[2] = ':') and
      (StrLen(@TempDir[0]) > 4) and
      (AnsiCompareText(TempDir[0], FileName[1]) <> 0) then
    begin
      STempDir := ExtractFilePath(FileName);
      Move(STempDir[1], TempDir, Length(STempDir) + 1);
    end;
  end;
  Res := GetTempFileName(
    TempDir, { address of directory name for temporary file}
    '~RA', { address of filename prefix}
    0, { number used to create temporary filename}
    TempFile); { address of buffer that receives the new filename}
  if Res <> 0 then
    Result := TempFile
  else
    Result := '~JVCLTemp.tmp';
  DeleteFile(Result);
end;

function GenTempFileNameExt(FileName: string; const FileExt: string): string;
begin
  Result := ChangeFileExt(GenTempFileName(FileName), FileExt);
end;

function GetTempDir: string;
var
  TempDir: array [0..MAX_PATH] of Char;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  Result := TempDir;
end;

function ClearDir(const Dir: string): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path: TFileName;
begin
  Result := True;
  Path := Dir;
  AddSlash(Path);
  DosError := FindFirst(Path + '*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Result := Result and DeleteDir(Path + SearchRec.Name)
      else
        Result := Result and DeleteFile(Path + SearchRec.Name);
      // if not Result then Exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function DeleteDir(const Dir: string): Boolean;
begin
  ClearDir(Dir);
{  if Dir[Length(Dir)] = '\' then Dir[Length(Dir)] := #0;}
  Result := RemoveDir(Dir);
end;

procedure DeleteFiles(const Folder: TFileName; const Masks: string);
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path: TFileName;
begin
  Path := AddSlash2(Folder);
  DosError := FindFirst(Path + '*.*', faAnyFile and not faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if FileEquMasks(Path + SearchRec.Name, Masks) then
      DeleteFile(Path + SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function GetParameter: string;
var
  FN, FN1: PChar;
begin
  if ParamCount = 0 then
  begin
    Result := '';
    Exit
  end;
  FN := cmdLine;
  if FN[0] = '"' then
  begin
    FN := StrScan(FN + 1, '"');
    if (FN[0] = #00) or (FN[1] = #00) then
      Result := ''
    else
    begin
      Inc(FN, 2);
      if FN[0] = '"' then
      begin
        Inc(FN, 1);
        FN1 := StrScan(FN + 1, '"');
        if FN1[0] <> #00 then
          FN1[0] := #00;
      end;
      Result := FN;
    end;
  end
  else
    Result := Copy(CmdLine, Length(ParamStr(0)) + 1, 260);
  while (Length(Result) > 0) and (Result[1] = ' ') do
    Delete(Result, 1, 1);
  Result := ReplaceString(Result, '"', '');
  if FileExists(Result) then
    Result := GetLongFileName(Result);
end;

function GetLongFileName(FileName: string): string;
var
  SearchRec: TSearchRec;
begin
  if FileGetInfo(FileName, SearchRec) then
    Result := ExtractFilePath(ExpandFileName(FileName)) + SearchRec.FindData.cFileName
  else
    Result := FileName;
end;

function FileEquMask(FileName, Mask: TFileName): Boolean;
var
  I: Integer;
  C: Char;
  P: PChar;
begin
  FileName := AnsiUpperCase(ExtractFileName(FileName));
  Mask := AnsiUpperCase(Mask);
  Result := False;
  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';
  I := 1;
  P := PChar(FileName);
  while I <= Length(Mask) do
  begin
    C := Mask[I];
    if (P[0] = #0) and (C <> '*') then
      Exit;
    case C of
      '*':
        if I = Length(Mask) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          P := StrScan(P, Mask[I + 1]);
          if P = nil then
            Exit;
        end;
      '?':
        Inc(P);
    else
      if C = P[0] then
        Inc(P)
      else
        Exit;
    end;
    Inc(I);
  end;
  if P[0] = #0 then
    Result := True;
end;

function FileEquMasks(FileName, Masks: TFileName): Boolean;
var
  I: Integer;
  Mask: string;
begin
  Result := False;
  I := 0;
  Mask := Trim(SubStr(Masks, I, ';'));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask) then
    begin
      Result := True;
      Break;
    end
    else
    begin
      Inc(I);
      Mask := Trim(SubStr(Masks, I, ';'));
    end;
end;

function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): Boolean;
var
  DosError: Integer;
  Path: TFileName;
begin
  Result := False;
  Path := ExtractFilePath(ExpandFileName(FileName)) + '*.*';
  FileName := AnsiUpperCase(ExtractFileName(FileName));
  DosError := FindFirst(Path, faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (AnsiCompareText(SearchRec.FindData.cFileName, FileName) = 0) or
      (AnsiCompareText(SearchRec.FindData.cAlternateFileName, FileName) = 0) then
    begin
      Result := True;
      Break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function HasSubFolder(APath: TFileName): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  Result := False;
  AddSlash(APath);
  APath := Concat(APath, '*.*');
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Attr and faDirectory = faDirectory) and (SearchRec.Name[1] <> '.') then
    begin
      Result := True;
      Break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function IsEmptyFolder(APath: TFileName): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  Result := True;
  AddSlash(APath);
  APath := Concat(APath, '*.*');
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if SearchRec.Name[1] <> '.' then
    begin
      Result := False;
      Break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function TTFontSelected(const DC: HDC): Boolean;
var
  TM: TTEXTMETRIC;
begin
  GetTextMetrics(DC, TM);
  Result := TM.tmPitchAndFamily and TMPF_TRUETYPE <> 0;
end;

function TrueInflateRect(const R: TRect; const I: Integer): TRect;
begin
  with R do
    SetRect(Result, Left - I, Top - I, Right + I, Bottom + I);
end;

procedure SetWindowTop(const Handle: HWND; const Top: Boolean);
const
  TopFlag: array [Boolean] of Longword = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, TopFlag[Top], 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

function LZFileExpand(const FileSource, FileDest: string): Boolean;
type
  TLZCopy = function(Source, Dest: Integer): Longint; stdcall;
  TLZOpenFile = function(Filename: PChar; var ReOpenBuff: TOFStruct; Style: Word): Integer; stdcall;
  TLZClose = procedure(hFile: Integer); stdcall;
var
  Source, Dest: Integer;
  OSSource, OSDest: TOFSTRUCT;
  Res: Integer;
  Ins: Integer;
  LZCopy: TLZCopy;
  LZOpenFile: TLZOpenFile;
  LZClose: TLZClose;
begin
  Result := False;
  Ins := LoadLibrary('LZ32.dll');
  try
    LZCopy := GetProcAddress(Ins, 'LZCopy');
    LZOpenFile := GetProcAddress(Ins, 'LZOpenFileA');
    LZClose := GetProcAddress(Ins, 'LZClose');
    OSSource.cBytes := SizeOf(TOFSTRUCT);
    OSDest.cBytes := SizeOf(TOFSTRUCT);
    Source := LZOpenFile(
      PChar(FileSource), // address of name of file to be opened
      OSSource, // address of open file structure
      OF_READ or OF_SHARE_DENY_NONE);// action to take
    if Source < 0 then
    begin
      DeleteFile(FileDest);
      Dest := LZOpenFile(
        PChar(FileDest), // address of name of file to be opened
        OSDest, // address of open file structure
        OF_CREATE or OF_WRITE or OF_SHARE_EXCLUSIVE); // action to take
      if Dest >= 0 then
      begin
        Res := LZCopy(Source, Dest);
        if Res >= 0 then
          Result := True;
      end;
      LZClose(Source);
      LZClose(Dest);
    end;
  finally
    FreeLibrary(Ins);
  end;
end;

function KeyPressed(VK: Integer): Boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

{$IFNDEF BCB1}
function BrowseForFolder(const Handle: HWND; const Title: string; var Folder: string): Boolean;
{$IFDEF COMPILER2}
type
  TSHItemID = packed record { mkid }
    cb: Word; { Size of the ID (including cb itself) }
    abID: array [0..0] of Byte; { The item ID (variable Length) }
  end;
  PItemIDList = ^TItemIDList;
  TItemIDList = packed record { idl }
    mkid: TSHItemID;
  end;
  TFNBFFCallBack = function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  TBrowseInfo = packed record
    hwndOwner: HWND;
    pidlRoot: PItemIDList;
    pszDisplayName: PAnsiChar; { Return display name of item selected. }
    lpszTitle: PAnsiChar; { text to go in the banner over the tree. }
    ulFlags: UINT; { Flags that control the return stuff }
    lpfn: TFNBFFCallBack;
    lParam: LPARAM; { extra info that's passed back in callbacks }
    iImage: Integer; { output var: where to return the Image Index. }
  end;

  function SHBrowseForFolder(var lpbi: TBrowseInfo): PItemIDList; stdcall; external 'shell32.dll' name
    'SHBrowseForFolderA';

  function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL; stdcall external 'shell32.dll' name
    'SHGetPathFromIDListA';
  {$ENDIF}
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
{$ENDIF BCB1}

procedure AddSlash(var Dir: TFileName);
begin
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Dir := Dir + '\';
end;

function AddSlash2(const Dir: TFileName): string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Result := Dir + '\';
end;

function AddPath(const FileName, Path: TFileName): TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash2(Path) + FileName
  else
    Result := FileName;
end;

function AddPaths(const PathList, Path: string): string;
var
  I: Integer;
  S: string;
begin
  Result := '';
  I := 0;
  S := SubStr(PathList, I, ';');
  while S <> '' do
  begin
    Result := ConcatSep(Result, AddPath(S, Path), ';');
    Inc(I);
    S := SubStr(PathList, I, ';');
  end;
end;

function ParentPath(const Path: TFileName): TFileName;
begin
  Result := {$IFDEF COMPILER6_UP} ExcludeTrailingPathDelimiter{$ELSE}ExcludeTrailingBackSlash{$ENDIF}(Path);
  Result := ExtractFilePath(Result);
end;

function FindInPath(const FileName, PathList: string): TFileName;
var
  I: Integer;
  S: string;
begin
  I := 0;
  S := SubStr(PathList, I, ';');
  while S <> '' do
  begin
    Result := AddSlash2(S) + FileName;
    if FileExists(Result) then
      Exit;
    Inc(I);
    S := SubStr(PathList, I, ';');
  end;
  Result := '';
end;

function GetComputerID: string;
var
  SN: DWORD;
  Nul: DWORD;
  WinDir: array [0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(WinDir, MAX_PATH);
  WinDir[3] := #0;
  if GetVolumeInformation(
    WinDir, // address of root directory of the file system
    nil, // address of name of the volume
    0, // Length of lpVolumeNameBuffer
    @SN, // address of volume serial number
    Nul, // address of system's maximum filename Length
    Nul, // address of file system flags
    nil, // address of name of file system
    0) // Length of lpFileSystemNameBuffer
    then
    Result := IntToHex(SN, 8)
  else
    Result := 'None';
end;

procedure SwapInt(var Int1, Int2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Int1;
  Int1 := Int2;
  Int2 := Tmp;
end;

function DeleteReadOnlyFile(const FileName: TFileName): Boolean;
begin
  FileSetAttr(FileName, 0); {clear Read Only Flag}
  Result := DeleteFile(FileName);
end;

function HasParam(const Param: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
  begin
    Result := Cmp(ParamStr(I), Param);
    if Result then
      Exit;
  end;
end;

function HasSwitch(const Param: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if HasChar(ParamStr(I)[1], '-/') then
    begin
      Result := Cmp(Copy(ParamStr(I), 2, Length(Param)), Param);
      if Result then
        Exit;
    end;
end;

function Switch(const Param: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount do
    if HasChar(ParamStr(I)[1], '-/\') and
      Cmp(Copy(ParamStr(I), 2, Length(Param)), Param) then
    begin
      Result := Copy(ParamStr(I), 2 + Length(Param), 260);
      Exit;
    end;
end;

function ExePath: TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function FileNewExt(const FileName, NewExt: TFileName): TFileName;
begin
  Result := Copy(FileName, 1, Length(FileName) - Length(ExtractFileExt(FileName))) + NewExt;
end;

{$IFNDEF COMPILER3_UP}

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;

{$ENDIF}

// {roko} Todo: Use Math.IntPower
function IntPower(Base, Exponent: Integer): Integer;
begin
  if Exponent > 0 then
  begin
    Result := Base;
    Dec(Exponent);
    while Exponent > 0 do
    begin
      Result := Result * Base;
      Dec(Exponent);
    end;
  end
  else
  if Exponent < 0 then
  begin
    Result := 1;
    Inc(Exponent);
    while Exponent < 0 do
    begin
      Result := Result div Base;
      Inc(Exponent);
    end;
  end
  else
    Result := Base;
end;

function ChangeTopException(E: TObject): TObject;
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
begin
  { CBuilder 3 Warning !}
  { if linker error occured with message "unresolved external 'System::RaiseList'" try
    comment this function implementation, compile,
    then uncomment and compile again. }
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := E
  end
  else
    Result := nil;
//    raise Exception.Create('Not in exception');
end;

function MakeValidFileName(const FileName: TFileName;
  const ReplaceBadChar: Char): TFileName;
var
  I: Integer;
begin
  Result := FileName;
  for I := 1 to Length(Result) do
    if HasChar(Result[I], '''":?*\/') then
      Result[I] := ReplaceBadChar;
end;

function Var2Type(V: Variant; const VarType: Integer): Variant;
begin
  if TVarData(V).VType in [varEmpty, varNull] then
  begin
    case VarType of
      varString, varOleStr:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
      varVariant:
        Result := Null;
    else
      Result := VarAsType(V, VarType);
    end;
  end
  else
    Result := VarAsType(V, VarType);
  if (VarType = varInteger) and (TVarData(V).VType = varBoolean) then
    Result := Integer(V = True);
end;

function VarToInt(V: Variant): Integer;
begin
  Result := Var2Type(V, varInteger);
end;

function VarToFloat(V: Variant): Double;
begin
  Result := Var2Type(V, varDouble);
end;

function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path, DestPath: TFileName;
begin
  Result := False;
  if not CreateDir(DestDir) then
    Exit;
  Path := SourceDir;
  DestPath := AddSlash2(DestDir);
  AddSlash(Path);
  DosError := FindFirst(Path + '*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Result := CopyDir(Path + SearchRec.Name, AddSlash2(DestDir) + SearchRec.Name)
      else
        Result := CopyFile(PChar(Path + SearchRec.Name),
          PChar(DestPath + SearchRec.Name), True);
      if not Result then
        Exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Result := True;
end;

function FileTimeToDateTime(const FT: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  FileDate: Integer;
begin
  FileTimeToLocalFileTime(FT, LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime, LongRec(FileDate).Hi, LongRec(FileDate).Lo);
  Result := FileDateToDateTime(FileDate);
end;

function GetComputerName: string;
var
  nSize: DWORD;
begin
  nSize := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, nSize);
  if Windows.GetComputerName(
    PChar(Result), // address of name buffer
    nSize) then // address of size of name buffer
    SetLength(Result, nSize)
  else
    Result := '';
end;

function StrToBool(const S: string): Boolean;
begin
  Result := (S = '1') or Cmp(S, 'True') or Cmp(S, 'yes');
end;

procedure LoadIcoToImage(ALarge, ASmall: TImageList; const NameRes: string);
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

procedure WordBreak(Canvas: TCanvas; const S: string; Ss: TStrings);
begin
  Ss.Text := S;
end;

procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string);
begin
  RATextOutEx(Canvas, R, RClip, S, False);
end;

function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): Integer;
begin
  Result := RATextOutEx(Canvas, R, R, S, True);
end;

function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string; const CalcHeight: Boolean): Integer;
var
  Ss: TStrings;
  I: Integer;
  H: Integer;
begin
  Ss := TStringList.Create;
  try
    WordBreak(Canvas, S, Ss);
    H := Canvas.TextHeight('A');
    Result := H * Ss.Count;
    if not CalcHeight then
      for I := 0 to Ss.Count - 1 do
        ExtTextOut(
          Canvas.Handle, // handle of device context
          R.Left, // X-coordinate of reference point
          R.Top + H * I, // Y-coordinate of reference point
          ETO_CLIPPED, // text-output options
          @RClip, // optional clipping and/or opaquing rectangle
          PChar(Ss[I]),
          Length(Ss[I]), // number of characters in string
          nil); // address of array of intercharacter spacing values
  finally
    Ss.Free;
  end;
end;

procedure Cinema(Canvas: TCanvas; rS, rD: TRect);
const
  Pause = 30; {milliseconds}
  Steps = 7;
  Width = 1;
var
  R: TRect;
  I: Integer;
  PenOld: TPen;

  procedure FrameR(R: TRect);
  begin
    with Canvas do
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
      LineTo(R.Right, R.Bottom);
      LineTo(R.Right, R.Top);
      LineTo(R.Left, R.Top);
    end;
  end;

  procedure Frame;
  begin
    FrameR(R);
    with Canvas do
    begin
      MoveTo(rS.Left, rS.Top);
      LineTo(R.Left, R.Top);
      if R.Top <> rS.Top then
      begin
        MoveTo(rS.Right, rS.Top);
        LineTo(R.Right, R.Top);
      end;
      if R.Left <> rS.Left then
      begin
        MoveTo(rS.Left, rS.Bottom);
        LineTo(R.Left, R.Bottom);
      end;
      if (R.Bottom <> rS.Bottom) and (R.Right <> rS.Right) then
      begin
        MoveTo(rS.Right, rS.Bottom);
        LineTo(R.Right, R.Bottom);
      end;
    end;
  end;

begin
  PenOld := TPen.Create;
  PenOld.Assign(Canvas.Pen);
  Canvas.Pen.Mode := pmNot;
  Canvas.Pen.Width := Width;
  Canvas.Pen.Style := psDot;
  FrameR(rS);
  R := rS;
  for I := 1 to Steps do
  begin
    R.Left := rS.Left + (rD.Left - rS.Left) div Steps * I;
    R.Top := rS.Top + (rD.Top - rS.Top) div Steps * I;
    R.Bottom := rS.Bottom + (rD.Bottom - rS.Bottom) div Steps * I;
    R.Right := rS.Right + (rD.Right - rS.Right) div Steps * I;
    Frame;
    Sleep(Pause);
    Frame;
  end;
  FrameR(rS);
  Canvas.Pen.Assign(PenOld);
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

function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): Boolean;
var
  F: Integer;
  S: string;
begin
  with TStringList.Create do
  try
    LoadFromFile(IniFileName);
    F := IndexOf('[' + Section + ']');
    Result := F > -1;
    if Result then
    begin
      Ss.Clear;
      Inc(F);
      while F < Count do
      begin
        S := Strings[F];
        if (Length(S) > 0) and (Trim(S[1]) = '[') then
          Break;
        Ss.Add(S);
        Inc(F);
      end;
    end;
  finally
    Free;
  end;
end;

procedure SaveTextFile(const FileName: TFileName; const Source: string);
begin
  with TStringList.Create do
  try
    Text := Source;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

function LoadTextFile(const FileName: TFileName): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    Result := Text;
  finally
    Free;
  end;
end;

function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): Integer;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  FileList.Clear;
  Result := FindFirst(AddSlash2(Folder) + Mask, faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do
  begin
    if not ((SearchRec.Attr and faDirectory) = faDirectory) then
      FileList.Add(SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function ReadFolders(const Folder: TFileName; FolderList: TStrings): Integer;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  FolderList.Clear;
  Result := FindFirst(AddSlash2(Folder) + '*.*', faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do
  begin
    if ((SearchRec.Attr and faDirectory) = faDirectory) and
      (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      FolderList.Add(SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

{$IFDEF COMPILER3_UP}

function TargetFileName(const FileName: TFileName): TFileName;
begin
  Result := FileName;
  if Cmp(ExtractFileExt(FileName), '.lnk') then
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

{$ENDIF COMPILER3_UP}

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

procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: Boolean);
var
  I: Integer;
  J: Integer;
  R: TRect;
  V: Boolean;
  H: Boolean;
begin
  H := True;
  V := True;
  for I := 0 to (ARect.Right - ARect.Left) div 4 do
  begin
    for J := 0 to (ARect.Bottom - ARect.Top) div 4 do
    begin
      if AVert then
      begin
        if V then
          R := Bounds(ARect.Left + I * 4 + 2, ARect.Top + J * 4, 2, 2)
        else
          R := Bounds(ARect.Left + I * 4, ARect.Top + J * 4, 2, 2);
      end
      else
      begin
        if H then
          R := Bounds(ARect.Left + I * 4, ARect.Top + J * 4 + 2, 2, 2)
        else
          R := Bounds(ARect.Left + I * 4, ARect.Top + J * 4, 2, 2);
      end;
      Frame3D(ACanvas, R, clBtnHighlight, clBtnShadow, 1);
      V := not V;
    end;
    H := not H;
  end;
end;

function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, Index: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Canvas.CopyRect(Rect(0, 0, AWidth, AHeight), SrcBitmap.Canvas, Bounds(AWidth * Index, 0, AWidth, AHeight));
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

function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar;
  const Compressed: Boolean; const FileName: string): Boolean;
var
  RhRsrc: HRSRC;
  RhGlobal: HGLOBAL;
  RAddr: Pointer;
  RLen: DWORD;
  Stream: TFileStream;
  FileDest: string;
begin
  Result := False;
  RhRsrc := FindResource(
    Instance, // resource-module handle
    Name, // address of resource name
    Typ); // address of resource type
  if RhRsrc = 0 then
    Exit;
  RhGlobal := LoadResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RhGlobal = 0 then
    Exit;
  RAddr := LockResource(
    RhGlobal); // handle to resource to lock
  FreeResource(RhGlobal);
  if RAddr = nil then
    Exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RLen = 0 then
    Exit;
  { And now it is possible to duplicate [translated] }
  Stream := nil; { for Free [translated] }
  if Compressed then
    FileDest := GenTempFileName(FileName)
  else
    FileDest := FileName;
  try
    try
      Stream := TFileStream.Create(FileDest, fmCreate or fmOpenWrite or fmShareExclusive);
      Stream.WriteBuffer(RAddr^, RLen);
    finally
      Stream.Free;
    end;
    if Compressed then
    begin
      Result := LZFileExpand(FileDest, FileName);
      DeleteFile(FileDest);
    end
    else
      Result := True;
  except
  end;
end;

function ResSaveToFile(const Typ, Name: string; const Compressed: Boolean;
  const FileName: string): Boolean;
begin
  Result := ResSaveToFileEx(hInstance, PChar(Typ), PChar(Name), Compressed, FileName);
end;

function ResSaveToString(Instance: HINST; const Typ, Name: string;
  var S: string): Boolean;
var
  RhRsrc: HRSRC;
  RhGlobal: HGLOBAL;
  RAddr: Pointer;
  RLen: DWORD;
begin
  Result := False;
  RhRsrc := FindResource(
    Instance, // resource-module handle
    PChar(Name), // address of resource name
    PChar(Typ)); // address of resource type
  if RhRsrc = 0 then
    Exit;
  RhGlobal := LoadResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RhGlobal = 0 then
    Exit;
  RAddr := LockResource(
    RhGlobal); // handle to resource to lock
  FreeResource(RhGlobal);
  if RAddr = nil then
    Exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RLen = 0 then
    Exit;
  { And now it is possible to duplicate [translated] }
  SetString(S, PChar(RAddr), RLen);
end;

// (rom) a thread to wait would be more elegant, also JCL function available

function Execute(const CommandLine, WorkingDirectory: string): Integer;
var
  R: Boolean;
  ProcessInformation: TProcessInformation;
  StartupInfo: TStartupInfo;
  {$IFDEF COMPILER4_UP}
  ExCode: Cardinal;
  {$ELSE}
  ExCode: Integer;
  {$ENDIF}
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

function TextWidth(AStr: string): Integer;
var
  Canvas: TCanvas;
  DC: HDC;
begin
  DC := GetDC(HWND_DESKTOP);
  Canvas := TCanvas.Create;
  // (rom) secured
  try
    Canvas.Handle := DC;
    Result := Canvas.TextWidth(AStr);
    Canvas.Handle := 0;
    Canvas.Free;
  finally
    ReleaseDC(HWND_DESKTOP, DC);
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

function MsgDlgDef1(const Msg, ACaption: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; UseDefButton: Boolean;
  AHelpContext: Integer; Control: TWinControl): Integer;
const
  {$IFNDEF COMPILER2}
  ButtonNames: array [TMsgDlgBtn] of PChar =
    ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
     'YesToAll', 'Help');
  {$ELSE}
  ButtonNames: array [TMsgDlgBtn] of string =
    ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'Help');
  {$ENDIF COMPILER2}
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
    {$IFDEF COMPILER3_UP}
    Font.CharSet := MsgDlgCharSet;
    {$ENDIF COMPILER3_UP}
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

{$IFDEF COMPILER2}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    Inc     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF COMPILER2}

{ function DefineCursor was typed from
  book "Secrets of Delphi 2" by Ray Lischner }

function DefineCursor(Identifer: PChar): TCursor;
var
  Handle: HCursor;
begin
  Handle := LoadCursor(hInstance, Identifer);
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

procedure Delay(MSec: Longword);
var
  T: Longword;
begin
  T := GetTickCount;
  while GetTickCount - T < MSec do
    Application.ProcessMessages;
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

procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint);
var
  I: Integer;
  PropInfo: PPropInfo;
begin
  for I := 0 to Owner.ComponentCount - 1 do
  begin
    PropInfo := GetPropInfo(Owner.Components[I].ClassInfo, PropName);
    if PropInfo <> nil then
      SetOrdProp(Owner.Components[I], PropInfo, Value);
  end;
end;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL: string;
  I: Integer;
  M1: string;
  OriRect: TRect; // it's added
  oldFontStyles: TFontStyles;
  oldFontColor: TColor;

  function Cmp(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
    if Result then
      Inc(I, Length(M1));
  end;

  function CmpL(M1: string): Boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1: string): Boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M: string);
  begin
    if not Assigned(Canvas) then
      Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left := Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if not Assigned(Canvas) then
      Exit;
    if Include then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

begin
  PlainItem := '';
  oldFontColor := 0; { satisfy compiler }
  if Canvas <> nil then
  begin
    oldFontStyles := Canvas.Font.Style;
    oldFontColor := Canvas.Font.Color;
  end;
  try
    if HideSelColor and Assigned(Canvas) then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Font.Color := clWindowText;
    end;
    if Assigned(Canvas) then
      Canvas.FillRect(Rect);

    Width := Rect.Left;
    Rect.Left := Rect.Left + 2;

    OriRect := Rect; //save origin rectangle

    M1 := '';
    I := 1;
    while I <= Length(Text) do
    begin
      if (Text[I] = '<') and
        (CmpL('b') or CmpL('/b') or
        CmpL('i') or CmpL('/i') or
        CmpL('u') or CmpL('/u') or
        Cmp('c:')) then
      begin
        Draw(M1);
        PlainItem := PlainItem + M1;

        if CmpL1('b') then
          Style(fsBold, True)
        else
        if CmpL1('/b') then
          Style(fsBold, False)
        else
        if CmpL1('i') then
          Style(fsItalic, True)
        else
        if CmpL1('/i') then
          Style(fsItalic, False)
        else
        if CmpL1('u') then
          Style(fsUnderline, True)
        else
        if CmpL1('/u') then
          Style(fsUnderline, False)
        else
        if Cmp1('c:') then
        begin
          CL := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
          Inc(I, Length(CL) + 1 {'>'});
        end;

        M1 := '';
      end
      else
      // next lines were added
      if (Text[I] = Chr(13)) and Cmp1(string(Chr(10))) then
      begin
                        // new line
        Draw(M1);
        PlainItem := PlainItem + M1;
        Rect.Left := OriRect.Left;
        Rect.Top := Rect.Top + Canvas.TextHeight(M1);
        M1 := '';
      end
      else
                        // add text
        M1 := M1 + Text[I];
      Inc(I);
    end; { for }
    Draw(M1);
    PlainItem := PlainItem + M1;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := oldFontStyles;
      Canvas.Font.Color := oldFontColor;
    end;
  end;
  Width := Rect.Left - Width + 2;
end;

function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function ItemHtPlain(const Text: string): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(nil, Rect(0, 0, -1, -1), [], Text, False, S, W, False);
  Result := S;
end;

function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, True);
  Result := W;
end;

procedure ClearList(List: TList);
var
  I: Integer;
begin
  if not Assigned(List) then
    Exit;
  for I := 0 to List.Count - 1 do
    TObject(List[I]).Free;
  List.Clear;
end;

procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: Word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE, MemStream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(MemStream.Memory^, DataPtr^, MemStream.Size);
        Clipboard.Clear;
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: Word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then
      Exit;
    DataPtr := GlobalLock(Data);
    if DataPtr = nil then
      Exit;
    try
      MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
      MemStream.Position := 0;
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    Result := tkUnknown
  else
    Result := PropInf^.PropType^.Kind;
end;

function GetPropStr(Obj: TObject; const PropName: string): string;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in
    [tkString, tkLString {$IFDEF COMPILER3_UP}, tkWString {$ENDIF COMPILER3_UP}]) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result := GetStrProp(Obj, PropInf);
end;

function GetPropOrd(Obj: TObject; const PropName: string): Integer;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in
    [tkInteger, tkChar, tkWChar, tkEnumeration, tkClass]) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result := GetOrdProp(Obj, PropInf);
end;

function GetPropMethod(Obj: TObject; const PropName: string): TMethod;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind = tkMethod) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result := GetMethodProp(Obj, PropInf);
end;

procedure PrepareIniSection(SS: TStrings);
var
  I: Integer;
  S: string;
begin
  I := 0;
  while I < Ss.Count do
  begin
    S := Trim(Ss[I]);
    if (Length(S) = 0) or (S[1] in [';', '#']) then
      Ss.Delete(I)
    else
      Inc(I);
  end;
end;

{:Creates a TPointL structure from a pair of coordinates.
Call PointL to create a TPointL structure that represents the specified
coordinates. Use PointL to construct parameters for functions
that require a TPointL, rather than setting up local variables
for each parameter.
@param  X    The X coordinate.
@param  Y    The Y coordinate.
@return      A TPointL structure for coordinates X and Y.
@example        <Code>
var
  p: TPointL;
begin
  p := PointL(100, 100);
end;
</Code>
}

function PointL(const X, Y: Longint): TPointL;
begin
  Result.X := X;
  Result.Y := Y;
end;

{:Conditional assignment.
Returns the value in True or False depending on the condition Test.
@param  Test    The test condition.
@param  True    Returns this value if Test is True.
@param  False   Returns this value if Test is False.
@return         Value in True or False depending on Test.
@example        <Code>
bar := iif(foo, 1, 0);
</Code>
<br>has the same effects as:<br>
<Code>
if foo then
  bar := 1
else
  bar := 0;
</Code>
}

function iif(const Test: Boolean; const ATrue, AFalse: Variant): Variant;
begin
  if Test then
    Result := ATrue
  else
    Result := AFalse;
end;

end.

