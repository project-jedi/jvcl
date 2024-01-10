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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : common routines

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvUtils;

interface

{$DEFINE INCLUDE_RAUTILSW}

uses
  Windows, Messages, Forms, Controls, Graphics, SysUtils, Classes,
  StdCtrls, ExtCtrls, Dialogs, Menus {$IFDEF COMPILER3_UP}, ShlObj{$ENDIF},
  Clipbrd,
  {$IFDEF COMPILER3_UP} ActiveX {$ELSE} Ole2 {$ENDIF},
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF}
  TypInfo;


 { This unit contain a lot of routines.}

{$IFNDEF COMPILER4_UP}
type
  longword = integer;
{$ENDIF}

type
  TTickCount = cardinal;


 {**** string handling routines}

const
  Separators : set of char = [#00,' ','-',#13, #10,'.',',','/','\', '#', '"', '''',
    ':','+','%','*','(',')',';','=','{','}','[',']', '{', '}', '<', '>'];
 {const Separators is used in GetWordOnPos, JvUtils.ReplaceSokr and SubWord}

{$IFDEF Delphi}
type
  TSetOfChar = set of char;
{$ENDIF Delphi}
{$IFDEF BCB}
type
  TSetOfChar = string;
{$ENDIF BCB}

  { GetWordOnPos returns word from string, S,
    on the cursor position, P}

  function GetWordOnPos(const S : string; const P : integer) : string;

  { GetWordOnPosEx working like GetWordOnPos function, but
    also returns word position in iBeg, iEnd variables }

  function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;

  { SubStr returns substring from string, S,
    separated with Separator string}

  function SubStr(const S : string; const index : integer; const Separator : string) : string;

  { SubStrEnd same to previous function but index numerated
    from the end of string }

  function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;

  { SubWord returns next word from string, P, and
    offsets pointer to the end of word, P2 }

  function SubWord(P : PChar; var P2 : PChar) : string;

  { NumberByWord returns the text representation of
    the number, N, in normal russian language.
    Was typed from Monitor magazine }

  function NumberByWord(const N : longint): string;


//  function CurrencyByWord(Value : Currency) : string;

  { GetLineByPos returns the Line number, there
    the symbol Pos is pointed.
    Lines separated with #13 symbol }

  function GetLineByPos(const S : string; const Pos : integer) : integer;

  { GetXYByPos is same to previous function, but
    returns X position in line too}

  procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);

  { ReplaceString searches for all substrings, OldPattern,
    in a string, S, and replaces them with NewPattern }

  function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;

  { ConcatSep concatenate S and S2 strings with Separator.
    if S = '', separator don't included }

  function ConcatSep(const S, S2, Separator : string) : string;

  { ConcatLeftSep is same to previous function, but
    strings concatenate right to left }

  function ConcatLeftSep(const S, S2, Separator : string) : string;

  { MinimizeString trunactes long string, S, and appends
    '...' symbols, if length of S is more than MaxLen }

  function MinimizeString(const S : string; const MaxLen : integer) : string;

  { Next 4 function for russian chars transliterating.
    This functions are needed because Oem2Ansi and Ansi2Oem functions
    sometimes works sucks }

  procedure Dos2Win(var S : string);
  procedure Win2Dos(var S : string);
  function Dos2WinRes(const S : string) : string;
  function Win2DosRes(const S : string) : string;
  function Win2Koi(const S : string) : string;

  { Spaces returns string consists on N space chars }

  function Spaces(const N : integer) : string;

  { AddSpaces add spaces to string, S, if it length is smaller than N }

  function AddSpaces(const S : string; const N : integer) : string;

  { function LastDate for russian users only }
  { returns date relative to current date: 'два дн€ назад' }

  function LastDate(const Dat : TDateTime) : string;

  { CurrencyToStr format currency, Cur, using ffCurrency float format}

  function CurrencyToStr(const Cur : currency): string;

  { Cmp compares two strings and returns true if they
    are equal. Case-insensitive.}

  function Cmp(const S1, S2 : string) : boolean;

  { StringCat add S2 string to S1 and returns this string }

  function StringCat(var S1 : string; S2 : string) : string;

  { HasChar returns true, if char, Ch, contains in string, S }

  function HasChar(const Ch : Char; const S : string) : boolean;

  function HasAnyChar(const Chars : string; const S : string) : boolean;

  function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;

  function CountOfChar(const Ch : Char; const S : string) : Integer;

  function DefStr(const S: string; Default: string): string;

 {#### string handling routines}


 {**** files routines}

  { GetWinDir returns Windows folder name }

  function GetWinDir : TFileName;

  { GetTempDir returns Windows temporary folder name }

  function GetTempDir : string;

  { GenTempFileName returns temporary file name on
    drive, there FileName is placed }

  function GenTempFileName(FileName : string) : string;

  { GenTempFileNameExt same to previous function, but
    returning filename has given extension, FileExt }

  function GenTempFileNameExt(FileName : string; const FileExt : string) : string;

  { ClearDir clears folder Dir }

  function ClearDir(const Dir : string) : boolean;

  { DeleteDir clears and than delete folder Dir }

  function DeleteDir(const Dir : string) : boolean;

  { FileEquMask returns true if file, FileName,
    is compatible with given dos file mask, Mask }

  function FileEquMask(FileName, Mask : TFileName) : boolean;

  { FileEquMasks returns true if file, FileName,
    is compatible with given Masks.
    Masks must be separated with comma (';') }

  function FileEquMasks(FileName, Masks : TFileName) : boolean;


  procedure DeleteFiles(const Folder : TFileName; const Masks : string);

  { LZFileExpand expand file, FileSource,
    into FileDest. Given file must be compressed, using MS Compress program }

  function LZFileExpand(const FileSource, FileDest : string) : boolean;

  { FileGetInfo fills SearchRec record for specified file attributes}

  function FileGetInfo(FileName : TFileName; var SearchRec : TSearchRec) : boolean;

  { HasSubFolder returns true, if folder APath contains other folders }

  function HasSubFolder(APath : TFileName) : boolean;

  { IsEmptyFolder returns true, if there are no files or
    folders in given folder, APath}

  function IsEmptyFolder(APath : TFileName) : boolean;

  { AddSlash add slash char to Dir parameter, if needed }

  procedure AddSlash(var Dir : TFileName);

  { AddSlash returns string with added slash char to Dir parameter, if needed }

  function AddSlash2(const Dir : TFileName) : string;

  { AddPath returns FileName with Path, if FileName not contain any path }

  function AddPath(const FileName, Path : TFileName) : TFileName;

  function AddPaths(const PathList, Path: string): string;

  function ParentPath(const Path: TFileName): TFileName;

  function FindInPath(const FileName, PathList: string): TFileName;

 {$IFNDEF BCB1}

  { BrowseForFolder displays Browse For Folder dialog }

  function BrowseForFolder(const Handle : HWnd; const Title : string; var Folder : string) : boolean;

 {$ENDIF BCB1}

  { DeleteReadOnlyFile clears R/O file attribute and delete file }

  function DeleteReadOnlyFile(const FileName : TFileName) : boolean;

  { HasParam returns true, if program running with
    specified parameter, Param }

  function HasParam(const Param : string) : boolean;

  function HasSwitch(const Param : string) : boolean;
  function Switch(const Param : string) : string;

  { ExePath returns ExtractFilePath(ParamStr(0)) }

  function ExePath : TFileName;

  function CopyDir(const SourceDir, DestDir: TFileName): Boolean;

  function FileTimeToDateTime(const FT: TFileTime): TDateTime;

  function MakeValidFileName(const FileName : TFileName;
    const ReplaceBadChar : Char) : TFileName;

 {#### files routines }


 {**** Graphic routines }

  { TTFontSelected returns true, if True Type font
    is selected in specified device context }

  function TTFontSelected(const DC : HDC) : boolean;

  { TrueInflateRect inflates rect in other
    method, than InflateRect API function }

  function TrueInflateRect(const R : TRect; const I : integer) : TRect;

 {#### Graphic routines }



 {**** Windows routines }

  { SetWindowTop put window to top without recreating window }

  procedure SetWindowTop(const Handle : HWND; const Top : boolean);

 {#### Windows routines }



 {**** other routines }

  { KeyPressed returns true, if Key VK is now pressed }

  function KeyPressed(VK : integer) : boolean;

  { functions Max and Min not need comments }

  function Max(x, y :integer):integer;

  function Min(x, y :integer):integer;

  procedure SwapInt(var Int1, Int2: Integer);

  function IntPower(Base, Exponent : integer) : integer;

  function ChangeTopException(E : TObject): TObject;

  function StrToBool(const S: string): Boolean;

 {#### other routines }



 {$IFNDEF COMPILER3_UP}

{ AnsiStrLIComp compares S1 to S2, without case-sensitivity, up to a maximum
  length of MaxLen bytes. The compare operation is controlled by the
  current Windows locale. The return value is the same as for CompareStr. }

  function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
  function AnsiStrIComp(S1, S2: PChar): Integer;

 {$ENDIF}

  function Var2Type(V : Variant; const VarType : integer) : variant;

  function VarToInt(V : Variant) : Integer;

  function VarToFloat(V : Variant) : Double;

 { following functions are not documented
   because they are don't work properly sometimes,
   so don't use them }

  function ReplaceSokr1(S : string; const Word, Frase : string) : string;
  { ReplaceSokr1 is full equal to ReplaceString function
    - only for compatibility - don't use }

  { GetSubStr is full equal to SubStr function
    - only for compatibility - don't use }
  { GetSubStr - устаревша€, используйте SubStr }

  function GetSubStr(const S : string; const index : integer; const Separator : Char) : string;

  function GetParameter : string;
  function GetLongFileName(FileName : string) : string;
  {* from unit FileCtrl}
  function DirectoryExists(const Name: string): Boolean;
  procedure ForceDirectories(Dir: string);
  {# from unit FileCtrl}
  function FileNewExt(const FileName, NewExt : TFileName) : TFileName;
  function GetComputerID : string;

  function GetComputerName: string;


 {**** string routines - строковые функции}

  { ReplaceAllSokr searches for all substrings, Words,
    in a string, S, and replaces them with Frases with the same index.
    Also see RAUtilsW.ReplaceSokr1 function }

  function ReplaceAllSokr(S : string; Words, Frases : TStrings) : string;

  { ReplaceSokr searches the word in a string, S, on PosBeg position,
    in the list, Words, and if founds, replaces this word
    with string from another list, Frases, with the same index,
    and then update NewSelStart variable }

  function ReplaceSokr(S : string; PosBeg, Len : integer; Words, Frases : TStrings; var NewSelStart : integer) : string;

  { CountOfLines calculates the lines count in a string, S,
    each line must be separated from another with #13#10 sequence }

  function CountOfLines(const S : string) : integer;

  { DeleteEmptyLines deletes all empty lines from strings, Ss.
    Lines contained only spaces also deletes. }

  procedure DeleteEmptyLines(Ss : TStrings);

  { SQLAddWhere addes or modifies existing where-statement, where,
    to the strings, SQL.
    Note: If strings SQL allready contains where-statement,
    it must be started on the begining of any line }

  procedure SQLAddWhere(SQL : TStrings; const where : string);

 {#### string routines - строковые функции}


 {**** files routines - файловые функции}

  { ResSaveToFile save resource named as Name with Typ type into file FileName.
    Resource can be compressed using MS Compress program}

  function ResSaveToFile(const Typ, Name : string; const Compressed : boolean; const FileName : string) : boolean;

  function ResSaveToFileEx(Instance: HINST; Typ, Name : PChar;
    const Compressed : boolean; const FileName : string) : boolean;

  function ResSaveToString(Instance: HINST; const Typ, Name : string;
    var S: string) : boolean;

  { Execute executes other program and waiting for it
    terminating, then return it exit code }

  function Execute(const CommandLine, WorkingDirectory : string) : integer;

  { IniReadSection read section, Section, from ini-file,
    IniFileName, into strings, Ss.
    This function reads ALL strings from specified section.
    Note: TIninFile.ReadSection function reads only strings with '=' symbol.}

  function IniReadSection(const IniFileName : TFileName; const Section : string; Ss : TStrings) : boolean;

  { LoadTextFile load text file, FileName, into string }

  function LoadTextFile(const FileName : TFileName): string;

  procedure SaveTextFile(const FileName : TFileName; const Source : string);

  { ReadFolder reads files list from disk folder, Folder,
    that are equal to mask, Mask, into strings, FileList}

  function ReadFolder(const Folder, Mask : TFileName; FileList : TStrings) : integer;

  function ReadFolders(const Folder : TFileName; FolderList : TStrings) : integer;
 {#### files routines - файловые функции}

 {$IFDEF COMPILER3_UP}
  { TargetFileName - if FileName is ShortCut returns filename ShortCut linked to }
  function TargetFileName(const FileName: TFileName): TFileName;

  { return filename ShortCut linked to }
  function ResolveLink(const hwnd: HWND; const LinkFile: TFileName;
    var FileName: TFileName): HRESULT;
 {$ENDIF COMPILER3_UP}

 {**** Graphic routines - графические функции}

  { LoadIcoToImage loads two icons from resource named NameRes,
    into two image lists ALarge and ASmall}

  procedure LoadIcoToImage(ALarge, ASmall : TImageList; const NameRes : string);

  { RATextOut same with TCanvas.TextOut procedure, but
    can clipping drawing with rectangle, RClip. }

  procedure RATextOut(Canvas : TCanvas; const R, RClip : TRect; const S : string);

  { RATextOutEx same with RATextOut function, but
    can calculate needed height for correct output }

  function RATextOutEx(Canvas : TCanvas; const R, RClip : TRect; const S : string; const CalcHeight : boolean) : integer;

  { RATextCalcHeight calculate needed height for
    correct output, using RATextOut or RATextOutEx functions }

  function RATextCalcHeight(Canvas : TCanvas; const R: TRect; const S : string) : integer;

  { Cinema draws some visual effect }

  procedure Cinema(Canvas : TCanvas; rS{Source}, rD{Dest} : TRect);

  { Roughed fills rect with special 3D pattern }

  procedure Roughed(ACanvas : TCanvas; const ARect : TRect; const AVert : boolean);

  { BitmapFromBitmap creates new small bitmap from part
    of source bitmap, SrcBitmap, with specified width and height,
    AWidth, AHeight and placed on a specified index, index in the
    source bitmap }

  function BitmapFromBitmap(SrcBitmap : TBitmap; const AWidth, AHeight, index : integer) : TBitmap;

  { TextWidth calculate text with for writing using standard desktop font }

  function TextWidth(AStr : string) : integer;

  { DefineCursor load cursor from resource, and return
    available cursor number, assigned to it }

  function DefineCursor(Identifer : PChar) : TCursor;

 {#### Graphic routines - графические функции}



 {**** other routines - прочие функции}

  { FindFormByClass returns first form with specified
    class, FormClass, owned by Application global variable }

  function FindFormByClass(FormClass : TFormClass) : TForm;

  function FindFormByClassName(FormClassName : string) : TForm;

  { FindByTag returns the control with specified class,
    ComponentClass, from WinContol.Controls property,
    having Tag property value, equaled to Tag parameter }

  function FindByTag(WinControl : TWinControl; ComponentClass : TComponentClass; const Tag : integer) : TComponent;

  { ControlAtPos2 equal to TWinControl.ControlAtPos function,
    but works better }

  function ControlAtPos2(Parent : TWinControl; X, Y : integer) : TControl;

  { RBTag searches WinControl.Controls for checked
    RadioButton and returns its Tag property value }
   {¬озвращает Tag выбранного TRadioButton дл€ заданного Parent}

  function RBTag(Parent : TWinControl) : integer;

  { AppMinimized returns true, if Application is minimized }

  function AppMinimized : boolean;

  { MessageBox is Application.MessageBox with string (not PChar) parameters.
    if Caption parameter = '', it replaced with Application.Title }

  function MessageBox(const Message : string; Caption : string;
    const Flags : integer) : integer;

  function MsgDlg2(const Msg, ACaption : string; DlgType : TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpContext : integer; Control : TWinControl): Integer;

  function MsgDlgDef(const Msg, ACaption : string; DlgType : TMsgDlgType;
    Buttons: TMsgDlgButtons; DefButton : TMsgDlgBtn; HelpContext : integer;
    Control : TWinControl): Integer;

  { Delay stop program execution to MSec msec }

  procedure Delay(MSec : longword);


  procedure CenterHor(Parent : TControl; MinLeft : integer; Controls : array of TControl);

  procedure EnableControls(Control : TWinControl; const Enable : boolean);

  procedure EnableMenuItems(MenuItem : TMenuItem; const Tag : integer; const Enable : boolean);

  procedure ExpandWidth(Parent : TControl; MinWidth : integer; Controls : array of TControl);

  function PanelBorder(Panel : TCustomPanel) : integer;

  function Pixels(Control : TControl; APixels : integer) : integer;

  procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint);

  procedure Error(const Message : string);

  procedure ItemHtDrawEx(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean; var PlainItem: string;
    var Width: Integer; CalcWidth: Boolean);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <c:Red>red <c:Green>green <c:blue>blue </i>' }

  function ItemHtDraw(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean): string;

  function ItemHtWidth(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean): Integer;

  function ItemHtPlain(const Text : string): string;

 { ClearList - clears list of TObject }
  procedure ClearList(List: TList);

 {#### other routines - прочие функции}

  procedure MemStreamToClipBoard(MemStream : TMemoryStream; const Format : word);

  procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format : word);


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

  procedure ShowMenu(Form : TForm; MenuAni : TMenuAnimation);

type
  TProcObj = procedure of object;

  procedure ExecAfterPause(Proc : TProcObj; Pause : integer);

const
  NoHelp = 0; { for MsgDlg2 }
  MsgDlgCharSet : Integer = DEFAULT_CHARSET;

implementation

uses JvCtlConst;


function GetLineByPos(const S : string; const Pos : integer) : integer;
var
  i : integer;
begin
  if Length(S) < Pos then Result := -1
  else begin
    i := 1;
    Result := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then inc(Result);
      inc(i);
    end;
  end;
end;

procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
var
  i, iB : integer;
begin
  X := -1; Y := -1; iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then begin
    i := 1;
    Y := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then begin inc(Y); iB := i+1 end;
      inc(i);
    end;
    X := Pos - iB;
  end;
end;

function GetWordOnPos(const S : string; const P : integer) : string;
var
  i, Beg : integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  for i := P downto 1 do
    if S[i] in Separators then break;
  Beg := i + 1;
  for i := P to Length(S) do
    if S[i] in Separators then break;
  if i > Beg then
    Result := Copy(S, Beg, i-Beg) else
    Result := S[P];
end;

function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  iBeg := P;
  if P > 1 then
    if S[P] in Separators then
      if (P < 1) or ((P - 1 > 0) and (S[P-1] in Separators)) then
        inc(iBeg)
      else if not ((P - 1 > 0) and (S[P-1] in Separators)) then
        dec(iBeg);
  while iBeg >= 1 do
    if S[iBeg] in Separators then break else dec(iBeg);
  inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then break else inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg) else
    Result := S[P];
end;

function GetWinDir : TFileName;
var
  WinDir  : array[0..MAX_PATH] of char;
begin
  WinDir[GetWindowsDirectory(WinDir, MAX_PATH)] := #0;
  Result := WinDir;
end;

function GenTempFileName(FileName : string) : string;
var
  TempDir  : array[0..MAX_PATH] of char;
  TempFile : array[0..MAX_PATH] of char;
  STempDir : TFileName;
  Res : integer;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  if FileName <> '' then begin
    if Length(FileName) < 4 then FileName := ExpandFileName(FileName);
    if (Length(FileName) > 4) and (FileName[2] = ':')
        and (StrLen(@TempDir[0]) > 4)
        and (ANSICompareText(TempDir[0], FileName[1]) <> 0)
    then begin
      STempDir := ExtractFilePath(FileName);
      Move(STempDir[1], TempDir, Length(STempDir)+1);
    end;
  end;
  Res := GetTempFileName(
    TempDir, { address of directory name for temporary file}
    '~RA',   { address of filename prefix}
    0,       { number used to create temporary filename}
    TempFile { address of buffer that receives the new filename}
   );
  if Res <> 0 then Result := TempFile else Result := '~JVCLTemp.tmp';
  DeleteFile(Result);
end;

function GenTempFileNameExt(FileName : string; const FileExt : string) : string;
begin
  Result := ChangeFileExt(GenTempFileName(FileName), FileExt);
end;

function GetTempDir : string;
var
  TempDir  : array[0..MAX_PATH] of char;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  Result := TempDir;
end;

function ClearDir(const Dir : string) : boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
  Path : TFileName;
begin
  Result := True;
  Path := Dir;
  AddSlash(Path);
  DosError := FindFirst(Path+'*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if ((SearchRec.Attr and faDirectory) = faDirectory)then
        Result := Result and DeleteDir(Path+SearchRec.Name)
      else
        Result := Result and DeleteFile(Path+SearchRec.Name);
      // if not Result then exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function DeleteDir(const Dir : string) : boolean;
begin
  ClearDir(Dir);
{  if Dir[Length(Dir)] = '\' then Dir[Length(Dir)] := #0;}
  Result := RemoveDir(Dir);
end;

procedure DeleteFiles(const Folder : TFileName; const Masks : string);
var
  SearchRec : TSearchRec;
  DosError  : integer;
  Path : TFileName;
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


function GetParameter : string;
var
  FN, FN1 : PChar;
begin
  if ParamCount = 0 then begin Result := ''; exit end;
  FN := cmdLine;
  if FN[0] = '"' then begin
    FN := StrScan(FN+1, '"');
    if (FN[0] = #00) or (FN[1] = #00)
    then Result := ''
    else begin
      inc(FN, 2);
      if FN[0] = '"' then begin
        inc(FN, 1);
        FN1 := StrScan(FN+1, '"');
        if FN1[0] <> #00 then FN1[0] := #00;
      end;
      Result := FN;
    end;
  end else Result := Copy(CmdLine, Length(ParamStr(0))+1, 260);{Ќе работает дл€ длинных имен с пробелами}
  while (Length(Result)>0) and (Result[1] = ' ') do Delete(Result, 1, 1);
  Result := ReplaceString(Result, '"', '');
  if FileExists(Result) then
    Result := GetLongFileName(Result);
end;


function GetLongFileName(FileName : string) : string;
var
  SearchRec : TSearchRec;
begin
  if FileGetInfo(FileName, SearchRec) then
    Result := ExtractFilePath(ExpandFileName(FileName))+SearchRec.FindData.cFileName
  else
    Result := FileName;
end;

function FileEquMask(FileName, Mask : TFileName) : boolean;
var
  i : integer;
  C : char;
  P : PChar;
begin
  FileName := ANSIUpperCase(ExtractFileName(FileName));
  Mask := ANSIUpperCase(Mask);
  Result := false;
  if Pos('.', FileName) = 0 then FileName := FileName+'.';
  i := 1; P := PChar(FileName);
  while (i <= length(Mask)) do begin
    C := Mask[i];
    if (P[0] = #0) and (C <> '*') then exit;
    case C of
      '*' :
        if i = length(Mask) then begin
          Result := true;
          exit;
        end else begin
          P := StrScan(P, Mask[i+1]);
          if P = nil then exit;
        end;
      '?' : inc(P);
      else if C = P[0] then inc(P) else exit;
    end;
    inc(i);
  end;
  if P[0] = #0 then Result := true;
end;

function FileEquMasks(FileName, Masks : TFileName) : boolean;
var
  i : integer;
  Mask : string;
begin
  Result := false;
  i := 0;
  Mask := Trim(GetSubStr(Masks, i, ';'));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask) then begin
      Result := true;
      break;
    end else begin
      inc(i);
      Mask := Trim(GetSubStr(Masks, i, ';'));
    end;
end;

function NumberByWord(const N : longint): string;
const
  Ten : array[0..9] of string = ('ноль',  'один',  'два',    'три', 'четыре',
                                 'п€ть', 'шесть', 'семь', 'восемь', 'дев€ть');
  Hun : array[1..9] of string = ('сто', 'двести', 'триста', 'четыреста', 'п€тьсот',
                                 'шестьсот', 'семьсот', 'восемьсот', 'дев€тьсот');
  OnTen : array[10..19] of string = ('дес€ть', 'одиннадцать', 'двенадцать', 'тринадцать',
                                     'четырнадцать', 'п€тнадцать', 'шестнадцать',
                                     'семнадцать', 'восемнадцать', 'дев€тнадцать');
  HunIn : array[2..9] of string = ('двадцать', 'тридцать', 'сорок', 'п€тьдес€т',
                                   'шестьдес€т', 'семьдес€т', 'восемьдес€т', 'дев€носто');

var
  StrVsp  : string;
  NumStr  : string;
  StrVsp2 : string;
  i       : byte;

  function IndNumber(Stri : string; Place : byte) : byte;
  begin
    IndNumber := Ord(Stri[Place]) - 48;
  end;

  function Back(Stri : string) : longint;
  var
    code : integer;
    LI   : longint;
  begin
    Result := 0;
    Val(Stri, LI, code);
    if (code = 0) then Result := LI;
  end;

begin
  NumStr := IntToStr(N);
  if (Length(NumStr) > 9) then begin
    Result := '*****';
    Exit;
  end;
  case Length(NumStr) of
    1 : StrVsp := Ten[N];
    2 : case NumStr[1] of
          '1'      :   StrVsp := OnTen[N];
          '0'      :   StrVsp := NumberByWord(IndNumber(NumStr, 2));
          '2'..'9' : begin
            StrVsp := HunIn[IndNumber(NumStr, 1)];
            if NumStr[2] <> '0' then
              StrVsp := StrVsp + ' ' + NumberByWord(IndNumber(NumStr, 2));
          end;
        end;
    3 : begin
          StrVsp := Hun[IndNumber(NumStr, 1)];
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 2)));
        end;
    4 : begin
          StrVsp := Ten[IndNumber(NumStr, 1)];
          case NumStr[1] of
            '1'      : StrVsp := 'одна тыс€ча';
            '2'      : StrVsp := 'две тыс€чи';
            '3', '4' : StrVsp := StrVsp + ' тыс€чи';
            '5'..'9' : StrVsp := StrVsp + ' тыс€ч';
          end;
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 3)));
        end;
    5 : begin
          StrVsp2 := NumberByWord(Back(Copy(NumStr, 1, 2)));
          i := Pos(' два', StrVsp2);
          if (Pos(' два', StrVsp2) = i) then i := 0;
          if (i <> 0) then StrVsp2[i+3] := 'e';
          i := Pos(' один', StrVsp2);
          if (Pos(' одинн', StrVsp2) = i) then i := 0;
          if (i <> 0) then begin
            StrVsp2[i+3] := 'н';
            StrVsp2[i+4] := 'а';
          end;
          if NumStr[1] <> '1' then case NumStr[2] of
            '1'      : StrVsp := ' тыс€ча ';
            '2'..'4' : StrVsp := ' тыс€чи ';
            '5'..'9' : StrVsp := ' тыс€ч ';
          end else StrVsp := ' тыс€ч ';
          StrVsp := StrVsp2 + StrVsp + NumberByWord(Back(Copy(NumStr, 3, 3)));
        end;
    6 : begin
          StrVsp2 :=NumberByWord(Back(Copy(NumStr, 1, 3)));
          i := Pos(' два', StrVsp2);
          if (Pos(' двад', StrVsp2) = i) then i := 0;
          if (i <> 0) then StrVsp2[i+3] := 'е';
          i := Pos(' один', Strvsp2);
          if (Pos(' одинн', StrVsp2) = i) then i := 0;
          if (i <> 0) then begin
            StrVsp2[i+3] := 'н';
            StrVsp2[i+4] := 'а';
          end;
          if NumStr[2] <> '1' then case numStr[3] of
            '1'      : StrVsp := ' тыс€ча ';
            '2'..'4' : StrVsp := ' тыс€чи ';
            '5'..'9' : StrVsp := ' тыс€ч ';
          end else StrVsp := ' тыс€ч ';
          StrVsp := StrVsp2 + StrVsp + NumberByWord(Back(Copy(NumStr, 4, 3)));
        end;
    7 : begin
          StrVsp := Ten[IndNumber(NumStr, 1)];
          case NumStr[1] of
            '1'      : StrVsp := 'один миллион';
            '2'..'4' : StrVsp := StrVsp + ' миллиона';
            '5'..'9' : StrVsp := StrVsp + ' миллионов';
          end;
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 6)));
        end;
    8 : begin
          StrVsp := NumberByWord(Back(Copy(NumStr, 1, 2)));
          StrVsp := StrVsp + ' миллион';
          if (NumStr[1] <> '1') then case NumStr[2] of
            '2'..'4'     : StrVsp := StrVsp + 'а';
            '0','5'..'9' : StrVsp := StrVsp + 'ов';
          end else StrVsp := StrVsp + 'ов';
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 3, 6)));
        end;
    9 : begin
          StrVsp := NumberByWord(Back(Copy(Numstr, 1, 3)));
          StrVsp := StrVsp + ' миллион';
          if (NumStr[2] <> '1') then case NumStr[3] of
            '2'..'4'      : StrVsp := StrVsp + 'а';
            '0', '5'..'9' : StrVsp := StrVsp + 'ов';
          end else StrVsp := StrVsp + 'ов';
          StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 4, 6)));
        end;
  end;
  if ((Length(StrVsp) > 4) and (Copy(StrVsp, Length(StrVsp)-3, 4) = Ten[0])) then
    StrVsp := Copy(StrVsp, 1, Length(StrVsp) - 4);
  Result := StrVsp;
end;

function GetSubStr(const S : string; const index : integer; const Separator : Char) : string;
begin
  Result := SubStr(S, index, Separator);
end;

function SubStr(const S : string; const index : integer; const Separator : string) : string;
 {¬ырезает подстроку. ѕодстроки раздел€ютс€ символом Sep}
var
  i : integer;
  pB, pE : PChar;
begin
  Result := '';
  if ((index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
    if pB[0] = #0 then exit;
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;
 {то же что и SubStr, но подстроки нумеруютс€ с конца}
var
  MaxIndex : integer;
  pB : PChar;
begin
 {неоптимальна€ реализаци€}
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do begin
    inc(MaxIndex);
    pB := StrPos(pB+Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - index, Separator);
end;

function FileGetInfo(FileName : TFileName; var SearchRec : TSearchRec) : boolean;
var
  DosError  : integer;
  Path : TFileName;
begin
  Result := false;
  Path := ExtractFilePath(ExpandFileName(FileName))+'*.*';
  FileName := ANSIUpperCase(ExtractFileName(FileName));
  DosError := FindFirst(Path, faAnyFile, SearchRec);
  while DosError = 0 do begin
    if (ANSICompareText(SearchRec.FindData.cFileName, FileName) = 0)
    or (ANSICompareText(SearchRec.FindData.cAlternateFileName, FileName) = 0)
    then begin
      Result := true;
      break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function HasSubFolder(APath : TFileName) : boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  Result := false;
  AddSlash(APath);
  APath := Concat(APath, '*.*');
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do begin
    if (SearchRec.Attr and faDirectory = faDirectory) and (SearchRec.Name[1] <> '.') then begin
      Result := true;
      break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function IsEmptyFolder(APath : TFileName) : boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  Result := true;
  AddSlash(APath);
  APath := Concat(APath, '*.*');
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do begin
    if SearchRec.Name[1] <> '.' then begin
      Result := false;
      break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function TTFontSelected(const DC : HDC) : boolean;
var
  TM : TTEXTMETRIC;
begin
  GetTextMetrics(DC, TM);
  Result := TM.tmPitchAndFamily and TMPF_TRUETYPE <> 0;
end;

function SubWord(P : PChar; var P2 : PChar) : string;
var
  i : integer;
begin
  i := 0;
  while not (P[i] in Separators) do inc(i);
  SetString(Result, P, i);
  P2 := P+i;
end;

function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;
var
  LW : integer;
  P : PChar;
  Sm : integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do begin
    Sm := P-PChar(S);
    S := Copy(S, 1, Sm)+NewPattern+Copy(S, Sm+LW+1, Length(S));
    P := StrPos(PChar(S)+Sm+Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

function ReplaceSokr1(S : string; const Word, Frase : string) : string;
begin
  Result := ReplaceString(S, Word, Frase);
end;

function ConcatSep(const S, S2, Separator : string) : string;
begin
  Result := S;
  if Result <> '' then Result := Result + Separator;
  Result := Result + S2;
end;

function ConcatLeftSep(const S, S2, Separator : string) : string;
begin
  Result := S;
  if Result <> '' then Result := Separator + Result;
  Result := S2 + Result;
end;

function MinimizeString(const S : string; const MaxLen : integer) : string;
begin
  if Length(S) > MaxLen then
    if MaxLen < 3 then
      Result := Copy(S, 1, MaxLen)
    else
      Result := Copy(S, 1, MaxLen-3) + '...'
  else
    Result := S;
end;

function TrueInflateRect(const R : TRect; const I : integer) : TRect;
begin
  with R do SetRect(Result, Left - I, Top - I, Right + I, Bottom + I);
end;

procedure SetWindowTop(const Handle : HWND; const Top : boolean);
const
  TopFlag : array[boolean] of longword = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, TopFlag[Top], 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

{* from unit FileCtrl}

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := Integer(GetFileAttributes(PChar(Name)));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

procedure ForceDirectories(Dir: string);
begin
  if Dir[Length(Dir)] = '\' then
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; { avoid 'xyz:\' problem.}
  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;

{# from unit FileCtrl}

function LZFileExpand(const FileSource, FileDest : string) : boolean;
type
  TLZCopy     = function (Source, Dest: Integer): Longint; stdcall;
  TLZOpenFile = function (Filename: PChar; var ReOpenBuff: TOFStruct; Style: Word): Integer; stdcall;
  TLZClose    = procedure (hFile: Integer); stdcall;
var
  Source, Dest : integer;
  OSSource, OSDest : TOFSTRUCT;
  Res : integer;
  Ins : integer;
  LZCopy     : TLZCopy;
  LZOpenFile : TLZOpenFile;
  LZClose    : TLZClose;
begin
  Result := false;
  Ins := LoadLibrary('LZ32.dll');
  try
    LZCopy     := GetProcAddress(Ins, 'LZCopy');
    LZOpenFile := GetProcAddress(Ins, 'LZOpenFileA');
    LZClose    := GetProcAddress(Ins, 'LZClose');
    OSSource.cBytes := sizeof(TOFSTRUCT);
    OSDest.cBytes := sizeof(TOFSTRUCT);
    Source := LZOpenFile(
      PChar(FileSource), // address of name of file to be opened
      OSSource, // address of open file structure
      OF_READ or OF_SHARE_DENY_NONE// action to take
     );
    if Source < 0 then begin
      DeleteFile(FileDest);
      Dest := LZOpenFile(
        PChar(FileDest), // address of name of file to be opened
        OSDest, // address of open file structure
        OF_CREATE or OF_WRITE or OF_SHARE_EXCLUSIVE// action to take
       );
      if Dest >= 0 then begin
        Res := LZCopy(Source, Dest);
        if Res >= 0 then Result := true;
      end;
      LZClose(Source);
      LZClose(Dest);
    end;
  finally
    FreeLibrary(Ins);
  end;
end;

procedure Dos2Win(var S : string);
var
  i : integer;
begin
  for i := 1 to Length(S) do
    case S[i] of
      #$80..#$AF : S[i] := char(byte(S[i])+(192-$80));
      #$E0..#$EF : S[i] := char(byte(S[i])+(240-$E0));
    end;
end;

procedure Win2Dos(var S : string);
var
  i : integer;
begin
  for i := 1 to Length(S) do
    case S[i] of
      #$C0..#$EF : S[i] := char(byte(S[i])-(192-$80));
      #$F0..#$FF : S[i] := char(byte(S[i])-(240-$E0));
    end;
end;

function Dos2WinRes(const S : string) : string;
begin
  Result := S;
  Dos2Win(Result);
end;

function Win2DosRes(const S : string) : string;
begin
  Result := S;
  Win2Dos(Result);
end;

function Win2Koi(const S : string) : string;
const
  W = 'абвгдеЄжзийклмнопрстуфхчцшщьыъэю€јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’„÷Ўў№џЏЁёя';
  K = 'Ѕ¬„«ƒ≈£÷Џ… ЋћЌќѕ–“”‘’∆»ё√џЁЎўя№ј—бвчзде≥цъйклмнопртуфхжиюгыэшщ€ьас';
var
  i, j : integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    j := Pos(Result[i], W);
    if j > 0 then
      Result[i] := K[j];
  end;
end;

function Spaces(const N : integer) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to N do Result := Result+' ';
end;

function AddSpaces(const S : string; const N : integer) : string;
begin
  Result := S;
  while Length(Result) < N do Result := Result+' ';
end;

function KeyPressed(VK : integer) : boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

{$IFNDEF BCB1}
function BrowseForFolder(const Handle : HWnd; const Title : string; var Folder : string) : boolean;
{$IFDEF COMPILER2}
type
  TSHItemID = packed record           { mkid }
    cb: Word;                         { Size of the ID (including cb itself) }
    abID: array[0..0] of Byte;        { The item ID (variable length) }
  end;
  PItemIDList = ^TItemIDList;
  TItemIDList = packed record         { idl }
     mkid: TSHItemID;
   end;
  TFNBFFCallBack = function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  TBrowseInfo = packed record
    hwndOwner: HWND;
    pidlRoot: PItemIDList;
    pszDisplayName: PAnsiChar;  { Return display name of item selected. }
    lpszTitle: PAnsiChar;      { text to go in the banner over the tree. }
    ulFlags: UINT;           { Flags that control the return stuff }
    lpfn: TFNBFFCallBack;
    lParam: LPARAM;          { extra info that's passed back in callbacks }
    iImage: Integer;         { output var: where to return the Image index. }
  end;
  function SHBrowseForFolder(var lpbi: TBrowseInfo): PItemIDList; stdcall; external 'shell32.dll' name 'SHBrowseForFolderA';
  function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL; stdcall external 'shell32.dll' name 'SHGetPathFromIDListA';
{$ENDIF}
var
  browseinfo : TBrowseinfo;
  Id : PItemIDList;
  FN : array[0..MAX_PATH] of char;
begin
  with browseinfo do begin
    hwndOwner := Handle;
    pidlRoot  := nil;
    pszDisplayName := FN;
    lpszTitle := PChar(Title);
    ulFlags := 0;
    lpfn := nil;
  end;
  Id := SHBrowseForFolder(browseinfo);
  Result := Id <> nil;
  if Result then begin
    SHGetPathFromIDList(Id, FN);
    Folder := FN;
  end;
end;
{$ENDIF BCB1}

function LastDate(const Dat : TDateTime) : string;
const
  D2D : array [0..9] of 1..3 = (3, 1, 2, 2, 2, 3, 3, 3, 3, 3);
  Day   : array [1..3] of string = ('день', 'дн€', 'дней');
  Month : array [1..3] of string = ('мес€ц', 'мес€ца', 'мес€цев');
  Year  : array [1..3] of string = ('год', 'года', 'лет');
  Week  : array [1..4] of string = ('неделю', '2 недели', '3 недели', 'мес€ц');
var
  Y, M, D : integer;
begin
  if Date = Dat then Result := 'сегодн€'
  else if Dat = Date - 1 then Result := 'вчера'
  else if Dat = Date - 2 then Result := 'позавчера'
  else if Dat > Date then Result := 'в будущем'
  else begin
    D := Trunc(Date - Dat);
    Y := Round(D / 365);
    M := Round(D / 30);
    if Y > 0 then
      Result := IntToStr(Y)+' '+Year[D2D[StrToInt(IntToStr(Y)[Length(IntToStr(Y))])]]+' назад'
    else if M > 0 then
      Result := IntToStr(M)+' '+Month[D2D[StrToInt(IntToStr(M)[Length(IntToStr(M))])]]+' назад'
    else if D > 6 then
      Result := Week[D div 7]+' назад'
    else if D > 0 then
      Result := IntToStr(D)+' '+Day[D2D[StrToInt(IntToStr(D)[Length(IntToStr(D))])]]+' назад'
  end;
end;

procedure AddSlash(var Dir : TFileName);
begin
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Dir := Dir +'\';
end;

function AddSlash2(const Dir : TFileName) : string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Result := Dir +'\';
end;

function AddPath(const FileName, Path : TFileName) : TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash2(Path) + FileName
  else
    Result := FileName;
end;

function AddPaths(const PathList, Path: string): string;
var
  i: Integer;
  S: string;
begin
  Result := '';
  i := 0;
  S := SubStr(PathList, i, ';');
  while S <> '' do
  begin
    Result := ConcatSep(Result, AddPath(S, Path), ';');
    inc(i);
    S := SubStr(PathList, i, ';');
  end;
end;

function ParentPath(const Path: TFileName): TFileName;
begin
  Result := Path;
  if (Length(Result) > 0) and (Result[Length(Result)] = '\') then
    Delete(Result, Length(Result), 1);
  Result := ExtractFilePath(Result);
end;

function FindInPath(const FileName, PathList: string): TFileName;
var
  i: Integer;
  S: string;
begin
  i := 0;
  S := SubStr(PathList, i, ';');
  while S <> '' do
  begin
    Result := AddSlash2(S) + FileName;
    if FileExists(Result) then
      Exit;
    inc(i);
    S := SubStr(PathList, i, ';');
  end;
  Result := '';
end;

function GetComputerID : string;
var
  SN  : DWORD;
  Nul : DWORD;
  WinDir  : array[0..MAX_PATH] of char;
begin
  GetWindowsDirectory(WinDir, MAX_PATH);
  WinDir[3] := #0;
  if GetVolumeInformation(
    WinDir,   // address of root directory of the file system
    nil,   // address of name of the volume
    0,     // length of lpVolumeNameBuffer
    @SN,    // address of volume serial number
    Nul,   // address of system's maximum filename length
    Nul,   // address of file system flags
    nil,   // address of name of file system
    0      // length of lpFileSystemNameBuffer
   )
  then
    Result := IntToHex(SN, 8)
  else
    Result := 'None';
end;

function CurrencyToStr(const Cur : currency): string;
begin
  Result := CurrToStrF(Cur, ffCurrency, CurrencyDecimals)
end;

function Cmp(const S1, S2 : string) : boolean;
begin
  //Result := ANSICompareText(S1, S2) = 0;
  Result := ANSIStrIComp(PChar(S1), PChar(S2)) = 0;
end;

function StringCat(var S1 : string; S2 : string) : string;
begin
  S1 := S1 + S2;
  Result := S1;
end;

function HasChar(const Ch : Char; const S : string) : boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function HasAnyChar(const Chars : string; const S : string) : boolean;
var
  i : integer;
begin
  for i := 1 to Length(Chars) do
    if HasChar(Chars[i], S) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function CountOfChar(const Ch : Char; const S : string) : Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = Ch then
      inc(Result);
end;

function Max(x,y:integer):integer;
begin
  if x > y then Result := x else Result := y;
end;

function Min(x,y:integer):integer;
begin
  if x < y then Result := x else Result := y;
end;

procedure SwapInt(var Int1, Int2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Int1;
  Int1 := Int2;
  Int2 := Tmp;
end;

function DeleteReadOnlyFile(const FileName : TFileName) : boolean;
begin
  FileSetAttr(FileName, 0); {clear Read Only Flag}
  Result := DeleteFile(FileName);
end;

function HasParam(const Param : string) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 1 to ParamCount do begin
    Result := Cmp(ParamStr(i), Param);
    if Result then exit;
  end;
end;

function HasSwitch(const Param : string) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 1 to ParamCount do
    if HasChar(ParamStr(i)[1], '-/') then
    begin
      Result := Cmp(Copy(ParamStr(i), 2, Length(Param)), Param);
      if Result then exit;
    end;
end;

function Switch(const Param : string) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    if HasChar(ParamStr(i)[1], '-/\') and
       Cmp(Copy(ParamStr(i), 2, Length(Param)), Param) then
    begin
      Result := Copy(ParamStr(i), 2 + Length(Param), 260);
      exit;
    end;
end;

function ExePath : TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function FileNewExt(const FileName, NewExt : TFileName) : TFileName;
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


function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;
begin
{$IFDEF Delphi}
  Result := Ch in SetOfChar;
{$ENDIF Delphi}
{$IFDEF BCB}
  Result := Pos(Ch, SetOfChar) > 0;
{$ENDIF BCB}
end;

function IntPower(Base, Exponent : integer) : integer;
begin
  if Exponent > 0 then
  begin
    Result := Base;
    dec(Exponent);
    while Exponent > 0 do
    begin
      Result := Result * Base;
      dec(Exponent);
    end;
  end else
  if Exponent < 0 then
  begin
    Result := 1;
    inc(Exponent);
    while Exponent < 0 do
    begin
      Result := Result div Base;
      inc(Exponent);
    end;
  end else
    Result := Base;
end;

function ChangeTopException(E : TObject): TObject;
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
  { if linker error occurred with message "unresolved external 'System::RaiseList'" try
    comment this function implementation, compile,
    then uncomment and compile again. }
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := E
  end else
   Result := nil;
//    raise Exception.Create('Not in exception');
end;

function MakeValidFileName(const FileName : TFileName;
	const ReplaceBadChar : Char) : TFileName;
var
  i : Integer;
begin
  Result := FileName;
  for i := 1 to Length(Result) do
    if HasChar(Result[i], '''":?*\/') then
      Result[i] := ReplaceBadChar;
end;

function Var2Type(V : Variant; const VarType : integer) : variant;
begin
  if TVarData(V).VType in [varEmpty, varNull] then
  begin
    case VarType of
      varString,
      varOleStr    : Result := '';
      varInteger,
      varSmallint,
      varByte      : Result := 0;
      varBoolean   : Result := false;
      varSingle,
      varDouble,
      varCurrency,
      varDate      : Result := 0.0;
      varVariant   : Result := Null;
      else Result := VarAsType(V, VarType);
    end;
  end else
    Result := VarAsType(V, VarType);
  if (VarType = varInteger) and (TVarData(V).VType = varBoolean) then
    Result := Integer(V = True);
end;

function VarToInt(V : Variant) : Integer;
begin
   Result := Var2Type(V, varInteger);
end;

function VarToFloat(V : Variant) : Double;
begin
   Result := Var2Type(V, varDouble);
end;

function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
var
  SearchRec : TSearchRec;
  DosError  : integer;
  Path, DestPath : TFileName;
begin
  Result := false;
  if not CreateDir(DestDir) then Exit;
  Path := SourceDir;
  DestPath := AddSlash2(DestDir);
  AddSlash(Path);
  DosError := FindFirst(Path+'*.*', faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if ((SearchRec.Attr and faDirectory) = faDirectory) then
        Result := CopyDir(Path + SearchRec.Name, AddSlash2(DestDir) + SearchRec.Name)
      else
        Result := CopyFile(PChar(Path+SearchRec.Name),
          PChar(DestPath+SearchRec.Name), true);
      if not Result then exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Result := true;
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

function DefStr(const S: string; Default: string): string;
begin
  if S <> '' then
    Result := S
  else
    Result := Default;
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
  Result := (S = '1') or Cmp(S, 'true') or Cmp(S, 'yes');
end;

procedure LoadIcoToImage(ALarge, ASmall : TImageList; const NameRes : string);
var
  Ico  : TIcon;
begin
  Ico := TIcon.Create;
  if ALarge <> nil then begin
    Ico.Handle := LoadImage(hInstance, PChar(NameRes), IMAGE_ICON, 32, 32, 0);
    ALarge.AddIcon(Ico);
  end;
  if ASmall <> nil then begin
    Ico.Handle := LoadImage(hInstance, PChar(NameRes), IMAGE_ICON, 16, 16, 0);
    ASmall.AddIcon(Ico);
  end;
  Ico.Free;
end;

procedure WordBreak(Canvas : TCanvas; const S : string; Ss : TStrings);
begin
  Ss.Text := S;
end;

procedure RATextOut(Canvas : TCanvas; const R, RClip : TRect; const S : string);
begin
  RATextOutEx(Canvas, R, RClip, S, false);
end;

function RATextCalcHeight(Canvas : TCanvas; const R: TRect; const S : string) : integer;
begin
  Result := RATextOutEx(Canvas, R, R, S, true);
end;

function RATextOutEx(Canvas : TCanvas; const R, RClip : TRect; const S : string; const CalcHeight : boolean) : integer;
var
  Ss : TStrings;
  i  : integer;
  H  : integer;
begin
  Ss := TStringList.Create;
  try
    WordBreak(Canvas, S, Ss);
    H := Canvas.TextHeight('A');
    Result := H * Ss.Count;
    if not CalcHeight then
      for i := 0 to Ss.Count-1 do
        ExtTextOut(
          Canvas.Handle, // handle of device context
          R.Left,        // x-coordinate of reference point
          R.Top + H*i,   // y-coordinate of reference point
          ETO_CLIPPED,   // text-output options
          @RClip,        // optional clipping and/or opaquing rectangle
          PChar(Ss[i]),
          Length(Ss[i]), // number of characters in string
          nil            // address of array of intercharacter spacing values
        );
  finally
    Ss.Free;
  end;
end;

procedure Cinema(Canvas : TCanvas; rS, rD : TRect);
const
  Pause = 30; {milliseconds}
  Steps = 7;
  Width = 1;
var
  R : TRect;

  procedure FrameR(R : TRect);
  begin
    with Canvas do begin
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
    with Canvas do begin
      MoveTo(rS.Left, rS.Top);
      LineTo(R.Left, R.Top);
      if (R.Top <> rS.Top) then begin
        MoveTo(rS.Right, rS.Top);
        LineTo(R.Right, R.Top);
      end;
      if (R.Left <> rS.Left) then begin
        MoveTo(rS.Left, rS.Bottom);
        LineTo(R.Left, R.Bottom);
      end;
      if (R.Bottom <> rS.Bottom) and (R.Right <> rS.Right) then begin
        MoveTo(rS.Right, rS.Bottom);
        LineTo(R.Right, R.Bottom);
      end;
    end;
  end;
var
  i : integer;
  PenOld : TPen;
begin
  PenOld := TPen.Create;
  PenOld.Assign(Canvas.Pen);
  Canvas.Pen.Mode := pmNot;
  Canvas.Pen.Width := Width;
  Canvas.Pen.Style := psDot;
  FrameR(rS);
  R := rS;
  for i := 1 to Steps do begin
    R.Left   := rS.Left   + (rD.Left   - rS.Left  ) div Steps * i;
    R.Top    := rS.Top    + (rD.Top    - rS.Top   ) div Steps * i;
    R.Bottom := rS.Bottom + (rD.Bottom - rS.Bottom) div Steps * i;
    R.Right  := rS.Right  + (rD.Right  - rS.Right ) div Steps * i;
    Frame;
    Sleep(Pause);
    Frame;
  end;
  FrameR(rS);
  Canvas.Pen.Assign(PenOld);
end;

function FindFormByClass(FormClass : TFormClass) : TForm;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = FormClass.ClassName then
    begin
      Result := Application.Components[i] as TForm;
      Break;
    end;
end;

function FindFormByClassName(FormClassName : string) : TForm;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = FormClassName then
    begin
      Result := Application.Components[i] as TForm;
      Break;
    end;
end;

function FindByTag(WinControl : TWinControl; ComponentClass : TComponentClass; const Tag : integer) : TComponent;
var
  i : integer;
begin
  for i := 0 to WinControl.ControlCount-1 do begin
    Result := WinControl.Controls[i];
    if (Result is ComponentClass) and (Result.Tag = Tag) then exit;
  end;
  Result := nil;
end;

function ControlAtPos2(Parent : TWinControl; X, Y : integer) : TControl;
var
  i : integer;
  P : TPoint;
begin
  P := Point(X, Y);
  for I := Parent.ControlCount - 1 downto 0 do
  begin
    Result := Parent.Controls[I];
    with Result do
      if PtInRect(BoundsRect, P) then Exit;
  end;
  Result := nil;
end;

function RBTag(Parent : TWinControl) : integer;
var
  RB : TRadioButton;
  i : integer;
begin
  RB := nil;
  with Parent do
    for i := 0 to ControlCount-1 do
      if (Controls[i] is TRadioButton) and
         (Controls[i] as TRadioButton).Checked
      then begin
        RB := Controls[i] as TRadioButton;
        break;
      end;
  if RB <> nil then
    Result := RB.Tag else
    Result := 0;
end;

function IniReadSection(const IniFileName : TFileName; const Section : string; Ss : TStrings) : boolean;
var
  F : integer;
  S : string;
begin
  with TStringList.Create do
  try
    LoadFromFile(IniFileName);
    F := IndexOf('['+Section+']');
    Result := F > -1;
    if Result then begin
      Ss.Clear;
      inc(F);
      while F < Count do begin
        S := Strings[F];
        if (Length(S) > 0) and (Trim(S[1])= '[') then break;
        Ss.Add(S);
        inc(F);
      end;
    end;
  finally
    Free;
  end;
end;

procedure SaveTextFile(const FileName : TFileName; const Source : string);
begin
  with TStringList.Create do
  try
  Text := Source;
  SaveToFile(FileName);
  finally
    Free;
  end;
end;

function LoadTextFile(const FileName : TFileName): string;
begin
  with TStringList.Create do
  try
  LoadFromFile(FileName);
  Result := Text;
  finally
    Free;
  end;
end;

function ReadFolder(const Folder, Mask : TFileName; FileList : TStrings) : integer;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  FileList.Clear;
  Result := FindFirst(AddSlash2(Folder)+Mask, faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do begin
    if not ((SearchRec.Attr and faDirectory) = faDirectory)  then
      FileList.Add(SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function ReadFolders(const Folder : TFileName; FolderList : TStrings) : integer;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  FolderList.Clear;
  Result := FindFirst(AddSlash2(Folder)+'*.*', faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do begin
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

function ResolveLink(const hwnd: HWND; const LinkFile: TFileName;
  var FileName: TFileName): HRESULT;
var
  psl: IShellLink;
  WLinkFile: array [0..MAX_PATH] of WideChar;
  wfd: TWIN32FINDDATA;
  ppf: IPersistFile;
begin
  pointer(psl) := nil;
  pointer(ppf) := nil;
  Result := CoInitialize(nil);
  if SUCCEEDED(Result) then
  begin
    // Get a pointer to the IShellLink interface.
    Result := CoCreateInstance(CLSID_ShellLink, nil,
            CLSCTX_INPROC_SERVER, IShellLink, psl);
    if SUCCEEDED(Result) then
    begin

        // Get a pointer to the IPersistFile interface.
        Result := psl.QueryInterface(IPersistFile, ppf);
        if SUCCEEDED(Result) then
        begin
            StringToWideChar(LinkFile, WLinkFile, SizeOf(WLinkFile)-1);
            // Load the shortcut.
            Result := ppf.Load(WLinkFile, STGM_READ);
            if SUCCEEDED(Result) then
            begin
                // Resolve the link.
                Result := psl.Resolve(hwnd, SLR_ANY_MATCH);
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
        // Release the pointer to the IPersistFile interface.
        ppf._Release;
        end;
    // Release the pointer to the IShellLink interface.

    psl._Release;
    end;
    CoUnInitialize;
  end;
  pointer(psl) := nil;
  pointer(ppf) := nil;
end;
{$ENDIF COMPILER3_UP}


{ ‘ункци€ '—ловарь —окращений', пример вызова:
    with memEdit do begin
      Text := ReplaceSokr(Text, SelStart+1, SelLength, memWords.Lines, memFrases.Lines, NewSelStart);
      SelStart := NewSelStart-1;
    end; }
function ReplaceSokr(S : string; PosBeg, Len : integer; Words, Frases : TStrings; var NewSelStart : integer) : string;
var
  i, Beg, Ent, LS, F : integer;
  Word : string;
begin
  NewSelStart := PosBeg; Result := S;
  LS := Length(S);
  if Len = 0 then begin
    if PosBeg < 1 then exit;
    if PosBeg = 1 then PosBeg := 2;
    for i := PosBeg-1 downto 1 do
      if S[i] in Separators then break;
    Beg := i+1;
    for Ent := PosBeg to LS do
      if S[Ent] in Separators then break;
    if Ent > Beg then
      Word := Copy(S, Beg, Ent-Beg) else
      Word := S[PosBeg];
  end else begin
    Word := Copy(S, PosBeg, Len);
    Beg := PosBeg;
    Ent := PosBeg + Len;
  end;
  if Word = '' then exit;
  F := Words.IndexOf(Word);
  if (F > -1) and (F < Frases.Count) then begin
    Result := Copy(S, 1, Beg-1)+Frases[F]+Copy(S, Ent, LS);
    NewSelStart := Beg + Length(Frases[F]);
  end;
end;

{ ‘ункци€ '—ловарь —окращений дл€ всего текста', пример вызова:
    with memEdit do
      Text := ReplaceAllSokr(Text, memWords.Lines, memFrases.Lines);
}
function ReplaceAllSokr(S : string; Words, Frases : TStrings) : string;
var
  i, LW : integer;
  P : PChar;
  Sm : integer;
begin
  for i := 0 to Words.Count-1 do begin
    LW := Length(Words[i]);
    P := StrPos(PChar(S), PChar(Words[i]));
    while P <> nil do begin
      Sm := P-PChar(S);
      S := Copy(S, 1, Sm)+Frases[i]+Copy(S, Sm+LW+1, Length(S));
      P := StrPos(PChar(S)+Sm+Length(Frases[i]), PChar(Words[i]));
    end;
  end;
  Result := S;
end;

function CountOfLines(const S : string) : integer;
begin
  with TStringList.Create do
  try
    Text := S;
    Result := Count;
  finally
    Free;
  end;
end;

procedure DeleteEmptyLines(Ss : TStrings);
var
  i : integer;
begin
  i := 0;
  while i < Ss.Count do
    if Trim(Ss[i]) = '' then
      Ss.Delete(i) else
      inc(i);
end;

procedure SQLAddWhere(SQL : TStrings; const where : string);
var
  i, j : integer;
begin
  j := SQL.Count-1;
  for i := 0 to SQL.Count-1 do
    if StrLIComp(PChar(SQL[i]), 'where ', 6) = 0 then
    begin
      j := i+1;
      while j < SQL.Count do
      begin
        if (StrLIComp(PChar(SQL[j]), 'order ', 6) = 0) or
           (StrLIComp(PChar(SQL[j]), 'group ', 6) = 0) then
          break;
        inc(j);
      end;
    end;
  SQL.Insert(j, 'and '+Where);
end;

{-----------------------------------------------------------------------}
var
  ProcList : TList = nil;
type
  TJvProcItem  = class
    ProcObj : TProcObj;
    constructor Create(AProcObj : TProcObj);
  end;

constructor TJvProcItem .Create(AProcObj : TProcObj);
begin
  ProcObj := AProcObj;
end;

procedure TmrProc(hwnd : HWND; uMsg : integer; idEvent : integer; dwTime : integer); stdcall;
var
  Pr : TProcObj;
begin
  if ProcList[idEvent] <> nil then begin
    Pr := TJvProcItem (ProcList[idEvent]).ProcObj;
    TJvProcItem (ProcList[idEvent]).Free;
  end else Pr := nil;
  ProcList.Delete(idEvent);
  KillTimer(hwnd, idEvent);
  if ProcList.Count <= 0 then begin
    ProcList.Free;
    ProcList := nil;
  end;
  if Assigned(Pr) then Pr;
end;

procedure ExecAfterPause(Proc : TProcObj; Pause : integer);
var
  Num : integer;
  i : integer;
begin
  if ProcList = nil then ProcList := TList.Create;
  Num := -1;
  for i := 0 to ProcList.Count-1 do
    if @TJvProcItem (ProcList[i]).ProcObj = @Proc then begin
      Num := i;
      break;
    end;
  if Num <> -1 then KillTimer(Application.Handle, Num)
  else Num := ProcList.Add(TJvProcItem .Create(Proc));
  SetTimer(Application.Handle, Num, Pause, @TmrProc);
end;
{=======================================================================}

procedure Roughed(ACanvas : TCanvas; const ARect : TRect; const AVert : boolean);
var
  i : integer;
  j : integer;
  R : TRect;
  V : boolean;
  H : boolean;
begin
  H := true;
  V := true;
  for i := 0 to (ARect.Right - ARect.Left) div 4 do begin
    for j := 0 to (ARect.Bottom - ARect.Top) div 4 do begin
      if AVert then begin
        if V then
          R := Bounds(ARect.Left + i * 4 + 2, ARect.Top + j * 4, 2, 2)
        else
          R := Bounds(ARect.Left + i * 4, ARect.Top + j * 4, 2, 2);
      end else begin
        if H then
          R := Bounds(ARect.Left + i * 4, ARect.Top + j * 4 + 2, 2, 2)
        else
          R := Bounds(ARect.Left + i * 4, ARect.Top + j * 4, 2, 2);
      end;
      Frame3D(ACanvas, R, clBtnHighlight, clBtnShadow, 1);
      V := not V;
    end;
    H := not H;
  end;
end;

function BitmapFromBitmap(SrcBitmap : TBitmap; const AWidth, AHeight, index : integer) : TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Canvas.CopyRect(Rect(0, 0 , AWidth, AHeight), SrcBitmap.Canvas, Bounds(AWidth * index, 0, AWidth, AHeight));
end;

procedure ShowMenu(Form : TForm; MenuAni : TMenuAnimation);
var
  i : integer;
  h : integer;
  w : integer;
begin
  case MenuAni of
    maNone : Form.Show;
    maRandom : ;
    maUnfold : begin
      h := Form.Height;
      Form.Height := 0;
      Form.Show;
      for i := 0 to h div 10 do
        if Form.Height < h then Form.Height := Form.Height + 10;
    end;
    maSlide : begin
      h := Form.Height;
      w := Form.Width;
      Form.Height := 0;
      Form.Width  := 0;
      Form.Show;
      for i := 0 to max(h div 5, w div 5) do begin
        if Form.Height < h then Form.Height := Form.Height + 5;
        if Form.Width  < w then Form.Width  := Form.Width  + 5;
      end;
//      CS_SAVEBITS
    end;
  end;
end;

function ResSaveToFileEx(Instance: HINST; Typ, Name : PChar;
  const Compressed : boolean; const FileName : string) : boolean;
var
  Rhrsrc   : HRSRC;
  Rhglobal : HGLOBAL;
  RAddr    : Pointer;
  RLen     : DWORD;
  Stream   : TFileStream;
  FileDest : string;
begin
  Result := false;
  Rhrsrc := FindResource(
    Instance,// resource-module handle
    Name, // address of resource name
    Typ // address of resource type
    );
  if Rhrsrc = 0 then exit;
  Rhglobal := LoadResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
  );
  if Rhglobal = 0 then exit;
  RAddr := LockResource(
    Rhglobal // handle to resource to lock
   );
  FreeResource(Rhglobal);
  if RAddr = nil then exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
   );
  if RLen = 0 then exit;
  {ј вот теперь можно копировать}
  Stream := nil;{дл€ Free}
  if Compressed then
    FileDest := GenTempFileName(FileName) else
    FileDest := FileName;
  try
    try
      Stream := TFileStream.Create(FileDest, fmCreate or fmOpenWrite or fmShareExclusive);
      Stream.WriteBuffer(RAddr^, RLen);
    finally
     Stream.Free;
    end;
    if Compressed then begin
      Result := LZFileExpand(FileDest, FileName);
      DeleteFile(FileDest);
    end else Result := true;
  except end;
end;

function ResSaveToFile(const Typ, Name : string; const Compressed : boolean;
  const FileName : string) : boolean;
begin
  Result := ResSaveToFileEx(hInstance, PChar(Typ), PChar(Name), Compressed, FileName);
end;

function ResSaveToString(Instance: HINST; const Typ, Name : string;
  var S: string) : boolean;
var
  Rhrsrc   : HRSRC;
  Rhglobal : HGLOBAL;
  RAddr    : Pointer;
  RLen     : DWORD;
begin
  Result := false;
  Rhrsrc := FindResource(
    Instance,// resource-module handle
    PChar(Name), // address of resource name
    PChar(Typ) // address of resource type
    );
  if Rhrsrc = 0 then exit;
  Rhglobal := LoadResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
  );
  if Rhglobal = 0 then exit;
  RAddr := LockResource(
    Rhglobal // handle to resource to lock
   );
  FreeResource(Rhglobal);
  if RAddr = nil then exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    Rhrsrc  // resource handle
   );
  if RLen = 0 then exit;
  {ј вот теперь можно копировать}
  SetString(S, PChar(RAddr), RLen);
end;

function Execute(const CommandLine, WorkingDirectory : string) : integer;
var
  R : boolean;
  ProcessInformation : TProcessInformation;
  StartupInfo : TStartupInfo;
{$IFDEF COMPILER4_UP}
  ExCode : cardinal;
{$ELSE}
  ExCode : integer;
{$ENDIF}
begin
  Result := 0;
  FillChar(StartupInfo, sizeof(TStartupInfo), 0);
  with StartupInfo do begin
    cb := sizeof(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_SHOW;
  end;
  R := CreateProcess(
    nil, // pointer to name of executable module
    PChar(CommandLine), // pointer to command line string
    nil,// pointer to process security attributes
    nil,// pointer to thread security attributes
    false, // handle inheritance flag
    0, // creation flags
    nil,// pointer to new environment block
    PChar(WorkingDirectory), // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    ProcessInformation // pointer to PROCESS_INFORMATION
   );
  if R then
    while (GetExitCodeProcess(ProcessInformation.hProcess, ExCode) and
          (ExCode = STILL_ACTIVE))
    do Application.ProcessMessages
  else
    Result := GetLastError;
end;

function TextWidth(AStr : string) : integer;
var
  Canvas : TCanvas;
  DC : HDC;
begin
  DC := GetDC(0);
  Canvas := TCanvas.Create;
  Canvas.Handle := DC;
  Result := Canvas.TextWidth(AStr);
  Canvas.Handle := 0;
  Canvas.Free;
  ReleaseDC(0, DC);
end;

function AppMinimized : boolean;
begin
  Result := IsIconic(Application.Handle);
end;

function MessageBox(const Message : string; Caption : string; const Flags : integer) : integer;
begin
  if Caption = '' then Caption := Application.Title;
  Result := Application.MessageBox(PChar(Message), PChar(Caption), Flags);
end;

function MsgDlgDef1(const Msg, ACaption : string; DlgType : TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton : TMsgDlgBtn; UseDefButton : boolean;
  AHelpContext : integer; Control : TWinControl): Integer;
const
 {$IFNDEF COMPILER2}
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');
 {$ELSE}
  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'Help');
 {$ENDIF COMPILER2}
var
  P : TPoint;
  i : integer;
  Btn : TButton;
  StayOnTop : boolean;
begin
  if AHelpContext <> 0 then Buttons := Buttons + [mbHelp];
  StayOnTop := false;
  with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      {$IFDEF COMPILER3_UP}
      Font.CharSet := MsgDlgCharSet;
      {$ENDIF COMPILER3_UP}
      if (Screen.ActiveForm <> nil) and
         (Screen.ActiveForm.FormStyle = fsStayOnTop) then
      begin
        StayOnTop := true;
        SetWindowTop(Screen.ActiveForm.Handle, false);
      end;
      if ACaption <> '' then Caption := ACaption;
      if Control = nil then
      begin
        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;
      end else
      begin
        P := Point((Control.Width - Width) div 2,
            (Control.Height - Height) div 2);
        P := Control.ClientToScreen(P);
        Left := P.X;
        Top := P.Y
      end;
      if Left < 0 then Left := 0
      else if Left > Screen.Width then Left := Screen.Width - Width;
      if Top < 0 then Top := 0
      else if Top > Screen.Height then Top := Screen.Height - Height;
      HelpContext := AHelpContext;

      Btn := FindComponent(ButtonNames[DefButton]) as TButton;
      if UseDefButton and (Btn <> nil) then
      begin
        for i := 0 to ComponentCount - 1 do
          if Components[i] is TButton then
            (Components[i] as TButton).Default := false;
        Btn.Default := true;
        ActiveControl := Btn;
      end;
      Btn := FindComponent(ButtonNames[mbIgnore]) as TButton;
      if Btn <> nil then
      begin
       // Btn.Width := Btn.Width * 5 div 4;
        {сдвинуть кнопку Help}
      end;
      Result := ShowModal;
    finally
      Free;
      if (Screen.ActiveForm <> nil) and StayOnTop then
        SetWindowTop(Screen.ActiveForm.Handle, true);
    end;
end;

function MsgDlgDef(const Msg, ACaption : string; DlgType : TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButton : TMsgDlgBtn; HelpContext : integer;
  Control : TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, DefButton, true, HelpContext, Control);
end;

function MsgDlg2(const Msg, ACaption : string; DlgType : TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext : integer;
  Control : TWinControl): Integer;
begin
  Result := MsgDlgDef1(Msg, ACaption, DlgType, Buttons, mbHelp, false, HelpContext, Control);
end;

procedure CenterHor(Parent : TControl; MinLeft : integer; Controls : array of TControl);
var
  i : integer;
begin
  for i := Low(Controls) to High(Controls) do
    Controls[i].Left := Max(MinLeft, (Parent.Width - Controls[i].Width) div 2)
end;

procedure EnableControls(Control : TWinControl; const Enable : boolean);
var
  i : integer;
begin
  for i := 0 to Control.ControlCount - 1 do
    Control.Controls[i].Enabled := Enable;
end;

procedure EnableMenuItems(MenuItem : TMenuItem; const Tag : integer; const Enable : boolean);
var
  i : integer;
begin
  for i := 0 to MenuItem.Count - 1 do
    if MenuItem[i].Tag <> Tag then
      MenuItem[i].Enabled := Enable;
end;

procedure ExpandWidth(Parent : TControl; MinWidth : integer; Controls : array of TControl);
var
  i : integer;
begin
  for i := Low(Controls) to High(Controls) do
   Controls[i].Width := Max(MinWidth, Parent.ClientWidth - 2 * Controls[i].Left);
end;

function PanelBorder(Panel : TCustomPanel) : integer;
begin
  Result := TPanel(Panel).BorderWidth;
  if TPanel(Panel).BevelOuter <> bvNone then Inc(Result, TPanel(Panel).BevelWidth);
  if TPanel(Panel).BevelInner <> bvNone then Inc(Result, TPanel(Panel).BevelWidth);
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
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF COMPILER2}

{ function DefineCursor was typed from
  book "Secrets of Delphi 2" by Ray Lischner }

function DefineCursor(Identifer : PChar) : TCursor;
var
  Handle : HCursor;
begin
  Handle := LoadCursor(hInstance, Identifer);
  if Handle = 0 then
    raise EOutOfResources.Create('Cannot load cursor resource');
  for Result := 1 to High(TCursor) do
    if Screen.Cursors[Result] = Screen.Cursors[crDefault] then
    begin
      Screen.Cursors[Result] := Handle;
      exit;
    end;
  raise EOutOfResources.Create('Too many user-defined cursors');
end;

procedure Delay(MSec : longword);
var
  T : longword;
begin
  T := GetTickCount;
  while GetTickCount - T < MSec do
    Application.ProcessMessages;
end;

function Pixels(Control : TControl; APixels : integer) : integer;
var
  Form : TForm;
begin
  Result := APixels;
  if Control is TForm then
    Form := TForm(Control) else
    Form := TForm(GetParentForm(Control));
  if Form.Scaled then
    Result := Result * Form.PixelsPerInch div 96;
end;

procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint);
var
  i : integer;
  PropInfo : PPropInfo;
begin
  for i := 0 to Owner.ComponentCount - 1 do
  begin
    PropInfo := GetPropInfo(Owner.Components[i].ClassInfo, PropName);
    if PropInfo <> nil then
      SetOrdProp(Owner.Components[i], PropInfo, Value);
  end;
end;

procedure Error(const Message : string);
begin
  raise Exception.Create(Message);
end;

procedure ItemHtDrawEx(Canvas : TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text : string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL : string;
  i : integer;
  M1 : string;
  OriRect: TRect; // it's added

  function Cmp(M1 : string) : boolean;
  begin
    Result := ANSIStrLIComp(PChar(Text)+ i, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1 : string) : boolean;
  begin
    Result := ANSIStrLIComp(PChar(Text)+ i, PChar(M1), Length(M1)) = 0;
    if Result then inc(i, Length(M1));
  end;

  function CmpL(M1 : string) : boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1 : string) : boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M : string);
  begin
    if not Assigned(Canvas) then Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left :=  Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: boolean);
  begin
    if not Assigned(Canvas) then Exit;
    if Include then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;    { if }

var
  oldFontStyles: TFontStyles;
  oldFontColor: TColor;
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

  OriRect := Rect;  //save origin rectangle


  M1 := '';
  i := 1;
  while i <= Length(Text) do
  begin
    if (Text[i] = '<') and
      (CmpL('b') or CmpL('/b') or
       CmpL('i') or CmpL('/i') or
       CmpL('u') or CmpL('/u') or
       Cmp('c:') )then
    begin
      Draw(M1);
      PlainItem := PlainItem + M1;

      if CmpL1('b') then
        Style(fsBold, True)
      else if CmpL1('/b') then
        Style(fsBold, False)
      else if CmpL1('i') then
        Style(fsItalic, True)
      else if CmpL1('/i') then
        Style(fsItalic, False)
      else if CmpL1('u') then
        Style(fsUnderline, True)
      else if CmpL1('/u') then
        Style(fsUnderline, False)
      else if Cmp1('c:') then
      begin
        CL := SubStr(PChar(Text)+ i, 0, '>');
        if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
        inc(i, Length(CL) + 1 {'>'});
      end;

      M1 := '';
    end else
        // next lines were added
        if (Text[i] = chr(13)) AND (Cmp1(string(chr(10)))) then begin
                        // new line
                      Draw(M1);
                      PlainItem := PlainItem + M1;
                                Rect.Left := OriRect.Left;
            Rect.Top := Rect.Top + Canvas.TextHeight(M1);
                      M1 := '';
        end else
                        // add text
            M1 := M1 + Text[i];
    inc(i);
  end;    { for }
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

function ItemHtDraw(Canvas : TCanvas; Rect: TRect;
 const State: TOwnerDrawState; const Text : string;
 const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function ItemHtPlain(const Text : string): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(nil, Rect(0, 0, -1, -1), [], Text, False, S, W, False);
  Result := S;
end;

function ItemHtWidth(Canvas : TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text : string;
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
 i: Integer;
begin
  if not Assigned(List) then Exit;
 for i := 0 to List.Count - 1 do    { Iterate }
  TObject(List[i]).Free;
 List.Clear;
end;    { ClearList }


procedure MemStreamToClipBoard(MemStream : TMemoryStream; const Format : word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  ClipBoard.Open;
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
    ClipBoard.Close;
  end;
end;

procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format : word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  ClipBoard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then Exit;
    DataPtr := GlobalLock(Data);
    if DataPtr = nil then Exit;
    try
      MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
      MemStream.Position := 0;
    finally
      GlobalUnlock(Data);
    end;
  finally
    ClipBoard.Close;
  end;
end;


{*******************  *********************}
function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    Result := tkUnknown
  else
    Result := PropInf^.PropType^.Kind;
end;

function GetPropStr(Obj: TObject; const PropName: string): string;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in [tkString, tkLString {$IFDEF COMPILER3_UP}, tkWString {$ENDIF COMPILER3_UP}]) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result:= GetStrProp(Obj, PropInf);
end;

function GetPropOrd(Obj: TObject; const PropName: string): Integer;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in [tkInteger, tkChar, tkWChar,
     tkEnumeration, tkClass]) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result:= GetOrdProp(Obj, PropInf);
end;

function GetPropMethod(Obj: TObject; const PropName: string): TMethod;
var
  PropInf : PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateFmt(SPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind = tkMethod) then
    raise Exception.CreateFmt(SInvalidPropertyType, [PropName]);
  Result:= GetMethodProp(Obj, PropInf);
end;

procedure PrepareIniSection(SS: TStrings);
var
  i: Integer;
  S: string;
begin
  i := 0;
  while i < Ss.Count do
  begin
    S := Trim(Ss[i]);
    if (Length(S) = 0) or (S[1] in [';', '#']) then
      Ss.Delete(i)
    else
      inc(i);
  end;
end;

end.
