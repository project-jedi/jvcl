{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBasethb.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer@Excite.com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):


Last Modified: 2002-07-12

You may Thumb the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvBaseThumbnail;

interface
uses Classes, Controls, ExtCtrls, Windows, SysUtils, Messages, Forms;

type

  { The TfileName Object has been created to handle the first field of a Thumb
    Wich is the Thumbs actual FileName complite with the path because no
    duplicates are alowed in the final list.
    It Has the following properties
      01) Filename : it Keeps the filename as gived by the user
      02) LongName : It always returns the longName of the file
      03) ShortName : It Always returns the short name of the file
      04) Size      : It returns the size in byte that it will occupy if saved in a stream
      05) Length    : The "Filename" property Length;
    and the following Methods
      01) LoadFromStream(Astream:TStream;Apos:Integer); It loads a filename from a stream
          if Apos < 0 then don't change the cursors position in the stream
          else AStream.seek(Apos,0);
      02) SaveToStream(Astream:TStream;Apos:Integer); Save the Filename to AStream
          If APos>-1 then AStream.seek(Apos,0);
          SaveData;
  }
  TProgressNotify = procedure(Sender: TObject; Position: integer; var Break: boolean) of object;
  TGRF_type = (GR_BMP, GR_JPG, GR_WMF, GR_EMF, GR_ICO, {$IFNDEF COMPILER6_UP}GR_GIF, {$ENDIF}GR_PNG); //,GR_PCX,GR_TGA);
  TPercent = -100..100;

  TFileName = class(TObject)
  private
    VLongName: string;
    VShortName: string;
    VFileName: string;
    VCreated,
      VAccessed,
      VModified: TDatetime;
    VFileSize: LongInt;
  protected
    procedure SetName(NewName: string); virtual;
    function GetLength: integer;
    procedure SetLength(NewLength: integer);
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Astream: TStream; Apos: Integer); //Load From stream;
    // both of this routines are inserting extract data to the stream its self
    // like a header and data end string;
    procedure SaveToStream(AStream: TStream; Apos: Integer); // Save to a Stream;
  published
    property LongName: string read VLongName; // The LongName of this filenam;
    property ShortName: string read VShortName; // shortname of this filename
    property Filename: string read VFileName write SetName; // The Filename AS given by the user;
    property Length: Integer read GetLength write SetLength;
  end;

  { The Following classes are declared here so I can handle interaction of the mouse
    between the three components.
  }
  PJvThumbtitle = ^TJvThumbTitle;
  TJvThumbTitle = class(TPanel)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(button: Tmousebutton; shift: Tshiftstate;
      x, y: integer); override;
    procedure MouseUp(button: Tmousebutton; shift: tshiftstate;
      x, y: integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end; {}

  TJvBaseThumbImage = class(TImage)
  private
    VIgnoreMouse: Boolean;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
  public
    constructor Create(Aowner: TComponent); override;
  published
    property IgnoreMouse: Boolean read VIgnoreMouse write VIgnoreMouse;
  end;

  TJvBaseThumbnail = class(TPanel)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(Aowner: Tcomponent); override;
  published
  end;

  TJvBaseThumbView = class(TScrollBox)
  private
    //    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
  public
    constructor Create(Aowner: TComponent); override;
  published
  end;


function BoundByte(min, max, value: integer): byte;
procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
function ProportionalSize(phisicalsize, newsize: tpoint): Tpoint;
function CompletePath(const Path: string): string;
function IncompletePath(APath: string): string;
function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: longint; CaseSensitive: boolean): string;
function Jkceil(i: extended): longint;
function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
  CaseSensitive: boolean): string;

implementation
const
{$IFDEF COMPILER5_UP}
  PathEnd = '\';
{$ENDIF}
{$IFDEF LINUX}
  PathEnd = '/';
{$ENDIF}

function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
  CaseSensitive: boolean): string;
var
  Cnt: Integer;
  S1, S2, SF: string;

begin
  S1 := Str;
  if CaseSensitive then
  begin
    S2 := S1;
    SF := SearchFor;
  end
  else
  begin
    S2 := UpperCase(S1);
    SF := UpperCase(SearchFor);
  end;
  Result := '';
  repeat
    Cnt := Pos(SF, S2);
    if Cnt > 0 then
    begin
      Result := Result + Copy(S1, 1, Cnt - 1) + ReplaceWith;
      S1 := Copy(S1, Cnt + Length(SF), Length(S1));
      if CaseSensitive then S2 := S1 else S2 := UpperCase(S1);
    end
    else
      Result := Result + S1;
  until Cnt <= 0;
end;

function Jkceil(i: extended): longint;
var
  t: longint;
begin
  t := trunc(i);
  if t <> i then
    if i > 0 then t := t + 1
    else t := t - 1;
  result := t;
end;

function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: Longint; CaseSensitive: boolean): string;
var
  Count: Longint;
  RepCount: longint;
  res: string;
begin
  res := Astr;
  if replaceNo > 0 then repcount := 0 else repcount := -1;
  Count := 1;
  if length(Res) > 0 then repeat
      if res[count] = chartofind then
      begin
        res[count] := newchar;
        if repcount >= 0 then inc(repcount, 1);
      end;
      inc(Count, 1);
    until (Count > length(Res)) or (RepCount >= ReplaceNo);
  result := res;
end;

// add the slash character at the end of the path.

function CompletePath(const Path: string): string;
var
  res: string;
begin
  if Length(Path) > 0 then
  begin
    if path[length(Path)] <> PathEnd then res := path + PathEnd
    else res := path;
  end
  else res := path;
  result := res;
end;
// Remove the slash character from the end of the path.

function IncompletePath(APath: string): string;
begin
  if Length(APath) > 0 then
  begin
    if Apath[length(Apath)] = PathEnd then
      result := Copy(Apath, 1, Length(Apath) - 1)
    else
      Result := APath;
  end
  else Result := APath;
end;

function ProportionalSize(phisicalsize, newsize: tpoint): Tpoint;
var
  Percent: single;
  tempx, tempy: Single;
  fs: tpoint;
begin
  // Υπολογισμός ποσοστού  επί της εκατό που θα επιδοθεί στην τιμή προς
  // αλλαγή.
  if phisicalsize.x <> 0 then
    tempx := ((newsize.x) / phisicalsize.x) * 100
  else tempx := 0;
  if Phisicalsize.y <> 0 then
    tempy := ((newsize.y) / phisicalsize.y) * 100
  else Tempy := 0;
  //Ευρεση μικρότερου ποσοστού αλαγής και χρήση αυτού.
  if tempx <= tempy then
    percent := tempx
  else
    percent := tempy;
  //fs.x:=round((phisicalsize.x/100)*percent);
  //fs.y:=round((phisicalsize.y/100)*percent);
  fs.x := trunc((phisicalsize.x / 100) * percent);
  fs.y := trunc((phisicalsize.y / 100) * percent);
  result := fs;
end;

procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
begin
  SetLength(Str, Length(str) + Length(NewStr));
  CopyMemory(@Str[pos + Length(NewStr) + 1], @Str[Pos + 1], Length(Str) - Pos - Length(NewStr));
  CopyMemory(@str[pos + 1], @NewStr[1], Length(newStr));
end;

function BoundByte(min, max, value: integer): byte;
begin
  if value < min then result := min
  else
    if value > max then result := max
    else result := value;
end;

//******************* Thumbtitle Procedure and functions ******************

procedure TJvThumbTitle.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 1;
end;

procedure TJvThumbTitle.Mousedown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).mousedown(button, shift, x + left, y + top)
  else inherited;
end;

procedure TJvThumbTitle.mouseup(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).mouseup(button, shift, x + left, y + top)
  else inherited;
end;

procedure TJvThumbTitle.click;
begin
  if parent is TJvBaseThumbnail then TJvBaseThumbnail(parent).click
  else inherited;
end;

procedure TJvThumbTitle.Dblclick;
begin
  if parent is TJvBaseThumbnail then TJvBaseThumbnail(parent).Dblclick
  else inherited;
end;

procedure TJvThumbTitle.mousemove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).mousemove(shift, x + left, y + top)
  else inherited;
end;

function TJvThumbTitle.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  //Inherited;
  if parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(parent).DoMouseWheel(shift, WheelDelta, MousePos)
  else Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  //  Result := true;
end;

function TJvThumbTitle.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(parent).DoMouseWheelDown(shift, MousePos)
  else Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvThumbTitle.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(parent).DoMouseWheelUp(shift, MousePos)
  else Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TJvThumbTitle.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).KeyDown(Key, Shift)
  else inherited;
end;

procedure TJvThumbTitle.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).KeyUp(Key, Shift)
  else inherited;
end;

procedure TJvThumbTitle.KeyPress(var Key: Char);
begin
  if parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).KeyPress(Key)
  else inherited;
end;

constructor TJvThumbTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvBaseThumbnail then controlstyle := controlstyle - [csSetCaption, csCaptureMouse, csClickEvents, csDoubleClicks]
  else controlstyle := controlstyle - [csSetCaption];
end;

//******************* TJvBaseThumbImage Procedure and functions ******************

procedure TJvBaseThumbImage.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 1;
end;

procedure TJvBaseThumbImage.Mousedown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).mousedown(button, shift, x + left, y + top)
  else inherited mousedown(button, shift, x, y);
end;

procedure TJvBaseThumbImage.mouseup(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).mouseup(button, shift, x + left, y + top)
  else inherited mouseup(button, shift, x, y);
end;

procedure TJvBaseThumbImage.mousemove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(parent).mousemove(shift, x + left, y + top)
  else inherited mousemove(shift, x, y);
end;

procedure TJvBaseThumbImage.click;
begin
  if parent is TJvBaseThumbnail then TJvBaseThumbnail(parent).click
  else inherited click;
end;

procedure TJvBaseThumbImage.dblclick;
begin
  if parent is TJvBaseThumbnail then TJvBaseThumbnail(parent).Dblclick
  else inherited dblclick;
end;

constructor TJvBaseThumbImage.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := controlstyle - [csSetCaption];
  {  If AOwner is TJvBaseThumbnail then begin
      ControlStyle := ControlStyle-[csCaptureMouse];
      VIgnoreMouse := True
    end Else{} VIgnoreMouse := False;
end;

procedure TJvBaseThumbImage.CMHitTest(var Message: TCMHitTest);
begin
  if VIgnoreMouse then Message.Result := HTNOWHERE //  0;
  else Message.Result := HTCLIENT; // 1;
end;

//******************* TJvBaseThumbnail Procedure and functions *******************

procedure TJvBaseThumbnail.mousedown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).mousedown(button, shift, left + x, top + y)
  else inherited;
end;

procedure TJvBaseThumbnail.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 1;
end;

procedure TJvBaseThumbnail.mousemove(Shift: TShiftState; X, Y: Integer);
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).MouseMove(shift, left + x, top + y)
  else inherited;
end;

function TJvBaseThumbnail.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  //Inherited;
  if parent is TJvBaseThumbview then
    Result := TJvBaseThumbview(parent).DoMouseWheel(shift, WheelDelta, MousePos)
  else Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseThumbview then
    Result := TJvBaseThumbview(parent).DoMouseWheelDown(shift, MousePos)
  else Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if parent is TJvBaseThumbview then
    Result := TJvBaseThumbview(parent).DoMouseWheelUp(shift, MousePos)
  else Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TJvBaseThumbnail.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).KeyDown(Key, Shift)
  else inherited;
end;

procedure TJvBaseThumbnail.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).KeyUp(Key, Shift)
  else inherited;
end;

procedure TJvBaseThumbnail.KeyPress(var Key: Char);
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).KeyPress(Key)
  else inherited;
end;

constructor TJvBaseThumbnail.Create(Aowner: Tcomponent);
begin
  inherited;
  if AOwner is TJvBaseThumbview then
    controlstyle := controlstyle - [csSetCaption, csCaptureMouse] //,
    //                                csClickEvents,csDoubleClicks]
  else {}  controlstyle := controlstyle - [csSetCaption];
end;

procedure TJvBaseThumbnail.mouseup(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).MouseUp(Button, shift, left + x, top + y)
  else inherited;
end;

procedure TJvBaseThumbnail.click;
begin
  if parent is TJvBaseThumbview then TJvBaseThumbview(parent).click
  else inherited click;
end;

procedure TJvBaseThumbnail.dblclick;
begin
  if parent is TJvBaseThumbview then
    TJvBaseThumbview(parent).Dblclick
  else {}  inherited dblclick;
end;

constructor TJvBaseThumbview.Create(Aowner: Tcomponent);
begin
  inherited;
  controlstate := controlstate + [csFocusing];
  controlstyle := controlstyle + [csopaque] - [csSetCaption];
end;
{
procedure TJvBaseThumbview.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 0;
end;{}

//******************* TFileName Procedure and functions *******************

procedure TFileName.SetName(NewName: string);
begin
  VFileName := NewName;
  if (NewName <> LongName) and (NewName <> ShortName) then init;
end;

procedure TFileName.Init;
var
  Temp: TWin32FindData;
  SearchHandle: THandle;
  dft: dword;
  lft: tfiletime;
begin
  SearchHandle := FindFirstFile(PChar(VFileName), Temp);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    VLongName := string(Temp.cFileName);
    VShortName := string(Temp.cAlternateFileName);
    if VLongName = '' then VLongName := VShortName;
    if VShortName = '' then VShortName := VLongName;
    //fdFileAccessed
    filetimetolocalfiletime(Temp.ftLastAccessTime, lft);
    filetimetodosdatetime(lft, longrec(dft).hi, longrec(dft).lo);
    VAccessed := Dft;
    //fdFilechanged
    filetimetolocalfiletime(Temp.ftLastwriteTime, lft);
    filetimetodosdatetime(lft, longrec(dft).hi, longrec(dft).lo);
    VModified := Dft;
    //fdFilecreated
    filetimetolocalfiletime(Temp.ftCreationTime, lft);
    filetimetodosdatetime(lft, longrec(dft).hi, longrec(dft).lo);
    VCreated := DFT;
    VFileSize := (Temp.nfilesizehigh * maxdword) + Temp.nfilesizelow;
    //VFilename:=NewName;
  end
  else VFileName := '';
  Windows.FindClose(SearchHandle);
end;


procedure TFileName.LoadFromStream(Astream: TStream; Apos: Integer);
begin
  // Uner Construction;
end;

procedure TFileName.SaveToStream(AStream: TStream; Apos: Integer);
begin
  //Under Construction
end;

function TFileName.GetLength: Integer;
begin
  Result := System.Length(VFileName);
end;

procedure TFileName.SetLength(NewLength: integer);
begin
  System.SetLength(VfileName, NewLength);
end;

constructor TFileName.Create;
begin
  inherited;
end;

destructor TFileName.Destroy;
begin
  inherited;
end;

end.

