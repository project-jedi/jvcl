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

uses
  Classes, Controls, ExtCtrls, Windows, SysUtils, Messages, Forms;

type
  { The TFileName Object has been created to handle the first field of a Thumb
    which is the Thumbs actual FileName complete with the path because no
    duplicates are allowed in the final list.
    It has the following properties
      01) Filename : It keeps the filename as given by the user
      02) LongName : It always returns the long name of the file
      03) ShortName : It always returns the short name of the file
      04) Size      : It returns the size in bytes that it will occupy if saved in a stream
      05) Length    : The "Filename" property Length;
    and the following Methods
      01) LoadFromStream(AStream: TStream; APos: Integer); It loads a filename from a stream
          if APos < 0 then it dooes not change the position in the stream
          else AStream.Seek(APos, 0);
      02) SaveToStream(AStream: TStream; APos: Integer); Save the Filename to AStream
          If APos > -1 then AStream.Seek(APos, 0);
          SaveData;
  }
  TProgressNotify = procedure(Sender: TObject; Position: Integer; var ABreak: Boolean) of object;
  TGRF_Type = (GR_BMP, GR_JPG, GR_WMF, GR_EMF, GR_ICO,
    {$IFNDEF COMPILER6_UP} GR_GIF, {$ENDIF} GR_PNG); //, GR_PCX, GR_TGA);
  TPercent = -100..100;

  TFileName = class(TObject)
  private
    FLongName: string;
    FShortName: string;
    FFileName: string;
    FCreated: TDateTime;
    FAccessed: TDateTime;
    FModified: TDateTime;
    FFileSize: Longint;
  protected
    procedure SetName(NewName: string); virtual;
    function GetLength: Integer;
    procedure SetLength(NewLength: Integer);
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream; APos: Integer); // Load from stream
    // both of this routines are inserting extract data to the stream its self
    // like a header and data end string;
    procedure SaveToStream(AStream: TStream; APos: Integer); // Save to a Stream
  published
    property LongName: string read FLongName; // the LongName of this filename
    property ShortName: string read FShortName; // shortname of this filename
    property Filename: string read FFileName write SetName; // the Filename as given by the user;
    property Length: Integer read GetLength write SetLength;
  end;

  { The following classes are declared here so I can handle interaction of the mouse
    between the three components.
  }
  PJvThumbTitle = ^TJvThumbTitle;
  TJvThumbTitle = class(TPanel)
  private
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
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
  end;

  TJvBaseThumbImage = class(TImage)
  private
    FIgnoreMouse: Boolean;
    procedure CMHitTest(var Msg: TCMHitTest); message CM_HITTEST;
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property IgnoreMouse: Boolean read FIgnoreMouse write FIgnoreMouse default False;
  end;

  TJvBaseThumbnail = class(TPanel)
  private
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
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
    constructor Create(AOwner: Tcomponent); override;
  published
  end;

  TJvBaseThumbView = class(TScrollBox)
  private
    //    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function BoundByte(Min, Max, Value: Integer): Byte;
procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
function ProportionalSize(PhysicalSize, NewSize: TPoint): TPoint;
function CompletePath(const Path: string): string;
function IncompletePath(Path: string): string;
function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: Longint; CaseSensitive: Boolean): string;
function Jkceil(i: Extended): Longint;
function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
  CaseSensitive: Boolean): string;

implementation

const
  {$IFDEF COMPILER5_UP}
  PathEnd = '\';
  {$ENDIF}
  {$IFDEF LINUX}
  PathEnd = '/';
  {$ENDIF}

function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
  CaseSensitive: Boolean): string;
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
      if CaseSensitive then
        S2 := S1
      else
        S2 := UpperCase(S1);
    end
    else
      Result := Result + S1;
  until Cnt <= 0;
end;

function Jkceil(i: Extended): Longint;
var
  t: Longint;
begin
  t := Trunc(i);
  if t <> i then
    if i > 0 then
      t := t + 1
    else
      t := t - 1;
  Result := t;
end;

function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: Longint; CaseSensitive: Boolean): string;
var
  Count: Longint;
  RepCount: Longint;
  res: string;
begin
  res := AStr;
  if ReplaceNo > 0 then
    RepCount := 0
  else
    RepCount := -1;
  Count := 1;
  if Length(Res) > 0 then
    repeat
      if res[Count] = CharToFind then
      begin
        res[Count] := NewChar;
        if RepCount >= 0 then
           Inc(RepCount, 1);
      end;
      Inc(Count, 1);
    until (Count > Length(Res)) or (RepCount >= ReplaceNo);
  Result := res;
end;

// add the slash character at the end of the path.

function CompletePath(const Path: string): string;
begin
  Result := Path;
  if Length(Path) > 0 then
    if Path[Length(Path)] <> PathEnd then
      Result := Path + PathEnd;
end;

// Remove the slash character from the end of the path.

function IncompletePath(Path: string): string;
begin
  Result := Path;
  if Length(Path) > 0 then
    if Path[Length(Path)] = PathEnd then
      Result := Copy(Path, 1, Length(Path) - 1);
end;

function ProportionalSize(PhysicalSize, NewSize: TPoint): TPoint;
var
  Percent: Single;
  TempX, TempY: Single;
  fs: TPoint;
begin
  // Υπολογισμός ποσοστού  επί της εκατό που θα επιδοθεί στην τιμή προς
  // αλλαγή.
  if PhysicalSize.X <> 0 then
    TempX := (NewSize.X / PhysicalSize.X) * 100
  else
    TempX := 0;
  if PhysicalSize.Y <> 0 then
    TempY := (NewSize.Y / PhysicalSize.Y) * 100
  else
    TempY := 0;
  //Ευρεση μικρότερου ποσοστού αλαγής και χρήση αυτού.
  if TempX <= TempY then
    Percent := TempX
  else
    Percent := TempY;
  //fs.X:=round((PhysicalSize.X/100)*Percent);
  //fs.Y:=round((PhysicalSize.Y/100)*Percent);
  fs.X := Trunc((PhysicalSize.X / 100) * Percent);
  fs.Y := Trunc((PhysicalSize.Y / 100) * Percent);
  Result := fs;
end;

procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
begin
  SetLength(Str, Length(str) + Length(NewStr));
  CopyMemory(@Str[Pos + Length(NewStr) + 1], @Str[Pos + 1], Length(Str) - Pos - Length(NewStr));
  CopyMemory(@Str[Pos + 1], @NewStr[1], Length(NewStr));
end;

function BoundByte(Min, Max, Value: Integer): Byte;
begin
  if Value < Min then
    Result := Min
  else
  if Value > Max then
    Result := Max
  else
    Result := Value;
end;

//******************* Thumbtitle Procedure and functions ******************

procedure TJvThumbTitle.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvThumbTitle.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseDown(Button, Shift, X + Left, Y + Top)
  else
    inherited;
end;

procedure TJvThumbTitle.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseUp(Button, Shift, X + Left, Y + Top)
  else
    inherited;
end;

procedure TJvThumbTitle.Click;
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).Click
  else
    inherited;
end;

procedure TJvThumbTitle.DblClick;
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).DblClick
  else
    inherited;
end;

procedure TJvThumbTitle.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseMove(Shift, X + Left, Y + Top)
  else
    inherited;
end;

function TJvThumbTitle.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  //Inherited;
  if Parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(Parent).DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  //  Result := true;
end;

function TJvThumbTitle.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(Parent).DoMouseWheelDown(Shift, MousePos)
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvThumbTitle.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(Parent).DoMouseWheelUp(Shift, MousePos)
  else
    Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TJvThumbTitle.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).KeyDown(Key, Shift)
  else
    inherited;
end;

procedure TJvThumbTitle.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).KeyUp(Key, Shift)
  else
    inherited;
end;

procedure TJvThumbTitle.KeyPress(var Key: Char);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).KeyPress(Key)
  else
    inherited;
end;

constructor TJvThumbTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvBaseThumbnail then
    ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse, csClickEvents, csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csSetCaption];
end;

//******************* TJvBaseThumbImage Procedure and functions ******************

procedure TJvBaseThumbImage.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvBaseThumbImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseDown(Button, Shift, X + Left, Y + Top)
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvBaseThumbImage.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseUp(Button, Shift, X + Left, Y + Top)
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvBaseThumbImage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseMove(Shift, X + Left, Y + Top)
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure TJvBaseThumbImage.Click;
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).Click
  else
    inherited Click;
end;

procedure TJvBaseThumbImage.DblClick;
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).DblClick
  else
    inherited DblClick;
end;

constructor TJvBaseThumbImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FIgnoreMouse := False;
end;

procedure TJvBaseThumbImage.CMHitTest(var Msg: TCMHitTest);
begin
  if FIgnoreMouse then
    Msg.Result := HTNOWHERE //  0;
  else
    Msg.Result := HTCLIENT; // 1;
end;

//******************* TJvBaseThumbnail Procedure and functions *******************

procedure TJvBaseThumbnail.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).MouseDown(Button, Shift, Left + X, Top + Y)
  else
    inherited;
end;

procedure TJvBaseThumbnail.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvBaseThumbnail.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).MouseMove(Shift, Left + X, Top + Y)
  else
    inherited;
end;

function TJvBaseThumbnail.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  //Inherited;
  if Parent is TJvBaseThumbView then
    Result := TJvBaseThumbView(Parent).DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbView then
    Result := TJvBaseThumbView(Parent).DoMouseWheelDown(Shift, MousePos)
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbView then
    Result := TJvBaseThumbView(Parent).DoMouseWheelUp(Shift, MousePos)
  else
    Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TJvBaseThumbnail.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).KeyDown(Key, Shift)
  else
    inherited;
end;

procedure TJvBaseThumbnail.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).KeyUp(Key, Shift)
  else
    inherited;
end;

procedure TJvBaseThumbnail.KeyPress(var Key: Char);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).KeyPress(Key)
  else
    inherited;
end;

constructor TJvBaseThumbnail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvBaseThumbView then
    ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse] //,
    //                                csClickEvents,csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TJvBaseThumbnail.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).MouseUp(Button, Shift, Left + X, Top + Y)
  else
    inherited;
end;

procedure TJvBaseThumbnail.Click;
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).Click
  else
    inherited Click;
end;

procedure TJvBaseThumbnail.DblClick;
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).DblClick
  else
    inherited DblClick;
end;

constructor TJvBaseThumbView.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csFocusing];
  ControlStyle := ControlStyle + [csOpaque] - [csSetCaption];
end;
{
procedure TJvBaseThumbView.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 0;
end;{}

//******************* TFileName Procedure and functions *******************

procedure TFileName.SetName(NewName: string);
begin
  FFileName := NewName;
  if (NewName <> LongName) and (NewName <> ShortName) then
    Init;
end;

procedure TFileName.Init;
var
  Temp: TWin32FindData;
  SearchHandle: THandle;
  Dft: DWORD;
  Lft: TFileTime;
begin
  SearchHandle := FindFirstFile(PChar(FFileName), Temp);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    FLongName := Temp.cFileName;
    FShortName := Temp.cAlternateFileName;
    if FLongName = '' then
      FLongName := FShortName;
    if FShortName = '' then
      FShortName := FLongName;
    //fdFileAccessed
    FileTimeToLocalFileTime(Temp.ftLastAccessTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).hi, LongRec(Dft).lo);
    FAccessed := Dft;
    //fdFilechanged
    FileTimeToLocalFileTime(Temp.ftLastWriteTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).hi, LongRec(Dft).lo);
    FModified := Dft;
    //fdFilecreated
    FileTimeToLocalFileTime(Temp.ftCreationTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).hi, LongRec(Dft).lo);
    FCreated := Dft;
    FFileSize := (Temp.nFileSizeHigh * MAXDWORD) + Temp.nFileSizeLow;
    //FFileName:=NewName;
  end
  else
    FFileName := '';
  Windows.FindClose(SearchHandle);
end;


procedure TFileName.LoadFromStream(AStream: TStream; APos: Integer);
begin
  // Under Construction;
end;

procedure TFileName.SaveToStream(AStream: TStream; APos: Integer);
begin
  //Under Construction
end;

function TFileName.GetLength: Integer;
begin
  Result := System.Length(FFileName);
end;

procedure TFileName.SetLength(NewLength: Integer);
begin
  System.SetLength(FFileName, NewLength);
end;

constructor TFileName.Create;
begin
  inherited Create;
end;

destructor TFileName.Destroy;
begin
  inherited Destroy;
end;

end.

