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
  This file contains (most likely) greek comments.
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBaseThumbnail;

interface

uses
  Classes, Controls, ExtCtrls, Windows, SysUtils, Messages, Forms;

type
  { The TfileName Object has been created to handle the first field of a Thumb
    Wich is the Thumbs actual FileName complite with the Path because no
    duplicates are alowed in the final list.
    It Has the following properties
      01) Filename : it Keeps the filename as gived by the user
      02) LongName : It always returns the longName of the file
      03) ShortName : It always returns the short name of the file
      04) Size      : It returns the size in Byte that it will occupy if saved in a stream
      05) Length    : The "Filename" property Length;
    and the following Methods
      01) LoadFromStream(AStream:TStream;APos:Integer); It loads a filename from a stream
          if APos < 0 then don't change the cursors position in the stream
          else AStream.seek(APos,0);
      02) SaveToStream(AStream:TStream;APos:Integer); Save the Filename to AStream
          If APos>-1 then AStream.seek(APos,0);
          SaveData;
  }
  TProgressNotify = procedure(Sender: TObject; Position: Integer; var Break: Boolean) of object;
  TInvalidImageEvent = procedure(Sender: TObject; const AFilename: string) of object;
  // (rom) renamed
  TGRFKind = (grBMP, grJPG, grWMF, grEMF, grICO, {$IFNDEF COMPILER6_UP} grGIF,{$ENDIF} grPNG); //,grPCX,grTGA);
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
    procedure LoadFromStream(AStream: TStream; APos: Integer); //Load From stream;
    // both of this routines are inserting extract data to the stream its self
    // like a header and data end string;
    procedure SaveToStream(AStream: TStream; APos: Integer); // Save to a Stream;
    // (rom) moved to public
    property LongName: string read FLongName; // The LongName of this filenam;
    property ShortName: string read FShortName; // shortname of this filename
  published
    property Filename: string read FFileName write SetName; // The Filename as given by the user;
    property Length: Integer read GetLength write SetLength;
  end;

  { The Following classes are declared here so I can handle interaction of the mouse
    between the three components.
  }
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
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
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
    property IgnoreMouse: Boolean read FIgnoreMouse write FIgnoreMouse;
  end;

  TJvBaseThumbnail = class(TPanel)
  private
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
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
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TJvBaseThumbView = class(TScrollBox)
  private
    //    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

function BoundByte(Min, Max, Value: Integer): Byte;
procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
function ProportionalSize(PhysicalSize, NewSize: TPoint): TPoint;
function CompletePath(const Path: string): string;
function IncompletePath(APath: string): string;
function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: Longint; CaseSensitive: Boolean): string;
function JkCeil(I: Extended): Longint;
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

function JkCeil(I: Extended): Longint;
var
  T: Longint;
begin
  T := Trunc(I);
  if T <> I then
    if I > 0 then
      T := T + 1
    else
      T := T - 1;
  Result := T;
end;

function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: Longint; CaseSensitive: Boolean): string;
var
  Count: Longint;
  RepCount: Longint;
  Res: string;
begin
  Res := Astr;
  if ReplaceNo > 0 then
    RepCount := 0
  else
    RepCount := -1;
  Count := 1;
  if Length(Res) > 0 then
    repeat
      if Res[count] = CharToFind then
      begin
        Res[count] := NewChar;
        if RepCount >= 0 then
          Inc(RepCount, 1);
      end;
      Inc(Count, 1);
    until (Count > Length(Res)) or (RepCount >= ReplaceNo);
  Result := Res;
end;

// add the slash character at the end of the Path.

function CompletePath(const Path: string): string;
var
  Res: string;
begin
  if Length(Path) > 0 then
  begin
    if Path[Length(Path)] <> PathEnd then
      Res := Path + PathEnd
    else
      Res := Path;
  end
  else
    Res := Path;
  Result := Res;
end;

// Remove the slash character from the end of the Path.

function IncompletePath(APath: string): string;
begin
  if Length(APath) > 0 then
  begin
    if APath[Length(APath)] = PathEnd then
      Result := Copy(APath, 1, Length(APath) - 1)
    else
      Result := APath;
  end
  else
    Result := APath;
end;

function ProportionalSize(PhysicalSize, NewSize: TPoint): TPoint;
var
  Percent: Single;
  TempX, TempY: Single;
begin
  // Υπολογισμός ποσοστού  επί της εκατό που θα επιδοθεί στην τιμή προς
  // αλλαγή. [This seems to be greek, couldn't find translator]
  if PhysicalSize.X <> 0 then
    TempX := ((NewSize.X) / PhysicalSize.X) * 100.0
  else
    TempX := 0;
  if PhysicalSize.Y <> 0 then
    TempY := ((NewSize.Y) / PhysicalSize.Y) * 100.0
  else
    TempY := 0;
  //Ευρεση μικρότερου ποσοστού αλαγής και χρήση αυτού.
  // [this seems to be greek, couldn't find translator]
  if TempX <= TempY then
    Percent := TempX
  else
    Percent := TempY;
  //Fs.X:=round((PhysicalSize.X/100)*Percent);
  //Fs.Y:=round((PhysicalSize.Y/100)*Percent);
  Result.X := Trunc((PhysicalSize.X / 100.0) * Percent);
  Result.Y := Trunc((PhysicalSize.Y / 100.0) * Percent);
end;

procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
begin
  SetLength(Str, Length(Str) + Length(NewStr));
  Move(Str[Pos + 1], Str[Pos + Length(NewStr) + 1], Length(Str) - Pos - Length(NewStr));
  Move(NewStr[1], Str[Pos + 1], Length(NewStr));
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

//=== TJvThumbTitle ==========================================================

constructor TJvThumbTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvBaseThumbnail then
    ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse, csClickEvents, csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TJvThumbTitle.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvThumbTitle.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseDown(Button, Shift, X + Left, Y + Top)
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvThumbTitle.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseUp(Button, Shift, X + Left, Y + Top)
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvThumbTitle.Click;
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).Click
  else
    inherited Click;
end;

procedure TJvThumbTitle.DblClick;
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).DblClick
  else
    inherited DblClick;
end;

procedure TJvThumbTitle.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).MouseMove(Shift, X + Left, Y + Top)
  else
    inherited MouseMove(Shift, X, Y);
end;

function TJvThumbTitle.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(Parent).DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
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
    inherited KeyDown(Key, Shift);
end;

procedure TJvThumbTitle.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).KeyUp(Key, Shift)
  else
    inherited KeyUp(Key, Shift);
end;

procedure TJvThumbTitle.KeyPress(var Key: Char);
begin
  if Parent is TJvBaseThumbnail then
    TJvBaseThumbnail(Parent).KeyPress(Key)
  else
    inherited KeyPress(Key);
end;

//=== TJvBaseThumbImage ======================================================

constructor TJvBaseThumbImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  {  If AOwner is TJvBaseThumbnail then begin
      ControlStyle := ControlStyle-[csCaptureMouse];
      FIgnoreMouse := True
    end Else{}
  FIgnoreMouse := False;
end;

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

procedure TJvBaseThumbImage.CMHitTest(var Msg: TCMHitTest);
begin
  if csDesigning in ComponentState then
    inherited;
  if FIgnoreMouse then
    Msg.Result := HTNOWHERE //  0;
  else
    Msg.Result := HTCLIENT; // 1;
end;

//=== TJvBaseThumbnail =======================================================

constructor TJvBaseThumbnail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvBaseThumbview then
    ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse] //,
    //                                csClickEvents,csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TJvBaseThumbnail.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).MouseDown(Button, Shift, Left + X, Top + Y)
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvBaseThumbnail.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvBaseThumbnail.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).MouseMove(Shift, Left + X, Top + Y)
  else
    inherited MouseMove(Shift, X, Y);
end;

function TJvBaseThumbnail.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbview then
    Result := TJvBaseThumbview(Parent).DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbview then
    Result := TJvBaseThumbview(Parent).DoMouseWheelDown(Shift, MousePos)
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbview then
    Result := TJvBaseThumbview(Parent).DoMouseWheelUp(Shift, MousePos)
  else
    Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TJvBaseThumbnail.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).KeyDown(Key, Shift)
  else
    inherited KeyDown(Key, Shift);
end;

procedure TJvBaseThumbnail.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).KeyUp(Key, Shift)
  else
    inherited KeyUp(Key, Shift);
end;

procedure TJvBaseThumbnail.KeyPress(var Key: Char);
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).KeyPress(Key)
  else
    inherited KeyPress(Key);
end;

procedure TJvBaseThumbnail.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).MouseUp(Button, Shift, Left + X, Top + Y)
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvBaseThumbnail.Click;
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).Click
  else
    inherited Click;
end;

procedure TJvBaseThumbnail.DblClick;
begin
  if Parent is TJvBaseThumbview then
    TJvBaseThumbview(Parent).DblClick
  else
    inherited DblClick;
end;

//=== TJvBaseThumbview =======================================================

constructor TJvBaseThumbview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csFocusing];
  ControlStyle := ControlStyle + [csOpaque] - [csSetCaption];
end;
{
procedure TJvBaseThumbview.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  message.Result := 0;
end;{}

//=== TFileName ==============================================================

constructor TFileName.Create;
begin
  inherited Create;
end;

destructor TFileName.Destroy;
begin
  inherited Destroy;
end;

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
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    FAccessed := Dft;
    //fdFilechanged
    FileTimeToLocalFileTime(Temp.ftLastwriteTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    FModified := Dft;
    //fdFilecreated
    FileTimeToLocalFileTime(Temp.ftCreationTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
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
  // Uner Construction;
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

end.

