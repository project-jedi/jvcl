{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBasethb.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer att Excite dott com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

You may Thumb the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  This file contains (most likely) greek comments.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}


{$WARN SYMBOL_PLATFORM OFF} // TSearchRec.FindData


unit JvQBaseThumbnail;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, // TWin32FindData
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc, // stat()
  {$ENDIF LINUX}
  
  
  QGraphics, QControls, QForms, QExtCtrls, Types,
  
  SysUtils, Classes,
  JvQExForms, JvQExExtCtrls;

// (rom) TFileName is already declared in SysUtils

type
  { The TFileName object has been created to handle the first field of a Thumb
    Which is the Thumbs actual FileName complete with the Path because no
    duplicates are allowed in the final list.
    It Has the following properties
      01) FileName : it keeps the filename as given by the user
      02) LongName : it always returns the LongName of the file
      03) ShortName: it always returns the short name of the file
      04) Size     : it returns the size in Bytes that it will occupy if saved in a stream
      05) Length   : the "FileName" property Length;
    and the following methods
      01) LoadFromStream(AStream: TStream; APos: Integer); loads a filename from a stream
          if APos < 0 then don't change the cursor position in the stream
          else AStream.Seek(APos, 0);
      02) SaveToStream(AStream: TStream; APos: Integer); Save the FileName to AStream
          if APos > -1 then AStream.Seek(APos, 0);
          SaveData;
  }
  TProgressNotify = procedure(Sender: TObject; Position: Integer; var Stop: Boolean) of object;
  TInvalidImageEvent = procedure(Sender: TObject; const AFileName: string) of object;
  // (rom) renamed
  TGRFKind = (grBMP, grJPG, grWMF, grEMF, grICO,  grPNG); //,grPCX,grTGA);
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
    procedure LoadFromStream(AStream: TStream; APos: Integer); //Load From stream
    // both of this routines are inserting extract data to the stream its self
    // like a header and data end string;
    procedure SaveToStream(AStream: TStream; APos: Integer); // Save to a Stream
    // (rom) moved to public
    property LongName: string read FLongName; // The LongName of this filename
    property ShortName: string read FShortName; // shortname of this filename
  published
    property FileName: string read FFileName write SetName; // The FileName as given by the user
    property Length: Integer read GetLength write SetLength;
  end;

  { The Following classes are declared here so I can handle interaction of the mouse
    between the three components.
  }
  TJvThumbTitle = class(TJvExPanel)
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
       const  MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState;
       const  MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;
       const  MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvBaseThumbImage = class(TJvExImage)
  private
    FIgnoreMouse: Boolean;
  protected
    function HitTest(X, Y: Integer): Boolean; override;
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

  TJvBaseThumbnail = class(TJvExPanel)
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
       const  MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState;
       const  MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;
       const  MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvBaseThumbView = class(TJvExScrollBox)
  protected
    // function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function BoundByte(Min, Max, Value: Integer): Byte;
procedure InsertStr(var Str: string; const NewStr: string; Pos: Longint);
function ProportionalSize(PhysicalSize, NewSize: TPoint): TPoint;
function ReplaceChar(const AStr: string; const CharToFind, NewChar: Char;
  ReplaceNo: Longint; CaseSensitive: Boolean): string;
function JkCeil(I: Extended): Longint;
function ReplaceAllStr(const Str, SearchFor, ReplaceWith: string;
  CaseSensitive: Boolean): string;

implementation

uses
  JvQThemes;

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
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
end;

function TJvThumbTitle.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  inherited DoPaintBackground(Canvas, Param);
  Result := True;
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
   const  MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(Parent).DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TJvThumbTitle.DoMouseWheelDown(Shift: TShiftState;
   const  MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbnail then
    Result := TJvBaseThumbnail(Parent).DoMouseWheelDown(Shift, MousePos)
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvThumbTitle.DoMouseWheelUp(Shift: TShiftState;
   const  MousePos: TPoint): Boolean;
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
  {  If AOwner is TJvBaseThumbnail then
     begin
      ControlStyle := ControlStyle - [csCaptureMouse];
      FIgnoreMouse := True;
    end
    else}
  FIgnoreMouse := False;
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

function TJvBaseThumbImage.HitTest(X, Y: Integer): Boolean;
{const
  Hits: array [Boolean] of Longint = (HTCLIENT, HTNOWHERE);}
begin
  if csDesigning in ComponentState then
    Result := inherited HitTest(X, Y)
  else
    Result := not IgnoreMouse;
    //Msg.Result := Hits[IgnoreMouse];
end;

//=== TJvBaseThumbnail =======================================================

constructor TJvBaseThumbnail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvBaseThumbView then
    ControlStyle := ControlStyle - [csSetCaption, csCaptureMouse]
    //                                csClickEvents,csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TJvBaseThumbnail.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).MouseDown(Button, Shift, Left + X, Top + Y)
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

function TJvBaseThumbnail.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  inherited DoPaintBackground(Canvas, Param);
  Result := True;
end;

procedure TJvBaseThumbnail.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).MouseMove(Shift, Left + X, Top + Y)
  else
    inherited MouseMove(Shift, X, Y);
end;

function TJvBaseThumbnail.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
   const  MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbView then
    Result := TJvBaseThumbView(Parent).DoMouseWheel(Shift, WheelDelta, MousePos)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelDown(Shift: TShiftState;
   const  MousePos: TPoint): Boolean;
begin
  if Parent is TJvBaseThumbView then
    Result := TJvBaseThumbView(Parent).DoMouseWheelDown(Shift, MousePos)
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TJvBaseThumbnail.DoMouseWheelUp(Shift: TShiftState;
   const  MousePos: TPoint): Boolean;
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
    inherited KeyDown(Key, Shift);
end;

procedure TJvBaseThumbnail.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).KeyUp(Key, Shift)
  else
    inherited KeyUp(Key, Shift);
end;

procedure TJvBaseThumbnail.KeyPress(var Key: Char);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).KeyPress(Key)
  else
    inherited KeyPress(Key);
end;

procedure TJvBaseThumbnail.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Parent is TJvBaseThumbView then
    TJvBaseThumbView(Parent).MouseUp(Button, Shift, Left + X, Top + Y)
  else
    inherited MouseUp(Button, Shift, X, Y);
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

//=== TJvBaseThumbView =======================================================

constructor TJvBaseThumbView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csFocusing];
  ControlStyle := ControlStyle + [csOpaque] - [csSetCaption];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
end;
{
function TJvBaseThumbView.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  //Result :=
    inherited DoPaintBackground(Canvas, Param);
  Result := False;
end;
}

//=== TFileName ==============================================================

procedure TFileName.SetName(NewName: string);
begin
  FFileName := NewName;
  if (NewName <> LongName) and (NewName <> ShortName) then
    Init;
end;

procedure TFileName.Init;
var
  {$IFDEF MSWINDOWS}
  Dft: DWORD;
  Lft: TFileTime;
  {$ENDIF MSWINDOWS}
  sr: TSearchRec;
begin
  if FindFirst(FFileName, faAnyFile or faDirectory, sr) = 0 then
  begin
    FindClose(sr);

    FLongName := sr.FindData.cFileName;
    FShortName := sr.FindData.cAlternateFileName;
    if FLongName = '' then
      FLongName := FShortName;
    if FShortName = '' then
      FShortName := FLongName;
    //fdFileAccessed
    FileTimeToLocalFileTime(sr.FindData.ftLastAccessTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    FAccessed := Dft;
    //fdFilechanged
    FileTimeToLocalFileTime(sr.FindData.ftLastwriteTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    FModified := Dft;
    //fdFilecreated
    FileTimeToLocalFileTime(sr.FindData.ftCreationTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    FCreated := Dft;
    FFileSize := (sr.FindData.nFileSizeHigh * MAXDWORD) + sr.FindData.nFileSizeLow;
    //FFileName:=NewName;
  end;
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

end.

