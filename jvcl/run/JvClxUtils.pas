{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClxUtils.pas released on 2003-10-25.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen,
[Andreas.Hausladen@gmx.de] and André Snepvangers
All Rights Reserved.

Contributor(s):

Last Modified: 2003-10-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvClxUtils;
interface
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF INUX}
  Libc,
  {$ENDIF}
  {$IFDEF COMPLIB_VCL}
  Graphics,
  {$ENDIF}
  {$IFDEF COMPLIB_CLX}
  Qt, QTypes, Types, QGraphics, QForms,
  {$ENDIF}
  SysUtils, Classes,
  JvTypes;

{$IFDEF LINUX}
type
  TColorRef = Integer;
{$ENDIF}

{$IFDEF COMPLIB_CLX}

function GetSysColor(Color: Integer): TColorRef;

procedure SetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement);
procedure GetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement);
function IsWindowVisible(Handle: QWidgetH): Boolean;
function IsWindowEnabled(Handle: QWidgetH): Boolean;
procedure EnableWindow(Handle: QWidgetH; Value: Boolean);
procedure SwitchToThisWindow(Handle: QWidgetH; Restore: Boolean);
procedure SetFocus(Handle: QWidgetH);

function RGB(Red, Green, Blue: Integer): TColorRef;
function GetBValue(Col: TColorRef): Integer;
function GetGValue(Col: TColorRef): Integer;
function GetRValue(Col: TColorRef): Integer;

procedure MessageBeep(Value: Integer);

{$IFDEF LINUX}
function GetTickCount: Cardinal;
function MakeIntResource(Value: Integer): PChar;
{$ENDIF}

type
  TSysMetrics = (
    SM_CXSCREEN, SM_CYSCREEN,

    SM_CXVSCROLL, SM_CYVSCROLL,

    SM_CXSMICON, SM_CXICON,

    SM_CXBORDER, SM_CYBORDER,

    SM_CXFRAME, SM_CYFRAME

  );

// limited implementation of

function GetSystemMetrics(PropItem: TSysMetrics): Integer;

function TruncatePath(const FilePath: string; Canvas: TCanvas; MaxLen: Integer): string;
function TruncateName(const Name: String; Canvas: TCanvas; MaxLen: Integer): string;

const
// constants for Canvas.TextRect
  AlignLeft = 1 { $1 };
  AlignRight = 2 { $2 };
  AlignHCenter = 4 { $4 };
  AlignTop = 8 { $8 };
  AlignBottom = 16 { $10 };
  AlignVCenter = 32 { $20 };
  AlignCenter = 36 { $24 };
  SingleLine = 64 { $40 };
  DontClip = 128 { $80 };
  ExpandTabs = 256 { $100 };
  ShowPrefix = 512 { $200 };
  WordBreak = 1024 { $400 };
  ModifyString = 2048 { $800 };
  DontPrint = 4096 { $1000 };
  ClipPath = 8192 { $2000 };
  ClipName = 16382 { $4000 };
  CalcRect =  32764 { $8000 } ;
  pf24bit = pf32bit;
{$ENDIF COMPLIB_CLX}

const
  DT_ELLIPSIS = DT_END_ELLIPSIS;

{$IFDEF LINUX}
const
  { ClxDrawText() Format Flags }
  DT_TOP = 0;          // default
  DT_LEFT = 0;         // default
  DT_CENTER = 1;
  DT_RIGHT = 2;
  DT_VCENTER = 4;
  DT_BOTTOM = 8;
  DT_WORDBREAK = $10;
  DT_SINGLELINE = $20;
  DT_EXPANDTABS = $40;
//  DT_TABSTOP = $80;
  DT_NOCLIP = $100;
//  DT_EXTERNALLEADING = $200;
  DT_CALCRECT = $400;
  DT_NOPREFIX = $800;
//  DT_INTERNAL = $1000;
//  DT_HIDEPREFIX = $00100000;
//  DT_PREFIXONLY = $00200000;

//  DT_EDITCONTROL = $2000;
  DT_PATH_ELLIPSIS = $4000;
  DT_END_ELLIPSIS = $8000;
  DT_ELLIPSIS = DT_END_ELLIPSIS;
  DT_MODIFYSTRING = $10000;
//  DT_RTLREADING = $20000;
//  DT_WORD_ELLIPSIS = $40000;

  { ClxExtTextOut() Format Flags }
  ETO_OPAQUE = 2;
  ETO_CLIPPED = 4;
//  ETO_GLYPH_INDEX = $10;
  ETO_RTLREADING = $80;  // ignored
//  ETO_NUMERICSLOCAL = $400;
//  ETO_NUMERICSLATIN = $800;
//  ETO_IGNORELANGUAGE = $1000;
//  ETO_PDY = $2000;

  { ShowWindow() Commands }
  SW_HIDE = 0;
  SW_SHOWNORMAL = 1;
  SW_NORMAL = 1;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMAXIMIZED = 3;
  SW_MAXIMIZE = 3;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOW = 5;
  SW_MINIMIZE = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA = 8;
  SW_RESTORE = 9;
  SW_SHOWDEFAULT = 10;
  SW_MAX = 10;
{$ENDIF LINUX}


function ClxDrawText(Canvas: TCanvas; var Caption: string; var R: TRect;
  Flags: Integer): Integer;
function ClxDrawTextW(Canvas: TCanvas; var Caption: WideString; var R: TRect;
  Flags: Integer): Integer;

function ClxExtTextOut(Canvas: TCanvas; X, Y: Integer; Flags: Integer; Rect: PRect;
  const Text: String; lpDx: Pointer): Boolean;
function ClxExtTextOutW(Canvas: TCanvas; X, Y: Integer; Flags: Integer;
  Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean;

implementation

{$IFDEF COMPLIB_CLX}
function GetSysColor(Color: Integer): TColorRef;
begin
  Result := TColorRef(Application.Palette.GetColor(Color));
end;

procedure EnableWindow(Handle: QWidgetH; Value: Boolean);
begin
  QWidget_setEnabled(Handle, Value);
end;

procedure SetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement);
begin
  with W.rcNormalPosition do
    QWidget_setGeometry(Handle, Left, Top, Right - Left, Bottom - Top);
  case W.ShowCmd of
    SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
      QWidget_showMinimized(Handle);
    SW_MAXIMIZE:
      QWidget_showMaximized(Handle);
    SW_HIDE:
      QWidget_hide(Handle);
  else
    QWidget_showNormal(Handle);
  end;
end;

procedure GetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement);
var
  R : TRect;
begin
  QWidget_geometry(Handle, @R);
  W.rcNormalPosition.Left := R.Left;
  W.rcNormalPosition.Top := R.Top;
  W.rcNormalPosition.Right := R.Right;
  W.rcNormalPosition.Bottom := R.Left;
  if QWidget_isMinimized(Handle) then
    W.showCmd := SW_SHOWMINIMIZED
  else if QWidget_isMaximized(Handle) then
    W.showCmd := SW_SHOWMAXIMIZED
  else if not QWidget_isVisible(Handle) then
    W.showCmd := SW_HIDE
  else
    W.showCmd := SW_SHOWNORMAL;
end;

function IsWindowVisible(Handle: QWidgetH): Boolean;
begin
  Result := QWidget_isVisible(Handle);
end;

function IsWindowEnabled(Handle: QWidgetH): Boolean;
begin
  Result := QWidget_isEnabled(Handle);
end;

procedure SetFocus(Handle: QWidgetH);
begin
  QWidget_setFocus(Handle);
end;

procedure SetForegroundWindow(Handle: QWidgetH);
begin
  QWidget_raise(Handle);
end;

procedure SwitchToThisWindow(Handle: QWidgetH; Restore: Boolean);
begin
  if Restore then
    QWidget_Show(Handle);
  QWidget_setActiveWindow(Handle);
end;

// limited implementation of
function GetSystemMetrics(PropItem: TSysMetrics): Integer;
var

  size: TSize;
begin
  case PropItem of
    SM_CXVSCROLL:
      begin
        QStyle_scrollBarExtent(Application.Style.Handle, @size);
        Result := size.cx;

      end;

    SM_CYVSCROLL:
      begin
        QStyle_scrollBarExtent(Application.Style.Handle, @size);
        Result := size.cy;

      end;

    SM_CXSMICON:

      Result := 16;

    SM_CXICON:
      Result := 32;
    SM_CXSCREEN:
      Result := Screen.Width;
    SM_CYSCREEN:
      Result := Screen.Height;
    SM_CXBORDER, SM_CYBORDER:
      Result := Application.Style.DefaultFrameWidth; // (probably) wrong ?
    SM_CXFRAME, SM_CYFRAME:
      Result := Application.Style.DefaultFrameWidth; // or this one
  else
    Result := 0;
  end;

end;

function RGB(Red, Green, Blue: Integer): TColorRef;
begin
  Result := (Blue shl 16) or (Green shl 8) or Red;
end;

function GetBValue(Col: TColorRef): Integer;
begin
  Result := (Col shr 16) and $FF;
end;

function GetGValue(Col: TColorRef): Integer;
begin
  Result := (Col shr 8) and $FF;
end;

function GetRValue(Col: TColorRef): Integer;
begin
  Result := Col and $FF;
end;

procedure MessageBeep(Value: Integer);
begin
  Beep;
end;

{$IFDEF LINUX}
function GetTickCount: Cardinal;
var
  Info: TSysInfo;
  TimeVal: TTimeVal;
begin
  sysinfo(Info);
  gettimeofday(TimeVal, nil);
  Result := Cardinal((Int64(Info.uptime) * 1000) + Round(TimeVal.tv_usec / 1000));
end;

function MakeIntResource(Value: Integer): PChar;
begin
  Result := PChar(Value and $0000ffff);
end;
{$ENDIF LINUX}

function TruncatePath(const FilePath: string; Canvas: TCanvas; MaxLen: Integer): string;
const
  Ellipses = '...';
var
  Paths: TStrings;
  k, i, start: Integer;
  CurPath: string;
begin
  if Canvas.TextWidth(FilePath) <= MaxLen then
    Result := FilePath
  else
  begin    // FilePath too long
    Paths := TStringList.Create;
    try
      Paths.Delimiter := PathDelim;
      Paths.DelimitedText := FilePath ; // splits the filepath
      if Length(paths[0]) = 0 then
        start := 1
      else
        start := 0;
      for k := start to Paths.Count - 2 do
      begin
        CurPath := Paths[k] ;
        if Length(CurPath) > 2 then   // this excludes ~ ..
        begin
          Paths[k] := Ellipses ; // replace with ellipses
          I := 1;
          while Canvas.TextWidth(Paths.DelimitedText) <= MaxLen do
          begin
            Paths[k] := Copy(CurPath, I, MaxInt) + Ellipses;   // add a character
            Inc(I);
          end;
          if I <> 1 then
          begin
            // remove last added character
            Paths[k] := Copy(Paths[k], 2, MaxInt);
            Result := Paths.DelimitedText ; // something /.../P../bin/file.tst
            Exit; // ready
          end;
        end
      end;
      // not succeeded.
      // replace /.../.../.../<filename> with .../<filename>
      // before starting to minimize filename
      for k := Paths.count - 2 downto 1 do
        Paths.Delete(k);
      Paths[0] := Ellipses;
      if Canvas.TextWidth(Paths.DelimitedText) > MaxLen then
      begin
        CurPath := Paths[1];
        Paths[1] := Ellipses; // replace with ellipses
        I := 1 ;
        while Canvas.TextWidth(Paths.DelimitedText) <= MaxLen do
        begin
          Paths[1] := Copy(CurPath, I, MaxInt) + Ellipses;
          Inc(I);
        end;
        if I <> 1 then
          Paths[1] := Copy(Paths[1], 2, MaxInt);
      end;
      Result := Paths.DelimitedText;    // will be something .../Progr...
    finally
      Paths.Free;
    end;
  end;
end;

function TruncateName(const Name: String; Canvas: TCanvas; MaxLen: Integer): string;
const
  Ellipses = '...';
var
  I: Integer;
begin
  if Canvas.TextWidth(Name) <= MaxLen then
    Result := Name
  else
  begin
    Result := Ellipses ; // replace with ellipses
    I := 1;
    while Canvas.TextWidth(Result) <= MaxLen do
    begin
      Result := Copy(Name, I, MaxInt) + Ellipses;   // add a character
      Inc(I);
    end;
    if I <> 1 then
      // remove last added character
      Delete(Result, 1, 1);
  end;
end;

{$ENDIF COMPLIB_CLX}

function ClxDrawText(Canvas: TCanvas; var Caption: string; var R: TRect;
  Flags: Integer): Integer;
{$IFNDEF COMPLIB_VCL}
var W: WideString;
{$ENDIF}
begin
{$IFDEF COMPLIB_VCL}
  Result := DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, Flags);
{$ELSE}
  W := Caption;
  Result := ClxDrawTextW(Canvas, W, R, Flags);
  if Flags and DT_MODIFYSTRING <> 0 then
    Caption := W;
{$ENDIF}
end;

function ClxDrawTextW(Canvas: TCanvas; var Caption: WideString; var R: TRect;
  Flags: Integer): Integer;
{$IFDEF COMPLIB_CLX}
var
  Flgs: Word;
  Text: string;
{$ENDIF}
begin
{$IFDEF COMPLIB_VCL}
  Result := DrawTextW(Canvas.Handle, PWideChar(Caption), Length(Caption), R, Flags);
{$ENDIF}
{$IFDEF COMPLIB_CLX}
  Text := Caption;
  with Canvas do
  begin
    Flgs := 0;
    if Flags and DT_SINGLELINE <> 0 then
       Flgs := SingleLine;
    if Flags and DT_WORDBREAK <> 0 then
       Flgs := Flgs or WordBreak;
    if Flags and DT_EXPANDTABS <> 0 then
      Flgs := Flgs or ExpandTabs;
    if Flags and DT_NOPREFIX = 0 then
      Flgs := Flgs or ShowPrefix;
    if Flags and DT_RIGHT <> 0 then
      Flgs := Flgs or AlignRight
    else if Flags and DT_CENTER <> 0 then
      Flgs := Flgs or AlignHCenter
    else
      Flgs := Flgs or AlignLeft ; // default
    // vertical alignment
    if Flags and DT_BOTTOM <> 0 then
      Flgs := Flgs or AlignTop
    else if Flags and DT_VCENTER <> 0 then
      Flgs := Flgs or AlignVCenter
    else
      Flgs := Flgs or AlignTop;  // default

    if Flags and DT_ELLIPSIS <> 0 then
      Text := TruncateName(Text, Canvas, R.Right - R.Left)
    else if Flags and DT_PATH_ELLIPSIS <> 0 then
      Text := TruncatePath(Text, Canvas, R.Right - R.Left)
    else if Flags and DT_CALCRECT <> 0 then
    begin
      TextExtent(Caption, R, flgs);
      Result := R.Bottom - R.Top;
      Exit;
    end;
    Canvas.TextRect(R, R.Left, R.Top, Text, Flgs);
    if Flags and DT_MODIFYSTRING <> 0 then
      Caption := Text;
  end;
  Result := 1;
{$ENDIF COMPLIB_CLX}
end;

function ClxExtTextOut(Canvas: TCanvas; X, Y: Integer; Flags: Integer; Rect: PRect;
  const Text: String; lpDx: Pointer): Boolean;
begin
{$IFDEF COMPLIB_VCL}
  Result := ExtTextOut(Canvas.Handle, X, Y, Flags, Rect, PChar(Text),
    Length(Text), lpDx);
{$ELSE}
  Result := ClxExtTextOutW(Canvas, X, Y, Flags, Rect, WideString(Text), lpDx);
{$ENDIF}
end;

function ClxExtTextOutW(Canvas: TCanvas; X, Y: Integer; Flags: Integer;
  Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean;
{$IFDEF COMPLIB_CLX}
{ missing feature: horizontal text alignment }
var
  RecallBrush: TBrush;
  RecallPenPos: TPoint;
  Ch: WideChar;
  Index, Width: Integer;
  Dx: PInteger;
  R, CellRect: TRect;
  TextLen: Integer;
{$ENDIF}
begin
{$IFDEF COMPLIB_VCL}
  Result := ExtTextOutW(Canvas.Handle, X, Y, Flags, Rect, PWideChar(Text),
    Length(Text), lpDx);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
  with Canvas do
  begin
    Result := False;
    if (Text = '') then
      Exit;
    if (Flags and ETO_CLIPPED <> 0) and (Rect = nil) then
      Flags := Flags and not ETO_CLIPPED;

    RecallPenPos := PenPos;
    Result := True;
    RecallBrush := nil;
    try
      if Flags and ETO_OPAQUE <> 0 then
      begin
        if Brush.Style <> bsSolid then
        begin
          RecallBrush := TBrush.Create;
          RecallBrush.Assign(Brush);
          Brush.Style := bsSolid;
        end;
        if Rect <> nil then
          FillRect(Rect^);
      end
      else
        if (Brush.Style = bsSolid) then
        begin
          RecallBrush := TBrush.Create;
          RecallBrush.Assign(Brush);
          Brush.Style := bsClear;
        end;

      if lpDx = nil then
      begin
        if (Flags and ETO_CLIPPED <> 0) then
          TextRect(Rect^, X, Y, Text)
        else
          TextOut(X, Y, Text);
      end
      else
      begin
       // put each char in it's cell
        TextLen := Length(Text);
        if (Flags and ETO_OPAQUE <> 0) and (Rect = nil) then
        begin
          Dx := lpDx;
          Width := 0;
          for Index := 1 to TextLen do
          begin
            Inc(Width, Dx^);
            Inc(Dx);
          end;
          R.Left := X;
          R.Right := X + Width;
          R.Top := Y;
          R.Bottom := Y + TextHeight(Text);
          FillRect(R);
        end;

        Dx := lpDx;
        for Index := 1 to TextLen do
        begin
          if (Rect <> nil) and (X >= Rect^.Right) then
            Break;

          Ch := Text[Index];
          if Flags and ETO_CLIPPED <> 0 then
          begin
            CellRect.Left := X;
            CellRect.Right := X + Dx^;
            CellRect.Top := Rect^.Top;
            CellRect.Bottom := Rect^.Bottom;
            if CellRect.Right > Rect^.Right then
              CellRect.Right := Rect^.Right;
            TextRect(R, X, Y, Ch);
          end
          else
            TextOut(X, Y, Ch);

          if Index = TextLen then
            Break;

          Inc(X, Dx^);
          Inc(Dx);
        end;
      end;
    finally
      if Assigned(RecallBrush) then
      begin
        Brush.Assign(RecallBrush);
        RecallBrush.Free;
      end;
    end;
    PenPos := RecallPenPos;
  end;
{$ENDIF COMPLIB_CLX}
end;

end.
