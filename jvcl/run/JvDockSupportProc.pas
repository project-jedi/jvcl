{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockSupportProc.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDockSupportProc;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms;

type
  TJvDockListScanKind = (lskForward, lskBackward);

function JvDockStreamDataToString(Stream: TStream): string;
procedure JvDockStringToStreamData(Stream: TStream; Data: string);

function JvDockFindDockFormWithName(FormName: string;
  FromDockManager: Boolean = False;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockServerFormWithName(FormName: string;
  FromDockManager: Boolean = False;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockClientFormWithName(FormName: string;
  FromDockManager: Boolean = False;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockServerFromDockManager(FormName: string;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockClientFromDockManager(FormName: string;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockFormFromScreen(FormName: string;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;

function JvDockGetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;

function JvDockGetNoNClientMetrics: TNONCLIENTMETRICS;

function JvDockGetSysCaptionHeight: Integer;

function JvDockGetSysBorderWidth: Integer;
function JvDockGetSysCaptionHeightAndBorderWidth: Integer;
function JvDockGetActiveTitleBeginColor: TColor;
function JvDockGetActiveTitleEndColor: TColor;
function JvDockGetInactiveTitleBeginColor: TColor;
function JvDockGetInactiveTitleEndColor: TColor;
function JvDockGetTitleFontColor(Active: Boolean): TColor;
function JvDockGetActiveTitleFontColor: TColor;
function JvDockGetInactiveTitleFontColor: TColor;
function JvDockGetTitleFont: TFont;

procedure JvDockLockWindow(Control: TWinControl);
procedure JvDockUnLockWindow;

function JvDockCreateNCMessage(Control: TControl; Msg: Cardinal; HTFlag: Integer; Pos: TPoint): TWMNCHitMessage;
function JvDockExchangeOrient(Orient: TDockOrientation): TDockOrientation;
function JvDockGetControlOrient(AControl: TControl): TDockOrientation;
function JvDockGetControlSize(AControl: TControl): Integer;

implementation

uses
  SysUtils, Math,
  JvDockControlForm, JvDockGlobals;

var
  JvDockTitleFont: TFont = nil;

function JvDockStreamDataToString(Stream: TStream): string;
var
  Ch: Char;
begin
  Result := '';
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(Ch, SizeOf(Ch));
    Result := Result + IntToHex(Ord(Ch), 2);
  end;
end;

procedure JvDockStringToStreamData(Stream: TStream; Data: string);
var
  I: Integer;
  Ch: Char;
begin
  I := 1;
  while I < Length(Data) do
  begin
    Ch := Char(StrToInt('$' + Copy(Data, I, 2)));
    Stream.Write(Ch, SizeOf(Ch));
    Inc(I, 2);
  end;
end;

function JvDockFindDockFormWithName(FormName: string; FromDockManager: Boolean;
  FromList: Boolean; ScanKind: TJvDockListScanKind): TCustomForm;
begin
  Result := JvDockFindDockClientFormWithName(FormName, FromDockManager, FromList, ScanKind);
  if Result = nil then
    Result := JvDockFindDockServerFormWithName(FormName, FromDockManager, FromList, ScanKind);
end;

function JvDockFindDockServerFormWithName(FormName: string; FromDockManager: Boolean;
  FromList: Boolean; ScanKind: TJvDockListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := JvDockFindDockServerFromDockManager(FormName, FromList, ScanKind)
  else
    Result := JvDockFindDockFormFromScreen(FormName, ScanKind);
end;

function JvDockFindDockClientFormWithName(FormName: string; FromDockManager: Boolean;
  FromList: Boolean; ScanKind: TJvDockListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := JvDockFindDockClientFromDockManager(FormName, FromList, ScanKind)
  else
    Result := JvDockFindDockFormFromScreen(FormName, ScanKind);
end;

function JvDockFindDockServerFromDockManager(FormName: string; FromList: Boolean;
  ScanKind: TJvDockListScanKind): TCustomForm;
var
  I: Integer;
begin
  Result := nil;
  case ScanKind of
    lskForward:
      for I := 0 to JvGlobalDockManager.DockServersList.Count - 1 do
        if FormName = TCustomForm(JvGlobalDockManager.DockServersList[I]).Name then
        begin
          Result := TCustomForm(JvGlobalDockManager.DockServersList[I]);
          Break;
        end;
    lskBackward:
       for I := JvGlobalDockManager.DockServersList.Count - 1 downto 0 do
         if FormName = TCustomForm(JvGlobalDockManager.DockServersList[I]).Name then
         begin
           Result := TCustomForm(JvGlobalDockManager.DockServersList[I]);
           Break;
         end;
  end;
end;

function JvDockFindDockClientFromDockManager(FormName: string; FromList: Boolean;
  ScanKind: TJvDockListScanKind): TCustomForm;
var
  I: Integer;
begin
  Result := nil;
  case ScanKind of
    lskForward:
      for I := 0 to JvGlobalDockManager.DockClientsList.Count - 1 do
        if FormName = TCustomForm(JvGlobalDockManager.DockClientsList[I]).Name then
        begin
          Result := TCustomForm(JvGlobalDockManager.DockClientsList[I]);
          Break;
        end;
    lskBackward:
      for I := JvGlobalDockManager.DockClientsList.Count - 1 downto 0 do
        if FormName = TCustomForm(JvGlobalDockManager.DockClientsList[I]).Name then
        begin
          Result := TCustomForm(JvGlobalDockManager.DockClientsList[I]);
          Break;
        end;
  end;
end;

function JvDockFindDockFormFromScreen(FormName: string;
  ScanKind: TJvDockListScanKind): TCustomForm;
var
  I: Integer;
begin
  Result := nil;
  case ScanKind of
    lskForward:
      for I := 0 to Screen.CustomFormCount - 1 do
        if FormName = Screen.CustomForms[I].Name then
        begin
          Result := Screen.CustomForms[I];
          Break;
        end;
    lskBackward:
      for I := Screen.CustomFormCount - 1 downto 0 do
        if FormName = Screen.CustomForms[I].Name then
        begin
          Result := Screen.CustomForms[I];
          Break;
        end;
  end;
end;

function JvDockGetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;
begin
  if (Scale < 0) or (Scale > 1) then
    Scale := 1;
  Result := Min(TBDockSize, Round(ControlSize * Scale));
end;

function JvDockGetNoNClientMetrics: TNONCLIENTMETRICS;
begin
  Result.cbSize := SizeOf(TNONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, Result.cbSize,
    @Result, 0);
end;

function JvDockGetSysCaptionHeight: Integer;
begin
  Result := JvDockGetNoNClientMetrics.iCaptionHeight
end;

function JvDockGetSysBorderWidth: Integer;
begin
  Result := JvDockGetNoNClientMetrics.iBorderWidth;
end;

function JvDockGetSysCaptionHeightAndBorderWidth: Integer;
var
  NoNCM: TNONCLIENTMETRICS;
begin
  NoNCM := JvDockGetNoNClientMetrics;
  Result := NoNCM.iBorderWidth + NoNCM.iCaptionHeight;
end;

function JvDockGetActiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_ACTIVECAPTION);
end;

function JvDockGetActiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
end;

function JvDockGetInactiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTION);
end;

function JvDockGetInactiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
end;

function JvDockGetTitleFontColor(Active: Boolean): TColor;
begin
  if Active then
    Result := JvDockGetActiveTitleFontColor
  else
    Result := JvDockGetInactiveTitleFontColor;
end;

function JvDockGetActiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_CAPTIONTEXT);
end;

function JvDockGetInactiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTIONTEXT);
end;

function JvDockGetTitleFont: TFont;
var
  NoNCM: TNONCLIENTMETRICS;
begin
  if JvDockTitleFont = nil then
    JvDockTitleFont := TFont.Create;
  Result := JvDockTitleFont;
  NoNCM := JvDockGetNoNClientMetrics;
  Result.Handle := CreateFontIndirect(NoNCM.lfCaptionFont);
end;

procedure JvDockLockWindow(Control: TWinControl);
var
  Handle: HWND;
begin
  if Control = nil then
    Handle := GetDesktopWindow
  else
    Handle := Control.Handle;
  LockWindowUpdate(Handle);
end;

procedure JvDockUnLockWindow;
begin
  LockWindowUpdate(0);
end;

function JvDockCreateNCMessage(Control: TControl; Msg: Cardinal; HTFlag: Integer;
  Pos: TPoint): TWMNCHitMessage;
begin
  Result.Msg := Msg;
  Result.HitTest := HTFlag;
  Pos := Control.ClientToScreen(Pos);
  Result.XCursor := Pos.X;
  Result.YCursor := Pos.Y;
end;

function JvDockExchangeOrient(Orient: TDockOrientation): TDockOrientation;
begin
  case Orient of
    doHorizontal:
      Result := doVertical;
    doVertical:
      Result := doHorizontal;
  else
    Result := doNoOrient;
  end;
end;

function JvDockGetControlOrient(AControl: TControl): TDockOrientation;
begin
  Assert(AControl <> nil);
  Result := doNoOrient;
  case AControl.Align of
    alClient, alNone:
      Result := doNoOrient;
    alLeft, alRight:
      Result := doVertical;
    alTop, alBottom:
      Result := doHorizontal;
  end;
end;

function JvDockGetControlSize(AControl: TControl): Integer;
begin
  case JvDockGetControlOrient(AControl) of
    doVertical:
      Result := AControl.Width;
    doHorizontal:
      Result := AControl.Height;
  else
    raise Exception.CreateRes(@RsEDockCannotGetValueWithNoOrient);
  end;
end;

initialization

finalization
  FreeAndNil(JvDockTitleFont);

end.

