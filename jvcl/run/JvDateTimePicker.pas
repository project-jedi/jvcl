{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDateTimePicker.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S?stien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com]

Peter Thrnqvist [peter3 att peter3 dott com]:
* Added NullDate, NullText and DropDownDate properties
  * Bug: When TDateTImePicker is used for TIMES, it is impossible to turn
     off the NullDate feature. It should be optional! -W.Postma.

Marc Geldon [marcgeldon att web dott de]
* Fixed CheckNullValue (any nonformat characters must be enclosed within single quotes!)


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
* (p3) To make NullDate and NullText maximally useful, set ParseInput to True and handle the
  OnUserInput something like this:
    if UserString = '' then
      DateAndTime := JvDateTimePicker1.NullDate;

-----------------------------------------------------------------------------}
// $Id$

unit JvDateTimePicker;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls,
  JvExComCtrls;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDateTimePicker = class(TJvExDateTimePicker)
  private
    FNullText: string;
    FNullDate: TDateTime;
    FDropDownDate: TDate;
    FWeekNumbers: Boolean;
    FMsgSetDateTimeEmptyNullText: Boolean;
    FShowTodayCircle: Boolean;
    FShowToday: Boolean;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure SetNullDate(const Value: TDateTime);
    procedure SetNullText(const Value: string);
    procedure UpdateCalendar(CalHandle: THandle);
    function CheckNullDateEntry(const aKind: TDateTimeKind; const ADateTime, ANullDate: TDateTime): Boolean;
  protected
    function WithinDelta(Val1, Val2: TDateTime): Boolean; virtual;
    // returns True if NullDate matches Date or frac(NullDate) matches frac(Time) depending on Kind
    function CheckNullValue: Boolean; overload;
    function CheckNullValue(const ANullText, AFormat: string; AKind: TDateTimeKind; ADateTime, ANullDate: TDateTime): Boolean; overload; virtual;
    procedure Change; override;
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
    procedure CheckValidDate(Value: TDate); override;
    procedure CheckEmptyDate; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    function IsNull: Boolean;
  published
    property AutoSize;
    // The initial date to display when the drop-down calendar is shown and NullDate = Date/Time
    property DropDownDate: TDate read FDropDownDate write FDropDownDate;
    // The Date/Time (depending on the Kind property) that represents an empty "null" value, default is 1899-12-31
    property NullDate: TDateTime read FNullDate write SetNullDate;
    // The text to display when NullDate = Date/Time
    property NullText: string read FNullText write SetNullText;
    property WeekNumbers: Boolean read FWeekNumbers write FWeekNumbers default False;
    property ShowToday: Boolean read FShowToday write FShowToday default True;
    property ShowTodayCircle: Boolean read FShowTodayCircle write FShowTodayCircle default True;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  CommCtrl,
  Types,
  JclSysInfo,
  JvThemes;

{$IFNDEF COMPILER7_UP}
const
  ComCtlVersionIE6 = $00060000;
{$ENDIF !COMPILER7_UP}

procedure SetCalendarStyle(AHandle: THandle; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if AHandle <> 0 then
  begin
    Style := GetWindowLong(AHandle, GWL_STYLE);
    if UseStyle then
      Style := Style or Value
    else
      Style := Style and not Value;
    SetWindowLong(AHandle, GWL_STYLE, Style);
  end;
end;

constructor TJvDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//   FNullText := RsNoneCaption;  XXX Don't do this unless you also set the 'default' specifier in the property declaration above! Causes problems. -WP
  FDropDownDate := SysUtils.Date;
  FShowToday := True;
  FShowTodayCircle := True;
end;

function TJvDateTimePicker.IsNull: Boolean;
begin
  Result := (DateTime = NullDate) and (FNullText <> '');
end;

function TJvDateTimePicker.WithinDelta(Val1, Val2: TDateTime): Boolean;
begin
  Result := Abs(Frac(Val1) - Frac(Val2)) < EncodeTime(0, 0, 1, 0);
end;

function TJvDateTimePicker.CheckNullValue: Boolean;
begin
  Result := CheckNullValue(FNullText, Format, Kind, DateTime, NullDate);
end;

procedure TJvDateTimePicker.CheckEmptyDate;
begin
  // Don't throw the EDateTimeError if Date=0.0 and ShowCheckBox=False (Mantis #4651).
  // The VCL has some strange behavior here. Why is 1899-12-30 not allowed. The Windows
  // Control supports it, but TDateTimePicker doesn't.
  // TDateTimePicker.SetTime() can't be "fixed" because it doesn't call CheckEmptyDate() but
  // has the exact same code inline.
  if not ShowCheckbox then
  begin
    Checked := False;
    Invalidate;
  end
  else
    inherited CheckEmptyDate;
end;

function TJvDateTimePicker.CheckNullDateEntry(Const aKind: TDateTimeKind;
     Const ADateTime, ANullDate: TDateTime) : Boolean;
begin
  case aKind of
    dtkDate :
      Result := (Trunc(ADateTime) = Trunc(ANullDate));
    dtkTime :
      Result := WithinDelta(ADateTime, ANullDate);
    else
      raise Exception.CreateFmt('Unsupported datetime kind: %d', [Ord(aKind)]);
  end;
end;

function TJvDateTimePicker.CheckNullValue(const ANullText, AFormat: string;
  AKind: TDateTimeKind; ADateTime, ANullDate: TDateTime): Boolean;
begin
  // Warren added NullText length check so that this feature can be disabled if not used!
  if ANullText = '' then
    Result := False
  else
    Result := CheckNullDateEntry(aKind, aDateTime, aNullDate);

  if Result then
    SendMessage(Handle, DTM_SETFORMAT, 0, LPARAM(PChar('''' + ANullText + '''')))
  else
    SendMessage(Handle, DTM_SETFORMAT, 0, LPARAM(PChar(AFormat)));
end;

procedure TJvDateTimePicker.CheckValidDate(Value: TDate);
begin
  if Value <> NullDate then
    inherited CheckValidDate(Value);
end;

procedure TJvDateTimePicker.Clear;
begin
  DateTime := NullDate;
  Checked := False;
end;

procedure TJvDateTimePicker.SetNullDate(const Value: TDateTime);
begin
  FNullDate := Trunc(Value);
  CheckNullValue;
end;

procedure TJvDateTimePicker.SetNullText(const Value: string);
begin
  if FNullText <> Value then
  begin
    FNullText := Value;
    CheckNullValue;
  end;
end;

function TJvDateTimePicker.MsgSetDateTime(Value: TSystemTime): Boolean;
var
  LNullText: string;
begin
  Result := inherited MsgSetDateTime(Value);
  
  if not Result then
    Result := CheckNullDateEntry(Kind, SystemTimeToDateTime(Value), NullDate);

  if FMsgSetDateTimeEmptyNullText then
    LNullText := ''
  else
    LNullText := FNullText;
  CheckNullValue(LNullText, Format, Kind, SystemTimeToDateTime(Value), NullDate);
end;

procedure TJvDateTimePicker.Change;
begin
  inherited Change;
  CheckNullValue;
end;

function IsBlankSysTime(const St: TSystemTime): Boolean;
begin
  with St do
    Result := (wYear = 0) and (wMonth = 0) and
      (wDayOfWeek = 0) and (wDay = 0) and
      (wHour = 0) and (wMinute = 0) and
      (wSecond = 0) and (wMilliseconds = 0);
end;

function IsWinVista_UP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and JclCheckWinVersion(6, 0);
end;

procedure TJvDateTimePicker.UpdateCalendar(CalHandle: THandle);
var
  R, WindowRect: TRect;
  MinWidth, MinHeight, MaxTodayWidth: Integer;
  SizeHandle: THandle;
begin
  if CalHandle <> 0 then
  begin
    SetCalendarStyle(CalHandle,MCS_WEEKNUMBERS, WeekNumbers);
    SetCalendarStyle(CalHandle,MCS_NOTODAY, not ShowToday);
    SetCalendarStyle(CalHandle,MCS_NOTODAYCIRCLE, not ShowTodayCircle);

    MonthCal_GetMinReqRect(CalHandle, R);
    with R do
    begin
      MinHeight := Bottom - Top;
      MinWidth := Right - Left;
    end;
    MaxTodayWidth := MonthCal_GetMaxTodayWidth(CalHandle);
    if MinWidth < MaxTodayWidth then MinWidth := MaxTodayWidth;
    if IsWinVista_UP and (GetComCtlVersion >= ComCtlVersionIE6) then
    begin
      // On Vista the popup month calendar has a parent window that we must resize
      SizeHandle := GetParent(CalHandle);
      // The dropdown window uses a 'border' of..
      {$IFDEF JVCLThemesEnabled}
      if StyleServices.Enabled then
      begin
        // .. 3 pixels when themed
        Inc(MinWidth, 3*2);
        Inc(MinHeight, 3*2);
      end
      else
      {$ENDIF JVCLThemesEnabled}
      begin
        // .. otherwise 5 pixels
        Inc(MinWidth, 5*2);
        Inc(MinHeight, 5*2);
      end;
    end
    else
      SizeHandle := CalHandle;
    GetWindowRect(SizeHandle, WindowRect);
    MoveWindow(SizeHandle, WindowRect.Left, WindowRect.Top, MinWidth, MinHeight, False);
  end;
end;

procedure TJvDateTimePicker.CNNotify(var Msg: TWMNotify);
var
  ACal: THandle;
  St: TSystemTime;
  Dt: TDateTime;
  AllowChange: Boolean;
  WasMsgEmptyNullText: Boolean;
begin
  with Msg, NMHdr^ do
    case code of
      DTN_DROPDOWN:
        begin
          inherited;
          ACal := DateTime_GetMonthCal(Handle);
          UpdateCalendar(ACal);
          if CheckNullValue and (ACal <> 0) then
          begin
            DateTimeToSystemTime(FDropDownDate, St);
            if not IsBlankSysTime(St) then
              MonthCal_SetCurSel(ACal, St);
          end;
        end;
      DTN_USERSTRING:
        begin
          with PNMDateTimeString(NMHdr)^ do
          begin
            if not TryStrToDateTime(pszUserString, Dt) then
              Dt := NullDate;
            if Assigned(OnUserInput) then
            begin
              AllowChange := True;
              OnUserInput(Self, pszUserString, Dt, AllowChange);
              dwFlags := Ord(not AllowChange);
            end
            else
              dwFlags := Ord(False);
            DateTimeToSystemTime(Dt, St);
          end;
        end;
      DTN_CLOSEUP:
        begin
          // We need to use the NullText in MsgSetDateTime() because if the user clicked outside
          // the popup calendar and the old value was NullValue, the NullValue date would be
          // displayed instead of the NullText. And we don't want that to happen.
          // Alternatively we could call CheckNullValue() after "inherited". But why do the check
          // twice with the possibility of a short time span where the NullDate is visible.
          WasMsgEmptyNullText := FMsgSetDateTimeEmptyNullText;
          FMsgSetDateTimeEmptyNullText := False;
          try
            inherited;
          finally
            FMsgSetDateTimeEmptyNullText := WasMsgEmptyNullText;
          end;
        end;
      NM_SETFOCUS:
        begin
          FMsgSetDateTimeEmptyNullText := True;
          inherited;
        end;
      NM_KILLFOCUS:
        begin
          FMsgSetDateTimeEmptyNullText := False;
          inherited;
        end;
    else
      inherited;
    end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
