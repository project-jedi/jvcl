{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDateTimePicker.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com]

Peter Thörnqvist [peter3@peter3.com]:
* Added NullDate, NullText and DropDownDate properties
  * Bug: When TDateTImePicker is used for TIMES, it is impossible to turn
     off the NullDate feature. It should be optional! -W.Postma.

Marc Geldon [marcgeldon@web.de]
* Fixed CheckNullValue (any nonformat characters must be enclosed within single quotes!)


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* (p3) To make NullDate and NullText maximally useful, set ParseInput to True and handle the
  OnUserInput something like this:
    if UserString = '' then
      DateAndTime := JvDateTimePicker1.NullDate;

-----------------------------------------------------------------------------}
// $Id$

{$I JVCL.INC}

unit JvDateTimePicker;

interface

uses
  Windows, Messages, CommCtrl,
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JvExComCtrls, JvTypes;

type
  TJvDateTimePicker = class(TJvExDateTimePicker)
  private
    FNullText: string;
    FNullDate: TDateTime;
    FDropDownDate: TDate;
    FWeekNumbers: boolean;
    {$IFDEF COMPILER5}
    FFormat: string;
    procedure SetFormat(const Value: string);
    {$ENDIF COMPILER5}
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure SetNullDate(const Value: TDateTime);
    procedure SetNullText(const Value: string);
    procedure UpdateWeekNumbers(CalHandle: THandle);
  protected
    function WithinDelta(Val1, Val2: TDateTime): Boolean; virtual;
    // returns True if NullDate matches Date or frac(NullDate) matches frac(Time) depending on Kind
    function CheckNullValue: Boolean; overload;
    function CheckNullValue(const ANullText, AFormat: string; AKind: TDateTimeKind; ADateTime, ANullDate: TDateTime): Boolean; overload; virtual;
    procedure Change; override;
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    {$IFDEF COMPILER5}
    property Format: string read FFormat write SetFormat;
    {$ENDIF COMPILER5}
    // The initial date to display when the drop-down calendar is shown and NullDate = Date/Time
    property DropDownDate: TDate read FDropDownDate write FDropDownDate;
    // The Date/Time (depending on the Kind property) that represents an empty "null" value, default is 1899-12-31
    property NullDate: TDateTime read FNullDate write SetNullDate;
    // The text to display when NullDate = Date/Time
    property NullText: string read FNullText write SetNullText;
    property WeekNumbers: boolean read FWeekNumbers write FWeekNumbers default False;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  JvJVCLUtils, JvResources;

constructor TJvDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//   FNullText := RsNoneCaption;  XXX Don't do this unless you also set the 'default' specifier in the property declaration above! Causes problems. -WP
  FDropDownDate := SysUtils.Date;
end;

function TJvDateTimePicker.WithinDelta(Val1, Val2: TDateTime): Boolean;
const
  cOneSecond = 1 / 86400;
begin
  Result := Abs(Frac(Val1) - Frac(Val2)) <= cOneSecond;
end;

function TJvDateTimePicker.CheckNullValue: Boolean;
begin
  Result := CheckNullValue(NullText, Format, Kind, DateTime, NullDate);
end;

function TJvDateTimePicker.CheckNullValue(const ANullText, AFormat: string;
  AKind: TDateTimeKind; ADateTime, ANullDate: TDateTime): Boolean;
begin
 // Warren added NullText length check so that this feature can be disabled if not used!
  if Length(ANullText) = 0 then
  begin
    Result := false;
  end
  else
    Result := ((AKind = dtkDate) and (Trunc(ADateTime) = Trunc(ANullDate)) or
      ((AKind = dtkTime) and WithinDelta(ADateTime, ANullDate)));

  if Result then
    SendMessage(Handle, DTM_SETFORMAT, 0, Integer(PChar('''' + ANullText + '''')))
  else
    SendMessage(Handle, DTM_SETFORMAT, 0, Integer(PChar(AFormat)));
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

{$IFDEF COMPILER5}

procedure TJvDateTimePicker.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    CheckNullValue;
  end;
end;
{$ENDIF COMPILER5}

function TJvDateTimePicker.MsgSetDateTime(Value: TSystemTime): Boolean;
begin
  Result := inherited MsgSetDateTime(Value);
  CheckNullValue(NullText, Format, Kind, SystemTimeToDateTime(Value), NullDate);
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

procedure TJvDateTimePicker.UpdateWeekNumbers(CalHandle: THandle);
var
  AStyle: Cardinal;
begin
  if CalHandle <> 0 then
  begin
    AStyle := GetWindowLong(CalHandle, GWL_STYLE);
    if not WeekNumbers then
      AStyle := AStyle and not MCS_WEEKNUMBERS
    else
      AStyle := AStyle or MCS_WEEKNUMBERS;
    SetWindowLong(CalHandle, GWL_STYLE, AStyle);
  end;
end;

procedure TJvDateTimePicker.CNNotify(var Msg: TWMNotify);
var
  ACal: THandle;
  St: TSystemTime;
  Dt: TDateTime;
  AllowChange: Boolean;
begin
  with Msg, NMHdr^ do
    case code of
      DTN_DROPDOWN:
        begin
          inherited;
          ACal := DateTime_GetMonthCal(Handle);
          UpdateWeekNumbers(ACal);
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
    else
      inherited;
    end;
end;

end.

