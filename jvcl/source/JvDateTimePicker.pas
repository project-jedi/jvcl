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

Last Modified: 2002-06-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* (p3) To make NullDate and NullText maximally useful, set ParseInput to True and handle the
  OnUserInput something like this:
    if UserString = '' then
      DateAndTime := JvDateTimePicker1.NullDate;

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDateTimePicker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JVCLVer;

type
  TJvDateTimePicker = class(TDateTimePicker)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FNullText: string;
    FNullDate: TDateTime;
    FDropDownDate: TDate;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure SetNullDate(const Value: TDateTime);
  protected
    function WithinDelta(Val1, Val2: TDateTime): Boolean; virtual;
    // returns True if NullDate matches Date or frac(NullDate) matches frac(Time) depending on Kind
    function CheckNullValue: Boolean; virtual;
    procedure Change; override;
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    // The initial date to display when the drop-down calendar is shown and NullDate = Date/Time
    property DropDownDate: TDate read FDropDownDate write FDropDownDate;
    // The Date/Time (depending on the Kind property) that represents an empty "null" value, default is 1899-12-31
    property NullDate: TDateTime read FNullDate write SetNullDate;
    // The text to display when NullDate = Date/Time
    property NullText: string read FNullText write FNullText;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

uses
  CommCtrl;

resourcestring
  SNullText = '(none)';

{$IFNDEF COMPILER6_UP}

function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
begin
  try
    Value := StrToDateTime(S);
    Result := True;
  except
    Result := False;
  end;
end;
{$ENDIF}

constructor TJvDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FNullText := SNullText;
  FDropDownDate := SysUtils.Date;
end;

procedure TJvDateTimePicker.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvDateTimePicker.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FHintColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvDateTimePicker.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TJvDateTimePicker.WithinDelta(Val1, Val2: TDateTime): Boolean;
const
  cOneSecond = 1 / 86400;
begin
  Result := Abs(Frac(Val1) - Frac(Val2)) <= cOneSecond;
end;

function TJvDateTimePicker.CheckNullValue: Boolean;
begin
  Result := ((Kind = dtkDate) and (Trunc(DateTime) = Trunc(NullDate)) or
    ((Kind = dtkTime) and WithinDelta(DateTime, NullDate)));
  if Result then
    SendMessage(Handle, DTM_SETFORMAT, 0, Integer(PChar(FNullText)))
  {$IFDEF COMPILER6_UP}
  // (p3) the Format property doesn't exists in D5: what to do?
  else
    SendMessage(Handle, DTM_SETFORMAT, 0, Integer(PChar(Format)));
  {$ENDIF}
end;

procedure TJvDateTimePicker.SetNullDate(const Value: TDateTime);
begin
  FNullDate := Trunc(Value);
  CheckNullValue;
end;

function TJvDateTimePicker.MsgSetDateTime(Value: TSystemTime): Boolean;
begin
  Result := inherited MsgSetDateTime(Value);
  CheckNullValue;
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
          if CheckNullValue then
          begin
            ACal := DateTime_GetMonthCal(Handle);
            if ACal <> 0 then
            begin
              DateTimeToSystemTime(FDropDownDate, St);
              if not IsBlankSysTime(St) then
                MonthCal_SetCurSel(ACal, St);
            end;
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

