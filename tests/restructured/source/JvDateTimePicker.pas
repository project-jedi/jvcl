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
* (p3) To make NullDate and NullText maximally useful, set ParseInput to true and handle the
  OnUserInput something like this:
    if UserString = '' then
      DateAndTime := JvDateTimePicker1.NullDate;

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDateTimePicker;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, JVCLVer;

type
  TJvDateTimePicker = class(TDateTimePicker)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FNullText: string;
    FNullDate: TDateTime;
    FDropDownDate: TDate;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;

    procedure SetNullDate(const Value: TDateTime);
  protected
    function WithinDelta(Val1, Val2: TDateTime): boolean; virtual;
    // returns true if NullDate matches Date or frac(NullDate) matches frac(Time) depending on Kind
    function CheckNullValue: boolean; virtual;
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
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

resourcestring
  SNullText = '(none)';

implementation
uses
  CommCtrl;

{$IFNDEF COMPILER6_UP}
function TryStrToDateTime(const S:String;out Value:TDateTime):boolean;
begin
  try
    Value := StrToDateTime(S);
  except
    Result := false;
  end;
end;
{$ENDIF}

{*****************************************************************}

constructor TJvDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FNullText := SNullText;
  FDropDownDate := SysUtils.Date;
end;

{**************************************************}

procedure TJvDateTimePicker.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvDateTimePicker.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvDateTimePicker.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TJvDateTimePicker.WithinDelta(Val1, Val2: TDateTime): boolean;
const
  cOneSecond = 1 / 86400;
begin
  Result := Abs(frac(Val1) - frac(Val2)) <= cOneSecond;
end;

function TJvDateTimePicker.CheckNullValue: boolean;
begin
  Result := ((Kind = dtkDate) and (trunc(DateTime) = trunc(NullDate))
    or ((Kind = dtkTime) and WithinDelta(DateTime, NullDate)));
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
  FNullDate := trunc(Value);
  CheckNullValue;
end;

function TJvDateTimePicker.MsgSetDateTime(Value: TSystemTime): Boolean;
begin
  Result := inherited MsgSetDateTime(Value);
  CheckNullValue;
end;

procedure TJvDateTimePicker.Change;
begin
  inherited;
  CheckNullValue;
end;

function IsBlankSysTime(const ST: TSystemTime): Boolean;
type
  TFast = array[0..3] of DWORD;
begin
  Result := (TFast(ST)[0] or TFast(ST)[1] or TFast(ST)[2] or TFast(ST)[3]) = 0;
end;

procedure TJvDateTimePicker.CNNotify(var Message: TWMNotify);
var aCal: THandle; st: TSystemTime; DT: TDateTime; AllowChange: boolean;
begin
  with Message, NMHdr^ do
    case code of
      DTN_DROPDOWN:
        begin
          inherited;
          if CheckNullValue then
          begin
            aCal := DateTime_GetMonthCal(Handle);
            if aCal <> 0 then
            begin
              DateTimeToSystemTime(FDropDownDate, st);
              if not IsBlankSysTime(st) then
                MonthCal_SetCurSel(aCal, st);
            end;
          end;
        end;
      DTN_USERSTRING:
        begin
          with PNMDateTimeString(NMHdr)^ do
          begin

            if not TryStrToDateTime(pszUserString, DT) then
              DT := NullDate;
            if Assigned(OnUserInput) then
            begin
              AllowChange := True;
              OnUserInput(Self, pszUserString, DT, AllowChange);
              dwFlags := Ord(not AllowChange);
            end
            else
              dwFlags := Ord(False);
            DateTimeToSystemTime(DT, st);
          end;
        end;
    else
      inherited;
    end;
end;

end.

