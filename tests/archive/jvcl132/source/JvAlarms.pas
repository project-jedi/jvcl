{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlarms.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAlarms;

{$OBJEXPORTALL On}

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, JvComponent;

type
  TJvTriggerKind = (tkOneShot, tkEachSecond, tkEachMinute,
    tkEachHour, tkEachDay, tkEachMonth, tkEachYear);

  TJvAlarm = record
    Name: string;
    Time: TDateTime;
    Kind: TJvTriggerKind;
  end;

  TJvOnAlarm = procedure(Sender: TObject;
    const Alarm: TJvAlarm; const TriggerTime: TDateTime) of object;

  TJvAlarms = class(TJvComponent)
  private
    FActive: Boolean;
    FAlarms: array of TJvAlarm;
    FLast: TTimeStamp;
    FOnAlarm: TJvOnAlarm;
    FRunning: Boolean;
    FTimer: TTimer;
    function GetAlarm(Idx: Cardinal): TJvAlarm;
    function GetAlarmCount: Cardinal;
    procedure OnTimer(Sender: TObject);
    procedure SetActive(const Value: Boolean);
  protected
    procedure DoAlarm(const Alarm: TJvAlarm; const TriggerTime: TDateTime);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const Name: string; const Time: TDateTime; const Kind: TJvTriggerKind = tkOneShot);
    procedure Delete(const Idx: Cardinal);

    property Alarms[Idx: Cardinal]: TJvAlarm read GetAlarm;
    property Count: Cardinal read GetAlarmCount;
    property Running: Boolean read FRunning;
  published
    property Active: Boolean read FActive write SetActive default False;
    property OnAlarm: TJvOnAlarm read FOnAlarm write FOnAlarm;
  end;

implementation

resourcestring
  RC_Alarm = 'Unable to get alarm number %d. Index out of bounds';

  {*****************************************************}

constructor TJvAlarms.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FAlarms, 0);
  FActive := False;
  FRunning := False;
  FOnAlarm := nil;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := False;
  FLast := DateTimeToTimeStamp(Now);
end;

{*****************************************************}

destructor TJvAlarms.Destroy;
begin
  SetLength(FAlarms, 0);
  FTimer.Free;
  inherited Destroy;
end;

{*****************************************************}

procedure TJvAlarms.Add(const Name: string; const Time: TDateTime; const Kind: TJvTriggerKind);
begin
  SetLength(FAlarms, Count + 1);
  FAlarms[Count - 1].Name := Name;
  FAlarms[Count - 1].Time := Time;
  FAlarms[Count - 1].Kind := Kind;
  FRunning := Active;
  FTimer.Enabled := Running;
end;

{*****************************************************}

procedure TJvAlarms.Delete(const Idx: Cardinal);
begin
  if Idx < Count then
    begin
      Move(FAlarms[Idx + 1], FAlarms[Idx], (Count - Idx - 1) * SizeOf(TJvAlarm));
      SetLength(FAlarms, Count - 1);
      FRunning := (Count > 0);
      FTimer.Enabled := Running;
    end;
end;

{*****************************************************}

function TJvAlarms.GetAlarmCount: Cardinal;
begin
  Result := Length(FAlarms);
end;

{*****************************************************}

function TJvAlarms.GetAlarm(Idx: Cardinal): TJvAlarm;
begin
  if Idx < Count then
    begin
      Result.Name := FAlarms[Idx].Name;
      Result.Time := FAlarms[Idx].Time;
      Result.Kind := FAlarms[Idx].Kind;
    end
  else
    raise Exception.CreateFmt(RC_Alarm, [Idx]);
end;

{*****************************************************}

procedure TJvAlarms.DoAlarm(const Alarm: TJvAlarm; const TriggerTime: TDateTime);
begin
  if Assigned(FOnAlarm) then
    FOnAlarm(Self, Alarm, TriggerTime);
end;

{*****************************************************}

procedure TJvAlarms.OnTimer(Sender: TObject);
var
  I: Cardinal;
  Alarm: TJvAlarm;
  Current: TDateTime;
  Stamp: TTimeStamp;
  Year, Month, Day: Word;
begin
  if Count = 0 then
    Exit;
  Current := Now;
  Stamp := DateTimeToTimeStamp(Now);
  // sort out delayed Timer events which may arrive in bunches
  if (Stamp.Time - FLast.Time) >= 1000 then
    begin
      FLast := Stamp;
      for I := Count - 1 downto 0 do
        begin
          Alarm := Alarms[I];
          if Current >= Alarm.Time then
            begin
              DoAlarm(Alarm, Current);
              Stamp := DateTimeToTimeStamp(Alarm.Time);
              case Alarm.Kind of
                tkOneShot:
                  Delete(I);
                tkEachSecond:
                  Inc(Stamp.Time, 1000);
                tkEachMinute:
                  Inc(Stamp.Time, 60 * 1000);
                tkEachHour:
                  Inc(Stamp.Time, 60 * 60 * 1000);
                tkEachDay:
                  Inc(Stamp.Date);
                tkEachMonth:
                  Stamp := DateTimeToTimeStamp(IncMonth(Alarm.Time, 1));
                tkEachYear:
                  begin
                    DecodeDate(Current, Year, Month, Day);
                    if IsLeapYear(Year) then
                      Inc(Stamp.Date, 366)
                    else
                      Inc(Stamp.Date, 365);
                  end;
              end;
            end;
          if Stamp.Time > 24 * 60 * 60 * 1000 then
            begin
              Inc(Stamp.Date);
              Dec(Stamp.Time, 24 * 60 * 60 * 1000);
            end;
          if Alarm.Kind <> tkOneShot then
            FAlarms[I].Time := TimeStampToDateTime(Stamp);
        end;
    end;
end;

{*****************************************************}

procedure TJvAlarms.SetActive(const Value: Boolean);
begin
  FActive := Value;
  FRunning := (Count > 0);
  FTimer.Enabled := Running;
end;

end.

