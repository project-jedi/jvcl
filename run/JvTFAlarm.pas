{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFAlarm.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTFAlarm;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Controls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QExtCtrls,
  {$ENDIF VisualCLX}
  {$IFDEF BCB}
  JvTypes,
  {$ENDIF BCB}
  JvTFManager;

type
  TJvTFAlarm = class;

  TJvTFAlarmInfo = class(TObject)
  private
    FAppt: TJvTFAppt;
    FSnoozeMins: Integer;
    FDismiss: Boolean;
    FNextAlarmTime: TTime;
  protected
    property NextAlarmTime: TTime read FNextAlarmTime write FNextAlarmTime;
  public
    constructor Create(AAppt: TJvTFAppt); virtual;
    property Appt: TJvTFAppt read FAppt;
    property SnoozeMins: Integer read FSnoozeMins write FSnoozeMins;
    property Dismiss: Boolean read FDismiss write FDismiss;
  end;

  TJvTFAlarmList = class(TStringList)
  private
    FOwner: TJvTFAlarm;
  public
    procedure Clear; override;
    function GetAlarmForAppt(AAppt: TJvTFAppt): TJvTFAlarmInfo;
    function GetAlarmForApptID(const ID: string): TJvTFAlarmInfo;
    function IndexOfAppt(AAppt: TJvTFAppt): Integer;
    procedure AddAppt(AAppt: TJvTFAppt);
    procedure DeleteAppt(AAppt: TJvTFAppt);
    property Owner: TJvTFAlarm read FOwner write FOwner;
  end;

  TJvTFAlarmEvent = procedure(Sender: TObject; AAppt: TJvTFAppt;
    var SnoozeMins: Integer; var Dismiss: Boolean) of object;

  TJvTFAlarm = class(TJvTFComponent)
  private
    FResources: TStringList;
    FTimer: TTimer;
    FCurrentDate: TDate;
    FAlarmList: TJvTFAlarmList;
    FOnAlarm: TJvTFAlarmEvent;
    FDefaultSnoozeMins: Integer;
    function GetResources: TStrings;
    procedure SetResources(Value: TStrings);
    function GetTimerInterval: Integer;
    procedure SetTimerInterval(Value: Integer);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure InternalTimer(Sender: TObject);
  protected
    procedure DestroyApptNotification(AAppt: TJvTFAppt); override;
    procedure ConnectSchedules; virtual;
    procedure DisconnectSchedules; virtual;
    procedure TimerCheck; virtual;
    procedure AlarmCheck; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Resources: TStrings read GetResources write SetResources;
    property TimerInterval: Integer read GetTimerInterval write SetTimerInterval default 30000;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property DefaultSnoozeMins: Integer read FDefaultSnoozeMins write FDefaultSnoozeMins default 5;
    property OnAlarm: TJvTFAlarmEvent read FOnAlarm write FOnAlarm;
  end;

implementation
uses
  JvTFUtils;

//=== { TJvTFAlarm } =========================================================

constructor TJvTFAlarm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultSnoozeMins := 5;
  FCurrentDate := Date;
  FResources := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30000;
  FTimer.Enabled := True;
  FTimer.OnTimer := InternalTimer;
  FAlarmList := TJvTFAlarmList.Create;
  FAlarmList.Owner := Self;
end;

destructor TJvTFAlarm.Destroy;
begin
  DisconnectSchedules;
  FTimer.Free;
  FResources.Free;
  FAlarmList.Create;
  FAlarmList.Free;
  inherited Destroy;
end;

procedure TJvTFAlarm.Loaded;
begin
  inherited Loaded;
  ConnectSchedules;
end;

procedure TJvTFAlarm.AlarmCheck;
var
  I, J, SnoozeMins: Integer;
  Dismiss: Boolean;
  Sched: TJvTFSched;
  Appt: TJvTFAppt;
  AlarmInfo: TJvTFAlarmInfo;
  AlarmTime: TTime;
begin
  // 1. Roll through all schedules and add an alarm for each appt with a start
  //    time that is less than the current time.  (Duplicate appts will be ignored.)
  // 2. Roll through the alarm list and fire an OnAlarm event when appropriate.

  // 1.
  for I := 0 to ScheduleCount - 1 do
  begin
    Sched := Schedules[I];
    for J := 0 to Sched.ApptCount - 1 do
    begin
      Appt := Sched.Appts[J];
      AlarmTime := Appt.StartTime - Appt.AlarmAdvance * ONE_MINUTE;
      if (AlarmTime < Frac(Time)) and Appt.AlarmEnabled then
        FAlarmList.AddAppt(Appt);
    end;
  end;

  // 2.
  for I := 0 to FAlarmList.Count - 1 do
  begin
    AlarmInfo := TJvTFAlarmInfo(FAlarmList.Objects[I]);
    if not AlarmInfo.Dismiss and (AlarmInfo.NextAlarmTime < Frac(Time)) then
    begin
      SnoozeMins := AlarmInfo.SnoozeMins;
      Dismiss := False;
      if Assigned(FOnAlarm) then
      begin
        FOnAlarm(Self, AlarmInfo.Appt, SnoozeMins, Dismiss);
        AlarmInfo.SnoozeMins := SnoozeMins;
        AlarmInfo.Dismiss := Dismiss;
      end;
      AlarmInfo.NextAlarmTime := Time + SnoozeMins * ONE_MINUTE;
    end;
  end;
end;

procedure TJvTFAlarm.ConnectSchedules;
var
  I: Integer;
  CurrentSchedules: TStringList;
  Schedule: TJvTFSched;
begin
  CurrentSchedules := TStringList.Create;
  try
    FTimer.Enabled := False;
    // request all appropriate schedules.  Store in temporary list so that
    // we can release all schedules no longer needed.
    for I := 0 to Resources.Count - 1 do
    begin
      Schedule := RetrieveSchedule(Resources[I], Date);
      CurrentSchedules.AddObject('', Schedule);
    end;

    // Now release all schedules no longer needed.  (Cross check CurrentSchedules
    // against Schedules list.)
    for I := 0 to ScheduleCount - 1 do
    begin
      Schedule := Schedules[I];
      if CurrentSchedules.IndexOfObject(Schedule) = -1 then
        ReleaseSchedule(Schedule.SchedName, Schedule.SchedDate);
    end;
  finally
    CurrentSchedules.Free;
    FTimer.Enabled := True;
  end;
end;

procedure TJvTFAlarm.DestroyApptNotification(AAppt: TJvTFAppt);
begin
  FAlarmList.DeleteAppt(AAppt);
  inherited DestroyApptNotification(AAppt);
end;

procedure TJvTFAlarm.DisconnectSchedules;
begin
  ReleaseSchedules;
end;

function TJvTFAlarm.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TJvTFAlarm.GetTimerInterval: Integer;
begin
  Result := FTimer.Interval;
end;

function TJvTFAlarm.GetResources: TStrings;
begin
  Result := FResources;
end;

procedure TJvTFAlarm.InternalTimer(Sender: TObject);
begin
  if Trunc(Date) <> Trunc(FCurrentDate) then
  begin
    FCurrentDate := Date;
    ConnectSchedules;
  end;
  TimerCheck;
end;

procedure TJvTFAlarm.SetEnabled(Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

procedure TJvTFAlarm.SetResources(Value: TStrings);
begin
  FResources.Assign(Value);
  ConnectSchedules;
end;

procedure TJvTFAlarm.SetTimerInterval(Value: Integer);
begin
  FTimer.Interval := Value;
end;

procedure TJvTFAlarm.TimerCheck;
begin
  AlarmCheck;
end;

//=== { TJvTFAlarmInfo } =====================================================

constructor TJvTFAlarmInfo.Create(AAppt: TJvTFAppt);
begin
  inherited Create;
  FAppt := AAppt;
end;

//=== { TJvTFAlarmList } =====================================================

procedure TJvTFAlarmList.AddAppt(AAppt: TJvTFAppt);
var
  AlarmInfo: TJvTFAlarmInfo;
begin
  if Assigned(AAppt) and (IndexOfAppt(AAppt) = -1) then
  begin
    AlarmInfo := TJvTFAlarmInfo.Create(AAppt);
    AlarmInfo.SnoozeMins := Owner.DefaultSnoozeMins;
    AlarmInfo.NextAlarmTime := AAppt.StartTime - AAppt.AlarmAdvance * ONE_MINUTE;
    AddObject(AAppt.ID, AlarmInfo);
  end;
end;

procedure TJvTFAlarmList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
end;

procedure TJvTFAlarmList.DeleteAppt(AAppt: TJvTFAppt);
var
  I: Integer;
begin
  I := IndexOfAppt(AAppt);
  if I > -1 then
  begin
    Objects[I].Free;
    Delete(I);
  end;
end;

function TJvTFAlarmList.GetAlarmForAppt(AAppt: TJvTFAppt): TJvTFAlarmInfo;
begin
  Result := GetAlarmForApptID(AAppt.ID);
end;

function TJvTFAlarmList.GetAlarmForApptID(const ID: string): TJvTFAlarmInfo;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(ID);
  if I > -1 then
    Result := TJvTFAlarmInfo(Objects[I]);
end;

function TJvTFAlarmList.IndexOfAppt(AAppt: TJvTFAppt): Integer;
begin
  Result := IndexOf(AAppt.ID);
end;

end.

