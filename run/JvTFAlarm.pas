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

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTFAlarm;
{ Modified 1/26/2002 12:59:29 PM by the CDK, Version 5.14 Rev. E (Professional Version) }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, JvTFManager;

type
{$IFDEF BCB}
  TDate = TDateTime;
  TTime = TDateTime;
{$ENDIF}

  TJvTFAlarm = class;

  TJvTFAlarmInfo = class
  private
    FAppt: TJvTFAppt;
    FSnoozeMins: Integer;
    FDismiss: Boolean;
    FNextAlarmTime : TTime;
  protected
    property NextAlarmTime : TTime read FNextAlarmTime write FNextAlarmTime;
  public
    constructor Create(aAppt: TJvTFAppt); virtual;
    property Appt : TJvTFAppt read FAppt;
    property SnoozeMins : Integer read FSnoozeMins write FSnoozeMins;
    property Dismiss: Boolean read FDismiss write FDismiss;
  end;

  TJvTFAlarmList = class(TStringList)
  private
    FOwner: TJvTFAlarm;
  public
    procedure Clear; override;
    function GetAlarmForAppt(aAppt: TJvTFAppt): TJvTFAlarmInfo;
    function GetAlarmForApptID(ID: String): TJvTFAlarmInfo;
    function IndexOfAppt(aAppt: TJvTFAppt): Integer;
    procedure AddAppt(aAppt: TJvTFAppt);
    procedure DeleteAppt(aAppt: TJvTFAppt);
    property Owner: TJvTFAlarm read FOwner write FOwner;
  end;

  TJvTFAlarmEvent = procedure(Sender: TObject; aAppt: TJvTFAppt;
    var SnoozeMins: Integer; var Dismiss: Boolean) of object;

  TJvTFAlarm = class(TJvTFComponent)
  private
    FResources : TStrings;
    FTimer : TTimer;
    FCurrentDate : TDate;
    FAlarmList : TJvTFAlarmList;
    FOnAlarm : TJvTFAlarmEvent;
    FDefaultSnoozeMins : Integer;
    procedure SetResources(Value: TStrings);
    function GetTimerInterval: Integer;
    procedure SetTimerInterval(Value: Integer);
    function GetEnabled : Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure OnTimer(Sender: TObject);
  protected
    procedure DestroyApptNotification(anAppt: TJvTFAppt); override;
    procedure ConnectSchedules; virtual;
    procedure DisconnectSchedules; virtual;
    procedure TimerCheck; virtual;
    procedure AlarmCheck; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Resources: TStrings read FResources write SetResources;
    property TimerInterval: Integer read GetTimerInterval write SetTimerInterval default 30000;
    property Enabled : Boolean read GetEnabled write SetEnabled default True;
    property DefaultSnoozeMins : Integer read FDefaultSnoozeMins write FDefaultSnoozeMins default 5;
    property OnAlarm : TJvTFAlarmEvent read FOnAlarm write FOnAlarm;
  end;

implementation

uses JvTFUtils;

{ TJvTFAlarm }

procedure TJvTFAlarm.AlarmCheck;
var
  I,
  J,
  SnoozeMins : Integer;
  Dismiss : Boolean;
  Sched : TJvTFSched;
  Appt : TJvTFAppt;
  AlarmInfo : TJvTFAlarmInfo;
  AlarmTime : TTime;
begin
  // 1. Roll through all schedules and add an alarm for each appt with a start
  //    time that is less than the current time.  (Duplicate appts will be ignored.)
  // 2. Roll through the alarm list and fire an OnAlarm event when appropriate.

  // 1.
  For I := 0 to ScheduleCount - 1 do
    Begin
      Sched := Schedules[I];
      For J := 0 to Sched.ApptCount - 1 do
        Begin
          Appt := Sched.Appts[J];
          AlarmTime := Appt.StartTime - Appt.AlarmAdvance * ONE_MINUTE;
          If (AlarmTime < Frac(Time)) and Appt.AlarmEnabled Then
            FAlarmList.AddAppt(Appt);
        End;
    End;

  // 2.
  For I := 0 to FAlarmList.Count - 1 do
    Begin
      AlarmInfo := TJvTFAlarmInfo(FAlarmList.Objects[I]);
      If not AlarmInfo.Dismiss and (AlarmInfo.NextAlarmTime < Frac(Time)) Then
        Begin
          SnoozeMins := AlarmInfo.SnoozeMins;
          Dismiss := False;
          If Assigned(FOnAlarm) Then
            Begin
              FOnAlarm(Self, AlarmInfo.Appt, SnoozeMins, Dismiss);
              AlarmInfo.SnoozeMins := SnoozeMins;
              AlarmInfo.Dismiss := Dismiss;
            End;
          AlarmInfo.NextAlarmTime := Time + SnoozeMins * ONE_MINUTE;
        End;
    End;
end;

procedure TJvTFAlarm.ConnectSchedules;
var
  I : Integer;
  CurrentSchedules : TStringList;
  Schedule : TJvTFSched;
begin
  CurrentSchedules := TStringList.Create;

  Try
    FTimer.Enabled := False;

    // request all appropriate schedules.  Store in temporary list so that
    // we can release all schedules no longer needed.
    For I := 0 to Resources.Count - 1 do
      Begin
        Schedule := RetrieveSchedule(Resources[I], Date);
        CurrentSchedules.AddObject('', Schedule);
      End;

    // Now release all schedules no longer needed.  (Cross check CurrentSchedules
    // against Schedules list.)
    For I := 0 to ScheduleCount - 1 do
      Begin
        Schedule := Schedules[I];
        If CurrentSchedules.IndexOfObject(Schedule) = -1 Then
          ReleaseSchedule(Schedule.SchedName, Schedule.SchedDate);
      End;
  Finally
    CurrentSchedules.Free;
    FTimer.Enabled := True;
  End;
end;

constructor TJvTFAlarm.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultSnoozeMins := 5;
  FCurrentDate := Date;
  FResources := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30000;
  FTimer.Enabled := True;
  FTimer.OnTimer := OnTimer;
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
  inherited;
end;

procedure TJvTFAlarm.DestroyApptNotification(anAppt: TJvTFAppt);
begin
  FAlarmList.DeleteAppt(anAppt);
  inherited;
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

procedure TJvTFAlarm.Loaded;
begin
  inherited;
  ConnectSchedules;
end;

procedure TJvTFAlarm.OnTimer(Sender: TObject);
begin
  If Trunc(Date) <> Trunc(FCurrentDate) Then
    Begin
      FCurrentDate := Date;
      ConnectSchedules;
    End;
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

{ TJvTFAlarmInfo }
constructor TJvTFAlarmInfo.Create(aAppt: TJvTFAppt);
begin
  FAppt := aAppt;
end;

{ TJvTFAlarmList }

procedure TJvTFAlarmList.AddAppt(aAppt: TJvTFAppt);
var
  AlarmInfo : TJvTFAlarmInfo;
begin
  If Assigned(aAppt) and (IndexOfAppt(aAppt) = -1) Then
    Begin
      AlarmInfo := TJvTFAlarmInfo.Create(aAppt);
      AlarmInfo.SnoozeMins := Owner.DefaultSnoozeMins;
      AlarmInfo.NextAlarmTime := aAppt.StartTime - aAppt.AlarmAdvance * ONE_MINUTE;
      AddObject(aAppt.ID, AlarmInfo);
    End;
end;

procedure TJvTFAlarmList.Clear;
var
  I : Integer;
begin
  For I := 0 to Count - 1 do
    Objects[I].Free;
  inherited;
end;

procedure TJvTFAlarmList.DeleteAppt(aAppt: TJvTFAppt);
var
  I : Integer;
begin
  I := IndexOfAppt(aAppt);
  If I > -1 Then
    Begin
      Objects[I].Free;
      Delete(I);
    End;
end;

function TJvTFAlarmList.GetAlarmForAppt(aAppt: TJvTFAppt): TJvTFAlarmInfo;
begin
  Result := GetAlarmForApptID(aAppt.ID);
end;

function TJvTFAlarmList.GetAlarmForApptID(ID: String): TJvTFAlarmInfo;
var
  I : Integer;
begin
  Result := nil;
  I := IndexOf(ID);
  If I > -1 Then
    Result := TJvTFAlarmInfo(Objects[I]);
end;

function TJvTFAlarmList.IndexOfAppt(aAppt: TJvTFAppt): Integer;
begin
  Result := IndexOf(aAppt.ID);
end;

end.

