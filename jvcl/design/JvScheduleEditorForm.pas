{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScheduleEditorForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvScheduleEditorForm;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, AppEvnts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QStdCtrls, QComCtrls, QExtCtrls, Types,
  {$ENDIF VisualCLX}
  JclSchedule;

type
  TFrmScheduleEditor = class(TForm)
    pnlStartInfo: TPanel;
    lblStartDate: TLabel;
    lblStartTime: TLabel;
    lblStartCaption: TLabel;
    dtpStartDate: TDateTimePicker;
    dtpStartTime: TDateTimePicker;
    pnlEndInfo: TPanel;
    lblEndCaption: TLabel;
    rbInfinite: TRadioButton;
    rbTriggerCount: TRadioButton;
    rbDayCount: TRadioButton;
    rbDate: TRadioButton;
    lblEndTime: TLabel;
    dtpEndDate: TDateTimePicker;
    dtpEndTime: TDateTimePicker;
    edEventCount: TEdit;
    edDayCount: TEdit;
    pnlRecurringInfo: TPanel;
    lblScheduleType: TLabel;
    rbSingleShot: TRadioButton;
    rbDaily: TRadioButton;
    rbWeekly: TRadioButton;
    rbMonthly: TRadioButton;
    rbYearly: TRadioButton;
    bvlSeparation: TBevel;
    pnlDailySchedule: TPanel;
    lblDailyCaption: TLabel;
    rbDailyEveryWeekDay: TRadioButton;
    rbDailyInterval: TRadioButton;
    edDailyInterval: TEdit;
    lblDailyIntervalUnit: TLabel;
    pnlWeeklySchedule: TPanel;
    lblWeeklyCaption: TLabel;
    lblWeeklyInterval: TLabel;
    edWeeklyInterval: TEdit;
    lblWeeklyInterval2: TLabel;
    cbWeeklyMon: TCheckBox;
    cbWeeklyTue: TCheckBox;
    cbWeeklyWed: TCheckBox;
    cbWeeklyThu: TCheckBox;
    cbWeeklyFri: TCheckBox;
    cbWeeklySat: TCheckBox;
    cbWeeklySun: TCheckBox;
    pnlMonthlySchedule: TPanel;
    lblMonthlyCaption: TLabel;
    rbMonthlyDay: TRadioButton;
    edMonthlyEveryMonth: TEdit;
    lblMonthlyDayIntervalSuffix: TLabel;
    rbMonthlyEveryIndex: TRadioButton;
    cbMonthlyIndexValue: TComboBox;
    cbMonthlyIndexType: TComboBox;
    edMonthlyDay: TEdit;
    lblMontlhyDayInterval: TLabel;
    lblMonthlyIndexInterval: TLabel;
    edMonthlyIndexInterval: TEdit;
    lblMonthlyIndexIntervalSuffix: TLabel;
    bvlScheduleType: TBevel;
    pnlYearlySchedule: TPanel;
    lblYearlyCaption: TLabel;
    lblYearlyIntervalSuffix: TLabel;
    lblYearlyDateOf: TLabel;
    lblYearlyIndexInterval: TLabel;
    lblYearlyIndexIntervalSuffix: TLabel;
    rbYearlyDate: TRadioButton;
    edYearlyDateInterval: TEdit;
    rbYearlyIndex: TRadioButton;
    cbYearlyIndexValue: TComboBox;
    cbYearlyIndexKind: TComboBox;
    edYearlyDateDay: TEdit;
    edYearlyIndexInterval: TEdit;
    cbYearlyDateMonth: TComboBox;
    lblYearlyDateInterval: TLabel;
    lblYearlyIndexMonth: TLabel;
    cbYearlyIndexMonth: TComboBox;
    bvlDailyFreq: TBevel;
    pnlDailyFreq: TPanel;
    lblDailyFreq: TLabel;
    rbFreqOneshot: TRadioButton;
    dtpDayFreqOneshot: TDateTimePicker;
    rbFreqInterval: TRadioButton;
    edFreqInterval: TEdit;
    cbFreqIntervalUnit: TComboBox;
    lblFreqFrom: TLabel;
    dtpFreqFrom: TDateTimePicker;
    lblFreqTo: TLabel;
    dtpFreqTo: TDateTimePicker;
    Bevel1: TBevel;
    Bevel2: TBevel;
    AppEvents: TApplicationEvents;
    gbTestSettings: TGroupBox;
    cxStartToday: TCheckBox;
    cxCountMissedEvents: TCheckBox;
    btnTest: TButton;
    mmLog: TMemo;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure btnOkClick(Sender: TObject);
  private
    FTestSchedule: IJclSchedule;
    FSchedule: IJclSchedule;
    FBusy: Boolean;
    procedure SetSchedule(Value: IJclSchedule);
    procedure SelectRecurringInfoPage;
    procedure UpdateDailyPageInfo;
    procedure UpdateWeeklyPageInfo;
    procedure UpdateMonthlyPageInfo;
    procedure UpdateYearlyPageInfo;
    procedure UpdateFrequencyPageInfo;
    procedure UpdateEndPageInfo;
    procedure UpdateTestSettings;
    procedure InitSchedule(const ASchedule: IJclSchedule);
    procedure ScheduleToUI(const ASchedule: IJclSchedule);
  public
    property Schedule: IJclSchedule read FSchedule write SetSchedule;
  end;

implementation

uses
  JclDateTime,
  JvDsgnConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure DecodeTimeStampTime(const Stamp: TTimeStamp;
  var ADays, AHour, AMinute, ASecond, AMSec: Word);
var
  TempTime: Integer;
begin
  TempTime := Stamp.Time;
  AMSec := TempTime mod 1000;
  TempTime := TempTime div 1000;
  ASecond := TempTime mod 60;
  TempTime := TempTime div 60;
  AMinute := TempTime mod 60;
  TempTime := TempTime div 60;
  AHour := TempTime mod 24;
  TempTime := TempTime div 24;
  ADays := TempTime;
end;

function IndexOfRBChecked(const Controls: array of TRadioButton): Integer;
begin
  Result := High(Controls);
  while (Result >= 0) and not Controls[Result].Checked do
    Dec(Result);
end;

procedure TFrmScheduleEditor.SetSchedule(Value: IJclSchedule);
begin
  FSchedule := Value;
  ScheduleToUI(FSchedule);
end;

procedure TFrmScheduleEditor.SelectRecurringInfoPage;
begin
  pnlDailySchedule.Visible := rbDaily.Checked;
  pnlWeeklySchedule.Visible := rbWeekly.Checked;
  pnlMonthlySchedule.Visible := rbMonthly.Checked;
  pnlYearlySchedule.Visible := rbYearly.Checked;
end;

procedure TFrmScheduleEditor.UpdateDailyPageInfo;
begin
  edDailyInterval.Enabled := rbDailyInterval.Checked;
end;

procedure TFrmScheduleEditor.UpdateWeeklyPageInfo;
begin
end;

procedure TFrmScheduleEditor.UpdateMonthlyPageInfo;
begin
  edMonthlyDay.Enabled := rbMonthlyDay.Checked;
  edMonthlyEveryMonth.Enabled := rbMonthlyDay.Checked;

  cbMonthlyIndexValue.Enabled := rbMonthlyEveryIndex.Checked;
  cbMonthlyIndexType.Enabled := rbMonthlyEveryIndex.Checked;
  edMonthlyIndexInterval.Enabled := rbMonthlyEveryIndex.Checked;
end;

procedure TFrmScheduleEditor.UpdateYearlyPageInfo;
begin
  edYearlyDateDay.Enabled := rbYearlyDate.Checked;
  cbYearlyDateMonth.Enabled := rbYearlyDate.Checked;
  edYearlyDateInterval.Enabled := rbYearlyDate.Checked;

  cbYearlyIndexValue.Enabled := rbYearlyIndex.Checked;
  cbYearlyIndexKind.Enabled := rbYearlyIndex.Checked;
  cbYearlyIndexMonth.Enabled := rbYearlyIndex.Checked;
  edYearlyIndexInterval.Enabled := rbYearlyIndex.Checked;
end;

procedure TFrmScheduleEditor.UpdateFrequencyPageInfo;
begin
  pnlDailyFreq.Visible := not rbSingleShot.Checked;

  dtpDayFreqOneshot.Enabled := rbFreqOneshot.Checked;

  edFreqInterval.Enabled := rbFreqInterval.Checked;
  cbFreqIntervalUnit.Enabled := rbFreqInterval.Checked;
  dtpFreqFrom.Enabled := rbFreqInterval.Checked;
  dtpFreqTo.Enabled := rbFreqInterval.Checked;
end;

procedure TFrmScheduleEditor.UpdateEndPageInfo;
begin
  pnlEndInfo.Visible := not rbSingleShot.Checked;

  edEventCount.Enabled := rbTriggerCount.Checked;

  edDayCount.Enabled := rbDayCount.Checked;

  dtpEndDate.Enabled := rbDate.Checked;
  dtpEndTime.Enabled := rbDate.Checked;
end;

procedure TFrmScheduleEditor.UpdateTestSettings;
begin
  cxCountMissedEvents.Enabled := cxStartToday.Checked;
end;

procedure TFrmScheduleEditor.InitSchedule(const ASchedule: IJclSchedule);
var
  TempDOW: TScheduleWeekDays;
begin
  with ASchedule do
  begin
    RecurringType := TScheduleRecurringKind(IndexOfRBChecked([rbSingleShot, rbDaily, rbWeekly,
      rbMonthly, rbYearly]));
    StartDate := DateTimeToTimeStamp(Trunc(dtpStartDate.Date) + Frac(dtpStartTime.Time));
    EndType := TScheduleEndKind(IndexOfRBChecked([rbInfinite, rbDate, rbTriggerCount, rbDayCount]));
    if RecurringType = srkOneShot then
    begin
      EndType := sekDate;
      EndDate := StartDate;
      with ASchedule as IJclScheduleDayFrequency do
      begin
        StartTime := StartDate.Time;
        EndTime := EndDate.Time;
        Interval := 1;
      end;
    end
    else
    begin
      case RecurringType of
        srkDaily:
          begin
            with ASchedule as IJclDailySchedule do
            begin
              EveryWeekDay := rbDailyEveryWeekDay.Checked;
              if not EveryWeekDay then
                Interval := StrToInt64(edDailyInterval.Text);
            end;
          end;
        srkWeekly:
          begin
            with ASchedule as IJclWeeklySchedule do
            begin
              TempDOW := [];
              if cbWeeklyMon.Checked then
                Include(TempDOW, swdMonday);
              if cbWeeklyTue.Checked then
                Include(TempDOW, swdTuesday);
              if cbWeeklyWed.Checked then
                Include(TempDOW, swdWednesday);
              if cbWeeklyThu.Checked then
                Include(TempDOW, swdThursday);
              if cbWeeklyFri.Checked then
                Include(TempDOW, swdFriday);
              if cbWeeklySat.Checked then
                Include(TempDOW, swdSaturday);
              if cbWeeklySun.Checked then
                Include(TempDOW, swdSunday);
              DaysOfWeek := TempDOW;
              Interval := StrToInt64(edWeeklyInterval.Text);
            end;
          end;
        srkMonthly:
          begin
            with ASchedule as IJclMonthlySchedule do
            begin
              if rbMonthlyDay.Checked then
              begin
                IndexKind := sikNone;
                Day := StrToInt64(edMonthlyDay.Text);
                Interval := StrToInt64(edMonthlyEveryMonth.Text);
              end
              else
              begin
                IndexKind := TScheduleIndexKind(cbMonthlyIndexType.ItemIndex + 1);
                if cbMonthlyIndexValue.ItemIndex > -1 then
                begin
                  if cbMonthlyIndexValue.ItemIndex < 4 then
                    IndexValue := cbMonthlyIndexValue.ItemIndex + 1
                  else
                    IndexValue := sivLast;
                end
                else
                  IndexValue := StrToInt64(cbMonthlyIndexValue.Text);
                Interval := StrToInt64(edMonthlyIndexInterval.Text);
              end;
            end;
          end;
        srkYearly:
          begin
            with ASchedule as IJclYearlySchedule do
            begin
              if rbYearlyDate.Checked then
              begin
                IndexKind := sikNone;
                Day := StrToInt64(edYearlyDateDay.Text);
                Month := cbYearlyDateMonth.ItemIndex + 1;
                Interval := StrToInt64(edYearlyDateInterval.Text);
              end
              else
              begin
                IndexKind := TScheduleIndexKind(cbYearlyIndexKind.ItemIndex + 1);
                if cbYearlyIndexValue.ItemIndex > -1 then
                begin
                  if cbYearlyIndexValue.ItemIndex < 4 then
                    IndexValue := cbYearlyIndexValue.ItemIndex + 1
                  else
                    IndexValue := sivLast;
                end
                else
                  IndexValue := StrToInt64(cbYearlyIndexValue.Text);
                Month := cbYearlyIndexMonth.ItemIndex + 1;
                Interval := StrToInt64(edYearlyIndexInterval.Text);
              end;
            end;
          end;
      end;

      with ASchedule as IJclScheduleDayFrequency do
      begin
        if rbFreqOneshot.Checked then
        begin
          StartTime := DateTimeToTimeStamp(dtpDayFreqOneshot.Time).Time;
          EndTime := StartTime;
          Interval := 1;
        end
        else
        begin
          StartTime := DateTimeToTimeStamp(dtpFreqFrom.Time).Time;
          EndTime := DateTimeToTimeStamp(dtpFreqTo.Time).Time;
          case cbFreqIntervalUnit.ItemIndex of
            0: { Milliseconds }
              Interval := StrToInt64(edFreqInterval.Text);
            1: { Seconds }
              Interval := 1000 * StrToInt64(edFreqInterval.Text);
            2: { Minutes }
              Interval := 60 * 1000 * StrToInt64(edFreqInterval.Text);
            3: { Hours }
              Interval := 60 * 60 * 1000 * StrToInt64(edFreqInterval.Text);
          end;
        end;
      end;

      case EndType of
        sekDate:
          EndDate := DateTimeToTimeStamp(Trunc(dtpEndDate.Date) + Frac(dtpEndTime.Time));
        sekTriggerCount:
          EndCount := StrToInt64(edEventCount.Text);
        sekDayCount:
          EndCount := StrToInt64(edDayCount.Text);
      end;
    end;
    Reset;
  end;
end;

procedure TFrmScheduleEditor.ScheduleToUI(const ASchedule: IJclSchedule);
var
  TempStamp: TTimeStamp;
begin
  with ASchedule do
  begin
    dtpStartDate.Date := Trunc(TimeStampToDateTime(StartDate));
    dtpStartTime.Time := Frac(TimeStampToDateTime(StartDate));

    case RecurringType of
      srkOneShot:
        rbSingleShot.Checked := True;
      srkDaily:
        begin
          rbDaily.Checked := True;
          with ASchedule as IJclDailySchedule do
          begin
            if EveryWeekDay then
              rbDailyEveryWeekDay.Checked := True
            else
            begin
              rbDailyInterval.Checked := True;
              edDailyInterval.Text := IntToStr(Interval);
            end;
          end;
        end;
      srkWeekly:
        begin
          rbWeekly.Checked := True;
          with ASchedule as IJclWeeklySchedule do
          begin
            cbWeeklyMon.Checked := swdMonday in DaysOfWeek;
            cbWeeklyTue.Checked := swdTuesday in DaysOfWeek;
            cbWeeklyWed.Checked := swdWednesday in DaysOfWeek;
            cbWeeklyThu.Checked := swdThursday in DaysOfWeek;
            cbWeeklyFri.Checked := swdFriday in DaysOfWeek;
            cbWeeklySat.Checked := swdSaturday in DaysOfWeek;
            cbWeeklySun.Checked := swdSunday in DaysOfWeek;
            edWeeklyInterval.Text := IntToStr(Interval);
          end
        end;
      srkMonthly:
        begin
          rbMonthly.Checked := True;
          with ASchedule as IJclMonthlySchedule do
          begin
            case IndexKind of
              sikNone:
                begin
                  rbMonthlyDay.Checked := True;
                  edMonthlyDay.Text := IntToStr(Day);
                  edMonthlyEveryMonth.Text := IntToStr(Interval);
                end;
              sikDay, sikWeekDay, sikWeekendDay, sikMonday, sikTuesday,
              sikWednesday, sikThursday, sikFriday, sikSaturday,
              sikSunday:
                begin
                  rbMonthlyEveryIndex.Checked := True;
                  if (IndexValue > 0) and (IndexValue < 5) then
                    cbMonthlyIndexValue.ItemIndex := IndexValue - 1
                  else
                  if IndexValue = sivLast then
                    cbMonthlyIndexValue.ItemIndex := 4
                  else
                  begin
                    cbMonthlyIndexValue.ItemIndex := -1;
                    cbMonthlyIndexValue.Text := IntToStr(IndexValue);
                  end;
                  cbMonthlyIndexType.ItemIndex := Ord(IndexKind) - 1;
                  edMonthlyIndexInterval.Text := IntToStr(Interval);
                end;
            else
              raise ESchedule.CreateRes(@RsEInvalidScheduleSettingsFound);
            end;
          end;
        end;
      srkYearly:
        begin
          rbYearly.Checked := True;
          with ASchedule as IJclYearlySchedule do
          begin
            case IndexKind of
              sikNone:
                begin
                  rbYearlyDate.Checked := True;
                  edYearlyDateDay.Text := IntToStr(Day);
                  cbYearlyDateMonth.ItemIndex := Month - 1;
                  edYearlyDateInterval.Text := IntToStr(Interval);
                end;
              sikDay, sikWeekDay, sikWeekendDay, sikMonday, sikTuesday,
                sikWednesday, sikThursday, sikFriday, sikSaturday,
                sikSunday:
                begin
                  rbYearlyIndex.Checked := True;
                  if (IndexValue > 0) and (IndexValue < 5) then
                    cbYearlyIndexValue.ItemIndex := IndexValue - 1
                  else
                  if IndexValue = sivLast then
                    cbYearlyIndexValue.ItemIndex := 4
                  else
                  begin
                    cbYearlyIndexValue.ItemIndex := -1;
                    cbYearlyIndexValue.Text := IntToStr(IndexValue);
                  end;
                  cbYearlyIndexKind.ItemIndex := Ord(IndexKind) - 1;
                  cbYearlyIndexMonth.ItemIndex := Month - 1;
                  edYearlyIndexInterval.Text := IntToStr(Interval);
                end;
            else
              raise ESchedule.CreateRes(@RsEInvalidScheduleSettingsFound);
            end;
          end;
        end;
    end;
    case EndType of
      sekNone:
        rbInfinite.Checked := True;
      sekDate:
        begin
          rbDate.Checked := True;
          dtpEndDate.Date := Trunc(TimeStampToDateTime(EndDate));
          dtpEndTime.Time := Frac(TimeStampToDateTime(EndDate));
        end;
      sekTriggerCount:
        begin
          rbTriggerCount.Checked := True;
          edEventCount.Text := IntToStr(EndCount);
        end;
      sekDayCount:
        begin
          rbDayCount.Checked := True;
          edDayCount.Text := IntToStr(EndCount);
        end;
    end;
    if RecurringType <> srkOneShot then
    begin
      with ASchedule as IJclScheduleDayFrequency do
      begin
        rbFreqOneshot.Checked := StartTime = EndTime;
        rbFreqInterval.Checked := StartTime <> EndTime;
        if rbFreqOneshot.Checked then
        begin
          TempStamp := DateTimeToTimeStamp(Now);
          TempStamp.Time := StartTime;
          dtpDayFreqOneshot.Time := TimeStampToDateTime(TempStamp);
        end
        else
        begin
          TempStamp := DateTimeToTimeStamp(Now);
          TempStamp.Time := StartTime;
          dtpFreqFrom.Time := TimeStampToDateTime(TempStamp);
          TempStamp.Time := EndTime;
          dtpFreqTo.Time := TimeStampToDateTime(TempStamp);

          if Interval mod (60 * 60 * 1000) = 0 then
          begin
            cbFreqIntervalUnit.ItemIndex := 3;
            edFreqInterval.Text := IntToStr(Interval div (60 * 60 * 1000));
          end
          else
          if Interval mod (60 * 1000) = 0 then
          begin
            cbFreqIntervalUnit.ItemIndex := 2;
            edFreqInterval.Text := IntToStr(Interval div (60 * 1000));
          end
          else
          if Interval mod 1000 = 0 then
          begin
            cbFreqIntervalUnit.ItemIndex := 1;
            edFreqInterval.Text := IntToStr(Interval div 1000);
          end
          else
          begin
            cbFreqIntervalUnit.ItemIndex := 0;
            edFreqInterval.Text := IntToStr(Interval);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFrmScheduleEditor.FormCreate(Sender: TObject);
const
  cTimeFormat = 'HH:mm:ss';
  cDateFormat = 'dd-MM-yyyy';
begin
  FTestSchedule := CreateSchedule;
  {$IFDEF COMPILER6_UP}
  dtpStartDate.Format := cDateFormat;
  dtpStartTime.Format := cTimeFormat;
  dtpEndDate.Format := cDateFormat;
  dtpEndTime.Format := cTimeFormat;
  dtpDayFreqOneshot.Format := cTimeFormat;
  dtpFreqFrom.Format := cTimeFormat;
  dtpFreqTo.Format := cTimeFormat;
  {$ELSE}
  dtpStartDate.DateFormat := dfShort;
  dtpEndDate.DateFormat := dfShort;
  {$ENDIF COMPILER6_UP}
  dtpStartDate.DateTime := Now;
  dtpEndDate.DateTime := Now;

  cbMonthlyIndexValue.ItemIndex := 0;
  cbMonthlyIndexType.ItemIndex := 1;
  cbYearlyIndexValue.ItemIndex := 0;
  cbYearlyIndexKind.ItemIndex := 1;
  cbYearlyDateMonth.ItemIndex := 0;
  cbYearlyIndexMonth.ItemIndex := 0;
  cbFreqIntervalUnit.ItemIndex := 2;
end;

procedure TFrmScheduleEditor.btnTestClick(Sender: TObject);
var
  Stamp: TTimeStamp;
  AYear, AMonth, ADay: Word;
  ADays, AHour, AMinute, ASecond, AMSec: Word;
begin
  if FBusy then
    FBusy := False
  else
  begin
    try
      FBusy := True;
      btnTest.Caption := RsStop;
      btnOk.Enabled := False;
      btnCancel.Enabled := False;
      InitSchedule(FTestSchedule);
      mmLog.Lines.Clear;
      if cxStartToday.Checked then
        Stamp := FTestSchedule.NextEventFromNow(cxCountMissedEvents.Checked)
      else
        Stamp := FTestSchedule.NextEventFrom(FTestSchedule.StartDate, True);
      while (Stamp.Date > 0) and FBusy do
      begin
        JclDateTime.DecodeDate(TimeStampToDateTime(Stamp), AYear, AMonth, ADay);
        DecodeTimeStampTime(Stamp, ADays, AHour, AMinute, ASecond, AMSec);
        mmLog.Lines.Add(Format('%.5d (%.4d): %.2d-%.2d-%.4d@%.2d:%.2d:%.2d.%.3d',
          [FTestSchedule.TriggerCount, FTestSchedule.DayCount, ADay, AMonth,
           AYear, AHour, AMinute, ASecond, AMSec]));
        Application.ProcessMessages;
        Stamp := FTestSchedule.NextEvent(True);
      end;
    finally
      FBusy := False;
      btnTest.Caption := RsRun;
      btnOk.Enabled := True;
      btnCancel.Enabled := True;
    end;
  end;
end;

procedure TFrmScheduleEditor.AppEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  SelectRecurringInfoPage;
  if rbDaily.Checked then
    UpdateDailyPageInfo
  else
  if rbWeekly.Checked then
    UpdateWeeklyPageInfo
  else
  if rbMonthly.Checked then
    UpdateMonthlyPageInfo
  else
  if rbYearly.Checked then
    UpdateYearlyPageInfo;
  UpdateFrequencyPageInfo;
  UpdateEndPageInfo;

  UpdateTestSettings;
end;

procedure TFrmScheduleEditor.btnOkClick(Sender: TObject);
begin
  InitSchedule(Schedule);
end;

end.

