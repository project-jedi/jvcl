{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlarmsForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAlarmsForm;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls;

type
  TFormAlarm = class(TForm)
    AlarmListBox: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    NameEdit: TEdit;
    TimePick: TDateTimePicker;
    AddBtn: TButton;
    RemoveBtn: TButton;
    CancelBtn: TButton;
    OkBtn: TButton;
    DatePick: TDateTimePicker;
    procedure CancelBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure TimePickChange(Sender: TObject);
  public
    procedure LoadFromStr(Value: TStringList);
    function SetFromStr: TStringList;
  end;

implementation

{$R *.DFM}

type
  TAlarmInf = class(TObject)
    AlarmTime: TDateTime;
  end;

procedure TFormAlarm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAlarm.LoadFromStr(Value: TStringList);
var
  I: Integer;
  Al: TAlarmInf;
begin
  for I := 0 to Value.Count - 1 do
  begin
    Al := TAlarmInf.Create;
    Al.AlarmTime := StrToDateTime(Copy(Value[I], 1, Pos('|', Value[I]) - 1));
    AlarmListBox.Items.AddObject(Copy(Value[I], Pos('|', Value[I]) + 1, Length(Value[I])), TObject(Al));
  end;
end;

function TFormAlarm.SetFromStr: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to AlarmListBox.Items.Count - 1 do
    Result.Add(DateTimeToStr(TAlarmInf(AlarmListBox.Items.Objects[I]).AlarmTime) + '|' + AlarmListBox.Items[I]);
end;

procedure TFormAlarm.OkBtnClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;

procedure TFormAlarm.AddBtnClick(Sender: TObject);
var
  Al: TAlarmInf;
begin
  Al := TAlarmInf.Create;
  Al.AlarmTime := Now;
  AlarmListBox.ItemIndex := AlarmListBox.Items.AddObject('New', TObject(Al));
  ListBox1DblClick(Self);
end;

procedure TFormAlarm.ListBox1DblClick(Sender: TObject);
begin
  GroupBox1.Enabled := True;
  NameEdit.Text := AlarmListBox.Items[AlarmListBox.ItemIndex];
  TimePick.Time :=
    StrToTime(TimeToStr(TAlarmInf(AlarmListBox.Items.Objects[AlarmListBox.ItemIndex]).AlarmTime));
  DatePick.Date :=
    StrToDate(DateToStr(TAlarmInf(AlarmListBox.Items.Objects[AlarmListBox.ItemIndex]).AlarmTime));
end;

procedure TFormAlarm.RemoveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := 0;
  while I < AlarmListBox.Items.Count do
    if AlarmListBox.Selected[I] then
      AlarmListBox.Items.Delete(I)
    else
      Inc(I);
  if AlarmListBox.Items.Count > 0 then
    AlarmListBox.ItemIndex := 0
  else
    GroupBox1.Enabled := False;
end;

procedure TFormAlarm.NameEditChange(Sender: TObject);
begin
  AlarmListBox.Items[AlarmListBox.ItemIndex] := NameEdit.Text;
end;

procedure TFormAlarm.TimePickChange(Sender: TObject);
begin
  TAlarmInf(AlarmListBox.Items.Objects[AlarmListBox.ItemIndex]).AlarmTime :=
    StrToDateTime(DateToStr(DatePick.Date) + ' ' + TimeToStr(TimePick.Time));
end;

end.
