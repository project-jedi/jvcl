{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormAlarms.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormAlarms;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  JvButton;

type
  TFormAlarm = class(TForm)
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    DateTimePicker2: TDateTimePicker;
    BUButton1: TJvButton;
    BUButton2: TJvButton;
    BUButton3: TJvButton;
    BUButton4: TJvButton;
    DateTimePicker1: TDateTimePicker;
    procedure BUButton3Click(Sender: TObject);
    procedure BUButton4Click(Sender: TObject);
    procedure BUButton1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure BUButton2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure DateTimePicker2Change(Sender: TObject);
  private
  public
    procedure LoadFromStr(Value: TStringList);
    function SetFromStr: TstringList;
  end;

implementation

{$R *.DFM}

type
  TAlarmInf = class
    AlarmTime: TDateTime;
  end;

  {************************************************************}

procedure TFormAlarm.BUButton3Click(Sender: TObject);
begin
  Close;
end;

{************************************************************}

procedure TFormAlarm.LoadFromStr(Value: TStringList);
var
  i: Integer;
  al: TAlarmInf;
begin
  for i := 0 to Value.Count - 1 do
  begin
    al := TAlarmInf.Create;
    al.AlarmTime := StrToDateTime(Copy(Value[i], 1, Pos('|', Value[i]) - 1));
    ListBox1.Items.AddObject(Copy(Value[i], Pos('|', Value[i]) + 1, Length(Value[i])), TObject(al));
  end;
end;

{************************************************************}

function TFormAlarm.SetFromStr: TstringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to ListBox1.Items.Count - 1 do
    Result.Add(DateTimeToStr(TAlarmInf(ListBox1.Items.Objects[i]).AlarmTime) + '|' + ListBox1.Items[i]);
end;

{************************************************************}

procedure TFormAlarm.BUButton4Click(Sender: TObject);
begin
  Tag := 0;
  Close;
end;

{************************************************************}

procedure TFormAlarm.BUButton1Click(Sender: TObject);
var
  al: TAlarmInf;
begin
  al := TAlarmInf.Create;
  al.AlarmTime := Now;
  ListBox1.ItemIndex := ListBox1.Items.AddObject('New', TObject(al));
  ListBox1DblClick(Self);
end;

{************************************************************}

procedure TFormAlarm.ListBox1DblClick(Sender: TObject);
begin
  GroupBox1.Enabled := True;
  Edit1.Text := ListBox1.Items[ListBox1.ItemIndex];
  DateTimePicker2.Time :=
    StrToTime(TimeToStr(TAlarmInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).AlarmTime));
  DateTimePicker1.date :=
    StrToDate(DateToStr(TAlarmInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).AlarmTime));
end;

{************************************************************}

procedure TFormAlarm.BUButton2Click(Sender: TObject);
var
  i: Integer;
begin
  i := 0;
  while i < ListBox1.Items.Count do
  begin
    if ListBox1.Selected[i] then
      ListBox1.Items.Delete(i)
    else
      Inc(i);
  end;
  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := 0
  else
    GroupBox1.Enabled := False;
end;

{************************************************************}

procedure TFormAlarm.Edit1Change(Sender: TObject);
begin
  ListBox1.Items[ListBox1.ItemIndex] := Edit1.Text;
end;

{************************************************************}

procedure TFormAlarm.DateTimePicker2Change(Sender: TObject);
begin
  TAlarmInf(ListBox1.Items.Objects[ListBox1.ItemIndex]).AlarmTime :=
    StrToDateTime(DateToStr(DateTimePicker1.Date) + ' ' + TimeToStr(DateTimePicker2.Time));
end;

end.
