unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvCalendar, JvComponent;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    chkMulti: TCheckBox;
    chkShow: TCheckBox;
    chkCircle: TCheckBox;
    chkWeeks: TCheckBox;
    mc: TJvMonthCalendar2;
    procedure mcSelChange(Sender: TObject; StartDate,
      EndDate: TDateTime);
    procedure mcSelect(Sender: TObject; StartDate,
      EndDate: TDateTime);
    procedure chkMultiClick(Sender: TObject);
    procedure chkShowClick(Sender: TObject);
    procedure chkCircleClick(Sender: TObject);
    procedure chkWeeksClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.mcSelChange(Sender: TObject; StartDate,
  EndDate: TDateTime);
begin
  StatusBar1.Panels[0].Text := Format('Visible dates are from %s to %s (incl. partial months)',[DateToStr(Mc.FirstVisibleDate(true)),DateToStr(Mc.LastVisibleDate(true))]);
end;

procedure TForm1.mcSelect(Sender: TObject; StartDate,
  EndDate: TDateTime);
var Y,M,D:Word;
begin
  Caption := Format('User selected from %s to %s',[DateToStr(StartDate),DateToStr(EndDate)]);
  DecodeDate(StartDate,Y,M,D);
  { toggle bold on / off }
  mc.Bold[Y,M,D] := not mc.Bold[Y,M,D];
end;

procedure TForm1.chkMultiClick(Sender: TObject);
begin
  mc.MultiSelect := chkMulti.Checked;
end;

procedure TForm1.chkShowClick(Sender: TObject);
begin
  mc.ShowToday := chkShow.Checked;
end;

procedure TForm1.chkCircleClick(Sender: TObject);
begin
  mc.CircleToday := chkCircle.Checked;
end;

procedure TForm1.chkWeeksClick(Sender: TObject);
begin
  mc.WeekNumbers := chkWeeks.Checked;
end;

procedure TForm1.FormShow(Sender: TObject);
var Y,M,D:word;
begin
  mc.Today := Date;
  DecodeDate(Date,y,m,d);
  mc.BoldDays.Add(Format('0000%.2d=2,4,7,11',[M]));
  mc.BoldDays.Add(Format('%.4d%.2d=1,3,5,9',[Y,M]));
  Caption :=  Format('ComCtl32.dll version: %d.%d',[HiWord(GetComCtlVersion),LoWord(GetComCtlVersion)]);
end;

end.
