unit JvDateTimeU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvCalendar, ComCtrls, JvMonthCalendar, JvDateTimePicker,
  StdCtrls, JvEdit, JvTypedEdit, Mask, JvToolEdit, JvxClock, ExtCtrls,
  JvAnalogClock, JvClock;

type
  TJvDateTimeFrm = class(TFrame)
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    JvDateTimePicker1: TJvDateTimePicker;
    JvMonthCalendar1: TJvMonthCalendar;
    JvMonthCalendar21: TJvMonthCalendar2;
    JvYearEdit1: TJvYearEdit;
    Label1: TLabel;
    JvDateEdit1: TJvDateEdit;
    Label4: TLabel;
    JvClock1: TJvClock;
    JvAnalogClock1: TJvAnalogClock;
    Label6: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    JvxClock1: TJvxClock;
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvDateTimeFrm.ComboBox1Change(Sender: TObject);
begin
 case ComboBox1.itemIndex of
   0 : JvClock1.ClockStyle := csTime;
   1 : JvClock1.ClockStyle := csTimeDate;
   2 : JvClock1.ClockStyle := csDateTime;
   3 : JvClock1.ClockStyle := csDate;
 end;
end;

end.
