unit JvDateTimeU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvCalendar, ComCtrls, JvMonthCalendar, JvDateTimePicker,
  StdCtrls, JvEdit, JvValidateEdit, Mask, JvToolEdit, ExtCtrls,
  JvAnalogClock, JvClock;

type
  TJvDateTimeFrm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    JvDateTimePicker1: TJvDateTimePicker;
    JvMonthCalendar1: TJvMonthCalendar;
    JvYearEdit1: TJvValidateEdit;
    Label1: TLabel;
    JvDateEdit1: TJvDateEdit;
    Label4: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    JvxClock1: TJvClock;
    JvClock1: TJvClock;
  end;

implementation

{$R *.DFM}

end.
