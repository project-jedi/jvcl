unit JvDBDateTimePickerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, Db, DBTables,
  JvDBDateTimePicker, StdCtrls, JvDateTimePicker, JvComponent,
  JvCaptionPanel;

type
  TJvDBDateTimePickerMainForm = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    Label1: TLabel;
    Label2: TLabel;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    JvDBDateTimePicker1: TJvDBDateTimePicker;
    JvDBDateTimePicker2: TJvDBDateTimePicker;
    procedure FormCreate(Sender: TObject);
  end;

var
  JvDBDateTimePickerMainForm: TJvDBDateTimePickerMainForm;

implementation

{$R *.DFM}

procedure TJvDBDateTimePickerMainForm.FormCreate(Sender: TObject);
begin
  Table1.Active := true;
end;

end.
