unit JvDBDateTimeU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, Db, DBTables,
  JvDBDateTimePicker, StdCtrls, JvDateTimePicker, JvComponent,
  JvCaptionPanel;

type
  TJvDBDateTimeForm = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    JvDBDateTimePicker1: TJvDBDateTimePicker;
    JvDBDateTimePicker2: TJvDBDateTimePicker;
    DataSource1: TDataSource;
    Table1: TTable;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JvDBDateTimeForm: TJvDBDateTimeForm;

implementation

{$R *.DFM}

procedure TJvDBDateTimeForm.FormCreate(Sender: TObject);
begin
  Table1.Active := true;
end;

end.
