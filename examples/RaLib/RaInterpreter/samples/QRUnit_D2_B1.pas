unit QRUnit_D2_B1;

// QuickReport simple list 
// - Connect a datasource to the QuickReport component
// - Put QRDBText components on the Detail band

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Quickrep, StdCtrls, ExtCtrls, DB, DBTables, Qrctrls, QuickRpt;

type
  TQRListForm = class(TForm)
    QuickReport1: TQuickReport;
    Title: TQRBand;
    PageHeader: TQRBand;
    Detail: TQRBand;
    PageNumber: TQRSysData;
    PageFooter: TQRBand;
    QRDateTime: TQRSysData;
    QRSysData1: TQRSysData;
    QRLabel1: TQRLabel;
    QRLabel3: TQRLabel;
    Table1: TTable;
    DataSource1: TDataSource;
    QRDBText1: TQRDBText;
    QRDBText2: TQRDBText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QRListForm: TQRListForm;

implementation

{$R *.DFM}

end.
