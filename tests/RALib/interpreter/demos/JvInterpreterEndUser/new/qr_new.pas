unit qr_new;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Quickrpt, QRCtrls, Db, DBTables;

type
  TForm1 = class(TForm)
    QuickRep1 : TQuickRep;
    TitleBand1 : TQRBand;
    QRLabel1 : TQRLabel;
    PageFooterBand1 : TQRBand;
    QRSysData1 : TQRSysData;
    ColumnHeaderBand1 : TQRBand;
    DetailBand1 : TQRBand;
    QRLabel2 : TQRLabel;
    QRExpr1 : TQRExpr;
    QRLabel3 : TQRLabel;
    QRExpr2 : TQRExpr;
    QRLabel4 : TQRLabel;
    QRExpr3 : TQRExpr;
    QRLabel5 : TQRLabel;
    QRExpr4 : TQRExpr;
    QRLabel6 : TQRLabel;
    QRExpr5 : TQRExpr;
    QRLabel7 : TQRLabel;
    QRExpr6 : TQRExpr;
    procedure FormCreate(Sender: TObject);
    procedure DetailBand1BeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure QuickRep1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure QuickRep1BeforePrint(Sender: TQuickRep;
      var PrintReport: Boolean);
  private
  public
    L, M: Integer;
    DataSet: TDataSet;
  end;

var
  Form1: TForm1;

implementation

//uses fJvInterpreterTest;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DataSet := Application.FindComponent('DataModule1').FindComponent('Table1');
  QRLabel1.Caption := 'OnNeedData report';
  L := QRExpr2.Left;
  M := 0;
end;

procedure TForm1.DetailBand1BeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  QRExpr2.Left := L;
  QRExpr1.Expression := DataSet.FieldValues['EmpNo'];
  QRExpr2.Expression := DataSet.FieldValues['LastName'];
  QRExpr3.Expression := DataSet.FieldValues['FirstName'];
  L := L + 1;
end;

procedure TForm1.QuickRep1NeedData(Sender: TObject; var MoreData: Boolean);
begin
  M := M + 1;
  MoreData := M < 10;
  DataSet.Next;
end;

procedure TForm1.QuickRep1BeforePrint(Sender: TQuickRep;
  var PrintReport: Boolean);
begin
  DataSet.Open;
  DataSet.First;
end;

end.
