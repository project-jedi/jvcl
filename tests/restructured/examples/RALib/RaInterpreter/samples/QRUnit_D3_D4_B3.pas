unit QRUnit_D3_D4_B3;

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
    QRLabel3 : TQRLabel;
    QRLabel4 : TQRLabel;
    QRExpr1: TQRExpr;
    QRLabel5: TQRLabel;
    procedure FormCreate(Sender: TObject);
    procedure DetailBand1BeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure QuickRep1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure QuickRep1BeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
  end;

var
  Form1: TForm1;

implementation

//uses fJvInterpreterTest;

{$R *.DFM}

var
    L, M: Integer;
    DataSet: TDataSet;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DataSet := Application.FindComponent('Test').FindComponent('Table1');
  QRLabel1.Caption := 'Demo';
  L := Field2.Left;
  M := 0;
  Field2.Left := 100;
end;

procedure TForm1.DetailBand1BeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
  Res: TQREvResult;
begin
  Field1.Caption := DataSet.FieldValues['EmpNo'];
  Field2.Caption := DataSet.FieldValues['LastName'];
  Field3.Caption := DataSet.FieldValues['FirstName'];
  Field2.Left := L;
  L := L + 5;
 { QRExpr1.Value.strResult := 'Hello, Didier....';
  QRLabel5.Caption := QRExpr1.Value.strResult; }
  //Res.intResult := 111;
  //QRLabel5.Caption := IntToStr(Res.intResult);
  //QRExpr1.Value := Res;
  //QRExpr1.Value.intResult := 1;
  //QRLabel5.Caption := IntToStr(QRExpr1.Value.intResult);
end;

procedure TForm1.QuickRep1NeedData(Sender: TObject; var MoreData: Boolean);
begin
  M := M + 1;
  MoreData := M < 10;
  DataSet.Next;
end;

procedure TForm1.QuickRep1BeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
begin
  DataSet.Open;
  DataSet.First;
end;

end.
