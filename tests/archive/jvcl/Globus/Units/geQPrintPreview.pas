unit geQPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, QuickRpt, Qrctrls, glRLabel, QRPrntr, ComCtrls, ToolWin,
  glBevel, ImgList, glLabel, shellApi, StdCtrls;

type
  TfPrintPreview = class(TForm)
    Panel2: TPanel;
    ToolBar1: TToolBar;
    tbPrior: TToolButton;
    glBevel1: TglBevel;
    tbNext: TToolButton;
    tbFirst: TToolButton;
    ImageList3: TImageList;
    glBevel2: TglBevel;
    ImageList4: TImageList;
    QRPreview1: TQRPreview;
    Panel1: TPanel;
    glLabel1: TLabel;
    Shape1: TShape;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    tbPrinterSetup: TToolButton;
    tbPrintRange: TToolButton;
    ToolButton8: TToolButton;
    tbPrint: TToolButton;
    tbLoad: TToolButton;
    tbSave: TToolButton;
    SB: TStatusBar;
    tbExportExcel: TToolButton;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    cbDuplex: TCheckBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PB: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure qrPreview(Sender: TObject);
    procedure tbPriorClick(Sender: TObject);
    procedure tbFirstClick(Sender: TObject);
    procedure tbNextClick(Sender: TObject);
    procedure glLabel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbPrintClick(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure tbExportExcelClick(Sender: TObject);
    procedure tbPrinterSetupClick(Sender: TObject);
    procedure tbPrintRangeClick(Sender: TObject);
    procedure cbDuplexClick(Sender: TObject);
    procedure tbLoadClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
  private
    qr: TCustomQuickRep;
    procedure UpdateStatus;
    procedure InitPrintDialog;
    procedure SavePrintDialog;
    procedure QuickRep1StartPage(Sender: TCustomQuickRep);

  public
    procedure Execute(qr: TCustomQuickRep);
  end;

  TmyQRPreview = class(TQRPreview)
  end;
var
  fPrintPreview: TfPrintPreview;
  l: TglQRLabel;

implementation
uses glTypes, glExport, geQPrintSetup, printers;//, mdrpt;
{$R *.DFM}

procedure TfPrintPreview.FormCreate(Sender: TObject);
begin
{  l := TglQRLabel.Create(self);
  l.Caption := 'FormCreate(Sender: TObject)';
  l.Top := 40;
  l.Left := 40;
  l.Direction := fldDownUp;
  l.Parent := Band;}
end;

procedure TfPrintPreview.qrPreview(Sender: TObject);
begin
  QRPreview1.QRPrinter := qr.QRPrinter;
//  QRPreview1.UpdateImage;
end;

procedure TfPrintPreview.tbPriorClick(Sender: TObject);
var e: extended;
begin
  QRPreview1.PageNumber := QRPreview1.PageNumber - 1;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 1;
  UpdateStatus;
end;

procedure TfPrintPreview.tbFirstClick(Sender: TObject);
begin
  QRPreview1.PageNumber := 1;
  QRPreview1.UpdateZoom;
  tbPrior.Enabled := false;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  UpdateStatus;
end;

procedure TfPrintPreview.tbNextClick(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.PageNumber + 1;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber <> QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 0;
  UpdateStatus;
end;

procedure TfPrintPreview.glLabel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShellExecute(0,nil,'http://shop.biblio-globus.ru/cpr/',nil,nil,sw_show);
end;

procedure TfPrintPreview.ToolButton4Click(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.QRPrinter.PageCount;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 1;
  UpdateStatus;
end;

procedure TfPrintPreview.UpdateStatus;
begin
  SB.Panels[0].Text := 'Страница ' + IntToStr(QRPreview1.PageNumber) + ' из ' + IntToStr(QRPreview1.QRPrinter.PageCount);
  try
    PB.Max := QRPreview1.QRPrinter.PageCount;
    PB.Min := QRPreview1.PageNumber;
  except end;
end;

procedure TfPrintPreview.tbExportExcelClick(Sender: TObject);
begin
  ExportToExcel(qr);
end;

procedure TfPrintPreview.InitPrintDialog;
begin
  Printer.Orientation := qr.Page.Orientation;
  PrintDialog.PrintRange := prAllPages;
  PrintDialog.Copies := qr.PrinterSettings.Copies;
  PrintDialog.FromPage := 0;
  PrintDialog.ToPage := 0;
end;

procedure TfPrintPreview.SavePrintDialog;
begin
  qr.Page.Orientation := Printer.Orientation;
  qr.PrinterSettings.Copies := PrintDialog.Copies;
  qr.PrinterSettings.FirstPage := PrintDialog.FromPage;
  qr.PrinterSettings.LastPage := PrintDialog.ToPage;

//  qr.Printer.FirstPage := PrintDialog.FromPage;
//  qr.Printer.LastPage := PrintDialog.ToPage;
end;

procedure TfPrintPreview.tbPrinterSetupClick(Sender: TObject);
begin
  Printer.Orientation := qr.Page.Orientation;
//  Printer.Paper
  PrinterSetupDialog.Execute;
  qr.Page.Orientation := Printer.Orientation;
  qr.PrinterSettings.Orientation := Printer.Orientation;

  qr.Prepare;
  QRPreview1.QRPrinter := qr.QRPrinter;
  QRPreview1.UpdateZoom;
  UpdateStatus;
end;

procedure TfPrintPreview.tbPrintRangeClick(Sender: TObject);
begin
//  InitPrintDialog;
//  PrintDialog.FromPage := QRPreview1.PageNumber;
//  PrintDialog.ToPage := QRPreview1.PageNumber;
//  PrintDialog.PrintRange := prPageNums;
//  if not PrintDialog.Execute then exit;
//  SavePrintDialog;
  qr.PrinterSettings.FirstPage := QRPreview1.PageNumber;
  qr.PrinterSettings.LastPage := QRPreview1.PageNumber;

  QRPreview1.Zoom := 100;
  qr.Print;
end;

procedure TfPrintPreview.tbPrintClick(Sender: TObject);
begin
  InitPrintDialog;
  if not PrintDialog.Execute then exit;
  SavePrintDialog;
  QRPreview1.Zoom := 100;
  qr.Print;
end;

procedure TfPrintPreview.cbDuplexClick(Sender: TObject);
begin
  qr.PrinterSettings.Duplex := cbDuplex.Checked;
end;

procedure TfPrintPreview.tbLoadClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  qr.QRPrinter.Load(OpenDialog.FileName);
  QRPreview1.QRPrinter := qr.QRPrinter;
  qr.Update;
end;

procedure TfPrintPreview.tbSaveClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  qr.QRPrinter.Save(SaveDialog.FileName);
end;

procedure TfPrintPreview.QuickRep1StartPage(Sender: TCustomQuickRep);
begin
  PB.Position := QRPreview1.PageNumber;
end;

procedure TfPrintPreview.Execute(qr: TCustomQuickRep);
begin
  self.qr := qr;
  qr.OnStartPage := nil;
  qr.Prepare;
  QRPreview1.QRPrinter := qr.QRPrinter;
  QRPreview1.UpdateZoom;
  UpdateStatus;

  qr.OnStartPage := QuickRep1StartPage;

  cbDuplex.Checked := qr.PrinterSettings.Duplex;
  ShowModal;
end;

end.

