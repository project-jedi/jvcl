{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgQPrintPreview.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgQPrintPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, QuickRpt, Qrctrls, JvgQRLabel, QRPrntr, ComCtrls, ToolWin,
  JvgBevel, ImgList, JvgLabel, shellApi, StdCtrls;

type
  TJvgfPrintPreview = class(TForm)
    Panel2: TPanel;
    ToolBar1: TToolBar;
    tbPrior: TToolButton;
    glBevel1: TJvgBevel;
    tbNext: TToolButton;
    tbFirst: TToolButton;
    ImageList3: TImageList;
    glBevel2: TJvgBevel;
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

  TJvgMyQRPreview = class(TQRPreview)
  end;
var
  fPrintPreview: TJvgfPrintPreview;
  l: TJvgQRLabel;

implementation
uses JvgTypes, JvgExport, JvgQPrintSetup, printers; //, mdrpt;
{$R *.DFM}

procedure TJvgfPrintPreview.FormCreate(Sender: TObject);
begin
  {  l := TJvgQRLabel.Create(self);
    l.Caption := 'FormCreate(Sender: TObject)';
    l.Top := 40;
    l.Left := 40;
    l.Direction := fldDownUp;
    l.Parent := Band;}
end;

procedure TJvgfPrintPreview.qrPreview(Sender: TObject);
begin
  QRPreview1.QRPrinter := qr.QRPrinter;
  //  QRPreview1.UpdateImage;
end;

procedure TJvgfPrintPreview.tbPriorClick(Sender: TObject);
var
  e: extended;
begin
  QRPreview1.PageNumber := QRPreview1.PageNumber - 1;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 1;
  UpdateStatus;
end;

procedure TJvgfPrintPreview.tbFirstClick(Sender: TObject);
begin
  QRPreview1.PageNumber := 1;
  QRPreview1.UpdateZoom;
  tbPrior.Enabled := false;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  UpdateStatus;
end;

procedure TJvgfPrintPreview.tbNextClick(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.PageNumber + 1;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber <> QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 0;
  UpdateStatus;
end;

procedure TJvgfPrintPreview.glLabel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShellExecute(0, nil, 'http://shop.biblio-globus.ru/cpr/', nil, nil, sw_show);
end;

procedure TJvgfPrintPreview.ToolButton4Click(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.QRPrinter.PageCount;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 1;
  UpdateStatus;
end;

procedure TJvgfPrintPreview.UpdateStatus;
begin
  SB.Panels[0].Text := 'Страница ' + IntToStr(QRPreview1.PageNumber) + ' из ' + IntToStr(QRPreview1.QRPrinter.PageCount);
  try
    PB.Max := QRPreview1.QRPrinter.PageCount;
    PB.Min := QRPreview1.PageNumber;
  except
  end;
end;

procedure TJvgfPrintPreview.tbExportExcelClick(Sender: TObject);
begin
  ExportToExcel(qr);
end;

procedure TJvgfPrintPreview.InitPrintDialog;
begin
  Printer.Orientation := qr.Page.Orientation;
  PrintDialog.PrintRange := prAllPages;
  PrintDialog.Copies := qr.PrinterSettings.Copies;
  PrintDialog.FromPage := 0;
  PrintDialog.ToPage := 0;
end;

procedure TJvgfPrintPreview.SavePrintDialog;
begin
  qr.Page.Orientation := Printer.Orientation;
  qr.PrinterSettings.Copies := PrintDialog.Copies;
  qr.PrinterSettings.FirstPage := PrintDialog.FromPage;
  qr.PrinterSettings.LastPage := PrintDialog.ToPage;

  //  qr.Printer.FirstPage := PrintDialog.FromPage;
  //  qr.Printer.LastPage := PrintDialog.ToPage;
end;

procedure TJvgfPrintPreview.tbPrinterSetupClick(Sender: TObject);
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

procedure TJvgfPrintPreview.tbPrintRangeClick(Sender: TObject);
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

procedure TJvgfPrintPreview.tbPrintClick(Sender: TObject);
begin
  InitPrintDialog;
  if not PrintDialog.Execute then exit;
  SavePrintDialog;
  QRPreview1.Zoom := 100;
  qr.Print;
end;

procedure TJvgfPrintPreview.cbDuplexClick(Sender: TObject);
begin
  qr.PrinterSettings.Duplex := cbDuplex.Checked;
end;

procedure TJvgfPrintPreview.tbLoadClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  qr.QRPrinter.Load(OpenDialog.FileName);
  QRPreview1.QRPrinter := qr.QRPrinter;
  qr.Update;
end;

procedure TJvgfPrintPreview.tbSaveClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  qr.QRPrinter.Save(SaveDialog.FileName);
end;

procedure TJvgfPrintPreview.QuickRep1StartPage(Sender: TCustomQuickRep);
begin
  PB.Position := QRPreview1.PageNumber;
end;

procedure TJvgfPrintPreview.Execute(qr: TCustomQuickRep);
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
