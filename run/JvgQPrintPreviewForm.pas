{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgQPrintPreview.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgQPrintPreviewForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  QuickRpt, QRCtrls, QRPrntr,
  ComCtrls, ToolWin, ImgList, ShellAPI, StdCtrls,
  JvComponent, JvExControls,
  JvgQRLabel;

type
  TJvgPrintPreview = class(TJvForm)
    Panel2: TPanel;
    ToolBar1: TToolBar;
    tbPrior: TToolButton;
    tbNext: TToolButton;
    tbFirst: TToolButton;
    ImageList4: TImageList;
    Panel1: TPanel;
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
    QRPreview1: TQRPreview;
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
    QR: TCustomQuickRep;
    procedure UpdateStatus;
    procedure InitPrintDialog;
    procedure SavePrintDialog;
    procedure QuickRep1StartPage(Sender: TCustomQuickRep);
  public
    procedure Execute(QR: TCustomQuickRep);
  end;

  TJvgMyQRPreview = class(TQRPreview);

var
  fPrintPreview: TJvgPrintPreview;
  l: TJvgQRLabel;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Printers,
  JvResources, JvConsts,
  JvgTypes, JvgExport, JvgQPrintSetupForm;

{$R *.dfm}

procedure TJvgPrintPreview.FormCreate(Sender: TObject);
begin
  {  l := TJvgQRLabel.Create(Self);
    l.Caption := 'FormCreate(Sender: TObject)';
    l.Top := 40;
    l.Left := 40;
    l.Direction := fldDownUp;
    l.Parent := Band;}
end;

procedure TJvgPrintPreview.qrPreview(Sender: TObject);
begin
  QRPreview1.QRPrinter := QR.QRPrinter;
  //  QRPreview1.UpdateImage;
end;

procedure TJvgPrintPreview.tbPriorClick(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.PageNumber - 1;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 1;
  UpdateStatus;
end;

procedure TJvgPrintPreview.tbFirstClick(Sender: TObject);
begin
  QRPreview1.PageNumber := 1;
  QRPreview1.UpdateZoom;
  tbPrior.Enabled := False;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  UpdateStatus;
end;

procedure TJvgPrintPreview.tbNextClick(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.PageNumber + 1;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber <> QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 0;
  UpdateStatus;
end;

procedure TJvgPrintPreview.glLabel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // (rom) should be removed
  //ShellExecute(0, nil, 'http://shop.biblio-globus.ru/cpr/', nil, nil, SW_SHOW);
end;

procedure TJvgPrintPreview.ToolButton4Click(Sender: TObject);
begin
  QRPreview1.PageNumber := QRPreview1.QRPrinter.PageCount;
  QRPreview1.UpdateZoom;
  tbNext.Enabled := QRPreview1.PageNumber < QRPreview1.QRPrinter.PageCount;
  tbPrior.Enabled := QRPreview1.PageNumber > 1;
  UpdateStatus;
end;

procedure TJvgPrintPreview.UpdateStatus;
begin
  SB.Panels[0].Text := Format(RsPageOfPages, [QRPreview1.PageNumber, QRPreview1.QRPrinter.PageCount]);
  try
    PB.Max := QRPreview1.QRPrinter.PageCount;
    PB.Min := QRPreview1.PageNumber;
  except
  end;
end;

procedure TJvgPrintPreview.tbExportExcelClick(Sender: TObject);
begin
  ExportToExcel(QR);
end;

procedure TJvgPrintPreview.InitPrintDialog;
begin
  Printer.Orientation := QR.Page.Orientation;
  PrintDialog.PrintRange := prAllPages;
  PrintDialog.Copies := QR.PrinterSettings.Copies;
  PrintDialog.FromPage := 0;
  PrintDialog.ToPage := 0;
end;

procedure TJvgPrintPreview.SavePrintDialog;
begin
  QR.Page.Orientation := Printer.Orientation;
  QR.PrinterSettings.Copies := PrintDialog.Copies;
  QR.PrinterSettings.FirstPage := PrintDialog.FromPage;
  QR.PrinterSettings.LastPage := PrintDialog.ToPage;
  //  QR.Printer.FirstPage := PrintDialog.FromPage;
  //  QR.Printer.LastPage := PrintDialog.ToPage;
end;

procedure TJvgPrintPreview.tbPrinterSetupClick(Sender: TObject);
begin
  Printer.Orientation := QR.Page.Orientation;
  //  Printer.Paper
  PrinterSetupDialog.Execute;
  QR.Page.Orientation := Printer.Orientation;
  QR.PrinterSettings.Orientation := Printer.Orientation;

  QR.Prepare;
  QRPreview1.QRPrinter := QR.QRPrinter;
  QRPreview1.UpdateZoom;
  UpdateStatus;
end;

procedure TJvgPrintPreview.tbPrintRangeClick(Sender: TObject);
begin
  //  InitPrintDialog;
  //  PrintDialog.FromPage := QRPreview1.PageNumber;
  //  PrintDialog.ToPage := QRPreview1.PageNumber;
  //  PrintDialog.PrintRange := prPageNums;
  //  if not PrintDialog.Execute then exit;
  //  SavePrintDialog;
  QR.PrinterSettings.FirstPage := QRPreview1.PageNumber;
  QR.PrinterSettings.LastPage := QRPreview1.PageNumber;

  QRPreview1.Zoom := 100;
  QR.Print;
end;

procedure TJvgPrintPreview.tbPrintClick(Sender: TObject);
begin
  InitPrintDialog;
  if PrintDialog.Execute then
  begin
    SavePrintDialog;
    QRPreview1.Zoom := 100;
    QR.Print;
  end;
end;

procedure TJvgPrintPreview.cbDuplexClick(Sender: TObject);
begin
  QR.PrinterSettings.Duplex := cbDuplex.Checked;
end;

procedure TJvgPrintPreview.tbLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    QR.QRPrinter.Load(OpenDialog.FileName);
    QRPreview1.QRPrinter := QR.QRPrinter;
    QR.Update;
  end;
end;

procedure TJvgPrintPreview.tbSaveClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    QR.QRPrinter.Save(SaveDialog.FileName);
end;

procedure TJvgPrintPreview.QuickRep1StartPage(Sender: TCustomQuickRep);
begin
  PB.Position := QRPreview1.PageNumber;
end;

procedure TJvgPrintPreview.Execute(QR: TCustomQuickRep);
begin
  Self.QR := QR;
  QR.OnStartPage := nil;
  QR.Prepare;
  QRPreview1.QRPrinter := QR.QRPrinter;
  QRPreview1.UpdateZoom;
  UpdateStatus;

  QR.OnStartPage := QuickRep1StartPage;

  cbDuplex.Checked := QR.PrinterSettings.Duplex;
  ShowModal;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.