unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvPrvwDoc, ComCtrls, StdCtrls, ExtCtrls, Menus, JvRichEdit, JvPrvwRender;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    udCols: TUpDown;
    Edit2: TEdit;
    udRows: TUpDown;
    Edit3: TEdit;
    udShadowWidth: TUpDown;
    Label4: TLabel;
    Edit4: TEdit;
    udZoom: TUpDown;
    PrinterSetupDialog1: TPrinterSetupDialog;
    cbPreview: TComboBox;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Printer1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    View1: TMenuItem;
    First1: TMenuItem;
    Previous1: TMenuItem;
    Next1: TMenuItem;
    Last1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Options1: TMenuItem;
    mnuMargins: TMenuItem;
    PageControl1: TPageControl;
    tabPreview: TTabSheet;
    tabOriginal: TTabSheet;
    OpenDialog1: TOpenDialog;
    reOriginal: TJvRichEdit;
    PrintDialog1: TPrintDialog;
    Print1: TMenuItem;
    Label6: TLabel;
    cbScaleMode: TComboBox;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure udColsClick(Sender: TObject; Button: TUDBtnType);
    procedure udRowsClick(Sender: TObject; Button: TUDBtnType);
    procedure udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure udZoomClick(Sender: TObject; Button: TUDBtnType);
    procedure cbPreviewChange(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Printer1Click(Sender: TObject);
    procedure mnuMarginsClick(Sender: TObject);
    procedure First1Click(Sender: TObject);
    procedure Previous1Click(Sender: TObject);
    procedure Next1Click(Sender: TObject);
    procedure Last1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure cbScaleModeChange(Sender: TObject);
  private
    { Private declarations }
    procedure OpenFile(const Filename: string);
    procedure DoChange(Sender: TObject);
    procedure DoVertScroll(Sender: TObject);
    procedure BuildPreview;
  public
    { Public declarations }
    pd: TJvPreviewDoc;
  end;


var
  frmMain: TfrmMain;

implementation
uses
  Printers;

{$R *.dfm}


procedure TfrmMain.Print1Click(Sender: TObject);
var jp: TJvPrinter;
begin
  PrintDialog1.PrintRange := prAllPages;
  if pd.PageCount < 1 then
    PrintDialog1.Options := PrintDialog1.Options - [poPageNums]
  else
  begin
    PrintDialog1.Options := PrintDialog1.Options + [poPageNums];
    PrintDialog1.FromPage := 1;
    PrintDialog1.ToPage := pd.PageCount;
  end;
  if PrintDialog1.Execute then
  begin
    jp := TJvPrinter.Create(nil);
    try
      jp.Printer := Printer;
      if PrintDialog1.PrintRange = prPageNums then
        pd.PrintRange(jp, PrintDialog1.FromPage - 1, PrintDialog1.ToPage - 1, PrintDialog1.Copies, PrintDialog1.Collate)
      else
        pd.PrintRange(jp, 0, -1, PrintDialog1.Copies, PrintDialog1.Collate)
    finally
      jp.Free;
    end;
  end;
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pd := TJvPreviewDoc.Create(self);
  pd.Parent := tabPreview;
  pd.Align := alClient;
  pd.TabStop := true;
  pd.BeginUpdate;
  pd.OnChange := DoChange;
  try
    pd.Options.DrawMargins := mnuMargins.Checked;
    pd.Options.Rows := udRows.Position;
    pd.Options.Cols := udCols.Position;
    pd.Options.Shadow.Offset := udShadowWidth.Position;
    pd.Options.Scale := udZoom.Position;
    pd.OnVertScroll := DoVertScroll;

    cbPreview.ItemIndex := 1; // printer
    cbPreviewChange(nil);
    cbScaleMode.ItemIndex := 0; // full page
//    cbScaleModeChange(nil);

  finally
    pd.EndUpdate;
  end;
end;

procedure TfrmMain.DoChange(Sender: TObject);
begin
  udCols.Position := pd.Options.Cols;
  udRows.Position := pd.Options.Rows;
  udShadowWidth.Position := pd.Options.Shadow.Offset;
  udZoom.Position := pd.Options.Scale;
  mnuMargins.Checked := pd.Options.DrawMargins;
  cbScaleMode.ItemIndex := Ord(pd.Options.ScaleMode);
  Statusbar1.Panels[0].Text := ExtractFilename(OpenDIalog1.Filename);
  Statusbar1.Panels[1].Text := Format('%d pages',[pd.PageCount]);
  Statusbar1.Panels[2].Text := Format('Cols: %d, Rows: %d, Page %d',[pd.TotalCols,pd.VisibleRows,pd.TopPage]);
end;

procedure TfrmMain.udColsClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Cols := udCols.Position;
  udCols.Position := pd.Options.Cols;
end;

procedure TfrmMain.udRowsClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Rows := udRows.Position;
  udRows.Position := pd.Options.Rows;
end;

procedure TfrmMain.udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Shadow.Offset := udShadowWidth.Position;
  udShadowWidth.Position := pd.Options.Shadow.Offset;
end;

procedure TfrmMain.udZoomClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Scale := udZoom.Position;
  udZoom.Position := pd.Options.Scale;
end;

procedure TfrmMain.cbPreviewChange(Sender: TObject);
begin
  case cbPreview.ItemIndex of
    0:
      pd.DeviceInfo.ReferenceHandle := 0; // reset to default (screen)
    1:
      pd.DeviceInfo.ReferenceHandle := Printer.Handle;
  end;
  // set 0.5 inch margin
  pd.DeviceInfo.OffsetLeft := Max(pd.DeviceInfo.InchToXPx(0.5),pd.DeviceInfo.OffsetLeft);
  pd.DeviceInfo.OffsetRight := Max(pd.DeviceInfo.InchToXPx(0.5),pd.DeviceInfo.OffsetRight);
  pd.DeviceInfo.OffsetTop := Max(pd.DeviceInfo.InchToYPx(0.5),pd.DeviceInfo.OffsetTop);
  pd.DeviceInfo.OffsetBottom := Max(pd.DeviceInfo.InchToYPx(0.5),pd.DeviceInfo.OffsetBottom);
  
  BuildPreview;
end;

procedure TfrmMain.OpenFile(const Filename: string);
begin
  reOriginal.Lines.LoadFromFile(OpenDialog1.Filename);
  Screen.Cursor := crHourGlass;
  BuildPreview;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(OpenDialog1.Filename);
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.Printer1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  cbPreviewChange(Sender);
end;

procedure TfrmMain.mnuMarginsClick(Sender: TObject);
begin
  mnuMargins.Checked := not mnuMargins.Checked;
  pd.Options.DrawMargins := mnuMargins.Checked;
end;

procedure TfrmMain.First1Click(Sender: TObject);
begin
  pd.First;
end;

procedure TfrmMain.Previous1Click(Sender: TObject);
begin
  pd.Prior;
end;

procedure TfrmMain.Next1Click(Sender: TObject);
begin
  pd.Next;
end;

procedure TfrmMain.Last1Click(Sender: TObject);
begin
  pd.Last;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('JvPreviewDocument Demo');
end;

procedure TfrmMain.cbScaleModeChange(Sender: TObject);
begin
  pd.Options.ScaleMode := TJvPreviewScaleMode(cbScaleMode.ItemIndex);
  cbScaleMode.ItemIndex := Ord(pd.Options.ScaleMode);
end;

procedure TfrmMain.DoVertScroll(Sender: TObject);
begin
  Statusbar1.Panels[2].Text := Format('Cols: %d, Rows: %d, Page %d',[pd.TotalCols,pd.VisibleRows,pd.TopPage]);
end;

procedure TfrmMain.BuildPreview;
begin
  with TJvPreviewRichEditRender.Create(nil) do
  try
    RichEdit := reOriginal;
    PrintPreview := pd;
    CreatePreview(false);
  finally
    Free;
  end;
end;

end.

