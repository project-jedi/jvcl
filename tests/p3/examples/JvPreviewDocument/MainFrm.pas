unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvPrvwDoc, ComCtrls, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    chkAutoScroll: TCheckBox;
    Label3: TLabel;
    Edit1: TEdit;
    udColumns: TUpDown;
    Edit2: TEdit;
    udCount: TUpDown;
    Edit3: TEdit;
    udShadowWidth: TUpDown;
    Label4: TLabel;
    Edit4: TEdit;
    udZoom: TUpDown;
    Button1: TButton;
    PrinterSetupDialog1: TPrinterSetupDialog;
    chkMargins: TCheckBox;
    cbPreview: TComboBox;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure chkAutoScrollClick(Sender: TObject);
    procedure udColumnsClick(Sender: TObject; Button: TUDBtnType);
    procedure udCountClick(Sender: TObject; Button: TUDBtnType);
    procedure udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure udZoomClick(Sender: TObject; Button: TUDBtnType);
    procedure Button1Click(Sender: TObject);
    procedure chkMarginsClick(Sender: TObject);
    procedure cbPreviewChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pd:TJvPreviewDoc;
  end;

var
  Form1: TForm1;

implementation
uses
  Printers;
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  pd := TJvPreviewDoc.Create(self);
  pd.Parent := self;
  pd.Align := alClient;
  pd.Options.DrawMargins := chkMargins.Checked;
  pd.Options.Count := udCount.Position;
  pd.Options.Columns := udColumns.Position;
  pd.Options.Shadow.Offset := udShadowWidth.Position;
  pd.Options.Zoom := udZoom.Position;
  cbPreview.ItemIndex := 2;
  cbPreviewChange(nil);
end;

procedure TForm1.chkAutoScrollClick(Sender: TObject);
begin
  pd.AutoScroll := chkAutoScroll.Checked;
end;

procedure TForm1.udColumnsClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Columns := udColumns.Position;
end;

procedure TForm1.udCountClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Count := udCount.Position;
end;

procedure TForm1.udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Shadow.Offset := udShadowWidth.Position;
end;

procedure TForm1.udZoomClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Zoom := udZoom.Position;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  cbPreviewChange(nil);
end;

procedure TForm1.chkMarginsClick(Sender: TObject);
begin
  pd.Options.DrawMargins := chkMargins.Checked;
end;

procedure TForm1.cbPreviewChange(Sender: TObject);
begin
  case cbPreview.ItemIndex of
  0:
    pd.DeviceInfo.ReferenceHandle := 0;
  1:
    pd.DeviceInfo.ReferenceHandle := Canvas.Handle; // should give same result as Screen
  2:
    pd.DeviceInfo.ReferenceHandle := Printer.Handle;
  end;
end;

end.
