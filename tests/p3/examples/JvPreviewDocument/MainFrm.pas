unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvPrvwDoc, ComCtrls, StdCtrls, ExtCtrls, Menus;

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
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure chkAutoScrollClick(Sender: TObject);
    procedure udColumnsClick(Sender: TObject; Button: TUDBtnType);
    procedure udCountClick(Sender: TObject; Button: TUDBtnType);
    procedure udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure udZoomClick(Sender: TObject; Button: TUDBtnType);
    procedure Button1Click(Sender: TObject);
    procedure chkMarginsClick(Sender: TObject);
    procedure cbPreviewChange(Sender: TObject);
    procedure Open1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pd:TJvPreviewDoc;
  end;
  // NB! Doesn't wordwrap!
  TJvStringsPreviewRenderer = class(TObject)
  private
    FStrings:TStringlist;
    FPreview:TJvPreviewDoc;
    FFinished:boolean;
    FCurrentRow:integer;
    procedure CreatePreview;
    procedure DoAddPage(Sender: TObject; PageIndex: integer;
      Canvas: TCanvas; PageRect, PrintRect: TRect);
  public
    constructor Create(Preview:TJvPreviewDoc;Strings:TStrings);
    destructor Destroy;override;
    property Finished:boolean read FFinished;
  end;


var
  Form1: TForm1;

implementation
uses
  Printers;
{$R *.dfm}

{ TJvStringsPreviewRenderer }

constructor TJvStringsPreviewRenderer.Create(Preview: TJvPreviewDoc;
  Strings: TStrings);
begin
  inherited Create;
  FPreview := Preview;
  FStrings := TStringlist.Create;
  FStrings.Assign(Strings);
  CreatePreview;
end;

procedure TJvStringsPreviewRenderer.DoAddPage(Sender: TObject; PageIndex: integer; Canvas: TCanvas;
    PageRect, PrintRect: TRect);
var i,X,Y,IncValue:integer;ARect:TRect;tm:TTextMetric;
begin
  if not FFinished then
  begin
    Canvas.Font.Name := 'Courier New';
    Canvas.Font.Size := 10;
    GetTextMetrics(Canvas.Handle,tm);
    IncValue := Canvas.TextHeight('Wq') + tm.tmInternalLeading + tm.tmExternalLeading;
    ARect := PrintRect;
    OffsetRect(ARect,1,1);
    ARect.Bottom := ARect.Top + IncValue + 1;
    for i := FCurrentRow to FStrings.Count - 1 do
    begin
      DrawText(Canvas.Handle,PChar(FStrings[i]),-1, ARect, DT_NOPREFIX or DT_EXPANDTABS or DT_SINGLELINE or DT_LEFT);
      OffsetRect(ARect,0,IncValue);
      if ARect.Bottom > PrintRect.Bottom then
      begin
        FPreview.Add;
        FCurrentRow := i + 1;
        Exit;
      end;
    end;
    FFinished := true;
  end;
end;

procedure TJvStringsPreviewRenderer.CreatePreview;
begin
  FPreview.Clear;
  FPreview.OnAddPage := DoAddPage;
  FFinished := false;
  FCurrentRow := 0;
  if FStrings.Count > 0 then
    FPreview.Add
  else
  FFinished := true;
end;

destructor TJvStringsPreviewRenderer.Destroy;
begin
  FStrings.Free;
  inherited;
end;

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
  cbPreview.ItemIndex := 0;
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
    pd.DeviceInfo.ReferenceHandle := 0;  // reset to default (screen)
  1:
    pd.DeviceInfo.ReferenceHandle := Canvas.Handle; // should give same result as Screen
  2:
    pd.DeviceInfo.ReferenceHandle := Printer.Handle;
  end;
end;

procedure TForm1.Open1Click(Sender: TObject);
var S:TStringlist;
begin
  with TOpenDialog.Create(nil) do
  try
    if Execute then
    begin
      S := TStringlist.Create;
      try
        S.LoadFromFile(Filename);
        with TJvStringsPreviewRenderer.Create(pd,S) do
        try
          while not Finished do Application.ProcessMessages;
        finally
          Free;
        end;
      finally
        S.Free;
      end;
    end;
  finally
    Free;
  end;
end;

end.
