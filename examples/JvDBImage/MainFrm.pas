{$I jvcl.inc}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvDBImage, ExtCtrls, DBCtrls, Grids, DBGrids, DB, DBClient,
  jpeg, JvGIF, JvPCX, JvAni, ExtDlgs, StdCtrls, JvExDBGrids, JvDBGrid;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    DBGrid1: TJvDBGrid;
    ClientDataSet1Filename: TStringField;
    ClientDataSet1Image: TGraphicField;
    btnAdd: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    ClientDataSet1FileType: TStringField;
    btnClear: TButton;
    chkTransparent: TCheckBox;
    chkStretch: TCheckBox;
    chkProportional: TCheckBox;
    chkAutoDisplay: TCheckBox;
    chkAutoSize: TCheckBox;
    JvDBImage1: TJvDBImage;
    ScrollBox1:TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure ClientDataSet1BeforeOpen(DataSet: TDataSet);
    procedure chkTransparentClick(Sender: TObject);
    procedure chkStretchClick(Sender: TObject);
    procedure chkProportionalClick(Sender: TObject);
    procedure chkAutoDisplayClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
  private
    procedure AddImage(const Filename: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientDataSet1.Open;
  ClientDataSet1.LogChanges := False;
  chkProportional.Checked := JvDBImage1.Proportional;
  chkStretch.Checked := JvDBImage1.Stretch;
  chkTransparent.Checked := JvDBImage1.Transparent;
  chkAutoDisplay.Checked := JvDBImage1.AutoDisplay;
  chkAutoSize.Checked := JvDBImage1.AutoSize;
end;

procedure TForm1.AddImage(const Filename:string);
begin
  ClientDataSet1.Append;
  JvDBImage1.Picture.LoadFromFile(Filename);
  ClientDataSet1.FieldByName('Filename').AsString := ExtractFileName(Filename);
  ClientDataSet1.FieldByName('FileType').AsString := AnsiUpperCase(Copy(ExtractFileExt(Filename), 2, MaxInt));
  ClientDataSet1.Post;
end;

procedure TForm1.btnAddClick(Sender: TObject);
var i:integer;
begin
  with OpenPictureDialog1 do
  if Execute then
  begin
    // don't disable the db controls! the actual loading of the image into
    // the control is what stores it in the table!
//    ClientDataSet1.DisableControls;
    try
      for i := 0 to Files.Count - 1 do
        AddImage(Files[i]);
    finally
//      ClientDataSet1.EnableControls;
    end;
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  while not ClientDataSet1.EOF do
    ClientDataSet1.Delete;
end;

procedure TForm1.ClientDataSet1BeforeOpen(DataSet: TDataSet);
begin
  ClientDataSet1.Filename := ExpandUNCFilename(ClientDataSet1.Filename);
end;

procedure TForm1.chkTransparentClick(Sender: TObject);
begin
  JvDBImage1.Transparent := chkTransparent.Checked;
end;

procedure TForm1.chkStretchClick(Sender: TObject);
begin
  JvDBImage1.Stretch := chkStretch.Checked;
end;

procedure TForm1.chkProportionalClick(Sender: TObject);
begin
  JvDBImage1.Proportional := chkProportional.Checked;
end;

procedure TForm1.chkAutoDisplayClick(Sender: TObject);
begin
  JvDBImage1.AutoDisplay := chkAutoDisplay.Checked;
end;

procedure TForm1.chkAutoSizeClick(Sender: TObject);
begin
  JvDBImage1.AutoSize := chkAutoSize.Checked;
  if not JvDBImage1.AutoSize then
    JvDBImage1.Align := alClient
  else
    JvDBImage1.Align := alNone;
end;

initialization
  // we register the JVCL image support components so
  // we can store them in the database:
  RegisterGraphicSignature([$0A], 0, TJvPcx);
  RegisterGraphicSignature('ACON', 8, TJvAni);
  RegisterGraphicSignature('GIF', 0, TJvGIFImage);

end.
