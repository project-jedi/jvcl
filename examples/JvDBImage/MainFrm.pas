{$I jvcl.inc}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvDBImage, ExtCtrls, DBCtrls, Grids, DBGrids, DB, DBClient,
  jpeg, JvGIF, JvPCX, JvAni, ExtDlgs, StdCtrls;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    DBGrid1: TDBGrid;
    ClientDataSet1Filename: TStringField;
    ClientDataSet1Image: TGraphicField;
    btnAdd: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    ClientDataSet1FileType: TStringField;
    ScrollBox1: TScrollBox;
    btnClear: TButton;
    chkTransparent: TCheckBox;
    chkStretch: TCheckBox;
    chkProportional: TCheckBox;
    chkAutoDisplay: TCheckBox;
    chkAutoSize: TCheckBox;
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
    JDB:TJvDBImage;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientDataSet1.Open;
  ClientDataSet1.LogChanges := False;
  JDB := TJvDBImage.Create(Self);
  with JDB do
  begin
    Parent := ScrollBox1;
    Align := alClient;
    DataSource := DataSource1;
    Proportional := True;
    DataField := 'Image';

    chkProportional.Checked := Proportional;
    chkStretch.Checked := Stretch;
    chkTransparent.Checked := Transparent;
    chkAutoDisplay.Checked := AutoDisplay;
    chkAutoSize.Checked := AutoSize;
  end;
end;

procedure TForm1.AddImage(const Filename:string);
begin
  ClientDataSet1.Append;
  JDB.Picture.LoadFromFile(Filename);
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
  JDB.Transparent := chkTransparent.Checked;
end;

procedure TForm1.chkStretchClick(Sender: TObject);
begin
  JDB.Stretch := chkStretch.Checked;
end;

procedure TForm1.chkProportionalClick(Sender: TObject);
begin
  JDB.Proportional := chkProportional.Checked;
end;

procedure TForm1.chkAutoDisplayClick(Sender: TObject);
begin
  JDB.AutoDisplay := chkAutoDisplay.Checked;
end;

procedure TForm1.chkAutoSizeClick(Sender: TObject);
begin
  JDB.AutoSize := chkAutoSize.Checked;
  if not JDB.AutoSize then
    JDB.Align := alClient
  else
    JDB.Align := alNone;
end;

initialization
  // we register the JVCL image support components so we can store them in the database:
  RegisterGraphicSignature([$0A], 0, TJvPcx);
  RegisterGraphicSignature('RIFF', 0, TJvAni);
  RegisterGraphicSignature('GIF', 0, TJvGIFImage);

end.
