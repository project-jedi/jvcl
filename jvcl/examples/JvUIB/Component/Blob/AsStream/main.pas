unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, ExtCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB;

type
  TMainForm = class(TForm)
    Image: TImage;
    LoadImage: TButton;
    SaveImage: TButton;
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    OpenDialog: TOpenDialog;
    procedure LoadImageClick(Sender: TObject);
    procedure SaveImageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses Math;

{$R *.dfm}

procedure TMainForm.LoadImageClick(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Query.SQL.Text := 'Select Stream from TBLOB';
  Query.Params.Clear;
  Query.Open;
  Stream := TMemoryStream.Create;
  try
    Query.ReadBlob('STREAM', Stream);
    Image.Picture.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMainForm.SaveImageClick(Sender: TObject);
var Stream: TFileStream;
begin
  If OpenDialog.Execute then
  begin
    Stream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    try
      Query.SQL.Text := 'UPDATE TBLOB SET STREAM = ?';
      Query.ParamsSetBlob(0, Stream);
      Query.ExecSQL;
    finally
      Stream.Free;
    end;
    Query.Close(etmCommit);
  end;
end;

end.
