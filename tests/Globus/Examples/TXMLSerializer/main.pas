unit main;
{ the Example shows saving/loading of a class into/from XML.
Before saving, the class is initialized by the test data.

coded by Xelby, 09.2001
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvgXMLSerializer, JvComponent;

type
  TfglXMLSerializerDemo = class(TForm)
    bLoadXML: TButton;
    bSaveXML: TButton;
    JvgXMLSerializer: TJvgXMLSerializer;
    Memo1: TMemo;
    eTestFileName: TEdit;
    Label1: TLabel;
    bViewXML: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bLoadXMLClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bSaveXMLClick(Sender: TObject);
    procedure bViewXMLClick(Sender: TObject);
    procedure glXMLSerializerGetXMLHeader(Sender: TObject;
      var Value: String);
  private
    sTestFileName: string; // File to be saved to XML
  public
    { Public declarations }
  end;

var
  fglXMLSerializerDemo: TfglXMLSerializerDemo;

implementation
uses testClasses, ShellApi;
{$R *.DFM}

var
    Catalogue: TCatalogue; // the class, which we will preserve and load

procedure TfglXMLSerializerDemo.FormCreate(Sender: TObject);
var
  i: integer;
begin
  {  we will use the local file  test.xml}
  sTestFileName := ExtractFilePath(ParamStr(0)) + 'test.xml';
  eTestFileName.Text := sTestFileName;

  { TCatalogue - class which we will test}

  Catalogue := TCatalogue.Create(self);
end;

procedure TfglXMLSerializerDemo.FormDestroy(Sender: TObject);
begin
  Catalogue.Free;
end;




{ Save object into XML }
procedure TfglXMLSerializerDemo.bSaveXMLClick(Sender: TObject);
var
  fs: TFileStream;
  i: integer;
begin

  //We fill object with the any data

  Catalogue.Header := 'Catalog of descriptions of book news';
  for i := 1 to 30 do
  with Catalogue.Documents.Add do
  begin
    DocIndex := i;
    Title := 'Title ' + IntToStr(i);
    Author := 'Author ' + IntToStr(i);
    PublicDate :=DateTimeToStr(now);
  end;
  Catalogue.Footer := 'Created ' + DateToStr(date);

  { сохраняем }
  try
    fs := TFileStream.Create( sTestFileName, fmCreate);
    JvgXMLSerializer.Serialize(Catalogue, fs);
  finally
    fs.Free;
  end;

  ShowMessage('Object is saved to the file ');
end;



{ Initialization of object from XML }
procedure TfglXMLSerializerDemo.bLoadXMLClick(Sender: TObject);
var
  fs: TFileStream;
begin
  Catalogue.Documents.Clear;
  try
    fs := TFileStream.Create( sTestFileName, fmOpenRead);
    JvgXMLSerializer.DeSerialize(Catalogue, fs);
  finally
    fs.Free;
  end;
  ShowMessage('Object is loaded. Запией: ' + IntToStr(Catalogue.Documents.Count));
end;

{ Open in browser}
procedure TfglXMLSerializerDemo.bViewXMLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(sTestFileName), 0, 0, SW_SHOW);
end;


{ Standard title with the indication of coding for the Russian letters}
procedure TfglXMLSerializerDemo.glXMLSerializerGetXMLHeader(Sender: TObject; var Value: String);
begin
  Value := '<?xml version="1.0" encoding="windows-1251"?>';
end;

end.
