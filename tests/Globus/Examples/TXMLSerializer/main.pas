unit main;
{
Пример демонстрирует сохранение класса в XML и загрузку данных из XML.

Перед сохранением класс инициализируется тестовыми данными.


coded by Xelby, 09.2001
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvgXMLSerializer;

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
    sTestFileName: string; // файл лдя сохранения XML
  public
    { Public declarations }
  end;

var
  fglXMLSerializerDemo: TfglXMLSerializerDemo;

implementation
uses testClasses, ShellApi;
{$R *.DFM}

var
    Catalogue: TCatalogue; // класс, который будем сохранять и загружать

procedure TfglXMLSerializerDemo.FormCreate(Sender: TObject);
var
  i: integer;
begin
  { будем использовать локальный файл test.xml }
  sTestFileName := ExtractFilePath(ParamStr(0)) + 'test.xml';
  eTestFileName.Text := sTestFileName;

  { TCatalogue - класс, на котором будем тренироваться }
  Catalogue := TCatalogue.Create(self);
end;

procedure TfglXMLSerializerDemo.FormDestroy(Sender: TObject);
begin
  Catalogue.Free;
end;




{ Сохранени объекта в XML }
procedure TfglXMLSerializerDemo.bSaveXMLClick(Sender: TObject);
var
  fs: TFileStream;
  i: integer;
begin

  { Наполняем объект произвольными данными }

  Catalogue.Header := 'Каталог описаний книжных новинок';
  for i := 1 to 30 do
  with Catalogue.Documents.Add do
  begin
    DocIndex := i;
    Title := 'Title ' + IntToStr(i);
    Author := 'Author ' + IntToStr(i);
    PublicDate :=DateTimeToStr(now);
  end;
  Catalogue.Footer := 'Создано ' + DateToStr(date);

  { сохраняем }
  try
    fs := TFileStream.Create( sTestFileName, fmCreate);
    JvgXMLSerializer.Serialize(Catalogue, fs);
  finally
    fs.Free;
  end;

  ShowMessage('Объект сохранен в файл');
end;



{ Инициализация объекта из XML }
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
  ShowMessage('Объект загружен. Запией: ' + IntToStr(Catalogue.Documents.Count));
end;

{ Открытие в браузере }
procedure TfglXMLSerializerDemo.bViewXMLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(sTestFileName), 0, 0, SW_SHOW);
end;


{ Стандартный заголовок с указанием кодировки для русских букв }
procedure TfglXMLSerializerDemo.glXMLSerializerGetXMLHeader(Sender: TObject; var Value: String);
begin
  Value := '<?xml version="1.0" encoding="windows-1251"?>';
end;

end.
