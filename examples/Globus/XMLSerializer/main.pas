{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit main;

{ the Example shows saving/loading of a class into/from XML.
Before saving, the class is initialized by the test data.

coded by Xelby, 09.2001 }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  JvgXMLSerializer, JvComponent;

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
  end;

var
  fglXMLSerializerDemo: TfglXMLSerializerDemo;

implementation

uses
  testClasses, ShellApi;

{$R *.dfm}

var
  Catalogue: TCatalogue; // the class, which we will preserve and load

procedure TfglXMLSerializerDemo.FormCreate(Sender: TObject);
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
  fs := TFileStream.Create( sTestFileName, fmCreate);
  try
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
  fs := TFileStream.Create( sTestFileName, fmOpenRead);
  try
    JvgXMLSerializer.DeSerialize(Catalogue, fs);
  finally
    fs.Free;
  end;
  ShowMessage('Object is loaded. Count: ' + IntToStr(Catalogue.Documents.Count));
end;

{ Open in browser}

procedure TfglXMLSerializerDemo.bViewXMLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(sTestFileName), nil, nil, SW_SHOW);
end;

{ Standard title with the indication of coding for the Russian letters}

procedure TfglXMLSerializerDemo.glXMLSerializerGetXMLHeader(Sender: TObject; var Value: String);
begin
  Value := '<?xml version="1.0" encoding="windows-1251"?>';
end;

end.
