{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, ExtCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, JvComponent;

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
      Query.SQL.Text := 'UPDATE TBLOB SET STREAM = :blob';
      Query.ParamsSetBlob('blob', Stream);
      Query.ExecSQL;
    finally
      Stream.Free;
    end;
    Query.Close(etmCommit);
  end;
end;

end.
