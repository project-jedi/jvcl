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
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls, QGrids,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, Grids,
{$ENDIF}
  SysUtils, Classes, JvUIB, RemoteObject_UIB;

type
  TMainForm = class(TForm)
    Button1: TButton;
    StringGrid: TStringGrid;
    Button2: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation
uses JvUIBLib;

{$R *.dfm}
var ClientObj: TProxyRemoteObject;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ClientObj := TProxyRemoteObject.Create;
  ClientObj.Host := 'localhost';
  ClientObj.Port := 9545;
  ClientObj.Active := true;
  ClientObj.ClassID := CLSID_RemoteObject
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ClientObj.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Stream: TMemoryStream;
  SQLResult: TSQLResult;
  i, j: Integer;
begin
  Stream := TMemoryStream.Create;
  SQLResult := TSQLResult.Create;
  try
    ClientObj.OpenQuery('select * from project', Stream);
    SQLResult.LoadFromStream(Stream);

    StringGrid.ColCount := SQLResult.FieldCount  + 1;
    StringGrid.RowCount := SQLResult.RecordCount + 1;

    for i := 1 to SQLResult.FieldCount do
      StringGrid.Cells[i, 0] := SQLResult.AliasName[i-1];

    for i := 1 to SQLResult.RecordCount do
    begin
      SQLResult.GetRecord(i-1);
      StringGrid.Cells[0, i] := Inttostr(i);
      for j := 1 to SQLResult.FieldCount do
        StringGrid.Cells[j, i] := SQLResult.AsString[j-1];
    end;
  finally
     Stream.Free;
     SQLResult.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var Count: Integer;
begin
  ClientObj.GetEmployeeCount(Count);
  Label1.Caption := IntToStr(Count);
end;

end.
