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
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, SyncObjs, JvComponent;

type
  TForm1 = class(TForm)
    DataBase: TJvUIBDataBase;
    Button1: TButton;
    Transaction: TJvUIBTransaction;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  TMyThread = class(TThread)
  public
    procedure Execute; override;
    destructor destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 49 do
    TMyThread.Create(False);
end;

var
  x: integer = 0;

{ TMyThread }

destructor TMyThread.destroy;
begin
  beep;
  inherited;
end;

procedure TMyThread.Execute;
var
  Query: TJvUIBQuery;
begin
  FreeOnTerminate := true;
  //Form1.DataBase.Lock; //simulate single thread
  try
    Query := TJvUIBQuery.Create(nil);
    try
      Query.Transaction := Form1.Transaction;
      Query.FetchBlobs := True;
      Query.SQL.Text := 'select * from project';
      Query.Open;
      while not Query.EOF do
      begin
        Query.Next;
        Sleep(10); // simulate activity
      end;
    finally
      Query.Free;
    end;
  finally
    //Form1.DataBase.UnLock; //simulate single thread
  end;
end;

end.
