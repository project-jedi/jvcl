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

unit fThread;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvThread, JvComponent;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    JvThread1: TJvThread;
    JvThread2: TJvThread;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    procedure JvThread1Execute(Sender: TObject; params: Pointer);
    procedure Button1Click(Sender: TObject);
    procedure JvThread2Execute(Sender: TObject; params: Pointer);
    procedure Button2Click(Sender: TObject);
  private
  public
    Value: Integer;
    Value2: Integer;
    procedure Stats1(Sender: TObject);
    procedure Stats2(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.JvThread1Execute(Sender: TObject; params: Pointer);
var
 i,j,k: Integer;
begin
  //Do the job here
  k := 0;
  for i:=0 to 1000000 do
    for j:=0 to 1000000 do
      begin
        inc(k);

        //To use global variable/objects, you have to synchronize (to avoid conflicts)
        TForm1(params).Value := k;
        Synchronize(TForm1(params).Stats1);
      end;
end;

procedure TForm1.JvThread2Execute(Sender: TObject; params: Pointer);
var
 i,j,k: Integer;
begin
  //Do the job here
  k := 0;
  for i:=0 to 1000000 do
    for j:=0 to 1000000 do
      begin
        inc(k,5); //This is the only difference with the other thread

        //To use global variable/objects, you have to synchronize (to avoid conflicts)
        TForm1(params).Value2 := k;
        Synchronize(TForm1(params).Stats2);
      end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvThread1.Execute(self);
  (Sender as TButton).Enabled := false;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JvThread2.Execute(self);
  (Sender as TButton).Enabled := false;
end;

procedure TForm1.Stats1(Sender: TObject);
begin
  Label2.Caption := IntToStr(Value);
end;

procedure TForm1.Stats2(Sender: TObject);
begin
  Label4.Caption := IntToStr(Value2);
end;

end.
