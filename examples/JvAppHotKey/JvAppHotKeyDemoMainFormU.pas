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

unit JvAppHotKeyDemoMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvAppHotKey, StdCtrls, ComCtrls;

type
  TJvAppHotKeyDemoMainForm = class(TForm)
    HotKey1: THotKey;
    Label1: TLabel;
    btnAdd: TButton;
    lbHotKeys: TListBox;
    Label2: TLabel;
    procedure btnAddClick(Sender: TObject);
  private
    procedure DoHotKey(Sender: TObject);
  end;

var
  JvAppHotKeyDemoMainForm: TJvAppHotKeyDemoMainForm;

implementation

uses
  Menus;

{$R *.dfm}

procedure TJvAppHotKeyDemoMainForm.DoHotKey(Sender:TObject);
begin
  Application.BringToFront;
  ShowMessage(Format('HotKey "%s" pressed!',[ShortCutToText((Sender as TJvApplicationHotKey).HotKey)]));
end;

procedure TJvAppHotKeyDemoMainForm.btnAddClick(Sender: TObject);
var S:string;
begin
  S := ShortCutToText(HotKey1.HotKey);
  if lbHotKeys.Items.IndexOf(S) > -1 then
  begin
    ShowMessage('Hot key already assigned!');
    Exit;
  end;
  with TJvApplicationHotKey.Create(self) do
  begin
    HotKey := HotKey1.HotKey;
    Active := true;
    OnHotKey := DoHotKey;
    lbHotKeys.Items.Add(S);
  end;
end;

end.
