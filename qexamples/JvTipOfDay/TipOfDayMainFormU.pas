{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

{$I jvcl.inc}

unit TipOfDayMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, JvQComponent, JvQBaseDlg, JvQTipOfDay;

type
  TTipOfDayMainForm = class(TForm)
    JvTip: TJvTipOfDay;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    cbStyle: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FileDirectory: string;
  end;

var
  TipOfDayMainForm: TTipOfDayMainForm;

implementation

{$R *.xfm}

procedure TTipOfDayMainForm.Button2Click(Sender: TObject);
begin
  JvTip.Style := TJvTipOfDayStyle(cbStyle.ItemIndex);
  JvTip.Execute;
  if toShowOnStartUp in JvTip.Options then
    ShowMessage('You want to see tips the next time the application is started')
  else
    ShowMessage('You don''t want to see tips the next time the application is started')
end;

procedure TTipOfDayMainForm.Button1Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := FileDirectory;
  if OpenDialog1.Execute then
    JvTip.LoadFromFile(OpenDialog1.FileName);
end;

procedure TTipOfDayMainForm.Button3Click(Sender: TObject);
begin
  SaveDialog1.InitialDir := FileDirectory;
  if SaveDialog1.Execute then
    JvTip.SaveToFile(SaveDialog1.FileName);
end;

procedure TTipOfDayMainForm.FormCreate(Sender: TObject);
begin
  FileDirectory := ExtractFileDir(Application.ExeName);
  cbStyle.ItemIndex := 0;
end;

end.

