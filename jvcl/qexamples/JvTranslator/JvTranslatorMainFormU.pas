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

unit JvTranslatorMainFormU;

interface

uses
  QWindows, QMessages, SysUtils,  Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, JvQTranslator, QStdCtrls, QComCtrls, JvQComponent;

type
  TJvTranslatorMainForm = class(TForm)
    TreeView1: TTreeView;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    JvTranslator1: TJvTranslator;
    Variables: TJvTranslatorStrings;
    Button3: TButton;
    JvTranslator2: TJvTranslator;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  JvTranslatorMainForm: TJvTranslatorMainForm;

implementation

uses
  JclStrings;

{$R *.xfm}

var
 CONST_SomeText: string = 'Wooow, this was good :p';


procedure TJvTranslatorMainForm.FormCreate(Sender: TObject);
begin
  Variables.Add('SomeText',CONST_SomeText);
end;

procedure TJvTranslatorMainForm.Button3Click(Sender: TObject);
begin
  ShowMessage(CONST_SomeText);
end;

procedure TJvTranslatorMainForm.Button1Click(Sender: TObject);
var
  transFileName : string;
begin

  {$IFDEF MSWINDOWS}
  transFileName := '..\JvTranslator\Translations\French.xml';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  transFileName := '../JvTranslator/Translations/French.xml';
  {$ENDIF LINUX}
  if not FileExists(transFileName) then
    MessageDlg('File not found: ' + transFileName, mtError, [mbOK], 0)
  else
    JvTranslator1.Translate(transFileName);
end;

procedure TJvTranslatorMainForm.Button2Click(Sender: TObject);
var
  transFileName : string;
begin
  {$IFDEF MSWINDOWS}
  transFileName := '..\JvTranslator\Translations\English.xml';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  transFileName := '../JvTranslator/Translations/English.xml';
  {$ENDIF LINUX}
  if not FileExists(transFileName) then
    MessageDlg('File not found: ' + transFileName, mtError, [mbOK], 0)
  else
    JvTranslator1.Translate(transFileName);
end;

end.
