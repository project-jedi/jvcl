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

unit JvQDataEmbeddedMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, JvQDataEmbedded, JvQComponent;

type
  TJvDataEmbeddedMainForm = class(TForm)
    JvDataEmbedded1: TJvDataEmbedded;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  end;

var
  JvDataEmbeddedMainForm: TJvDataEmbeddedMainForm;

implementation

{$R *.xfm}

procedure TJvDataEmbeddedMainForm.Button1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromStream(JvDataEmbedded1.Data);
end;

end.
