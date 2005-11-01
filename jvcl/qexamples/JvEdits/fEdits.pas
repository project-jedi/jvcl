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

unit fEdits;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QMask,
  JvQEdit, JvQToolEdit, JvQBaseEdits, JvQExMask, QComboEdits,
  JvQExComboEdits, JvQMaskEdit, JvQCheckedMaskEdit, JvQAutoComplete,
  JvQExStdCtrls;

type
  TForm1 = class(TForm)
    JvFileNameBox1: TJvFilenameEdit;
    JvDirectoryBox1: TJvDirectoryEdit;
    JvDateEdit1: TJvDateEdit;
    JvButtonBox1: TJvComboEdit;
    JvCalcEdit1: TJvCalcEdit;
    JvCheckedMaskEdit1: TJvCheckedMaskEdit;
    JvEdit1: TJvEdit;
    JvLookupAutoComplete1: TJvLookupAutoComplete;
    procedure JvButtonBox1ButtonClick(Sender: TObject);
    procedure FormLoaded(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.JvButtonBox1ButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked');
end;

procedure TForm1.FormLoaded(Sender: TObject);
begin
  JvCalcEdit1.BorderStyle := bsRaised3D;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  JvFilenameBox1.BorderStyle := bsNone;
  JvCheckedMaskEdit1.BorderStyle := bsDouble;
  JvDateEdit1.BorderStyle := bsRaisedPanel;
end;

end.
