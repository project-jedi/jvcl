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

unit Main;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, JvQColorBox, QMenus, QButtons, QExtCtrls,
  JvQComponent, JvQExControls, JvQColorButton;

type
  TColorDemoMainForm = class(TForm)
    ColorButton1: TJvColorButton;
    procedure ColorButton1Change(Sender: TObject);
  public
  end;

var
  ColorDemoMainForm: TColorDemoMainForm;

implementation

uses JvQColorForm;

{$R *.xfm}


procedure TColorDemoMainForm.ColorButton1Change(Sender: TObject);
begin
  ColorDemoMainForm.Color := ColorButton1.Color;
end;

end.
