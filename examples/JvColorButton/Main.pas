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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvColorBox, Menus, Buttons,ExtCtrls, 
  JvComponent, JvExControls, JvColorButton;

type
  TForm2 = class(TForm)
    ColorButton1: TJvColorButton;
    procedure ColorBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorBox1ColorClick(Sender: TObject; Button: TMouseButton;
      Color: TColor);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses JvColorForm;

{$R *.DFM}


procedure TForm2.ColorBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     if Button = mbLeft then
       Caption := Caption + ' Left '
     else
       Caption := Caption + ' Right '
end;


procedure TForm2.ColorBox1ColorClick(Sender: TObject; Button: TMouseButton;
  Color: TColor);
begin
  Caption := ColorToString(Color);

end;

end.
