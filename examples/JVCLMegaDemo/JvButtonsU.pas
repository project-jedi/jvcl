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

unit JvButtonsU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvGIF, ExtCtrls, JvImage, ImgList, ComCtrls, JvComponent,
  StdCtrls, JvButton, Buttons, JvBitBtn, JvExButtons,
  JvExExtCtrls, JvArrowButton, JvExControls, JvTransparentButton;

type
  TJvButtons = class(TForm)
    JvTransparentButton21: TJvTransparentButton2;
    JvArrowButton1: TJvArrowButton;
    ilTreeview: TImageList;
    JvImageJEDI: TJvImage;
    CheckBoxImage: TCheckBox;
    JvBitBtn1: TJvBitBtn;
    Label1: TLabel;
    procedure CheckBoxImageClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

implementation

uses
  Unitmain;

{$R *.dfm}

procedure TJvButtons.CheckBoxImageClick(Sender: TObject);
begin
 JvImageJEDI.visible := CheckBoxImage.Checked;
end;

procedure TJvButtons.Button1Click(Sender: TObject);
begin
  Mainform.CreateDemoForm(63);
end;

end.
