{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit ArrowButtonMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QMenus, QExtCtrls, QButtons, QComCtrls, QStdCtrls, QImgList,
  JvQComponent, JvQExControls, JvQArrowButton, QTypes;

type
  TArrowButtonMainForm = class(TForm)
    PopupMenu1: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Edit1: TMenuItem;
    Replace1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    ArrowButton1: TJvArrowButton;
    ArrowButton2: TJvArrowButton;
    ImageList1: TImageList;
    procedure Close1Click(Sender: TObject);
  private
  public
  end;

var
  ArrowButtonMainForm: TArrowButtonMainForm;

implementation

{$R *.xfm}

procedure TArrowButtonMainForm.Close1Click(Sender: TObject);
begin
  Close;
end;

end.
