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

unit JvSystemPopupMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, JvSystemPopup, JvTypes, JvComponent;

type
  TJvSystemPopupMainForm = class(TForm)
    JvSystemPopup1: TJvSystemPopup;
    PopupMenu1: TPopupMenu;
    firstone1: TMenuItem;
    secondone1: TMenuItem;
    thirdone1: TMenuItem;
    RadioGroup1: TRadioGroup;
    SubMenu11: TMenuItem;
    SubMenu12: TMenuItem;
    SubMenu21: TMenuItem;
    SubMenu31: TMenuItem;
    N1: TMenuItem;
    SubMenu22: TMenuItem;
    SubMenuItem11: TMenuItem;
    SubMenuItem21: TMenuItem;
    SubMenuItem31: TMenuItem;
    SubMenuItem41: TMenuItem;
    SubMenuItem51: TMenuItem;
    SubMenuItem1: TMenuItem;
    SubMenuItem71: TMenuItem;
    SubMenuItem81: TMenuItem;
    SubMenuItem91: TMenuItem;
    procedure firstone1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  end;

var
  JvSystemPopupMainForm: TJvSystemPopupMainForm;

implementation

{$R *.DFM}

procedure TJvSystemPopupMainForm.firstone1Click(Sender: TObject);
begin
  showmessage((sender as TMEnuItem).caption);
end;

procedure TJvSystemPopupMainForm.RadioGroup1Click(Sender: TObject);
begin
  case self.RadioGroup1.itemindex of
    0 : self.JvSystemPopup1.Position := ppNone;
    1 : self.JvSystemPopup1.Position := ppApplication;
    2 : self.JvSystemPopup1.Position := ppForm;
  end;
end;

end.
