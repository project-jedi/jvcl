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

unit ContentScrollerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvGIF, ExtCtrls, JvCntScr, JvComponent, JvCaptionPanel,
  JvContentScroller;

type
  TContentScrollerMainForm = class(TForm)
    ContentScroller1: TJvContentScroller;
    Label1: TLabel;
    Image11: TImage;
    chkGoDown: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  ContentScrollerMainForm: TContentScrollerMainForm;

implementation

{$R *.dfm}

procedure TContentScrollerMainForm.Button1Click(Sender: TObject);
begin
  if chkGoDown.Checked then
    ContentScroller1.ScrollDirection := sdDown
  else
    ContentScroller1.ScrollDirection := sdUp;
  ContentScroller1.Active := not ContentScroller1.Active;
end;

end.
