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

unit jvanimatedappicondemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, JvComponent, JvAppAnimatedIcon, StdCtrls;

type
  TfrAnimatedApplicationicon = class(TForm)
    JvAppAnimatedIcon1: TJvAppAnimatedIcon;
    ImageList1: TImageList;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;


implementation

{$R *.dfm}

procedure TfrAnimatedApplicationicon.Button1Click(Sender: TObject);
begin
 if JvAppAnimatedIcon1.Tag = 0 then
 begin
   JvAppAnimatedIcon1.Active := true;
   JvAppAnimatedIcon1.Tag := 1;
   Button1.Caption := 'stop that now!'
 end
 else
 begin
   JvAppAnimatedIcon1.Active := false;
   JvAppAnimatedIcon1.Tag := 0;
   Button1.Caption := 'start the animation!'
 end;
end;

end.
