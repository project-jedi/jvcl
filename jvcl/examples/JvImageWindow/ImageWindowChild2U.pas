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

unit ImageWindowChild2U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ImgList, JvImageWindow, JvComponent;

type
  TImageWindowChild2 = class(TForm)
    Panel1: TPanel;
    ImageWindow1: TJvImageWindow;
    ImageList1: TImageList;
    Label1: TLabel;
    procedure ImageWindow1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  ImageWindowChild2: TImageWindowChild2;

implementation

{$R *.DFM}

procedure TImageWindowChild2.ImageWindow1Click(Sender: TObject);
begin
  Caption := Format('Clicked image %d (ESC to quit)',[ImageWindow1.ImageIndex]);
end;

procedure TImageWindowChild2.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

end.
