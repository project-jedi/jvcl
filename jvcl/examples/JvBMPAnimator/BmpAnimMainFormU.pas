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

unit BmpAnimMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  ExtCtrls, Menus, ComCtrls, StdCtrls, ImgList, JvComponent, JvBmpAnimator,
  JvExControls;

type
  TBmpAnimMainForm = class(TForm)
    Edit1: TEdit;
    UpDown1: TUpDown;
    OnOff: TButton;
    BmpAnimator1: TJvBmpAnimator;
    Edit2: TEdit;
    UpDown2: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    ImageList1: TImageList;
    Transparent: TCheckBox;
    procedure OnOffClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure TransparentClick(Sender: TObject);
  end;

var
  BmpAnimMainForm: TBmpAnimMainForm;

implementation

{$R *.dfm}

procedure TBmpAnimMainForm.OnOffClick(Sender: TObject);
begin
  with BmpAnimator1 do
  begin
    Active := not Active;
    if not Active then
      Position := 0;
  end;
end;

procedure TBmpAnimMainForm.Edit1Change(Sender: TObject);
begin
  if not BmpAnimator1.Active then
     BmpAnimator1.Position := UpDown1.Position;
end;

procedure TBmpAnimMainForm.Edit2Change(Sender: TObject);
begin
  try
    BmpAnimator1.Speed := StrToInt(Edit2.Text);
  except
    BmpAnimator1.Speed := 15;
  end;
end;

procedure TBmpAnimMainForm.TransparentClick(Sender: TObject);
begin
  BmpAnimator1.Transparent := Transparent.Checked;
end;

end.
