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

unit ImageWindowChild1U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, JvImageWindow, JvComponent, JvExControls,
  JvImageSquare;

type
  TImageWindowChild1 = class(TForm)
    ImageList1: TImageList;
    ImageBox1: TJvImageSquare;
    ImageBox2: TJvImageSquare;
    ImageBox3: TJvImageSquare;
    ImageBox4: TJvImageSquare;
    ImageBox5: TJvImageSquare;
    ImageBox6: TJvImageSquare;
    ImageBox7: TJvImageSquare;
    ImageBox8: TJvImageSquare;
    Panel1: TPanel;
    procedure ImageBox1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    procedure ImageBox1MouseEnter(Sender: TObject);
  end;

var
  ImageWindowChild1: TImageWindowChild1;

implementation

{$R *.DFM}

procedure TImageWindowChild1.ImageBox1Click(Sender: TObject);
begin
{ this is the OnClick handler for all TImageBoxes on this Form }
   with Sender as TJvImageSquare do
     Panel1.Caption := Format('Clicked image %d ',[ImageIndex]);
end;

procedure TImageWindowChild1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

procedure TImageWindowChild1.FormActivate(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
   if Components[i] is TJvImageSquare then
   with Components[i] as TJvImageSquare do
   begin
     Hint := Text;
     ShowHint := True;
   end;
end;

procedure TImageWindowChild1.Panel1DblClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
   if Components[i] is TJvImageSquare then
     TJvImageSquare(Components[i]).ShowClick := not TJvImageSquare(Components[i]).ShowClick;
end;

procedure TImageWindowChild1.ImageBox1MouseEnter(Sender: TObject);
begin
{ this is the OnEnter handler for all TImageBoxes on this Form }
   with Sender as TJvImageSquare do
     Panel1.Caption := Text;
end;

end.
