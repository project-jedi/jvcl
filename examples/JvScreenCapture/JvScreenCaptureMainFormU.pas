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

unit JvScreenCaptureMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ClipBrd, Buttons;

type
  TJvScreenCaptureMainForm = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Button1: TBitBtn;
    Button2: TBitBtn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  JvScreenCaptureMainForm: TJvScreenCaptureMainForm;

implementation

{$R *.DFM}

uses
  JclGraphics;

procedure TJvScreenCaptureMainForm.Button1Click(Sender: TObject);
var
  B: TBitmap;
begin
   b := TBitmap.Create;
   ScreenShot(B);
   Self.Image1.Picture.Bitmap.Assign(B);
   B.Free;
end;

procedure TJvScreenCaptureMainForm.Button2Click(Sender: TObject);
var
  AFormat: Word;
  AData: THandle;
  APalette: HPALETTE;
begin
   Self.Image1.Picture.Bitmap.SaveToClipboardFormat(AFormat, AData, APalette);
   ClipBoard.SetAsHandle(AFormat, AData);
end;

end.
