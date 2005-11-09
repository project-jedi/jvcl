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

unit JvClipboardViewerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,
  JvComponent, JvClipboardViewer, JvExForms;

type
  TJvClipboardViewerMainForm = class(TForm)
    JvClipboardViewer1: TJvClipboardViewer;
    Label1: TLabel;
    Memo1: TMemo;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; AText: string);
  end;

var
  JvClipboardViewerMainForm: TJvClipboardViewerMainForm;

implementation

{$R *.dfm}

procedure TJvClipboardViewerMainForm.JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
begin
  Image1.Picture.Bitmap.Assign(Image);
end;

procedure TJvClipboardViewerMainForm.JvClipboardViewer1Text(Sender: TObject; AText: string);
begin
  Memo1.Lines.Text := AText;
end;

end.

