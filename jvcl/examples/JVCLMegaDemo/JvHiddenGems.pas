{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [Ralf dot Grenzing@gmx.de]

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

unit JvHiddenGems;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvDataEmbedded, StdCtrls, JvExStdCtrls, JvRichEdit, ShellAPI;

type
  TJvHiddenGemsForm = class(TForm)
    JvRichEditHiddenGems: TJvRichEdit;
    JvDataEmbeddedHiddenGemsRTF: TJvDataEmbedded;
    procedure FormCreate(Sender: TObject);
    procedure JvRichEditHiddenGemsURLClick(Sender: TObject;
      const URLText: String; Button: TMouseButton);
  end;

var
  JvHiddenGemsForm: TJvHiddenGemsForm;

implementation

{$R *.dfm}

procedure TJvHiddenGemsForm.FormCreate(Sender: TObject);
begin
 JvRichEditHiddenGems.Lines.LoadFromStream(JvDataEmbeddedHiddenGemsRTF.Data);
end;

procedure TJvHiddenGemsForm.JvRichEditHiddenGemsURLClick(Sender: TObject;
  const URLText: String; Button: TMouseButton);
begin
  ShellExecute(0, nil, PChar('"' + URLText + '"'), nil, nil, SW_SHOWNORMAL);
end;

end.
