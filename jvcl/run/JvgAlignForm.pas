{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgAlignForm.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgAlignForm;

interface

uses
  Classes,
  {$IFDEF VCL}
  Controls, Forms, StdCtrls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QStdCtrls, QExtCtrls,
  {$ENDIF VisualCLX}
  JvgTypes, JvComponent;

type
  TAlignForm = class(TJvForm)
    g_Horz: TRadioGroup;
    g_Vert: TRadioGroup;
    B_Ok: TButton;
    B_Cancel: TButton;
    procedure B_OkClick(Sender: TObject);
  public
    Horz: TglHComponentAlign;
    Vert: TglVComponentAlign;
  end;

implementation

{$R *.dfm}

procedure TAlignForm.B_OkClick(Sender: TObject);
begin
  Horz := TglHComponentAlign(g_Horz.ItemIndex);
  Vert := TglVComponentAlign(g_Vert.ItemIndex);
end;

end.

