{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens.fudickar@oratool.de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens.fudickar@oratool.de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDynControlEngine_VCLRed;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl,
  JvDynControlEngine, JvDynControlEngine_Interface;

type
  TJvDynControlEngine_VCLRed = class (TJvDynControlEngine)
  private
  protected
    procedure AfterCreateControl(aControl: TControl); override;
  public
  published
  end;


function DynControlEngine_VCLRed: TJvDynControlEngine;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvDynControlEngine_VCL,
  SysUtils, ExtDlgs;

type
  tHackControl = class (tControl);

procedure TJvDynControlEngine_VclRed.AfterCreateControl(aControl: TControl);
begin
  inherited AfterCreateControl(aControl);
  if aControl is TButton then
    TButton(aControl).Font.Color := clRed
  else
    tHackControl(aControl).Color := clRed;
end;

var
  IntDynControlEngine_VCLRed: TJvDynControlEngine = nil;

function DynControlEngine_VCLRed: TJvDynControlEngine;
begin
  Result := IntDynControlEngine_VCLRed;
end;

initialization
  IntDynControlEngine_VCLRed := TJvDynControlEngine_VCLRed.Create;
  IntDynControlEngine_VCLRed.RegisterControl(jctLabel, TJvDynControlVCLLabel);
  IntDynControlEngine_VCLRed.RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  IntDynControlEngine_VCLRed.RegisterControl(jctButton, TJvDynControlVCLButton);
  IntDynControlEngine_VCLRed.RegisterControl(jctScrollBox, TJvDynControlVCLScrollBox);
  IntDynControlEngine_VCLRed.RegisterControl(jctPanel, TJvDynControlVCLPanel);
  IntDynControlEngine_VCLRed.RegisterControl(jctImage, TJvDynControlVCLImage);
  IntDynControlEngine_VCLRed.RegisterControl(jctCheckBox, TJvDynControlVCLCheckBox);
  IntDynControlEngine_VCLRed.RegisterControl(jctComboBox, TJvDynControlVCLComboBox);
  IntDynControlEngine_VCLRed.RegisterControl(jctListBox, TJvDynControlVCLListBox);
  IntDynControlEngine_VCLRed.RegisterControl(jctRadioGroup, TJvDynControlVCLRadioGroup);
  IntDynControlEngine_VCLRed.RegisterControl(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  IntDynControlEngine_VCLRed.RegisterControl(jctTimeEdit, TJvDynControlVCLTimeEdit);
  IntDynControlEngine_VCLRed.RegisterControl(jctDateEdit, TJvDynControlVCLDateEdit);
  IntDynControlEngine_VCLRed.RegisterControl(jctEdit, TJvDynControlVCLMaskEdit);
 //  IntDynControlEngine_VCLRed.RegisterControl(jctCalculateEdit, TJvDynControlVCLMaskEdit);
 //  IntDynControlEngine_VCLRed.RegisterControl(jctSpinEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngine_VCLRed.RegisterControl(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  IntDynControlEngine_VCLRed.RegisterControl(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  IntDynControlEngine_VCLRed.RegisterControl(jctMemo, TJvDynControlVCLMemo);
  SetDefaultDynControlEngine(IntDynControlEngine_VCLRed);

finalization
  FreeAndNil(IntDynControlEngine_VCLRed);

end.
