{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDynControlEngineVCLRed;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs,
  JvDynControlEngine, JvDynControlEngineIntf;

type
  TJvDynControlEngineVCLRed = class (TJvDynControlEngine)
  private
  protected
    procedure AfterCreateControl(aControl: TControl); override;
  public
    procedure RegisterControls; override;
  published
  end;


function DynControlEngineVCLRed: TJvDynControlEngine;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvDynControlEngineVCL,
  SysUtils, ExtDlgs;

type
  tHackControl = class (tControl);

procedure TJvDynControlEngineVclRed.AfterCreateControl(aControl: TControl);
begin
  inherited AfterCreateControl(aControl);
  if aControl is TButton then
    TButton(aControl).Font.Color := clRed
  else if aControl is tControl then
    tHackControl(aControl).Color := clRed;
end;

var
  IntDynControlEngineVCLRed: TJvDynControlEngine = nil;

function DynControlEngineVCLRed: TJvDynControlEngine;
begin
  Result := IntDynControlEngineVCLRed;
end;

procedure TJvDynControlEngineVclRed.RegisterControls;
begin
  RegisterControl(jctLabel, TJvDynControlVCLLabel);
  {$IFDEF VCL}
  RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  {$ENDIF VCL}
  RegisterControl(jctButton, TJvDynControlVCLButton);
  RegisterControl(jctScrollBox, TJvDynControlVCLScrollBox);
  RegisterControl(jctPanel, TJvDynControlVCLPanel);
  RegisterControl(jctImage, TJvDynControlVCLImage);
  RegisterControl(jctCheckBox, TJvDynControlVCLCheckBox);
  RegisterControl(jctComboBox, TJvDynControlVCLComboBox);
  RegisterControl(jctListBox, TJvDynControlVCLListBox);
  RegisterControl(jctCheckListBox, TJvDynControlVCLCheckListBox);
  RegisterControl(jctRadioGroup, TJvDynControlVCLRadioGroup);
  {$IFDEF VCL}
  RegisterControl(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  RegisterControl(jctTimeEdit, TJvDynControlVCLTimeEdit);
  RegisterControl(jctDateEdit, TJvDynControlVCLDateEdit);
  {$ENDIF VCL}
  RegisterControl(jctEdit, TJvDynControlVCLMaskEdit);
//  RegisterControl(jctCalculateEdit, TJvDynControlVCLMaskEdit);
//  RegisterControl(jctSpinEdit, TJvDynControlVCLMaskEdit);
  RegisterControl(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  RegisterControl(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  RegisterControl(jctMemo, TJvDynControlVCLMemo);
  RegisterControl(jctButtonEdit, TJvDynControlVCLButtonEdit);
end;

initialization
  IntDynControlEngineVCLRed := TJvDynControlEngineVclRed.Create;
  SetDefaultDynControlEngine(IntDynControlEngineVCLRed);

finalization
  FreeAndNil(IntDynControlEngineVCLRed);

end.
