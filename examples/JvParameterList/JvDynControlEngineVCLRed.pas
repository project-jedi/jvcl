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
  JvDynControlEngine, JvDynControlEngineVCL,
  JvDynControlEngineIntf;

type
  TJvDynControlEngineVCLRed = class (TJvDynControlEngineVCL)
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
  Inherited RegisterControls;
(*  RegisterControlType(jctLabel, TJvDynControlVCLLabel);
  {$IFDEF VCL}
  RegisterControlType(jctStaticText, TJvDynControlVCLStaticText);
  {$ENDIF VCL}
  RegisterControlType(jctButton, TJvDynControlVCLButton);
  RegisterControlType(jctScrollBox, TJvDynControlVCLScrollBox);
  RegisterControlType(jctPanel, TJvDynControlVCLPanel);
  RegisterControlType(jctImage, TJvDynControlVCLImage);
  RegisterControlType(jctCheckBox, TJvDynControlVCLCheckBox);
  RegisterControlType(jctComboBox, TJvDynControlVCLComboBox);
  RegisterControlType(jctListBox, TJvDynControlVCLListBox);
  RegisterControlType(jctCheckListBox, TJvDynControlVCLCheckListBox);
  RegisterControlType(jctRadioGroup, TJvDynControlVCLRadioGroup);
  {$IFDEF VCL}
  RegisterControlType(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  RegisterControlType(jctTimeEdit, TJvDynControlVCLTimeEdit);
  RegisterControlType(jctDateEdit, TJvDynControlVCLDateEdit);
  {$ENDIF VCL}
  RegisterControlType(jctEdit, TJvDynControlVCLMaskEdit);
//  RegisterControlType(jctCalculateEdit, TJvDynControlVCLMaskEdit);
//  RegisterControlType(jctSpinEdit, TJvDynControlVCLMaskEdit);
  RegisterControlType(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  RegisterControlType(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  RegisterControlType(jctMemo, TJvDynControlVCLMemo);
  RegisterControlType(jctButtonEdit, TJvDynControlVCLButtonEdit);
  *)
end;

initialization
  IntDynControlEngineVCLRed := TJvDynControlEngineVclRed.Create;
  SetDefaultDynControlEngine(IntDynControlEngineVCLRed);

finalization
  FreeAndNil(IntDynControlEngineVCLRed);

end.
