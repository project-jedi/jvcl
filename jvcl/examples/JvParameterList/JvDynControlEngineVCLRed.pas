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

unit JvDynControlEngineVCLRed;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl,
  JvDynControlEngine, JvDynControlEngineIntf;

type
  TJvDynControlEngineVCLRed = class (TJvDynControlEngine)
  private
  protected
    procedure AfterCreateControl(aControl: TControl); override;
  public
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

initialization
  IntDynControlEngineVCLRed := TJvDynControlEngineVCLRed.Create;
  IntDynControlEngineVCLRed.RegisterControl(jctLabel, TJvDynControlVCLLabel);
  IntDynControlEngineVCLRed.RegisterControl(jctStaticText, TJvDynControlVCLStaticText);
  IntDynControlEngineVCLRed.RegisterControl(jctButton, TJvDynControlVCLButton);
  IntDynControlEngineVCLRed.RegisterControl(jctScrollBox, TJvDynControlVCLScrollBox);
  IntDynControlEngineVCLRed.RegisterControl(jctPanel, TJvDynControlVCLPanel);
  IntDynControlEngineVCLRed.RegisterControl(jctImage, TJvDynControlVCLImage);
  IntDynControlEngineVCLRed.RegisterControl(jctCheckBox, TJvDynControlVCLCheckBox);
  IntDynControlEngineVCLRed.RegisterControl(jctComboBox, TJvDynControlVCLComboBox);
  IntDynControlEngineVCLRed.RegisterControl(jctListBox, TJvDynControlVCLListBox);
  IntDynControlEngineVCLRed.RegisterControl(jctRadioGroup, TJvDynControlVCLRadioGroup);
  IntDynControlEngineVCLRed.RegisterControl(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  IntDynControlEngineVCLRed.RegisterControl(jctTimeEdit, TJvDynControlVCLTimeEdit);
  IntDynControlEngineVCLRed.RegisterControl(jctDateEdit, TJvDynControlVCLDateEdit);
  IntDynControlEngineVCLRed.RegisterControl(jctEdit, TJvDynControlVCLMaskEdit);
//  IntDynControlEngineVCLRed.RegisterControl(jctCalculateEdit, TJvDynControlVCLMaskEdit);
//  IntDynControlEngineVCLRed.RegisterControl(jctSpinEdit, TJvDynControlVCLMaskEdit);
  IntDynControlEngineVCLRed.RegisterControl(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  IntDynControlEngineVCLRed.RegisterControl(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  IntDynControlEngineVCLRed.RegisterControl(jctMemo, TJvDynControlVCLMemo);
  IntDynControlEngineVCLRed.RegisterControl(jctButtonEdit, TJvDynControlVCLButtonEdit);
  SetDefaultDynControlEngine(IntDynControlEngineVCLRed);

finalization
  FreeAndNil(IntDynControlEngineVCLRed);

end.
