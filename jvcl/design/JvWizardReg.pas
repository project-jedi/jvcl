{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizardReg.PAS, released on 2002-01-24.

The Initial Developer of the Original Code is William Yu Wei.
Portions created by William Yu Wei are Copyright (C) 2002 William Yu Wei.
All Rights Reserved.

Contributor(s):
Peter Thörnqvist - converted to JVCL naming conventions on 2003-07-11

Last Modified: 2002-01-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{*****************************************************************************
  Purpose:   Jv Wizard Component Editor

  History:
  ---------------------------------------------------------------------------
  Date(mm/dd/yy)   Comments
  ---------------------------------------------------------------------------
  01/24/2002       First Create
  01/25/2002       TJvWizardAboutDialogProperty, Added by <Steve Forbes>
******************************************************************************}

unit JvWizardReg;

interface

{$I JVCL.INC}


procedure Register;

implementation

{$R ..\resources\JvWizardReg.dcr}

uses
  Classes, {$IFDEF COMPILER6_UP}DesignIntf, DesignEditors{$ELSE}DsgnIntf{$ENDIF},
  JvWizard, JvWizardRouteMapNodes, JvWizardRouteMapSteps,
  {$IFNDEF USEJVCL}
  JvWizardAboutInfoForm,
  {$ENDIF USEJVCL}
  JvWizardEditorForm;

procedure Register;
begin
  RegisterComponents('Jv Wizard', [
    TJvWizard,TJvWizardRouteMapSteps,TJvWizardRouteMapNodes
    ]);
  RegisterClasses([TJvWizardCustomPage, TJvWizardWelcomePage, TJvWizardInteriorPage]);
  RegisterComponentEditor(TJvWizard, TJvWizardComponentEditor);
  RegisterComponentEditor(TJvWizardCustomPage, TJvWizardComponentEditor);
  RegisterComponentEditor(TJvWizardWelcomePage, TJvWizardComponentEditor);
  RegisterComponentEditor(TJvWizardInteriorPage, TJvWizardComponentEditor);
  RegisterPropertyEditor(TypeInfo(TJvWizardCustomPage), TJvWizard, 'ActivePage',
    TJvWizardActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TJvWizardWelcomePage), TJvWizard, 'ActivePage',
    TJvWizardActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TJvWizardInteriorPage), TJvWizard, 'ActivePage',
    TJvWizardActivePageProperty);
  // Added By Steve Forbes
  {$IFNDEF USEJVCL}
  RegisterPropertyEditor(TypeInfo(TJvWizardAboutInfoForm), nil, 'About',
    TJvWizardAboutDialogProperty);
  {$ENDIF}
  // JvWizard Page List Editor
  RegisterPropertyEditor(TypeInfo(TJvWizardPageList), TJvWizard, 'Pages',
    TJvWizardPageListProperty);
end; {Register}

end.
