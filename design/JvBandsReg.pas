{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBandsReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, ImgList, ToolsAPI,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFNDEF COMPILER7_UP}
  ExptIntf,
  {$ENDIF COMPILER7_UP}
  JclSchedule,
  JvBandForms, JvBandObject, JvBandObjectDLLWizard,
  JvBandObjectDLLWizardForm;

procedure Register;
begin
  RegisterCustomModule(TJvBandForm, TCustomModule);
  RegisterPackageWizard(TJvBandObjectDLLWizard.Create);
end;


end.
