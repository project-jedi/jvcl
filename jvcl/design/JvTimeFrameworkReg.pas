{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeFrameworkReg.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTimeFrameworkReg;

{$I jvcl.inc}

interface

uses
  DesignIntf, DesignEditors,
  ColnEdit;

{$R JvTimeFrameworkReg.dcr}

type
  TJvTFGlanceCellsProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

procedure Register;

implementation

uses
  Classes,
  JvDsgnConsts,
  JvTFGlance, JvTFGlanceTextViewer, JvTFMonths, JvTFWeeks, JvTFDays,
  JvTFAlarm, JvTFManager;
  
//=== { TJvTFGlanceCellsProperty } ===========================================

function TJvTFGlanceCellsProperty.GetColOptions: TColOptions;
begin
  Result := [];
end;

procedure Register;
begin
  RegisterComponents(RsPaletteTimeFramework, [TJvTFScheduleManager,
    TJvTFUniversalPrinter]);
//  RegisterPropertyEditor(TypeInfo(string), TJvTFControl, 'Version', TutfVersionEditor);
//  RegisterPropertyEditor(TypeInfo(string), TJvTFScheduleManager, 'Version', TutfVersionEditor);
  RegisterComponents(RsPaletteTimeFramework, [TJvTFGlanceTextViewer, TJvTFMonths,
    TJvTFWeeks, TJvTFAlarm]);
//  RegisterPropertyEditor(TypeInfo(TJvTFGlanceCells), '', 'Cells',
//    TJvTFGlanceCellsProperty);

  // register a nil property editor for now, so cells cannot be added,
  // deleted, or moved at design time... BAD THINGS HAPPEN
  RegisterPropertyEditor(TypeInfo(TJvTFGlanceCells), TJvTFMonths, 'Cells', nil);
  RegisterComponents(RsPaletteTimeFramework, [TJvTFDays, TJvTFDaysPrinter]);
end;

end.
