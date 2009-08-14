{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJansReg.PAS, released on 2003-01-07.

The Initial Developer of the Original Code is John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  this should be merged with other Registration Unit
-----------------------------------------------------------------------------}
// $Id$

unit JvJansReg;

{$I jvcl.inc}

interface

uses
  Classes, Controls,
  DesignIntf, DesignEditors,
  VCLEditors,
  JvDsgnConsts,
  JvShapedButton, JvSticker, JvJanTreeView, JvMarkupLabel, JvMarkupViewer,
  JvSAL, JvSALCore, JvSALMath, JvYearGrid, JvTracker, JvAirBrush, JvGridFilter,
  JvGridPrinter, JvArrayButton, JvForth, JvTurtle, JvPaintFX, JvDrawImage,
  JvBitmapButton, JvSimScope, JvSimIndicator, JvSimPID, JvSIMPIDLinker,
  JvSimLogic, JvCSVBaseControls, JvCsvBaseEditor;

procedure Register;

implementation

{$R JvJansReg.dcr}

procedure Register;
const
  cCSVField = 'CSVField';
  cCSVFieldName = 'CSVFieldName';
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvSALCore, TControl);
  GroupDescendentsWith(TJvSALMath, TControl);
  GroupDescendentsWith(TJvGridFilter, TControl);
  GroupDescendentsWith(TJvGridPrinter, TControl);
  GroupDescendentsWith(TJvPaintFX, TControl);
  GroupDescendentsWith(TJvForthScript, TControl);
  GroupDescendentsWith(TJvTurtle, TControl);
  GroupDescendentsWith(TJvSimPIDLinker, TControl);
  {$ENDIF COMPILER7_UP}

  //TODO: Register a TShortCut Property Editor on TTreeKeyMappings class in TJvJanTreeView
  RegisterComponents(RsPaletteJans, [TJvMarkupLabel, TJvMarkupViewer, TJvSAL,
    TJvSticker,
    TJvSALCore, TJvSALMath, TJvYearGrid, TJvAirBrush, TJvTracker,
    TJvGridFilter, TJvGridPrinter,
    TJvJanTreeview, TJvPaintFX, TJvDrawImage, TJvArrayButton, TJvForthScript, TJvTurtle,
    TJvBitmapButton, TJvShapedButton]);
  // Simulator Components
  RegisterComponents(RsPaletteJansSim, [TJvSimScope, TJvSimIndicator, TJvSimPID,
    TJvSimPIDLinker, TJvSimConnector, TJvLogic, TJvSimButton, TJvSimLight,
    TJvSimLogicBox, TJvSimReverse]);
  // CSV Components
  RegisterComponents(RsPaletteJansCsv, [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox,
    TJvCSVCheckBox, TJvCSVNavigator]);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, cCSVFieldName, TJvCSVFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, cCSVField, TJvCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, cCSVField, TJvCSVFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, cCSVField, TJvCSVFieldProperty);
end;

end.