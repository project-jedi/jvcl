{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRegLibDelphi3.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

//  this unit contains registration procedures for Delphi 3
//  Delphi 3 is obsolete. Use Delphi 5-7.

unit JvgRegLibDelphi3;
interface

// {$DEFINE INC_ALPHA_UNITS} // compile untested components

procedure Register;

implementation
{$R *.DCR}
uses Classes, DsgnIntf,
  JvgBevel, JvgButton, JvgCheckBox, JvgCaption, JvgDigits, JvgFlyingText, JvgGroupBox, JvgImage, JvgJump,
  JvgLabel, JvgListBox, JvgAskListBox, JvgPage, JvgProgress, JvgTab, JvgScrollBox, JvgTreeView, JvgShadow,
  JvgHint, Jvg3DColors, JvgProcess, JvgStringGrid, JvgEdit, JvgRuler, JvgCrossTable, JvgDBGrid, JvgVertDBGrid, JvgStringContainer,
  JvgMultiResources, JvgMultiResourceEditor, JvgSysInf, JvgPropertyCenter, JvgComponentListEditor, JvgReport, JvgReportEditorForm, JvgReportParamsForm, JvgGraphicButton,
  JvgQRLabel, JvgBitBtn, JvgHShape, JvgSmallFontsDefence;

procedure Register;
begin
  RegisterComponents('JVCL Globus Controls', [TJvgBevel, TJvgButton, TJvgGraphicButton, TJvgCheckBox,
    TJvgDigits, TJvgShadow, TJvgFlyingText, TJvgGroupBox, TJvgBitmapImage,
      TJvgLabel, TJvgBitBtn, TJvgMaskEdit, TJvgStaticTextLabel, TJvgListBox,
      TJvgCheckListBox, TJvgAskListBox,
      TJvgPageControl, TJvgTabControl, TJvgScrollBox, TJvgHoleShape,
      TJvgTreeView, TJvgCheckTreeView, TJvgProgress, TJvgRuler, TJvgStringGrid]);

  RegisterComponents('JVCL Globus Components', [Tgl3DColors, TJvgCaption,
    TJvgHint, TJvgJumpingComponent, TJvgProcess, TJvgStringContainer,
      TJvgMultipleResources, TJvgPropertyCenter, TJvgSysInfo,
      TJvgReport, TJvgReportEditor, TJvgReportParamsEditor,
      TJvgSmallFontsDefence]);

  {$IFDEF INC_ALPHA_UNITS}
  RegisterComponents('JVCL Globus DBAware', [TJvgDBGrid, TJvgVertDBSGrid, TJvgPrintCrossTable]);
  {$ENDIF};

  {RegisterComponents('JVCL Globus ExportImport', [TJvgExportExcel, TJvgExportDBETable]);}

  RegisterComponents('JVCL Globus QReport', [TJvgQRLabel, TJvgQRDBText]);

  //  RegisterPropertyEditor(TypeInfo(string), TglEdit, 'EditMask', TMaskProperty);
  //  RegisterPropertyEditor(TypeInfo(string), TJvgProcess, 'FileName', TFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TJvgResStringList), TJvgMultipleResources, 'Resources', TJvgResourcesProperty);

  RegisterComponentEditor(TJvgPropertyCenter, TJvgComponentListEditor);
  RegisterPropertyEditor(TypeInfo(TStringList), TJvgPropertyCenter, 'ComponentList', TJvgComponentListProperty);

  RegisterComponentEditor(TJvgReport, TJvgReportCompEditor);
  RegisterPropertyEditor(TypeInfo(TStringList), TJvgReport, 'Report', TJvgRepProperty);
  RegisterComponentEditor(TJvgReportEditor, TJvgReportCompEditor);
  RegisterComponentEditor(TJvgReportParamsEditor, TJvgRepParamsEditor);

end;

end.
