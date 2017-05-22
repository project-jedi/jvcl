{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRegLibCBB5.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

//  this unit contains registration procedures for C++Builder 5

unit JvgRegLibCBB5;

interface

//{$DEFINE INC_ALPHA_UNITS} // компилировать также недоработанные компоненты
{ Also compile not complete(finished) components [translated] }

procedure Register;

implementation

uses Classes,
  //{ beta version units - готовые к использованию компоненты }
  { beta version units - components ready to be used [translated] }
  JvgBevel, JvgLabel, JvgEdit, JvgCheckBox, JvgTreeView, JvgFlyingText, JvgPage, JvgTab,
  JvgHint, Jvg3DColors, JvgCaption, JvgProgress, JvgHShape, JvgSplit, JvgJump,
  JvgDigits, JvgGroupBox, JvgImage, JvgShadow, JvgListBox, JvgAskListBox, JvgScrollBox, JvgGraphicButton, { JvgQRLabel,}
  JvgBitBtn, JvgRuler, JvgStringGrid, JvgProcess, JvgSysInf, {JvgShape,}
  {JvgReport, JvgReportEditorForm,} JvgMailSlots, JvgExceptionHandler, JvgSpeedButton,
  JvgSingleInstance, JvgHelpPanel, JvgStringContainer, JvgSysRequirements,
  JvgSmallFontsDefence, JvgWizardHeader, JvgXMLSerializer,
  {JvgExportComponents, } JvgShadowEditor, JvgHelpPanelEditor

  //{ alpha version units - компоненты в стадии доработки }
  { alpha version units - components in stage of completing them [translated] }
  {$IFDEF INC_ALPHA_UNITS}
  {JvgMultiResources, JvgButton,  },
  JvgPropertyCenter,
  JvgGridHeaderControl,
  JvgCrossTable,
  JvgReportParamsForm,
  JvgComponentListEditor,
  geGHC,
  JvgLogics, JvgLogicsEditor,
  JvgInspectorGrid,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignWindows, DesignEditors
  {$ELSE}
  DsgnIntf{$ENDIF};

procedure Register;
begin
  RegisterComponents('JVCL Globus Controls', [TJvgSplitter, TJvgBevel, TJvgLabel, TJvgBitBtn, TJvgGraphicButton, TJvgMaskEdit, TJvgCheckBox, TJvgTreeView, TJvgCheckTreeView, TJvgFlyingText,
    TJvgPageControl, TJvgTabControl, TJvgProgress, TJvgHoleShape,
      TJvgDigits, TJvgShadow, TJvgGroupBox, TJvgBitmapImage, TJvgStaticTextLabel, TJvgListBox,
      TJvgCheckListBox, TJvgAskListBox, TJvgScrollBox, TJvgMaskEdit, TJvgRuler, TJvgStringGrid,
      TJvgSplitter, TJvgSpeedButton,
      TJvgHelpPanel, TJvgWizardHeader
      ]);

  RegisterComponents('JVCL Globus Components', [Tgl3DColors, TJvgCaption, TJvgHint, TJvgProcess, TJvgSysInfo,
    TJvgJumpingComponent, TJvgMailSlotServer, TJvgMailSlotClient,
      //TJvgReport, TJvgReportEditor,
    TJvgExceptionHandler, TJvgSingleInstance, TJvgStringContainer, TJvgSysRequirements,
      TJvgSmallFontsDefence, TJvgXMLSerializer {, TJvgMultipleResources}]);

  //  RegisterComponents( 'JVCL Globus QReport', [ TJvgQRLabel, TJvgQRDBText ] );

  //  RegisterComponents('JVCL Globus ExportImport', [TJvgExportExcel, TJvgExportDBETable{, TJvgExportHTML, TJvgExportXML}]);

  {$IFDEF INC_ALPHA_UNITS}
  RegisterComponents('JVCL Globus Controls', [TJvgGridHeaderControl, TJvgInspectorGrid {, TJvgButton}]);
  RegisterComponents('JVCL Globus Components', [TJvgReportParamsEditor, TJvgLogicProducer {, TJvgMultipleResources}]);
  RegisterComponents('JVCL Globus DB', [TJvgPrintCrossTable]);

  RegisterComponentEditor(TJvgPropertyCenter, TJvgComponentListEditor);
  RegisterPropertyEditor(TypeInfo(TStringList), TJvgPropertyCenter, 'ComponentList', TJvgComponentListProperty);
  RegisterComponentEditor(TJvgReportParamsEditor, TJvgRepParamsEditor);
  RegisterComponentEditor(TJvgGridHeaderControl, TglGridHeaderControl_Editor);
  RegisterComponentEditor(TJvgLogicProducer, TJvgLogicsComponentEditor);
  {
    RegisterComponents( 'JVCL Globus Components', [ Tgl3DColors, TJvgCaption,
          TJvgHint, TJvgJumpingComponent, TJvgProcess, TJvgStringContainer,
          TJvgMultipleResources , TJvgPropertyCenter, TJvgSysInfo,
                        TJvgReport, TJvgReportEditor, TJvgReportParamsEditor] );
    RegisterComponents( 'JVCL Globus DBAware', [ TJvgDBGrid, TJvgVertDBSGrid, TJvgPrintCrossTable ] );
  }
  //  RegisterPropertyEditor(TypeInfo(string), TglEdit, 'EditMask', TMaskProperty);
  //  RegisterPropertyEditor(TypeInfo(string), TJvgProcess, 'FileName', TFilenameProperty);
  //  RegisterPropertyEditor( TypeInfo(TJvgResStringList), TJvgMultipleResources, 'Resources', TJvgResourcesProperty );
  {$ENDIF};

  //  RegisterComponentEditor(TJvgReport, TJvgReportCompEditor);
  //  RegisterPropertyEditor( TypeInfo(TStringList), TJvgReport, 'Report', TJvgRepProperty );
  //  RegisterComponentEditor(TJvgReportEditor, TJvgReportCompEditor);

  RegisterComponentEditor(TJvgShadow, TJvgShadowEditor);
  RegisterComponentEditor(TJvgHelpPanel, TJvgHelpPanelEditor);

end;

end.
