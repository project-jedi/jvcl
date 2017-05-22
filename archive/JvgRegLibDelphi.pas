{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRegLibDelphi.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Rob den Braasem [rbraasem att xs4all dott nl].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgRegLibDelphi;

// this unit contains registration procedures for Delphi 4 - 7

interface

procedure Register;

implementation
{$R ..\Resources\JvGlobusReg.dcr}

uses Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors,
  PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgReportEditorForm, JvgAlignForm, JvgAlignFunction, JvgReportParamEditorForm,
  JvgBitBtn, JvgRuler, JvgReport, JvgCaption, JvgCGI, JvgReportParamsForm,
  JvgRichEditUtils, JvgCommClasses, JvgRttiUtils, JvgScrollBox,
  JvgConstSysRequirements, JvgCrossTable, JvgShade, JvgDBNav, JvgDigits,
  JvgDrawTab, JvgEdit, JvgExceptionHandler, JvgExport, JvgShadowEditor,
  JvgFileIterator, JvgFileUtils, JvgFixFont, JvgShadow, {JvgGraph,}
  JvgGraphicButton, JvgGridHeaderControl, JvgSingleInstance, JvgShape,
  JvgSpeedButton, JvgStaticText, JvgHoleShape, JvgHTTPVersionInfo, JvgSplit,
  {JvgImageGroup, }JvgInspectorGrid, JvgJump, JvgSmallFontsDefense, JvgSysInf,
  JvgLogicItemEditorForm, JvgLogics, JvgStringContainer, JvgMailSlots,
  JvgStringGrid, JvgTabComm, JvgTab, JvgSysRequirements, JvgProcess,
  JvgProcessUtils, JvgPropertyCenter, JvgQPrintPreviewForm, JvgQPrintSetupForm,
  JvgQRLabel, JvgTransparentMemo, JvgTreeView, JvgTypes, JvgVertDBGrid,
  JvgUtils, {JvgWinMask, } JvgWebDocumentIterator, JvgRTFPreviewForm,
  JvgWizardHeader, JvgXMLSerializer, Jvg3DColors, JvgAskListBox, JvgBevel,
  JvgButton, JvgCheckBox, JvgCheckVersionInfoForm, JvgCompDescription,
  JvgComponentListEditorForm, JvgDBGrid, JvgExportComponents, JvgFlyingText,
  JvgGroupBox, JvgHelpPanel, JvgHelpPanelEditor, JvgHint, JvgImage,
  JvgLabel, JvgLanguageLoader, JvgListBox, JvgLogicsEditorForm,
  JvgMultiResourceEditorForm, { JvgMultiResources, } JvgPage, JvgProgress,
  JvgLabelEditorForm;
// JvgStepLabel,
// JvgTagParser,
// JvgPointEditor,

procedure Register;
begin
  RegisterComponents('JVCL Globus Components 1', [TJvgExportDBETable,
    TJvgDBNAvigator, TJvgPrintCrossTable, TJvgDBGrid, TJvgVertDBSGrid,
      TJvgSysInfo, TJvgMaskEdit, TJvgBevel, TJvgBitBtn, TJvgGraphicButton,
      { TJvgGraph, } TJvgTreeView, TJvgCheckTreeView, TJvgSplitter, TJvgShadow,
      TJvgShade, TJvgButton, {TJvgImageGroup, }TJvgProgress, TJvgTransparentMemo,
      {TJvgWinMask, } TJvgGroupBox, TJvgBitmapImage, TJvgListBox, TJvgCheckListBox,
      TJvgAskListBox, TJvgScrollBox, TJvgStringGrid, TJvgSpeedButton,
      TJvgWizardHeader, TJvgCaption, TJvgGridHeaderControl]);

  RegisterComponents('JVCL Globus Components 2', [TJvgCheckBox,
    TJvgRuler, TJvgPageControl, TJvgTabControl, TJvgProcess, TJvgMailSlotServer,
      TJvgMailSlotClient, TJvgLabel, TJvgFlyingText, TJvgDigits, TJvgStaticText,
      TJvgHoleShape, TJvgExportExcel, TJvgExportHTML, TJvgHelpPanel,
      TJvgExportXML, TJvgXMLSerializer, TJvgLanguageLoader, TJvgExceptionHandler,
      TJvgJumpingComponent, TJvgStringContainer, TJvgSysRequirements,
      TJvg3DColors, TJvgHint, TJvginspectorGrid, TJvgReport,
      TJvgReportParamsEditor, TJvgLogicProducer, TJvgSmallFontsDefence,
      {TJvgMultipleResources, }TJvgSingleInstance, TJvgFixFont,
      {TJvgComponentDescription, } TJvgQRLabel, TJvgQRDBText, TJvgMyQRPreview]);

  RegisterComponentEditor(TJvgPropertyCenter, TJvgComponentListEditor);
  RegisterPropertyEditor(TypeInfo(TStringList), TJvgPropertyCenter,
    'ComponentList', TJvgComponentListProperty);
  RegisterComponentEditor(TJvgReportParamsEditor, TJvgRepParamsEditor);
  RegisterComponentEditor(TJvgLogicProducer, TJvgLogicsComponentEditor);
  RegisterComponentEditor(TJvgReport, TJvgReportCompEditor);
  RegisterPropertyEditor(TypeInfo(TStringList), TJvgReport, 'Report', TJvgRepProperty);
  RegisterComponentEditor(TJvgReportEditor, TJvgReportCompEditor);
  RegisterComponentEditor(TJvgShadow, TJvgShadowEditor);
  RegisterComponentEditor(TJvgHelpPanel, TJvgHelpPanelEditor);
  RegisterComponentEditor(TJvgLabel, TJvgLabelEditor);
{  RegisterPropertyEditor(TypeInfo(TJvgResStringList), TJvgMultipleResources,
    'Resources', TJvgResourcesProperty); }

  //RegisterComponentEditor(TJvgGridHeaderControl, TJvgGridHeaderControl_Editor);
  //RegisterPropertyEditor(TypeInfo(STRING), TJvgMaskEdit, 'EditMask',
  //  TMaskProperty);
  //RegisterPropertyEditor(TypeInfo(STRING), TJvgProcess, 'FileName',
  //  TFilenameProperty);
end;

end.

