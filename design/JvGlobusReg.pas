{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGlobusReg.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Rob den Braasem [rbraasem att xs4all dott nl].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvGlobusReg;

// this unit contains registration procedures for Delphi 5 - 7

interface

procedure Register;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvGlobusReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvGlobusReg.dcr}
{$ENDIF LINUX}

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnConsts,
  JvgReportEditorForm, JvgAlignForm, JvgAlignFunction, JvgReportParamEditorForm,
  JvgBitBtn, JvgRuler, JvgReport, JvgCaption, JvgCGI, JvgReportParamsEditor,
  JvgReportParamsForm, JvgRichEditUtils, JvgCommClasses, JvgRttiUtils,
  JvgScrollBox, JvgConstSysRequirements, JvgShade, JvgDigits,
  JvgDrawTab, JvgEdit, JvgExceptionHandler, JvgShadowEditor,
  JvgFileIterator, JvgFileUtils, JvgFixFont, JvgShadow, {JvgGraph,}
  JvgGraphicButton, JvgSingleInstance, JvgShape,
  JvgSpeedButton, JvgStaticText, JvgHoleShape, JvgSplit,
  {JvgImageGroup,} JvgInspectorGrid, JvgJump, JvgSmallFontsDefense, JvgSysInf,
  JvgLogicItemEditorForm, JvgLogics, JvgStringContainer, JvgMailSlots,
  JvgStringGrid, JvgTabComm, JvgTab, JvgSysRequirements, JvgProcess,
  JvgProcessUtils, JvgPropertyCenter, JvgTransparentMemo, JvgTreeView, JvgTypes,
  JvgUtils, {JvgWinMask,} JvgRTFPreviewForm,
  JvgWizardHeader, JvgXMLSerializer, Jvg3DColors, JvgAskListBox, JvgBevel,
  JvgButton, JvgCheckBox, JvgCompDescription,
  JvgComponentListEditorForm, JvgFlyingText,
  JvgGroupBox, JvgHelpPanel, JvgHelpPanelEditor, JvgHint, JvgImage,
  JvgLabel, JvgLanguageLoader, JvgListBox, JvgLogicsEditorForm,
  JvgMultiResourceEditorForm, {JvgMultiResources,} JvgPage, JvgProgress,
  JvgGridHeaderControl, 
  {$IFNDEF DelphiPersonalEdition}
  JvgCrossTable, JvgDBNav, JvgExport, JvgDBGrid, JvgExportComponents,
  {$IFNDEF COMPILER8_UP}
  JvgWebDocumentIterator, JvgHTTPVersionInfo,
  JvgCheckVersionInfoForm,
  {$ENDIF}
  {$IFDEF JVCL_UseQuickReport}
  JvgQPrintPreviewForm, JvgQPrintSetupForm, JvgQRLabel,
  {$ENDIF JVCL_UseQuickReport}
  JvgVertDBGrid,
  {$ENDIF DelphiPersonalEdition}
  JvgLabelEditorForm;

// JvgStepLabel,
// JvgTagParser,
// JvgPointEditor,

procedure Register;
begin
  RegisterComponents(RsPaletteGlobusComponents1, [
    {$IFNDEF DelphiPersonalEdition}
    {TJvgExportDataset,} TJvgDBNavigator, TJvgPrintCrossTable, TJvgDBGrid,
    TJvgVertDBSGrid,
    {$ENDIF DelphiPersonalEdition}
    TJvgGridHeaderControl,
    TJvgSysInfo, TJvgMaskEdit, TJvgBevel, TJvgBitBtn, TJvgGraphicButton,
    {TJvgGraph,} TJvgTreeView, TJvgCheckTreeView, TJvgSplitter, TJvgShadow,
    TJvgShade, TJvgButton, {TJvgImageGroup,} TJvgProgress, TJvgTransparentMemo,
    {TJvgWinMask,} TJvgGroupBox, TJvgBitmapImage, TJvgListBox, TJvgCheckListBox,
    TJvgAskListBox, TJvgScrollBox, TJvgStringGrid, TJvgSpeedButton,
    TJvgExtSpeedButton, TJvgWizardHeader, TJvgCaption]);

  RegisterComponents(RsPaletteGlobusComponents2, [TJvgCheckBox,
    TJvgRuler, TJvgPageControl, TJvgTabControl, TJvgProcess,
    TJvgMailSlotServer, TJvgMailSlotClient, TJvgLabel, TJvgFlyingText,
    TJvgDigits, TJvgStaticText, TJvgHoleShape, TJvgHelpPanel,
    TJvgXMLSerializer, TJvgLanguageLoader, TJvgExceptionHandler,
    TJvgJumpingComponent, TJvgStringContainer, TJvgSysRequirements,
    TJvg3DColors, TJvgHint, TJvginspectorGrid, TJvgReport,
    TJvgReportParamsEditor, TJvgLogicProducer, TJvgSmallFontsDefense,
    {$IFNDEF DelphiPersonalEdition}
    TJvgExportExcel, TJvgExportHTML, TJvgExportXML, TJvgExportDataset,
    {$IFDEF JVCL_UseQuickReport}TJvgQRLabel, TJvgQRDBText, TJvgMyQRPreview,{$ENDIF}
    {$ENDIF DelphiPersonalEdition}
    {TJvgMultipleResources,} {TJvgComponentDescription,} TJvgSingleInstance,
    TJvgFixFont]);

  RegisterPropertyEditor(TypeInfo(TStringList), TJvgPropertyCenter,
    'ComponentList', TJvgComponentListProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TJvgReport, 'Report', TJvgRepProperty);
  RegisterComponentEditor(TJvgPropertyCenter, TJvgComponentListEditor);
  RegisterComponentEditor(TJvgReportParamsEditor, TJvgRepParamsEditor);
  RegisterComponentEditor(TJvgLogicProducer, TJvgLogicsComponentEditor);
  RegisterComponentEditor(TJvgReport, TJvgReportCompEditor);
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

