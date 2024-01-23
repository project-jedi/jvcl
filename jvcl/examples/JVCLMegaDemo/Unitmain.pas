{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [Ralf dot Grenzing@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit Unitmain;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvButton, JvNavigationPane, JvBaseDlg, JvJVCLAboutForm, JvAutoComplete,
  ImgList, JvComponent, Buttons, JvExButtons, JvBitBtn, ComCtrls,
  JvExComCtrls, JvComCtrls, StdCtrls, JvExStdCtrls, JvRichEdit, JvEdit,
  JvListBox, JvExControls, JvLabel, JvXPBar, JvXPCore, JvXPContainer,
  ExtCtrls, JvExExtCtrls, JvSplitter, JvCtrls, JvCaptionPanel, JvToolBar,
  JvAppStorageBaseMainFrmU, ControlsExampleMainFormU, JvCheckBox,
  JvHtControls, JvStatusBar, JvNetscapeSplitter, JvDbMaskEditDemoForm,
  JvOutlookBarCustomDrawDemoMainForm, JvComponentBase, JvExtComponent;

type
  TMainform = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    JvNavPaneStyleMan: TJvNavPaneStyleManager;
    LargeImages: TImageList;
    PanelTop: TPanel;
    aJvImgBtn: TJvImgBtn;
    sbxWinXPBar: TScrollBox;
    JvXPContainer1: TJvXPContainer;
    JvXPBarBrowseDemos: TJvXPBar;
    jvXPBarSettings: TJvXPBar;
    JvXPBarIDE: TJvXPBar;
    Panel4: TPanel;
    JvListBoxAllDemos: TJvListBox;
    JvLookupAutoCompl: TJvLookupAutoComplete;
    aJvBitBtn: TJvBitBtn;
    JvRichEditHints: TJvRichEdit;
    JvSplitter1: TJvSplitter;
    JvJVCLAboutComp: TJvJVCLAboutComponent;
    JvNavPanelButton2: TJvNavPanelButton;
    JvNavPanelButton3: TJvNavPanelButton;
    JvNavPanelBtnIdePageCtrl: TJvNavPanelButton;
    JvNavPanelBtnLoadFormInIDE: TJvNavPanelButton;
    JvNavPanelBtnJumpHelp: TJvNavPanelButton;
    JvNavPanelBtnExit: TJvNavPanelButton;
    JvXPBarInformation: TJvXPBar;
    JvPageControlComps: TJvPageControl;
    JvXPBarSearchByCompName: TJvXPBar;
    Panel2: TPanel;
    JvLabel2: TJvLabel;
    JvListBoxDemosCompNameSorted: TJvListBox;
    JvEdtCompSearch: TJvEdit;
    JvLabel1: TJvLabel;
    Panel1: TPanel;
    JvLabel3: TJvLabel;
    JvCheckBoxAllowOnlyOneExpanded: TJvCheckBox;
    JvLabel4: TJvLabel;
    JvLabel5: TJvLabel;
    StatusBar: TJvStatusBar;
    JvNavPanelButton1: TJvNavPanelButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CreateDemoForm(const ID: Integer; ShowForm: Boolean = True);
    procedure btnLoadIdeClick(Sender: TObject);
    procedure CompClick(Sender: TObject);
    procedure JvNavPanelBtnIdePageCtrlClick(Sender: TObject);
    procedure ShowJVCLAboutDlg(Sender: TObject);
    procedure ShellExecHint(Sender: TObject);
    procedure JvNavPanelBtnExitClick(Sender: TObject);
    procedure JvXPBarIDECollapsedChange(Sender: TObject; Collapsing: Boolean);
    procedure WinXPBarEnsureOnlyOneExpanded(Sender: TObject; Collapsing: Boolean);
    procedure JvRichEditHintsURLClick(Sender: TObject;
      const URLText: string; Button: TMouseButton);
    procedure JvListBoxAllDemosChange(Sender: TObject);
    procedure JvNavPanelBtnJumpHelpClick(Sender: TObject);
    procedure JvXPBarSearchByCompNameAfterCollapsedChange(Sender: TObject;
      Collapsing: Boolean);
    procedure JvXPBarBrowseDemosAfterCollapsedChange(
      Sender: TObject; Collapsing: Boolean);
    procedure JvNavPanelButton1Click(Sender: TObject);
  private
    procedure ReadAllDemosFromIni;
  end;

const
  MAX_FORMS = 150;

var
  Mainform: TMainform;
  TheToolBar: TJvToolBar;
  TheFormArray: array[1..MAX_FORMS] of TForm;
  FormID: Integer;
  gBitmapFilePath: string;

implementation

uses
  JvFrameEmpty, hello, JvLabelsU, JvFormsU, JvDialogsU, JvButtonsU, JvDateTimeU,
  ArrowButtonMainFormU, JvPanelsU, JvChoosersU, JvControlsU, JvSearchFiles, JvWinDialogsU,
  JvEditsU, JvAniMainFormU, JvSearchFileMainFormU, JvNTEventLogMainFormU, JvMruListMainFormU,
  JvLogFileMainFormU, InstallLabelMainFormU, JvDBDateTimePickerMainFormU, ContentScrollerMainFormU,
  JvDataEmbeddedMainFormU, JvBrowseFolderMainFormU,
  JvClipboardViewerMainFormU, JvZoomMainFormU, JvWindowsTitleMainFomU,
  RaHtHintsMainFormU, FileListBoxMainFormU, JvTreeViewAsMenuMainFormU,
  ListCombMainFormU, JvBalloonHintMainFormU, JvHTMLParserMainFormU,
  JvLinkLabelMainFormU, JvScreenCaptureMainFormU,
  JvShFileOperationMainFormU, JvSystemPopup2MainFormU, JvSystemPopupMainFormU,
  JvThumbnailMainFormU, RegTVMainFormU, RunDll32MainFormU, TimelineMainFormU,
  TipOfDayMainFormU, TMTimeLineMainFormU, FindReplaceMainFormU, JvPlayListMainFormU,
  JvZLibMultipleMainFormU, OtherStandAlone, Profiler32MainFormU,
  RessourcesFormMain, SearchingForm, JclDebug, JclStrings, JclFileUtils, ShellAPI,
  IniFiles, CreateProcessExampleMainFormU, ConsoleExampleMainFormU,
  XMLSerializerMainFormU, JvAppHotKeyDemoMainFormU, JvAppStorageSelListMainfrmU,
  JvAppStorageSubStorageMainFrm, JvAviCapDemoFrmU, TVDemoMain, MDIBkgndDemoMain,
  MDIBkgndDemoSettings, MDIBkgndDemoChld, JvBehaviorLblMainFrmU, BmpAnimMainFormU,
  ChangeNotificationMainFormU, OLBarMainFormU, MailExampleMainFormU,
  JvInspectorDBDemoMainFormU, InspectorSimpleExampleMain, InspectorExampleMain,
  TransBtnFormMainU, JvShellHookDemoMainFormU, ScrollWinMainFormU, LEDMain,
  StarFieldMain, JvCharMapMainFrmU, JvChartDemoFm, JvCheckBoxRadioBtnFrmU,
  CheckTVDemoFrm, JvColorComboDemoMainFormU, CsvDataSourceDemoFm, JvID3v1MainFormU,
  JvID3v2MainFormU, JvHiddenGems, DSADialogsMainFormU, MessageDlgEditorMain,
  JvComboListBoxDemoForm, JvComputerInfoExDemoForm,
  JvDBActionMainForm,
  JvDBFindEditDemoForm, JvDBGridExportDemoMainForm, JvDBHTLabelDemoMainForm,
  JvDBImageDemoMainForm, JvDBSearchDemoMainForm, JvDesktopAlertDemoForm,
  WebMapperDemoMainForm, UseCaseDemoMainForm, DependencyWalkerDemoMainForm,
  fDialogs, JvDomainUpDownDemoMainForm, JvDotNetDemoMainForm,
  JvErrorIndicatorMainDemoForm, JvFooterAndGroupHeaderDemoForm,
  JvFullColorCircleDialogMainForm, JvFullColorDialogMainForm,
  EditorMainFormU, GIFMAIN, uJvMouseGesture, JvNavPaneDemoMainForm, fBalls,
  fPhilosophers, JvMarkupLabelDemoMainForm, JvMenusExampleMainForm,
  PackageModifierMainForm, JvPageListTreeViewMainForm, JvPanelDemoFrm;

{$R *.dfm}

procedure TMainform.ReadAllDemosFromIni;
var
  IniFile, IniFileCompsUsed: TIniFile;
  allDemoFrms, compUsedSL: TStringList;
  I, J, K: Integer;
  aTabSheet: TTabSheet;
  aJvXPBar: TJvXPBar;
  aFileName: string;
  found: Boolean;

  function getJvXPBarByCaption(aCaption: string): TJvXPBar;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to JvXPContainer1.ControlCount - 1 do
      if TJvXPBar(JvXPContainer1.Controls[I]).caption = aCaption then
      begin
        Result := TJvXPBar(JvXPContainer1.Controls[I]);
        Exit;
      end;
  end;

  function createJvXPBar(aCaption: string): TJvXPBar;
  begin
    Result := TJvXPBar.Create(Self);
    Result.Parent := JvXPContainer1;
    Result.Caption := aCaption;
    Result.Collapsed := True;
    Result.ShowItemFrame := False;
    Result.RoundedItemFrame := 0;
    Result.Align := alTop;
    Result.ParentFont := False;
    Result.Colors.BodyColor := clInfoBK;
    Result.HeaderFont.Color := clBlack;
    Result.Font.Color := clNavy; // color of items
    Result.HeaderFont.Size := 11;
    Font.Color := clNavy; // color of items
    Result.Colors.GradientFrom := $00F4E2E1;
    Result.Colors.GradientTo := $00B09494;
    Result.HotTrackColor := clBlack;
  end;

  function getTabsSheetByCaption(aCaption: string): TTabSheet;
  var
    I: Integer;
  begin
    Result := nil;

    for I := 0 to JvPageControlComps.PageCount - 1 do
    begin
      if JvPageControlComps.Pages[I].Caption = aCaption then
      begin
        Result := JvPageControlComps.Pages[I];
        Exit;
      end;
    end;
  end;

  function createTabSheet(aCaption: string): TTabSheet;
  begin
    Result := TTabSheet.Create(self);
    Result.Parent := JvPageControlComps;
    Result.caption := aCaption;
    Result.PageControl := JvPageControlComps;
  end;

begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\data\JvMegaDemoAllDemoForms.ini');
  IniFileCompsUsed := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\data\JvMegaDemoCompsTabs.ini');
  allDemoFrms := TStringList.Create;
  compUsedSL := TStringList.Create;
  try
    IniFile.ReadSections(allDemoFrms);
    for I := 0 to allDemoFrms.Count - 1 do
    begin

      if IniFile.ReadString(allDemoFrms[I], 'Title', '') = '' then
      begin
        MessageDlg('Demo Form Id ' + allDemoFrms[I] + ' has no title!', mtError, [mbOK], 0);
        Exit;
      end;

      // add entry to the ListBox of all Demos
      JvListBoxAllDemos.Items.Add(IniFile.ReadString(allDemoFrms[I], 'Title', ''));

      // add an entry to the XPBars
      if IniFile.ReadString(allDemoFrms[I], 'XPBar', '') <> '' then
      begin
        aJvXPBar := getJvXPBarByCaption(IniFile.ReadString(allDemoFrms[I], 'XPBar', ''));

        if aJvXPBar = nil then
          aJvXPBar := createJvXPBar(IniFile.ReadString(allDemoFrms[I], 'XPBar', ''));

        with aJvXPBar.Items.Add do
        begin
          Caption := IniFile.ReadString(allDemoFrms[I], 'Title', '');
          Hint := IniFile.ReadString(allDemoFrms[I], 'descr', '');
          ImageIndex := IniFile.ReadInteger(allDemoFrms[I], 'ImageIdx', -1);
          Tag := StrToInt(allDemoFrms[I]);
          OnClick := CompClick;
        end;
      end;

      // add an entry for every to the "components as in thh Delphi IDE"
      if IniFile.ReadString(allDemoFrms[I], 'compsUsed', '') <> '' then
      begin
        compUsedSL.commaText := IniFile.ReadString(allDemoFrms[I], 'compsUsed', '');

        for J := 0 to compUsedSL.count - 1 do
        begin
          aTabSheet := getTabsSheetByCaption(IniFileCompsUsed.ReadString(compUsedSL[J], 'Tab', ''));

          if aTabSheet = nil then
            aTabSheet := createTabSheet(IniFileCompsUsed.ReadString(compUsedSL[J], 'Tab', ''));

          // check if for that comp a ImgBtn already created
          found := False;
          for K := 0 to aTabSheet.ControlCount - 1 do
            if aTabSheet.Controls[K].Name = compUsedSL[J] then
            begin
              aTabSheet.Controls[K].Hint := aTabSheet.Controls[K].Hint + ',' + allDemoFrms[I];
              found := True;
            end;
          if found = False then
          begin
            aJvBitBtn := TJvBitBtn.Create(Self);
            aJvBitBtn.Parent := Self;
            aJvBitBtn.Name := compUsedSL[J];
            aJvBitBtn.Left := aTabSheet.Tag;
            aTabSheet.Tag := aTabSheet.Tag + 30;
            aJvBitBtn.Top := 2;
            aJvBitBtn.Width := 25;
            aJvBitBtn.Height := 25;
            aJvBitBtn.ShowHint := False;
            aJvBitBtn.OnClick := CompClick;
            aFileName := gBitmapFilePath + 'T' + compUsedSL[J] + '.BMP';
            {
            if not FileExists(AFileName) and (aTabSheet.Caption <> 'none') then
              MessageDlg('File "' + AFileName + '" for Bitmap not found!', mtError, [mbOk], 0)
            else
            }
            try
              if aTabSheet.Caption <> 'none' then
                aJvBitBtn.Glyph.LoadFromFile(AFileName);
            except
            end;
            aJvBitBtn.Hint := allDemoFrms[I];
            aJvBitBtn.caption := '';
            aJvBitBtn.Parent := aTabSheet;

            JvListBoxDemosCompNameSorted.items.add(compUsedSL[J]);
          end;
        end;
      end;
    end;
  finally
    IniFile.Free;
    IniFileCompsUsed.Free;
    allDemoFrms.Free;
    compUsedSL.Free;
  end;
end;

procedure TMainform.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to MAX_FORMS do
    if (TheFormArray[I] <> nil) then
      TheFormArray[I].Free;
end;

procedure TMainform.CreateDemoForm(const ID: Integer; ShowForm: Boolean);
begin
  // free the last Demo Form
  FreeAndNil(TheFormArray[FormID]);
  case ID of
    1: TheFormArray[ID] := TJvFormsFrm.Create(nil);
    2: TheFormArray[ID] := TJvDialogs.Create(nil);
//    3: TheFormArray[ID] := TGLDemoFrm.Create(nil);
    4: TheFormArray[ID] := TJvLabelsFrm.Create(nil);
    5: TheFormArray[ID] := TRaHtHintsMainForm.Create(nil);
    6: TheFormArray[ID] := TJvZoomMainForm.Create(nil);
    7: TheFormArray[ID] := TJvEdits.Create(nil);
    8: TheFormArray[ID] := TSearchingFormMain.Create(nil);
    9: TheFormArray[ID] := TJvPanelsFrm.Create(nil);
    10: TheFormArray[ID] := TfglXMLSerializerDemo.Create(nil);
    11: TheFormArray[ID] := TJvSearchFileMainForm.Create(nil);
    12: TheFormArray[ID] := TJvDateTimeFrm.Create(nil);
    13: TheFormArray[ID] := TJvChoosersFrm.Create(nil);
    14: TheFormArray[ID] := TJvControls.Create(nil);
    15: TheFormArray[ID] := TJvAniMainForm.Create(nil);
//    16 : TheFormArray[ID] := TJvMousePositionnerMainForm.Create(nil);
    17: TheFormArray[ID] := TJvDataEmbeddedMainForm.Create(nil);
    18: TheFormArray[ID] := TBmpAnimMainForm.Create(nil);
    19: TheFormArray[ID] := TArrowButtonMainForm.Create(nil);
    20: TheFormArray[ID] := TJvClipboardViewerMainForm.Create(nil);
    21: TheFormArray[ID] := TJvBrowseFolderMainForm.Create(nil);
    22: TheFormArray[ID] := TInstallLabelMainForm.Create(nil);
    24: TheFormArray[ID] := TJvLogFileMainForm.Create(nil);
    25: TheFormArray[ID] := TOLBarMainForm.Create(nil);
    27: TheFormArray[ID] := TChangeNotificationMainForm.Create(nil);
    28: TheFormArray[ID] := TCreateProcessExampleMainForm.Create(nil);
    29: TheFormArray[ID] := TJvNTEventLogMainForm.Create(nil);
    30: TheFormArray[ID] := TJvAppHotKeyDemoMainForm.Create(nil);
    31: TheFormArray[ID] := TJvWindowsTitleMainForm.Create(nil);
//    32 : TheFormArray[ID] := TJvSpecialProgressMainForm.Create(nil);
    33: TheFormArray[ID] := TJvColorComboDemoMainForm.Create(nil);
    34: TheFormArray[ID] := TContentScrollerMainForm.Create(nil);
    35: TheFormArray[ID] := TMailExampleMainForm.Create(nil);
    36: TheFormArray[ID] := TJvTreeViewAsMenuMainForm.Create(nil);
    37: TheFormArray[ID] := TListCombMainForm.Create(nil);
    38: TheFormArray[ID] := TJvDBDateTimePickerMainForm.Create(nil);
    39: TheFormArray[ID] := TJvInspectorDBDemoMainForm.Create(nil);
    40: TheFormArray[ID] := TJvMruListMainForm.Create(nil);
    41: TheFormArray[ID] := TFileListBoxMainForm.Create(nil);
    42: TheFormArray[ID] := TJvButtons.Create(nil);
    43: TheFormArray[ID] := TJvBalloonHintMainForm.Create(nil);
    44: TheFormArray[ID] := TDSADialogsMainForm.Create(nil);
    45: TheFormArray[ID] := TfrmMessageDlgEditor.Create(nil);
    46: TheFormArray[ID] := TJvHTMLParserMainForm.Create(nil);
    47: TheFormArray[ID] := TJvLinkLabelMainForm.Create(nil);
    48: TheFormArray[ID] := TJvScreenCaptureMainForm.Create(nil);
    49: TheFormArray[ID] := TJvShellHookDemoMainForm.Create(nil);
    50: TheFormArray[ID] := TJvShFileOperationMainForm.Create(nil);
    51: TheFormArray[ID] := TJvSystemPopupMainForm.Create(nil);
    52: TheFormArray[ID] := TJvSystemPopup2MainForm.Create(nil);
    53: TheFormArray[ID] := TJvThumbnailMainForm.Create(nil);
//    54 : TheFormArray[ID] := TJvTranslatorMainForm.Create(nil);
//    55 : TheFormArray[ID] := TJvWndProcHookDemoMainForm.Create(nil);
//    56 : TheFormArray[ID] := TJvWndProcHookDemoMainForm.Create(nil);
    57: TheFormArray[ID] := TRegTVMainForm.Create(nil);
    58: TheFormArray[ID] := TRunDll32MainForm.Create(nil);
    59: TheFormArray[ID] := TJvScrollingWindowMainForm.Create(nil);
    60: TheFormArray[ID] := TTimelineMainForm.Create(nil);
    61: TheFormArray[ID] := TTipOfDayMainForm.Create(nil);
    62: TheFormArray[ID] := TTMTimeLineMainForm.Create(nil);
    63: TheFormArray[ID] := TTransBtnFormMain.Create(nil);
    64: TheFormArray[ID] := TJvZLibMultipleMainForm.Create(nil);
    65: TheFormArray[ID] := TWelcomeForm.Create(nil);
    66: TheFormArray[ID] := TOtherMainForm.Create(nil);
    67: TheFormArray[ID] := TProfiler32MainForm.Create(nil);
    68: TheFormArray[ID] := TFindReplaceMainForm.Create(nil);
    69: TheFormArray[ID] := TJvPlaylistMainForm.Create(nil);
//    70 : TheFormArray[ID] := TImageWindowMainForm.Create(nil);
    71: TheFormArray[ID] := TRessourcesForm.Create(nil);
    72: TheFormArray[ID] := TConsoleExampleMainForm.Create(nil);
    73: TheFormArray[ID] := TJvAppStorageBaseMainFrm.Create(nil);
    74: TheFormArray[ID] := TJvAppStorageSelListMainfrm.Create(nil);
    75: TheFormArray[ID] := TJvAppStorageSubStorageMainForm.Create(nil);
    76: TheFormArray[ID] := TJvAviCapDemoFrm.Create(nil);
    77: TheFormArray[ID] := TJvBackgroundDemoFrm.Create(nil);
    78:
      begin
        TheFormArray[ID] := TMDIMainForm.Create(nil);
          // TMDIMainForm.Create(Application)
          //  TBkgndImageSettings.Create(Application);
           // TMDIChildForm.Create(Application);
      end;
    79: TheFormArray[ID] := TJvBehaviorLblMainFrm.Create(nil);
    80: TheFormArray[ID] := TSimpleMainForm.Create(nil);
    81: TheFormArray[ID] := TfrmInspector.Create(nil);
    82: TheFormArray[ID] := TLEDDemoMain.Create(nil);
    83: TheFormArray[ID] := TStarfieldMainForm.Create(nil);
    84: TheFormArray[ID] := TJvCharMapMainFrm.Create(nil);
    85: TheFormArray[ID] := TJvChartDemoForm.Create(nil);
    86: TheFormArray[ID] := TJvCheckBoxRadioBtnFrm.Create(nil);
    87: TheFormArray[ID] := TfrmCheckTVDemo.Create(nil);
    88: TheFormArray[ID] := TControlsExampleMainForm.Create(nil);
    89: TheFormArray[ID] := TCsvDataSourceForm.Create(nil);
    90: TheFormArray[ID] := TJvID3v1MainForm.Create(nil);
    91: TheFormArray[ID] := TJvID3v2MainForm.Create(nil);
    92: TheFormArray[ID] := TJvHiddenGemsForm.Create(nil);
    93: TheFormArray[ID] := TJvComboListBoxDemoFrm.Create(nil);
    94: TheFormArray[ID] := TJvComputerInfoExDemoFrm.Create(nil);
    95: TheFormArray[ID] := TJvDBActionMainFrm.Create(nil);
    96: TheFormArray[ID] := TJvDBFindEditDemoFrm.Create(nil);
    97: TheFormArray[ID] := TJvDBGridExportDemoMainFrm.Create(nil);
    98: TheFormArray[ID] := TJvDBHTLabelDemoMainFrm.Create(nil);
    99: TheFormArray[ID] := TJvDBImageDemoMainFrm.Create(nil);
    100: TheFormArray[ID] := TJvDbMaskEditDemoFrm.Create(nil);
    101: TheFormArray[ID] := TJvDBSearchDemoMainFrm.Create(nil);
    102: TheFormArray[ID] := TJvDesktopAlertDemoFrm.Create(nil);
    103: TheFormArray[ID] := TWebMapperDemoMainFrm.Create(nil);
    104: TheFormArray[ID] := TUseCaseDemoMainFrm.Create(nil);
    105: TheFormArray[ID] := TDependencyWalkerDemoMainFrm.Create(nil);
    106: TheFormArray[ID] := TJvDialogsDemoFrm.Create(nil);
    107: TheFormArray[ID] := TJvDomainUpDownDemoMainFrm.Create(nil);
    108: TheFormArray[ID] := TJvDotNetDemoMainFrm.Create(nil);
    109: TheFormArray[ID] := TJvErrorIndicatorMainDemoFrm.Create(nil);
    110: TheFormArray[ID] := TJvFooterAndGroupHeaderDemoFrm.Create(nil);
    111: TheFormArray[ID] := TJvFullColorCircleDlgMainFrm.Create(nil);
    112: TheFormArray[ID] := TJvFullColorDialogMainFrm.Create(nil);
    113: TheFormArray[ID] := TEditorMainForm.Create(nil);
    114: TheFormArray[ID] := TAnimatorForm.Create(nil);
    115: TheFormArray[ID] := TJvMouseGestureDemoMainFrm.Create(nil);
    116: TheFormArray[ID] := TJvNavPaneDemoMainFrm.Create(nil);
    117: TheFormArray[ID] := TfBouncingBalls.Create(nil);
    118: TheFormArray[ID] := TfrmDiningPhilosophers.Create(nil);
    119: TheFormArray[ID] := TJvMarkupLabelDemoMainFrm.Create(nil);
    120: TheFormArray[ID] := TJvMenusExampleMainFrm.Create(nil);
    121: TheFormArray[ID] := TJvOutlookBarCustomDrawDemoMainFrm.Create(nil);
    122: TheFormArray[ID] := TPackageModifierMainFrm.Create(nil);
    123: TheFormArray[ID] := TJvPageListTreeViewMainFrm.Create(nil);
    124: TheFormArray[ID] := TJvPanelDemoMainFrm.Create(nil);
    {$IFDEF INCLUDE_DEVEXP_CX}
    125: TheFormArray[ID] := TJvParameterListDemoMainFrm.Create(nil);
    {$ENDIF INCLUDE_DEVEXP_CX}
    //125: TheFormArray[ID] := T .Create(nil);
    //126: TheFormArray[ID] := T .Create(nil);
    //127: TheFormArray[ID] := T .Create(nil);
    //128: TheFormArray[ID] := T .Create(nil);
    //129: TheFormArray[ID] := T .Create(nil);
  else
    TheFormArray[ID] := TfrEmpty.Create(nil);
  end;

  //Execute the forms appearance only if they need to be shown
  if ShowForm then
  begin
    // embed the form in JvCaptionPanel1
    TheFormArray[ID].Parent := JvCaptionPanel1;
    TheFormArray[ID].BorderStyle := bsNone;
    TheFormArray[ID].Scaled := False;
    TheFormArray[ID].Visible := True;
    TheFormArray[ID].left := 0;
    TheFormArray[ID].top := 25;

    //take the with, height and caption from the form
    JvCaptionPanel1.width := TheFormArray[ID].Width + 5;
    JvCaptionPanel1.height := TheFormArray[ID].height + 30;
    JvCaptionPanel1.Caption := TheFormArray[ID].Caption;

    // check if the form has a MainMenu and plug it in
    if TheFormArray[ID].Menu <> nil then
    begin
      TheToolBar := TJvToolBar.Create(TheFormArray[ID]);
      TheToolBar.Parent := TheFormArray[ID];
      TheToolBar.Menu := TheFormArray[ID].Menu;
      TheToolBar.Flat := True;
    end;
  end;
  //save the last form ID
  FormID := ID;
end;

procedure TMainform.FormShow(Sender: TObject);
var
  IniFile: TIniFile;
  IniFileName: string;
begin

  IniFileName := ExtractFilePath(Application.ExeName) + 'data\JvMegaDemoConfig.ini';
  if not FileExists(IniFileName) then
  begin
    MessageDlg(
      'JVCL MegaDemo Configuration File ''' + IniFileName + ''' not found!'#13 +
      'Programm will be aborted!', mtError, [mbOK], 0);
    Exit;
  end;

  IniFile := TIniFile.Create(IniFileName);
  gBitmapFilePath := IniFile.ReadString('Main', 'BitmapFilePath', '');

  if FileExists(ExtractFilePath(Application.ExeName) + 'Help\JVCL3.HLP') then
    Application.HelpFile := ExtractFilePath(Application.ExeName) + 'Help\JVCL3.HLP'
  else
    Application.HelpFile := IniFile.ReadString('Main', 'HelpFile', '');

  {
  if not FileExists(Application.HelpFile) then
    if MessageDlg(
      'JVCL MegaDemo Help File ''' + Application.HelpFile + ''' was not found!'#13 +
      'Do you want to open the config file?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ShellExecute(0, nil, PChar('"' + IniFile.FileName + '"'), nil, nil, SW_SHOWNORMAL);
  }

  FreeAndNil(IniFile);

  //Show the Welcome-Frame
  CreateDemoForm(65);

  ReadAllDemosFromIni;

  // workaround, because anchors seems not to work here
  JvPageControlComps.width := self.width - 40;
end;

procedure TMainform.btnLoadIdeClick(Sender: TObject);
var
  aFileStringList: TStringList;
  SearchPathExpr: string;
begin
  if ModuleOfAddr(TheFormArray[FormID].ClassInfo) = '' then
  begin
    MessageDlg(
      'This functionality is disabled unless you build the demo from the'#13 +
      'Delphi IDE with map file generation enabled.', mtWarning, [mbOK], 0);
    Exit;
  end;

  screen.Cursor := crHourGlass;

  // uses the function 'ModuleOfAddr' from JclDebug unit to get the unit name
  SearchPathExpr := ExtractFilePath(Application.ExeName) +
    ModuleOfAddr(TheFormArray[FormID].ClassInfo) + '.pas';
  StrReplace(SearchPathExpr, '\bin', '\examples', [rfIgnoreCase]);

  // uses AdvBuildFileList to get the file location in the example diretory tree
  aFileStringList := TStringList.create;
  AdvBuildFileList(SearchPathExpr, faAnyFile, aFileStringList, amAny,
    [flFullNames, flRecursive], '', nil);

  screen.Cursor := crDefault;

  if aFileStringList.Count < 1 then
    MessageDlg('File not found', mtError, [mbOK], 0)
  else
    ShellExecute(0, nil, PChar('"' + aFileStringList.Strings[0] + '"'), nil, nil, SW_SHOWNORMAL);

  StatusBar.SimpleText := 'file ''' + aFileStringList.Strings[0] + ''' has been launched';
  aFileStringList.free;
end;

procedure TMainform.CompClick(Sender: TObject);

  procedure addWithFormat(aStr: string);
  begin
    JvRichEditHints.selattributes.style := [fsBold]; // set bold attribute
    JvRichEditHints.selattributes.Color := clNavy; // color to blue
    JvRichEditHints.seltext := aStr;
    JvRichEditHints.selattributes.style := []; // revert to normal
    JvRichEditHints.selattributes.Color := clBlack;
  // JvRichEditHints.selstart := JvRichEditHints.getTextLen; // set caret to end of text
  end;

var
  aSL, allSections, tempSL: TStringList;
  I, J: Integer;
  IniFile: TIniFile;
begin

  if (JvListBoxAllDemos.ItemIndex = -1) and (Sender = JvListBoxAllDemos) then // File is loading of nothing is selected
    Exit;

  aSL := TStringList.Create;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\data\JvMegaDemoAllDemoForms.ini');
  JvRichEditHints.Text := '';

 // there are 3 possible callers for his method:

  if Sender is TJvXPBarItem then
    aSL.CommaText := IntToStr(TJvXPBarItem(sender).Tag)
  else
  if Sender = JvListBoxAllDemos then
  begin
   // we have only the Demo Title, so we have to search for it in the while ini file
    IniFile.ReadSections(aSL);
    for I := 0 to aSL.Count - 1 do
      if IniFile.ReadString(aSL[I], 'Title', '') = JvListBoxAllDemos.Items[JvListBoxAllDemos.ItemIndex] then
      begin
        aSL.CommaText := aSL[I];
        Break;
      end;
  end
  else
  if Sender = JvListBoxDemosCompNameSorted then
  begin
    addWithFormat(JvListBoxDemosCompNameSorted.Items[JvListBoxDemosCompNameSorted.ItemIndex]);
    JvRichEditHints.Lines.add(' is used in following Demos:');
   // we have one the Comonent Name, so we have to search in whole ini, in the comma separated valuues
    allSections := TStringList.create;
    tempSL := TStringList.create;
    IniFile.ReadSections(allSections);
    for I := 0 to allSections.Count - 1 do
    begin
      tempSL.CommaText := IniFile.ReadString(allSections[I], 'compsUsed', '');
      for J := 0 to tempSL.count - 1 do
        if tempSL[J] = JvListBoxDemosCompNameSorted.Items[JvListBoxDemosCompNameSorted.ItemIndex] then
          aSL.Add(allSections[I]);
    end;

    allSections.Free;
    tempSL.Free;
  end
  else // the comps in the "Comps as in the Delphi IDE"
  begin
    aSL.CommaText := (Sender as TControl).Hint;
    addWithFormat((Sender as TControl).Name);
    JvRichEditHints.Lines.add(' is used in following Demos:');
  end;

 // in the CommaText of the StringList aSL are now the DemoFormId[s]
  for I := 0 to aSL.Count - 1 do
  begin
    JvRichEditHints.Lines.Add('');
    addWithFormat('http:' + aSL[I] + ' ' + IniFile.ReadString(aSL[I], 'Title', ''));
    JvRichEditHints.Lines.Add('');
    JvRichEditHints.Lines.Add(IniFile.ReadString(aSL[I], 'Descr', ''));
  end;

  FreeAndNil(aSL);
  FreeAndNil(IniFile);
end;

procedure TMainform.JvNavPanelBtnIdePageCtrlClick(Sender: TObject);
begin
  JvXPBarIDE.Collapsed := not JvXPBarIDE.Collapsed;
end;

procedure TMainform.ShowJVCLAboutDlg(Sender: TObject);
begin
  JvJVCLAboutComp.Execute;
end;

procedure TMainform.ShellExecHint(Sender: TObject);
var
  aFileName: string;
begin

  if Sender is TControl then
    aFileName := TControl(Sender).Hint
  else
  if Sender is TJvXPBarItem then
    aFileName := TJvXPBarItem(Sender).Hint
  else
  begin
    MessageDlg('Unknown Sender Type! Could not extract FileName from Hint', mtError, [mbOK], 0);
    Exit;
  end;

  // check if aFileName is a real filename
  if (pos('http', aFileName) = 0) and (pos('mailto', aFileName) = 0) then
  begin
    aFileName := ExtractFileDir(Application.ExeName) + '\' + aFileName;
    if not FileExists(aFilename) then
    begin
      MessageDlg('File ''' + aFilename + ''' could not be found!', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  screen.Cursor := crHourGlass;
  ShellExecute(0, nil, PChar(aFileName), nil, nil, SW_SHOWNORMAL);
  StatusBar.SimpleText := 'file ''' + aFileName + ''' has been launched';
  screen.Cursor := crDefault;
end;

procedure TMainform.JvNavPanelBtnExitClick(Sender: TObject);
begin
  close;
end;

procedure TMainform.JvXPBarIDECollapsedChange(Sender: TObject; Collapsing: Boolean);
begin
  if Collapsing then
    JvCaptionPanel1.Top := JvCaptionPanel1.Top - 83
  else
    JvCaptionPanel1.Top := JvCaptionPanel1.Top + 83;
end;

procedure TMainform.WinXPBarEnsureOnlyOneExpanded(Sender: TObject; Collapsing: Boolean);
// this procedure ensures that only one group is expanded - if the user choose so
var
  I: Integer;
begin
  if JvCheckBoxAllowOnlyOneExpanded.Checked = False then
    Exit;

  if Collapsing then
    Exit;

 // collapse all other JvXPBars except the sender
  for I := 0 to JvXPContainer1.ControlCount - 1 do
    if JvXPContainer1.Controls[I] <> Sender then
      TJvXPBar(JvXPContainer1.Controls[I]).collapsed := True;
end;

procedure TMainform.JvRichEditHintsURLClick(Sender: TObject;
  const URLText: string; Button: TMouseButton);
begin
  if StrIsDigit(copy(UrlText, 6, 99)) then
    self.CreateDemoForm(StrToInt(copy(UrlText, 6, 99)))
  else
    ShellExecute(0, nil, PChar('"' + UrlText + '"'), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainform.JvListBoxAllDemosChange(Sender: TObject);
var
  IniFile: TIniFile;
  allDemoFrms: TStringList;
  I: Integer;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\data\JvMegaDemoAllDemoForms.ini');
  allDemoFrms := TStringList.Create;
  try
    IniFile.ReadSections(allDemoFrms);
    for I := 0 to allDemoFrms.Count - 1 do
    begin
      JvListBoxAllDemos.Items.AddObject(IniFile.ReadString(allDemoFrms[I], 'Title', ''), nil)
    end;
  finally
    IniFile.Free;
    allDemoFrms.Free;
  end;
end;

procedure TMainform.JvNavPanelBtnJumpHelpClick(Sender: TObject);
var
  I: Integer;
  firstLine: string;
begin
  // get the first word in the JvRichedit, becaues it is the component Name
  // not a very nice design, but at least it works
  firstLine := JvRichEditHints.Lines[0];

  if firstLine = '' then
  begin
    MessageDlg(
      'This function is only availible when a component from the PageControl in '#13 +
      '''Components as in the Delphi IDE'' is the current item!', mtWarning, [mbOK], 0);
    Exit;
  end;
  I := 0;

  while firstLine[I] <> ' ' do
    inc(I);

  Application.HelpKeyword('T' + copy(FirstLine, 1, I - 1));
  StatusBar.SimpleText := 'help topic ''T' + copy(FirstLine, 1, I - 1) + ''' has been launched';
end;

procedure TMainform.JvXPBarSearchByCompNameAfterCollapsedChange(
  Sender: TObject; Collapsing: Boolean);
begin
  JvEdtCompSearch.SetFocus;
end;

procedure TMainform.JvXPBarBrowseDemosAfterCollapsedChange(
  Sender: TObject; Collapsing: Boolean);
begin
  JvListBoxAllDemos.SetFocus;
end;

procedure TMainform.JvNavPanelButton1Click(Sender: TObject);
var
  A, B: Double;
begin
  A := 0;
  B := 0;
  ShowMessageFmt('%f', [A / B]);
end;

end.
