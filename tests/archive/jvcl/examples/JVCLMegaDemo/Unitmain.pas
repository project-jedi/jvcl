{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [ralfgspam@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au
                                                                  
 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvComCtrls, JvComponent, JvOLBar, ExtCtrls, JvAutoSizeCompo,
  JvCaptionPanel, ArrowButtonMainFormU, ImgList;

type
  TMainform = class(TForm)
    JvOutlookBar1: TJvOutlookBar;
    JvCaptionPanel1: TJvCaptionPanel;
    procedure FormDestroy(Sender: TObject);
    procedure JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
    procedure FormShow(Sender: TObject);
  private
    procedure CreateDemoForm(const ID: integer);
  end;

var
  Mainform : TMainform;
  TheForm  : TForm;

implementation

uses
  jvFrameEmpty, hello, JvLabelsU, JvFormsU, JvDialogsU, jvButtonsU, JvDateTimeU,
  JvPanelsU, JvChoosersU, JvUtilsU, JvControlsU, JvSearchFiles, JvWinDialogsU,
  JvEditsU, BmpAnimMainFormU, ChangeNotificationMainFormU, JvAniMainFormU,
  JvMousePositionnerMainFormU, MonthCalendarMainFormU, MailExampleMainFormU,
  OLBarMainFormU, JvSearchFileMainFormU, JvNTEventLogMainFormU, JvMruListMainFormU,
  JvLogFileMainFormU, InstallLabelMainFormU, JvAppHotKeyDemoMainFormU,
  JvDBDateTimePickerMainFormU, ContentScrollerMainFormU, JvDataEmbeddedMainFormU,
  JvBrowseFolderMainFormU, CreateProcessExampleMainFormU,
  JvClipboardViewerMainFormU, JvZoomMainFormU, JvSpecialProgressMainFormU,
  JvColorComboDemoMainFormU, JvInspectorDBDemoMainFormU, JvWindowsTitleMainFomU,
  RaHtHintsMainFormU, FileListBoxMainFormU, JvTreeViewAsMenuMainFormU,
  ListCombMainFormU, ControlsExampleMainFormU, JvBalloonHintMainFormU,
  DSADialogsMainFormU, MessageDlgEditorMain, JvHTMLParserMainFormU,
  JvLinkLabelMainFormU, JvScreenCaptureMainFormU, JvShellHookDemoMainFormU,
  JvShFileOperationMainFormU, JvSystemPopup2MainFormU, JvSystemPopupMainFormU,
  JvThumbnailMainFormU, JvTranslatorMainFormU, JvWndProcHookDemoMainFormU,
  RegTVMainFormU, RunDll32MainFormU, ScrollWinMainFormU, TimelineMainFormU,
  TipOfDayMainFormU, TMTimeLineMainFormU, TransBtnFormMainU,
  JvZLibMultipleMainFormU, OtherStandAlone, Profiler32MainFormU;

{$R *.DFM}

procedure TMainform.FormDestroy(Sender: TObject);
begin
  if (TheForm <> nil) then
    TheForm.Free;
end;

procedure TMainform.CreateDemoForm(const ID: integer);
begin

  if (TheForm <> nil) then
    freeAndNil(TheForm);

  case ID of
    65 : TheForm  := Tfrm_hello.Create(nil);
     1 : TheForm  := TJvFormsFrm.Create(nil);
     2 : TheForm  := TJvDialogs.Create(nil);
     3 : TheForm  := TJvUtilsFrm.Create(nil);
     4 : TheForm  := TJvLabelsFrm.Create(nil);
    26 : TheForm  := TControlsExampleMainForm.Create(nil);
     7 : TheForm := TJvEdits.Create(nil);
     9 : TheForm := TJvPanelsFrm.Create(nil);
    12 : TheForm := TJvDateTimeFrm.Create(nil);
    13 : TheForm := TJvChoosersFrm.Create(nil);
    14 : TheForm := TJvControls.Create(nil);
    42 : TheForm := TJvButtons.Create(nil);
    18 : TheForm := TBmpAnimMainForm.Create(nil);
    19 : TheForm := TArrowButtonMainForm.Create(nil);
    27 : TheForm := TChangeNotificationMainForm.Create(nil);
    15 : TheForm := TJvAniMainForm.Create(nil);
    16 : TheForm := TJvMousePositionnerMainForm.Create(nil);
    17 : TheForm := TJvDataEmbeddedMainForm.Create(nil);
    10 : TheForm := TMonthCalendarMainForm.Create(nil);
    35 : TheForm := TMailExampleMainForm.Create(nil);
    25 : TheForm := TOLBarMainForm.Create(nil);
    11 : TheForm := TJvSearchFileMainForm.Create(nil);
    29 : TheForm := TJvNTEventLogMainForm.Create(nil);
    40 : TheForm := TJvMruListMainForm.Create(nil);
    24 : TheForm := TJvLogFileMainForm.Create(nil);
    22 : TheForm := TInstallLabelMainForm.Create(nil);
    30 : TheForm := TJvAppHotKeyDemoMainForm.Create(nil);
    38 : TheForm := TJvDBDateTimePickerMainForm.Create(nil);
    34 : TheForm := TContentScrollerMainForm.Create(nil);
    21 : TheForm := TJvBrowseFolderMainForm.Create(nil);
    28 : TheForm := TCreateProcessExampleMainForm.Create(nil);
    20 : TheForm := TJvClipboardViewerMainForm.Create(nil);
     6 : TheForm := TJvZoomMainForm.Create(nil);
    32 : TheForm := TJvSpecialProgressMainForm.Create(nil);
    33 : TheForm := TJvColorComboDemoMainForm.Create(nil);
    39 : TheForm := TJvInspectorDBDemoMainForm.Create(nil);
    31 : TheForm := TJvWindowsTitleMainForm.Create(nil);
     5 : TheForm := TRaHtHintsMainForm.Create(nil);
    41 : TheForm := TFileListBoxMainForm.Create(nil);
    36 : TheForm := TJvTreeViewAsMenuMainForm.Create(nil);
    37 : TheForm := TListCombMainForm.Create(nil);
    43 : TheForm := TJvBalloonHintMainForm.Create(nil);
    44 : TheForm := TDSADialogsMainForm.Create(nil);
    45 : TheForm := TfrmMessageDlgEditor.Create(nil);
    46 : TheForm := TJvHTMLParserMainForm.Create(nil);
    47 : TheForm := TJvLinkLabelMainForm.Create(nil);
    48 : TheForm := TJvScreenCaptureMainForm.Create(nil);
    49 : TheForm := TJvShellHookDemoMainForm.Create(nil);
    50 : TheForm := TJvShFileOperationMainForm.Create(nil);
    51 : TheForm := TJvSystemPopupMainForm.Create(nil);
    52 : TheForm := TJvSystemPopup2MainForm.Create(nil);
    53 : TheForm := TJvThumbnailMainForm.Create(nil);
    54 : TheForm := TJvTranslatorMainForm.Create(nil);
    55 : TheForm := TJvWndProcHookDemoMainForm.Create(nil);
    56 : TheForm := TJvWndProcHookDemoMainForm.Create(nil);
    57 : TheForm := TRegTVMainForm.Create(nil);
    58 : TheForm := TRunDll32MainForm.Create(nil);
    59 : TheForm := TJvScrollingWindowMainForm.Create(nil);
    60 : TheForm := TTimelineMainForm.Create(nil);
    61 : TheForm := TTipOfDayMainForm.Create(nil);
    62 : TheForm := TTMTimeLineMainForm.Create(nil);
    63 : TheForm := TTransBtnFormMain.Create(nil);
    64 : TheForm := TJvZLibMultipleMainForm.Create(nil);
    66 : TheForm := TOtherMainForm.Create(nil);
    67 : TheForm := TProfiler32MainForm.Create(nil);
//    59 : TheForm := T MainForm.Create(nil);
//    59 : TheForm := T MainForm.Create(nil);
//    59 : TheForm := T MainForm.Create(nil);
//    59 : TheForm := T MainForm.Create(nil);
//    59 : TheForm := T MainForm.Create(nil);

  else
    TheForm := tfrEmpty.create(nil);
  end;


  TheForm.Parent := JvCaptionPanel1;

  JvCaptionPanel1.Caption := TheForm.Caption;
  TheForm.BorderStyle :=  bsNone;
  TheForm.Visible := true;
  TheForm.left := 0;
  TheForm.top  := 25;

  //adjust the with and heigth from the form
  JvCaptionPanel1.width  := TheForm.Width + 5;
  JvCaptionPanel1.height := TheForm.height + 30;

end;

procedure TMainform.JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
var
  P : TJvOutlookBarPage;
begin
  if (Index > -1) then
  begin
    P := JvOutlookBar1.Pages[JvOutlookBar1.ActivePageIndex];
    CreateDemoForm(P.Buttons[Index].tag);
  end;
end;

procedure TMainform.FormShow(Sender: TObject);
begin
  //Show the Welcome-Frame
  CreateDemoForm(65);
//  TheFrame := tfrm_hello.Create(self);
//  TheFrame.parent := mainform;
//  TheFrame.align := alClient;
//  TheFrame.Visible := true;

 //Start the scrolling of the ScollText comp
//  tfrm_hello(TheFrame).frmh_st.active := true;
end;

end.


