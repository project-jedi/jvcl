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

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvComCtrls, JvComponent, ExtCtrls, JvCaptionPanel,
  ArrowButtonMainFormU, ImgList, ToolWin, JvToolBar,
  StdCtrls, JvBaseDlg, JvExControls, JvOutlookBar,
  JvExExtCtrls;

type
  TMainform = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    Panel1: TPanel;
    JvOutlookBar1: TJvOutlookBar;
    Panel2: TPanel;
    btnLoadIde: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
    procedure FormShow(Sender: TObject);
    procedure CreateDemoForm(const ID: integer; ShowForm: Boolean=true);
    procedure btnLoadIdeClick(Sender: TObject);
  end;

const
  MAX_FORMS       = 71;

var
  Mainform         : TMainform;
  TheToolBar       : TJvToolBar;
  TheFormArray     : array[1..MAX_FORMS] of TForm;
  FormID           : integer; 

implementation

uses
  JvFrameEmpty, hello, JvLabelsU, JvFormsU, JvDialogsU, JvButtonsU, JvDateTimeU,
  JvPanelsU, JvChoosersU, JvControlsU, JvSearchFiles, JvWinDialogsU,
  JvEditsU, JvAniMainFormU,
  JvSearchFileMainFormU, JvNTEventLogMainFormU, JvMruListMainFormU,
  JvLogFileMainFormU, InstallLabelMainFormU, 
  JvDBDateTimePickerMainFormU, ContentScrollerMainFormU, JvDataEmbeddedMainFormU,
  JvBrowseFolderMainFormU, 
  JvClipboardViewerMainFormU, JvZoomMainFormU,
  JvWindowsTitleMainFomU,
  RaHtHintsMainFormU, FileListBoxMainFormU, JvTreeViewAsMenuMainFormU,
  ListCombMainFormU, JvBalloonHintMainFormU,
  JvHTMLParserMainFormU,
  JvLinkLabelMainFormU, JvScreenCaptureMainFormU,
  JvShFileOperationMainFormU, JvSystemPopup2MainFormU, JvSystemPopupMainFormU,
  JvThumbnailMainFormU,
  RegTVMainFormU, RunDll32MainFormU, TimelineMainFormU,
  TipOfDayMainFormU, TMTimeLineMainFormU,
  JvZLibMultipleMainFormU, OtherStandAlone, Profiler32MainFormU,
  FindReplaceMainFormU, JvPlayListMainFormU,
  RessourcesFormMain, SearchingForm, JclDebug, JclStrings, JclFileUtils, ShellAPI;

{$R *.dfm}

procedure TMainform.FormDestroy(Sender: TObject);
var
  I:integer;
begin
  for I := 1 to MAX_FORMS do
    if (TheFormArray[I] <> nil) then
      TheFormArray[I].Free;
end;

procedure TMainform.CreateDemoForm(const ID: integer; ShowForm: Boolean);
begin

  if (TheFormArray[ID] <> nil) then
      freeAndNil(TheFormArray[ID]);

  case ID of

     1 : TheFormArray[ID]  := TJvFormsFrm.Create(nil);
     2 : TheFormArray[ID]  := TJvDialogs.Create(nil);
//     3 : TheFormArray[ID]  := TJvUtilsFrm.Create(nil);
     4 : TheFormArray[ID]  := TJvLabelsFrm.Create(nil);
     5 : TheFormArray[ID] := TRaHtHintsMainForm.Create(nil);
     6 : TheFormArray[ID] := TJvZoomMainForm.Create(nil);
     7 : TheFormArray[ID] := TJvEdits.Create(nil);
     8 : TheFormArray[ID] := TSearchingFormMain.Create(nil);
     9 : TheFormArray[ID] := TJvPanelsFrm.Create(nil);
//    10 : TheFormArray[ID] := TMonthCalendarMainForm.Create(nil);
    11 : TheFormArray[ID] := TJvSearchFileMainForm.Create(nil);
    12 : TheFormArray[ID] := TJvDateTimeFrm.Create(nil);
    13 : TheFormArray[ID] := TJvChoosersFrm.Create(nil);
    14 : TheFormArray[ID] := TJvControls.Create(nil);
    15 : TheFormArray[ID] := TJvAniMainForm.Create(nil);
//    16 : TheFormArray[ID] := TJvMousePositionnerMainForm.Create(nil);
    17 : TheFormArray[ID] := TJvDataEmbeddedMainForm.Create(nil);
//    18 : TheFormArray[ID] := TBmpAnimMainForm.Create(nil);
    19 : TheFormArray[ID] := TArrowButtonMainForm.Create(nil);
    20 : TheFormArray[ID] := TJvClipboardViewerMainForm.Create(nil);
    21 : TheFormArray[ID] := TJvBrowseFolderMainForm.Create(nil);
    22 : TheFormArray[ID] := TInstallLabelMainForm.Create(nil);
    24 : TheFormArray[ID] := TJvLogFileMainForm.Create(nil);
//    25 : TheFormArray[ID] := TOLBarMainForm.Create(nil);
//    26 : TheFormArray[ID]  := TControlsExampleMainForm.Create(nil);
//    27 : TheFormArray[ID] := TChangeNotificationMainForm.Create(nil);
//    28 : TheFormArray[ID] := TCreateProcessExampleMainForm.Create(nil);
    29 : TheFormArray[ID] := TJvNTEventLogMainForm.Create(nil);
//    30 : TheFormArray[ID] := TJvAppHotKeyDemoMainForm.Create(nil);
    31 : TheFormArray[ID] := TJvWindowsTitleMainForm.Create(nil);
//    32 : TheFormArray[ID] := TJvSpecialProgressMainForm.Create(nil);
//    33 : TheFormArray[ID] := TJvColorComboDemoMainForm.Create(nil);
    34 : TheFormArray[ID] := TContentScrollerMainForm.Create(nil);
//    35 : TheFormArray[ID] := TMailExampleMainForm.Create(nil);
    36 : TheFormArray[ID] := TJvTreeViewAsMenuMainForm.Create(nil);
    37 : TheFormArray[ID] := TListCombMainForm.Create(nil);
    38 : TheFormArray[ID] := TJvDBDateTimePickerMainForm.Create(nil);
//    39 : TheFormArray[ID] := TJvInspectorDBDemoMainForm.Create(nil);
    40 : TheFormArray[ID] := TJvMruListMainForm.Create(nil);
    41 : TheFormArray[ID] := TFileListBoxMainForm.Create(nil);
    42 : TheFormArray[ID] := TJvButtons.Create(nil);
    43 : TheFormArray[ID] := TJvBalloonHintMainForm.Create(nil);
//    44 : TheFormArray[ID] := TDSADialogsMainForm.Create(nil);
//    45 : TheFormArray[ID] := TfrmMessageDlgEditor.Create(nil);
    46 : TheFormArray[ID] := TJvHTMLParserMainForm.Create(nil);
    47 : TheFormArray[ID] := TJvLinkLabelMainForm.Create(nil);
    48 : TheFormArray[ID] := TJvScreenCaptureMainForm.Create(nil);
//    49 : TheFormArray[ID] := TJvShellHookDemoMainForm.Create(nil);
    50 : TheFormArray[ID] := TJvShFileOperationMainForm.Create(nil);
    51 : TheFormArray[ID] := TJvSystemPopupMainForm.Create(nil);
    52 : TheFormArray[ID] := TJvSystemPopup2MainForm.Create(nil);
    53 : TheFormArray[ID] := TJvThumbnailMainForm.Create(nil);
//    54 : TheFormArray[ID] := TJvTranslatorMainForm.Create(nil);
//    55 : TheFormArray[ID] := TJvWndProcHookDemoMainForm.Create(nil);
//    56 : TheFormArray[ID] := TJvWndProcHookDemoMainForm.Create(nil);
    57 : TheFormArray[ID] := TRegTVMainForm.Create(nil);
    58 : TheFormArray[ID] := TRunDll32MainForm.Create(nil);
//    59 : TheFormArray[ID] := TJvScrollingWindowMainForm.Create(nil);
    60 : TheFormArray[ID] := TTimelineMainForm.Create(nil);
    61 : TheFormArray[ID] := TTipOfDayMainForm.Create(nil);
    62 : TheFormArray[ID] := TTMTimeLineMainForm.Create(nil);
//    63 : TheFormArray[ID] := TTransBtnFormMain.Create(nil);
    64 : TheFormArray[ID] := TJvZLibMultipleMainForm.Create(nil);
    65 : TheFormArray[ID]  := TWelcomeForm.Create(nil);
    66 : TheFormArray[ID] := TOtherMainForm.Create(nil);
    67 : TheFormArray[ID] := TProfiler32MainForm.Create(nil);
    68 : TheFormArray[ID] := TFindReplaceMainForm.Create(nil);
    69 : TheFormArray[ID] := TJvPlaylistMainForm.Create(nil);
//    70 : TheFormArray[ID] := TImageWindowMainForm.Create(nil);
    71 : TheFormArray[ID] := TRessourcesForm.Create(nil);

  else
    TheFormArray[ID] := tfrEmpty.create(nil);
  end;

  //Execute the forms appearance only if they need to be shown
  if ShowForm then
  begin
    // embed the form in JvCaptionPanel1
    TheFormArray[ID].Parent := JvCaptionPanel1;
    TheFormArray[ID].BorderStyle :=  bsNone;
    TheFormArray[ID].Scaled := false;
    TheFormArray[ID].Visible := true;
    TheFormArray[ID].left := 0;
    TheFormArray[ID].top  := 25;

    //take the with, heigth and caption from the form
    JvCaptionPanel1.width  := TheFormArray[ID].Width + 5;
    JvCaptionPanel1.height := TheFormArray[ID].height + 30;
    JvCaptionPanel1.Caption:= TheFormArray[ID].Caption;

    // check if the form has a mainMenu and plug it in
    {$IFDEF COMPILER6_UP}
    if TheFormArray[ID].Menu <> nil then
    begin
       TheToolBar := TJvToolBar.Create(TheFormArray[ID]);
       TheToolBar.Parent := TheFormArray[ID];
       TheToolBar.Menu := TheFormArray[ID].Menu;
       TheToolBar.Flat := true;
    end;
    {$ENDIF COMPILER6_UP}
  end;
  //save the last form
  FormID := ID;
end;

procedure TMainform.JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
var
  P : TJvOutlookBarPage;
begin
  if (Index > -1) then
  begin
    P := JvOutlookBar1.Pages[JvOutlookBar1.ActivePageIndex];
    CreateDemoForm(P.Buttons[Index].tag, true);
  end;
end;

procedure TMainform.FormShow(Sender: TObject);
begin
  //Show the Welcome-Frame
  CreateDemoForm(65);
end;

procedure TMainform.btnLoadIdeClick(Sender: TObject);
var
  aFileStringList : TStringList;
  SearchPathExpr  : string;
begin
  if ModuleOfAddr(TheFormArray[FormID].ClassInfo) = '' then
  begin
    MessageDlg('This functionality is disabled unless'+#13+#10+
               'you build the demo from the Delphi IDE'+#13+#10+
               'with map file generation enabled.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // uses the function 'ModuleOfAddr' from JclDebug unit to get the unit name
  SearchPathExpr := ExtractFilePath(Application.ExeName) +
    ModuleOfAddr(TheFormArray[FormID].ClassInfo) + '.pas';
  StrReplace(SearchPathExpr, '\bin', '\examples', [rfIgnoreCase]);

  // uses AdvBuildFileList to get the file location in the example diretory tree
  aFileStringList := TStringList.create;
  AdvBuildFileList(SearchPathExpr, faAnyFile, aFileStringList, amAny,
   [flFullNames, flRecursive], '', nil);

 if aFileStringList.Count < 1 then
   MessageDlg('File not found', mtError, [mbOK], 0)
 else
   ShellExecute(0, nil, PChar('"' +aFileStringList.Strings[0]+ '"'), nil, nil, SW_SHOWNORMAL);

 aFileStringList.free;

end;

end.


