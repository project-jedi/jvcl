/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

 This is a port from the demo written in Delphi by
   Ralf Grenzing [ralfgspam@gmx.de]
   Uwe Rupprecht [uwe-rupprecht@gmx.de]
   Michael Beck [mbeck1@compuserve.com]
   Angus Johnson [ajohnson@rpi.net.au]

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

******************************************************************/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
#include "JvToolbar.hpp"
#include "JclDebug.hpp"
#include "WelcomeForm.h"
#include "JvFormsForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvCaptionPanel"
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvExExtCtrls"
#pragma link "JvOutlookBar"
#pragma link "JvToolbar"
#pragma resource "*.dfm"
TfrmMain *frmMain;

const MAX_FORMS = 71;
static TJvToolBar *TheToolBar;
static TForm* TheFormArray[MAX_FORMS];
static int FormID;

void __fastcall TfrmMain::CreateDemoForm(int ID, bool ShowForm /*=true*/)
{
  if (TheFormArray[FormID] != NULL)
    JvCaptionPanel1->RemoveControl(TheFormArray[FormID]);

  ID--;
  if (TheFormArray[ID] != NULL)
  {
      delete TheFormArray[ID];
      TheFormArray[ID] = NULL;
  }

  switch (ID+1)
  {
     case 1:
      TheFormArray[ID]  = new TfrmJvForms(NULL);
      break;
/*     2 : TheFormArray[ID]  := TJvDialogs.Create(nil);
     3 : TheFormArray[ID]  := TJvUtilsFrm.Create(nil);
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
    32 : TheFormArray[ID] := TJvSpecialProgressMainForm.Create(nil);
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
    54 : TheFormArray[ID] := TJvTranslatorMainForm.Create(nil);
//    55 : TheFormArray[ID] := TJvWndProcHookDemoMainForm.Create(nil);
//    56 : TheFormArray[ID] := TJvWndProcHookDemoMainForm.Create(nil);
    57 : TheFormArray[ID] := TRegTVMainForm.Create(nil);
    58 : TheFormArray[ID] := TRunDll32MainForm.Create(nil);
//    59 : TheFormArray[ID] := TJvScrollingWindowMainForm.Create(nil);
    60 : TheFormArray[ID] := TTimelineMainForm.Create(nil);
    61 : TheFormArray[ID] := TTipOfDayMainForm.Create(nil);
    62 : TheFormArray[ID] := TTMTimeLineMainForm.Create(nil);
//    63 : TheFormArray[ID] := TTransBtnFormMain.Create(nil);
    64 : TheFormArray[ID] := TJvZLibMultipleMainForm.Create(nil);*/
    case 65:
      TheFormArray[ID] = new TfrmWelcome(NULL);
      break;
/*    66 : TheFormArray[ID] := TOtherMainForm.Create(nil);
    67 : TheFormArray[ID] := TProfiler32MainForm.Create(nil);
    68 : TheFormArray[ID] := TFindReplaceMainForm.Create(nil);
    69 : TheFormArray[ID] := TJvPlaylistMainForm.Create(nil);
//    70 : TheFormArray[ID] := TImageWindowMainForm.Create(nil);
    71 : TheFormArray[ID] := TRessourcesForm.Create(nil);

  else
    TheFormArray[ID] := tfrEmpty.create(nil);*/
  }

  //Execute the forms appearance only if they need to be shown
  if (TheFormArray[ID] != NULL && ShowForm)
  {
    // embed the form in JvCaptionPanel1
    TheFormArray[ID]->Parent = JvCaptionPanel1;
    TheFormArray[ID]->BorderStyle =  bsNone;
    TheFormArray[ID]->Scaled = false;
    TheFormArray[ID]->Visible = true;
    TheFormArray[ID]->Left = 0;
    TheFormArray[ID]->Top  = 25;

    //take the with, heigth and caption from the form
    JvCaptionPanel1->Width  = TheFormArray[ID]->Width + 5;
    JvCaptionPanel1->Height = TheFormArray[ID]->Height + 30;
    JvCaptionPanel1->Caption= TheFormArray[ID]->Caption;

    // check if the form has a mainMenu and plug it in
    #ifdef COMPILER6_UP
    if (TheFormArray[ID]->Menu != NULL)
    {
       TheToolBar = TJvToolBar->Create(TheFormArray[ID]);
       TheToolBar->Parent = TheFormArray[ID];
       TheToolBar->Menu = TheFormArray[ID]->Menu;
       TheToolBar->Flat = true;
    }
    #endif COMPILER6_UP
  }
  //save the last form
  FormID = ID;
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
  for (int I = 0; I < MAX_FORMS; I++)
    TheFormArray[I] = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormDestroy(TObject *Sender)
{
  for (int I = 0; I < MAX_FORMS; I++)
    if (TheFormArray[I] != NULL)
      delete TheFormArray[I];
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  //Show the Welcome-Frame
  CreateDemoForm(65);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvOutlookBar1ButtonClick(TObject *Sender,
      int Index)
{
  TJvOutlookBarPage *P;

  if (Index > -1)
  {
    P = (*JvOutlookBar1->Pages)[JvOutlookBar1->ActivePageIndex];
    CreateDemoForm((*P->Buttons)[Index]->Tag, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnLoadIdeClick(TObject *Sender)
{
  TStringList *aFileStringList;
  AnsiString SearchPathExpr;

  if (ModuleOfAddr(TheFormArray[FormID]->ClassInfo()) == "")
  {
    MessageDlg("This functionality is disabled unless\n"
               "you build the demo from the C++ Builder IDE\n"
               "with map file generation enabled.", mtWarning, TMsgDlgButtons() << mbOK, 0);
    return;
  }

  // uses the function "ModuleOfAddr" from JclDebug unit to get the unit name
  SearchPathExpr = ExtractFilePath(Application->ExeName) +
    ModuleOfAddr(TheFormArray[FormID]->ClassInfo()) + "->pas";
  StrReplace(SearchPathExpr, "\bin", "\examples", TReplaceFlags() << rfIgnoreCase);

  // uses AdvBuildFileList to get the file location in the example diretory tree
  aFileStringList = new TStringList();
  AdvBuildFileList(SearchPathExpr, faAnyFile, aFileStringList, amAny,
   TFileListOptions() << flFullNames << flRecursive, "", NULL);

 if (aFileStringList->Count < 1)
   MessageDlg("File not found", mtError, TMsgDlgButtons() << mbOK, 0);
 else
   ShellExecute(0,
                NULL,
                ("\"" + aFileStringList->Strings[0] + "\"").c_str(),
                NULL,
                NULL,
                SW_SHOWNORMAL);

 delete aFileStringList;
}
//---------------------------------------------------------------------------
