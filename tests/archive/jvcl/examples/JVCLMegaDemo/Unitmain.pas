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
  ComCtrls, JvComCtrls, JvComponent, JvOLBar, ExtCtrls, JvAutoSizeCompo;

type
  TMainform = class(TForm)
    JvOutlookBar1: TJvOutlookBar;
    pnlParent: TPanel;
    JvAutoSizeCompo1: TJvAutoSizeCompo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
  private
    procedure CreateDemo(const ID: integer);
  end;

var
  Mainform : TMainform;
  TheFrame : TFrame;
  TheForm  : TForm;

implementation

uses
  jvFrameEmpty, hello, JvLabelsU, JvFormsU, JvDialogsU, jvButtonsU, JvDateTimeU,
  JvPanelsU, JvChoosersU, JvFilesU, JvUtilsU, JvControlsU, JvAniViewerU,
  JvSearchFiles, JvMousePositionnerU, JvDataEmbeddedPU, JvBmpAnimatorU,
  JvArrowButtonU, JvClipboardViewerU, JvBrowseFolderU, JvInstallLabelU,
  JvWinDialogsU, JvEditsU, JvControlsMainU, JvLogFileDemoU, JvOutlookBarU,
  JvChangeNotifyUMainForm, JvCreateProcessFormU, JvNTEventLogFormU,
  JvSearchFileForm, JvZoomU, JvWindowsLister, JvColorComboU, JvColoredHintU,
  JvContentScroller, JvMailU, JvTreeViewAsMenuU, JvMonthCalendar2U, JvListCombU,
  JvDBDateTimeU, JvInspectorU, JvHotKeyFormU, JvMruListU;

{$R *.DFM}

procedure TMainform.FormCreate(Sender: TObject);
begin

  //Show the Welcome-Frame
  TheFrame := tfrm_hello.Create(self);
  TheFrame.parent := mainform;
  TheFrame.align := alClient;
  TheFrame.Visible := true;

 //Start the scrolling of the ScollText comp
  tfrm_hello(TheFrame).frmh_st.active := true;
end;

procedure TMainform.FormDestroy(Sender: TObject);
begin
  if (TheFrame <> nil) then
    TheFrame.Free;
  if (TheForm <> nil) then
    TheForm.Free;
end;

procedure TMainform.CreateDemo(const ID: integer);
begin
  if (TheFrame <> nil) then
    freeAndNil(TheFrame);
  if (TheForm <> nil) then
    freeAndNil(TheForm);

  case ID of
    0: TheFrame := Tfrm_hello.Create(nil);
    1: TheFrame := TJvFormsFrm.Create(nil);
    2: TheFrame := TJvDialogs.Create(nil);
    3: TheFrame := TJvUtilsFrm.Create(nil);
    4: TheFrame := TJvLabelsFrm.Create(nil);
    6: TheForm  := TJvZoomForm.Create(MainForm);
    7: TheFrame := TJvEdits.Create(nil);
    8: TheFrame := TJvFilesFrm.Create(nil);
    9: TheFrame := TJvPanelsFrm.Create(nil);
   10: TheForm  := TJvMonthCalendar2Form.Create(MainForm);
   11: TheForm  := TJvSeachFilesForm.Create(MainForm);
   12: TheFrame := TJvDateTimeFrm.Create(nil);
   13: TheFrame := TJvChoosersFrm.Create(nil);
   14: TheFrame := TJvControls.Create(nil);
   15: TheFrame := TjvAniViewer.Create(nil);
   16: TheFrame := TJvMousePositionnerFrm.Create(nil);
   17: TheFrame := TJvDataEmbeddedFrm.Create(nil);
   18: TheFrame := TJvBmpAnimatorFrm.Create(nil);
   19: TheFrame := TJvArrowButtonFrm.Create(nil);
   20: TheFrame := TJvClipboardViewerFrm.Create(nil);
   21: TheFrame := TJvBrowseFolderFrm.Create(nil);
   22: TheFrame := TJvInstallLabelFrm.Create(nil);
   23: TheFrame := TJvWinDialogs.Create(nil);
   24: TheFrame := TJvLogFileDemo.Create(nil);
   25: TheForm  := TJvOutlookBarForm.Create(MainForm);
   26: TheForm  := TJvControlsMainForm.Create(MainForm);
   27: TheForm  := TJvChangeNotifyMainForm.Create(MainForm);
   28: TheForm  := TJvCreateProcessForm.Create(MainForm);
   29: TheForm  := TJvNTEventLogForm.Create(MainForm);
   30: TheForm  := TJvHotKeyForm.Create(MainForm);
   31: TheForm  := TJvWindowsListerForm.Create(MainForm);
   32: TheForm  := TJvColorCombo.Create(MainForm);
   33: TheForm  := TJvColoredHint.Create(MainForm);
   34: TheForm  := TJvContenScrollerForm.Create(MainForm);
   35: TheForm  := TJvMailForm.Create(MainForm);
   36: TheForm  := TJvTreeViewAsMenu.Create(MainForm);
   37: TheForm  := TJvListCombForm.Create(MainForm);
   38: TheForm  := TJvDBDateTimeForm.Create(MainForm);
   39: TheForm  := TJvInspectorDBForm.Create(MainForm);
   40: TheForm  := TJvMruListForm.Create(MainForm);

  else
    TheFrame := tfrEmpty.create(nil);
  end;

  if TheFrame <> nil then
  begin
    TheFrame.Parent := pnlParent;
    TheFrame.Align := alClient;
    TheFrame.Visible := true;
    TheFrame.SetFocus;
  end;

 if TheForm <> nil then
 begin
   TheForm.Parent := pnlParent;
   TheForm.Visible := true;
   TheForm.left := 10;
   TheForm.top  := 10;
   TheForm.SetFocus;
  end;
end;

procedure TMainform.JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
var
  P : TJvOutlookBarPage;
begin
  if (Index > -1) then
  begin
    P := JvOutlookBar1.Pages[JvOutlookBar1.ActivePageIndex];
    CreateDemo(P.Buttons[Index].tag);
  end;
end;

end.


