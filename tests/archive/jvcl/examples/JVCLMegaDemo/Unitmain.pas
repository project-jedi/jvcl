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
  ImgList, ComCtrls, JvComCtrls;

type
  TMainform = class(TForm)
    tv_main: TJvTreeView;
    ilTreeview: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tv_mainClick(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure CreateFrame(const FrameID: integer);

  public
    { Public-Deklarationen }
    TheFrame: TFrame;
  end;

var
  Mainform: TMainform;

type
  TFrameClass = class of TFrame;


implementation
uses
  inifiles,
  jvFrameEmpty,         //Empty frame
  hello,                //Hello frame
  JvLabelsU,
  JvFormsU,
  JvDialogsU,
  jvButtonsU,
  JvExtEditsU,
  JvDateTimeU,
  JvPanelsU,
  JvChoosersU,
  JvFilesU,
  JvGraphicalU,
  JvUtilsU,
  JvControlsU,
  JvAniViewerU,          // A Simple ANI Viewer
  JvSearchFile,
  JvSearchFileU,
  JvMousePositionnerU,
  JvDataEmbeddedPU,
  JvBmpAnimatorU,
  JvArrowButtonU,
  JvClipboardViewerU,
  JvBrowseFolderU,
  jvInstallLabelU,
  JvWinDialogsU,
  JvEditsU;
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
end;

procedure TMainform.CreateFrame(const FrameID: integer);
begin
  if (TheFrame <> nil) then
    TheFrame.Free;

  case FrameID of
    0: TheFrame := Tfrm_hello.Create(nil);
    1: TheFrame := TJvFormsFrm.Create(nil);
    2: TheFrame := TJvDialogs.Create(nil);
    3: TheFrame := TJvUtilsFrm.Create(nil);
    4: TheFrame := TJvLabelsFrm.Create(nil);
    6: TheFrame := TJvEditsFrm.Create(nil);
    7: TheFrame := TJvEdits.Create(NIL);
    8: TheFrame := TJvFilesFrm.Create(nil);
    9: TheFrame := TJvPanelsFrm.Create(nil);
   10: TheFrame := TJvGraphicalFrm.Create(nil);
   11: TheFrame := TJvSearchFileFrm.Create(nil);
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
   23: TheFrame := TJvWinDialogs.Create(NIL);

  else
    TheFrame := tfrEmpty.create(NIL);
  end;

  TheFrame.Parent := mainForm;
  TheFrame.Align := alClient;
  theFrame.Visible := true;
  theFrame.SetFocus;
end;

procedure TMainform.tv_mainClick(Sender: TObject);
begin
  CreateFrame(tv_main.selected.AbsoluteIndex);
end;

end.
