{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, JvComponent, JvTrayIcon, ExtCtrls,
  JvExControls, JvDockControlForm, ImgList;

type
  TfrmMain = class(TForm)
    JvTrayIcon1: TJvTrayIcon;
    GroupBox1: TGroupBox;
    chkActive: TCheckBox;
    Label1: TLabel;
    edHint: TEdit;
    chkSnap: TCheckBox;
    chkTaskBar: TCheckBox;
    chkTaskList: TCheckBox;
    chkAutoHide: TCheckBox;
    chkRestoreClick: TCheckBox;
    chkRestoreDblClick: TCheckBox;
    chkMinClick: TCheckBox;
    chkMinDblClick: TCheckBox;
    popTrayIcon: TPopupMenu;
    mnuShowHide: TMenuItem;
    chkPopUp: TCheckBox;
    btnUpdate: TButton;
    chkDropDown: TCheckBox;
    chkAutoRestore: TCheckBox;
    RestoreTimer: TTimer;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    edBalloonTitle: TEdit;
    Label3: TLabel;
    edBalloonText: TEdit;
    btnBalloon: TButton;
    Label4: TLabel;
    cbBalloonType: TComboBox;
    chkAutoHideIcon: TCheckBox;
    ImageList1: TImageList;
    chkAnimated: TCheckBox;
    procedure btnUpdateClick(Sender: TObject);
    procedure mnuShowHideClick(Sender: TObject);
    procedure chkRestoreClickClick(Sender: TObject);
    procedure chkRestoreDblClickClick(Sender: TObject);
    procedure chkMinClickClick(Sender: TObject);
    procedure chkMinDblClickClick(Sender: TObject);
    procedure RestoreTimerTimer(Sender: TObject);
    procedure chkAutoRestoreClick(Sender: TObject);
    procedure btnBalloonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkActiveClick(Sender: TObject);
    procedure chkAnimatedClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnUpdateClick(Sender: TObject);
var Options:TTrayVisibilities;
begin
  with JvTrayIcon1 do
  begin
    Active := false;
    Animated := chkAnimated.Checked;
    IconIndex := -1;
    Hint := edHint.Text;
    Snap := chkSnap.Checked;
    if chkPopUp.Checked then
      PopUpMenu := popTrayIcon
    else
      PopUpMenu := nil;
    if chkDropDown.Checked then
      DropDownMenu := popTrayIcon
    else
      DropDownMenu := nil;
    Options := [];
    if chkTaskBar.Checked then
      Include(Options,tvVisibleTaskBar);
    if chkTaskList.Checked then
      Include(Options,tvVisibleTaskList);
    if chkAutohide.Checked then
      Include(Options,tvAutoHide);
    if chkAutoHideIcon.Checked then
      Include(Options,tvAutoHideIcon);
    if chkRestoreClick.Checked and chkRestoreClick.Enabled then
      Include(Options,tvRestoreClick);
    if chkRestoreDblClick.Checked and chkRestoreDblClick.Enabled then
      Include(Options,tvRestoreDbClick);
//    if chkMinClick.Checked and chkMinClick.Enabled then
//      Include(Options,tvMinimizeClick);
//    if chkMinDblClick.Checked and chkMinDblClick.Enabled then
//      Include(Options,tvMinimizeDbClick);
    Visibility := Options;
    Active := chkActive.Checked;
    btnBalloon.Enabled := Active;
  end;
end;

procedure TfrmMain.mnuShowHideClick(Sender: TObject);
begin
  if IsWindowVisible(Handle) then
    JvTrayIcon1.HideApplication
  else
    JvTrayIcon1.ShowApplication;
end;

procedure TfrmMain.chkRestoreClickClick(Sender: TObject);
begin
  chkRestoreDblClick.Enabled := not chkRestoreClick.Checked;
end;

procedure TfrmMain.chkRestoreDblClickClick(Sender: TObject);
begin
  chkRestoreClick.Enabled := not chkRestoreDblClick.Checked;
end;

procedure TfrmMain.chkMinClickClick(Sender: TObject);
begin
  chkMinDblClick.Enabled := not chkMinClick.Checked;
end;

procedure TfrmMain.chkMinDblClickClick(Sender: TObject);
begin
  chkMinClick.Enabled := not chkMinDblClick.Checked;
end;

procedure TfrmMain.RestoreTimerTimer(Sender: TObject);
begin
  if not IsWindowVisible(Handle) then
    JvTrayIcon1.ShowApplication;
end;

procedure TfrmMain.chkAutoRestoreClick(Sender: TObject);
begin
  RestoreTimer.Enabled := not chkAutoRestore.Checked;
end;

procedure TfrmMain.btnBalloonClick(Sender: TObject);
begin
  JvTrayIcon1.BalloonHint(edBalloonTitle.Text,edBalloonText.Text,TBalloonType(cbBalloontype.ItemIndex),5000,true);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  cbBalloonType.ItemIndex := 0;
end;

procedure TfrmMain.chkActiveClick(Sender: TObject);
begin
  JvTrayIcon1.Active := chkActive.Checked;
  btnBalloon.Enabled := JvTrayIcon1.Active;
end;

procedure TfrmMain.chkAnimatedClick(Sender: TObject);
begin
  JvTrayIcon1.Animated := chkAnimated.Checked;
end;

end.
