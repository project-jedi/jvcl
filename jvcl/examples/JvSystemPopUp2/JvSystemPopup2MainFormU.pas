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

{$I jvcl.inc}

unit JvSystemPopup2MainFormU;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, Menus, JvComponent, JvSystemPopup, ActnList,
  StdCtrls, ExtCtrls;

type
  TJvSystemPopup2MainForm = class(TForm)
    JvSystemPopup1: TJvSystemPopup;
    PopupMenu1: TPopupMenu;
    est1: TMenuItem;
    ActionList1: TActionList;
    actClickToCheck: TAction;
    Action11: TMenuItem;
    PopupMenu2: TPopupMenu;
    Action21: TMenuItem;
    btnSwitch: TButton;
    rgrPosition: TRadioGroup;
    rgrPositionInMenu: TRadioGroup;
    edtAdd: TEdit;
    btnAdd: TButton;
    Radio21: TMenuItem;
    Radio31: TMenuItem;
    Menu21: TMenuItem;
    DisabledItem1: TMenuItem;
    actClickToSwitchIcon: TAction;
    ImageList1: TImageList;
    ClickToSwitchIcon1: TMenuItem;
    chbHideHideableItem: TCheckBox;
    actHideableItem: TAction;
    HideableItem1: TMenuItem;
    HideableItem2: TMenuItem;
    actShortCut: TAction;
    ShortCut1: TMenuItem;
    ShortCut2: TMenuItem;
    procedure actClickToCheckExecute(Sender: TObject);
    procedure btnSwitchClick(Sender: TObject);
    procedure rgrPositionClick(Sender: TObject);
    procedure rgrPositionInMenuClick(Sender: TObject);
    procedure RadioClick(Sender: TObject);
    procedure actClickToSwitchIconExecute(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure chbHideHideableItemClick(Sender: TObject);
    procedure actHideableItemExecute(Sender: TObject);
    procedure actShortCutExecute(Sender: TObject);
  end;

var
  JvSystemPopup2MainForm: TJvSystemPopup2MainForm;

implementation

uses
  JvTypes;

{$R *.dfm}

procedure TJvSystemPopup2MainForm.actClickToCheckExecute(Sender: TObject);
begin
  if Sender is TAction then
    with Sender as TAction do
      Checked := not Checked;
end;

procedure TJvSystemPopup2MainForm.btnSwitchClick(Sender: TObject);
begin
  with JvSystemPopup1 do
    if Popup = PopupMenu1 then
      Popup := PopupMenu2
    else
      Popup := PopupMenu1;
end;

procedure TJvSystemPopup2MainForm.rgrPositionClick(Sender: TObject);
begin
  JvSystemPopup1.Position := TJvPopupPosition(rgrPosition.ItemIndex);
end;

procedure TJvSystemPopup2MainForm.rgrPositionInMenuClick(Sender: TObject);
begin
  JvSystemPopup1.PositionInMenu := TJvPositionInMenu(rgrPositionInMenu.ItemIndex);
end;

procedure TJvSystemPopup2MainForm.RadioClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    with Sender as TMenuItem do
      Checked := not Checked;
end;

procedure TJvSystemPopup2MainForm.actClickToSwitchIconExecute(Sender: TObject);
begin
  if Sender is TAction then
    with Sender as TAction do
      ImageIndex := ((ImageIndex + 2) mod 4) - 1;
end;

procedure TJvSystemPopup2MainForm.btnAddClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(nil);
  with MenuItem do
  begin
    Caption := edtAdd.Text;
  end;
  PopupMenu1.Items.Items[0].Add(MenuItem);
  if JvSystemPopup1.Popup = PopupMenu1 then
    JvSystemPopup1.Refresh;
end;

procedure TJvSystemPopup2MainForm.chbHideHideableItemClick(Sender: TObject);
begin
  actHideableItem.Visible := not chbHideHideableItem.Checked;
end;

procedure TJvSystemPopup2MainForm.actHideableItemExecute(Sender: TObject);
begin
  chbHideHideableItem.Checked := True;
end;

procedure TJvSystemPopup2MainForm.actShortCutExecute(Sender: TObject);
begin
  ShowMessage('ShortCut');
end;

end.

