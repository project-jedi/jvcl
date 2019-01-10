{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

unit CaptionBtnMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvCaptionButton, ExtCtrls, JvComponent, ComCtrls, ImgList;

type
  TCaptionBtnMainForm = class(TForm)
    JvCaptionButton1: TJvCaptionButton;
    Label1: TLabel;
    udPosition: TUpDown;
    lblPos: TLabel;
    chkToggle: TCheckBox;
    chkDown: TCheckBox;
    Label3: TLabel;
    cbStandard: TComboBox;
    chkVisible: TCheckBox;
    Label2: TLabel;
    edCaption: TEdit;
    Label4: TLabel;
    cbBorderStyle: TComboBox;
    gbButtons: TGroupBox;
    chkSysMenu: TCheckBox;
    chkMax: TCheckBox;
    chkMin: TCheckBox;
    chkHelp: TCheckBox;
    meEvents: TMemo;
    Label5: TLabel;
    edHint: TEdit;
    Label6: TLabel;
    chkShowHints: TCheckBox;
    chkEnabled: TCheckBox;
    chkLogEvents: TCheckBox;
    tbBtnWidth: TTrackBar;
    Label7: TLabel;
    Bevel1: TBevel;
    btnAdd: TButton;
    btnDelete: TButton;
    ImageList1: TImageList;
    chkShowImage: TCheckBox;
    procedure JvCaptionButton1Click(Sender: TObject);
    procedure JvCaptionButton1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure JvCaptionButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure JvCaptionButton1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure chkVisibleClick(Sender: TObject);
    procedure chkToggleClick(Sender: TObject);
    procedure cbStandardChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure udPositionClick(Sender: TObject; Button: TUDBtnType);
    procedure chkDownClick(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure chkSysMenuClick(Sender: TObject);
    procedure chkMaxClick(Sender: TObject);
    procedure chkMinClick(Sender: TObject);
    procedure chkHelpClick(Sender: TObject);
    procedure cbBorderStyleChange(Sender: TObject);
    procedure chkShowHintsClick(Sender: TObject);
    procedure edHintChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure tbBtnWidthChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure chkShowImageClick(Sender: TObject);
  private
    procedure ResetButtons;
  end;

var
  CaptionBtnMainForm: TCaptionBtnMainForm;

implementation

{$R *.dfm}

procedure TCaptionBtnMainForm.JvCaptionButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if chkLogEvents.Checked then
    meEvents.Lines.Add(Format('MouseDown at %d, %d', [X, Y]));
end;

procedure TCaptionBtnMainForm.JvCaptionButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if chkLogEvents.Checked then
    meEvents.Lines.Add(Format('MouseUp at %d, %d', [X, Y]));
end;

procedure TCaptionBtnMainForm.JvCaptionButton1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if chkLogEvents.Checked then
    meEvents.Lines.Add(Format('CaptionButton OnMouseMove at %d, %d', [X, Y]));
end;

procedure TCaptionBtnMainForm.JvCaptionButton1Click(Sender: TObject);
var
  P: TPoint;
begin
  if chkLogEvents.Checked then
  begin
    GetCursorPos(P);
    meEvents.Lines.Add(Format('Click at %d, %d', [P.X, P.Y]));
  end;
  chkDown.Checked := JvCaptionButton1.Down;
  // show "copyright" when showing special image
  if (JvCaptionButton1.Standard = tsbNone) and (chkShowImage.Checked) then
    MessageBox(Handle,PChar('JvCaptionButton Demo. Copyright (c) 2003 by JEDI VCL; all rights reserved.'),
      PChar('About this demo...'),MB_OK or MB_ICONINFORMATION);
end;

procedure TCaptionBtnMainForm.chkVisibleClick(Sender: TObject);
begin
  JvCaptionButton1.Visible := chkVisible.Checked;
end;

procedure TCaptionBtnMainForm.chkToggleClick(Sender: TObject);
begin
  JvCaptionButton1.Toggle := chkToggle.Checked;
  chkDown.Checked := JvCaptionButton1.Down;
  chkDown.Enabled := chkToggle.Checked;
end;

procedure TCaptionBtnMainForm.cbStandardChange(Sender: TObject);
begin
  JvCaptionButton1.Standard := TJvStandardButton(cbStandard.ItemIndex);
  chkShowImage.Enabled := JvCaptionButton1.Standard = tsbNone;
end;

procedure TCaptionBtnMainForm.FormCreate(Sender: TObject);
begin
  udPosition.Position := JvCaptionButton1.Position;
  udPositionClick(Sender, btNext);
  cbStandard.ItemIndex := Ord(JvCaptionButton1.Standard);
  cbStandardChange(Sender);
  chkToggle.Checked := JvCaptionButton1.Toggle;
  chkVisible.Checked := JvCaptionButton1.Visible;
  edCaption.Text := JvCaptionButton1.Caption;
  edHint.Text := JvCaptionButton1.Hint;
  chkSysMenu.Checked := biSystemMenu in BorderIcons;
  chkMax.Checked := biMaximize in BorderIcons;
  chkMin.Checked := biMinimize in BorderIcons;
  chkHelp.Checked := biHelp in BorderIcons;
  chkEnabled.Checked := JvCaptionButton1.Enabled;
  cbBorderStyle.ItemIndex := integer(BorderStyle);
  cbBorderStyleChange(Sender);
  chkShowHints.Checked := ShowHint;
  tbBtnWidth.Min := JvCaptionButton1.ButtonWidth;
end;

procedure TCaptionBtnMainForm.udPositionClick(Sender: TObject;
  Button: TUDBtnType);
begin
  JvCaptionButton1.Position := udPosition.Position;
  lblPos.Caption := Format('(%d)', [JvCaptionButton1.Position]);
end;

procedure TCaptionBtnMainForm.chkDownClick(Sender: TObject);
begin
  JvCaptionButton1.Down := chkDown.Checked;
  chkDown.Checked := JvCaptionButton1.Down;
end;

procedure TCaptionBtnMainForm.edCaptionChange(Sender: TObject);
begin
  JvCaptionButton1.Caption := edCaption.Text;
end;

procedure TCaptionBtnMainForm.chkSysMenuClick(Sender: TObject);
begin
  if chkSysMenu.Checked then
    BorderIcons := BorderIcons + [biSystemMenu]
  else
    BorderIcons := BorderIcons - [biSystemMenu];
  ResetButtons;
end;

procedure TCaptionBtnMainForm.chkMaxClick(Sender: TObject);
begin
  if chkMax.Checked then
    BorderIcons := BorderIcons + [biMaximize]
  else
    BorderIcons := BorderIcons - [biMaximize];
  ResetButtons;
end;

procedure TCaptionBtnMainForm.chkMinClick(Sender: TObject);
begin
  if chkMin.Checked then
    BorderIcons := BorderIcons + [biMinimize]
  else
    BorderIcons := BorderIcons - [biMinimize];
  ResetButtons;
end;

procedure TCaptionBtnMainForm.chkHelpClick(Sender: TObject);
begin
  if chkHelp.Checked then
    BorderIcons := BorderIcons + [biHelp]
  else
    BorderIcons := BorderIcons - [biHelp];
  ResetButtons;
end;

procedure TCaptionBtnMainForm.cbBorderStyleChange(Sender: TObject);
begin
  BorderStyle := TBorderStyle(cbBorderStyle.ItemIndex);
  ResetButtons;
end;

procedure TCaptionBtnMainForm.chkShowHintsClick(Sender: TObject);
begin
  ShowHint := chkShowHints.Checked;
  Application.ShowHint := ShowHint;
end;

procedure TCaptionBtnMainForm.edHintChange(Sender: TObject);
begin
  JvCaptionButton1.Hint := edHint.Text;
end;

procedure TCaptionBtnMainForm.chkEnabledClick(Sender: TObject);
begin
  JvCaptionButton1.Enabled := chkEnabled.Checked;
end;

procedure TCaptionBtnMainForm.tbBtnWidthChange(Sender: TObject);
var P:TPoint;
begin
  JvCaptionButton1.ButtonWidth := tbBtnWidth.Position;
  tbBtnWidth.Hint := Format('(%d)',[tbBtnWidth.Position]);
  GetCursorPos(P);
  Application.ActivateHint(P);
end;

procedure TCaptionBtnMainForm.btnAddClick(Sender: TObject);
var i,j:integer;
begin
  j := 0;
  // find next position
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TJvCaptionButton) and (TJvCaptionButton(Components[i]).Position >= j) then
      j := TJvCaptionButton(Components[i]).Position + 1;
  with TJvCaptionButton.Create(self) do
  begin
    Assign(JvCaptionButton1);
    Position := j;
    OnClick := JvCaptionButton1Click;
    OnMouseMove := JvCaptionButton1MouseMove;
    OnMouseUp := JvCaptionButton1MouseUp;
    OnMouseDown := JvCaptionButton1MouseDown;
  end;
end;

procedure TCaptionBtnMainForm.btnDeleteClick(Sender: TObject);
var i:integer;
begin
  for i := ComponentCount - 1 downto 0 do
    if (Components[i] is TJvCaptionButton) and (Components[i] <> JvCaptionButton1) then
    begin
      Components[i].Free;
      Exit;
    end;
end;

procedure TCaptionBtnMainForm.chkShowImageClick(Sender: TObject);
begin
  if chkShowImage.Checked then
  begin
    JvCaptionButton1.Caption := '';
    JvCaptionButton1.ImageIndex := 3;
  end
  else
  begin
    JvCaptionButton1.Caption := edCaption.Text;
    JvCaptionButton1.ImageIndex := -1;
  end;
end;

procedure TCaptionBtnMainForm.ResetButtons;
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvCaptionButton then
      TJvCaptionButton(Components[i]).ResetButton;
end;

end.
