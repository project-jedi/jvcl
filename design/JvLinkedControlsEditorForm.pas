{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLinkedControlsEditorForm.PAS, released on 2004-01-25.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}
unit JvLinkedControlsEditorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ImgList;

type
  TfrmLinkedControlsEditor = class(TForm)
    lbAvailable: TListBox;
    Label1: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lbLinked: TListBox;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    acDefault: TActionList;
    acMoveSelLeft: TAction;
    acMoveAllLeft: TAction;
    acMoveSelRight: TAction;
    acMoveAllRight: TAction;
    ImageList1: TImageList;
    procedure acMoveSelLeftExecute(Sender: TObject);
    procedure acMoveAllLeftExecute(Sender: TObject);
    procedure acMoveSelRightExecute(Sender: TObject);
    procedure acMoveAllRightExecute(Sender: TObject);
    procedure acDefaultUpdate(Action: TBasicAction;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    class function EditLinkedControls(AControl:TControl; LinkedControls:TStrings):boolean;
  end;

function EditLinkedControls(AControl:TControl; LinkedControls:TStrings):boolean;

implementation

{$R *.dfm}

function EditLinkedControls(AControl:TControl; LinkedControls:TStrings):boolean;
begin
  Result := TfrmLinkedControlsEditor.EditLinkedControls(AControl, LinkedControls);
end;

{ TfrmLinkedControlsEditor }

class function TfrmLinkedControlsEditor.EditLinkedControls(
  AControl: TControl; LinkedControls: TStrings): boolean;
var
  frm:TfrmLinkedControlsEditor;
  F:TCustomForm;
  C:TComponent;
  i:integer;
begin
  Result := false;
  F := GetParentForm(AControl);
  if (F = nil) or (LinkedControls = nil) then Exit;
  frm := self.Create(Application);
  try
    frm.Caption := Format(frm.Caption,[AControl.Name]);
    // remove invalid links
    for i := 0 to LinkedControls.Count -1 do
    begin
      C := F.FindComponent(LinkedControls[i]);
      if (C is TControl) and (C <> AControl) then
        frm.lbLinked.Items.Add(LinkedControls[i]);
    end;
    // add available TControls not already in LinkedControls
    for i := 0 to F.ComponentCount -1 do
      if (F.Components[i] <> AControl) and (F.Components[i] is TControl) and (frm.lbLinked.Items.IndexOf(F.Components[i].Name) < 0) then
        frm.lbAvailable.Items.Add(F.Components[i].Name);
    Result := frm.ShowModal = mrOK;
    if Result then
      LinkedControls.Assign(frm.lbLinked.Items);
  finally
    frm.Free;
  end;
end;

procedure TfrmLinkedControlsEditor.acMoveSelLeftExecute(Sender: TObject);
var i:integer;
begin
  for i := lbLinked.Items.Count -1 downto 0 do
    if lbLinked.Selected[i] then
    begin
      lbAvailable.Items.Add(lbLinked.Items[i]);
      lbLinked.Items.Delete(i);
    end;
end;

procedure TfrmLinkedControlsEditor.acMoveAllLeftExecute(Sender: TObject);
begin
  lbAvailable.Items.AddStrings(lbLinked.Items);
  lbLinked.Items.Clear;
end;

procedure TfrmLinkedControlsEditor.acMoveSelRightExecute(Sender: TObject);
var i:integer;
begin
  for i := lbAvailable.Items.Count -1 downto 0 do
    if lbAvailable.Selected[i] then
    begin
      lbLinked.Items.Add(lbAvailable.Items[i]);
      lbAvailable.Items.Delete(i);
    end;
end;

procedure TfrmLinkedControlsEditor.acMoveAllRightExecute(Sender: TObject);
begin
  lbLinked.Items.AddStrings(lbAvailable.Items);
  lbAvailable.Items.Clear;
end;

procedure TfrmLinkedControlsEditor.acDefaultUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acMoveSelLeft.Enabled := lbLinked.SelCount > 0;
  acMoveAllLeft.Enabled := lbLinked.Items.Count > 0;
  acMoveSelRight.Enabled := lbAvailable.SelCount > 0;
  acMoveAllRight.Enabled := lbAvailable.Items.Count > 0;
end;

end.
