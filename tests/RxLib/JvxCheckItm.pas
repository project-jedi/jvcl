{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxCheckItm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}



unit JvxCheckItm;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Messages,
  SysUtils, Classes, Controls, Forms, Menus, Graphics, StdCtrls, JvxPlacemnt,
  Dialogs, JvxCtrls, ExtCtrls, RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  CheckLst;

type

{ TJvxCheckItemEditor }

  TJvxCheckItemEditor = class(TForm)
  private
    FEdit: TEdit;
    FOkBtn: TButton;
    FCancelBtn: TButton;
    FComboBox: TComboBox;
    FEnableBox: TCheckBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TJvxCheckItemsEditor }

  TJvxCheckItemsEditor = class(TForm)
    Panel2: TPanel;
    DeleteBtn: TButton;
    NewBtn: TButton;
    EditBtn: TButton;
    Panel3: TPanel;
    CancelBtn: TButton;
    Panel1: TPanel;
    FormPlacement: TJvxFormPlacement;
    OkBtn: TButton;
    Popup: TPopupMenu;
    AddListBtn: TButton;
    cbGrayedItem: TMenuItem;
    cbCheckedItem: TMenuItem;
    cbUncheckedItem: TMenuItem;
    N2: TMenuItem;
    EnabledItem: TMenuItem;
    ClearBtn: TButton;
    CheckList: TJvxCheckListBox;
    UpBtn: TButton;
    DownBtn: TButton;
    procedure EditBtnClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EnabledItemClick(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure AddListBtnClick(Sender: TObject);
    procedure cbGrayedItemClick(Sender: TObject);
    procedure cbCheckedItemClick(Sender: TObject);
    procedure cbUncheckedItemClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure CheckListClick(Sender: TObject);
    procedure UpDownBtnClick(Sender: TObject);
    procedure CheckListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CheckListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    procedure CheckButtons;
  end;

{ CheckItems property editor }

  TJvxCheckItemsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

uses {$IFDEF Delphi3_Up} JvxStrLEdit, {$ELSE} StrEdit, {$ENDIF} Consts, JvxConst,
  JvxVCLUtils, JvxBoxProcs;

{ TJvxCheckItemsProperty }

function TJvxCheckItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TJvxCheckItemsProperty.Edit;
var
  Comp: TPersistent;
begin
  with TJvxCheckItemsEditor.Create(Application) do
  try
    Comp := Self.GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + Self.GetName
    else Caption := Self.GetName;
    if Comp is TJvxCheckListBox then begin
      CheckList.AllowGrayed := TJvxCheckListBox(Comp).AllowGrayed;
      CheckList.Sorted := TJvxCheckListBox(Comp).Sorted;
      CheckList.CheckKind := TJvxCheckListBox(Comp).CheckKind;
    end;
    CheckList.Items := TStrings(GetOrdValue);
    if ShowModal = mrOk then SetOrdValue(LongInt(CheckList.Items));
  finally
    Free;
  end;
end;

{ TJvxCheckItemEditor }

constructor TJvxCheckItemEditor.Create(AOwner: TComponent);
begin
{$IFDEF CBUILDER}
  inherited CreateNew(AOwner, 0);
{$ELSE}
  inherited CreateNew(AOwner);
{$ENDIF}
  { Form definitions }
  {Left := 354;
  Top := 338;}
  BorderStyle := bsDialog;
  Caption := 'Item editor';
  ClientHeight := 92;
  ClientWidth := 330;
  Font.Color := clWindowText;
  Font.Size := 8;
  Font.Name := 'MS Sans Serif';
  Font.Style := [];
  Scaled := True;
  Position := poScreenCenter;
  { FEdit }
  FEdit := TEdit.Create(Self);
  with FEdit do begin
    Parent := Self;
    Left := 8;
    Top := 12;
    Width := 313;
    Height := 21;
    TabOrder := 0;
  end;
  { FOkBtn }
  FOkBtn := TButton.Create(Self);
  with FOkBtn do begin
    Parent := Self;
    Left := 168;
    Top := 60;
    Width := 75;
    Height := 25;
    Caption := ResStr(SOKButton);
    Default := True;
    ModalResult := mrOk;
    TabOrder := 1;
  end;
  { FCancelBtn }
  FCancelBtn := TButton.Create(Self);
  with FCancelBtn do begin
    Parent := Self;
    Left := 246;
    Top := 60;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := ResStr(SCancelButton);
    ModalResult := mrCancel;
    TabOrder := 2;
  end;
  { FCheckBox }
  FComboBox := TComboBox.Create(Self);
  with FComboBox do begin
    Parent := Self;
    Style := csDropDownList;
    Items.Add('Unchecked');
    Items.Add('Checked');
    Items.Add('Grayed');
    Left := 8;
    Top := 38;
    Width := 88;
    TabOrder := 3;
  end;
  { FEnableBox }
  FEnableBox := TCheckBox.Create(Self);
  with FEnableBox do begin
    Parent := Self;
    Left := 104;
    Top := 40;
    Width := 70;
    Height := 17;
    Caption := 'Enabled';
    State := cbChecked;
    TabOrder := 4;
  end;
end;

{ TJvxCheckItemsEditor }

procedure TJvxCheckItemsEditor.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  with FormPlacement do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ENDIF}
end;

procedure TJvxCheckItemsEditor.CheckButtons;
begin
  DeleteBtn.Enabled := CheckList.ItemIndex >= 0;
  EditBtn.Enabled := DeleteBtn.Enabled;
  UpBtn.Enabled := CheckList.ItemIndex > 0;
  DownBtn.Enabled := (CheckList.ItemIndex < CheckList.Items.Count - 1)
    and (CheckList.ItemIndex >= 0);
end;

procedure TJvxCheckItemsEditor.EditBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := CheckList.ItemIndex;
  if I >= 0 then
    with TJvxCheckItemEditor.Create(Application) do
    try
      if Screen.PixelsPerInch <> 96 then begin { scale to screen res }
        ScaleBy(Screen.PixelsPerInch, 96);
        { The ScaleBy method does not scale the font well, so set the
          font back to the original info. }
        Font.Name := 'MS Sans Serif';
        Font.Size := 8;
        Left := (Screen.Width div 2) - (Width div 2);
        Top := (Screen.Height div 2) - (Height div 2);
      end;
      FEdit.Text := CheckList.Items[I];
      FComboBox.ItemIndex := Integer(CheckList.State[I]);
      FEnableBox.Checked := CheckList.EnabledItem[I];
      if ShowModal = mrOk then begin
        CheckList.Items[I] := FEdit.Text;
        CheckList.State[I] := TCheckBoxState(FComboBox.ItemIndex);
        CheckList.EnabledItem[I] := FEnableBox.Checked;
      end;
      Self.CheckList.ItemIndex := I;
    finally
      Free;
    end;
end;

procedure TJvxCheckItemsEditor.NewBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  with TJvxCheckItemEditor.Create(Application) do
  try
    FEdit.Text := '';
    FComboBox.ItemIndex := Integer(clbDefaultState);
    FEnableBox.Checked := clbDefaultEnabled;
    if ShowModal = mrOk then begin
      Index := CheckList.Items.Add(FEdit.Text);
      CheckList.State[Index] := TCheckBoxState(FComboBox.ItemIndex);
      CheckList.EnabledItem[Index] := FEnableBox.Checked;
      CheckButtons;
    end;
  finally
    Free;
  end;
end;

procedure TJvxCheckItemsEditor.DeleteBtnClick(Sender: TObject);
begin
  if CheckList.ItemIndex >= 0 then begin
    CheckList.Items.Delete(CheckList.ItemIndex);
    CheckButtons;
  end;
end;

procedure TJvxCheckItemsEditor.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvxCheckItemsEditor.EnabledItemClick(Sender: TObject);
begin
  CheckList.EnabledItem[CheckList.ItemIndex] :=
    not CheckList.EnabledItem[CheckList.ItemIndex];
end;

procedure TJvxCheckItemsEditor.PopupPopup(Sender: TObject);
var
  Enable: Boolean;
begin
  Enable := CheckList.ItemIndex >= 0;
  EnabledItem.Enabled := Enable;
  cbGrayedItem.Enabled := Enable;
  cbCheckedItem.Enabled := Enable;
  cbUncheckedItem.Enabled := Enable;
  cbGrayedItem.Checked := False;
  cbCheckedItem.Checked := False;
  cbUncheckedItem.Checked := False;
  if Enable then begin
    EnabledItem.Checked := CheckList.EnabledItem[CheckList.ItemIndex];
    case CheckList.State[CheckList.ItemIndex] of
      cbChecked: cbCheckedItem.Checked := True;
      cbUnchecked: cbUncheckedItem.Checked := True;
      cbGrayed: cbGrayedItem.Checked := True;
    end;
  end;
end;

procedure TJvxCheckItemsEditor.AddListBtnClick(Sender: TObject);
var
  I: LongInt;
begin
  with TJvxStrEditDlg.Create(Application) do
    try
{$IFDEF WIN32}
{$IFNDEF Delphi3_Up}
      CodeWndBtn.Visible := False;
{$ENDIF}
{$ENDIF}
      if ShowModal = mrOk then begin
        for I := 0 to Memo.Lines.Count - 1 do
          if Memo.Lines[I] <> '' then
            CheckList.Items.Add(Memo.Lines[I]);
        CheckButtons;
      end;
    finally
      Free;
    end;
end;

procedure TJvxCheckItemsEditor.cbGrayedItemClick(Sender: TObject);
begin
  CheckList.State[CheckList.ItemIndex] := cbGrayed;
end;

procedure TJvxCheckItemsEditor.cbCheckedItemClick(Sender: TObject);
begin
  CheckList.State[CheckList.ItemIndex] := cbChecked;
end;

procedure TJvxCheckItemsEditor.cbUncheckedItemClick(Sender: TObject);
begin
  CheckList.State[CheckList.ItemIndex] := cbUnchecked;
end;

procedure TJvxCheckItemsEditor.ClearBtnClick(Sender: TObject);
begin
  CheckList.Clear;
end;

procedure TJvxCheckItemsEditor.CheckListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvxCheckItemsEditor.UpDownBtnClick(Sender: TObject);
var
  OldIndex, NewIndex: Integer;
begin
  OldIndex := CheckList.ItemIndex;
  if Sender = UpBtn then NewIndex := OldIndex - 1
  else {if Sender = DownBtn then} NewIndex := OldIndex + 1;
  CheckList.Items.Move(OldIndex, NewIndex);
  CheckList.ItemIndex := NewIndex;
  CheckButtons;
end;

procedure TJvxCheckItemsEditor.CheckListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  case Key of
    VK_DELETE:
      if ssCtrl in Shift then begin
        DeleteBtnClick(nil);
        Key := 0;
      end;
    VK_INSERT:
      if Shift = [] then begin
        AddListBtnClick(nil);
        Key := 0;
      end;
    VK_DOWN, VK_UP:
      if (ssCtrl in Shift) then begin
        if Key = VK_DOWN then Incr := 1
        else Incr := -1;
        BoxMoveFocusedItem(CheckList, CheckList.ItemIndex + Incr);
        CheckButtons;
        Key := 0;
      end;
  end;
end;

procedure TJvxCheckItemsEditor.CheckListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = CheckList then begin
    BoxMoveFocusedItem(CheckList, CheckList.ItemAtPos(Point(X, Y), True));
    CheckButtons;
  end;
end;

procedure TJvxCheckItemsEditor.CheckListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(CheckList, Source, X, Y, State, Accept, CheckList.Sorted);
  if State = dsDragLeave then CheckList.DragCursor := crDrag
  else if (State = dsDragEnter) and (CheckList.SelCount > 1) then
    CheckList.DragCursor := crMultiDrag;
end;

end.
