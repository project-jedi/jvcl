{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGradEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGradientCaptionForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvFormPlacement, JvGradientCaption, JvListBox, JvCtrls,
  JvComponent, JvToolEdit, JvAppStorage, JvAppRegistryStorage, JvExMask;

type
  TGradCaptionsEditor = class(TJvForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    OkButton: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    CaptionText: TEdit;
    CaptionInactiveColor: TComboBox;
    GroupBox1: TGroupBox;
    NewButton: TButton;
    DeleteButton: TButton;
    CaptionParentFont: TCheckBox;
    CaptionGlueNext: TCheckBox;
    CaptionVisible: TCheckBox;
    Label2: TLabel;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    GradientCaption: TJvGradientCaption;
    FormStorage: TJvFormStorage;
    CaptionList: TListBox;
    CaptionFont: TJvComboEdit;
    AppStorage: TJvAppRegistryStorage;
    procedure FormCreate(Sender: TObject);
    procedure CaptionListClick(Sender: TObject);
    procedure CaptionListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CaptionListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure NewButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure CaptionInactiveColorDblClick(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure CaptionTextChange(Sender: TObject);
    procedure CaptionFontButtonClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
  private
    FComponent: TJvGradientCaption;
    FDesigner: IDesigner;
    FUpdating: Boolean;
    procedure AddColorItem(const ColorName: string);
    procedure EnableControls(Enable: Boolean);
    procedure UpdateCaptionList(Index: Integer);
    procedure ReadControls;
    procedure UpdateControls;
    procedure ClearControls;
    function GetActiveCaption: TJvCaption;
    procedure ApplyChanges;
  public
    procedure SetGradientCaption(Component: TJvGradientCaption;
      Designer: IDesigner);
    property ActiveCaption: TJvCaption read GetActiveCaption;
  end;

  TGradientCaptionEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

function EditGradientCaption(Component: TJvGradientCaption;
  ADesigner: IDesigner): Boolean;

implementation

uses
  JvJVCLUtils, JvBoxProcs, JvConsts, JvDsgnConsts;

{$R *.dfm}

function EditGradientCaption(Component: TJvGradientCaption;
  ADesigner: IDesigner): Boolean;
begin
  with TGradCaptionsEditor.Create(Application) do
    try
      SetGradientCaption(Component, ADesigner);
      Result := ShowModal = mrOk;
    finally
      Free;
    end;
end;

//=== { TGradientCaptionEditor } =============================================

procedure TGradientCaptionEditor.Edit;
begin
  EditGradientCaption(TJvGradientCaption(Component), Designer);
end;

procedure TGradientCaptionEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit;
end;

function TGradientCaptionEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsCaptionDesigner
  else
    Result := '';
end;

function TGradientCaptionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== { TGradCaptionsEditor } ================================================

procedure TGradCaptionsEditor.UpdateCaptionList(Index: Integer);
var
  I, Save: Integer;
begin
  if Index >= 0 then
    Save := Index
  else
    Save := CaptionList.ItemIndex;
  CaptionList.Items.BeginUpdate;
  try
    CaptionList.Items.Clear;
    for I := 0 to GradientCaption.Captions.Count - 1 do
      CaptionList.Items.Add(Format('%s[%d]', [RsGradientCaptions, I]));
    if Save < 0 then
      Save := 0;
    if Save >= CaptionList.Items.Count then
      Save := CaptionList.Items.Count - 1;
  finally
    CaptionList.Items.EndUpdate;
    CaptionList.ItemIndex := Save;
  end;
end;

function TGradCaptionsEditor.GetActiveCaption: TJvCaption;
var
  I: Integer;
begin
  Result := nil;
  I := CaptionList.ItemIndex;
  if (I >= 0) and (I < GradientCaption.Captions.Count) then
    Result := GradientCaption.Captions[I];
end;

procedure TGradCaptionsEditor.SetGradientCaption(Component: TJvGradientCaption;
  Designer: IDesigner);
begin
  FComponent := Component;
  FDesigner := Designer;
  if Component <> nil then
    with GradientCaption do
    begin
      Active := False;
      Font := Component.Font;
      DefaultFont := Component.DefaultFont;
      FontInactiveColor := Component.FontInactiveColor;
      GradientActive := Component.GradientActive;
      GradientInactive := Component.GradientInactive;
      StartColor := Component.StartColor;
      HideDirection := Component.HideDirection;
      GradientSteps := Component.GradientSteps;
      Captions := Component.Captions;
      if Component.Name <> '' then
        FormCaption := Format('%s.%s', [Component.Name, RsGradientCaptions])
      else
        FormCaption := Format('%s.%s', [Component.ClassName, RsGradientCaptions]);
      Active := True;
    end;
  UpdateCaptionList(-1);
  UpdateControls;
end;

procedure TGradCaptionsEditor.ApplyChanges;
begin
  ReadControls;
  if Assigned(FComponent) then
  begin
    FComponent.Captions := GradientCaption.Captions;
    if Assigned(FDesigner) then
      FDesigner.Modified;
  end;
end;

procedure TGradCaptionsEditor.AddColorItem(const ColorName: string);
begin
  CaptionInactiveColor.Items.Add(ColorName);
end;

procedure TGradCaptionsEditor.UpdateControls;
begin
  if ActiveCaption = nil then
  begin
    ClearControls;
    EnableControls(False);
  end
  else
    with ActiveCaption do
    begin
      FUpdating := True;
      try
        FontDialog.Font := Font;
        CaptionText.Text := Caption;
        CaptionInactiveColor.ItemIndex := -1;
        CaptionInactiveColor.Text := ColorToString(InactiveColor);
        CaptionFont.Text := Font.Name;
        CaptionParentFont.Checked := ParentFont;
        CaptionGlueNext.Checked := GlueNext;
        CaptionVisible.Checked := Visible;
        EnableControls(True);
      finally
        FUpdating := False;
      end;
    end;
end;

procedure TGradCaptionsEditor.EnableControls(Enable: Boolean);
begin
  CaptionText.Enabled := Enable;
  CaptionInactiveColor.Enabled := Enable;
  CaptionFont.Enabled := Enable;
  CaptionParentFont.Enabled := Enable;
  CaptionGlueNext.Enabled := Enable;
  CaptionVisible.Enabled := Enable;
  DeleteButton.Enabled := Enable;
end;

procedure TGradCaptionsEditor.ClearControls;
begin
  FUpdating := True;
  try
    CaptionText.Text := '';
    CaptionInactiveColor.ItemIndex := -1;
    CaptionInactiveColor.Text := '';
    CaptionFont.Text := '';
    CaptionParentFont.Checked := False;
    CaptionGlueNext.Checked := False;
    CaptionVisible.Checked := False;
  finally
    FUpdating := False;
  end;
end;

procedure TGradCaptionsEditor.ReadControls;
begin
  if not FUpdating and (ActiveCaption <> nil) then
  begin
    GradientCaption.Captions.BeginUpdate;
    FUpdating := True;
    try
      with ActiveCaption do
      begin
        Caption := CaptionText.Text;
        InactiveColor := StringToColor(CaptionInactiveColor.Text);
        ParentFont := CaptionParentFont.Checked;
        GlueNext := CaptionGlueNext.Checked;
        Visible := CaptionVisible.Checked;
      end;
    finally
      GradientCaption.Captions.EndUpdate;
      FUpdating := False;
    end;
  end;
end;

procedure TGradCaptionsEditor.FormCreate(Sender: TObject);
begin
  AppStorage.Root := SDelphiKey;
  CaptionInactiveColor.Items.BeginUpdate;
  try
    GetColorValues(AddColorItem);
  finally
    CaptionInactiveColor.Items.EndUpdate;
  end;
end;

procedure TGradCaptionsEditor.CaptionListClick(Sender: TObject);
begin
  if not FUpdating then
    UpdateControls;
end;

procedure TGradCaptionsEditor.CaptionListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  I: Integer;
begin
  I := CaptionList.ItemAtPos(Point(X, Y), True);
  if (I >= 0) and (I < CaptionList.Items.Count) and
    (I <> CaptionList.ItemIndex) then
  begin
    GradientCaption.MoveCaption(CaptionList.ItemIndex, I);
    CaptionList.ItemIndex := I;
    if not FUpdating then
      UpdateControls;
  end;
end;

procedure TGradCaptionsEditor.CaptionListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(CaptionList, Source, X, Y, State, Accept, CaptionList.Sorted);
end;

procedure TGradCaptionsEditor.NewButtonClick(Sender: TObject);
begin
  if GradientCaption.Captions.Add <> nil then
  begin
    UpdateCaptionList(GradientCaption.Captions.Count - 1);
    UpdateControls;
    if CaptionText.CanFocus then
      ActiveControl := CaptionText;
  end;
end;

procedure TGradCaptionsEditor.DeleteButtonClick(Sender: TObject);
begin
  if ActiveCaption <> nil then
  begin
    ActiveCaption.Free;
    UpdateCaptionList(-1);
    UpdateControls;
  end;
end;

procedure TGradCaptionsEditor.OkButtonClick(Sender: TObject);
begin
  ApplyChanges;
  ModalResult := mrOk;
end;

procedure TGradCaptionsEditor.ApplyButtonClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TGradCaptionsEditor.CaptionInactiveColorDblClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := StringToColor(CaptionInactiveColor.Text);
    if Execute then
    begin
      CaptionInactiveColor.Text := ColorToString(Color);
      if not FUpdating and (ActiveCaption <> nil) then
        ActiveCaption.InactiveColor := Color;
    end;
  end;
end;

procedure TGradCaptionsEditor.ControlExit(Sender: TObject);
begin
  if not FUpdating then
    ReadControls;
end;

procedure TGradCaptionsEditor.CaptionTextChange(Sender: TObject);
begin
  if not FUpdating and (ActiveCaption <> nil) then
    ActiveCaption.Caption := CaptionText.Text;
end;

procedure TGradCaptionsEditor.CaptionFontButtonClick(Sender: TObject);
begin
  if ActiveCaption <> nil then
  begin
    with FontDialog do
    begin
      Font := ActiveCaption.Font;
      Font.Color := ColorToRGB(ActiveCaption.Font.Color);
      if Execute then
      begin
        FUpdating := True;
        try
          CaptionFont.Text := Font.Name;
          ActiveCaption.Font := Font;
          CaptionParentFont.Checked := ActiveCaption.ParentFont;
        finally
          FUpdating := False;
        end;
      end;
    end;
  end
  else
    Beep;
end;

procedure TGradCaptionsEditor.CheckBoxClick(Sender: TObject);
begin
  if not FUpdating then
    ReadControls;
end;

end.

