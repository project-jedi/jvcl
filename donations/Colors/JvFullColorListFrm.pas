{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFullColorListFrm.pas, released on 2004-09-27.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorListFrm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, Buttons, ImgList,
  FullColorFrm, ColorSpaces, ColorDialogs, ColorCtrls;

type
  TJvFullColorListForm = class(TForm)
    JvFullColorDialog: TJvFullColorDialog;
    ListBoxColors: TListBox;
    ActionList: TActionList;
    ActionNew: TAction;
    ActionModify: TAction;
    ActionDelete: TAction;
    ButtonNew: TButton;
    ButtonModify: TButton;
    ButtonDelete: TButton;
    Button4: TButton;
    ButtonOK: TButton;
    BitBtnMoveUp: TBitBtn;
    ActionMoveUp: TAction;
    ActionMoveDown: TAction;
    BitBtnMoveDown: TBitBtn;
    ButtonApply: TButton;
    Button1: TButton;
    ActionClear: TAction;
    Button2: TButton;
    ActionInsert: TAction;
    procedure ActionNewUpdate(Sender: TObject);
    procedure ActionModifyUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionMoveUpUpdate(Sender: TObject);
    procedure ActionMoveDownUpdate(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionClearUpdate(Sender: TObject);
    procedure ActionModifyExecute(Sender: TObject);
    procedure ActionInsertUpdate(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionInsertExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure JvFullColorDialogApply(Sender: TObject;
      AFullColor: TJvFullColor);
    procedure ListBoxColorsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FColorList: TJvFullColorList;
    FOnApply: TNotifyEvent;
    procedure SetColorList(const Value: TJvFullColorList);
    function GetColorList: TJvFullColorList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property ColorList: TJvFullColorList read GetColorList write SetColorList;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

var
  JvFullColorListForm: TJvFullColorListForm;

implementation

{$R *.dfm}

constructor TJvFullColorListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorList := TJvFullColorList.Create;
end;

destructor TJvFullColorListForm.Destroy;
begin
  FColorList.Free;
  inherited Destroy;
end;

procedure TJvFullColorListForm.ActionClearExecute(Sender: TObject);
begin
  ListBoxColors.Clear;
end;

procedure TJvFullColorListForm.ActionClearUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxColors.Count > 0;
end;

procedure TJvFullColorListForm.ActionDeleteExecute(Sender: TObject);
begin
  ListBoxColors.DeleteSelected;
end;

procedure TJvFullColorListForm.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxColors.SelCount >= 1;
end;

procedure TJvFullColorListForm.ActionInsertExecute(Sender: TObject);
begin
  JvFullColorDialog.Options := JvFullColorDialog.Options - [foShowApply];
  if (JvFullColorDialog.Execute) then
    ListBoxColors.Items.InsertObject(ListBoxColors.ItemIndex, '',
      TObject(JvFullColorDialog.FullColor));
end;

procedure TJvFullColorListForm.ActionInsertUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxColors.SelCount = 1;
end;

procedure TJvFullColorListForm.ActionModifyExecute(Sender: TObject);
begin
  JvFullColorDialog.Options := JvFullColorDialog.Options + [foShowApply];
  JvFullColorDialog.FullColor := TJvFullColor(ListBoxColors.Items.Objects[ListBoxColors.ItemIndex]);
  if JvFullColorDialog.Execute then
    ListBoxColors.Items.Objects[ListBoxColors.ItemIndex] := TObject(JvFullColorDialog.FullColor);
end;

procedure TJvFullColorListForm.ActionModifyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxColors.SelCount = 1;
end;

procedure TJvFullColorListForm.ActionMoveDownExecute(Sender: TObject);
var
  OldIndex: Integer;
begin
  with ListBoxColors do
  begin
    OldIndex := ItemIndex;
    Items.Move(ItemIndex, ItemIndex + 1);
    Selected[OldIndex + 1] := True;
  end;
end;

procedure TJvFullColorListForm.ActionMoveDownUpdate(Sender: TObject);
begin
  with ListBoxColors do
    (Sender as TAction).Enabled := (SelCount = 1) and (ItemIndex < (Count - 1));
end;

procedure TJvFullColorListForm.ActionMoveUpExecute(Sender: TObject);
var
  OldIndex: Integer;
begin
  with ListBoxColors do
  begin
    OldIndex := ItemIndex;
    Items.Move(ItemIndex, ItemIndex - 1);
    Selected[OldIndex - 1] := True;
  end;
end;

procedure TJvFullColorListForm.ActionMoveUpUpdate(Sender: TObject);
begin
  with ListBoxColors do
    (Sender as TAction).Enabled := (SelCount = 1) and (ItemIndex > 0);
end;

procedure TJvFullColorListForm.ActionNewExecute(Sender: TObject);
begin
  JvFullColorDialog.Options := JvFullColorDialog.Options - [foShowApply];
  if JvFullColorDialog.Execute then
    ListBoxColors.Items.AddObject('', TObject(JvFullColorDialog.FullColor));
end;

procedure TJvFullColorListForm.ActionNewUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

function TJvFullColorListForm.Execute: Boolean;
begin
  Result := (ShowModal = mrOK);
end;

function TJvFullColorListForm.GetColorList: TJvFullColorList;
var
  Index: Integer;
begin
  FColorList.BeginUpdate;
  FColorList.Clear;
  for Index := 0 to ListBoxColors.Count - 1 do
    FColorList.Add(TJvFullColor(ListBoxColors.Items.Objects[Index]));
  FColorList.EndUpdate;
  Result := FColorList;
end;

procedure TJvFullColorListForm.SetColorList(const Value: TJvFullColorList);
var
  I: Integer;
begin
  with ListBoxColors.Items, ColorSpaceManager do
  begin
    BeginUpdate;
    for I := 0 to Value.Count - 1 do
      AddObject('', TObject(Value.Items[I]));
    EndUpdate;
  end;
end;

procedure TJvFullColorListForm.ButtonApplyClick(Sender: TObject);
begin
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TJvFullColorListForm.JvFullColorDialogApply(Sender: TObject;
  AFullColor: TJvFullColor);
begin
  ListBoxColors.Items.Objects[ListBoxColors.ItemIndex] := TObject(JvFullColorDialog.FullColor);
end;

procedure TJvFullColorListForm.ListBoxColorsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AFullColor: TJvFullColor;
begin
  with TListBox(Control), Canvas do
  begin
    if odSelected in State then
      Font.Color := clCaptionText;

    Pen.Style := psSolid;
    Pen.Color := Brush.Color;
    Brush.Style := bsSolid;

    Rectangle(Rect);

    AFullColor := TJvFullColor(Items.Objects[Index]);

    with ColorSpaceManager, ColorSpace[GetColorSpaceID(AFullColor)] do
      TextOut(Rect.Left + Rect.Bottom - Rect.Top + 2, Rect.Top + 2,
        Format('%s : %s = $%.2x; %s = $%.2x; %s = $%.2x',
         [Name,
          AxisName[axIndex0], GetAxisValue(AFullColor, axIndex0),
          AxisName[axIndex1], GetAxisValue(AFullColor, axIndex1),
          AxisName[axIndex2], GetAxisValue(AFullColor, axIndex2)]));

    Brush.Color := ColorSpaceManager.ConvertToColor(AFullColor);
    Pen.Color := clBlack;
    Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + Rect.Bottom - Rect.Top - 2, Rect.Bottom - 2);
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\design'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

