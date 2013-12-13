{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Color form for the @link(TJvColorButton) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvColorForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  Classes, Graphics, Controls, Forms, Buttons, Dialogs,
  JvConsts,
  JvColorBox, JvComponent;

const
  cButtonWidth = 22;

type
  // (ahuser) TJvColorDialog is not registered as component
  TJvColorDialog = class(TColorDialog)
  published
    property OnShow;
    property OnClose;
  end;

  TJvColorForm = class(TJvForm)
    OtherBtn: TSpeedButton;
    procedure OtherBtnClick(Sender: TObject);
    procedure DoColorClick(Sender: TObject);
    procedure DoColorChange(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FOwner: TControl;
    FCDVisible: Boolean;
    FCS: TJvColorSquare;
    FButtonSize: Integer;
    FColorDialog: TJvColorDialog;
    FSelectedColor: TColor;
  protected
    procedure ShowCD(Sender: TObject);
    procedure HideCD(Sender: TObject);
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure SetButtonSize(const Value: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateSize; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure MakeColorButtons;
    procedure SetButton(Button: TControl);
    property ButtonSize: Integer read FButtonSize write SetButtonSize default cButtonWidth;
    property ColorDialog: TJvColorDialog read FColorDialog write FColorDialog;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor default clBlack;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  ExtCtrls,
  JvColorButton;

constructor TJvColorForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
//  IsPopupWindow := True;
  FButtonSize := cButtonWidth;
  FSelectedColor := clBlack;
  BorderIcons := [];
  BorderStyle := bsDialog;
  // (rom) this is not a standard Windows font
//  Font.Name := 'MS Shell Dlg 2';
//  FormStyle := fsStayOnTop;
  KeyPreview := True;
  OnActivate := FormActivate;
  OnClose := FormClose;
  OnKeyUp := FormKeyUp;

  FColorDialog := TJvColorDialog.Create(Self);
  FCDVisible := False;
  FColorDialog.OnShow := ShowCD;
  FColorDialog.OnClose := HideCD;
  MakeColorButtons;
  IsFocusable := False;
end;

procedure TJvColorForm.SetButton(Button: TControl);
begin
  FOwner := Button;
end;

procedure TJvColorForm.ShowCD(Sender: TObject);
begin
  FCDVisible := True;
end;

procedure TJvColorForm.HideCD(Sender: TObject);
begin
  FCDVisible := False;
end;

procedure TJvColorForm.OtherBtnClick(Sender: TObject);
begin
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
  FColorDialog.Color := SelectedColor;
  if FColorDialog.Execute then
  begin
    FCS.Color := FColorDialog.Color;
    if FOwner is TJvColorButton then
    begin
      TJvColorButton(FOwner).CustomColors.Assign(FColorDialog.CustomColors);
      TJvColorButton(FOwner).Color := SelectedColor;
    end;
    ModalResult := mrOK;
  end
  else
    ModalResult := mrCancel;
  Hide;
end;

procedure TJvColorForm.FormDeactivate(Sender: TObject);
begin
  if not FCDVisible then
  begin
    if Visible then
      Hide;
    ModalResult := mrCancel;
  end;
end;



procedure TJvColorForm.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
    FormDeactivate(Self);
end;

procedure TJvColorForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_CAPTION;
end;





procedure TJvColorForm.DoColorClick(Sender: TObject);
begin
  if Sender is TJvColorSquare then
    SelectedColor := (Sender as TJvColorSquare).Color;
  Hide;
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
  ModalResult := mrOK;
end;

procedure TJvColorForm.DoColorChange(Sender: TObject);
begin
  SelectedColor := FCS.Color;
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
end;

procedure TJvColorForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift * KeyboardShiftStates = []) then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvColorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TJvColorForm.FormActivate(Sender: TObject);
var
  R: TRect;
  Boundary: TRect;
  Pt: TPoint;
begin
  { set placement }
  if Assigned(FOwner) then
  begin
    R := FOwner.ClientRect;
    Pt.X := R.Left;
    Pt.Y := R.Bottom;
    Pt := FOwner.ClientToScreen(Pt);
    Boundary := Screen.MonitorFromPoint(Pt).WorkareaRect;

    Left := Pt.X;
    if (Left + Width) > Boundary.Right then
      Left := Boundary.Right - Width;

    Top := Pt.Y;
    if (Top + Height) > Boundary.Bottom then
      Top := Pt.Y - Height - (R.Bottom - R.Top);

    if FOwner is TJvColorButton then
      SelectedColor := TJvColorButton(FOwner).Color;
  end;
  UpdateSize;
end;

procedure TJvColorForm.MakeColorButtons;
const
  cColorArray: array [0..19] of TColor =
   (clWhite, clBlack, clSilver, clGray,
    clRed, clMaroon, clYellow, clOlive,
    clLime, clGreen, clAqua, clTeal,
    clBlue, clNavy, clFuchsia, clPurple,
    clMoneyGreen, clSkyBlue, clCream, clMedGray);
var
  I, X, Y: Integer;
  ParentControl: TWinControl;
  Offset: Integer;
begin
  for I := ControlCount - 1 downto 0 do
    if (Controls[I] is TJvColorSquare) or (Controls[I] is TBevel) then
      Controls[I].Free;

  ParentControl := Self;
  Offset := 0;

  X := Offset;
  Y := Offset;
  for I := 0 to 19 do
  begin
    FCS := TJvColorSquare.Create(Self);
    FCS.SetBounds(X, Y, FButtonSize, FButtonSize);
    FCS.Color := cColorArray[I];
    FCS.OnClick := DoColorClick;
    FCS.Parent := ParentControl;
    FCS.BorderStyle := bsSingle;
    Inc(X, FButtonSize);
    if (I + 1) mod 4 = 0 then
    begin
      Inc(Y, FButtonSize);
      X := Offset;
    end;
  end;
  if OtherBtn = nil then
    OtherBtn := TSpeedButton.Create(Self);
  with OtherBtn do
  begin
    SetBounds(Offset, Y + 6, FButtonSize * 3, FButtonSize);
    Parent := ParentControl;
//    Caption := SOtherCaption;
    OnClick := OtherBtnClick;
  end;
  FCS := TJvColorSquare.Create(Self);
  FCS.Color := cColorArray[0];
  FCS.OnClick := DoColorClick;
  FCS.OnChange := DoColorChange;
  FCS.Parent := ParentControl;
  FCS.BorderStyle := bsSingle;
  FCS.SetBounds(Offset + FButtonSize * 3, Y + 6, FButtonSize, FButtonSize);
  UpdateSize;
  with TBevel.Create(Self) do
  begin
    Parent := ParentControl;
    Shape := bsTopLine;
    SetBounds(2, Y, Self.Width - 4, 4);
    Anchors := [akLeft, akBottom, akRight];
  end;
end;

procedure TJvColorForm.UpdateSize;
begin
  Height := OtherBtn.Top + OtherBtn.Height + 8;
  ClientWidth := FCS.Left + FCS.Width;
end;

procedure TJvColorForm.SetButtonSize(const Value: Integer);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    MakeColorButtons;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
