{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Color form for the @link(TJvColorButton) component

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvColorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Buttons, ExtCtrls, Dialogs,
  JvColorBox;

const
  cButtonWidth = 22;

type
  TJvClrFrm = class(TForm)
    Bevel1: TBevel;
    OtherBtn: TSpeedButton;
    procedure OtherBtnClick(Sender: TObject);
    procedure DoColorClick(Sender: TObject);
    procedure DoColorChange(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    FOwner: TControl;
    FCDVisible: Boolean;
    FCS: TJvColorSquare;
    FButtonSize: Integer;
    FColorDialog: TColorDialog;
    FSelectedColor: TColor;
    procedure ShowCD(Sender: TObject);
    procedure HideCD(Sender: TObject);
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure SetButtonSize(const Value: Integer);
  protected
    procedure CreateWnd; override;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure MakeColorButtons;
    procedure SetButton(Button: TControl);
    property ButtonSize: Integer read FButtonSize write SetButtonSize default cButtonWidth;
    property ColorDialog: TColorDialog read FColorDialog write FColorDialog;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor default clBlack;
  end;

implementation

uses
  JvColorButton, JvConsts;

{.$R *.dfm}

constructor TJvClrFrm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  FButtonSize := cButtonWidth;
  FSelectedColor := clBlack;
  BorderIcons := [];
  BorderStyle := bsDialog;
  // (rom) this is not a standard Windows font
  Font.Name := 'MS Shell Dlg 2';
  FormStyle := fsStayOnTop;
  KeyPreview := True;
  OnActivate := FormActivate;
  OnClose := FormClose;
  OnKeyUp := FormKeyUp;

  FColorDialog := TColorDialog.Create(Self);
  FCDVisible := False;
  FColorDialog.OnShow := ShowCD;
  FColorDialog.OnClose := HideCD;
  MakeColorButtons;
end;

procedure TJvClrFrm.SetButton(Button: TControl);
begin
  FOwner := Button;
end;

procedure TJvClrFrm.ShowCD(Sender: TObject);
begin
  FCDVisible := True;
end;

procedure TJvClrFrm.HideCD(Sender: TObject);
begin
  FCDVisible := False;
end;

procedure TJvClrFrm.OtherBtnClick(Sender: TObject);
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

procedure TJvClrFrm.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  if (Msg.Active = WA_INACTIVE) and not FCDVisible then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvClrFrm.DoColorClick(Sender: TObject);
begin
  if Sender is TJvColorSquare then
    SelectedColor := (Sender as TJvColorSquare).Color;
  Hide;
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
  ModalResult := mrOK;
end;

procedure TJvClrFrm.DoColorChange(Sender: TObject);
begin
  SelectedColor := FCS.Color;
  if Assigned(FOwner) and (FOwner is TJvColorButton) then
    TJvColorButton(FOwner).Color := SelectedColor;
end;

procedure TJvClrFrm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvClrFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TJvClrFrm.CreateWnd;
begin
  inherited CreateWnd;
//  Hide;
  SetWindowLong(Handle, GWL_STYLE,
    GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);
//  Show;
end;

procedure TJvClrFrm.FormActivate(Sender: TObject);
var
  R: TRect;
  Pt: TPoint;
begin
  { set placement }
  if Assigned(FOwner) then
  begin
    R := FOwner.ClientRect;
    Pt.X := R.Left;
    Pt.Y := R.Top + R.Bottom;
    Pt := FOwner.ClientToScreen(Pt);
    Left := Pt.X;
    Top := Pt.Y;
    if FOwner is TJvColorButton then
      SelectedColor := TJvColorButton(FOwner).Color;
  end;
  ClientWidth := FCS.Left + FCS.Width;
  Height := OtherBtn.Top + OtherBtn.Height + 8;
end;

procedure TJvClrFrm.MakeColorButtons;
const
  cColorArray: array [0..19] of TColor =
   (clWhite, clBlack, clSilver, clGray,
    clRed, clMaroon, clYellow, clOlive,
    clLime, clGreen, clAqua, clTeal,
    clBlue, clNavy, clFuchsia, clPurple,
    clMoneyGreen, clSkyBlue, clCream, clMedGray);
var
  I, X, Y: Integer;
begin
  for I := ControlCount - 1 downto 0 do
    if (Controls[I] is TJvColorSquare) or (Controls[I] is TBevel) then
      Controls[I].Free;
  X := 0;
  Y := 0;
  for I := 0 to 19 do
  begin
    FCS := TJvColorSquare.Create(Self);
    FCS.SetBounds(X, Y, FButtonSize, FButtonSize);
    FCS.Color := cColorArray[I];
    FCS.OnClick := DoColorClick;
    FCS.Parent := Self;
    FCS.BorderStyle := bsSingle;
    Inc(X, FButtonSize);
    if (I + 1) mod 4 = 0 then
    begin
      Inc(Y, FButtonSize);
      X := 0;
    end;
  end;
  if OtherBtn = nil then
    OtherBtn := TSpeedButton.Create(Self);
  with OtherBtn do
  begin
    SetBounds(0, Y + 6, FButtonSize * 3, FButtonSize);
    Parent := Self;
//    Caption := SOtherCaption;
    OnClick := OtherBtnClick;
  end;
  FCS := TJvColorSquare.Create(Self);
  FCS.Color := cColorArray[0];
  FCS.OnClick := DoColorClick;
  FCS.OnChange := DoColorChange;
  FCS.Parent := Self;
  FCS.BorderStyle := bsSingle;
  FCS.SetBounds(FButtonSize * 3, Y + 6, FButtonSize, FButtonSize);
  Self.ClientWidth := FCS.Left + FCS.Width;
  Self.ClientHeight := OtherBtn.Top + OtherBtn.Height;
  with TBevel.Create(Self) do
  begin
    Parent := Self;
    Shape := bsTopLine;
    SetBounds(2, Y, Self.Width - 4, 4);
    Anchors := [akLeft, akBottom, akRight];
  end;
end;

procedure TJvClrFrm.SetButtonSize(const Value: Integer);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    MakeColorButtons;
  end;
end;

end.

