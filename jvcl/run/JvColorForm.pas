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
located at http://jvcl.sourceforge.net

Description:
  Color form for the @link(TJvColorButton) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvColorForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, Qt, QWindows,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Forms, Buttons, Dialogs,
  JvConsts, // missing color constants for D5
  JvColorBox;

const
  cButtonWidth = 22;

type
  // (ahuser) TJvColorDialog is not registered as component
  TJvColorDialog = class(TColorDialog)
  published
    property OnShow;
    property OnClose;
  end;

  TJvColorForm = class(TForm)
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
    procedure ShowCD(Sender: TObject);
    procedure HideCD(Sender: TObject);
    {$IFDEF VCL}
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    {$ENDIF VCL}
    procedure SetButtonSize(const Value: Integer);
  protected
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$ENDIF VisualCLX}
    procedure UpdateSize; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure MakeColorButtons;
    procedure SetButton(Button: TControl);
    property ButtonSize: Integer read FButtonSize write SetButtonSize default cButtonWidth;
    property ColorDialog: TJvColorDialog read FColorDialog write FColorDialog;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor default clBlack;
  end;

implementation

uses
  ExtCtrls,
  JvColorButton;

constructor TJvColorForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  FButtonSize := cButtonWidth;
  FSelectedColor := clBlack;
  BorderIcons := [];
  {$IFDEF VCL}
  BorderStyle := bsDialog;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  BorderStyle := fbsDialog;
  {$ENDIF VisualCLX}
  // (rom) this is not a standard Windows font
//  Font.Name := 'MS Shell Dlg 2';
  FormStyle := fsStayOnTop;
  KeyPreview := True;
  OnActivate := FormActivate;
  OnClose := FormClose;
  OnKeyUp := FormKeyUp;

  FColorDialog := TJvColorDialog.Create(Self);
  FCDVisible := False;
  FColorDialog.OnShow := ShowCD;
  FColorDialog.OnClose := HideCD;
  MakeColorButtons;
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
  if (not FCDVisible) then
  begin
    if Visible then
      Hide;
    ModalResult := mrCancel;
  end;
end;

{$IFDEF VCL}

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

{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvColorForm.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags and
    not Integer(WidgetFlags_WStyle_Title) or Integer(WidgetFlags_WType_Popup);
end;

type
  TWidgetControlAccessProtected = class(TWidgetControl);

function TJvColorForm.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  case QEvent_type(Event) of
    QEventType_Show:
      FormActivate(Self);   // prevent visual moving
    QEventType_FocusOut:
      FormDeactivate(Self);
    QEventType_Hide:
      if FOwner is TJvColorButton then
        TWidgetControlAccessProtected(FOwner).MouseUp(mbLeft, [ssLeft], 0, 0);
    QEventType_Close:
      begin
        QCloseEvent_ignore(QCloseEventH(Event)); // do not close
        Result := True;
        Hide;
        Exit;
      end;
  end;
  Result := inherited EventFilter(Sender, Event);
end;

{$ENDIF VisualCLX}

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

  {$IFDEF VCL}
  ParentControl := Self;
  Offset := 0;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  ParentControl := TPanel.Create(Self);
  ParentControl.Align := alClient;
  ParentControl.Parent := Self;
  TPanel(ParentControl).BevelInner := bvRaised;
  TPanel(ParentControl).BevelOuter := bvRaised;
  Offset := 2;
  {$ENDIF VisualCLX}

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
  {$IFDEF VCL}
  ClientWidth := FCS.Left + FCS.Width;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  // workaround a VisualCLX bug: ClientWidth does not allow values smaller than 100
  Width := FCS.Left + FCS.Width + 2;

  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Constraints.MaxWidth;
  Constraints.MinHeight := Constraints.MaxHeight;
  {$ENDIF VisualCLX}
end;

procedure TJvColorForm.SetButtonSize(const Value: Integer);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    MakeColorButtons;
  end;
end;

end.

