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
dejoy(dejoy att ynl dott gov dott cn)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Color form for the @link(TJvColorButton) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvOfficeColorPanel;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Buttons, ExtCtrls, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QWindows, Qt, QGraphics, QControls, QForms, QButtons, QExtCtrls,
  QDialogs,
  {$ENDIF VisualCLX}
  JvComponent, JvSpeedButton;

const
  MaxColorButtonNumber = 40;

  Tag_AutoCaption = 0;
  Tag_OtherCaption = 1;
  Tag_AutoHint = 2;
  Tag_OtherHint = 3;

  MinButtonHeight = 22;
  MinButtonWidth = 23;
  MinColorSize = 18;
  MinColorSpace = 0;
  MinColorSpaceTop = 4;
  MinColorSpaceBottom = 4;
  MinTopMargin = 2;
  MinBottomMargin = 4;
  MinHorizontalMargin = 7;

  Tag_ButtonHeight = 0;
  Tag_ButtonWidth = 1;
  Tag_ColorSize = 2;
  Tag_ColorSpace = 3;
  Tag_ColorSpaceTop = 4;
  Tag_ColorSpaceBottom = 5;
  Tag_TopMargin = 6;
  Tag_BottomMargin = 7;
  Tag_HorizontalMargin = 8;

  LineColorButtonCount = 8;

  SubColorButtonColors: array [0..MaxColorButtonNumber - 1] of TColor =
   ($000000, $003399, $003333, $003300, $663300, $800000, $993333, $333333,
    $000080, $0066FF, $008080, $008000, $808000, $FF0000, $996666, $808080,
    $0000FF, $0099FF, $00CC99, $669933, $CCCC33, $FF6633, $800080, $999999,
    $FF00FF, $00CCFF, $00FFFF, $00FF00, $FFFF00, $FFCC00, $663399, $C0C0C0,
    $CC99FF, $99CCFF, $99FFFF, $CCFFCC, $FFFFCC, $FFCC99, $FF99CC, $FFFFFF);

type
  TJvClickColorButtonType =
    (cbctColorsButton, cbctAutoButton, cbctOtherButton, cbctNone);
  TJvPropertiesChangedEvent = procedure(Sender: TObject; PropName: string) of object;

  TJvColorSpeedButton = class(TJvSpeedButton)
  private
    FButtonColor: TColor;
    procedure SetButtonColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
  end;

  TJvSubColorButton = class(TJvColorSpeedButton)
  private
    procedure SetEdgeWidth(const Value: Integer);
  protected
    FEdgeWidth: Integer;
    procedure Paint; override;
    function GetEdgeWidth: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    property EdgeWidth: Integer read GetEdgeWidth write SetEdgeWidth;
  end;

  // (ahuser) TJvColorDialog is not registered as component
  TJvOfficeColorDialog = class(TColorDialog)
  published
    property OnShow;
    property OnClose;
  end;

  TJvOfficeColorPanelProperties = class(TPersistent)
  private
    FShowAutoButton: Boolean;
    FShowOtherButton: Boolean;
    FTopMargin: Integer;
    FColorSpaceBottom: Integer;
    FHorizontalMargin: Integer;
    FColorSpace: Integer;
    FColorSpaceTop: Integer;
    FButtonHeight: Integer;
    FColorSize: Integer;
    FBottomMargin: Integer;

    FAutoCaption: string;
    FOtherCaption: string;
    FAutoHint: string;
    FOtherHint: string;

    FOnPropertiesChanged: TJvPropertiesChangedEvent;
    FAutoColor: TColor;
    FShowColorHint: Boolean;
    procedure SetShowAutoButton(const Value: Boolean);
    procedure SetShowOtherButton(const Value: Boolean);
    procedure SetMeasure(const Index, Value: Integer);
    function GetStringValue(const Index: Integer): string;
    procedure SetStringValue(const Index: Integer; const Value: string);
    procedure SetAutoColor(const Value: TColor);
    procedure SetShowColorHint(const Value: Boolean);
  protected
    procedure Changed(PropName: string); virtual;
    procedure CreateDefaultText; virtual;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property AutoColor: TColor read FAutoColor write SetAutoColor default clDefault;
    property OnPropertiesChanged: TJvPropertiesChangedEvent read FOnPropertiesChanged write FOnPropertiesChanged;
  published
    property ShowAutoButton: Boolean read FShowAutoButton write SetShowAutoButton default True;
    property ShowOtherButton: Boolean read FShowOtherButton write SetShowOtherButton default True;
    property ShowColorHint: Boolean read FShowColorHint write SetShowColorHint default True;

    property TopMargin: Integer index Tag_TopMargin read FTopMargin write SetMeasure
      default MinTopMargin;
    property BottomMargin: Integer index Tag_BottomMargin read FBottomMargin write SetMeasure
      default MinBottomMargin;
    property HorizontalMargin: Integer index Tag_HorizontalMargin read FHorizontalMargin write SetMeasure
      default MinHorizontalMargin;
    property ColorSpace: Integer index Tag_ColorSpace read FColorSpace write SetMeasure
       default MinColorSpace;
    property ColorSpaceTop: Integer index Tag_ColorSpaceTop read FColorSpaceTop write SetMeasure
      default MinColorSpaceTop;
    property ColorSpaceBottom: Integer index Tag_ColorSpaceBottom read FColorSpaceBottom write SetMeasure
      default MinColorSpaceBottom;
    property ColorSize: Integer index Tag_ColorSize read FColorSize write SetMeasure
      default MinColorSize;
    property ButtonHeight: Integer index Tag_ButtonHeight read FButtonHeight write SetMeasure
      default MinButtonHeight;
    property AutoCaption: string index Tag_AutoCaption read GetStringValue write SetStringValue;
    property OtherCaption: string index Tag_OtherCaption read GetStringValue write SetStringValue;
    property AutoHint: string index Tag_AutoHint read GetStringValue write SetStringValue;
    property OtherHint: string index Tag_OtherHint read GetStringValue write SetStringValue;
  end;

  TJvCustomOfficeColorPanel = class(TJvCustomPanel)
  private
    FColorButtons: array [0..MaxColorButtonNumber - 1] of TJvSubColorButton;
    FAutoButton: TJvSubColorButton;
    FOtherButton: TJvColorSpeedButton;
    FProperties: TJvOfficeColorPanelProperties;
    FOwner: TControl;
    FColorDialog: TJvOfficeColorDialog;
    FSelectedColor: TColor;
    FWordStyle: Boolean;
    FFlat: Boolean;
    FInited: Boolean;
    FOnColorChange: TNotifyEvent;
    FOnColorButtonClick: TNotifyEvent;
    FClickColorButton: TJvClickColorButtonType;
    {$IFDEF VCL}
    FColorDialogOptions: TColorDialogOptions;
    procedure SetColorDialogOptions(const Value: TColorDialogOptions);
    {$ENDIF VCL}
    procedure ColorButtonClick(Sender: TObject);
    procedure SetFlat(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor);
    function GetCustomColors: TStrings;
    procedure SetCustomColors(const Value: TStrings);
    function GetProperties: TJvOfficeColorPanelProperties;
    procedure SetProperties(const Value: TJvOfficeColorPanelProperties);
  protected
    {$IFDEF VCL}
    procedure CreateWnd; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure InitWidget; override;
    {$ENDIF VisualCLX}
    procedure Resize; override;
    procedure Paint; override;
    procedure ShowHintChanged; override;
    procedure PropertiesChanged(Sender: TObject; PropName: string); virtual;
    procedure SetWordStyle(const Value: Boolean);
    procedure MakeColorButtons;
    procedure AdjustColorButtons;
    procedure SetEnabled({$IFDEF VisualCLX} const {$ENDIF} Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetButton(Button: TControl);
    property ColorDialog: TJvOfficeColorDialog read FColorDialog write FColorDialog;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clBlack;

    property ClickColorButton: TJvClickColorButtonType read FClickColorButton;
    property Color: TColor read FSelectedColor write SetSelectedColor default clBlack;
    property Flat: Boolean read FFlat write SetFlat default True;
    property CustomColors: TStrings read GetCustomColors write SetCustomColors;
    property Properties: TJvOfficeColorPanelProperties read GetProperties write SetProperties;
    {$IFDEF VCL}
    property Options: TColorDialogOptions read FColorDialogOptions write SetColorDialogOptions default [];
    {$ENDIF VCL}
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnColorButtonClick: TNotifyEvent read FOnColorButtonClick write FOnColorButtonClick;
  end;

  TJvOfficeColorPanel = class(TJvCustomOfficeColorPanel)
  published
    property Flat;
    property Color;
    property CustomColors;
    {$IFDEF VCL}
    property Options;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnEndDock;
    property OnGetSiteInfo;
    {$ENDIF VCL}

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;

    property Properties;
    property OnColorChange;
    property OnColorButtonClick;
    property OnClick;
  end;

implementation

uses
  JvJCLUtils, JvResources;

//=== { TJvOfficeColorPanelProperties } ======================================

constructor TJvOfficeColorPanelProperties.Create;
begin
  inherited Create;
  FShowAutoButton := True;
  FShowOtherButton := True;
  FShowColorHint := True;
  FAutoColor := clDefault;

  FHorizontalMargin := MinHorizontalMargin;
  FTopMargin := MinTopMargin;
  FColorSpace := MinColorSize;
  FColorSpaceTop := MinColorSpaceTop;
  FColorSize := MinColorSize;
  FButtonHeight := MinButtonHeight;
  FColorSpaceBottom := MinColorSpaceBottom;
  FBottomMargin := MinBottomMargin;

  CreateDefaultText;
end;

procedure TJvOfficeColorPanelProperties.Assign(Source: TPersistent);
begin
  if Source is TJvOfficeColorPanelProperties then
    with TJvOfficeColorPanelProperties(Source) do
    begin
      Self.FShowAutoButton := ShowAutoButton;
      Self.FShowOtherButton := ShowOtherButton;
      Self.FShowColorHint := ShowColorHint;
      Self.FTopMargin := TopMargin;
      Self.FColorSpaceBottom := ColorSpaceBottom;
      Self.FHorizontalMargin := HorizontalMargin;
      Self.FColorSpace := ColorSpace;
      Self.FColorSpaceTop := ColorSpaceTop;
      Self.FButtonHeight := ButtonHeight;
      Self.FColorSize := ColorSize;
      Self.FBottomMargin := BottomMargin;

      Self.FAutoCaption := AutoCaption;
      Self.FOtherCaption := OtherCaption;
      Self.FAutoHint := AutoHint;
      Self.FOtherHint := OtherHint;
      Self.FAutoColor := AutoColor;
    end
  else
    inherited Assign(Source);
end;

procedure TJvOfficeColorPanelProperties.Changed(PropName: string);
begin
  if Assigned(FOnPropertiesChanged) then
    FOnPropertiesChanged(Self, PropName);
end;

procedure TJvOfficeColorPanelProperties.CreateDefaultText;
begin
  FAutoCaption := RsAutoCaption;
  FOtherCaption := RsOtherColorCaption;
end;

function TJvOfficeColorPanelProperties.GetStringValue(const Index: Integer): string;
begin
  case Index of
    Tag_AutoCaption:
      Result := FAutoCaption;
    Tag_OtherCaption:
      Result := FOtherCaption;
    Tag_AutoHint:
      Result := FAutoHint;
    Tag_OtherHint:
      Result := FOtherHint;
  end;
end;

procedure TJvOfficeColorPanelProperties.SetAutoColor(const Value: TColor);
begin
  if FAutoColor<>Value then
  begin
    FAutoColor := Value;
    Changed('AutoColor');
  end;
end;

procedure TJvOfficeColorPanelProperties.SetMeasure(const Index, Value: Integer);
var
  MeasureItem: PInteger;
  MeasureConst: Integer;
  LName: string;
begin
  case Index of
    Tag_TopMargin:
      begin
        MeasureItem := @FTopMargin;
        MeasureConst := MinTopMargin;
        LName := 'TopMargin';
      end;
    Tag_BottomMargin:
      begin
        MeasureItem := @FBottomMargin;
        MeasureConst := MinBottomMargin;
        LName := 'BottomMargin';
      end;
    Tag_HorizontalMargin:
      begin
        MeasureItem := @FHorizontalMargin;
        MeasureConst := MinHorizontalMargin;
        LName := 'HorizontalMargin';
      end;
    Tag_ColorSpace:
      begin
        MeasureItem := @FColorSpace;
        MeasureConst := MinColorSpace;
        LName := 'ColorSpace';
      end;
    Tag_ColorSpaceTop:
      begin
        MeasureItem := @FColorSpaceTop;
        MeasureConst := MinColorSpaceTop;
        LName := 'ColorSpaceTop';
      end;
    Tag_ColorSpaceBottom:
      begin
        MeasureItem := @FColorSpaceBottom;
        MeasureConst := MinColorSpaceBottom;
        LName := 'ColorSpaceBottom';
      end;
    Tag_ColorSize:
      begin
        MeasureItem := @FColorSize;
        MeasureConst := MinColorSize;
        LName := 'ColorSize';
      end;
    Tag_ButtonHeight:
      begin
        MeasureItem := @FButtonHeight;
        MeasureConst := MinButtonHeight;
        LName := 'ButtonHeight';
      end;
  else
    Exit;
  end;
  if MeasureItem^ = Value then
    Exit;

  MeasureItem^ := Value;
  if MeasureItem^ < MeasureConst then
    MeasureItem^ := MeasureConst;
  Changed(LName);
end;

procedure TJvOfficeColorPanelProperties.SetShowAutoButton(const Value: Boolean);
begin
  if FShowAutoButton <> Value then
  begin
    FShowAutoButton := Value;
    Changed('ShowAutoButton');
  end;
end;

procedure TJvOfficeColorPanelProperties.SetShowColorHint(const Value: Boolean);
begin
  if FShowColorHint <> Value then
  begin
    FShowColorHint := Value;
    Changed('ShowColorHint');
  end;
end;

procedure TJvOfficeColorPanelProperties.SetShowOtherButton(const Value: Boolean);
begin
  if FShowOtherButton <> Value then
  begin
    FShowOtherButton := Value;
    Changed('ShowOtherButton');
  end;
end;

procedure TJvOfficeColorPanelProperties.SetStringValue(const Index: Integer;
  const Value: string);
begin
  case Index of
    Tag_AutoCaption:
      if FAutoCaption <> Value then
      begin
        FAutoCaption := Value;
        Changed('AutoCaption');
      end;
    Tag_OtherCaption:
      if FOtherCaption <> Value then
      begin
        FOtherCaption := Value;
        Changed('OtherCaption');
      end;
    Tag_AutoHint:
      if FAutoHint <> Value then
      begin
        FAutoHint := Value;
        Changed('AutoHint');
      end;
    Tag_OtherHint:
      if FAutoHint <> Value then
      begin
        FOtherHint := Value;
        Changed('OtherHint');
      end;
  end;
end;

//=== { TJvSubColorButton } ==================================================

constructor TJvSubColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgeWidth := 4;
end;

function TJvSubColorButton.GetEdgeWidth: Integer;
begin
  Result := Height div 5;
end;

procedure TJvSubColorButton.Paint;
var
  B, X, Y: Integer;
  FColor: TColor;
begin
  if not Visible then
    Exit;
  inherited Paint;

  if Enabled then
    FColor := ButtonColor
  else
    FColor := clGray;
  if EdgeWidth >= 0 then
    B := EdgeWidth
  else
    B := Height div 5;
  with Canvas do
  begin
    if not Glyph.Empty then
    begin
      Glyph.Transparent := True;
      X := (Width div 2) - 9 + Integer(FState in [TJvButtonState(bsDown)]);
      Y := (Height div 2) + 4 + Integer(FState in [TJvButtonState(bsDown)]);
      Pen.Color := FColor;
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      Rectangle(X, Y, X + 17, Y + 4);
    end
    else
    begin
      if Caption = '' then
      begin
        Pen.Color := clGray;
        Brush.Color := FColor;
        Brush.Style := bsSolid;
        Rectangle(B, B, Width - B, Height - B);
      end
      else
      begin
        Pen.Color := clGray;
        Brush.Style := bsClear;
        Polygon([Point(B - 1, B - 1), Point(Width - (B - 1), B - 1),
          Point(Width - (B - 1), Height - (B - 1)), Point(B - 1, Height - (B - 1))]);
        Pen.Color := clGray;
        Brush.Color := FColor;
        Brush.Style := bsSolid;
        Rectangle(B + 1, B + 1, Height, Height - B);
      end;
    end;
  end;
end;

procedure TJvSubColorButton.SetEdgeWidth(const Value: Integer);
begin
  if FEdgeWidth <> Value then
  begin
    FEdgeWidth := Value;
    Repaint;
  end;
end;

//=== { TJvCustomOfficeColorPanel } ==========================================

constructor TJvCustomOfficeColorPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FInited := False;
  FSelectedColor := clBlack;
  {$IFDEF VCL}
  FColorDialogOptions := [];
  {$ENDIF VCL}
  FClickColorButton := cbctNone;

  FProperties := TJvOfficeColorPanelProperties.Create;
  FProperties.OnPropertiesChanged := PropertiesChanged;

  FAutoButton := TJvSubColorButton.Create(Self);

  with FAutoButton do
  begin
    Parent := Self;
    GroupIndex := 1;
    Tag := MaxColorButtonNumber + 1;
    Down := True;
    AllowAllUp := True;
    ButtonColor := FProperties.AutoColor;
    Hint := ColorToString(ButtonColor);
    Visible := False;
    OnClick := ColorButtonClick;
  end;

  FOtherButton := TJvSubColorButton.Create(Self);
  with FOtherButton do
  begin
    Parent := Self;
    GroupIndex := 1;
    Tag := MaxColorButtonNumber + 2;
    ButtonColor := clDefault;
    Hint := ColorToString(ButtonColor);
    AllowAllUp := True;
    Visible := False;
    OnClick := ColorButtonClick;
  end;

  FColorDialog := TJvOfficeColorDialog.Create(Self);
  {$IFDEF VCL}
  FColorDialog.Options := FColorDialogOptions;
  {$ENDIF VCL}

//  Font.Name := 'MS Shell Dlg 2';
  FAutoButton.Flat := True;
  FOtherButton.Flat := True;
  Flat := True;
  SetWordStyle(True);

  MakeColorButtons;

  FInited := True;
end;

destructor TJvCustomOfficeColorPanel.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TJvCustomOfficeColorPanel.SetButton(Button: TControl);
begin
  FOwner := Button;
end;

procedure TJvCustomOfficeColorPanel.MakeColorButtons;
var
  I: Integer;
begin
  for I := 0 to MaxColorButtonNumber - 1 do
  begin
    FColorButtons[I].Free;
    FColorButtons[I] := TJvSubColorButton.Create(Self);
    with FColorButtons[I] do
    begin
      Parent := Self;
      GroupIndex := 1;
      AllowAllUp := True;
      ButtonColor := SubColorButtonColors[I];
      Tag := I;
      Flat := True;
      Hint := ColorToString(ButtonColor);
      OnClick := ColorButtonClick;
    end;
  end;
  Invalidate;
end;

procedure TJvCustomOfficeColorPanel.AdjustColorButtons;
var
  I: Integer;
  ButtonLine: Integer;
  TempHeight: Integer;
  LButtonCount, LColorsButtonTop, LColorsButtonLeft: Integer;
begin
  if (not FInited) or (Parent = nil) then
    Exit;
  DisableAlign;
  TempHeight := 0;
  LButtonCount := 0;
  ButtonLine := (MaxColorButtonNumber + LineColorButtonCount - 1) div LineColorButtonCount;

  if Properties.ShowAutoButton then
    Inc(LButtonCount);
  if Properties.ShowOtherButton then
    Inc(LButtonCount);

  with Properties do
  begin
    Width := HorizontalMargin * 2 + ColorSize * LineColorButtonCount +
      ColorSpace * (LineColorButtonCount - 1);
    Height := TopMargin + BottomMargin + ColorSpaceTop + ColorSpaceBottom + ColorSize * ButtonLine +
      ColorSpace * (ButtonLine - 1) + ButtonHeight * LButtonCount + TempHeight;
  end;

  with Properties do
    if not Properties.ShowAutoButton then
    begin
      FAutoButton.Visible := False;
      LColorsButtonTop := 0;
      LColorsButtonLeft := HorizontalMargin;
    end
    else
    begin
      FAutoButton.Visible := True;
      FAutoButton.Caption := AutoCaption;
      LColorsButtonTop := FAutoButton.Top + FAutoButton.Height;
      FAutoButton.SetBounds(HorizontalMargin, TopMargin + TempHeight,
        ClientWidth - HorizontalMargin * 2, ButtonHeight);
      LColorsButtonLeft := FAutoButton.Left;
    end;

  with Properties do
    for I := 0 to MaxColorButtonNumber - 1 do
      FColorButtons[I].SetBounds(LColorsButtonLeft + (I mod LineColorButtonCount) * (ColorSpace + ColorSize),
        LColorsButtonTop + ColorSpaceTop + (I div LineColorButtonCount) * (ColorSpace + ColorSize),
        ColorSize, ColorSize);

  with Properties do
    if not Properties.ShowOtherButton then
      FOtherButton.Visible := False
    else
    begin
      FOtherButton.Visible := True;
      FOtherButton.Caption := OtherCaption;
      FOtherButton.SetBounds(FAutoButton.Left,
        FColorButtons[MaxColorButtonNumber - 1].Top + ColorSize + ColorSpaceBottom,
        FAutoButton.Width, ButtonHeight);
    end;
end;

procedure TJvCustomOfficeColorPanel.ColorButtonClick(Sender: TObject);
{$IFDEF VisualCLX}
var
  I: Integer;
{$ENDIF VisualCLX}
begin
  if Sender is TJvColorSpeedButton then
  begin
    if TComponent(Sender).Tag = FAutoButton.Tag then
       FClickColorButton := cbctAutoButton
    else
    if TComponent(Sender).Tag = FOtherButton.Tag then
       FClickColorButton := cbctOtherButton
    else
      FClickColorButton := cbctColorsButton;
  end
  else
    FClickColorButton := cbctNone;

  if Assigned(FOnColorButtonClick) then
     FOnColorButtonClick(Sender);

  if TComponent(Sender).Tag = FOtherButton.Tag then
  begin
    {$IFDEF VCL}
    FColorDialog.Options := FColorDialogOptions;
    {$ENDIF VCL}
    FColorDialog.Color := FSelectedColor;
    if FColorDialog.Execute then
    begin
      SetSelectedColor(FColorDialog.Color);
      FOtherButton.ButtonColor := FSelectedColor;
      FOtherButton.Hint := ColorToString(FOtherButton.ButtonColor);
    end
    else
      Exit;
  end
  else
  begin
    TJvSubColorButton(Sender).Down := True;
    {$IFDEF VisualCLX}
    //in clx have bug
    FAutoButton.Down := FAutoButton = Sender;
    FOtherButton.Down := FOtherButton = Sender;
    for I := 0 to MaxColorButtonNumber - 1 do
      FColorButtons[I].Down := FColorButtons[I] = Sender;
    {$ENDIF VisualCLX}
    SetSelectedColor(TJvSubColorButton(Sender).ButtonColor);
  end;
end;

procedure TJvCustomOfficeColorPanel.SetWordStyle(const Value: Boolean);
begin
  if FWordStyle <> Value then
  begin
    FWordStyle := Value;
    with Properties do
      if FWordStyle then
      begin
        SetFlat(True);

        ButtonHeight := MinButtonHeight;
        ColorSize := MinColorSize;
        ColorSpace := MinColorSpace;
        ColorSpaceTop := MinColorSpaceTop;
        ColorSpaceBottom := MinColorSpaceBottom;
        TopMargin := MinTopMargin;
        BottomMargin := MinBottomMargin;
        HorizontalMargin := MinHorizontalMargin;
      end;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetSelectedColor(const Value: TColor);
var
  I: Integer;
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Color := Value;
    if FAutoButton.ButtonColor = Value then
      FAutoButton.Down := True
    else
    begin
      FAutoButton.Down := False;
      for I := 0 to MaxColorButtonNumber - 1 do
        if FColorButtons[I].ButtonColor = Value then
        begin
          FColorButtons[I].Down := True;
          Break;
        end
        else
          FColorButtons[I].Down := False;
    end;

    if Assigned(FOnColorChange) then
      FOnColorChange(Self);
  end;
end;

function TJvCustomOfficeColorPanel.GetCustomColors: TStrings;
begin
  Result := FColorDialog.CustomColors;
end;

procedure TJvCustomOfficeColorPanel.SetCustomColors(const Value: TStrings);
begin
  FColorDialog.CustomColors.Assign(Value);
end;

procedure TJvCustomOfficeColorPanel.Resize;
begin
  inherited Resize;
  if FInited then
    AdjustColorButtons;
end;

procedure TJvCustomOfficeColorPanel.Paint;
begin
  inherited Paint;
  if FFlat then
  begin
    Canvas.Brush.Color := clBtnFace;
    {$IFDEF VCL}
    Canvas.FrameRect(ClientRect);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FrameRect(Canvas, ClientRect);
    {$ENDIF VisualCLX}
    Canvas.Brush.Color := Color;
  end;
  if FInited then
    AdjustColorButtons;
end;

procedure TJvCustomOfficeColorPanel.SetEnabled({$IFDEF VisualCLX} const {$ENDIF} Value: Boolean);
var
  I: Integer;
begin
  inherited SetEnabled(Value);
  FAutoButton.Enabled := Value;
  FOtherButton.Enabled := Value;
  for I := 0 to MaxColorButtonNumber - 1 do
    FColorButtons[I].Enabled := Value;
end;

procedure TJvCustomOfficeColorPanel.ShowHintChanged;
var
  I: Integer;
begin
  inherited ShowHintChanged;
  FAutoButton.ShowHint := ShowHint;
  FOtherButton.ShowHint := ShowHint;
  for I := 0 to MaxColorButtonNumber - 1 do
    FColorButtons[I].ShowHint := ShowHint;
end;

{$IFDEF VCL}

procedure TJvCustomOfficeColorPanel.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  FColorDialogOptions := Value;
end;

procedure TJvCustomOfficeColorPanel.CreateWnd;
begin
  inherited CreateWnd;
  AdjustColorButtons;
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomOfficeColorPanel.InitWidget;
begin
  inherited InitWidget;
  AdjustColorButtons;
end;
{$ENDIF VisualCLX}

function TJvCustomOfficeColorPanel.GetProperties: TJvOfficeColorPanelProperties;
begin
  Result := FProperties;
end;

procedure TJvCustomOfficeColorPanel.SetProperties(const Value: TJvOfficeColorPanelProperties);
begin
  if FProperties <> Value then
    FProperties.Assign(Value);
end;

procedure TJvCustomOfficeColorPanel.PropertiesChanged(Sender: TObject;
  PropName: string);
var
  LFlag: Boolean;
  I: Integer;
begin
  LFlag := False;
  if Cmp(PropName, 'ShowAutoButton') or cmp(PropName, 'ShowOtherButton') then
    LFlag := True
  else
  if Cmp(PropName, 'AutoCaption') then
  begin
    if Properties.AutoCaption = '' then
      Properties.ShowAutoButton := False;
  end
  else
  if Cmp(PropName, 'OtherCaption') then
  begin
    if Properties.OtherCaption = '' then
      Properties.ShowOtherButton := False;
  end
  else
  if Cmp(PropName, 'AutoHint') then
    FAutoButton.Hint := Properties.AutoHint
  else
  if Cmp(PropName, 'OtherHint') then
    FOtherButton.Hint := Properties.OtherHint
  else
  if Cmp(PropName, 'AutoColor') then
    FAutoButton.ButtonColor := Properties.AutoColor
  else
  if Cmp(PropName, 'ShowColorHint') then
  begin
    FAutoButton.ShowHint :=  Properties.ShowColorHint;
    FOtherButton.ShowHint :=  Properties.ShowColorHint;
    for I := 0 to MaxColorButtonNumber - 1 do
      FColorButtons[I].ShowHint := Properties.ShowColorHint;
  end
  else
    LFlag := True;
  if LFlag then
    AdjustColorButtons;
end;

//=== { TJvColorSpeedButton } ================================================

constructor TJvColorSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FButtonColor := clDefault;
end;

procedure TJvColorSpeedButton.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;

end.

