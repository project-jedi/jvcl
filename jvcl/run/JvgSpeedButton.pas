{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSpeedButton.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgSpeedButton;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics,
  ExtCtrls, Buttons, StdCtrls, Forms,
  {$IFDEF USEJVCL}
  JVCLVer,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, JvgUtils;

type
  TJvgSpeedButton = class(TSpeedButton)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FMouseEnter: Boolean;
    FColor: TColor;
    FIsDown: Boolean;
    FControl: TControl;
    FFrame: Boolean;
    FCaptionLabel: TLabel;
    FDefaultStyle: Boolean;
    FModalResult: TModalResult;
    FFrameColor: TColor;
    FActiveColor: TColor;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetControl(const Value: TControl);
    procedure SetFrame(const Value: Boolean);
    procedure SetCaptionLabel(const Value: TLabel);
    procedure SetDefaultStyle(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
  protected
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    { (rb) Better respond to CM_ENABLEDCHANGED, but don't know if that works on D5,D6 }
    procedure SetEnabled(Value: Boolean); override;
    function GetEnabled: Boolean; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    property Canvas;
  published
    {$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    {$ENDIF USEJVCL}
    property Color: TColor read FColor write SetColor;
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    property Control: TControl read FControl write SetControl;
    property CaptionLabel: TLabel read FCaptionLabel write SetCaptionLabel;
    property Frame: Boolean read FFrame write SetFrame default True;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property DefaultStyle: Boolean read FDefaultStyle write SetDefaultStyle;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ModalResult: TModalResult read FModalResult write FModalResult;
  end;

  TJvgExtSpeedButton = class(TJvgSpeedButton)
  private
    FStyle: TJvgSpeedButtonStyle;
    FStyleActive: TJvgSpeedButtonStyle;
    FStylePushed: TJvgSpeedButtonStyle;
    procedure SetColor(const Value: TColor);
    procedure SetActiveColor(const Value: TColor);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetActiveColor: TColor;
    function GetColor: TColor;
    procedure SetStyle(Value: TJvgSpeedButtonStyle);
    procedure SetStyleActive(Value: TJvgSpeedButtonStyle);
    procedure SetStylePushed(Value: TJvgSpeedButtonStyle);
    procedure ButtonChanged(Sender: TObject);
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActiveColor: TColor read GetActiveColor write SetActiveColor stored False;
    property Color: TColor read GetColor write SetColor stored False;
    property Style: TJvgSpeedButtonStyle read FStyle write SetStyle;
    property StyleActive: TJvgSpeedButtonStyle read FStyleActive write SetStyleActive;
    property StylePushed: TJvgSpeedButtonStyle read FStylePushed write SetStylePushed;
    property Font: TFont read GetFont write SetFont;
  end;

{$IFNDEF USEJVCL}
  {$UNDEF UNITVERSIONING}
{$ENDIF ~USEJVCL}

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

//=== { TJvgSpeedButton } ====================================================

constructor TJvgSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //..defaults
  FColor := IncColor(GetSysColor(COLOR_BTNFACE), 30);
  FActiveColor := IncColor(FColor, 10);
  FFrame := True;
end;

procedure TJvgSpeedButton.Paint;
var
  R: TRect;
  BevelOuter: TPanelBevel;
begin
  if csDestroying in ComponentState then
    Exit;
  if DefaultStyle then
  begin
    inherited Paint;
    Exit;
  end;
  if SystemColorDepth < 16 then
    FColor := GetNearestColor(Canvas.Handle, FColor);

  R := ClientRect;

  if FIsDown and FMouseEnter then
    BevelOuter := bvLowered
  else
    BevelOuter := bvRaised;
  if Flat and not FIsDown then
    BevelOuter := bvNone;

  if FFrame then
    InflateRect(R, -1, -1);
  Dec(R.Right);
  Dec(R.Bottom);
  DrawBoxEx(Canvas.Handle, R, ALLGLSIDES, bvNone, BevelOuter, False,
    IIF(FMouseEnter, ActiveColor, Color), False);

  if Transparent then
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT)
  else
    SetBkMode(Canvas.Handle, Windows.OPAQUE);

  Canvas.Font.Assign(Font);
  if not Enabled then
    Canvas.Font.Color := clGrayText;
  if Assigned(Glyph) then
    Inc(R.Left, Glyph.Width);

  if FIsDown then
    OffsetRect(R, 1, 1);
  Windows.DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
    DT_SINGLELINE or DT_CENTER or DT_VCENTER);

  R := ClientRect;
  Canvas.Brush.Color := clBlack;
  if FFrame then
  begin
    Canvas.Font.Color := FFrameColor;
    Canvas.FrameRect(R);
  end;

  if Assigned(Glyph) then
    CreateBitmapExt(Canvas.Handle, Glyph, ClientRect, (Width - Glyph.Width -
      Canvas.TextWidth(Caption)) div 2 + Integer(FIsDown) - 1 - Spacing, 1 +
      (Height - Glyph.Height) div 2 + Integer(FIsDown),
      fwoNone, fdsDefault,
      True, GetTransparentColor(Glyph, ftcLeftBottomPixel), 0);
end;

procedure TJvgSpeedButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(TControl(Msg.LParam));
end;

procedure TJvgSpeedButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(TControl(Msg.LParam));
end;

procedure TJvgSpeedButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  FMouseEnter := True;
  if FIsDown or (Color <> ActiveColor) then
    Invalidate;
end;

procedure TJvgSpeedButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  FMouseEnter := False;
  if FIsDown or (Color <> ActiveColor) then
    Invalidate;
end;

procedure TJvgSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FIsDown := True;
  Invalidate;
end;

procedure TJvgSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FIsDown := False;
  Invalidate;
end;

procedure TJvgSpeedButton.Click;
var
  Form: TCustomForm;
begin
  inherited Click;
  if ModalResult = mrNone then
    Exit;
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;
end;

procedure TJvgSpeedButton.SetControl(const Value: TControl);
begin
  FControl := Value;
end;

procedure TJvgSpeedButton.SetFrame(const Value: Boolean);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    Invalidate;
  end;
end;

procedure TJvgSpeedButton.SetCaptionLabel(const Value: TLabel);
begin
  if FCaptionLabel <> Value then
  begin
    FCaptionLabel := Value;
    Invalidate;
  end;
end;

procedure TJvgSpeedButton.SetDefaultStyle(const Value: Boolean);
begin
  if FDefaultStyle <> Value then
  begin
    FDefaultStyle := Value;
    Invalidate;
  end;
end;

procedure TJvgSpeedButton.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Assigned(FControl) then
    FControl.Enabled := Value;
end;

function TJvgSpeedButton.GetEnabled: Boolean;
begin
  Result := inherited GetEnabled;
end;

procedure TJvgSpeedButton.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TJvgSpeedButton.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

//=== { TJvgExtSpeedButton } =================================================

constructor TJvgExtSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := TJvgSpeedButtonStyle.Create;
  FStyleActive := TJvgSpeedButtonStyle.Create;
  FStylePushed := TJvgSpeedButtonStyle.Create;

  FStyle.OnChanged := ButtonChanged;
  FStyleActive.OnChanged := ButtonChanged;
  FStylePushed.OnChanged := ButtonChanged;
  //..defaults
  FStyle.Color := IncColor(clBtnFace, 30);
  FStyleActive.Color := IncColor(FStyle.Color, 10);
  FStylePushed.Color := DecColor(FStyle.Color, 10);
  FStyle.Bevel.Inner := bvRaised;
  FStyleActive.Bevel.Inner := bvRaised;
  FStylePushed.Bevel.Inner := bvLowered;
end;

destructor TJvgExtSpeedButton.Destroy;
begin
  FStyle.Free;
  FStyleActive.Free;
  FStylePushed.Free;
  inherited Destroy;
end;

procedure TJvgExtSpeedButton.Paint;
var
  R: TRect;
  Offset: Integer;
  LStyle: TJvgSpeedButtonStyle;

  function TextStyle: TglTextStyle;
  begin
    if Enabled then
      Result := LStyle.TextStyle
    else
      Result := fstPushed;
  end;

begin
  if DefaultStyle then
  begin
    inherited Paint;
    Exit;
  end;
  R := ClientRect;

  if FIsDown and FMouseEnter then
    LStyle := StylePushed
  else
  if FMouseEnter then
    LStyle := StyleActive
  else
    LStyle := Style;

  if FFrame then
    InflateRect(R, -1, -1);
  Dec(R.Right);
  Dec(R.Bottom);

  with LStyle do
  begin
    R := DrawBoxEx(Canvas.Handle, R, Bevel.Sides, Bevel.Inner, Bevel.Outer, Bevel.Bold, Color, Gradient.Active);
    if Gradient.Active then
    begin
      Inc(R.Right);
      Inc(R.Bottom);
      Gradient.Draw(Canvas.Handle, R, Integer(psSolid), 1);
      Dec(R.Right);
      Dec(R.Bottom);
    end;
  end;

  if not Glyph.Empty then
    Inc(R.Left, Glyph.Width);

  Canvas.Font.Assign(LStyle.Font);
  if FIsDown then
    Offset := 1
  else
    Offset := 0;
  ExtTextOutExt(Canvas.Handle, R.Left + Offset + (R.Right - R.Left - Canvas.TextWidth(Caption)) div 2, R.Top + Offset +
    (R.Bottom - R.Top - Canvas.TextHeight(Caption)) div 2, R, Caption,
    TextStyle, False { fcoDelineatedText in Options},
    False, LStyle.Font.Color, LStyle.DelineateColor,
    LStyle.HighlightColor, LStyle.ShadowColor,
    nil, LStyle.TextGradient, LStyle.Font);

  R := ClientRect;
  Canvas.Brush.Color := 0;
  if FFrame then
  begin
    Canvas.Font.Color := FFrameColor;
    Canvas.FrameRect(R);
  end;

  if Assigned(Glyph) then
    CreateBitmapExt(Canvas.Handle, Glyph, ClientRect, (Width - Glyph.Width - Canvas.TextWidth(Caption)) div 2 +
      Integer(FIsDown) - 1 - Spacing, 1 + (Height - Glyph.Height) div 2 + Integer(FIsDown),
      fwoNone, fdsDefault,
      True, GetTransparentColor(Glyph, ftcLeftBottomPixel), 0);
end;

procedure TJvgExtSpeedButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseEnter(Control);
  if Enabled then
  begin
    FMouseEnter := True;
    Repaint;
  end;
end;

procedure TJvgExtSpeedButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  if Enabled then
  begin
    FMouseEnter := False;
    Repaint;
  end;
end;

procedure TJvgExtSpeedButton.ButtonChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TJvgExtSpeedButton.SetColor(const Value: TColor);
begin
  if Style.Color <> Value then
  begin
    Style.Color := Value;
    Invalidate;
  end;
end;

procedure TJvgExtSpeedButton.SetActiveColor(const Value: TColor);
begin
  if StyleActive.Color <> Value then
  begin
    StyleActive.Color := Value;
    Invalidate;
  end;
end;

function TJvgExtSpeedButton.GetActiveColor: TColor;
begin
  Result := StyleActive.Color;
end;

function TJvgExtSpeedButton.GetColor: TColor;
begin
  Result := Style.Color;
end;

function TJvgExtSpeedButton.GetFont: TFont;
begin
  Result := inherited Font;
end;

procedure TJvgExtSpeedButton.SetFont(const Value: TFont);
begin
  inherited Font.Assign(Font);
  Style.Font.Assign(Font);
end;

procedure TJvgExtSpeedButton.SetStyle(Value: TJvgSpeedButtonStyle);
begin
  FStyle.Assign(Value);
end;

procedure TJvgExtSpeedButton.SetStyleActive(Value: TJvgSpeedButtonStyle);
begin
  FStyleActive.Assign(Value);
end;

procedure TJvgExtSpeedButton.SetStylePushed(Value: TJvgSpeedButtonStyle);
begin
  FStylePushed.Assign(Value);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

