{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSpeedButton.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgSpeedButton;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, JvgTypes, JvgCommClasses, JvgUtils,
  ExtCtrls, Buttons, JVCLVer, StdCtrls, Forms;

type
  TJvgSpeedButton = class(TSpeedButton)
  private
    FCanvas: TControlCanvas;
    fMouseEnter: boolean;
    FColor: TColor;
    IsDown: boolean;
    FControl: TControl;
    FFrame: boolean;
    FCaptionLabel: TLabel;
    FDefaultStyle: boolean;
    FModalResult: TModalResult;
    FFrameColor: TColor;
    FActiveColor: TColor;
    FAboutJVCL: TJVCLAboutInfo;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetCanvas: TCanvas;
    procedure SetControl(const Value: TControl);
    procedure SetFrame(const Value: boolean);
    procedure SetCaptionLabel(const Value: TLabel);
    procedure SetDefaultStyle(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
  protected
    { (rb) Better respond to CM_ENABLEDCHANGED, but don't know if that works
           on D5,D6 }
    procedure SetEnabled(Value: boolean); override;
    function GetEnabled: boolean; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
      false;
    property Color: TColor read FColor write SetColor;
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    property Control: TControl read FControl write SetControl;
    property CaptionLabel: TLabel read FCaptionLabel write SetCaptionLabel;
    property Frame: boolean read FFrame write SetFrame default true;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property DefaultStyle: boolean read FDefaultStyle write SetDefaultStyle;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property ModalResult: TModalResult read FModalResult write FModalResult;
  end;

  TJvgExtSpeedButton = class(TJvgSpeedButton)
  private
    FStyle: TJvgSpeedButtonStyle;
    FStyleActive: TJvgSpeedButtonStyle;
    FStylePushed: TJvgSpeedButtonStyle;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetColor(const Value: TColor);
    procedure SetActiveColor(const Value: TColor);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetActiveColor: TColor;
    function GetColor: TColor;
  protected
    procedure Paint; override;
    procedure OnChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Style: TJvgSpeedButtonStyle read FStyle write FStyle;
    property StyleActive: TJvgSpeedButtonStyle read FStyleActive write FStyleActive;
    property StylePushed: TJvgSpeedButtonStyle read FStylePushed write FStylePushed;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor stored false;
    property ActiveColor: TColor read GetActiveColor write SetActiveColor stored false;
  end;

implementation

constructor TJvgSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self; //...i can draw now! :)
  //..defaults
  FColor := IncColor(GetSysColor(COLOR_BTNFACE), 30);
  FActiveColor := IncColor(FColor, 10);
  FFrame := true;
end;

destructor TJvgSpeedButton.Destroy;
begin
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
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
    FColor := GetNearestColor(FCanvas.Handle, FColor);

  R := ClientRect;

  if IsDown and fMouseEnter then
    BevelOuter := bvLowered
  else
    BevelOuter := bvRaised;
  if Flat and not IsDown then
    BevelOuter := bvNone;

  if FFrame then
    InflateRect(R, -1, -1);
  dec(R.Right);
  dec(R.Bottom);
  DrawBoxEx(FCanvas.Handle, R, ALLGLSIDES, bvNone, BevelOuter, false,
    iif(fMouseEnter, ActiveColor, Color), false);

  if Transparent then
    SetBkMode(FCanvas.Handle, Windows.TRANSPARENT)
  else
    SetBkMode(FCanvas.Handle, Windows.OPAQUE);

  FCanvas.Font.Assign(Font);
  if not Enabled then
    FCanvas.Font.Color := clGrayText;
  if Assigned(Glyph) then
    Inc(R.Left, Glyph.Width);

  if IsDown then
    OffsetRect(R, 1, 1);
  DrawText(FCanvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE or
    DT_CENTER or DT_VCENTER);

  R := ClientRect;
  FCanvas.Brush.Color := 0;
  if FFrame then
  begin
    FCanvas.Font.Color := FFrameColor;
    FCanvas.FrameRect(R);
  end;

  if Assigned(Glyph) then
    CreateBitmapExt(FCanvas.Handle, Glyph, ClientRect, (Width - Glyph.Width -
      FCanvas.TextWidth(Caption)) div 2 + integer(IsDown) - 1 - Spacing, 1 +
      (Height - Glyph.Height) div 2 + integer(IsDown),
      fwoNone, fdsDefault,
      true, GetTransparentColor(Glyph, ftcLeftBottomPixel), 0);

end;

procedure TJvgSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  fMouseEnter := true;
  if IsDown or (Color <> ActiveColor) then
    Invalidate;
end;

procedure TJvgSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  fMouseEnter := false;
  if IsDown or (Color <> ActiveColor) then
    Invalidate;
end;

function TJvgSpeedButton.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvgSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  IsDown := true;
  Invalidate;
end;

procedure TJvgSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  IsDown := false;
  Invalidate;
end;

procedure TJvgSpeedButton.Click;
var
  Form: TCustomForm;
begin
  inherited;
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

procedure TJvgSpeedButton.SetFrame(const Value: boolean);
begin
  FFrame := Value;
  Invalidate;
end;

procedure TJvgSpeedButton.SetCaptionLabel(const Value: TLabel);
begin
  FCaptionLabel := Value;
  Invalidate;
end;

procedure TJvgSpeedButton.SetDefaultStyle(const Value: boolean);
begin
  FDefaultStyle := Value;
  Invalidate;
end;

procedure TJvgSpeedButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if Assigned(FControl) then
    FControl.Enabled := Value
end;

function TJvgSpeedButton.GetEnabled: boolean;
begin
  Result := inherited GetEnabled;
end;

procedure TJvgSpeedButton.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;

procedure TJvgSpeedButton.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
  Invalidate;
end;

{ TJvgExtSpeedButton }

constructor TJvgExtSpeedButton.Create( AOwner : TComponent );
begin
  inherited;
  FStyle := TJvgSpeedButtonStyle.Create;
  FStyleActive := TJvgSpeedButtonStyle.Create;
  FStylePushed := TJvgSpeedButtonStyle.Create;

  FStyle.OnChanged := OnChanged;
  FStyleActive.OnChanged := OnChanged;
  FStylePushed.OnChanged := OnChanged;
  //..defaults
  FStyle.Color := incColor(clBtnFace, 30);
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
  inherited;
end;

procedure TJvgExtSpeedButton.Paint;
var
  R: TRect;
  offset: integer;
  _Style: TJvgSpeedButtonStyle;
  function TextStyle: TglTextStyle;
  begin
    if Enabled then Result := _Style.TextStyle else Result := fstPushed;
  end;
begin
  if DefaultStyle then
  begin
    inherited Paint;
    exit;
  end;
  R := ClientRect;

  if IsDown and fMouseEnter then
    _Style := StylePushed
  else if fMouseEnter then
    _Style := StyleActive
  else
    _Style := Style;

  if FFrame then InflateRect(R, -1, -1);
  dec(R.Right); dec(R.Bottom);

  with _Style do
  begin
    R := DrawBoxEx(Canvas.Handle, R, Bevel.Sides, Bevel.Inner, Bevel.Outer, Bevel.Bold, Color, Gradient.Active );
    if Gradient.Active then
    begin
      inc(R.Right); inc(R.Bottom);
      Gradient.Draw(Canvas.Handle, R, integer(psSolid), 1);
      dec(R.Right); dec(R.Bottom);
    end;
  end;

  if not Glyph.Empty then
    inc(R.Left, Glyph.Width);

  Canvas.Font.Assign(_Style.Font);
  if IsDown then offset := 1 else offset := 0;
  ExtTextOutExt(Canvas.Handle, R.Left+offset+(R.Right - R.Left -Canvas.TextWidth(Caption)) div 2, R.Top+offset+(R.Bottom - R.Top - Canvas.TextHeight(Caption)) div 2, R, Caption,
    TextStyle, false { fcoDelineatedText in Options},
    false, _Style.Font.Color, _Style.DelineateColor,
    _Style.HighlightColor, _Style.ShadowColor,
    nil, _Style.TextGradient, _Style.Font );

  R := ClientRect;
  Canvas.Brush.Color := 0;
  if FFrame then
  begin
    Canvas.Font.Color := FFrameColor;
    Canvas.FrameRect(R);
  end;

  if Assigned(Glyph) then
    CreateBitmapExt( Canvas.Handle, Glyph, ClientRect, (Width - Glyph.Width - Canvas.TextWidth(Caption))div 2 + integer(IsDown) - 1-Spacing, 1+(Height - Glyph.Height)div 2 + integer(IsDown),
      fwoNone, fdsDefault,
      true, GetTransparentColor(Glyph, ftcLeftBottomPixel), 0 );

end;

procedure TJvgExtSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not Enabled then exit;
  fMouseEnter := true;
  Paint;
end;

procedure TJvgExtSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not Enabled then exit;
  fMouseEnter := false;
  Paint;
end;


procedure TJvgExtSpeedButton.OnChanged(Sender: TObject);
begin
  if csLoading in ComponentState then exit;
  Invalidate;
end;

procedure TJvgExtSpeedButton.SetColor(const Value: TColor);
begin
  Style.Color := Value;
  Invalidate;
end;

procedure TJvgExtSpeedButton.SetActiveColor(const Value: TColor);
begin
  StyleActive.Color := Value;
  Invalidate;
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

end.

