{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCheckBox.PAS, released on 2003-01-15.

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

unit JvgCheckBox;

{$I jvcl.inc}
{$I windowsonly.inc} // (ahuser) uses WndProc and Wnd hooks

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, JvgUtils;

type
  {$IFDEF USEJVCL}
  TJvgCheckBox = class(TJvGraphicControl)
  {$ELSE}
  TJvgCheckBox = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FChecked: Boolean;
    FColors: TJvgLabelColors;
    FIllumination: TJvgIllumination;
    FGlyph: TBitmap;
    FGlyphOn: TBitmap;
    FGlyphOff: TBitmap;
    FGlyphDisabled: TBitmap;
    FGradient: TJvgGradient;
    FGroupIndex: Integer;
    FGlyphShift: TJvgPointClass;
    FOptions: TglCheckBoxOptions;
    FTransparent: Boolean;
    FTextStyles: TJvgLabelTextStyles;
    FDisabledMaskColor: TColor;
    FInterspace: Integer;
    FFocusControl: TWinControl;
    FFocusControlMethod: TFocusControlMethod;
    FAfterPaint: TNotifyEvent;
    FGlyphKind: TglGlyphKind;
    FPrevWndProc: Pointer;
    FNewWndProc: Pointer;
    FActiveNow: Boolean;
    FShowAsActiveWhileControlFocused: Boolean;
    FImg: TBitmap;
    FNeedUpdateOnlyMainText: Boolean;
    FSuppressCMFontChanged: Boolean;
    FOnlyTextStyleChanged: Boolean;
    FAlignment: TLeftRight;
    FNeedRebuildBackground: Boolean;
    function IsCustomGlyph: Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    function GetGlyph: TBitmap;
    procedure SetGlyphOn(Value: TBitmap);
    function GetGlyphOn: TBitmap;
    procedure SetGlyphOff(Value: TBitmap);
    function GetGlyphOff: TBitmap;
    procedure SetGlyphDisabled(Value: TBitmap);
    function GetGlyphDisabled: TBitmap;
    procedure SetGroupIndex(Value: Integer);
    procedure SetOptions(Value: TglCheckBoxOptions);
    procedure SetTransparent(Value: Boolean);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetInterspace(Value: Integer);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetGlyphKind(Value: TglGlyphKind);

    procedure OnGradientChanged(Sender: TObject);
    procedure OnIlluminationChanged(Sender: TObject);
    procedure WMLButtonUp(var Msg: TMessage); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure SetAlignment(const Value: TLeftRight);
  protected
    {$IFDEF USEJVCL}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure FontChanged; override;
    procedure TextChanged; override;
    {$ENDIF USEJVCL}
    procedure Resize; override;
    procedure Paint; override;
    procedure HookFocusControlWndProc;
    procedure UnhookFocusControlWndProc;
    procedure FocusControlWndHookProc(var Msg: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function GetCheckedItemInGroup: TJvgCheckBox;
    procedure SetCheckedItemInGroup(TagNo: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Anchors;
    property Align;
    property Caption;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property DragCursor;
    property DragMode;
    property Font;

    property Alignment: TLeftRight read FAlignment write SetAlignment default taRightJustify;
    property GlyphKind: TglGlyphKind read FGlyphKind write SetGlyphKind default fgkDefault;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphOn: TBitmap read GetGlyphOn write SetGlyphOn stored IsCustomGlyph;
    property GlyphOff: TBitmap read GetGlyphOff write SetGlyphOff stored IsCustomGlyph;
    property GlyphDisabled: TBitmap read GetGlyphDisabled write SetGlyphDisabled stored IsCustomGlyph;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property GlyphShift: TJvgPointClass read FGlyphShift write FGlyphShift;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TextStyles: TJvgLabelTextStyles read FTextStyles write FTextStyles;
    property Colors: TJvgLabelColors read FColors write FColors;
    property Options: TglCheckBoxOptions read FOptions write SetOptions;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Illumination: TJvgIllumination read FIllumination write FIllumination;
    property DisabledMaskColor: TColor read FDisabledMaskColor write SetDisabledMaskColor default clBlack;
    property Interspace: Integer read FInterspace write SetInterspace default 0;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property FocusControlMethod: TFocusControlMethod read FFocusControlMethod write FFocusControlMethod default fcmOnMouseDown;

    {$IFDEF USEJVCL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF USEJVCL}
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
  end;

implementation

{$IFDEF USEJVCL}
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvThemes, JvJVCLUtils;
{$ELSE}
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;
{$ENDIF USEJVCL}

{$R ..\Resources\JvgCheckBox.res}

{$IFNDEF USEJVCL}

function JvMakeObjectInstance(Method: TWndMethod): Pointer;
begin
  {$IFDEF COMPILER6_UP}
  Result := Classes.MakeObjectInstance(Method);
  {$ELSE}
  Result := MakeObjectInstance(Method);
  {$ENDIF COMPILER6_UP}
end;

procedure JvFreeObjectInstance(ObjectInstance: Pointer);
begin
  if ObjectInstance <> nil then
    {$IFDEF COMPILER6_UP}
    Classes.FreeObjectInstance(ObjectInstance);
    {$ELSE}
    FreeObjectInstance(ObjectInstance);
    {$ENDIF COMPILER6_UP}
end;

{$ENDIF !USEJVCL}

constructor TJvgCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle :=
    [csCaptureMouse, csOpaque, csClickEvents, csSetCaption, csReplicatable];
  //  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  {$IFDEF USEJVCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF USEJVCL}

  //  FGlyphOn := TBitmap.Create;
  //  FGlyphOff := TBitmap.Create;
  FImg := TBitmap.Create;
  TextStyles := TJvgLabelTextStyles.Create;
  Colors := TJvgLabelColors.Create;
  Gradient := TJvgGradient.Create;
  FIllumination := TJvgIllumination.Create;
  FGlyphShift := TJvgPointClass.Create;

  //..defaults
  Width := 80;
  Height := 17;
  FAlignment := taRightJustify;
  FChecked := False;
  FTransparent := False;
  FGradient.OnChanged := OnGradientChanged;
  FIllumination.OnChanged := OnIlluminationChanged;
  TextStyles.OnChanged := OnIlluminationChanged;
  Colors.OnChanged := OnIlluminationChanged;
  FGlyphShift.OnChanged := OnGradientChanged;
  FOptions := [fcoFastDraw];
  FGroupIndex := 0;
  FInterspace := 0;
  FFocusControlMethod := fcmOnMouseDown;
  FNeedRebuildBackground := True;

  FImg.Canvas.Brush.Color := clBtnFace;
  FImg.Canvas.Brush.Style := bsSolid;
  //  FNeedUpdateOnlyMainText := False;
  {$IFDEF FR_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF FR_RUS}
  GlyphKind := fgkDefault;
end;

destructor TJvgCheckBox.Destroy;
begin
  FGlyphOn.Free;
  FGlyphOff.Free;
  FGlyph.Free;
  FGlyphDisabled.Free;
  FImg.Free;
  FTextStyles.Free;
  FColors.Free;
  FGradient.Free;
  FIllumination.Free;
  FGlyphShift.Free;
  SetFocusControl(nil);
  inherited Destroy;
end;

{$IFDEF USEJVCL}

procedure TJvgCheckBox.FontChanged;
begin
  if not FSuppressCMFontChanged then
  begin
    FImg.Canvas.Font.Assign(Font);
    Invalidate;
    inherited FontChanged;
  end;
end;

procedure TJvgCheckBox.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not Enabled or (fcoIgnoreMouse in Options) or
    FShowAsActiveWhileControlFocused then
    Exit;
  if Assigned(FocusControl) and (FocusControlMethod = fcmOnMouseEnter) then
    FocusControl.SetFocus;
  FNeedRebuildBackground := True;
  FActiveNow := True;
  with TextStyles, Colors do
    if (Passive <> Active) or (fcoUnderlinedActive in Options) then
      Repaint
    else
    if (fcoDelineatedText in Options) and (DelineateActive <> Delineate) then
      Repaint
    else
    if (not Transparent) and (Colors.Background <>
      Colors.BackgroundActive) then
      Repaint
    else
    if (TextActive <> Text) or (fcoUnderlinedActive in Options) then
    begin
      FNeedUpdateOnlyMainText := True;
      Repaint;
    end;
  inherited MouseEnter(Control);
end;

procedure TJvgCheckBox.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not Enabled or (fcoIgnoreMouse in Options) or
    FShowAsActiveWhileControlFocused then
    Exit;
  FNeedRebuildBackground := True;
  FActiveNow := False;
  with TextStyles, Colors do
    if (Passive <> Active) or (fcoUnderlinedActive in Options) then
      Repaint
    else
    if (fcoDelineatedText in Options) and (DelineateActive <> Delineate) then
      Repaint
    else
    if (not Transparent) and (Colors.Background <>
      Colors.BackgroundActive) then
      Repaint
    else
    if TextActive <> Text then
    begin
      FNeedUpdateOnlyMainText := True;
      Repaint;
    end;
  inherited MouseLeave(Control);
end;

procedure TJvgCheckBox.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

{$ENDIF USEJVCL}

procedure TJvgCheckBox.WMLButtonUp(var Msg: TMessage);
var
  pt: TPoint;
begin
  if not Enabled or (fcoIgnoreMouse in Options) then
    Exit;
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  if PtInRect(ClientRect, pt) then
    SetChecked(not FChecked);
  if Assigned(FocusControl) then
  begin
    if fcoEnabledFocusControlWhileChecked in Options then
      FocusControl.Enabled := FChecked;
    if (FocusControlMethod = fcmOnMouseUp) and FocusControl.CanFocus then
      FocusControl.SetFocus;
  end;
  inherited;
end;

procedure TJvgCheckBox.WMLButtonDown(var Msg: TMessage);
begin
  if not Enabled or (fcoIgnoreMouse in Options) then
    Exit;
  inherited;
  if (FocusControlMethod = fcmOnMouseDown) and Assigned(FocusControl) and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;

procedure TJvgCheckBox.Resize;
begin
  inherited Resize;
  //  Img.Width := Width; Img.Height := Height;
end;

procedure TJvgCheckBox.Paint;
var
  X, Y: Integer;
  DrawState: TglDrawState;
  Bitmap: TBitmap;
  FontColor: TColor;
  CurrTextStyle: TglTextStyle;
  CurrDelinColor: TColor;
  isGradientActive: Boolean;
  Size: TSize;
  R: TRect;
  BackBrush: HBRUSH;
begin
  //FNeedUpdateOnlyMainText := False;
  //FNeedRebuildBackground := False;
  FSuppressCMFontChanged := True;
  if fcoBoldChecked in Options then
    if Checked then
      Font.Style := Font.Style + [fsBold]
    else
      Font.Style := Font.Style - [fsBold];
  if Enabled then
  begin
    if Checked then
      Bitmap := FGlyphOn
    else
      Bitmap := FGlyphOff;
    DrawState := fdsDefault;
  end
  else
  begin
    if FGlyphDisabled.Handle <> 0 then
    begin
      Bitmap := FGlyphDisabled;
      DrawState := fdsDefault;
    end
    else
    begin
      if Checked then
        Bitmap := FGlyphOn
      else
        Bitmap := FGlyphOff;
      DrawState := fdsDefault;
    end;
  end;

  //...CAPTION
  SetBkMode(Canvas.Handle, Integer(Transparent));
  with TextStyles, Colors do
  begin
    if FActiveNow then
    begin
      CurrTextStyle := Active;
      CurrDelinColor := DelineateActive;
      FontColor := TextActive;
    end
    else
    if Enabled then
    begin
      CurrTextStyle := Passive;
      CurrDelinColor := Delineate;
      FontColor := Text;
    end
    else
    begin
      CurrTextStyle := Disabled;
      CurrDelinColor := Delineate;
      FontColor := TextDisabled;
    end;

    if fcoUnderlinedActive in Options then
      if FActiveNow then
        Font.Style := Font.Style + [fsUnderline]
      else
        Font.Style := Font.Style - [fsUnderline];
  end;
  GetTextExtentPoint32(FImg.Canvas.Handle, PChar(Caption),
    length(Caption), Size);
  Y := Max(0, (Height - Size.cy) div 2);
  X := 0;
  if Assigned(FGlyphOn) then
    X := Max(X, FGlyphOn.Width);
  if Assigned(FGlyphOff) then
    X := Max(X, FGlyphOff.Width);
  if Assigned(FGlyphDisabled) then
    X := Max(X, FGlyphDisabled.Width);
  if Assigned(FGlyph) then
    X := Max(X, FGlyph.Width);

  FImg.Width := Width;
  FImg.Height := Height;

  if (not FNeedUpdateOnlyMainText) {and (not Transparent)} then
  begin
    R := GetClientRect;
    if FActiveNow then
      BackBrush := CreateSolidBrush(ColorToRGB(Colors.BackgroundActive))
    else
      BackBrush := CreateSolidBrush(ColorToRGB(Colors.Background));
    FillRect(FImg.Canvas.Handle, R, BackBrush);
    DeleteObject(BackBrush);
  end;

  if FTransparent and (not FNeedUpdateOnlyMainText) then
    if (not (fcoFastDraw in Options)) or FNeedRebuildBackground or (csDesigning
      in ComponentState) then
      GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
        FImg.Canvas.Handle);

  if Alignment = taLeftJustify then
  begin
    X := 0;
    if FGlyph <> nil then
      Inc(X, FGlyph.Width);
  end
  else
    Inc(X, Interspace);

  //...Supress Gradient if needed
  isGradientActive := Gradient.Active;
  if FActiveNow and (Colors.TextActive <> Colors.Text) then
    Gradient.Active := False;

  ExtTextOutExt(FImg.Canvas.Handle, X, Y, GetClientRect, Caption,
    CurrTextStyle, fcoDelineatedText in Options,
    FNeedUpdateOnlyMainText, FontColor, CurrDelinColor,
    Colors.Highlight, Colors.Shadow,
    Illumination, Gradient, Font);

  Gradient.Active := isGradientActive;

  if not FNeedUpdateOnlyMainText then
  begin
    if (not (fcoFastDraw in Options)) or FNeedRebuildBackground or (csDesigning
      in ComponentState) then
    begin
      if FGlyph <> nil then //...TransparentColor -> Left Bottom Pixel
      begin
        if not Transparent then
          ChangeBitmapColor(FGlyph, GetPixel(FGlyph.Canvas.Handle, 0,
            FGlyph.Height - 1), clBtnFace);

        // glyph always left
        CreateBitmapExt(FImg.Canvas.Handle, FGlyph, ClientRect, 0,
          Max(0, (Height - FGlyph.Height) div 2),
          fwoNone, DrawState, Transparent,
          GetPixel(FGlyph.Canvas.Handle, 0, FGlyph.Height - 1)
          {TransparentColor},
          DisabledMaskColor);
      end;
      FNeedRebuildBackground := False;
    end;
    if not Transparent then
      if FActiveNow then
        ChangeBitmapColor(Bitmap, GetPixel(Bitmap.Canvas.Handle, 0,
          Bitmap.Height - 1), Colors.BackgroundActive)
      else
        ChangeBitmapColor(Bitmap, GetPixel(Bitmap.Canvas.Handle, 0,
          Bitmap.Height - 1), Colors.Background);

    if Alignment = taRightJustify then
      X := GlyphShift.X
    else
      X := Width - Bitmap.Width;

    if Assigned(Bitmap) then
      CreateBitmapExt(FImg.Canvas.Handle, Bitmap, ClientRect, X,
        Integer(GlyphShift.Y + Max(0, (Height - Bitmap.Height) div 2)),
        fwoNone, DrawState, Transparent,
        GetPixel(Bitmap.Canvas.Handle, 0, Bitmap.Height - 1),
        DisabledMaskColor);
  end;

{  BitBlt(Canvas.Handle, 0, 0, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0,
    SRCCOPY);}
  FImg.Transparent := True;
  FImg.TransparentMode := tmAuto;
  Canvas.Draw(0, 0, FImg);

  FSuppressCMFontChanged := False;
  FOnlyTextStyleChanged := False;
  FNeedUpdateOnlyMainText := False;
  if Assigned(FAfterPaint) then
    FAfterPaint(Self);
end;

procedure TJvgCheckBox.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FocusControl) and (Operation = opRemove) then
  begin {UnhookFocusControlWndProc;}
    FFocusControl := nil;
  end;
end;

procedure TJvgCheckBox.HookFocusControlWndProc;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    FNewWndProc := JvMakeObjectInstance(FocusControlWndHookProc);
    SetWindowLong(FocusControl.Handle, GWL_WNDPROC, Longint(FNewWndProc));
  end;
end;

procedure TJvgCheckBox.UnhookFocusControlWndProc;
begin
  //  if not(csDesigning in ComponentState) then Exit;
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil) and
    (Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC)) = FNewWndProc) then
  begin
    SetWindowLong(FocusControl.Handle, GWL_WNDPROC, Longint(FPrevWndProc));
    // (rom) JvFreeObjectInstance call added
    JvFreeObjectInstance(FNewWndProc);
    FNewWndProc := nil;
  end;
end;

procedure TJvgCheckBox.FocusControlWndHookProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_SETFOCUS:
      begin
        {$IFDEF USEJVCL}
        MouseEnter(Self);
        {$ENDIF USEJVCL}
        FShowAsActiveWhileControlFocused := True;
      end;
    WM_KILLFOCUS:
      begin
        FShowAsActiveWhileControlFocused := False;
        {$IFDEF USEJVCL}
        MouseLeave(Self);
        {$ENDIF USEJVCL}
      end;
    WM_DESTROY: {fNeedRehookFocusControl := True};
  end;
  with Msg do
    Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam,
      LParam);
end;

procedure TJvgCheckBox.SetFocusControl(Value: TWinControl);
begin
  if FFocusControl = Value then
    Exit;
  if (fcoActiveWhileControlFocused in Options) and Assigned(FFocusControl) then
    UnhookFocusControlWndProc;
  FFocusControl := Value;
  if (fcoActiveWhileControlFocused in Options) and Assigned(FFocusControl) then
    HookFocusControlWndProc;
end;

procedure TJvgCheckBox.OnGradientChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    FNeedUpdateOnlyMainText := True;
  Repaint;
end;

procedure TJvgCheckBox.OnIlluminationChanged(Sender: TObject);
begin
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  Repaint;
end;

function TJvgCheckBox.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = fgkCustom;
end;

function TJvgCheckBox.GetCheckedItemInGroup: TJvgCheckBox;
var
  I: Integer;
begin
  if FChecked then
  begin
    Result := Self;
    Exit;
  end;
  Result := nil;
  if GroupIndex <> 0 then
  begin
    for I := 0 to Owner.ComponentCount - 1 do
      if (Owner.Components[I] is TJvgCheckBox) and
        (TJvgCheckBox(Owner.Components[I]).GroupIndex = GroupIndex) and
        (TJvgCheckBox(Owner.Components[I]).Checked) then
      begin
        Result := TJvgCheckBox(Owner.Components[I]);
        Break;
      end;
  end;
end;

procedure TJvgCheckBox.SetCheckedItemInGroup(TagNo: Integer);
var
  I: Integer;
begin
  if GroupIndex <> 0 then
  begin
    for I := 0 to Owner.ComponentCount - 1 do
      if (Owner.Components[I] is TJvgCheckBox) and
        (TJvgCheckBox(Owner.Components[I]).GroupIndex = GroupIndex) and
        (TJvgCheckBox(Owner.Components[I]).Tag = TagNo) then
      begin
        TJvgCheckBox(Owner.Components[I]).Checked := True;
        Break;
      end;
  end;
end;
//...______________________________________________PROPERTIES METHODS

procedure TJvgCheckBox.SetChecked(Value: Boolean);
var
  I: Integer;
begin
  if FChecked = Value then
    Exit;
  FNeedRebuildBackground := True;
  if GroupIndex <> 0 then
  begin
    if not FChecked then
    begin
      for I := 0 to Owner.ComponentCount - 1 do
        if (Owner.Components[I] is TJvgCheckBox) and
          (TJvgCheckBox(Owner.Components[I]).GroupIndex = GroupIndex) and
          (TJvgCheckBox(Owner.Components[I]).Checked) and
          (Owner.Components[I] <> Self) then
        begin
          TJvgCheckBox(Owner.Components[I]).FChecked := False;
          TJvgCheckBox(Owner.Components[I]).FNeedRebuildBackground := True;
          TJvgCheckBox(Owner.Components[I]).Invalidate;
        end;
      FChecked := True;
    end;
  end
  else
    FChecked := Value;
  Invalidate;
end;

procedure TJvgCheckBox.SetGlyph(Value: TBitmap);
begin
  if Assigned(FGlyph) then
    FGlyph.Free;
  FGlyph := TBitmap.Create;
  FGlyph.Assign(Value);
  FNeedRebuildBackground := True;
  Invalidate;
end;

function TJvgCheckBox.GetGlyph: TBitmap;
begin
  if not Assigned(FGlyph) then
    FGlyph := TBitmap.Create;
  Result := FGlyph;
end;

procedure TJvgCheckBox.SetGlyphOn(Value: TBitmap);
begin
  if Assigned(FGlyphOn) then
    FGlyphOn.Free;
  FGlyphOn := TBitmap.Create;
  FGlyphKind := fgkCustom;
  FGlyphOn.Assign(Value);
  Invalidate;
end;

function TJvgCheckBox.GetGlyphOn: TBitmap;
begin
  if not Assigned(FGlyphOn) then
    FGlyphOn := TBitmap.Create;
  Result := FGlyphOn;
end;

procedure TJvgCheckBox.SetGlyphOff(Value: TBitmap);
begin
  if Assigned(FGlyphOff) then
    FGlyphOff.Free;
  FGlyphOff := TBitmap.Create;
  FGlyphKind := fgkCustom;
  FGlyphOff.Assign(Value);
  Invalidate;
end;

function TJvgCheckBox.GetGlyphOff: TBitmap;
begin
  if not Assigned(FGlyphOff) then
    FGlyphOff := TBitmap.Create;
  Result := FGlyphOff;
end;

procedure TJvgCheckBox.SetGlyphDisabled(Value: TBitmap);
begin
  if Assigned(FGlyphDisabled) then
    FGlyphDisabled.Free;
  FGlyphDisabled := TBitmap.Create;
  FGlyphDisabled.Assign(Value);
  Invalidate;
end;

function TJvgCheckBox.GetGlyphDisabled: TBitmap;
begin
  if not Assigned(FGlyphDisabled) then
    FGlyphDisabled := TBitmap.Create;
  Result := FGlyphDisabled;
end;

procedure TJvgCheckBox.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    if FChecked and (Value <> 0) then
    begin
      FChecked := False;
      //    SetChecked( True );
      FChecked := True;
    end;
  end;
end;

procedure TJvgCheckBox.SetOptions(Value: TglCheckBoxOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
    Invalidate;
  end;
end;

procedure TJvgCheckBox.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    {$IFDEF USEJVCL}
    if FTransparent then
      ExcludeThemeStyle(Self, [csParentBackground])
    else
      IncludeThemeStyle(Self, [csParentBackground]);
    {$ENDIF USEJVCL}
    Repaint;
  end;
end;

procedure TJvgCheckBox.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor <> Value then
  begin
    FDisabledMaskColor := Value;
    FNeedRebuildBackground := True;
    Invalidate;
  end;
end;

procedure TJvgCheckBox.SetInterspace(Value: Integer);
begin
  if FInterspace <> Value then
  begin
    FInterspace := Value;
    FNeedRebuildBackground := True;
    Invalidate;
  end;
end;

procedure TJvgCheckBox.SetGlyphKind(Value: TglGlyphKind);
begin
  FGlyphKind := Value;

  if (FGlyphKind = fgkCustom) and (csReading in ComponentState) then
  begin
    GlyphOn := nil;
    GlyphOff := nil;
    GlyphDisabled := nil;
  end
  else
    //if (csDesigning in ComponentState){and not(csLoading in ComponentState)}then
  begin
    if not Assigned(FGlyphOn) then
      FGlyphOn := TBitmap.Create;
    if not Assigned(FGlyphOff) then
      FGlyphOff := TBitmap.Create;
    if not Assigned(FGlyphDisabled) then
      FGlyphDisabled := TBitmap.Create;
    FGlyphOn.LoadFromResourceName(hInstance, 'ON');
    FGlyphOff.LoadFromResourceName(hInstance, 'OFF');
    FGlyphDisabled.LoadFromResourceName(hInstance, 'DISABLED');

    FGlyphOn.Transparent := True;
    FGlyphOn.TransparentMode := tmAuto;
    FGlyphOff.Transparent := True;
    FGlyphOff.TransparentMode := tmAuto;
    FGlyphDisabled.Transparent := True;
    FGlyphDisabled.TransparentMode := tmAuto;
  end;

end;

procedure TJvgCheckBox.SetAlignment(const Value: TLeftRight);
begin
  FAlignment := Value;
  Invalidate;
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
