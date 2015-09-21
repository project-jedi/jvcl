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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgCheckBox;

{$I jvcl.inc}
{$I windowsonly.inc} // (ahuser) uses WndProc and Wnd hooks

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  JvComponent, JvJVCLUtils,
  JvgTypes, JvgCommClasses, JvgUtils;

type
  TJvgCheckBox = class(TJvGraphicControl)
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
    FInitialPainted: Boolean; // set to True after the first call to Paint(), this is only a workaround
    function IsCustomGlyph: Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphOn(Value: TBitmap);
    procedure SetGlyphOff(Value: TBitmap);
    procedure SetGlyphDisabled(Value: TBitmap);
    procedure SetGroupIndex(Value: Integer);
    procedure SetOptions(Value: TglCheckBoxOptions);
    procedure SetTransparent(Value: Boolean);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetInterspace(Value: Integer);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetGlyphKind(Value: TglGlyphKind);

    procedure GradientChanged(Sender: TObject);
    procedure IlluminationChanged(Sender: TObject);
    procedure WMLButtonUp(var Msg: TMessage); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Msg: TMessage); message WM_LBUTTONDOWN;
    procedure SetAlignment(const Value: TLeftRight);
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure FontChanged; override;
    procedure TextChanged; override;
    procedure Paint; override;
    procedure HookFocusControlWndProc;
    procedure UnhookFocusControlWndProc;
    procedure FocusControlWndHookProc(var Msg: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCheckedItemInGroup: TJvgCheckBox;
    procedure SetCheckedItemInGroup(TagNo: Integer);
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
    property GlyphKind: TglGlyphKind read FGlyphKind write SetGlyphKind default fgkDefault; // must be above "GlyphOn/Off/Disabled"
    property Checked: Boolean read FChecked write SetChecked default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphOn: TBitmap read FGlyphOn write SetGlyphOn stored IsCustomGlyph;
    property GlyphOff: TBitmap read FGlyphOff write SetGlyphOff stored IsCustomGlyph;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled stored IsCustomGlyph;
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

    property OnMouseEnter;
    property OnMouseLeave;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
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
  Math,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils, // SetWindowLongPtr
  {$ENDIF ~COMPILER12_UP}
  JvThemes;

{$R JvgCheckBox.res}

constructor TJvgCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle :=
    [csCaptureMouse, csOpaque, csClickEvents, csSetCaption, csReplicatable];
  //  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  IncludeThemeStyle(Self, [csParentBackground]);

  FGlyphOn := TBitmap.Create;
  FGlyphOff := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
  FGlyph := TBitmap.Create;
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
  FGradient.OnChanged := GradientChanged;
  FIllumination.OnChanged := IlluminationChanged;
  TextStyles.OnChanged := IlluminationChanged;
  Colors.OnChanged := IlluminationChanged;
  FGlyphShift.OnChanged := GradientChanged;
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
  if (csDesigning in ComponentState) or not FInitialPainted then
    Exit;
  if not Enabled or (fcoIgnoreMouse in Options) or FShowAsActiveWhileControlFocused then
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
    if (not Transparent) and (Colors.Background <> Colors.BackgroundActive) then
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
  if (csDesigning in ComponentState) or not FInitialPainted then
    Exit;
  if not Enabled or (fcoIgnoreMouse in Options) or FShowAsActiveWhileControlFocused then
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
    if (not Transparent) and (Colors.Background <> Colors.BackgroundActive) then
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
  FInitialPainted := True;

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
  GetTextExtentPoint32(FImg.Canvas.Handle, PChar(Caption), Length(Caption), Size);
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

  if not FNeedUpdateOnlyMainText {and not Transparent} then
  begin
    R := GetClientRect;
    if FActiveNow then
      BackBrush := CreateSolidBrush(ColorToRGB(Colors.BackgroundActive))
    else
      BackBrush := CreateSolidBrush(ColorToRGB(Colors.Background));
    FillRect(FImg.Canvas.Handle, R, BackBrush);
    DeleteObject(BackBrush);
  end;

  if FTransparent and not FNeedUpdateOnlyMainText then
    if not (fcoFastDraw in Options) or FNeedRebuildBackground or
       (csDesigning in ComponentState) then
      GetParentImageRect(Self, Bounds(Left, Top, Width, Height), FImg.Canvas.Handle);

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
          JvgUtils.ChangeBitmapColor(FGlyph, GetPixel(FGlyph.Canvas.Handle, 0, FGlyph.Height - 1),
            clBtnFace);

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
        JvgUtils.ChangeBitmapColor(Bitmap, GetPixel(Bitmap.Canvas.Handle, 0, Bitmap.Height - 1),
          Colors.BackgroundActive)
      else
        JvgUtils.ChangeBitmapColor(Bitmap, GetPixel(Bitmap.Canvas.Handle, 0, Bitmap.Height - 1),
          Colors.Background);

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
  P := Pointer(GetWindowLongPtr(FocusControl.Handle, GWLP_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    FNewWndProc := JvMakeObjectInstance(FocusControlWndHookProc);
    SetWindowLongPtr(FocusControl.Handle, GWLP_WNDPROC, LONG_PTR(FNewWndProc));
  end;
end;

procedure TJvgCheckBox.UnhookFocusControlWndProc;
begin
  //  if not(csDesigning in ComponentState) then Exit;
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil) and
    (Pointer(GetWindowLongPtr(FocusControl.Handle, GWLP_WNDPROC)) = FNewWndProc) then
  begin
    SetWindowLongPtr(FocusControl.Handle, GWLP_WNDPROC, LONG_PTR(FPrevWndProc));
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
        MouseEnter(Self);
        FShowAsActiveWhileControlFocused := True;
      end;
    WM_KILLFOCUS:
      begin
        FShowAsActiveWhileControlFocused := False;
        MouseLeave(Self);
      end;
    WM_DESTROY: {fNeedRehookFocusControl := True};
  end;
  with Msg do
    Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam, LParam);
end;

procedure TJvgCheckBox.SetFocusControl(Value: TWinControl);
begin
  if FFocusControl <> Value then
  begin
    if (fcoActiveWhileControlFocused in Options) and Assigned(FFocusControl) then
      UnhookFocusControlWndProc;
    ReplaceComponentReference(Self, Value, TComponent(FFocusControl));
    if (fcoActiveWhileControlFocused in Options) and Assigned(FFocusControl) then
      HookFocusControlWndProc;
  end;
end;

procedure TJvgCheckBox.GradientChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    FNeedUpdateOnlyMainText := True;
  Repaint;
end;

procedure TJvgCheckBox.IlluminationChanged(Sender: TObject);
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
    Result := Self
  else
  begin
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
  if FChecked <> Value then
  begin
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
end;

procedure TJvgCheckBox.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  FNeedRebuildBackground := True;
  Invalidate;
end;

procedure TJvgCheckBox.SetGlyphOn(Value: TBitmap);
begin
  FGlyphKind := fgkCustom;
  FGlyphOn.Assign(Value);
  Invalidate;
end;

procedure TJvgCheckBox.SetGlyphOff(Value: TBitmap);
begin
  FGlyphKind := fgkCustom;
  FGlyphOff.Assign(Value);
  Invalidate;
end;

procedure TJvgCheckBox.SetGlyphDisabled(Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
  Invalidate;
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
    if FTransparent then
      ExcludeThemeStyle(Self, [csParentBackground])
    else
      IncludeThemeStyle(Self, [csParentBackground]);
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
  if Value <> FGlyphKind then
  begin
    FGlyphKind := Value;

    if (FGlyphKind = fgkCustom) and (csReading in ComponentState) then
    begin
      FGlyphOn.Assign(nil);
      FGlyphOff.Assign(nil);
      FGlyphDisabled.Assign(nil);
    end
    else
    //if (csDesigning in ComponentState){and not(csLoading in ComponentState)}then
    begin
      FGlyphOn.Assign(nil); // fixes GDI resource leak
      FGlyphOff.Assign(nil); // fixes GDI resource leak
      FGlyphDisabled.Assign(nil); // fixes GDI resource leak
      FGlyphOn.LoadFromResourceName(HInstance, 'JvgON');
      FGlyphOff.LoadFromResourceName(HInstance, 'JvgOFF');
      FGlyphDisabled.LoadFromResourceName(HInstance, 'JvgDISABLED');

      FGlyphOn.Transparent := True;
      FGlyphOn.TransparentMode := tmAuto;
      FGlyphOff.Transparent := True;
      FGlyphOff.TransparentMode := tmAuto;
      FGlyphDisabled.Transparent := True;
      FGlyphDisabled.TransparentMode := tmAuto;
    end;
  end;
end;

procedure TJvgCheckBox.SetAlignment(const Value: TLeftRight);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
