{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCheckBox.PAS, released on 2003-01-15.

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

unit JvgCheckBox;

interface
uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  JvgTypes,
  JvgCommClasses,
  JVComponent,
  JvgUtils;

type

  TJvgCheckBox = class(TJvGraphicControl)
  private
    FChecked: boolean;
    FColors: TJvgLabelColors;
    FIllumination: TJvgIllumination;
    FGlyph: TBitmap;
    FGlyphOn: TBitmap;
    FGlyphOff: TBitmap;
    FGlyphDisabled: TBitmap;
    FGradient: TJvgGradient;
    FGroupIndex: integer;
    FGlyphShift: TJvgPointClass;
    FOptions: TglCBoxOptions;
    FTransparent: boolean;
    FTextStyles: TJvgLabelTextStyles;
    FDisabledMaskColor: TColor;
    FInterspace: integer;
    FFocusControl: TWinControl;
    FFocusControlMethod: TFocusControlMethod;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FAfterPaint: TNotifyEvent;

    FGlyphKind: TglGlyphKind;

    FPrevWndProc: Pointer;
    FNewWndProc: Pointer;
    fActiveNow: boolean;
    fShowAsActiveWhileControlFocused: boolean;
    Img: TBitmap;
    fNeedUpdateOnlyMainText: boolean;
    fSuppressCMFontChanged: boolean;
    fOnlyTextStyleChanged: boolean;
    FAlignment: TLeftRight;
    function IsCustomGlyph: Boolean;
    procedure SetChecked(Value: boolean);
    procedure SetGlyph(Value: TBitmap);
    function GetGlyph: TBitmap;
    procedure SetGlyphOn(Value: TBitmap);
    function GetGlyphOn: TBitmap;
    procedure SetGlyphOff(Value: TBitmap);
    function GetGlyphOff: TBitmap;
    procedure SetGlyphDisabled(Value: TBitmap);
    function GetGlyphDisabled: TBitmap;
    procedure SetGroupIndex(Value: integer);
    procedure SetOptions(Value: TglCBoxOptions);
    procedure SetTransparent(Value: boolean);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetInterspace(Value: integer);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetGlyphKind(Value: TglGlyphKind);

    procedure OnGradientChanged(Sender: TObject);
    procedure OnIlluminationChanged(Sender: TObject);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure OnLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure OnLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CallMouseEnter; virtual;
    procedure CallMouseLeave; virtual;
    procedure SetAlignment(const Value: TLeftRight);
  protected
    procedure Paint; override;
    procedure HookFocusControlWndProc;
    procedure UnhookFocusControlWndProc;
    procedure FocusControlWndHookProc(var Msg_: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    fNeedRebuildBackground: boolean;
    function GetCheckedItemInGroup: TJvgCheckBox;
    procedure SetCheckedItemInGroup(TagNo: integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF COMPILER5_UP}
    property Anchors;
    {$ENDIF}
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

    property Alignment: TLeftRight read FAlignment write SetAlignment default
      taRightJustify;
    property GlyphKind: TglGlyphKind read FGlyphKind write SetGlyphKind default
      fgkDefault;
    property Checked: boolean read FChecked write SetChecked default false;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphOn: TBitmap read GetGlyphOn write SetGlyphOn stored
      IsCustomGlyph;
    property GlyphOff: TBitmap read GetGlyphOff write SetGlyphOff stored
      IsCustomGlyph;
    property GlyphDisabled: TBitmap read GetGlyphDisabled write
      SetGlyphDisabled stored IsCustomGlyph;
    property GroupIndex: integer read FGroupIndex write SetGroupIndex default
      0;
    property GlyphShift: TJvgPointClass read FGlyphShift write FGlyphShift;
    property Transparent: boolean read FTransparent write SetTransparent
      default false;
    property TextStyles: TJvgLabelTextStyles read FTextStyles write
      FTextStyles;
    property Colors: TJvgLabelColors read FColors write FColors;
    property Options: TglCBoxOptions read FOptions write SetOptions;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Illumination: TJvgIllumination read FIllumination write
      FIllumination;
    property DisabledMaskColor: TColor read FDisabledMaskColor write
      SetDisabledMaskColor
      default clBlack;
    property Interspace: integer read FInterspace write SetInterspace default
      0;
    property FocusControl: TWinControl read FFocusControl write
      SetFocusControl;
    property FocusControlMethod: TFocusControlMethod read FFocusControlMethod
      write FFocusControlMethod
      default fcmOnMouseDown;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write
      FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write
      FOnMouseLeave;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;

  end;

implementation
{$R ..\Resources\JvgCheckBox.res}

//________________________________________________________ Methods _

constructor TJvgCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csSetCaption,
    csReplicatable];
  //  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

  //  FGlyphOn := TBitmap.Create;
  //  FGlyphOff := TBitmap.Create;
  Img := TBitmap.Create;
  TextStyles := TJvgLabelTextStyles.Create;
  Colors := TJvgLabelColors.Create;
  Gradient := TJvgGradient.Create;
  FIllumination := TJvgIllumination.Create;
  FGlyphShift := TJvgPointClass.Create;

  //..defaults
  Width := 80;
  Height := 17;
  FAlignment := taRightJustify;
  FChecked := false;
  FTransparent := false;
  FGradient.OnChanged := OnGradientChanged;
  FIllumination.OnChanged := OnIlluminationChanged;
  TextStyles.OnChanged := OnIlluminationChanged;
  Colors.OnChanged := OnIlluminationChanged;
  FGlyphShift.OnChanged := OnGradientChanged;
  FOptions := [fcoFastDraw];
  FGroupIndex := 0;
  FInterspace := 0;
  FFocusControlMethod := fcmOnMouseDown;
  fNeedRebuildBackground := true;

  Img.Canvas.Brush.Color := clBtnFace;
  Img.Canvas.Brush.Style := bsSolid;
  //  fNeedUpdateOnlyMainText := false;
  {$IFDEF FR_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF}
  GlyphKind := fgkDefault;
end;
//______________________________________________________________

destructor TJvgCheckBox.Destroy;
begin
  if Assigned(FGlyphOn) then
    FGlyphOn.Free;
  if Assigned(FGlyphOff) then
    FGlyphOff.Free;
  if Assigned(FGlyph) then
    FGlyph.Free;
  if Assigned(FGlyphDisabled) then
    FGlyphDisabled.Free;
  Img.Free;
  FTextStyles.Free;
  FColors.Free;
  FGradient.Free;
  FIllumination.Free;
  FGlyphShift.Free;
  SetFocusControl(nil);
  inherited;
end;
//______________________________________________________________

procedure TJvgCheckBox.CMFontChanged(var Message: TMessage);
begin
  if fSuppressCMFontChanged then
    exit;
  Img.Canvas.Font.Assign(Font);
  Repaint;
  inherited;
end;
//______________________________________________________________

procedure TJvgCheckBox.CMMouseEnter(var Message: TMessage);
begin
  if not Enabled or (fcoIgnoreMouse in Options) or
    fShowAsActiveWhileControlFocused then
    exit;
  if Assigned(FocusControl) and (FocusControlMethod = fcmOnMouseEnter) then
    FocusControl.SetFocus;
  fNeedRebuildBackground := true;
  fActiveNow := true;
  with TextStyles, Colors do
    if (Passive <> Active) or (fcoUnderlinedActive in Options) then
      Paint
    else if (fcoDelineatedText in Options) and (DelineateActive <> Delineate) then
      Paint
    else if (not Transparent) and (Colors.Background <>
      Colors.BackgroundActive) then
      Paint
    else if (TextActive <> Text) or (fcoUnderlinedActive in Options) then
    begin
      fNeedUpdateOnlyMainText := true;
      Paint;
    end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;
//______________________________________________________________

procedure TJvgCheckBox.CMMouseLeave(var Message: TMessage);
begin
  if not Enabled or (fcoIgnoreMouse in Options) or
    fShowAsActiveWhileControlFocused then
    exit;
  fNeedRebuildBackground := true;
  fActiveNow := false;
  with TextStyles, Colors do
    if (Passive <> Active) or (fcoUnderlinedActive in Options) then
      Paint
    else if (fcoDelineatedText in Options) and (DelineateActive <> Delineate) then
      Paint
    else if (not Transparent) and (Colors.Background <>
      Colors.BackgroundActive) then
      Paint
    else if TextActive <> Text then
    begin
      fNeedUpdateOnlyMainText := true;
      Paint;
    end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;
//______________________________________________________________

procedure TJvgCheckBox.OnLButtonUp(var Message: TMessage);
var
  pt: TPoint;
begin
  if not Enabled or (fcoIgnoreMouse in Options) then
    exit;
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  if PtInRect(ClientRect, pt) then
    SetChecked(not FChecked);
  if Assigned(FocusControl) then
  begin
    if fcoEnabledFocusControlWhileChecked in Options then
      FocusControl.Enabled := FChecked;
    if (FocusControlMethod = fcmOnMouseUp)
      and FocusControl.CanFocus then
      FocusControl.SetFocus;
  end;
  inherited;
end;
//______________________________________________________________

procedure TJvgCheckBox.OnLButtonDown(var Message: TMessage);
begin
  if not Enabled or (fcoIgnoreMouse in Options) then
    exit;
  inherited;
  if (FocusControlMethod = fcmOnMouseDown)
    and Assigned(FocusControl)
    and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;
//______________________________________________________________

procedure TJvgCheckBox.WMSize(var Message: TMessage);
begin
  inherited;
  //  Img.Width := Width; Img.Height := Height;
end;
//______________________________________________________________

procedure TJvgCheckBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;
//______________________________________________________________

procedure TJvgCheckBox.Paint;
var
  x, y: integer;
  DrawState: TglDrawState;
  Bitmap: TBitmap;
  FontColor: TColor;
  CurrTextStyle: TglTextStyle;
  CurrDelinColor: TColor;
  isGradientActive: boolean;
  Size: TSize;
  R: TRect;
  BackBrush: HBRUSH;
begin
  //fNeedUpdateOnlyMainText := false;
  //fNeedRebuildBackground := false;
  fSuppressCMFontChanged := true;
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
  SetBkMode(Canvas.handle, integer(Transparent));
  with TextStyles, Colors do
  begin
    if fActiveNow then
    begin
      CurrTextStyle := Active;
      CurrDelinColor := DelineateActive;
      FontColor := TextActive;
    end
    else if Enabled then
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
      if fActiveNow then
        Font.Style := Font.Style + [fsUnderline]
      else
        Font.Style := Font.Style - [fsUnderline];
  end;
  GetTextExtentPoint32(Img.Canvas.Handle, PChar(Caption),
    length(Caption), Size);
  y := max(0, (Height - Size.cy) div 2);
  X := 0;
  if Assigned(FGlyphOn) then
    X := max(X, FGlyphOn.Width);
  if Assigned(FGlyphOff) then
    X := max(X, FGlyphOff.Width);
  if Assigned(FGlyphDisabled) then
    X := max(X, FGlyphDisabled.Width);
  if Assigned(FGlyph) then
    X := max(X, FGlyph.Width);

  Img.Width := Width;
  Img.Height := Height;

  if (not fNeedUpdateOnlyMainText) {and (not Transparent)} then
  begin
    R := GetClientRect;
    if fActiveNow then
      BackBrush := CreateSolidBrush(ColorToRGB(Colors.BackgroundActive))
    else
      BackBrush := CreateSolidBrush(ColorToRGB(Colors.Background));
    FillRect(Img.Canvas.Handle, R, BackBrush);
    DeleteObject(BackBrush);
  end;

  if FTransparent and (not fNeedUpdateOnlyMainText) then
    if (not (fcoFastDraw in Options)) or fNeedRebuildBackground or (csDesigning
      in ComponentState) then
      GetParentImageRect(self, Bounds(Left, Top, Width, Height),
        Img.Canvas.Handle);

  if Alignment = taLeftJustify then
  begin
    X := 0;
    if FGlyph <> nil then
      inc(X, FGlyph.Width);
  end
  else
    inc(X, Interspace);

  //...Supress Gradient if needed
  isGradientActive := Gradient.Active;
  if fActiveNow and (Colors.TextActive <> Colors.Text) then
    Gradient.FActive := false;

  ExtTextOutExt(Img.Canvas.Handle, X, y, GetClientRect, Caption,
    CurrTextStyle, fcoDelineatedText in Options,
    fNeedUpdateOnlyMainText, FontColor, CurrDelinColor,
    Colors.Highlight, Colors.Shadow,
    Illumination, Gradient, Font);

  Gradient.FActive := isGradientActive;

  if not fNeedUpdateOnlyMainText then
  begin
    if (not (fcoFastDraw in Options)) or fNeedRebuildBackground or (csDesigning
      in ComponentState) then
    begin
      if FGlyph <> nil then //...TransparentColor -> Left Bottom Pixel
      begin
        if not Transparent then
          ChangeBitmapColor(FGlyph, GetPixel(FGlyph.Canvas.Handle, 0,
            FGlyph.Height - 1), clBtnFace);

        // glyph always left
        CreateBitmapExt(Img.Canvas.Handle, FGlyph, ClientRect, 0, max(0,
          (Height - FGlyph.Height) div 2),
          fwoNone, DrawState, Transparent,
          GetPixel(FGlyph.Canvas.Handle, 0, FGlyph.Height - 1)
          {TransparentColor},
          DisabledMaskColor);
      end;
      fNeedRebuildBackground := false;
    end;
    if not Transparent then
      if fActiveNow then
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
      CreateBitmapExt(Img.Canvas.Handle, Bitmap, ClientRect, X,
        integer(GlyphShift.Y + max(0, (Height - Bitmap.Height) div 2)),
        fwoNone, DrawState, Transparent,
        GetPixel(Bitmap.Canvas.Handle, 0, Bitmap.Height - 1),
        DisabledMaskColor);
  end;

  BitBlt(Canvas.Handle, 0, 0, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0,
    SRCCOPY);

  fSuppressCMFontChanged := false;
  fOnlyTextStyleChanged := false;
  fNeedUpdateOnlyMainText := false;
  if Assigned(FAfterPaint) then
    FAfterPaint(self);
end;
//______________________________________________________________

procedure TJvgCheckBox.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FocusControl) and (Operation = opRemove) then
  begin {UnhookFocusControlWndProc;}
    FFocusControl := nil;
  end;
end;
//______

procedure TJvgCheckBox.CallMouseEnter;
var
  EmptyMsg: TMessage;
begin
  CMMouseEnter(EmptyMsg);
end;
//______

procedure TJvgCheckBox.CallMouseLeave;
var
  EmptyMsg: TMessage;
begin
  CMMouseLeave(EmptyMsg);
end;
//______________________________________________________________

procedure TJvgCheckBox.HookFocusControlWndProc;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    FNewWndProc := MakeObjectInstance(FocusControlWndHookProc);
    SetWindowLong(FocusControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
  end;
end;
//______

procedure TJvgCheckBox.UnhookFocusControlWndProc;
begin
  //  if not(csDesigning in ComponentState) then exit;
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil)
    and (Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC)) =
    FNewWndProc) then
  begin
    SetWindowLong(FocusControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
    FNewWndProc := nil;
  end;
end;
//______

procedure TJvgCheckBox.FocusControlWndHookProc(var Msg_: TMessage);
begin
  case Msg_.Msg of
    WM_SETFOCUS:
      begin
        CallMouseEnter;
        fShowAsActiveWhileControlFocused := true;
      end;
    WM_KILLFOCUS:
      begin
        fShowAsActiveWhileControlFocused := false;
        CallMouseLeave;
      end;
    WM_DESTROY: {fNeedRehookFocusControl := true};
  end;
  with Msg_ do
    Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam,
      LParam);
end;
//______

procedure TJvgCheckBox.SetFocusControl(Value: TWinControl);
begin
  if FFocusControl = Value then
    exit;
  if (fcoActiveWhileControlFocused in Options) and Assigned(FFocusControl) then
    UnhookFocusControlWndProc;
  FFocusControl := Value;
  if (fcoActiveWhileControlFocused in Options) and Assigned(FFocusControl) then
    HookFocusControlWndProc;
end;
//______

procedure TJvgCheckBox.OnGradientChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    fNeedUpdateOnlyMainText := true;
  Repaint;
end;
//______________________________________________________________

procedure TJvgCheckBox.OnIlluminationChanged(Sender: TObject);
begin
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  Repaint;
end;
//______

function TJvgCheckBox.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = fgkCustom;
end;

function TJvgCheckBox.GetCheckedItemInGroup: TJvgCheckBox;
var
  i: integer;
begin
  if FChecked then
  begin
    Result := self;
    exit;
  end;
  Result := nil;
  if GroupIndex <> 0 then
  begin
    for i := 0 to Owner.ComponentCount - 1 do
      if (OWner.Components[i] is TJvgCheckBox)
        and (TJvgCheckBox(OWner.Components[i]).GroupIndex = GroupIndex)
        and (TJvgCheckBox(OWner.Components[i]).Checked) then
      begin
        Result := TJvgCheckBox(OWner.Components[i]);
        break;
      end;
  end;
end;

procedure TJvgCheckBox.SetCheckedItemInGroup(TagNo: integer);
var
  i: integer;
begin
  if GroupIndex <> 0 then
  begin
    for i := 0 to Owner.ComponentCount - 1 do
      if (OWner.Components[i] is TJvgCheckBox)
        and (TJvgCheckBox(OWner.Components[i]).GroupIndex = GroupIndex)
        and (TJvgCheckBox(OWner.Components[i]).Tag = TagNo) then
      begin
        TJvgCheckBox(OWner.Components[i]).Checked := true;
        break;
      end;
  end;
end;
//...______________________________________________PROPERTIES METHODS

procedure TJvgCheckBox.SetChecked(Value: boolean);
var
  i: integer;
begin
  if FChecked = Value then
    exit;
  fNeedRebuildBackground := true;
  if GroupIndex <> 0 then
  begin
    if not FChecked then
    begin
      for i := 0 to Owner.ComponentCount - 1 do
        if (OWner.Components[i] is TJvgCheckBox)
          and (TJvgCheckBox(OWner.Components[i]).GroupIndex = GroupIndex)
          and (TJvgCheckBox(OWner.Components[i]).Checked)
          and (OWner.Components[i] <> Self) then
        begin
          TJvgCheckBox(OWner.Components[i]).FChecked := false;
          TJvgCheckBox(OWner.Components[i]).fNeedRebuildBackground := true;
          TJvgCheckBox(OWner.Components[i]).Repaint;
        end;
      FChecked := true;
    end;
  end
  else
    FChecked := Value;
  Repaint;
end;

procedure TJvgCheckBox.SetGlyph(Value: TBitmap);
begin
  if Assigned(FGlyph) then
    FGlyph.Free;
  FGlyph := TBitmap.Create;
  FGlyph.Assign(Value);
  fNeedRebuildBackground := true;
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

procedure TJvgCheckBox.SetGroupIndex(Value: integer);
begin
  if FGroupIndex = Value then
    exit;
  FGroupIndex := Value;
  if FChecked and (Value <> 0) then
  begin
    FChecked := false;
    //    SetChecked( true );
    FChecked := true;
  end;
end;

procedure TJvgCheckBox.SetOptions(Value: TglCBoxOptions);
begin
  if FOptions = Value then
    exit;
  FOptions := Value;
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  Invalidate;
end;

procedure TJvgCheckBox.SetTransparent(Value: boolean);
begin
  if FTransparent = Value then
    exit;
  FTransparent := Value;
  Repaint;
end;

procedure TJvgCheckBox.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor = Value then
    exit;
  FDisabledMaskColor := Value;
  fNeedRebuildBackground := true;
  Invalidate;
end;

procedure TJvgCheckBox.SetInterspace(Value: integer);
begin
  if FInterspace = Value then
    exit;
  FInterspace := Value;
  fNeedRebuildBackground := true;
  Invalidate;
end;

procedure TJvgCheckBox.SetGlyphKind(Value: TglGlyphKind);
begin
  if FGlyphKind <> Value then
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
  end;

end;

procedure TJvgCheckBox.SetAlignment(const Value: TLeftRight);
begin
  FAlignment := Value;
  Invalidate;
end;

end.
