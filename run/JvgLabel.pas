{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLabel.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc} // (ahuser) uses WndProc and Wnd hooks

unit JvgLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  JvgTypes, JvComponent, JvgCommClasses, JvgUtils;

const
  FTextAlign = DT_LEFT or DT_SINGLELINE;
  RadianEscapments: array[TgllabelDir] of Integer = (0, -1800, -900, 900);

type
  TJvgCustomLabel = class(TJvGraphicControl)
  private
    FAutoSize: Boolean;
    FFocusControl: TWinControl;
    FFocusControlMethod: TFocusControlMethod;
    FTransparent: Boolean;
    FPrevWndProc: Pointer;
    FNewWndProc: Pointer;
    procedure SetFocusControl(Value: TWinControl);
    procedure SetTransparent(Value: Boolean);
    procedure WMLMouseUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure WMLMouseDown(var Message: TMessage); message WM_LBUTTONDOWN;
  protected
    FActiveNow: Boolean;
    FShowAsActiveWhileControlFocused: Boolean;
    ActiveWhileControlFocused: Boolean;
    FNeedRehookFocusControl: Boolean;
    FExternalCanvas: TCanvas;
    procedure HookFocusControlWndProc;
    procedure UnhookFocusControlWndProc;
    procedure FocusControlWndHookProc(var Msg_: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseEnter(Control: TControl); override;
    procedure TextChanged; override;

    property AutoSize: Boolean read FAutoSize write FAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property FocusControlMethod: TFocusControlMethod read FFocusControlMethod
      write FFocusControlMethod default fcmOnMouseDown;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property ExternalCanvas: TCanvas read FExternalCanvas write FExternalCanvas;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvgLabel = class(TJvgCustomLabel)
  private
    FDirection: TglLabelDir;
    FTextStyles: TJvgLabelTextStyles;
    FColors: TJvgLabelColors;
    FFontWeight: TFontWeight;
    // FActiveTextColor: TColor;
    FOptions: TglLabelOptions;
    FSupressPaint: Boolean;
    FGradient: TJvgGradient;
    FIllumination: TJvgIllumination;
    FTexture: TBitmap;
    FBackground: TBitmap;
    FTextureImage: TImage;
    FBackgroundImage: TImage;
    FAlignment: TAlignment;
    uFontWeight: word;
    fRunOnce: Boolean;
    bFirstCreate: Boolean;
    FNeedUpdateOnlyMainText: Boolean;
    fNeedRemakeTextureMask: Boolean;
    Img: TBitmap;
    TextureMask: TBitmap;
    BackgroundBmp: TBitmap;
    TextureBmp: TBitmap;
    TargetCanvas: TCanvas;

    procedure SetDirection(Value: TglLabelDir);
    procedure SetFontWeight(Value: TFontWeight);
    procedure SetOptions(Value: TglLabelOptions);
    procedure SetTexture(Value: TBitmap);
    procedure SetBackground(Value: TBitmap);
    function GetTexture: TBitmap;
    function GetBackground: TBitmap;
    procedure SetTextureImage(Value: TImage);
    procedure SetBackgroundImage(Value: TImage);
    procedure SetAlignment(Value: TAlignment);

    procedure OnGradientChanged(Sender: TObject);
    procedure OnIlluminationChanged(Sender: TObject);
    procedure CreateLabelFont;
    procedure InvalidateLabel(UpdateBackgr: Boolean);
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure FontChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    FreeFont: TFont;
    property Canvas;
    property ExternalCanvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property SupressPaint: Boolean read FSupressPaint write FSupressPaint;
  published
    property Anchors;
    property Align;
    property Caption;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    //    property ShowAccelChar;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property FocusControl;
    property FocusControlMethod;
    property AutoSize;
    property Transparent;

    property Direction: TglLabelDir read FDirection write SetDirection
      default fldLeftRight;
    property TextStyles: TJvgLabelTextStyles read FTextStyles write
      FTextStyles;
    property Colors: TJvgLabelColors read FColors write FColors;
    property FontWeight: TFontWeight read FFontWeight write SetFontWeight;
    property Options: TglLabelOptions read FOptions write SetOptions;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Illumination: TJvgIllumination read FIllumination write
      FIllumination;
    property Texture: TBitmap read GetTexture write SetTexture;
    property Background: TBitmap read GetBackground write SetBackground;
    property TextureImage: TImage read FTextureImage write SetTextureImage;
    property BackgroundImage: TImage read FBackgroundImage write
      SetBackgroundImage;
    property Alignment: TAlignment read FAlignment write SetAlignment;
  end;

  TJvgStaticTextLabel = class(TJvgCustomLabel)
  private
    FActiveColor: TColor;
    FAlignment: TglAlignment;
    FOptions: TglStaticTextOptions;
    FWordWrap: Boolean;

    procedure DrawTextBroadwise(Canvas: TCanvas);
    procedure AdjustBounds;
    procedure SetAlignment(Value: TglAlignment);
    procedure SetOptions(Value: TglStaticTextOptions);
    procedure SetWordWrap(Value: Boolean);
    function GetAutoSize: Boolean;
  protected
    procedure SetAutoSize(Value: Boolean); override; 
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  public
    procedure Paint; override;
    property Canvas;
    property ExternalCanvas;
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property Align;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property FocusControl;
    property FocusControlMethod;
    property Transparent;
    property ActiveColor: TColor read FActiveColor write FActiveColor
      default clWhite;
    property Alignment: TglAlignment read FAlignment write SetAlignment
      default ftaBroadwise;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    property Options: TglStaticTextOptions read FOptions write SetOptions;
    property WordWrap: Boolean read FWordWrap write SetWordWrap
      default True;
  end;

  TJvgGlyphLabel = class(TJvgLabel)
  private
    FGlyphOn: TBitmap;
    FGlyphOff: TBitmap;
    FGlyphDisabled: TBitmap;
    FGlyphKind: TglGlyphKind;

    function IsCustomGlyph: Boolean;
    procedure SetGlyphOn(Value: TBitmap);
    function GetGlyphOn: TBitmap;
    procedure SetGlyphOff(Value: TBitmap);
    function GetGlyphOff: TBitmap;
    procedure SetGlyphDisabled(Value: TBitmap);
    function GetGlyphDisabled: TBitmap;
    procedure SetGlyphKind(Value: TglGlyphKind);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GlyphKind: TglGlyphKind read FGlyphKind write SetGlyphKind default
      fgkDefault;
    property GlyphOn: TBitmap read GetGlyphOn write SetGlyphOn stored True;
    property GlyphOff: TBitmap read GetGlyphOff write SetGlyphOff stored True;
    property GlyphDisabled: TBitmap read GetGlyphDisabled write
      SetGlyphDisabled stored IsCustomGlyph;
  end;

implementation

uses
  Math,
  JvJVCLUtils;

constructor TJvgCustomLabel.Create(AOwner: TComponent);
begin
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  ActiveWhileControlFocused := True;
  FAutoSize := True;
  FTransparent := True;
  FFocusControlMethod := fcmOnMouseDown;
  inherited Create(AOwner);
end;
//______

destructor TJvgCustomLabel.Destroy;
begin
  SetFocusControl(nil);
  inherited Destroy;
end;
//______

procedure TJvgCustomLabel.Paint;
begin //...if FocusControl have changed his parent in Run-Time...
  if FNeedRehookFocusControl then
    HookFocusControlWndProc;
  //don't inherited;
end;

procedure TJvgCustomLabel.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FocusControl) and (Operation = opRemove) then
  begin {UnhookFocusControlWndProc;}
    FFocusControl := nil;
  end;
end;
//______

procedure TJvgCustomLabel.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseEnter(Control);
  if Assigned(FocusControl) and (FocusControlMethod = fcmOnMouseEnter) then
    FocusControl.SetFocus;
end;
//______

procedure TJvgCustomLabel.WMLMouseUp(var Message: TMessage);
begin
  inherited;
  if Enabled and (FocusControlMethod = fcmOnMouseUp)
    and Assigned(FocusControl)
    and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;
//______

procedure TJvgCustomLabel.WMLMouseDown(var Message: TMessage);
begin
  inherited;
  if Enabled and (FocusControlMethod = fcmOnMouseDown)
    and Assigned(FocusControl)
    and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;
//______

procedure TJvgCustomLabel.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;
//______

procedure TJvgCustomLabel.HookFocusControlWndProc;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    FNewWndProc := JvMakeObjectInstance(FocusControlWndHookProc);
    SetWindowLong(FocusControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
  end;
end;
//______

procedure TJvgCustomLabel.UnhookFocusControlWndProc;
begin
  //  if not(csDesigning in ComponentState) then exit;
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil)
    and (Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC)) =
    FNewWndProc) then
  begin
    SetWindowLong(FocusControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
    // (rom) JvFreeObjectInstance call added
    JvFreeObjectInstance(FNewWndProc);
    FNewWndProc := nil;
  end;
end;
//______

procedure TJvgCustomLabel.FocusControlWndHookProc(var Msg_: TMessage);
begin
  case Msg_.Msg of
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
    WM_DESTROY: FNeedRehookFocusControl := True;
  end;
  with Msg_ do
    Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam,
      LParam);
end;
//______

procedure TJvgCustomLabel.SetFocusControl(Value: TWinControl);
begin
  if FFocusControl = Value then
    exit;
  if ActiveWhileControlFocused and Assigned(FFocusControl) then
    UnhookFocusControlWndProc;
  FFocusControl := Value;
  if ActiveWhileControlFocused and Assigned(FFocusControl) then
    HookFocusControlWndProc;
end;
//______

procedure TJvgCustomLabel.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;
//______

//________________________________________________________ TJvgLabel _

constructor TJvgLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TextStyles := TJvgLabelTextStyles.Create;
  Colors := TJvgLabelColors.Create;
  Gradient := TJvgGradient.Create;
  FIllumination := TJvgIllumination.Create;
  Img := TBitmap.Create;

  bFirstCreate := True;
  FreeFont := TFont.Create;
  if csDesigning in ComponentState then
    Self.Font.Name := 'Arial';
  AutoSize := True;
  //  fRunOnce:=False;
  //  FActiveNow := False;

  FDirection := fldLeftRight;
  FFontWeight := fwDONTCARE;
  //  FSupressPaint := False;
  uFontWeight := word(fwDONTCARE);
  //  FNeedUpdateOnlyMainText:=False;
  FGradient.OnChanged := OnGradientChanged;
  FIllumination.OnChanged := OnIlluminationChanged;
  TextStyles.OnChanged := OnIlluminationChanged;
  Colors.OnChanged := OnIlluminationChanged;
  FOptions := [floActiveWhileControlFocused];
  TargetCanvas := Canvas;
  FTransparent := True;
  Width := 100;
  Height := 16;
end;
//______

destructor TJvgLabel.Destroy;
begin
  TextStyles.Free;
  Colors.Free;
  Gradient.Free;
  FIllumination.Free;
  if Assigned(FTexture) then
    FTexture.Free;
  if Assigned(FBackground) then
    FBackground.Free;
  if Assigned(TextureMask) then
    TextureMask.Free;
  Img.Free;
  inherited Destroy;
  DeleteObject(FreeFont.Handle);
  FreeFont.Free;
end;
//______

procedure TJvgLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = BackgroundImage) and (Operation = opRemove) then
    BackgroundImage := nil
  else
  if (AComponent = TextureImage) and (Operation = opRemove) then
    TextureImage := nil;
end;
//______

procedure TJvgLabel.FontChanged;
begin
  inherited FontChanged;
  CreateLabelFont;
  Invalidate;
end;
//______

procedure TJvgLabel.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;

  if not Enabled or (floIgnoreMouse in Options) or
     FShowAsActiveWhileControlFocused then
    Exit;
  //inherited;
  FActiveNow := True;
  with TextStyles, Colors do
    if (Passive <> Active) or ((Background <> BackgroundActive) and not
      Transparent) then
    begin
      if floBufferedDraw in Options then
        Repaint
      else
        InvalidateLabel(True);
    end
    else
    if (floDelineatedText in Options) and (DelineateActive <> Delineate) then
      Repaint
    else
    if TextActive <> Text then
    begin
      FNeedUpdateOnlyMainText := True;
      Repaint;
    end;
  inherited MouseEnter(Control);
end;
//______

procedure TJvgLabel.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not Enabled or (floIgnoreMouse in Options) or
     FShowAsActiveWhileControlFocused then
    Exit;
  //inherited;
  FActiveNow := False;
  with TextStyles, Colors do
    if (Passive <> Active) or ((Background <> BackgroundActive) and not
      Transparent) then
    begin
      if floBufferedDraw in Options then
        Repaint
      else
        InvalidateLabel(True);
    end
    else
    if (floDelineatedText in Options) and (DelineateActive <> Delineate) then
      Repaint
    else
    if TextActive <> Text then
    begin
      FNeedUpdateOnlyMainText := True;
      Repaint;
    end;
  inherited MouseLeave(Control);
end;
//______

procedure TJvgLabel.Loaded;
begin
  inherited;
  if FTexture <> nil then
    TextureBmp := FTexture
  else
  if Assigned(FTextureImage) then
    TextureBmp := FTextureImage.Picture.Bitmap
  else
    TextureBmp := nil;
  if Assigned(FBackground) then
    BackgroundBmp := FBackground
  else
  if Assigned(FBackgroundImage) then
    BackgroundBmp := FBackgroundImage.Picture.Bitmap
  else
    BackgroundBmp := nil;
end;
//______

procedure TJvgLabel.Paint;
var
  R: TRect;
  x, y, x_, y_, Tx, Ty: Integer;
  Size, TextSize: TSIZE;
  FontColor: TColor;
  CurrTextStyle: TglTextStyle;
  {ShadowColor_, HighlightColor_,CurrTextColor,} CurrDelinColor: TColor;
  OldGradientFActive, fUseBackgroundBmp, fUseTextureBmp, fBufferedDraw:
  Boolean;
begin
  inherited;
  if FSupressPaint or (length(Caption) = 0) then
    exit;
  if floTransparentFont in Options then
    fBufferedDraw := True
  else
    fBufferedDraw := (floBufferedDraw in Options) and not (csDesigning in
      ComponentState);
  if fBufferedDraw then
    TargetCanvas := Img.Canvas
  else
  if Assigned(ExternalCanvas) then
    TargetCanvas := ExternalCanvas
  else
    TargetCanvas := Canvas;
  FNeedUpdateOnlyMainText := FNeedUpdateOnlyMainText and not (fBufferedDraw) and
    (not IsItAFilledBitmap(BackgroundBmp));
  if not fRunOnce then
  begin
    FNeedUpdateOnlyMainText := False;
    fRunOnce := True;
  end;
  TargetCanvas.Font := FreeFont;
  //...CALC POSITION
  GetTextExtentPoint32(TargetCanvas.handle, PChar(Caption),
    length(Caption), Size);
  with TextStyles, Colors do
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
  x := 0;
  y := 0;
  Size.cx := Size.cx + 2 + trunc(Size.cx * 0.01);
  //  Size.cy:=Size.cy+trunc(Size.cy*0.1);
  Size.cy := Size.cy + 2;
  TextSize := Size;
  if (CurrTextStyle = fstShadow) or (CurrTextStyle = fstVolumetric) then
  begin
    inc(Size.cy, Illumination.ShadowDepth);
    inc(Size.cx, Illumination.ShadowDepth);
  end;
  if floDelineatedText in Options then
  begin
    inc(Size.cy, 2);
    inc(Size.cx, 2);
  end;

  if (Align = alNone) and AutoSize then
    case FDirection of
      fldLeftRight, fldRightLeft:
        begin
          width := Size.cx;
          height := Size.cy;
        end;
    else {fldDownUp,fldUpDown:}
      begin
        width := Size.cy;
        height := Size.cx;
      end;
    end;

  //  pt := CalcAlignedTextPosition( TargetCanvas.handle, Caption, Size );
  //  x := pt.x; y := pt.y;
  //CalcAlignedTextPosition( TargetCanvas.handle, Caption, Size );

  case FDirection of
    fldLeftRight:
      begin //if Align = alNone then begin width:=max(w,Size.cx);height:=max(h,Size.cy); end;
        case Alignment of
          taCenter: x := (Width - Size.cx) div 2;
          taRightJustify: x := Width - Size.cx;
        end;
      end;
    fldRightLeft:
      begin //if Align = alNone then begin width:=max(w,Size.cx);height:=max(h,Size.cy);x:=width;y:=height; end;
        case Alignment of
          taCenter: x := (Width + Size.cx) div 2;
          taLeftJustify: x := Width - (Size.cx - TextSize.cx) - 2;
        else
          x := TextSize.cx;
        end;
        y := TextSize.cy;
      end;
    fldDownUp:
      begin //if Align = alNone then begin height:=max(h,Size.cx);width:=max(w,Size.cy);y:=height-2; end;
        case Alignment of
          taCenter: y := (Height + TextSize.cx - (Size.cy - TextSize.cy))
            div 2;
          taRightJustify: y := TextSize.cx - 4;
        else
          y := Height - (Size.cy - TextSize.cy) - 2;
        end;
      end;
    fldUpDown:
      begin //if Align = alNone then begin height:=max(h,Size.cx);width:=max(w,Size.cy);x:=width; end;
        case Alignment of
          taCenter: y := (Height - Size.cx) div 2;
          taRightJustify: y := Height - Size.cx;
        else
          y := 1;
        end;
        x := TextSize.cy;
      end;
  end;

  //...CALC POSITION end

  R := GetClientRect;
  if TargetCanvas = Img.Canvas then
  begin
    Img.Width := Width;
    Img.Height := Height;
  end;

  SetBkMode(TargetCanvas.handle, 1 {TRANSPARENT});
  if not Transparent then
  begin
    TargetCanvas.Brush.Style := bsSolid;
    if FActiveNow then
      TargetCanvas.Brush.Color := Colors.BackgroundActive
    else
      TargetCanvas.Brush.Color := Colors.Background;
    TargetCanvas.FillRect(R);
  end;

  try
    fUseBackgroundBmp := IsItAFilledBitmap(BackgroundBmp);
  except
    //  raise;
    fUseBackgroundBmp := False;
    BackgroundBmp := nil;
    FBackgroundImage := nil;
  end;

  try
    fUseTextureBmp := IsItAFilledBitmap(TextureBmp);
  except
    fUseTextureBmp := False;
    TextureBmp := nil;
    FTextureImage := nil;
  end;

  //  ShadowColor_ := Colors.Shadow;
  //  HighlightColor_ := Colors.Highlight;
  if fUseBackgroundBmp then
  begin //...FillBackground
    Tx := 0;
    Ty := 0;
    while Tx < Width do
    begin
      while Ty < Height do
      begin
        BitBlt(TargetCanvas.Handle, Tx, Ty,
          BackgroundBmp.Width, BackgroundBmp.Height,
          BackgroundBmp.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(Ty, BackgroundBmp.Height);
      end;
      Inc(Tx, BackgroundBmp.Width);
      Ty := 0;
    end;
  end
  else
  if fBufferedDraw then
    with TargetCanvas do
    begin
      if Transparent or (floTransparentFont in Options) then
      try
        Brush.Color := Parent.Brush.Color;
        Brush.Style := bsSolid;
        FillRect(R);
        Brush.Style := bsClear;
        GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
          TargetCanvas.Handle);
      except
      end;
    end;

  OldGradientFActive := Gradient.Active;
  //...Supress Gradient if needed
  with Colors do
    if (FActiveNow and (TextActive <> Text)) or not Enabled then
      Gradient.Active := False;
  if floDelineatedText in Options then
  begin
    x_ := 4;
    y_ := 4;
  end
  else
  begin
    x_ := 2;
    y_ := 2;
  end;

  if CurrTextStyle = fstNone then
  begin
    x_ := x_ div 2 - 1;
    y_ := y_ div 2 - 1;
  end;
  if CurrTextStyle = fstShadow then
  begin
    x_ := x_ div 2 - 1;
    y_ := y_ div 2 - 1;
  end;
  if {fNeedRemakeTextureMask and}  fUseTextureBmp or (floTransparentFont in
    Options) then
  begin
    if not Assigned(TextureMask) then
      TextureMask := TBitmap.Create;
    with TextureMask do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Canvas.Brush.Color := clBlack;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(GetClientRect);
      Canvas.Font := FreeFont;
      Canvas.Font.Color := clWhite;
      if (CurrTextStyle = fstNone) or (CurrTextStyle = fstShadow) then
        Canvas.TextOut(x + x_, y + y_, Caption)
      else
        Canvas.TextOut(x + x_ div 2, y + y_ div 2, Caption);
      Tx := 0;
      Ty := 0;

      if not Self.Transparent then
      begin
        BitBlt(Canvas.Handle, Tx, Ty, Width, Height, TargetCanvas.Handle, 0,
          0, SRCAND);
        if FActiveNow then
          ChangeBitmapColor(TextureMask, clBlack, Colors.BackgroundActive)
        else
          ChangeBitmapColor(TextureMask, clBlack, Colors.Background);
        BitBlt(Self.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0,
          SRCCOPY);
        exit;
      end;

      if floTransparentFont in Options then
        BitBlt(Canvas.Handle, Tx, Ty, Width, Height, TargetCanvas.Handle, 0,
          0, SRCAND)
      else
      if fUseTextureBmp then //...fill mask with texture
        while Tx < Width do
        begin
          while Ty < Height do
          begin
            BitBlt(Canvas.Handle, Tx, Ty, TextureBmp.Width,
              TextureBmp.Height, TextureBmp.canvas.Handle, 0, 0, SRCAND);
            Inc(Ty, TextureBmp.Height);
          end;
          Inc(Tx, TextureBmp.Width);
          Ty := 0;
        end;
    end;
  end;

  if IsItAFilledBitmap(TextureBmp) then
    FontColor := 0;
  ExtTextOutExt(TargetCanvas.Handle, x, y, GetClientRect, Caption,
    CurrTextStyle, floDelineatedText in Options,
    FNeedUpdateOnlyMainText, FontColor, CurrDelinColor,
    Colors.Highlight, Colors.Shadow,
    Illumination, Gradient, FreeFont);

  //  SetBkMode( TargetCanvas.handle, iOldBkMode );
  FNeedUpdateOnlyMainText := False;
  Gradient.Active := OldGradientFActive;

  if (Assigned(TextureBmp) or (floTransparentFont in Options)) and
    (CurrTextStyle <> fstPushed) then
    BitBlt(TargetCanvas.Handle, 0, 0, TextureMask.Width, TextureMask.Height,
      TextureMask.canvas.Handle, 0, 0, SRCPAINT);

  if Img.Canvas = TargetCanvas then
    BitBlt(Canvas.Handle, 0, 0, Img.Width, Img.Height,
      TargetCanvas.Handle, 0, 0, SRCCOPY);

  //R:=Rect(left,top,left+width,top+height);
  //ValidateRect( Parent.Handle, @r );

end;

//______

procedure TJvgLabel.CreateLabelFont;
begin
  if not (bFirstCreate) then
    DeleteObject(FreeFont.Handle);
  FreeFont.Handle := CreateRotatedFont(Font, RadianEscapments[FDirection]);
  bFirstCreate := False;
end;
//______

procedure TJvgLabel.InvalidateLabel(UpdateBackgr: Boolean);
var
  r: TRect;
begin
  R := Bounds(Left, Top, Width, Height);
  if not (csDestroying in ComponentState) then
    InvalidateRect(Parent.Handle, @r, UpdateBackgr);
end;
//______

procedure TJvgLabel.OnGradientChanged(Sender: TObject);
begin
  FNeedUpdateOnlyMainText := True;
  Repaint;
  //InvalidateLabel(False);
end;
//______

procedure TJvgLabel.OnIlluminationChanged(Sender: TObject);
begin
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  InvalidateLabel(True);
end;
//____________________________________________________Properties

procedure TJvgLabel.SetDirection(Value: TglLabelDir);
begin
  FDirection := Value;
  CreateLabelFont;
  fNeedRemakeTextureMask := True;
  InvalidateLabel(True);
end;
//______________________________________________________________

procedure TJvgLabel.SetFontWeight(Value: TFontWeight);
begin
  if FFontWeight = Value then
    exit;
  FFontWeight := Value;
  uFontWeight := word(Value) * 100;
  CreateLabelFont;
  fNeedRemakeTextureMask := True;
  InvalidateLabel(True);
end;
//______________________________________________________________

procedure TJvgLabel.SetOptions(Value: TglLabelOptions);
begin
  if FOptions = Value then
    exit;
  FOptions := Value;
  ActiveWhileControlFocused := floActiveWhileControlFocused in Options;
  if floTransparentFont in Options then
    Options := Options + [floBufferedDraw];
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  fNeedRemakeTextureMask := True;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetTexture(Value: TBitmap);
begin
  if Assigned(FTexture) then
    FTexture.Free;
  FTexture := nil;
  if (Value <> nil) and (Value.Handle <> 0) then
  begin
    FTexture := TBitmap.Create;
    FTexture.Assign(Value);
    TextureBmp := FTexture;
  end
  else
  if Assigned(FTextureImage) then
    TextureBmp := FTextureImage.Picture.Bitmap
  else
    TextureBmp := nil;
  fNeedRemakeTextureMask := True;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetBackground(Value: TBitmap);
begin
  if Assigned(FBackground) then
    FBackground.Free;
  FBackground := nil;
  if (Value <> nil) and (Value.Handle <> 0) then
  begin
    FBackground := TBitmap.Create;
    FBackground.Assign(Value);
    BackgroundBmp := FBackground;
  end
  else
  if FBackgroundImage <> nil then
    BackgroundBmp := FBackgroundImage.Picture.Bitmap
  else
    BackgroundBmp := nil;
  InvalidateLabel(True);
end;

function TJvgLabel.GetTexture: TBitmap;
begin
  if not Assigned(FTexture) then
    FTexture := TBitmap.Create;
  Result := FTexture;
end;

function TJvgLabel.GetBackground: TBitmap;
begin
  if not Assigned(FBackground) then
    FBackground := TBitmap.Create;
  Result := FBackground;
end;

procedure TJvgLabel.SetTextureImage(Value: TImage);
begin
  FTextureImage := Value;
  //mb  if (not IsItAFilledBitmap(FTexture)) and Assigned(Value) then
  if Value <> nil then
  begin
    TextureBmp := FTextureImage.Picture.Bitmap;
  end
  else
  if FTexture <> nil then
    TextureBmp := FTexture
  else
    TextureBmp := nil;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetBackgroundImage(Value: TImage);
begin
  FBackgroundImage := Value;
  //mb  if (not IsItAFilledBitmap(FBackground)) and Assigned(Value) then
  if Value <> nil then
  begin
    BackgroundBmp := FBackgroundImage.Picture.Bitmap;
    InvalidateLabel(True);
  end
  else
  if FBackground <> nil then
    BackgroundBmp := FBackground
  else
    BackgroundBmp := nil;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

//________________________________________________________ TJvgStaticTextLabel _

constructor TJvgStaticTextLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveColor := clWhite;
  FAlignment := ftaBroadwise;
  FOptions := [ftoActiveWhileControlFocused];
  FWordWrap := True;
  Width := 100;
  Height := 16;
end;
//______
//______

procedure TJvgStaticTextLabel.MouseEnter(Control: TControl);
begin
  if (ftoIgnoreMouse in Options) or FShowAsActiveWhileControlFocused then
    Exit;
  FActiveNow := True;
  Repaint;
  inherited;
end;
//______

procedure TJvgStaticTextLabel.MouseLeave(Control: TControl);
begin
  if (ftoIgnoreMouse in Options) or FShowAsActiveWhileControlFocused then
    Exit;
  FActiveNow := False;
  if ftoUnderlinedActive in Options then
    Invalidate
  else
    Repaint;
  inherited;
end;

//______

procedure TJvgStaticTextLabel.Paint;
const
  Alignments: array[TglAlignment] of Word = (DT_LEFT,
    DT_RIGHT, DT_CENTER, 0);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Alignment_: TglAlignment;
  TargetCanvas: TCanvas;
  Rect: TRect;
begin
  //inherited;
  if Caption = '' then
    Exit;

  if Assigned(ExternalCanvas) then
    TargetCanvas := ExternalCanvas
  else
    TargetCanvas := Canvas;
  TargetCanvas.Font.Assign(Font);
  Alignment_ := FAlignment;
  SetBkMode(TargetCanvas.Handle, Integer(FTransparent));

  {  if FActiveNow and(ftoUnderlinedActive in Options) then
      TargetCanvas.Font.Style := Font.Style + [fsUnderline]
    else
      TargetCanvas.Font.Style := Font.Style - [fsUnderline];
  }
  if FActiveNow then
    SetTextColor(TargetCanvas.Handle, ColorToRGB(ActiveColor))
  else
    SetTextColor(TargetCanvas.Handle, ColorToRGB(Font.Color));

  //  TextOut( TargetCanvas.Handle, 0, 0, 'lpszString', 10);
  //  BitBlt( TargetCanvas.Handle, 0, 0, Width, Height, Image.TargetCanvas.Handle, Width, Height, SRCCOPY );
  if (Alignment = ftaBroadwise) then
  begin
    if FWordWrap then
    begin
      DrawTextBroadwise(TargetCanvas);
      exit;
    end
    else
      Alignment_ := ftaLeftJustify;
  end;
  Rect := ClientRect;
  DrawText(TargetCanvas.Handle, PChar(Caption), Length(Caption), Rect,
    DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[Alignment_]);
end;
//______

procedure TJvgStaticTextLabel.DrawTextBroadwise(Canvas: TCanvas);
var
  DrawPos, Pos1, Pos2, LineWidth,
    LineNo, LexemCount, TextHeight: Integer;
  Lexem: string;
  Size: TSIZE;
  fStop, fBroadwiseLine: Boolean;

  function GetNextLexem(var Pos1, Pos2: Integer; fTrimleft: Boolean): string;
  var
    Pos: Integer;
  begin
    pos := pos1;
    if Caption[Pos] = ' ' then
      repeat inc(Pos);
      until (Pos > length(Caption)) or (Caption[Pos] <> ' ');
    Pos2 := Pos;
    if fTrimleft and (LineNo > 0) then
      Pos1 := Pos;
    repeat inc(Pos2);
    until (Pos2 > length(Caption)) or (Caption[Pos2] = ' ');

    Result := copy(Caption, Pos1, Pos2 - Pos1);
  end;

  procedure DrawLine(AdditSpace: cardinal);
  var
    i, DrawPos1, DrawPos2: Integer;
    Lexem: string;
    Size: TSIZE;
    X, X_: single;
  begin
    DrawPos1 := DrawPos;
    DrawPos2 := DrawPos;
    X := 0;
    X_ := 0;
    LineWidth := 0;
    for i := 1 to LexemCount do
    begin
      Lexem := GetNextLexem(DrawPos1, DrawPos2, i = 1);
      //      if LexemCount=1 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
      inc(LineWidth, trunc(X));
      X := X + Size.cx;
      if (trunc(X) > Width) and (LexemCount > 1) then
        exit;

      if (LexemCount > 1) and fBroadwiseLine then
        X := X + AdditSpace / (LexemCount - 1);
      TextOut(Canvas.Handle, trunc(X_), LineNo * TextHeight, PChar(Lexem),
        length(Lexem));
      X_ := X;
      DrawPos1 := DrawPos2;
    end;
  end;
begin
  if Text = '' then
    exit;
  LineWidth := 0;
  LineNo := 0;
  DrawPos := 1;
  Pos1 := 1;
  Pos2 := 1;
  LexemCount := 0;
  TextHeight := 0;
  fStop := False;
  fBroadwiseLine := True;
  repeat
    Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
    //    if LexemCount=0 then Lexem:=Lexem+' ';
    GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
    inc(LineWidth, Size.cx);
    inc(LexemCount);
    if TextHeight < Size.cy then
      TextHeight := Size.cy;
    if (LineWidth > Width) or (Pos2 >= length(Caption)) then
    begin
      if (LineWidth > Width) then
      begin
        if LexemCount = 1 then
          Pos1 := Pos2;
        if LexemCount > 1 then
          dec(LexemCount);
        DrawLine(Width - (LineWidth - Size.cx));
        DrawPos := Pos1;
        inc(LineNo);
        LexemCount := 0;
        LineWidth := 0;
        fStop := Pos1 > length(Caption);
      end
      else
      begin
        fBroadwiseLine := ftoBroadwiseLastLine in Options;
        DrawLine(Width - LineWidth);
        inc(LineNo);
        fStop := True;
      end;
    end
    else
      Pos1 := Pos2;
  until fStop;
  if FAutoSize then
    Height := max(12, LineNo * TextHeight);
end;
//______

procedure TJvgStaticTextLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
      DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if FAlignment = ftaRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;
//______

procedure TJvgStaticTextLabel.SetAlignment(Value: TglAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;
//______

procedure TJvgStaticTextLabel.SetOptions(Value: TglStaticTextOptions);
begin
  FOptions := Value;
  ActiveWhileControlFocused := ftoActiveWhileControlFocused in Options;
  Invalidate;
end;
//______

procedure TJvgStaticTextLabel.SetWordWrap(Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;
//______

procedure TJvgStaticTextLabel.SetAutoSize(Value: Boolean);
begin
  inherited AutoSize := Value; 
  AdjustBounds;
end;
//______

function TJvgStaticTextLabel.GetAutoSize: Boolean;
begin
  Result := inherited AutoSize;
end;

//________________________________________________________ TJvgGlyphLabel _

constructor TJvgGlyphLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csSetCaption,
    csReplicatable];
end;

destructor TJvgGlyphLabel.Destroy;
begin
  FGlyphOn.Free;
  FGlyphOff.Free;
  FGlyphDisabled.Free;
  inherited;
end;

function TJvgGlyphLabel.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = fgkCustom;
end;

procedure TJvgGlyphLabel.SetGlyphOn(Value: TBitmap);
begin
  if Assigned(FGlyphOn) then
    FGlyphOn.Free;
  FGlyphOn := TBitmap.Create;
  FGlyphKind := fgkCustom;
  FGlyphOn.Assign(Value);
  Invalidate;
end;

function TJvgGlyphLabel.GetGlyphOn: TBitmap;
begin
  if not Assigned(FGlyphOn) then
    FGlyphOn := TBitmap.Create;
  Result := FGlyphOn;
end;

procedure TJvgGlyphLabel.SetGlyphOff(Value: TBitmap);
begin
  if Assigned(FGlyphOff) then
    FGlyphOff.Free;
  FGlyphOff := TBitmap.Create;
  FGlyphKind := fgkCustom;
  FGlyphOff.Assign(Value);
  Invalidate;
end;

function TJvgGlyphLabel.GetGlyphOff: TBitmap;
begin
  if not Assigned(FGlyphOff) then
    FGlyphOff := TBitmap.Create;
  Result := FGlyphOff;
end;

procedure TJvgGlyphLabel.SetGlyphDisabled(Value: TBitmap);
begin
  if Assigned(FGlyphDisabled) then
    FGlyphDisabled.Free;
  FGlyphDisabled := TBitmap.Create;
  FGlyphDisabled.Assign(Value);
  Invalidate;
end;

function TJvgGlyphLabel.GetGlyphDisabled: TBitmap;
begin
  if not Assigned(FGlyphDisabled) then
    FGlyphDisabled := TBitmap.Create;
  Result := FGlyphDisabled;
end;

procedure TJvgGlyphLabel.SetGlyphKind(Value: TglGlyphKind);
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
  begin
    FGlyphOn.LoadFromResourceName(hInstance, 'ON');
    FGlyphOff.LoadFromResourceName(hInstance, 'OFF');
    FGlyphDisabled := TBitmap.Create;
    FGlyphDisabled.LoadFromResourceName(hInstance, 'DISABLED');
  end;

end;

end.

