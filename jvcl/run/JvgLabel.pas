{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLabel.PAS, released on 2003-01-15.

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

unit JvgLabel;

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

const
  FTextAlign = DT_LEFT or DT_SINGLELINE;
  RadianEscapments: array [TglLabelDir] of Integer = (0, -1800, -900, 900);

type
  TFontWeight = (fwDONTCARE, fwTHIN, fwEXTRALIGHT, fwLIGHT, fwNORMAL, fwMEDIUM,
    fwSEMIBOLD, fwBOLD, fwEXTRABOLD, fwHEAVY);

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
    procedure WMLMouseUp(var Msg: TMessage); message WM_LBUTTONUP;
    procedure WMLMouseDown(var Msg: TMessage); message WM_LBUTTONDOWN;
  protected
    FActiveNow: Boolean;
    FShowAsActiveWhileControlFocused: Boolean;
    ActiveWhileControlFocused: Boolean;
    FNeedRehookFocusControl: Boolean;
    FExternalCanvas: TCanvas;
    procedure HookFocusControlWndProc;
    procedure UnhookFocusControlWndProc;
    procedure FocusControlWndHookProc(var Msg: TMessage);
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
    FUFontWeight: Word;
    FRunOnce: Boolean;
    FFirstCreate: Boolean;
    FNeedUpdateOnlyMainText: Boolean;
    FNeedRemakeTextureMask: Boolean;
    FImg: TBitmap;
    FTextureMask: TBitmap;
    FBackgroundBmp: TBitmap;
    FTextureBmp: TBitmap;
    FTargetCanvas: TCanvas;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property SupressPaint: Boolean read FSupressPaint write FSupressPaint;
    property Canvas;
    property ExternalCanvas;
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
    property Direction: TglLabelDir read FDirection write SetDirection  default fldLeftRight;
    property TextStyles: TJvgLabelTextStyles read FTextStyles write FTextStyles;
    property Colors: TJvgLabelColors read FColors write FColors;
    property FontWeight: TFontWeight read FFontWeight write SetFontWeight;
    property Options: TglLabelOptions read FOptions write SetOptions;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Illumination: TJvgIllumination read FIllumination write FIllumination;
    property Texture: TBitmap read GetTexture write SetTexture;
    property Background: TBitmap read GetBackground write SetBackground;
    property TextureImage: TImage read FTextureImage write SetTextureImage;
    property BackgroundImage: TImage read FBackgroundImage write SetBackgroundImage;
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
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Canvas;
    property ExternalCanvas;
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
    property ActiveColor: TColor read FActiveColor write FActiveColor default clWhite;
    property Alignment: TglAlignment read FAlignment write SetAlignment default ftaBroadwise;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    property Options: TglStaticTextOptions read FOptions write SetOptions;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
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
    property GlyphKind: TglGlyphKind read FGlyphKind write SetGlyphKind default fgkDefault;
    property GlyphOn: TBitmap read GetGlyphOn write SetGlyphOn stored True;
    property GlyphOff: TBitmap read GetGlyphOff write SetGlyphOff stored True;
    property GlyphDisabled: TBitmap read GetGlyphDisabled write
      SetGlyphDisabled stored IsCustomGlyph;
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
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils, // SetWindowLongPtr
  {$ENDIF ~COMPILER12_UP}
  Math;

//=== { TJvgCustomLabel } ====================================================

constructor TJvgCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  ActiveWhileControlFocused := True;
  FAutoSize := True;
  FTransparent := True;
  FFocusControlMethod := fcmOnMouseDown;
end;

destructor TJvgCustomLabel.Destroy;
begin
  SetFocusControl(nil);
  inherited Destroy;
end;

procedure TJvgCustomLabel.Paint;
begin
  //...if FocusControl have changed his parent in Run-Time...
  if FNeedRehookFocusControl then
    HookFocusControlWndProc;
  //don't inherited;
end;

procedure TJvgCustomLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FocusControl) and (Operation = opRemove) then
  begin
    {UnhookFocusControlWndProc;}
    FFocusControl := nil;
  end;
end;

procedure TJvgCustomLabel.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseEnter(Control);
  if Assigned(FocusControl) and (FocusControlMethod = fcmOnMouseEnter) then
    FocusControl.SetFocus;
end;

procedure TJvgCustomLabel.WMLMouseUp(var Msg: TMessage);
begin
  inherited;
  if Enabled and (FocusControlMethod = fcmOnMouseUp) and
    Assigned(FocusControl) and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;

procedure TJvgCustomLabel.WMLMouseDown(var Msg: TMessage);
begin
  inherited;
  if Enabled and (FocusControlMethod = fcmOnMouseDown) and
    Assigned(FocusControl) and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;

procedure TJvgCustomLabel.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TJvgCustomLabel.HookFocusControlWndProc;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLongPtr(FocusControl.Handle, GWLP_WNDPROC));
  if P <> FNewWndProc then
  begin
    FPrevWndProc := P;
    FNewWndProc := JvMakeObjectInstance(FocusControlWndHookProc);
    SetWindowLongPtr(FocusControl.Handle, GWLP_WNDPROC, LONG_PTR(FNewWndProc));
  end;
end;

procedure TJvgCustomLabel.UnhookFocusControlWndProc;
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

procedure TJvgCustomLabel.FocusControlWndHookProc(var Msg: TMessage);
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
    WM_DESTROY:
      FNeedRehookFocusControl := True;
  end;
  Msg.Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle,
    Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TJvgCustomLabel.SetFocusControl(Value: TWinControl);
begin
  if FFocusControl = Value then
    Exit;
  if ActiveWhileControlFocused and Assigned(FFocusControl) then
    UnhookFocusControlWndProc;
  ReplaceComponentReference(Self, Value, TComponent(FFocusControl));
  if ActiveWhileControlFocused and Assigned(FFocusControl) then
    HookFocusControlWndProc;
end;

procedure TJvgCustomLabel.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

//=== { TJvgLabel } ==========================================================

constructor TJvgLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TextStyles := TJvgLabelTextStyles.Create;
  Colors := TJvgLabelColors.Create;
  Gradient := TJvgGradient.Create;
  FIllumination := TJvgIllumination.Create;
  FImg := TBitmap.Create;

  FFirstCreate := True;
  FreeFont := TFont.Create;
  if csDesigning in ComponentState then
    Self.Font.Name := 'Arial';
  AutoSize := True;
  //  FRunOnce:=False;
  //  FActiveNow := False;

  FDirection := fldLeftRight;
  FFontWeight := fwDONTCARE;
  //  FSupressPaint := False;
  FUFontWeight := Word(fwDONTCARE);
  //  FNeedUpdateOnlyMainText:=False;
  FGradient.OnChanged := OnGradientChanged;
  FIllumination.OnChanged := OnIlluminationChanged;
  TextStyles.OnChanged := OnIlluminationChanged;
  Colors.OnChanged := OnIlluminationChanged;
  FOptions := [floActiveWhileControlFocused];
  FTargetCanvas := Canvas;
  FTransparent := True;
  Width := 100;
  Height := 16;
end;

destructor TJvgLabel.Destroy;
begin
  TextStyles.Free;
  Colors.Free;
  Gradient.Free;
  FIllumination.Free;
  FTexture.Free;
  FBackground.Free;
  FTextureMask.Free;
  FImg.Free;
  inherited Destroy;
  DeleteObject(FreeFont.Handle);
  FreeFont.Free;
end;

procedure TJvgLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = BackgroundImage) and (Operation = opRemove) then
    BackgroundImage := nil
  else
  if (AComponent = TextureImage) and (Operation = opRemove) then
    TextureImage := nil;
end;

procedure TJvgLabel.FontChanged;
begin
  inherited FontChanged;
  CreateLabelFont;
  Invalidate;
end;

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
    if (Passive <> Active) or
      ((Background <> BackgroundActive) and not Transparent) then
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
    if (Passive <> Active) or
      ((Background <> BackgroundActive) and not Transparent) then
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

procedure TJvgLabel.Loaded;
begin
  inherited Loaded;
  if FTexture <> nil then
    FTextureBmp := FTexture
  else
  if Assigned(FTextureImage) then
    FTextureBmp := FTextureImage.Picture.Bitmap
  else
    FTextureBmp := nil;
  if Assigned(FBackground) then
    FBackgroundBmp := FBackground
  else
  if Assigned(FBackgroundImage) then
    FBackgroundBmp := FBackgroundImage.Picture.Bitmap
  else
    FBackgroundBmp := nil;
end;

procedure TJvgLabel.Paint;
var
  R: TRect;
  X, Y, X1, Y1, TX, TY: Integer;
  Size, TextSize: TSize;
  FontColor: TColor;
  CurrTextStyle: TglTextStyle;
  CurrDelinColor: TColor;
  OldGradientFActive, LUseBackgroundBmp, LUseTextureBmp, LBufferedDraw: Boolean;
begin
  inherited Paint;
  if FSupressPaint or (Length(Caption) = 0) then
    Exit;
  if floTransparentFont in Options then
    LBufferedDraw := True
  else
    LBufferedDraw := (floBufferedDraw in Options) and
      not (csDesigning in ComponentState);
  if LBufferedDraw then
    FTargetCanvas := FImg.Canvas
  else
  if Assigned(ExternalCanvas) then
    FTargetCanvas := ExternalCanvas
  else
    FTargetCanvas := Canvas;
  FNeedUpdateOnlyMainText := FNeedUpdateOnlyMainText and not LBufferedDraw and
    (not IsItAFilledBitmap(FBackgroundBmp));
  if not FRunOnce then
  begin
    FNeedUpdateOnlyMainText := False;
    FRunOnce := True;
  end;
  FTargetCanvas.Font := FreeFont;
  //...CALC POSITION
  GetTextExtentPoint32(FTargetCanvas.Handle, PChar(Caption), Length(Caption), Size);
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
  X := 0;
  Y := 0;
  Size.cx := Size.cx + 2 + Trunc(Size.cx * 0.01);
  //  Size.cy:=Size.cy+Trunc(Size.cy*0.1);
  Size.cy := Size.cy + 2;
  TextSize := Size;
  if (CurrTextStyle = fstShadow) or (CurrTextStyle = fstVolumetric) then
  begin
    Inc(Size.cy, Illumination.ShadowDepth);
    Inc(Size.cx, Illumination.ShadowDepth);
  end;
  if floDelineatedText in Options then
  begin
    Inc(Size.cy, 2);
    Inc(Size.cx, 2);
  end;

  if (Align = alNone) and AutoSize then
    case FDirection of
      fldLeftRight, fldRightLeft:
        begin
          Width := Size.cx;
          Height := Size.cy;
        end;
    else {fldDownUp,fldUpDown:}
      begin
        Width := Size.cy;
        Height := Size.cx;
      end;
    end;

  //  pt := CalcAlignedTextPosition( FTargetCanvas.Handle, Caption, Size );
  //  X := pt.X; Y := pt.Y;
  //CalcAlignedTextPosition( FTargetCanvas.Handle, Caption, Size );

  case FDirection of
    fldLeftRight:
      begin //if Align = alNone then begin Width:=Max(w,Size.cx);Height:=Max(h,Size.cy); end;
        case Alignment of
          taCenter:
            X := (Width - Size.cx) div 2;
          taRightJustify:
            X := Width - Size.cx;
        end;
      end;
    fldRightLeft:
      begin //if Align = alNone then begin Width:=Max(w,Size.cx);Height:=Max(h,Size.cy);X:=Width;Y:=Height; end;
        case Alignment of
          taCenter:
            X := (Width + Size.cx) div 2;
          taLeftJustify:
            X := Width - (Size.cx - TextSize.cx) - 2;
        else
          X := TextSize.cx;
        end;
        Y := TextSize.cy;
      end;
    fldDownUp:
      begin //if Align = alNone then begin Height:=Max(h,Size.cx);Width:=Max(w,Size.cy);Y:=Height-2; end;
        case Alignment of
          taCenter:
            Y := (Height + TextSize.cx - (Size.cy - TextSize.cy)) div 2;
          taRightJustify:
            Y := TextSize.cx - 4;
        else
          Y := Height - (Size.cy - TextSize.cy) - 2;
        end;
      end;
    fldUpDown:
      begin //if Align = alNone then begin Height:=Max(h,Size.cx);Width:=Max(w,Size.cy);X:=Width; end;
        case Alignment of
          taCenter:
            Y := (Height - Size.cx) div 2;
          taRightJustify:
            Y := Height - Size.cx;
        else
          Y := 1;
        end;
        X := TextSize.cy;
      end;
  end;

  //...CALC POSITION end

  R := GetClientRect;
  if FTargetCanvas = FImg.Canvas then
  begin
    FImg.Width := Width;
    FImg.Height := Height;
  end;

  SetBkMode(FTargetCanvas.Handle, Windows.TRANSPARENT);
  if not Transparent then
  begin
    FTargetCanvas.Brush.Style := bsSolid;
    if FActiveNow then
      FTargetCanvas.Brush.Color := Colors.BackgroundActive
    else
      FTargetCanvas.Brush.Color := Colors.Background;
    FTargetCanvas.FillRect(R);
  end;

  try
    LUseBackgroundBmp := IsItAFilledBitmap(FBackgroundBmp);
  except
    //  raise;
    LUseBackgroundBmp := False;
    FBackgroundBmp := nil;
    FBackgroundImage := nil;
  end;

  try
    LUseTextureBmp := IsItAFilledBitmap(FTextureBmp);
  except
    LUseTextureBmp := False;
    FTextureBmp := nil;
    FTextureImage := nil;
  end;

  //  ShadowColor_ := Colors.Shadow;
  //  HighlightColor_ := Colors.Highlight;
  if LUseBackgroundBmp then
  begin //...FillBackground
    TX := 0;
    TY := 0;
    while TX < Width do
    begin
      while TY < Height do
      begin
        BitBlt(FTargetCanvas.Handle, TX, TY,
          FBackgroundBmp.Width, FBackgroundBmp.Height,
          FBackgroundBmp.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(TY, FBackgroundBmp.Height);
      end;
      Inc(TX, FBackgroundBmp.Width);
      TY := 0;
    end;
  end
  else
  if LBufferedDraw then
    with FTargetCanvas do
    begin
      if Transparent or (floTransparentFont in Options) then
      try
        Brush.Color := Parent.Brush.Color;
        Brush.Style := bsSolid;
        FillRect(R);
        Brush.Style := bsClear;
        GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
          FTargetCanvas.Handle);
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
    X1 := 4;
    Y1 := 4;
  end
  else
  begin
    X1 := 2;
    Y1 := 2;
  end;

  if CurrTextStyle = fstNone then
  begin
    X1 := X1 div 2 - 1;
    Y1 := Y1 div 2 - 1;
  end;
  if CurrTextStyle = fstShadow then
  begin
    X1 := X1 div 2 - 1;
    Y1 := Y1 div 2 - 1;
  end;
  if {FNeedRemakeTextureMask and} LUseTextureBmp or
    (floTransparentFont in Options) then
  begin
    if not Assigned(FTextureMask) then
      FTextureMask := TBitmap.Create;
    with FTextureMask do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Canvas.Brush.Color := clBlack;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(GetClientRect);
      Canvas.Font := FreeFont;
      Canvas.Font.Color := clWhite;
      if (CurrTextStyle = fstNone) or (CurrTextStyle = fstShadow) then
        Canvas.TextOut(X + X1, Y + Y1, Caption)
      else
        Canvas.TextOut(X + X1 div 2, Y + Y1 div 2, Caption);
      TX := 0;
      TY := 0;

      if not Self.Transparent then
      begin
        BitBlt(Canvas.Handle, TX, TY, Width, Height, FTargetCanvas.Handle, 0,
          0, SRCAND);
        if FActiveNow then
          JvgUtils.ChangeBitmapColor(FTextureMask, clBlack, Colors.BackgroundActive)
        else
          JvgUtils.ChangeBitmapColor(FTextureMask, clBlack, Colors.Background);
        BitBlt(Self.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0,
          SRCCOPY);
        Exit;
      end;

      if floTransparentFont in Options then
        BitBlt(Canvas.Handle, TX, TY, Width, Height, FTargetCanvas.Handle, 0,
          0, SRCAND)
      else
      if LUseTextureBmp then //...fill mask with texture
        while TX < Width do
        begin
          while TY < Height do
          begin
            BitBlt(Canvas.Handle, TX, TY, FTextureBmp.Width,
              FTextureBmp.Height, FTextureBmp.Canvas.Handle, 0, 0, SRCAND);
            Inc(TY, FTextureBmp.Height);
          end;
          Inc(TX, FTextureBmp.Width);
          TY := 0;
        end;
    end;
  end;

  if IsItAFilledBitmap(FTextureBmp) then
    FontColor := 0;
  ExtTextOutExt(FTargetCanvas.Handle, X, Y, GetClientRect, Caption,
    CurrTextStyle, floDelineatedText in Options,
    FNeedUpdateOnlyMainText, FontColor, CurrDelinColor,
    Colors.Highlight, Colors.Shadow,
    Illumination, Gradient, FreeFont);

  //  SetBkMode( FTargetCanvas.Handle, iOldBkMode );
  FNeedUpdateOnlyMainText := False;
  Gradient.Active := OldGradientFActive;

  if (Assigned(FTextureBmp) or (floTransparentFont in Options)) and
    (CurrTextStyle <> fstPushed) then
    if Assigned(FTextureMask) then {fix access violation! WPostma.}
      BitBlt(FTargetCanvas.Handle, 0, 0, FTextureMask.Width, FTextureMask.Height,
        FTextureMask.Canvas.Handle, 0, 0, SRCPAINT);

  if FImg.Canvas = FTargetCanvas then
    BitBlt(Canvas.Handle, 0, 0, FImg.Width, FImg.Height,
      FTargetCanvas.Handle, 0, 0, SRCCOPY);

  //R:=Rect(Left,Top,Left+Width,Top+Height);
  //ValidateRect( Parent.Handle, @R );
end;

procedure TJvgLabel.CreateLabelFont;
begin
  if not FFirstCreate then
    DeleteObject(FreeFont.Handle);
  FreeFont.Handle := CreateRotatedFont(Font, RadianEscapments[FDirection]);
  FFirstCreate := False;
end;

procedure TJvgLabel.InvalidateLabel(UpdateBackgr: Boolean);
var
  R: TRect;
begin
  R := Bounds(Left, Top, Width, Height);
  if not (csDestroying in ComponentState) then
    InvalidateRect(Parent.Handle, @R, UpdateBackgr);
end;

procedure TJvgLabel.OnGradientChanged(Sender: TObject);
begin
  FNeedUpdateOnlyMainText := True;
  Repaint;
  //InvalidateLabel(False);
end;

procedure TJvgLabel.OnIlluminationChanged(Sender: TObject);
begin
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetDirection(Value: TglLabelDir);
begin
  FDirection := Value;
  CreateLabelFont;
  FNeedRemakeTextureMask := True;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetFontWeight(Value: TFontWeight);
begin
  if FFontWeight = Value then
    Exit;
  FFontWeight := Value;
  FUFontWeight := Word(Value) * 100;
  CreateLabelFont;
  FNeedRemakeTextureMask := True;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetOptions(Value: TglLabelOptions);
begin
  if FOptions = Value then
    Exit;
  FOptions := Value;
  ActiveWhileControlFocused := floActiveWhileControlFocused in Options;
  if floTransparentFont in Options then
    Options := Options + [floBufferedDraw];
  CalcShadowAndHighlightColors((Parent as TWinControl).Brush.Color, Colors);
  FNeedRemakeTextureMask := True;
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
    FTextureBmp := FTexture;
  end
  else
  if Assigned(FTextureImage) then
    FTextureBmp := FTextureImage.Picture.Bitmap
  else
    FTextureBmp := nil;
  FNeedRemakeTextureMask := True;
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
    FBackgroundBmp := FBackground;
  end
  else
  if FBackgroundImage <> nil then
    FBackgroundBmp := FBackgroundImage.Picture.Bitmap
  else
    FBackgroundBmp := nil;
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
    FTextureBmp := FTextureImage.Picture.Bitmap;
  end
  else
  if FTexture <> nil then
    FTextureBmp := FTexture
  else
    FTextureBmp := nil;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetBackgroundImage(Value: TImage);
begin
  FBackgroundImage := Value;
  //mb  if (not IsItAFilledBitmap(FBackground)) and Assigned(Value) then
  if Value <> nil then
  begin
    FBackgroundBmp := FBackgroundImage.Picture.Bitmap;
    InvalidateLabel(True);
  end
  else
  if FBackground <> nil then
    FBackgroundBmp := FBackground
  else
    FBackgroundBmp := nil;
  InvalidateLabel(True);
end;

procedure TJvgLabel.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

//=== { TJvgStaticTextLabel } ================================================

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

procedure TJvgStaticTextLabel.MouseEnter(Control: TControl);
begin
  if (ftoIgnoreMouse in Options) or FShowAsActiveWhileControlFocused then
    Exit;
  FActiveNow := True;
  Repaint;
  inherited MouseEnter(Control);
end;

procedure TJvgStaticTextLabel.MouseLeave(Control: TControl);
begin
  if (ftoIgnoreMouse in Options) or FShowAsActiveWhileControlFocused then
    Exit;
  FActiveNow := False;
  if ftoUnderlinedActive in Options then
    Invalidate
  else
    Repaint;
  inherited MouseLeave(Control);
end;

procedure TJvgStaticTextLabel.Paint;
const
  Alignments: array [TglAlignment] of Word =
    (DT_LEFT, DT_RIGHT, DT_CENTER, 0);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  LAlignment: TglAlignment;
  FTargetCanvas: TCanvas;
  Rect: TRect;
begin
  //inherited;
  if Caption = '' then
    Exit;

  if Assigned(ExternalCanvas) then
    FTargetCanvas := ExternalCanvas
  else
    FTargetCanvas := Canvas;
  FTargetCanvas.Font.Assign(Font);
  LAlignment := FAlignment;
  SetBkMode(FTargetCanvas.Handle, Integer(FTransparent));

  {  if FActiveNow and(ftoUnderlinedActive in Options) then
      FTargetCanvas.Font.Style := Font.Style + [fsUnderline]
    else
      FTargetCanvas.Font.Style := Font.Style - [fsUnderline];
  }
  if FActiveNow then
    SetTextColor(FTargetCanvas.Handle, ColorToRGB(ActiveColor))
  else
    SetTextColor(FTargetCanvas.Handle, ColorToRGB(Font.Color));

  //  TextOut( FTargetCanvas.Handle, 0, 0, 'lpszString', 10);
  //  BitBlt( FTargetCanvas.Handle, 0, 0, Width, Height, Image.FTargetCanvas.Handle, Width, Height, SRCCOPY );
  if Alignment = ftaBroadwise then
  begin
    if FWordWrap then
    begin
      DrawTextBroadwise(FTargetCanvas);
      Exit;
    end
    else
      LAlignment := ftaLeftJustify;
  end;
  Rect := ClientRect;
  Windows.DrawText(FTargetCanvas.Handle, PChar(Caption), Length(Caption), Rect,
    DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[LAlignment]);
end;

procedure TJvgStaticTextLabel.DrawTextBroadwise(Canvas: TCanvas);
var
  DrawPos, Pos1, Pos2, LineWidth, LineNo, LexemCount, TextHeight: Integer;
  Lexem: string;
  Size: TSize;
  LStop, LBroadwiseLine: Boolean;

  function GetNextLexem(var Pos1, Pos2: Integer; ATrimLeft: Boolean): string;
  var
    Pos: Integer;
  begin
    Pos := Pos1;
    if Caption[Pos] = ' ' then
      repeat
        Inc(Pos);
      until (Pos > Length(Caption)) or (Caption[Pos] <> ' ');
    Pos2 := Pos;
    if ATrimLeft and (LineNo > 0) then
      Pos1 := Pos;
    repeat
      Inc(Pos2);
    until (Pos2 > Length(Caption)) or (Caption[Pos2] = ' ');

    Result := Copy(Caption, Pos1, Pos2 - Pos1);
  end;

  procedure DrawLine(AdditSpace: Cardinal);
  var
    I, DrawPos1, DrawPos2: Integer;
    Lexem: string;
    Size: TSize;
    X, X1: Single;
  begin
    DrawPos1 := DrawPos;
    DrawPos2 := DrawPos;
    X := 0;
    X1 := 0;
    LineWidth := 0;
    for I := 1 to LexemCount do
    begin
      Lexem := GetNextLexem(DrawPos1, DrawPos2, I = 1);
      //      if LexemCount=1 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), Length(Lexem), Size);
      Inc(LineWidth, Trunc(X));
      X := X + Size.cx;
      if (Trunc(X) > Width) and (LexemCount > 1) then
        Exit;

      if (LexemCount > 1) and LBroadwiseLine then
        X := X + AdditSpace / (LexemCount - 1);
      TextOut(Canvas.Handle, Trunc(X1), LineNo * TextHeight, PChar(Lexem),
        Length(Lexem));
      X1 := X;
      DrawPos1 := DrawPos2;
    end;
  end;

begin
  if Text = '' then
    Exit;
  LineWidth := 0;
  LineNo := 0;
  DrawPos := 1;
  Pos1 := 1;
  Pos2 := 1;
  LexemCount := 0;
  TextHeight := 0;
  LStop := False;
  LBroadwiseLine := True;
  repeat
    Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
    //    if LexemCount=0 then Lexem:=Lexem+' ';
    GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), Length(Lexem), Size);
    Inc(LineWidth, Size.cx);
    Inc(LexemCount);
    if TextHeight < Size.cy then
      TextHeight := Size.cy;
    if (LineWidth > Width) or (Pos2 >= Length(Caption)) then
    begin
      if LineWidth > Width then
      begin
        if LexemCount = 1 then
          Pos1 := Pos2;
        if LexemCount > 1 then
          Dec(LexemCount);
        DrawLine(Width - (LineWidth - Size.cx));
        DrawPos := Pos1;
        Inc(LineNo);
        LexemCount := 0;
        LineWidth := 0;
        LStop := Pos1 > Length(Caption);
      end
      else
      begin
        LBroadwiseLine := ftoBroadwiseLastLine in Options;
        DrawLine(Width - LineWidth);
        Inc(LineNo);
        LStop := True;
      end;
    end
    else
      Pos1 := Pos2;
  until LStop;
  if FAutoSize then
    Height := Max(12, LineNo * TextHeight);
end;

procedure TJvgStaticTextLabel.AdjustBounds;
const
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(HWND_DESKTOP);
    Canvas.Handle := DC;
    Windows.DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
      DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(HWND_DESKTOP, DC);
    X := Left;
    if FAlignment = ftaRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvgStaticTextLabel.SetAlignment(Value: TglAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TJvgStaticTextLabel.SetOptions(Value: TglStaticTextOptions);
begin
  FOptions := Value;
  ActiveWhileControlFocused := ftoActiveWhileControlFocused in Options;
  Invalidate;
end;

procedure TJvgStaticTextLabel.SetWordWrap(Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

procedure TJvgStaticTextLabel.SetAutoSize(Value: Boolean);
begin
  inherited AutoSize := Value;
  AdjustBounds;
end;

function TJvgStaticTextLabel.GetAutoSize: Boolean;
begin
  Result := inherited AutoSize;
end;

//=== { TJvgGlyphLabel } =====================================================

// (rom) Glyph handling is a mess

constructor TJvgGlyphLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csOpaque,
    csClickEvents, csSetCaption, csReplicatable];
end;

destructor TJvgGlyphLabel.Destroy;
begin
  FGlyphOn.Free;
  FGlyphOff.Free;
  FGlyphDisabled.Free;
  inherited Destroy;
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
    FGlyphOn.Assign(nil); // fixes GDI resource leak
    FGlyphOff.Assign(nil); // fixes GDI resource leak
    FGlyphOn.LoadFromResourceName(HInstance, 'JvgON');
    FGlyphOff.LoadFromResourceName(HInstance, 'JvgOFF');
    FGlyphDisabled := TBitmap.Create;
    FGlyphDisabled.LoadFromResourceName(HInstance, 'JvgDISABLED');
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
