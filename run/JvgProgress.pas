{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgProgress.PAS, released on 2003-01-15.

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

unit JvgProgress;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, SysUtils, ExtCtrls, ImgList,
  {$IFDEF USEJVCL}
  JvComponent, JvThemes,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, JvgUtils, JvExControls;

type
  {$IFDEF USEJVCL}
  TJvgProgress = class(TJvGraphicControl, IJvDenySubClassing)
  {$ELSE}
  TJvgProgress = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelBold: Boolean;
    FColors: TJvgSimleLabelColors;
    FGradient: TJvgGradient;
    FGradientBack: TJvgGradient;
    FPercent: TPercentRange;
    FCaptionAlignment: TAlignment;
    FCaptionDirection: TglLabelDir;
    FCaptionStyle: TglTextStyle;
    FStep: Integer;
    FInterspace: Integer;
    FOptions: TglProgressOptions;
    FImage: TBitmap;
    FBackImage: TBitmap;
    FNeedRebuildBackground: Boolean;
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelBold(Value: Boolean);
    procedure SetPercent(Value: TPercentRange);
    procedure SetCaptionAlignment(Value: TAlignment);
    procedure SetCaptionDirection(Value: TglLabelDir);
    procedure SetCaptionStyle(Value: TglTextStyle);
    procedure SetStep(Value: Integer);
    procedure SetInterspace(Value: Integer);
    procedure SetOptions(Value: TglProgressOptions);
    procedure OnSmthChanged(Sender: TObject);
  protected
    {$IFDEF USEJVCL}
    procedure TextChanged; override;
    {$ENDIF USEJVCL}
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvLowered;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelBold: Boolean read FBevelBold write SetBevelBold default False;
    property Colors: TJvgSimleLabelColors read FColors write FColors;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property GradientBack: TJvgGradient read FGradientBack write FGradientBack;
    property Percent: TPercentRange read FPercent write SetPercent;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property CaptionDirection: TglLabelDir read FCaptionDirection write SetCaptionDirection default fldLeftRight;
    property CaptionStyle: TglTextStyle read FCaptionStyle write SetCaptionStyle default fstShadow;
    property Step: Integer read FStep write SetStep default 3;
    property Interspace: Integer read FInterspace write SetInterspace default 1;
    property Options: TglProgressOptions read FOptions write SetOptions;
    property Anchors;
    property Align;
    property Caption;
    property Color default clBlack;
    property DragCursor;
    property DragMode;
    property Font;
    property Height default 15;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property Width default 150;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{$IFDEF USEJVCL}
uses
  JvResources;
{$ENDIF USEJVCL}

{$IFNDEF USEJVCL}
resourcestring
  RsProgressCaption = 'Progress...[%d%%]';
{$ENDIF USEJVCL}

constructor TJvgProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csDoubleClicks];
  FColors := TJvgSimleLabelColors.Create;
  FGradientBack := TJvgGradient.Create;
  FGradient := TJvgGradient.Create;
  if csDesigning in ComponentState then
    with FColors do
    begin
      FGradient.Orientation := fgdVertical;
      FGradientBack.Orientation := fgdVertical;
      FGradient.Active := True;
      FGradientBack.Active := True;
      FGradient.FromColor := clGreen;
      FGradient.ToColor := clYellow;
      FGradientBack.FromColor := 0;
      FGradientBack.ToColor := clGreen;
      FGradient.PercentFilling := FPercent;
      Delineate := clGray;
      Shadow := 0;
      Background := 0;
      Caption := RsProgressCaption;
    end;
  FColors.OnChanged := OnSmthChanged;
  FGradientBack.OnChanged := OnSmthChanged;
  FGradient.OnChanged := OnSmthChanged;
  FImage := TBitmap.Create;
  FBackImage := TBitmap.Create;
  Width := 150;
  Height := 15;
  FCaptionDirection := fldLeftRight;
  FCaptionAlignment := taLeftJustify;
  FStep := 3;
  FInterspace := 1;
  FCaptionStyle := fstShadow;
  FCaptionAlignment := taCenter;
  Font.Color := clWhite;
  FBevelInner := bvLowered;
  FBevelOuter := bvNone;
  Color := clBlack;
end;

destructor TJvgProgress.Destroy;
begin
  FGradient.Free;
  FGradientBack.Free;
  FColors.Free;
  FBackImage.Free;
  FImage.Free;
  inherited Destroy;
end;

procedure TJvgProgress.Loaded;
begin
  inherited Loaded;
  { FImage.Width := Width;
    FImage.Height := Height;
    FBackImage.Width := Width;
    FBackImage.Height := Height;
    if fpoTransparent in Options then
    GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
      FImage.Canvas.Handle );}
end;

{$IFDEF USEJVCL}
procedure TJvgProgress.TextChanged;
begin
  Repaint;
end;
{$ENDIF USEJVCL}

procedure TJvgProgress.Paint;
const
  ShadowDepth = 2;
var
  R: TRect;
  I, X, X2, Y: Integer;
  Size, TextSize: TSize;
  Capt: string;
begin
  if (FImage.Width <> Width) or (FImage.Height <> Height) then
  begin
    FImage.Width := Width;
    FImage.Height := Height;
    FBackImage.Width := Width;
    FBackImage.Height := Height;
    FNeedRebuildBackground := True;
  end;
  R := ClientRect;
  if (fpoTransparent in Options) and FNeedRebuildBackground then
  begin
    (*{$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
      PerformEraseBackground(Self, FBackImage.Canvas.Handle)
    else
    {$ENDIF JVCLThemesEnabled}
    GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
      FBackImage.Canvas.Handle);*)

    FBackImage.Canvas.Brush.Color := Parent.Brush.Color;
    FBackImage.Canvas.FillRect(R);
    FNeedRebuildBackground := False;
  end;
  BitBlt(FImage.Canvas.Handle, 0, 0, Width, Height,
    FBackImage.Canvas.Handle, 0, 0, SRCCOPY);
  with FImage.Canvas do
  begin
    Dec(R.Bottom);
    Dec(R.Right);
    R := DrawBoxEx(Handle, R, [fsdLeft, fsdTop, fsdRight, fsdBottom],
      FBevelInner, FBevelOuter,
      FBevelBold, Colors.Background, fpoTransparent in Options);
    // PercentWidth := Round(Width * Percent / 100);
    // PercentWidth := Width;
    Brush.Color := Colors.Background;
    Inc(R.Top);
    if Percent > 0 then
    begin
      GradientBox(Handle, R, FGradientBack, Integer(psSolid), 1);
      GradientBox(Handle, R, FGradient, Integer(psSolid), 1);
      X := R.Left;
      if not (fpoTransparent in Options) then
        for I := R.Left to Width div (FStep + FInterspace) + 1 do
        begin
          X2 := X + FInterspace;
          if X2 > R.Right then
            if X < R.Right then
              X2 := R.Right
            else
              Break;
          FillRect(Rect(X, R.Top, X2, R.Bottom));
          Inc(X, FStep + FInterspace);
        end;
    end;
    //...CALC POSITION
    try
      Capt := Format(Caption, [Percent]);
    except
      Capt := Caption;
    end;
    GetTextExtentPoint32(Self.Canvas.Handle, PChar(Capt), Length(Capt), Size);

    X := 2;
    Y := 0;
    //  Size.cx:=Size.cx+2+trunc(Size.cx*0.01);
    //  Size.cy := Size.cy+2;
    TextSize := Size;
    if (FCaptionStyle = fstShadow) or (FCaptionStyle = fstVolumetric) then
    begin
      Inc(Size.cy, ShadowDepth);
      Inc(Size.cx, ShadowDepth);
    end;
    if fpoDelineatedText in FOptions then
    begin
      Inc(Size.cy, 2);
      Inc(Size.cx, 2);
    end;

    case FCaptionDirection of
      fldLeftRight:
        begin
          case FCaptionAlignment of
            taCenter:
              X := (Width - Size.cx) div 2;
            taRightJustify:
              X := Width - Size.cx;
          end;
          Y := (Height - Size.cy) div 2;
        end;
      fldRightLeft:
        begin
          case FCaptionAlignment of
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
        case FCaptionAlignment of
          taCenter:
            Y := (Height + TextSize.cx - (Size.cy - TextSize.cy)) div 2;
          taRightJustify:
            Y := TextSize.cx - 4;
        else
          Y := Height - (Size.cy - TextSize.cy) - 2;
        end;
      fldUpDown:
        begin
          case FCaptionAlignment of
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

    ExtTextOutExt(Handle, X, Y, GetClientRect, Capt,
      FCaptionStyle, fpoDelineatedText in FOptions,
      False, Self.Font.Color, FColors.Delineate,
      FColors.Highlight, FColors.Shadow,
      nil, nil, Self.Font);
  end;
  FImage.Transparent := fpoTransparent in FOptions;
  FImage.TransparentColor := Parent.Brush.Color;
  Canvas.Draw(0, 0, FImage);
  {$IFDEF JVCLThemesEnabled}
  if BevelBold and ((BevelInner <> bvNone) or (BevelOuter <> bvNone)) and
    ThemeServices.ThemesEnabled then
    DrawThemedBorder(Self);
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvgProgress.OnSmthChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TJvgProgress.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Repaint;
end;

procedure TJvgProgress.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

procedure TJvgProgress.SetBevelBold(Value: Boolean);
begin
  FBevelBold := Value;
  Repaint;
end;

procedure TJvgProgress.SetPercent(Value: TPercentRange);
begin
  if FPercent <> Value then
  begin
    FPercent := Value;
    FGradient.PercentFilling := FPercent;
  end;
end;

procedure TJvgProgress.SetCaptionAlignment(Value: TAlignment);
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
    Repaint;
  end;
end;

procedure TJvgProgress.SetCaptionDirection(Value: TglLabelDir);
begin
  if FCaptionDirection <> Value then
  begin
    FCaptionDirection := Value;
    Repaint;
  end;
end;

procedure TJvgProgress.SetCaptionStyle(Value: TglTextStyle);
begin
  if FCaptionStyle <> Value then
  begin
    FCaptionStyle := Value;
    Repaint;
  end;
end;

procedure TJvgProgress.SetStep(Value: Integer);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    Repaint;
  end;
end;

procedure TJvgProgress.SetInterspace(Value: Integer);
begin
  if FInterspace <> Value then
  begin
    FInterspace := Value;
    Repaint;
  end;
end;

procedure TJvgProgress.SetOptions(Value: TglProgressOptions);
begin
  FOptions := Value;
  if fpoTransparent in FOptions then
  begin
    ControlStyle := ControlStyle - [csOpaque];
    {$IFDEF USEJVCL}
    IncludeThemeStyle(Self, [csParentBackground]);
    {$ENDIF USEJVCL}
  end
  else
  begin
    ControlStyle := ControlStyle + [csOpaque];
    {$IFDEF USEJVCL}
    ExcludeThemeStyle(Self, [csParentBackground]);
    {$ENDIF USEJVCL}
  end;
  Repaint;
end;

end.

