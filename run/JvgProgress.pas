{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgProgress.PAS, released on 2003-01-15.

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

unit JvgProgress;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  SysUtils,
  JvgTypes,
  JvgCommClasses,
  JvgUtils,
  JVComponent,
  ExtCtrls{$IFDEF COMPILER5_UP},
  Imglist{$ENDIF};
type

  TJvgProgress = class(TJvGraphicControl)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelBold: boolean;
    FColors: TJvgSimleLabelColors;
    FGradientF: TJvgGradient;
    FGradientB: TJvgGradient;
    FPercent: TglPercent;
    FCaptionAlignment: TAlignment;
    FCaptionDirection: TglLabelDir;
    FCaptionStyle: TglTextStyle;
    FStep: integer;
    FInterspace: integer;
    FOptions: TglProgressOptions;
    Image: TBitmap;
    BackImage: TBitmap;
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelBold(Value: boolean);
    procedure SetPercent(Value: TglPercent);
    procedure SetCaptionAlignment(Value: TAlignment);
    procedure SetCaptionDirection(Value: TglLabelDir);
    procedure SetCaptionStyle(Value: TglTextStyle);
    procedure SetStep(Value: integer);
    procedure SetInterspace(Value: integer);
    procedure SetOptions(Value: TglProgressOptions);

    procedure OnSmthChanged(Sender: TObject);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Loaded; override;
    procedure Paint; override;

  public
    fNeedRebuildBackground: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF COMPILER5_UP}
    property Anchors;
    {$ENDIF}
    property Align;
    property Caption;
    property Font;
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

    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner
      default bvLowered;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter
      default bvNone;
    property BevelBold: boolean read FBevelBold write SetBevelBold
      default false;
    property Colors: TJvgSimleLabelColors read FColors write FColors;
    property Gradient: TJvgGradient read FGradientF write FGradientF;
    property GradientBack: TJvgGradient read FGradientB write FGradientB;
    property Percent: TglPercent read FPercent write SetPercent;
    property CaptionAlignment: TAlignment read FCaptionAlignment write
      SetCaptionAlignment;
    property CaptionDirection: TglLabelDir read FCaptionDirection write
      SetCaptionDirection;
    property CaptionStyle: TglTextStyle read FCaptionStyle write
      SetCaptionStyle;
    property Step: integer read FStep write SetStep;
    property Interspace: integer read FInterspace write SetInterspace;
    property Options: TglProgressOptions read FOptions write SetOptions;

  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TJvgProgress.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csOpaque, csDoubleClicks];
  FColors := TJvgSimleLabelColors.Create;
  FGradientB := TJvgGradient.Create;
  FGradientF := TJvgGradient.Create;
  //..defaults
  if csDesigning in ComponentState then
    with FColors do
    begin
      FGradientF.Orientation := fgdVertical;
      FGradientB.Orientation := fgdVertical;
      FGradientF.Active := true;
      FGradientB.Active := true;
      FGradientF.FromColor := clGreen;
      FGradientF.ToColor := clYellow;
      FGradientB.FromColor := 0;
      FGradientB.ToColor := clGreen;
      FGradientF.PercentFilling := FPercent;
      Delineate := clGray;
      Shadow := 0;
      Background := 0;
      Caption := 'progress...[%d%%]';
    end;
  FColors.OnChanged := OnSmthChanged;
  FGradientB.OnChanged := OnSmthChanged;
  FGradientF.OnChanged := OnSmthChanged;
  Image := TBitmap.Create;
  BackImage := TBitmap.Create;
  Width := 150;
  height := 15;

  FCaptionDirection := fldLeftRight;
  FCaptionAlignment := taLeftJustify;
  //...
  FStep := 3;
  FInterspace := 1;
  FCaptionStyle := fstShadow;
  FCaptionAlignment := taCenter;
  Font.Color := clWhite;
  FBevelInner := bvLowered;
  //...
  Color := clBlack;
end;

destructor TJvgProgress.Destroy;
begin
  FGradientF.Free;
  FGradientB.Free;
  FColors.Free;
  BackImage.Free;
  Image.Free;
  inherited;
end;

procedure TJvgProgress.Loaded;
begin
  inherited loaded;
  {  Image.Width := Width; Image.Height := Height;
    BackImage.Width := Width; BackImage.Height := Height;
    if fpoTransparent in Options then
    GetParentImageRect( self, Bounds(Left,Top,Width,Height),
                 Image.Canvas.Handle );}

end;

procedure TJvgProgress.CMTextChanged(var Message: TMessage);
begin
  Repaint;
end;

procedure TJvgProgress.Paint;
const
  ShadowDepth = 2;
var
  r: TRect;
  i, x, x2, y: integer;
  Size, TextSize: TSize;
  Capt: string;
begin
  if (Image.Width <> Width) or (Image.Height <> Height) then
  begin
    Image.Width := Width;
    Image.Height := Height;
    BackImage.Width := Width;
    BackImage.Height := Height;
    fNeedRebuildBackground := true;
  end;
  r := ClientRect;
  if (fpoTransparent in Options) and fNeedRebuildBackground then
  begin
    GetParentImageRect(self, Bounds(Left, Top, Width, Height),
      BackImage.Canvas.Handle);
    fNeedRebuildBackground := false;
  end;
  BitBlt(Image.Canvas.Handle, 0, 0, Width, Height, BackImage.Canvas.Handle, 0,
    0, SRCCOPY);
  with Image.Canvas do
  begin
    dec(r.bottom);
    dec(r.right);
    r := DrawBoxEx(Handle, r, [fsdLeft, fsdTop, fsdRight, fsdBottom],
      FBevelInner, FBevelOuter,
      FBevelBold, Colors.Background, fpoTransparent in Options);

    //    PercentWidth := trunc( Width * Percent / 100 );
    //    PercentWidth := Width;
    Brush.Color := Colors.Background;
    inc(r.top);
    if Percent > 0 then
    begin
      GradientBox(handle, r, FGradientB, integer(psSolid), 1);
      GradientBox(handle, r, FGradientF, integer(psSolid), 1);
      x := r.left;
      if not (fpoTransparent in Options) then
        for i := r.left to Width div (FStep + FInterspace) + 1 do
        begin
          x2 := x + FInterspace;
          if x2 > r.right then
            if x < r.right then
              x2 := r.right
            else
              break;
          FillRect(Rect(x, r.top, x2, r.Bottom));
          inc(x, FStep + FInterspace);
        end;

    end;
    //...CALC POSITION
    try
      Capt := Format(Caption, [Percent]);
    except Capt := Caption;
    end;
    GetTextExtentPoint32(Self.Canvas.Handle, PChar(Capt), length(Capt), Size);

    x := 2;
    y := 0;
    //  Size.cx:=Size.cx+2+trunc(Size.cx*0.01);
    //  Size.cy := Size.cy+2;
    TextSize := Size;
    if (FCaptionStyle = fstShadow) or (FCaptionStyle = fstVolumetric) then
    begin
      inc(Size.cy, ShadowDepth);
      inc(Size.cx, ShadowDepth);
    end;
    if fpoDelineatedText in FOptions then
    begin
      inc(Size.cy, 2);
      inc(Size.cx, 2);
    end;

    case FCaptionDirection of
      fldLeftRight:
        begin
          case FCaptionAlignment of
            taCenter: x := (Width - Size.cx) div 2;
            taRightJustify: x := Width - Size.cx;
          end;
          y := (Height - Size.cy) div 2;
        end;
      fldRightLeft:
        begin
          case FCaptionAlignment of
            taCenter: x := (Width + Size.cx) div 2;
            taLeftJustify: x := Width - (Size.cx - TextSize.cx) - 2;
          else
            x := TextSize.cx;
          end;
          y := TextSize.cy;
        end;
      fldDownUp:
        begin
          case FCaptionAlignment of
            taCenter: y := (Height + TextSize.cx - (Size.cy - TextSize.cy))
              div 2;
            taRightJustify: y := TextSize.cx - 4;
          else
            y := Height - (Size.cy - TextSize.cy) - 2;
          end;
        end;
      fldUpDown:
        begin
          case FCaptionAlignment of
            taCenter: y := (Height - Size.cx) div 2;
            taRightJustify: y := Height - Size.cx;
          else
            y := 1;
          end;
          x := TextSize.cy;
        end;

    end;
    //...CALC POSITION end

    ExtTextOutExt(Handle, x, y, GetClientRect, Capt,
      FCaptionStyle, fpoDelineatedText in FOptions,
      false, Self.Font.Color, FColors.Delineate,
      FColors.Highlight, FColors.Shadow,
      nil, nil, Self.Font);

  end;
  Canvas.Draw(0, 0, Image);

end;

procedure TJvgProgress.OnSmthChanged(Sender: TObject);
begin
  Repaint;
end;
//...______________________________________________PROPERTIES METHODS

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

procedure TJvgProgress.SetBevelBold(Value: boolean);
begin
  FBevelBold := Value;
  Repaint;
end;

procedure TJvgProgress.SetPercent(Value: TglPercent);
begin
  if FPercent = Value then
    exit;
  FPercent := Value;
  FGradientF.PercentFilling := FPercent;
end;

procedure TJvgProgress.SetCaptionAlignment(Value: TAlignment);
begin
  FCaptionAlignment := Value;
  Repaint;
end;

procedure TJvgProgress.SetCaptionDirection(Value: TglLabelDir);
begin
  FCaptionDirection := Value;
  Repaint;
end;

procedure TJvgProgress.SetCaptionStyle(Value: TglTextStyle);
begin
  FCaptionStyle := Value;
  Repaint;
end;

procedure TJvgProgress.SetStep(Value: integer);
begin
  FStep := Value;
  Repaint;
end;

procedure TJvgProgress.SetInterspace(Value: integer);
begin
  FInterspace := Value;
  Repaint;
end;

procedure TJvgProgress.SetOptions(Value: TglProgressOptions);
begin
  FOptions := Value;
  Repaint;
end;

end.
