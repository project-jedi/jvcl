{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransBtn.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTransBtn;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, CommCtrl,
  ExtCtrls, Menus, Forms, JvComponent;

type
  TNumGlyphs = 1..4;
  TJvFrameStyle = (fsRegular, fsIndent, fsExplorer, fsNone, fsLight, fsDark, fsMono);
  TJvButtonState = (bsUp, bsDown, bsExclusive);
  TJvTextAlign = (ttaTopLeft, ttaTop, ttaTopRight, ttaRight, ttaBottomRight,
    ttaBottom, ttaBottomLeft, ttaLeft, ttaCenter);

type
  TJvTransparentButton = class(TJvGraphicControl)
  private
    FTextAlign: TJvTextAlign;
    FCaption: TCaption;
    FAutoGray: Boolean;
    FTransparent: Boolean;
    FMouseInside: Boolean;
    FShowPressed: Boolean;
    FOffset: integer;
    FSpacing: integer;
    FGlyph: TBitmap;
    FGrayGlyph: TBitmap;
    FDropDownMenu: TPopUpMenu;
    FDisabledGlyph: TBitmap;
    FState: TJvButtonState;
    FBorderSize: Cardinal;
    FNumGlyphs: TNumGlyphs;
    ImList: TImageList;
    FOutline: TJvFrameStyle;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FInsideButton: Boolean;
    FWordWrap: Boolean;
    FStayDown: Boolean;
    FFlat: Boolean;
    FPattern: TBitmap;
    procedure SetFlat(Value: boolean);
    procedure SetStayDown(Value: boolean);
    procedure SetWordWrap(Value: boolean);
    procedure SetSpacing(Value: integer);
    procedure SetAutoGray(Value: boolean);
    procedure SetTextAlign(Value: TJvTextAlign);
    procedure SetCaption(Value: TCaption);
    procedure SetGlyph(Bmp: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetFrameStyle(Value: TJvFrameStyle);
    procedure SetTransparent(Value: boolean);
    procedure SetBorderWidth(Value: Cardinal);
    procedure GlyphChanged(Sender: TObject);
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddGlyphs(aGlyph: TBitmap; aColor: TColor; Value: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; virtual;
    procedure MouseExit; virtual;
    procedure Paint; override;
    procedure PaintButton(Canvas: TCanvas); virtual;
    procedure PaintFrame(Canvas: TCanvas); virtual;
    procedure DrawTheText(aRect: TRect; Canvas: TCanvas); virtual;
    procedure DrawTheBitmap(aRect: TRect; Canvas: TCanvas); virtual;
    function InsideBtn(X, Y: Integer): boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas;
  published
    property Align;
    property AutoGray: boolean read FAutoGray write SetAutoGray default True;
    property BorderWidth: Cardinal read FBorderSize write SetBorderWidth default 1;
    property Caption: TCaption read FCaption write SetCaption;
    property Color;
    property Down: boolean read FStayDown write SetStayDown default False;
    property Enabled;
    property Font;
    property Flat: boolean read FFlat write SetFlat default true;
    property FrameStyle: TJvFrameStyle read FOutline write SetFrameStyle default fsExplorer;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property Offset: integer read FOffset write FOffset default 1;
    property ParentFont;
    property ParentShowHint;
    property DropDownMenu: TPopUpMenu read FDropDownMenu write FDropDownMenu;
    property PopupMenu;
    property ShowHint;
    property ShowPressed: boolean read FShowPressed write FShowPressed default True;
    property Spacing: integer read FSpacing write SetSpacing default 2;
    property TextAlign: TJvTextAlign read FTextAlign write SetTextAlign default ttaCenter;
    property Transparent: boolean read FTransparent write SetTransparent default True;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap default False;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnStartDrag;
    property OnDragDrop;
    property OnEndDrag;
    property OnDragOver;
  end;

implementation

{ create a grayed version of a color bitmap }
{ SLOW! don't use in realtime! }

procedure MonoBitmap(Bmp: TBitmap; R, G, B: integer);
var i, j: integer; col: longint;
begin
  if Bmp.Empty then
    Exit;
  for i := 0 to Bmp.Width do
    for j := 0 to Bmp.Height do
    begin
      Col := Bmp.Canvas.Pixels[i, j];
      Col := (GetRValue(Col) * R + GetGValue(Col) * G + GetBValue(Col) * B) div (R + G + B);
      Bmp.Canvas.Pixels[i, j] := RGB(Col, Col, Col);
    end;
end;

{ create a disabled bitmap from a regular one, works best when bitmap has been
reduced to a few colors. Used by BWBitmap }

procedure DisabledBitmap(Bmp: TBitmap);
const ROP_DSPDxax = $00E20746;
var MonoBmp, TmpImage: TBitmap;
  W, H: integer;
begin
  if Bmp.Empty then
    Exit;
  MonoBmp := TBitmap.Create;
  TmpImage := TBitmap.Create;
  W := Bmp.Width;
  H := Bmp.Height;

  with TmpImage do
  begin
    Width := W;
    Height := H;
    Canvas.Brush.Color := clBtnFace;
  end;

  try
    with MonoBmp do
    begin
      Assign(Bmp);
      Canvas.Font.Color := clWhite;
      Canvas.Brush.Color := clBlack;
      Monochrome := True;
    end;

    with TmpImage.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, W, H));
      Brush.Color := clBtnHighLight;
      SetTextColor(Handle, clBlack);
      SetBkColor(Handle, clWhite);
      BitBlt(Handle, 1, 1, W + 1, H + 1, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      Brush.Color := clBtnShadow;
      SetTextColor(Handle, clBlack);
      SetBkColor(Handle, clWhite);
      BitBlt(Handle, 0, 0, W, H, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    Bmp.Assign(TmpImage);
  finally
    MonoBmp.Free;
    TmpImage.Free;
  end;
end;

{ create a disabled bitmap by changing all colors to either black or tCol and then
  running it through DisabledBitmap }
{ SLOW! don't use in realtime! }

procedure BWBitmap(Bmp: TBitmap);
var i, j, W, H: integer; tcol: TColor; col: longint;
begin
  if Bmp.Empty then
    Exit;

  W := Bmp.Width;
  H := Bmp.Height;
  tCol := Bmp.Canvas.Pixels[0, 0];

  for i := 0 to W do
    for j := 0 to H do
    begin
      Col := Bmp.Canvas.Pixels[i, j];
      if (Col <> clWhite) and (Col <> tCol) then
        Col := clBlack
      else
        Col := tCol;
      Bmp.Canvas.Pixels[i, j] := Col;
    end;
  DisabledBitmap(Bmp);
end;

function CreateBrushPattern: TBitmap;
var X, Y: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := 8; { must have this size }
  Result.Height := 8;
  with Result.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Result.Width, Result.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
          Pixels[X, Y] := clWhite; { on even/odd rows }
  end;
end;

constructor TJvTransparentButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (csOpaque in ControlStyle) then
    ControlStyle := ControlStyle - [csOpaque];
  if (csDoubleClicks in ControlStyle) then
    ControlStyle := ControlStyle - [csDoubleClicks];
  FNumGlyphs := 1;
  FState := bsUp;
  FMouseInside := False;
  FAutoGray := True;
  FShowPressed := True;
  FOffset := 1;
  FBorderSize := 1;
  FStayDown := False;
  SetBounds(0, 0, 40, 40);
  FTransparent := True;

  ImList := TImageList.CreateSize(Width, Height);
  FGlyph := TBitmap.Create;
  FGrayGlyph := TBitmap.Create;
  FDisabledGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;

  FNumGlyphs := 1;
  FSpacing := 2;
  FTextAlign := ttaCenter;
  FInsideButton := False;
  FWordwrap := False;
  FOutline := fsExplorer;
  FFlat := true;
  FPattern := CreateBrushPattern;
end;

destructor TJvTransparentButton.Destroy;
begin
  FGlyph.Free;
  FGrayGlyph.Free;
  FDisabledGlyph.Free;
  ImList.Free;
  FPattern.Free;
  inherited Destroy;
end;

procedure TJvTransparentButton.AddGlyphs(aGlyph: TBitmap; aColor: TColor; Value: integer);
var Bmp: TBitmap; i, TmpWidth: integer; Dest, Source: TRect;
begin
  Bmp := TBitmap.Create;
  try
    if aGlyph.Empty then
      Exit;
    if not aGlyph.Empty then
    begin
      { destroy old list }
      ImList.Clear;
      TmpWidth := aGlyph.Width div FNumGlyphs;
      ImList.Width := TmpWidth;
      ImList.Height := aGlyph.Height;
      Bmp.Width := ImList.Width;
      Bmp.Height := ImList.Height;
      Dest := Rect(0, 0, Bmp.Width, Bmp.Height);
      { create the imagelist }
      for i := 0 to FNumGlyphs - 1 do
      begin
        Source := Rect(i * Bmp.Width, 0, i * Bmp.Width + Bmp.Width, Bmp.Height);
        Bmp.Canvas.CopyRect(Dest, aGlyph.Canvas, Source);
        if i = 0 then { first picture }
        begin
          { create the disabled and grayed bitmaps too }
          FGrayGlyph.Assign(Bmp);
          MonoBitmap(FGrayGlyph, 11, 59, 30);
          FDisabledGlyph.Assign(Bmp);
          BWBitmap(FDisabledGlyph);
        end;
        ImList.AddMasked(Bmp, Bmp.TransparentColor);
      end;
      { add last }
      ImList.AddMasked(FGrayGlyph, FGrayGlyph.TransparentColor);
      ImList.AddMasked(FDisabledGlyph, FDisabledGlyph.TransparentColor);
    end;
  finally
    Bmp.Free;
  end;
  Invalidate;
end;

procedure TJvTransparentButton.SetGlyph(Bmp: TBitmap);
begin
  FGlyph.Assign(Bmp);
  Invalidate;
end;

procedure TJvTransparentButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetFrameStyle(Value: TJvFrameStyle);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetTransparent(Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetBorderWidth(Value: Cardinal);
begin
  if FBorderSize <> Value then
  begin
    FBorderSize := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetFlat(Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;

end;

procedure TJvTransparentButton.SetStayDown(Value: boolean);
begin
  if FStayDown <> Value then
  begin
    FStayDown := Value;
    if FStayDown then
    begin
      FState := bsDown;
      {     Click; }{ uncomment and see what happens... }
    end
    else
      FState := bsUp;
    Repaint;
  end;
end;

procedure TJvTransparentButton.SetWordWrap(Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordwrap := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetSpacing(Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetAutoGray(Value: boolean);
begin
  if FAutoGray <> Value then
  begin
    FAutoGray := Value;
    Invalidate;
  end;

end;

procedure TJvTransparentButton.SetTextAlign(Value: TJvTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Invalidate;
  end;
end;

function TJvTransparentButton.InsideBtn(X, Y: Integer): boolean;
begin
  Result := PtInRect(Rect(0, 0, Width, Height), Point(X, Y));
end;

{ paint everything but bitmap and text }

procedure TJvTransparentButton.Paint;
begin
  { repaint rest }
  PaintFrame(Canvas);
  PaintButton(Canvas);
end;

procedure TJvTransparentButton.PaintFrame(Canvas: TCanvas);
var TmpRect: TRect;
  aCanvas: TCanvas;
  FDrawIt: boolean;
begin
  aCanvas := Canvas;
  TmpRect := Rect(0, 0, Width, Height);
  { draw the outline }
  with aCanvas do
  begin
    Brush.Color := Color;
    Pen.Color := clBlack;
    Pen.Width := BorderWidth;

    if not Transparent then
      FillRect(TmpRect);

    if (FState = bsDown) then
    begin
      case FrameStyle of
        fsRegular:
          if ShowPressed then
          begin
            Frame3D(aCanvas, TmpRect, clBlack, clBtnHighLight, BorderWidth);
            Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnFace, BorderWidth);
          end;
        fsExplorer:
          if FInsideButton or FStayDown then
          begin
            if ShowPressed then
              Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnHighLight, BorderWidth)
            else
              Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, BorderWidth);
          end;
        fsIndent:
          if ShowPressed then
          begin
            Frame3D(aCanvas, TmpRect, clBlack, clBtnHighLight, BorderWidth);
            Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnFace, BorderWidth);
          end;
        fsLight:
          if ShowPressed then
            Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnHighLight, BorderWidth);
        fsDark:
          if ShowPressed then
            Frame3D(aCanvas, TmpRect, cl3DDkShadow, clBtnFace, BorderWidth);
        fsMono:
          if ShowPressed then
            Frame3D(aCanvas, TmpRect, cl3DDkShadow, clBtnHighLight, BorderWidth);
      end; { case }
    end
    else if (FState = bsUp) then
    begin
      FDrawIt := (FInsideButton and FFlat) or not FFlat or (csDesigning in ComponentState);
      case FrameStyle of
        fsNone:
          if csDesigning in ComponentState then
            Frame3D(aCanvas, TmpRect, clBlack, clBlack, 1);
        fsRegular:
          if FDrawIt then
          begin
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBlack, BorderWidth);
            Frame3D(aCanvas, TmpRect, RGB(223, 223, 223), clBtnShadow, BorderWidth);
          end;
        fsExplorer:
          if FInsideButton or (csDesigning in ComponentState) then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, BorderWidth);
        fsIndent:
          if FDrawIt then
          begin
            Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnHighLight, BorderWidth);
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, BorderWidth);
          end;
        fsLight:
          if FDrawIt then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, BorderWidth);
        fsDark:
          if FDrawIt then
            Frame3D(aCanvas, TmpRect, clBtnFace, cl3DDkShadow, BorderWidth);
        fsMono:
          if FDrawIt then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, cl3DDkShadow, BorderWidth);
      end; { case }
    end;
  end; { with Canvas do }
end;

procedure TJvTransparentButton.PaintButton(Canvas: TCanvas);
var Dest: TRect; TmpWidth: integer;
begin
  with Canvas do
  begin
    { find glyph bounding rect - adjust according to textalignment}
    TmpWidth := ImList.Width;
    if TmpWidth <= 0 then
      TmpWidth := FGlyph.Width;

    { do top }
    if TextAlign in [ttaBottomLeft, ttaBottom, ttaBottomRight] then
      Dest.Top := Spacing
    else if TextAlign in [ttaTopLeft, ttaTop, ttaTopRight] then
      Dest.Top := Height - ImList.Height - Spacing
    else
      Dest.Top := (Height - ImList.Height) div 2;

    { do left }
    if TextAlign = ttaLeft then
      Dest.Left := Width - TmpWidth - Spacing
    else if TextAlign = ttaRight then
      Dest.Left := Spacing
    else { left, center, right }
      Dest.Left := (Width - TmpWidth) div 2;
    Dest.Bottom := Dest.Top + ImList.Height;
    Dest.Right := Dest.Left + TmpWidth;

    if not FGlyph.Empty then
    begin
      DrawTheBitmap(Dest, Canvas);
      FGlyph.Dormant;
    end;
    { finally, do the caption }
    if Length(FCaption) > 0 then
      DrawTheText(Dest, Canvas);
  end;
end;

{ just like DrawText, but draws disabled instead }

function DrawDisabledText(hDC: integer; lpString: PChar; nCount: integer; var lpRect: TRect; uFormat: integer): integer;
var oldCol: integer;
begin
  oldCol := SetTextColor(hDC, ColorToRGB(clBtnHighLight));
  OffsetRect(lpRect, 1, 1);
  DrawText(hDC, lpString, nCount, lpRect, uFormat);
  OffsetRect(lpRect, -1, -1);
  SetTextColor(hDC, ColorToRGB(clBtnShadow));
  Result := DrawText(hDC, lpString, nCount, lpRect, uFormat);
  SetTextColor(hDC, oldCol);
end;

{ aRect contains the bitmap bounds }

procedure TJvTransparentButton.DrawTheText(aRect: TRect; Canvas: TCanvas);
var Flags, MidX, MidY: Integer; DC: THandle; { Col:TColor; }
  tmprect: TRect;
begin

  Canvas.Font := Self.Font;
  DC := Canvas.Handle; { reduce calls to GetHandle }

  if FWordWrap then
    Flags := DT_WORDBREAK
  else
    Flags := DT_SINGLELINE;

  tmpRect := Rect(0, 0, Width, Height);

  { calculate width and height of text: }
  DrawText(DC, PChar(FCaption), Length(FCaption), tmpRect, Flags or DT_CALCRECT);
  MidY := tmpRect.Bottom - tmpRect.Top;
  MidX := tmpRect.Right - tmpRect.Left;
  Flags := DT_CENTER;
  { div 2 and shr 1 generates the exact same Asm code... }
  case TextAlign of
    ttaTop:
      OffsetRect(tmpRect, Width div 2 - MidX div 2, aRect.Top - MidY - Spacing);
    ttaTopLeft:
      OffsetRect(tmpRect, Spacing, aRect.Top - MidY - Spacing);
    ttaTopRight:
      OffsetRect(tmpRect, Width - tmpRect.right - Spacing, aRect.Top - MidY - Spacing);
    ttaBottom:
      OffsetRect(tmpRect, Width div 2 - MidX div 2, aRect.Bottom + Spacing);
    ttaBottomLeft:
      OffsetRect(tmpRect, Spacing, aRect.Bottom + Spacing);
    ttaBottomRight:
      OffsetRect(tmpRect, Width - MidX - Spacing, aRect.Bottom + Spacing);
    ttaCenter:
      OffsetRect(tmpRect, Width div 2 - MidX div 2, Height div 2 - MidY div 2);
    ttaRight:
      OffsetRect(tmpRect, Width - MidX - Spacing, Height div 2 - MidY div 2);
    ttaLeft:
      OffsetRect(tmpRect, Spacing, Height div 2 - MidY div 2);
  end; { case }
  if FWordWrap then
    Flags := Flags or DT_WORDBREAK or DT_NOCLIP
  else
    Flags := Flags or DT_SINGLELINE or DT_NOCLIP;

  if ((FState = bsDown) and FShowPressed) then
    OffsetRect(tmpRect, FOffset, FOffset);

  SetBkMode(DC, Windows.TRANSPARENT);

  if not Enabled then
    DrawDisabledText(DC, PChar(FCaption), -1, tmpRect, Flags)
  else
  begin
    SetTextColor(DC, self.Font.Color);
    DrawText(DC, PChar(FCaption), -1, tmpRect, Flags);
  end;
end;

procedure TJvTransparentButton.DrawTheBitmap(aRect: TRect; Canvas: TCanvas);
var index: integer;
  HelpRect: TRect;
begin
  with ImList do
  begin
    Index := 0;

    case FNumGlyphs of {normal,disabled,down,down }
      2: if not Enabled then
          Index := 1;
      3: if not Enabled then
          Index := 1
        else if (FState = bsDown) or FStayDown then
          Index := 2;
      4: if not Enabled then
          Index := 1
        else if (FState = bsDown) or FStayDown then
          Index := 2;
    else
      Index := 0;
    end; { case }

    if FGlyph.Empty then
      Exit;

    if ((FState = bsDown) and FShowPressed) or FStayDown then
      OffsetRect(aRect, FOffset, FOffset);
    { do we need the grayed bitmap ? }
    if (FFlat or (FrameStyle = fsExplorer)) and FAutoGray and not FInsideButton and not FStayDown then
      Index := Count - 2;

    { do we need the disabled bitmap ? }
    if not Enabled and (FNumGlyphs = 1) then
      Index := Count - 1;

    { Norris }
    if FInsideButton and FStayDown then
    begin
      HelpRect := ClientRect;
      InflateRect(HelpRect, -BorderWidth - 1, -BorderWidth - 1);
      Canvas.Brush.Bitmap := FPattern;
      Self.Canvas.FillRect(HelpRect);
    end;

    ImageList_DrawEx(Handle, Index, Canvas.Handle, aRect.Left, aRect.Top, 0, 0,
      clNone, clNone, ILD_Transparent);
  end; { with ImList do }
end;

procedure TJvTransparentButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp: TPoint; Msg: TMsg;
begin
  if not Enabled then
    Exit;

  inherited MouseDown(Button, Shift, X, Y);

  //   if Assigned(OnMouseDown) then OnMouseDown(Self,Button,Shift,X,Y);

  if InsideBtn(X, Y) then
  begin
    FState := bsDown;
    Repaint;
  end;
  if Assigned(FDropDownMenu) and (Button = mbLeft) then
  begin
    { calc where to put menu }
    tmp := ClientToScreen(Point(0, Height));
    FDropDownMenu.Popup(tmp.X, tmp.Y);
    { wait 'til menu is done }
    while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      ;
    { release button }
    MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TJvTransparentButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then
    Exit;
  if FStayDown then
    Exit;
  inherited MouseUp(Button, Shift, X, Y);

  FState := bsUp;
  Repaint;
  //  if Assigned(OnMouseUp) then OnMouseUp(Self,Button,Shift,X,Y);
end;

procedure TJvTransparentButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  //   if Assigned(OnMouseMove) then OnMouseMove(Self,Shift,X,Y);
  if FState = bsDown then
  begin
    if not InsideBtn(X, Y) then
    begin
      if FState = bsDown then { mouse has slid off, so release }
      begin
        FState := bsUp;
        Repaint;
      end;
    end
    else
    begin
      if FState = bsUp then { mouse has slid back on, so push }
      begin
        FState := bsDown;
        Repaint;
      end;
    end;
  end;
end;

procedure TJvTransparentButton.GlyphChanged(Sender: TObject);
var GlyphNum: integer;
begin
  Invalidate;
  GlyphNum := 1;
  if (Glyph <> nil) and (Glyph.Height > 0) then
  begin
    if Glyph.Width mod Glyph.Height = 0 then
    begin
      GlyphNum := Glyph.Width div Glyph.Height;
      if GlyphNum > 4 then
        GlyphNum := 1;
      SetNumGlyphs(GlyphNum);
    end;
    AddGlyphs(Glyph, Glyph.TransparentColor {Glyph.Canvas.Pixels[0,Height]}, GlyphNum);
  end;
end;

{ Handle speedkeys (Alt + key) }

procedure TJvTransparentButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FCaption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TJvTransparentButton.CMEnabledChanged(var Message: TMessage);
begin
  if not (Enabled) then
  begin
    FState := bsUp;
    FInsideButton := False;
  end;
  Repaint;
end;

procedure TJvTransparentButton.CMMouseEnter(var msg: TMessage);
begin
  MouseEnter;
end;

procedure TJvTransparentButton.CMMouseLeave(var msg: TMessage);
begin
  MouseExit;
end;

procedure TJvTransparentButton.MouseEnter;
begin
  if Enabled then
  begin
    FInsideButton := True;
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
    if FFlat then
      Refresh;
  end;
end;

procedure TJvTransparentButton.MouseExit;
begin
  if Enabled then
  begin
    if FInsideButton then
      FInsideButton := False;
    if Assigned(FOnMouseExit) then
      FOnMouseExit(Self);
    if FFlat then
      Refresh;
  end;
end;

procedure TJvTransparentButton.Click;
begin
  inherited Click;
end;

procedure TJvTransparentButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
end;

end.

