{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransBtn2.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTransBtn2;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, CommCtrl,
  ExtCtrls, Menus, Forms, ImgList, JvComponent;

type
  TJvFrameStyle = (fsRegular, fsIndent, fsExplorer, fsNone, fsLight, fsDark, fsMono);
  TJvButtonState = (bsUp, bsDown, bsExclusive);
  TJvTextAlign = (ttaTopLeft, ttaTop, ttaTopRight, ttaRight, ttaBottomRight,
    ttaBottom, ttaBottomLeft, ttaLeft, ttaCenter);

type
  TJvTransparentButton2 = class(TJvGraphicControl)
  private
    FIsDown: Boolean;
    FTextAlign: TJvTextAlign;
    FCaption: TCaption;
    FAutoGray: Boolean;
    FTransparent: Boolean;
    FMouseDown: Boolean;
    FMouseInside: Boolean;
    FShowPressed: Boolean;
    FSpacing: integer;
    FPopUpMenu: TPopUpMenu;
    FState: TJvButtonState;
    FBorderSize: Cardinal;
    FGrayList: TImageList;
    FActiveList: TImageList;
    FDisabledList: TImageList;
    FDownList: TImageList;
    FGrayLink: TChangeLink;
    FActiveLink: TChangeLink;
    FDisabledLink: TChangeLink;
    FDownLink: TChangeLink;
    FGrayIndex: integer;
    FActiveIndex: integer;
    FDisabledIndex: integer;
    FDownIndex: integer;
    ImList: TImageList;
    FOutline: TJvFrameStyle;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FInsideButton: Boolean;
    FWordWrap: Boolean;
    FStayDown: Boolean;
    { ... }
 {    Norris}
    FPattern: TBitmap; {Fill pattern when button is set to stay down}
    FHiFont: TFont;
    procedure SetHiFont(Value: TFont);
    procedure SetAutoGray(Value: boolean);
    procedure SetGrayList(Value: TImageList);
    procedure SetActiveList(Value: TImageList);
    procedure SetDisabledList(Value: TImageList);
    procedure SetDownList(Value: TImageList);
    procedure SetGrayIndex(Value: integer);
    procedure SetActiveIndex(Value: integer);
    procedure SetDisabledIndex(Value: integer);
    procedure SetDownIndex(Value: integer);
    procedure SetStayDown(Value: boolean);
    procedure SetWordWrap(Value: boolean);
    procedure SetSpacing(Value: integer);
    procedure SetTextAlign(Value: TJvTextAlign);
    procedure SetCaption(Value: TCaption);
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
    procedure AddGlyphs;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintButton(aCanvas: TCanvas); virtual;
    procedure PaintFrame(aCanvas: TCanvas); virtual;
    procedure DrawTheText(aRect: TRect; aCanvas: TCanvas); virtual;
    procedure DrawTheBitmap(aRect: TRect; aCanvas: TCanvas); virtual;
    function InsideBtn(X, Y: Integer): boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas;
    property InternalList: TImageList read ImList;
  published
    property Align;
    property Anchors;
    property Constraints;
    property AutoGray: boolean read FAutoGray write SetAutoGray default True;
    property BorderWidth: Cardinal read FBorderSize write SetBorderWidth default 1;
    property Caption: TCaption read FCaption write SetCaption;
    property Color;
    property Down: boolean read FStayDown write SetStayDown default False;
    property Enabled;
    property Font;
    property HiFont: TFont read FHiFont write SetHiFont;
    property FrameStyle: TJvFrameStyle read FOutline write SetFrameStyle default fsExplorer;
    property ActiveImage: TImageList read FActiveList write SetActiveList;
    property ActiveIndex: integer read FActiveIndex write SetActiveIndex default -1;
    property GrayImage: TImageList read FGrayList write SetGrayList;
    property GrayIndex: integer read FGrayIndex write SetGrayIndex default -1;
    property DisabledImage: TImageList read FDisabledList write SetDisabledList;
    property DisabledIndex: integer read FDisabledIndex write SetDisabledIndex default -1;
    property DownImage: TImageList read FDownList write SetDownList;
    property DownIndex: integer read FDownIndex write SetDownIndex default -1;
    property ParentFont;
    property ParentShowHint;
    property PopUpMenu: TPopUpMenu read FPopUpMenu write FPopUpMenu;
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
    { AL: }
  end;

implementation

{ create a grayed version of a color bitmap }
{ SLOW! don't use in realtime! }

procedure GrayBitmap(Bmp: TBitmap; R, G, B: integer);
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

{* create a disabled bitmap from a regular one, works best when bitmap has been
reduced to a few colors. Used by BWBitmap }

procedure ReducedColorDisabledBitmap(Bmp: TBitmap);
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

procedure DisabledBitmap(Bmp: TBitmap);
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
  ReducedColorDisabledBitmap(Bmp);
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

constructor TJvTransparentButton2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (csDoubleClicks in ControlStyle) then
    ControlStyle := ControlStyle - [csDoubleClicks];
  FHiFont := TFont.Create;
  FHiFont.Assign(Font);
  FState := bsUp;
  FMouseInside := False;
  FAutoGray := True;
  FShowPressed := True;
  FBorderSize := 1;
  FStayDown := False;
  SetBounds(0, 0, 40, 40);
  FTransparent := True;
  FGrayLink := TChangeLink.Create;
  FGrayLink.OnChange := GlyphChanged;
  FActiveLink := TChangeLink.Create;
  FActiveLink.OnChange := GlyphChanged;
  FDisabledLink := TChangeLink.Create;
  FDisabledLink.OnChange := GlyphChanged;
  FDownLink := TChangeLink.Create;
  FDownLink.OnChange := GlyphChanged;
  FActiveIndex := -1;
  FDisabledIndex := -1;
  FDownIndex := -1;
  FGrayIndex := -1;
  ImList := TImageList.CreateSize(Width, Height);
  FSpacing := 2;
  FMouseDown := False;
  FTextAlign := ttaCenter;
  FInsideButton := False;
  FWordwrap := False;
  FOutline := fsExplorer;
  FIsDown := False;
  { Norris }
  FPattern := CreateBrushPattern;
end;

destructor TJvTransparentButton2.Destroy;
begin
  FGrayLink.Free;
  FActiveLink.Free;
  FDisabledLink.Free;
  FDownLink.Free;
  ImList.Free;
  FPattern.Free;
  FHiFont.Free;
  inherited Destroy;
end;

procedure TJvTransparentButton2.AddGlyphs;
var Bmp: TBitmap; Ico: TIcon;
begin
  Bmp := TBitmap.Create;
  Ico := TIcon.Create;
  try
    { destroy old list }
    ImList.Clear;
    Repaint;
    { create the imagelist }
    if Assigned(FActiveList) and (FActiveIndex > -1) then
    begin
      ImList.Height := FActiveList.Height;
      ImList.Width := FActiveList.Width;
      Bmp.Height := ImList.Height;
      Bmp.Width := ImList.Width;
      FActiveList.GetIcon(FActiveIndex, Ico);
      ImList.AddIcon(Ico);
    end
    else
      Exit;

    if Assigned(FDisabledList) and (FDisabledIndex > -1) then
    begin
      FDisabledList.GetIcon(FDisabledIndex, Ico);
      ImList.AddIcon(Ico);
    end
    else
    begin
      FActiveList.GetBitmap(FActiveIndex, Bmp);
      DisabledBitmap(Bmp);
      ImList.AddMasked(Bmp, Bmp.TransparentColor);
    end;

    if Assigned(FDownList) and (FDownIndex > -1) then
    begin
      FDownList.GetIcon(FDownIndex, Ico);
      ImList.AddIcon(Ico);
    end
    else
    begin
      FActiveList.GetIcon(FActiveIndex, Ico);
      ImList.AddIcon(Ico);
    end;

    if Assigned(FGrayList) and (FGrayIndex > -1) then
    begin
      FGrayList.GetIcon(FGrayIndex, Ico);
      ImList.AddIcon(Ico);
    end
    else
    begin
      FActiveList.GetBitmap(FActiveIndex, Bmp);
      GrayBitmap(Bmp, 11, 59, 30);
      ImList.AddMasked(Bmp, Bmp.TransparentColor);
    end;

  finally
    Bmp.Free;
    Ico.Free;
  end;
  Repaint;
end;

procedure TJvTransparentButton2.SetHiFont(Value: TFont);
begin
  FHiFont.Assign(Value);
  Invalidate;
end;

procedure TJvTransparentButton2.SetAutoGray(Value: boolean);
begin
  if FAutoGray <> Value then
  begin
    FAutoGray := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetGrayList(Value: TImageList);
begin
  if FGrayList <> nil then
    FGrayList.UnRegisterChanges(FGrayLink);
  FGrayList := Value;

  if FGrayList <> nil then
    FGrayList.RegisterChanges(FGrayLink);
  AddGlyphs;
end;

procedure TJvTransparentButton2.SetActiveList(Value: TImageList);
begin
  if FActiveList <> nil then
    FActiveList.UnRegisterChanges(FActiveLink);
  FActiveList := Value;

  if FActiveList <> nil then
  begin
    ImList.Assign(FActiveList); // get properties
    ImList.BkColor := clNone;
    FActiveList.RegisterChanges(FActiveLink);
  end;
  AddGlyphs;
end;

procedure TJvTransparentButton2.SetDisabledList(Value: TImageList);
begin
  if FDisabledList <> nil then
    FDisabledList.UnRegisterChanges(FDisabledLink);
  FDisabledList := Value;

  if FDisabledList <> nil then
    FDisabledList.RegisterChanges(FDisabledLink);
  AddGlyphs;
end;

procedure TJvTransparentButton2.SetDownList(Value: TImageList);
begin
  if FDownList <> nil then
    FDownList.UnRegisterChanges(FDownLink);
  FDownList := Value;

  if FDownList <> nil then
    FDownList.RegisterChanges(FDownLink);
  AddGlyphs;
end;

procedure TJvTransparentButton2.SetGrayIndex(Value: integer);
begin
  if FGrayIndex <> Value then
  begin
    FGrayIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetActiveIndex(Value: integer);
begin
  if FActiveIndex <> Value then
  begin
    FActiveIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetDisabledIndex(Value: integer);
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetDownIndex(Value: integer);
begin
  if FDownIndex <> Value then
  begin
    FDownIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetFrameStyle(Value: TJvFrameStyle);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetTransparent(Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetCaption(Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetBorderWidth(Value: Cardinal);
begin
  if FBorderSize <> Value then
  begin
    FBorderSize := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetStayDown(Value: boolean);
begin
  if FStayDown <> Value then
  begin
    FStayDown := Value;
    if FStayDown then
    begin
      FMouseDown := True;
      FState := bsDown;
      {     Click; }{ uncomment and see what happens... }
    end
    else
    begin
      FMouseDown := False;
      FState := bsUp;
    end;
    Repaint;
  end;
end;

procedure TJvTransparentButton2.SetWordWrap(Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordwrap := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetSpacing(Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetTextAlign(Value: TJvTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Invalidate;
  end;
end;

function TJvTransparentButton2.InsideBtn(X, Y: Integer): boolean;
begin
  Result := PtInRect(Rect(0, 0, Width, Height), Point(X, Y));
end;

{ paint everything but bitmap and text }

procedure TJvTransparentButton2.Paint;
begin
  inherited Paint;
  PaintFrame(Canvas);
  PaintButton(Canvas);
end;

procedure TJvTransparentButton2.PaintFrame(aCanvas: TCanvas);
var TmpRect: TRect;
begin
  TmpRect := Rect(0, 0, Width, Height);
  { draw the outline }
  with aCanvas do
  begin
    Brush.Color := Color;
    Pen.Color := clBlack;
    Pen.Width := BorderWidth;

    case FrameStyle of
      fsNone:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, width, height));
          if (csDesigning in ComponentState) then
            Frame3D(aCanvas, TmpRect, clBlack, clBlack, 1);
        end;
      fsExplorer:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, width, height));
          if (csDesigning in ComponentState) then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, 1);
        end;
      fsRegular:
        begin
          { draw outline }
          Pen.Color := clBlack;
          if not Transparent then
            Rectangle(1, 1, Width, Height)
          else
          begin
            TmpRect := Rect(1, 1, Width, Height);
            Frame3D(aCanvas, TmpRect, clBlack, clBlack, BorderWidth);
          end;
        end;
      fsIndent:
        begin
          { draw outline }
          Pen.Color := clBtnShadow;
          if not Transparent then
            Rectangle(0, 0, Width - 1, Height - 1)
          else
          begin
            TmpRect := Rect(0, 0, Width - 1, Height - 1);
            Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnShadow, BorderWidth)
          end;
          TmpRect := Rect(1, 1, Width, Height);
          Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnHighLight, BorderWidth);
        end;
      fsLight:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, width, height));
          if (csDesigning in ComponentState) then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, 1);
        end;
      fsDark:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, width, height));
          if (csDesigning in ComponentState) then
            Frame3D(aCanvas, TmpRect, clBtnFace, cl3DDkShadow, 1);
        end;
      fsMono:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, width, height));
          if (csDesigning in ComponentState) then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, cl3DDkShadow, 1);
        end;
    end; { case }

    TmpRect := Rect(1, 1, Width - 1, Height - 1);

    if (FState = bsDown) then
    begin
      if not (FrameStyle = fsNone) then
      begin
        InflateRect(TmpRect, 1, 1);
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
              Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnHighLight, 1);
          fsDark:
            if ShowPressed then
              Frame3D(aCanvas, TmpRect, cl3DDkShadow, clBtnFace, 1);
          fsMono:
            if ShowPressed then
              Frame3D(aCanvas, TmpRect, cl3DDkShadow, clBtnHighLight, 1);
        end; { case }
      end;
    end;

    if (FState = bsUp) then
    begin
      InflateRect(TmpRect, 1, 1);
      case FrameStyle of
        fsRegular:
          begin
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBlack, BorderWidth);
            Frame3D(aCanvas, TmpRect, clBtnFace, clBtnShadow, BorderWidth);
          end;
        fsExplorer:
          if FInsideButton then
            Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, BorderWidth);

        fsIndent: Frame3D(aCanvas, TmpRect, clBtnShadow, clBtnHighLight, BorderWidth);
        fsLight: Frame3D(aCanvas, TmpRect, clBtnHighLight, clBtnShadow, 1);
        fsDark: Frame3D(aCanvas, TmpRect, clBtnFace, cl3DDkShadow, 1);
        fsMono: Frame3D(aCanvas, TmpRect, clBtnHighLight, cl3DDkShadow, 1);
      end; { case }
    end;
    if (FHiFont <> Font) and (FCaption <> '') then
    begin
      InflateRect(TmpRect, 1, 1);
      DrawTheText(TmpRect, aCanvas);
    end;

  end; { with Canvas do }
end;

procedure TJvTransparentButton2.PaintButton(aCanvas: TCanvas);
var Dest: TRect; TmpWidth: integer;
begin
  with aCanvas do
  begin
    TmpWidth := ImList.Width;

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

    {
        if Dest.Top < Spacing then Dest.Top := Spacing;
        if Dest.Left < Spacing then Dest.Left := Spacing;
    }

    Dest.Bottom := Dest.Top + ImList.Height;
    Dest.Right := Dest.Left + TmpWidth;

    {
        if Dest.Bottom > Height - Spacing then
           Dest.Top := Height - FGlyph.Height - Spacing;
    }

    if ImList.Count > 0 then
      DrawTheBitmap(Dest, aCanvas);
    { finally, do the caption }
    if Length(FCaption) > 0 then
      DrawTheText(Dest, aCanvas);
  end;
end;

{ aRect contains the bitmap bounds }

procedure TJvTransparentButton2.DrawTheText(aRect: TRect; aCanvas: TCanvas);
var Flags, MidX, MidY: Integer; DC: THandle; { Col:TColor; }
  tmprect: TRect;
begin

  if FInsideButton then
    aCanvas.Font := FHiFont
  else
    aCanvas.Font := self.Font;
  DC := aCanvas.Handle; { reduce calls to GetHandle }

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
    OffsetRect(tmpRect, 1, 1);

  SetBkMode(DC, Windows.TRANSPARENT);

  if not Enabled then
  begin
    SetTextColor(DC, ColorToRGB(clBtnHighLight));
    OffsetRect(tmpRect, 1, 1);
    DrawText(DC, PChar(FCaption), Length(FCaption), tmpRect, Flags);
    OffsetRect(tmpRect, -1, -1);
    SetTextColor(DC, ColorToRGB(clBtnShadow));
  end
  else if FInsideButton then
    SetTextColor(DC, FHiFont.Color)
  else
    SetTextColor(DC, self.Font.Color);

  DrawText(DC, PChar(FCaption), Length(FCaption), tmpRect, Flags);
end;

procedure TJvTransparentButton2.DrawTheBitmap(aRect: TRect; aCanvas: TCanvas);
var index: integer;
  {  HelpRect:TRect; }
begin
  if ImList.Count = 0 then
    Exit;

  with ImList do
  begin
    if not Enabled then
      Index := 1
    else if (FState = bsDown) then
      Index := 2
    else if (FrameStyle = fsExplorer) and FAutoGray and not FInsideButton then
      Index := 3 { autogray }
    else
      Index := 0; { active }

    if ((FState = bsDown) and FShowPressed) then
      OffsetRect(aRect, 1, 1);

    { Norris }
(*     if {FIsDown and }FStayDown and (FState = bsDown) then
    begin
      HelpRect := ClientRect;
      InflateRect(HelpRect, -2, -2);
      aCanvas.Brush.Bitmap := FPattern;
      aCanvas.FillRect(HelpRect);
    end;
*)
    ImList.Draw(Canvas, aRect.Left, aRect.Top, Index);
  end; { with ImList do }
end;

procedure TJvTransparentButton2.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp: TPoint; Msg: TMsg;
begin
  if not Enabled then
    Exit;

  if FIsDown then
    Exit
  else
    FIsDown := not FIsDown;

  inherited MouseDown(Button, Shift, X, Y);

  //   if Assigned(OnMouseDown) then OnMouseDown(Self,Button,Shift,X,Y);

  if InsideBtn(X, Y) then
  begin
    FMouseDown := True;
    FState := bsDown;
    Repaint;
  end;
  if Assigned(FPopUpMenu) then
  begin
    { calc where to put menu }
    tmp := ClientToScreen(Point(0, Height));
    FPopUpMenu.Popup(tmp.X, tmp.Y);
    { wait 'til menu is done }
    while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      ;
    { release button }
    MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TJvTransparentButton2.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if not Enabled then
    Exit;
  if not FIsDown then
    Exit
  else
    FIsDown := not FIsDown;
  if FStayDown then
    Exit;
  inherited MouseUp(Button, Shift, X, Y);

  FMouseDown := False;
  FState := bsUp;
  Repaint;
  //  if Assigned(OnMouseUp) then OnMouseUp(Self,Button,Shift,X,Y);
end;

procedure TJvTransparentButton2.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  //   if Assigned(OnMouseMove) then OnMouseMove(Self,Shift,X,Y);
  if FMouseDown then
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

procedure TJvTransparentButton2.GlyphChanged(Sender: TObject);
begin
  AddGlyphs;
end;

{ Handle speedkeys (Alt + key) }

procedure TJvTransparentButton2.CMDialogChar(var Message: TCMDialogChar);
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

procedure TJvTransparentButton2.CMEnabledChanged(var Message: TMessage);
begin
  if not (Enabled) then
  begin
    FState := bsUp;
    FMousedown := False;
    FIsDown := False;
    FInsideButton := False;
  end;
  Repaint;
end;

procedure TJvTransparentButton2.CMMouseEnter(var msg: TMessage);
begin
  if Enabled then
  begin
    FInsideButton := True;
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
    if (FrameStyle = fsExplorer) then
      Invalidate;
  end;
end;

procedure TJvTransparentButton2.CMMouseLeave(var msg: TMessage);
begin
  if Enabled then
  begin
    if FInsideButton then
      FInsideButton := False;
    if Assigned(FOnMouseExit) then
      FOnMouseExit(Self);
    if (FrameStyle = fsExplorer) then
      Invalidate;
  end;
end;

procedure TJvTransparentButton2.Click;
begin
  inherited Click;
end;

procedure TJvTransparentButton2.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if (AComponent = FPopUpMenu) then
      FPopUpMenu := nil;
    if (AComponent = FGrayList) then
      FGrayList := nil;
    if (AComponent = FActiveList) then
      FActiveList := nil;
    if (AComponent = FDisabledList) then
      FDisabledList := nil;
    if (AComponent = FDownList) then
      FDownList := nil;
  end;
end;

end.

