{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransparentButton.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTransparentButton;

interface

uses
  SysUtils, Classes,
  QWindows, QMessages, Types, QGraphics, QControls,
  QExtCtrls, QMenus, QForms, QImgList, QActnList, QButtons, 
  JvQComponent, JvQButton;

type
  TJvFrameStyle =
    (fsRegular, fsIndent, fsExplorer, fsNone, fsLight, fsDark, fsMono);
  TJvTextAlign = (ttaTopLeft, ttaTop, ttaTopRight, ttaRight, ttaBottomRight,
    ttaBottom, ttaBottomLeft, ttaLeft, ttaCenter);

type
  TJvTransparentButtonActionLink = class(TControlActionLink)
  protected
    FClient: TJvCustomGraphicButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override; 
    procedure SetChecked(Value: Boolean); override;
  end;

  TJvTransparentButton = class(TJvCustomGraphicButton)
  private
    FTextAlign: TJvTextAlign;
    FAutoGray: Boolean;
    FTransparent: Boolean;
    FShowPressed: Boolean;
    FOffset: Integer;
    FSpacing: Integer;
    FGlyph: TBitmap;
    FGrayGlyph: TBitmap;
    FDisabledGlyph: TBitmap;
    FBorderSize: Cardinal;
    FNumGlyphs: TNumGlyphs;
    FImList: TImageList;
    FOutline: TJvFrameStyle;
    FWordWrap: Boolean;
    procedure SetWordWrap(Value: Boolean);
    procedure SetSpacing(Value: Integer);
    procedure SetAutoGray(Value: Boolean);
    procedure SetTextAlign(Value: TJvTextAlign);
    procedure SetGlyph(Bmp: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetFrameStyle(Value: TJvFrameStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetBorderWidth(Value: Cardinal);
    procedure GlyphChanged(Sender: TObject);
  protected
    procedure AddGlyphs(aGlyph: TBitmap; AColor: TColor; Value: Integer);
    procedure PaintButton(Canvas: TCanvas); override;
    procedure PaintFrame(Canvas: TCanvas); override;
    procedure DrawTheText(ARect: TRect; Canvas: TCanvas); virtual;
    procedure DrawTheBitmap(ARect: TRect; Canvas: TCanvas); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean);override;
    function GetActionLinkClass: TControlActionLinkClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Action;
    property Align;
    property Anchors;
    property Constraints;

    property AutoGray: Boolean read FAutoGray write SetAutoGray default True;
    property BorderWidth: Cardinal read FBorderSize write SetBorderWidth default 1;
    property Caption;
    property Color;
    property DropDownMenu;
    property Down;
    property Enabled;
    property Font;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;

    property FrameStyle: TJvFrameStyle read FOutline write SetFrameStyle default fsExplorer;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property Offset: Integer read FOffset write FOffset default 1;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowPressed: Boolean read FShowPressed write FShowPressed default True;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property TextAlign: TJvTextAlign read FTextAlign write SetTextAlign default ttaCenter;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnStartDrag;
  end;

  TJvTransparentButton2 = class(TJvCustomGraphicButton)
  private
    FTextAlign: TJvTextAlign;
    FAutoGray: Boolean;
    FTransparent: Boolean;
    FShowPressed: Boolean;
    FSpacing: Integer;
    FBorderSize: Cardinal;
    FGrayList: TImageList;
    FActiveList: TImageList;
    FDisabledList: TImageList;
    FDownList: TImageList;
    FGrayLink: TChangeLink;
    FActiveLink: TChangeLink;
    FDisabledLink: TChangeLink;
    FDownLink: TChangeLink;
    FGrayIndex: Integer;
    FActiveIndex: Integer;
    FDisabledIndex: Integer;
    FDownIndex: Integer;
    FImList: TImageList;
    FOutline: TJvFrameStyle;
    FWordWrap: Boolean;
    procedure SetAutoGray(Value: Boolean);
    procedure SetGrayList(Value: TImageList);
    procedure SetActiveList(Value: TImageList);
    procedure SetDisabledList(Value: TImageList);
    procedure SetDownList(Value: TImageList);
    procedure SetGrayIndex(Value: Integer);
    procedure SetActiveIndex(Value: Integer);
    procedure SetDisabledIndex(Value: Integer);
    procedure SetDownIndex(Value: Integer);
    procedure SetWordWrap(Value: Boolean);
    procedure SetSpacing(Value: Integer);
    procedure SetTextAlign(Value: TJvTextAlign);
    procedure SetFrameStyle(Value: TJvFrameStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetBorderWidth(Value: Cardinal);
    procedure GlyphChanged(Sender: TObject);
  protected
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddGlyphs;
    procedure PaintButton(Canvas: TCanvas); override;
    procedure PaintFrame(Canvas: TCanvas); override;
    procedure DrawTheText(ARect: TRect; Canvas: TCanvas); virtual;
    procedure DrawTheBitmap(ARect: TRect; Canvas: TCanvas); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property InternalList: TImageList read FImList;
  published
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property AutoGray: Boolean read FAutoGray write SetAutoGray default True;
    property BorderWidth: Cardinal read FBorderSize write SetBorderWidth default 1;
    property Caption;
    property Color;
    property DropDownMenu;
    property Down;
    property Enabled;
    property Font;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;

    property FrameStyle: TJvFrameStyle read FOutline write SetFrameStyle default fsExplorer;
    property ActiveImage: TImageList read FActiveList write SetActiveList;
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex default -1;
    property GrayImage: TImageList read FGrayList write SetGrayList;
    property GrayIndex: Integer read FGrayIndex write SetGrayIndex default -1;
    property DisabledImage: TImageList read FDisabledList write SetDisabledList;
    property DisabledIndex: Integer read FDisabledIndex write SetDisabledIndex default -1;
    property DownImage: TImageList read FDownList write SetDownList;
    property DownIndex: Integer read FDownIndex write SetDownIndex default -1;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowPressed: Boolean read FShowPressed write FShowPressed default True;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property TextAlign: TJvTextAlign read FTextAlign write SetTextAlign default ttaCenter;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnClick;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnStartDrag;
  end;

implementation

uses
  JvQConsts;

{ create a grayed version of a color bitmap }
{ SLOW! don't use in realtime! }

procedure GrayBitmap(Bmp: TBitmap; R, G, B: Integer);
var
  I, J: Integer;
  Col: Longint;
begin
  if Bmp.Empty then
    Exit;
  for I := 0 to Bmp.Width do
    for J := 0 to Bmp.Height do
    begin
      Col := Bmp.Canvas.Pixels[I, J];
      Col := (GetRValue(Col) * R + GetGValue(Col) * G + GetBValue(Col) * B) div (R + G + B);
      Bmp.Canvas.Pixels[I, J] := RGB(Col, Col, Col);
    end;
end;

{ create a grayed version of a color bitmap }
{ SLOW! don't use in realtime! }

procedure MonoBitmap(Bmp: TBitmap; R, G, B: Integer);
var
  I, J: Integer;
  Col: Longint;
begin
  if Bmp.Empty then
    Exit;
  for I := 0 to Bmp.Width do
    for J := 0 to Bmp.Height do
    begin
      Col := Bmp.Canvas.Pixels[I, J];
      Col := (GetRValue(Col) * R + GetGValue(Col) * G + GetBValue(Col) * B) div (R + G + B);
      Bmp.Canvas.Pixels[I, J] := RGB(Col, Col, Col);
    end;
end;

{ create a disabled bitmap from a regular one, works best when bitmap has been
reduced to a few colors. Used by BWBitmap }

procedure DisabledBitmap(Bmp: TBitmap);
var
  MonoBmp, TmpImage: TBitmap;
  W, H: Integer;
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
      Brush.Color := clBtnHighlight;
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

{ create a disabled bitmap by changing all colors to either black or TopLeftCol and then
  running it through DisabledBitmap }
{ SLOW! don't use in realtime! }

procedure BWBitmap(Bmp: TBitmap);
var
  I, J, W, H: Integer;
  TopLeftCol: TColor;
  Col: Longint;
begin
  if Bmp.Empty then
    Exit;

  W := Bmp.Width;
  H := Bmp.Height;
  TopLeftCol := Bmp.Canvas.Pixels[0, 0];

  for I := 0 to W do
    for J := 0 to H do
    begin
      Col := Bmp.Canvas.Pixels[I, J];
      if (Col <> clWhite) and (Col <> TopLeftCol) then
        Col := clBlack
      else
        Col := TopLeftCol;
      Bmp.Canvas.Pixels[I, J] := Col;
    end;
  DisabledBitmap(Bmp);
end;

{ TJvTransparentButtonActionLink }

procedure TJvTransparentButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvCustomGraphicButton;
end;

function TJvTransparentButtonActionLink.IsCheckedLinked: Boolean;
begin
  if FClient is TJvTransparentButton then
    Result := inherited IsCheckedLinked and (TJvTransparentButton(FClient).Down = (Action as TCustomAction).Checked)
  else
  if FClient is TJvTransparentButton2 then
    Result := inherited IsCheckedLinked and (TJvTransparentButton2(FClient).Down = (Action as TCustomAction).Checked)
  else
    Result := false;
end;



procedure TJvTransparentButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
  begin
    if FClient is TJvTransparentButton then
      TJvTransparentButton(FClient).Down := Value
    else
    if FClient is TJvTransparentButton2 then
      TJvTransparentButton2(FClient).Down := Value;
  end;
end;

//=== TJvTransparentButton ===================================================

constructor TJvTransparentButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowAllUp := True;
  FNumGlyphs := 1;
  FAutoGray := True;
  FShowPressed := True;
  FOffset := 1;
  FBorderSize := 1;
  FTransparent := True;
  Flat := True;

  FImList := TImageList.Create(Self);
  FGlyph := TBitmap.Create;
  FGrayGlyph := TBitmap.Create;
  FDisabledGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;

  FNumGlyphs := 1;
  FSpacing := 2;
  FTextAlign := ttaCenter;
  FWordwrap := False;
  FOutline := fsExplorer;
end;

destructor TJvTransparentButton.Destroy;
begin
  FGlyph.Free;
  FGrayGlyph.Free;
  FDisabledGlyph.Free;
//  FImList.Free;
  inherited Destroy;
end;

procedure TJvTransparentButton.AddGlyphs(aGlyph: TBitmap; AColor: TColor; Value: Integer);
var
  Bmp: TBitmap;
  I, TmpWidth: Integer;
  Dest, Source: TRect;
begin
  Bmp := TBitmap.Create;
  try
    if aGlyph.Empty then
      Exit;
    if not aGlyph.Empty then
    begin
      { destroy old list }
      FImList.Clear;
      TmpWidth := aGlyph.Width div FNumGlyphs;
      FImList.Width := TmpWidth;
      FImList.Height := aGlyph.Height;
      Bmp.Width := FImList.Width;
      Bmp.Height := FImList.Height;
      Dest := Rect(0, 0, Bmp.Width, Bmp.Height);
      { create the imagelist }
      for I := 0 to FNumGlyphs - 1 do
      begin
        Source := Rect(I * Bmp.Width, 0, I * Bmp.Width + Bmp.Width, Bmp.Height);
        Bmp.Canvas.CopyRect(Dest, aGlyph.Canvas, Source);
        if I = 0 then { first picture }
        begin
          { create the disabled and grayed bitmaps too }
          FGrayGlyph.Assign(Bmp);
          MonoBitmap(FGrayGlyph, 11, 59, 30);
          FDisabledGlyph.Assign(Bmp);
          BWBitmap(FDisabledGlyph);
        end;
        FImList.AddMasked(Bmp, Bmp.TransparentColor);
      end;
      { add last }
      FImList.AddMasked(FGrayGlyph, FGrayGlyph.TransparentColor);
      FImList.AddMasked(FDisabledGlyph, FDisabledGlyph.TransparentColor);
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
    Flat := FTransparent;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Flat := FTransparent;
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

procedure TJvTransparentButton.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordwrap := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetAutoGray(Value: Boolean);
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

procedure TJvTransparentButton.PaintFrame(Canvas: TCanvas);
var
  TmpRect: TRect;
  FDrawIt: Boolean;
begin
  TmpRect := Rect(0, 0, Width, Height);
  { draw the outline }
  with Canvas do
  begin
    Brush.Color := Color;
    Pen.Color := clBlack;
    Pen.Width := BorderWidth;

    if not Transparent then
      FillRect(TmpRect);

    if (bsMouseDown in MouseStates) then
    begin
      case FrameStyle of
        fsRegular:
          if ShowPressed then
          begin
            Frame3D(Canvas, TmpRect, clBlack, clBtnHighlight, BorderWidth);
            Frame3D(Canvas, TmpRect, clBtnShadow, clBtnFace, BorderWidth);
          end;
        fsExplorer:
          if (bsMouseInside in MouseStates) or Down then
          begin
            if ShowPressed then
              Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, BorderWidth)
            else
              Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
          end;
        fsIndent:
          if ShowPressed then
          begin
            Frame3D(Canvas, TmpRect, clBlack, clBtnHighlight, BorderWidth);
            Frame3D(Canvas, TmpRect, clBtnShadow, clBtnFace, BorderWidth);
          end;
        fsLight:
          if ShowPressed then
            Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, BorderWidth);
        fsDark:
          if ShowPressed then
            Frame3D(Canvas, TmpRect, cl3DDkShadow, clBtnFace, BorderWidth);
        fsMono:
          if ShowPressed then
            Frame3D(Canvas, TmpRect, cl3DDkShadow, clBtnHighlight, BorderWidth);
      end;
    end
    else
    begin
      FDrawIt := ((bsMouseInside in MouseStates) and Transparent) or not Transparent or (csDesigning in ComponentState);
      case FrameStyle of
        fsNone:
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBlack, clBlack, 1);
        fsRegular:
          if FDrawIt then
          begin
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBlack, BorderWidth);
            Frame3D(Canvas, TmpRect, RGB(223, 223, 223), clBtnShadow, BorderWidth);
          end;
        fsExplorer:
          if (bsMouseInside in MouseStates) or (csDesigning in ComponentState) then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
        fsIndent:
          if FDrawIt then
          begin
            Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, BorderWidth);
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
          end;
        fsLight:
          if FDrawIt then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
        fsDark:
          if FDrawIt then
            Frame3D(Canvas, TmpRect, clBtnFace, cl3DDkShadow, BorderWidth);
        fsMono:
          if FDrawIt then
            Frame3D(Canvas, TmpRect, clBtnHighlight, cl3DDkShadow, BorderWidth);
      end;
    end;
  end;
end;

procedure TJvTransparentButton.PaintButton(Canvas: TCanvas);
var
  Dest: TRect;
  TmpWidth: Integer;
begin
  with Canvas do
  begin
    { find glyph bounding rect - adjust according to textalignment}
    TmpWidth := FImList.Width;
    if TmpWidth <= 0 then
      TmpWidth := FGlyph.Width;

    { do top }
    if Self.TextAlign in [ttaBottomLeft, ttaBottom, ttaBottomRight] then
      Dest.Top := Spacing
    else
    if Self.TextAlign in [ttaTopLeft, ttaTop, ttaTopRight] then
      Dest.Top := Height - FImList.Height - Spacing
    else
      Dest.Top := (Height - FImList.Height) div 2;

    { do left }
    if Self.TextAlign = ttaLeft then
      Dest.Left := Width - TmpWidth - Spacing
    else
    if Self.TextAlign = ttaRight then
      Dest.Left := Spacing
    else { left, center, right }
      Dest.Left := (Width - TmpWidth) div 2;
    Dest.Bottom := Dest.Top + FImList.Height;
    Dest.Right := Dest.Left + TmpWidth;

    if not FGlyph.Empty then
    begin
      DrawTheBitmap(Dest, Canvas);
      FGlyph.Dormant;
    end;
    { finally, do the caption }
    if Length(Caption) > 0 then
      DrawTheText(Dest, Canvas);
  end;
end;

{ just like DrawText, but draws disabled instead }



function DrawDisabledText(DC: HDC; lpString: PWideChar;
  nCount: Integer; var lpRect: TRect; uFormat: Integer): Integer;

var
  OldCol: Integer;
begin
  OldCol := SetTextColor(DC, ColorToRGB(clBtnHighlight));
  OffsetRect(lpRect, 1, 1);  
  DrawTextW(DC, lpString, nCount, lpRect, uFormat); 
  OffsetRect(lpRect, -1, -1);
  SetTextColor(DC, ColorToRGB(clBtnShadow));  
  Result := DrawTextW(DC, lpString, nCount, lpRect, uFormat); 
  SetTextColor(DC, OldCol);
end;

{ ARect contains the bitmap bounds }

procedure TJvTransparentButton.DrawTheText(ARect: TRect; Canvas: TCanvas);
var
  Flags, MidX, MidY: Integer;
  DC: HDC; { Col:TColor; }
  TmpRect: TRect;
begin
  if (bsMouseInside in MouseStates) and HotTrack then
    Canvas.Font := HotTrackFont
  else
    Canvas.Font := Self.Font;
  DC := Canvas.Handle; { reduce calls to GetHandle }

  if FWordWrap then
    Flags := DT_WORDBREAK
  else
    Flags := DT_SINGLELINE;

  TmpRect := Rect(0, 0, Width, Height);

  { calculate width and height of text: }  
  DrawText(Canvas, Caption, Length(Caption), TmpRect, Flags or DT_CALCRECT);
{
  if FWordWrap then
    Canvas.TextExtent(Caption, TmpRect, WordBreak)
  else
    Canvas.TextExtent(Caption, TmpRect, 0);
} 
  MidY := TmpRect.Bottom - TmpRect.Top;
  MidX := TmpRect.Right - TmpRect.Left;
  Flags := DT_CENTER;
  { div 2 and shr 1 generates the exact same Asm code... }
  case Self.TextAlign of
    ttaTop:
      OffsetRect(TmpRect, Width div 2 - MidX div 2, ARect.Top - MidY - Spacing);
    ttaTopLeft:
      OffsetRect(TmpRect, Spacing, ARect.Top - MidY - Spacing);
    ttaTopRight:
      OffsetRect(TmpRect, Width - TmpRect.Right - Spacing, ARect.Top - MidY - Spacing);
    ttaBottom:
      OffsetRect(TmpRect, Width div 2 - MidX div 2, ARect.Bottom + Spacing);
    ttaBottomLeft:
      OffsetRect(TmpRect, Spacing, ARect.Bottom + Spacing);
    ttaBottomRight:
      OffsetRect(TmpRect, Width - MidX - Spacing, ARect.Bottom + Spacing);
    ttaCenter:
      OffsetRect(TmpRect, Width div 2 - MidX div 2, Height div 2 - MidY div 2);
    ttaRight:
      OffsetRect(TmpRect, Width - MidX - Spacing, Height div 2 - MidY div 2);
    ttaLeft:
      OffsetRect(TmpRect, Spacing, Height div 2 - MidY div 2);
  end;
  if FWordWrap then
    Flags := Flags or DT_WORDBREAK or DT_NOCLIP
  else
    Flags := Flags or DT_SINGLELINE or DT_NOCLIP;

  if ((bsMouseDown in MouseStates) or Down) and FShowPressed then
    OffsetRect(TmpRect, FOffset, FOffset);
  
  if not Enabled then
    DrawDisabledText(DC, PWideChar(Caption), -1, TmpRect, Flags)
  else
  begin
    if (bsMouseInside in MouseStates) and HotTrack then
      SetTextColor(DC, ColorToRGB(HotTrackFont.Color))
    else
      SetTextColor(DC, ColorToRGB(Self.Font.Color));
    DrawText(Canvas, Caption, -1, TmpRect, Flags);
  end; 

end;

procedure TJvTransparentButton.DrawTheBitmap(ARect: TRect; Canvas: TCanvas);
var
  Index: Integer;
  HelpRect: TRect;
begin
  with FImList do
  begin
    Index := 0;

    case FNumGlyphs of {normal,disabled,down,down }
      2:
        if not Enabled then
          Index := 1;
      3:
        if not Enabled then
          Index := 1
        else
        if (bsMouseDown in MouseStates) or Down then
          Index := 2;
      4:
        if not Enabled then
          Index := 1
        else
        if (bsMouseDown in MouseStates) or Down then
          Index := 2;
    else
      Index := 0;
    end; { case }

    if FGlyph.Empty then
      Exit;

    if ((bsMouseDown in MouseStates) and FShowPressed) or Down then
      OffsetRect(ARect, FOffset, FOffset);
    { do we need the grayed bitmap ? }
    if (Flat or (FrameStyle = fsExplorer)) and FAutoGray and not (bsMouseInside in MouseStates) and not Down then
      Index := Count - 2;

    { do we need the disabled bitmap ? }
    if not Enabled and (FNumGlyphs = 1) then
      Index := Count - 1;

    { Norris }
    if (bsMouseInside in MouseStates) and Down then
    begin
      HelpRect := ClientRect;
      InflateRect(HelpRect, -BorderWidth - 1, -BorderWidth - 1);
      Canvas.Brush.Bitmap := Pattern;
      Self.Canvas.FillRect(HelpRect);
    end;  
    FImList.Draw(Canvas, ARect.Left, ARect.Top, Index); 
  end;
end;

procedure TJvTransparentButton.GlyphChanged(Sender: TObject);
var
  GlyphNum: Integer;
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

procedure TJvTransparentButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
    GlyphChanged(Glyph);
  end;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults then
        Down := Checked;
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;

end;

function TJvTransparentButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvTransparentButtonActionLink;
end;

//=== TJvTransparentButton2 ==================================================

constructor TJvTransparentButton2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowAllUp := True;
  FAutoGray := True;
  FShowPressed := True;
  FBorderSize := 1;
  FTransparent := True;
  Flat := True;
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
  FImList := TImageList.CreateSize(Width, Height);
  FSpacing := 2;
  FTextAlign := ttaCenter;
  FWordwrap := False;
  FOutline := fsExplorer;
end;

destructor TJvTransparentButton2.Destroy;
begin
  FGrayLink.Free;
  FActiveLink.Free;
  FDisabledLink.Free;
  FDownLink.Free;
  FImList.Free;
  inherited Destroy;
end;

procedure TJvTransparentButton2.AddGlyphs;
var
  Bmp: TBitmap; 
begin
  Bmp := TBitmap.Create;
  try
    { destroy old list }
    FImList.Clear;
//    Repaint;
    { create the imagelist }
    if Assigned(FActiveList) and (FActiveIndex > -1) then
    begin
      FImList.Height := FActiveList.Height;
      FImList.Width := FActiveList.Width;
      Bmp.Height := FImList.Height;
      Bmp.Width := FImList.Width;  
      FActiveList.GetBitmap(FActiveIndex, Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor); 
    end
    else
      Exit;

    if Assigned(FDisabledList) and (FDisabledIndex > -1) then
    begin  
      FDisabledList.GetBitmap(FDisabledIndex, Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor); 
    end
    else
    begin
      FActiveList.GetBitmap(FActiveIndex, Bmp);
      DisabledBitmap(Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor);
    end;

    if Assigned(FDownList) and (FDownIndex > -1) then
    begin  
      FDownList.GetBitmap(FDownIndex, Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor); 
    end
    else
    begin  
      FActiveList.GetBitmap(FActiveIndex, Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor); 
    end;

    if Assigned(FGrayList) and (FGrayIndex > -1) then
    begin  
      FGrayList.GetBitmap(FGrayIndex, Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor); 
    end
    else
    begin  
      FActiveList.GetBitmap(FActiveIndex, Bmp);
      FImList.AddMasked(Bmp, Bmp.TransparentColor); 
    end;
  finally
    Bmp.Free;
  end;
  Repaint;
end;

procedure TJvTransparentButton2.SetAutoGray(Value: Boolean);
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
    FImList.Assign(FActiveList); // get properties
    FImList.BkColor := clNone;
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

procedure TJvTransparentButton2.SetGrayIndex(Value: Integer);
begin
  if FGrayIndex <> Value then
  begin
    FGrayIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetActiveIndex(Value: Integer);
begin
  if FActiveIndex <> Value then
  begin
    FActiveIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetDisabledIndex(Value: Integer);
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButton2.SetDownIndex(Value: Integer);
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
    Flat := FTransparent;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Flat := FTransparent;
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

procedure TJvTransparentButton2.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordwrap := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton2.SetSpacing(Value: Integer);
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

{ paint everything but bitmap and text }

procedure TJvTransparentButton2.PaintFrame(Canvas: TCanvas);
var
  TmpRect: TRect;
begin
  TmpRect := Rect(0, 0, Width, Height);
  { draw the outline }
  with Canvas do
  begin
    Brush.Color := Color;
    Pen.Color := clBlack;
    Pen.Width := BorderWidth;

    case FrameStyle of
      fsNone:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, Width, Height));
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBlack, clBlack, 1);
        end;
      fsExplorer:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, Width, Height));
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, 1);
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
            Frame3D(Canvas, TmpRect, clBlack, clBlack, BorderWidth);
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
            Frame3D(Canvas, TmpRect, clBtnShadow, clBtnShadow, BorderWidth)
          end;
          TmpRect := Rect(1, 1, Width, Height);
          Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnHighlight, BorderWidth);
        end;
      fsLight:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, Width, Height));
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, 1);
        end;
      fsDark:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, Width, Height));
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBtnFace, cl3DDkShadow, 1);
        end;
      fsMono:
        begin
          if not Transparent then
            FillRect(Rect(0, 0, Width, Height));
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBtnHighlight, cl3DDkShadow, 1);
        end;
    end;

    TmpRect := Rect(1, 1, Width - 1, Height - 1);

    if bsMouseDown in MouseStates then
    begin
      if FrameStyle <> fsNone then
      begin
        InflateRect(TmpRect, 1, 1);
        case FrameStyle of
          fsRegular:
            if ShowPressed then
            begin
              Frame3D(Canvas, TmpRect, clBlack, clBtnHighlight, BorderWidth);
              Frame3D(Canvas, TmpRect, clBtnShadow, clBtnFace, BorderWidth);
            end;
          fsExplorer:
            if (bsMouseInside in MouseStates) or Down then
            begin
              if ShowPressed then
                Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, BorderWidth)
              else
                Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
            end;
          fsIndent:
            if ShowPressed then
            begin
              Frame3D(Canvas, TmpRect, clBlack, clBtnHighlight, BorderWidth);
              Frame3D(Canvas, TmpRect, clBtnShadow, clBtnFace, BorderWidth);
            end;
          fsLight:
            if ShowPressed then
              Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, 1);
          fsDark:
            if ShowPressed then
              Frame3D(Canvas, TmpRect, cl3DDkShadow, clBtnFace, 1);
          fsMono:
            if ShowPressed then
              Frame3D(Canvas, TmpRect, cl3DDkShadow, clBtnHighlight, 1);
        end;
      end;
    end;

    if not (bsMouseDown in MouseStates) then
    begin
      InflateRect(TmpRect, 1, 1);
      case FrameStyle of
        fsRegular:
          begin
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBlack, BorderWidth);
            Frame3D(Canvas, TmpRect, clBtnFace, clBtnShadow, BorderWidth);
          end;
        fsExplorer:
          if bsMouseInside in MouseStates then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
        fsIndent:
          Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, BorderWidth);
        fsLight:
          Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, 1);
        fsDark:
          Frame3D(Canvas, TmpRect, clBtnFace, cl3DDkShadow, 1);
        fsMono:
          Frame3D(Canvas, TmpRect, clBtnHighlight, cl3DDkShadow, 1);
      end;
    end;
    //
    if (HotTrackFont <> Font) and (Caption <> '') then
    begin
      InflateRect(TmpRect, 1, 1);
      DrawTheText(TmpRect, Canvas);
    end;
  end;
end;

procedure TJvTransparentButton2.PaintButton(Canvas: TCanvas);
var
  Dest: TRect;
  TmpWidth: Integer;
begin
  with Canvas do
  begin
    TmpWidth := FImList.Width;

    { do top }
    if Self.TextAlign in [ttaBottomLeft, ttaBottom, ttaBottomRight] then
      Dest.Top := Spacing
    else
    if Self.TextAlign in [ttaTopLeft, ttaTop, ttaTopRight] then
      Dest.Top := Height - FImList.Height - Spacing
    else
      Dest.Top := (Height - FImList.Height) div 2;

    { do left }
    if Self.TextAlign = ttaLeft then
      Dest.Left := Width - TmpWidth - Spacing
    else
    if Self.TextAlign = ttaRight then
      Dest.Left := Spacing
    else { left, center, right }
      Dest.Left := (Width - TmpWidth) div 2;
    {
        if Dest.Top < Spacing then Dest.Top := Spacing;
        if Dest.Left < Spacing then Dest.Left := Spacing;
    }
    Dest.Bottom := Dest.Top + FImList.Height;
    Dest.Right := Dest.Left + TmpWidth;
    {
        if Dest.Bottom > Height - Spacing then
           Dest.Top := Height - FGlyph.Height - Spacing;
    }
    if FImList.Count > 0 then
      DrawTheBitmap(Dest, Canvas);
    { finally, do the caption }
    if Length(Caption) > 0 then
      DrawTheText(Dest, Canvas);
  end;
end;

{ ARect contains the bitmap bounds }

procedure TJvTransparentButton2.DrawTheText(ARect: TRect; Canvas: TCanvas);
var
  Flags, MidX, MidY: Integer;
  DC: HDC; { Col:TColor; }
  TmpRect: TRect;
begin
  if (bsMouseInside in MouseStates) and HotTrack then
    Canvas.Font := HotTrackFont
  else
    Canvas.Font := Self.Font;
  DC := Canvas.Handle; { reduce calls to GetHandle }

  if FWordWrap then
    Flags := DT_WORDBREAK
  else
    Flags := DT_SINGLELINE;

  TmpRect := Rect(0, 0, Width, Height);

  { calculate width and height of text: }  
  DrawText(Canvas, Caption, Length(Caption), TmpRect, Flags or DT_CALCRECT); 
  MidY := TmpRect.Bottom - TmpRect.Top;
  MidX := TmpRect.Right - TmpRect.Left;
  Flags := DT_CENTER;
  { div 2 and shr 1 generates the exact same Asm code... }
  case Self.TextAlign of
    ttaTop:
      OffsetRect(TmpRect, Width div 2 - MidX div 2, ARect.Top - MidY - Spacing);
    ttaTopLeft:
      OffsetRect(TmpRect, Spacing, ARect.Top - MidY - Spacing);
    ttaTopRight:
      OffsetRect(TmpRect, Width - TmpRect.Right - Spacing, ARect.Top - MidY - Spacing);
    ttaBottom:
      OffsetRect(TmpRect, Width div 2 - MidX div 2, ARect.Bottom + Spacing);
    ttaBottomLeft:
      OffsetRect(TmpRect, Spacing, ARect.Bottom + Spacing);
    ttaBottomRight:
      OffsetRect(TmpRect, Width - MidX - Spacing, ARect.Bottom + Spacing);
    ttaCenter:
      OffsetRect(TmpRect, Width div 2 - MidX div 2, Height div 2 - MidY div 2);
    ttaRight:
      OffsetRect(TmpRect, Width - MidX - Spacing, Height div 2 - MidY div 2);
    ttaLeft:
      OffsetRect(TmpRect, Spacing, Height div 2 - MidY div 2);
  end;
  if FWordWrap then
    Flags := Flags or DT_WORDBREAK or DT_NOCLIP
  else
    Flags := Flags or DT_SINGLELINE or DT_NOCLIP;

  if ((bsMouseDown in MouseStates)) and FShowPressed then
    OffsetRect(TmpRect, 1, 1);

  
  SetBkMode(DC, QWindows.TRANSPARENT); 
  if not Enabled then
  begin
    SetTextColor(DC, ColorToRGB(clBtnHighlight));
    OffsetRect(TmpRect, 1, 1);
  
    DrawText(Canvas, Caption, Length(Caption), TmpRect, Flags); 
    OffsetRect(TmpRect, -1, -1);
    SetTextColor(DC, ColorToRGB(clBtnShadow));
  end
  else
  if (bsMouseInside in MouseStates) and HotTrack then
    SetTextColor(DC, ColorToRGB(HotTrackFont.Color))
  else
    SetTextColor(DC, ColorToRGB(Self.Font.Color));
  
  DrawText(Canvas, Caption, Length(Caption), TmpRect, Flags); 
end;

procedure TJvTransparentButton2.DrawTheBitmap(ARect: TRect; Canvas: TCanvas);
var
  Index: Integer;
begin
  if FImList.Count = 0 then
    Exit;

  with FImList do
  begin
    if not Enabled then
      Index := 1
    else
    if (bsMouseDown in MouseStates) then
      Index := 2
    else
    if (FrameStyle = fsExplorer) and FAutoGray and (MouseStates = []) then
      Index := 3 { autogray }
    else
      Index := 0; { active }

    if (bsMouseDown in MouseStates) and FShowPressed then
      OffsetRect(ARect, 1, 1);

    FImList.Draw(Canvas, ARect.Left, ARect.Top, Index);
  end;
end;

procedure TJvTransparentButton2.GlyphChanged(Sender: TObject);
begin
  AddGlyphs;
end;

procedure TJvTransparentButton2.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FGrayList then
      GrayImage := nil;
    if AComponent = FActiveList then
      ActiveImage := nil;
    if AComponent = FDisabledList then
      DisabledImage := nil;
    if AComponent = FDownList then
      DownImage := nil;
  end;
end;

procedure TJvTransparentButton2.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults then
        Down := Checked;
      if not CheckDefaults or (ActiveIndex = -1) then
        ActiveIndex := ImageIndex;
    end;
end;

function TJvTransparentButton2.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvTransparentButtonActionLink;
end;

end.

