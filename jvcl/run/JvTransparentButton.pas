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
  Andreas Hausladen (refactored)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTransparentButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls,
  ExtCtrls, Menus, Forms, ImgList, ActnList, Buttons,
  CommCtrl,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvJCLUtils, JvButton;

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
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TJvTransparentButton = class;

  TJvTransparentButtonImages = class(TPersistent)
  private
    FButton: TJvTransparentButton;

    FGrayList: TCustomImageList;
    FActiveList: TCustomImageList;
    FDisabledList: TCustomImageList;
    FDownList: TCustomImageList;
    FHotList: TCustomImageList;
    FGrayLink: TChangeLink;
    FActiveLink: TChangeLink;
    FDisabledLink: TChangeLink;
    FDownLink: TChangeLink;
    FHotLink: TChangeLink;
    FGrayIndex: TImageIndex;
    FActiveIndex: TImageIndex;
    FDisabledIndex: TImageIndex;
    FDownIndex: TImageIndex;
    FHotIndex: TImageIndex;
    procedure SetGrayList(Value: TCustomImageList);
    procedure SetActiveList(Value: TCustomImageList);
    procedure SetDisabledList(Value: TCustomImageList);
    procedure SetDownList(Value: TCustomImageList);
    procedure SetHotList(Value: TCustomImageList);
    procedure SetGrayIndex(Value: TImageIndex);
    procedure SetActiveIndex(Value: TImageIndex);
    procedure SetDisabledIndex(Value: TImageIndex);
    procedure SetDownIndex(Value: TImageIndex);
    procedure SetHotIndex(Value: TImageIndex);
  protected
    procedure AddGlyphs;
    procedure SetImageList(var List: TCustomImageList; Link: TChangeLink;
      Value: TCustomImageList);
  public
    constructor Create(AButton: TJvTransparentButton);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ActiveImage: TCustomImageList read FActiveList write SetActiveList;
    property ActiveIndex: TImageIndex read FActiveIndex write SetActiveIndex default -1;
    property GrayImage: TCustomImageList read FGrayList write SetGrayList;
    property GrayIndex: TImageIndex read FGrayIndex write SetGrayIndex default -1;
    property DisabledImage: TCustomImageList read FDisabledList write SetDisabledList;
    property DisabledIndex: TImageIndex read FDisabledIndex write SetDisabledIndex default -1;
    property DownImage: TCustomImageList read FDownList write SetDownList;
    property DownIndex: TImageIndex read FDownIndex write SetDownIndex default -1;
    property HotImage: TCustomImageList read FHotList write SetHotList;
    property HotIndex: TImageIndex read FHotIndex write SetHotIndex default -1;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvTransparentButton = class(TJvCustomGraphicButton)
  private
    FTextAlign: TJvTextAlign;
    FAutoGray: Boolean;
    FTransparent: Boolean;
    FShowPressed: Boolean;
    FPressOffset: Integer;
    FSpacing: Integer;
    FBorderSize: Cardinal;
    FImList: TCustomImageList;
    FOutline: TJvFrameStyle;
    FWordWrap: Boolean;

    FGlyph: TBitmap;
    FNumGlyphs: TNumGlyphs;
    FKeepMouseLeavePressed: Boolean;
    FImages: TJvTransparentButtonImages;
    FGlyphStretched: Boolean;
    procedure SetGlyph(Bmp: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure CalcGlyphCount;
    procedure SetImages(Value: TJvTransparentButtonImages);

    procedure SetWordWrap(Value: Boolean);
    procedure SetSpacing(Value: Integer);
    procedure SetAutoGray(Value: Boolean);
    procedure SetTextAlign(Value: TJvTextAlign);
    procedure SetFrameStyle(Value: TJvFrameStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetBorderWidth(Value: Cardinal);
    function GetUseImages: Boolean;
    procedure SetGlyphStretched(const Value: Boolean);
  protected
    procedure PaintButton(Canvas: TCanvas); override;
    procedure PaintFrame(Canvas: TCanvas); override;
    procedure DrawTheText(ARect: TRect; Canvas: TCanvas); virtual;
    procedure DrawTheBitmap(ARect: TRect; Canvas: TCanvas); virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure GlyphChanged(Sender: TObject);

    procedure AddGlyphGlyphs(AGlyph: TBitmap; AColor: TColor; Value: Integer);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddImageGlyphs;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property InternalList: TCustomImageList read FImList;
    property Canvas;
    property UseImages: Boolean read GetUseImages;
  published
    property Action;
    property AllowAllUp;
    property Align;
    property Anchors;
    property Constraints;

    property AutoGray: Boolean read FAutoGray write SetAutoGray default True;
    property BorderWidth: Cardinal read FBorderSize write SetBorderWidth default 1;
    property Caption;
    property Color;
    property DropDownMenu;
    property DropArrow;
    property Down;
    property Enabled;
    property Font;
    property GroupIndex;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;

    property FrameStyle: TJvFrameStyle read FOutline write SetFrameStyle default fsExplorer;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowPressed: Boolean read FShowPressed write FShowPressed default True;
    property PressOffset: Integer read FPressOffset write FPressOffset default 1;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property TextAlign: TJvTextAlign read FTextAlign write SetTextAlign default ttaCenter;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property OnClick;
    property OnDragDrop;
    property OnDropDownMenu;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnStartDrag;

    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphStretched: Boolean read FGlyphStretched write SetGlyphStretched default False;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property KeepMouseLeavePressed: Boolean read FKeepMouseLeavePressed write FKeepMouseLeavePressed default False;
    property Images: TJvTransparentButtonImages read FImages write SetImages;
  end;

{ SLOW! don't use in realtime! }
procedure GrayBitmap(Bmp: TBitmap; R, G, B: Integer);
procedure DisabledBitmap(Bmp: TBitmap);
procedure MonoBitmap(Bmp: TBitmap; R, G, B: Integer);
procedure BWBitmap(Bmp: TBitmap);

function DrawDisabledText(DC: HDC; Caption: TCaption; nCount: Integer;
  var lpRect: TRect; uFormat: Integer): Integer;

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
  JvConsts;

{ create a grayed version of a color bitmap }
{ SLOW! don't use in realtime! }

procedure GrayBitmap(Bmp: TBitmap; R, G, B: Integer);
var
  I, J: Integer;
  Col: Longint;
  DC: HDC;
begin
  if Bmp.Empty then
    Exit;
  DC := Bmp.Canvas.Handle;
  for I := 0 to Bmp.Width do
    for J := 0 to Bmp.Height do
    begin
      //Col := Bmp.Canvas.Pixels[I, J];
      Col := GetPixel(DC, I, J);
      Col := (GetRValue(Col) * R + GetGValue(Col) * G + GetBValue(Col) * B) div (R + G + B);
      //Bmp.Canvas.Pixels[I, J] := RGB(Col, Col, Col);
      SetPixel(DC, I, J, RGB(Col, Col, Col));
    end;
end;

{ create a grayed version of a color bitmap }
{ SLOW! don't use in realtime! }

procedure MonoBitmap(Bmp: TBitmap; R, G, B: Integer);
var
  I, J: Integer;
  Col: Longint;
  DC: HDC;
begin
  if Bmp.Empty then
    Exit;
  DC := Bmp.Canvas.Handle;
  for I := 0 to Bmp.Width do
    for J := 0 to Bmp.Height do
    begin
      //Col := Bmp.Canvas.Pixels[I, J];
      Col := GetPixel(DC, I, J);
      Col := (GetRValue(Col) * R + GetGValue(Col) * G + GetBValue(Col) * B) div (R + G + B);
      //Bmp.Canvas.Pixels[I, J] := RGB(Col, Col, Col);
      SetPixel(DC, I, J, RGB(Col, Col, Col));
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
  try
    W := Bmp.Width;
    H := Bmp.Height;

    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Width := W;
    TmpImage.Height := H;

    MonoBmp.Assign(Bmp);
    MonoBmp.Canvas.Font.Color := clWhite;
    MonoBmp.Canvas.Brush.Color := clBlack;
    MonoBmp.Monochrome := True;

    //TmpImage.Canvas.Brush.Color := clBtnFace;
    //TmpImage.Canvas.FillRect(Rect(0, 0, W, H));
    TmpImage.Canvas.Brush.Color := clBtnHighlight;
    SetTextColor(TmpImage.Canvas.Handle, clBlack);
    SetBkColor(TmpImage.Canvas.Handle, clWhite);
    BitBlt(TmpImage.Canvas.Handle, 1, 1, W + 1, H + 1, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    TmpImage.Canvas.Brush.Color := clBtnShadow;
    SetTextColor(TmpImage.Canvas.Handle, clBlack);
    SetBkColor(TmpImage.Canvas.Handle, clWhite);
    BitBlt(TmpImage.Canvas.Handle, 0, 0, W, H, MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
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
  DC: HDC;
begin
  if Bmp.Empty then
    Exit;

  W := Bmp.Width;
  H := Bmp.Height;
  TopLeftCol := Bmp.Canvas.Pixels[0, 0];

  DC := Bmp.Canvas.Handle;
  for I := 0 to W do
    for J := 0 to H do
    begin
      //Col := Bmp.Canvas.Pixels[I, J];
      Col := GetPixel(DC, I, J);
      if (Col <> clWhite) and (Col <> TopLeftCol) then
        Col := clBlack
      else
        Col := TopLeftCol;
      //Bmp.Canvas.Pixels[I, J] := Col;
      SetPixel(DC, I, J, Col);
    end;
  DisabledBitmap(Bmp);
end;

{ just like DrawText, but draws disabled instead }

function DrawDisabledText(DC: HDC; Caption: TCaption; nCount: Integer;
  var lpRect: TRect; uFormat: Integer): Integer;
var
  OldCol: Integer;
begin
  OldCol := SetTextColor(DC, ColorToRGB(clBtnHighlight));
  OffsetRect(lpRect, 1, 1);
  DrawText(DC, Caption, nCount, lpRect, uFormat);
  OffsetRect(lpRect, -1, -1);
  SetTextColor(DC, ColorToRGB(clBtnShadow));
  Result := DrawText(DC, Caption, nCount, lpRect, uFormat);
  SetTextColor(DC, OldCol);
end;

//=== { TJvTransparentButtonActionLink } =====================================

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
    Result := False;
end;

function TJvTransparentButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := False;
end;

procedure TJvTransparentButtonActionLink.SetGroupIndex(Value: Integer);
begin
end;

procedure TJvTransparentButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked and (FClient is TJvTransparentButton) then
    TJvTransparentButton(FClient).Down := Value;
end;

procedure TJvTransparentButtonActionLink.SetImageIndex(Value: Integer);
begin
  // Force updating the image indexes when the action changes its own value
  if (FClient is TJvTransparentButton) then
  begin
    TJvTransparentButton(FClient).Images.ActiveIndex := Value;
    TJvTransparentButton(FClient).Images.GrayIndex := Value;
    TJvTransparentButton(FClient).Images.DisabledIndex := Value;
    TJvTransparentButton(FClient).Images.DownIndex := Value;
    TJvTransparentButton(FClient).Images.HotIndex := Value;
  end;
end;

//=== { TJvTransparentButtonImages } =========================================

constructor TJvTransparentButtonImages.Create(AButton: TJvTransparentButton);
begin
  inherited Create;
  FButton := AButton;

  FGrayLink := TChangeLink.Create;
  FGrayLink.OnChange := AButton.GlyphChanged;

  FActiveLink := TChangeLink.Create;
  FActiveLink.OnChange := AButton.GlyphChanged;

  FDisabledLink := TChangeLink.Create;
  FDisabledLink.OnChange := AButton.GlyphChanged;

  FDownLink := TChangeLink.Create;
  FDownLink.OnChange := AButton.GlyphChanged;

  FHotLink := TChangeLink.Create;
  FHotLink.OnChange := AButton.GlyphChanged;

  FActiveIndex := -1;
  FDisabledIndex := -1;
  FDownIndex := -1;
  FGrayIndex := -1;
  FHotIndex := -1;
end;

destructor TJvTransparentButtonImages.Destroy;
begin
  FGrayLink.Free;
  FActiveLink.Free;
  FDisabledLink.Free;
  FDownLink.Free;
  FHotLink.Free;
  inherited Destroy;
end;

procedure TJvTransparentButtonImages.Assign(Source: TPersistent);
var
  Src: TJvTransparentButtonImages;
begin
  if Source is TJvTransparentButtonImages then
  begin
    Src := TJvTransparentButtonImages(Source);

    ActiveImage := Src.ActiveImage;
    GrayImage := Src.GrayImage;
    DisabledImage := Src.DisabledImage;
    DownImage := Src.DownImage;
    HotImage := Src.HotImage;

    ActiveIndex := Src.ActiveIndex;
    GrayIndex := Src.GrayIndex;
    DisabledIndex := Src.DisabledIndex;
    DownIndex := Src.DownIndex;
    HotIndex := Src.HotIndex;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTransparentButtonImages.AddGlyphs;
begin
  FButton.AddImageGlyphs;
end;

procedure TJvTransparentButtonImages.SetImageList(var List: TCustomImageList; Link: TChangeLink;
  Value: TCustomImageList);
begin
  if Value <> List then
  begin
    if Assigned(List) then
      List.UnRegisterChanges(Link);
    List := Value;
    if Assigned(List) then
      List.RegisterChanges(Link);
    AddGlyphs;
  end;
end;

procedure TJvTransparentButtonImages.SetGrayList(Value: TCustomImageList);
begin
  SetImageList(FGrayList, FGrayLink, Value);
end;

procedure TJvTransparentButtonImages.SetActiveList(Value: TCustomImageList);
begin
  SetImageList(FActiveList, FActiveLink, Value);
end;

procedure TJvTransparentButtonImages.SetDisabledList(Value: TCustomImageList);
begin
  SetImageList(FDisabledList, FDisabledLink, Value);
end;

procedure TJvTransparentButtonImages.SetDownList(Value: TCustomImageList);
begin
  SetImageList(FDownList, FDownLink, Value);
end;

procedure TJvTransparentButtonImages.SetHotList(Value: TCustomImageList);
begin
  SetImageList(FHotList, FHotLink, Value);
end;

procedure TJvTransparentButtonImages.SetGrayIndex(Value: TImageIndex);
begin
  if FGrayIndex <> Value then
  begin
    FGrayIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButtonImages.SetActiveIndex(Value: TImageIndex);
begin
  if FActiveIndex <> Value then
  begin
    FActiveIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButtonImages.SetDisabledIndex(Value: TImageIndex);
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButtonImages.SetDownIndex(Value: TImageIndex);
begin
  if FDownIndex <> Value then
  begin
    FDownIndex := Value;
    AddGlyphs;
  end;
end;

procedure TJvTransparentButtonImages.SetHotIndex(Value: TImageIndex);
begin
  if FHotIndex <> Value then
  begin
    FHotIndex := Value;
    AddGlyphs;
  end;
end;

//=== { TJvTransparentButton } ===============================================

constructor TJvTransparentButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoGray := True;
  FShowPressed := True;
  FBorderSize := 1;
  FTransparent := True;

  FImList := TImageList.Create(Self);
  FPressOffset := 1;
  FSpacing := 2;
  FTextAlign := ttaCenter;
  FWordWrap := False;
  FOutline := fsExplorer;

  FImages := TJvTransparentButtonImages.Create(Self);

  FNumGlyphs := 1;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FNumGlyphs := 1;

  // obones: removed, it goes against the default values set in the ancestor
  // and will force them to be true when loading from a DFM.
  //AllowAllUp := True;
  //Flat := True;
end;

destructor TJvTransparentButton.Destroy;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FImages);
  // FImList.Free; // owner-destroyed
  inherited Destroy;
end;

procedure TJvTransparentButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FImages <> nil) then
  begin
    if AComponent = FImages.FGrayList then
      FImages.GrayImage := nil;
    if AComponent = FImages.FActiveList then
      FImages.ActiveImage := nil;
    if AComponent = FImages.FDisabledList then
      FImages.DisabledImage := nil;
    if AComponent = FImages.FDownList then
      FImages.DownImage := nil;
    if AComponent = FImages.FHotList then
      FImages.HotImage := nil;
  end;
end;

function TJvTransparentButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvTransparentButtonActionLink;
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
    FWordWrap := Value;
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

{ paint everything but bitmap and text }

procedure TJvTransparentButton.PaintFrame(Canvas: TCanvas);
var
  TmpRect: TRect;
  DrawIt: Boolean;
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

    if (bsMouseDown in MouseStates) or Down then
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
    end
    else
    begin
      DrawIt := ((bsMouseInside in MouseStates) and Transparent) or not Transparent or (csDesigning in ComponentState);
      InflateRect(TmpRect, 1, 1);
      case FrameStyle of
        fsNone:
          if csDesigning in ComponentState then
            Frame3D(Canvas, TmpRect, clBlack, clBlack, 1);
        fsRegular:
          if DrawIt then
          begin
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBlack, BorderWidth);
            Frame3D(Canvas, TmpRect, RGB(223, 223, 223), clBtnShadow, BorderWidth);
          end;
        fsExplorer:
          if (bsMouseInside in MouseStates) or (csDesigning in ComponentState) then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
        fsIndent:
          if DrawIt then
          begin
            Frame3D(Canvas, TmpRect, clBtnShadow, clBtnHighlight, BorderWidth);
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, BorderWidth);
          end;
        fsLight:
          if DrawIt then
            Frame3D(Canvas, TmpRect, clBtnHighlight, clBtnShadow, 1);
        fsDark:
          if DrawIt then
            Frame3D(Canvas, TmpRect, clBtnFace, cl3DDkShadow, 1);
        fsMono:
          if DrawIt then
            Frame3D(Canvas, TmpRect, clBtnHighlight, cl3DDkShadow, 1);
      end;
    end;

    if (HotTrackFont <> Font) and (Caption <> '') then
    begin
      InflateRect(TmpRect, 1, 1);
      DrawTheText(TmpRect, Canvas);
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
    if Caption <> '' then
      DrawTheText(Dest, Canvas);
  end;
end;

{ ARect contains the bitmap bounds }

procedure TJvTransparentButton.DrawTheText(ARect: TRect; Canvas: TCanvas);
var
  Flags, MidX, MidY: Integer;
  DC: HDC; { Col: TColor; }
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
  DrawText(DC, Caption, Length(Caption), TmpRect, Flags or DT_CALCRECT);
{
  if FWordWrap then
    Canvas.TextExtent(Caption, TmpRect, WordBreak)
  else
    Canvas.TextExtent(Caption, TmpRect, 0);
}
  MidY := TmpRect.Bottom - TmpRect.Top;
  MidX := TmpRect.Right - TmpRect.Left;
  Flags := DT_CENTER;
  { div 2 and shr 1 generates the exact same assembler code... }
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
    OffsetRect(TmpRect, FPressOffset, FPressOffset);

  SetBkMode(DC, Windows.TRANSPARENT);
  if not Enabled then
    DrawDisabledText(DC, Caption, -1, TmpRect, Flags)
  else
  begin
    if (bsMouseInside in MouseStates) and HotTrack then
      SetTextColor(DC, ColorToRGB(HotTrackFont.Color))
    else
      SetTextColor(DC, ColorToRGB(Self.Font.Color));
    DrawText(DC, Caption, -1, TmpRect, Flags);
  end;
end;

procedure TJvTransparentButton.SetImages(Value: TJvTransparentButtonImages);
begin
  if Value <> FImages then
    FImages.Assign(Value);
end;

procedure TJvTransparentButton.CalcGlyphCount;
var
  GlyphNum: Integer;
begin
  if not Glyph.Empty then
  begin
    if Glyph.Width mod Glyph.Height = 0 then
    begin
      GlyphNum := Glyph.Width div Glyph.Height;
      if GlyphNum > 4 then
        GlyphNum := 1;
      SetNumGlyphs(GlyphNum);
    end;
  end;
end;

procedure TJvTransparentButton.AddGlyphGlyphs(AGlyph: TBitmap; AColor: TColor; Value: Integer);
var
  Bmp: TBitmap;
  I, TmpWidth: Integer;
  Dest, Source: TRect;
  GrayGlyph: TBitmap;
  DisabledGlyph: TBitmap;
begin
  if UseImages then
    Exit;

  FImList.Clear;
  if not AGlyph.Empty then
  begin
    Bmp := TBitmap.Create;
    GrayGlyph := TBitmap.Create;
    DisabledGlyph := TBitmap.Create;
    try
      { destroy old list }
      TmpWidth := AGlyph.Width div FNumGlyphs;
      FImList.Width := TmpWidth;
      FImList.Height := AGlyph.Height;
      Bmp.Width := FImList.Width;
      Bmp.Height := FImList.Height;
      Dest := Rect(0, 0, Bmp.Width, Bmp.Height);
      { create the imagelist }
      for I := 0 to FNumGlyphs - 1 do
      begin
        Source := Rect(I * Bmp.Width, 0, I * Bmp.Width + Bmp.Width, Bmp.Height);
        Bmp.Canvas.CopyRect(Dest, AGlyph.Canvas, Source);
        if I = 0 then { first picture }
        begin
          { create the disabled and grayed bitmaps too }
          GrayGlyph.Assign(Bmp);
          MonoBitmap(GrayGlyph, 11, 59, 30);
          DisabledGlyph.Assign(Bmp);
          BWBitmap(DisabledGlyph);
        end;
        FImList.AddMasked(Bmp, Bmp.TransparentColor);
      end;
      { add last }
      FImList.AddMasked(GrayGlyph, GrayGlyph.TransparentColor);
      FImList.AddMasked(DisabledGlyph, DisabledGlyph.TransparentColor);
    finally
      Bmp.Free;
      GrayGlyph.Free;
      DisabledGlyph.Free;
    end;
    FGlyph.Dormant;
  end;
  Invalidate;
end;

procedure TJvTransparentButton.SetGlyph(Bmp: TBitmap);
begin
  FGlyph.Assign(Bmp);
  if UseImages then
    AddImageGlyphs;
  CalcGlyphCount;
  Invalidate;
end;

procedure TJvTransparentButton.SetGlyphStretched(const Value: Boolean);
begin
  if FGlyphStretched <> Value then
  begin
    FGlyphStretched := Value;
    Invalidate;
  end;
end;

procedure TJvTransparentButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    if not UseImages then
    begin
      GlyphChanged(Self);
      Invalidate;
    end;
  end;
end;

procedure TJvTransparentButton.DrawTheBitmap(ARect: TRect; Canvas: TCanvas);
var
  Index: TImageIndex;
  HelpRect: TRect;
  Icon: TIcon;
  Bitmap: TBitmap;
begin
  if FImList.Count = 0 then
    Exit;
  Index := 0;

  // Important: Draw parent image to keep transparency chain working

  // This line should be added, but currently it introduces some mess in this component.
  // Mainly, the frame around the button is not displayed.
  // That needs to be fixed before being added again.

  //CopyParentImage(Self, Canvas);


  if UseImages then
  begin
    // Images
    with FImList do
    begin
      if not Enabled then
        Index := 1 { disabled }
      else
      if ((bsMouseDown in MouseStates) and
          (KeepMouseLeavePressed or (bsMouseInside in MouseStates))) or Down then
        Index := 2 { down }
      else
      if (FrameStyle = fsExplorer) and FAutoGray and (MouseStates = []) then
        Index := 3 { autogray }
      else
      if (bsMouseInside in MouseStates) and HotTrack then
        Index := 4 { hot }
      else
        Index := 0; { normal/active }
    end;
  end
  else
  begin
    // Glyph
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
    end;

    { do we need the grayed bitmap ? }
    if (Flat or (FrameStyle = fsExplorer)) and FAutoGray and not (bsMouseInside in MouseStates) and not Down then
      Index := FImList.Count - 2;

    { do we need the disabled bitmap ? }
    if not Enabled and (FNumGlyphs = 1) then
      Index := FImList.Count - 1;
  end;

  if ((bsMouseDown in MouseStates) or Down) and FShowPressed then
    OffsetRect(ARect, FPressOffset, FPressOffset);

  // Mantis 3641: Do not draw the rectangle if we are transparent, it's of no use.
  if (bsMouseInside in MouseStates) and ((bsMouseDown in MouseStates) or Down) and
    not Transparent then
  begin
    HelpRect := ClientRect;
    InflateRect(HelpRect, -BorderWidth - 1, -BorderWidth - 1);
    Canvas.Brush.Bitmap := Pattern;
    Self.Canvas.FillRect(HelpRect);
  end;

  if GlyphStretched then
  begin
    Bitmap := TBitmap.Create;
    try
      FImList.GetBitmap(Index, Bitmap);
      Canvas.StretchDraw(ClientRect, Bitmap);
    finally
      Bitmap.Free;
    end;
  end
  else
  begin
    // Use a TIcon instead of FImList.Draw to avoid triggering Mantis 3851
    Icon := TIcon.Create;
    try
      FImList.GetIcon(Index, Icon);
      Canvas.Draw(ARect.Left, ARect.Top, Icon);
    finally
      Icon.Free;
    end;
  end;
end;

function TJvTransparentButton.GetUseImages: Boolean;
begin
  Result := not Assigned(Glyph) or Glyph.Empty;
end;

procedure TJvTransparentButton.GlyphChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    if UseImages then
      AddImageGlyphs
    else
      AddGlyphGlyphs(Glyph, Glyph.TransparentColor, NumGlyphs);
  end;
  Invalidate;
end;

procedure TJvTransparentButton.Loaded;
begin
  inherited Loaded;
  GlyphChanged(Self);
end;

procedure TJvTransparentButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  {procedure CopyImage(ImageList: TCustomImageList; Index: TImageIndex);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Self.Canvas.Brush.Color := clFuchsia; //! for lack of a better color
      Self.Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Self.Canvas, 0, 0, Index);
    end;
    GlyphChanged(Glyph);
  end;}

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults then
        Self.Down := Checked;
      {if UseImages then
      begin}
        // action uses ActiveImage
        Glyph.Width := 0;
        Glyph.Height := 0;

        if not CheckDefaults or ((Images <> nil) and (Self.Images.ActiveIndex = -1)) then
        begin
          Self.Images.ActiveImage := ActionList.Images;
          Self.Images.ActiveIndex := ImageIndex;
        end;
      {end
      else
      begin
        // Copy image from action's imagelist
        if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
           (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
          CopyImage(ActionList.Images, ImageIndex);
      end;}
    end;
  end;
end;

procedure TJvTransparentButton.AddImageGlyphs;
var
  Bmp: TBitmap; // creating a Bitmap is time consuming in Qt, so use one for all

  function AddGlyph(Images: TCustomImageList; Index: TImageIndex): Boolean;
  var
    Icon: HICON;
  begin
    Result := (Images <> nil) and (Index >= 0);
    if Result then
    begin
      Icon := ImageList_GetIcon(Images.Handle, Index, ILD_TRANSPARENT);
      ImageList_AddIcon(FImList.Handle, Icon);
      DestroyIcon(Icon);
    end;
  end;

begin
  if not UseImages then
    Exit;

  Bmp := TBitmap.Create;
  try
    { get imagelist properties (width, height, ...) }
    FImList.BkColor := clNone;
    { destroy old list }
    FImList.Clear;
    { create the imagelist }
    with Images do
    begin
      if Assigned(FActiveList) and (FActiveIndex > -1) then
      begin
        FImList.Height := FActiveList.Height;
        FImList.Width := FActiveList.Width;

        { copy some properties }
        FImList.Masked := FActiveList.Masked;
        FImList.ImageType := FActiveList.ImageType;
        FImList.DrawingStyle := FActiveList.DrawingStyle;
        FImList.BlendColor := FActiveList.BlendColor;

        AddGlyph(FActiveList, FActiveIndex);
      end
      else
        Exit; // without an active image the component cannot do anything

      Bmp.Height := FImList.Height;
      Bmp.Width := FImList.Width;

      if not AddGlyph(FDisabledList, FDisabledIndex) then
      begin
        FActiveList.GetBitmap(FActiveIndex, Bmp);
        DisabledBitmap(Bmp);
        FImList.AddMasked(Bmp, Bmp.TransparentColor);
      end;

      if not AddGlyph(FDownList, FDownIndex) then
        AddGlyph(FActiveList, FActiveIndex);

      if not AddGlyph(FGrayList, FGrayIndex) then
      begin
        FActiveList.GetBitmap(FActiveIndex, Bmp);
        GrayBitmap(Bmp, 11, 59, 30);
        FImList.AddMasked(Bmp, Bmp.TransparentColor);
      end;

      if not AddGlyph(FHotList, FHotIndex) then
        AddGlyph(FActiveList, FActiveIndex);
    end;
  finally
    Bmp.Free;
    Repaint;
  end;
end;

initialization
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
