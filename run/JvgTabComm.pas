{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTabComm.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  ...common for JvgTab and JvgPage classes declaration

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgTabComm;

interface

uses
  Windows, Graphics, Controls, Classes, ExtCtrls, ComCtrls,
  JvgTypes, JvgCommClasses;

const
  TCS_SCROLLOPPOSITE = $0001; //вкладка с несколькими страницами; multipage tab [translated]
  {$EXTERNALSYM TCS_SCROLLOPPOSITE}
  TCS_BOTTOM = $0002;
  {$EXTERNALSYM TCS_BOTTOM}
  TCS_RIGHT = $0002; //используется с TCS_VERTICAL; used with TCS_VERTICAL [translated]
  {$EXTERNALSYM TCS_RIGHT}
  TCS_HOTTRACK = $0040;
  {$EXTERNALSYM TCS_HOTTRACK}
  TCS_VERTICAL = $0080; //только для режима с несколькими строками; Only for multi-line mode [translated]
  {$EXTERNALSYM TCS_VERTICAL}

type
  TglOnGetGradientColors = procedure(Sender: TObject; Index: Integer; var Gradient: TJvgGradient) of object;
  TJvgTabStyle = class;
  TJvgTabsWallpaper = class;

  TDRAWTABSTRUCT = record
    lpDrawItemStr: PDrawItemStruct;
    ClientR: TRect;
    TabsCount: Integer;
    Caption: string;
    Wallpaper: TJvgTabsWallpaper;
    Glyph: TBitmap;
    GlyphOption: TglWallpaperOption;
    BoxStyle: TJvgTabStyle;
    Font_: TFont;
    fButton: Boolean;
    Position: TglSide;
    Options: TglTabOptions;
    FontDirection: TglLabelDir;
    BackgrColor_: TColor;
    FlatButtons: Boolean;
    Gradient: TJvgGradient;
  end;

  TJvgTabStyle = class(TPersistent)
  private
    FBorders: TglSides;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBold: Boolean;
    FBackgrColor: TColor;
    FFont: TFont;
    FTextStyle: TglTextStyle;
    FCaptionHAlign: TglHorAlign;
    FCaptionVAlign: TglVertAlign;
    FGlyphHAlign: TglHorAlign;
    FGlyphVAlign: TglVertAlign;
    FGradient: TJvgGradient;
    FParent: TWinControl;
    FOnChanged: TNotifyEvent;
    FOnFontChanged: TNotifyEvent;
    procedure SetBorders(Value: TglSides);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBold(Value: Boolean);
    procedure SetBackgrColor(Value: TColor);
    //  procedure SetFillBackgr(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetTextStyle(Value: TglTextStyle);
    procedure SetCaptionHAlign(Value: TglHorAlign);
    procedure SetCaptionVAlign(Value: TglVertAlign);
    procedure SetGlyphHAlign(Value: TglHorAlign);
    procedure SetGlyphVAlign(Value: TglVertAlign);
    procedure SetChanged(Value: TNotifyEvent);
  protected
    procedure Changed;
    procedure FontChanged;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write SetChanged;
    property OnFontChanged: TNotifyEvent read FOnFontChanged write FOnFontChanged;
  published
    property Borders: TglSides read FBorders write SetBorders;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter;
    property Bold: Boolean read FBold write SetBold;
    property BackgrColor: TColor read FBackgrColor write SetBackgrColor; // default clBtnFace;
    //  property FillBackgr: Boolean read FFillBackgr write SetFillBackgr default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property TextStyle: TglTextStyle read FTextStyle write SetTextStyle default fstNone;
    property CaptionHAlign: TglHorAlign read FCaptionHAlign write SetCaptionHAlign default fhaLeft;
    property CaptionVAlign: TglVertAlign read FCaptionVAlign write SetCaptionVAlign default fvaCenter;
    property GlyphHAlign: TglHorAlign read FGlyphHAlign write SetGlyphHAlign default fhaLeft;
    property GlyphVAlign: TglVertAlign read FGlyphVAlign write SetGlyphVAlign default fvaCenter;
    property Gradient: TJvgGradient read FGradient write FGradient;
  end;

  TJvgTabsWallpaper = class(TPersistent)
  private
    FBitmap: TBitmap;
    FImage: TImage;
    FFillCaptionBakgr: Boolean;
    FFillCaptions: Boolean;
    FFillClient: Boolean;
    FTile: Boolean;
    FIncludeBevels: Boolean;
    FOnChanged: TNotifyEvent;
    FBmp: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetFillCaptionBakgr(Value: Boolean);
    procedure SetFillCaptions(Value: Boolean);
    procedure SetFillClient(Value: Boolean);
    procedure SetTile(Value: Boolean);
    procedure SetIncludeBevels(Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Bmp: TBitmap read FBmp write FBmp;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property FillCaptions: Boolean read FFillCaptions write SetFillCaptions default True;
    property FillCaptionBakgr: Boolean read FFillCaptionBakgr write SetFillCaptionBakgr default False;
    property FillClient: Boolean read FFillClient write SetFillClient default False;
    property Tile: Boolean read FTile write SetTile default True;
    property IncludeBevels: Boolean read FIncludeBevels write SetIncludeBevels default True;
  end;

implementation

type
  TJvgShowFont = class(TControl)
  public
    property Font;
  end;

//=== TJvgTabStyle ===========================================================

constructor TJvgTabStyle.Create(AOwner: TWinControl);
begin
  inherited Create;
  FGradient := TJvgGradient.Create;
  //...set defaults
  // FBevelInner := bvRaised;
  // FBevelOuter := bvLowered;
  FBorders := [fsdLeft, fsdTop, fsdRight, fsdBottom];
  FBold := False;
  FBackgrColor := clBtnFace;
  // FFillBackgr := False;
  FParent := TWinControl(AOwner);
  FFont := TFont.Create;
  Font.Assign(TJvgShowFont(FParent).Font);
  FTextStyle := fstNone;
  FCaptionHAlign := fhaLeft;
  FCaptionVAlign := fvaCenter;
  FGlyphHAlign := fhaLeft;
  FGlyphVAlign := fvaCenter;
end;

destructor TJvgTabStyle.Destroy;
begin
  FFont.Free;
  FGradient.Free;
  inherited Destroy;
end;

procedure TJvgTabStyle.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvgTabStyle.SetBorders(Value: TglSides);
begin
  if FBorders <> Value then
  begin
    FBorders := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetBevelInner(Value: TPanelBevel);
begin
  if FBevelInner <> Value then
  begin
    FBevelInner := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetBevelOuter(Value: TPanelBevel);
begin
  if FBevelOuter <> Value then
  begin
    FBevelOuter := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetBold(Value: Boolean);
begin
  if FBold <> Value then
  begin
    FBold := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetBackgrColor(Value: TColor);
begin
  if FBackgrColor <> Value then
  begin
    FBackgrColor := Value;
    Changed;
  end;
end;

{procedure TJvgTabStyle.SetFillBackgr(Value: Boolean);
begin
  if FFillBackgr = Value then
    Exit;
  FFillBackgr := Value;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;
}

procedure TJvgTabStyle.SetFont(Value: TFont);
begin
  if Assigned(Value) then
    FFont.Assign(Value);
  if TTabControl(FParent).Font.Size < Value.Size then
    TTabControl(FParent).Font.Assign(Value);
  FontChanged;
end;

procedure TJvgTabStyle.FontChanged;
begin
  if Assigned(FOnFontChanged) then
    FOnFontChanged(Self);
end;

procedure TJvgTabStyle.SetTextStyle(Value: TglTextStyle);
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetCaptionHAlign(Value: TglHorAlign);
begin
  if FCaptionHAlign <> Value then
  begin
    FCaptionHAlign := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetCaptionVAlign(Value: TglVertAlign);
begin
  if FCaptionVAlign <> Value then
  begin
    FCaptionVAlign := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetGlyphHAlign(Value: TglHorAlign);
begin
  if FGlyphHAlign <> Value then
  begin
    FGlyphHAlign := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetGlyphVAlign(Value: TglVertAlign);
begin
  if FGlyphVAlign <> Value then
  begin
    FGlyphVAlign := Value;
    Changed;
  end;
end;

procedure TJvgTabStyle.SetChanged(Value: TNotifyEvent);
begin
  FOnChanged := Value;
  FGradient.OnChanged := Value;
end;

//=== TJvgTabsWallpaper ======================================================

constructor TJvgTabsWallpaper.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  //...set defaults
  FFillCaptionBakgr := False;
  FFillCaptions := True;
  FFillClient := False;
  FIncludeBevels := True;
  FTile := True;
end;

destructor TJvgTabsWallpaper.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TJvgTabsWallpaper.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvgTabsWallpaper.SetBitmap(Value: TBitmap);
begin
  if Assigned(FBitmap) then FBitmap.Free;
  FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then
    FBmp := FBitmap
  else
  if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
    FBmp := nil;
end;

procedure TJvgTabsWallpaper.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
  if Assigned(FBitmap) then
    FBmp := FBitmap
  else
    FBmp := nil;
  Changed;
end;

procedure TJvgTabsWallpaper.SetFillCaptionBakgr(Value: Boolean);
begin
  if FFillCaptionBakgr <> Value then
  begin
    FFillCaptionBakgr := Value;
    Changed;
  end;
end;

procedure TJvgTabsWallpaper.SetFillCaptions(Value: Boolean);
begin
  if FFillCaptions <> Value then
  begin
    FFillCaptions := Value;
    Changed;
  end;
end;

procedure TJvgTabsWallpaper.SetFillClient(Value: Boolean);
begin
  if FFillClient <> Value then
  begin
    FFillClient := Value;
    Changed;
  end;
end;

procedure TJvgTabsWallpaper.SetTile(Value: Boolean);
begin
  if FTile <> Value then
  begin
    FTile := Value;
    Changed;
  end;
end;

procedure TJvgTabsWallpaper.SetIncludeBevels(Value: Boolean);
begin
  if FIncludeBevels <> Value then
  begin
    FIncludeBevels := Value;
    Changed;
  end;
end;

end.

