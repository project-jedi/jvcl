{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTabComm.PAS, released on 2003-01-15.

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

//...common for JvgTab and JvgPage classes declaration

unit JvgTabComm;

interface
uses Windows, Graphics, Controls, Classes, ExtCtrls, JvgTypes, ComCtrls, JvgCommClasses;

const
  TCS_SCROLLOPPOSITE = $0001; //вкладка с несколькими страницами
  TCS_BOTTOM = $0002;
  TCS_RIGHT = $0002; //используется с TCS_VERTICAL
  TCS_HOTTRACK = $0040;
  TCS_VERTICAL = $0080; //только для режима с несколькими строками

type
  TglOnGetGradientColors = procedure(Sender: TObject; Index: integer; var Gradient: TJvgGradient) of object;
  TJvgTabStyle = class;
  TJvgTabsWallpaper = class;

  TDRAWTABSTRUCT = record
    lpDrawItemStr: PDRAWITEMSTRUCT;
    ClientR: TRect;
    TabsCount: integer;
    Caption: string;
    Wallpaper: TJvgTabsWallpaper;
    Glyph: TBitmap;
    GlyphOption: TglWallpaperOption;
    BoxStyle: TJvgTabStyle;
    Font_: TFont;
    fButton: boolean;
    Position: TglSide;
    Options: TglTabOptions;
    FontDirection: TglLabelDir;
    BackgrColor_: TColor;
    FlatButtons: boolean;
    Gradient: TJvgGradient;
  end;

  //______________________________________{ . TJvgTabStyle . }
  TJvgTabStyle = class(TPersistent)
  private
    FBorders: TglSides;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBold: boolean;
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
    procedure SetBorders(Value: TglSides);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBold(Value: boolean);
    procedure SetBackgrColor(Value: TColor);
    //  procedure SetFillBackgr( Value:boolean );
    procedure SetFont(Value: TFont);
    procedure SetTextStyle(Value: TglTextStyle);
    procedure SetCaptionHAlign(Value: TglHorAlign);
    procedure SetCaptionVAlign(Value: TglVertAlign);
    procedure SetGlyphHAlign(Value: TglHorAlign);
    procedure SetGlyphVAlign(Value: TglVertAlign);
    procedure SetOnChanged(Value: TNotifyEvent);
  public
    OnFontChanged: TNotifyEvent;
    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
  published
    property Borders: TglSides read FBorders write SetBorders;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter;
    property Bold: boolean read FBold write SetBold;
    property BackgrColor: TColor read FBackgrColor write SetBackgrColor; // default clBtnFace;
    //  property FillBackgr  :boolean read FFillBackgr write SetFillBackgr default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property TextStyle: TglTextStyle read FTextStyle write SetTextStyle default fstNone;
    property CaptionHAlign: TglHorAlign read FCaptionHAlign write SetCaptionHAlign default fhaLeft;
    property CaptionVAlign: TglVertAlign read FCaptionVAlign write SetCaptionVAlign default fvaCenter;
    property GlyphHAlign: TglHorAlign read FGlyphHAlign write SetGlyphHAlign default fhaLeft;
    property GlyphVAlign: TglVertAlign read FGlyphVAlign write SetGlyphVAlign default fvaCenter;
    property Gradient: TJvgGradient read FGradient write FGradient;
  end;
  //______________________________________{ . TJvgTabsWallpaper . }
  TJvgTabsWallpaper = class(TPersistent)
  private
    FBitmap: TBitmap;
    FImage: TImage;
    FFillCaptionBakgr: boolean;
    FFillCaptions: boolean;
    FFillClient: boolean;
    FTile: boolean;
    FIncludeBevels: boolean;
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetFillCaptionBakgr(Value: boolean);
    procedure SetFillCaptions(Value: boolean);
    procedure SetFillClient(Value: boolean);
    procedure SetTile(Value: boolean);
    procedure SetIncludeBevels(Value: boolean);
  public
    Bmp: TBitmap;
    OnChanged: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property FillCaptions: boolean
      read FFillCaptions write SetFillCaptions default true;
    property FillCaptionBakgr: boolean
      read FFillCaptionBakgr write SetFillCaptionBakgr default false;
    property FillClient: boolean
      read FFillClient write SetFillClient default false;
    property Tile: boolean read FTile write SetTile
      default true;
    property IncludeBevels: boolean
      read FIncludeBevels write SetIncludeBevels default true;
  end;
  //.............../\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
implementation
//______________________________________{ . TJvgTabStyle methods . }

constructor TJvgTabStyle.Create(AOwner: TWinControl);
begin
  inherited Create;
  FGradient := TJvgGradient.Create;
  //...set defaults
//  FBevelInner   := bvRaised;
//  FBevelOuter   := bvLowered;
  FBorders := [fsdLeft, fsdTop, fsdRight, fsdBottom];
  FBold := false;
  FBackgrColor := clBtnFace;
  //  FFillBackgr   := false;
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
  inherited;
end;

procedure TJvgTabStyle.SetBorders(Value: TglSides);
begin
  FBorders := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetBold(Value: boolean);
begin
  FBold := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetBackgrColor(Value: TColor);
begin
  FBackgrColor := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

{procedure TJvgTabStyle.SetFillBackgr( Value:boolean );
begin if FFillBackgr=Value then exit;
  FFillBackgr:=Value; if Assigned(OnChanged) then OnChanged(self);
end;
}

procedure TJvgTabStyle.SetFont(Value: TFont);
begin
  if Assigned(Value) then FFont.Assign(Value);
  if TTabControl(FParent).Font.Size < Value.Size then
    TTabControl(FParent).Font.Assign(Value);
  if Assigned(OnFontChanged) then OnFontChanged(self);
end;

procedure TJvgTabStyle.SetTextStyle(Value: TglTextStyle);
begin
  FTextStyle := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetCaptionHAlign(Value: TglHorAlign);
begin
  FCaptionHAlign := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetCaptionVAlign(Value: TglVertAlign);
begin
  FCaptionVAlign := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetGlyphHAlign(Value: TglHorAlign);
begin
  FGlyphHAlign := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetGlyphVAlign(Value: TglVertAlign);
begin
  FGlyphVAlign := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabStyle.SetOnChanged(Value: TNotifyEvent);
begin
  FOnChanged := Value;
  FGradient.OnChanged := FOnChanged;
end;
//______________________________________{ . TJvgTabsWallpaper methods . }

constructor TJvgTabsWallpaper.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  //...set defaults
  FFillCaptionBakgr := false;
  FFillCaptions := true;
  FFillClient := false;
  FIncludeBevels := true;
  FTile := true;
end;

destructor TJvgTabsWallpaper.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TJvgTabsWallpaper.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

procedure TJvgTabsWallpaper.SetBitmap(Value: TBitmap);
begin
  if Assigned(FBitmap) then FBitmap.Free;
  FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then
    Bmp := FBitmap
  else if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else
    Bmp := nil;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabsWallpaper.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap
  else if Assigned(FBitmap) then
    Bmp := FBitmap
  else
    Bmp := nil;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabsWallpaper.SetFillCaptionBakgr(Value: boolean);
begin
  if FFillCaptionBakgr = Value then exit;
  FFillCaptionBakgr := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabsWallpaper.SetFillCaptions(Value: boolean);
begin
  if FFillCaptions = Value then exit;
  FFillCaptions := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabsWallpaper.SetFillClient(Value: boolean);
begin
  if FFillClient = Value then exit;
  FFillClient := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabsWallpaper.SetTile(Value: boolean);
begin
  if FTile = Value then exit;
  FTile := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TJvgTabsWallpaper.SetIncludeBevels(Value: boolean);
begin
  if FIncludeBevels = Value then exit;
  FIncludeBevels := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

end.
