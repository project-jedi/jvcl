{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTab.PAS, released on 2003-01-15.

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

{ TabControl  component that can  display  its  pages  captions   in
 3D styles with 3D borders.  Component  can display  glyphs  on  own
 captions and fill background with bitmap.  You  can  set  different
 fonts for selected page caption and for other captions.}

unit JvgTab;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Imglist,
  JvclVer, CommCtrl, JvgTypes, JvgUtils, JvgDrawTab, JvgTabComm, JvgCommClasses;

const
  TCM_SETTEXTCOLOR = (TCM_FIRST + 36);
type

  TJvgTabControl = class(TTabControl)
  private
    FGlyphs: TImageList;
    FSingleGlyph: boolean;
    FTabStyle: TJvgTabStyle;
    FTabSelectedStyle: TJvgTabStyle;
    FWallpaper: TJvgTabsWallpaper;
    FDrawGlyphsOption: TglWallpaperOption;
    FLookLikeButtons: boolean;
    FTabsPosition: TglSide;
    FOptions: TglTabOptions;
    FFontDirection: TglLabelDir;
    FOnGetItemColor: TglOnGetItemColorEvent;
    FOnGetItemFontColor: TglOnGetItemColorEvent;

    GlyphsChangeLink: TChangeLink;
    DrawTabStr: TDRAWTABSTRUCT;
    GlyphTmpBitmap: TBitmap;
    FontNormal: TFont;
    FontSelected: TFont;
    fNotFirst: boolean;
    aTabColors: array[0..100] of TColor;
    FAboutJVCL: TJVCLAboutInfo;

    function GetGlyphIndex(Index: Integer): Integer;
    procedure SetGlyphIndex(Index: Integer; imgIndex: Integer);
    procedure SetGlyphs(Value: TImageList);
    procedure SetSingleGlyph(Value: boolean);
    procedure SetDrawGlyphsOption(Value: TglWallpaperOption);
    procedure SetLookLikeButtons(Value: boolean);
    procedure SetTabsPosition(Value: TglSide);
    procedure SetOptions(Value: TglTabOptions);
    procedure SetFontDirection(Value: TglLabelDir);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetTabColor(Index: integer): TColor;
    procedure SetTabColor(Index: integer; Value: TColor);

    procedure SmthChanged(Sender: TObject);
    procedure FontsChanged(Sender: TObject);
    procedure DrawItem(lpDrawItemStr: PDrawItemStruct);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure GlyphsListChanged(Sender: TObject);
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    fSupressDraw: boolean;
    procedure RemakeFonts;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property GlyphIndex[Index: Integer]: Integer read GetGlyphIndex write
    SetGlyphIndex;
    property TabColor[Index: integer]: TColor read GetTabColor write
    SetTabColor;
    //     property GlyphState[Index: Integer]: Integer read GetGlyphState write SetGlyphState;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
      False;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property SingleGlyph: boolean read FSingleGlyph write SetSingleGlyph
      default false;
    property TabStyle: TJvgTabStyle read FTabStyle write FTabStyle;
    property TabSelectedStyle: TJvgTabStyle read FTabSelectedStyle write
      FTabSelectedStyle;
    property Wallpaper: TJvgTabsWallpaper read FWallpaper write FWallpaper;
    property DrawGlyphsOption: TglWallpaperOption
      read FDrawGlyphsOption write SetDrawGlyphsOption default fwoNone;
    property LookLikeButtons: boolean read FLookLikeButtons write
      SetLookLikeButtons
      default false;
    property TabsPosition: TglSide read FTabsPosition write SetTabsPosition
      default fsdTop;
    property Options: TglTabOptions read FOptions write SetOptions;
    property FontDirection: TglLabelDir
      read FFontDirection write SetFontDirection default fldLeftRight;
    property Font: TFont read GetFont write SetFont;
    property OnGetItemColor: TglOnGetItemColorEvent read FOnGetItemColor write
      FOnGetItemColor;
    property OnGetItemFontColor: TglOnGetItemColorEvent read
      FOnGetItemFontColor write FOnGetItemFontColor;
  end;

procedure Register;

implementation
const
  FontDirs: array[TglSide] of TglLabelDir
  = (fldDownUp, fldLeftRight, fldUpDown,
    fldLeftRight);

procedure Register;
begin
end;
//*****************************************_____________LowLevel METHODS

constructor TJvgTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TabStop := false;
  FTabStyle := TJvgTabStyle.Create(self);
  FTabSelectedStyle := TJvgTabStyle.Create(self);
  FWallpaper := TJvgTabsWallpaper.Create;
  FontNormal := TFont.Create;
  FontSelected := TFont.Create;
  DrawTabStr.Font_ := TFont.Create;

  FTabStyle.Font.Name := 'Arial';
  FTabSelectedStyle.Font.Name := 'Arial';

  //  if csDesigning in ComponentState then
  //    FTabSelectedStyle.BackgrColor := clbtnHighlight;

  GlyphTmpBitmap := TBitmap.Create;
  GlyphsChangeLink := TChangeLink.Create;
  GlyphsChangeLink.OnChange := GlyphsListChanged;

  //...set defaults
  FSingleGlyph := false;

  FDrawGlyphsOption := fwoNone;
  FTabsPosition := fsdTop;
  FOptions := [ftoAutoFontDirection, ftoExcludeGlyphs];
  FFontDirection := fldLeftRight;

  FTabStyle.OnChanged := SmthChanged;
  FTabSelectedStyle.OnChanged := SmthChanged;
  FTabStyle.OnFontChanged := FontsChanged;
  FTabSelectedStyle.OnFontChanged := FontsChanged;
  FWallpaper.OnChanged := SmthChanged;
  FillMemory(@aTabColors, SizeOf(aTabColors), $FF);
end;

destructor TJvgTabControl.Destroy;
begin
  FTabStyle.Free;
  FTabSelectedStyle.Free;
  GlyphTmpBitmap.Free;
  FWallpaper.Free;
  GlyphsChangeLink.Free;
  FontNormal.Free;
  FontSelected.Free;
  DrawTabStr.Font_.Free;
  inherited;
end;

procedure TJvgTabControl.SmthChanged;
begin
  Invalidate;
end;

procedure TJvgTabControl.FontsChanged;
begin
  RemakeFonts;
  Invalidate;
end;

//---------------------------------------------------special procs

procedure TJvgTabControl.CreateParams(var Params: TCreateParams);
const
  PosStyles: array[TglSide] of DWORD =
  (TCS_VERTICAL, 0, TCS_VERTICAL or TCS_RIGHT, TCS_BOTTOM
    {or TCS_SCROLLOPPOSITE or TCS_BUTTONS});
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if LookLikeButtons then
      Style := Style or TCS_BUTTONS;
    Style := Style or TCS_OWNERDRAWFIXED or PosStyles[FTabsPosition];
  end;
end; // {TCS_HOTTRACK-?};

procedure TJvgTabControl.Loaded;
begin
  inherited Loaded;
  RemakeFonts;
  if Assigned(Wallpaper.Bitmap) and (not Wallpaper.Bitmap.Empty) then
    Wallpaper.bmp := Wallpaper.Bitmap;
end;

procedure TJvgTabControl.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(Wallpaper) and (AComponent = Wallpaper.Image) and (Operation =
    opRemove) then
    Wallpaper.Image := nil;
end;

procedure TJvgTabControl.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Pointer(Message.DrawItemStruct));
end;

procedure TJvgTabControl.WndProc(var Message: TMessage);
var
  GlyphID: integer;
begin
  inherited WndProc(Message);
  with Message do
    case Msg of
      TCM_INSERTITEM:
        begin
          result := 0;
          if not Assigned(FGlyphs) then
            exit;
          GlyphID := -1;
          if FSingleGlyph then
            GlyphID := 0
          else if wParam < FGlyphs.Count then
            GlyphID := wParam;
          if GlyphID = -1 then
            exit;
          TTCItem(Pointer(Message.lParam)^).iImage := GlyphID;
          TTCItem(Pointer(Message.lParam)^).Mask := TCIF_IMAGE;
          SendMessage(handle, TCM_SETITEM, wParam, lParam);
        end;
      TCM_DELETEITEM:
        begin
        end;
      TCM_DELETEALLITEMS:
        begin
        end;
    end;
end;

procedure TJvgTabControl.GlyphsListChanged(Sender: TObject);
begin
  if HandleAllocated then
    SendMessage(Handle, TCM_SETIMAGELIST, 0,
      Longint(TImageList(Sender).Handle));
end;

procedure TJvgTabControl.DrawItem(lpDrawItemStr: PDrawItemStruct);
//var lpTabBoxStyle:^TTabBoxStyle;
var
  FontColor: TColor;
begin
  if fSupressDraw then
    exit;
  with lpDrawItemStr^ do
    if CtlType = ODT_TAB then
    begin
      //fLoaded:=true; Options:=NewOptions;
      DrawTabStr.lpDrawItemStr := lpDrawItemStr;
      DrawTabStr.Caption := Tabs[ItemID];

      if GlyphIndex[ItemID] <> -1 then
      begin
        FGlyphs.GetBitmap(GlyphIndex[ItemID], GlyphTmpBitmap);
        DrawTabStr.Glyph := GlyphTmpBitmap;
      end
      else
        DrawTabStr.Glyph := nil;

      if (itemState and ODS_DISABLED) <> 0 then
      begin
        DrawTabStr.BoxStyle := FTabStyle;
        DrawTabStr.Font_.Assign(FontNormal);
      end
      else if (itemState and ODS_SELECTED) <> 0 then
      begin
        DrawTabStr.BoxStyle := FTabSelectedStyle;
        DrawTabStr.Font_.Assign(FontSelected);
      end
      else
      begin
        DrawTabStr.BoxStyle := FTabStyle;
        DrawTabStr.Font_.Assign(FontNormal);
      end;

      if Assigned(OnGetItemFontColor) then
      begin
        OnGetItemFontColor(self, ItemID, FontColor);
        DrawTabStr.Font_.Color := FontColor;
      end;

      DrawTabStr.GlyphOption := FDrawGlyphsOption;
      DrawTabStr.Wallpaper := FWallpaper;
      DrawTabStr.ClientR := ClientRect;
      DrawTabStr.TabsCount := Tabs.Count;
      DrawTabStr.fButton := LookLikeButtons;
      DrawTabStr.Position := TabsPosition;
      DrawTabStr.Options := Options;
      DrawTabStr.FontDirection := FontDirection;
      if Assigned(OnGetItemColor) then
        OnGetItemColor(self, ItemID, DrawTabStr.BackgrColor_)
      else if aTabColors[ItemID] <> -1 then
        DrawTabStr.BackgrColor_ := aTabColors[ItemID]
      else
        DrawTabStr.BackgrColor_ := DrawTabStr.BoxStyle.BackgrColor;
      DrawOwnTab(DrawTabStr); //FWallpaper.IncludeBevels
    end;
end;

procedure TJvgTabControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if ftoInheriteTabFonts in Options then
  begin
    FTabStyle.Font.Assign(inherited Font);
    FTabSelectedStyle.Font.Assign(inherited Font);
    // Disabled.Assign(inherited Font);
    RemakeFonts;
  end;
end;

procedure TJvgTabControl.RemakeFonts;
const
  RadianEscapments: array[TgllabelDir] of integer = (0, -1800, -900,
    900);
begin
  if csReading in ComponentState then
    exit;
  if fNotFirst then
    DeleteObject(FTabStyle.Font.Handle);
  fNotFirst := true;

  FontNormal.Handle := CreateRotatedFont(FTabStyle.Font,
    RadianEscapments[FFontDirection]);
  FontNormal.Color := FTabStyle.Font.Color;
  FontSelected.Handle := CreateRotatedFont(FTabSelectedStyle.Font,
    RadianEscapments[FFontDirection]);
  FontSelected.Color := FTabSelectedStyle.Font.Color;

end;
//*****************************************_____________PROPERTY METHODS

procedure TJvgTabControl.SetGlyphs(Value: TImageList);
var
  i: word;
label
  SkipAutoGlypsSet;
begin
  if Assigned(FGlyphs) then
    FGlyphs.UnregisterChanges(GlyphsChangeLink);
  FGlyphs := Value;
  if Assigned(FGlyphs) then
  begin
    FGlyphs.RegisterChanges(GlyphsChangeLink);
    SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(FGlyphs.Handle));
    for i := 0 to min(Tabs.Count - 1, FGlyphs.Count - 1) do
      if GlyphIndex[i] <> -1 then
        goto SkipAutoGlypsSet;
    SetSingleGlyph(FSingleGlyph);
    SkipAutoGlypsSet:
  end
  else
    SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(0));
end;

procedure TJvgTabControl.SetGlyphIndex(Index: Integer; imgIndex: Integer);
var
  r: TRect;
  Item: TTCItem;
begin
  Item.iImage := imgIndex;
  Item.mask := TCIF_IMAGE;
  SendMessage(Handle, TCM_SETITEM, Index, Longint(@Item));
  SendMessage(Handle, TCM_GETITEMRECT, Index, Longint(@r));
  InvalidateRect(Handle, @r, true);
end;

function TJvgTabControl.GetGlyphIndex(Index: Integer): Integer;
var
  imgItem: TTCItem;
begin
  if Assigned(FGlyphs) then
  begin
    imgItem.mask := TCIF_IMAGE;
    SendMessage(Handle, TCM_GETITEM, Index, Longint(@imgItem));
    Result := imgItem.iImage;
  end
  else
    Result := -1;
end;

procedure TJvgTabControl.SetSingleGlyph(Value: boolean);
var
  i: word;
begin
  FSingleGlyph := Value;
  if (Tabs = nil) or (FGlyphs = nil) then
    exit;
  if FSingleGlyph then
    for i := 0 to Tabs.Count - 1 do
      GlyphIndex[i] := 0
  else
    for i := 0 to Tabs.Count - 1 do
      if FGlyphs.Count >= i then
        GlyphIndex[i] := i
      else
        break;
end;

procedure TJvgTabControl.SetDrawGlyphsOption(Value: TglWallpaperOption);
begin
  if FDrawGlyphsOption = Value then
    exit;
  FDrawGlyphsOption := Value;
  Invalidate;
end;

procedure TJvgTabControl.SetLookLikeButtons(Value: boolean);
begin
  if FLookLikeButtons = Value then
    exit;
  FLookLikeButtons := Value;
  RecreateWnd;
end;

procedure TJvgTabControl.SetTabsPosition(Value: TglSide);
begin
  if FTabsPosition = Value then
    exit;
  FTabsPosition := Value;
  RecreateWnd;
  if (ftoAutoFontDirection in FOptions) and not (csLoading in ComponentState) then
    FontDirection := FontDirs[TabsPosition];
end;

procedure TJvgTabControl.SetOptions(Value: TglTabOptions);
begin
  if FOptions = Value then
    exit;
  FOptions := Value;
  if ftoAutoFontDirection in FOptions then
    FontDirection := FontDirs[TabsPosition];
  Invalidate;
end;

procedure TJvgTabControl.SetFontDirection(Value: TgllabelDir);
begin
  if FFontDirection = Value then
    exit;
  FFontDirection := Value;
  RemakeFonts;
  Invalidate;
end;

function TJvgTabControl.GetFont: TFont;
begin
  Result := inherited Font;
end;

procedure TJvgTabControl.SetFont(Value: TFont);
begin
  inherited Font := Value;
  if ftoInheriteTabFonts in Options then
  begin
    FTabStyle.Font.Assign(inherited Font);
    FTabSelectedStyle.Font.Assign(inherited Font);
  end;
end;

function TJvgTabControl.GetTabColor(Index: integer): TColor;
begin
  if Index < 100 then
    Result := aTabColors[Index]
  else
    Result := -1;
end;

procedure TJvgTabControl.SetTabColor(Index: integer; Value: TColor);
var
  TCItem: TTCItem;
begin
  if (Index < 100) and (TabColor[Index] <> Value) then
    aTabColors[Index] := Value
  else
    exit;
  if not fSupressDraw then
  begin
    //  Repaint;
    TCItem.mask := TCIF_TEXT;
    TCItem.pszText := PChar(Tabs[Index]);
    SendMessage(Handle, TCM_SETITEM, Index, Longint(@TCItem));
  end;
end;

end.

