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

{ TabControl  component	that can  display  its	pages  captions  in
 3D styles with 3D borders.  Component	can display  glyphs  on  own
 captions and fill background with bitmap.  You  can  set  different
 fonts for selected page caption and for other captions.}

UNIT JvgTab;
INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ComCtrls,
   JvclVer,
   CommCtrl,
   JvgTypes,
   JvgUtils,
   JvgDrawTab,
   JvgTabComm,
   ExtCtrls,
   JvgCommClasses{$IFDEF COMPILER4_UP},
   Imglist{$ENDIF};

CONST
   TCM_SETTEXTCOLOR           = (TCM_FIRST + 36);
TYPE

   TJvgTabControl = CLASS(TTabControl)
   PRIVATE
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
      aTabColors: ARRAY[0..100] OF TColor;
      FAboutJVCL: TJVCLAboutInfo;

      FUNCTION GetGlyphIndex(Index: Integer): Integer;
      PROCEDURE SetGlyphIndex(Index: Integer; imgIndex: Integer);
      PROCEDURE SetGlyphs(Value: TImageList);
      PROCEDURE SetSingleGlyph(Value: boolean);
      PROCEDURE SetDrawGlyphsOption(Value: TglWallpaperOption);
      PROCEDURE SetLookLikeButtons(Value: boolean);
      PROCEDURE SetTabsPosition(Value: TglSide);
      PROCEDURE SetOptions(Value: TglTabOptions);
      PROCEDURE SetFontDirection(Value: TglLabelDir);
      FUNCTION GetFont: TFont;
      PROCEDURE SetFont(Value: TFont);
      FUNCTION GetTabColor(Index: integer): TColor;
      PROCEDURE SetTabColor(Index: integer; Value: TColor);

      PROCEDURE SmthChanged(Sender: TObject);
      PROCEDURE FontsChanged(Sender: TObject);
      PROCEDURE DrawItem(lpDrawItemStr: PDRAWITEMSTRUCT);
      PROCEDURE CNDrawItem(VAR Message: TWMDrawItem); MESSAGE CN_DRAWITEM;
      PROCEDURE CMFontChanged(VAR Message: TMessage); MESSAGE CM_FONTCHANGED;
   PROTECTED
      PROCEDURE GlyphsListChanged(Sender: TObject);
      PROCEDURE WndProc(VAR Message: TMessage); OVERRIDE;
      PROCEDURE CreateParams(VAR Params: TCreateParams); OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      fSupressDraw: boolean;
      PROCEDURE RemakeFonts;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

      PROPERTY GlyphIndex[Index: Integer]: Integer READ GetGlyphIndex WRITE
         SetGlyphIndex;
      PROPERTY TabColor[Index: integer]: TColor READ GetTabColor WRITE
         SetTabColor;
      //     property GlyphState[Index: Integer]: Integer read GetGlyphState write SetGlyphState;
   PUBLISHED
      PROPERTY AboutJVCL: TJVCLAboutInfo READ FAboutJVCL WRITE FAboutJVCL STORED
         False;
      PROPERTY Glyphs: TImageList READ FGlyphs WRITE SetGlyphs;
      PROPERTY SingleGlyph: boolean READ FSingleGlyph WRITE SetSingleGlyph
         DEFAULT false;
      PROPERTY TabStyle: TJvgTabStyle READ FTabStyle WRITE FTabStyle;
      PROPERTY TabSelectedStyle: TJvgTabStyle READ FTabSelectedStyle WRITE
         FTabSelectedStyle;
      PROPERTY Wallpaper: TJvgTabsWallpaper READ FWallpaper WRITE FWallpaper;
      PROPERTY DrawGlyphsOption: TglWallpaperOption
         READ FDrawGlyphsOption WRITE SetDrawGlyphsOption DEFAULT fwoNone;
      PROPERTY LookLikeButtons: boolean READ FLookLikeButtons WRITE
         SetLookLikeButtons
         DEFAULT false;
      PROPERTY TabsPosition: TglSide READ FTabsPosition WRITE SetTabsPosition
         DEFAULT fsdTop;
      PROPERTY Options: TglTabOptions READ FOptions WRITE SetOptions;
      PROPERTY FontDirection: TglLabelDir
         READ FFontDirection WRITE SetFontDirection DEFAULT fldLeftRight;
      PROPERTY Font: TFont READ GetFont WRITE SetFont;
      PROPERTY OnGetItemColor: TglOnGetItemColorEvent READ FOnGetItemColor WRITE
         FOnGetItemColor;
      PROPERTY OnGetItemFontColor: TglOnGetItemColorEvent READ
         FOnGetItemFontColor WRITE FOnGetItemFontColor;
   END;

PROCEDURE Register;

IMPLEMENTATION
CONST
   FontDirs                   : ARRAY[TglSide] OF TglLabelDir
                                    = (fldDownUp, fldLeftRight, fldUpDown,
         fldLeftRight);

PROCEDURE Register;
BEGIN
END;
//*****************************************_____________LowLevel METHODS

CONSTRUCTOR TJvgTabControl.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);

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
   FillMemory(@aTabColors, sizeof(aTabColors), $FF);
END;

DESTRUCTOR TJvgTabControl.Destroy;
BEGIN
   FTabStyle.Free;
   FTabSelectedStyle.Free;
   GlyphTmpBitmap.Free;
   FWallpaper.Free;
   GlyphsChangeLink.Free;
   FontNormal.Free;
   FontSelected.Free;
   DrawTabStr.Font_.Free;
   INHERITED;
END;

PROCEDURE TJvgTabControl.SmthChanged;
BEGIN
   Invalidate;
END;

PROCEDURE TJvgTabControl.FontsChanged;
BEGIN
   RemakeFonts;
   Invalidate;
END;

//---------------------------------------------------special procs

PROCEDURE TJvgTabControl.CreateParams(VAR Params: TCreateParams);
CONST
   PosStyles                  : ARRAY[TglSide] OF DWORD =
      (TCS_VERTICAL, 0, TCS_VERTICAL OR TCS_RIGHT, TCS_BOTTOM
         {or TCS_SCROLLOPPOSITE or TCS_BUTTONS});
BEGIN
   INHERITED CreateParams(Params);
   WITH Params DO
   BEGIN
      IF LookLikeButtons THEN
         Style := Style OR TCS_BUTTONS;
      Style := Style OR TCS_OWNERDRAWFIXED OR PosStyles[FTabsPosition];
   END;
END;                                    // {TCS_HOTTRACK-?};

PROCEDURE TJvgTabControl.Loaded;
BEGIN
   INHERITED Loaded;
   RemakeFonts;
   IF Assigned(Wallpaper.Bitmap) AND (NOT Wallpaper.Bitmap.Empty) THEN
      Wallpaper.bmp := Wallpaper.Bitmap;
END;

PROCEDURE TJvgTabControl.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF Assigned(Wallpaper) AND (AComponent = Wallpaper.Image) AND (Operation =
      opRemove) THEN
      Wallpaper.Image := NIL;
END;

PROCEDURE TJvgTabControl.CNDrawItem(VAR Message: TWMDrawItem);
BEGIN
   DrawItem(Pointer(Message.DrawItemStruct));
END;

PROCEDURE TJvgTabControl.WndProc(VAR Message: TMessage);
VAR
   GlyphID                    : integer;
BEGIN
   INHERITED WndProc(Message);
   WITH Message DO
      CASE Msg OF
         TCM_INSERTITEM:
            BEGIN
               result := 0;
               IF NOT Assigned(FGlyphs) THEN
                  exit;
               GlyphID := -1;
               IF FSingleGlyph THEN
                  GlyphID := 0
               ELSE IF wParam < FGlyphs.Count THEN
                  GlyphID := wParam;
               IF GlyphID = -1 THEN
                  exit;
               TTCItem(Pointer(Message.lParam)^).iImage := GlyphID;
               TTCItem(Pointer(Message.lParam)^).Mask := TCIF_IMAGE;
               SendMessage(handle, TCM_SETITEM, wParam, lParam);
            END;
         TCM_DELETEITEM:
            BEGIN
            END;
         TCM_DELETEALLITEMS:
            BEGIN
            END;
      END;
END;

PROCEDURE TJvgTabControl.GlyphsListChanged(Sender: TObject);
BEGIN
   IF HandleAllocated THEN
      SendMessage(Handle, TCM_SETIMAGELIST, 0,
         Longint(TImageList(Sender).Handle));
END;

PROCEDURE TJvgTabControl.DrawItem(lpDrawItemStr: PDRAWITEMSTRUCT);
//var lpTabBoxStyle:^TTabBoxStyle;
VAR
   FontColor                  : TColor;
BEGIN
   IF fSupressDraw THEN
      exit;
   WITH lpDrawItemStr^ DO
      IF CtlType = ODT_TAB THEN
      BEGIN
         //fLoaded:=true; Options:=NewOptions;
         DrawTabStr.lpDrawItemStr := lpDrawItemStr;
         DrawTabStr.Caption := Tabs[ItemID];

         IF GlyphIndex[ItemID] <> -1 THEN
         BEGIN
            FGlyphs.GetBitmap(GlyphIndex[ItemID], GlyphTmpBitmap);
            DrawTabStr.Glyph := GlyphTmpBitmap;
         END
         ELSE
            DrawTabStr.Glyph := NIL;

         IF (itemState AND ODS_DISABLED) <> 0 THEN
         BEGIN
            DrawTabStr.BoxStyle := FTabStyle;
            DrawTabStr.Font_.Assign(FontNormal);
         END
         ELSE IF (itemState AND ODS_SELECTED) <> 0 THEN
         BEGIN
            DrawTabStr.BoxStyle := FTabSelectedStyle;
            DrawTabStr.Font_.Assign(FontSelected);
         END
         ELSE
         BEGIN
            DrawTabStr.BoxStyle := FTabStyle;
            DrawTabStr.Font_.Assign(FontNormal);
         END;

         IF Assigned(OnGetItemFontColor) THEN
         BEGIN
            OnGetItemFontColor(self, ItemID, FontColor);
            DrawTabStr.Font_.Color := FontColor;
         END;

         DrawTabStr.GlyphOption := FDrawGlyphsOption;
         DrawTabStr.Wallpaper := FWallpaper;
         DrawTabStr.ClientR := ClientRect;
         DrawTabStr.TabsCount := Tabs.Count;
         DrawTabStr.fButton := LookLikeButtons;
         DrawTabStr.Position := TabsPosition;
         DrawTabStr.Options := Options;
         DrawTabStr.FontDirection := FontDirection;
         IF Assigned(OnGetItemColor) THEN
            OnGetItemColor(self, ItemID, DrawTabStr.BackgrColor_)
         ELSE IF aTabColors[ItemID] <> -1 THEN
            DrawTabStr.BackgrColor_ := aTabColors[ItemID]
         ELSE
            DrawTabStr.BackgrColor_ := DrawTabStr.BoxStyle.BackgrColor;
         DrawOwnTab(DrawTabStr);        //FWallpaper.IncludeBevels
      END;
END;

PROCEDURE TJvgTabControl.CMFontChanged(VAR Message: TMessage);
BEGIN
   INHERITED;
   IF ftoInheriteTabFonts IN Options THEN
   BEGIN
      FTabStyle.Font.Assign(INHERITED Font);
      FTabSelectedStyle.Font.Assign(INHERITED Font);
      // Disabled.Assign(inherited Font);
      RemakeFonts;
   END;
END;

PROCEDURE TJvgTabControl.RemakeFonts;
CONST
   RadianEscapments              : ARRAY[TgllabelDir] OF integer = (0, -1800, -900,
      900);
BEGIN
   IF csReading IN ComponentState THEN
      exit;
   IF fNotFirst THEN
      DeleteObject(FTabStyle.Font.Handle);
   fNotFirst := true;

   FontNormal.Handle := CreateRotatedFont(FTabStyle.Font,
      RadianEscapments[FFontDirection]);
   FontNormal.Color := FTabStyle.Font.Color;
   FontSelected.Handle := CreateRotatedFont(FTabSelectedStyle.Font,
      RadianEscapments[FFontDirection]);
   FontSelected.Color := FTabSelectedStyle.Font.Color;

END;
//*****************************************_____________PROPERTY METHODS

PROCEDURE TJvgTabControl.SetGlyphs(Value: TImageList);
VAR
   i                          : word;
LABEL
   SkipAutoGlypsSet;
BEGIN
   IF Assigned(FGlyphs) THEN
      FGlyphs.UnregisterChanges(GlyphsChangeLink);
   FGlyphs := Value;
   IF Assigned(FGlyphs) THEN
   BEGIN
      FGlyphs.RegisterChanges(GlyphsChangeLink);
      SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(FGlyphs.Handle));
      FOR i := 0 TO min(Tabs.Count - 1, FGlyphs.Count - 1) DO
         IF GlyphIndex[i] <> -1 THEN
            GOTO SkipAutoGlypsSet;
      SetSingleGlyph(FSingleGlyph);
      SkipAutoGlypsSet:
   END
   ELSE
      SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(0));
END;

PROCEDURE TJvgTabControl.SetGlyphIndex(Index: Integer; imgIndex: Integer);
VAR
   r                          : TRect;
   Item                       : TTCItem;
BEGIN
   Item.iImage := imgIndex;
   Item.mask := TCIF_IMAGE;
   SendMessage(Handle, TCM_SETITEM, Index, Longint(@Item));
   SendMessage(Handle, TCM_GETITEMRECT, Index, Longint(@r));
   InvalidateRect(Handle, @r, true);
END;

FUNCTION TJvgTabControl.GetGlyphIndex(Index: Integer): Integer;
VAR
   imgItem                    : TTCItem;
BEGIN
   IF Assigned(FGlyphs) THEN
   BEGIN
      imgItem.mask := TCIF_IMAGE;
      SendMessage(Handle, TCM_GETITEM, Index, Longint(@imgItem));
      Result := imgItem.iImage;
   END
   ELSE
      Result := -1;
END;

PROCEDURE TJvgTabControl.SetSingleGlyph(Value: boolean);
VAR
   i                          : word;
BEGIN
   FSingleGlyph := Value;
   IF (Tabs = NIL) OR (FGlyphs = NIL) THEN
      exit;
   IF FSingleGlyph THEN
      FOR i := 0 TO Tabs.Count - 1 DO
         GlyphIndex[i] := 0
   ELSE
      FOR i := 0 TO Tabs.Count - 1 DO
         IF FGlyphs.Count >= i THEN
            GlyphIndex[i] := i
         ELSE
            break;
END;

PROCEDURE TJvgTabControl.SetDrawGlyphsOption(Value: TglWallpaperOption);
BEGIN
   IF FDrawGlyphsOption = Value THEN
      exit;
   FDrawGlyphsOption := Value;
   Invalidate;
END;

PROCEDURE TJvgTabControl.SetLookLikeButtons(Value: boolean);
BEGIN
   IF FLookLikeButtons = Value THEN
      exit;
   FLookLikeButtons := Value;
   RecreateWnd;
END;

PROCEDURE TJvgTabControl.SetTabsPosition(Value: TglSide);
BEGIN
   IF FTabsPosition = Value THEN
      exit;
   FTabsPosition := Value;
   RecreateWnd;
   IF (ftoAutoFontDirection IN FOptions) AND NOT (csLoading IN ComponentState)
      THEN
      FontDirection := FontDirs[TabsPosition];
END;

PROCEDURE TJvgTabControl.SetOptions(Value: TglTabOptions);
BEGIN
   IF FOptions = Value THEN
      exit;
   FOptions := Value;
   IF ftoAutoFontDirection IN FOptions THEN
      FontDirection := FontDirs[TabsPosition];
   Invalidate;
END;

PROCEDURE TJvgTabControl.SetFontDirection(Value: TgllabelDir);
BEGIN
   IF FFontDirection = Value THEN
      exit;
   FFontDirection := Value;
   RemakeFonts;
   Invalidate;
END;

FUNCTION TJvgTabControl.GetFont: TFont;
BEGIN
   Result := INHERITED Font;
END;

PROCEDURE TJvgTabControl.SetFont(Value: TFont);
BEGIN
   INHERITED Font := Value;
   IF ftoInheriteTabFonts IN Options THEN
   BEGIN
      FTabStyle.Font.Assign(INHERITED Font);
      FTabSelectedStyle.Font.Assign(INHERITED Font);
   END;
END;

FUNCTION TJvgTabControl.GetTabColor(Index: integer): TColor;
BEGIN
   IF Index < 100 THEN
      Result := aTabColors[Index]
   ELSE
      Result := -1;
END;

PROCEDURE TJvgTabControl.SetTabColor(Index: integer; Value: TColor);
VAR
   TCItem                     : TTCItem;
BEGIN
   IF (Index < 100) AND (TabColor[Index] <> Value) THEN
      aTabColors[Index] := Value
   ELSE
      exit;
   IF NOT fSupressDraw THEN
   BEGIN
      //  Repaint;
      TCItem.mask := TCIF_TEXT;
      TCItem.pszText := PChar(Tabs[Index]);
      SendMessage(Handle, TCM_SETITEM, Index, Longint(@TCItem));
   END;
END;

END.

