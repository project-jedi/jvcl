{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTab.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  TabControl  component that can  display  its  pages  captions   in
  3D styles with 3D borders.  Component  can display  glyphs  on  own
  captions and fill background with bitmap.  You  can  set  different
  fonts for selected page caption and for other captions.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgTab;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, ExtCtrls, ImgList,
  JvclVer, JvgTypes, JvgUtils, JvgDrawTab, JvgTabComm, JvgCommClasses;

// (rom) disabled  unused
//const
//  TCM_SETTEXTCOLOR = TCM_FIRST + 36;

type
  TJvgTabControl = class(TTabControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FGlyphs: TImageList;
    FSingleGlyph: Boolean;
    FTabStyle: TJvgTabStyle;
    FTabSelectedStyle: TJvgTabStyle;
    FWallpaper: TJvgTabsWallpaper;
    FDrawGlyphsOption: TglWallpaperOption;
    FLookLikeButtons: Boolean;
    FTabsPosition: TglSide;
    FOptions: TglTabOptions;
    FFontDirection: TglLabelDir;
    FOnGetItemColor: TglOnGetItemColorEvent;
    FOnGetItemFontColor: TglOnGetItemColorEvent;
    FSuppressDraw: Boolean;
    FGlyphsChangeLink: TChangeLink;
    FDrawTabStr: TDRAWTABSTRUCT;
    FGlyphTmpBitmap: TBitmap;
    FFontNormal: TFont;
    FFontSelected: TFont;
    FNotFirst: Boolean;
    FTabColors: array [0..100] of TColor;
    function GetGlyphIndex(Index: Integer): Integer;
    procedure SetGlyphIndex(Index: Integer; ImgIndex: Integer);
    procedure SetGlyphs(Value: TImageList);
    procedure SetSingleGlyph(Value: Boolean);
    procedure SetDrawGlyphsOption(Value: TglWallpaperOption);
    procedure SetLookLikeButtons(Value: Boolean);
    procedure SetTabsPosition(Value: TglSide);
    procedure SetOptions(Value: TglTabOptions);
    procedure SetFontDirection(Value: TglLabelDir);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetTabColor(Index: Integer): TColor;
    procedure SetTabColor(Index: Integer; Value: TColor);
    procedure SmthChanged(Sender: TObject);
    procedure FontsChanged(Sender: TObject);
    procedure DrawItem(lpDrawItemStr: PDrawItemStruct);
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
  protected
    procedure GlyphsListChanged(Sender: TObject);
    procedure WndProc(var Mesg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure RemakeFonts;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property GlyphIndex[Index: Integer]: Integer read GetGlyphIndex write SetGlyphIndex;
    property TabColor[Index: Integer]: TColor read GetTabColor write SetTabColor;
    //     property GlyphState[Index: Integer]: Integer read GetGlyphState write SetGlyphState;
    property SuppressDraw: Boolean read FSuppressDraw write FSuppressDraw;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property SingleGlyph: Boolean read FSingleGlyph write SetSingleGlyph default False;
    property TabStyle: TJvgTabStyle read FTabStyle write FTabStyle;
    property TabSelectedStyle: TJvgTabStyle read FTabSelectedStyle write FTabSelectedStyle;
    property Wallpaper: TJvgTabsWallpaper read FWallpaper write FWallpaper;
    property DrawGlyphsOption: TglWallpaperOption
      read FDrawGlyphsOption write SetDrawGlyphsOption default fwoNone;
    property LookLikeButtons: Boolean read FLookLikeButtons write SetLookLikeButtons
      default False;
    property TabsPosition: TglSide read FTabsPosition write SetTabsPosition default fsdTop;
    property Options: TglTabOptions read FOptions write SetOptions;
    property FontDirection: TglLabelDir read FFontDirection write SetFontDirection default fldLeftRight;
    property Font: TFont read GetFont write SetFont;
    property OnGetItemColor: TglOnGetItemColorEvent read FOnGetItemColor write FOnGetItemColor;
    property OnGetItemFontColor: TglOnGetItemColorEvent read FOnGetItemFontColor write FOnGetItemFontColor;
  end;

implementation

uses
  Math;

const
  FontDirs: array [TglSide] of TglLabelDir =
    (fldDownUp, fldLeftRight, fldUpDown, fldLeftRight);

constructor TJvgTabControl.Create(AOwner: TComponent);
const
  cFontName = 'Arial';
begin
  inherited Create(AOwner);
  TabStop := False;
  FTabStyle := TJvgTabStyle.Create(Self);
  FTabSelectedStyle := TJvgTabStyle.Create(Self);
  FWallpaper := TJvgTabsWallpaper.Create;
  FFontNormal := TFont.Create;
  FFontSelected := TFont.Create;
  FDrawTabStr.Font_ := TFont.Create;

  FTabStyle.Font.Name := cFontName;
  FTabSelectedStyle.Font.Name := cFontName;

  //  if csDesigning in ComponentState then
  //    FTabSelectedStyle.BackgrColor := clbtnHighlight;

  FGlyphTmpBitmap := TBitmap.Create;
  FGlyphsChangeLink := TChangeLink.Create;
  FGlyphsChangeLink.OnChange := GlyphsListChanged;

  //...set defaults
  FSingleGlyph := False;

  FDrawGlyphsOption := fwoNone;
  FTabsPosition := fsdTop;
  FOptions := [ftoAutoFontDirection, ftoExcludeGlyphs];
  FFontDirection := fldLeftRight;

  FTabStyle.OnChanged := SmthChanged;
  FTabSelectedStyle.OnChanged := SmthChanged;
  FTabStyle.OnFontChanged := FontsChanged;
  FTabSelectedStyle.OnFontChanged := FontsChanged;
  FWallpaper.OnChanged := SmthChanged;
  FillChar(FTabColors, SizeOf(FTabColors), $FF);
end;

destructor TJvgTabControl.Destroy;
begin
  FTabStyle.Free;
  FTabSelectedStyle.Free;
  FGlyphTmpBitmap.Free;
  FWallpaper.Free;
  FGlyphsChangeLink.Free;
  FFontNormal.Free;
  FFontSelected.Free;
  FDrawTabStr.Font_.Free;
  inherited Destroy;
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

procedure TJvgTabControl.CreateParams(var Params: TCreateParams);
const
  PosStyles: array [TglSide] of DWORD =
    (TCS_VERTICAL, 0, TCS_VERTICAL or TCS_RIGHT, TCS_BOTTOM);
    // or TCS_SCROLLOPPOSITE or TCS_BUTTONS
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if LookLikeButtons then
      Style := Style or TCS_BUTTONS;
    Style := Style or TCS_OWNERDRAWFIXED or PosStyles[FTabsPosition];
  end;
end;

procedure TJvgTabControl.Loaded;
begin
  inherited Loaded;
  RemakeFonts;
  if Assigned(Wallpaper.Bitmap) and (not Wallpaper.Bitmap.Empty) then
    Wallpaper.Bmp := Wallpaper.Bitmap;
end;

procedure TJvgTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(Wallpaper) and (AComponent = Wallpaper.Image) and
    (Operation = opRemove) then
    Wallpaper.Image := nil;
end;

procedure TJvgTabControl.CNDrawItem(var Msg: TWMDrawItem);
begin
  DrawItem(Pointer(Msg.DrawItemStruct));
end;

procedure TJvgTabControl.WndProc(var Mesg: TMessage);
var
  GlyphID: Integer;
begin
  inherited WndProc(Mesg);
  with Mesg do
    case Msg of
      TCM_INSERTITEM:
        begin
          Result := 0;
          if not Assigned(FGlyphs) then
            Exit;
          GlyphID := -1;
          if FSingleGlyph then
            GlyphID := 0
          else
          if wParam < FGlyphs.Count then
            GlyphID := wParam;
          if GlyphID = -1 then
            Exit;
          TTCItem(Pointer(Mesg.LParam)^).iImage := GlyphID;
          TTCItem(Pointer(Mesg.LParam)^).Mask := TCIF_IMAGE;
          SendMessage(Handle, TCM_SETITEM, WParam, LParam);
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
    SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(TImageList(Sender).Handle));
end;

procedure TJvgTabControl.DrawItem(lpDrawItemStr: PDrawItemStruct);
var
  FontColor: TColor;
begin
  if SuppressDraw then
    Exit;
  with lpDrawItemStr^ do
    if CtlType = ODT_TAB then
    begin
      //fLoaded:=True; Options:=NewOptions;
      FDrawTabStr.lpDrawItemStr := lpDrawItemStr;
      FDrawTabStr.Caption := Tabs[ItemID];

      if GlyphIndex[ItemID] <> -1 then
      begin
        FGlyphs.GetBitmap(GlyphIndex[ItemID], FGlyphTmpBitmap);
        FDrawTabStr.Glyph := FGlyphTmpBitmap;
      end
      else
        FDrawTabStr.Glyph := nil;

      if (itemState and ODS_DISABLED) <> 0 then
      begin
        FDrawTabStr.BoxStyle := FTabStyle;
        FDrawTabStr.Font_.Assign(FFontNormal);
      end
      else
      if (itemState and ODS_SELECTED) <> 0 then
      begin
        FDrawTabStr.BoxStyle := FTabSelectedStyle;
        FDrawTabStr.Font_.Assign(FFontSelected);
      end
      else
      begin
        FDrawTabStr.BoxStyle := FTabStyle;
        FDrawTabStr.Font_.Assign(FFontNormal);
      end;

      if Assigned(OnGetItemFontColor) then
      begin
        OnGetItemFontColor(Self, ItemID, FontColor);
        FDrawTabStr.Font_.Color := FontColor;
      end;

      FDrawTabStr.GlyphOption := FDrawGlyphsOption;
      FDrawTabStr.Wallpaper := FWallpaper;
      FDrawTabStr.ClientR := ClientRect;
      FDrawTabStr.TabsCount := Tabs.Count;
      FDrawTabStr.fButton := LookLikeButtons;
      FDrawTabStr.Position := TabsPosition;
      FDrawTabStr.Options := Options;
      FDrawTabStr.FontDirection := FontDirection;
      if Assigned(FOnGetItemColor) then
        FOnGetItemColor(Self, ItemID, FDrawTabStr.BackgrColor_)
      else
      if FTabColors[ItemID] <> -1 then
        FDrawTabStr.BackgrColor_ := FTabColors[ItemID]
      else
        FDrawTabStr.BackgrColor_ := FDrawTabStr.BoxStyle.BackgrColor;
      DrawOwnTab(FDrawTabStr); //FWallpaper.IncludeBevels
    end;
end;

procedure TJvgTabControl.CMFontChanged(var Msg: TMessage);
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
  RadianEscapments: array [TglLabelDir] of Integer =
    (0, -1800, -900, 900);
begin
  if csReading in ComponentState then
    Exit;
  if FNotFirst then
    DeleteObject(FTabStyle.Font.Handle);
  FNotFirst := True;

  FFontNormal.Handle := CreateRotatedFont(FTabStyle.Font,
    RadianEscapments[FFontDirection]);
  FFontNormal.Color := FTabStyle.Font.Color;
  FFontSelected.Handle := CreateRotatedFont(FTabSelectedStyle.Font,
    RadianEscapments[FFontDirection]);
  FFontSelected.Color := FTabSelectedStyle.Font.Color;

end;

procedure TJvgTabControl.SetGlyphs(Value: TImageList);
var
  I: Word;
  B: Boolean;
begin
  if Assigned(FGlyphs) then
    FGlyphs.UnregisterChanges(FGlyphsChangeLink);
  FGlyphs := Value;
  if Assigned(FGlyphs) then
  begin
    FGlyphs.RegisterChanges(FGlyphsChangeLink);
    SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(FGlyphs.Handle));
    B := True;
    for I := 0 to Min(Tabs.Count - 1, FGlyphs.Count - 1) do
      if GlyphIndex[I] <> -1 then
      begin
        B := False;
        Break;
      end;
    if B then
      SetSingleGlyph(FSingleGlyph);
  end
  else
    SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(0));
end;

procedure TJvgTabControl.SetGlyphIndex(Index: Integer; ImgIndex: Integer);
var
  R: TRect;
  Item: TTCItem;
begin
  Item.iImage := ImgIndex;
  Item.mask := TCIF_IMAGE;
  SendMessage(Handle, TCM_SETITEM, Index, Longint(@Item));
  SendMessage(Handle, TCM_GETITEMRECT, Index, Longint(@R));
  InvalidateRect(Handle, @R, True);
end;

function TJvgTabControl.GetGlyphIndex(Index: Integer): Integer;
var
  ImgItem: TTCItem;
begin
  if Assigned(FGlyphs) then
  begin
    ImgItem.mask := TCIF_IMAGE;
    SendMessage(Handle, TCM_GETITEM, Index, Longint(@ImgItem));
    Result := ImgItem.iImage;
  end
  else
    Result := -1;
end;

procedure TJvgTabControl.SetSingleGlyph(Value: Boolean);
var
  I: Word;
begin
  FSingleGlyph := Value;
  if (Tabs = nil) or (FGlyphs = nil) then
    Exit;
  if FSingleGlyph then
    for I := 0 to Tabs.Count - 1 do
      GlyphIndex[I] := 0
  else
    for I := 0 to Tabs.Count - 1 do
      if FGlyphs.Count >= I then
        GlyphIndex[I] := I
      else
        Break;
end;

procedure TJvgTabControl.SetDrawGlyphsOption(Value: TglWallpaperOption);
begin
  if FDrawGlyphsOption <> Value then
  begin
    FDrawGlyphsOption := Value;
    Invalidate;
  end;
end;

procedure TJvgTabControl.SetLookLikeButtons(Value: Boolean);
begin
  if FLookLikeButtons <> Value then
  begin
    FLookLikeButtons := Value;
    RecreateWnd;
  end;
end;

procedure TJvgTabControl.SetTabsPosition(Value: TglSide);
begin
  if FTabsPosition <> Value then
  begin
    FTabsPosition := Value;
    RecreateWnd;
    if (ftoAutoFontDirection in FOptions) and not (csLoading in ComponentState) then
      FontDirection := FontDirs[TabsPosition];
  end;
end;

procedure TJvgTabControl.SetOptions(Value: TglTabOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if ftoAutoFontDirection in FOptions then
      FontDirection := FontDirs[TabsPosition];
    Invalidate;
  end;
end;

procedure TJvgTabControl.SetFontDirection(Value: TgllabelDir);
begin
  if FFontDirection <> Value then
  begin
    FFontDirection := Value;
    RemakeFonts;
    Invalidate;
  end;
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

function TJvgTabControl.GetTabColor(Index: Integer): TColor;
begin
  if Index < 100 then
    Result := FTabColors[Index]
  else
    Result := -1;
end;

procedure TJvgTabControl.SetTabColor(Index: Integer; Value: TColor);
var
  TCItem: TTCItem;
begin
  if (Index < 100) and (TabColor[Index] <> Value) then
    FTabColors[Index] := Value
  else
    Exit;
  if not SuppressDraw then
  begin
    //  Repaint;
    TCItem.mask := TCIF_TEXT;
    TCItem.pszText := PChar(Tabs[Index]);
    SendMessage(Handle, TCM_SETITEM, Index, Longint(@TCItem));
  end;
end;

end.

