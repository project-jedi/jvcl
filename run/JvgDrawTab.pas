{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDrawTab.PAS, released on 2003-01-15.

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

unit JvgDrawTab;

interface

uses Windows, CommCtrl, graphics, Classes, ExtCtrls,
  JvgTypes, JvgUtils, JvgTabComm;

procedure DrawOwnTab(DrawTabStr: TDRAWTABSTRUCT);

implementation

procedure DrawOwnTab(DrawTabStr: TDRAWTABSTRUCT);
var
  hPen_, hOldPen_: HPEN;
  r, r_, AddR, ItemR: TRect;
  TransparentColor: TCOLORREF;
  x, y: integer;
  fSelected, fVert, fHor: boolean;
  fWallpaper: boolean;
  WallpaperOption: TglWallpaperOption;
  Size: TSIZE;
  OldFont: Windows.HFont;
  GrFromColor, GrToColor: TColor;
const
  aWordWrap: array[boolean] of UINT = (DT_SINGLELINE, DT_WORDBREAK);
begin

  if not Assigned(DrawTabStr.lpDrawItemStr) then exit;
  with DrawTabStr, DrawTabStr.lpDrawItemStr^, DrawTabStr.BoxStyle do
  begin
    fSelected := bool(itemState and ODS_SELECTED);
    //_________________________________________WALLPAPER WORKS_
    WallpaperOption := fwoNone;
    fWallpaper := IsItAFilledBitmap(Wallpaper.Bmp);
    if fWallpaper then
    begin
      if Wallpaper.Tile then WallpaperOption := fwoTile;
      //...calc and fill tabs background
      if (ItemID = UINT(TabsCount - 1)) and (Wallpaper.FillCaptionBakgr) then
      begin
        AddR := ClientR;
        ItemR := rcItem;
        AddR.Top := 0;
        AddR.Left := ItemR.Right;
        AddR.Bottom := ItemR.Bottom;
        if fButton then
        begin
          inc(AddR.Bottom, 3);
        end
        else
          inc(AddR.Bottom, 2); //if fSelected then AddR.Bottom:=AddR.Bottom-2;
        CreateBitmapExt(HDC, Wallpaper.Bmp, AddR, 0, 0,
          WallpaperOption, fdsDefault, false, 0, clBlack);
      end;
      //...calc client area exclude tabs and user-client area
      r := ClientR;
      case Position of
        fsdTop:
          begin
            r.top := r.top + rcItem.Bottom - rcItem.Top;
            if fButton then
            begin
              inc(r.top, 4);
            end
            else if fSelected then
              dec(r.top)
            else
              inc(r.top, 5);
            dec(r.right);
            dec(r.bottom);
            inc(r.left);
          end;
        fsdBottom:
          begin
            r.bottom := r.bottom - (rcItem.Bottom - rcItem.Top);
            if fButton then
            begin
              dec(r.bottom, 4);
            end
            else if fSelected then
              dec(r.bottom)
            else
              dec(r.bottom, 5);
            dec(r.right);
            inc(r.top);
            inc(r.left);
          end;
        fsdLeft:
          begin
            r.left := r.left + (rcItem.Right - rcItem.Left);
            if fButton then
            begin
              inc(r.left, 3);
            end
            else if fSelected then
              dec(r.left)
            else
              inc(r.left, 5);
            dec(r.right);
            dec(r.bottom);
            inc(r.top);
          end;
        fsdRight:
          begin
            r.right := r.right - (rcItem.Right - rcItem.Left);
            if fButton then
            begin
              dec(r.right, 4);
            end
            else if fSelected then
              dec(r.right)
            else
              dec(r.right, 5);
            inc(r.left);
            dec(r.bottom);
            inc(r.top);
          end;
      end;
      //...fill client exclude tabs
      if Wallpaper.FillClient then
        CreateBitmapExt(HDC, Wallpaper.Bmp, r, 0, 0,
          WallpaperOption, fdsDefault, false, 0, clBlack);
      //...calc tab to fill
      if Wallpaper.FillCaptions and Wallpaper.IncludeBevels then
      begin
        r := rcItem;
        if not fButton then
          case Position of
            fsdTop: dec(r.bottom);
            fsdLeft: dec(r.right);
            fsdRight: if not fSelected then dec(r.Left);
          end;
        CreateBitmapExt(HDC, Wallpaper.Bmp, r, 0, 0,
          WallpaperOption, fdsDefault, false, 0, clBlack);
      end;
    end; //...end wallpaper works. uf! [`:-)
    //_________________________________________CORRECT TAB RECT_ to draw bevels
    r := rcItem;
    if fButton then
    begin
      if not fSelected then OffsetRect(r, -1, -1);
    end
    else
    begin
      if fSelected then
      begin
        //      InflateRect(R, 4, 4);
        hPen_ := CreatePen(PS_SOLID, 1, ColorToRGB(clBtnface));
        hOldPen_ := SelectObject(HDC, hPen_);
        case Position of
          fsdTop:
            begin {
              MoveToEx( HDC, rcItem.Left, rcItem.Bottom-2, nil );
              LineTo( HDC, rcItem.Right, rcItem.Bottom-2 );}
              r.Left := r.Left + 3;
              r.right := r.right - 4;
              r.top := r.top + 2;
              //if FInteriorOffset then InflateRect(r,1,0);
            end;
          fsdBottom:
            begin
              MoveToEx(HDC, rcItem.Left, rcItem.Top + 1, nil);
              LineTo(HDC, rcItem.Right - 2, rcItem.Top + 1);
              MoveToEx(HDC, rcItem.Left, rcItem.Top, nil);
              LineTo(HDC, rcItem.Right - 2, rcItem.Top);
              r.Left := r.Left + 3;
              r.right := r.right - 4;
              dec(r.bottom);
              //if FInteriorOffset then InflateRect(r,1,0);
            end;
          fsdLeft:
            begin
              inc(r.Left, 2);
              dec(r.right, 2);
              inc(r.Top, 2);
              dec(r.Bottom, 2);
              //if FInteriorOffset then inc(r.Bottom);
            end;
          fsdRight:
            begin
              MoveToEx(HDC, rcItem.Left, rcItem.Top, nil);
              LineTo(HDC, rcItem.Left, rcItem.Bottom - 2);
              MoveToEx(HDC, rcItem.Left + 1, rcItem.Top, nil);
              LineTo(HDC, rcItem.Left + 1, rcItem.Bottom - 2);
              inc(r.Left, 0);
              dec(r.right, 4);
              inc(r.Top, 2);
              dec(r.Bottom, 2);
              //if FInteriorOffset then inc(r.Bottom);
            end;
        end;
        DeleteObject(SelectObject(HDC, hOldPen_));
      end
      else
      begin
        case Position of
          fsdTop:
            begin
              inc(r.Left);
              dec(r.right, 2);
              OffsetRect(r, 0, 2);
              //if FInteriorOffset then InflateRect(r,1,0);
            end;
          fsdBottom:
            begin
              inc(r.Left);
              dec(r.right, 2);
              if not fSelected then OffsetRect(r, 0, -1);
              if not (fsdTop in Borders) then dec(r.top);
              //if FInteriorOffset then InflateRect(r,1,0);
            end;
          fsdLeft:
            begin
              inc(r.Left, 2); {if FInteriorOffset then inc(r.Bottom);}
            end;
          fsdRight:
            begin
              dec(r.right, 4); {if FInteriorOffset then inc(r.Bottom);}
            end;
        end;
      end;
      dec(r.bottom, 2);
    end;
    //_______________________________________________DRAW BEVELS_
    if Assigned(Gradient) and Gradient.Active then
    begin
      //      GrFromColor := Gradient.FRGBFromColor;
      //      GrToColor := Gradient.FRGBToColor;
      //      if BackgrColor_ > 0 then begin
      //        if ftoTabColorAsGradientFrom in Options then Gradient.FRGBFromColor := BackgrColor_;
      //        if ftoTabColorAsGradientTo in Options then Gradient.FRGBToColor := BackgrColor_;
      //      end;
      GradientBox(HDC, r, Gradient, integer(psSolid), 1);
      //      Gradient.FRGBFromColor := GrFromColor;
      //      Gradient.FRGBToColor := GrToColor;
    end;

    if DrawTabStr.FlatButtons then InflateRect(r, 3, 3);
    r := DrawBoxEx(HDC, r, Borders, BevelInner, BevelOuter, Bold, BackgrColor_, (Wallpaper.FillCaptions and fWallpaper) or Gradient.Active);
    if DrawTabStr.FlatButtons then InflateRect(r, -3, -3);
    //_________________________________________DRAW caption BACKGROUND_
    if Wallpaper.FillCaptions and (not Wallpaper.IncludeBevels) then
      CreateBitmapExt(HDC, Wallpaper.Bmp, r, 0, 0,
        WallpaperOption, fdsDefault, false, 0, clBlack);
    inc(r.bottom); //inc(r.right);
    //_______________________________________________DRAW GLYPH_
    r_ := r;
    fHor := (Position = fsdTop) or (Position = fsdBottom);
    fVert := not fHor;
    if IsItAFilledBitmap(Glyph) and (not (ftoHideGlyphs in Options)) then
    begin
      //if GlyphOption = fwoNone then ?
      case GlyphHAlign of
        fhaLeft:
          begin
            x := 1;
            if fHor then
              inc(r_.left, Glyph.Width + 1)
          end;
        fhaCenter: x := (r.Right - r.Left - Glyph.Width) div 2;
      else {fhaRight}
        begin
          x := r.Right - r.Left - Glyph.Width - 1;
          if fHor then dec(r_.right, Glyph.Width + 1);
        end;
      end;
      case GlyphVAlign of
        fvaTop:
          begin
            y := 0;
            if fVert then inc(r_.top, Glyph.Height + 1);
          end;
        fvaCenter: y := (r.Bottom - r.Top - Glyph.Height) div 2;
      else {fvaBottom}
        begin
          y := r.Bottom - r.top - Glyph.Height;
          if fVert then dec(r_.bottom, Glyph.Height + 1);
        end;
      end;
      TransparentColor := GetPixel(Glyph.Canvas.Handle, 0, Glyph.Height - 1);
      //    MoveToEx( hDC, r_.left+x, r_.top+y, nil );
      //    LineTo( hDC, r_.right, r_.bottom );
      CreateBitmapExt(HDC, Glyph,
        r, x, y, GlyphOption, fdsDefault,
        true, TransparentColor, clBlack);
      if GlyphOption <> fwoNone then
        r_ := r
      else
        r_.Left := r_.Left + 1;
    end; //...end draw glyph_

    //   case Position of
   //	fsdTop,fsdBottom: begin inc(r_.Left,2); dec(r_.Right,2); end;
   //    end;

    if not (ftoExcludeGlyphs in Options) then r_ := r;
    SetBkMode(HDC, TRANSPARENT);
    OldFont := SelectObject(HDC, Font_.Handle);
    SetTextColor(HDC, ColorToRGB(Font_.Color));
    GetTextExtentPoint32(HDC, PChar(Caption), length(Caption), Size);
    SelectObject(HDC, OldFont);
    x := 0;
    y := 0;
    case FontDirection of
      fldLeftRight:
        begin
          DrawTextInRectWithAlign(HDC, r_, Caption,
            CaptionHAlign,
            CaptionVAlign,
            TextStyle, Font_, aWordWrap[ftoWordWrap in Options]);
          exit;
        end;
      fldRightLeft:
        begin
          case DrawTabStr.BoxStyle.CaptionHAlign of
            fhaLeft: x := r_.right;
            fhaCenter: x := r_.left + (r_.right - r_.left + Size.cx) div 2;
            fhaRight: y := r_.left + Size.cx;
          end;
          case CaptionVAlign of
            fvaTop: y := r_.bottom;
            fvaCenter: y := r_.top + (r_.bottom - r_.top + Size.cy) div 2;
            fvaBottom: y := r_.top + Size.cy;
          end;
        end;
      fldDownUp:
        begin
          x := r_.left;
          y := r_.bottom - 4;
          case CaptionHAlign of
            fhaCenter: dec(y, (r_.bottom - r_.top - Size.cx) div 2);
            fhaRight: y := r_.top + Size.cx + 3;
          end;
          case CaptionVAlign of
            fvaCenter: inc(x, (r_.right - r_.left - Size.cy) div 2);
            fvaBottom: x := r_.right - Size.cy - 1;
          end;
        end;
    else {fldUpDown}
      begin
        x := r_.right;
        y := r_.top + 4;
        case CaptionHAlign of
          fhaCenter:
            begin
              inc(y, (r_.bottom - r_.top - Size.cx) div 2);
            end;
          fhaRight: y := r_.bottom - Size.cx - 3;
        end;
        case CaptionVAlign of
          fvaCenter:
            begin
              dec(x, (r_.right - r_.left - Size.cy) div 2);
            end;
          fvaBottom: x := r_.left + Size.cy;
        end;
      end;
    end;
    ExtTextOutExt(HDC, x, y, r_, PChar(Caption), TextStyle, false, false, Font.Color, 0 {DelinColor},
      clBtnHighlight, clBtnShadow, nil, nil, Font_);

  end;
end;

end.
