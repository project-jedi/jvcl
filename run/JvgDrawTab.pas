{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDrawTab.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgDrawTab;

interface

uses
  Windows, CommCtrl, Graphics, Classes, ExtCtrls,
  JvgTypes, JvgUtils, JvgTabComm;

procedure DrawOwnTab(DrawTabStr: TDRAWTABSTRUCT);

implementation

procedure DrawOwnTab(DrawTabStr: TDRAWTABSTRUCT);
const
  cWordWrap: array [Boolean] of UINT = (DT_SINGLELINE, DT_WORDBREAK);
var
  HndPen: HPEN;
  hOldPen: HPEN;
  R, R1, AddR, ItemR: TRect;
  TransparentColor: TCOLORREF;
  X, Y: Integer;
  Selected, Vert, Hor: Boolean;
  BWallpaper: Boolean;
  WallpaperOption: TglWallpaperOption;
  Size: TSIZE;
  OldFont: Windows.HFont;
  GrFromColor, GrToColor: TColor;
begin
  if not Assigned(DrawTabStr.lpDrawItemStr) then
    Exit;
  with DrawTabStr, DrawTabStr.lpDrawItemStr^, DrawTabStr.BoxStyle do
  begin
    Selected := (itemState and ODS_SELECTED) <> 0;
    // WALLPAPER WORKS
    WallpaperOption := fwoNone;
    BWallpaper := IsItAFilledBitmap(Wallpaper.Bmp);
    if BWallpaper then
    begin
      if Wallpaper.Tile then
        WallpaperOption := fwoTile;
      //...calc and fill tabs background
      if (ItemID = UINT(TabsCount - 1)) and Wallpaper.FillCaptionBakgr then
      begin
        AddR := ClientR;
        ItemR := rcItem;
        AddR.Top := 0;
        AddR.Left := ItemR.Right;
        AddR.Bottom := ItemR.Bottom;
        if fButton then
          Inc(AddR.Bottom, 3)
        else
          Inc(AddR.Bottom, 2); //if Selected then AddR.Bottom:=AddR.Bottom-2;
        CreateBitmapExt(HDC, Wallpaper.Bmp, AddR, 0, 0,
          WallpaperOption, fdsDefault, False, 0, clBlack);
      end;
      //...calc client area exclude tabs and user-client area
      R := ClientR;
      case Position of
        fsdTop:
          begin
            R.Top := R.Top + rcItem.Bottom - rcItem.Top;
            if fButton then
              Inc(R.Top, 4)
            else
            if Selected then
              Dec(R.Top)
            else
              Inc(R.Top, 5);
            Dec(R.Right);
            Dec(R.Bottom);
            Inc(R.Left);
          end;
        fsdBottom:
          begin
            R.Bottom := R.Bottom - (rcItem.Bottom - rcItem.Top);
            if fButton then
              Dec(R.Bottom, 4)
            else
            if Selected then
              Dec(R.Bottom)
            else
              Dec(R.Bottom, 5);
            Dec(R.Right);
            Inc(R.Top);
            Inc(R.Left);
          end;
        fsdLeft:
          begin
            R.Left := R.Left + (rcItem.Right - rcItem.Left);
            if fButton then
              Inc(R.Left, 3)
            else
            if Selected then
              Dec(R.Left)
            else
              Inc(R.Left, 5);
            Dec(R.Right);
            Dec(R.Bottom);
            Inc(R.Top);
          end;
        fsdRight:
          begin
            R.Right := R.Right - (rcItem.Right - rcItem.Left);
            if fButton then
              Dec(R.Right, 4)
            else
            if Selected then
              Dec(R.Right)
            else
              Dec(R.Right, 5);
            Inc(R.Left);
            Dec(R.Bottom);
            Inc(R.Top);
          end;
      end;
      //...fill client exclude tabs
      if Wallpaper.FillClient then
        CreateBitmapExt(HDC, Wallpaper.Bmp, R, 0, 0,
          WallpaperOption, fdsDefault, False, 0, clBlack);
      //...calc tab to fill
      if Wallpaper.FillCaptions and Wallpaper.IncludeBevels then
      begin
        R := rcItem;
        if not fButton then
          case Position of
            fsdTop:
              Dec(R.Bottom);
            fsdLeft:
              Dec(R.Right);
            fsdRight:
              if not Selected then
                Dec(R.Left);
          end;
        CreateBitmapExt(HDC, Wallpaper.Bmp, R, 0, 0,
          WallpaperOption, fdsDefault, False, 0, clBlack);
      end;
    end;
    // CORRECT TAB RECT to draw bevels
    R := rcItem;
    if fButton then
    begin
      if not Selected then
        OffsetRect(R, -1, -1);
    end
    else
    begin
      if Selected then
      begin
        //      InflateRect(R, 4, 4);
        HndPen := CreatePen(PS_SOLID, 1, ColorToRGB(clBtnFace));
        hOldPen := SelectObject(HDC, HndPen);
        case Position of
          fsdTop:
            begin {
              MoveToEx( HDC, rcItem.Left, rcItem.Bottom-2, nil );
              LineTo( HDC, rcItem.Right, rcItem.Bottom-2 );}
              R.Left := R.Left + 3;
              R.Right := R.Right - 4;
              R.Top := R.Top + 2;
              //if FInteriorOffset then InflateRect(R,1,0);
            end;
          fsdBottom:
            begin
              MoveToEx(HDC, rcItem.Left, rcItem.Top + 1, nil);
              LineTo(HDC, rcItem.Right - 2, rcItem.Top + 1);
              MoveToEx(HDC, rcItem.Left, rcItem.Top, nil);
              LineTo(HDC, rcItem.Right - 2, rcItem.Top);
              R.Left := R.Left + 3;
              R.Right := R.Right - 4;
              Dec(R.Bottom);
              //if FInteriorOffset then InflateRect(R,1,0);
            end;
          fsdLeft:
            begin
              Inc(R.Left, 2);
              Dec(R.Right, 2);
              Inc(R.Top, 2);
              Dec(R.Bottom, 2);
              //if FInteriorOffset then Inc(R.Bottom);
            end;
          fsdRight:
            begin
              MoveToEx(HDC, rcItem.Left, rcItem.Top, nil);
              LineTo(HDC, rcItem.Left, rcItem.Bottom - 2);
              MoveToEx(HDC, rcItem.Left + 1, rcItem.Top, nil);
              LineTo(HDC, rcItem.Left + 1, rcItem.Bottom - 2);
              Inc(R.Left, 0);
              Dec(R.Right, 4);
              Inc(R.Top, 2);
              Dec(R.Bottom, 2);
              //if FInteriorOffset then Inc(R.Bottom);
            end;
        end;
        DeleteObject(SelectObject(HDC, hOldPen));
      end
      else
      begin
        case Position of
          fsdTop:
            begin
              Inc(R.Left);
              Dec(R.Right, 2);
              OffsetRect(R, 0, 2);
              //if FInteriorOffset then InflateRect(R,1,0);
            end;
          fsdBottom:
            begin
              Inc(R.Left);
              Dec(R.Right, 2);
              if not Selected then
                OffsetRect(R, 0, -1);
              if not (fsdTop in Borders) then
                Dec(R.Top);
              //if FInteriorOffset then InflateRect(R,1,0);
            end;
          fsdLeft:
            begin
              Inc(R.Left, 2); {if FInteriorOffset then Inc(R.Bottom);}
            end;
          fsdRight:
            begin
              Dec(R.Right, 4); {if FInteriorOffset then Inc(R.Bottom);}
            end;
        end;
      end;
      Dec(R.Bottom, 2);
    end;
    // DRAW BEVELS
    if Assigned(Gradient) and Gradient.Active then
    begin
      //      GrFromColor := Gradient.FRGBFromColor;
      //      GrToColor := Gradient.FRGBToColor;
      //      if BackgrColor_ > 0 then
      //      begin
      //        if ftoTabColorAsGradientFrom in Options then Gradient.FRGBFromColor := BackgrColor_;
      //        if ftoTabColorAsGradientTo in Options then Gradient.FRGBToColor := BackgrColor_;
      //      end;
      GradientBox(HDC, R, Gradient, Integer(psSolid), 1);
      //      Gradient.FRGBFromColor := GrFromColor;
      //      Gradient.FRGBToColor := GrToColor;
    end;

    if DrawTabStr.FlatButtons then
      InflateRect(R, 3, 3);
    R := DrawBoxEx(HDC, R, Borders, BevelInner, BevelOuter, Bold, BackgrColor_,
      (Wallpaper.FillCaptions and BWallpaper) or Gradient.Active);
    if DrawTabStr.FlatButtons then
      InflateRect(R, -3, -3);
    // DRAW caption BACKGROUND
    if Wallpaper.FillCaptions and not Wallpaper.IncludeBevels then
      CreateBitmapExt(HDC, Wallpaper.Bmp, R, 0, 0,
        WallpaperOption, fdsDefault, False, 0, clBlack);
    Inc(R.Bottom); //Inc(R.Right);
    // DRAW GLYPH
    R1 := R;
    Hor := (Position = fsdTop) or (Position = fsdBottom);
    Vert := not Hor;
    if IsItAFilledBitmap(Glyph) and not (ftoHideGlyphs in Options) then
    begin
      //if GlyphOption = fwoNone then ?
      case GlyphHAlign of
        fhaLeft:
          begin
            X := 1;
            if Hor then
              Inc(R1.Left, Glyph.Width + 1)
          end;
        fhaCenter:
          X := (R.Right - R.Left - Glyph.Width) div 2;
      else {fhaRight}
        X := R.Right - R.Left - Glyph.Width - 1;
        if Hor then
          Dec(R1.Right, Glyph.Width + 1);
      end;
      case GlyphVAlign of
        fvaTop:
          begin
            Y := 0;
            if Vert then
              Inc(R1.Top, Glyph.Height + 1);
          end;
        fvaCenter:
          Y := (R.Bottom - R.Top - Glyph.Height) div 2;
      else {fvaBottom}
        Y := R.Bottom - R.Top - Glyph.Height;
        if Vert then
          Dec(R1.Bottom, Glyph.Height + 1);
      end;
      TransparentColor := GetPixel(Glyph.Canvas.Handle, 0, Glyph.Height - 1);
      //    MoveToEx( hDC, R1.Left+X, R1.Top+Y, nil );
      //    LineTo( hDC, R1.Right, R1.Bottom );
      CreateBitmapExt(HDC, Glyph,
        R, X, Y, GlyphOption, fdsDefault,
        True, TransparentColor, clBlack);
      if GlyphOption <> fwoNone then
        R1 := R
      else
        R1.Left := R1.Left + 1;
    end; //...end draw glyph_

    //   case Position of
    //     fsdTop, fsdBottom: begin Inc(R1.Left,2); Dec(R1.Right,2); end;
    //   end;

    if not (ftoExcludeGlyphs in Options) then
      R1 := R;
    SetBkMode(HDC, TRANSPARENT);
    OldFont := SelectObject(HDC, Font_.Handle);
    SetTextColor(HDC, ColorToRGB(Font_.Color));
    GetTextExtentPoint32(HDC, PChar(Caption), Length(Caption), Size);
    SelectObject(HDC, OldFont);
    X := 0;
    Y := 0;
    case FontDirection of
      fldLeftRight:
        begin
          DrawTextInRectWithAlign(HDC, R1, Caption,
            CaptionHAlign, CaptionVAlign,
            TextStyle, Font_, cWordWrap[ftoWordWrap in Options]);
          Exit;
        end;
      fldRightLeft:
        begin
          case DrawTabStr.BoxStyle.CaptionHAlign of
            fhaLeft:
              X := R1.Right;
            fhaCenter:
              X := R1.Left + (R1.Right - R1.Left + Size.cx) div 2;
            fhaRight:
              Y := R1.Left + Size.cx;
          end;
          case CaptionVAlign of
            fvaTop:
              Y := R1.Bottom;
            fvaCenter:
              Y := R1.Top + (R1.Bottom - R1.Top + Size.cy) div 2;
            fvaBottom:
              Y := R1.Top + Size.cy;
          end;
        end;
      fldDownUp:
        begin
          X := R1.Left;
          Y := R1.Bottom - 4;
          case CaptionHAlign of
            fhaCenter:
              Dec(Y, (R1.Bottom - R1.Top - Size.cx) div 2);
            fhaRight:
              Y := R1.Top + Size.cx + 3;
          end;
          case CaptionVAlign of
            fvaCenter:
              Inc(X, (R1.Right - R1.Left - Size.cy) div 2);
            fvaBottom:
              X := R1.Right - Size.cy - 1;
          end;
        end;
    else {fldUpDown}
      X := R1.Right;
      Y := R1.Top + 4;
      case CaptionHAlign of
        fhaCenter:
          Inc(Y, (R1.Bottom - R1.Top - Size.cx) div 2);
        fhaRight:
          Y := R1.Bottom - Size.cx - 3;
      end;
      case CaptionVAlign of
        fvaCenter:
          Dec(X, (R1.Right - R1.Left - Size.cy) div 2);
        fvaBottom:
          X := R1.Left + Size.cy;
      end;
    end;
    ExtTextOutExt(HDC, X, Y, R1, PChar(Caption), TextStyle, False, False, Font.Color, 0 {DelinColor},
      clBtnHighlight, clBtnShadow, nil, nil, Font_);
  end;
end;

end.

