{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgAskListBox.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Rob den Braasem [rbraasem att xs4all dott nl]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgAskListBox;

interface

{ ListBox-based component  that provides  convenient  interface  for
 realization of the different  tests for users.  Component  is  very
 useful during setup and install processes.
 Items  captions align in one of 9 positions.  Component can display
 glyphs on own items and fill background  with  bitmap.  You can set
 different fonts  for  selected  item  and  for  other  list  items.
}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommCtrl, ExtCtrls,
  JVCLVer,
  JvgTypes, JvgCommClasses;

type
  TglAskLBOption = (aloAutoScroll, aloIgnoreMouse, aloShowFocus,
    aloTransparentButtons, aloWordWrap);
  TglAskLBOptions = set of TglAskLBOption;

  TJvgAskListBox = class(TCustomListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FAutoTransparentColor: TglAutoTransparentColor;
    FWallpaper: TBitmap;
    FWallpaperImage: TImage;
    FWallpaperOption: TglWallpaperOption;
    FNumGlyphs: Word;
    FGlyphsAlign: TJvg2DAlign;
    FTextAlign: TJvg2DAlign;
    FCaptionsAlign: TJvg2DAlign;
    FTransparentColor: TColor;
    FItemStyle: TJvgAskListBoxItemStyle;
    FItemSelStyle: TJvgAskListBoxItemStyle;
    FGlyphs: TImageList;
    FShowWallpaper: Boolean;
    FShowGlyphs: Boolean;
    FItemHeight: Word;
    FTextAlign_: UINT;
    FCaptionsAlign_: UINT;
    FShowText: Boolean;
    FSegment1Width: Word;
    FPushedButton: array[0..1023] of Byte;
    FOnButtonClicked: TNotifyEvent;
    FSelectedItem: Word;
    FButtons: TStringList;
    FButtonWidth: Word;
    FOptions: TglAskLBOptions;
    //'''''''''''''''''''''''''''''''''''''''''
    WallpaperBmp: TBitmap;
    TmpBitmap: TBitmap;
    BtnRect: TRect;
    BtnTxtRect: TRect;
    MouseClickPoint: TPoint;
    procedure SetAutoTransparentColor(Value: TglAutoTransparentColor);
    procedure SetWallpaper(Value: TBitmap);
    function GetWallpaper: TBitmap;
    procedure SetWallpaperImage(Value: Timage);
    procedure SetWallpaperOption(Value: TglWallpaperOption);
    procedure SetNumGlyphs(Value: Word);
    procedure SetGlyphs(Value: TImageList);
    procedure SetItemHeight(Value: Word);
    procedure SetShowText(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetSelectedItem(Value: Word);
    procedure SetButtons(Value: TStringList);
    procedure SetButtonWidth(Value: Word);
    procedure SetOptions(Value: TglAskLBOptions);
    procedure DrawWallpaper(R: TRect);
    procedure DrawGlyph(R: TRect; Index: Word; Shift: Word);
    procedure SetAlign(Align: TJvg2DAlign; var Align_: UINT);
    procedure ButtonClicked;
    procedure RecalcHeights;
    procedure SmthChanged(Sender: TObject);
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InitState(var State: TOwnerDrawState; ByteState: Byte);
  public
    function IsFilled: Boolean;
    function CountPushedButtonsInColon(Colon: Integer): Integer;
    function GetPushedButtonInLine(Index: Word): Integer;
    function SetPushedButtonInLine(Index: Word; Value: Word): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property IntegralHeight;
    property Items;
    property ParentColor;
    //    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    //    property AfterItemWasDrown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property AutoTransparentColor: TglAutoTransparentColor
      read FAutoTransparentColor write SetAutoTransparentColor default ftcLeftBottomPixel;
    property Wallpaper: TBitmap read GetWallpaper write SetWallpaper;
    property WallpaperImage: TImage read FWallpaperImage write SetWallpaperImage;
    property WallpaperOption: TglWallpaperOption read FWallpaperOption write
      SetWallpaperOption default fwoNone;
    property NumGlyphs: Word read FNumGlyphs write SetNumGlyphs default 1;
    property GlyphsAlign: TJvg2DAlign read FGlyphsAlign write FGlyphsAlign;
    property ItemStyle: TJvgAskListBoxItemStyle read FItemStyle write FItemStyle;
    property ItemSelStyle: TJvgAskListBoxItemStyle read FItemSelStyle write FItemSelStyle;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property TextAlign: TJvg2DAlign read FTextAlign write FTextAlign;
    property ItemHeight: Word read FItemHeight write SetItemHeight default 12;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property OnButtonClicked: TNotifyEvent read FOnButtonClicked write FOnButtonClicked;
    property SelectedItem: Word read FSelectedItem write SetSelectedItem default 0;
    property Buttons: TStringList read FButtons write SetButtons;
    property ButtonWidth: Word read FButtonWidth write SetButtonWidth default 30;
    property Options: TglAskLBOptions read FOptions write SetOptions;
  end;

implementation

uses
  JvConsts, JvJCLUtils,
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvgUtils;

{$IFNDEF USEJVCL}
resourcestring
  RsYes = 'yes';
  RsNo = 'no';
{$ENDIF USEJVCL}

constructor TJvgAskListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawVariable;
  FGlyphsAlign := TJvg2DAlign.Create;
  FTextAlign := TJvg2DAlign.Create;
  FCaptionsAlign := TJvg2DAlign.Create;
  FButtons := TStringList.Create;
  FItemStyle := TJvgAskListBoxItemStyle.Create;
  FItemSelStyle := TJvgAskListBoxItemStyle.Create;

  TmpBitmap := TBitmap.Create;
  FButtons.Add(RsYes);
  FButtons.Add(RsNo);
  FAutoTransparentColor := ftcLeftBottomPixel;
  FWallpaperOption := fwoNone;
  FShowWallpaper := True;
  FShowGlyphs := True;
  if csDesigning in ComponentState then
  begin
    with FItemStyle do
    begin
      //      Style := idsRaised;
      Color := clBtnFace;
      BtnColor := clBtnFace;
      TextStyle := fstRaised;
      BtnTextStyle := fstPushed;
    end;
    with FItemSelStyle do
    begin
      //      Style := idsRaised;
      Color := clBtnShadow;
      BtnColor := clBtnFace;
      TextStyle := fstRaised;
      BtnTextStyle := fstPushed;
    end;
  end;
  NumGlyphs := 1;
  FTransparentColor := clOlive;
  FItemHeight := 12;
  FTextAlign_ := DT_LEFT or DT_WORDBREAK or DT_VCENTER; // or DT_SINGLELINE;
  FCaptionsAlign_ := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  FShowText := True;
  FButtonWidth := 30;
  FSelectedItem := 0;
  FOptions := [aloWordWrap];
  FItemStyle.OnChanged := SmthChanged;
  FItemSelStyle.OnChanged := SmthChanged;
  FGlyphsAlign.OnChanged := SmthChanged;
  FTextAlign.OnChanged := SmthChanged;
  FCaptionsAlign.OnChanged := SmthChanged;

  FillChar(FPushedButton, SizeOf(FPushedButton), #0);
  FWallpaper := nil;
end;

destructor TJvgAskListBox.Destroy;
begin
  FWallpaper.Free;
  TmpBitmap.Free;
  FGlyphsAlign.Free;
  FTextAlign.Free;
  FCaptionsAlign.Free;
  FButtons.Free;
  FItemStyle.Free;
  FItemSelStyle.Free;
  inherited Destroy;
end;

procedure TJvgAskListBox.Loaded;
begin
  inherited Loaded;
  RecalcHeights;
end;

procedure TJvgAskListBox.CNMeasureItem(var Msg: TWMMeasureItem);
var
  R: TRect;
begin
  R.Left := 3;
  R.Top := 0;
  R.Bottom := 0;
  R.Right := FSegment1Width;
  if FShowGlyphs and (FGlyphs <> nil) then
    Inc(R.Left, FGlyphs.Width);
  Dec(R.Right, 5);
  with Msg.MeasureItemStruct^ do
  begin
    DrawText(Canvas, Items[itemID],
      Length(Items[itemID]), R, DT_CALCRECT or DT_WORDBREAK);
    itemHeight := R.Bottom - R.Top + 6;
    if itemHeight < FItemHeight then
      itemHeight := FItemHeight;
  end;
end;

procedure TJvgAskListBox.CNDrawItem(var Msg: TWMDrawItem);
const
  w = 1;
var
  Index: Integer;
  Rect: TRect;
  State: TOwnerDrawState;
  fSelected: Boolean;
  Shift, OldPushedBtn, I: Integer;
  Rect1: TRect;
  ItemStyle: TJvgAskListBoxItemStyle;
  //  TS:TglTextStyle;
  //  TA:UINT;

  procedure DrawLBItem(ItemSt: TglItemsDrawStyle; R: TRect);
  begin
    case ItemSt of
      idsRecessed:
        begin
          Shift := 0;
          Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1);
        end;
      idsRaised:
        begin
          Shift := 2;
          Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
        end;
    end;
  end;

  procedure DrawTextInRect(Rect: TRect; Align: Word; StrListNum: Integer);
  var
    FontColor: TColor;
    szStr: array[0..255] of Char;
    Len: Word;
    TextStyle: TglTextStyle;
  begin
    Dec(Rect.Right, 2);
    //    FillMemory(@szStr,255,0);
    if StrListNum = -1 then
      StrPCopy(szStr, Items[Index])
    else
      StrPCopy(szStr, FButtons[StrListNum - 1]);

    Len := StrLen(szStr);

    if StrListNum = -1 then
    begin
      Canvas.Font := ItemStyle.Font;
      TextStyle := ItemStyle.TextStyle;
    end
    else
    begin
      Canvas.Font := ItemStyle.BtnFont;
      if FPushedButton[Index] = StrListNum then
        TextStyle := fstNone
      else
        TextStyle := ItemStyle.BtnTextStyle;
    end;

    FontColor := Canvas.Font.Color;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    InflateRect(Rect, -1, -1);
    if StrListNum <> -1 then
      Inc(Rect.Top, 2);

    case TextStyle of
      fstRaised:
        begin
          Canvas.Font.Color := clBtnHighlight;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas, szStr, Len, Rect, Align);
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(Rect, 2, 2);
          DrawText(Canvas, szStr, Len, Rect, Align);
          Canvas.Font.Color := FontColor;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas, szStr, Len, Rect, Align);
        end;
      fstRecessed:
        begin
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas, szStr, Len, Rect, Align);
          Canvas.Font.Color := clBtnHighlight;
          OffsetRect(Rect, 2, 2);
          DrawText(Canvas, szStr, Len, Rect, Align);
          Canvas.Font.Color := FontColor;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas, szStr, Len, Rect, Align);
        end;
      fstPushed:
        begin
          Canvas.Font.Color := clBtnHighlight;
          DrawText(Canvas, szStr, Len, Rect, Align);
          OffsetRect(Rect, -1, -1);
          Canvas.Font.Color := clBtnShadow;
          DrawText(Canvas, szStr, Len, Rect, Align);
        end;
      fstShadow:
        begin
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(Rect, 2, 2);
          DrawText(Canvas, szStr, Len, Rect, Align);
          Canvas.Font.Color := FontColor;
          OffsetRect(Rect, -2, -2);
          DrawText(Canvas, szStr, Len, Rect, Align);
        end;
    else
      begin
        Canvas.Font.Color := FontColor;
        DrawText(Canvas, szStr, Len, Rect, Align);
      end;
    end;
  end;

begin
  with Msg.DrawItemStruct^ do
  begin
    Index := itemID;
    if Index = -1 then
      Exit;
    InitState(State, WordRec(LongRec(ItemState).Lo).Lo);
    //   State := TOwnerDrawState(WordRec(LongRec(ItemState).Lo).Lo);
    Canvas.Handle := hDC;
    Rect := rcItem;
  end;

  Canvas.Brush := Brush;
  //  if State = [odSelected,odFocused] then Exit;
  Canvas.FrameRect(Rect);
  Inc(Rect.Top);
  Inc(Rect.Left);
  fSelected := (State = [odSelected, odFocused]) or (State = [odSelected]);

  if fSelected then
  begin
    ItemStyle := FItemSelStyle;
    Shift := 2;
  end
  else
  begin
    ItemStyle := FItemStyle;
    Shift := 0;
  end;
  Canvas.Brush.Color := ItemStyle.Color;

  Rect1 := Rect;
  Rect1.Right := FSegment1Width;

  if IsItAFilledBitmap(WallpaperBmp) then
  begin
    if aloTransparentButtons in Options then
      DrawWallpaper(Rect)
    else
      DrawWallpaper(Rect1);
  end
  else
    Canvas.FillRect(Rect1);

  //DrawLBItem( idsRecessed, Rect1 );
  DrawBoxEx(Canvas.Handle, Rect1, ItemStyle.Bevel.Sides, ItemStyle.Bevel.Inner,
    ItemStyle.Bevel.Outer, ItemStyle.Bevel.Bold, 0, True);
  if fSelected then
  begin
    InflateRect(Rect1, -1, -1);
    Canvas.FillRect(Rect1);
    InflateRect(Rect1, 1, 1);
  end;

  OldPushedBtn := FPushedButton[Index];

  BtnRect := Rect;
  BtnRect.Left := Rect1.Right + 2;
  BtnRect.Right := BtnRect.Left + FButtonWidth;

  Canvas.Brush.Color := ItemStyle.BtnColor;
  for I := 1 to FButtons.Count do // draw buttons
  begin
    if PtInRectExclusive(BtnRect,MouseClickPoint) then
    begin
      if I = FPushedButton[Index] then
        FPushedButton[Index] := 0 //...none pushed
      else
        FPushedButton[Index] := I;
    end;
    if not (aloTransparentButtons in Options) or
      (not IsItAFilledBitmap(WallpaperBmp)) then
      Canvas.FillRect(BtnRect);
    BtnTxtRect := BtnRect;
    if FPushedButton[Index] = I then
    begin
      DrawLBItem(idsRecessed, BtnRect);
      OffsetRect(BtnTxtRect, 1, 1);
    end
    else
      DrawLBItem(idsRaised, BtnRect);
    //...button text
    if FPushedButton[Index] = I then
      ItemStyle.BtnFont.Style := [fsBold]
    else
      ItemStyle.BtnFont.Style := [];
    DrawTextInRect(BtnTxtRect, FCaptionsAlign_ or DT_SINGLELINE, I);
    Inc(BtnRect.Left, FButtonWidth + 1);
    Inc(BtnRect.Right, FButtonWidth + 1);
  end;

  MouseClickPoint.X := -1;
  MouseClickPoint.Y := 0;

  Rect1.Left := 3;
  Rect1.Right := FSegment1Width;

  Inc(Rect1.Top);
  Inc(Rect.Left);
  Dec(Rect1.Bottom);
  Dec(Rect.Right);

  if (FShowGlyphs) and (FGlyphs <> nil) and
    (FGlyphs.Width > 0) and (FGlyphs.Height > 0) then
  begin
    DrawGlyph(Rect, Index, Shift);
    Rect1.Left := Rect1.Left + FGlyphs.Width;
  end;
  //...text
  DrawTextInRect(Rect1, FTextAlign_, -1);

  with Msg.DrawItemStruct^ do
    if (odFocused in State) and (aloShowFocus in Options) then
      DrawFocusRect(hDC, rcItem);

  FSelectedItem := Index;
  if OldPushedBtn <> FPushedButton[Index] then
    ButtonClicked;
  Canvas.Handle := 0;
end;

procedure TJvgAskListBox.DrawWallpaper(R: TRect);
var
  X, Y, SaveIndex: Integer;
  UpdateRgn: HRGN;
begin
  X := 0;
  Y := 0;
  SaveIndex := SaveDC(Canvas.Handle);
  UpdateRgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);

  SelectClipRgn(Canvas.Handle, UpdateRgn);
  case WallpaperOption of
    fwoStretch:
      Canvas.StretchDraw(R, WallpaperBmp);
    fwoTile:
      while X < R.Right - R.Left do
      begin
        while Y < R.Bottom - R.Top do
        begin
          Canvas.Draw(R.Left + X, R.Top + Y, WallpaperBmp);
          Inc(Y, WallpaperBmp.Height);
        end;
        Inc(X, WallpaperBmp.Width);
        Y := 0;
      end;
  else
    Canvas.Draw(R.Left, R.Top, WallpaperBmp);
  end;
  DeleteObject(UpdateRgn);
  RestoreDC(Canvas.Handle, SaveIndex);
end;

procedure TJvgAskListBox.DrawGlyph(R: TRect; Index: Word; Shift: Word);
var
  I: Integer;
  OldRect: TRect;
begin
  if (FGlyphs = nil) or (FGlyphs.Count = 0) then
    Exit;
  R.Right := R.Left + FSegment1Width - 4;
  OldRect := R;
  Inc(R.Top);
  Inc(R.Left);
  case FGlyphsAlign.Horizontal of
    fhaCenter:
      OffsetRect(R, (R.Right - R.Left - Glyphs.Width) div 2, 0);
    fhaRight:
      OffsetRect(R, R.Right - R.Left - Glyphs.Width - Shift, 0);
  end;
  case FGlyphsAlign.Vertical of
    fvaCenter:
      OffsetRect(R, 0, (R.Bottom - R.Top - Glyphs.Height) div 2);
    fvaBottom:
      OffsetRect(R, 0, R.Bottom - R.Top - Glyphs.Height - Shift);
  end;

  I := -1;
  if NumGlyphs = 1 then
    I := 0
  else
  if Index < NumGlyphs then
    I := Index;
  if I >= 0 then
  begin
    FGlyphs.GetBitmap(I, TmpBitmap);
    if FAutoTransparentColor = ftcUser then
      CreateBitmapExt(Canvas.Handle, TmpBitmap, Rect(0, 0, 100, 100), R.Left,
        R.Top, fwoNone, fdsDefault, True, FTransparentColor, clBlack)
    else
      CreateBitmapExt(Canvas.Handle, TmpBitmap, Rect(0, 0, 100, 100), R.Left,
        R.Top, fwoNone, fdsDefault, True,
        GetTransparentColor(TmpBitmap, FAutoTransparentColor), clBlack);
  end;
end;

procedure TJvgAskListBox.SetAutoTransparentColor(Value: TglAutoTransparentColor);
begin
  if FAutoTransparentColor = Value then
    Exit;
  FAutoTransparentColor := Value;
  Invalidate;
end;

procedure TJvgAskListBox.SetWallpaper(Value: TBitmap);
begin
  if Assigned(FWallpaper) then
    FWallpaper.Free;
  FWallpaper := TBitmap.Create;
  FWallpaper.Assign(Value);
  if (not Assigned(Value)) and Assigned(WallpaperImage) then
    if Assigned(FWallpaper) then
      WallpaperBmp := FWallpaper
    else
    if Assigned(FWallpaperImage) then
      WallpaperBmp := FWallpaperImage.Picture.Bitmap
    else
      WallpaperBmp := nil;

  if FShowWallpaper then
    Invalidate;
end;

function TJvgAskListBox.GetWallpaper: TBitmap;
begin
  if not Assigned(FWallpaper) then
    FWallpaper := TBitmap.Create;
  WallpaperBmp := FWallpaper;
  Result := FWallpaper;
end;

procedure TJvgAskListBox.SetWallpaperImage(Value: Timage);
begin
  FWallpaperImage := Value;
  if (not IsItAFilledBitmap(FWallpaper)) and Assigned(Value) then
  begin
    WallpaperBmp := Value.Picture.Bitmap;
    if FShowWallpaper then
      Invalidate;
  end;
end;

procedure TJvgAskListBox.SetWallpaperOption(Value: TglWallpaperOption);
begin
  FWallpaperOption := Value;
  if FShowWallpaper then
    Invalidate;
end;

procedure TJvgAskListBox.SetNumGlyphs(Value: Word);
begin
  if Value < 1 then
    Exit;
  FNumGlyphs := Value;
  if FShowGlyphs then
    Invalidate;
end;

{procedure TJvgAskListBox.SetItemStyle( Value: TItemsDrawStyle );
begin
  if FItemStyle = Value then Exit;
  FItemStyle := Value;
  Invalidate;
end;

procedure TJvgAskListBox.SetSelItemStyle( Value: TItemsDrawStyle );
begin
  if FSelItemStyle = Value then Exit;
  FSelItemStyle := Value;
  Invalidate;
end;
}

procedure TJvgAskListBox.SetGlyphs(Value: TImageList);
begin
  //if (Value=nil)or(Value.Width<=0)or(Value.Height<=0) then Exit;
  FGlyphs := Value;
  if FShowGlyphs then
    Invalidate;
end;

{procedure TJvgAskListBox.SetSelFont( Value: TFont );
begin
  if Value=nil then Exit;
  FSelFont.Assign( Value );
  Invalidate;
end;
}

{
procedure TJvgAskListBox.SetColor(Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value; Canvas.Brush.Color:=Value; Invalidate;
end;

procedure TJvgAskListBox.SetSelColor(Value: TColor);
begin
  if FColor = Value then Exit;
  FSelColor := Value; Canvas.Brush.Color:=Value; Invalidate;
end;
}

procedure TJvgAskListBox.SetItemHeight(Value: Word);
begin
  if (Value > 6) and (FItemHeight <> Value) then
  begin
    FItemHeight := Value;
    //inherited ItemHeight:=FItemHeight;
    RecalcHeights;
  end;
end;

procedure TJvgAskListBox.SetAlign(Align: TJvg2DAlign; var Align_: UINT);
begin
  case Align.Horizontal of
    fhaLeft:
      Align_ := Align_ or DT_LEFT;
    fhaCenter:
      Align_ := Align_ or DT_CENTER;
  else
    Align_ := Align_ or DT_RIGHT;
  end;
  case Align.Vertical of
    fvaTop:
      Align_ := Align_ or DT_TOP;
    fvaCenter:
      Align_ := Align_ or DT_VCENTER;
  else
    Align_ := Align_ or DT_BOTTOM;
  end;
end;

{
procedure TJvgAskListBox.SetTextStyle(Value: TglTextStyle);
begin
  if FTextStyle = Value then Exit;
  FTextStyle := Value; if FShowText then Invalidate;
end;

procedure TJvgAskListBox.SetButtonsTextStyle(Value: TglTextStyle);
begin
  if FButtonsTextStyle = Value then Exit;
  FButtonsTextStyle := Value; if FShowText then Invalidate;
end;
}

procedure TJvgAskListBox.SetShowText(Value: Boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    Invalidate;
  end;
end;

procedure TJvgAskListBox.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    if FShowGlyphs then
      Invalidate;
  end;
end;

procedure TJvgAskListBox.SetButtonWidth(Value: Word);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    RecalcHeights;
  end;
end;

procedure TJvgAskListBox.SetOptions(Value: TglAskLBOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    RecalcHeights;
  end;
end;

function TJvgAskListBox.IsFilled: Boolean;
var
  I: Word;
begin
  Result := False;
  for I := 0 to Items.Count - 1 do
    if FPushedButton[I] = 0 then
      Exit;
  Result := True;
end;

function TJvgAskListBox.CountPushedButtonsInColon(Colon: Integer): Integer;
var
  I: Word;
begin
  Result := 0;
  if Colon = 0 then
  begin
    for I := 0 to Items.Count - 1 do
      if FPushedButton[I] <> 0 then
        Inc(Result);
  end
  else
    for I := 0 to Items.Count - 1 do
      if FPushedButton[I] = Colon then
        Inc(Result);
end;

procedure TJvgAskListBox.SetSelectedItem(Value: Word);
begin
  if Value >= Items.Count then
    Exit;
  SendMessage(Handle, LB_SETCURSEL, Value, Longint(0));
end;

procedure TJvgAskListBox.SetButtons(Value: TStringList);
begin
  if (Value <> nil) and (Value.Count <> 0) then
  begin
    FButtons.Assign(Value);
    RecalcHeights;
    Invalidate;
  end;
end;

function TJvgAskListBox.GetPushedButtonInLine(Index: Word): Integer;
begin
  if Index >= Items.Count then
    Result := -1
  else
    Result := FPushedButton[Index];
end;

function TJvgAskListBox.SetPushedButtonInLine(Index: Word; Value: Word): Boolean;
var
  R: TRect;
begin
  if (Index < Items.Count) and (Value in [0..2]) then
  begin
    Result := True;
    if FPushedButton[Index] = Value then
      Exit;
    FPushedButton[Index] := Value;
    SendMessage(Handle, LB_GETITEMRECT, Index, Longint(@R));
    R.Left := FSegment1Width;
    InvalidateRect(Handle, @R, True);
    //ButtonClicked;
    if (aloAutoScroll in Options) and (Value <> 0) then
      SendMessage(Handle, LB_SETCURSEL, FSelectedItem + 1, Longint(0));
  end
  else
    Result := False;
end;

procedure TJvgAskListBox.WMLButtonDown(var Msg: TWMLButtonDown);
var
  R: TRect;
  ItemN: Integer;
begin
  inherited;
  if aloIgnoreMouse in Options then
    Exit;
  MouseClickPoint.X := Msg.XPos;
  MouseClickPoint.Y := Msg.YPos;
  if Msg.XPos > integer(FSegment1Width) then
  begin
    ItemN := ItemAtPos(SmallPointToPoint(Msg.Pos), True);
    SendMessage(Handle, LB_GETITEMRECT, ItemN, LPARAM(@R));
    Inc(R.Left, FSegment1Width);
    InvalidateRect(Handle, @R, False);
    //if (aloAutoScroll in Options)then SendMessage( Handle, LB_SETCURSEL, FSelectedItem+1, Longint(0));
  end;
end;

procedure TJvgAskListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  RecalcHeights;
end;

procedure TJvgAskListBox.ButtonClicked;
begin
  if Assigned(FOnButtonClicked) then
    FOnButtonClicked(Self);
end;

procedure TJvgAskListBox.RecalcHeights;
var
  I: Integer;
  R: TRect;
begin
  if Items.Count = 0 then
    Exit;

  SendMessage(Handle, LB_GETITEMRECT, Items.Count - 1, LPARAM(@R));
  FSegment1Width := Word((R.Right - R.Left) - (FButtonWidth + 1) *
    (FButtons.Count) - 1);

  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
  begin
    Items.Insert(I, Items.Strings[I]);
    Items.Delete(I + 1);
  end;
  Items.EndUpdate;
end;

procedure TJvgAskListBox.SmthChanged(Sender: TObject);
begin
  FTextAlign_ := DT_WORDBREAK;
  FCaptionsAlign_ := DT_SINGLELINE;
  SetAlign(FTextAlign, FTextAlign_);
  SetAlign(FCaptionsAlign, FCaptionsAlign_);
  FCaptionsAlign_ := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  Invalidate;
end;

procedure TJvgAskListBox.InitState(var State: TOwnerDrawState; ByteState: Byte);
begin
  State := [];
  if ByteState and ODS_CHECKED <> 0 then
    Include(State, odChecked); //TOwnerDrawState
  if ByteState and ODS_COMBOBOXEDIT <> 0 then
    Include(State, odComboBoxEdit);
  if ByteState and ODS_DEFAULT <> 0 then
    Include(State, odDefault);
  if ByteState and ODS_DISABLED <> 0 then
    Include(State, odDisabled);
  if ByteState and ODS_FOCUS <> 0 then
    Include(State, odFocused);
  if ByteState and ODS_GRAYED <> 0 then
    Include(State, odGrayed);
  if ByteState and ODS_SELECTED <> 0 then
    Include(State, odSelected);
end;

procedure TJvgAskListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = WallpaperImage) and (Operation = opRemove) then
    WallpaperImage := nil;
  if (AComponent = FGlyphs) and (Operation = opRemove) then
    Glyphs := nil;
end;

end.

