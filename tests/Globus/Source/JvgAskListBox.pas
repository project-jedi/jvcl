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
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ ListBox-based component  that	provides  convenient  interface  for
 realization of the different  tests for users.  Component  is	very
 useful during setup and install processes.
 Items	captions align in one of 9 positions.  Component can display
 glyphs on own items and fill background  with	bitmap.  You can set
 different fonts  for  selected  item  and  for  other	list  items.
}
unit JvgAskListBox;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, commctrl, ExtCtrls, JvgTypes, JvgCommClasses;

const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

type
  TPushedButtons = record
    Total: word;
    FirstColumn: word;
  end;
  TglAskLBOptions_ = (aloAutoScroll, aloIgnoreMouse, aloShowFocus, aloTransparentButtons, aloWordWrap);
  TglAskLBOptions = set of TglAskLBOptions_;

  TJvgAskListBox = class(TCustomListBox)
  private
    FAutoTrColor: TglAutoTransparentColor;
    FWallpaper: TBitmap;
    FWallpaperImage: TImage;
    FWallpaperOption: TglWallpaperOption;
    FNumGlyphs: word;
    FGlyphsAlign: TJvg2DAlign;
    FTextAlign: TJvg2DAlign;
    FCaptionsAlign: TJvg2DAlign;

    FTransparentColor: TColor;
    FItemStyle: TJvgAskListBoxItemStyle;
    FItemSelStyle: TJvgAskListBoxItemStyle;
    FGlyphs: TImageList;
    FShowWallpaper: boolean;
    FShowGlyphs: boolean;
    FItemHeight: word;
    FTextAlign_: UINT;
    FCaptionsAlign_: UINT;
    FShowText: boolean;
    FSegment1Width: word;
    FPushedButton: array[0..1023] of byte;
    FOnButtonClicked: TNotifyEvent;
    FSelectedItem: word;
    FButtons: TStringList;
    FButtonWidth: word;
    FOptions: TglAskLBOptions;
    //'''''''''''''''''''''''''''''''''''''''''
    WallpaperBmp: TBitmap;
    TmpBitmap: TBitmap;
    BtnRect, BtnTxtRect: TRect;
    MouseClickPoint: TPoint;
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetWallpaper(Value: TBitmap);
    function GetWallpaper: TBitmap;
    procedure SetWallpaperImage(Value: Timage);
    procedure SetWOpt(Value: TglWallpaperOption);
    procedure SetNumGlyphs(Value: word);
    procedure SetGlyphs(Value: TImageList);
    procedure SetItemHeight(Value: word);
    procedure SetShowText(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetSelectedItem(Value: word);
    procedure SetButtons(Value: TStringList);
    procedure SetButtonWidth(Value: word);
    procedure SetOptions(Value: TglAskLBOptions);

    procedure DrawWallpaper(r: TRect);
    procedure DrawGlyph(r: TRect; Index: word; Shift: word);
    procedure SetAlign(Align: TJvg2DAlign; var Align_: UINT);
    procedure ButtonClicked;
    procedure RecalcHeights;
    procedure SmthChanged(Sender: TObject);

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
  protected
    procedure Loaded; override;
    procedure InitState(var State: TOwnerDrawState; ByteState: Byte);
  public //_____________________________for users
    function IsFilled: boolean;
    function CountPushedButtonsInColon(Colon: integer): integer;
    function GetPushedButtonInLine(Index: word): integer;
    function SetPushedButtonInLine(Index: word; Value: word): boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property IntegralHeight;
    property Items;
    property ParentColor;
    //    property ParentCtl3D;
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
      read FAutoTrColor write SetAutoTrColor default ftcLeftBottomPixel;
    property Wallpaper: TBitmap read GetWallpaper write SetWallpaper;
    property WallpaperImage: TImage read FWallpaperImage write SetWallpaperImage;
    property WallpaperOption: TglWallpaperOption read FWallpaperOption write SetWOpt default fwoNone;
    property NumGlyphs: word read FNumGlyphs write SetNumGlyphs default 1;
    property GlyphsAlign: TJvg2DAlign read FGlyphsAlign write FGlyphsAlign;
    property ItemStyle: TJvgAskListBoxItemStyle read FItemStyle write FItemStyle;
    property ItemSelStyle: TJvgAskListBoxItemStyle read FItemSelStyle write FItemSelStyle;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property TextAlign: TJvg2DAlign read FTextAlign write FTextAlign;
    property ItemHeight: word read FItemHeight write SetItemHeight default 12;
    property ShowText: boolean read FShowText write SetShowText default true;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property OnButtonClicked: TNotifyEvent read FOnButtonClicked write FOnButtonClicked;
    property SelectedItem: word read FSelectedItem write SetSelectedItem default 0;
    property Buttons: TStringList read FButtons write SetButtons;
    property ButtonWidth: word read FButtonWidth write SetButtonWidth default 30;
    property Options: TglAskLBOptions read FOptions write SetOptions;
  end;

implementation
uses JvgUtils;


//*****************************************_____________LowLevel METHODS

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
  FButtons.Add('yes');
  FButtons.Add('no');
  FAutoTrColor := ftcLeftBottomPixel;
  FWallpaperOption := fwoNone;
  FShowWallpaper := true;
  FShowGlyphs := true;
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
  FShowText := true;
  FButtonWidth := 30;
  FSelectedItem := 0;
  FOptions := [aloWordWrap];
  FItemStyle.OnChanged := SmthChanged;
  FItemSelStyle.OnChanged := SmthChanged;
  FGlyphsAlign.OnChanged := SmthChanged;
  FTextAlign.OnChanged := SmthChanged;
  FCaptionsAlign.OnChanged := SmthChanged;

  FillMemory(@FPushedButton, 1024, 0);
end;
//------

destructor TJvgAskListBox.Destroy;
begin
  if Assigned(FWallpaper) then FWallpaper.Free;
  TmpBitmap.Free;
  FGlyphsAlign.Free;
  FTextAlign.Free;
  FCaptionsAlign.Free;
  FButtons.Free;
  FItemStyle.Free;
  FItemSelStyle.Free;
  inherited Destroy;
end;
//______________________________________________________________

procedure TJvgAskListBox.Loaded;
begin
  inherited;
  RecalcHeights;
end;
//______________________________________________________________

procedure TJvgAskListBox.CNMeasureItem(var Message: TWMMeasureItem);
var
  r: TRect;
begin
  r.left := 3;
  r.top := 0;
  r.bottom := 0;
  r.right := FSegment1Width;
  if FShowGlyphs and (FGlyphs <> nil) then inc(r.left, FGlyphs.Width);
  dec(r.right, 5);
  with Message.MeasureItemStruct^ do
  begin
    DrawText(Canvas.Handle, PChar(Items[itemID]),
      Length(Items[itemID]), r, DT_CALCRECT or DT_WORDBREAK);
    itemHeight := r.bottom - r.top + 6;
    if itemHeight < FItemHeight then itemHeight := FItemHeight;
  end;
end;
//______________________________________________________________

procedure TJvgAskListBox.CNDrawItem(var Message: TWMDrawItem);
const
  w = 1;
var
  Index: Integer;
  Rect: TRect;
  State: TOwnerDrawState;
  fSelected: boolean;
  Shift, OldPushedBtn, i: integer;
  Rect1: TRect;
  ItemStyle: TJvgAskListBoxItemStyle;
  //  TS:TglTextStyle;
  //  TA:UINT;

  procedure DrawLBItem(ItemSt: TglItemsDrawStyle; r: TRect);
  begin
    case ItemSt of
      idsRecessed:
        begin
          Shift := 0;
          Frame3D(Canvas, r, clBtnShadow, clBtnHighlight, 1);
        end;
      idsRaised:
        begin
          Shift := 2;
          Frame3D(Canvas, r, clBtnHighlight, clBtnShadow, 1);
        end;
    end;
  end;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  procedure DrawTextInRect(rect: TRect; Align: Word; StrListNum: integer);
  var
    FontColor: TColor;
    szStr: array[0..255] of char;
    Len: word;
    TextStyle: TglTextStyle;
  begin
    dec(rect.right, 2);
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
    if StrListNum = -1 then
    else
      inc(Rect.top, 2);

    case TextStyle of
      fstRaised:
        begin
          Canvas.Font.Color := clBtnHighlight;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(Rect, 2, 2);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
          Canvas.Font.Color := FontColor;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
        end;
      fstRecessed:
        begin
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
          Canvas.Font.Color := clBtnHighlight;
          OffsetRect(Rect, 2, 2);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
          Canvas.Font.Color := FontColor;
          OffsetRect(Rect, -1, -1);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
        end;
      fstPushed:
        begin
          Canvas.Font.Color := clBtnHighlight;
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
          OffsetRect(Rect, -1, -1);
          Canvas.Font.Color := clBtnShadow;
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
        end;
      fstShadow:
        begin
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(Rect, 2, 2);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
          Canvas.Font.Color := FontColor;
          OffsetRect(Rect, -2, -2);
          DrawText(Canvas.Handle, szStr, Len, rect, Align);
        end;
    else
      begin
        Canvas.Font.Color := FontColor;
        DrawText(Canvas.Handle, szStr, Len, rect, Align);
      end;
    end;
  end;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~
begin
  with Message.DrawItemStruct^ do
  begin
    Index := itemID;
    if Index = -1 then exit;
    InitState(State, WordRec(LongRec(ItemState).Lo).Lo); //   State := TOwnerDrawState(WordRec(LongRec(ItemState).Lo).Lo);
    Canvas.Handle := hDC;
    Rect := rcItem;
  end;

  Canvas.Brush := Brush;
  //  if State = [odSelected,odFocused] then exit;
  Canvas.FrameRect(Rect);
  inc(rect.top);
  inc(rect.left);
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

  Rect1 := rect;
  rect1.right := FSegment1Width;

  if IsItAFilledBitmap(WallpaperBmp) then
  begin
    if aloTransparentButtons in Options then
      DrawWallpaper(rect)
    else
      DrawWallpaper(rect1);
  end
  else
    Canvas.FillRect(Rect1);

  //DrawLBItem( idsRecessed, rect1 );
  DrawBoxEx(Canvas.Handle, rect1, ItemStyle.Bevel.Sides, ItemStyle.Bevel.Inner,
    ItemStyle.Bevel.Outer, ItemStyle.Bevel.Bold, 0, true);
  if fSelected then
  begin
    InflateRect(Rect1, -1, -1);
    Canvas.FillRect(Rect1);
    InflateRect(Rect1, 1, 1);
  end;
  //~~~~~~~~~
  OldPushedBtn := FPushedButton[Index];

  BtnRect := Rect;
  BtnRect.Left := Rect1.Right + 2;
  BtnRect.Right := BtnRect.Left + FButtonWidth;

  Canvas.Brush.Color := ItemStyle.BtnColor;
  for i := 1 to FButtons.Count do //______________________DRAW BUTTONS
  begin
    if IsPointInRect(MouseClickPoint, BtnRect) then
    begin
      if i = FPushedButton[Index] then
        FPushedButton[Index] := 0 //...none pushed
      else
        FPushedButton[Index] := i;
    end;
    if not (aloTransparentButtons in Options) or (not IsItAFilledBitmap(WallpaperBmp)) then Canvas.FillRect(BtnRect);
    BtnTxtRect := BtnRect;
    if FPushedButton[Index] = i then
    begin
      DrawLBItem(idsRecessed, BtnRect);
      OffsetRect(BtnTxtRect, 1, 1);
    end
    else
      DrawLBItem(idsRaised, BtnRect);
    //...button text
    if FPushedButton[Index] = i then
      ItemStyle.BtnFont.Style := [fsBold]
    else
      ItemStyle.BtnFont.Style := [];
    DrawTextInRect(BtnTxtRect, FCaptionsAlign_ or DT_SINGLELINE, i);
    inc(BtnRect.Left, FButtonWidth + 1);
    inc(BtnRect.Right, FButtonWidth + 1);
  end; //_________________________________________________DRAW BUTTONS END
  MouseClickPoint.x := -1;
  MouseClickPoint.y := 0;
  //~~~~~~~~~
  rect1.left := 3;
  rect1.right := FSegment1Width;

  inc(rect1.top);
  inc(rect.left);
  dec(rect1.bottom);
  dec(rect.right);

  if (FShowGlyphs) and (FGlyphs <> nil)
    and (FGlyphs.Width > 0) and (FGlyphs.Height > 0) then
  begin
    DrawGlyph(rect, Index, Shift);
    Rect1.left := Rect1.left + FGlyphs.Width;
  end;
  //...text
  DrawTextInRect(rect1, FTextAlign_, -1);

  with Message.DrawItemStruct^ do
    if (odFocused in State) and (aloShowFocus in Options) then DrawFocusRect(hDC, rcItem);

  FSelectedItem := Index;
  if OldPushedBtn <> FPushedButton[Index] then ButtonClicked;
  Canvas.Handle := 0;
end;
//_____________________________________________________

procedure TJvgAskListBox.DrawWallpaper(r: TRect);
var
  X, Y, SaveIndex: integer;
  UpdateRgn: HRGN;
begin
  x := 0;
  y := 0;
  SaveIndex := SaveDC(Canvas.Handle);
  UpdateRgn := CreateRectRgn(r.left, r.top, r.right, r.bottom);

  SelectClipRgn(Canvas.Handle, UpdateRgn);
  case WallpaperOption of
    fwoStretch:
      Canvas.StretchDraw(r, WallpaperBmp);
    fwoTile:
      while x < r.Right - r.Left do
      begin
        while y < r.bottom - r.top do
        begin
          Canvas.Draw(r.Left + x, r.top + y, WallpaperBmp);
          Inc(y, WallpaperBmp.Height);
        end;
        Inc(x, WallpaperBmp.Width);
        y := 0;
      end;
  else
    Canvas.Draw(r.left, r.top, WallpaperBmp);
  end;
  DeleteObject(UpdateRgn);
  RestoreDC(Canvas.Handle, SaveIndex);
end;
//______________________________________________________________

procedure TJvgAskListBox.DrawGlyph(r: TRect; Index: word; Shift: word);
var
  i: integer;
  OldRect: TRect;
begin
  if (FGlyphs = nil) or (FGlyphs.Count = 0) then exit;
  r.right := r.left + FSegment1Width - 4;
  OldRect := r;
  inc(r.top);
  inc(r.left);
  case FGlyphsAlign.Horizontal of
    fhaCenter: OffsetRect(r, (r.right - r.left - Glyphs.Width) div 2, 0);
    fhaRight: OffsetRect(r, r.right - r.left - Glyphs.Width - Shift, 0);
  end;
  case FGlyphsAlign.Vertical of
    fvaCenter: OffsetRect(r, 0, (r.bottom - r.top - Glyphs.height) div 2);
    fvaBottom: OffsetRect(r, 0, r.bottom - r.top - Glyphs.height - Shift);
  end;

  i := -1;
  if NumGlyphs = 1 then
    i := 0
  else if Index < NumGlyphs then
    i := Index;
  if i >= 0 then
  begin
    FGlyphs.GetBitmap(i, TmpBitmap);
    if FAutoTrColor = ftcUser then
      CreateBitmapExt(Canvas.Handle, TmpBitmap, Rect(0, 0, 100, 100), r.left, r.top,
        fwoNone, fdsDefault, true, FTransparentColor, clBlack)
    else
      CreateBitmapExt(Canvas.Handle, TmpBitmap, Rect(0, 0, 100, 100), r.left, r.top,
        fwoNone, fdsDefault, true, GetTransparentColor(TmpBitmap, FAutoTrColor), clBlack);
  end;
end;

//*****************************************_____________PROPERTY METHODS

procedure TJvgAskListBox.SetAutoTrColor(Value: TglAutoTransparentColor);
begin
  if FAutoTrColor = Value then exit;
  FAutoTrColor := Value;
  Invalidate;
end;

procedure TJvgAskListBox.SetWallpaper(Value: TBitmap);
begin
  if Assigned(FWallpaper) then FWallpaper.Free;
  FWallpaper := TBitmap.Create;
  FWallpaper.Assign(Value);
  if (not Assigned(Value)) and Assigned(WallpaperImage) then

    if Assigned(FWallpaper) then
      WallpaperBmp := FWallpaper
    else if Assigned(FWallpaperImage) then
      WallpaperBmp := FWallpaperImage.Picture.Bitmap
    else
      WallpaperBmp := nil;

  if FShowWallpaper then Invalidate;
end;

function TJvgAskListBox.GetWallpaper: TBitmap;
begin
  if not Assigned(FWallpaper) then FWallpaper := TBitmap.Create;
  WallpaperBmp := FWallpaper;
  Result := FWallpaper;
end;

procedure TJvgAskListBox.SetWallpaperImage(Value: Timage);
begin
  FWallpaperImage := Value;
  if (not IsItAFilledBitmap(FWallpaper)) and Assigned(Value) then
  begin
    WallpaperBmp := Value.Picture.Bitmap;
    if FShowWallpaper then Invalidate;
  end;
end;

procedure TJvgAskListBox.SetWOpt(Value: TglWallpaperOption);
begin
  FWallpaperOption := Value;
  if FShowWallpaper then Invalidate;
end;

procedure TJvgAskListBox.SetNumGlyphs(Value: word);
begin
  if Value < 1 then exit;
  FNumGlyphs := Value;
  if FShowGlyphs then Invalidate;
end;

{procedure TJvgAskListBox.SetItemStyle( Value: TItemsDrawStyle );
begin
  if FItemStyle = Value then exit;
  FItemStyle := Value;
  Invalidate;
end;

procedure TJvgAskListBox.SetSelItemStyle( Value: TItemsDrawStyle );
begin
  if FSelItemStyle = Value then exit;
  FSelItemStyle := Value;
  Invalidate;
end;
}

procedure TJvgAskListBox.SetGlyphs(Value: TImageList);
begin
  //if (Value=nil)or(Value.Width<=0)or(Value.Height<=0) then exit;
  FGlyphs := Value;
  if FShowGlyphs then Invalidate;
end;

{procedure TJvgAskListBox.SetSelFont( Value: TFont );
begin
  if Value=nil then exit;
  FSelFont.Assign( Value );
  Invalidate;
end;
}

{
procedure TJvgAskListBox.SetColor(Value: TColor);
begin
  if FColor = Value then exit;
  FColor := Value; Canvas.Brush.Color:=Value; Invalidate;
end;

procedure TJvgAskListBox.SetSelColor(Value: TColor);
begin
  if FColor = Value then exit;
  FSelColor := Value; Canvas.Brush.Color:=Value; Invalidate;
end;
}

procedure TJvgAskListBox.SetItemHeight(Value: word);
begin
  if (Value <= 6) or (FItemHeight = Value) then exit;
  FItemHeight := Value;
  //inherited ItemHeight:=FItemHeight;
  RecalcHeights;
end;

procedure TJvgAskListBox.SetAlign(Align: TJvg2DAlign; var Align_: UINT);
begin
  case Align.Horizontal of
    fhaLeft: Align_ := Align_ or DT_LEFT;
    fhaCenter: Align_ := Align_ or DT_CENTER;
  else
    Align_ := Align_ or DT_RIGHT;
  end;
  case Align.Vertical of
    fvaTop: Align_ := Align_ or DT_TOP;
    fvaCenter: Align_ := Align_ or DT_VCENTER;
  else
    Align_ := Align_ or DT_BOTTOM;
  end;
end;
{
procedure TJvgAskListBox.SetTextStyle(Value: TglTextStyle);
begin
  if FTextStyle = Value then exit;
  FTextStyle := Value; if FShowText then Invalidate;
end;

procedure TJvgAskListBox.SetButtonsTextStyle(Value: TglTextStyle);
begin
  if FButtonsTextStyle = Value then exit;
  FButtonsTextStyle := Value; if FShowText then Invalidate;
end;
}

procedure TJvgAskListBox.SetShowText(Value: boolean);
begin
  if FShowText = Value then exit;
  FShowText := Value;
  Invalidate;
end;

procedure TJvgAskListBox.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor = Value then exit;
  FTransparentColor := Value;
  if FShowGlyphs then Invalidate;
end;

procedure TJvgAskListBox.SetButtonWidth(Value: word);
begin
  if FButtonWidth = Value then exit;
  FButtonWidth := Value;
  RecalcHeights;
end;

procedure TJvgAskListBox.SetOptions(Value: TglAskLBOptions);
begin
  if FOptions = Value then exit;
  FOptions := Value;
  RecalcHeights;
end;

//_______________________________________________SERVEICE PROCs

function TJvgAskListBox.IsFilled: boolean;
var
  i: word;
begin
  Result := false;
  for i := 0 to Items.Count - 1 do
    if FPushedButton[i] = 0 then exit;
  Result := true;
end;

function TJvgAskListBox.CountPushedButtonsInColon(Colon: integer): integer;
var
  i: word;
begin
  Result := 0;
  if Colon = 0 then
  begin
    for i := 0 to Items.Count - 1 do
      if FPushedButton[i] <> 0 then inc(Result);
  end
  else
    for i := 0 to Items.Count - 1 do
      if FPushedButton[i] = Colon then inc(Result);
end;

procedure TJvgAskListBox.SetSelectedItem(Value: word);
begin
  if Value >= Items.Count then exit;
  SendMessage(Handle, LB_SETCURSEL, Value, Longint(0));
end;

procedure TJvgAskListBox.SetButtons(Value: TStringList);
begin
  if (Value = nil) or (Value.Count = 0) then exit;
  FButtons.Assign(Value);
  RecalcHeights;
  Invalidate;
end;

function TJvgAskListBox.GetPushedButtonInLine(Index: word): integer;
begin
  if Index >= Items.Count then
    Result := -1
  else
    Result := FPushedButton[Index];
end;

function TJvgAskListBox.SetPushedButtonInLine(Index: word; Value: word): boolean;
var
  r: TRect;
begin
  if (Index < Items.Count) and (Value in [0..2]) then
  begin
    Result := true;
    if FPushedButton[Index] = Value then exit;
    FPushedButton[Index] := Value;
    SendMessage(Handle, LB_GETITEMRECT, Index, Longint(@r));
    r.left := FSegment1Width;
    InvalidateRect(Handle, @r, true);
    //ButtonClicked;
    if (aloAutoScroll in Options) and (Value <> 0) then
      SendMessage(Handle, LB_SETCURSEL, FSelectedItem + 1, Longint(0));
  end
  else
    Result := false;
end;

procedure TJvgAskListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  R: TRect;
  ItemN: integer;
begin
  inherited;
  if aloIgnoreMouse in Options then exit;
  MouseClickPoint.x := Message.xPos;
  MouseClickPoint.y := Message.yPos;
  if Message.xPos > FSegment1Width then
  begin
    ItemN := ItemAtPos(SmallPointToPoint(Message.Pos), True);
    SendMessage(handle, LB_GETITEMRECT, ItemN, LPARAM(@r));
    inc(R.Left, FSegment1Width);
    InvalidateRect(Handle, @R, false);
    //if (aloAutoScroll in Options)then SendMessage( Handle, LB_SETCURSEL, FSelectedItem+1, Longint(0));
  end;
end;

procedure TJvgAskListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  RecalcHeights;
end;

procedure TJvgAskListBox.ButtonClicked;
begin
  if Assigned(FOnButtonClicked) then FOnButtonClicked(self);
end;

procedure TJvgAskListBox.RecalcHeights;
var
  i: integer;
  r: TRect;
begin
  if Items.Count = 0 then exit;

  SendMessage(handle, LB_GETITEMRECT, Items.Count - 1, LPARAM(@r));
  FSegment1Width := word((r.right - r.left) - (FButtonWidth + 1) * (FButtons.Count) - 1);

  Items.BeginUpdate;
  for i := 0 to Items.Count - 1 do
  begin
    Items.Insert(i, Items.Strings[i]);
    Items.Delete(i + 1);
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
  if ByteState and ODS_CHECKED <> 0 then Include(State, odChecked); //TOwnerDrawState
  {$IFDEF COMPILER5_UP}
  if ByteState and ODS_COMBOBOXEDIT <> 0 then Include(State, odComboBoxEdit);
  if ByteState and ODS_DEFAULT <> 0 then Include(State, odDefault);
  {$ENDIF}
  if ByteState and ODS_DISABLED <> 0 then Include(State, odDisabled);
  if ByteState and ODS_FOCUS <> 0 then Include(State, odFocused);
  if ByteState and ODS_GRAYED <> 0 then Include(State, odGrayed);
  if ByteState and ODS_SELECTED <> 0 then Include(State, odSelected);
end;

end.
