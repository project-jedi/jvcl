{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgListBox.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Enhanced ListBox component that can display its items in three
  dimensional styles. Items captions align in one of 9 positions.
  Component can display glyphs on own items and fill background  with
  bitmap. You can set different fonts for selected item and for
  other list items.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgListBox;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, CommCtrl, ExtCtrls, ImgList,
  {$IFDEF USEJVCL}
  JVCLVer,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, Jvg3DColors;

const
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);

type
  TglLBWallpaperOption = (fwlNone, fwlStretch, fwlTile, fwlGlobal);
  TglLBChangeEvent = procedure(Sender: TObject;
    FOldSelItemIndex, FSelItemIndex: Integer) of object;
  TglLBOnDrawEvent = procedure(Sender: TObject; Msg: TWMDrawItem) of object;
  TglOnGetDragImageEvent = procedure(Sender: TObject; Bitmap: TBitmap;
    var TransparentColor: TColor; var HotSpotX, HotSpotY: Integer) of object;

  TJvgListBox = class(TCustomListBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FAutoTransparentColor: TglAutoTransparentColor;
    FWallpaper: TBitmap;
    FWallpaperImage: TImage;
    FWallpaperOption: TglLBWallpaperOption;
    FNumGlyphs: Word;
    FGlyphsAlign: TJvg2DAlign;
    FTextAlign: TJvg2DAlign;
    FTransparentColor: TColor;
    FHotTrackColor: TColor;
    FItemStyle: TJvgListBoxItemStyle;
    FItemSelStyle: TJvgListBoxItemStyle;
    FGlyphs: TImageList;
    FItemHeight: Word;
    FTextAlign_: UINT;
    HotTrackingItemIndex: Integer;
    FOptions: TglListBoxOptions;
    FChangeGlyphColor: TJvgTwainColors;
    FDragImage: TImageList;
    FOnDrawItem: TglLBOnDrawEvent;
    FOnChange: TglLBChangeEvent;
    FOnGetItemColor: TglOnGetItemColorEvent;
    FOnGetItemFontColor: TglOnGetItemColorEvent;
    FOnGetDragImage: TglOnGetDragImageEvent;
    ThreeDColors: TJvg3DLocalColors;
    FWallpaperBmp: TBitmap;
    FTmpBitmap: TBitmap;
    FOldSelItemIndex: Integer;
    FSelItemIndex: Integer;
    FUseWallpaper: Boolean;
    procedure SetAutoTransparentColor(Value: TglAutoTransparentColor);
    procedure SetWallpaper(Value: TBitmap);
    function GetWallpaper: TBitmap;
    procedure SetWallpaperImage(Value: TImage);
    procedure SetWOpt(Value: TglLBWallpaperOption);
    procedure SetNumGlyphs(Value: Word);
    procedure SetGlyphs(Value: TImageList);
    procedure SetItemHeight(Value: Word);
    procedure SetTransparentColor(Value: TColor);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetAlign;
    procedure SetOptions(Value: TglListBoxOptions);
    function GetSelectedObject: Pointer;
    procedure RecalcHeights;
    procedure SmthChanged(Sender: TObject);
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Msg: TMessage); message WM_MOUSEMOVE;
  protected
    function GetSelCount: Integer; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DestroyWnd; override;
    procedure CreateDragImage;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure InitState(var State: TOwnerDrawState; ByteState: Byte);
  public
    FLeftIndent: Integer;
    FreeObjectsOnDestroy: Boolean;
    IndentLeft: Integer;
    IndentRight: Integer;
    TextIndent: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDragImages: TDragImageList; override;
    property SelectedObject: Pointer read GetSelectedObject;
    property SelCount: Integer read GetSelCount;
  published
    {$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    {$ENDIF USEJVCL}
    property Anchors;
    property Align;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property MultiSelect;
    //    property IntegralHeight;
    property Items;
    property ParentColor;
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
    property WallpaperOption: TglLBWallpaperOption read FWallpaperOption write SetWOpt default fwlNone;
    property NumGlyphs: Word read FNumGlyphs write SetNumGlyphs default 1;
    property GlyphsAlign: TJvg2DAlign read FGlyphsAlign write FGlyphsAlign;
    property ItemStyle: TJvgListBoxItemStyle read FItemStyle write FItemStyle;
    property ItemSelStyle: TJvgListBoxItemStyle read FItemSelStyle write FItemSelStyle;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property TextAlign: TJvg2DAlign read FTextAlign write FTextAlign;
    property ItemHeight: Word read FItemHeight write SetItemHeight default 0;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clBlue;
    property Options: TglListBoxOptions read FOptions write SetOptions;
    property ChangeGlyphColor: TJvgTwainColors read FChangeGlyphColor write FChangeGlyphColor;
    property OnDrawItem: TglLBOnDrawEvent read FOnDrawItem write FOnDrawItem;
    property OnChange: TglLBChangeEvent read FOnChange write FOnChange;
    property OnGetItemColor: TglOnGetItemColorEvent read FOnGetItemColor write FOnGetItemColor;
    property OnGetItemFontColor: TglOnGetItemColorEvent read FOnGetItemFontColor write FOnGetItemFontColor;
    property OnGetDragImage: TglOnGetDragImageEvent read FOnGetDragImage write FOnGetDragImage;
  end;

  TJvgCheckListBox = class(TJvgListBox)
  private
    FCheckWidth: Integer;
    FCheckHeight: Integer;
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetChecked(Index: Integer; State: TCheckBoxState);
    function GetChecked(Index: Integer): TCheckBoxState;
    //    procedure ToggleClickCheck( Index: Integer );
    //    procedure InvalidateCheck( Index: Integer );
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    property Checked[Index: Integer]: TCheckBoxState read GetChecked write SetChecked;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvgUtils;

//=== { TJvgListBox } ========================================================

constructor TJvgListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDisplayDragImage];
  Style := lbOwnerDrawVariable;
  ThreeDColors := TJvg3DLocalColors.Create(Self);
  FWallpaper := TBitmap.Create;
  FTmpBitmap := TBitmap.Create;
  FGlyphsAlign := TJvg2DAlign.Create;
  FTextAlign := TJvg2DAlign.Create;
  FItemStyle := TJvgListBoxItemStyle.Create;
  FItemSelStyle := TJvgListBoxItemStyle.Create;
  FChangeGlyphColor := TJvgTwainColors.Create;
  FDragImage := TImageList.CreateSize(32, 32);
  HotTrackingItemIndex := -1;
  FHotTrackColor := clBlue;
  FLeftIndent := 0;
  FWallpaper := TBitmap.Create;
  //...defaults
  if csDesigning in ComponentState then
  begin
    FItemStyle.Color := clBtnFace;
    FItemStyle.TextStyle := fstNone;
    FItemSelStyle.Color := clBtnShadow;
    FItemSelStyle.TextStyle := fstNone;
  end;
  FWallpaperOption := fwlNone;
  NumGlyphs := 1;
  FTransparentColor := clOlive;
  FAutoTransparentColor := ftcLeftBottomPixel;
  FOptions := [fboHotTrack, fboWordWrap, fboExcludeGlyphs];
  FChangeGlyphColor.FromColor := clBlack;
  FChangeGlyphColor.ToColor := clWhite;
  FGlyphsAlign.OnChanged := SmthChanged;
  FTextAlign.OnChanged := SmthChanged;
  FItemStyle.OnChanged := SmthChanged;
  FItemSelStyle.OnChanged := SmthChanged;
  FChangeGlyphColor.OnChanged := SmthChanged;
end;

destructor TJvgListBox.Destroy;
begin
  FWallpaper.Free;
  ThreeDColors.Free;
  FGlyphsAlign.Free;
  FTextAlign.Free;
  FTmpBitmap.Free;
  FItemStyle.Free;
  FItemSelStyle.Free;
  FChangeGlyphColor.Free;
  FDragImage.Free;
  inherited Destroy;
end;

procedure TJvgListBox.Loaded;
begin
  inherited Loaded;
  Font := ItemStyle.Font;
  Canvas.Font := ItemStyle.Font;
  SetAlign;
  RecalcHeights;

  if fboTransparent in FOptions then
  begin
    if not Assigned(FWallpaper) then
      FWallpaper := TBitmap.Create;
    FWallpaper.Width := Width;
    FWallpaper.Height := Height;
    GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
      FWallpaper.Canvas.Handle);
    FWallpaperBmp := FWallpaper;
    FUseWallpaper := True;
  end
  else
  begin
    if Assigned(FWallpaper) and not FWallpaper.Empty then
      FWallpaperBmp := FWallpaper;
    FUseWallpaper := IsItAFilledBitmap(FWallpaperBmp);
  end;
end;

procedure TJvgListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = WallpaperImage) and (Operation = opRemove) then
    WallpaperImage := nil;
end;

procedure TJvgListBox.DestroyWnd;
var
  I: Integer;
begin
  if FreeObjectsOnDestroy then
    for I := 0 to Items.Count do
      try
        Items.Objects[I].Free;
        Items.Objects[I] := nil;
      except
      end;
  inherited DestroyWnd;
end;

procedure TJvgListBox.CNMeasureItem(var Msg: TWMMeasureItem);
var
  R: TRect;
  Shift: Integer;
const
  WordBreak: array [Boolean] of Integer = (0, DT_WORDBREAK);
begin
  if csReading in ComponentState then
    Exit;
  R := Rect(0, 0, Width - FLeftIndent, 0);
  Shift := 0;
  if (fboExcludeGlyphs in Options) and Assigned(FGlyphs) then
    if FGlyphsAlign.Horizontal = fhaLeft then
      R.Left := FGlyphs.Width
    else
    if FGlyphsAlign.Horizontal = fhaRight then
      R.Right := R.Right - FGlyphs.Width;
  with Msg.MeasureItemStruct^ do
  begin
    Windows.DrawText(Canvas.Handle, PChar(Items[itemID]),
      Length(Items[itemID]), R, DT_CALCRECT or WordBreak[fboWordWrap in Options]);
    if R.Bottom = 0 then
      R.Bottom := 14;
    Msg.MeasureItemStruct^.itemHeight := R.Bottom - R.Top;
    if (ItemStyle.Bevel.Inner <> bvNone) or (ItemSelStyle.Bevel.Inner <> bvNone) then
      if (ItemStyle.Bevel.Bold) or (ItemSelStyle.Bevel.Bold) then
        Inc(Shift, 2)
      else
        Inc(Shift);
    if (ItemStyle.Bevel.Outer <> bvNone) or (ItemSelStyle.Bevel.Outer <> bvNone) then
      if (ItemStyle.Bevel.Bold) or (ItemSelStyle.Bevel.Bold) then
        Inc(Shift, 2)
      else
        Inc(Shift);
    if (ItemStyle.TextStyle <> fstNone) or (ItemSelStyle.TextStyle <> fstNone) then
      Inc(Shift, 2);
    if Assigned(FGlyphs) and (FGlyphs.Height > Integer(itemHeight)) then
      itemHeight := FGlyphs.Height;
    Inc(Msg.MeasureItemStruct^.itemHeight, Shift);
    if FItemHeight > 0 then
      Msg.MeasureItemStruct^.itemHeight := FItemHeight;
  end;
  //  Msg.MeasureItemStruct^.itemHeight:=13;
end;

procedure TJvgListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  Index: Integer;
  R, TxtRect: TRect;
  State: TOwnerDrawState;
  ItemStyle: TJvgListBoxItemStyle;
  LSelected, LDrawWallpaper: Boolean;
  DC: HDC;
  Image: TBitmap;
  TargetCanvas: TCanvas;
  ItemColor, FontColor, GrFromColor, GrToColor: TColor;

  procedure DrawGlyph(R: TRect);
  var
    I, FTranspColor: Integer;
    OldRect: TRect;
  begin
    if (FGlyphs = nil) or (FGlyphs.Count = 0) then
      Exit;
    OldRect := R;
    Inc(R.Top);
    Inc(R.Left);
    case FGlyphsAlign.Horizontal of
      fhaCenter:
        OffsetRect(R, (R.Right - R.Left - Glyphs.Width) div 2, 0);
      fhaRight:
        OffsetRect(R, R.Right - R.Left - Glyphs.Width - 1, 0);
    end;
    case GlyphsAlign.Vertical of
      fvaCenter:
        OffsetRect(R, 0, (R.Bottom - R.Top - Glyphs.Height) div 2);
      fvaBottom:
        OffsetRect(R, 0, R.Bottom - R.Top - Glyphs.Height - 1);
    end;

    if fboSingleGlyph in Options then
      I := 0
    else
    if Index < NumGlyphs then
      I := Index
    else
      I := -1;

    if I >= 0 then
    begin
      FGlyphs.GetBitmap(I, FTmpBitmap);
      if LSelected and (fboChangeGlyphColor in Options) then
        ChangeBitmapColor(FTmpBitmap, FChangeGlyphColor.FromColor,
          FChangeGlyphColor.ToColor);

      if FAutoTransparentColor = ftcUser then
        FTranspColor := FTransparentColor
      else
        FTranspColor := GetTransparentColor(FTmpBitmap, FAutoTransparentColor);

      //      if LDrawWallpaper then
      CreateBitmapExt(DC, FTmpBitmap, Rect(0, 0, 100, 100), R.Left, R.Top,
        fwoNone, fdsDefault, True, FTranspColor, clBlack);
        //      else
        //      begin
 //        ChangeBitmapColor( FTmpBitmap, FTranspColor, ItemStyle.Color );
 //        BitBlt( DC, R.Left, R.Top, FTmpBitmap.Width, FTmpBitmap.Height, FTmpBitmap.Canvas.Handle,
 //                0, 0, SRCCOPY );
 //      end;
    end;
  end;

  procedure DrawWallpaper;

    procedure FillTiled(R: TRect; YOffset: Integer);
    var
      Y, X1, Y1, IWidth, IHeight: Integer;
    begin
      IWidth := Min(R.Right - R.Left + 1, FWallpaperBmp.Width);
      IHeight := Min(R.Bottom - R.Top, FWallpaperBmp.Height);
      X1 := R.Left;
      Y1 := R.Top;
      Y := Y1;
      while X1 < R.Right do
      begin
        if X1 + IWidth > R.Right then
          IWidth := R.Right - X1;
        while Y1 < R.Bottom do
        begin
          //if Y1+IHeight > R.Bottom then IHeight:=R.Bottom-Y1;
          BitBlt(DC, X1, Y1, IWidth, IHeight, FWallpaperBmp.Canvas.Handle,
            0, YOffset, SRCCOPY);
          Inc(Y1, IHeight);
          YOffset := 0;
        end;
        Inc(X1, IWidth);
        Y1 := Y;
      end;
    end;

  begin
    if Assigned(FWallpaperBmp) then
    begin
      case WallpaperOption of
        fwlStretch:
          Canvas.StretchDraw(R, FWallpaperBmp);
        fwlTile:
          FillTiled(R, 0);
        fwlGlobal:
          begin {
            if fboBufferedDraw in Options then with Msg.DrawItemStruct^ do
              Y :=  R.Top + rcItem.Top else Y := R.Top;

            Y := Y-trunc((Y div FWallpaperBmp.Height)*FWallpaperBmp.Height);
            FillTiled( R, Y );

            if Msg.DrawItemStruct^.itemID = UINT(Items.Count-1) then
            begin
              if fboBufferedDraw in Options then with Msg.DrawItemStruct^ do
                Y :=  R.Bottom + rcItem.Top else Y := R.Bottom;
              R2 := Rect ( R.Left-1, R.Bottom+2, R.Right+1, Height );
              Y := Y-trunc((Y div FWallpaperBmp.Height)*FWallpaperBmp.Height);
              FillTiled( R2, Y );
            end;}
            BitBlt(DC, R.Left + 1, R.Top, R.Right - R.Left - 1, R.Bottom -
              R.Top, FWallpaperBmp.Canvas.Handle, 0, R.Top, SRCCOPY);

            with Msg.DrawItemStruct^ do
              if itemID = UINT(Items.Count - 1) then
                BitBlt(DC, rcItem.Left, rcItem.Bottom, rcItem.Right -
                  rcItem.Left, Height, FWallpaperBmp.Canvas.Handle,
                  rcItem.Left, rcItem.Bottom, SRCCOPY);
          end;
      else
        BitBlt(DC, R.Left, R.Top, Min(FWallpaperBmp.Width, R.Right - R.Left),
          Min(FWallpaperBmp.Height, R.Bottom - R.Top),
          FWallpaperBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end;
  end;

begin
  if Items.Count = 0 then
    Exit;

  if not FWallpaper.Empty then
    FWallpaperBmp := FWallpaper
  else
  if Assigned(FWallpaperImage) and Assigned(FWallpaperImage.Picture) and
    Assigned(FWallpaperImage.Picture.Bitmap) then
    FWallpaperBmp := FWallpaperImage.Picture.Bitmap
  else
    FWallpaperBmp := nil;

  FUseWallpaper := IsItAFilledBitmap(FWallpaperBmp);

  with Msg.DrawItemStruct^ do
  begin
    Index := UINT(itemID);
    if Index = -1 then
    begin
      if IsItAFilledBitmap(FWallpaperBmp) then
        BitBlt(hDC, 0, 0, Width, Height, FWallpaperBmp.Canvas.Handle, 0, 0, SRCCOPY);
      Exit;
    end;

    InitState(State, WordRec(LongRec(ItemState).Lo).Lo);

    Canvas.Handle := hDC;
    R := rcItem;
  end;
  Inc(R.Left, IndentLeft);
  Dec(R.Right, IndentRight);
  if fboBufferedDraw in Options then
  begin
    Image := TBitmap.Create;
    Image.Width := R.Right - R.Left;
    Image.Height := R.Bottom - R.Top;
    TargetCanvas := Image.Canvas;
    Dec(R.Bottom, R.Top);
    R.Top := 0;
  end
  else
  begin
    Image := nil;
    TargetCanvas := Canvas;
  end;
  DC := TargetCanvas.Handle;

  LSelected := (State = [odSelected, odFocused]) or (State = [odSelected]);
  if LSelected then
    ItemStyle := FItemSelStyle
  else
    ItemStyle := FItemStyle;

  LDrawWallpaper := (not (LSelected and (FItemStyle.Color <> FItemSelStyle.Color))) and FUseWallpaper;

  //...DrawLBItem
  Inc(R.Left);
  Dec(R.Right);
  Dec(R.Bottom);
  ItemColor := ItemStyle.Color;
  if Assigned(FOnGetItemColor) then
    FOnGetItemColor(Self, Index, ItemColor);
  if fboAutoCtl3DColors in Options then
  begin
    ThreeDColors.CreateAuto3DColors(ItemColor);
    ThreeDColors.MakeGlobal;
  end;
  R := DrawBoxEx(DC, R, ItemStyle.Bevel.Sides, ItemStyle.Bevel.Inner,
    ItemStyle.Bevel.Outer, ItemStyle.Bevel.Bold, ItemColor, LDrawWallpaper);
  if fboAutoCtl3DColors in Options then
    ThreeDColors.MakeLocal;

  Dec(R.Left);
  Inc(R.Right);
  Inc(R.Bottom);
  if ItemStyle.Gradient.Active then
    with ItemStyle do
    begin
      GrFromColor := Gradient.RGBFromColor;
      GrToColor := Gradient.RGBToColor;
      if ItemColor > 0 then
      begin
        if fboItemColorAsGradientFrom in Options then
          Gradient.RGBFromColor := ItemColor;
        if fboItemColorAsGradientTo in Options then
          Gradient.RGBToColor := ItemColor;
      end;
      GradientBox(DC, R, Gradient, Integer(psSolid), 1);
      Gradient.RGBFromColor := GrFromColor;
      Gradient.RGBToColor := GrToColor;
    end;

  if LDrawWallpaper then
    DrawWallpaper;

  if Assigned(FGlyphs) then
  begin
    DrawGlyph(R);
    if fboExcludeGlyphs in Options then
      if FGlyphsAlign.Horizontal = fhaLeft then
        R.Left := R.Left + FGlyphs.Width
      else
      if FGlyphsAlign.Horizontal = fhaRight then
        R.Right := R.Right - FGlyphs.Width
  end;
  Inc(R.Left, FLeftIndent);
  SetBkMode(DC, TRANSPARENT);
  Inc(R.Left);
  Dec(R.Right, 2);

  TxtRect := R;
  Inc(TxtRect.Left, TextIndent);
  if not (fboHideText in Options) then
  begin
    if Assigned(OnGetItemFontColor) then
    begin
      ItemColor := ItemStyle.Font.Color;
      OnGetItemFontColor(Self, Index, ItemColor);
      ItemStyle.Font.Color := ItemColor;
    end;
    FontColor := ItemStyle.Font.Color;
    if HotTrackingItemIndex = Index then
    begin
      ItemStyle.Font.Color := FHotTrackColor;
      //      if LSelected then ItemStyle.Font.Color := clWhite;
    end;
    DrawTextInRect(TargetCanvas.Handle, TxtRect, Items[Index],
      ItemStyle.TextStyle, ItemStyle.Font, FTextAlign_);
    ItemStyle.Font.Color := FontColor;
  end;
  if TargetCanvas <> Canvas then
    BitBlt(Msg.DrawItemStruct^.hDC, Msg.DrawItemStruct^.rcItem.Left,
      Msg.DrawItemStruct^.rcItem.Top,
      Image.Width, Image.Height, Image.Canvas.Handle, 0, 0, SRCCOPY);

  with Msg.DrawItemStruct^ do
    if (odFocused in State) and (fboShowFocus in Options) then
      DrawFocusRect(hDC, rcItem);

  Image.Free;
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Msg);
  if Assigned(FOnChange) then
  begin
    FOldSelItemIndex := FSelItemIndex;
    FSelItemIndex := ItemIndex;
    if FOldSelItemIndex <> FSelItemIndex then
      FOnChange(Self, FOldSelItemIndex, FSelItemIndex);
  end;
end;

procedure TJvgListBox.CMMouseLeave(var Msg: TMessage);
var
  R: TRect;
begin
  inherited;
  if HotTrackingItemIndex <> -1 then
  begin
    R := ItemRect(HotTrackingItemIndex);
    HotTrackingItemIndex := -1;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJvgListBox.WMMouseMove(var Msg: TMessage);
var
  Pt: TPoint;
  R: TRect;
  ItemIndex: Integer;
begin
  inherited;
  if not (fboHotTrack in Options) and not (fboHotTrackSelect in Options) then
    Exit;
  Pt.X := LOWORD(Msg.lParam);
  Pt.Y := HiWord(Msg.lParam);
  ItemIndex := ItemAtPos(Pt, True);

  if ItemIndex = HotTrackingItemIndex then
    Exit;

  if fboHotTrackSelect in Options then
  begin
    Self.ItemIndex := ItemIndex;
    InvalidateRect(Handle, nil, False);
    Exit;
  end;

  if HotTrackingItemIndex <> -1 then
  begin
    R := ItemRect(HotTrackingItemIndex);
    InvalidateRect(Handle, @R, False);
  end;
  HotTrackingItemIndex := ItemIndex;
  if HotTrackingItemIndex <> -1 then
  begin
    R := ItemRect(HotTrackingItemIndex);
    InvalidateRect(Handle, @R, False);
  end;
end;

function TJvgListBox.GetDragImages: TDragImageList;
begin
  if FDragImage.Count > 0 then
    Result := FDragImage
  else
    Result := nil;
end;

procedure TJvgListBox.CreateDragImage;
var
  HotSpotX, HotSpotY: Integer;
  TranspColor: TColor;
  Bmp: TBitmap;
  Pt: TPoint;
  R: TRect;
begin
  FDragImage.Clear;
  if ItemIndex = -1 then
    Exit;
  R := ItemRect(ItemIndex);

  Bmp := TBitmap.Create;
  with Bmp do
  try
    GetCursorPos(Pt);
    with ScreenToClient(Pt) do
    begin
      HotSpotX := X - R.Left;
      HotSpotY := Y - R.Top
    end;
    if Assigned(FOnGetDragImage) then
      FOnGetDragImage(Self, Bmp, TranspColor, HotSpotX, HotSpotY)
    else
    begin
      Width := R.Right - R.Left;
      Height := R.Bottom - R.Top;
      Canvas.Font := ItemSelStyle.Font;
      Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(1, 1, Items[ItemIndex]);
      TranspColor := clWhite;
    end;
    FDragImage.Width := Width;
    FDragImage.Height := Height;
    FDragImage.AddMasked(Bmp, TranspColor);
    FDragImage.SetDragImage(0, HotSpotX, HotSpotY);
  finally
    Bmp.Free;
  end;
end;

procedure TJvgListBox.DoStartDrag(var DragObject: TDragObject);
begin
  inherited DoStartDrag(DragObject);
  CreateDragImage;
end;

procedure TJvgListBox.SetAutoTransparentColor(Value: TglAutoTransparentColor);
begin
  if FAutoTransparentColor <> Value then
  begin
    FAutoTransparentColor := Value;
    Invalidate;
  end;
end;

function TJvgListBox.GetWallpaper: TBitmap;
begin
  if not Assigned(FWallpaper) then
    FWallpaper := TBitmap.Create;
  Result := FWallpaper;
end;

procedure TJvgListBox.SetWallpaper(Value: TBitmap);
begin
  Wallpaper.Assign(Value);
  Invalidate;
end;

procedure TJvgListBox.SetWallpaperImage(Value: TImage);
begin
  FWallpaperImage := Value;
  Invalidate;
end;

procedure TJvgListBox.SetWOpt(Value: TglLBWallpaperOption);
begin
  FWallpaperOption := Value;
  Invalidate;
end;

procedure TJvgListBox.SetNumGlyphs(Value: Word);
begin
  if Value >= 1 then
  begin
    FNumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvgListBox.SetGlyphs(Value: TImageList);
begin
  FGlyphs := Value;
  Invalidate;
end;

procedure TJvgListBox.SetItemHeight(Value: Word);
begin
  FItemHeight := Value;
  RecalcHeights;
end;

procedure TJvgListBox.SetAlign;
begin
  if fboWordWrap in Options then
    FTextAlign_ := DT_WORDBREAK or DT_NOPREFIX
  else
    FTextAlign_ := DT_SINGLELINE or DT_NOPREFIX;
  case FTextAlign.Horizontal of
    fhaLeft:
      FTextAlign_ := FTextAlign_ or DT_LEFT;
    fhaCenter:
      FTextAlign_ := FTextAlign_ or DT_CENTER;
  else
    FTextAlign_ := FTextAlign_ or DT_RIGHT;
  end;
  case FTextAlign.Vertical of
    fvaTop:
      FTextAlign_ := FTextAlign_ or DT_TOP;
    fvaCenter:
      FTextAlign_ := FTextAlign_ or DT_VCENTER;
  else
    FTextAlign_ := FTextAlign_ or DT_BOTTOM;
  end;
end;

procedure TJvgListBox.SetTransparentColor(Value: TColor);
begin
  FTransparentColor := Value;
  if FAutoTransparentColor <> ftcUser then
    Invalidate;
end;

procedure TJvgListBox.SetHotTrackColor(Value: TColor);
var
  R: TRect;
begin
  if FHotTrackColor = Value then
    Exit;
  FHotTrackColor := Value;
  if HotTrackingItemIndex <> -1 then //...user can program hottrack blinking effect!
  begin
    R := ItemRect(HotTrackingItemIndex);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJvgListBox.SetOptions(Value: TglListBoxOptions);
begin
  if FOptions = Value then
    Exit;
  if not (csLoading in ComponentState) then
    {  if (fboTransparent in Value) and not (fboTransparent in FOptions)then
      begin
        FWallpaper.Width := Width; FWallpaper.Height := Height;
        GetParentImageRect( Self, Bounds(Left,Top,Width,Height),
                     FWallpaper.Canvas.Handle );
        FWallpaperBmp := FWallpaper;
        FUseWallpaper := True;
      end;  }
    FOptions := Value;
  SetAlign;
  RecalcHeights;
  Invalidate;
end;

function TJvgListBox.GetSelectedObject: Pointer;
begin
  if ItemIndex >= 0 then
    Result := Items.Objects[ItemIndex]
  else
    Result := nil;
end;

function TJvgListBox.GetSelCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Selected[I] then
      Inc(Result);
end;

procedure TJvgListBox.RecalcHeights;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
  begin
    if Assigned(Items.Objects[I]) then
      Items.InsertObject(I, Items.Strings[I], Items.Objects[I])
    else
      Items.Insert(I, Items.Strings[I]);
    Items.Delete(I + 1);
  end;
  Items.EndUpdate;
end;

procedure TJvgListBox.SmthChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    RecalcHeights;
    SetAlign;
    Invalidate;
  end;
end;

//=== { TJvgCheckListBox } ===================================================

constructor TJvgCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckWidth := 14;
  FCheckHeight := 14;
  FLeftIndent := 22;
end;

procedure TJvgCheckListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  R: TRect;
  Index: Integer;
  State: TOwnerDrawState;
begin
  inherited;
  with Msg.DrawItemStruct^ do
  begin
    InitState(State, WordRec(LongRec(ItemState).Lo).Lo);

    Canvas.Handle := hDC;
    R := rcItem;
    Index := itemID;
  end;
  if Index < Items.Count then
  begin
    R.Right := R.Left + FCheckWidth + 5;
    DrawCheck(R, GetState(Index));
  end;
end;

function TJvgCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if Index > -1 then
    Result := TCheckBoxState(Items.Objects[Index])
  else
    Result := cbUnchecked;
end;

procedure TJvgCheckListBox.DrawCheck(R: TRect; AState: TCheckBoxState);
var
  DrawState: Integer;
  DrawRect: TRect;
begin
  case AState of
    cbChecked:
      DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
    cbUnchecked:
      DrawState := DFCS_BUTTONCHECK;
  else // cbGrayed
    DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
  end;
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckWidth) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;

  DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
end;

procedure TJvgListBox.InitState(var State: TOwnerDrawState; ByteState: Byte);
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

function TJvgCheckListBox.GetChecked(Index: Integer): TCheckBoxState;
begin
  Result := TCheckBoxState(Items.Objects[Index]);
end;

procedure TJvgCheckListBox.SetChecked(Index: Integer; State: TCheckBoxState);
begin
  Items.Objects[Index] := Pointer(State);
end;

procedure TJvgCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  APoint: TPoint;
  Index: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    APoint.X := X;
    APoint.Y := Y;
    Index := ItemAtPos(APoint, True);
    case TCheckBoxState(Items.Objects[Index]) of
      cbUnchecked:
        Items.Objects[Index] := Pointer(cbChecked);
      cbChecked:
        Items.Objects[Index] := Pointer(cbUnchecked);
      cbGrayed:
       ;
    end;
    Invalidate;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

