{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgListBox.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
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

{  Enhanced ListBox component that  can  display    its  items in  three
 dimensional  styles.   Items  captions align in one of 9 positions.
 Component can display glyphs on own items and fill background  with
 bitmap.  You can set different fonts  for  selected  item  and  for
 other list items.
}

unit JvgListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, CommCtrl, ExtCtrls, Imglist,
  JvgTypes, JVCLVer, JvgCommClasses, Jvg3DColors;

const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

type
  TglLBWallpaperOption = (fwlNone, fwlStretch, fwlTile, fwlGlobal);
  TglLBChangeEvent = procedure(Sender: TObject; OldSelItemIndex, SelItemIndex:
    Integer) of object;
  TglLBOnDrawEvent = procedure(Sender: TObject; Message: TWMDrawItem) of
    object;
  TglOnGetDragImageEvent = procedure(Sender: TObject; Bitmap: TBitmap; var
    TransparentColor: TColor;
    var HotSpotX, HotSpotY: Integer) of object;

  TJvgListBox = class(TCustomListBox)
  private
    FAutoTrColor: TglAutoTransparentColor;
    FWallpaper: TBitmap;
    FWallpaperImage: TImage;
    FWallpaperOption: TglLBWallpaperOption;
    FNumGlyphs: word;
    FGlyphsAlign: TJvg2DAlign;
    FTextAlign: TJvg2DAlign;
    FTransparentColor: TColor;
    FHotTrackColor: TColor;
    FItemStyle: TJvgListBoxItemStyle;
    FItemSelStyle: TJvgListBoxItemStyle;
    FGlyphs: TImageList;
    FItemHeight: word;
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
    //'''''''''''''''''''''''''''''''''''''''''
    ThreeDColors: TJvg3DLocalColors;
    WallpaperBmp: TBitmap;
    TmpBitmap: TBitmap;
    OldSelItemIndex,
      SelItemIndex: Integer;
    fUseWallpaper: Boolean;
    fAboutJVCL: TJVCLAboutInfo;
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetWallpaper(Value: TBitmap);
    function GetWallpaper: TBitmap;
    procedure SetWallpaperImage(Value: Timage);
    procedure SetWOpt(Value: TglLBWallpaperOption);
    procedure SetNumGlyphs(Value: word);
    procedure SetGlyphs(Value: TImageList);
    procedure SetItemHeight(Value: word);
    procedure SetTransparentColor(Value: TColor);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetAlign;
    procedure SetOptions(Value: TglListBoxOptions);
    function GetSelectedObject: Pointer;

    procedure RecalcHeights;
    procedure SmthChanged(Sender: TObject);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message
      CN_MEASUREITEM;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
  protected
    function GetSelCount: Integer; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DestroyWnd; override;
    procedure CreateDragImage;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure InitState(var State: TOwnerDrawState; ByteState: Byte);
  public
    FLeftIndent: Integer;
    FreeObjectsOnDestroy: Boolean;
    IndentLeft, IndentRight, TextIndent: Integer;
    property SelectedObject: Pointer read GetSelectedObject;
    property SelCount: Integer read GetSelCount;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDragImages: TDragImageList; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;

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
      read FAutoTrColor write SetAutoTrColor default ftcLeftBottomPixel;
    property Wallpaper: TBitmap read GetWallpaper write SetWallpaper;
    property WallpaperImage: TImage read FWallpaperImage write SetWallpaperImage;
    property WallpaperOption: TglLBWallpaperOption read FWallpaperOption write SetWOpt default fwlNone;
    property NumGlyphs: word read FNumGlyphs write SetNumGlyphs default 1;
    property GlyphsAlign: TJvg2DAlign read FGlyphsAlign write FGlyphsAlign;
    property ItemStyle: TJvgListBoxItemStyle read FItemStyle write FItemStyle;
    property ItemSelStyle: TJvgListBoxItemStyle read FItemSelStyle write FItemSelStyle;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property TextAlign: TJvg2DAlign read FTextAlign write FTextAlign;
    property ItemHeight: word read FItemHeight write SetItemHeight default 0;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clBlue;
    property Options: TglListBoxOptions read FOptions write SetOptions;
    property ChangeGlyphColor: TJvgTwainColors read FChangeGlyphColor write FChangeGlyphColor;
    property OnDrawItem: TglLBOnDrawEvent read FOnDrawItem write FOnDrawItem;
    property OnChange: TglLBChangeEvent read FOnChange write FOnChange;
    property OnGetItemColor: TglOnGetItemColorEvent read FOnGetItemColor write FOnGetItemColor;
    property OnGetItemFontColor: TglOnGetItemColorEvent read FOnGetItemFontColor
       write FOnGetItemFontColor;
    property OnGetDragImage: TglOnGetDragImageEvent read FOnGetDragImage write FOnGetDragImage;
  end;

  TJvgCheckListBox = class(TJvgListBox)
  private
    FCheckWidth, FCheckHeight: Integer;
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetChecked(Index: Integer; State: TCheckBoxState);
    function GetChecked(Index: Integer): TCheckBoxState;
    //    procedure ToggleClickCheck( Index: Integer );
    //    procedure InvalidateCheck( Index: Integer );
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    property Checked[Index: Integer]: TCheckBoxState read GetChecked write SetChecked;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  JvgUtils;

//*****************************************_____________LowLevel METHODS

constructor TJvgListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDisplayDragImage];
  Style := lbOwnerDrawVariable;
  ThreeDColors := TJvg3DLocalColors.Create(Self);
  FWallpaper := TBitmap.Create;
  TmpBitmap := TBitmap.Create;
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
    with FItemStyle do
    begin
      Color := clBtnFace;
      TextStyle := fstNone;
    end;
    with FItemSelStyle do
    begin
      Color := clBtnShadow;
      TextStyle := fstNone;
    end;
  end;
  FWallpaperOption := fwlNone;
  NumGlyphs := 1;
  FTransparentColor := clOlive;
  FAutoTrColor := ftcLeftBottomPixel;
  FOptions := [fboHotTrack, fboWordWrap, fboExcludeGlyphs];
  FChangeGlyphColor.FromColor := clBlack;
  FChangeGlyphColor.ToColor := clWhite;
  FGlyphsAlign.OnChanged := SmthChanged;
  FTextAlign.OnChanged := SmthChanged;
  FItemStyle.OnChanged := SmthChanged;
  FItemSelStyle.OnChanged := SmthChanged;
  FChangeGlyphColor.OnChanged := SmthChanged;
end;
//------

destructor TJvgListBox.Destroy;
begin
  FWallpaper.Free;
  ThreeDColors.Free;
  FGlyphsAlign.Free;
  FTextAlign.Free;
  TmpBitmap.Free;
  FItemStyle.Free;
  FItemSelStyle.Free;
  FChangeGlyphColor.Free;
  FDragImage.Free;
  inherited Destroy;
end;
//______________________________________________________________

procedure TJvgListBox.Loaded;
begin
  inherited;
  Font := ItemStyle.Font;
  Canvas.Font := ItemStyle.Font;
  SetAlign;
  RecalcHeights;

  if (fboTransparent in FOptions) then
  begin
    if not Assigned(FWallpaper) then
      FWallpaper := TBitmap.Create;
    FWallpaper.Width := Width;
    FWallpaper.Height := Height;
    GetParentImageRect(Self, Bounds(Left, Top, Width, Height),
      FWallpaper.Canvas.Handle);
    WallpaperBmp := FWallpaper;
    fUseWallpaper := True;
  end
  else
  begin
    if Assigned(FWallpaper) and not FWallpaper.Empty then
      WallpaperBmp := FWallpaper;
    fUseWallpaper := IsItAFilledBitmap(WallpaperBmp);
  end;
end;

//______________________________________________________________

procedure TJvgListBox.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = WallpaperImage) and (Operation = opRemove) then
    WallpaperImage := nil;
end;
//______________________________________________________________

procedure TJvgListBox.DestroyWnd;
var
  i: Integer;
begin //...free all objects is assotiated with items
  if FreeObjectsOnDestroy then
    for i := 0 to Items.Count do
    try
      if Assigned(Items.Objects[i]) then
        Items.Objects[i].Free;
    except
    end;
  inherited;
end;
//______________________________________________________________

procedure TJvgListBox.CNMeasureItem(var Message: TWMMeasureItem);
var
  r: TRect;
  Shift: Integer;
const
  WordBreak: array[Boolean] of Integer = (0, DT_WORDBREAK);
begin
  if csReading in ComponentState then
    exit;
  R := Rect(0, 0, Width - FLeftIndent, 0);
  Shift := 0;
  if (fboExcludeGlyphs in Options) and Assigned(FGlyphs) then
    if FGlyphsAlign.Horizontal = fhaLeft then
      r.left := FGlyphs.Width
    else if FGlyphsAlign.Horizontal = fhaRight then
      r.right := r.right - FGlyphs.Width;
  with Message.MeasureItemStruct^ do
  begin
    DrawText(Canvas.Handle, PChar(Items[itemID]),
      Length(Items[itemID]), r, DT_CALCRECT or WordBreak[fboWordWrap in
      Options]);
    if r.bottom = 0 then
      r.bottom := 14;
    Message.MeasureItemStruct^.itemHeight := r.bottom - r.top;
    if (ItemStyle.Bevel.Inner <> bvNone) or (ItemSelStyle.Bevel.Inner <>
      bvNone) then
      if (ItemStyle.Bevel.Bold) or (ItemSelStyle.Bevel.Bold) then
        inc(Shift, 2)
      else
        inc(Shift);
    if (ItemStyle.Bevel.Outer <> bvNone) or (ItemSelStyle.Bevel.Outer <>
      bvNone) then
      if (ItemStyle.Bevel.Bold) or (ItemSelStyle.Bevel.Bold) then
        inc(Shift, 2)
      else
        inc(Shift);
    if (ItemStyle.TextStyle <> fstNone) or (ItemSelStyle.TextStyle <> fstNone) then
      inc(Shift, 2);
    if Assigned(FGlyphs) and (FGlyphs.Height > Integer(itemHeight)) then
      itemHeight := FGlyphs.Height;
    inc(Message.MeasureItemStruct^.itemHeight, Shift);
    if (FItemHeight > 0)
      {and(Message.MeasureItemStruct^.itemHeight<FItemHeight)}then
      Message.MeasureItemStruct^.itemHeight := FItemHeight;
  end;
  //  Message.MeasureItemStruct^.itemHeight:=13;
end;
//______________________________________________________________

procedure TJvgListBox.CNDrawItem(var Message: TWMDrawItem);
var
  Index: Integer;
  R, TxtRect: TRect;
  State: TOwnerDrawState;
  ItemStyle: TJvgListBoxItemStyle;
  fSelected, fDrawWallpapper: Boolean;
  DC: HDC;
  Image: TBitmap;
  TargetCanvas: TCanvas;
  ItemColor, FontColor, GrFromColor, GrToColor: TColor;

  procedure DrawGlyph(r: TRect);
  var
    i, FTranspColor: Integer;
    OldRect: TRect;
  begin
    if (FGlyphs = nil) or (FGlyphs.Count = 0) then
      exit;
    OldRect := r;
    inc(r.top);
    inc(r.left);
    case FGlyphsAlign.Horizontal of
      fhaCenter: OffsetRect(r, (r.right - r.left - Glyphs.Width) div 2, 0);
      fhaRight: OffsetRect(r, r.right - r.left - Glyphs.Width - 1, 0);
    end;
    case GlyphsAlign.Vertical of
      fvaCenter: OffsetRect(r, 0, (r.bottom - r.top - Glyphs.height) div 2);
      fvaBottom: OffsetRect(r, 0, r.bottom - r.top - Glyphs.height - 1);
    end;

    if fboSingleGlyph in Options then
      i := 0
    else if Index < NumGlyphs then
      i := Index
    else
      i := -1;

    if i >= 0 then
    begin
      FGlyphs.GetBitmap(i, TmpBitmap);
      if fSelected and (fboChangeGlyphColor in Options) then
        ChangeBitmapColor(TmpBitmap, FChangeGlyphColor.FromColor,
          FChangeGlyphColor.ToColor);

      if FAutoTrColor = ftcUser then
        FTranspColor := FTransparentColor
      else
        FTranspColor := GetTransparentColor(TmpBitmap, FAutoTrColor);

      //      if fDrawWallpapper then
      CreateBitmapExt(DC, TmpBitmap, Rect(0, 0, 100, 100), r.left, r.top,
        fwoNone, fdsDefault, True, FTranspColor, clBlack)
        //      else
        //      begin
 //        ChangeBitmapColor( TmpBitmap, FTranspColor, ItemStyle.Color );
 //        BitBlt( DC, r.left, r.top, TmpBitmap.Width, TmpBitmap.Height, TmpBitmap.Canvas.handle,
 //                0, 0, SRCCOPY );
 //      end;
    end;
  end;

  procedure DrawWallpaper; //( DC: HDC; r:TRect );

    procedure FillTiled(R: TRect; yOffset: Integer);
    var
      Y, x_, y_, IWidth, IHeight: Integer;
    begin
      IWidth := min(r.right - r.left + 1, WallpaperBmp.Width);
      IHeight := min(r.bottom - r.top, WallpaperBmp.Height);
      x_ := r.Left;
      y_ := r.top;
      y := y_;
      while x_ < r.right do
      begin
        if x_ + IWidth > r.right then
          IWidth := r.right - x_;
        while y_ < r.bottom do
        begin
          //if y_+IHeight > r.bottom then IHeight:=r.bottom-y_;
          BitBlt(DC, x_, y_, IWidth, IHeight, WallpaperBmp.Canvas.Handle,
            0, yOffset, SRCCOPY);
          Inc(y_, IHeight);
          YOffset := 0;
        end;
        Inc(x_, IWidth);
        y_ := y;
      end;
    end;
  begin
    if Assigned(WallpaperBmp) then
    begin
      case WallpaperOption of
        fwlStretch:
          Canvas.StretchDraw(r, WallpaperBmp);
        fwlTile:
          begin
            FillTiled(R, 0);
          end;
        fwlGlobal:
          begin {
            if fboBufferedDraw in Options then with Message.DrawItemStruct^ do
              y :=  r.top + rcItem.Top else y := r.top;

            y := y-trunc((y div WallpaperBmp.Height)*WallpaperBmp.Height);
            FillTiled( R, y );

            if Message.DrawItemStruct^.itemID = UINT(Items.Count-1) then
            begin
              if fboBufferedDraw in Options then with Message.DrawItemStruct^ do
                y :=  r.bottom + rcItem.Top else y := r.bottom;
              R2 := Rect ( r.left-1, r.bottom+2, r.right+1, height );
              y := y-trunc((y div WallpaperBmp.Height)*WallpaperBmp.Height);
              FillTiled( R2, y );
            end;}
            BitBlt(DC, r.left + 1, r.top, r.right - r.left - 1, r.bottom -
              r.top, WallpaperBmp.Canvas.Handle, 0, r.top, SRCCOPY);

            with Message.DrawItemStruct^ do
              if itemID = UINT(Items.Count - 1) then
              begin
                BitBlt(DC, rcItem.left, rcItem.bottom, rcItem.right -
                  rcItem.left, height, WallpaperBmp.Canvas.Handle,
                  rcItem.left, rcItem.bottom, SRCCOPY);
              end;

          end;
      else
        BitBlt(DC, r.left, r.top, min(WallpaperBmp.Width, r.right - r.left),
          min(WallpaperBmp.Height, r.bottom - r.top),
          WallpaperBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end;
  end;
begin
  if Items.Count = 0 then
    exit;

  if not FWallpaper.Empty then
    WallpaperBmp := FWallpaper
  else if Assigned(FWallpaperImage) and Assigned(FWallpaperImage.Picture) and
    Assigned(FWallpaperImage.Picture.Bitmap) then
    WallpaperBmp := FWallpaperImage.Picture.Bitmap
  else
    WallpaperBmp := nil;

  fUseWallpaper := IsItAFilledBitmap(WallpaperBmp);

  with Message.DrawItemStruct^ do
  begin
    Index := UINT(itemID);
    if Index = -1 then
    begin
      if IsItAFilledBitmap(WallpaperBmp) then
        BitBlt(hDC, 0, 0, Width, Height, WallpaperBmp.Canvas.Handle, 0, 0,
          SRCCOPY);
      exit;
    end;

    InitState(State, WordRec(LongRec(ItemState).Lo).Lo);

    Canvas.Handle := hDC;
    R := rcItem;
  end;
  inc(R.Left, IndentLeft);
  dec(R.Right, IndentRight);
  if fboBufferedDraw in Options then
  begin
    Image := TBitmap.Create;
    Image.Width := R.right - R.left;
    Image.Height := R.bottom - R.top;
    TargetCanvas := Image.Canvas;
    dec(R.Bottom, R.top);
    R.top := 0;
  end
  else
  begin
    Image := nil;
    TargetCanvas := Canvas;
  end;
  DC := TargetCanvas.Handle;

  fSelected := (State = [odSelected, odFocused]) or (State = [odSelected]);
  if fSelected then
    ItemStyle := FItemSelStyle
  else
    ItemStyle := FItemStyle;

  fDrawWallpapper := (not (fSelected and (FItemStyle.Color <>
    FItemSelStyle.Color))) and fUseWallpaper;

  //...DrawLBItem
  inc(R.left);
  dec(R.right);
  dec(R.bottom);
  ItemColor := ItemStyle.Color;
  if Assigned(OnGetItemColor) then
    OnGetItemColor(Self, Index, ItemColor);
  if fboAutoCtl3DColors in Options then
  begin
    ThreeDColors.CreateAuto3DColors(ItemColor);
    ThreeDColors.MakeGlobal;
  end;
  R := DrawBoxEx(DC, R, ItemStyle.Bevel.Sides, ItemStyle.Bevel.Inner,
    ItemStyle.Bevel.Outer,
    ItemStyle.Bevel.Bold, ItemColor, fDrawWallpapper);
  if fboAutoCtl3DColors in Options then
    ThreeDColors.MakeLocal;

  dec(R.left);
  inc(R.right);
  inc(R.bottom);
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

  if fDrawWallpapper then
    DrawWallpaper;

  if Assigned(FGlyphs) then
  begin
    DrawGlyph(R);
    if (fboExcludeGlyphs in Options) then
      if FGlyphsAlign.Horizontal = fhaLeft then
        R.left := R.left + FGlyphs.Width
      else if FGlyphsAlign.Horizontal = fhaRight then
        R.right := R.right - FGlyphs.Width
  end;
  inc(R.Left, FLeftIndent);
  SetBkMode(DC, TRANSPARENT);
  inc(R.Left);
  dec(R.right, 2);

  TxtRect := R;
  inc(TxtRect.Left, TextIndent);
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
      //      if fSelected then ItemStyle.Font.Color := clWhite;
    end;
    DrawTextInRect(TargetCanvas.handle, TxtRect, Items[Index],
      ItemStyle.TextStyle, ItemStyle.Font, FTextAlign_);
    ItemStyle.Font.Color := FontColor;
  end;
  if TargetCanvas <> Canvas then
    BitBlt(Message.DrawItemStruct^.hDC, Message.DrawItemStruct^.rcItem.Left,
      Message.DrawItemStruct^.rcItem.Top,
      Image.Width, Image.Height, Image.Canvas.handle, 0, 0, SRCCOPY);

  with Message.DrawItemStruct^ do
    if (odFocused in State) and (fboShowFocus in Options) then
      DrawFocusRect(hDC, rcItem);

  if Assigned(Image) then
    Image.Free;
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Message);
  if Assigned(FOnChange) then
  begin
    OldSelItemIndex := SelItemIndex;
    SelItemIndex := ItemIndex;
    if OldSelItemIndex <> SelItemIndex then
      FOnChange(Self, OldSelItemIndex, SelItemIndex);
  end;
end;

procedure TJvgListBox.CMMouseLeave(var Message: TMessage);
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

procedure TJvgListBox.WMMouseMove(var Message: TMessage);
var
  pt: Tpoint;
  R: TRect;
  itemIndex: Integer;
begin
  inherited;
  if not (fboHotTrack in Options) and not (fboHotTrackSelect in Options) then
    exit;
  pt.x := LOWORD(Message.lParam);
  pt.y := HIWORD(Message.lParam);
  itemIndex := ItemAtPos(pt, True);

  if itemIndex = HotTrackingItemIndex then
    exit;

  if fboHotTrackSelect in Options then
  begin
    Self.ItemIndex := itemIndex;
    InvalidateRect(Handle, nil, False);
    exit;
  end;

  if HotTrackingItemIndex <> -1 then
  begin
    R := ItemRect(HotTrackingItemIndex);
    InvalidateRect(Handle, @R, False);
  end;
  HotTrackingItemIndex := itemIndex;
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
  pt: TPoint;
  R: TRect;
begin
  FDragImage.Clear;
  if ItemIndex = -1 then
    exit;
  R := ItemRect(ItemIndex);

  Bmp := TBitmap.Create;
  with Bmp do
  try
    GetCursorPos(pt);
    with ScreenToClient(pt) do
    begin
      HotSpotX := X - R.Left;
      HotSpotY := Y - R.Top
    end;
    if Assigned(OnGetDragImage) then
      OnGetDragImage(Self, Bmp, TranspColor, HotSpotX, HotSpotY)
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
  inherited;
  CreateDragImage;
end;

//*****************************************_____________PROPERTY METHODS

procedure TJvgListBox.SetAutoTrColor(Value: TglAutoTransparentColor);
begin
  if FAutoTrColor = Value then
    exit;
  FAutoTrColor := Value;
  Invalidate;
end;

function TJvgListBox.GetWallpaper: TBitmap;
begin
  if not Assigned(FWallpaper) then
    FWallpaper := TBitmap.Create;
  Result := FWallpaper;
end;

procedure TJvgListBox.SetWallpaper(Value: TBitmap);
begin
  FWallpaper.Assign(Value);
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

procedure TJvgListBox.SetNumGlyphs(Value: word);
begin
  if Value < 1 then
    exit;
  FNumGlyphs := Value;
  Invalidate;
end;

procedure TJvgListBox.SetGlyphs(Value: TImageList);
begin
  FGlyphs := Value;
  Invalidate;
end;

procedure TJvgListBox.SetItemHeight(Value: word);
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
    fhaLeft: FTextAlign_ := FTextAlign_ or DT_LEFT;
    fhaCenter: FTextAlign_ := FTextAlign_ or DT_CENTER;
  else
    FTextAlign_ := FTextAlign_ or DT_RIGHT;
  end;
  case FTextAlign.Vertical of
    fvaTop: FTextAlign_ := FTextAlign_ or DT_TOP;
    fvaCenter: FTextAlign_ := FTextAlign_ or DT_VCENTER;
  else
    FTextAlign_ := FTextAlign_ or DT_BOTTOM;
  end;
end;

procedure TJvgListBox.SetTransparentColor(Value: TColor);
begin
  FTransparentColor := Value;
  if FAutoTrColor <> ftcUser then
    Invalidate;
end;

procedure TJvgListBox.SetHotTrackColor(Value: TColor);
var
  R: TRect;
begin
  if FHotTrackColor = Value then
    exit;
  FHotTrackColor := Value;
  if HotTrackingItemIndex <> -1 then //...user can programm hottrack blinking effect!
  begin
    R := ItemRect(HotTrackingItemIndex);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJvgListBox.SetOptions(Value: TglListBoxOptions);
begin
  if FOptions = Value then
    exit;
  if not (csLoading in ComponentState) then
    {  if (fboTransparent in Value) and not (fboTransparent in FOptions)then
      begin
        FWallpaper.Width := Width; FWallpaper.Height := Height;
        GetParentImageRect( Self, Bounds(Left,Top,Width,Height),
                     FWallpaper.Canvas.Handle );
        WallpaperBmp := FWallpaper;
        fUseWallpaper := True;
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
  i: Integer;
begin
  Result := 0;
  for i := 0 to Items.Count - 1 do
    if Selected[i] then
      inc(Result);
end;
//_______________________________________________SERVEICE PROCs

procedure TJvgListBox.RecalcHeights;
var
  i: Integer;
begin
  Items.BeginUpdate;
  for i := 0 to Items.Count - 1 do
  begin
    if Assigned(Items.Objects[i]) then
      Items.InsertObject(i, Items.Strings[i], Items.Objects[i])
    else
      Items.Insert(i, Items.Strings[i]);
    Items.Delete(i + 1);
  end;
  Items.EndUpdate;
end;

procedure TJvgListBox.SmthChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) then
    exit;
  RecalcHeights;
  SetAlign;
  Invalidate;
end;

//==============================================

constructor TJvgCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FCheckWidth := 14;
  FCheckHeight := 14;
  FLeftIndent := 22;
end;

destructor TJvgCheckListBox.Destroy;
begin
  inherited;
end;

procedure TJvgCheckListBox.CNDrawItem(var Message: TWMDrawItem);
var
  R: TRect;
  Index: Integer;
  State: TOwnerDrawState;
begin
  inherited;
  with Message.DrawItemStruct^ do
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
  inherited;
  if Button = mbLeft then
  begin
    APoint.X := X;
    APoint.Y := Y;
    Index := ItemAtPos(APoint, True);
    case TCheckBoxState(Items.Objects[Index]) of
      cbUnchecked: Items.Objects[Index] := Pointer(cbChecked);
      cbChecked: Items.Objects[Index] := Pointer(cbUnchecked);
      cbGrayed: ;
    end;
    invalidate;
  end;
end;

end.

