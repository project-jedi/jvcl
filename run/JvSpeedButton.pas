{$I JVCL.INC}
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpeedButton.pas, released on 2003-10-19.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Peter Thornqvist [peter3@peter3.com]

Last Modified: 2003-10-19

Changes:
2003-10-19:
  * Moved TJvSpeedButton from JvxCtrls to this unit

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit JvSpeedButton;

interface
uses
  Windows,
  Messages, Classes, Controls, Graphics, ExtCtrls,
  Buttons, Menus, ImgList,
  JvComponent, JvTypes;

type
  TJvNumGlyphs = 1..5;
  TJvDropDownMenuPos = (dmpBottom, dmpRight);
  TJvButtonState = (rbsUp, rbsDisabled, rbsDown, rbsExclusive, rbsInactive);

  TJvSpeedButton = class(TJvGraphicControl)
  private
    FAllowAllUp: boolean;
    FAllowTimer, FOver: boolean;
    FDown: boolean;
    FDragging: boolean;
    FDrawImage: TBitmap;
    FDropDownMenu: TPopupMenu;
    FFlat: boolean;
    FFontSave: TFont;
    FGlyph: Pointer;
    FGroupIndex: integer;
    FHintColor: TColor;
    FHotGlyph: TBitmap;
    FHotTrack: boolean;
    FHotTrackFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FInactiveGrayed: boolean;
    FInitRepeatPause: Word;
    FLayout: TButtonLayout;
    FMargin: integer;
    FMarkDropDown: boolean;
    FMenuPosition: TJvDropDownMenuPos;
    FMenuTracking: boolean;
    FModalResult: TModalResult;
    FMouseInControl: boolean;
    FOldGlyph: TBitmap;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FRepeatPause: Word;
    FRepeatTimer: TTimer;
    FSaved: TColor;
    FSpacing: integer;
    FStyle: TButtonStyle;
    FTransparent: boolean;
    function GetAlignment: TAlignment;
    function GetGlyph: TBitmap;
    function GetGrayNewStyle: boolean;
    function GetNumGlyphs: TJvNumGlyphs;
    function GetWordWrap: boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAllowAllUp(Value: boolean);
    procedure SetAllowTimer(Value: boolean);
    procedure SetDown(Value: boolean);
    procedure SetDropDownMenu(Value: TPopupMenu);
    procedure SetFlat(Value: boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGrayNewStyle(const Value: boolean);
    procedure SetGroupIndex(Value: integer);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetInactiveGrayed(Value: boolean);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: integer);
    procedure SetMarkDropDown(Value: boolean);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    procedure SetSpacing(Value: integer);
    procedure SetStyle(Value: TButtonStyle);
    procedure SetTransparent(Value: boolean);
    procedure SetWordWrap(Value: boolean);

    function CheckMenuDropDown(const Pos: TSmallPoint; Manual: boolean): boolean;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GlyphChanged(Sender: TObject);
    procedure TimerExpired(Sender: TObject);
    procedure UpdateExclusive;

    procedure CMButtonPressed(var Msg: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Msg: TMessage); message WM_MOUSEMOVE;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
  protected
    FState: TJvButtonState;
    //Polaris
    FFlatStandard: boolean;
    procedure SetFlatStandard(Value: boolean);
    procedure ActionChange(Sender: TObject; CheckDefaults: boolean); override;
    function GetDropDownMenuPos: TPoint;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure PaintGlyph(Canvas: TCanvas; ARect: TRect; AState: TJvButtonState;
      DrawMark: boolean); virtual;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property ButtonGlyph: Pointer read FGlyph;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ButtonClick;
    function CheckBtnMenuDropDown: boolean;
    procedure Click; override;
    procedure UpdateTracking;
    property MouseInControl: boolean read FMouseInControl;
  published
    property Action;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property AllowAllUp: boolean read FAllowAllUp write SetAllowAllUp default false;
    property AllowTimer: boolean read FAllowTimer write SetAllowTimer default false;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    { Ensure group index is declared before Down }
    property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
    property Down: boolean read FDown write SetDown default false;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Enabled;
    property Flat: boolean read FFlat write SetFlat default false;
    property FlatStandard: boolean read FFlatStandard write SetFlatStandard default false;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GrayedInactive: boolean read FInactiveGrayed write SetInactiveGrayed default true;
    property GrayNewStyle: boolean read GetGrayNewStyle write SetGrayNewStyle default true;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property HotGlyph: TBitmap read FHotGlyph write SetGlyph;
    property HotTrack: boolean read FHotTrack write FHotTrack default false;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property InitPause: Word read FInitRepeatPause write FInitRepeatPause default 500;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: integer read FMargin write SetMargin default -1;
    property MarkDropDown: boolean read FMarkDropDown write SetMarkDropDown default true;
    property MenuPosition: TJvDropDownMenuPos read FMenuPosition write FMenuPosition default dmpBottom;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint default false;
    property RepeatInterval: Word read FRepeatPause write FRepeatPause default 100;
    property ShowHint default true;
    property Spacing: integer read FSpacing write SetSpacing default 1;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Transparent: boolean read FTransparent write SetTransparent default false;
    property Visible;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default false;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  TJvButtonImage = class(TObject)
  private
    FGlyph: TObject;
    FButtonSize: TPoint;
    FCaption: TCaption;
    function GetNumGlyphs: TJvNumGlyphs;
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function GetWordWrap: boolean;
    procedure SetWordWrap(Value: boolean);
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    procedure DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: integer;
      Layout: TButtonLayout; AFont: TFont; Images: TImageList;
      ImageIndex: integer; Flags: Word);
    procedure Draw(Canvas: TCanvas; X, Y, Margin, Spacing: integer;
      Layout: TButtonLayout; AFont: TFont; Flags: Word);
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Caption: TCaption read FCaption write FCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonSize: TPoint read FButtonSize write FButtonSize;
    property WordWrap: boolean read GetWordWrap write SetWordWrap;
  end;

  TJvxButtonGlyph = class
  private
    FAlignment: TAlignment;
    FGlyphList: TImageList;
    FGrayNewStyle: boolean;
    FIndexs: array[TJvButtonState] of integer;
    FNumGlyphs: TJvNumGlyphs;
    FOnChange: TNotifyEvent;
    FOriginal: TBitmap;
    FTransparentColor: TColor;
    FWordWrap: boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGrayNewStyle(const Value: boolean);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    function MapColor(Color: TColor): TColor;
  protected
    procedure MinimizeCaption(Canvas: TCanvas; const Caption: string;
      Buffer: PChar; MaxLen, Width: integer);
    function CreateButtonGlyph(State: TJvButtonState): integer;
    function CreateImageGlyph(State: TJvButtonState; Images: TImageList;
      Index: integer): integer;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      var Caption: string; Layout: TButtonLayout; Margin, Spacing: integer;
      PopupMark: boolean; var GlyphPos: TPoint; var TextBounds: TRect;
      Flags: Word; Images: TImageList; ImageIndex: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    function DrawButtonGlyph(Canvas: TCanvas; X, Y: integer;
      State: TJvButtonState): TPoint;
    function DrawButtonImage(Canvas: TCanvas; X, Y: integer; Images: TImageList;
      ImageIndex: integer; State: TJvButtonState): TPoint;
    function DrawEx(Canvas: TCanvas; const Client: TRect; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: integer; PopupMark: boolean;
      Images: TImageList; ImageIndex: integer; State: TJvButtonState;
      Flags: Word): TRect;
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TJvButtonState; Flags: Word);
    procedure DrawPopupMark(Canvas: TCanvas; X, Y: integer;
      State: TJvButtonState);
    function Draw(Canvas: TCanvas; const Client: TRect; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: integer; PopupMark: boolean;
      State: TJvButtonState; Flags: Word): TRect;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property GrayNewStyle: boolean read FGrayNewStyle write SetGrayNewStyle;
    property NumGlyphs: TJvNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property WordWrap: boolean read FWordWrap write FWordWrap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Forms, SysUtils, Math, ActnList, CommCtrl, JvJVCLUtils;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[boolean] of Word = (0, DT_WORDBREAK);

//=== TJvGlyphList ===========================================================

type
  TJvGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: integer;
    function AllocateIndex: integer;
  public
    constructor CreateSize(AWidth, AHeight: integer);
    destructor Destroy; override;
    function Add(Image, Mask: TBitmap): integer;
    function AddMasked(Image: TBitmap; MaskColor: TColor): integer;
    procedure Delete(Index: integer);
    property Count: integer read FCount;
  end;

constructor TJvGlyphList.CreateSize(AWidth, AHeight: integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

destructor TJvGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TJvGlyphList.AllocateIndex: integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := true;
end;

function TJvGlyphList.Add(Image, Mask: TBitmap): integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

function TJvGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TJvGlyphList.Delete(Index: integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := false;
  end;
end;

//=== TJvGlyphCache ==========================================================

type
  TJvGlyphCache = class(TObject)
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: integer): TJvGlyphList;
    procedure ReturnList(List: TJvGlyphList);
    function Empty: boolean;
  end;

constructor TJvGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TJvGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TJvGlyphCache.GetList(AWidth, AHeight: integer): TJvGlyphList;
var
  I: integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then
        Exit;
  end;
  Result := TJvGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TJvGlyphCache.ReturnList(List: TJvGlyphList);
begin
  if List = nil then
    Exit;
  if List.Count = 0 then
  begin
    FGlyphLists.Remove(List);
    List.Free;
  end;
end;

function TJvGlyphCache.Empty: boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

//=== TJvxButtonGlyph =========================================================

// (rom) changed to var
var
  GlyphCache: TJvGlyphCache = nil;

procedure TJvxButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  var Caption: string; Layout: TButtonLayout; Margin, Spacing: integer;
  PopupMark: boolean; var GlyphPos: TPoint; var TextBounds: TRect; Flags: Word;
  Images: TImageList; ImageIndex: integer);
var
  TextPos: TPoint;
  MaxSize, ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  cString: array[0..255] of char;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);
  if Assigned(Images) and (Images.Width > 0) and (ImageIndex >= 0) and
    (ImageIndex < Images.Count) then
    GlyphSize := Point(Images.Width, Images.Height)
  else if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    MaxSize.X := ClientSize.X - GlyphSize.X;
    if Margin <> -1 then
      Dec(MaxSize.X, Margin);
    if Spacing <> -1 then
      Dec(MaxSize.X, Spacing);
    if PopupMark then
      Dec(MaxSize.X, 9);
    MaxSize.Y := ClientSize.Y;
  end
  else { blGlyphTop, blGlyphBottom }
  begin
    MaxSize.X := ClientSize.X;
    MaxSize.Y := ClientSize.Y - GlyphSize.Y;
    if Margin <> -1 then
      Dec(MaxSize.Y, Margin);
    if Spacing <> -1 then
      Dec(MaxSize.Y, Spacing);
  end;
  MaxSize.X := Max(0, MaxSize.X);
  MaxSize.Y := Max(0, MaxSize.Y);
  MinimizeCaption(Canvas, Caption, cString, sizeof(cString) - 1, MaxSize.X);
  Caption := StrPas(cString);
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, MaxSize.X, 0);
    DrawText(Canvas.Handle, cString, -1, TextBounds, DT_CALCRECT or DT_CENTER or
      DT_VCENTER or WordWraps[FWordWrap] or Flags);
  end
  else
    TextBounds := Rect(0, 0, 0, 0);
  TextBounds.Bottom := Max(TextBounds.Top, TextBounds.Top +
    Min(MaxSize.Y, HeightOf(TextBounds)));
  TextBounds.Right := Max(TextBounds.Left, TextBounds.Left +
    Min(MaxSize.X, WidthOf(TextBounds)));
  TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
    TextBounds.Top);
  if PopupMark then
    if ((GlyphSize.X = 0) or (GlyphSize.Y = 0)) or (Layout = blGlyphLeft) then
      Inc(TextSize.X, 9)
    else if GlyphSize.X > 0 then
      Inc(GlyphSize.X, 6);
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y div 2) - (GlyphSize.Y div 2);
    TextPos.Y := (ClientSize.Y div 2) - (TextSize.Y div 2);
  end
  else
  begin
    GlyphPos.X := (ClientSize.X div 2) - (GlyphSize.X div 2);
    TextPos.X := (ClientSize.X div 2) - (TextSize.X div 2);
  end;
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X div 2) - (TotalSize.X div 2)
      else
        Margin := (ClientSize.Y div 2) - (TotalSize.Y div 2);
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X div 2) - (TextSize.X div 2)
      else
        Spacing := (TotalSize.Y div 2) - (TextSize.Y div 2);
    end;
  end;
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

constructor TJvxButtonGlyph.Create;
var
  I: TJvButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clFuchsia;
  FAlignment := taCenter;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then
    GlyphCache := TJvGlyphCache.Create;
end;

function TJvxButtonGlyph.CreateButtonGlyph(State: TJvButtonState): integer;
var
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight, X, Y: integer;
  IRect, ORect: TRect;
  I: TJvButtonState;
begin
  if (State = rbsDown) and (NumGlyphs < 3) then
    State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then
    Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    I := State;
    if Ord(I) >= NumGlyphs then
      I := rbsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end;
      rbsDisabled:
        if NumGlyphs > 1 then
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end
        else
        begin
          if FGrayNewStyle then
          begin
            MonoBmp := CreateDisabledBitmap_NewStyle(FOriginal, FTransparentColor);
            try
              FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(MonoBmp,
                FTransparentColor);
            finally
              MonoBmp.Free;
            end;
          end
          else
          begin
            MonoBmp := CreateDisabledBitmap(FOriginal, clBlack);
            try
              FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(MonoBmp,
                ColorToRGB(clBtnFace));
            finally
              MonoBmp.Free;
            end;
          end;
        end;
      rbsInactive:
        if NumGlyphs > 4 then
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end
        else
        begin
          with TmpImage do
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(FOriginal.Canvas.Pixels[X, Y]);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            FTransparentColor);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

function TJvxButtonGlyph.CreateImageGlyph(State: TJvButtonState;
  Images: TImageList; Index: integer): integer;
var
  TmpImage, Mask: TBitmap;
  IWidth, IHeight, X, Y: integer;
begin
  if State = rbsDown then
    State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (Images.Width = 0) or (Images.Height = 0) or
    (Images.Count = 0) then
    Exit;
  IWidth := Images.Width;
  IHeight := Images.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          with TmpImage.Canvas do
          begin
            FillRect(Rect(0, 0, IWidth, IHeight));
            ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_NORMAL);
          end;
          Mask := TBitmap.Create;
          try
            with Mask do
            begin
              Monochrome := true;
              Height := IHeight;
              Width := IWidth;
            end;
            with Mask.Canvas do
            begin
              FillRect(Rect(0, 0, IWidth, IHeight));
              ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
            end;
            FIndexs[State] := TJvGlyphList(FGlyphList).Add(TmpImage, Mask);
          finally
            Mask.Free;
          end;
        end;
      rbsDisabled:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
          ImageListDrawDisabled(Images, TmpImage.Canvas, 0, 0, Index,
            clBtnHighlight, clBtnShadow, true);
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
      rbsInactive:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
          ImageList_Draw(Images.Handle, Index, TmpImage.Canvas.Handle, 0, 0,
            ILD_NORMAL);
          with TmpImage do
          begin
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(Canvas.Pixels[X, Y]);
          end;
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
end;

destructor TJvxButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

function TJvxButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: integer;
  PopupMark: boolean; State: TJvButtonState; Flags: Word): TRect;
begin
  Result := DrawEx(Canvas, Client, Caption, Layout, Margin, Spacing,
    PopupMark, nil, -1, State, Flags);
end;

function TJvxButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: integer;
  State: TJvButtonState): TPoint;
var
  Index: integer;
begin
  Result := Point(0, 0);
  if (FOriginal = nil) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then
    Exit;
  Index := CreateButtonGlyph(State);
  if Index >= 0 then
  begin
    ImageList_Draw(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL);
    Result := Point(FGlyphList.Width, FGlyphList.Height);
  end;
end;

function TJvxButtonGlyph.DrawButtonImage(Canvas: TCanvas; X, Y: integer;
  Images: TImageList; ImageIndex: integer; State: TJvButtonState): TPoint;
var
  Index: integer;
begin
  Result := Point(0, 0);
  if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then
    Exit;
  if State = rbsDisabled then
  begin
    ImageListDrawDisabled(Images, Canvas, X, Y, ImageIndex, clBtnHighlight,
      clBtnShadow, true);
  end
  else if State = rbsInactive then
  begin
    Index := CreateImageGlyph(State, Images, ImageIndex);
    if Index >= 0 then
      ImageList_Draw(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL);
  end
  else
    ImageList_Draw(Images.Handle, ImageIndex, Canvas.Handle, X, Y, ILD_NORMAL);
  Result := Point(Images.Width, Images.Height);
end;

procedure TJvxButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TJvButtonState; Flags: Word);
var
  cString: array[0..255] of char;
begin
  Canvas.Brush.Style := bsClear;
  StrPLCopy(cString, Caption, sizeof(cString) - 1);
  Flags := DT_VCENTER or WordWraps[FWordWrap] or Flags;
  if State = rbsDisabled then
  begin
    with Canvas do
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, cString, Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, cString, Length(Caption), TextBounds, Flags);
    end;
  end
  else
    DrawText(Canvas.Handle, cString, -1, TextBounds, Flags);
end;

function TJvxButtonGlyph.DrawEx(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: integer;
  PopupMark: boolean; Images: TImageList; ImageIndex: integer;
  State: TJvButtonState; Flags: Word): TRect;
var
  UseImages: boolean;
  GlyphPos, PopupPos: TPoint;
  TextBounds: TRect;
  CaptionText: string;
begin
  CaptionText := Caption;
  CalcButtonLayout(Canvas, Client, CaptionText, Layout, Margin, Spacing,
    PopupMark, GlyphPos, TextBounds, Flags, Images,
    ImageIndex);
  UseImages := false;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) and
    (Images.Width > 0) then
  begin
    UseImages := true;
    PopupPos := DrawButtonImage(Canvas, GlyphPos.X, GlyphPos.Y, Images,
      ImageIndex, State);
  end
  else
    PopupPos := DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State);
  DrawButtonText(Canvas, CaptionText, TextBounds, State, Flags);
  if PopupMark then
    if (Layout <> blGlyphLeft) and (((FOriginal <> nil) and
      (FOriginal.Width > 0)) or UseImages) then
    begin
      PopupPos.X := GlyphPos.X + PopupPos.X + 1;
      PopupPos.Y := GlyphPos.Y + PopupPos.Y div 2;
      DrawPopupMark(Canvas, PopupPos.X, PopupPos.Y, State);
    end
    else
    begin
      if CaptionText <> '' then
        PopupPos.X := TextBounds.Right + 3
      else
        PopupPos.X := (Client.Left + Client.Right - 7) div 2;
      PopupPos.Y := TextBounds.Top + HeightOf(TextBounds) div 2;
      DrawPopupMark(Canvas, PopupPos.X, PopupPos.Y, State);
    end;
  Result := TextBounds;
end;

procedure TJvxButtonGlyph.DrawPopupMark(Canvas: TCanvas; X, Y: integer;
  State: TJvButtonState);
var
  AColor: TColor;

  procedure DrawMark;
  var
    I: integer;
  begin
    with Canvas do
    begin
      for I := 0 to 6 do
      begin
        Pixels[X + I, Y - 1] := AColor;
        if (I > 0) and (I < 6) then
        begin
          Pixels[X + I, Y] := AColor;
          if (I > 1) and (I < 5) then
            Pixels[X + I, Y + 1] := AColor;
        end;
      end;
      Pixels[X + 3, Y + 2] := AColor;
    end;
  end;

begin
  if State = rbsDisabled then
  begin
    AColor := clBtnHighlight;
    Inc(X, 1);
    Inc(Y, 1);
    DrawMark;
    Dec(X, 1);
    Dec(Y, 1);
    AColor := clBtnShadow;
  end
  else
    AColor := clBtnText;
  DrawMark;
end;

procedure TJvxButtonGlyph.GlyphChanged(Sender: TObject);
var
  Glyphs: integer;
begin
  if Sender = FOriginal then
  begin
    Invalidate;
    if (FOriginal <> nil) and (FOriginal.Height > 0) then
    begin
      FTransparentColor := FOriginal.TransparentColor and not PaletteMask;
      if FOriginal.Width mod FOriginal.Height = 0 then
      begin
        Glyphs := FOriginal.Width div FOriginal.Height;
        if Glyphs > (Ord(High(TJvButtonState)) + 1) then
          Glyphs := 1;
        SetNumGlyphs(Glyphs);
      end;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TJvxButtonGlyph.Invalidate;
var
  I: TJvButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if Assigned(FGlyphList) then
      if FIndexs[I] <> -1 then
        TJvGlyphList(FGlyphList).Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(TJvGlyphList(FGlyphList));
  FGlyphList := nil;
end;

function TJvxButtonGlyph.MapColor(Color: TColor): TColor;
var
  Index: byte;
begin
  if (Color = FTransparentColor) or (ColorToRGB(Color) = ColorToRGB(clBtnFace)) then
    Result := Color
  else
  begin
    Color := ColorToRGB(Color);
    Index := byte(Longint(Word(GetRValue(Color)) * 77 +
      Word(GetGValue(Color)) * 150 + Word(GetBValue(Color)) * 29) shr 8);
    Result := RGB(Index, Index, Index);
  end;
end;

procedure TJvxButtonGlyph.MinimizeCaption(Canvas: TCanvas; const Caption: string;
  Buffer: PChar; MaxLen, Width: integer);
var
  I: integer;
  Lines: TStrings;
begin
  StrPLCopy(Buffer, Caption, MaxLen);
  if FWordWrap then
    Exit;
  Lines := TStringlist.Create;
  try
    Lines.Text := Caption;
    for I := 0 to Lines.Count - 1 do
      Lines[I] := MinimizeText(Lines[I], Canvas, Width);
    StrPLCopy(Buffer, TrimRight(Lines.Text), MaxLen);
  finally
    Lines.Free;
  end;
end;

procedure TJvxButtonGlyph.SetGlyph(Value: TBitmap);
begin
  Invalidate;
  FOriginal.Assign(Value);
end;

procedure TJvxButtonGlyph.SetGrayNewStyle(const Value: boolean);
begin
  if Value <> FGrayNewStyle then
  begin
    Invalidate;
    FGrayNewStyle := Value;
  end;
end;

procedure TJvxButtonGlyph.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

//=== TJvButtonImage =========================================================

// (rom) changed to var
var
  ButtonCount: integer = 0;

  { DrawButtonFrame - returns the remaining usable area inside the Client rect }

function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: boolean; Style: TButtonStyle): TRect;
var
  NewStyle: boolean;
begin
  Result := Client;
  NewStyle := (Style = bsNew) or (NewStyleControls and (Style = bsAutoDetect));
  if IsDown then
  begin
    if NewStyle then
    begin
      //Polaris
      //Frame3D(Canvas, Result,clBtnShadow{ clWindowFrame}, clBtnHighlight, 1);
      //if not IsFlat then
      //  Frame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
      if not IsFlat then
      begin
        Frame3D(Canvas, Result, clWindowFrame, clBtnHighlight, 1);
        Frame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
      end
      else
        Frame3D(Canvas, Result, clBtnShadow, clBtnHighlight, 1);
    end
    else
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clWindowFrame, clBtnHighlight, 1)
          // Frame3D(Canvas, Result, clBtnShadow, clBtnHighlight, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        Canvas.Pen.Color := clBtnShadow;
        Canvas.PolyLine([Point(Result.Left, Result.Bottom - 1),
          Point(Result.Left, Result.Top), Point(Result.Right, Result.Top)]);
      end;
    end;
  end
  else
  begin
    if NewStyle then
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1)
      else
      begin
        Frame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1);
        Frame3D(Canvas, Result, clBtnFace, clBtnShadow, 1);
      end;
    end
    else
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        Frame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1);
      end;
    end;
  end;
  InflateRect(Result, -1, -1);
end;

constructor TJvButtonImage.Create;
begin
  FGlyph := TJvxButtonGlyph.Create;
  NumGlyphs := 1;
  FButtonSize := Point(24, 23);
end;

destructor TJvButtonImage.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvButtonImage.Invalidate;
begin
  TJvxButtonGlyph(FGlyph).Invalidate;
end;

function TJvButtonImage.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := TJvxButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TJvButtonImage.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  TJvxButtonGlyph(FGlyph).NumGlyphs := Value;
end;

function TJvButtonImage.GetWordWrap: boolean;
begin
  Result := TJvxButtonGlyph(FGlyph).WordWrap;
end;

procedure TJvButtonImage.SetWordWrap(Value: boolean);
begin
  TJvxButtonGlyph(FGlyph).WordWrap := Value;
end;

function TJvButtonImage.GetGlyph: TBitmap;
begin
  Result := TJvxButtonGlyph(FGlyph).Glyph;
end;

procedure TJvButtonImage.SetGlyph(Value: TBitmap);
begin
  TJvxButtonGlyph(FGlyph).Glyph := Value;
end;

function TJvButtonImage.GetAlignment: TAlignment;
begin
  Result := TJvxButtonGlyph(FGlyph).Alignment;
end;

procedure TJvButtonImage.SetAlignment(Value: TAlignment);
begin
  TJvxButtonGlyph(FGlyph).Alignment := Value;
end;

procedure TJvButtonImage.Draw(Canvas: TCanvas; X, Y, Margin, Spacing: integer;
  Layout: TButtonLayout; AFont: TFont; Flags: Word);
begin
  DrawEx(Canvas, X, Y, Margin, Spacing, Layout, AFont, nil, -1, Flags);
end;

procedure TJvButtonImage.DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: integer;
  Layout: TButtonLayout; AFont: TFont; Images: TImageList; ImageIndex: integer;
  Flags: Word);
var
  Target: TRect;
  SaveColor: integer;
  SaveFont: TFont;
begin
  SaveColor := Canvas.Brush.Color;
  SaveFont := TFont.Create;
  SaveFont.Assign(Canvas.Font);
  try
    Target := Bounds(X, Y, FButtonSize.X, FButtonSize.Y);
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Target);
    Frame3D(Canvas, Target, clBtnShadow, clWindowFrame, 1);
    Frame3D(Canvas, Target, clBtnHighlight, clBtnShadow, 1);
    if AFont <> nil then
      Canvas.Font := AFont;
    TJvxButtonGlyph(FGlyph).DrawEx(Canvas, Target, Caption, Layout, Margin,
      Spacing, false, Images, ImageIndex, rbsUp, Flags);
  finally
    Canvas.Font.Assign(SaveFont);
    SaveFont.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

//=== TJvSpeedButton ========================================================

procedure TJvSpeedButton.ActionChange(Sender: TObject;
  CheckDefaults: boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
      TransparentColor := clFuchsia;
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (not CheckDefaults or (Self.Down = false)) and (FGroupIndex <> 0) then
        Self.Down := Checked;
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

procedure TJvSpeedButton.ButtonClick;
var
  FirstTickCount, Now: Longint;
begin
  if FMenuTracking or (not Enabled) or (Assigned(FDropDownMenu) and
    DropDownMenu.AutoPopup) then
    Exit;
  if not FDown then
  begin
    FState := rbsDown;
    Repaint;
  end;
  try
    FirstTickCount := GetTickCount;
    repeat
      Now := GetTickCount;
    until (Now - FirstTickCount >= 20) or (Now < FirstTickCount);
    if FGroupIndex = 0 then
      Click;
  finally
    FState := rbsUp;
    if FGroupIndex = 0 then
      Repaint
    else
    begin
      SetDown(not FDown);
      Click;
    end;
  end;
end;

function TJvSpeedButton.CheckBtnMenuDropDown: boolean;
begin
  Result := CheckMenuDropDown(PointToSmallPoint(GetDropDownMenuPos), true);
end;

function TJvSpeedButton.CheckMenuDropDown(const Pos: TSmallPoint;
  Manual: boolean): boolean;
var
  Form: TCustomForm;
begin
  Result := false;
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FDropDownMenu) and (DropDownMenu.AutoPopup or Manual) then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.SendCancelMode(nil);
    DropDownMenu.PopupComponent := Self;
    with ClientToScreen(SmallPointToPoint(Pos)) do
      DropDownMenu.Popup(X, Y);
    Result := true;
  end;
end;

procedure TJvSpeedButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited Click;
end;

procedure TJvSpeedButton.CMButtonPressed(var Msg: TMessage);
var
  Sender: TControl;
begin
  if (Msg.wParam = FGroupIndex) and Parent.HandleAllocated then
  begin
    Sender := TControl(Msg.lParam);
    if (Sender <> nil) and (Sender is TJvSpeedButton) then
      if Sender <> Self then
      begin
        if TJvSpeedButton(Sender).Down and FDown then
        begin
          FDown := false;
          FState := rbsUp;
          Repaint;
        end;
        FAllowAllUp := TJvSpeedButton(Sender).AllowAllUp;
      end;
  end;
end;

procedure TJvSpeedButton.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TJvSpeedButton.CMEnabledChanged(var Msg: TMessage);
var
  State: TJvButtonState;
begin
  inherited;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  TJvxButtonGlyph(FGlyph).CreateButtonGlyph(State);
  UpdateTracking;
  Repaint;
end;

procedure TJvSpeedButton.CMFontChanged(var Msg: TMessage);
begin
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
  Invalidate;
end;

procedure TJvSpeedButton.CMMouseEnter(var Msg: TMessage);
{$IFDEF JVCLThemesEnabled}
var
  NeedRepaint: boolean;
{$ENDIF}
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if not FHotGlyph.Empty then
    begin
      FOldGlyph.Assign(Glyph);
      Glyph.Assign(FHotGlyph);
    end;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FOver := true;
  end;

{$IFDEF JVCLThemesEnabled}
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := FFlat and not FMouseInControl and Enabled and
    (DragMode <> dmAutomatic) and (GetCapture = 0);

  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint or ThemeServices.ThemesEnabled) and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := true;
    if Enabled then
      Repaint;
  end;
{$ELSE}
  if not FMouseInControl and Enabled and IsForegroundTask then
  begin
    FMouseInControl := true;
    if FFlat then
      Repaint;
    MouseEnter;
  end;
{$ENDIF JVCLThemesEnabled}
end;

procedure TJvSpeedButton.CMMouseLeave(var Msg: TMessage);
{$IFDEF JVCLThemesEnabled}
var
  NeedRepaint: boolean;
{$ENDIF}
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if not FOldGlyph.Empty then
      Glyph.Assign(FOldGlyph);
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := false;
  end;
{$IFDEF JVCLThemesEnabled}
  NeedRepaint := FFlat and FMouseInControl and Enabled and not FDragging;
  { Windows XP introduced hot states also for non-flat buttons. }
  if NeedRepaint or ThemeServices.ThemesEnabled then
  begin
    FMouseInControl := false;
    if Enabled then
      Repaint;
  end;
{$ELSE}
  if FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := false;
    if FFlat then
      Invalidate;
    MouseLeave;
  end;
{$ENDIF JVCLThemesEnabled}
end;

procedure TJvSpeedButton.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvSpeedButton.CMSysColorChange(var Msg: TMessage);
begin
  TJvxButtonGlyph(FGlyph).Invalidate;
  Invalidate;
end;

procedure TJvSpeedButton.CMTextChanged(var Msg: TMessage);
begin
  Invalidate;
end;

procedure TJvSpeedButton.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Visible then
    UpdateTracking;
end;

constructor TJvSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := false;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  FOver := false;
  FHotGlyph := TBitmap.Create;
  FOldGlyph := TBitmap.Create;
  FFlatStandard := false;
  SetBounds(0, 0, 25, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
  FInactiveGrayed := true;
  FDrawImage := TBitmap.Create;
  FGlyph := TJvxButtonGlyph.Create;
  TJvxButtonGlyph(FGlyph).OnChange := GlyphChanged;
  TJvxButtonGlyph(FGlyph).GrayNewStyle := true;
  ParentFont := true;
  ParentShowHint := false;
  ShowHint := true;
  FSpacing := 1;
  FMargin := -1;
  FInitRepeatPause := 500;
  FRepeatPause := 100;
  FStyle := bsAutoDetect;
  FLayout := blGlyphTop;
  FMarkDropDown := true;
  Inc(ButtonCount);
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvSpeedButton.Destroy;
begin
  TJvxButtonGlyph(FGlyph).Free;
  Dec(ButtonCount);
  FDrawImage.Free;
  FDrawImage := nil;
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  FHotGlyph.Free;
  FOldGlyph.Free;
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited Destroy;
end;

procedure TJvSpeedButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  DoClick: boolean;
begin
  if FDragging and (Button = mbLeft) then
  begin
    FDragging := false;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      FState := rbsUp;
      FMouseInControl := false;
      if DoClick and not (FState in [rbsExclusive, rbsDown]) then
        Repaint
      else
        Invalidate;
    end
    else if DoClick then
    begin
      SetDown(not FDown);
      if FDown then
        Repaint;
    end
    else
    begin
      if FDown then
        FState := rbsExclusive;
      Repaint;
    end;
    if DoClick and not FMenuTracking then
      Click;
  end;
  UpdateTracking;
end;

function TJvSpeedButton.GetAlignment: TAlignment;
begin
  Result := TJvxButtonGlyph(FGlyph).Alignment;
end;

function TJvSpeedButton.GetDropDownMenuPos: TPoint;
begin
  if Assigned(FDropDownMenu) then
  begin
    if MenuPosition = dmpBottom then
    begin
      case FDropDownMenu.Alignment of
        paLeft:
          Result := Point(-1, Height);
        paRight:
          Result := Point(Width + 1, Height);
      else {paCenter}
        Result := Point(Width div 2, Height);
      end;
    end
    else { dmpRight }
    begin
      case FDropDownMenu.Alignment of
        paLeft:
          Result := Point(Width, -1);
        paRight:
          Result := Point(-1, -1);
      else {paCenter}
        Result := Point(Width div 2, Height);
      end;
    end;
  end
  else
    Result := Point(0, 0);
end;

function TJvSpeedButton.GetGlyph: TBitmap;
begin
  Result := TJvxButtonGlyph(FGlyph).Glyph;
end;

function TJvSpeedButton.GetGrayNewStyle: boolean;
begin
  Result := TJvxButtonGlyph(FGlyph).GrayNewStyle;
end;

function TJvSpeedButton.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := TJvxButtonGlyph(FGlyph).NumGlyphs;
end;

function TJvSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TJvSpeedButton.GetWordWrap: boolean;
begin
  Result := TJvxButtonGlyph(FGlyph).WordWrap;
end;

procedure TJvSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvSpeedButton.Loaded;
var
  State: TJvButtonState;
begin
  inherited Loaded;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  TJvxButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TJvSpeedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  P: TPoint;
  Msg: TMsg;
begin
  if FMenuTracking then
    Exit;
  inherited MouseDown(Button, Shift, X, Y);
  if (not FMouseInControl) and Enabled then
  begin
    FMouseInControl := true;
    Repaint;
  end;
  if (Button = mbLeft) and Enabled {and not (ssDouble in Shift)} then
  begin
    if not FDown then
    begin
      FState := rbsDown;
      Repaint;
    end;
    FDragging := true;
    FMenuTracking := true;
    try
      P := GetDropDownMenuPos;
      if CheckMenuDropDown(PointToSmallPoint(P), false) then
        DoMouseUp(Button, Shift, X, Y);
      if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
      begin
        if (Msg.Message = WM_LBUTTONDOWN) or (Msg.Message = WM_LBUTTONDBLCLK) then
        begin
          P := ScreenToClient(Msg.Pt);
          if (P.X >= 0) and (P.X < ClientWidth) and (P.Y >= 0)
            and (P.Y <= ClientHeight) then
            KillMessage(HWND_DESKTOP, Msg.Message);
        end;
      end;
    finally
      FMenuTracking := false;
    end;
    if FAllowTimer then
    begin
      if FRepeatTimer = nil then
        FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.Interval := InitPause;
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Enabled := true;
    end;
  end;
end;

procedure TJvSpeedButton.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvSpeedButton.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvSpeedButton.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NewState: TJvButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then
      NewState := rbsUp
    else
      NewState := rbsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := rbsExclusive
      else
        NewState := rbsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end;

procedure TJvSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := false;
end;

procedure TJvSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = DropDownMenu) and (Operation = opRemove) then
    DropDownMenu := nil;
end;

procedure TJvSpeedButton.Paint;
var
  PaintRect: TRect;
  AState: TJvButtonState;
{$IFDEF JVCLThemesEnabled}
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if not Enabled {and not (csDesigning in ComponentState)} then
  begin
    FState := rbsDisabled;
    FDragging := false;
  end
  else if FState = rbsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := rbsExclusive
    else
      FState := rbsUp;
  AState := FState;

  if FFlat and not FMouseInControl and not (csDesigning in ComponentState) then
    AState := rbsInactive;

{$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    PerformEraseBackground(Self, Canvas.Handle);

    if not Enabled then
      Button := tbPushButtonDisabled
    else if AState in [rbsDown, rbsExclusive] then
      Button := tbPushButtonPressed
    else if FMouseInControl then
      Button := tbPushButtonHot
    else
      Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if FFlat then
    begin
      case Button of
        tbPushButtonDisabled:
          ToolButton := ttbButtonDisabled;
        tbPushButtonPressed:
          ToolButton := ttbButtonPressed;
        tbPushButtonHot:
          ToolButton := ttbButtonHot;
        tbPushButtonNormal:
          ToolButton := ttbButtonNormal;
      end;
    end;

    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then
    begin
      InflateRect(PaintRect, 1, 1);
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
    end
    else
    begin
      Details := ThemeServices.GetElementDetails(ToolButton);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
    end;

    if Button = tbPushButtonPressed then
    begin
      // A pressed speed button has a white text. This applies however only to flat buttons.
      if ToolButton <> ttbToolbarDontCare then
        Canvas.Font.Color := clHighlightText;
      OffsetRect(PaintRect, 1, 0);
    end;
    PaintGlyph({FDrawImage.}Canvas, PaintRect, AState, FMarkDropDown and
      Assigned(FDropDownMenu));
  end
  else
{$ENDIF JVCLThemesEnabled}
  begin
    PaintRect := Rect(0, 0, Width, Height);
    FDrawImage.Width := Self.Width;
    FDrawImage.Height := Self.Height;
    with FDrawImage.Canvas do
    begin
      Font := Self.Font;
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(PaintRect);
      if FTransparent then
        CopyParentImage(Self, FDrawImage.Canvas);
      if (AState <> rbsInactive) or (FState = rbsExclusive) then
        PaintRect := DrawButtonFrame(FDrawImage.Canvas, PaintRect,
          FState in [rbsDown, rbsExclusive], FFlat, FStyle)
      else if FFlat then
        InflateRect(PaintRect, -2, -2);
    end;
    if (FState = rbsExclusive) and not Transparent and
      (not FFlat or (AState = rbsInactive)) then
    begin
      FDrawImage.Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      InflateRect(PaintRect, 1, 1);
      FDrawImage.Canvas.FillRect(PaintRect);
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [rbsDown, rbsExclusive] then
      OffsetRect(PaintRect, 1, 1);
    if (FState = rbsDisabled) or not FInactiveGrayed then
      AState := FState;
    PaintGlyph(FDrawImage.Canvas, PaintRect, AState, FMarkDropDown and
      Assigned(FDropDownMenu));
    Canvas.Draw(0, 0, FDrawImage);
  end;
end;

procedure TJvSpeedButton.PaintGlyph(Canvas: TCanvas; ARect: TRect;
  AState: TJvButtonState; DrawMark: boolean);
begin
  if FFlatStandard and (AState = rbsInactive) then
    AState := rbsExclusive; // Polaris
  TJvxButtonGlyph(FGlyph).Draw(Canvas, ARect, Caption, FLayout,
    FMargin, FSpacing, DrawMark, AState, DrawTextBiDiModeFlags(Alignments[Alignment]));
end;

procedure TJvSpeedButton.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    TJvxButtonGlyph(FGlyph).Alignment := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetAllowAllUp(Value: boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TJvSpeedButton.SetAllowTimer(Value: boolean);
begin
  FAllowTimer := Value;
  if not FAllowTimer and (FRepeatTimer <> nil) then
  begin
    FRepeatTimer.Enabled := false;
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TJvSpeedButton.SetDown(Value: boolean);
begin
  if FGroupIndex = 0 then
    Value := false;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then
      Exit;
    FDown := Value;
    if Value then
    begin
      if FState = rbsUp then
        Invalidate;
      FState := rbsExclusive;
    end
    else
    begin
      FState := rbsUp;
    end;
    Repaint;
    if Value then
      UpdateExclusive;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetDropDownMenu(Value: TPopupMenu);
begin
  FDropDownMenu := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if FMarkDropDown then
    Invalidate;
end;

procedure TJvSpeedButton.SetFlat(Value: boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetFlatStandard(Value: boolean);
begin
  { Polaris }
  if FFlatStandard <> Value then
  begin
    FFlatStandard := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetGlyph(Value: TBitmap);
begin
  TJvxButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

procedure TJvSpeedButton.SetGrayNewStyle(const Value: boolean);
begin
  if GrayNewStyle <> Value then
  begin
    TJvxButtonGlyph(FGlyph).GrayNewStyle := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetGroupIndex(Value: integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvSpeedButton.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvSpeedButton.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvSpeedButton.SetInactiveGrayed(Value: boolean);
begin
  if Value <> FInactiveGrayed then
  begin
    FInactiveGrayed := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetMargin(Value: integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetMarkDropDown(Value: boolean);
begin
  if Value <> FMarkDropDown then
  begin
    FMarkDropDown := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > Ord(High(TJvButtonState)) + 1 then
    Value := Ord(High(TJvButtonState)) + 1;
  if Value <> TJvxButtonGlyph(FGlyph).NumGlyphs then
  begin
    TJvxButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetSpacing(Value: integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetStyle(Value: TButtonStyle);
begin
  if Style <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetTransparent(Value: boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.SetWordWrap(Value: boolean);
begin
  if Value <> WordWrap then
  begin
    TJvxButtonGlyph(FGlyph).WordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatInterval;
  if (FState = rbsDown) and MouseCapture then
  try
    Click;
  except
    FRepeatTimer.Enabled := false;
    raise;
  end;
end;

procedure TJvSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_JVBUTTONPRESSED;
    Msg.wParam := FGroupIndex;
    Msg.lParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvSpeedButton.UpdateTracking;
var
  P: TPoint;
  OldValue: boolean;
begin
  OldValue := FMouseInControl;
  GetCursorPos(P);
  FMouseInControl := Enabled and (FindDragTarget(P, true) = Self) and
    IsForegroundTask;
  if FMouseInControl <> OldValue then
    if FMouseInControl then
    begin
      if Flat then
        Repaint;
      MouseEnter;
    end
    else
    begin
      if Flat then
        Invalidate;
      MouseLeave;
    end;
end;

procedure TJvSpeedButton.WMLButtonDblClk(var Msg: TWMLButtonDown);
begin
  if not FMenuTracking then
  begin
    inherited;
    if FDown then
      DblClick;
  end;
end;

procedure TJvSpeedButton.WMMouseMove(var Msg: TMessage);
begin
  inherited;
end;

procedure TJvSpeedButton.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvSpeedButton.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

end.

