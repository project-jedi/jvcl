{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Thornqvist [peter3 at sourceforge dot net]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Changes:
2003-10-19:
  * Moved TJvCustomLabel from JvxCtrls to this unit
2003-09-13:
  * Turned TJvCustomLabel into a consumer.
    Notes: * angled labels will simply use the current item's Text to render and ignore any provider
             specified rendering implementations.
           * D5 users: when changing a property that might clear out the provider (Caption,
             ImageIndex and Image) you can run into Access Violations if the Provider property is
             collapsed. This is due to a limitation in D5 property editors and can not be solved.
2003-08-17:
  * Implementation moved to TJvCustomLabel. TJvLabel now only publishes properties and events.
2003-03-24:
  * JvHotLink merged into JvLabel:
    To simulate JvHotlink, set AutoOpenURL to True, modify HotTrackFont to fit and assign
    a URL (or file-path) to the URL property.
  * JvAngleLabel merged into JvLabel: set Angle > 0 and font to a TrueTrype font to rotate the text // peter3

  Contributor(s): dierk schmid
  //dierk 2004-5-04
  --add property RoundedFrame in TJvCustomLabel (Integer>0 is the radius corner)

Known Issues:
* AutoSize calculations aren't correct when RoundedFrame and/or Shadow are active
-----------------------------------------------------------------------------}
// $Id$

unit JvQLabel;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, Classes, QGraphics, QControls, QStdCtrls, QImgList, 
  JvQTypes, JvQComponent, JvQDataProvider;

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);
  TJvLabelRotateAngle = -360..360;
  TJvTextEllipsis = (teNone, teWordEllipsis, tePathEllipsis, teEndEllipsis);

  TJvCustomLabel = class(TJvGraphicControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FShadowColor: TColor;
    FShadowSize: Byte;
    FShadowPos: TShadowPosition;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FShowFocus: Boolean;
    FFocused: Boolean;
    FDragging: Boolean;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FChangeLink: TChangeLink;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FAutoOpenURL: Boolean;
    FURL: string;
    FAngle: TJvLabelRotateAngle;
    FSpacing: Integer;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FConsumerSvc: TJvDataConsumer;
    FNeedsResize: Boolean;
    FTextEllipsis: TJvTextEllipsis;
    FFrameColor: TColor;
    FRoundedFrame: Integer; // DS
    FMarginLeft: Integer;
    FMarginTop: Integer;
    FMarginRight: Integer;
    FMarginBottom: Integer;
    function GetTransparent: Boolean;
    procedure UpdateTracking;
    procedure SetAlignment(Value: TAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetLayout(Value: TTextLayout);
    procedure SetMargin(Value: Integer);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowSize(Value: Byte);
    procedure SetShadowPos(Value: TShadowPosition);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure SetShowFocus(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImages(Value: TCustomImageList);
    procedure DoImagesChange(Sender: TObject);
    procedure DrawAngleText(var Rect: TRect; Flags: Word; HasImage: Boolean;
      ShadowSize: Byte; ShadowColor: TColorRef; ShadowPos: TShadowPosition);
    procedure SetAngle(Value: TJvLabelRotateAngle);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetSpacing(Value: Integer);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetTextEllipsis(Value: TJvTextEllipsis);
    procedure SetFrameColor(const Value: TColor);
    procedure SetRoundedFrame(const Value: Integer);
    function GetMargin: Integer;
  protected
    procedure DoDrawCaption(var Rect: TRect; Flags: Integer);virtual;
    procedure DoProviderDraw(var Rect: TRect; Flags: Integer);virtual;
    procedure DoFocusChanged(Control: TWinControl); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure EnabledChanged; override;
    procedure VisibleChanged; override;

    procedure DoDrawText(var Rect: TRect; Flags: Integer); virtual;
    procedure AdjustBounds;  
    procedure SetAutoSize(Value: Boolean); virtual; 

    // MarginXxx do not update the control.
    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    property MarginTop: Integer read FMarginTop write FMarginTop;
    property MarginRight: Integer read FMarginRight write FMarginRight;
    property MarginBottom: Integer read FMarginBottom write FMarginBottom;

    function GetDefaultFontColor: TColor; virtual;
    function GetLabelCaption: string; virtual;
    function IsValidImage: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    function GetImageWidth: Integer; virtual;
    function GetImageHeight: Integer; virtual;
    procedure SetConsumerService(Value: TJvDataConsumer);
    function ProviderActive: Boolean;
    procedure ConsumerServiceChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
    procedure NonProviderChange;
    property Angle: TJvLabelRotateAngle read FAngle write SetAngle default 0;
    property AutoOpenURL: Boolean read FAutoOpenURL write FAutoOpenURL;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property NeedsResize: Boolean read FNeedsResize write FNeedsResize;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clNone;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property TextEllipsis: TJvTextEllipsis read FTextEllipsis write SetTextEllipsis default teNone;
    // specifies the offset between the right edge of the image and the left edge of the text (in pixels)
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property Margin: Integer read GetMargin write SetMargin default 0;
    property RoundedFrame: Integer read FRoundedFrame write SetRoundedFrame default 0; //DS
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnHighlight;
    property ShadowSize: Byte read FShadowSize write SetShadowSize default 0;
    property ShadowPos: TShadowPosition read FShadowPos write SetShadowPos default spRightBottom;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property URL: string read FURL write FURL;
    property Provider: TJvDataConsumer read FConsumerSvc write SetConsumerService;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property MouseOver;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  end;

  TJvLabel = class(TJvCustomLabel)
  published
    property Action;
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color; 
    property DragMode;
    property Enabled;
    property FocusControl;
    property FrameColor;
    property Font;
    property Anchors;
    property Constraints;
    property Layout;
    property Margin;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RoundedFrame;
    property ShadowColor;
    property ShadowSize;
    property ShadowPos;
    property ShowAccelChar;
    property ShowFocus;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDrag;
    property OnContextPopup;

    property Angle;
    property AutoOpenURL;
    property HintColor;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property Images;
    property ImageIndex;
    property Provider;
    property Spacing;
    property TextEllipsis;
    property URL;
    property OnParentColorChange;
  end;

function DrawShadowText(Canvas: TCanvas; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;

procedure FrameRounded(Canvas: TCanvas; ARect: TRect; AColor: TColor; R: Integer);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Math, QForms,
  JvQDataProviderIntf,
  JvQConsts, JvQThemes, JvQJCLUtils, JvQJVCLUtils;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

//=== { TJvCustomLabel } =====================================================

function DrawShadowText(Canvas: TCanvas; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;
var
  RText, RShadow: TRect;
  Color: TColorRef;
begin
  RText := Rect;
  RShadow := Rect;
  Color := SetTextColor(Canvas.Handle, ShadowColor);
  case ShadowPos of
    spLeftTop:
      OffsetRect(RShadow, -ShadowSize, -ShadowSize);
    spRightBottom:
      OffsetRect(RShadow, ShadowSize, ShadowSize);
    spLeftBottom:
      begin
        {OffsetRect(RText, ShadowSize, 0);}
        OffsetRect(RShadow, -ShadowSize, ShadowSize);
      end;
    spRightTop:
      begin
        {OffsetRect(RText, 0, ShadowSize);}
        OffsetRect(RShadow, ShadowSize, -ShadowSize);
      end;
  end;
  Result := DrawText(Canvas, Str, Count, RShadow, Format);
  if Result > 0 then
    Inc(Result, ShadowSize);
  SetTextColor(Canvas.Handle, Color);
  DrawText(Canvas, Str, Count, RText, Format);
  UnionRect(Rect, RText, RShadow);
end;

constructor TJvCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrameColor := clNone;
  FImageIndex := -1;
  FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RendersSingleItem]);
  FConsumerSvc.OnChanged := ConsumerServiceChanged;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable]; 
  FHotTrack := False;
  // (rom) needs better font handling
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FSpacing := 4;
  FShowAccelChar := True;
  FShadowColor := clBtnHighlight;
  FShadowSize := 0;
  FShadowPos := spRightBottom;
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvCustomLabel.Destroy;
begin
  FChangeLink.Free;
  FHotTrackFont.Free;
  FFontSave.Free;
  FreeAndNil(FConsumerSvc);
  inherited Destroy;
end;

function TJvCustomLabel.GetLabelCaption: string;
var
  ItemText: IJvDataItemText;
begin
  if ProviderActive then
  begin
    Provider.Enter;
    try
      if Supports((Provider as IJvDataConsumerItemSelect).GetItem, IJvDataItemText, ItemText) then
        Result := ItemText.Caption
      else
        Result := Caption;
    finally
      Provider.Leave;
    end;
  end
  else
    Result := Caption;
end;

function TJvCustomLabel.GetDefaultFontColor: TColor;
begin
  Result := Font.Color;
end;

procedure TJvCustomLabel.DoProviderDraw(var Rect: TRect; Flags: Integer);
var
  Tmp: TSize;
  TmpItem: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
  DrawState: TProviderDrawStates;
begin
  Provider.Enter;
  try
    if not Enabled then
      DrawState := [pdsDisabled]
    else
      DrawState := [];
    TmpItem := (Provider as IJvDataConsumerItemSelect).GetItem;
    if (TmpItem <> nil) and (Supports(TmpItem.GetItems, IJvDataItemsRenderer, ItemsRenderer) or
      Supports(TmpItem, IJvDataItemRenderer, ItemRenderer)) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Font := Font;
      if (Flags and DT_CALCRECT <> 0) then
      begin
        if ItemsRenderer <> nil then
          Tmp := ItemsRenderer.MeasureItem(Canvas, TmpItem)
        else
          Tmp := ItemRenderer.Measure(Canvas);
        Rect.Right := Tmp.cx;
        Rect.Bottom := Tmp.cy;
      end
      else
      begin
        if ItemsRenderer <> nil then
          ItemsRenderer.DrawItem(Canvas, Rect, TmpItem, DrawState)
        else
          ItemRenderer.Draw(Canvas, Rect, DrawState);
      end;
    end
    else
      DoDrawCaption(Rect, Flags);
  finally
    Provider.Leave;
  end;
end;

procedure TJvCustomLabel.DoDrawCaption(var Rect: TRect; Flags: Integer);
const
  EllipsisFlags: array[TJvTextEllipsis] of Integer =
  (0, DT_WORD_ELLIPSIS, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS);
var
  Text: string;
  PosShadow: TShadowPosition;
  SizeShadow: Byte;
  ColorShadow: TColor;
  X, Y: Integer;
begin
  Text := GetLabelCaption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + ' ';
  if not FShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  Flags := Flags or EllipsisFlags[TextEllipsis];
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  Canvas.Font.Color := GetDefaultFontColor;
  PosShadow := FShadowPos;
  SizeShadow := FShadowSize;
  ColorShadow := FShadowColor;
  if not Enabled then
  begin
    if (FShadowSize = 0) and NewStyleControls then
    begin
      PosShadow := spRightBottom;
      SizeShadow := 1;
    end;
    Canvas.Font.Color := clGrayText;
    ColorShadow := clBtnHighlight;
  end;
  if IsValidImage then
    Inc(Rect.Left, GetImageWidth + Spacing); 
  Canvas.Start;
  RequiredState(Canvas, [csHandleValid, csFontValid]);
  try 
    if Angle <> 0 then
      DrawAngleText(Rect, Flags, IsValidImage, SizeShadow, ColorToRGB(ColorShadow), PosShadow)
    else
      DrawShadowText(Canvas, PChar(Text), Length(Text), Rect, Flags,
        SizeShadow, ColorToRGB(ColorShadow), PosShadow); 
  finally
    Canvas.Stop;
  end; 
  // (p3) draw image here since it can potentionally change background and font color
  if IsValidImage and (Flags and DT_CALCRECT = 0) then
  begin
    X := MarginLeft;
    case Layout of
      tlTop:
        Y := MarginTop;
      tlBottom:
        Y := Height - Images.Height - MarginBottom;
    else
      Y := (Height - Images.Height) div 2;
    end;
    if Y < MarginTop then
      Y := MarginTop;
    Images.Draw(Canvas, X, Y, ImageIndex,itImage, Enabled);
  end;
end;

procedure TJvCustomLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  if ProviderActive then
    DoProviderDraw(Rect, Flags)
  else
    DoDrawCaption(Rect, Flags);
end;




//
// TODO: replace TextOutAngle by DrawText(...., Angle) (asn)
//
procedure TJvCustomLabel.DrawAngleText(var Rect: TRect; Flags: Word; HasImage: Boolean;
  ShadowSize: Byte; ShadowColor: TColorRef; ShadowPos: TShadowPosition);
const // (ahuser) no function known for these
  XOffsetFrame = 0;
  YOffsetFrame = 0;
var
  Text: array[0..4096] of Char;
  TextX, TextY: Integer;
  Phi: Real;
  w, h: Integer;
  CalcRect: Boolean;
begin
  CalcRect := (Flags and DT_CALCRECT <> 0);
  StrLCopy(@Text, PChar(GetLabelCaption), SizeOf(Text) - 1);
  if CalcRect and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');

  Canvas.Start;
  try
    Phi := Angle * Pi / 180;
    if not AutoSize then
    begin
      w := Rect.Right - Rect.Left;
      h := Rect.Bottom - Rect.Top;
      TextX := Trunc(0.5 * w - 0.5 * Canvas.TextWidth(Text) * Cos(Phi) - 0.5 * Canvas.TextHeight(Text) *
        Sin(Phi));
      TextY := Trunc(0.5 * h - 0.5 * Canvas.TextHeight(Text) * Cos(Phi) + 0.5 * Canvas.TextWidth(Text) *
        Sin(Phi));
    end
    else
    begin
      w := 4 + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)) + Canvas.TextHeight(Text) * Abs(Sin(Phi)));
      h := 4 + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)) + Canvas.TextWidth(Text) * Abs(Sin(Phi)));
      TextX := 3;
      TextY := 3;
      if Angle <= 90 then
      begin
        TextX := TextX + Trunc(Canvas.TextHeight(Text) * Sin(Phi) / 2);
        TextY := TextY + Trunc(Canvas.TextWidth(Text) * Sin(Phi) + Canvas.TextHeight(Text) * Cos(Phi) / 2);
      end
      else
      if Angle >= 270 then
        TextX := 3 - Trunc(Canvas.TextHeight(Text) * Sin(Phi) / 2)
      else
      if Angle <= 180 then
      begin
        TextX := ClientWidth - 3 - Trunc(Canvas.TextHeight(Text) * Sin(Phi) / 2);
        TextY := ClientHeight - 3 + Ceil(Canvas.TextHeight(Text) * Cos(Phi));
      end
      else // (180 - 270)
      begin
        TextX := ClientWidth - 3 + Ceil(Canvas.TextHeight(Text) * Sin(Phi) / 2);
        TextY := TextY + Ceil(Canvas.TextHeight(Text) * Cos(Phi));
      end;
    end;

    if CalcRect then
    begin
      Rect.Right := Rect.Left + w;
      Rect.Bottom := Rect.Top + h;
      if HasImage then
        Inc(Rect.Right, Images.Width);
      InflateRect(Rect, -XOffsetFrame, -YOffsetFrame);
    end
    else
    begin
      if HasImage then
        Inc(TextX, Images.Width);
      Inc(TextX, XOffsetFrame);
      Inc(TextY, YOffsetFrame);

      if not Enabled then
      begin
        Canvas.Font.Color := clBtnHighlight;
        TextOutAngle(Canvas, Angle, TextX + 1, TextY + 1, Text);
        Canvas.Font.Color := clBtnShadow;
        TextOutAngle(Canvas, Angle, TextX, TextY, Text);
      end
      else
        TextOutAngle(Canvas, Angle, TextX, TextY, Text);
    end;
  finally
    Canvas.Stop;
  end;
end;


procedure TJvCustomLabel.Paint;
var
  Rect,CalcRect: TRect;
  DrawStyle: Integer;
  InteriorMargin: Integer;
begin
  InteriorMargin := 0;
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  with Canvas do
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    if not Transparent and ((RoundedFrame = 0) or (FrameColor = clNone)) then
      DrawThemedBackground(Self, Canvas, ClientRect)
    else
    if Transparent then
      Canvas.Brush.Style := bsClear;
    if FrameColor <> clNone then
    begin
      if RoundedFrame = 0 then
      begin
        Brush.Color := FrameColor;
        FrameRect( Canvas,  ClientRect);
      end
      else
      begin
        Brush.Color := Color;
        FrameRounded(Canvas, ClientRect, FrameColor, RoundedFrame); 
      end;
    end;
    Rect := ClientRect;
    Inc(Rect.Left, MarginLeft + InteriorMargin);
    Dec(Rect.Right, MarginRight + InteriorMargin);
    Inc(Rect.Top, MarginTop + InteriorMargin);
    Dec(Rect.Bottom, MarginBottom + InteriorMargin);
    InflateRect(Rect, -1, 0);
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or
      Alignments[FAlignment];
    { Calculate vertical layout }
    if FLayout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    Rect.Left := MarginLeft;
    Rect.Right := Rect.Right - MarginRight;
    DoDrawText(Rect, DrawStyle);
    if FShowFocus and Assigned(FFocusControl) and FFocused and
      not (csDesigning in ComponentState) then
    begin
      InflateRect(Rect, 1, 0);
      Brush.Color := Self.Color;
      DrawFocusRect(Rect);
    end;
//    if Angle = 0 then
//      AdjustBounds;
  end;
end;

procedure TJvCustomLabel.Loaded;
begin
  inherited Loaded;
  Provider.Loaded;
  FNeedsResize := True;
  AdjustBounds;
end;

procedure TJvCustomLabel.AdjustBounds;
var
  DC: HDC;
  X: Integer;
  Rect, R: TRect;
  AAlignment: TAlignment;
begin
  if not (csReading in ComponentState) and AutoSize and FNeedsResize then
  begin
    Rect := ClientRect;
    InflateRect(Rect, -1, 0);
    DC := GetDC(NullHandle);
    Canvas.Handle := DC; 
    Canvas.Start(False);
    try 
      if Angle = 0 then
      begin
        R := Rect;
        Inc(Rect.Left, MarginLeft);
        Inc(Rect.Top, MarginTop);
        Dec(Rect.Right, MarginRight);
        Dec(Rect.Bottom, MarginBottom);
        //InflateRect(Rect, -Margin, -Margin);

        DoDrawText(Rect, DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);

        Dec(Rect.Left, MarginLeft);
        Dec(Rect.Top, MarginTop);
        Inc(Rect.Right, MarginRight);
        Inc(Rect.Bottom, MarginBottom);
        //InflateRect(Rect, Margin, Margin);

        Inc(Rect.Bottom, MarginTop);
      end
      else
        DrawAngleText(Rect, DT_CALCRECT or DT_EXPANDTABS or DT_WORDBREAK or Alignments[Alignment], IsValidImage, 0, 0, spLeftTop); 
    finally
      Canvas.Stop;
    end; 
    Canvas.Handle := NullHandle;
    ReleaseDC(NullHandle, DC);
    InflateRect(Rect, 1, 0);
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);
    if IsValidImage then
    begin
      Rect.Bottom := Max(Rect.Bottom, Rect.Top + GetImageHeight);
//      Inc(Rect.Right, Spacing);
    end;
    if (AAlignment = taRightJustify) and not IsValidImage then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
  FNeedsResize := False;
end;

procedure TJvCustomLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetAutoSize(Value: Boolean);
begin 
  FAutoSize := Value;
  FNeedsResize := FAutoSize;
  AdjustBounds;
end;

procedure TJvCustomLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

function TJvCustomLabel.GetMargin: Integer;
begin
  Result := FMarginLeft;
end;

procedure TJvCustomLabel.SetMargin(Value: Integer);
begin
  Value := Max(Value, 0);
  if Margin <> Value then
  begin
    MarginLeft := Value;
    MarginTop := Value;
    MarginRight := Value;
    MarginBottom := Value;

    FNeedsResize := True;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShadowSize(Value: Byte);
begin
  if Value <> FShadowSize then
  begin
    FShadowSize := Value;
    FNeedsResize := True;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShadowPos(Value: TShadowPosition);
begin
  if Value <> FShadowPos then
  begin
    FShadowPos := Value;
    Invalidate;
  end;
end;

function TJvCustomLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TJvCustomLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if FShowFocus then
    Invalidate;
end;

procedure TJvCustomLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin 
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    FNeedsResize := True;
    AdjustBounds;
  end;
end;

procedure TJvCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FFocusControl then
      FocusControl := nil;
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TJvCustomLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
    FDragging := True;
  if Button = mbRight then // (ahuser) moved from WMRButtonDown
    UpdateTracking;
end;

procedure TJvCustomLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging and (Button = mbLeft) then
    FDragging := False;
  UpdateTracking;
end;

procedure TJvCustomLabel.UpdateTracking;
var
  P: TPoint;
  OldValue: Boolean;
begin
  OldValue := MouseOver;
  GetCursorPos(P);
  MouseOver := Enabled and (FindDragTarget(P, True) = Self) and
    IsForegroundTask;
  if MouseOver <> OldValue then
    if MouseOver then
      MouseEnter(Self)
    else
      MouseLeave(Self);
end;

procedure TJvCustomLabel.DoFocusChanged(Control: TWinControl);
var
  Active: Boolean;
begin
  Active := Assigned(FFocusControl) and (Control = FFocusControl);
  if FFocused <> Active then
  begin
    FFocused := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited DoFocusChanged(Control);
end;

procedure TJvCustomLabel.TextChanged;
begin
  inherited TextChanged;
  NonProviderChange;
  Invalidate;
  FNeedsResize := True;
  AdjustBounds;
end;

procedure TJvCustomLabel.FontChanged;
begin
  inherited FontChanged;
  FNeedsResize := True;
  AdjustBounds;
  UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
end;

function TJvCustomLabel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Key, GetLabelCaption) and (ssAlt in Shift);
  if Result then
    if FFocusControl.CanFocus then
      FFocusControl.SetFocus;
end;

procedure TJvCustomLabel.EnabledChanged;
begin
  inherited EnabledChanged;
  UpdateTracking;
end;

procedure TJvCustomLabel.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible then
    UpdateTracking;
end;

procedure TJvCustomLabel.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver and Enabled and IsForegroundTask then
  begin
    if HotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvCustomLabel.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if HotTrack then
      Font.Assign(FFontSave);
    inherited MouseLeave(Control);
  end;
end;

procedure TJvCustomLabel.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    if IsValidImage then
      NonProviderChange;
    FNeedsResize := True;
    FImageIndex := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    NonProviderChange;
    if FImages <> nil then
    begin
      FImages.RemoveFreeNotification(Self);
      FImages.UnRegisterChanges(FChangeLink);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FChangeLink);
    end;
    if AutoSize then
    begin
      FNeedsResize := True;
      AdjustBounds;
    end
    else
      Invalidate;
  end;
end;

function TJvCustomLabel.GetImageHeight: Integer;
begin
  Result := 0;
  if not ProviderActive and IsValidImage then
    Result := Images.Height;
end;

procedure TJvCustomLabel.SetConsumerService(Value: TJvDataConsumer);
begin
end;

function TJvCustomLabel.ProviderActive: Boolean;
begin
  Result := (Provider <> nil) and (Provider.ProviderIntf <> nil);
end;

procedure TJvCustomLabel.ConsumerServiceChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
  if ProviderActive or (Reason = ccrProviderSelect) then
  begin
    FNeedsResize := True;
    AdjustBounds;
  end;
end;

procedure TJvCustomLabel.NonProviderChange;
begin
  { TODO 3 -oJVCL -cPROVIDER : Causes AV at designtime when trying to change Images property }
  if ProviderActive then
    Provider.Provider := nil;
end;

function TJvCustomLabel.GetImageWidth: Integer;
begin
  Result := 0;
  if not ProviderActive and IsValidImage then
    Result := Images.Width;
end;

procedure TJvCustomLabel.SetHotTrackFont(Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvCustomLabel.Click;
var
  HasBeenHandled: Boolean;
  TmpItem: IJvDataItem;
  ItemHandler: IJvDataItemBasicAction;
begin
  HasBeenHandled := False;
  if ProviderActive then
  begin
    Provider.Enter;
    try
      TmpItem := (Provider as IJvDataConsumerItemSelect).GetItem;
      if (TmpItem <> nil) and Supports(TmpItem, IJvDataItemBasicAction, ItemHandler) then
        HasBeenHandled := ItemHandler.Execute(Self);
    finally
      Provider.Leave;
    end;
  end;
  if not HasBeenHandled then
  begin
    inherited Click;
    if AutoOpenURL and (URL <> '') then
      OpenObject(URL);
  end;
end;

procedure TJvCustomLabel.SetAngle(Value: TJvLabelRotateAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    if FAngle < 0 then
      Inc(FAngle, 360);
    FNeedsResize := AutoSize;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomLabel.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    if AutoSize then
    begin
      FNeedsResize := True;
      AdjustBounds;
    end
    else
      Invalidate;
  end;
end;

procedure TJvCustomLabel.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvCustomLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FNeedsResize := (ALeft <> Left) or (ATop <> Top) or (AWidth <> Width) or (AHeight <> Height);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TJvCustomLabel.SetTextEllipsis(Value: TJvTextEllipsis);
begin
  if Value <> FTextEllipsis then
  begin
    FTextEllipsis := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetRoundedFrame(const Value: Integer);
begin
  if FRoundedFrame <> Value then
  begin
    // no negative and too hight value
    if (Value < Height div 2) and (Value >= 0) then
    begin
      FRoundedFrame := Value;
      Invalidate;
    end;
  end;
end;

procedure FrameRounded(Canvas: TCanvas; ARect: TRect; AColor: TColor; R: Integer);
begin
  // Draw Frame with round corners
  with Canvas, ARect do
  begin
    Pen.Color := AColor;
    Dec(Right);
    Dec(Bottom);
    Polygon([
      Point(Left + R, Top),
        Point(Right - R, Top),
        Point(Right, Top + R),
        Point(Right, Bottom - R),
        Point(Right - R, Bottom),
        Point(Left + R, Bottom),
        Point(Left, Bottom - R),
        Point(Left, Top + R),
        Point(Left + R, Top)
        ]);
    Inc(Right);
    Inc(Bottom);
  end;
end;

function TJvCustomLabel.IsValidImage: Boolean;
begin
  Result := (Images <> nil) and (ImageIndex >= 0);
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

