{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S?stien Buysse [sbuysse att buypin dott com]
Portions created by S?stien Buysse are Copyright (C) 2001 S?stien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Thornqvist [peter3 at sourceforge dot net]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Changes:
2005-07-20:(dejoy)
  * TJvCustomLabel implemented interface of IJvHotTrack.
2005-04-02:
  * Fixed (Added) support for Alignment when used with Angle. (Layout still to do)
  * Fixed Shadow (was not visible when JvLabel not Transparent).
  * Fixed RoundedFrame (was not visible when not Transparent).
2004-04-05:
  * Add property RoundedFrame in TJvCustomLabel (Integer>0 is the radius corner)
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

  Contributor(s):
    Dierk schmid
    Stephane Bischoff (Tief)
    Dejoy Den

Known Issues:
* AutoSize calculations aren't correct when RoundedFrame and/or Shadow are active
-----------------------------------------------------------------------------}
// $Id$

unit JvLabel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Types, Classes, Graphics, Controls, StdCtrls, ImgList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvTypes, JvComponent, JvDataProvider, JvExControls, JvHotTrackPersistent;

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);
  TJvLabelRotateAngle = -360..360;
  TJvTextEllipsis = (teNone, teWordEllipsis, tePathEllipsis, teEndEllipsis);

  TAngleInfo = record
    TextWidth: Integer;
    TextHeight: Integer;
    TextGapWidth: Integer;
    TextGapHeight: Integer;
    TotalWidth: Integer;
    TotalHeight: Integer;
    PosX: Integer;
    PosY: Integer;
  end;

  TJvLabelHotTrackOptions = TJvHotTrackOptions;

  TJvCustomLabel = class(TJvGraphicControl, IJvHotTrack)
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
    FHotTrackFontOptions: TJvTrackFontOptions;
    FHotTrackOptions: TJvLabelHotTrackOptions;

    FAutoOpenURL: Boolean;
    FURL: string;
    FAngle: TJvLabelRotateAngle;
    FSpacing: Integer;
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
    procedure SetSpacing(Value: Integer);
    procedure SetTextEllipsis(Value: TJvTextEllipsis);
    procedure SetFrameColor(const Value: TColor);
    procedure SetRoundedFrame(const Value: Integer);
    function GetMargin: Integer;
    procedure HotFontChanged(Sender: TObject);

    {IJvHotTrack}  //added by dejoy 2005-07-20
    function GetHotTrack: Boolean;
    function GetHotTrackFont: TFont;
    function GetHotTrackFontOptions: TJvTrackFontOptions;
    function GetHotTrackOptions: TJvLabelHotTrackOptions;
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure SetHotTrackOptions(Value: TJvLabelHotTrackOptions);
    procedure IJvHotTrack_Assign(Source: IJvHotTrack);
    procedure IJvHotTrack.Assign = IJvHotTrack_Assign;
    function IsHotTrackFontStored: Boolean;
  protected
    procedure DoDrawCaption(var Rect: TRect; Flags: Integer); virtual;
    procedure DoProviderDraw(var Rect: TRect; Flags: Integer); virtual;
    procedure FocusChanged(AControl: TWinControl); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; override;
    procedure EnabledChanged; override;

    procedure DoDrawText(var Rect: TRect; Flags: Integer); virtual;
    procedure AdjustBounds; virtual;
    procedure SetAutoSize(Value: Boolean); override;

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
    property AutoOpenURL: Boolean read FAutoOpenURL write FAutoOpenURL default True;

    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont stored IsHotTrackFontStored;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property HotTrackOptions: TJvLabelHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;

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
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvLabel = class(TJvCustomLabel)
  published
    property Action;
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
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
    property HotTrackOptions;
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

function CalculateAlignment(Alignment: TAlignment; Angle: Integer; X, Y: Real;
  Info: TAngleInfo): TPoint;
procedure CalculateAngleInfo(Canvas: TCanvas; Angle: Integer; Text: string;
  Rect: TRect; var Info: TAngleInfo; AutoSize: Boolean = True;
  Alignment: TAlignment = taLeftJustify);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, Math, Forms,
  JvDataProviderIntf, JvConsts, JvThemes, JvJCLUtils, JvJVCLUtils;

const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);

procedure FrameRounded(Canvas: TCanvas; ARect: TRect; AColor: TColor; R: Integer);
begin
  // Draw Frame with round corners
  Canvas.Pen.Color := AColor;
  Dec(ARect.Right);
  Dec(ARect.Bottom);
  Canvas.Polygon(
   [Point(ARect.Left  + R, ARect.Top       ),
    Point(ARect.Right - R, ARect.Top       ),
    Point(ARect.Right    , ARect.Top    + R),
    Point(ARect.Right    , ARect.Bottom - R),
    Point(ARect.Right - R, ARect.Bottom    ),
    Point(ARect.Left  + R, ARect.Bottom    ),
    Point(ARect.Left     , ARect.Bottom - R),
    Point(ARect.Left     , ARect.Top    + R),
    Point(ARect.Left  + R, ARect.Top       )]);
  Inc(ARect.Right);
  Inc(ARect.Bottom);
end;

function CalculateAlignment(Alignment: TAlignment; Angle: Integer; X, Y: Real; Info: TAngleInfo): TPoint;
begin
  with Info do
    case Angle of
      0..89:
        case Alignment of
          taLeftJustify:
            Result := Point(0, Round(Y + (TotalHeight - 2 * TextGapHeight) / 2));
          taCenter:
            Result := Point(Round(X - TotalWidth / 2), Round(Y + (TotalHeight - 2 * TextGapHeight) / 2));
          taRightJustify:
            Result := Point(Round(X * 2 - TotalWidth), Round(Y + (TotalHeight - 2 * TextGapHeight) / 2));
        end;
      90..179:
        case Alignment of
          taLeftJustify:
            Result := Point(TextWidth, Round(Y + TotalHeight / 2));
          taCenter:
            Result := Point(Round(X + (TotalWidth - 2 * TextGapWidth) / 2), Round(Y + TotalHeight / 2));
          taRightJustify:
            Result := Point(Round(X * 2 - TextGapWidth), Round(Y + TotalHeight / 2));
        end;
      180..269:
        case Alignment of
          taLeftJustify:
            Result := Point(TotalWidth, Round(Y - (TotalHeight - 2 * TextGapHeight) / 2));
          taCenter:
            Result := Point(Round(X + TotalWidth / 2), Round(Y - (TotalHeight - 2 * TextGapHeight) / 2));
          taRightJustify:
            Result := Point(Round(X * 2), Round(Y - (TotalHeight - 2 * TextGapHeight) / 2));
        end;
      else
        case Alignment of
          taLeftJustify:
            Result := Point(TextGapWidth, Round(Y - TotalHeight / 2));
          taCenter:
            Result := Point(Round(X - (TotalWidth - 2 * TextGapWidth) / 2), Round(Y - TotalHeight / 2));
          taRightJustify:
            Result := Point(Round(X * 2 - TextWidth), Round(Y - TotalHeight / 2));
        end;
    end;
end;

procedure CalculateAngleInfo(Canvas: TCanvas; Angle: Integer; Text: string;
  Rect: TRect; var Info: TAngleInfo; AutoSize: Boolean = True;
  Alignment: TAlignment = taLeftJustify);
var
  TxtWdt, TxtHgt: Extended;
  AngleB, X, Y: Real;
  Origin: TPoint;
begin
  // Calculate intermediate values
  case Angle of
    0..89:
      AngleB := DegToRad(90 - Angle);
    90..179:
      AngleB := DegToRad(Angle - 90);
    180..269:
      AngleB := DegToRad(270 - Angle);
  else {270..359}
    AngleB := DegToRad(Angle - 270);
  end;
  with Canvas do
  begin
    TxtWdt := TextWidth(Text);
    TxtHgt := TextHeight(Text);
  end;
  with Info do
  begin
    TextWidth := Round(Sin(AngleB) * TxtWdt);
    TextGapWidth := Round(Cos(AngleB) * TxtHgt);
    TextHeight := Round(Cos(AngleB) * TxtWdt);
    TextGapHeight := Round(Sin(AngleB) * TxtHgt);
    // Calculate new sizes of component
    TotalWidth := (TextWidth + TextGapWidth);
    TotalHeight := (TextHeight + TextGapHeight);
  end;
  // Calculate draw position of text
  X := (Rect.Right - Rect.Left) / 2;
  Y := (Rect.Bottom - Rect.Top) / 2;
  // Calculate Layout and Alignment Position
  //SetTextAlign(Canvas.Handle, TA_LEFT);
  Origin := CalculateAlignment(Alignment, Angle, X, Y, Info);
  if AutoSize then
  begin
    case Angle of
      0..89:
        begin
          Info.PosX := 0;
          Info.PosY := Info.TextHeight;
        end;
      90..179:
        begin
          Info.PosX := Info.TextWidth;
          Info.PosY := Info.TotalHeight;
        end;
      180..269:
        begin
          Info.posX := Info.TotalWidth;
          Info.posY := Info.TextGapHeight;
        end;
    else{270..359}
      Info.PosX := Info.TextGapWidth;
      Info.PosY := 0;
    end;
  end
  else
  begin
    Info.PosX := Origin.X;
    Info.PosY := Origin.Y;
  end;
end;

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
  Canvas.Brush.Style := bsClear;
  Result := DrawText(Canvas, Str, Count, RShadow, Format);
  if Result > 0 then
    Inc(Result, ShadowSize);
  SetTextColor(Canvas.Handle, Color);
  DrawText(Canvas, Str, Count, RText, Format);
  UnionRect(Rect, RText, RShadow);
end;

//=== { TJvCustomLabel } =====================================================

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
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
    ControlStyle := ControlStyle - [csOpaque];
  {$ENDIF JVCLThemesEnabled}

  FHotTrack := False;
  // (rom) needs better font handling
  FHotTrackFont := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FHotTrackOptions := TJvLabelHotTrackOptions.Create(Self);
  // (rom) needs better font handling
  FHotTrackFont.OnChange := HotFontChanged;

  Width := 65;
  Height := 17;
  FAutoSize := True;
  FSpacing := 4;
  FShowAccelChar := True;
  FShadowColor := clBtnHighlight;
  FShadowSize := 0;
  FShadowPos := spRightBottom;
  FAutoOpenURL := True;
end;

destructor TJvCustomLabel.Destroy;
begin
  FChangeLink.Free;
  FHotTrackFont.Free;
  FHotTrackOptions.Free;
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
        Result := ItemText.Text
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
      if MouseOver then
        Canvas.Font := HotTrackFont
      else
        Canvas.Font := Font;
      if (Flags and DT_CALCRECT) <> 0 then
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
  EllipsisFlags: array [TJvTextEllipsis] of Integer =
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
  if not MouseOver or not HotTrack then
  begin
    Canvas.Font := Font;
    Canvas.Font.Color := GetDefaultFontColor;
  end
  else
  begin
    if HotTrack then
      if MouseOver then
        Canvas.Font := HotTrackFont
  end;
  PosShadow := FShadowPos;
  SizeShadow := FShadowSize;
  ColorShadow := FShadowColor;
  if not Enabled then
  begin
    if FShadowSize = 0 then
    begin
      PosShadow := spRightBottom;
      SizeShadow := 1;
    end;
    Canvas.Font.Color := clGrayText;
    ColorShadow := clBtnHighlight;
  end;
  if IsValidImage then
    Inc(Rect.Left, GetImageWidth + Spacing);
    if Angle <> 0 then
      DrawAngleText(Rect, Flags, IsValidImage, SizeShadow, ColorToRGB(ColorShadow), PosShadow)
    else
      DrawShadowText(Canvas, PChar(Text), Length(Text), Rect, Flags,
        SizeShadow, ColorToRGB(ColorShadow), PosShadow);
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
    Images.Draw(Canvas, X, Y, ImageIndex,  Enabled);
  end;
end;

procedure TJvCustomLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  if ProviderActive then
    DoProviderDraw(Rect, Flags)
  else
    DoDrawCaption(Rect, Flags);
end;

procedure TJvCustomLabel.DrawAngleText(var Rect: TRect; Flags: Word; HasImage: Boolean;
  ShadowSize: Byte; ShadowColor: TColorRef; ShadowPos: TShadowPosition);
var
  Text: array [0..4096] of Char;
  LogFont, NewLogFont: TLogFont;
  NewFont: HFont;
  TextX, TextY, ShadowX, ShadowY: Integer;
  Angle10: Integer;
  W, H: Integer;
  Info: TAngleInfo;
  CalcRect: Boolean;
begin
  Angle10 := Angle * 10;
  CalcRect := (Flags and DT_CALCRECT <> 0);
  StrLCopy(@Text, PChar(GetLabelCaption), Length(Text) - 1);
  if CalcRect and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');
  if MouseOver then
    Canvas.Font := HotTrackFont
  else
    Canvas.Font := Font;
  if GetObject(Font.Handle, SizeOf(TLogFont), @LogFont) = 0 then
    RaiseLastOSError;
  NewLogFont := LogFont;
  NewLogFont.lfEscapement := Angle10;
  NewLogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  NewFont := CreateFontIndirect(NewLogFont);
  {
    (p3) unnecessary
    OldFont := SelectObject(Canvas.Font.Handle, NewFont);
    DeleteObject(OldFont);
    ...this does the same thing:
  }
  Canvas.Font.Handle := NewFont;
  Canvas.Brush.Style := bsClear; // Do not Erase Shadow or Background

  CalculateAngleInfo(Canvas, Angle, Text, ClientRect, Info, AutoSize, Alignment);
  W := Info.TotalWidth;
  H := Info.TotalHeight;
  TextX := Info.posX;
  TextY := Info.posY;

  if CalcRect then
  begin
    Rect.Right := Rect.Left + W;
    Rect.Bottom := Rect.Top + H;
    if HasImage then
      Inc(Rect.Right, Images.Width);
    Inc(Rect.Right, MarginLeft + MarginRight);
    Inc(Rect.Bottom, MarginTop + MarginBottom);
  end
  else
  begin
    if HasImage then
    begin
      case Alignment of
        taLeftJustify:
          Inc(TextX, Images.Width);
        taCenter:
          Inc(TextX, Images.Width div 2);
        taRightJustify:
          Inc(TextX, 0);
      end;
    end;
    Inc(TextX, MarginLeft);
    Inc(TextY, MarginTop);
    if ShadowSize > 0 then
    begin
      ShadowX := TextX;
      ShadowY := TextY;
      case ShadowPos of
        spLeftTop:
          begin
            Dec(ShadowX, ShadowSize);
            Dec(ShadowY, ShadowSize);
          end;
        spRightBottom:
          begin
            Inc(ShadowX, ShadowSize);
            Inc(ShadowY, ShadowSize);
          end;
        spLeftBottom:
          begin
            Dec(ShadowX, ShadowSize);
            Inc(ShadowY, ShadowSize);
          end;
        spRightTop:
          begin
            Inc(ShadowX, ShadowSize);
            Dec(ShadowY, ShadowSize);
          end;
      end;
      Canvas.Font.Color := ShadowColor;
      Canvas.TextOut(ShadowX, ShadowY, Text);
    end;
    Canvas.Font.Color := Self.Font.Color;
    if not Enabled then
    begin
      Canvas.Font.Color := clBtnHighlight;
      Canvas.TextOut(TextX + 1, TextY + 1, Text);
      Canvas.Font.Color := clBtnShadow;
    end;
    Canvas.TextOut(TextX, TextY, Text);
  end;
end;




procedure TJvCustomLabel.Paint;
var
  Rect,CalcRect: TRect;
  DrawStyle: Integer;
  InteriorMargin: Integer;
  OldPenColor: TColor;
begin
  InteriorMargin := 0;
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;

  with Canvas do
  begin
    Rect := ClientRect;

    {Inserted by (dejoy) 2005-07-20}
    if Enabled and MouseOver and HotTrack  then
    begin
      if HotTrackOptions.Enabled then
      begin
        Canvas.Brush.Color := HotTrackOptions.Color;
        Canvas.Brush.Style := bsSolid;
        if HotTrackOptions.FrameVisible then
        begin
          OldPenColor := Pen.Color;
          if RoundedFrame = 0 then
          begin
            Canvas.Pen.Color := HotTrackOptions.FrameColor;
            Canvas.Rectangle(0, 0, Width, Height);
          end
          else
          begin
            if not Transparent then // clx: TODO
              FloodFill(ClientRect.Left + 1, ClientRect.Top + RoundedFrame, HotTrackOptions.FrameColor, fsBorder);
            FrameRounded(Canvas, ClientRect, HotTrackOptions.FrameColor, RoundedFrame);
          end;
          Canvas.Pen.Color := OldPenColor;
        end
        else
          Canvas.FillRect(Rect);
      end;
    end
    else
    begin
      Canvas.Font := Self.Font;
    {Insert End by (dejoy)}

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
          FrameRect( ClientRect);
        end
        else
        begin
          Brush.Color := Color;
          if not Transparent then // clx: TODO
            FloodFill(ClientRect.Left + 1, ClientRect.Top + RoundedFrame, FrameColor, fsBorder);
          FrameRounded(Canvas, ClientRect, FrameColor, RoundedFrame);
        end;
      end;
    end;

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
      if FLayout = tlBottom then
        OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else
        OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
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

procedure TJvCustomLabel.HotFontChanged(Sender: TObject);
begin
  if MouseOver then
    Invalidate;
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
  inherited SetAutoSize(Value);
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
  ReplaceComponentReference(Self, Value, TComponent(FFocusControl));
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
    {$IFDEF JVCLThemesEnabled}
    if StyleServices.Enabled then
      Value := True; // themes aware Labels are always transparent
    {$ENDIF JVCLThemesEnabled}
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
  OldValue, OtherDragging: Boolean;
begin
  OldValue := MouseOver;
  OtherDragging := KeyPressed(VK_LBUTTON) or Mouse.IsDragging;

  MouseOver := Enabled and not OtherDragging and
    (FindDragTarget(Mouse.CursorPos, True) = Self) and IsForegroundTask;
  if MouseOver <> OldValue then
    Invalidate;
end;

procedure TJvCustomLabel.FocusChanged(AControl: TWinControl);
var
  Active: Boolean;
begin
  Active := Assigned(FFocusControl) and (AControl = FFocusControl);
  if FFocused <> Active then
  begin
    FFocused := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited FocusChanged(AControl);
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

function TJvCustomLabel.WantKey(Key: Integer; Shift: TShiftState): Boolean;
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

procedure TJvCustomLabel.MouseEnter(Control: TControl);
var
  NeedRepaint: Boolean;
  OtherDragging:Boolean;
begin
  if csDesigning in ComponentState then
    Exit;

  if not MouseOver and Enabled and IsForegroundTask then
  begin
    OtherDragging := KeyPressed(VK_LBUTTON) or Mouse.IsDragging;
    NeedRepaint := not Transparent and
      (
      {$IFDEF JVCLThemesEnabled}
      StyleServices.Enabled or
      {$ENDIF JVCLThemesEnabled}
      (FHotTrack and not (FDragging or OtherDragging)));

    UpdateTracking; // set MouseOver

    inherited MouseEnter(Control);

    if NeedRepaint then
      Invalidate;
  end;
end;

procedure TJvCustomLabel.MouseLeave(Control: TControl);
var
  NeedRepaint: Boolean;
  OtherDragging: Boolean;
begin
  if csDesigning in ComponentState then
    Exit;
  if  MouseOver and Enabled then
  begin
    OtherDragging := KeyPressed(VK_LBUTTON) or Mouse.IsDragging;

    NeedRepaint := not Transparent and
      (
      {$IFDEF JVCLThemesEnabled}
      StyleServices.Enabled or
      {$ENDIF JVCLThemesEnabled}
      (FHotTrack and (FDragging or not OtherDragging)));

    UpdateTracking; // set MouseOver

    inherited MouseLeave(Control);

    if NeedRepaint then
      Invalidate;
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
    ReplaceImageListReference(Self, Value, FImages, FChangeLink);
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

procedure TJvCustomLabel.SetHotTrackFontOptions(Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

function TJvCustomLabel.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJvCustomLabel.GetHotTrackFont: TFont;
begin
  Result := FHotTrackFont;
end;

function TJvCustomLabel.GetHotTrackFontOptions: TJvTrackFontOptions;
begin
  Result := FHotTrackFontOptions;
end;

function TJvCustomLabel.GetHotTrackOptions: TJvLabelHotTrackOptions;
begin
  Result := FHotTrackOptions;
end;

procedure TJvCustomLabel.SetHotTrack(Value: Boolean);
begin
  FHotTrack := Value;
end;

procedure TJvCustomLabel.SetHotTrackOptions(Value: TJvLabelHotTrackOptions);
begin
  if (FHotTrackOptions <> Value) and (Value <> nil) then
    FHotTrackOptions.Assign(Value);
end;

procedure TJvCustomLabel.IJvHotTrack_Assign(
  Source: IJvHotTrack);
begin
  if (Source <> nil) and (IJvHotTrack(Self) <> Source) then
  begin
    HotTrack := Source.HotTrack;
    HotTrackFont :=Source.HotTrackFont;
    HotTrackFontOptions := Source.HotTrackFontOptions;
    HotTrackOptions := Source.HotTrackOptions;
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
    if (Value < Height div 2) and (Value >= 0) then
    begin
      FRoundedFrame := Value;
      Invalidate;
    end;
end;

function TJvCustomLabel.IsHotTrackFontStored: Boolean;
begin
  Result := IsHotTrackFontDfmStored(HotTrackFont, Font, HotTrackFontOptions);
end;

function TJvCustomLabel.IsValidImage: Boolean;
begin
  Result := (Images <> nil) and (ImageIndex >= 0);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

