{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thornqvist [peter3@peter3.com]

Last Modified: 2003-10-19

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
    To simulate JvHotlink, set AutoOpenURL to true, modify HotTrackFont to fit and assign
    a URL (or file-path) to the URL property.
  * JvAngleLabel merged into JvLabel: set Angle > 0 and font to a TrueTrype font to rotate the text // peter3
Known Issues:
* Images are only displayed in TJvCustomLabel if Angle = 0.
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ImgList,
  JVCLVer, JvTypes, JvComponent, JvConsts, JvDataProvider, JvDataProviderIntf;

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);
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
    FMouseInControl: Boolean;
    FDragging: Boolean;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FAboutJVCL: TJVCLAboutInfo;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FChangeLink:TChangeLink;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHintColor: TColor;
    FHintSaved: TColor;
    FAutoOpenURL: boolean;
    FURL: string;
    FAngle: TJvLabelRotateAngle;
    FSpacing: integer;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FConsumerSvc: TJvDataConsumer;
    function GetTransparent: Boolean;
    procedure UpdateTracking;
    procedure SetAlignment(Value: TAlignment);
    {$IFNDEF COMPILER6_UP} // Polaris
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF}
    procedure SetFocusControl(Value: TWinControl);
    procedure SetLayout(Value: TTextLayout);
    procedure SetLeftMargin(Value: Integer);
    procedure SetRightMargin(Value: Integer);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowSize(Value: Byte);
    procedure SetShadowPos(Value: TShadowPosition);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure SetShowFocus(Value: Boolean);
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender:TObject);
    procedure DrawAngleText(Flags: Word);
    procedure SetAngle(const Value: TJvLabelRotateAngle);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetSpacing(const Value: integer);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    procedure DoDrawCaption(var Rect: TRect; Flags: Word); virtual;
    procedure DoDrawText(var Rect: TRect; Flags: Word); virtual;
    procedure AdjustBounds;
    {$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF}
    function GetDefaultFontColor: TColor; virtual;
    function GetLabelCaption: string; virtual;
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
    function GetImageWidth:integer;virtual;
    function GetImageHeight:integer;virtual;
    procedure SetConsumerService(Value: TJvDataConsumer);
    function ProviderActive: Boolean;
    procedure ConsumerServiceChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
    procedure NonProviderChange;
    property Angle: TJvLabelRotateAngle read FAngle write SetAngle default 0;
    property AutoOpenURL: boolean read FAutoOpenURL write FAutoOpenURL;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions:TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Images:TCustomImageList read FImages write SetImages;
    property ImageIndex:TImageIndex read FImageIndex write SetImageIndex;
    // specifies the offset between the right edge of the image and the left edge of the text (in pixels)
    property Spacing:integer read FSpacing write SetSpacing default 4;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 0;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 0;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnHighlight;
    property ShadowSize: Byte read FShadowSize write SetShadowSize default 0;
    property ShadowPos: TShadowPosition read FShadowPos write SetShadowPos default spLeftTop;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property URL: string read FURL write FURL;
    property Provider: TJvDataConsumer read FConsumerSvc write SetConsumerService;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvLabel = class(TJvCustomLabel)
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
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
    property OnEndDock;
    property OnStartDock;

    property AboutJVCL;
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
    property URL;
    property OnParentColorChange;
  end;

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;

implementation
uses
  JvThemes, JvJCLUtils, JvJVCLUtils, Math;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

//=== TJvCustomLabel =========================================================

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;
var
  RText, RShadow: TRect;
  Color: TColorRef;
begin
  RText := Rect;
  RShadow := Rect;
  Color := SetTextColor(DC, ShadowColor);
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
  Result := DrawText(DC, Str, Count, RShadow, Format);
  if Result > 0 then
    Inc(Result, ShadowSize);
  SetTextColor(DC, Color);
  DrawText(DC, Str, Count, RText, Format);
  UnionRect(Rect, RText, RShadow);
end;

constructor TJvCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RendersSingleItem]);
  FConsumerSvc.OnChanged := ConsumerServiceChanged;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
{$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
    ControlStyle := ControlStyle - [csOpaque];
{$ENDIF}
  FHotTrack := False;
  // (rom) needs better font handling
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FSpacing := 4;
  FShowAccelChar := True;
  FShadowColor := clBtnHighlight;
  FShadowSize := 0;
  FShadowPos := spLeftTop;
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvCustomLabel.Destroy;
begin
  FChangeLink.Free;
  FHotTrackFont.Free;
  FFontSave.Free;
  FreeAndNil(FConsumerSvc);
  inherited;
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

procedure TJvCustomLabel.DoDrawCaption(var Rect: TRect; Flags: Word);
var
  Text: string;
  PosShadow: TShadowPosition;
  SizeShadow: Byte;
  ColorShadow: TColor;
begin
  Text := GetLabelCaption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + ' ';
  if not FShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
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
  if Images <> nil then
  begin
    Inc(Rect.Left, GetImageWidth + Spacing);
    if Flags and DT_CALCRECT = 0 then
      Images.Draw(Canvas, 0,0,ImageIndex);
  end;
  DrawShadowText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags,
    SizeShadow, ColorToRGB(ColorShadow), PosShadow);
end;

procedure TJvCustomLabel.DoDrawText(var Rect: TRect; Flags: Word);
var
  Tmp: TSize;
  TmpItem: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
  DrawState: TProviderDrawStates;
begin
  if ProviderActive then
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
  end
  else
    DoDrawCaption(Rect, Flags);
end;

procedure TJvCustomLabel.DrawAngleText(Flags: Word);
var
  Text: array[0..4096] of Char;
  LogFont, NewLogFont: TLogFont;
  NewFont: HFont;
  MRect: TRect;
  TextX, TextY: Integer;
  Phi: Real;
  Angle10: Integer;
begin
  Angle10 := Angle * 10;
  StrLCopy(@Text, PChar(GetLabelCaption), SizeOf(Text) - 1);
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');
  Canvas.Font := Font;
  if GetObject(Font.Handle, SizeOf(TLogFont), @LogFont) = 0 then
    RaiseLastOSError;
  NewLogFont := LogFont;
  MRect := ClientRect;
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
  Phi := Angle10 * Pi / 1800;
  if not AutoSize then
  begin
    TextX := Trunc(0.5 * ClientWidth - 0.5 * Canvas.TextWidth(Text) * Cos(Phi) - 0.5 * Canvas.TextHeight(Text) *
      Sin(Phi));
    TextY := Trunc(0.5 * ClientHeight - 0.5 * Canvas.TextHeight(Text) * Cos(Phi) + 0.5 * Canvas.TextWidth(Text) *
      Sin(Phi));
  end
  else
  begin
    ClientWidth := 4 + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)) + Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    ClientHeight := 4 + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)) + Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    TextX := 2;
    if (Angle10 > 900) and (Angle10 < 2700) then
      TextX := TextX + Trunc(Canvas.TextWidth(Text) * Abs(Cos(Phi)));
    if Angle10 > 1800 then
      TextX := TextX + Trunc(Canvas.TextHeight(Text) * Abs(Sin(Phi)));
    TextY := 2;
    if Angle10 < 1800 then
      TextY := TextY + Trunc(Canvas.TextWidth(Text) * Abs(Sin(Phi)));
    if (Angle10 > 900) and (Angle10 < 2700) then
      TextY := TextY + Trunc(Canvas.TextHeight(Text) * Abs(Cos(Phi)));
  end;
  if not Enabled then
  begin
    Canvas.Font.Color := clBtnHighlight;
    Canvas.TextOut(TextX+1, TextY+1, Text);
    Canvas.Font.Color := clBtnShadow;
    Canvas.TextOut(TextX, TextY, Text);
  end
  else
    Canvas.TextOut(TextX, TextY, Text);
end;

procedure TJvCustomLabel.Paint;
var
  Rect: TRect;
  DrawStyle: Integer;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  with Canvas do
  begin
    if not Transparent then
     // only FillRect mode because Transparent is always True on JVCLThemesEnabled
      DrawThemedBackground(Self, Canvas, ClientRect, Self.Color);
    Brush.Style := bsClear;
    if Angle <> 0 then
      DrawAngleText(DT_EXPANDTABS or DT_WORDBREAK or Alignments[Alignment])
    else
    begin
      Rect := ClientRect;
      Inc(Rect.Left, FLeftMargin);
      Dec(Rect.Right, FRightMargin);
      InflateRect(Rect, -1, 0);
      DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
      { Calculate vertical layout }
      if FLayout <> tlTop then
      begin
        DoDrawText(Rect, DrawStyle or DT_CALCRECT);
        Rect.Left := ClientRect.Left + FLeftMargin;
        Rect.Right := ClientRect.Right - FRightMargin;
//        if Images <> nil then
//          Inc(Rect.Left,GetImageWidth + 4);
        if FLayout = tlBottom then
          OffsetRect(Rect, 0, Height - Rect.Bottom)
        else
          OffsetRect(Rect, 0, (Height - Rect.Bottom) div 2);
      end;
      DoDrawText(Rect, DrawStyle);
    end;
    if FShowFocus and Assigned(FFocusControl) and FFocused and
      not (csDesigning in ComponentState) then
    begin
      InflateRect(Rect, 1, 0);
      Brush.Color := Self.Color;
      DrawFocusRect(Rect);
    end;
  end;
end;

procedure TJvCustomLabel.Loaded;
begin
  inherited Loaded;
  Provider.Loaded;
end;

procedure TJvCustomLabel.AdjustBounds;
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if AutoSize then
  begin
    Rect := ClientRect;
    Inc(Rect.Left, FLeftMargin);
    Dec(Rect.Right, FRightMargin);
    InflateRect(Rect, -1, 0);
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);
    Dec(Rect.Left, FLeftMargin);
    Inc(Rect.Right, FRightMargin);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    InflateRect(Rect, 1, 0);
    X := Left;
    AAlignment := FAlignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);
    Rect.Bottom := Max(Rect.Bottom, Rect.Top + GetImageHeight);
    if (AAlignment = taRightJustify) and (Images = nil) then
      Inc(X, Width - Rect.Right);
    if Images <> nil then
      Dec(Rect.Left,GetImageWidth + Spacing);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
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
  {$IFDEF COMPILER6_UP}
  inherited SetAutoSize(Value);
  {$ENDIF}
  FAutoSize := Value;
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

procedure TJvCustomLabel.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Max(Value, 0);
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetRightMargin(Value: Integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Max(Value, 0);
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
  {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
     Value := True; // themes aware Label are always transparent transparent
  {$ENDIF}
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
    AdjustBounds;
  end;
end;

procedure TJvCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FFocusControl) then
      FocusControl := nil;
    if (AComponent = Images) then
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
  P: TPoint;
  OldValue: Boolean;
begin
  OldValue := FMouseInControl;
  GetCursorPos(P);
  FMouseInControl := Enabled and (FindDragTarget(P, True) = Self) and
    IsForegroundTask;
  if FMouseInControl <> OldValue then
    if FMouseInControl then
      MouseEnter(Self)
    else
      MouseLeave(Self);
end;

procedure TJvCustomLabel.CMFocusChanged(var Msg: TCMFocusChanged);
var
  Active: Boolean;
begin
  Active := Assigned(FFocusControl) and (Msg.Sender = FFocusControl);
  if FFocused <> Active then
  begin
    FFocused := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited;
end;

procedure TJvCustomLabel.CMTextChanged(var Msg: TMessage);
begin
  NonProviderChange;
  Invalidate;
  AdjustBounds;
end;

procedure TJvCustomLabel.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustBounds;
  UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
end;

procedure TJvCustomLabel.CMDialogChar(var Msg: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Msg.CharCode, GetLabelCaption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Msg.Result := 1;
      end;
end;

procedure TJvCustomLabel.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomLabel.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomLabel.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  UpdateTracking;
end;

procedure TJvCustomLabel.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Visible then
    UpdateTracking;
end;

procedure TJvCustomLabel.MouseEnter(Control: TControl);
begin
  if not FMouseInControl and Enabled and IsForegroundTask then
  begin
    FHintSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if HotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FMouseInControl := True;
    inherited;
  end;
end;

procedure TJvCustomLabel.MouseLeave(Control: TControl);
begin
  if FMouseInControl and Enabled and not FDragging then
  begin
    Application.HintColor := FHintSaved;
    if HotTrack then
      Font.Assign(FFontSave);
    FMouseInControl := False;
    inherited;
  end;
end;

procedure TJvCustomLabel.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    if Images <> nil then
      NonProviderChange;
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    NonProviderChange;
    if FImages <> nil then
    begin
      FImages.RemoveFreeNotification(self);
      FImages.UnRegisterChanges(FChangeLink);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(self);
      FImages.RegisterChanges(FChangeLink);
    end;
    if AutoSize then AdjustBounds else Invalidate;
  end;
end;

function TJvCustomLabel.GetImageHeight: integer;
begin
  Result := 0;
  if not ProviderActive and (Images <> nil) then
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
    AdjustBounds;
end;

procedure TJvCustomLabel.NonProviderChange;
begin
  if Provider <> nil then
    Provider.Provider := nil;
end;

function TJvCustomLabel.GetImageWidth: integer;
begin
  Result := 0;
  if not ProviderActive and (Images <> nil) then
    Result := Images.Width;
end;

procedure TJvCustomLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomLabel.SetHotTrackFont(const Value: TFont);
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

procedure TJvCustomLabel.SetAngle(const Value: TJvLabelRotateAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLabel.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomLabel.SetSpacing(const Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    if AutoSize then AdjustBounds else Invalidate;
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


end.

