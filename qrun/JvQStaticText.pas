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

The Original Code is: JvStaticText.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Thörnqvist <peter3 at sourceforge dot net>

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Changes 2002-10-22:
  Totally reimplemented (though a lot of code was taken from original TStaticText) to add new properties:
  WordWrap, Layout (vertial Alignment), TextMargins (to offset from the edges)

  Also adds virtual DrawItem and AdjustBounds methods to make it easier to derive new
  components that handles the drawing differently

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQStaticText;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QStdCtrls, QForms,
  JvQTypes, JvQComponent;

type
  TStaticBorderStyle = (sbsNone, sbsSingle, sbsSunken);

  TJvTextMargins = class(TPersistent)
  private
    FX: Word;
    FY: Word;
    FOnChange: TNotifyEvent;
    procedure SetX(const Value: Word);
    procedure SetY(const Value: Word);
    procedure Change;
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: Word read FX write SetX;
    property Y: Word read FY write SetY;
  end;

  TJvCustomStaticText = class(TJvWinControl)
  private
    FFontSave: TFont;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FLayout: TTextLayout;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FBorderStyle: TStaticBorderStyle;
    FFocusControl: TWinControl;
    FShowAccelChar: Boolean;
    FTextMargins: TJvTextMargins;
    FWordWrap: Boolean;
    FHotTrackFontOptions: TJvTrackFOntOptions;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderStyle(Value: TStaticBorderStyle);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetLayout(const Value: TTextLayout); 
    procedure SetTextMargins(const Value: TJvTextMargins);
    procedure SetWordWrap(const Value: Boolean);
    procedure DoMarginsChange(Sender: TObject);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    function WantKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean; override;
    procedure FontChanged; override;
    procedure TextChanged; override;
    procedure AdjustBounds; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); //override;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); virtual;
    function GetTextDisplayInfo(ADC: HDC; var ARect: TRect): Cardinal;  
    procedure Paint; override; 
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property BorderStyle: TStaticBorderStyle read FBorderStyle write SetBorderStyle default sbsNone;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFOntOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property Layout: TTextLayout read FLayout write SetLayout;
    property TextMargins: TJvTextMargins read FTextMargins write SetTextMargins;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvStaticText = class(TJvCustomStaticText)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BorderStyle;
    property Caption: TCaption read GetText write SetText;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property HintColor;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextMargins;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnStartDrag;
  end;

implementation

uses
  JvQJVCLUtils, JvQThemes;

//=== { TJvCustomStaticText } ================================================

constructor TJvCustomStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextMargins := TJvTextMargins.Create;
  FTextMargins.OnChange := DoMarginsChange;

  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FLayout := tlTop;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csReplicatable, csDoubleClicks];
  IncludeThemeStyle(Self, [csParentBackground]);

  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvCustomStaticText.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  FTextMargins.Free;
  inherited Destroy;
end;

procedure TJvCustomStaticText.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvCustomStaticText.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if FHotTrack then
      Font.Assign(FFontSave);
    inherited MouseLeave(Control);
  end;
end;

procedure TJvCustomStaticText.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;




procedure TJvCustomStaticText.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  cBorders: array [TStaticBorderStyle] of DWORD = (0, BF_MONO, BF_SOFT);
var
  R: TRect;
  DrawStyle: Cardinal;
  B: HBRUSH;
begin
  B := CreateSolidBrush(ColorToRGB(Color));
  try
    with DrawItemStruct do
    begin
      R := rcItem;
      if BorderStyle <> sbsNone then
        QWindows.
        DrawEdge(hDC, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT or cBorders[BorderStyle]);
      DrawStyle := GetTextDisplayInfo(hDC, R);
      case Layout of
        tlTop:
          OffsetRect(R, 0, FTextMargins.Y);
        tlBottom:
          OffsetRect(R, 0, (ClientHeight - R.Bottom) - FTextMargins.Y);
        tlCenter:
          OffsetRect(R, 0, (ClientHeight - R.Bottom) div 2);
      end;
      case Alignment of
        taLeftJustify:
          OffsetRect(R, FTextMargins.X, 0);
        taRightJustify:
          OffsetRect(R, (Width - R.Right) - FTextMargins.X, 0);
        taCenter:
          OffsetRect(R, (Width - R.Right) div 2, 0);
      end;
      SetBkMode(hDC, QWindows.TRANSPARENT);
      DrawTextW(hDC, PWideChar(Caption), Length(Caption), R, DrawStyle);
//      DrawText(hDC, Caption, Length(Caption), R, DrawStyle);
    end;
  finally
    DeleteObject(B);
  end;
end;

procedure TJvCustomStaticText.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Value then
      AdjustBounds;
  end;
end;

procedure TJvCustomStaticText.AdjustBounds;
var
  DC: HDC;
  R: TRect;
  SaveFont: HFont;
  TextSize: TSize;
begin
  if not (csReading in ComponentState) and AutoSize and HandleAllocated then
  begin
    DC := GetDC(0);
    if not WordWrap then
    begin
      SaveFont := SelectObject(DC, Font.Handle);  
      GetTextExtentPoint32(DC, PWideChar(Caption), Length(Caption), TextSize); 
      SelectObject(DC, SaveFont);
      SetBounds(Left, Top,
        TextSize.cx + (GetSystemMetrics(SM_CXBORDER) * 4),
        TextSize.cy + (GetSystemMetrics(SM_CYBORDER) * 4));
    end
    else
    begin
      R := ClientRect;
      GetTextDisplayInfo(DC, R);
      SetBounds(Left, Top, R.Right, R.Bottom);
    end;
    ReleaseDC(0, DC);
  end;
end;

function TJvCustomStaticText.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Key, Caption) and (ssAlt in Shift);
  if Result then
    if FFocusControl.CanFocus then
      FFocusControl.SetFocus;
end;

procedure TJvCustomStaticText.FontChanged;
begin
  inherited FontChanged;
  AdjustBounds;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCustomStaticText.TextChanged;
begin
  inherited TextChanged;
  AdjustBounds;
end;

procedure TJvCustomStaticText.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TJvCustomStaticText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TJvCustomStaticText.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.SetBorderStyle(Value: TStaticBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvCustomStaticText.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TJvCustomStaticText.SetTextMargins(const Value: TJvTextMargins);
begin
  if Value = nil then
  begin
    FTextMargins.X := 0;
    FTextMargins.Y := 0
  end
  else
  begin
    FTextMargins.X := Value.X;
    FTextMargins.Y := Value.Y;
  end;
end;

procedure TJvCustomStaticText.DoMarginsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomStaticText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

function TJvCustomStaticText.GetTextDisplayInfo(ADC: HDC; var ARect: TRect): Cardinal;
const
  cAlignment: array [Boolean, TAlignment] of DWORD =
    ((DT_LEFT, DT_RIGHT, DT_CENTER), (DT_RIGHT, DT_LEFT, DT_CENTER));
  cLayout: array [TTextLayout] of DWORD = (DT_TOP, DT_VCENTER, DT_BOTTOM);
  cDrawAccel: array [Boolean] of DWORD = (DT_NOPREFIX, 0);
  cWordWrap: array [Boolean] of DWORD = (0{DT_SINGLELINE}, DT_WORDBREAK);
begin
  Result := DT_EXPANDTABS or cAlignment[UseRightToLeftAlignment, Alignment] or
    cLayout[Layout] or cDrawAccel[ShowAccelChar] or cWordWrap[WordWrap];  
  DrawTextW(ADC, PWideChar(Caption), Length(Caption), ARect, Result or DT_CALCRECT);
end;

procedure TJvCustomStaticText.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TJvCustomStaticText.SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font,FHotTrackFontOptions);
  end;
end;


procedure TJvCustomStaticText.Paint;
var
  FDrawItemStruct: TDrawItemStruct;
begin
  with FDrawItemStruct do
  begin
    rcItem := Bounds(0, 0, Width, Height);
    hDC := Canvas.Handle;
    hWndItem := Handle;
  end;
  DrawItem(FDrawItemStruct)
end;



//=== { TJvTextMargins } =====================================================

procedure TJvTextMargins.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTextMargins.SetX(const Value: Word);
begin
  if FX <> Value then
  begin
    FX := Value;
    Change;
  end;
end;

procedure TJvTextMargins.SetY(const Value: Word);
begin
  if FY <> Value then
  begin
    FY := Value;
    Change;
  end;
end;

end.

