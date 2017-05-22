{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGroupHeader.PAS, released on 2002-09-02.

The Initial Developer of the Original Code is Fernando Silva [fernando dott silva att myrealbox dott com]
Portions created by Fernando Silva are Copyright (C) 2002 Fernando Silva.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGroupHeader;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Windows, Messages, Graphics, Controls, ExtCtrls,
  JvJCLUtils, JvComponent, JvTypes;

type
  TJvGroupHeaderOptions = class(TPersistent)
  private
    FBrush: TBrush;
    FHeight: Integer;
    FPen: TPen;
    FShape: TShapeType;
    FStyle: TJvBevelStyle;
    FOnChange: TNotifyEvent;
    procedure SetBrush(Value: TBrush);
    procedure SetHeight(Value: Integer);
    procedure SetPen(Value: TPen);
    procedure SetStyle(Value: TJvBevelStyle);
    procedure SetShape(Value: TShapeType);
  protected
    procedure DoChange(Sender: TObject);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Height: Integer read FHeight write SetHeight default 2;
    property Pen: TPen read FPen write SetPen;
    property Shape: TShapeType read FShape write SetShape default stRectangle;
    property Style: TJvBevelStyle read FStyle write SetStyle default bsLowered;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvGroupHeader = class(TJvGraphicControl)
  private
    FAlignment: TAlignment;
    FLayout: TJvLayout;
//    FLabelOptions: TJvGroupHeaderOptions;
    FBevelOptions: TJvGroupHeaderOptions;
    FBevelSpace: Integer;
    FPositionOffset: Integer;
    procedure SetPositionOffset(const Value: Integer);
    function GetTransparent: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TJvLayout);
    procedure SetBevelOptions(Value: TJvGroupHeaderOptions);
    procedure SetBevelSpace(Value: Integer);
//    procedure SetLabelOptions(Value: TJvGroupHeaderOptions);
  protected
    procedure StyleChanged(Sender: TObject); virtual;
    procedure BevelLine(C: TColor; X, Y, Width: Integer); virtual;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); virtual;
    function GetLabelText: string; virtual;
    procedure Paint; override;
    procedure TextChanged; override;
    procedure FontChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property PositionOffset: Integer read FPositionOffset write SetPositionOffset default 0;
    property BevelOptions: TJvGroupHeaderOptions read FBevelOptions write SetBevelOptions;
    property BevelSpace: Integer read FBevelSpace write SetBevelSpace default 12;
    // (p3) is this used anywhere?
//    property LabelOptions: TJvGroupHeaderOptions read FLabelOptions write SetLabelOptions stored false;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Layout: TJvLayout read FLayout write SetLayout default lTop;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

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
  JvThemes;

//=== { TJvGroupHeaderOptions } ==============================================

constructor TJvGroupHeaderOptions.Create;
begin
  inherited Create;
  FPen := TPen.Create;
  FPen.OnChange := DoChange;

  FBrush := TBrush.Create;
  FBrush.OnChange := DoChange;

  FShape := stRectangle;
  FStyle := bsLowered;
  FHeight := 2;
end;

destructor TJvGroupHeaderOptions.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvGroupHeaderOptions.Assign(Source: TPersistent);
var
  FOldChange: TNotifyEvent;
begin
  if Source is TJvGroupHeaderOptions then
  begin
    FOldChange := FOnChange;
    try
      FOnChange := nil;
      Brush := TJvGroupHeaderOptions(Source).Brush;
      Height := TJvGroupHeaderOptions(Source).Height;
      Pen := TJvGroupHeaderOptions(Source).Pen;
      Shape := TJvGroupHeaderOptions(Source).Shape;
      Style := TJvGroupHeaderOptions(Source).Style;
    finally
      FOnChange := FOldChange;
    end;
    DoChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TJvGroupHeaderOptions.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TJvGroupHeaderOptions.SetHeight(Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    DoChange(Self);
  end;
end;

procedure TJvGroupHeaderOptions.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TJvGroupHeaderOptions.SetStyle(Value: TJvBevelStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    DoChange(Self);
  end;
end;

procedure TJvGroupHeaderOptions.SetShape(Value: TShapeType);
begin
  if Value <> FShape then
  begin
    FShape := Value;
    DoChange(Self);
  end;
end;

procedure TJvGroupHeaderOptions.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//=== { TJvGroupHeader } =====================================================

constructor TJvGroupHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  {$IFDEF JVCLThemesEnabled}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF JVCLThemesEnabled}
  Width := 200;
  Height := 17;

  FBevelOptions := TJvGroupHeaderOptions.Create;
  FBevelOptions.OnChange := StyleChanged;
  FBevelSpace := 12;
//  FLabelOptions := TJvGroupHeaderOptions.Create;
//  FLabelOptions.OnChange := StyleChanged;
end;

destructor TJvGroupHeader.Destroy;
begin
  FBevelOptions.Free;
//  FLabelOptions.Free;
  inherited Destroy;
end;

function TJvGroupHeader.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TJvGroupHeader.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: string;
begin
  Text := GetLabelText;
  Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas, Text, Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas, Text, Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas, Text, Length(Text), Rect, Flags);
end;

procedure TJvGroupHeader.Paint;
const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  // Text
  Rect, CalcRect: TRect;
  DrawStyle: Longint;
  // Standard Bevel
  Color1, Color2: TColor;
  lbHeight, lbWidth: Integer;
  LX1, LX2, LX3, LX4, LY: Integer;
  // Shape Bevel
  X, Y, W, H, S: Integer;
begin
  // D R A W  T E X T
  // ----------------
  Color1 := clBlack;
  Color2 := clBlack; // (p3) just to remove warnings...
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      DrawThemedBackground(Self, Canvas, ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    // DoDrawText takes care of BiDi alignments
    DrawStyle := DT_EXPANDTABS or WordWraps[False] or Alignments[FAlignment];
    // Calculate vertical layout
    OffsetRect(Rect, FPositionOffset, 0);
    if FLayout <> lTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = lBottom then
        OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else
        OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;

  // C A L C U L A T E  P O S I T I O N S
  // --------------------------------------
  lbHeight := Canvas.TextHeight(GetLabelText);
  lbWidth := Canvas.TextWidth(GetLabelText);

  LX1 := 0;
  LX2 := 0;
  LX3 := 0;
  LX4 := 0;
  case FAlignment of
    taLeftJustify:
      begin
        LX1 := 0;
        LX2 := PositionOffset - FBevelSpace;
        LX3 := PositionOffset + lbWidth + FBevelSpace;
        LX4 := Width - lbWidth - FBevelSpace;
      end;
    taCenter:
      begin
        LX1 := 0;
        LX2 := (Width - lbWidth) div 2 + PositionOffset - FBevelSpace;
        LX3 := LX2 + lbWidth + 2 * FBevelSpace;
        LX4 := Width;
      end;
    taRightJustify:
      begin
        LX1 := 0;
        LX2 := PositionOffset + Width - lbWidth - FBevelSpace;
        LX3 := LX2 + lbWidth + 2 * FBevelSpace;
        LX4 := Width;
      end;
  end;
  if LX2 < LX1 then
    LX2 := LX1;
  if LX4 < LX3 then
    LX4 := LX3;

  LY := 0;
  case FLayout of
    lTop:
      LY := lbHeight div 2;
    lCenter:
      LY := Height div 2;
    lBottom:
      LY := Height - (lbHeight div 2);
  end;

  // D R A W  B E V E L
  // ------------------
  if BevelOptions.Style <> bsShape then
  begin
    with Canvas do
    begin
      // Assign colors
      case BevelOptions.Style of
        bsLowered:
          begin
            Color1 := clBtnShadow;
            Color2 := clBtnHighlight;
          end;
        bsRaised:
          begin
            Color1 := clBtnHighlight;
            Color2 := clBtnShadow;
          end;
      end;

      if csDesigning in ComponentState then
      begin
        Pen.Style := psSolid;
        Pen.Mode := pmCopy;
        Pen.Color := clBlack;
        Brush.Style := bsSolid;
      end;

      Pen.Width := 1;

      // Locate and draw the line

      if LX1 <> LX2 then
      begin
        BevelLine(Color1, LX1, LY, LX2);
        BevelLine(Color2, LX1, LY + 1, LX2);
      end;
      if (LX3 <> LX4) then // Draw right bevel
      begin
        BevelLine(Color1, LX3, LY, LX4);
        BevelLine(Color2, LX3, LY + 1, LX4);
      end;
    end;
  end
  else
    with Canvas do
    begin
      Pen := BevelOptions.Pen;
      Brush := BevelOptions.Brush;
      X := LX1 + (Pen.Width div 2);
      Y := LY - (BevelOptions.Height div 2) + (Pen.Width div 2);
      W := LX2 - Pen.Width + 1;
      H := BevelOptions.Height - Pen.Width + 1;
      if Pen.Width = 0 then
      begin
        Dec(W);
        Dec(H);
      end;
      if W < H then
        S := W
      else
        S := H;
      if BevelOptions.Shape in [stSquare, stRoundSquare, stCircle] then
      begin
        Inc(X, (W - S) div 2);
        Inc(Y, (H - S) div 2);
        W := S;
        H := S;
        lbWidth := Width - X * 2 - W;
      end
      else
        lbWidth := Width - X - W;
      case BevelOptions.Shape of
        stRectangle, stSquare:
          begin
            Rectangle(X, Y, X + W, Y + H);
            if FAlignment = taCenter then
              Rectangle(X + lbWidth, Y, X + W + lbWidth, Y + H);
          end;
        stRoundRect, stRoundSquare:
          begin
            RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
            if FAlignment = taCenter then
              RoundRect(X + lbWidth, Y, X + W + lbWidth, Y + H, S div 4, S div 4);
          end;
        stCircle, stEllipse:
          begin
            Ellipse(X, Y, X + W, Y + H);
            if FAlignment = taCenter then
              Ellipse(X + lbWidth, Y, X + W + lbWidth, Y + H);
          end;
      end;
    end;
end;

procedure TJvGroupHeader.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

function TJvGroupHeader.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TJvGroupHeader.SetTransparent(Value: Boolean);
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

procedure TJvGroupHeader.SetLayout(Value: TJvLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJvGroupHeader.SetPositionOffset(const Value: Integer);
begin
  if Value <> FPositionOffset then
  begin
    FPositionOffset := Value;
    Invalidate;
  end;
end;

procedure TJvGroupHeader.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TJvGroupHeader.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

procedure TJvGroupHeader.SetBevelSpace(Value: Integer);
begin
  if Value <> FBevelSpace then
  begin
    FBevelSpace := Value;
    Invalidate;
  end;
end;

procedure TJvGroupHeader.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvGroupHeader.BevelLine(C: TColor; X, Y, Width: Integer);
begin
  with Canvas do
  begin
    Pen.Color := C;
    MoveTo(X, Y);
    LineTo(X + Width, Y);
  end;
end;

procedure TJvGroupHeader.SetBevelOptions(Value: TJvGroupHeaderOptions);
begin
  FBevelOptions.Assign(Value);
end;

{
procedure TJvGroupHeader.SetLabelOptions(Value: TJvGroupHeaderOptions);
begin
  FLabelOptions.Assign(Value);
end;
}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
