{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPContainer.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvXPContainer;

{$I jvcl.inc}

interface

uses
{$IFDEF COMPILER6_UP}
  TypInfo,
{$ENDIF}
  Windows, Classes, Controls, Graphics, StdCtrls, JvXPCore, JvXPCoreUtils;

type
{ TJvXPPaintEvent }

  TJvXPPaintEvent = procedure(Sender: TObject; Rect: TRect; ACanvas: TCanvas;
    AFont: TFont) of object;

{ TJvXPEnabledMode }

{$IFDEF COMPILER6_UP}
  TJvXPEnabledMode = (emAffectChilds, emNormal);
{$ENDIF}

{ TJvXPCustomContainer }

  TJvXPCustomContainer = class(TJvXPCustomControl)
  private
    FAlignment: TAlignment;
    FBorderWidth: TBorderWidth;
    FBoundColor: TColor;
    FBoundLines: TJvXPBoundLines;
  {$IFDEF COMPILER6_UP}
    FEnabledMode: TJvXPEnabledMode;
  {$ENDIF}
    FFocusable: Boolean;
    FGlyph: TBitmap;
    FGlyphLayout: TJvXPGlyphLayout;
    FLayout: TTextLayout;
    FShowBoundLines: Boolean;
    FShowCaption: Boolean;
    FSpacing: Byte;
    FWordWrap: Boolean;
  {$IFDEF COMPILER6_UP}
    FOnEnabledChanged: TNotifyEvent;
  {$ENDIF}
    FOnPaint: TJvXPPaintEvent;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBoundColor(Value: TColor);
    procedure SetBoundLines(Value: TJvXPBoundLines);
  {$IFDEF COMPILER6_UP}
    procedure SetEnabledMode(Value: TJvXPEnabledMode);
  {$ENDIF}
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphLayout(Value: TJvXPGlyphLayout);
    procedure SetLayout(Value: TTextLayout);
    procedure SetShowBoundLines(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetSpacing(Value: Byte);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
  {$IFDEF COMPILER6_UP}
    procedure HookEnabledChanged; override;
  {$ENDIF}
    procedure HookMouseDown; override;
    procedure HookPosChanged; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BoundColor: TColor read FBoundColor write SetBoundColor default clGray;
    property BoundLines: TJvXPBoundLines read FBoundLines write SetBoundLines default [];
  {$IFDEF COMPILER6_UP}
    property EnabledMode: TJvXPEnabledMode read FEnabledMode write SetEnabledMode
      default emNormal;
  {$ENDIF}
    property Focusable: Boolean read FFocusable write FFocusable default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphLayout: TJvXPGlyphLayout read FGlyphLayout write SetGlyphLayout
      default glCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property Height default 41;
    property ShowBoundLines: Boolean read FShowBoundLines write SetShowBoundLines
      default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption
      default False;
    property Spacing: Byte read FSpacing write SetSpacing default 5;
    property Width default 185;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  {$IFDEF COMPILER6_UP}
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
  {$ENDIF}
    property OnPaint: TJvXPPaintEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

{ TJvXPContainer }

  TJvXPContainer = class(TJvXPCustomContainer)
  published
    property Alignment;
    property AutoSize;
    property BorderWidth;
    property BoundColor;
    property BoundLines;
    property Caption;
    property Color;
    property Enabled;
  {$IFDEF COMPILER6_UP}
    property EnabledMode;
  {$ENDIF}
    property Focusable;
    property Glyph;
    property GlyphLayout;
    property Layout;
    property ParentColor;
    property ShowBoundLines;
    property ShowCaption;
    property Spacing;
    property WordWrap;
  {$IFDEF COMPILER6_UP}
    property OnEnabledChanged;
  {$ENDIF}
    property OnDblClick;
    property OnPaint;
    property OnResize;
  end;

implementation

{ TJvXPCustomContainer }

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.Create
  Author:    mh
  Date:      20-Aug-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvXPCustomContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Height := 41;
  Width := 185;
  FAlignment := taCenter;
  FBoundColor := clGray;
  FBoundLines := [];
{$IFDEF COMPILER6_UP}
  FEnabledMode := emNormal;
{$ENDIF}
  FFocusable := False;
  FGlyph := TBitmap.Create;
  FGlyph.Assign(nil);
  FGlyphLayout := glCenter;
  FLayout := tlCenter;
  FShowBoundLines := True;
  FShowCaption := False;
  FSpacing := 5;
  FWordWrap := False;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.Destroy
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvXPCustomContainer.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.CreateParams
  Author:    mh
  Date:      20-Aug-2002
  Arguments: var Params: TCreateParams
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.HookEnabledChanged
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

{$IFDEF COMPILER6_UP}
procedure TJvXPCustomContainer.HookEnabledChanged;
var
  i: Integer;
begin
  inherited;
  if FEnabledMode = emAffectChilds then
  for i := 0 to ControlCount - 1 do
    if IsPublishedProp(Controls[i], 'Enabled') then
      SetPropValue(Controls[i], 'Enabled', Enabled);
  if Assigned(FOnEnabledChanged) then
    FOnEnabledChanged(Self);
end;
{$ENDIF}

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.HookMouseDown
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.HookMouseDown;
begin
  case FFocusable of
    True:
      inherited;
    False:
      begin
        DrawState := DrawState + [dsClicked];
        InternalRedraw;
      end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.HookPosChanged
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.HookPosChanged;
begin
  inherited;
  InternalRedraw;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.AdjustClientRect
  Author:    mh
  Date:      20-Aug-2002
  Arguments: var Rect: TRect
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  JvXPAdjustBoundRect(BorderWidth, FShowBoundLines, FBoundLines, Rect);
  if not FGlyph.Empty then
    Inc(Rect.Left, FGlyph.Width);
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetAlignment
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TAlignment
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetBoundColor
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TColor
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetBoundColor(Value: TColor);
begin
  if Value <> FBoundColor then
  begin
    FBoundColor := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetBoundLines
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TJvXPBoundLines
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetBoundLines(Value: TJvXPBoundLines);
begin
  if Value <> FBoundLines then
  begin
    FBoundLines := Value;
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetBorderWidth
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TBorderWidth
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetBorderWidth(Value: TBorderWidth);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetEnabledMode
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TJvXPEnabledMode
  Result:    None
-----------------------------------------------------------------------------}

{$IFDEF COMPILER6_UP}
procedure TJvXPCustomContainer.SetEnabledMode(Value: TJvXPEnabledMode);
begin
  if Value <> FEnabledMode then
  begin
    FEnabledMode := Value;
    HookEnabledChanged;
  end;
end;
{$ENDIF}

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetGlyph
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: TBitmap
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetGlyph(Value: TBitmap);
begin
  if Value <> FGlyph then
  begin
    FGlyph.Assign(Value);
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetGlyphLayout
  Author:    mh
  Date:      22-Aug-2002
  Arguments: Value: TJvXPGlyphLayout
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetGlyphLayout(Value: TJvXPGlyphLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetLayout
  Author:    mh
  Date:      22-Aug-2002
  Arguments: Value: TTextLayout
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetShowBoundLines
  Author:    M. Hoffmann
  Date:      03-Feb-2003
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetShowBoundLines(Value: Boolean);
begin
  if Value <> FShowBoundLines then
  begin
    FShowBoundLines := Value;
    Realign;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetShowCaption
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetShowCaption(Value: Boolean);
begin
  if Value <> FShowCaption then
  begin
    FShowCaption := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetSpacing
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: Byte
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetSpacing(Value: Byte);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    InternalRedraw;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.SetWordWrap
  Author:    mh
  Date:      20-Aug-2002
  Arguments: Value: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    InternalRedraw;
  end;
end;

procedure dxDrawText(AParent: TJvXPCustomControl; ACaption: string; AFont: TFont;
  AAlignment: TAlignment; ALayout: TTextLayout; AWordWrap: Boolean; var ARect: TRect);
  procedure DoDrawText(Handle: THandle; ACaption: string; var ARect: TRect;
    Flags: Integer);
  begin
    DrawText(Handle, PChar(ACaption), -1, ARect, Flags);
  end;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DrawStyle: LongInt;
  CalcRect: TRect;
begin
  with AParent, Canvas do
  begin
    DrawStyle := Alignments[AAlignment];
    if (DrawStyle <> DT_LEFT) and (ARect.Right - ARect.Left < TextWidth(ACaption)) then
      DrawStyle := DT_LEFT;
    DrawStyle := DrawStyle or DT_EXPANDTABS or WordWraps[AWordWrap] or DT_END_ELLIPSIS;
    if ALayout <> tlTop then
    begin
      CalcRect := ARect;
      DoDrawText(Handle, ACaption, CalcRect, DrawStyle or DT_CALCRECT);
      if ALayout = tlBottom then
        OffsetRect(ARect, 0, ARect.Bottom - CalcRect.Bottom)
      else
        OffsetRect(ARect, 0, (ARect.Bottom - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Handle, ACaption, ARect, DrawStyle);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvXPCustomContainer.Paint
  Author:    mh
  Date:      20-Aug-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvXPCustomContainer.Paint;
var
  Rect: TRect;
begin
  with Canvas do
  begin
    Rect := GetClientRect;
    Brush.Color := Self.Color;
    FillRect(Rect);
    if csDesigning in ComponentState then
      DrawFocusRect(Rect);
    Brush.Style := bsClear;
    if (FShowBoundLines) and (FBoundLines <> []) then
      JvXPDrawBoundLines(Self.Canvas, FBoundLines, FBoundColor, Rect);
    JvXPAdjustBoundRect(BorderWidth, FShowBoundLines, FBoundLines, Rect);
    if Assigned(FOnPaint) then
      FOnPaint(Self, Rect, Self.Canvas, Font);
    if not FGlyph.Empty then
    begin
      FGlyph.Transparent := True;
      if FGlyphLayout = glBottom then
        Draw(Rect.Left, Rect.Bottom - FGlyph.Height, FGlyph);
      if FGlyphLayout = glCenter then
        Draw(Rect.Left, ((Rect.Bottom - Rect.Top) - FGlyph.Height) div 2 + 1, FGlyph);
      if FGlyphLayout = glTop then
        Draw(Rect.Left, Rect.Top, FGlyph);
      Inc(Rect.Left, FGlyph.Width);
    end;
    if FShowCaption then
    begin
      Font.Assign(Self.Font);
      InflateRect(Rect, -FSpacing, -1);
      if csDesigning in ComponentState then
      begin
        Pen.Color := clGray;
        Pen.Style := psInsideFrame;
        MoveTo(Rect.Left, Rect.Top);
        LineTo(Rect.Left, Rect.Bottom);
        MoveTo(Rect.Right, Rect.Top);
        LineTo(Rect.Right, Rect.Bottom);
      end;
      dxDrawText(Self, Caption, Font, FAlignment, FLayout, FWordWrap, Rect);
      //JvXPPlaceText(Self, Canvas, Caption, Font, Enabled, False, FAlignment,
      //  FWordWrap, Rect);
    end;
  end;
end;

end.
