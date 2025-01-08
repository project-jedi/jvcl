{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBevel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBevel;

{$I jvcl.inc}

interface

uses
  Types,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, ExtCtrls, Graphics,
  JvThemes, JvExExtCtrls;

type
  TJvBevelLines = class;

  TJvBevelStyle = (bsLowered, bsRaised, bsCustomStyle);
  TJvBevelShape = (bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine,
    bsRightLine, bsSpacer, bsCustomShape);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvBevel = class(TJvExBevel)
  private
    FStyle: TJvBevelStyle;
    FShape: TJvBevelShape;
    FOuter: TBevelCut;
    FInner: TBevelCut;
    FPenWidth: Integer;
    FPenStyle: TPenStyle;
    FBold: Boolean;
    FEdges: TBevelEdges;
    FVerticalLines: TJvBevelLines;
    FHorizontalLines: TJvBevelLines;
    FHorLines: TJvBevelLines;

    procedure ReadBevelInner(Reader: TReader);
    procedure ReadBevelOuter(Reader: TReader);
    procedure ReadBevelSides(Reader: TReader);
    procedure ReadBevelBold(Reader: TReader);
    procedure ReadBevelPenStyle(Reader: TReader);
    procedure ReadBevelPenWidth(Reader: TReader);
    procedure IgnoreValue(Reader: TReader);

    procedure LinesChange(Sender: TObject);

    procedure SetStyle(const Value: TJvBevelStyle);
    procedure SetShape(const Value: TJvBevelShape);
    procedure SetInner(const Value: TBevelCut);
    procedure SetOuter(const Value: TBevelCut);
    procedure SetPenWidth(const Value: Integer);
    procedure SetPenStyle(const Value: TPenStyle);
    procedure SetBold(const Value: Boolean);
    procedure SetEdges(const Value: TBevelEdges);
    procedure SetHorizontalLines(const Value: TJvBevelLines);
    procedure SetVerticalLines(const Value: TJvBevelLines);
  protected
    procedure DrawBevel(R: TRect; Cut: TBevelCut; EffectiveEdges: TBevelEdges);
    procedure DrawBold(R: TRect; Cut: TBevelCut; EffectiveEdges: TBevelEdges);
    procedure DrawLines(InnerRect: TRect; Lines: TJvBevelLines; Vertical: Boolean);
    procedure Paint; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bold: Boolean read FBold write SetBold default False;
    property Edges: TBevelEdges read FEdges write SetEdges default [beLeft, beTop, beRight, beBottom];
    property Inner: TBevelCut read FInner write SetInner default bvNone;
    property Outer: TBevelCut read FOuter write SetOuter default bvLowered;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle default psSolid;
    property PenWidth: Integer read FPenWidth write SetPenWidth default 1;
    property HintColor;
    property HorizontalLines: TJvBevelLines read FHorizontalLines write SetHorizontalLines;
    property Shape: TJvBevelShape read FShape write SetShape default bsBox;
    property Style: TJvBevelStyle read FStyle write SetStyle default bsLowered;
    property VerticalLines: TJvBevelLines read FVerticalLines write SetVerticalLines;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

  TJvBevelLines = class(TPersistent)
  private
    FCount: Cardinal;
    FStyle: TBevelCut;
    FBold: Boolean;
    FThickness: Byte;
    FIgnoreBorder: Boolean;
    FOnChange: TNotifyEvent;
    procedure IgnoreValue(Reader: TReader);
    procedure SetBold(const Value: Boolean);
    procedure SetCount(const Value: Cardinal);
    procedure SetIgnoreBorder(const Value: Boolean);
    procedure SetStyle(const Value: TBevelCut);
    procedure SetThickness(const Value: Byte);
  protected
    procedure DoChange; virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Count: Cardinal read FCount write SetCount default 0;
    property Style: TBevelCut read FStyle write SetStyle default bvLowered;
    property Bold: Boolean read FBold write SetBold default False;
    property Thickness: Byte read FThickness write SetThickness default 1;
    property IgnoreBorder: Boolean read FIgnoreBorder write SetIgnoreBorder default False;
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
  SysUtils, TypInfo, RTLConsts;

type
  TReaderAccess = class(TReader);

//=== { TJvBevel } ===========================================================

constructor TJvBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);

  FHorizontalLines := TJvBevelLines.Create;
  FVerticalLines := TJvBevelLines.Create;

  FHorLines := TJvBevelLines.Create;

  FHorizontalLines.OnChange := LinesChange;
  FVerticalLines.OnChange := LinesChange;

  FEdges := [beLeft, beTop, beRight, beBottom];
  FInner := bvNone;
  FOuter := bvLowered;
  FPenWidth := 1;
  FShape := bsBox;
  FStyle := bsLowered;
end;

procedure TJvBevel.DefineProperties(Filer: TFiler);
begin
  // Required for silent migration from Globus' TJvgBevel
  Filer.DefineProperty('BevelInner', ReadBevelInner, nil, False);
  Filer.DefineProperty('BevelOuter', ReadBevelOuter, nil, False);
  Filer.DefineProperty('BevelSides', ReadBevelSides, nil, False);
  Filer.DefineProperty('BevelBold', ReadBevelBold, nil, False);
  Filer.DefineProperty('BevelPenStyle', ReadBevelPenStyle, nil, False);
  Filer.DefineProperty('BevelPenWidth', ReadBevelPenWidth, nil, False);
  Filer.DefineProperty('InteriorOffset', IgnoreValue, nil, False);

  inherited DefineProperties(Filer);
end;

destructor TJvBevel.Destroy;
begin
  FHorizontalLines.Free;
  FVerticalLines.Free;

  FHorLines.Free;

  inherited Destroy;
end;

procedure TJvBevel.DrawBevel(R: TRect; Cut: TBevelCut; EffectiveEdges: TBevelEdges);
var
  ColorTopLeft: TColor;
  ColorBottomRight: TColor;
begin
  ColorTopLeft := clNone;
  ColorBottomRight := clNone;
  case Cut of
    bvLowered:
      begin
        ColorTopLeft := clBtnShadow;
        ColorBottomRight := clBtnHighlight;
      end;
    bvRaised:
      begin
        ColorTopLeft := clBtnHighlight;
        ColorBottomRight := clBtnShadow;
      end;
  end;

  if ColorTopLeft <> clNone then
  begin
    Canvas.Pen.Color := ColorTopLeft;
    if beLeft in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Left, R.Bottom - 1);
      Canvas.LineTo(R.Left, R.Top - 1);
    end;
    if beTop in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);
    end;
  end;

  if ColorBottomRight <> clNone then
  begin
    Canvas.Pen.Color := ColorBottomRight;
    if beRight in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Right, R.Top);
      Canvas.LineTo(R.Right, R.Bottom);
    end;
    if beBottom in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Right, R.Bottom);
      Canvas.LineTo(R.Left - 1, R.Bottom);
    end;
  end;
end;

procedure TJvBevel.DrawBold(R: TRect; Cut: TBevelCut; EffectiveEdges: TBevelEdges);
begin
  Canvas.Pen.Color := cl3DDkShadow;

  if Cut = bvLowered then
  begin
    if beLeft in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Left - 1, R.Bottom - 1);
      Canvas.LineTo(R.Left - 1, R.Top - 1);
    end;

    if beTop in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Left - 1, R.Top - 1);
      Canvas.LineTo(R.Right, R.Top - 1);
    end;
  end;

  if Cut = bvRaised then
  begin
    if beBottom in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Left, R.Bottom + 1);
      Canvas.LineTo(R.Right + 1, R.Bottom + 1);
    end;

    if beRight in EffectiveEdges then
    begin
      Canvas.MoveTo(R.Right + 1, R.Bottom + 1);
      Canvas.LineTo(R.Right + 1, R.Top - 1);
    end;
  end;
end;

procedure TJvBevel.DrawLines(InnerRect: TRect; Lines: TJvBevelLines;
  Vertical: Boolean);
var
  EffectiveRect: TRect;
  LineRect: TRect;
  I: Integer;
  LineEdges: TBevelEdges;
begin
  if Lines.IgnoreBorder then
    EffectiveRect := Rect(0, 0, Width-1, Height-1)
  else
    EffectiveRect := Rect(InnerRect.Left+1, InnerRect.Top+1, InnerRect.Right-1, InnerRect.Bottom-1);

  if Lines.Style = bvSpace then
    LineEdges := [beLeft, beTop]
  else
  if Vertical then
    LineEdges := [beLeft, beRight]
  else
    LineEdges := [beTop, beBottom];

  for I := 1 to Lines.Count do
  begin
    LineRect := EffectiveRect;

    if Vertical then
    begin
      LineRect.Left := Muldiv(I, Width, Lines.Count + 1);
      LineRect.Right := LineRect.Left + Lines.Thickness + Ord(Lines.Bold);
    end
    else
    begin
      LineRect.Top := Muldiv(I, Height, Lines.Count + 1);
      LineRect.Bottom := LineRect.Top + Lines.Thickness + Ord(Lines.Bold);
    end;

    DrawBevel(LineRect, Lines.Style, LineEdges);
  end;
end;

procedure TJvBevel.IgnoreValue(Reader: TReader);
begin
  TReaderAccess(Reader).SkipValue;
end;

procedure TJvBevel.LinesChange(Sender: TObject);
begin
  Invalidate;
  Style := bsCustomStyle;
end;

procedure TJvBevel.Paint;
var
  EffectiveOuterRect: TRect;
  EffectiveInnerRect: TRect;
begin
  if (Style = bsCustomStyle) or (Shape = bsCustomShape) then
  begin
    Canvas.Pen.Style := PenStyle;
    Canvas.Pen.Width := PenWidth;

    EffectiveOuterRect := Rect(0, 0, Width - 1, Height - 1);
    EffectiveInnerRect := Rect(1, 1, Width - 2, Height - 2);

    // Boldness adds a dark shadow line outside any border line that is
    // drawn in clBtnShadow. This effectively pushes the line inwards.
    // clBtnShadow is used at top left for bvLowered and at bottom right
    // for bvRaised. In these cases the place where the clBtnShadow line
    // is to be drawn has to be moved inward and a cl3DDkShadow drawn in
    // its place.
    if Bold then
    begin
      case Outer of
        bvLowered:
          begin
            Inc(EffectiveOuterRect.Left);
            Inc(EffectiveOuterRect.Top);
            Inc(EffectiveInnerRect.Left);
            Inc(EffectiveInnerRect.Top);
          end;
        bvRaised:
          begin
            Dec(EffectiveOuterRect.Right);
            Dec(EffectiveOuterRect.Bottom);
            Dec(EffectiveInnerRect.Right);
            Dec(EffectiveInnerRect.Bottom);
          end;
      end;

      case Inner of
        bvLowered:
          begin
            Inc(EffectiveInnerRect.Left);
            Inc(EffectiveInnerRect.Top);
          end;
        bvRaised:
          begin
            Dec(EffectiveInnerRect.Right);
            Dec(EffectiveInnerRect.Bottom);
          end;
      end;

      DrawBold(EffectiveOuterRect, Outer, Edges);
      DrawBold(EffectiveInnerRect, Inner, Edges);
    end;

    DrawBevel(EffectiveOuterRect, Outer, Edges);
    DrawBevel(EffectiveInnerRect, Inner, Edges);

    if Inner in [bvLowered, bvRaised] then
    begin
      DrawLines(EffectiveInnerRect, HorizontalLines, False);
      DrawLines(EffectiveInnerRect, VerticalLines, True);
    end
    else
    begin
      DrawLines(EffectiveOuterRect, HorizontalLines, False);
      DrawLines(EffectiveOuterRect, VerticalLines, True);
    end;
  end
  else
  begin
    inherited Paint;
  end;
end;

procedure TJvBevel.ReadBevelBold(Reader: TReader);
begin
  Bold := Reader.ReadBoolean;
end;

procedure TJvBevel.ReadBevelInner(Reader: TReader);
begin
  Inner := TBevelCut(GetEnumValue(TypeInfo(TBevelCut), Reader.ReadIdent));
end;

procedure TJvBevel.ReadBevelOuter(Reader: TReader);
begin
  Outer := TBevelCut(GetEnumValue(TypeInfo(TBevelCut), Reader.ReadIdent));
end;

procedure TJvBevel.ReadBevelPenStyle(Reader: TReader);
begin
  PenStyle := TPenStyle(GetEnumValue(TypeInfo(TPenStyle), Reader.ReadIdent));
end;

procedure TJvBevel.ReadBevelPenWidth(Reader: TReader);
begin
  PenWidth := Reader.ReadInteger;
end;

procedure TJvBevel.ReadBevelSides(Reader: TReader);
var
  EnumType: PTypeInfo;
  EnumName: string;
  Value: Integer;
begin
  // To allow for the Globus TglSide property to be read, we must read the
  // set ourselves, replacing the fsd prefix by be before reading the value
  try
    if Reader.ReadValue <> vaSet then
      raise EReadError.CreateRes(@SInvalidPropertyValue);

    EnumType := TypeInfo(TBevelEdge);
    Edges := [];
    while True do
    begin
      EnumName := Reader.ReadStr;
      if EnumName = '' then
        Break;

      EnumName := StringReplace(EnumName, 'fsd', 'be', []);
      Value := GetEnumValue(EnumType, EnumName);
      if Value = -1 then
        raise EReadError.CreateRes(@SInvalidPropertyValue);

      Include(FEdges, TBevelEdge(Value));
    end;
  except
    // Reader.SkipSetBody
    while Reader.ReadStr <> '' do
      ;
    raise;
  end;
end;

procedure TJvBevel.SetBold(const Value: Boolean);
begin
  if FBold <> Value then
  begin
    FBold := Value;
    Invalidate;
  end;
end;

procedure TJvBevel.SetEdges(const Value: TBevelEdges);
begin
  if FEdges <> Value then
  begin
    FEdges := Value;
    Shape := bsCustomShape;
    Invalidate;
  end;
end;

procedure TJvBevel.SetHorizontalLines(const Value: TJvBevelLines);
begin
  FHorizontalLines.Assign(Value);
end;

procedure TJvBevel.SetInner(const Value: TBevelCut);
begin
  if FInner <> Value then
  begin
    FInner := Value;
    Style := bsCustomStyle;
    Invalidate;
  end;
end;

procedure TJvBevel.SetOuter(const Value: TBevelCut);
begin
  if FOuter <> Value then
  begin
    FOuter := Value;
    Style := bsCustomStyle;
    Invalidate;
  end;
end;

procedure TJvBevel.SetPenStyle(const Value: TPenStyle);
begin
  if FPenStyle <> Value then
  begin
    FPenStyle := Value;
    Invalidate;
  end;
end;

procedure TJvBevel.SetPenWidth(const Value: Integer);
begin
  if FPenWidth <> Value then
  begin
    FPenWidth := Value;
    Invalidate;
  end;
end;

procedure TJvBevel.SetShape(const Value: TJvBevelShape);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    if FShape <> bsCustomShape then
    begin
      inherited Shape := TBevelShape(FShape);

      // Set the other properties so that should the style become
      // bsCustomStyle, the rendering is the closest it can be to
      // the one done in the ancestor.
      // Those next few lines define the most common properties.
      FEdges := [beTop, beLeft, beBottom, beRight];
      case Style of
        bsLowered:
          begin
            FInner := bvRaised;
            FOuter := bvLowered;
          end;
        bsRaised:
          begin
            FInner := bvLowered;
            FOuter := bvRaised;
          end;
      end;

      // And now we adjust.
      case FShape of
        bsBox:
          begin
            FInner := bvNone;
            case Style of
              bsLowered:
                FOuter := bvLowered;
              bsRaised:
                FOuter := bvRaised;
            end;
          end;
        bsTopLine:
          begin
            FEdges := [beTop];
          end;
        bsBottomLine:
          begin
            FEdges := [beBottom];
          end;
        bsLeftLine:
          begin
            FEdges := [beLeft];
          end;
        bsRightLine:
          begin
            FEdges := [beRight];
          end;
        bsSpacer:
          begin
            FInner := bvSpace;
            FOuter := bvSpace;
            FEdges := [];
          end;
      end;
    end;

    Invalidate;
  end;
end;

procedure TJvBevel.SetStyle(const Value: TJvBevelStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    if FStyle <> bsCustomStyle then
    begin
      inherited Style := TBevelStyle(FStyle);

      // Set the other properties so that should the shape become
      // bsCustomShape, the rendering is the closest it can be to
      // the one done in the ancestor.
      // Those next few lines define the most common properties.
      case FStyle of
        bsLowered:
          begin
            FInner := bvRaised;
            FOuter := bvLowered;
          end;
        bsRaised:
          begin
            FInner := bvLowered;
            FOuter := bvRaised;
          end;
      end;

      // And now we adjust
      case Shape of
        bsBox:
          begin
            FInner := bvNone;
            case FStyle of
              bsLowered:
                FOuter := bvLowered;
              bsRaised:
                FOuter := bvRaised;
            end;
          end;
        bsSpacer:
          begin
            FInner := bvSpace;
            FOuter := bvSpace;
          end;
      end;
    end;
    Invalidate;
  end;
end;

procedure TJvBevel.SetVerticalLines(const Value: TJvBevelLines);
begin
  FVerticalLines.Assign(Value);
end;

//=== { TJvBevelLines } ======================================================

constructor TJvBevelLines.Create;
begin
  inherited Create;

  FStyle := bvLowered;
  FThickness := 1;
end;

procedure TJvBevelLines.Assign(Source: TPersistent);
begin
  if Source is TJvBevelLines then
  begin
    FCount := (Source as TJvBevelLines).Count;
    FStyle := (Source as TJvBevelLines).Style;
    FBold := (Source as TJvBevelLines).Bold;
    FThickness := (Source as TJvBevelLines).Thickness;
    FIgnoreBorder := (Source as TJvBevelLines).IgnoreBorder;
    DoChange;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TJvBevelLines.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Step', IgnoreValue, nil, False);
  Filer.DefineProperty('Origin', IgnoreValue, nil, False);

  inherited DefineProperties(Filer);
end;

procedure TJvBevelLines.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvBevelLines.IgnoreValue(Reader: TReader);
begin
  TReaderAccess(Reader).SkipValue;
end;

procedure TJvBevelLines.SetBold(const Value: Boolean);
begin
  if FBold <> Value then
  begin
    FBold := Value;
    DoChange;
  end;
end;

procedure TJvBevelLines.SetCount(const Value: Cardinal);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    DoChange;
  end;
end;

procedure TJvBevelLines.SetIgnoreBorder(const Value: Boolean);
begin
  if FIgnoreBorder <> Value then
  begin
    FIgnoreBorder := Value;
    DoChange;
  end;
end;

procedure TJvBevelLines.SetStyle(const Value: TBevelCut);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    DoChange;
  end;
end;

procedure TJvBevelLines.SetThickness(const Value: Byte);
begin
  if FThickness <> Value then
  begin
    FThickness := Value;
    DoChange;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
