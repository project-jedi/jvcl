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

The Original Code is: JvRadioGroup.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQRadioGroup;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages,
  SysUtils, Classes, QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QToolWin,
  JvQJCLUtils, JvQThemes, JvQExControls, JvQExExtCtrls;

type
  TJvRadioGroupHintEvent = procedure(Sender: TObject; Index: Integer;
    var AHint: TCaption) of object;

  TJvRadioGroup = class(TJvExRadioGroup, IJvDenySubClassing)
  private
    FReadOnly: Boolean;
    FEdgeBorders: TEdgeBorders;
    FEdgeInner: TEdgeStyle;
    FEdgeOuter: TEdgeStyle;
    FCaptionVisible: Boolean;
    FOnItemHint: TJvRadioGroupHintEvent;
    procedure SetEdgeBorders(const Value: TEdgeBorders);
    procedure SetEdgeInner(const Value: TEdgeStyle);
    procedure SetEdgeOuter(const Value: TEdgeStyle);
    procedure SetCaptionVisible(Value: Boolean);
  protected
    procedure Paint; override;
    function CanModify: Boolean; override;
    procedure GetItemHint(Index: Integer; var AHint: TCaption); virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CaptionVisible: Boolean read FCaptionVisible write SetCaptionVisible;
    property EdgeBorders: TEdgeBorders read FEdgeBorders write SetEdgeBorders default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TEdgeStyle read FEdgeInner write SetEdgeInner default esRaised;
    property EdgeOuter: TEdgeStyle read FEdgeOuter write SetEdgeOuter default esLowered;
    property HintColor; 
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnItemHint: TJvRadioGroupHintEvent read FOnItemHint write FOnItemHint;
    property OnParentColorChange;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;

constructor TJvRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  FEdgeInner := esRaised;
  FEdgeOuter := esLowered;
  FCaptionVisible := True; 
end;

procedure TJvRadioGroup.Paint;
const
  InnerStyles: array [TEdgeStyle] of Integer =
    (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array [TEdgeStyle] of Integer =
    (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
  Ctl3DStyles: array [Boolean] of Integer =
    (BF_MONO, 0);
var
  H: Integer;
  R: TRect;
  Flags: Longint; 
begin 
  with Canvas do
  begin
    Font := Self.Font;
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height);
    QWindows.DrawEdge(Handle, R, InnerStyles[FEdgeInner] or OuterStyles[FEdgeOuter],
      Byte(FEdgeBorders)  or BF_ADJUST);
    if (Text <> '') and CaptionVisible then
    begin
      if not UseRightToLeftAlignment then
        R := Rect(8, 0, 0, H)
      else
        R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);

      // (rom) unified VCL/VisualCLX version
      DrawText(Canvas, Text, Length(Text), R, Flags or DT_CALCRECT);
      Brush.Color := Color;
      SetBkMode(Handle, OPAQUE);
      DrawText(Canvas, Text, Length(Text), R, Flags);
    end;
  end;
end;

function TJvRadioGroup.CanModify: Boolean;
begin
  if FReadOnly then
    Result := False
  else
    Result := inherited CanModify;
end;

procedure TJvRadioGroup.SetEdgeBorders(const Value: TEdgeBorders);
begin
  if FEdgeBorders <> Value then
  begin
    FEdgeBorders := Value;
    Invalidate;
  end;
end;

procedure TJvRadioGroup.SetEdgeInner(const Value: TEdgeStyle);
begin
  if FEdgeInner <> Value then
  begin
    FEdgeInner := Value;
    Invalidate;
  end;
end;

procedure TJvRadioGroup.SetEdgeOuter(const Value: TEdgeStyle);
begin
  if FEdgeOuter <> Value then
  begin
    FEdgeOuter := Value;
    Invalidate;
  end;
end;

procedure TJvRadioGroup.SetCaptionVisible(Value: Boolean);
begin
  if FCaptionVisible <> Value then
  begin
    FCaptionVisible := Value;
    Invalidate;
  end;
end;

procedure TJvRadioGroup.GetItemHint(Index: Integer; var AHint: TCaption);
begin
  if Assigned(FOnItemHint) then
    FOnItemHint(Self, Index, AHint);
end;

function TJvRadioGroup.HintShow(var HintInfo: THintInfo): Boolean;
var
  AItemX, AItemY,
  AHeight, AWidth, VertCount: Integer;
  ARect: TRect;
begin
  Result := False;
  with HintInfo do
  begin
    ARect := ClientRect;
    HintStr := Hint; // set default
    if Items.Count > 0 then
    begin
      VertCount := (Items.Count div Columns) + Ord(Items.Count mod Columns <> 0);
      AHeight := Height div VertCount;
      AWidth  := Width div Columns;
      if (AHeight > 0) then
      begin
        AItemX := CursorPos.X div AWidth;
        AItemY := CursorPos.Y div AHeight;
        if AItemY + AItemX * VertCount< Items.Count then
        begin
          GetItemHint(AItemY + AItemX * VertCount, TCaption(HintStr));
          ARect := Rect(AItemX * AWidth, AHeight * AItemY,
            AItemX * AWidth + AWidth, AHeight * AItemY + AHeight);
        end;
      end;
      CursorRect := ARect;
    end;
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

