{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QToolWin.pas, released on 2004-01-12

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-12

Known Issues:
----------------------------------------------------------------------------}

unit QToolWin;

interface

uses
  SysUtils, Classes, Types, Qt, QGraphics, QControls, QExtCtrls, QWindows;

type
{ TToolWindow }

  TEdgeBorder = (ebLeft, ebTop, ebRight, ebBottom);
  TEdgeBorders = set of TEdgeBorder;

  TToolWindow = class(TCustomControl)
  private
    FEdgeBorders: TEdgeBorders;
    FEdgeInner: TEdgeStyle;
    FEdgeOuter: TEdgeStyle;
    FBorderWidth: TBorderWidth;
    procedure SetEdgeBorders(Value: TEdgeBorders);
    procedure SetEdgeInner(Value: TEdgeStyle);
    procedure SetEdgeOuter(Value: TEdgeStyle);
    procedure SetBorderWidth(const Value: TBorderWidth);
  protected
    procedure Paint; override;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    procedure UpdateControl; virtual;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth;
  public
    constructor Create(AOwner: TComponent); override;
    property EdgeBorders: TEdgeBorders read FEdgeBorders write SetEdgeBorders default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TEdgeStyle read FEdgeInner write SetEdgeInner default esRaised;
    property EdgeOuter: TEdgeStyle read FEdgeOuter write SetEdgeOuter default esLowered;
  end;

implementation

{ TToolWindow }

constructor TToolWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgeInner := esRaised;
  FEdgeOuter := esLowered;
  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
end;

procedure TToolWindow.UpdateControl;
var
  R: TRect;
begin
  Invalidate;
  R := ClientRect;
  AlignControls(Self, R);
end;


procedure TToolWindow.SetEdgeBorders(Value: TEdgeBorders);
begin
  if Value <> FEdgeBorders then
  begin
    FEdgeBorders := Value;
    UpdateControl;
  end;
end;

procedure TToolWindow.SetEdgeInner(Value: TEdgeStyle);
begin
  if Value <> FEdgeInner then
  begin
    FEdgeInner := Value;
    UpdateControl;
  end;
end;

procedure TToolWindow.SetEdgeOuter(Value: TEdgeStyle);
begin
  if Value <> FEdgeOuter then
  begin
    FEdgeOuter := Value;
    UpdateControl;
  end;
end;

function TToolWindow.GetClientOrigin: TPoint;
begin
  Result := inherited GetClientOrigin;
  Inc(Result.X, BorderWidth);
  Inc(Result.Y, BorderWidth);
end;

function TToolWindow.GetClientRect: TRect;
var
  EdgeSize: Integer;
begin
  Result := inherited GetClientRect;
  InflateRect(Result, -BorderWidth, -BorderWidth);
  EdgeSize := 0;
  if EdgeInner <> esNone then
    Inc(EdgeSize, 1);
  if EdgeOuter <> esNone then
    Inc(EdgeSize, 1);
  if ebLeft in FEdgeBorders then
    Inc(Result.Left, EdgeSize);
  if ebTop in FEdgeBorders then
    Inc(Result.Top, EdgeSize);
  if ebRight in FEdgeBorders then
    Dec(Result.Right, EdgeSize);
  if ebBottom in FEdgeBorders then
    Dec(Result.Bottom, EdgeSize);
end;

procedure TToolWindow.Paint;
const
  InnerStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
var
  R: TRect;
begin
  R := BoundsRect;
  OffsetRect(R, -R.Left, -R.Top);
  DrawEdge(Canvas.Handle, R, InnerStyles[FEdgeInner] or OuterStyles[FEdgeOuter],
    Byte(FEdgeBorders) or BF_ADJUST);
end;

procedure TToolWindow.SetBorderWidth(const Value: TBorderWidth);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    UpdateControl;
  end;
end;

end.
