{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizardRouteMapNodes.PAS, released on 2002-02-05.

The Initial Developer of the Original Code is Steve Forbes.
Portions created by Steve Forbes are Copyright (C) 2002 Steve Forbes.
All Rights Reserved.

Contributor(s):
Peter Thörnqvist - converted to JVCL naming conventions on 2003-07-11
S Steed. - added AllowClickableNodes property

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Nodes style route map for TJvWizardRouteMap

History:
10/14/2003
  Added option to allow user to turn off the clicking of the nodes
  during runtime. S Steed.
05/02/2002
  Initial create

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWizardRouteMapNodes;

{$I jvcl.inc}

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWindows, QGraphics, QStdCtrls, Types,
  {$ENDIF VisualCLX}
  JvWizard;

type
  TJvWizardRouteMapNodes = class;

  TJvWizardRouteMapNodeColors = class(TPersistent)
  private
    FSelected: TColor;
    FUnselected: TColor;
    FDisabled: TColor;
    FLine: TColor;
    FRouteMap: TJvWizardRouteMapNodes;
  protected
    procedure SetLine(Value: TColor);
    procedure SetSelected(Value: TColor);
    procedure SetUnselected(Value: TColor);
    procedure SetDisabled(Value: TColor);
    procedure Changed;
  public
    constructor Create(ARouteMap: TJvWizardRouteMapNodes);
  published
    property Selected: TColor read FSelected write SetSelected default clLime;
    property Unselected: TColor read FUnselected write SetUnselected default clWhite;
    property Line: TColor read FLine write SetLine default clBtnShadow;
    property Disabled: TColor read FDisabled write SetDisabled default clBtnFace;
  end;

  TJvWizardRouteMapNodes = class(TJvWizardRouteMapControl)
  private
    FItemHeight: Integer;
    FUsePageTitle: Boolean;
    FNodeColors: TJvWizardRouteMapNodeColors;
    FIndent: Integer;
    FAllowClickableNodes: Boolean;
    procedure SetItemHeight(Value: Integer);
    procedure SetUsePageTitle(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetAllowClickableNodes(const Value: Boolean);
  protected
    function PageAtPos(Pt: TPoint): TJvWizardCustomPage; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 20;
    property AllowClickableNodes: Boolean read FAllowClickableNodes write SetAllowClickableNodes default True; // ss 10/14/2003
    property Align;
    property Color default clBackground;
    property Font;
    property Indent: Integer read FIndent write SetIndent default 8;
    property NodeColors: TJvWizardRouteMapNodeColors read FNodeColors write FNodeColors;
    property UsePageTitle: Boolean read FUsePageTitle write SetUsePageTitle default True;
    property OnDisplaying;
  end;

implementation

//=== { TJvWizardRouteMapNodeColors } ========================================

constructor TJvWizardRouteMapNodeColors.Create(ARouteMap: TJvWizardRouteMapNodes);
begin
  inherited Create;
  FRouteMap := ARouteMap;
  FSelected := clLime;
  FUnselected := clWhite;
  FLine := clBtnShadow;
  FDisabled := clBtnFace;
end;

procedure TJvWizardRouteMapNodeColors.Changed;
begin
  if Assigned(FRouteMap) then
    FRouteMap.Invalidate;
end;

procedure TJvWizardRouteMapNodeColors.SetDisabled(Value: TColor);
begin
  if FDisabled <> Value then
  begin
    FDisabled := Value;
    Changed;
  end;
end;

procedure TJvWizardRouteMapNodeColors.SetLine(Value: TColor);
begin
  if FLine <> Value then
  begin
    FLine := Value;
    Changed;
  end;
end;

procedure TJvWizardRouteMapNodeColors.SetSelected(Value: TColor);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Changed;
  end;
end;

procedure TJvWizardRouteMapNodeColors.SetUnselected(Value: TColor);
begin
  if FUnselected <> Value then
  begin
    FUnselected := Value;
    Changed;
  end;
end;

//=== { TJvWizardRouteMapNodes } =============================================

constructor TJvWizardRouteMapNodes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemHeight := 20;
  Color := clBackground;
  Font.Color := clWhite;
  FUsePageTitle := True;
  FIndent := 8;
  FAllowClickableNodes := True; // ss 10/14/2003
  FNodeColors := TJvWizardRouteMapNodeColors.Create(Self);
end;

destructor TJvWizardRouteMapNodes.Destroy;
begin
  FNodeColors.Free;
  inherited Destroy;
end;

function TJvWizardRouteMapNodes.PageAtPos(Pt: TPoint): TJvWizardCustomPage;
var
  I, Count: Integer;
  ARect: TRect;
begin
  if AllowClickableNodes then // ss 10/14/2003
  begin
    ARect := ClientRect;
    InflateRect(ARect, -1, -1);
    if PtInRect(ARect, Pt) then
    begin
      Count := PageCount;
      ARect := Bounds(ARect.Left, ARect.Top + Trunc((FItemHeight - 12) / 2),
        ARect.Right - ARect.Left, FItemHeight);
      I := 0;
      while I < Count do
      begin
        if CanDisplay(Pages[I]) then
        begin
          if PtInRect(ARect, Pt) then
          begin
            Result := Pages[I];
            Exit;
          end;
          OffsetRect(ARect, 0, FItemHeight);
        end;
        Inc(I);
      end;
    end;
  end;
  Result := nil;
end;

procedure TJvWizardRouteMapNodes.Paint;
var
  ARect, ATextRect, NodeRect: TRect;
  I: Integer;
  AColor: TColor;
  AFont: TFont;
  IsFirstPage, IsLastPage: Boolean;
begin
  ARect := ClientRect;
  with Canvas do
  begin
    Brush.Color := Color;
    Brush.Style := bsSolid;
    Pen.Color := clBtnShadow;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    InflateRect(ARect, -1, -1);
    AFont := TFont.Create;
    try
      AFont.Assign(Self.Font);
      ARect := Bounds(ARect.Left + FIndent, ARect.Top + FIndent,
        ARect.Right - ARect.Left - FIndent, FItemHeight);
      for I := 0 to PageCount - 1 do
      begin
        IsFirstPage := Wizard.IsFirstPage(Pages[I], not (csDesigning in ComponentState));
        IsLastPage := Wizard.IsLastPage(Pages[I], not (csDesigning in ComponentState));
        if CanDisplay(Pages[I]) then
        begin
          AColor := Color;
          if I = PageIndex then
          begin
            AFont.Color := Self.Font.Color;
            AFont.Style := AFont.Style + [fsBold]
          end
          else
          if not Pages[I].Enabled then
          begin
            AFont.Color := clBtnShadow;
            AFont.Style := AFont.Style - [fsBold];
          end
          else
          if not Pages[I].EnableJumpToPage then  // Nonn...
          begin
            AFont.Color := NodeColors.Disabled;
            AFont.Style := AFont.Style - [fsBold];    // ... Nonn
          end
          else
          begin
            AFont.Color := Self.Font.Color;
            AFont.Style := AFont.Style - [fsBold]
          end;

          ATextRect := ARect;
          if not (IsFirstPage or IsLastPage) then
            ATextRect.Left := ATextRect.Left + 18;

          NodeRect := ATextRect;
          NodeRect.Right := NodeRect.Left + 12;
          NodeRect.Top := NodeRect.Top + Trunc((FItemHeight - 12) / 2);
          NodeRect.Bottom := NodeRect.Top + 12;

          if not (IsFirstPage or IsLastPage) then
            ATextRect.Left := ATextRect.Left + 20
          else
            ATextRect.Left := ATextRect.Left + 18 + 20;

          try
            Pen.Color := FNodeColors.Line;
            if I = PageIndex then
              Brush.Color := FNodeColors.Selected
            else
            if not Pages[I].EnableJumpToPage then  // Nonn
              Brush.Color := FNodeColors.Disabled       // Nonn
            else
            if Pages[I].Enabled then
              Brush.Color := FNodeColors.Unselected
            else
              Brush.Color := FNodeColors.Disabled;
            Rectangle(NodeRect.Left, NodeRect.Top, NodeRect.Right,
              NodeRect.Bottom);

            Brush.Color := FNodeColors.Line;
            if IsFirstPage or IsLastPage then
            begin
              MoveTo(NodeRect.Right, NodeRect.Top + 5);
              LineTo(NodeRect.Right + 13, NodeRect.Top + 5);
              MoveTo(NodeRect.Right, NodeRect.Top + 6);
              LineTo(NodeRect.Right + 13, NodeRect.Top + 6);
              if IsFirstPage then
              begin
                MoveTo(NodeRect.Right + 11, NodeRect.Top + 6);
                LineTo(NodeRect.Right + 11, ATextRect.Bottom);
                MoveTo(NodeRect.Right + 12, NodeRect.Top + 6);
                LineTo(NodeRect.Right + 12, ATextRect.Bottom);
              end
              else
              begin
                MoveTo(NodeRect.Right + 11, NodeRect.Top + 5);
                LineTo(NodeRect.Right + 11, ATextRect.Top);
                MoveTo(NodeRect.Right + 12, NodeRect.Top + 5);
                LineTo(NodeRect.Right + 12, ATextRect.Top);
              end;
            end
            else
            begin
              MoveTo(NodeRect.Left + 5, NodeRect.Top);
              LineTo(NodeRect.Left + 5, ATextRect.Top - 1);
              MoveTo(NodeRect.Left + 6, NodeRect.Top);
              LineTo(NodeRect.Left + 6, ATextRect.Top - 1);
              MoveTo(NodeRect.Left + 5, NodeRect.Bottom);
              LineTo(NodeRect.Left + 5, ATextRect.Bottom + 1);
              MoveTo(NodeRect.Left + 6, NodeRect.Bottom);
              LineTo(NodeRect.Left + 6, ATextRect.Bottom + 1);
            end;

            Brush.Color := AColor;
            FillRect(ATextRect);
            Brush.Style := bsClear;
            Font.Assign(AFont);

            if FUsePageTitle then
              DrawText(Canvas.Handle,
                PChar((Pages[I] as TJvWizardCustomPage).Header.Title.Text), -1,
                ATextRect, DT_LEFT or DT_SINGLELINE or DT_VCENTER)
            else
              {$IFDEF VCL}
              DrawText(Canvas.Handle, PChar(Pages[I].Caption), -1, ATextRect,
                DT_LEFT or DT_SINGLELINE or DT_VCENTER);
              {$ENDIF VCL}
              {$IFDEF VisualCLX}
              DrawText(Canvas, Pages[I].Caption, -1, ATextRect,
                DT_LEFT or DT_SINGLELINE or DT_VCENTER);
              {$ENDIF VisualCLX}

          finally
            OffsetRect(ARect, 0, FItemHeight);
          end;
        end;
      end;
    finally
      AFont.Free;
    end;
  end;
end;

procedure TJvWizardRouteMapNodes.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapNodes.SetUsePageTitle(Value: Boolean);
begin
  if FUsePageTitle <> Value then
  begin
    FUsePageTitle := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapNodes.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapNodes.SetAllowClickableNodes(
  const Value: Boolean);
begin
  if FAllowClickableNodes <> Value then
  begin
    FAllowClickableNodes := Value;
    Invalidate;
  end;
end;

end.

