{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizardRouteMapSteps.PAS, released on 2002-02-11.

The Initial Developer of the Original Code is Max Evans.
Portions created by Max Evans are Copyright (C) 2002 Max Evans

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Purpose:
  Step style route map for TJvWizardRouteMap

History:

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWizardRouteMapSteps;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Types, SysUtils, Classes, Graphics, Controls, Forms,
  JvWizard;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvWizardRouteMapSteps = class(TJvWizardRouteMapControl)
  private
    FIndent: Integer;
    FNextStepText: string;
    FActiveStepFormat: string;
    FPreviousStepText: string;
    FShowDivider: Boolean;
    FShowNavigators: Boolean;
    FShowNavigation: Boolean;
    FMultiline: Boolean;
    function GetActiveStepRect: TRect;
    function GetPreviousStepRect: TRect;
    function GetNextStepRect: TRect;
    function GetPreviousArrowRect: TRect;
    function GetNextArrowRect: TRect;
    procedure SetIndent(const Value: Integer);
    procedure SetNextStepText(const Value: string);
    procedure SetActiveStepFormat(const Value: string);
    procedure SetPreviousStepText(const Value: string);
    procedure SetShowDivider(const Value: Boolean);
    procedure SetShowNavigators(const Value: Boolean);
    function DetectPageCount(var ActivePageIndex: Integer): Integer; // Add by Yu Wei
    function DetectPage(const Pt: TPoint): TJvWizardCustomPage; // Add by Yu Wei
    function StoreActiveStepFormat: Boolean;
    function StoreNextStepText: Boolean;
    function StorePreviousStepText: Boolean;
    procedure SetShowNavigation(const Value: Boolean);
    procedure SetMultiline(const Value: Boolean);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function PageAtPos(Pt: TPoint): TJvWizardCustomPage; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color default clBackground;
    property Font;
    property Image;
    property Indent: Integer read FIndent write SetIndent default 5;
    property PreviousStepText: string read FPreviousStepText write SetPreviousStepText stored StorePreviousStepText;
    property ActiveStepFormat: string read FActiveStepFormat write SetActiveStepFormat stored StoreActiveStepFormat;
    property Multiline: Boolean read FMultiline write SetMultiline default False;
    property NextStepText: string read FNextStepText write SetNextStepText stored StoreNextStepText;
    property ShowDivider: Boolean read FShowDivider write SetShowDivider default True;
    property ShowNavigators: Boolean read FShowNavigators write SetShowNavigators default True;
    property ShowNavigation: Boolean read FShowNavigation write SetShowNavigation default True;
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
  JvResources;

constructor TJvWizardRouteMapSteps.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndent := 5;
  Color := clBackground;
  Font.Color := clWhite;
  FPreviousStepText := RsBackTo;
  FActiveStepFormat := RsActiveStepFormat;
  FNextStepText := RsNextStep;
  FShowDivider := True;
  FShowNavigators := True;
  FShowNavigation := True;
end;

function TJvWizardRouteMapSteps.DetectPage(const Pt: TPoint): TJvWizardCustomPage;
begin
  if FShowNavigators then
  begin
    // Ignore all disabled pages at run time.
    if PtInRect(GetPreviousArrowRect, Pt) then
    begin
      if (PageIndex < Wizard.PageCount) and (PageIndex > 0) and
        not ((csDesigning in ComponentState) or (bkBack in Wizard.WizardPages[PageIndex].EnabledButtons)) then
        Result := nil
      else
        Result := Wizard.FindNextPage(PageIndex, -1, not (csDesigning in ComponentState));
    end
    else
      if PtInRect(GetNextArrowRect, Pt) then
      begin
        if (PageIndex < Wizard.PageCount) and (PageIndex > 0) and
          not ((csDesigning in ComponentState) or (bkNext in Wizard.WizardPages[PageIndex].EnabledButtons)) then
          Result := nil
        else
          Result := Wizard.FindNextPage(PageIndex, 1, not (csDesigning in ComponentState));
      end
      else
        Result := nil;
  end
  else
  begin
    Result := nil;
  end;
end;

function TJvWizardRouteMapSteps.GetActiveStepRect: TRect;
begin
  Result := Rect(Left + FIndent, (ClientHeight div 2 - Canvas.TextHeight('Wq')),
    Width, ClientHeight div 2);
end;

function TJvWizardRouteMapSteps.GetNextArrowRect: TRect;
begin
  Result := Rect(Left + FIndent, Height - Indent - 32, Left + FIndent + 16,
    (Height - FIndent) - 16);
end;

function TJvWizardRouteMapSteps.GetNextStepRect: TRect;
begin
  Result := Rect(Left + FIndent, Height - FIndent - 32, Width,
    Height - FIndent - 32 + Canvas.TextHeight('Wq'));
end;

function TJvWizardRouteMapSteps.DetectPageCount(var ActivePageIndex: Integer): Integer;
var
  I: Integer;
begin
  // Ignore all disabled pages at run time.
  ActivePageIndex := 0;
  Result := 0;
  for I := 0 to PageCount - 1 do
  begin
    if (csDesigning in ComponentState) or Pages[I].Enabled then
    begin
      if I <= PageIndex then
        Inc(ActivePageIndex);
      Inc(Result);
    end;
  end;
end;

function TJvWizardRouteMapSteps.GetPreviousArrowRect: TRect;
begin
  Result := Rect(Left + FIndent, Top + FIndent, Left + FIndent + 16,
    Top + FIndent + 16);
end;

function TJvWizardRouteMapSteps.GetPreviousStepRect: TRect;
begin
  Result := Rect(Left + FIndent, Top + FIndent, Width,
    Top + FIndent + Canvas.TextHeight('Wq'));
end;

procedure TJvWizardRouteMapSteps.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  APage: TJvWizardCustomPage;
begin
  inherited MouseMove(Shift, X, Y);
  if ShowNavigators and not (csDesigning in ComponentState) then
  begin
    Pt := Point(X, Y);
    if PtInRect(ClientRect, Pt) then
    begin
      APage := DetectPage(Pt);
      if Assigned(APage) then
        Screen.Cursor := crHandPoint
      else
        Screen.Cursor := crDefault;
    end
    else
      if Screen.Cursor = crHandPoint then
        Screen.Cursor := crDefault;
  end;
end;

function TJvWizardRouteMapSteps.PageAtPos(Pt: TPoint): TJvWizardCustomPage;
begin
  Result := DetectPage(Pt);
end;

procedure TJvWizardRouteMapSteps.Paint;
var
  LRect, TextRect, ArrowRect, DividerRect: TRect;
  ActivePageIndex, TotalPageCount: Integer;
  StepHeight: Integer;
  APage: TJvWizardCustomPage;
  S: string;
  LDrawProperties: Cardinal;
begin
  LRect := ClientRect;
  TotalPageCount := DetectPageCount(ActivePageIndex);
  Canvas.Brush.Color := Color;
  if HasPicture then
    Image.PaintTo(Canvas, LRect);

  TextRect := GetActiveStepRect;
  LRect := Classes.Rect(TextRect.TopLeft, TextRect.BottomRight);
  Canvas.Font.Assign(Font);
  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Style := bsClear;

  if Multiline then
  begin
    S := Pages[PageIndex].Caption;
    Canvas.Font.Style := [];
    StepHeight := DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
      DT_CALCRECT or DT_LEFT or DT_WORDBREAK);
    TextRect.Right := LRect.Right;
    OffsetRect(TextRect, 0, Round((-0.5) * StepHeight + Canvas.TextHeight('Wq')));
  end;

  Canvas.Font.Style := [fsBold];
  S := Format(ActiveStepFormat, [ActivePageIndex, TotalPageCount]);
  if Multiline then
  begin
    LDrawProperties := DT_LEFT or DT_WORDBREAK;
  end
  else
  begin
    LDrawProperties := DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
  end;
  StepHeight := DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
    LDrawProperties);

  // Display Active Page Description
  Canvas.Font.Style := [];
  OffsetRect(TextRect, 0, StepHeight);
  S := Pages[PageIndex].Caption;
  if Multiline then
  begin
    LDrawProperties := DT_LEFT or DT_WORDBREAK;
  end
  else
  begin
    LDrawProperties := DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER;
  end;
  DrawText(Canvas.Handle, PChar(S), Length(S), TextRect, LDrawProperties);

  Canvas.Font.Style := [];
  if Self.ShowDivider then
  begin
    SetRect(DividerRect, Left + Indent, TextRect.Bottom + 5, Width - Indent,
      TextRect.Bottom + 6);
    Windows.DrawEdge(Canvas.Handle, DividerRect, EDGE_RAISED, BF_FLAT or BF_BOTTOM);
  end;

  { do the previous step }

  // YW - Ignore all disabled pages at run time
  APage := Wizard.FindNextPage(PageIndex, -1, not (csDesigning in ComponentState));
  if Assigned(APage) and (PageIndex <> -1) and ShowNavigation then
  begin
    TextRect := GetPreviousStepRect;
    ArrowRect := GetPreviousArrowRect;
    Canvas.Font.Style := [];
    if ShowNavigators then
    begin
      if TextRect.Left + Indent + ArrowRect.Right - ArrowRect.Left < Width then
        OffsetRect(TextRect, ArrowRect.Right, 0);
      if (csDesigning in ComponentState) or (bkBack in Wizard.WizardPages[PageIndex].EnabledButtons) then
        DrawFrameControl(Canvas.Handle, ArrowRect, DFC_SCROLL,
          DFCS_SCROLLLEFT or DFCS_FLAT);
    end;

    S := PreviousStepText;
    StepHeight := DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
      DT_LEFT or DT_WORDBREAK or DT_END_ELLIPSIS);

    OffsetRect(TextRect, 0, StepHeight);
    S := APage.Caption;
    if Multiline then
    begin
      DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
        DT_CALCRECT or DT_LEFT or DT_WORDBREAK);
      TextRect.Right := LRect.Right;

      LDrawProperties := DT_LEFT or DT_WORDBREAK;
    end
    else
    begin
      LDrawProperties := DT_SINGLELINE or DT_LEFT or DT_END_ELLIPSIS or DT_VCENTER;
    end;
    DrawText(Canvas.Handle, PChar(S), Length(S), TextRect, LDrawProperties);
  end;

  { do the next step }

  // YW - Ignore all disabled pages at run time
  APage := Wizard.FindNextPage(PageIndex, 1, not (csDesigning in ComponentState));
  if Assigned(APage) and (PageIndex <> -1) and ShowNavigation then
  begin
    TextRect := GetNextStepRect;
    ArrowRect := GetNextArrowRect;
    Canvas.Font.Style := [];
    if ShowNavigators then
    begin
      OffsetRect(TextRect, ArrowRect.Right, 0);
      if (csDesigning in ComponentState) or (bkNext in Wizard.WizardPages[PageIndex].EnabledButtons) then
        DrawFrameControl(Canvas.Handle, ArrowRect, DFC_SCROLL,
          DFCS_SCROLLRIGHT or DFCS_FLAT);
    end;

    if Multiline then
    begin
      S := APage.Caption;
      StepHeight := DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
        DT_CALCRECT or DT_LEFT or DT_WORDBREAK);
      TextRect.Right := LRect.Right;
      OffsetRect(TextRect, 0, (-1) * StepHeight + Canvas.TextHeight('Wq'));
    end;

    S := NextStepText;
    StepHeight := DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
      DT_LEFT or DT_WORDBREAK);

    OffsetRect(TextRect, 0, StepHeight);
    S := APage.Caption;
    if Multiline then
    begin
      DrawText(Canvas.Handle, PChar(S), Length(S), TextRect,
        DT_CALCRECT or DT_LEFT or DT_WORDBREAK);
      TextRect.Right := LRect.Right;

      LDrawProperties := DT_LEFT or DT_WORDBREAK;
    end
    else
    begin
      LDrawProperties := DT_SINGLELINE or DT_LEFT or DT_END_ELLIPSIS or DT_VCENTER;
    end;
    DrawText(Canvas.Handle, PChar(S), Length(S), TextRect, LDrawProperties);
  end;
end;

procedure TJvWizardRouteMapSteps.SetShowDivider(const Value: Boolean);
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetIndent(const Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetMultiline(const Value: Boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetNextStepText(const Value: string);
begin
  if FNextStepText <> Value then
  begin
    FNextStepText := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetActiveStepFormat(const Value: string);
begin
  if FActiveStepFormat <> Value then
  begin
    FActiveStepFormat := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetPreviousStepText(const Value: string);
begin
  if FPreviousStepText <> Value then
  begin
    FPreviousStepText := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetShowNavigators(const Value: Boolean);
begin
  if FShowNavigators <> Value then
  begin
    if Screen.Cursor = crHandPoint then
      Screen.Cursor := crDefault;
    FShowNavigators := Value;
    Invalidate;
  end;
end;

procedure TJvWizardRouteMapSteps.SetShowNavigation(const Value: Boolean);
begin
  if Value <> FShowNavigation then
  begin
    FShowNavigation := Value;
    Invalidate;
  end;
end;

function TJvWizardRouteMapSteps.StoreActiveStepFormat: Boolean;
begin
  Result := ActiveStepFormat <> RsActiveStepFormat;
end;

function TJvWizardRouteMapSteps.StoreNextStepText: Boolean;
begin
  Result := NextStepText <> RsNextStep;
end;

function TJvWizardRouteMapSteps.StorePreviousStepText: Boolean;
begin
  Result := PreviousStepText <> RsBackTo;
end;

{$IFDEF UNITVERSIONING}

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
