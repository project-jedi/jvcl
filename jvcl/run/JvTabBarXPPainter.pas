{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTabBarXPPainter.pas, released on 2007-05-07.

The Initial Developer of the Original Code is Valdir Stiebe Junior <valdir att dype dott com dott br>
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTabBarXPPainter;

{$I jvcl.inc}

interface

{$IFDEF JVCLThemesEnabled}

uses
  Windows, SysUtils, Classes, Graphics, JvTabBar;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvTabBarXPPainter = class(TJvTabBarModernPainter)
  private
    FFixedTabSize: Integer;
    procedure SetFixedTabSize(const Value: Integer);
  protected
    procedure DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect); override;
    procedure DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect); override;
    procedure DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect); override;
    procedure DrawMoveDivider(Canvas: TCanvas; Tab: TJvTabBarItem; MoveLeft: Boolean); override;
    function GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer; override;
    function GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize; override;
    function GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect): TRect; override;
  published
    property FixedTabSize: Integer read FFixedTabSize write SetFixedTabSize;
  end;

{$ENDIF JVCLThemesEnabled}

implementation

{$IFDEF JVCLThemesEnabled}

uses
  Math, JvThemes;

{ TJvTabBarXPPainter }

procedure TJvTabBarXPPainter.DrawBackground(Canvas: TCanvas;
  TabBar: TJvCustomTabBar; R: TRect);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(ttTabRoot);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
    inherited DrawBackground(Canvas, TabBar, R);
end;

procedure TJvTabBarXPPainter.DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect);
begin
  if not StyleServices.Enabled then
    inherited DrawDivider(Canvas, LeftTab, R);
end;

procedure TJvTabBarXPPainter.DrawMoveDivider(Canvas: TCanvas; Tab: TJvTabBarItem;
  MoveLeft: Boolean);
begin
  if not StyleServices.Enabled then
    inherited DrawMoveDivider(Canvas, Tab, MoveLeft);
end;

procedure TJvTabBarXPPainter.DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem;
  R: TRect);
var
  TabDetails, ButtonDetails: TThemedElementDetails;
  CloseRect, TextRect: TRect;
begin
  if StyleServices.Enabled then
  begin
    if Tab.Selected then
    begin
      ButtonDetails := StyleServices.GetElementDetails(twSmallCloseButtonNormal);
      TabDetails := StyleServices.GetElementDetails(ttTabItemSelected);
    end
    else if Tab.Hot then
    begin
      ButtonDetails := StyleServices.GetElementDetails(twSmallCloseButtonHot);
      TabDetails := StyleServices.GetElementDetails(ttTabItemHot);
    end
    else
    begin
      ButtonDetails := StyleServices.GetElementDetails(twSmallCloseButtonNormal);
      TabDetails := StyleServices.GetElementDetails(ttTabItemNormal);
    end;

    if Tab.Closing then
      ButtonDetails := StyleServices.GetElementDetails(twSmallCloseButtonPushed);
    StyleServices.DrawElement(Canvas.Handle, TabDetails, R);

    if (Tab.ImageIndex <> -1) and (Tab.GetImages <> nil) then
    begin
      Tab.GetImages.Draw(Canvas, R.Left + 4, R.Top + (R.Bottom - R.Top - Tab.GetImages.Height) div 2,
        Tab.ImageIndex, Tab.Enabled);
      Inc(R.Left, Tab.GetImages.Width + 2);
    end;

    TextRect := R;
    TextRect.Left := TextRect.Left + Tab.TabBar.Margin;
    if Tab.TabBar.CloseButton then
    begin
      CloseRect := GetCloseRect(Canvas, Tab, R);
      TextRect.Right := CloseRect.Left - 3;
    end
    else
      Dec(TextRect.Right, 3);
    {$IFDEF COMPILER16_UP}
    StyleServices.DrawText(Canvas.Handle, TabDetails, Tab.Caption, TextRect, [tfSingleLine, tfVerticalCenter, tfWordEllipsis]);
    {$ELSE}
    StyleServices.DrawText(Canvas.Handle, TabDetails, Tab.Caption, TextRect, DT_SINGLELINE or DT_VCENTER or DT_WORD_ELLIPSIS, 0);
    {$ENDIF COMPILER16_UP}

    if Tab.TabBar.CloseButton then
      StyleServices.DrawElement(Canvas.Handle, ButtonDetails, CloseRect);
  end
  else
    inherited DrawTab(Canvas, Tab, R);
end;

function TJvTabBarXPPainter.GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem;
  R: TRect): TRect;
begin
  if StyleServices.Enabled then
  begin
    Result.Right := R.Right - 5;
    Result.Top := R.Top + ((R.Bottom div 2) - 8);
    Result.Left := Result.Right - 15;
    Result.Bottom := Result.Top + 15;
  end
  else
    Result := inherited GetCloseRect(Canvas, Tab, R);
end;

function TJvTabBarXPPainter.GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer;
begin
  if StyleServices.Enabled then
    Result := 1
  else
    Result := inherited GetDividerWidth(Canvas, LeftTab);
end;

function TJvTabBarXPPainter.GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize;
begin
  if FixedTabSize > 0 then
  begin
    if StyleServices.Enabled then
      Result.cx := FixedTabSize
    else
      Result.cx := Min(FixedTabSize + 40, Canvas.TextWidth(Tab.Caption) + 26);
  end
  else
  begin
    if StyleServices.Enabled then
    begin
      Result.cx := Canvas.TextWidth(Tab.Caption) + 16;
      if (Tab.ImageIndex <> -1) and (Tab.GetImages <> nil) then
        Inc(Result.cx, Tab.GetImages.Width + 2);
      if Tab.TabBar.CloseButton then
        Inc(Result.cx, 18);
    end
    else
      Result := inherited GetTabSize(Canvas, Tab);
  end;
  Result.cy := Tab.TabBar.Height - 3;
end;

procedure TJvTabBarXPPainter.SetFixedTabSize(const Value: Integer);
begin
  if Value <> FixedTabSize then
  begin
    FFixedTabSize := Value;
    Changed;
  end;
end;
{$ENDIF JVCLThemesEnabled}

end.