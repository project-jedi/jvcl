{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRadioGroup.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvRadioGroup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  JvThemes, JvExControls, JvExExtCtrls;

type
  TJvRadioGroup = class(TJvExRadioGroup, IJvDenySubClassing)
  private
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
    FReadOnly: Boolean;
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
    {$IFDEF JVCLThemesEnabledD56}
    procedure Paint; override;
    {$ENDIF JVCLThemesEnabledD56}
    function CanModify: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    {$IFDEF JVCLThemesEnabledD56}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabledD56}
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

uses
  Math;

constructor TJvRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  FReadOnly := False;
  {$IFDEF JVCLThemesEnabledD56}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF JVCLThemesEnabledD56}
end;

{$IFDEF JVCLThemesEnabledD56}

procedure TJvRadioGroup.Paint;
var
  Details: TThemedElementDetails;
  R, CaptionRect: TRect;
begin
  if ThemeServices.ThemesEnabled then
  begin
    if Enabled then
      Details := ThemeServices.GetElementDetails(tbGroupBoxNormal)
    else
      Details := ThemeServices.GetElementDetails(tbGroupBoxDisabled);
    R := ClientRect;
    Inc(R.Top, Canvas.TextHeight('0') div 2);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);

    CaptionRect := Rect(8, 0, Min(Canvas.TextWidth(Caption) + 8, ClientWidth - 8), Canvas.TextHeight(Caption));

    Canvas.Brush.Color := Self.Color;
    DrawThemedBackground(Self, Canvas, CaptionRect);
    ThemeServices.DrawText(Canvas.Handle, Details, Caption, CaptionRect, DT_LEFT, 0);
  end
  else
    inherited Paint;
end;

{$ENDIF JVCLThemesEnabledD56}

function TJvRadioGroup.CanModify: Boolean;
begin
  if FReadOnly then
    Result := False
  else
    Result := inherited CanModify;
end;

procedure TJvRadioGroup.MouseEnter(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    FOver := True;
    inherited MouseEnter(AControl);
  end;
end;

procedure TJvRadioGroup.MouseLeave(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
    inherited MouseLeave(AControl);
  end;
end;

procedure TJvRadioGroup.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

end.

