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

Last Modified: 2004-02-06

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvRadioGroup;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QForms, QStdCtrls, QExtCtrls,
  {$ENDIF VisualCLX}
  JvThemes, JvExControls, JvExExtCtrls;

type
  TJvRadioGroup = class(TJvExRadioGroup, IJvDenySubClassing)
  private
    FReadOnly: Boolean;
    FDrawFrame: Boolean;
    procedure SetDrawFrame(const Value: Boolean);
  protected
    procedure Paint; override;
    function CanModify: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HintColor;
    {$IFDEF JVCLThemesEnabledD56}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabledD56}
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property DrawFrame: Boolean read FDrawFrame write SetDrawFrame default True;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  Math;

constructor TJvRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawFrame := True;
  {$IFDEF JVCLThemesEnabledD56}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF JVCLThemesEnabledD56}
end;

procedure TJvRadioGroup.Paint;
{$IFDEF JVCLThemesEnabledD56}
var
  Details: TThemedElementDetails;
  R, CaptionRect: TRect;
{$ENDIF JVCLThemesEnabledD56}
begin
  {$IFDEF JVCLThemesEnabledD56}
  if DrawFrame and ThemeServices.ThemesEnabled then
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
  {$ENDIF JVCLThemesEnabledD56}
  if DrawFrame then
    inherited Paint;
end;

function TJvRadioGroup.CanModify: Boolean;
begin
  if FReadOnly then
    Result := False
  else
    Result := inherited CanModify;
end;

procedure TJvRadioGroup.SetDrawFrame(const Value: Boolean);
begin
  if Value <> FDrawFrame then
  begin
    FDrawFrame := Value;
    Invalidate;
  end;
end;

end.

