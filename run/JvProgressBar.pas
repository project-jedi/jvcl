{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvProgressBar;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls, CommCtrl,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QComCtrls,
  {$ENDIF VisualCLX}
  JvExComCtrls;

type
  TJvProgressBar = class(TJvExProgressBar)
  private
  {$IFDEF VCL}
    FFillColor: TColor;
    procedure SetFillColor(const Value: TColor);
  protected
    procedure CreateWnd; override;
  {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
  published
    {$IFDEF VCL}
    property FillColor: TColor read FFillColor write SetFillColor default clHighlight;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property FillColor default clHighlight;
    {$ENDIF VisualCLX}
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

constructor TJvProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillColor := clHighlight;
end;

{$IFDEF VCL}

procedure TJvProgressBar.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FFillColor));
end;

procedure TJvProgressBar.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    if HandleAllocated then
    begin
      SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FFillColor));
      // (rom) Invalidate is not good enough
      Repaint;
    end;
  end;
end;

{$ENDIF VCL}

end.

