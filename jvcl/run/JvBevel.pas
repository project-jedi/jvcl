{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBevel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvBevel;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, ExtCtrls, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QExtCtrls, QControls, QForms,
  {$ENDIF VisualCLX}
  JvThemes, JvExExtCtrls;

type
  TJvBevel = class(TJvExBevel)
  private
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF VCL}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

constructor TJvBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
end;

procedure TJvBevel.MouseEnter(AControl: TControl);
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

procedure TJvBevel.MouseLeave(AControl: TControl);
begin
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
    inherited MouseLeave(AControl);
  end;
end;

procedure TJvBevel.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

end.

