{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShape.PAS, released on 2001-02-28.

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

unit JvShape;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Messages, Graphics, Controls, ExtCtrls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QExtCtrls, QForms,
  {$ENDIF VisualCLX}
  JvExExtCtrls;

type
  TJvShape = class(TJvExShape)
  private
    FHintColor: TColor;
    FSaved: TColor;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property Constraints;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter;
    property OnMouseLeave;
    {$IFDEF VCL}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF COMPILER6_UP}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF COMPILER6_UP}
    property OnResize;
    property OnStartDrag;
  end;

implementation

constructor TJvShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
end;

procedure TJvShape.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvShape.MouseEnter(AControl: TControl);
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

procedure TJvShape.MouseLeave(AControl: TControl);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
    inherited MouseLeave(AControl);
  end;
end;

end.
