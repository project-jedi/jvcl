{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollBar.PAS, released on 2001-02-28.

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

unit JvScrollBar;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Messages, Graphics, Controls, Forms, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QStdCtrls, Types,
  {$ENDIF VisualCLX}
  JvExStdCtrls;

type
  TJvScrollBar = class(TJvExScrollBar)
  private
    FHintColor: TColor;
    FSaved: TColor;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FOver: Boolean;
    procedure SetHotTrack(Value: Boolean);
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

constructor TJvScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  FHotTrack := False;
  // ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvScrollBar.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvScrollBar.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    {$IFDEF VCL}
    if HotTrack then
      Ctl3D := True;
    {$ENDIF VCL}
    FOver := True;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvScrollBar.MouseLeave(Control: TControl);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    {$IFDEF VCL}
    if HotTrack then
      Ctl3D := False;
    {$ENDIF VCL}
    FOver := False;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvScrollBar.SetHotTrack(Value: Boolean);
begin
  FHotTrack := Value;
  {$IFDEF VCL}
  if FHotTrack then
    Ctl3D := False;
  {$ENDIF VCL}  
end;

end.

