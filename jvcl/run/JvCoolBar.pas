{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCoolBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCoolBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JVCLVer;

type
  TJvCoolBar = class(TCoolBar)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure ParentColorChanged; dynamic;
    procedure Ctl3DChanged; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

constructor TJvCoolBar.Create(AOwner: TComponent);
begin
  // (rom) inherited moved up
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvCoolBar.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  Ctl3DChanged;
end;

procedure TJvCoolBar.Ctl3DChanged;
begin
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCoolBar.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJvCoolBar.MouseEnter(AControl: TControl);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCoolBar.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJvCoolBar.MouseLeave(AControl: TControl);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCoolBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  ParentColorChanged;
end;

procedure TJvCoolBar.ParentColorChanged;
begin
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

end.
