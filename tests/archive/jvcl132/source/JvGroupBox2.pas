{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGroupBox2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvGroupBox2;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, JVCLVer;

type
  TJvGroupBox2 = class(TGroupBox)
  private
    FOnMouseEnter: TNotifyEvent;
    FColor, FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FPropage: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetPropage(const Value: Boolean);
  protected
    procedure CMEnabledChanged(var Msg: TMsg); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMsg); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMsg); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMsg); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMsg); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property PropagateEnabled: Boolean read FPropage write SetPropage default False;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

{**************************************************}

constructor TJvGroupBox2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  FPropage := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{**************************************************}

procedure TJvGroupBox2.CMCtl3DChanged(var Msg: TMsg);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvGroupBox2.CMParentColorChanged(var Msg: TMsg);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvGroupBox2.CMMouseEnter(var Msg: TMsg);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvGroupBox2.CMMouseLeave(var Msg: TMsg);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvGroupBox2.CMEnabledChanged(var Msg: TMsg);
var
  i: Integer;
begin
  inherited;
  if PropagateEnabled then
    for i := 0 to ControlCount - 1 do
      Controls[i].Enabled := Enabled;
end;

{**************************************************}

procedure TJvGroupBox2.SetPropage(const Value: Boolean);
var
  i: Integer;
begin
  FPropage := Value;
  for i := 0 to ControlCount - 1 do
    Controls[i].Enabled := Enabled;
end;

end.

