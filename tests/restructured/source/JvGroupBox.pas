{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvGroupBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvGroupBox = class(TGroupBox)
  private
    { Private declarations }
    FOnHotkey: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FColor, FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FPropagate: Boolean;
    procedure SetPropagate(const Value: Boolean);
    procedure CMDialogChar(var msg: TCMDialogChar);
      message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMsg); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMsg); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMsg); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMsg); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMsg); message CM_PARENTCOLORCHANGED;
  protected
    { Protected declarations }
    procedure DoHotkey; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property PropagateEnable: Boolean read FPropagate write SetPropagate default False;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnHotkey: TNotifyEvent read FOnHotkey write FOnHotkey;
  end;

implementation

{ TJvGroupBox }

procedure TJvGroupBox.CMCtl3DChanged(var Msg: TMsg);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvGroupBox.CMDialogChar(var msg: TCMDialogChar);
begin
  inherited;
  if msg.result <> 0 then
    DoHotkey;
end;

procedure TJvGroupBox.CMEnabledChanged(var Msg: TMsg);
var
  i: Integer;
begin
  inherited;
  if PropagateEnable then
    for i := 0 to ControlCount - 1 do
      Controls[i].Enabled := Enabled;
end;

procedure TJvGroupBox.CMMouseEnter(var Msg: TMsg);
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

procedure TJvGroupBox.CMMouseLeave(var Msg: TMsg);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvGroupBox.CMParentColorChanged(var Msg: TMsg);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

constructor TJvGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  FPropagate := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvGroupBox.DoHotkey;
begin
  if Assigned(FOnHotkey) then
    FOnHotkey(self);
end;

procedure TJvGroupBox.SetPropagate(const Value: Boolean);
var
  i: Integer;
begin
  FPropagate := Value;
  for i := 0 to ControlCount - 1 do
    Controls[i].Enabled := Enabled;
end;

end.

