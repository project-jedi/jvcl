{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemo.PAS, released on 2001-02-28.

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

unit JvMemo;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, JVCLVer;

type
  TJvMemo = class(TMemo)
  private
    FEffect: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetEffect(const Value: Boolean);
  protected
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HotTrack: Boolean read FEffect write SetEffect default False;
    property HintColor: TColor read FColor write FColor default clInfoBk;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

implementation

{**************************************************}

constructor TJvMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FEffect := False;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{***********************************************}

procedure TJvMemo.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvMemo.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvMemo.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

{**************************************************}

procedure TJvMemo.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

{**************************************************}

procedure TJvMemo.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FEffect then
      Ctl3d := True;
    FOver := True;
  end;
  if Assigned(FonMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvMemo.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FEffect then
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FonMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvMemo.SetEffect(const Value: Boolean);
begin
  FEffect := Value;
  if FEffect then
    Ctl3d := False;
end;

end.
