{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUpDown.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): Sebastien Buysse [sbuysse@buypin.com].

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvUpDown;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, CommCtrl, JVCLVer;

type
  TJvAlignButton = (abLeft, abRight, abNone);
  
  TJvUpDown = class(TUpDown)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FColor,FSaved:TColor;
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;
    FOnParentColorChanged : TNotifyEvent;
    FIncrement: Integer;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FAssociate: TWinControl;
    FHotTrack: Boolean;
    FAlignButton: TJvAlignButton;
    procedure MouseEnter(var msg:Tmessage); message CM_MOUSEENTER;
    procedure MouseLeave(var msg:Tmessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TMessage); message CM_ParentColorChangeD;
    function GetPosition: Integer;
    procedure SetIncrement(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure SetAssociate(const Value: TWinControl);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetAlignButton(const Value: TJvAlignButton);
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function AcceptPosition(Value: Integer):Boolean;
    function CanChange: Boolean; override;
  public
    constructor Create(AOwner: TComponent);override;
  published
    property AlignButton:TJvAlignButton read FAlignButton write SetAlignButton default abRight;
    property HintColor:TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter:TNotifyEvent read FonMouseEnter write FonMouseEnter;
    property OnMouseLeave:TNotifyEvent read FonMouseLeave write FonMouseLeave;
    property OnParentColorChange : TNotifyEvent read FonParentColorChanged write FonParentColorChanged;
    property Color;

    property Min:Integer read FMin write SetMin;
    property Max:Integer read FMax write SetMax default 100;
    property Increment:Integer read FIncrement write SetIncrement default 1;
    property Position:Integer read GetPosition write SetPosition;
    property Associate:TWinControl read FAssociate write SetAssociate;
    property HotTrack:Boolean read FHotTrack write SetHotTrack default false;

    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

const
  UDM_SETPOS32  =          (WM_USER+113);
  UDM_GETPOS32  =          (WM_USER+114);
  UDS_HOTTRACK  =          $0100;

{*****************************************************************}

constructor TJvUpDown.Create(AOwner: TComponent);
begin
  inherited;
  FMin := 0;
  FMax := 100;
  FColor := clInfoBk;
  FHotTrack := false;
  Controlstyle := Controlstyle + [csAcceptsControls];
  FAlignButton := abRight;
end;

{**************************************************}

procedure TJvUpDown.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FonParentColorChanged) then FonParentColorChanged(Self);
end;

{**************************************************}

procedure TJvUpDown.MouseEnter(var msg:Tmessage);
begin
  FSaved:=Application.HintColor;
  Application.HintColor:=FColor;
  if Assigned(FonMouseEnter) then
    FonMouseEnter(self);
end;

{**************************************************}

procedure TJvUpDown.MouseLeave(var msg:Tmessage);
begin
  Application.HintColor:=FSaved;
  if Assigned(FonMouseLeave) then
    FonMouseLeave(self);
end;

{**************************************************}

function TJvUpDown.GetPosition: Integer;
begin
  if HandleAllocated then
  begin
    Result := SendMessage(Handle, UDM_GETPOS32, 0, 0);
    FPosition := Result;
  end
  else
    Result := FPosition;
end;

{**************************************************}

procedure TJvUpDown.SetIncrement(const Value: Integer);
var
  AccelArray: array [0..0] of TUDAccel;
begin
  if Value <> FIncrement then
  begin
    FIncrement := Value;
    if HandleAllocated then
    begin
      SendMessage(Handle, UDM_GETACCEL, 1, Longint(@AccelArray));
      AccelArray[0].nInc := Value;
      SendMessage(Handle, UDM_SETACCEL, 1, Longint(@AccelArray));
    end;
  end;
end;

{**************************************************}

procedure TJvUpDown.SetMax(const Value: Integer);
begin
  if Value<>FMax then
  begin
    FMax := Value;
    if HandleAllocated then
      SendMessage(Handle, UDM_SETRANGE32, FMin, FMax);
  end;
end;

{**************************************************}

procedure TJvUpDown.SetMin(const Value: Integer);
begin
  if Value<>FMin then
  begin
    FMin := Value;
    if HandleAllocated then
      SendMessage(Handle, UDM_SETRANGE32, FMin, FMax);
  end;
end;

{**************************************************}

procedure TJvUpDown.SetPosition(const Value: Integer);
begin
  if Value <> FPosition then
    if AcceptPosition(Value) then
    begin
      FPosition := Value;
      if HandleAllocated then
        SendMessage(Handle, UDM_SETPOS32, 0, Value);
      if (FAssociate<>nil) and (FAssociate is TCustomEdit) then
        TCustomEdit(FAssociate).Text := IntToStr(FPosition);
    end;
end;

{**************************************************}

procedure TJvUpDown.CNNotify(var Message: TWMNotify);
begin
  with Message do
    if NMHdr^.code = UDN_DELTAPOS then
      if AcceptPosition(PNMUpDown(NMHdr).iPos+PNMUpDown(NMHdr).iDelta) then
        FPosition := PNMUpDown(NMHdr).iPos+PNMUpDown(NMHdr).iDelta;
end;

{**************************************************}

procedure TJvUpDown.SetAssociate(const Value: TWinControl);
begin
  FAssociate := Value;
  if HandleAllocated then
  begin
    if Value=nil then
      SendMessage(Handle, UDM_SETBUDDY, 0, 0)
    else
      SendMessage(Handle, UDM_SETBUDDY, Value.Handle, 0);
    if FAssociate is TCustomEdit then
      TCustomEdit(FAssociate).Text := IntToStr(FPosition);
  end;
end;

{**************************************************}

procedure TJvUpDown.CreateWnd;
begin
  inherited;
  SendMessage(Handle, UDM_SETRANGE32, FMin, FMax);
  SendMessage(Handle, UDM_SETPOS32, 0, FPosition);
  SetAssociate(FAssociate);
  Increment := 1;
end;

{**************************************************}

function TJvUpDown.AcceptPosition(Value: Integer): Boolean;
begin
  result := (Value>=Min) and ((Value<=Max) or (Max=0));
end;

{**************************************************}

procedure TJvUpDown.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    if FHotTrack then
      Style := Style or UDS_HOTTRACK;
    if (Style and UDS_ALIGNRIGHT)=UDS_ALIGNRIGHT then
      Style := Style - UDS_ALIGNRIGHT;
    if (Style and UDS_ALIGNLEFT)=UDS_ALIGNLEFT then
      Style := Style - UDS_ALIGNLEFT;
    case FAlignButton of
      abLeft:
        Style := Style or UDS_ALIGNLEFT;
      abRight:
        Style := Style or UDS_ALIGNRIGHT;
    end;
  end;
end;

{**************************************************}

procedure TJvUpDown.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  RecreateWnd;
end;

{**************************************************}

procedure TJvUpDown.SetAlignButton(const Value: TJvAlignButton);
begin
  FAlignButton := Value;
  RecreateWnd;
end;

{**************************************************}

function TJvUpDown.CanChange: Boolean;
begin
  Result := inherited CanChange;
  if result then
    if Assigned(Associate)
      and (Associate is TCustomEdit)
      and Assigned(Associate.Parent) then
      PostMessage(Associate.Parent.Handle,
        WM_COMMAND,
        MakeWParam(0, EN_CHANGE),
        Associate.handle);
end;

end.
