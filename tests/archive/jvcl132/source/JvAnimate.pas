{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnimate.PAS, released on 2001-02-28.

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

unit JvAnimate;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JVCLVer,
  ComCtrls;

type
  TJvAnimate = class(TAnimate)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure DoMouseEnter(var Msg: TMessage);
    procedure DoMouseLeave(var Msg: TMessage);
    procedure DoParentColorChange(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnResize;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

implementation

{*****************************************************************}

constructor TJvAnimate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{**************************************************}

procedure TJvAnimate.DoParentColorChange(var Msg: TMessage);
begin
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvAnimate.DoMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvAnimate.DoMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvAnimate.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  DoParentColorChange(Msg);
end;

{**************************************************}

procedure TJvAnimate.MouseEnter(var Msg: TMessage);
begin
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  DoMouseEnter(Msg);
end;

{**************************************************}

procedure TJvAnimate.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  DoMouseLeave(Msg);
end;

end.
