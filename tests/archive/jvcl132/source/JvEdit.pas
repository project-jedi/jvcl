{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEdit.PAS, released on 2001-02-28.

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

unit JvEdit;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Dialogs, Forms,
  JvPropAutoSave, JvMaxPixel, JVCLVer;

type
  TJvEdit = class(TEdit)
  private
    FAlignment: TAlignment;
    FAutoCtl3D: Boolean;
    FOver: Boolean;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FAutoSave: TJvAutoSave;
    FMaxPixel: TJvMaxPixel;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetAutoCtl3D(const Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure Change; override;
    procedure MaxPixelChanged(Sender: TObject);
  public
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    function IsEmpty: Boolean;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoSave: TJvAutoSave read FAutoSave write FAutoSave;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Align;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property HotTrack: Boolean read FAutoCtl3D write SetAutoCtl3D default False;
    property Modified;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property SelStart;
    property SelText;
    property SelLength;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
  end;

implementation

{***********************************************}

constructor TJvEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // (p3) bad hack! - fixed Change procedure (see below) 
//  Parent := TWinControl(AOwner);
  FColor := clInfoBk;
  FAutoctl3d := False;
  FOver := False;
  FAlignment := taLeftJustify;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FAutoSave := TJvAutoSave.Create(Self);
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
end;

{***********************************************}

procedure TJvEdit.Loaded;
var
  st: string;
begin
  inherited;
  if FAutoSave.LoadValue(st) then
  begin
    Text := st;
    if Assigned(FOnRestored) then
      FOnRestored(Self);
  end;
end;

{***********************************************}

destructor TJvEdit.Destroy;
begin
  FAutoSave.Free;
  FMaxPixel.Free;
  inherited;
end;

{***********************************************}

procedure TJvEdit.Change;
var
  st: string;
begin
  inherited;
  // (p3) must check this because Font may not be created here
  if not HasParent then Exit;
  st := Text;
  FMaxPixel.Test(st, Font);
  if st <> Text then
    Text := st;
  SelStart := Length(Text);
  FAutoSave.SaveValue(Text);
end;

{***********************************************}

procedure TJvEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Parent <> nil then
    case FAlignment of
      taLeftJustify:
        Params.Style := Params.Style or ES_LEFT;
      taRightJustify:
        Params.Style := Params.Style or ES_RIGHT;
      taCenter:
        Params.Style := Params.Style or ES_CENTER;
    end;
end;

{***********************************************}

procedure TJvEdit.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FAutoCtl3d then
      Ctl3d := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvEdit.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FAutoCtl3d then
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvEdit.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvEdit.SetAutoCtl3D(const Value: Boolean);
begin
  FAutoCtl3D := Value;
  if FAutoCtl3d then
    Ctl3d := False;
end;

{**************************************************}

function TJvEdit.IsEmpty: Boolean;
begin
  Result := Length(Caption) = 0;
end;

{***********************************************}

procedure TJvEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;

{***********************************************}

procedure TJvEdit.MaxPixelChanged(Sender: TObject);
var
  st: string;
begin
  st := Text;
  FMaxPixel.Test(st, Font);
  if st <> Text then
    Text := st;
  SelStart := Length(Text);
end;

end.
