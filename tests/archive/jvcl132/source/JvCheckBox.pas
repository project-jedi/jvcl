{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvCheckBox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvPropAutoSave, JVCLVer;

type
  TJvCheckBox = class(TCheckBox)
  private
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    FOver: Boolean;
    FAutoSave: TJvAutoSave;
    FAutoSize: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    FAssociated: TControl;
    procedure SetHotFont(const Value: TFont);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetAssociated(const Value: TControl);
  protected

    procedure SetAutoSize(Value: Boolean);  {$IFDEF Delphi6_Up} override;    {$ENDIF}
 
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    procedure Loaded; override;
    procedure Toggle; override;
    procedure Click; override;
    procedure SetChecked(Value: Boolean); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Associated:TControl read FAssociated write SetAssociated;
    property AutoSave: TJvAutoSave read FAutoSave write FAutoSave;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property Caption: TCaption read GetCaption write SetCaption;

    property OnMouseEnter: TNotifyEvent read FonMouseEnter write FonMouseEnter;
    property OnMouseLeave: TNotifyEvent read FonMouseLeave write FonMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FonCtl3DChanged write FonCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FonParentColorChanged write FonParentColorChanged;
    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
  end;

implementation

{**************************************************}

constructor TJvCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  FColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FAutoSave := TJvAutoSave.Create(Self);
  if Assigned(FAssociated) then
    FAssociated.Enabled := Checked;
end;

{**************************************************}

procedure TJvCheckBox.Toggle;
begin
  inherited;
  FAutoSave.SaveValue(Checked);
  if Assigned(FAssociated) then
    FAssociated.Enabled := Checked;
end;

{**************************************************}

procedure TJvCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_MULTILINE or BS_TOP;
end;

{**************************************************}

procedure TJvCheckBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;
{**************************************************}

procedure TJvCheckBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvCheckBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvCheckBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

destructor TJvCheckBox.Destroy;
begin
  FAutoSave.Free;
  FHotFont.Free;
  FFontSave.Free;
  inherited;
end;

{**************************************************}

procedure TJvCheckBox.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

{**************************************************}

procedure TJvCheckBox.Loaded;
var
  b: Boolean;
begin
  inherited;
  if FAutoSave.LoadValue(b) then
  begin
    Checked := b;
    if Assigned(FOnRestored) then
      FOnRestored(Self);
  end;
end;

{**************************************************}

function TJvCheckBox.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

{**************************************************}
procedure TJvCheckBox.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  SetCaption(Caption);
end;
{**************************************************}

procedure TJvCheckBox.SetCaption(const Value: TCaption);
begin
  inherited Caption := Value;
  if AutoSize then
  begin
    // (rom) TODO?
  end;
end;

{**************************************************}

procedure TJvCheckBox.SetAssociated(const Value: TControl);
begin
  FAssociated := Value;
  if Assigned(FAssociated) then
    FAssociated.Enabled := Checked;
end;

{**************************************************}

procedure TJvCheckBox.SetChecked(Value: Boolean);
begin
  inherited;
  if Assigned(FAssociated) then
    FAssociated.Enabled := Value;
end;

{**************************************************}

procedure TJvCheckBox.Click;
begin
  inherited;
  if Assigned(FAssociated) then
    FAssociated.Enabled := Checked;
end;

end.
