{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButtonPersistent.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com]
                Dejoy

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvButtonPersistent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  Classes, Graphics, Controls,
  JvTypes, JvHotTrackPersistent;

type

  IJvControlProperty = interface(IInterface)
  ['{33316D29-0F86-41C4-8DB4-3FE9756158B5}']
    function GetCaption: string;
    function GetEnabled: Boolean;
    function GetFlat: Boolean;
    function GetHint: string;
    function GetShowHint: Boolean;
    function GetVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetShowHint(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure Assign(Source: IJvControlProperty);

    property Caption: string read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Flat: Boolean read GetFlat write SetFlat;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  TJvButtonPersistent = class(TJvCustomHotTrackPersistent
                              ,IJvControlProperty
                              )
  private
    FVisible: Boolean;
    FFlat: Boolean;
    FEnabled: Boolean;
    FCaption: string;
    FHint: string;
    FShowHint: Boolean;
    {IJvControlProperty}
    function GetCaption: string;
    function GetEnabled: Boolean;
    function GetFlat: Boolean;
    function GetHint: string;
    function GetShowHint: Boolean;
    function GetVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetShowHint(const Value: Boolean);

    procedure IJvControlProperty_Assign(Source: IJvControlProperty);
    procedure IJvControlProperty.Assign = IJvControlProperty_Assign;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Sender: TPersistent); override;

    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Caption: string read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Flat: Boolean read GetFlat write SetFlat default True;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property Visible: Boolean read GetVisible write SetVisible default True;

    property HotTrack default True;
    property HotTrackFont;
    property HotTrackFontOptions;
    property HotTrackOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, StdCtrls, Buttons;

constructor TJvButtonPersistent.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  HotTrack := True;
  FEnabled := True;
  FFlat := True;
  FVisible := True;
end;

destructor TJvButtonPersistent.Destroy;
begin
  inherited Destroy;
end;

procedure TJvButtonPersistent.Assign(Source: TPersistent);
var
  intf: IJvControlProperty;
begin
  if Supports(Source, IJvControlProperty, intf) then
    IJvControlProperty(Self).Assign(intf);
  inherited Assign(Source);
end;

procedure TJvButtonPersistent.AssignTo(Sender: TPersistent);
var
  intf : IJvHotTrack;
  intf2: IJvControlProperty;
begin
  if Sender is TButton then
  begin
    if Supports(Sender, IJvControlProperty, intf2) then
      intf2.Assign(Self)
    else
    begin
      TButton(Sender).Enabled := Enabled;
      TButton(Sender).Visible := Visible;
      TButton(Sender).Caption := Caption;
      TButton(Sender).Hint := Hint;
      TButton(Sender).ShowHint := ShowHint;
    end;
    if Supports(Sender, IJvHotTrack, intf) then
    begin
      intf.Assign(Self);
    end;
  end
  else
  if Sender is TSpeedButton then
  begin
    if Supports(Sender, IJvControlProperty, intf2) then
      intf2.Assign(Self)
    else  
    begin
      TSpeedButton(Sender).Enabled := Enabled;
      TSpeedButton(Sender).Visible := Visible;
      TSpeedButton(Sender).Caption := Caption;
      TSpeedButton(Sender).Hint := Hint;
      TSpeedButton(Sender).ShowHint := ShowHint;
      TSpeedButton(Sender).Flat := Flat;
    end;
    if Supports(Sender, IJvHotTrack, intf) then
    begin
      intf.Assign(Self);
    end;
  end
  else
    inherited AssignTo(Sender);
end;

function TJvButtonPersistent.GetCaption: string;
begin
  Result := FCaption;
end;

function TJvButtonPersistent.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TJvButtonPersistent.GetFlat: Boolean;
begin
  Result := FFlat;
end;

function TJvButtonPersistent.GetHint: string;
begin
  Result := FHint;
end;

function TJvButtonPersistent.GetShowHint: Boolean;
begin
  Result := FShowHint;
end;

function TJvButtonPersistent.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TJvButtonPersistent.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    Changing;
    ChangingProperty('Caption');
    FCaption := Value;
    ChangedProperty('Caption');
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    Changing;
    ChangingProperty('Enabled');
    FEnabled := Value;
    ChangedProperty('Enabled');
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    Changing;
    ChangingProperty('Flat');
    FFlat := Value;
    ChangedProperty('Flat');
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    Changing;
    ChangingProperty('Hint');
    FHint := Value;
    ChangedProperty('Hint');
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetShowHint(const Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    Changing;
    ChangingProperty('ShowHint');
    FShowHint := Value;
    ChangedProperty('ShowHint');
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    Changing;
    ChangingProperty('Visible');
    FVisible := Value;
    ChangedProperty('Visible');
    Changed;
  end;
end;

procedure TJvButtonPersistent.IJvControlProperty_Assign(
  Source: IJvControlProperty);
begin
  if (Source <> nil) and (IJvControlProperty(Self) <> Source) then
  begin
    BeginUpdate;
    try
      Caption := Source.Caption;
      Enabled := Source.Enabled;
      Flat := Source.Flat;
      Hint := Source.Hint;
      ShowHint := Source.ShowHint;
      Visible := Source.Visible;
    finally
       EndUpdate;
    end;   
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

