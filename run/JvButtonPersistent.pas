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

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvButtonPersistent;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  Classes, Graphics, Controls,
  JvTypes;

type
  TJvButtonPersistent = class(TPersistent)
  private
    FVisible: Boolean;
    FFlat: Boolean;
    FEnabled: Boolean;
    FCaption: string;
    FOnChanged: TNotifyEvent;
    FFontEffect: Boolean;
    FHint: string;
    FShowHint: Boolean;
    FFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetFontEffect(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetShowHint(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    procedure Changed;
  public
    procedure AssignTo(Sender: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    constructor Create;
    destructor Destroy; override;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Hint: string read FHint write SetHint;
    property HotTrack: Boolean read FFontEffect write SetFontEffect default True;
    property HotTrackFont: TFont read FFont write SetFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

uses
  StdCtrls, Buttons,
  JvSpeedButton, JvButton;

constructor TJvButtonPersistent.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FEnabled := True;
  FFlat := True;
  FVisible := True;
  FFontEffect := True;
end;

destructor TJvButtonPersistent.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

type
  TJvCustomButtonAccessProtected = class(TJvCustomButton);

procedure TJvButtonPersistent.AssignTo(Sender: TPersistent);
begin
  if Sender is TButton then
  begin
    TButton(Sender).Enabled := Enabled;
    TButton(Sender).Visible := Visible;
    TButton(Sender).Caption := Caption;
    TButton(Sender).Hint := Hint;
    TButton(Sender).ShowHint := ShowHint;
    if Sender is TJvCustomButton then
    begin
      TJvCustomButtonAccessProtected(Sender).HotTrack := HotTrack;
      TJvCustomButtonAccessProtected(Sender).HotTrackFont.Assign(FFont);
      TJvCustomButtonAccessProtected(Sender).HotTrackFontOptions := HotTrackFontOptions;
    end;
  end
  else
  if Sender is TSpeedButton then
  begin
    TSpeedButton(Sender).Enabled := Enabled;
    TSpeedButton(Sender).Visible := Visible;
    TSpeedButton(Sender).Caption := Caption;
    TSpeedButton(Sender).Hint := Hint;
    TSpeedButton(Sender).ShowHint := ShowHint;
    TSpeedButton(Sender).Flat := Flat;
    if Sender is TJvSpeedButton then
    begin
      TJvSpeedButton(Sender).HotTrack := HotTrack;
      TJvSpeedButton(Sender).HotTrackFont.Assign(FFont);
      TJvSpeedButton(Sender).HotTrackFontOptions := HotTrackFontOptions;
    end;
  end
  else
    inherited AssignTo(Sender);
end;

procedure TJvButtonPersistent.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvButtonPersistent.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetFontEffect(const Value: Boolean);
begin
  if FFontEffect <> Value then
  begin
    FFontEffect := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetShowHint(const Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    FShowHint := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TJvButtonPersistent.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    Changed;
  end;
end;

end.

