{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnimTitle.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQAnimTitle;

{$I jvcl.inc}

interface

uses
  Classes, QControls, QExtCtrls, QForms,
  JvQComponent;

type
  TJvAnimTitle = class(TJvComponent)
  private
    FTimer: TTimer;
    FEnabled: Boolean;
    FTitle: string;
    FCurrentTitle: string;
    FDelay: Integer;
    FSens: Boolean;
    FForm: TCustomForm;
    FBlink: Integer;
    FBlinked: Integer;
    FBlinking: Boolean;
    procedure SetTitle(const NewTitle: string);
    procedure SetEnabled(NewEnable: Boolean);
    procedure SetDelay(NewDelay: Integer);
    procedure AnimateTitle(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read FTitle write SetTitle;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Delay: Integer read FDelay write SetDelay default 50;
    property Blink: Integer read FBlink write FBlink default 5;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvAnimTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := False;
  FDelay := 50;
  FBlink := 5;
  FForm := GetParentForm(TControl(AOwner));
  FTitle := FForm.Caption;
  FSens := True;
  FBlinking := False;
  FBlinked := 0;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := FEnabled;
  FTimer.Interval := FDelay;
  FTimer.OnTimer := AnimateTitle;
end;

destructor TJvAnimTitle.Destroy;
begin
  FTimer.Free;
  if not (csDestroying in FForm.ComponentState) then
    FForm.Caption := FTitle;
  inherited Destroy;
end;

procedure TJvAnimTitle.AnimateTitle(Sender: TObject);
begin
  if FBlinking then
  begin
    // (rom) this is a bad implementation better try to manipulate
    // (rom) the WM_GETTEXT and WM_SETTEXT to the Form window
    if FForm.Caption = Title then
      FForm.Caption := ''
    else
    begin
      FForm.Caption := Title;
      Inc(FBlinked);
      if FBlinked >= Blink then
      begin
        FBlinking := False;
        FBlinked := 0;
      end;
    end;
  end
  else
  begin
    if FSens then
    begin
      if Length(FCurrentTitle) = Length(Title) then
      begin
        FSens := False;
        if Blink > 0 then
          FBlinking := True;
      end
      else
        FCurrentTitle := FCurrentTitle + Title[Length(FCurrentTitle) + 1];
    end
    else
    if FCurrentTitle = '' then
      FSens := True
    else
      SetLength(FCurrentTitle, Length(FCurrentTitle) - 1);
    {$IFDEF UNIX}
    if FCurrentTitle = '' then
      FForm.Caption := ' '   // else caption becomes <1>
    else
    {$ENDIF UNIX}
    FForm.Caption := FCurrentTitle;
  end;
end;

procedure TJvAnimTitle.SetTitle(const NewTitle: string);
begin
  FTitle := NewTitle;
  FCurrentTitle := '';
  FSens := True;
end;

procedure TJvAnimTitle.SetEnabled(NewEnable: Boolean);
begin
  FEnabled := NewEnable;
  FTimer.Enabled := FEnabled;
end;

procedure TJvAnimTitle.SetDelay(NewDelay: Integer);
begin
  FDelay := NewDelay;
  FTimer.Interval := FDelay;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

