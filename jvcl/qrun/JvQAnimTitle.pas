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

{$I jvcl.inc}

unit JvQAnimTitle;

interface

uses
  SysUtils, Classes,  
  QControls, QExtCtrls, QForms, 
  JvQComponent;

type
  TJvAnimTitle = class(TJvComponent)
  private
    FTimer: TTimer;
    FEnable: Boolean;
    FInitialTitle: string;
    FTitle: string;
    FDelay: Integer;
    FSens: Boolean;
    FForm: TCustomForm;
    FBlink: Integer;
    FBlinked: Integer;
    FBlinking: Boolean;
    procedure ChangeTitle(const NewTitle: string);
    procedure EnableChange(NewEnable: Boolean);
    procedure ChangeDelay(NewDelay: Integer);
    procedure AnimateTitle(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read FInitialTitle write ChangeTitle;
    property Enabled: Boolean read FEnable write EnableChange default False;
    property Delay: Integer read FDelay write ChangeDelay default 50;
    property Blink: Integer read FBlink write FBlink default 5;
  end;

implementation

constructor TJvAnimTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnable := False;
  FDelay := 50;
  FBlink := 5;
  FForm := GetParentForm(TControl(AOwner));
  FInitialTitle := FForm.Caption;
  FSens := True;
  FBlinking := False;
  FBlinked := 0;
  FDelay := 100;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := FEnable;
  FTimer.Interval := FDelay;
  FTimer.OnTimer := AnimateTitle;
end;

destructor TJvAnimTitle.Destroy;
begin
  FTimer.Free;
  if not (csDestroying in FForm.ComponentState) then
    FForm.Caption := FInitialTitle;
  inherited Destroy;
end;

procedure TJvAnimTitle.AnimateTitle(Sender: TObject);
begin
  if FBlinking then
  begin
    // (rom) this is a bad implementation better try to manipulate
    // (rom) the WM_GETTEXT and WM_SETTEXT to the Form window
    if FForm.Caption = FInitialTitle then
      FForm.Caption := ''
    else
    begin
      FForm.Caption := FInitialTitle;
      Inc(FBlinked);
      if FBlinked >= FBlink then
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
      if Length(FTitle) = Length(FInitialTitle) then
      begin
        FSens := False;
        if FBlink > 0 then
          FBlinking := True;
      end
      else
        FTitle := FTitle + FInitialTitle[Length(FTitle) + 1];
    end
    else
    if FTitle = '' then
      FSens := True
    else
      SetLength(FTitle, Length(FTitle) - 1);
    {$IFDEF LINUX}
    if FTitle = '' then
      FForm.Caption := ' '   // else caption becomes <1>
    else
    {$ENDIF LINUX}
    FForm.Caption := FTitle;
  end;
end;

procedure TJvAnimTitle.ChangeTitle(const NewTitle: string);
begin
  FInitialTitle := NewTitle;
  FTitle := '';
  FSens := True;
end;

procedure TJvAnimTitle.EnableChange(NewEnable: Boolean);
begin
  FEnable := NewEnable;
  FTimer.Enabled := FEnable;
end;

procedure TJvAnimTitle.ChangeDelay(NewDelay: Integer);
begin
  FDelay := NewDelay;
  FTimer.Interval := FDelay;
end;

end.

