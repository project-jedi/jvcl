{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnimTitle.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAnimTitle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms,
  JvComponent;

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
    FBlinker: Integer;
    FBlinked: Integer;
    FBlinking: Boolean;
    procedure ChangeTitle(NewTitle: string);
    procedure EnableChange(NewEnable: Boolean);
    procedure ChangeDelay(NewDelay: Integer);
    procedure AnimateTitle(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read FInitialTitle write ChangeTitle;
    property Enabled: Boolean read FEnable write EnableChange default False;
    property Delay: Integer read FDelay write ChangeDelay default 50;
    property Blink: Integer read FBlinker write FBlinker default 5;
  end;

implementation

constructor TJvAnimTitle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnable := False;
  FDelay := 50;
  FBlinker := 5;
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
  inherited;
end;

procedure TJvAnimTitle.AnimateTitle(Sender: TObject);
begin
  if FBlinking then
  begin
    if FForm.Caption = FInitialTitle then
      FForm.Caption := ''
    else
    begin
      FForm.Caption := FInitialTitle;
      Inc(FBlinked);
      if FBlinked >= FBlinker then
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
        if FBlinker > 0 then
          FBlinking := True;
      end
      else
        FTitle := FTitle + FInitialTitle[Length(FTitle) + 1];
    end
    else
    if Length(FTitle) = 0 then
      FSens := True
    else
      FTitle := Copy(FTitle, 0, Length(FTitle) - 1);
    FForm.Caption := FTitle;
  end;
end;

procedure TJvAnimTitle.ChangeTitle(NewTitle: string);
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
