{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBouncingLabel.PAS, released on 2001-02-28.

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

unit JvBouncingLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  JvLabel;

type
  TJvBouncingLabel = class(TJvLabel)
  private
    FBouncing: Boolean;
    FTimer: TTimer;
    FDirection: Integer;
    FSpeed: Integer;
    FForm: TWinControl;
    FInterval: Cardinal;
    procedure Bounce(Sender: TObject);
    procedure SetBouncing(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bouncing: Boolean read FBouncing write SetBouncing default True;
    property Interval: Cardinal read FInterval write SetInterval default 20;
  end;

implementation

uses
  JvTypes;

resourcestring
  RC_MustBeWinControl = 'Owner must be of type TWinControl';

constructor TJvBouncingLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 20;
  FInterval := 20;
  FTimer.OnTimer := Bounce;
  FBouncing := True;
  FDirection := Random(4);
  FSpeed := Random(8);
  FTimer.Enabled := True;
  if AOwner is TWinControl then
    FForm := AOwner as TWinControl
  else
    raise EJVCLException.Create(RC_MustBeWinControl);
end;

destructor TJvBouncingLabel.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvBouncingLabel.Bounce(Sender: TObject);
begin
  if FSpeed = 0 then
    FSpeed := Random(8);
  case FDirection of
    0:
      begin //numpad1
        if (Left - FSpeed <= 0) or (Top + Height + FSpeed >= FForm.ClientHeight) then
        begin
          FDirection := Random(4);
          FSpeed := Random(8);
        end
        else
        begin
          Left := Left - FSpeed;
          Top := Top + FSpeed;
        end;
      end;
    1:
      begin
        if (Top + Height + FSpeed >= FForm.ClientHeight) or (Left + Width + FSpeed >=
          FForm.ClientWidth) then
        begin
          FDirection := Random(4);
          FSpeed := Random(8);
        end
        else
        begin
          Top := Top + FSpeed;
          Left := Left + FSpeed;
        end;
      end;
    2:
      begin
        if (Left - FSpeed <= 0) or (Top - FSpeed <= 0) then
        begin
          FDirection := Random(4);
          FSpeed := Random(8);
        end
        else
        begin
          Left := Left - FSpeed;
          Top := Top - FSpeed;
        end;
      end;
    3:
      begin
        if (Left + Width + FSpeed > FForm.ClientWidth) or (Top - FSpeed <= 0) then
        begin
          FDirection := Random(4);
          FSpeed := Random(8);
        end
        else
        begin
          Left := Left + FSpeed;
          Top := Top - FSpeed;
        end;
      end;
  end;
end;

procedure TJvBouncingLabel.SetBouncing(const Value: Boolean);
begin
  FBouncing := Value;
  FTimer.Enabled := Value;
end;

procedure TJvBouncingLabel.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
  FTimer.Interval := Value;
end;

end.

