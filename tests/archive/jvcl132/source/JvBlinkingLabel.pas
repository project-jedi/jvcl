{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBlinkingLabel.PAS, released on 2001-02-28.

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

unit JvBlinkingLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, JvLabel;

type
  TJvBlinkingLabel = class(TJvLabel)
  private
    FBlinking: Boolean;
    FTimer: TTimer;
    FDelay: Cardinal;
    FBlink: Integer;
    procedure SetBlinking(Value: Boolean);
    procedure SetDelay(Value: Cardinal);
    procedure OnBlink(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Blinking: Boolean read FBlinking write SetBlinking default True;
    property BlinkingDelay: Cardinal read FDelay write SetDelay default 400;
    property BlinkingTime: Integer read FBlink write FBlink default 200;
  end;

implementation

{**************************************************}

constructor TJvBlinkingLabel.Create(AOwner: TComponent);
begin
  FBlinking := True;
  FDelay := 400;
  FBlink := 200;
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := OnBlink;
  FTimer.Interval := FDelay;
  FTimer.Enabled := FBlinking;
end;

{**************************************************}

destructor TJvBlinkingLabel.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  inherited;
end;

{**************************************************}

procedure TJvBlinkingLabel.SetBlinking(Value: Boolean);
begin
  FBlinking := Value;
  FTimer.Enabled := Value;
  if not Value then
    Visible := True;
end;

{**************************************************}

procedure TJvBlinkingLabel.SetDelay(Value: Cardinal);
begin
  FDelay := Value;
  FTimer.Interval := Value;
end;

{**************************************************}

procedure TJvBlinkingLabel.OnBlink(Sender: TObject);
begin
  Visible := not Visible;
  if not Visible then
    Ftimer.Interval := FDelay
  else
    FTimer.Interval := FBlink;
end;

end.
