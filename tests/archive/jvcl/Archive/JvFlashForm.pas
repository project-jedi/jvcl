{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFlashForm.PAS, released on 2001-02-28.

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

unit JvFlashForm;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, JvComponent;

// (rom) should derive from JvFlashApplication. Only Window Handle differs

type
  TJvFlashForm = class(TJvComponent)
  private
    FEnabled: Boolean;
    FDelay: Integer;
    FTimer: TTimer;
    FFlashed: Boolean;
    FHandle: HWND;
    procedure SetEnabled(En: Boolean);
    procedure SetDelay(De: Integer);
    procedure OnFTimer(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Delay: Integer read FDelay write SetDelay default 200;
  end;

implementation

{****************************************************}

constructor TJvFlashForm.Create(AOwner: TComponent);
begin
  inherited;
  FDelay := 200;
  FFlashed := False;
  FHandle := GetParentForm(TControl(AOwner)).Handle;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := OnFTimer;
  FTimer.Enabled := FEnabled;
  FTimer.Interval := FDelay;
end;

{****************************************************}

destructor TJvFlashForm.Destroy;
begin
  FlashWindow(FHandle, False);
  FTimer.Free;
  inherited;
end;

{****************************************************}

procedure TJvFlashForm.SetEnabled(En: Boolean);
begin
  FEnabled := En;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := En;
end;

{****************************************************}

procedure TJvFlashForm.SetDelay(De: Integer);
begin
  FDelay := De;
  FTimer.Interval := De;
end;

{****************************************************}

procedure TJvFlashForm.OnFTimer(Sender: TObject);
begin
  FFlashed := not FFlashed;
  FlashWindow(FHandle, FFLashed);
end;

end.
