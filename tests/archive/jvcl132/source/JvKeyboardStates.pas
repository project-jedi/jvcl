{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvKeyboardStates.PAS, released on 2001-02-28.

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

unit JvKeyboardStates;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, JvTypes, JvComponent;

type
  TJvKeyboardStates = class(TJvComponent)
  private
    FTimer: TTimer;
    FAnimation: TJvAnimations;
    F1: Boolean;
    F2: Boolean;
    F3: Boolean;
    FCtrl: Boolean;
    procedure SetNumLock(Value: Boolean);
    function GetNumLock: Boolean;
    procedure SetScroll(Value: Boolean);
    function GetScroll: Boolean;
    procedure SetCapsLock(Value: Boolean);
    function GetCapsLock: Boolean;
    procedure OnAnimation(Sender: TObject);
    function GetState(Value: Integer): Boolean;
    procedure SetState(key: Integer; Value: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure SetCtrl(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default False;
    property Interval: Cardinal read GetInterval write SetInterval default 100;
    property Animation: TJvAnimations read FAnimation write FAnimation;
    property NumLock: Boolean read GetNumLock write SetNumLock;
    property ScrollLock: Boolean read GetScroll write SetScroll;
    property CapsLock: Boolean read GetCapsLock write SetCapsLock;
    property SystemKeys_Enabled: Boolean read FCtrl write SetCtrl;
  end;

implementation

{*****************************************************}

procedure TJvKeyboardStates.OnAnimation(Sender: TObject);
begin
  case FAnimation of
    anLeftRight:
      begin
        FTimer.Tag := (FTimer.Tag + 1) mod 4;
        SetNumLock(FTimer.Tag = 1);
        SetCapsLock(FTimer.Tag = 2);
        SetScroll(FTimer.Tag = 3);
      end;
    anRightLeft:
      begin
        FTimer.Tag := (FTimer.Tag + 1) mod 4;
        SetNumLock(FTimer.Tag = 3);
        SetCapsLock(FTimer.Tag = 2);
        SetScroll(FTimer.Tag = 1);
      end;
    anRightAndLeft:
      begin
        FTimer.Tag := (FTimer.Tag + 1) mod 8;
        SetNumLock((FTimer.Tag = 1) or (FTimer.Tag = 7));
        SetCapsLock((FTimer.Tag = 2) or (FTimer.Tag = 6));
        SetScroll((FTimer.Tag = 3) or (FTimer.Tag = 5));
      end;
    anLeftVumeter:
      begin
        FTimer.Tag := (FTimer.Tag + 1) mod 4;
        SetNumLock(FTimer.Tag > 0);
        SetCapsLock(FTimer.Tag > 1);
        SetScroll(FTimer.Tag > 2);
      end;
    anRightVumeter:
      begin
        FTimer.Tag := (FTimer.Tag + 1) mod 4;
        SetNumLock(FTimer.Tag > 2);
        SetCapsLock(FTimer.Tag > 1);
        SetScroll(FTimer.Tag > 0);
      end;
  end;
end;

{*****************************************************}

constructor TJvKeyboardStates.Create(AOwner: TComponent);
begin
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnAnimation;
  F1 := GetNumLock;
  F2 := GetCapsLock;
  F3 := GetScroll;
  FCtrl := True;
  inherited;
end;

{*****************************************************}

procedure TJvKeyboardStates.SetCtrl(Value: Boolean);
var
  dummy: LongInt;
begin
  FCtrl := Value;
  // (rom) improved
  SystemParametersInfo(SPI_SCREENSAVERRUNNING, Ord(not Value), @Dummy, 0)
end;

{*****************************************************}

destructor TJvKeyboardStates.Destroy;
begin
  if FTimer.Enabled then
    SetEnabled(False);
  FTimer.Free;
  inherited;
end;

{*****************************************************}

function TJvKeyboardStates.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

{*****************************************************}

procedure TJvKeyboardStates.SetEnabled(Value: Boolean);
begin
  if Value then
  begin
    if not FTimer.Enabled then
    begin
      F1 := GetNumLock;
      F2 := GetCapsLock;
      F3 := GetScroll;
    end;
    FTimer.Enabled := True;
    FTimer.Tag := 0;
  end
  else
  begin
    FTimer.Enabled := False;
    SetNumLock(F1);
    SetCapsLock(F2);
    SetScroll(F3);
  end;
end;

{*****************************************************}

function TJvKeyboardStates.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

{*****************************************************}

procedure TJvKeyboardStates.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

{*****************************************************}

function TJvKeyboardStates.GetState(Value: Integer): Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := KeyState[Value] = 1;
end;

{*****************************************************}

procedure TJvKeyboardStates.SetState(key: Integer; Value: Boolean);
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  KeyState[key] := Integer(Value);
  SetKeyboardState(KeyState);
end;

{*****************************************************}

procedure TJvKeyboardStates.SetNumLock(Value: Boolean);
begin
  SetState(VK_NUMLOCK, Value);
end;

{*****************************************************}

function TJvKeyboardStates.GetNumLock: Boolean;
begin
  Result := GetState(VK_NUMLOCK);
end;

{*****************************************************}

procedure TJvKeyboardStates.SetScroll(Value: Boolean);
begin
  SetState(VK_SCROLL, Value);
end;

{*****************************************************}

function TJvKeyboardStates.GetScroll: Boolean;
begin
  Result := GetState(VK_SCROLL);
end;

{*****************************************************}

procedure TJvKeyboardStates.SetCapsLock(Value: Boolean);
begin
  SetState(20, Value);
end;

{*****************************************************}

function TJvKeyboardStates.GetCapsLock: Boolean;
begin
  Result := GetState(20);
end;

end.
