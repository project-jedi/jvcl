{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJoystick.PAS, released on 2001-02-28.

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

unit JvJoystick;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, MMSystem, Forms, JvTypes, JvComponent;

// (rom) in the time of USB this unit may have to support more than 2 joysticks

type
  TJoystick = class(TPersistent)
  private
    FJoyInfo: JOYINFO;
    FJoy: JOYCAPS;
    FBidon: Cardinal;
    FBIdonw: Word;
    FBidonS: string;
    FCap: JvTypes.TJoyCaps;
    FCaps2: JvTypes.TJoyCaps;
    FszRegKey: string;
    FszOEMVxD: string;
    FszPName: string;
    FBidonI: Integer;
    FBidonB: Boolean;
    FJoyNumber: Integer;
    function GetButton1: Boolean;
    function GetButton2: Boolean;
    function GetButton3: Boolean;
    function GetButton4: Boolean;
    function GetXPosition: Integer;
    function GetYPosition: Integer;
    function GetZPosition: Integer;
    procedure RefreshJoy;
  protected
  public
    constructor CreateJoy(AOwner: TComponent; Joy: Integer);
  published
    property XPosition: Integer read GetXPosition write FBidonI;
    property YPosition: Integer read GetYPosition write FBidonI;
    property ZPosition: Integer read GetZPosition write FBidonI;
    property Button1Pressed: Boolean read GetButton1 write FBidonB;
    property Button2Pressed: Boolean read GetButton2 write FBidonB;
    property Button3Pressed: Boolean read GetButton3 write FBidonB;
    property Button4Pressed: Boolean read GetButton4 write FBidonB;
    property Manufacturer: Word read FJoy.wMid write FBidonW;
    property ProductIdentifier: Word read FJoy.wPid write FBidonW;
    property ProductName: string read FszPName write FbidonS;
    property XMin: Cardinal read FJoy.wXMin;
    property XMax: Cardinal read FJoy.wXMax write FBidon;
    property YMin: Cardinal read FJoy.wYMin write FBidon;
    property YMax: Cardinal read FJoy.wYMax write FBidon;
    property Zmin: Cardinal read FJoy.wZmin write FBidon;
    property Zmax: Cardinal read FJoy.wZmax write FBidon;
    property NumButtons: Cardinal read FJoy.wNumButtons write FBidon;
    property PeriodMin: Cardinal read FJoy.wPeriodMin write FBidon;
    property PeriodMax: Cardinal read FJoy.wPeriodMax write FBidon;
    property RudderMin: Cardinal read FJoy.wRmin write FBidon;
    property RudderMax: Cardinal read FJoy.wRmax write FBidon;
    property UMin: Cardinal read FJoy.wUMin write FBidon;
    property UMax: Cardinal read FJoy.wUMax write FBidon;
    property VMin: Cardinal read FJoy.wVMin write FBidon;
    property VMax: Cardinal read FJoy.wVMax write FBidon;
    property Capabilities: JvTypes.TJoyCaps read FCap write FCaps2;
    property MaxAxis: Cardinal read FJoy.wMaxAxes write FBidon;
    property NumAxis: Cardinal read FJoy.wNumAxes write FBidon;
    property MaxButtons: Cardinal read FJoy.wMaxButtons write FBidon;
    property RegKey: string read FszRegKey write FBidonS;
    property OemVXD: string read FszOEMVxD write FBidonS;
  end;

  TJvJoystick = class(TJvComponent)
  private
    FJoy: Boolean;
    FJoy1: TJoystick;
    FJoy2: TJoystick;
    FJoystick1: Boolean;
    FJoystick2: Boolean;
    FHandle: THandle;
    FCapture1: Boolean;
    FCapture2: Boolean;
    FPoll: Cardinal;
    FJoy1ButtonDown: TJoyButtonDown;
    FJoy2ButtonDown: TJoyButtonDown;
    FJoy1ButtonUp: TJoyButtonDown;
    FJoy2ButtonUp: TJoyButtonDown;
    FJoy2Move: TJoyMove;
    FJoy1Move: TJoyMove;
    FJoy1ZMove: TJoyZMove;
    FJoy2ZMove: TJoyZMove;
    FOnError: TJoyErrorMsg;
    procedure SetCapture1(const Value: Boolean);
    procedure SetCapture2(const Value: Boolean);
    function GetJoystick1: Boolean;
    function GetJoystick2: Boolean;
    function GetThreshold1: Integer;
    function GetThreshold2: Integer;
    procedure SetThreshold1(const Value: Integer);
    procedure SetThreshold2(const Value: Integer);
    procedure RaiseErrorCapture(Value: MMRESULT);
    procedure RaiseErrorRelease(Value: MMRESULT);
  protected
  public
    procedure WndProc(var Msg: TMessage);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Joy1Threshold: Integer read GetThreshold1 write SetThreshold1;
    property Joy2Threshold: Integer read GetThreshold2 write SetThreshold2;
    property HasJoystick1: Boolean read GetJoystick1 write FJoy;
    property HasJoystick2: Boolean read GetJoystick2 write FJoy;
    property PollTime: Cardinal read FPoll write FPoll default 50;
    property CaptureJoystick1: Boolean read FCapture1 write SetCapture1 default False;
    property CaptureJoystick2: Boolean read FCapture2 write SetCapture2 default False;
    property JoyStick1: TJoyStick read FJoy1;
    property JoyStick2: TJoyStick read FJoy2;
    property Joy1ButtonDown: TJoyButtonDown read FJoy1ButtonDown write FJoy1ButtonDown;
    property Joy2ButtonDown: TJoyButtonDown read FJoy2ButtonDown write FJoy2ButtonDown;
    property Joy1ButtonUp: TJoyButtonDown read FJoy1ButtonUp write FJoy1ButtonUp;
    property Joy2ButtonUp: TJoyButtonDown read FJoy2ButtonUp write FJoy2ButtonUp;
    property Joy1Move: TJoyMove read FJoy1Move write FJoy1Move;
    property Joy2Move: TJoyMove read FJoy2Move write FJoy2Move;
    property Joy1ZMove: TJoyZMove read FJoy1ZMove write FJoy1ZMove;
    property Joy2ZMove: TJoyZMove read FJoy2ZMove write FJoy2ZMove;
    property OnError: TJoyErrorMsg read FOnError write FOnError;
  end;

implementation

resourcestring
  RC_NoJoystickDriver = 'The joystick driver is not present.';
  RC_CanntCaptureJoystick = 'Can''t capture the joystick';
  RC_JoystickUnplugged = 'The specified joystick is not connected to the system.';
  RC_JoystickErrorParam = 'The specified joystick device identifier is invalid.';

  ///////////////////////////////////////////////////////////
  // TJvJoystick
  ///////////////////////////////////////////////////////////

constructor TJvJoystick.Create(AOwner: TComponent);
begin
  inherited;
  FJoyStick1 := joyGetNumDevs > 0;
  FJoystick2 := joyGetNumDevs > 1;
  FJoy1 := TJoystick.CreateJoy(Self, JOYSTICKID1);
  FJoy2 := TJoystick.CreateJoy(Self, JOYSTICKID2);
  FHandle := {$IFDEF Delphi6_Up}Classes.{$ENDIF}AllocateHWND(WndProc);
  FCapture1 := False;
  FCapture2 := False;
  FPoll := 50;
end;

{************************************************************}

destructor TJvJoystick.Destroy;
begin
  FJoy1.Free;
  FJoy2.Free;
  {$IFDEF Delphi6_Up}Classes.{$ENDIF}DeallocateHWnd(FHandle);
  if FCapture1 then
    joyReleaseCapture(JOYSTICKID1);
  if FCapture2 then
    joyReleaseCapture(JOYSTICKID2);
  inherited;
end;

{************************************************************}

function TJvJoystick.GetJoystick1: Boolean;
var
  j: JOYINFO;
begin
  Result := joyGetPos(JOYSTICKID1, @j) = JOYERR_NOERROR;
end;

{************************************************************}

function TJvJoystick.GetJoystick2: Boolean;
var
  j: JOYINFO;
begin
  Result := joyGetPos(JOYSTICKID2, @j) = JOYERR_NOERROR;
end;

{************************************************************}

function TJvJoystick.GetThreshold1: Integer;
begin
  joyGetThreshold(JOYSTICKID1, @Result);
end;

{************************************************************}

function TJvJoystick.GetThreshold2: Integer;
begin
  joyGetThreshold(JOYSTICKID2, @Result);
end;

{************************************************************}

procedure TJvJoystick.RaiseErrorCapture(Value: MMRESULT);
begin
  case Value of
    MMSYSERR_NODRIVER:
      if Assigned(FOnError) then
        FOnError(Self, MMSYSERR_NODRIVER, RC_NoJoystickDriver);
    JOYERR_NOCANDO:
      if Assigned(FOnError) then
        FOnError(Self, JOYERR_NOCANDO, RC_CanntCaptureJoystick);
    JOYERR_UNPLUGGED:
      if Assigned(FOnError) then
        FOnError(Self, JOYERR_NOCANDO, RC_JoystickUnplugged);
  end;
end;

{************************************************************}

procedure TJvJoystick.RaiseErrorRelease(Value: MMRESULT);
begin
  case Value of
    MMSYSERR_NODRIVER:
      if Assigned(FOnError) then
        FOnError(Self, MMSYSERR_NODRIVER, RC_NoJoystickDriver);
    JOYERR_PARMS:
      if Assigned(FOnError) then
        FOnError(Self, JOYERR_PARMS, RC_JoystickErrorParam);
  end;
end;
{************************************************************}

procedure TJvJoystick.SetCapture1(const Value: Boolean);
begin
  FCapture1 := Value;
  if Value then
    RaiseErrorCapture(JoySetCapture(FHandle, JOYSTICKID1, FPoll, True))
  else
    RaiseErrorRelease(joyReleaseCapture(JOYSTICKID1));
end;

{************************************************************}

procedure TJvJoystick.SetCapture2(const Value: Boolean);
begin
  FCapture2 := Value;
  if Value then
    RaiseErrorCapture(JoySetCapture(FHandle, JOYSTICKID2, FPoll, True))
  else
    RaiseErrorRelease(joyReleaseCapture(JOYSTICKID2));
end;

{************************************************************}

procedure TJvJoystick.SetThreshold1(const Value: Integer);
begin
  joySetThreshold(JOYSTICKID1, Value);
end;

{************************************************************}

procedure TJvJoystick.SetThreshold2(const Value: Integer);
begin
  joySetThreshold(JOYSTICKID2, Value);
end;

{************************************************************}

procedure TJvJoystick.WndProc(var Msg: TMessage);
var
  x, y: Byte;
  i: Integer;
  b1, b2, b3, b4: Boolean;

  procedure TestButtonDown(Value: TJoyButtonDown);
  begin
    if Assigned(Value) then
    begin
      x := msg.LParamLo;
      y := msg.LParamHi;
      if (msg.WParam and JOY_BUTTON1CHG) = JOY_BUTTON1CHG then
        i := 1
      else if (msg.WParam and JOY_BUTTON2CHG) = JOY_BUTTON2CHG then
        i := 2
      else if (msg.WParam and JOY_BUTTON3CHG) = JOY_BUTTON3CHG then
        i := 3
      else if (msg.WParam and JOY_BUTTON4CHG) = JOY_BUTTON4CHG then
        i := 4
      else
        i := 0;
      b1 := (msg.WParam and JOY_BUTTON1) = JOY_BUTTON1;
      b2 := (msg.WParam and JOY_BUTTON2) = JOY_BUTTON2;
      b3 := (msg.WParam and JOY_BUTTON3) = JOY_BUTTON3;
      b4 := (msg.WParam and JOY_BUTTON4) = JOY_BUTTON4;
      Value(Self, x, y, i, b1, b2, b3, b4);
    end;
  end;

  procedure TestButtonMove(Value: TJoyMove);
  begin
    if Assigned(Value) then
    begin
      x := msg.LParamLo;
      y := msg.LParamHi;
      b1 := (msg.WParam and JOY_BUTTON1) = JOY_BUTTON1;
      b2 := (msg.WParam and JOY_BUTTON2) = JOY_BUTTON2;
      b3 := (msg.WParam and JOY_BUTTON3) = JOY_BUTTON3;
      b4 := (msg.WParam and JOY_BUTTON4) = JOY_BUTTON4;
      Value(Self, x, y, b1, b2, b3, b4);
    end;
  end;

  procedure TestButtonZMove(Value: TJoyZMove);
  begin
    if Assigned(Value) then
    begin
      x := msg.LParamLo;
      b1 := (msg.WParam and JOY_BUTTON1) = JOY_BUTTON1;
      b2 := (msg.WParam and JOY_BUTTON2) = JOY_BUTTON2;
      b3 := (msg.WParam and JOY_BUTTON3) = JOY_BUTTON3;
      b4 := (msg.WParam and JOY_BUTTON4) = JOY_BUTTON4;
      Value(Self, x, b1, b2, b3, b4);
    end;
  end;

begin
  case Msg.Msg of
    MM_JOY1BUTTONDOWN:
      TestButtonDown(FJoy1ButtonDown);
    MM_JOY1BUTTONUP:
      TestButtonDown(FJoy1ButtonUp);
    MM_JOY1MOVE:
      TestButtonMove(FJoy1Move);
    MM_JOY1ZMOVE:
      TestButtonZMove(FJoy1ZMove);
    MM_JOY2BUTTONDOWN:
      TestButtonDown(FJoy2ButtonDown);
    MM_JOY2BUTTONUP:
      TestButtonDown(FJoy1ButtonUp);
    MM_JOY2MOVE:
      TestButtonMove(FJoy1Move);
    MM_JOY2ZMOVE:
      TestButtonZMove(FJoy1ZMove);
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end;
end;

///////////////////////////////////////////////////////////
// TJvJoystick
///////////////////////////////////////////////////////////

constructor TJoystick.CreateJoy(AOwner: TComponent; Joy: Integer);
begin
  FJoyNumber := Joy;
  if joyGetDevCaps(Joy, @FJoy, SizeOf(FJoy)) = MMSYSERR_NODRIVER then
    raise Exception.Create('Unable to initialize joystick driver');
  FCap := [];
  if (JOYCAPS_HASZ and FJoy.wCaps) = JOYCAPS_HASZ then
    FCap := FCap + [joHasZCoordinate];
  if (JOYCAPS_HASR and FJoy.wCaps) = JOYCAPS_HASR then
    FCap := FCap + [joHasRudder];
  if (JOYCAPS_HASU and FJoy.wCaps) = JOYCAPS_HASU then
    FCap := FCap + [joHasUCoordinate];
  if (JOYCAPS_HASV and FJoy.wCaps) = JOYCAPS_HASV then
    FCap := FCap + [joHasVCoordinate];
  if (JOYCAPS_HASPOV and FJoy.wCaps) = JOYCAPS_HASPOV then
    FCap := FCap + [joHasPointOfVue];
  if (JOYCAPS_POV4DIR and FJoy.wCaps) = JOYCAPS_POV4DIR then
    FCap := FCap + [joHasPointOfVDiscrete];
  if (JOYCAPS_POVCTS and FJoy.wCaps) = JOYCAPS_POVCTS then
    FCap := FCap + [joHasPointOfVContinuous];
  FszRegKey := FJoy.szRegKey;
  FszOEMVxD := FJoy.szOEMVxD;
  FszPName := FJoy.szPName;
end;

{************************************************************}

function TJoystick.GetButton1: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON1) = JOY_BUTTON1;
end;

{************************************************************}

function TJoystick.GetButton2: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON2) = JOY_BUTTON2;
end;

{************************************************************}

function TJoystick.GetButton3: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON3) = JOY_BUTTON3;
end;

{************************************************************}

function TJoystick.GetButton4: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON4) = JOY_BUTTON4;
end;

{************************************************************}

function TJoystick.GetXPosition: Integer;
begin
  RefreshJoy;
  Result := FJoyInfo.wXpos;
end;

{************************************************************}

function TJoystick.GetYPosition: Integer;
begin
  RefreshJoy;
  Result := FJoyInfo.wYpos;
end;

{************************************************************}

function TJoystick.GetZPosition: Integer;
begin
  RefreshJoy;
  Result := FJoyInfo.wZpos;
end;

{************************************************************}

procedure TJoystick.RefreshJoy;
begin
  joyGetPos(FJoyNumber, @FJoyInfo);
end;

end.
