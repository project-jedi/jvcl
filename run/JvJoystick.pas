{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJoystick.PAS, released on 2001-02-28.

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
{$I windowsonly.inc}

unit JvJoystick;

interface

uses
  Windows, Messages, SysUtils, Classes, MMSystem,
  {$IFDEF VCL}
  Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms,
  {$ENDIF VisualCLX}
  JvTypes, JvComponent;

// (rom) in the time of USB this unit may have to support more than 2 joysticks

type
  TJoyCap = (joHasZCoordinate, joHasRudder, joHasUCoordinate, joHasVCoordinate, joHasPointOfVue,
    joHasPointOfVDiscrete, joHasPointOfVContinuous);
  TJoyCaps = set of TJoyCap;
  TJoyButtonDown = procedure(Sender: TObject; X, Y: Integer; ButtonChanged: Integer; But1Pressed, But2Pressed,
    But3Pressed, But4Pressed: Boolean) of object;
  TJoyMove = procedure(Sender: TObject; X, Y: Integer;
    But1Pressed, But2Pressed, But3Pressed, But4Pressed: Boolean) of object;
  TJoyZMove = procedure(Sender: TObject; Z: Integer;
    But1Pressed, But2Pressed, But3Pressed, But4Pressed: Boolean) of object;
  TJoyErrorMsg = procedure(Sender: TObject; code: Integer; Msg: string) of object;

  TJoystick = class(TPersistent)
  private
    FJoyInfo: JOYINFO;
    FJoy: JOYCAPS;
    FDummy: Cardinal;
    FDummyW: Word;
    FDummyS: string;
    FCapabilities: TJoyCaps;
    FCapsDummy: TJoyCaps;
    FRegKey: string;
    FOEMVxD: string;
    FProductName: string;
    FDummyI: Integer;
    FDummyB: Boolean;
    FJoyNumber: Integer;
    function GetButton1: Boolean;
    function GetButton2: Boolean;
    function GetButton3: Boolean;
    function GetButton4: Boolean;
    function GetXPosition: Integer;
    function GetYPosition: Integer;
    function GetZPosition: Integer;
    procedure RefreshJoy;
  public
    constructor CreateJoy(AOwner: TComponent; Joy: Integer);
  published
    { Do not store dummies }
    property XPosition: Integer read GetXPosition write FDummyI stored False;
    property YPosition: Integer read GetYPosition write FDummyI stored False;
    property ZPosition: Integer read GetZPosition write FDummyI stored False;
    property Button1Pressed: Boolean read GetButton1 write FDummyB stored False;
    property Button2Pressed: Boolean read GetButton2 write FDummyB stored False;
    property Button3Pressed: Boolean read GetButton3 write FDummyB stored False;
    property Button4Pressed: Boolean read GetButton4 write FDummyB stored False;
    property Manufacturer: Word read FJoy.wMid write FDummyW stored False;
    property ProductIdentifier: Word read FJoy.wPid write FDummyW stored False;
    property ProductName: string read FProductName write FDummyS stored False;
    property XMin: Cardinal read FJoy.wXMin write FDummy stored False;
    property XMax: Cardinal read FJoy.wXMax write FDummy stored False;
    property YMin: Cardinal read FJoy.wYMin write FDummy stored False;
    property YMax: Cardinal read FJoy.wYMax write FDummy stored False;
    property ZMin: Cardinal read FJoy.wZmin write FDummy stored False;
    property ZMax: Cardinal read FJoy.wZmax write FDummy stored False;
    property NumButtons: Cardinal read FJoy.wNumButtons write FDummy stored False;
    property PeriodMin: Cardinal read FJoy.wPeriodMin write FDummy stored False;
    property PeriodMax: Cardinal read FJoy.wPeriodMax write FDummy stored False;
    property RudderMin: Cardinal read FJoy.wRmin write FDummy stored False;
    property RudderMax: Cardinal read FJoy.wRmax write FDummy stored False;
    property UMin: Cardinal read FJoy.wUMin write FDummy stored False;
    property UMax: Cardinal read FJoy.wUMax write FDummy stored False;
    property VMin: Cardinal read FJoy.wVMin write FDummy stored False;
    property VMax: Cardinal read FJoy.wVMax write FDummy stored False;
    property Capabilities: TJoyCaps read FCapabilities write FCapsDummy stored False;
    property MaxAxis: Cardinal read FJoy.wMaxAxes write FDummy stored False;
    property NumAxis: Cardinal read FJoy.wNumAxes write FDummy stored False;
    property MaxButtons: Cardinal read FJoy.wMaxButtons write FDummy stored False;
    property RegKey: string read FRegKey write FDummyS stored False;
    property OemVxD: string read FOEMVxD write FDummyS stored False;
  end;

  TJvJoystick = class(TJvComponent)
  private
    FJoyDummy: Boolean;
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
    function GetThreshold1: MMRESULT;
    function GetThreshold2: MMRESULT;
    procedure SetThreshold1(const Value: MMRESULT);
    procedure SetThreshold2(const Value: MMRESULT);
    procedure RaiseErrorCapture(Value: MMRESULT);
    procedure RaiseErrorRelease(Value: MMRESULT);
  public
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Msg: TMessage);
    destructor Destroy; override;
  published
    property Joy1Threshold: MMRESULT read GetThreshold1 write SetThreshold1;
    property Joy2Threshold: MMRESULT read GetThreshold2 write SetThreshold2;
    property HasJoystick1: Boolean read GetJoystick1 write FJoyDummy stored False;
    property HasJoystick2: Boolean read GetJoystick2 write FJoyDummy stored False;
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

uses
  JvJVCLUtils, JvResources;

//=== TJvJoystick ============================================================

constructor TJvJoystick.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJoyStick1 := joyGetNumDevs > 0;
  FJoystick2 := joyGetNumDevs > 1;
  FJoy1 := TJoystick.CreateJoy(Self, JOYSTICKID1);
  FJoy2 := TJoystick.CreateJoy(Self, JOYSTICKID2);
  FHandle := AllocateHWndEx(WndProc);
  FCapture1 := False;
  FCapture2 := False;
  FPoll := 50;
end;

destructor TJvJoystick.Destroy;
begin
  FJoy1.Free;
  FJoy2.Free;
  DeallocateHWndEx(FHandle);
  if FCapture1 then
    joyReleaseCapture(JOYSTICKID1);
  if FCapture2 then
    joyReleaseCapture(JOYSTICKID2);
  inherited Destroy;
end;

function TJvJoystick.GetJoystick1: Boolean;
var
  J: JOYINFO;
begin
  Result := joyGetPos(JOYSTICKID1, @J) = JOYERR_NOERROR;
end;

function TJvJoystick.GetJoystick2: Boolean;
var
  J: JOYINFO;
begin
  Result := joyGetPos(JOYSTICKID2, @J) = JOYERR_NOERROR;
end;

function TJvJoystick.GetThreshold1: MMRESULT;
begin
  joyGetThreshold(JOYSTICKID1, @Result);
end;

function TJvJoystick.GetThreshold2: MMRESULT;
begin
  joyGetThreshold(JOYSTICKID2, @Result);
end;

procedure TJvJoystick.RaiseErrorCapture(Value: MMRESULT);
begin
  case Value of
    MMSYSERR_NODRIVER:
      if Assigned(FOnError) then
        FOnError(Self, MMSYSERR_NODRIVER, RsNoJoystickDriver);
    JOYERR_NOCANDO:
      if Assigned(FOnError) then
        FOnError(Self, JOYERR_NOCANDO, RsCannotCaptureJoystick);
    JOYERR_UNPLUGGED:
      if Assigned(FOnError) then
        FOnError(Self, JOYERR_NOCANDO, RsJoystickUnplugged);
  end;
end;

procedure TJvJoystick.RaiseErrorRelease(Value: MMRESULT);
begin
  case Value of
    MMSYSERR_NODRIVER:
      if Assigned(FOnError) then
        FOnError(Self, MMSYSERR_NODRIVER, RsNoJoystickDriver);
    JOYERR_PARMS:
      if Assigned(FOnError) then
        FOnError(Self, JOYERR_PARMS, RsJoystickErrorParam);
  end;
end;

procedure TJvJoystick.SetCapture1(const Value: Boolean);
begin
  FCapture1 := Value;
  if Value then
    RaiseErrorCapture(JoySetCapture(FHandle, JOYSTICKID1, FPoll, True))
  else
    RaiseErrorRelease(joyReleaseCapture(JOYSTICKID1));
end;

procedure TJvJoystick.SetCapture2(const Value: Boolean);
begin
  FCapture2 := Value;
  if Value then
    RaiseErrorCapture(JoySetCapture(FHandle, JOYSTICKID2, FPoll, True))
  else
    RaiseErrorRelease(joyReleaseCapture(JOYSTICKID2));
end;

procedure TJvJoystick.SetThreshold1(const Value: MMRESULT);
begin
  joySetThreshold(JOYSTICKID1, Value);
end;

procedure TJvJoystick.SetThreshold2(const Value: MMRESULT);
begin
  joySetThreshold(JOYSTICKID2, Value);
end;

procedure TJvJoystick.WndProc(var Msg: TMessage);
var
  X, Y: Byte;
  I: Integer;
  B1, B2, B3, B4: Boolean;

  procedure TestButtonDown(Value: TJoyButtonDown);
  begin
    if Assigned(Value) then
    begin
      X := Msg.LParamLo;
      Y := Msg.LParamHi;
      if (Msg.WParam and JOY_BUTTON1CHG) = JOY_BUTTON1CHG then
        I := 1
      else
      if (Msg.WParam and JOY_BUTTON2CHG) = JOY_BUTTON2CHG then
        I := 2
      else
      if (Msg.WParam and JOY_BUTTON3CHG) = JOY_BUTTON3CHG then
        I := 3
      else
      if (Msg.WParam and JOY_BUTTON4CHG) = JOY_BUTTON4CHG then
        I := 4
      else
        I := 0;
      B1 := (Msg.WParam and JOY_BUTTON1) = JOY_BUTTON1;
      B2 := (Msg.WParam and JOY_BUTTON2) = JOY_BUTTON2;
      B3 := (Msg.WParam and JOY_BUTTON3) = JOY_BUTTON3;
      B4 := (Msg.WParam and JOY_BUTTON4) = JOY_BUTTON4;
      Value(Self, X, Y, I, B1, B2, B3, B4);
    end;
  end;

  procedure TestButtonMove(Value: TJoyMove);
  begin
    if Assigned(Value) then
    begin
      X := Msg.LParamLo;
      Y := Msg.LParamHi;
      B1 := (Msg.WParam and JOY_BUTTON1) = JOY_BUTTON1;
      B2 := (Msg.WParam and JOY_BUTTON2) = JOY_BUTTON2;
      B3 := (Msg.WParam and JOY_BUTTON3) = JOY_BUTTON3;
      B4 := (Msg.WParam and JOY_BUTTON4) = JOY_BUTTON4;
      Value(Self, X, Y, B1, B2, B3, B4);
    end;
  end;

  procedure TestButtonZMove(Value: TJoyZMove);
  begin
    if Assigned(Value) then
    begin
      X := Msg.LParamLo;
      B1 := (Msg.WParam and JOY_BUTTON1) = JOY_BUTTON1;
      B2 := (Msg.WParam and JOY_BUTTON2) = JOY_BUTTON2;
      B3 := (Msg.WParam and JOY_BUTTON3) = JOY_BUTTON3;
      B4 := (Msg.WParam and JOY_BUTTON4) = JOY_BUTTON4;
      Value(Self, X, B1, B2, B3, B4);
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

//=== TJoystick ==============================================================

constructor TJoystick.CreateJoy(AOwner: TComponent; Joy: Integer);
begin
  FJoyNumber := Joy;
  if joyGetDevCaps(Joy, @FJoy, SizeOf(FJoy)) = MMSYSERR_NODRIVER then
    raise EJVCLException.CreateRes(@RsEJoystickError);
  FCapabilities := [];
  if (JOYCAPS_HASZ and FJoy.wCaps) = JOYCAPS_HASZ then
    FCapabilities := FCapabilities + [joHasZCoordinate];
  if (JOYCAPS_HASR and FJoy.wCaps) = JOYCAPS_HASR then
    FCapabilities := FCapabilities + [joHasRudder];
  if (JOYCAPS_HASU and FJoy.wCaps) = JOYCAPS_HASU then
    FCapabilities := FCapabilities + [joHasUCoordinate];
  if (JOYCAPS_HASV and FJoy.wCaps) = JOYCAPS_HASV then
    FCapabilities := FCapabilities + [joHasVCoordinate];
  if (JOYCAPS_HASPOV and FJoy.wCaps) = JOYCAPS_HASPOV then
    FCapabilities := FCapabilities + [joHasPointOfVue];
  if (JOYCAPS_POV4DIR and FJoy.wCaps) = JOYCAPS_POV4DIR then
    FCapabilities := FCapabilities + [joHasPointOfVDiscrete];
  if (JOYCAPS_POVCTS and FJoy.wCaps) = JOYCAPS_POVCTS then
    FCapabilities := FCapabilities + [joHasPointOfVContinuous];
  FRegKey := FJoy.szRegKey;
  FOEMVxD := FJoy.szOEMVxD;
  FProductName := FJoy.szPName;
end;

function TJoystick.GetButton1: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON1) = JOY_BUTTON1;
end;

function TJoystick.GetButton2: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON2) = JOY_BUTTON2;
end;

function TJoystick.GetButton3: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON3) = JOY_BUTTON3;
end;

function TJoystick.GetButton4: Boolean;
begin
  RefreshJoy;
  Result := (FJoyInfo.wButtons and JOY_BUTTON4) = JOY_BUTTON4;
end;

function TJoystick.GetXPosition: Integer;
begin
  RefreshJoy;
  Result := FJoyInfo.wXpos;
end;

function TJoystick.GetYPosition: Integer;
begin
  RefreshJoy;
  Result := FJoyInfo.wYpos;
end;

function TJoystick.GetZPosition: Integer;
begin
  RefreshJoy;
  Result := FJoyInfo.wZpos;
end;

procedure TJoystick.RefreshJoy;
begin
  joyGetPos(FJoyNumber, @FJoyInfo);
end;

end.

