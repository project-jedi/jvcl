{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMouseGesture.PAS, released on 2003-07-10.

The Initial Developers of the Original Code are: Christian Vogt (christian att fam-vogt dott de)
Copyright (c) 2003 by Christian Vogt
All Rights Reserved.

Portions of code based on an idea of Mozilla browser mouse gesture addon

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  This unit implements mouse gestures. For this purpose
  actually two classes are available. one is the interpreter
  and can be used to enhance special components like a grid. In
  this case the programmer is responsible to fill matching
  OnMouseDown, OnMouseUp and OnMouseMove events of component.
  This works fine with MSWINDOWS and UNIX. The second component
  installs a hook for a specific application and fires an event
  after detecting a mouse gesture (Windows only in this version
  \:-( ).

  Programmers will get a string with the detected gesture from
  following matrix:
  <TABLE noborder>
  ==  ===  ==
  7   U    9
  L   \*   R
  1   D    3
  </TABLE>

  The asterix is the startpoint for the first vector. E.g. a
  gesture string "LU" means, user has first moved mouse to the
  left side and then up. There's no limit for complexity of a
  gesture ...

  Note
  See demo project for usage ...

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMouseGesture;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Messages,
  {$IFDEF VisualCLX}
  Qt, QControls, QForms,
  {$ENDIF VisualCLX}
  JvComponent;

type
  { Description
    Defines, whether or not the hook will be activated automatically or not.
  }
  TJvActivationMode = (amAppStart, amManual);

  { Description
    Defines a complex gesture (two or more letters event)

  }
  TOnMouseGestureCustomInterpretation = procedure(Sender:TObject; const AGesture: string) of object;

  { Description
    This class implements the basic interpreter. It can be used
    to enhance single components, too. E.g., if you want to
    enable a grid with gesture feature. For this purpose you have
    to do 4 steps:

    1) Fill the "OnMouseDown" event with code like


    <CODE>
      if Button = mbRight then
        JvMouseGesture1.StartMouseGesture(x,y);
    </CODE>


    2) Fill the OnMouseMove event with something like


    <CODE>
      if JvMouseGesture1.TrailActive then
        JvMouseGesture1.TrailMouseGesture(x,y);
    </CODE>


    3) Now fill the OnMouseUp event


    <CODE>
      if JvMouseGesture1.TrailActive then
        JvMouseGesture1.EndMouseGesture;
    </CODE>


    4) Last but not least fill components

    OnJvMouseGestureCustomInterpretation

    XOR

    OnJvMouseGesture\<xyz\>

    event

    Note:


    If CustomInterpreation is filled the other events are not
    fired!

    See Also

    TJvMouseGestureHook
  }
  TJvMouseGesture = class(TJvComponent)
  private
    FActive: Boolean;
    FTrailX: Integer;
    FTrailY: Integer;
    FTrailLength: Integer;
    FTrailLimit: Integer;
    FTrailActive: Boolean;
    FTrailStartTime: TDateTime;
    FdTolerance: Integer;
    FDelay: Integer;
    FTrailInterval: Integer;
    FGrid: Integer; // tolerance for diagonal movement. See TrailMouseGesture
    FGridHalf: Integer; // half of grid, needed for performance
    FLastPushed: Char;
    FGesture: string;
    FGestureList: TStringList;

    FOnMouseGestureRight: TNotifyEvent;
    FOnMouseGestureLeft: TNotifyEvent;
    FOnMouseGestureUp: TNotifyEvent;
    FOnMouseGestureDown: TNotifyEvent;
    FOnMouseGestureLeftLowerEdge: TNotifyEvent;
    FOnMouseGestureRightUpperEdge: TNotifyEvent;
    FOnMouseGestureLeftUpperEdge: TNotifyEvent;
    FOnMouseGestureRightLowerEdge: TNotifyEvent;
    FOnMouseGestureCancelled: TNotifyEvent;
    FOnMouseGestureCustomInterpretation: TOnMouseGestureCustomInterpretation;
    { Description
      Adds a detected sub gesture to gesture string
    }
    procedure AddGestureChar(AChar: Char);
    procedure SetTrailLimit(const Value: Integer);
    procedure SetTrailInterval(const Value: Integer);
    procedure SetDelay(const Value: Integer);
    procedure SetGrid(const Value: Integer);
    { Description
      Loads the known gestures for matching events

      Note:
      In this version only evaluation of simple mouse gestures are implemented
    }
    procedure LoadGestureTable;
    { Description
      Standard setter method for Active
    }
    procedure SetActive(const Value: Boolean);
  protected
    procedure DoMouseGestureRight;virtual;
    procedure DoMouseGestureLeft;virtual;
    procedure DoMouseGestureUp;virtual;
    procedure DoMouseGestureDown;virtual;
    procedure DoMouseGestureLeftLowerEdge;virtual;
    procedure DoMouseGestureRightUpperEdge;virtual;
    procedure DoMouseGestureLeftUpperEdge;virtual;
    procedure DoMouseGestureRightLowerEdge;virtual;
    procedure DoMouseGestureCancelled;virtual;
    function DoMouseGestureCustomInterpretation(const AGesture:string):boolean;virtual;
  public
    { Description
      Standard constructor
    }
    constructor Create(AOwner: TComponent); override;
    { Description
      Standard destructor
    }
    destructor Destroy; override;
    { Description
      Starts the mouse gesture interpretation

      Parameters:
      AMouseX: X coordinate of mouse cursor
      AMouseY: Y coordinate of mouse cursor
    }
    procedure StartMouseGesture(AMouseX, AMouseY: Integer);
    { Description
      Continues the mouse gesture interpretation during mouse move

      Parameters:
      AMouseX: X coordinate of mouse cursor
      AMouseY: Y coordinate of mouse cursor
    }
    procedure TrailMouseGesture(AMouseX, AMouseY: Integer);
    { Description
      Ends the mouse gesture interpretation and fires an event if a gesture
      was found
    }
    procedure EndMouseGesture;
    { Description
      The actual length of trail (not of gesture string!!!)
    }
    property TrailLength: Integer read FTrailLength;
    { Description
      TRUE, if in detection, otherwise FALSE
    }
    property TrailActive: Boolean read FTrailActive;
    { Description
      The gesture string. For string content see description of unit.
    }
    property Gesture: string read FGesture;
  published
    { Description
      The maximum length of trail (not of gesture string!!!)
      Normally never been changed
    }
    property TrailLimit: Integer read FTrailLimit write SetTrailLimit;
    { Description
      Trail interval
      Normally never been changed
    }
    property TrailInterval: Integer read FTrailInterval write SetTrailInterval;
    { Description
      Grid size for detection
      Normally never been changed
    }
    property Grid: Integer read FGrid write SetGrid;
    { Description
      The maximum delay before cancelling a gesture
      Normally never been changed
    }
    property Delay: Integer read FDelay write SetDelay;
    { Description
      TRUE if component is active, otherwise FALSE
    }
    property Active: Boolean read FActive write SetActive;
    { Description
      Event for own evaluation of detected gesture. If this event is used all
      others will be ignored!
    }
    property OnMouseGestureCustomInterpretation: TOnMouseGestureCustomInterpretation read
      FOnMouseGestureCustomInterpretation write FOnMouseGestureCustomInterpretation;
    { Description
      Event for a simple MOUSE UP gesture
    }
    property OnMouseGestureCancelled:TNotifyEvent read FOnMouseGestureCancelled write FOnMouseGestureCancelled;
    property OnMouseGestureUp: TNotifyEvent read FOnMouseGestureUp write FOnMouseGestureUp;
    { Description
      Event for a simple MOUSE DOWN gesture
    }
    property OnMouseGestureDown: TNotifyEvent read FOnMouseGestureDown write FOnMouseGestureDown;
    { Description
      Event for a simple MOUSE LEFT gesture
    }
    property OnMouseGestureLeft: TNotifyEvent read FOnMouseGestureLeft write FOnMouseGestureLeft;
    { Description
      Event for a simple MOUSE RIGHT gesture
    }
    property OnMouseGestureRight: TNotifyEvent read FOnMouseGestureRight write FOnMouseGestureRight;
    { Description
      Event for a simple diagonally MOUSE LEFT LOWER EDGE (point 1 in grid) gesture
    }
    property OnMouseGestureLeftLowerEdge: TNotifyEvent read FOnMouseGestureLeftLowerEdge write
      FOnMouseGestureLeftLowerEdge;
    { Description
      Event for a simple diagonally MOUSE RIGHT LOWER EDGE (point 3 in grid) gesture
    }
    property OnMouseGestureRightLowerEdge: TNotifyEvent read FOnMouseGestureRightLowerEdge write
      FOnMouseGestureRightLowerEdge;
    { Description
      Event for a simple diagonally MOUSE LEFT UPPER EDGE (point 7 in grid) gesture
    }
    property OnMouseGestureLeftUpperEdge: TNotifyEvent read FOnMouseGestureLeftUpperEdge write
      FOnMouseGestureLeftUpperEdge;
    { Description
      Event for a simple diagonally MOUSE RIGHT UPPER EDGE (point 9 in grid) gesture
    }
    property OnMouseGestureRightUpperEdge: TNotifyEvent read FOnMouseGestureRightUpperEdge write
      FOnMouseGestureRightUpperEdge;
  end;

  { Description
    This class implements a application wide mouse hook for mouse gestures.
    Programmers get only one event for a detected mouse gesture:

    OnMouseGestureCustomInterpretation

    See Also
    TJvMouseGesture
  }
  TJvMouseGestureHook = class(TJvComponent)
  private
    { Description
      True if a hook is installed
    }
    FHookInstalled: Boolean;
    {$IFDEF VCL}
    { Description
      Field for hook handle
    }
    FCurrentHook: HHook;
    { Description
      Field for method pointer
    }
    {$ENDIF VCL}
    FOnMouseGestureCustomInterpretation: TOnMouseGestureCustomInterpretation;
    { Description
      Field for active state of component
    }
    FActive: Boolean;
    { Description
      Field for mouse key
    }
    FMouseButton: TMouseButton;
    { Description
      Field for activation mode
    }
    FActivationMode: TJvActivationMode;
    { Description
      Standard setter method for evaluation of detected gesture
    }
    { Description
      Standard setter method for Active
    }
    procedure SetActive(const Value: Boolean);
    { Description
      Standard setter method for MouseButton
    }
    procedure SetMouseButton(const Value: TMouseButton);
    { Description
      Standard setter method for ActivationMode
    }
    procedure SetActivationMode(const Value: TJvActivationMode);
    procedure SetMouseGestureCustomInterpretation(const Value: TOnMouseGestureCustomInterpretation);
    function GetMouseGesture: TJvMouseGesture;
  protected
    { Description
      Create the hook. Maybe used in a later version as a new constructor
      to enable system wide hooks ...
    }
    procedure CreateForThreadOrSystem(AOwner: TComponent; ADwThreadID: Cardinal);
    function DoMouseGestureCustomInterpretation(const AGesture:string):boolean;virtual;
  public
    { Description
      Standard constructor
    }
    constructor Create(AOwner: TComponent); override;
    { Description
      Standard destructor
    }
    destructor Destroy; override;
    { Description
      TRUE if hook was installed successfully
    }
    {$IFDEF VCL}
    property HookInstalled: Boolean read FHookInstalled; //True if a hook is installed
    { Description
      handle of hook
    }
    property CurrentHook: HHook read FCurrentHook; //contains the handle of the currently installed hook
    {$ENDIF VCL}
    property MouseGesture:TJvMouseGesture read GetMouseGesture;
  published
    { Description
      TRUE if component is active, otherwise FALSE. Can be changed during runtime
    }
    property Active: Boolean read FActive write SetActive;
    { Description
      If property is set to <code>JvOnAppStart</code> then component will be
      activated on start of application, with <code>JvManually</code> you
      have to activate detection on your own
    }
    property ActivationMode: TJvActivationMode read FActivationMode write SetActivationMode;
    { Description
      Set the mouse key to be used for start/stop gesture

      See Also
      TMouseButton
    }
    property MouseButton: TMouseButton read FMouseButton write SetMouseButton default mbRight;
    { Description
      Set the event to be executed if a gesture will be detected
    }
    property OnMouseGestureCustomInterpretation: TOnMouseGestureCustomInterpretation read FOnMouseGestureCustomInterpretation write SetMouseGestureCustomInterpretation;
  end;

{$IFDEF VCL}
  { Description
    Hook call back function.
    DO NOT USE EXTERN!
  }
function JvMouseGestureHook(Code: Integer; wParam: Word; lParam: Longword): Longword; stdcall;
{$ENDIF VCL}
{$IFDEF VisualCLX}
function JvMouseGestureHook(App: TObject; Sender: QObjectH; Event: QEventH): Boolean;
{$ENDIF VisualCLX}

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvResources, JvTypes;

const
  JVMG_LEFT = 0;
  JVMG_RIGHT = 1;
  JVMG_UP = 2;
  JVMG_DOWN = 3;
  JVMG_LEFTUPPER = 4;
  JVMG_RIGHTUPPER = 5;
  JVMG_LEFTLOWER = 6;
  JVMG_RIGHTLOWER = 7;

var
  { Description
    Object pointer to interpreter class used by hook
  }
  JvMouseGestureInterpreter: TJvMouseGesture;
  { Description
    Some global vars to be accessed by call back function ...
  }
  JvMouseGestureHookAlreadyInstalled: Boolean = False;
  //<combine JvMouseGestureHookAlreadyInstalled>
  JvMouseGestureHookActive: Boolean = False;
  {$IFDEF VCL}
  //<combine JvMouseGestureHookAlreadyInstalled>
  JvMouseButtonDown: Cardinal = WM_RBUTTONDOWN;
  //<combine JvMouseGestureHookAlreadyInstalled>
  JvMouseButtonUp: Cardinal = WM_RBUTTONUP;

  JvCurrentHook: HHook = 0; //contains the handle of the currently installed hook
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  //<combine JvMouseGestureHookAlreadyInstalled>
  JvMouseButtonDown: ButtonState = ButtonState_RightButton;
  //<combine JvMouseGestureHookAlreadyInstalled>
  JvMouseButtonUp: ButtonState = ButtonState_RightButton;
  {$ENDIF VisualCLX}

//=== { TJvMouseGesture } ====================================================

constructor TJvMouseGesture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGestureList := TStringList.Create;
  FGestureList.Sorted := True;

  FDelay := 500;
  FTrailLimit := 1000;
  FTrailInterval := 2;
  FGrid := 15;
  FGridHalf := FGrid div 2;
  FTrailActive := False;
  FdTolerance := 75; // tolerance for diagonal movement. see processCoordinates()
  LoadGestureTable;

  FActive := not (csDesigning in ComponentState);
end;

destructor TJvMouseGesture.Destroy;
begin
  FTrailActive := False;
  FreeAndNil(FGestureList);
  inherited Destroy;
end;

procedure TJvMouseGesture.LoadGestureTable;
begin
  with FGestureList do
  begin
    AddObject('L', TObject(JVMG_LEFT));
    AddObject('R', TObject(JVMG_RIGHT));
    AddObject('U', TObject(JVMG_UP));
    AddObject('D', TObject(JVMG_DOWN));
    AddObject('1', TObject(JVMG_LEFTLOWER));
    AddObject('3', TObject(JVMG_RIGHTLOWER));
    AddObject('7', TObject(JVMG_LEFTUPPER));
    AddObject('9', TObject(JVMG_RIGHTUPPER));
  end;
end;

procedure TJvMouseGesture.SetActive(const Value: Boolean);
begin
  if csDesigning in ComponentState then
    FActive := False
  else
    FActive := Value;
end;

procedure TJvMouseGesture.SetTrailLimit(const Value: Integer);
begin
  FTrailLimit := Value;
  if (FTrailLimit < 100) or (FTrailLimit > 10000) then
    FTrailLimit := 1000;
end;

procedure TJvMouseGesture.SetTrailInterval(const Value: Integer);
begin
  FTrailInterval := Value;
  if (FTrailInterval < 1) or (FTrailInterval > 100) then
    FTrailInterval := 2;
end;

procedure TJvMouseGesture.SetDelay(const Value: Integer);
begin
  FDelay := Value;
  if FDelay < 500 then
    FDelay := 500;
end;

procedure TJvMouseGesture.SetGrid(const Value: Integer);
begin
  FGrid := Value;
  if (FGrid < 10) or (FGrid > 500) then
    FGrid := 15;

  FGridHalf := FGrid div 2;
end;

procedure TJvMouseGesture.AddGestureChar(AChar: Char);
begin
  if AChar <> FLastPushed then
  begin
    FGesture := FGesture + AChar;
    FLastPushed := AChar;
  end;
end;

procedure TJvMouseGesture.StartMouseGesture(AMouseX, AMouseY: Integer);
begin
  if not FActive then
    Exit;

  FLastPushed := #0;
  FGesture := '';
  FTrailActive := True;
  FTrailLength := 0;
  FTrailX := AMouseX;
  FTrailY := AMouseY;
  FTrailStartTime := now;
end;

procedure TJvMouseGesture.TrailMouseGesture(AMouseX, AMouseY: Integer);
var
  locX: Integer;
  locY: Integer;
  x_dir: Integer;
  y_dir: Integer;
  tolerancePercent: Double;
  x_divide_y: Double;
  y_divide_x: Double;

  function InBetween(AValue, AMin, AMax: Double): Boolean;
  begin
    Result := (AValue >= AMin) and (AValue <= AMax);
  end;

begin
  if not FActive then
    Exit;

  if (not FTrailActive) or (FTrailLength > FTrailLimit) then
  begin
    FTrailActive := False;
    Exit;
  end;

  try
    x_dir := AMouseX - FTrailX;
    y_dir := AMouseY - FTrailY;
    locX := abs(x_dir);
    locY := abs(y_dir);

    // process each half-grid
    if (locX >= FGridHalf) or (locY >= FGridHalf) then
    begin
      // diagonal movement:
      // dTolerance = 75 means that a movement is recognized as diagonal when
      // x/y or y/x is between 0.25 and 1
      tolerancePercent := 1 - FdTolerance / 100;
      if locY <> 0 then
        x_divide_y := locX / locY
      else
        x_divide_y := 0;
      if locX <> 0 then
        y_divide_x := locY / locX
      else
        y_divide_x := 0;
      if (FdTolerance <> 0) and
        (InBetween(x_divide_y, tolerancePercent, 1) or
        InBetween(y_divide_x, tolerancePercent, 1)) then
      begin
        if (x_dir < 0) and (y_dir > 0) then
        begin
          AddGestureChar('1');
        end
        else
        begin
          if (x_dir > 0) and (y_dir > 0) then
            AddGestureChar('3')
          else
          begin
            if (x_dir < 0) and (y_dir < 0) then
              AddGestureChar('7')
            else
            begin
              if (x_dir > 0) and (y_dir < 0) then
                AddGestureChar('9');
            end;
          end;
        end;
      end // of diaognal
      else
      begin
        // horizontal movement:
        if locX > locY then
        begin
          if x_dir > 0 then
            AddGestureChar('R')
          else
          begin
            if x_dir < 0 then
              AddGestureChar('L');
          end;
        end
        else
        begin
          // vertical movement:
          if locX < locY then
          begin
            if y_dir > 0 then
              AddGestureChar('D')
            else
            begin
              if y_dir < 0 then
                AddGestureChar('U');
            end;
          end;
        end;
      end;
    end; // of half grid
  finally
    FTrailX := AMouseX;
    FTrailY := AMouseY;
  end;
end;

procedure TJvMouseGesture.EndMouseGesture;
var
  Index: Integer;
begin
  if not FActive then
    Exit;

  FTrailActive := False;

  if FGesture = '' then
  begin
    DoMouseGestureCancelled;
    Exit;
  end;

  // check for custom interpretation first
  if DoMouseGestureCustomInterpretation(FGesture) then
    Exit;

  // if no custom interpretation is implemented we chaeck for known gestures
  // and matching events
  // CASE indexes are stored sequence independent. So we have to find gesture
  // first and get CASE INDEX stored as TObject in Object property. It's a
  // simple trick, but works fine ...
  Index := FGestureList.IndexOf(FGesture);
  if Index > -1 then
    Index := Integer(FGestureList.Objects[Index]);
  case Index of
    JVMG_LEFT:
      begin
        DoMouseGestureLeft;
      end;
    JVMG_RIGHT:
      begin
        DoMouseGestureRight;
      end;
    JVMG_UP:
      begin
        DoMouseGestureUp;
      end;
    JVMG_DOWN:
      begin
        DoMouseGestureDown;
      end;
    JVMG_LEFTLOWER:
      begin
        DoMouseGestureLeftLowerEdge;
      end;
    JVMG_RIGHTLOWER:
      begin
        DoMouseGestureRightLowerEdge;
      end;
    JVMG_LEFTUPPER:
      begin
        DoMouseGestureLeftUpperEdge;
      end;
    JVMG_RIGHTUPPER:
      begin
        DoMouseGestureRightUpperEdge;
      end;
  end;
end;

procedure TJvMouseGesture.DoMouseGestureCancelled;
begin
  if Assigned(FOnMouseGestureCancelled) then
    FOnMouseGestureCancelled(Self);
end;

function TJvMouseGesture.DoMouseGestureCustomInterpretation(const AGesture: string):boolean;
begin
   Result := Assigned(FOnMouseGestureCustomInterpretation);
   if Result then
     FOnMouseGestureCustomInterpretation(Self, FGesture);
end;

procedure TJvMouseGesture.DoMouseGestureDown;
begin
  if Assigned(FOnMouseGestureDown) then
    FOnMouseGestureDown(Self);
end;

procedure TJvMouseGesture.DoMouseGestureLeft;
begin
  if Assigned(FOnMouseGestureLeft) then
    FOnMouseGestureLeft(Self);
end;

procedure TJvMouseGesture.DoMouseGestureLeftLowerEdge;
begin
  if Assigned(FOnMouseGestureLeftLowerEdge) then
    FOnMouseGestureLeftLowerEdge(Self);
end;

procedure TJvMouseGesture.DoMouseGestureLeftUpperEdge;
begin
  if Assigned(FOnMouseGestureLeftUpperEdge) then
    FOnMouseGestureLeftUpperEdge(Self);
end;

procedure TJvMouseGesture.DoMouseGestureRight;
begin
  if Assigned(FOnMouseGestureRight) then
    FOnMouseGestureRight(Self);
end;

procedure TJvMouseGesture.DoMouseGestureRightLowerEdge;
begin
  if Assigned(FOnMouseGestureRightLowerEdge) then
    FOnMouseGestureRightLowerEdge(Self);
end;

procedure TJvMouseGesture.DoMouseGestureRightUpperEdge;
begin
  if Assigned(FOnMouseGestureRightUpperEdge) then
    FOnMouseGestureRightUpperEdge(Self);
end;

procedure TJvMouseGesture.DoMouseGestureUp;
begin
  if Assigned(FOnMouseGestureUp) then
    FOnMouseGestureUp(Self);
end;

//=== { TJvMouseGestureHook } ================================================

constructor TJvMouseGestureHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateForThreadOrSystem(AOwner, MainThreadID); // hook for complete application
end;

destructor TJvMouseGestureHook.Destroy;
{$IFDEF VisualCLX}
var
  Method: TMethod;
{$ENDIF VisualCLX}
begin
  FreeAndNil(JvMouseGestureInterpreter);

  if JvMouseGestureHookAlreadyInstalled then
  {$IFDEF VCL}
    JvMouseGestureHookAlreadyInstalled := UnhookWindowsHookEx(JvCurrentHook);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  begin
    Method.Code := @JvMouseGestureHook;
    Method.Data := nil;
    UninstallApplicationHook(TApplicationHook(Method));
    JvMouseGestureHookAlreadyInstalled := False;
  end;
  {$ENDIF VisualCLX}
  inherited Destroy;
end;

procedure TJvMouseGestureHook.CreateForThreadOrSystem(AOwner: TComponent; ADwThreadID: Cardinal);
{$IFDEF VisualCLX}
var
  Method: TMethod;
{$ENDIF VisualCLX}
begin
  if JvMouseGestureHookAlreadyInstalled then
    raise EJVCLException.CreateRes(@RsECannotHookTwice);

  JvMouseGestureInterpreter := TJvMouseGesture.Create(nil);
  FMouseButton := mbRight;
  if csDesigning in ComponentState then
  begin
    FActive := False;
    Exit;
  end;

  FActive := FActivationMode = amAppStart;

  {$IFDEF VCL}
  //install hook
  FCurrentHook := SetWindowsHookEx(WH_MOUSE, @JvMouseGestureHook, 0, ADwThreadID);

  //return True if it worked (read only for user). User should never see a
  //global var like MouseGestureHookAlreadyInstalled
  FHookInstalled := FCurrentHook <> 0;

  // global remember, internal use only
  JvMouseGestureHookAlreadyInstalled := FHookInstalled;
  JvCurrentHook := FCurrentHook;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Method.Code := @JvMouseGestureHook;
  Method.Data := Self;
  InstallApplicationHook(TApplicationHook(Method));

  JvMouseGestureHookAlreadyInstalled := True;
  FHookInstalled := True;
  {$ENDIF VisualCLX}

  // map event
  if Assigned(FOnMouseGestureCustomInterpretation) then
    JvMouseGestureInterpreter.OnMouseGestureCustomInterpretation :=
      FOnMouseGestureCustomInterpretation
  else
    JvMouseGestureInterpreter.OnMouseGestureCustomInterpretation := nil;
end;

function TJvMouseGestureHook.DoMouseGestureCustomInterpretation(const AGesture: string): boolean;
begin
  Result := Assigned(FOnMouseGestureCustomInterpretation);
  if Result then
    FOnMouseGestureCustomInterpretation(Self, AGesture);
end;

procedure TJvMouseGestureHook.SetActivationMode(const Value: TJvActivationMode);
begin
  FActivationMode := Value;
end;

procedure TJvMouseGestureHook.SetActive(const Value: Boolean);
begin
  if csDesigning in ComponentState then
    FActive := False
  else
    FActive := Value;

  JvMouseGestureHookActive := FActive;
end;

procedure TJvMouseGestureHook.SetMouseButton(const Value: TMouseButton);
begin
  FMouseButton := Value;
  {$IFDEF VCL}
  case Value of
    mbLeft:
      begin
        JvMouseButtonDown := WM_LBUTTONDOWN;
        JvMouseButtonUp := WM_LBUTTONUP;
      end;
    mbMiddle:
      begin
        JvMouseButtonDown := WM_MBUTTONDOWN;
        JvMouseButtonUp := WM_MBUTTONUP;
      end;
    mbRight:
      begin
        JvMouseButtonDown := WM_RBUTTONDOWN;
        JvMouseButtonUp := WM_RBUTTONUP;
      end;
  end;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  case Value of
    mbLeft:
      begin
        JvMouseButtonDown := ButtonState_LeftButton;
        JvMouseButtonUp := ButtonState_LeftButton;
      end;
    mbMiddle:
      begin
        JvMouseButtonDown := ButtonState_MidButton;
        JvMouseButtonUp := ButtonState_MidButton;
      end;
    mbRight:
      begin
        JvMouseButtonDown := ButtonState_RightButton;
        JvMouseButtonUp := ButtonState_RightButton;
      end;
  end;
  {$ENDIF VisualCLX}
end;

procedure TJvMouseGestureHook.SetMouseGestureCustomInterpretation(
  const Value: TOnMouseGestureCustomInterpretation);
begin
  FOnMouseGestureCustomInterpretation := Value;
  if Assigned(JvMouseGestureInterpreter) then
    JvMouseGestureInterpreter.OnMouseGestureCustomInterpretation := Value;
end;

function TJvMouseGestureHook.GetMouseGesture: TJvMouseGesture;
begin
  Result := JvMouseGestureInterpreter;
end;

//============================================================================

{$IFDEF VCL}
function JvMouseGestureHook(Code: Integer; wParam: Word; lParam: Longword): Longword; stdcall;
var
  locY: Integer;
  locX: Integer;
begin
  if (Code >= 0) and (JvMouseGestureHookActive) then
  begin
    with PMouseHookStruct(lParam)^ do
    begin
      locX := pt.X;
      locY := pt.Y;
    end;

    if wParam = WM_MOUSEMOVE then
      JvMouseGestureInterpreter.TrailMouseGesture(locX, locY);
    if wParam = JvMouseButtonDown then
      JvMouseGestureInterpreter.StartMouseGesture(locX, locY)
    else
    if wParam = JvMouseButtonUp then
      JvMouseGestureInterpreter.EndMouseGesture;
  end;
  Result := CallNextHookEx(JvCurrentHook, Code, wParam, lParam);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function JvMouseGestureHook(App: TObject; Sender: QObjectH; Event: QEventH): Boolean;
var
  locY: Integer;
  locX: Integer;
  etype: QEventType;
  Btn: ButtonState;
begin
  Result := False;
  if not JvMouseGestureHookActive then
    Exit;

  etype := QEvent_type(Event);
  case etype of
    QEventType_MouseButtonPress,
    QEventType_MouseButtonRelease,
    QEventType_MouseMove:
      begin
        locX := QMouseEvent_globalX(QMouseEventH(Event));
        locY := QMouseEvent_globalY(QMouseEventH(Event));
        Btn := QMouseEvent_button(QMouseEventH(Event));
        case etype of
          QEventType_MouseMove:
            JvMouseGestureInterpreter.TrailMouseGesture(locX, locY);
          QEventType_MouseButtonPress:
            begin
              if Btn = JvMouseButtonDown then
                JvMouseGestureInterpreter.StartMouseGesture(locX, locY);
            end;
          QEventType_MouseButtonRelease:
            begin
              if Btn = JvMouseButtonUp then
                JvMouseGestureInterpreter.EndMouseGesture;
            end;
        end;
        Result := True;
      end;
  end;
end;
{$ENDIF VisualCLX}

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

