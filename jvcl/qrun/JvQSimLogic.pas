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

The Original Code is: JvSimLogic.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  This unit includes several visual logic blocks that can be used without any programming.
  It is the start of a whole series of simulation blocks.

  There is a string seperation between the visual part and functionality.

  The user creates and removes blocks; joins and moves them.

  The functionality is created every 50 msec in the onTimer event of TJvSimLogicBox.

  No programming is required, just drop a TJvLogicBox in the corner of a form and Build the program.

  All the rest is up to the user.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQSimLogic;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, QGraphics, QControls, QForms, QDialogs, QExtCtrls,
  SysUtils, Classes,
  JvQTypes;

type
  TJvLogic = class;

  TJvGateStyle = (jgsDI, jgsDO);
  TJvLogicFunc = (jlfAND, jlfOR, jlfNOT);
  TJvGate = record
    Style: TJvGateStyle;
    State: Boolean;
    Active: Boolean;
    Pos: TPoint;
  end;

  TJvPointX = class(TPersistent)
  private
    FX: Integer;
    FY: Integer;
  public
    function Point: TPoint;
    procedure SetPoint(const Pt: TPoint);
    procedure Assign(Source: TPersistent); override;
  published
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

  TJvConMode = (jcmTL, jcmTR, jcmBR, jcmBL);
  TJvConPos = (jcpTL, jcpTR, jcpBR, jcpBL);
  TJvConShape = (jcsTLBR, jcsTRBL);

  TJvSIMConnector = class(TGraphicControl)
  private
    FMdp: TPoint;
    FOldp: TPoint;
    FConAnchor: TPoint;
    FConOffset: TPoint;
    FConMode: TJvConMode;
    FConHot: TJvConPos;
    FDoMove: Boolean;
    FDoEdge: Boolean;
    FDisCon: TControl;
    FDisConI: Integer;
    FMode: TJvConMode;
    FShape: TJvConShape;
    FConSize: Integer;
    FConPos: TJvConPos;
    FEdge: Extended;

    FFromLogic: TJvLogic;
    FToLogic: TJvLogic;
    FFromGate: Integer;
    FToGate: Integer;
    FFromPoint: TJvPointX;
    FToPoint: TJvPointX;
    procedure SetFromLogic(const Value: TJvLogic);
    procedure SetToLogic(const Value: TJvLogic);
    procedure SetFromGate(const Value: Integer);
    procedure SetToGate(const Value: Integer);
    procedure SetFromPoint(const Value: TJvPointX);
    procedure SetToPoint(const Value: TJvPointX);
    procedure DisconnectFinal;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoMouseDown(X, Y: Integer);
    procedure DoMouseMove(dx, dy: Integer);
    procedure AnchorCorner(LogTL: TPoint; ACorner: TJvConMode);
    procedure MoveConnector(LogTL: TPoint);
    procedure Connect;
    procedure Disconnect;
  published
    property FromLogic: TJvLogic read FFromLogic write SetFromLogic;
    property FromGate: Integer read FFromGate write SetFromGate;
    property FromPoint: TJvPointX read FFromPoint write SetFromPoint;
    property ToLogic: TJvLogic read FToLogic write SetToLogic;
    property ToGate: Integer read FToGate write SetToGate;
    property ToPoint: TJvPointX read FToPoint write SetToPoint;
  end;

  TJvLogic = class(TGraphicControl)
  private
    FDoMove: Boolean;
    FDoStyle: Boolean;
    FStyleDown: Boolean;
    FMdp: TPoint;
    FOldp: TPoint;
    FGates: array [0..5] of TJvGate;
    FConnectors: TList;
    FNewLeft: Integer;
    FNewTop: Integer;
    FInput1: Boolean;
    FInput2: Boolean;
    FInput3: Boolean;
    FOutput1: Boolean;
    FOutput2: Boolean;
    FOutput3: Boolean;
    FLogicFunc: TJvLogicFunc;
    function GetGate(Index: Integer): TJvGate;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure PaintLed(Index: Integer);
    procedure SetInput1(const Value: Boolean);
    procedure SetInput2(const Value: Boolean);
    procedure SetInput3(const Value: Boolean);
    procedure SetOutput1(const Value: Boolean);
    procedure SetOutput2(const Value: Boolean);
    procedure SetOutput3(const Value: Boolean);
    procedure SetLogicFunc(const Value: TJvLogicFunc);
    procedure OutCalc;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Gates[Index: Integer]: TJvGate read GetGate;
  published
    property Input1: Boolean read FInput1 write SetInput1;
    property Input2: Boolean read FInput2 write SetInput2;
    property Input3: Boolean read FInput3 write SetInput3;
    property Output1: Boolean read FOutput1 write SetOutput1;
    property Output2: Boolean read FOutput2 write SetOutput2;
    property Output3: Boolean read FOutput3 write SetOutput3;
    property LogicFunc: TJvLogicFunc read FLogicFunc write SetLogicFunc;
  end;

  TJvSimReverse = class(TGraphicControl)
  private
    FDoMove: Boolean;
    FMdp: TPoint;
    FOldp: TPoint;
    FGates: array [0..3] of TJvGate;
    FConnectors: TList;
    FNewLeft: Integer;
    FNewTop: Integer;
    FInput1: Boolean;
    FOutput1: Boolean;
    FOutput2: Boolean;
    FOutput3: Boolean;
    function GetGate(Index: Integer): TJvGate;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure PaintLed(Index: Integer);
    procedure SetInput1(const Value: Boolean);
    procedure SetOutput1(const Value: Boolean);
    procedure OutCalc;
    procedure SetOutput2(const Value: Boolean);
    procedure SetOutput3(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Gates[Index: Integer]: TJvGate read GetGate;
  published
    property Input1: Boolean read FInput1 write SetInput1;
    property Output1: Boolean read FOutput1 write SetOutput1;
    property Output2: Boolean read FOutput2 write SetOutput2;
    property Output3: Boolean read FOutput3 write SetOutput3;
  end;

  TJvSimButton = class(TGraphicControl)
  private
    FDoMove: Boolean;
    FMdp: TPoint;
    FOldp: TPoint;
    FConnectors: TList;
    FDown: Boolean;
    FDepressed: Boolean;
    FNewLeft: Integer;
    FNewTop: Integer;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure PaintLed(Pt: TPoint; Lit: Boolean);
    procedure SetDown(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Down: Boolean read FDown write SetDown;
  end;

  TJvSimLight = class(TGraphicControl)
  private
    FDoMove: Boolean;
    FMdp: TPoint;
    FOldp: TPoint;
    FConnectors: TList;
    FLit: Boolean;
    FColorOn: TColor;
    FColorOff: TColor;
    FNewLeft: Integer;
    FNewTop: Integer;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure SetLit(const Value: Boolean);
    procedure SetColorOff(const Value: TColor);
    procedure SetColorOn(const Value: TColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Lit: Boolean read FLit write SetLit;
    property ColorOn: TColor read FColorOn write SetColorOn;
    property ColorOff: TColor read FColorOff write SetColorOff;
  end;

  TJvSimBin = class(TGraphicControl)
  private
    FBmpBin: TBitmap;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
  end;

  TJvSimLogicBox = class(TGraphicControl)
  private
    FCpu: TTimer;
    FBmpCon: TBitmap;
    FRCon: TRect;
    FDCon: Boolean;
    FBmpLogic: TBitmap;
    FRLogic: TRect;
    FDLogic: Boolean;
    FBmpButton: TBitmap;
    FRButton: TRect;
    FDButton: Boolean;
    FBmpLight: TBitmap;
    FRLight: TRect;
    FDLight: Boolean;
    FBmpRev: TBitmap;
    FRRev: TRect;
    FDRev: Boolean;
    FBmpBin: TBitmap;
    procedure CpuOnTimer(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvSimImages.res}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvSimImages.res}
{$ENDIF UNIX}

// general bin procedure

procedure BinCheck(AControl: TControl);
var
  Wc: TWinControl;
  I: Integer;
  R, Rb: TRect;
  Keep: Boolean;
begin
  // check for TJvSimLogicBox
  Wc := AControl.Parent;
  R := AControl.BoundsRect;
  Keep := False;
  for I := 0 to Wc.ControlCount - 1 do
    if Wc.Controls[I] is TJvSimLogicBox then
    begin
      Rb := Wc.Controls[I].BoundsRect;
      Rb.Left := Rb.Right - 32;
      if PtInRect(Rb, Point(R.Left, R.Top)) then
        Break
      else
      if PtInRect(Rb, Point(R.Right, R.Top)) then
        Break
      else
      if PtInRect(Rb, Point(R.Right, R.Bottom)) then
        Break
      else
      if PtInRect(Rb, Point(R.Left, R.Bottom)) then
        Break
      else
        Keep := True;
    end;
  if not Keep then
    AControl.Free;
end;

//=== { TJvPointX } ==========================================================

procedure TJvPointX.Assign(Source: TPersistent);
begin
  if Source is TJvPointX then
  begin
    FX := TJvPointX(Source).X;
    FY := TJvPointX(Source).Y;
  end
  else
    inherited Assign(Source);
end;

function TJvPointX.Point: TPoint;
begin
  Result.X := FX;
  Result.Y := FY;
end;

procedure TJvPointX.SetPoint(const Pt: TPoint);
begin
  FX := Pt.X;
  FY := Pt.Y;
end;

//=== { TJvSIMConnector } ====================================================

constructor TJvSIMConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 50;
  FMode := jcmTL;
  FShape := jcsTLBR;
  FConSize := 8;
  FConPos := jcpTL;
  FEdge := 0.5;
  FFromPoint := TJvPointX.Create;
  FToPoint := TJvPointX.Create;
end;

destructor TJvSIMConnector.Destroy;
begin
  FFromPoint.Free;
  FToPoint.Free;
  inherited Destroy;
end;

procedure TJvSIMConnector.DoMouseDown(X, Y: Integer);
var
  P: TPoint;
  Rtl, Rbr, Rtr, Rbl: TRect;
  D: Integer;
begin
  FDoMove := False;
  FDoEdge := False;
  D := FConSize;
  FOldp := Point(X, Y);
  Rtl := Rect(0, 0, D, D);
  Rbr := Rect(Width - 1 - D, Height - 1 - D, Width - 1, Height - 1);
  Rtr := Rect(Width - 1 - D, 0, Width - 1, D);
  Rbl := Rect(0, Height - 1 - D, D, Height - 1);
  P := Point(X, Y);
  if PtInRect(Rtl, P) and (FShape = jcsTLBR) then
  begin
    FMode := jcmTL;
    FMdp := Point(X, Y);
  end
  else
  if PtInRect(Rtr, P) and (FShape = jcsTRBL) then
  begin
    FMode := jcmTR;
    FMdp := Point(Width - X, Y);
  end
  else
  if PtInRect(Rbr, P) and (FShape = jcsTLBR) then
  begin
    FMode := jcmBR;
    FMdp := Point(Width - X, Height - Y);
  end
  else
  if PtInRect(Rbl, P) and (FShape = jcsTRBL) then
  begin
    FMode := jcmBL;
    FMdp := Point(X, Height - Y);
  end
  else
  if Abs(X - Round(FEdge * Width)) < 10 then
    FDoEdge := True
  else
  begin
    FDoMove := True;
    FMdp := Point(X, Y);
    SetFromLogic(nil);
    SetToLogic(nil);
  end;
  if not FDoEdge then
    Disconnect;
end;

procedure TJvSIMConnector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  DoMouseDown(X, Y);
end;

procedure TJvSIMConnector.DoMouseMove(dx, dy: Integer);
var
  P: TPoint;
  D, d2, nw, nh: Integer;
  X, Y: Integer;
begin
  X := dx + FOldp.X;
  Y := dy + FOldp.Y;
  FOldp := Point(X, Y);
  P := ClientToScreen(Point(X, Y));
  P := Parent.ScreenToClient(P);
  D := FConSize;
  d2 := D div 2;
  if FDoEdge then
  begin
    FEdge := X / Width;
    Invalidate;
  end
  else
  if FDoMove then
  begin
    Left := P.X - FMdp.X;
    Top := P.Y - FMdp.Y;
  end
  else
  begin
    case FMode of
      jcmTL:
        begin
          Left := P.X - FMdp.X;
          Top := P.Y - FMdp.Y;
          nw := Width + (FMdp.X - X);
          if nw < d2 then
          begin
            Left := Left + nw - D;
            Width := -nw + D + D;
            FMode := jcmTR;
            FShape := jcsTRBL;
            case FConPos of
              jcpTL:
                FConPos := jcpTR;
              jcpBR:
                FConPos := jcpBL;
            end;
            FEdge := 1 - FEdge;
          end
          else
            Width := nw;
          nh := Height + (FMdp.Y - Y);
          if nh < d2 then
          begin
            Top := Top + nh - D;
            Height := -nh + D + D;
            FMode := jcmBL;
            FShape := jcsTRBL;
            case FConPos of
              jcpTL:
                FConPos := jcpBL;
              jcpBR:
                FConPos := jcpTR;
            end;
          end
          else
            Height := nh;
        end;
      jcmTR:
        begin
          Top := P.Y - FMdp.Y;
          nw := X + FMdp.X;
          if nw < d2 then
          begin
            Left := Left + nw - D;
            Width := -nw + D + D;
            FMode := jcmTL;
            FShape := jcsTLBR;
            case FConPos of
              jcpTR:
                FConPos := jcpTL;
              jcpBL:
                FConPos := jcpBR;
            end;
            FEdge := 1 - FEdge;
          end
          else
            Width := nw;
          nh := Height + (FMdp.Y - Y);
          if nh < d2 then
          begin
            Top := Top + nh - D;
            Height := -nh + D + D;
            FMode := jcmBR;
            FShape := jcsTLBR;
            case FConPos of
              jcpTR:
                FConPos := jcpBR;
              jcpBL:
                FConPos := jcpTL;
            end;
          end
          else
            Height := nh;
        end;
      jcmBR:
        begin
          nw := X + FMdp.X;
          if nw < d2 then
          begin
            Left := Left + nw - D;
            Width := -nw + D + D;
            FMode := jcmBL;
            FShape := jcsTRBL;
            case FConPos of
              jcpBR:
                FConPos := jcpBL;
              jcpTL:
                FConPos := jcpTR;
            end;
            FEdge := 1 - FEdge;
          end
          else
            Width := nw;
          nh := Y + FMdp.Y;
          if nh < d2 then
          begin
            Top := Top + nh - D;
            Height := -nh + D + D;
            FMode := jcmTR;
            FShape := jcsTRBL;
            case FConPos of
              jcpBR:
                FConPos := jcpTR;
              jcpTL:
                FConPos := jcpBL;
            end;
          end
          else
            Height := nh;
        end;
      jcmBL:
        begin
          Left := P.X - FMdp.X;
          nw := Width + (FMdp.X - X);
          if nw < d2 then
          begin
            Left := Left + nw - D;
            Width := -nw + D + D;
            FMode := jcmBR;
            FShape := jcsTLBR;
            case FConPos of
              jcpBL:
                FConPos := jcpBR;
              jcpTR:
                FConPos := jcpTL;
            end;
            FEdge := 1 - FEdge;
          end
          else
            Width := nw;
          nh := Y + FMdp.Y;
          if nh < d2 then
          begin
            Top := Top + nh - D;
            Height := -nh + D + D;
            FMode := jcmTL;
            FShape := jcsTLBR;
            case FConPos of
              jcpBL:
                FConPos := jcpTL;
              jcpTR:
                FConPos := jcpBR;
            end;
          end
          else
            Height := nh;
        end;
    end;
  end;
end;

procedure TJvSIMConnector.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    DoMouseMove(X - FOldp.X, Y - FOldp.Y);
end;

procedure TJvSIMConnector.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not FDoEdge then
    DisconnectFinal;
  BinCheck(Self);
end;

procedure TJvSIMConnector.DisconnectFinal;
begin
  if FDisCon = nil then
    Exit;
  if FDisCon is TJvSimLight then
    TJvSimLight(FDisCon).Lit := False
  else
  if FDisCon is TJvLogic then
  begin
    if FDisConI = 1 then
      TJvLogic(FDisCon).Input1 := False
    else
    if FDisConI = 2 then
      TJvLogic(FDisCon).Input2 := False
    else
    if FDisConI = 3 then
      TJvLogic(FDisCon).Input3 := False
  end;
end;

procedure TJvSIMConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FromLogic) then
    FromLogic := nil;
  if (Operation = opRemove) and (AComponent = ToLogic) then
    ToLogic := nil;
end;

procedure TJvSIMConnector.Paint;
var
  D, d2, w2, xw, yh: Integer;
begin
  D := FConSize;
  d2 := D div 2;
  w2 := Round(FEdge * Width);
  xw := Width - 1;
  yh := Height - 1;

  with Canvas do
  begin
    Brush.Color := clLime;
    case FShape of
      jcsTLBR:
        // a connector is drawn depending in the FConPos
        begin
          // start new code
          case FConPos of
            jcpTL: // Draw regular connector
              begin
                MoveTo(D, d2);
                LineTo(w2, d2);
                LineTo(w2, yh - d2);
                LineTo(xw - D, yh - d2);
                Brush.Color := clRed;
                Rectangle(0, 0, D, D);
                Brush.Color := clLime;
                Rectangle(xw - D, yh - D, xw, yh);
              end;
            jcpBR:
              begin
                MoveTo(D, d2);
                LineTo(xw - d2, d2);
                LineTo(xw - d2, yh - D);
                Brush.Color := clLime;
                Rectangle(0, 0, D, D);
                Brush.Color := clRed;
                Rectangle(xw - D, yh - D, xw, yh);
              end;
          end;
          // end new code
             {   MoveTo(D,d2);
                LineTo(w2,d2);
                LineTo(w2,yh-d2);
                LineTo(xw-D,yh-d2);
                case FConPos of
                  jcpTL: Brush.Color:=clRed;
                  else Brush.Color:=clLime;
                end;
                Rectangle(0,0,D,D);
                case FConPos of
                  jcpBR: Brush.Color:=clRed;
                  else Brush.Color:=clLime;
                end;
                Rectangle(xw-D,yh-D,xw,yh);}
        end;
      jcsTRBL:
        begin
          // start new code
          case FConPos of
            jcpTR: // Draw reverted connector
              begin
                MoveTo(xw - d2, D);
                LineTo(xw - d2, yh - d2);
                LineTo(D, yh - d2);
                Brush.Color := clRed;
                Rectangle(xw - D, 0, xw, D);
                Brush.Color := clLime;
                Rectangle(0, yh - D, D, yh);
              end;
            jcpBL: // Draw regular connector
              begin
                MoveTo(xw - D, d2);
                LineTo(w2, d2);
                LineTo(w2, yh - d2);
                LineTo(D - 1, yh - d2);
                Brush.Color := clLime;
                Rectangle(xw - D, 0, xw, D);
                Brush.Color := clRed;
                Rectangle(0, yh - D, D, yh);
              end;
          end;
          // end new code
          {      MoveTo(xw-D,d2);
                LineTo(w2,d2);
                LineTo(w2,yh-d2);
                LineTo(D-1,yh-d2);
                case FConPos of
                  jcpTR: Brush.Color:=clRed;
                  else Brush.Color:=clLime;
                end;
                Rectangle(xw-D,0,xw,D);
                case FConPos of
                  jcpBL: Brush.Color:=clRed;
                  else Brush.Color:=clLime;
                end;
                Rectangle(0,yh-D,D,yh);}
        end;
    end;
  end;
end;

procedure TJvSIMConnector.SetFromGate(const Value: Integer);
begin
  FFromGate := Value;
end;

procedure TJvSIMConnector.SetFromLogic(const Value: TJvLogic);
begin
  FFromLogic := Value;
end;

procedure TJvSIMConnector.SetToGate(const Value: Integer);
begin
  FToGate := Value;
end;

procedure TJvSIMConnector.SetToLogic(const Value: TJvLogic);
begin
  FToLogic := Value;
end;

procedure TJvSIMConnector.SetFromPoint(const Value: TJvPointX);
begin
  if Assigned(Value) then
    FFromPoint.Assign(Value);
end;

procedure TJvSIMConnector.SetToPoint(const Value: TJvPointX);
begin
  if Assigned(Value) then
    FToPoint.Assign(Value);
end;

procedure TJvSIMConnector.AnchorCorner(LogTL: TPoint; ACorner: TJvConMode);
var
  Rc: TRect;
begin
  FConMode := ACorner;
  Rc := BoundsRect;
  FConHot := FConPos;
  case ACorner of
    jcmTL:
      begin
        FConOffset := Point(Rc.Left - LogTL.X, Rc.Top - LogTL.Y);
        FConAnchor := Parent.ScreenToClient(ClientToScreen(Point(Width, Height)));
      end;
    jcmTR:
      begin
        FConOffset := Point(Rc.Right - LogTL.X, Rc.Top - LogTL.Y);
        FConAnchor := Parent.ScreenToClient(ClientToScreen(Point(0, Height)));
      end;
    jcmBR:
      begin
        FConOffset := Point(Rc.Right - LogTL.X, Rc.Bottom - LogTL.Y);
        FConAnchor := Parent.ScreenToClient(ClientToScreen(Point(0, 0)));
      end;
    jcmBL:
      begin
        FConOffset := Point(Rc.Left - LogTL.X, Rc.Bottom - LogTL.Y);
        FConAnchor := Parent.ScreenToClient(ClientToScreen(Point(Width, 0)));
      end;
  end;
end;

procedure TJvSIMConnector.MoveConnector(LogTL: TPoint);
var
  nw, nh: Integer;
  D: Integer;
  nc: TPoint;
begin
  D := FConSize;
//  d2 := D div 2;
  nc := Point(LogTL.X + FConOffset.X, LogTL.Y + FConOffset.Y);
  case FConMode of
    jcmTL:
      begin
        nw := FConAnchor.X - nc.X;
        if nw < D then
        begin
          Left := FConAnchor.X - D;
          Width := -nw + D + D;
        end
        else
        begin
          Left := nc.X;
          Width := FConAnchor.X - Left;
        end;
        nh := FConAnchor.Y - nc.Y;

        // adjust new hot position
        if (nw < D) and not (nh < D) then
        begin
          case FConHot of
            jcpTL:
              FConPos := jcpTR;
            jcpBR:
              FConPos := jcpBL;
          end;
          FShape := jcsTRBL;
        end
        else
        if (nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpTL:
              FConPos := jcpBR;
            jcpBR:
              FConPos := jcpTL;
          end;
          FShape := jcsTLBR;
        end
        else
        if (not nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpTL:
              FConPos := jcpBL;
            jcpBR:
              FConPos := jcpTR;
          end;
          FShape := jcsTRBL;
        end
        else
        begin
          case FConHot of
            jcpTL:
              FConPos := jcpTL;
            jcpBR:
              FConPos := jcpBR;
          end;
          FShape := jcsTLBR;
        end;
        // end of adjust TL new hot
        if nh < D then
        begin
          Top := FConAnchor.Y - D;
          Height := -nh + D + D;
        end
        else
        begin
          Top := nc.Y;
          Height := FConAnchor.Y - Top;
        end;
      end;
    jcmTR:
      begin
        nw := nc.X - FConAnchor.X;
        if nw <= 0 then
        begin
          Left := FConAnchor.X + nw - D;
          Width := -nw + D + D;
        end
        else
        if nw <= D then
        begin
          Left := nc.X - D;
          Width := -nw + D + D;
        end
        else
          Width := nw;
        nh := FConAnchor.Y - nc.Y;
        // adjust TR new hot position
        if (nw < D) and (not (nh < D)) then
        begin
          case FConHot of
            jcpTR:
              FConPos := jcpTL;
            jcpBL:
              FConPos := jcpBR;
          end;
          FShape := jcsTLBR;
        end
        else
        if (nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpTR:
              FConPos := jcpBL;
            jcpBL:
              FConPos := jcpTR;
          end;
          FShape := jcsTRBL;
        end
        else
        if (not nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpTR:
              FConPos := jcpBR;
            jcpBL:
              FConPos := jcpTL;
          end;
          FShape := jcsTLBR;
        end
        else
        begin
          case FConHot of
            jcpTR:
              FConPos := jcpTR;
            jcpBL:
              FConPos := jcpBL;
          end;
          FShape := jcsTRBL;
        end;
        // end of adjust TR new hot
        if nh < D then
        begin
          Top := FConAnchor.Y - D;
          Height := -nh + D + D;
        end
        else
        begin
          Top := FConAnchor.Y - nh;
          Height := nh;
        end;
      end;
    jcmBR:
      begin
        nw := nc.X - FConAnchor.X;
        if nw <= 0 then
        begin
          Left := nc.X - D;
          Width := -nw + D + D;
        end
        else
        if nw <= D then
        begin
          Left := nc.X - D;
          Width := -nw + D + D;
        end
        else
          Width := nw;
        nh := nc.Y - FConAnchor.Y;
        // adjust BR new hot position
        if (nw < D) and (not (nh < D)) then
        begin
          case FConHot of
            jcpBR:
              FConPos := jcpBL;
            jcpTL:
              FConPos := jcpTR;
          end;
          FShape := jcsTRBL;
        end
        else
        if (nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpBR:
              FConPos := jcpTL;
            jcpTL:
              FConPos := jcpBR;
          end;
          FShape := jcsTLBR;
        end
        else
        if (not nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpBR:
              FConPos := jcpTR;
            jcpTL:
              FConPos := jcpBL;
          end;
          FShape := jcsTRBL;
        end
        else
        begin
          case FConHot of
            jcpBR:
              FConPos := jcpBR;
            jcpTL:
              FConPos := jcpTL;
          end;
          FShape := jcsTLBR;
        end;
        // end of adjust BR new hot
        if nh < D then
        begin
          Top := FConAnchor.Y + nh - D;
          Height := -nh + D + D;
        end
        else
          Height := nh;
      end;
    jcmBL:
      begin
        nw := FConAnchor.X - nc.X;
        if nw < D then
        begin
          Left := FConAnchor.X - D;
          Width := -nw + D + D;
        end
        else
        begin
          Left := FConAnchor.X - nw;
          Width := nw;
        end;
        nh := nc.Y - FConAnchor.Y;
        // adjust BL new hot position
        if (nw < D) and (not (nh < D)) then
        begin
          case FConHot of
            jcpBL:
              FConPos := jcpBR;
            jcpTR:
              FConPos := jcpTL;
          end;
          FShape := jcsTLBR;
        end
        else
        if (nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpBL:
              FConPos := jcpTR;
            jcpTR:
              FConPos := jcpBL;
          end;
          FShape := jcsTRBL;
        end
        else
        if (not nw < D) and (nh < D) then
        begin
          case FConHot of
            jcpBL:
              FConPos := jcpTL;
            jcpTR:
              FConPos := jcpBR;
          end;
          FShape := jcsTLBR;
        end
        else
        begin
          case FConHot of
            jcpBL:
              FConPos := jcpBL;
            jcpTR:
              FConPos := jcpTR;
          end;
          FShape := jcsTRBL;
        end;
        // end of adjust BL new hot
        if nh < D then
        begin
          Top := FConAnchor.Y + nh - D;
          Height := -nh + D + D;
        end
        else
          Height := nh;
      end;
  end;
end;

procedure TJvSIMConnector.Connect;
var
  Pi, Po: TPoint;
  R: TRect;
  D, d2, xw, yh: Integer;
  Wc: TWinControl;
  Vi: Boolean;
  sBut: TJvSimButton;
  sLog: TJvLogic;
  sLight: TJvSimLight;
  sRev: TJvSimReverse;
  pl: TPoint;

  // convert a corner point to a Parent point

  function ParentPoint(X, Y: Integer): TPoint;
  var
    P: TPoint;
  begin
    P := Point(X, Y);
    P := ClientToScreen(P);
    Result := Wc.ScreenToClient(P);
  end;

  function GetVi: Boolean;
  var
    J: Integer;
  begin
    Result := True;
    for J := 0 to Wc.ControlCount - 1 do
    begin
      if Wc.Controls[J] is TJvSimButton then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Pi) then
        begin
          sBut := TJvSimButton(Wc.Controls[J]);
          Vi := sBut.Down;
          Exit;
        end;
      end
      else
      if Wc.Controls[J] is TJvSimReverse then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, D);
        if PtInRect(R, Pi) then
        begin
          sRev := TJvSimReverse(Wc.Controls[J]);
          // now check if P is the output area
          pl := sRev.Gates[1].Pos;
          R := Rect(sRev.Left + pl.X, sRev.Top - D, sRev.Left + pl.X + 12, sRev.Top + pl.Y + 12);
          if PtInRect(R, Pi) and sRev.Gates[1].Active then
          begin // output
            Vi := sRev.Output1;
            Exit;
          end;
          pl := sRev.Gates[2].Pos;
          R := Rect(sRev.Left - D, sRev.Top + pl.Y, sRev.Left + pl.X + 12, sRev.Top + pl.Y + 12);
          if PtInRect(R, Pi) and sRev.Gates[2].Active then
          begin // output
            Vi := sRev.Output2;
            Exit;
          end;
          pl := sRev.Gates[3].Pos;
          R := Rect(sRev.Left + pl.X, sRev.Top + pl.Y, sRev.Left + pl.X + 12, sRev.Top + sRev.Height + D);
          if PtInRect(R, Pi) and sRev.Gates[3].Active then
          begin // output
            Vi := sRev.Output3;
            Exit;
          end;
        end;
      end
      else
      if Wc.Controls[J] is TJvLogic then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Pi) then
        begin
          sLog := TJvLogic(Wc.Controls[J]);
          // now check if P is in one of the 3 output area's
          R := Rect(sLog.Left + 33, sLog.Top, sLog.Left + sLog.Width + FConSize, sLog.Top + 22);
          if PtInRect(R, Pi) and sLog.Gates[3].Active then
          begin // output is gate 3
            Vi := sLog.Output1;
            Exit;
          end;
          R := Rect(sLog.Left + 33, sLog.Top + 23, sLog.Left + sLog.Width + FConSize, sLog.Top + 44);
          if PtInRect(R, Pi) and sLog.Gates[4].Active then
          begin // output is gate 4
            Vi := sLog.Output2;
            Exit;
          end;
          R := Rect(sLog.Left + 33, sLog.Top + 45, sLog.Left + sLog.Width + FConSize, sLog.Top + 64);
          if PtInRect(R, Pi) and sLog.Gates[5].Active then
          begin // output is gate 5
            Vi := sLog.Output3;
            Exit;
          end;
        end;
      end;
    end;
    Result := False;
  end;

  procedure SetVo;
  var
    J: Integer;
  begin
    for J := 0 to Wc.ControlCount - 1 do
    begin
      if (Wc.Controls[J] is TJvSimLight) then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Po) then
        begin
          sLight := TJvSimLight(Wc.Controls[J]);
          sLight.Lit := Vi;
          Exit;
        end;
      end
      else
      if Wc.Controls[J] is TJvSimReverse then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Po) then
        begin
          sRev := TJvSimReverse(Wc.Controls[J]);
          // now check if P is in the input area
          pl := sRev.Gates[0].Pos;
          R := Rect(sRev.Left + pl.X, sRev.Top + pl.Y, sRev.Left + sRev.Width + D, sRev.Top + pl.Y + 12);
          if PtInRect(R, Po) and sRev.Gates[0].Active then
          begin // input
            sRev.Input1 := Vi;
            Exit;
          end;
        end;
      end
      else
      if Wc.Controls[J] is TJvLogic then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Po) then
        begin
          sLog := TJvLogic(Wc.Controls[J]);
          // now check if P is in one of the 3 input area's
          R := Rect(sLog.Left - D, sLog.Top, sLog.Left + 32, sLog.Top + 22);
          if PtInRect(R, Po) and sLog.Gates[0].Active then
          begin // input is gate 0
            sLog.Input1 := Vi;
            Exit;
          end;
          R := Rect(sLog.Left - D, sLog.Top + 23, sLog.Left + 32, sLog.Top + 44);
          if PtInRect(R, Po) and sLog.Gates[1].Active then
          begin // input is gate 1
            sLog.Input2 := Vi;
            Exit;
          end;
          R := Rect(sLog.Left - D, sLog.Top + 45, sLog.Left + 32, sLog.Top + 64);
          if PtInRect(R, Po) and sLog.Gates[2].Active then
          begin // input is gate 2
            sLog.Input3 := Vi;
            Exit;
          end;
        end;
      end;
    end;
  end;

begin
  // connect input and output using the FConPos
  d2 := FConSize div 2;
  D := FConSize;
  xw := Width - 1;
  yh := Height - 1;
  Wc := Parent;
  case FConPos of
    jcpTL:
      begin
        Pi := ParentPoint(d2, d2);
        Po := ParentPoint(xw - d2, yh - d2);
      end;
    jcpTR:
      begin
        Pi := ParentPoint(xw - d2, d2);
        Po := ParentPoint(d2, yh - d2);
      end;
    jcpBR:
      begin
        Pi := ParentPoint(xw - d2, yh - d2);
        Po := ParentPoint(d2, d2);
      end;
    jcpBL:
      begin
        Pi := ParentPoint(d2, yh - d2);
        Po := ParentPoint(xw - d2, d2);
      end;
  end;
  // get input Vi
  if GetVi then
    SetVo;
end;

procedure TJvSIMConnector.Disconnect;
var
  Pi, Po: TPoint;
  R: TRect;
  D, d2, xw, yh: Integer;
  Wc: TWinControl;
  sLog: TJvLogic;
  sLight: TJvSimLight;

  // convert a corner point to a Parent point

  function ParentPoint(X, Y: Integer): TPoint;
  var
    P: TPoint;
  begin
    P := Point(X, Y);
    P := ClientToScreen(P);
    Result := Wc.ScreenToClient(P);
  end;

  procedure SetVo;
  var
    J: Integer;
  begin
    for J := 0 to Wc.ControlCount - 1 do
    begin
      if Wc.Controls[J] is TJvSimLight then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Po) then
        begin
          sLight := TJvSimLight(Wc.Controls[J]);
          FDisCon := sLight;
          //sLight.Lit:=False;
          Exit;
        end;
      end
      else
      if Wc.Controls[J] is TJvLogic then
      begin
        R := Wc.Controls[J].BoundsRect;
        InflateRect(R, D, 0);
        if PtInRect(R, Po) then
        begin
          sLog := TJvLogic(Wc.Controls[J]);
          // now check if P is in one of the 3 input area's
          R := Rect(sLog.Left - D, sLog.Top, sLog.Left + 32, sLog.Top + 22);
          if PtInRect(R, Po) and sLog.Gates[0].Active then
          begin // input is gate 0
            FDisCon := sLog;
            FDisConI := 1;
            //            sLog.Input1:=False;
            Exit;
          end;
          R := Rect(sLog.Left - D, sLog.Top + 23, sLog.Left + 32, sLog.Top + 44);
          if PtInRect(R, Po) and sLog.Gates[1].Active then
          begin // input is gate 1
            FDisCon := sLog;
            FDisConI := 2;
            //            sLog.Input2:=False;
            Exit;
          end;
          R := Rect(sLog.Left - D, sLog.Top + 45, sLog.Left + 32, sLog.Top + 64);
          if PtInRect(R, Po) and sLog.Gates[2].Active then
          begin // input is gate 2
            FDisCon := sLog;
            FDisConI := 3;
            //            sLog.Input3:=False;
            Exit;
          end;
        end;
      end;
    end;
  end;

begin
  // connect input and output using the FConPos
  FDisCon := nil;
  FDisConI := 0;
  d2 := FConSize div 2;
  D := FConSize;
  xw := Width - 1;
  yh := Height - 1;
  Wc := Parent;
  case FConPos of
    jcpTL:
      begin
        Pi := ParentPoint(d2, d2);
        Po := ParentPoint(xw - d2, yh - d2);
      end;
    jcpTR:
      begin
        Pi := ParentPoint(xw - d2, d2);
        Po := ParentPoint(d2, yh - d2);
      end;
    jcpBR:
      begin
        Pi := ParentPoint(xw - d2, yh - d2);
        Po := ParentPoint(d2, d2);
      end;
    jcpBL:
      begin
        Pi := ParentPoint(d2, yh - d2);
        Po := ParentPoint(xw - d2, d2);
      end;
  end;
  // clear logic inputs and lights
  SetVo;
end;

//=== { TJvLogic } ===========================================================

constructor TJvLogic.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  Width := 65;
  Height := 65;
  // initialize Gates
  FGates[0].Pos := Point(1, 10);
  FGates[1].Pos := Point(1, 28);
  FGates[2].Pos := Point(1, 46);
  FGates[3].Pos := Point(52, 10);
  FGates[4].Pos := Point(52, 28);
  FGates[5].Pos := Point(52, 46);
  for I := 0 to 5 do
    FGates[I].State := False;
  for I := 0 to 2 do
  begin
    FGates[I].Style := jgsDI;
    FGates[I + 3].Style := jgsDO;
  end;
  FLogicFunc := jlfAND;
  FGates[0].Active := True;
  FGates[1].Active := False;
  FGates[2].Active := True;
  FGates[3].Active := False;
  FGates[4].Active := True;
  FGates[5].Active := False;
  FConnectors := TList.Create;
end;

destructor TJvLogic.Destroy;
begin
  FConnectors.Free;
  inherited Destroy;
end;

function TJvLogic.GetGate(Index: Integer): TJvGate;
begin
  Result := FGates[Index];
end;

procedure TJvLogic.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  FDoMove := False;
  FDoStyle := False;
  FStyleDown := False;
  FMdp := Point(X, Y);
  R := ClientRect;
  InflateRect(R, -15, -15);
  FDoStyle := PtInRect(R, FMdp);
  FDoMove := not FDoStyle;
  FOldp := Point(X, Y);
  if FDoMove then
    AnchorConnectors;
  if FDoStyle then
  begin
    FStyleDown := True;
    Invalidate;
  end;
end;

procedure TJvLogic.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := ClientToScreen(Point(X, Y));
  P := Parent.ScreenToClient(P);
  if ssLeft in Shift then
  begin
    if FDoMove then
    begin
      FNewLeft := P.X - FMdp.X;
      FNewTop := P.Y - FMdp.Y;
      MoveConnectors;
      Left := FNewLeft;
      Top := FNewTop;
    end
  end;
end;

procedure TJvLogic.AnchorConnectors;
var
  Wc: TWinControl;
  I: Integer;
  Con: TJvSIMConnector;
  R, Rc: TRect;
  P: TPoint;
begin
  Wc := Parent;
  FConnectors.Clear;
  R := BoundsRect;
  InflateRect(R, 8, 0);
  P := Point(Left, Top);
  for I := 0 to Wc.ControlCount - 1 do
    if Wc.Controls[I] is TJvSIMConnector then
    begin
      Con := TJvSIMConnector(Wc.Controls[I]);
      // check for corners in bounds
      Rc := Con.BoundsRect;
      // TL
      if PtInRect(R, Point(Rc.Left, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTL);
      end
      // TR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTR);
      end
      // BR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBR);
      end
      // BL
      else
      if PtInRect(R, Point(Rc.Left, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBL);
      end
    end;
end;

procedure TJvLogic.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FStyleDown := False;
  if FDoStyle then
  begin
    FDoStyle := False;
    case FLogicFunc of
      jlfAND:
        LogicFunc := jlfOR;
      jlfOR:
        LogicFunc := jlfNOT;
      jlfNOT:
        LogicFunc := jlfAND;
    end;
  end;
  BinCheck(Self);
end;

procedure TJvLogic.PaintLed(Index: Integer);
var
  SurfCol, LitCol: TColor;
  P: TPoint;
  X, Y: Integer;
  Lit: Boolean;
begin
  if not Gates[Index].Active then
    Exit;
  P := Gates[Index].Pos;
  X := P.X;
  Y := P.Y;
  if Index = 0 then
    Lit := FInput1
  else
  if Index = 1 then
    Lit := FInput2
  else
  if Index = 2 then
    Lit := FInput3
  else
  if Index = 3 then
    Lit := FOutput1
  else
  if Index = 4 then
    Lit := FOutput2
  else
  if Index = 5 then
    Lit := FOutput3
  else
    Lit := False;
  if Lit then
  begin
    if Gates[Index].Style = jgsDI then
      SurfCol := clLime
    else
      SurfCol := clRed;
    LitCol := clWhite;
  end
  else
  begin
    if Gates[Index].Style = jgsDI then
    begin
      SurfCol := clGreen;
      LitCol := clLime;
    end
    else
    begin
      SurfCol := clMaroon;
      LitCol := clRed;
    end;
  end;
  with Canvas do
  begin
    Brush.Color := clSilver;
    FillRect(Rect(X, Y, X + 12, Y + 13));
    Brush.Style := bsClear;
    Pen.Color := clGray;
    Ellipse(X, Y, X + 12, Y + 13);
    Pen.Color := clBlack;
    Brush.Color := SurfCol;
    Ellipse(X + 1, Y + 1, X + 11, Y + 12);
    Pen.Color := clWhite;
    Arc(X + 1, Y + 1, X + 11, Y + 12, X + 0, Y + 12, X + 12, Y + 0);
    Pen.Color := LitCol;
    Arc(X + 3, Y + 3, X + 8, Y + 9, X + 5, Y + 0, X + 0, Y + 8);
  end;
end;

procedure TJvLogic.Paint;
var
  I: Integer;
  R: TRect;
  S: string;
begin
  with Canvas do
  begin
    Brush.Color := clSilver;
    R := ClientRect;
    FillRect(R);
    Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
    //     Frame3D(Canvas,R,clBtnShadow,clBtnHighlight,1);
    Brush.Color := clRed;
    for I := 0 to 5 do
      PaintLed(I);
    R := ClientRect;
    InflateRect(R, -15, -15);
    if FStyleDown then
      Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
    else
      Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
    // Draw caption
    case FLogicFunc of
      jlfAND:
        S := 'AND'; // do not localize
      jlfOR:
        S := 'OR'; // do not localize
      jlfNOT:
        S := 'NOT'; // do not localize
    end;
    Brush.Style := bsClear;  
    DrawText(Canvas, S, -1, R, DT_SINGLELINE or DT_CENTER or DT_VCENTER); 
  end;
end;

procedure TJvLogic.Resize;
begin
  Width := 65;
  Height := 65;
end;

procedure TJvLogic.MoveConnectors;
var
  I: Integer;
  Con: TJvSIMConnector;
begin
  for I := 0 to FConnectors.Count - 1 do
  begin
    Con := TJvSIMConnector(FConnectors[I]);
    Con.MoveConnector(Point(FNewLeft, FNewTop));
  end;
end;

procedure TJvLogic.OutCalc;
begin
  case FLogicFunc of
    jlfAND:
      Output2 := Input1 and Input3;
    jlfOR:
      Output2 := Input1 or Input3;
    jlfNOT:
      Output2 := not Input2;
  end;

end;

procedure TJvLogic.SetInput1(const Value: Boolean);
begin
  if Value <> FInput1 then
  begin
    FInput1 := Value;
    Invalidate;
    OutCalc;
  end;
end;

procedure TJvLogic.SetInput2(const Value: Boolean);
begin
  if Value <> FInput2 then
  begin
    FInput2 := Value;
    Invalidate;
    OutCalc;
  end;
end;

procedure TJvLogic.SetInput3(const Value: Boolean);
begin
  if Value <> FInput3 then
  begin
    FInput3 := Value;
    Invalidate;
    OutCalc;
  end;
end;

procedure TJvLogic.SetOutput1(const Value: Boolean);
begin
  if Value <> FOutput1 then
  begin
    FOutput1 := Value;
    Invalidate;
  end;
end;

procedure TJvLogic.SetOutput2(const Value: Boolean);
begin
  if Value <> FOutput2 then
  begin
    FOutput2 := Value;
    Invalidate;
  end;
end;

procedure TJvLogic.SetOutput3(const Value: Boolean);
begin
  if Value <> FOutput3 then
  begin
    FOutput3 := Value;
    Invalidate;
  end;
end;

procedure TJvLogic.SetLogicFunc(const Value: TJvLogicFunc);
begin
  if Value <> FLogicFunc then
  begin
    FLogicFunc := Value;
    case FLogicFunc of
      jlfAND:
        begin
          FGates[0].Active := True;
          FGates[1].Active := False;
          FGates[2].Active := True;
          FGates[3].Active := False;
          FGates[4].Active := True;
          FGates[5].Active := False;
        end;
      jlfOR:
        begin
          FGates[0].Active := True;
          FGates[1].Active := False;
          FGates[2].Active := True;
          FGates[3].Active := False;
          FGates[4].Active := True;
          FGates[5].Active := False;
        end;
      jlfNOT:
        begin
          FGates[0].Active := False;
          FGates[1].Active := True;
          FGates[2].Active := False;
          FGates[3].Active := False;
          FGates[4].Active := True;
          FGates[5].Active := False;
        end;
    end;
    Invalidate;
    OutCalc;
  end;
end;

//=== { TJvSimButton } =======================================================

constructor TJvSimButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDown := False;
  Width := 65;
  Height := 65;
  FConnectors := TList.Create;
end;

destructor TJvSimButton.Destroy;
begin
  FConnectors.Free;
  inherited Destroy;
end;

procedure TJvSimButton.AnchorConnectors;
var
  Wc: TWinControl;
  I: Integer;
  Con: TJvSIMConnector;
  R, Rc: TRect;
  P: TPoint;
begin
  Wc := Parent;
  FConnectors.Clear;
  R := BoundsRect;
  InflateRect(R, 8, 8);
  P := Point(Left, Top);
  for I := 0 to Wc.ControlCount - 1 do
    if Wc.Controls[I] is TJvSIMConnector then
    begin
      Con := TJvSIMConnector(Wc.Controls[I]);
      // check for corners in bounds
      Rc := Con.BoundsRect;
      // TL
      if PtInRect(R, Point(Rc.Left, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTL);
      end
      // TR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTR);
      end
      // BR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBR);
      end
      // BL
      else
      if PtInRect(R, Point(Rc.Left, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBL);
      end
    end;
end;

procedure TJvSimButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  FMdp := Point(X, Y);
  R := ClientRect;
  InflateRect(R, -15, -15);
  FDoMove := not PtInRect(R, FMdp);
  FDepressed := not FDoMove;
  FOldp := Point(X, Y);
  if FDoMove then
    AnchorConnectors
  else
    Invalidate;
end;

procedure TJvSimButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDepressed then
    Exit;
  P := ClientToScreen(Point(X, Y));
  P := Parent.ScreenToClient(P);
  if ssLeft in Shift then
  begin
    if FDoMove then
    begin
      FNewLeft := P.X - FMdp.X;
      FNewTop := P.Y - FMdp.Y;
      MoveConnectors;
      Left := FNewLeft;
      Top := FNewTop;
    end
  end;
end;

procedure TJvSimButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  P: TPoint;
begin
  FDepressed := False;
  P := Point(X, Y);
  R := ClientRect;
  InflateRect(R, -15, -15);
  if PtInRect(R, P) then
  begin
    Down := not FDown;
  end
  else
    BinCheck(Self);
end;

procedure TJvSimButton.MoveConnectors;
var
  I: Integer;
  Con: TJvSIMConnector;
begin
  for I := 0 to FConnectors.Count - 1 do
  begin
    Con := TJvSIMConnector(FConnectors[I]);
    Con.MoveConnector(Point(FNewLeft, FNewTop));
  end;
end;

procedure TJvSimButton.Paint;
var
  P: TPoint;
  R: TRect;
begin
  with Canvas do
  begin
    Brush.Color := clSilver;
    R := ClientRect;
    FillRect(R);
    Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
    InflateRect(R, -15, -15);
    if FDepressed or FDown then
      Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
    else
      Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
    P := Point((Width div 2) - 6, (Height div 2) - 6);
    PaintLed(P, FDown);
  end;
end;

procedure TJvSimButton.PaintLed(Pt: TPoint; Lit: Boolean);
var
  SurfCol, LitCol: TColor;
  X, Y: Integer;
begin
  X := Pt.X;
  Y := Pt.Y;
  if Lit then
  begin
    SurfCol := clRed;
    LitCol := clWhite
  end
  else
  begin
    SurfCol := clMaroon;
    LitCol := clRed;
  end;
  with Canvas do
  begin
    Brush.Color := clSilver;
    FillRect(Rect(X, Y, X + 12, Y + 13));
    Brush.Style := bsClear;
    Pen.Color := clGray;
    Ellipse(X, Y, X + 12, Y + 13);
    Pen.Color := clBlack;
    Brush.Color := SurfCol;
    Ellipse(X + 1, Y + 1, X + 11, Y + 12);
    Pen.Color := clWhite;
    Arc(X + 1, Y + 1, X + 11, Y + 12, X + 0, Y + 12, X + 12, Y + 0);
    Pen.Color := LitCol;
    Arc(X + 3, Y + 3, X + 8, Y + 9, X + 5, Y + 0, X + 0, Y + 8);
  end;
end;

procedure TJvSimButton.Resize;
begin
  Width := 65;
  Height := 65;
end;

procedure TJvSimButton.SetDown(const Value: Boolean);
begin
  if Value <> FDown then
  begin
    FDown := Value;
    FDepressed := Value;
    Invalidate;
  end;
end;

//=== { TJvSimLight } ========================================================

constructor TJvSimLight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLit := False;
  Width := 65;
  Height := 65;
  FColorOn := clLime;
  FColorOff := clGreen;
  FConnectors := TList.Create;
end;

destructor TJvSimLight.Destroy;
begin
  FConnectors.Free;
  inherited Destroy;
end;

procedure TJvSimLight.AnchorConnectors;
var
  Wc: TWinControl;
  I: Integer;
  Con: TJvSIMConnector;
  R, Rc: TRect;
  P: TPoint;
begin
  Wc := Parent;
  FConnectors.Clear;
  R := BoundsRect;
  InflateRect(R, 8, 8);
  P := Point(Left, Top);
  for I := 0 to Wc.ControlCount - 1 do
    if Wc.Controls[I] is TJvSIMConnector then
    begin
      Con := TJvSIMConnector(Wc.Controls[I]);
      // check for corners in bounds
      Rc := Con.BoundsRect;
      // TL
      if PtInRect(R, Point(Rc.Left, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTL);
      end
      // TR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTR);
      end
      // BR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBR);
      end
      // BL
      else
      if PtInRect(R, Point(Rc.Left, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBL);
      end
    end;
end;

procedure TJvSimLight.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMdp := Point(X, Y);
  FDoMove := True;
  FOldp := Point(X, Y);
  AnchorConnectors;
end;

procedure TJvSimLight.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := ClientToScreen(Point(X, Y));
  P := Parent.ScreenToClient(P);
  if ssLeft in Shift then
  begin
    if FDoMove then
    begin
      FNewLeft := P.X - FMdp.X;
      FNewTop := P.Y - FMdp.Y;
      MoveConnectors;
      Left := FNewLeft;
      Top := FNewTop;
    end
  end;
end;

procedure TJvSimLight.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  BinCheck(Self);
end;

procedure TJvSimLight.MoveConnectors;
var
  I: Integer;
  Con: TJvSIMConnector;
begin
  for I := 0 to FConnectors.Count - 1 do
  begin
    Con := TJvSIMConnector(FConnectors[I]);
    Con.MoveConnector(Point(FNewLeft, FNewTop));
  end;
end;

procedure TJvSimLight.Paint;
var
  TlPoly, BrPoly: array [0..2] of TPoint;
  xw, yh: Integer;
  R: TRect;
  HiColor, LoColor, SurfCol: TColor;

  procedure DrawFrame;
  begin
    //   rgn :=  CreatePolygonRgn(TlPoly,3,WINDING);
    //   SelectClipRgn(Canvas.handle,rgn);
    with Canvas do
    begin
      Brush.Color := SurfCol;
      Pen.Color := HiColor;
      Pen.Width := 2;
      Ellipse(15, 15, xw - 15, yh - 15);
    end;
    //   SelectClipRgn(Canvas.handle,0);
    //   DeleteObject(rgn);
    //   rgn :=  CreatePolygonRgn(BrPoly,3,WINDING);
    //   SelectClipRgn(Canvas.handle,rgn);
    with Canvas do
    begin
      Brush.Color := SurfCol;
      Pen.Color := LoColor;
      Pen.Width := 2;
      Arc(15, 15, xw - 15, yh - 15, 0, yh, xw, 0);
      Pen.Width := 1;
    end;
    //   SelectClipRgn(Canvas.handle,0);
    //   DeleteObject(rgn);
  end;

begin
  if Lit then
    SurfCol := ColorOn
  else
    SurfCol := ColorOff;
  Canvas.Brush.Style := bsSolid;
  R := ClientRect;
  Canvas.Brush.Color := clSilver;
  Canvas.FillRect(R);
  Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
  xw := Width - 1;
  yh := Height - 1;
//  cr := Width div 4;
//  x4 := Width div 4;
  // topleft region
  TlPoly[0] := Point(Left, Top + yh);
  TlPoly[1] := Point(Left, Top);
  TlPoly[2] := Point(Left + xw, Top);
  // Bottom Right region
  BrPoly[0] := Point(Left + xw, Top);
  BrPoly[1] := Point(Left + xw, Top + yh);
  BrPoly[2] := Point(Left, Top + yh);
  Canvas.Pen.Style := psSolid;
  HiColor := clBtnHighlight;
  LoColor := clBtnShadow;
  DrawFrame;
end;

procedure TJvSimLight.Resize;
begin
  Width := 65;
  Height := 65;
end;

procedure TJvSimLight.SetLit(const Value: Boolean);
begin
  if Value <> FLit then
  begin
    FLit := Value;
    Invalidate;
  end;
end;

procedure TJvSimLight.SetColorOff(const Value: TColor);
begin
  if Value <> FColorOff then
  begin
    FColorOff := Value;
    Invalidate;
  end;
end;

procedure TJvSimLight.SetColorOn(const Value: TColor);
begin
  if Value <> FColorOn then
  begin
    FColorOn := Value;
    Invalidate;
  end;
end;

//=== { TJvSimBin } ==========================================================

constructor TJvSimBin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 65;
  Height := 65;
  FBmpBin := TBitmap.Create;
  FBmpBin.LoadFromResourceName(HInstance, 'RBIN'); // do not localize
end;

destructor TJvSimBin.Destroy;
begin
  FBmpBin.Free;
  inherited Destroy;
end;

procedure TJvSimBin.Paint;
var
  Rf: TRect;
begin
  Rf := ClientRect;
  Canvas.Brush.Color := clSilver;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Frame3D(Canvas, Rf, clBtnHighlight, clBtnShadow, 1);
  Canvas.Draw(16, 16, FBmpBin);
end;

procedure TJvSimBin.Resize;
begin
  inherited Resize;
  Width := 65;
  Height := 65;
end;

//=== { TJvSimLogicBox } =====================================================

constructor TJvSimLogicBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 130;
  Height := 65;
  FBmpCon := TBitmap.Create;
  FBmpLogic := TBitmap.Create;
  FBmpButton := TBitmap.Create;
  FBmpLight := TBitmap.Create;
  FBmpRev := TBitmap.Create;
  FBmpBin := TBitmap.Create;
  FBmpCon.LoadFromResourceName(HInstance, 'RCON'); // do not localize
  FBmpLogic.LoadFromResourceName(HInstance, 'RLOGIC'); // do not localize
  FBmpButton.LoadFromResourceName(HInstance, 'RBUTTON'); // do not localize
  FBmpLight.LoadFromResourceName(HInstance, 'RLIGHT'); // do not localize
  FBmpRev.LoadFromResourceName(HInstance, 'RREV'); // do not localize
  FBmpBin.LoadFromResourceName(HInstance, 'RBIN'); // do not localize
  FRCon := Rect(0, 0, 32, 32);
  FRLogic := Rect(33, 0, 64, 32);
  FRButton := Rect(0, 33, 32, 64);
  FRLight := Rect(33, 33, 64, 64);
  FRRev := Rect(65, 0, 97, 32);
  FDCon := False;
  FDLogic := False;
  FDButton := False;
  FDLight := False;
  FDRev := False;
  FCpu := TTimer.Create(Self);
  FCpu.Enabled := False;
  FCpu.OnTimer := CpuOnTimer;
  FCpu.Interval := 50;
end;

destructor TJvSimLogicBox.Destroy;
begin
  FCpu.Free;
  FBmpCon.Free;
  FBmpLogic.Free;
  FBmpButton.Free;
  FBmpLight.Free;
  FBmpRev.Free;
  FBmpBin.Free;
  inherited Destroy;
end;

procedure TJvSimLogicBox.Loaded;
begin
  inherited Loaded;
  FCpu.Enabled := True;
end;

procedure TJvSimLogicBox.CpuOnTimer(Sender: TObject);
var
  Wc: TWinControl;
  I: Integer;
begin
  Wc := Parent;
  // reset inputs
{  for I:=0 to Wc.ControlCount-1 do
    if (Wc.Controls[I] is TJvLogic) then
    begin
      sLogic:=TJvLogic(Wc.Controls[I]);
      for j:=0 to 2 do
        sLogic.FGates[j].State:=False;
    end
    else
    if (Wc.Controls[I] is TJvSimLight) then
    begin
      sLight:=TJvSimLight(Wc.Controls[I]);
      sLight.Lit:=False;
    end;}
  // make connections
  for I := 0 to Wc.ControlCount - 1 do
    if Wc.Controls[I] is TJvSIMConnector then
      TJvSIMConnector(Wc.Controls[I]).Connect;
end;

procedure TJvSimLogicBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := Point(X, Y);
  FDCon := False;
  FDLogic := False;
  FDButton := False;
  FDLight := False;
  if PtInRect(FRCon, P) then
    FDCon := True
  else
  if PtInRect(FRLogic, P) then
    FDLogic := True
  else
  if PtInRect(FRButton, P) then
    FDButton := True
  else
  if PtInRect(FRLight, P) then
    FDLight := True
  else
  if PtInRect(FRRev, P) then
    FDRev := True;
  Invalidate;
end;

procedure TJvSimLogicBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Wc: TWinControl;
  l, t: Integer;
begin
  Wc := Parent;
  l := Left;
  t := Top + Height + 10;
  if FDCon then
    with TJvSIMConnector.Create(Wc) do
    begin
      Parent := Wc;
      Left := l;
      Top := t;
    end
  else
  if FDLogic then
    with TJvLogic.Create(Wc) do
    begin
      Parent := Wc;
      Left := l;
      Top := t;
    end
  else
  if FDButton then
    with TJvSimButton.Create(Wc) do
    begin
      Parent := Wc;
      Left := l;
      Top := t;
    end
  else
  if FDLight then
    with TJvSimLight.Create(Wc) do
    begin
      Parent := Wc;
      Left := l;
      Top := t;
    end
  else
  if FDRev then
    with TJvSimReverse.Create(Wc) do
    begin
      Parent := Wc;
      Left := l;
      Top := t;
    end;
  FDCon := False;
  FDLogic := False;
  FDButton := False;
  FDLight := False;
  FDRev := False;
  Invalidate;
end;

procedure TJvSimLogicBox.Paint;
var
  Rb: TRect;
begin
  with Canvas do
  begin
    Brush.Color := clSilver;
    FillRect(ClientRect);
    Rb := FRCon;
    if not FDCon then
      Frame3D(Canvas, Rb, clBtnHighlight, clBtnShadow, 1)
    else
      Frame3D(Canvas, Rb, clBtnShadow, clBtnHighlight, 1);
    Draw(4, 4, FBmpCon);
    Rb := FRLogic;
    if not FDLogic then
      Frame3D(Canvas, Rb, clBtnHighlight, clBtnShadow, 1)
    else
      Frame3D(Canvas, Rb, clBtnShadow, clBtnHighlight, 1);
    Draw(36, 4, FBmpLogic);
    Rb := FRButton;
    if not FDButton then
      Frame3D(Canvas, Rb, clBtnHighlight, clBtnShadow, 1)
    else
      Frame3D(Canvas, Rb, clBtnShadow, clBtnHighlight, 1);
    Draw(4, 36, FBmpButton);
    Rb := FRLight;
    if not FDLight then
      Frame3D(Canvas, Rb, clBtnHighlight, clBtnShadow, 1)
    else
      Frame3D(Canvas, Rb, clBtnShadow, clBtnHighlight, 1);
    Draw(36, 36, FBmpLight);
    Rb := FRRev;
    if not FDRev then
      Frame3D(Canvas, Rb, clBtnHighlight, clBtnShadow, 1)
    else
      Frame3D(Canvas, Rb, clBtnShadow, clBtnHighlight, 1);
    Draw(Rb.Left + 3, Rb.Top + 3, FBmpRev);

    // Draw bin
    Draw(100, 16, FBmpBin);
  end;
end;

procedure TJvSimLogicBox.Resize;
begin
  Width := 130;
  Height := 65;
end;

//=== { TJvSimReverse } ======================================================

constructor TJvSimReverse.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  Width := 42;
  Height := 42;
  // initialize Gates
  FGates[0].Pos := Point(28, 14);
  FGates[1].Pos := Point(14, 1);
  FGates[2].Pos := Point(1, 14);
  FGates[3].Pos := Point(14, 28);
  for I := 0 to 3 do
  begin
    FGates[I].State := False;
    FGates[I].Active := True;
    FGates[I].Style := jgsDO;
  end;
  FGates[0].Style := jgsDI;
  FConnectors := TList.Create;
end;

destructor TJvSimReverse.Destroy;
begin
  FConnectors.Free;
  inherited Destroy;
end;

procedure TJvSimReverse.AnchorConnectors;
var
  Wc: TWinControl;
  I: Integer;
  Con: TJvSIMConnector;
  R, Rc: TRect;
  P: TPoint;
begin
  Wc := Parent;
  FConnectors.Clear;
  R := BoundsRect;
  InflateRect(R, 8, 0);
  P := Point(Left, Top);
  for I := 0 to Wc.ControlCount - 1 do
    if Wc.Controls[I] is TJvSIMConnector then
    begin
      Con := TJvSIMConnector(Wc.Controls[I]);
      // check for corners in bounds
      Rc := Con.BoundsRect;
      // TL
      if PtInRect(R, Point(Rc.Left, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTL);
      end
        // TR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Top)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmTR);
      end
        // BR
      else
      if PtInRect(R, Point(Rc.Right, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBR);
      end
        // BL
      else
      if PtInRect(R, Point(Rc.Left, Rc.Bottom)) then
      begin
        FConnectors.Add(Con);
        Con.AnchorCorner(P, jcmBL);
      end
    end;
end;

function TJvSimReverse.GetGate(Index: Integer): TJvGate;
begin
  Result := FGates[Index];
end;

procedure TJvSimReverse.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMdp := Point(X, Y);
  FOldp := Point(X, Y);
  FDoMove := True;
  AnchorConnectors;
end;

procedure TJvSimReverse.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := ClientToScreen(Point(X, Y));
  P := Parent.ScreenToClient(P);
  if ssLeft in Shift then
  begin
    if FDoMove then
    begin
      FNewLeft := P.X - FMdp.X;
      FNewTop := P.Y - FMdp.Y;
      MoveConnectors;
      Left := FNewLeft;
      Top := FNewTop;
    end
  end;
end;

procedure TJvSimReverse.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  BinCheck(Self);
end;

procedure TJvSimReverse.MoveConnectors;
var
  I: Integer;
  Con: TJvSIMConnector;
begin
  for I := 0 to FConnectors.Count - 1 do
  begin
    Con := TJvSIMConnector(FConnectors[I]);
    Con.MoveConnector(Point(FNewLeft, FNewTop));
  end;
end;

procedure TJvSimReverse.OutCalc;
begin
  Output1 := Input1;
  Output2 := Input1;
  Output3 := Input1;
end;

procedure TJvSimReverse.Paint;
var
  I: Integer;
  R: TRect;
  Poly: array [0..2] of TPoint;
begin
  with Canvas do
  begin
    Brush.Color := clSilver;
    R := ClientRect;
    FillRect(R);
    Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
    Brush.Color := clRed;
    for I := 0 to 3 do
      PaintLed(I);
    R := ClientRect;
    // paint triangle
    Poly[0] := Point(14, 20);
    Poly[1] := Point(26, 14);
    Poly[2] := Point(26, 26);
    Pen.Style := psClear;
    Brush.Color := clBlack;
    Polygon(Poly);
    Pen.Style := psSolid;
  end;
end;

procedure TJvSimReverse.PaintLed(Index: Integer);
var
  SurfCol, LitCol: TColor;
  P: TPoint;
  X, Y: Integer;
  Lit: Boolean;
begin
  if not Gates[Index].Active then
    Exit;
  P := Gates[Index].Pos;
  X := P.X;
  Y := P.Y;
  if Index = 0 then
    Lit := Input1
  else
  if Index = 1 then
    Lit := Output1
  else
  if Index = 2 then
    Lit := Output2
  else
  if Index = 3 then
    Lit := Output3
  else
    Lit := False;
  if Lit then
  begin
    if Gates[Index].Style = jgsDI then
      SurfCol := clLime
    else
      SurfCol := clRed;
    LitCol := clWhite;
  end
  else
  begin
    if Gates[Index].Style = jgsDI then
    begin
      SurfCol := clGreen;
      LitCol := clLime;
    end
    else
    begin
      SurfCol := clMaroon;
      LitCol := clRed;
    end;
  end;
  with Canvas do
  begin
    Brush.Color := clSilver;
    FillRect(Rect(X, Y, X + 12, Y + 13));
    Brush.Style := bsClear;
    Pen.Color := clGray;
    Ellipse(X, Y, X + 12, Y + 13);
    Pen.Color := clBlack;
    Brush.Color := SurfCol;
    Ellipse(X + 1, Y + 1, X + 11, Y + 12);
    Pen.Color := clWhite;
    Arc(X + 1, Y + 1, X + 11, Y + 12, X + 0, Y + 12, X + 12, Y + 0);
    Pen.Color := LitCol;
    Arc(X + 3, Y + 3, X + 8, Y + 9, X + 5, Y + 0, X + 0, Y + 8);
  end;
end;

procedure TJvSimReverse.Resize;
begin
  Width := 42;
  Height := 42;
end;

procedure TJvSimReverse.SetInput1(const Value: Boolean);
begin
  if Value <> FInput1 then
  begin
    FInput1 := Value;
    Invalidate;
    OutCalc;
  end;
end;

procedure TJvSimReverse.SetOutput1(const Value: Boolean);
begin
  if Value <> FOutput1 then
  begin
    FOutput1 := Value;
    Invalidate;
  end;
end;

procedure TJvSimReverse.SetOutput2(const Value: Boolean);
begin
  if Value <> FOutput2 then
  begin
    FOutput2 := Value;
    Invalidate;
  end;
end;

procedure TJvSimReverse.SetOutput3(const Value: Boolean);
begin
  if Value <> FOutput3 then
  begin
    FOutput3 := Value;
    Invalidate;
  end;
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

