{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimLogic.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvSimLogic;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, extctrls;

{ Copyright 2000 by Jan Verhoeven
This unit includes several visual logic blocks that can be used without any programming.
It is the start of a whole series of simulation blocks.

There is a string seperation between the visual part and functionality.

The user creates and removes blocks; joins and moves them.

The functionality is created every 50 msec in the onTimer event of TJvSimLogicBox.

No programming is required, just drop a TJvLogicBox in the corner of a form and Build the program.

All the rest is up to the user.
}

type
  TJvLogic = class;

  TjanGateStyle = (jgsDI, jgsDO);
  TJvLogicFunc = (jlfAND, jlfOR, jlfNOT);
  TjanGate = record
    Style: TjanGateStyle;
    State: boolean;
    Active: Boolean;
    pos: TPoint;
  end;

  TjanConMode = (jcmTL, jcmTR, jcmBR, jcmBL);
  TjanConPos = (jcpTL, jcpTR, jcpBR, jcpBL);
  TjanConShape = (jcsTLBR, jcsTRBL);

  TJvSIMConnector = class(TGraphicControl)
  private
    { Private declarations }
    mdp: TPoint;
    oldp: TPoint;
    ConAnchor: TPoint;
    ConOffset: TPoint;
    ConMode: TjanConMode;
    ConHot: TjanConPos;
    doMove: boolean;
    doEdge: boolean;
    DisCon: Tcontrol;
    DisConI: integer;
    Mode: TjanConMode;
    Shape: TjanConShape;
    conSize: integer;
    conPos: TjanConPos;
    Edge: extended;
    FFromLogic: TJvLogic;
    FToLogic: TJvLogic;
    FFromGate: integer;
    FToGate: integer;
    FFromPoint: TPoint;
    FToPoint: TPoint;
    procedure SetFromLogic(const Value: TJvLogic);
    procedure SetToLogic(const Value: TJvLogic);
    procedure SetFromGate(const Value: integer);
    procedure SetToGate(const Value: integer);
    procedure SetFromPoint(const Value: TPoint);
    procedure SetToPoint(const Value: TPoint);
    procedure DisConnectFinal;
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoMouseDown(x, y: integer);
    procedure DoMouseMove(dx, dy: integer);
    procedure AnchorCorner(logTL: TPoint; ACorner: TjanConMode);
    procedure MoveConnector(logTL: TPoint);
    procedure Connect;
    procedure DisConnect;
  published
    { Published declarations }
    property FromLogic: TJvLogic read FFromLogic write SetFromLogic;
    property FromGate: integer read FFromGate write SetFromGate;
    property FromPoint: TPoint read FFromPoint write SetFromPoint;
    property ToLogic: TJvLogic read FToLogic write SetToLogic;
    property ToGate: integer read FToGate write SetToGate;
    property ToPoint: TPoint read FToPoint write SetToPoint;
  end;

  TJvLogic = class(TGraphicControl)
  private
    doMove: boolean;
    doStyle: boolean;
    StyleDown: boolean;
    mdp: TPoint;
    oldp: TPoint;
    FGates: array[0..5] of TjanGate;
    Connectors: TList;
    newLeft: integer;
    newTop: integer;
    FOutPut1: boolean;
    FInput2: boolean;
    FOutPut3: boolean;
    FInput3: boolean;
    FOutPut2: boolean;
    FInput1: boolean;
    FLogicFunc: TJvLogicFunc;
    function GetGate(Index: Integer): TjanGate;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure PaintLed(index: integer);
    procedure SetInput1(const Value: boolean);
    procedure SetInput2(const Value: boolean);
    procedure SetInput3(const Value: boolean);
    procedure SetOutPut1(const Value: boolean);
    procedure SetOutPut2(const Value: boolean);
    procedure SetOutPut3(const Value: boolean);
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
    property Gates[index: integer]: TjanGate read GetGate;
  published
    property Input1: boolean read FInput1 write SetInput1;
    property Input2: boolean read FInput2 write SetInput2;
    property Input3: boolean read FInput3 write SetInput3;
    property OutPut1: boolean read FOutPut1 write SetOutPut1;
    property OutPut2: boolean read FOutPut2 write SetOutPut2;
    property OutPut3: boolean read FOutPut3 write SetOutPut3;
    property LogicFunc: TJvLogicFunc read FLogicFunc write SetLogicFunc;
  end;

  TJvSimReverse = class(TGraphicControl)
  private
    doMove: boolean;
    mdp: TPoint;
    oldp: TPoint;
    FGates: array[0..3] of TjanGate;
    Connectors: TList;
    newLeft: integer;
    newTop: integer;
    FOutPut1: boolean;
    FInput1: boolean;
    FOutPut3: boolean;
    FOutPut2: boolean;
    function GetGate(Index: Integer): TjanGate;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure PaintLed(index: integer);
    procedure SetInput1(const Value: boolean);
    procedure SetOutPut1(const Value: boolean);
    procedure OutCalc;
    procedure SetOutPut2(const Value: boolean);
    procedure SetOutPut3(const Value: boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Gates[index: integer]: TjanGate read GetGate;
  published
    property Input1: boolean read FInput1 write SetInput1;
    property OutPut1: boolean read FOutPut1 write SetOutPut1;
    property OutPut2: boolean read FOutPut2 write SetOutPut2;
    property OutPut3: boolean read FOutPut3 write SetOutPut3;
  end;

  TJvSimButton = class(TGraphicControl)
  private
    doMove: boolean;
    mdp: TPoint;
    oldp: TPoint;
    Connectors: TList;
    FDown: boolean;
    FDepressed: boolean;
    newLeft: integer;
    newTop: integer;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure PaintLed(pt: TPoint; lit: boolean);
    procedure SetDown(const Value: boolean);
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
    property Down: boolean read FDown write SetDown;
  end;

  TJvSimLight = class(TGraphicControl)
  private
    doMove: boolean;
    mdp: TPoint;
    oldp: TPoint;
    Connectors: TList;
    FLit: boolean;
    FOnColor: TColor;
    FOffColor: TColor;
    newleft: integer;
    newtop: integer;
    procedure AnchorConnectors;
    procedure MoveConnectors;
    procedure SetLit(const Value: boolean);
    procedure SetOffColor(const Value: TColor);
    procedure SetOnColor(const Value: TColor);
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
    property Lit: boolean read FLit write SetLit;
    property OnColor: TColor read FOnColor write SetOnColor;
    property OffColor: TColor read FOffColor write SetOffColor;
  end;

  TjanSimBin = class(TGraphicControl)
  private
    bmBin: Tbitmap;
  protected
    procedure resize; override;
  public
    constructor create(AOwner: Tcomponent); override;
    destructor destroy; override;
    procedure paint; override;
  published
  end;

  TJvSimLogicBox = class(TGraphicControl)
  private
    cpu: TTimer;
    bmCon: Tbitmap;
    RCon: TRect;
    DCon: boolean;
    bmLogic: Tbitmap;
    RLogic: TRect;
    DLogic: boolean;
    bmButton: Tbitmap;
    RButton: TRect;
    DButton: boolean;
    bmLight: Tbitmap;
    RLight: TRect;
    DLight: boolean;
    bmRev: Tbitmap;
    RRev: TRect;
    DRev: boolean;
    bmBin: TBitmap;
    procedure cpuOnTimer(sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure resize; override;
    procedure Loaded; override;
  public
    constructor create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
  end;

implementation

{$R ..\resources\JvSimImages.res}

procedure BinCheck(Acontrol: Tcontrol);
// general bin procedure
var
  wc: TWinControl;
  i: integer;
  R, Rb: TRect;
  keep: boolean;
begin
  // check for TJvSimLogicBox
  wc := AControl.parent;
  R := AControl.boundsrect;
  keep := false;
  for i := 0 to wc.ControlCount - 1 do
    if (wc.controls[i] is TJvSimLogicBox) then
    begin
      Rb := wc.controls[i].BoundsRect;
      Rb.left := Rb.right - 32;
      if ptinrect(Rb, point(R.left, R.top)) then
        break
      else if ptinrect(Rb, point(R.right, R.top)) then
        break
      else if ptinrect(Rb, point(R.right, R.bottom)) then
        break
      else if ptinrect(Rb, point(R.left, R.bottom)) then
        break
      else
        keep := true;
    end;
  if not keep then AControl.free;
end;

{ TJvSIMConnector }

constructor TJvSIMConnector.Create(AOwner: TComponent);
begin
  inherited;
  width := 100;
  height := 50;
  mode := jcmTL;
  shape := jcsTLBR;
  conSize := 8;
  conPos := jcpTL;
  Edge := 0.5;
end;

procedure TJvSIMConnector.DoMouseDown(x, y: integer);
var
  p: Tpoint;
  Rtl, Rbr, Rtr, Rbl: TRect;
  d: integer;
begin
  doMove := false;
  doEdge := false;
  d := conSize;
  oldp := point(x, y);
  Rtl := rect(0, 0, d, d);
  Rbr := rect(width - 1 - d, height - 1 - d, width - 1, height - 1);
  Rtr := rect(width - 1 - d, 0, width - 1, d);
  Rbl := rect(0, height - 1 - d, d, height - 1);
  p := point(x, y);
  if ptinrect(Rtl, p) and (shape = jcsTLBR) then
  begin
    mode := jcmTL;
    mdp := point(x, y);
  end
  else if ptinrect(Rtr, p) and (shape = jcsTRBL) then
  begin
    mode := jcmTR;
    mdp := point(width - x, y);
  end
  else if ptinrect(Rbr, p) and (shape = jcsTLBR) then
  begin
    mode := jcmBR;
    mdp := point(width - x, height - y);
  end
  else if ptinrect(Rbl, p) and (shape = jcsTRBL) then
  begin
    mode := jcmBL;
    mdp := point(x, height - y);
  end
  else if (abs(x - round(Edge * width)) < 10) then
  begin
    doEdge := true;
  end
  else
  begin
    doMove := true;
    mdp := point(x, y);
    SetFromLogic(nil);
    SetToLogic(nil);
  end;
  if not doEdge then
    DisConnect;

end;

procedure TJvSIMConnector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  doMouseDown(x, y);
end;

procedure TJvSIMConnector.DoMouseMove(dx, dy: integer);
var
  p: Tpoint;
  d, d2, nw, nh: integer;
  x, y: integer;
begin
  x := dx + oldp.x;
  y := dy + oldp.y;
  oldp := point(x, y);
  p := clienttoscreen(point(x, y));
  p := parent.ScreenToClient(p);
  d := conSize;
  d2 := d div 2;
  if doEdge then
  begin
    Edge := x / width;
    invalidate;
  end
  else if doMove then
  begin
    left := p.x - mdp.x;
    top := p.y - mdp.y;
  end
  else
  begin
    case mode of
      jcmTL:
        begin
          left := p.x - mdp.x;
          top := p.y - mdp.y;
          nw := width + (mdp.x - X);
          if nw < d2 then
          begin
            left := left + nw - d;
            width := -nw + d + d;
            mode := jcmTR;
            shape := jcsTRBL;
            case conPos of
              jcpTL: conPos := jcpTR;
              jcpBR: conPos := jcpBL;
            end;
            Edge := 1 - Edge;
          end
          else
            width := nw;
          nh := height + (mdp.y - Y);
          if nh < d2 then
          begin
            top := top + nh - d;
            height := -nh + d + d;
            mode := jcmBL;
            shape := jcsTRBL;
            case conPos of
              jcpTL: conPos := jcpBL;
              jcpBR: conPos := jcpTR;
            end;
          end
          else
            height := nh;
        end;
      jcmTR:
        begin
          top := p.y - mdp.y;
          nw := X + mdp.x;
          if nw < d2 then
          begin
            left := left + nw - d;
            width := -nw + d + d;
            mode := jcmTL;
            shape := jcsTLBR;
            case conPos of
              jcpTR: conPos := jcpTL;
              jcpBL: conPos := jcpBR;
            end;
            Edge := 1 - Edge;
          end
          else
            width := nw;
          nh := height + (mdp.y - Y);
          if nh < d2 then
          begin
            top := top + nh - d;
            height := -nh + d + d;
            mode := jcmBR;
            shape := jcsTLBR;
            case conPos of
              jcpTR: conPos := jcpBR;
              jcpBL: conPos := jcpTL;
            end;
          end
          else
            height := nh;
        end;
      jcmBR:
        begin
          nw := X + mdp.x;
          if nw < d2 then
          begin
            left := left + nw - d;
            width := -nw + d + d;
            mode := jcmBL;
            shape := jcsTRBL;
            case conPos of
              jcpBR: conPos := jcpBL;
              jcpTL: conPos := jcpTR;
            end;
            Edge := 1 - Edge;
          end
          else
            width := nw;
          nh := Y + mdp.y;
          if nh < d2 then
          begin
            top := top + nh - d;
            height := -nh + d + d;
            mode := jcmTR;
            shape := jcsTRBL;
            case conPos of
              jcpBR: conPos := jcpTR;
              jcpTL: conPos := jcpBL;
            end;
          end
          else
            height := nh;
        end;
      jcmBL:
        begin
          left := p.x - mdp.x;
          nw := width + (mdp.x - x);
          if nw < d2 then
          begin
            left := left + nw - d;
            width := -nw + d + d;
            mode := jcmBR;
            shape := jcsTLBR;
            case conPos of
              jcpBL: conPos := jcpBR;
              jcpTR: conPos := jcpTL;
            end;
            Edge := 1 - Edge;
          end
          else
            width := nw;
          nh := Y + mdp.y;
          if nh < d2 then
          begin
            top := top + nh - d;
            height := -nh + d + d;
            mode := jcmTL;
            shape := jcsTLBR;
            case conPos of
              jcpBL: conPos := jcpTL;
              jcpTR: conPos := jcpBR;
            end;
          end
          else
            height := nh;
        end;
    end;
  end;
end;

procedure TJvSIMConnector.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssleft in shift) then
  begin
    doMouseMove(x - oldp.x, y - oldp.y);
  end;
end;

procedure TJvSIMConnector.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not doEdge then
    DisConnectFinal;
  BinCheck(self);
end;

procedure TJvSIMConnector.DisConnectFinal;
begin
  if DisCon = nil then exit;
  if (Discon is TJvSimLight) then
  begin
    TJvSimLight(Discon).lit := false;
  end
  else if (DisCon is TJvLogic) then
  begin
    if DisConI = 1 then
      TJvLogic(DisCon).input1 := false
    else if DisConI = 2 then
      TJvLogic(DisCon).input2 := false
    else if DisConI = 3 then
      TJvLogic(DisCon).input3 := false
  end;
end;

procedure TJvSIMConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (operation = opremove) and (AComponent = FFromLogic) then
    SetFromLogic(nil);
  if (operation = opremove) and (AComponent = FToLogic) then
    SetToLogic(nil);
end;

procedure TJvSIMConnector.paint;
var
  d, d2, w2, xw, yh: integer;
begin
  d := conSize;
  d2 := d div 2;
  w2 := round(Edge * width);
  xw := width - 1;
  yh := height - 1;

  with canvas do
  begin
    brush.color := cllime;
    case shape of
      jcsTLBR:
        // a connector is drawn depending in the conPos
        begin
          // start new code
          case conPos of
            jcpTL: // draw regular connector
              begin
                moveto(d, d2);
                lineto(w2, d2);
                lineto(w2, yh - d2);
                lineto(xw - d, yh - d2);
                brush.color := clred;
                rectangle(0, 0, d, d);
                brush.color := cllime;
                rectangle(xw - d, yh - d, xw, yh);
              end;
            jcpBR:
              begin
                moveto(d, d2);
                lineto(xw - d2, d2);
                lineto(xw - d2, yh - d);
                brush.color := cllime;
                rectangle(0, 0, d, d);
                brush.color := clred;
                rectangle(xw - d, yh - d, xw, yh);
              end;
          end;
          // end new code
             {   moveto(d,d2);
                lineto(w2,d2);
                lineto(w2,yh-d2);
                lineto(xw-d,yh-d2);
                case conPos of
                  jcpTL: brush.color:=clred;
                  else brush.color:=cllime;
                end;
                rectangle(0,0,d,d);
                case conPos of
                  jcpBR: brush.color:=clred;
                  else brush.color:=cllime;
                end;
                rectangle(xw-d,yh-d,xw,yh);}
        end;
      jcsTRBL:
        begin
          // start new code
          case conPos of
            jcpTR: // draw reverted connector
              begin
                moveto(xw - d2, d);
                lineto(xw - d2, yh - d2);
                lineto(d, yh - d2);
                brush.color := clred;
                rectangle(xw - d, 0, xw, d);
                brush.color := cllime;
                rectangle(0, yh - d, d, yh);
              end;
            jcpBL: // draw regular connector
              begin
                moveto(xw - d, d2);
                lineto(w2, d2);
                lineto(w2, yh - d2);
                lineto(d - 1, yh - d2);
                brush.color := cllime;
                rectangle(xw - d, 0, xw, d);
                brush.color := clred;
                rectangle(0, yh - d, d, yh);
              end;
          end;
          // end new code
          {      moveto(xw-d,d2);
                lineto(w2,d2);
                lineto(w2,yh-d2);
                lineto(d-1,yh-d2);
                case conPos of
                  jcpTR: brush.color:=clred;
                  else brush.color:=cllime;
                end;
                rectangle(xw-d,0,xw,d);
                case conPos of
                  jcpBL: brush.color:=clred;
                  else brush.color:=cllime;
                end;
                rectangle(0,yh-d,d,yh);}
        end;
    end; // case
  end; //canvas
end;

procedure TJvSIMConnector.SetFromGate(const Value: integer);
begin
  FFromGate := Value;
end;

procedure TJvSIMConnector.SetFromLogic(const Value: TJvLogic);
begin
  FFromLogic := Value;
end;

procedure TJvSIMConnector.SetToGate(const Value: integer);
begin
  FToGate := Value;
end;

procedure TJvSIMConnector.SetToLogic(const Value: TJvLogic);
begin
  FToLogic := Value;
end;

procedure TJvSIMConnector.SetFromPoint(const Value: TPoint);
begin
  FFromPoint := Value;
end;

procedure TJvSIMConnector.SetToPoint(const Value: TPoint);
begin
  FToPoint := Value;
end;

procedure TJvSIMConnector.AnchorCorner(logTL: TPoint; ACorner: TjanConMode);
var
  Rc: TRect;
begin
  ConMode := ACorner;
  Rc := boundsrect;
  ConHot := ConPos;
  case Acorner of
    jcmTL:
      begin
        ConOffset := point(Rc.left - logTL.x, Rc.top - logTL.y);
        ConAnchor := parent.ScreenToClient(clienttoscreen(point(width, height)));
      end;
    jcmTR:
      begin
        ConOffset := point(Rc.Right - logTL.x, Rc.top - logTL.y);
        ConAnchor := parent.ScreenToClient(clienttoscreen(point(0, height)));
      end;
    jcmBR:
      begin
        ConOffset := point(Rc.Right - logTL.x, Rc.bottom - logTL.y);
        ConAnchor := parent.ScreenToClient(clienttoscreen(point(0, 0)));
      end;
    jcmBL:
      begin
        ConOffset := point(Rc.left - logTL.x, Rc.bottom - logTL.y);
        ConAnchor := parent.ScreenToClient(clienttoscreen(point(width, 0)));
      end;
  end;
end;

procedure TJvSIMConnector.MoveConnector(logTL: TPoint);
var
  nw, nh: integer;
  d, dd, d2: integer;
  nc: Tpoint;
begin
  d := conSize;
  d2 := d div 2;
  nc := point(LogTL.x + ConOffset.x, logTL.y + ConOffset.y);
  case conMode of
    jcmTL:
      begin
        nw := conAnchor.x - nc.x;
        if nw < d then
        begin
          left := conAnchor.x - d;
          width := -nw + d + d;
        end
        else
        begin
          left := nc.x;
          width := ConAnchor.x - left;
        end;
        nh := ConAnchor.y - nc.y;

        // adjust new hot position
        if (nw < d) and (not (nh < d)) then
        begin
          case conHot of
            jcpTL: conPos := jcpTR;
            jcpBR: conPos := jcpBL;
          end;
          shape := jcsTRBL;
        end
        else if (nw < d) and (nh < d) then
        begin
          case conHot of
            jcpTL: conPos := jcpBR;
            jcpBR: conPos := jcpTL;
          end;
          shape := jcsTLBR;
        end
        else if (not nw < d) and (nh < d) then
        begin
          case conHot of
            jcpTL: conPos := jcpBL;
            jcpBR: conPos := jcpTR;
          end;
          shape := jcsTRBL;
        end
        else
        begin
          case conHot of
            jcpTL: conPos := jcpTL;
            jcpBR: conPos := jcpBR;
          end;
          shape := jcsTLBR;
        end;
        // end of adjust TL new hot
        if nh < d then
        begin
          top := ConAnchor.y - d;
          height := -nh + d + d;
        end
        else
        begin
          top := nc.y;
          height := ConAnchor.y - Top;
        end;
      end;
    jcmTR:
      begin
        nw := nc.x - ConAnchor.x;
        if nw <= 0 then
        begin
          left := conAnchor.x + nw - d;
          width := -nw + d + d;
        end
        else if nw <= d then
        begin
          left := nc.x - d;
          width := -nw + d + d;
        end
        else
        begin
          width := nw;
        end;
        nh := ConAnchor.y - nc.y;
        // adjust TR new hot position
        if (nw < d) and (not (nh < d)) then
        begin
          case conHot of
            jcpTR: conPos := jcpTL;
            jcpBL: conPos := jcpBR;
          end;
          shape := jcsTLBR;
        end
        else if (nw < d) and (nh < d) then
        begin
          case conHot of
            jcpTR: conPos := jcpBL;
            jcpBL: conPos := jcpTR;
          end;
          shape := jcsTRBL;
        end
        else if (not nw < d) and (nh < d) then
        begin
          case conHot of
            jcpTR: conPos := jcpBR;
            jcpBL: conPos := jcpTL;
          end;
          shape := jcsTLBR;
        end
        else
        begin
          case conHot of
            jcpTR: conPos := jcpTR;
            jcpBL: conPos := jcpBL;
          end;
          shape := jcsTRBL;
        end;
        // end of adjust TR new hot
        if nh < d then
        begin
          top := ConAnchor.y - d;
          height := -nh + d + d;
        end
        else
        begin
          top := ConAnchor.y - nh;
          height := nh;
        end;
      end;
    jcmBR:
      begin
        nw := nc.x - ConAnchor.x;
        if nw <= 0 then
        begin
          left := nc.x - d;
          width := -nw + d + d;
        end
        else if nw <= d then
        begin
          left := nc.x - d;
          width := -nw + d + d;
        end
        else
        begin
          width := nw;
        end;
        nh := nc.y - ConAnchor.y;
        // adjust BR new hot position
        if (nw < d) and (not (nh < d)) then
        begin
          case conHot of
            jcpBR: conPos := jcpBL;
            jcpTL: conPos := jcpTR;
          end;
          shape := jcsTRBL;
        end
        else if (nw < d) and (nh < d) then
        begin
          case conHot of
            jcpBR: conPos := jcpTL;
            jcpTL: conPos := jcpBR;
          end;
          shape := jcsTLBR;
        end
        else if (not nw < d) and (nh < d) then
        begin
          case conHot of
            jcpBR: conPos := jcpTR;
            jcpTL: conPos := jcpBL;
          end;
          shape := jcsTRBL;
        end
        else
        begin
          case conHot of
            jcpBR: conPos := jcpBR;
            jcpTL: conPos := jcpTL;
          end;
          shape := jcsTLBR;
        end;
        // end of adjust BR new hot
        if nh < d then
        begin
          top := ConAnchor.y + nh - d;
          height := -nh + d + d;
        end
        else
        begin
          height := nh;
        end;
      end;
    jcmBL:
      begin
        nw := conAnchor.x - nc.x;
        if nw < d then
        begin
          left := conAnchor.x - d;
          width := -nw + d + d;
        end
        else
        begin
          left := ConAnchor.x - nw;
          width := nw;
        end;
        nh := nc.y - ConAnchor.y;
        // adjust BL new hot position
        if (nw < d) and (not (nh < d)) then
        begin
          case conHot of
            jcpBL: conPos := jcpBR;
            jcpTR: conPos := jcpTL;
          end;
          shape := jcsTLBR;
        end
        else if (nw < d) and (nh < d) then
        begin
          case conHot of
            jcpBL: conPos := jcpTR;
            jcpTR: conPos := jcpBL;
          end;
          shape := jcsTRBL;
        end
        else if (not nw < d) and (nh < d) then
        begin
          case conHot of
            jcpBL: conPos := jcpTL;
            jcpTR: conPos := jcpBR;
          end;
          shape := jcsTLBR;
        end
        else
        begin
          case conHot of
            jcpBL: conPos := jcpBL;
            jcpTR: conPos := jcpTR;
          end;
          shape := jcsTRBL;
        end;
        // end of adjust BL new hot
        if nh < d then
        begin
          top := ConAnchor.y + nh - d;
          height := -nh + d + d;
        end
        else
        begin
          height := nh;
        end;
      end;
  end;
end;

procedure TJvSIMConnector.Connect;
var
  Pi, Po: TPoint;
  R: Trect;
  i, g, d, d2, xw, yh: integer;
  wc: TWinControl;
  Vi: boolean;
  sBut: TJvSimButton;
  sLog: TJvLogic;
  sLight: TJvSimLight;
  sRev: TJvSimReverse;
  pl: TPoint;

  // convert a corner point to a parent point

  function pp(x, y: integer): TPoint;
  var
    p: Tpoint;
  begin
    p := point(x, y);
    p := clienttoscreen(p);
    result := wc.ScreenToClient(p);
  end;

  function getvi: boolean;
  var
    p: Tpoint;
    ii: integer;
  begin
    result := true;
    for ii := 0 to wc.ControlCount - 1 do
    begin
      if (wc.controls[ii] is TJvSimButton) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Pi) then
        begin
          sBut := TJvSimButton(wc.controls[ii]);
          Vi := sBut.Down;
          exit;
        end;
      end
      else if (wc.controls[ii] is TJvSimReverse) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, d);
        if ptinrect(R, Pi) then
        begin
          sRev := TJvSimReverse(wc.controls[ii]);
          // now check if p is the output area
          pl := SRev.Gates[1].pos;
          R := rect(sRev.left + pl.x, sRev.top - d, sRev.left + pl.x + 12, sRev.Top + pl.y + 12);
          if ptinrect(R, Pi) and SRev.Gates[1].Active then
          begin // output
            vi := SRev.OutPut1;
            exit;
          end;
          pl := SRev.Gates[2].pos;
          R := rect(sRev.left - d, sRev.top + pl.y, sRev.left + pl.x + 12, sRev.Top + pl.y + 12);
          if ptinrect(R, Pi) and SRev.Gates[2].Active then
          begin // output
            vi := SRev.OutPut2;
            exit;
          end;
          pl := SRev.Gates[3].pos;
          R := rect(sRev.left + pl.x, sRev.top + pl.y, sRev.left + pl.x + 12, sRev.top + sRev.height + d);
          if ptinrect(R, Pi) and SRev.Gates[3].Active then
          begin // output
            vi := SRev.OutPut3;
            exit;
          end;
        end;
      end
      else if (wc.controls[ii] is TJvLogic) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Pi) then
        begin
          sLog := TJvLogic(wc.controls[ii]);
          // now check if p is in one of the 3 output area's
          R := rect(sLog.left + 33, sLog.top, sLog.left + slog.Width + ConSize, sLog.Top + 22);
          if ptinrect(R, Pi) and SLog.Gates[3].Active then
          begin // output is gate 3
            vi := SLog.OutPut1;
            exit;
          end;
          R := rect(sLog.left + 33, sLog.top + 23, sLog.left + slog.Width + ConSize, sLog.Top + 44);
          if ptinrect(R, Pi) and SLog.Gates[4].Active then
          begin // output is gate 4
            vi := SLog.OutPut2;
            exit;
          end;
          R := rect(sLog.left + 33, sLog.top + 45, sLog.left + slog.Width + ConSize, sLog.Top + 64);
          if ptinrect(R, Pi) and SLog.Gates[5].Active then
          begin // output is gate 5
            vi := SLog.OutPut3;
            exit;
          end;
        end;
      end;
    end;
    result := false;
  end;

  procedure setVo;
  var
    p: Tpoint;
    ii: integer;
  begin
    for ii := 0 to wc.ControlCount - 1 do
    begin
      if (wc.controls[ii] is TJvSimLight) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Po) then
        begin
          sLight := TJvSimLight(wc.controls[ii]);
          SLight.Lit := Vi;
          exit;
        end;
      end
      else if (wc.controls[ii] is TJvSimReverse) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Po) then
        begin
          sRev := TJvSimReverse(wc.controls[ii]);
          // now check if p is in the input area
          pl := SRev.Gates[0].pos;
          R := rect(sRev.left + pl.x, sRev.top + pl.y, sRev.left + sRev.width + d, sRev.top + pl.y + 12);
          if ptinrect(R, Po) and SRev.Gates[0].Active then
          begin // input
            SRev.Input1 := vi;
            exit;
          end;
        end;
      end
      else if (wc.controls[ii] is TJvLogic) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Po) then
        begin
          sLog := TJvLogic(wc.controls[ii]);
          // now check if p is in one of the 3 input area's
          R := rect(sLog.left - d, sLog.top, sLog.left + 32, sLog.Top + 22);
          if ptinrect(R, Po) and SLog.Gates[0].Active then
          begin // input is gate 0
            SLog.Input1 := vi;
            exit;
          end;
          R := rect(sLog.left - d, sLog.top + 23, sLog.left + 32, sLog.Top + 44);
          if ptinrect(R, Po) and SLog.Gates[1].Active then
          begin // input is gate 1
            SLog.input2 := vi;
            exit;
          end;
          R := rect(sLog.left - d, sLog.top + 45, sLog.left + 32, sLog.Top + 64);
          if ptinrect(R, Po) and SLog.Gates[2].Active then
          begin // input is gate 2
            SLog.Input3 := vi;
            exit;
          end;
        end;
      end;
    end;
  end;

begin
  // connect input and output using the conPos
  d2 := conSize div 2;
  d := conSize;
  xw := width - 1;
  yh := height - 1;
  wc := parent;
  case conPos of
    jcpTL:
      begin
        Pi := pp(d2, d2);
        Po := pp(xw - d2, yh - d2);
      end;
    jcpTR:
      begin
        Pi := pp(xw - d2, d2);
        Po := pp(d2, yh - d2);
      end;
    jcpBR:
      begin
        Pi := pp(xw - d2, yh - d2);
        Po := pp(d2, d2);
      end;
    jcpBL:
      begin
        Pi := pp(d2, yh - d2);
        Po := pp(xw - d2, d2);
      end;
  end;
  // get input Vi
  if getvi then setvo;
end;

procedure TJvSIMConnector.DisConnect;
var
  Pi, Po: TPoint;
  R: Trect;
  i, g, d, d2, xw, yh: integer;
  wc: TWinControl;
  Vi: boolean;
  sBut: TJvSimButton;
  sLog: TJvLogic;
  sLight: TJvSimLight;

  // convert a corner point to a parent point

  function pp(x, y: integer): TPoint;
  var
    p: Tpoint;
  begin
    p := point(x, y);
    p := clienttoscreen(p);
    result := wc.ScreenToClient(p);
  end;

  procedure setVo;
  var
    p: Tpoint;
    ii: integer;
  begin
    for ii := 0 to wc.ControlCount - 1 do
    begin
      if (wc.controls[ii] is TJvSimLight) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Po) then
        begin
          sLight := TJvSimLight(wc.controls[ii]);
          DisCon := sLight;
          //SLight.Lit:=false;
          exit;
        end;
      end
      else if (wc.controls[ii] is TJvLogic) then
      begin
        R := wc.Controls[ii].BoundsRect;
        inflaterect(R, d, 0);
        if ptinrect(R, Po) then
        begin
          sLog := TJvLogic(wc.controls[ii]);
          // now check if p is in one of the 3 input area's
          R := rect(sLog.left - d, sLog.top, sLog.left + 32, sLog.Top + 22);
          if ptinrect(R, Po) and SLog.Gates[0].Active then
          begin // input is gate 0
            DisCon := sLog;
            DisConI := 1;
            //            SLog.Input1:=false;
            exit;
          end;
          R := rect(sLog.left - d, sLog.top + 23, sLog.left + 32, sLog.Top + 44);
          if ptinrect(R, Po) and SLog.Gates[1].Active then
          begin // input is gate 1
            DisCon := sLog;
            DisConI := 2;
            //            SLog.input2:=false;
            exit;
          end;
          R := rect(sLog.left - d, sLog.top + 45, sLog.left + 32, sLog.Top + 64);
          if ptinrect(R, Po) and SLog.Gates[2].Active then
          begin // input is gate 2
            DisCon := sLog;
            DisConI := 3;
            //            SLog.Input3:=false;
            exit;
          end;
        end;
      end;
    end;
  end;

begin
  // connect input and output using the conPos
  DisCon := nil;
  disConI := 0;
  d2 := conSize div 2;
  d := conSize;
  xw := width - 1;
  yh := height - 1;
  wc := parent;
  case conPos of
    jcpTL:
      begin
        Pi := pp(d2, d2);
        Po := pp(xw - d2, yh - d2);
      end;
    jcpTR:
      begin
        Pi := pp(xw - d2, d2);
        Po := pp(d2, yh - d2);
      end;
    jcpBR:
      begin
        Pi := pp(xw - d2, yh - d2);
        Po := pp(d2, d2);
      end;
    jcpBL:
      begin
        Pi := pp(d2, yh - d2);
        Po := pp(xw - d2, d2);
      end;
  end;
  // clear logic inputs and lights
  setvo;
end;

{ TJvLogic }

constructor TJvLogic.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  width := 65;
  height := 65;
  // initialize Gates
  FGates[0].pos := point(1, 10);
  FGates[1].pos := point(1, 28);
  FGates[2].pos := point(1, 46);
  FGates[3].pos := point(52, 10);
  FGates[4].pos := point(52, 28);
  FGates[5].pos := point(52, 46);
  for i := 0 to 5 do
    FGates[i].State := false;
  for i := 0 to 2 do
  begin
    FGates[i].style := jgsDI;
    FGates[i + 3].style := jgsDO;
  end;
  FLogicFunc := jlfAND;
  FGates[0].Active := true;
  FGates[1].Active := false;
  FGates[2].Active := true;
  FGates[3].Active := false;
  FGates[4].Active := true;
  FGates[5].Active := false;
  connectors := TList.create;
end;

function TJvLogic.GetGate(Index: Integer): TjanGate;
begin
  result := FGates[index];
end;

procedure TJvLogic.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
begin
  doMove := false;
  doStyle := false;
  StyleDown := false;
  mdp := point(x, y);
  R := clientRect;
  inflateRect(R, -15, -15);
  doStyle := ptinrect(R, mdp);
  doMove := not doStyle;
  oldp := point(x, y);
  if doMove then
    AnchorConnectors;
  if doStyle then
  begin
    StyleDown := true;
    invalidate;
  end;
end;

procedure TJvLogic.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p := clienttoscreen(point(x, y));
  p := parent.ScreenToClient(p);
  if (ssleft in shift) then
  begin
    if doMove then
    begin
      newleft := p.x - mdp.x;
      newtop := p.y - mdp.y;
      MoveConnectors;
      left := newleft;
      top := newtop;
    end
  end;
end;

procedure TJvLogic.AnchorConnectors;
var
  wc: TWincontrol;
  i: integer;
  con: TJvSIMConnector;
  R, Rc: TRect;
  p: TPoint;
begin
  wc := parent;
  connectors.Clear;
  R := boundsrect;
  inflateRect(R, 8, 0);
  p := point(left, top);
  for i := 0 to wc.ControlCount - 1 do
    if (wc.controls[i] is TJvSIMConnector) then
    begin
      con := TJvSIMConnector(wc.controls[i]);
      // check for corners in bounds
      Rc := con.BoundsRect;
      // TL
      if ptinrect(R, point(Rc.left, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTL);
      end
        // TR
      else if ptinrect(R, point(Rc.right, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTR);
      end
        // BR
      else if ptinrect(R, point(Rc.right, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBR);
      end
        // BL
      else if ptinrect(R, point(Rc.left, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBL);
      end
    end;
end;

procedure TJvLogic.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  StyleDown := false;
  if doStyle then
  begin
    doStyle := false;
    case FLogicFunc of
      jlfAND: LogicFunc := jlfOR;
      jlfOR: LogicFunc := jlfNOT;
      jlfNOT: LogicFunc := jlfAND;
    end;
  end;
  BinCheck(self);
end;

procedure TJvLogic.PaintLed(index: integer);
var
  surfcol, litcol: Tcolor;
  p: Tpoint;
  x, y: integer;
  lit: boolean;
begin
  if not Gates[index].Active then exit;
  p := Gates[index].pos;
  x := p.x;
  y := p.y;
  if index = 0 then
    lit := FInput1
  else if index = 1 then
    lit := Finput2
  else if index = 2 then
    lit := Finput3
  else if index = 3 then
    lit := Foutput1
  else if index = 4 then
    lit := Foutput2
  else if index = 5 then
    lit := Foutput3;
  if lit then
  begin
    if Gates[index].Style = jgsDI then
      surfcol := cllime
    else
      surfcol := clred;
    litcol := clwhite
  end
  else
  begin
    if Gates[index].Style = jgsDI then
    begin
      surfcol := clgreen;
      litcol := cllime;
    end
    else
    begin
      surfcol := clmaroon;
      litcol := clred;
    end;
  end;
  with Canvas do
  begin
    brush.color := clsilver;
    fillrect(rect(x, y, x + 12, y + 13));
    brush.style := bsclear;
    pen.color := clgray;
    ellipse(x, y, x + 12, y + 13);
    pen.color := clblack;
    brush.color := surfcol;
    ellipse(x + 1, y + 1, x + 11, y + 12);
    pen.color := clwhite;
    arc(x + 1, y + 1, x + 11, y + 12, x + 0, y + 12, x + 12, y + 0);
    pen.color := litcol;
    arc(x + 3, y + 3, x + 8, y + 9, x + 5, y + 0, x + 0, y + 8);
  end;
end;

procedure TJvLogic.Paint;
var
  i: integer;
  p: Tpoint;
  lit: boolean;
  R: TRect;
  s: string;
begin
  with canvas do
  begin
    brush.color := clsilver;
    R := ClientRect;
    Fillrect(R);
    Frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
    //     Frame3D(canvas,R,clbtnshadow,clbtnhighlight,1);
    brush.color := clred;
    for i := 0 to 5 do
      PaintLed(i);
    R := ClientRect;
    inflaterect(R, -15, -15);
    if StyleDown then
      Frame3D(canvas, R, clbtnshadow, clbtnhighlight, 1)
    else
      Frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
    // draw caption
    case FLogicFunc of
      jlfAND: s := 'AND';
      jlfOR: s := 'OR';
      jlfNOT: s := 'NOT';
    end;
    brush.style := bsclear;
    drawtext(canvas.handle, pchar(s), -1, R, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
end;

procedure TJvLogic.Resize;
begin
  width := 65;
  height := 65;
end;

destructor TJvLogic.Destroy;
begin
  Connectors.Free;
  inherited;

end;

procedure TJvLogic.MoveConnectors;
var
  i: integer;
  con: TJvSIMConnector;
begin
  if connectors.Count = 0 then exit;
  for i := 0 to connectors.count - 1 do
  begin
    con := TJvSIMConnector(connectors[i]);
    con.MoveConnector(point(newleft, newtop));
  end;
end;

procedure TJvLogic.OutCalc;
begin
  case FLogicFunc of
    jlfAND: OutPut2 := Input1 and Input3;
    jlfOR: OutPut2 := Input1 or Input3;
    jlfNOT: OutPut2 := not Input2;
  end;

end;

procedure TJvLogic.SetInput1(const Value: boolean);
begin
  if value <> FInput1 then
  begin
    FInput1 := Value;
    invalidate;
    OutCalc;
  end;
end;

procedure TJvLogic.SetInput2(const Value: boolean);
begin
  if value <> FInput2 then
  begin
    FInput2 := Value;
    invalidate;
    OutCalc;
  end;

end;

procedure TJvLogic.SetInput3(const Value: boolean);
begin
  if value <> FInput3 then
  begin
    FInput3 := Value;
    invalidate;
    OutCalc;
  end;
end;

procedure TJvLogic.SetOutPut1(const Value: boolean);
begin
  if value <> FOutput1 then
  begin
    FOutPut1 := Value;
    invalidate;
  end;
end;

procedure TJvLogic.SetOutPut2(const Value: boolean);
begin
  if value <> FOutput2 then
  begin
    FOutPut2 := Value;
    invalidate;
  end;

end;

procedure TJvLogic.SetOutPut3(const Value: boolean);
begin
  if value <> FOutput3 then
  begin
    FOutPut3 := Value;
    invalidate;
  end;

end;

procedure TJvLogic.SetLogicFunc(const Value: TJvLogicFunc);
begin
  if value <> FLogicFunc then
  begin
    FLogicFunc := Value;
    case FLogicFunc of
      jlfAND:
        begin
          FGates[0].Active := true;
          FGates[1].Active := false;
          FGates[2].Active := true;
          FGates[3].Active := false;
          FGates[4].Active := true;
          FGates[5].Active := false;
        end;
      jlfOR:
        begin
          FGates[0].Active := true;
          FGates[1].Active := false;
          FGates[2].Active := true;
          FGates[3].Active := false;
          FGates[4].Active := true;
          FGates[5].Active := false;
        end;
      jlfNOT:
        begin
          FGates[0].Active := false;
          FGates[1].Active := true;
          FGates[2].Active := false;
          FGates[3].Active := false;
          FGates[4].Active := true;
          FGates[5].Active := false;
        end;
    end;
    invalidate;
    OutCalc;
  end;
end;

{ TJvSimButton }

procedure TJvSimButton.AnchorConnectors;
var
  wc: TWincontrol;
  i: integer;
  con: TJvSIMConnector;
  R, Rc: TRect;
  p: TPoint;
begin
  wc := parent;
  connectors.Clear;
  R := boundsrect;
  inflateRect(R, 8, 8);
  p := point(left, top);
  for i := 0 to wc.ControlCount - 1 do
    if (wc.controls[i] is TJvSIMConnector) then
    begin
      con := TJvSIMConnector(wc.controls[i]);
      // check for corners in bounds
      Rc := con.BoundsRect;
      // TL
      if ptinrect(R, point(Rc.left, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTL);
      end
        // TR
      else if ptinrect(R, point(Rc.right, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTR);
      end
        // BR
      else if ptinrect(R, point(Rc.right, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBR);
      end
        // BL
      else if ptinrect(R, point(Rc.left, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBL);
      end
    end;
end;

constructor TJvSimButton.Create(AOwner: TComponent);
begin
  inherited;
  FDown := false;
  width := 65;
  height := 65;
  connectors := TList.create;
end;

destructor TJvSimButton.Destroy;
begin
  connectors.free;
  inherited;
end;

procedure TJvSimButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  mdp := point(x, y);
  R := clientRect;
  inflateRect(R, -15, -15);
  doMove := not (ptinrect(R, mdp));
  FDepressed := not doMove;
  oldp := point(x, y);
  if doMove then
    AnchorConnectors
  else
    invalidate;
end;

procedure TJvSimButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if FDepressed then exit;
  p := clienttoscreen(point(x, y));
  p := parent.ScreenToClient(p);
  if (ssleft in shift) then
  begin
    if doMove then
    begin
      newleft := p.x - mdp.x;
      newtop := p.y - mdp.y;
      MoveConnectors;
      left := newleft;
      top := newtop;
    end
  end;
end;

procedure TJvSimButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  p: Tpoint;
begin
  FDepressed := false;
  p := point(x, y);
  R := clientRect;
  inflateRect(R, -15, -15);
  if ptinrect(R, p) then
  begin
    Down := not FDown;
  end
  else
    BinCheck(self);
end;

procedure TJvSimButton.MoveConnectors;
var
  i: integer;
  con: TJvSIMConnector;
begin
  if connectors.Count = 0 then exit;
  for i := 0 to connectors.count - 1 do
  begin
    con := TJvSIMConnector(connectors[i]);
    con.MoveConnector(point(newleft, newtop));
  end;
end;

procedure TJvSimButton.Paint;
var
  p: Tpoint;
  R: TRect;
begin
  with canvas do
  begin
    brush.color := clsilver;
    R := ClientRect;
    Fillrect(R);
    Frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
    inflaterect(R, -15, -15);
    if FDepressed or FDown then
      Frame3D(canvas, R, clbtnshadow, clbtnhighlight, 1)
    else
      Frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
    p := point((width div 2) - 6, (height div 2) - 6);
    PaintLed(p, FDown);
  end;
end;

procedure TJvSimButton.PaintLed(pt: TPoint; lit: boolean);
var
  surfcol, litcol: Tcolor;
  x, y: integer;
begin
  x := pt.x;
  y := pt.y;
  if lit then
  begin
    surfcol := clred;
    litcol := clwhite
  end
  else
  begin
    surfcol := clmaroon;
    litcol := clred;
  end;
  with Canvas do
  begin
    brush.color := clsilver;
    fillrect(rect(x, y, x + 12, y + 13));
    brush.style := bsclear;
    pen.color := clgray;
    ellipse(x, y, x + 12, y + 13);
    pen.color := clblack;
    brush.color := surfcol;
    ellipse(x + 1, y + 1, x + 11, y + 12);
    pen.color := clwhite;
    arc(x + 1, y + 1, x + 11, y + 12, x + 0, y + 12, x + 12, y + 0);
    pen.color := litcol;
    arc(x + 3, y + 3, x + 8, y + 9, x + 5, y + 0, x + 0, y + 8);
  end;
end;

procedure TJvSimButton.Resize;
begin
  width := 65;
  height := 65;
end;

procedure TJvSimButton.SetDown(const Value: boolean);
begin
  if value <> FDown then
  begin
    FDown := Value;
    FDepressed := value;
    invalidate;
  end;
end;

procedure TJvSimLight.AnchorConnectors;
var
  wc: TWincontrol;
  i: integer;
  con: TJvSIMConnector;
  R, Rc: TRect;
  p: TPoint;
begin
  wc := parent;
  connectors.Clear;
  R := boundsrect;
  inflateRect(R, 8, 8);
  p := point(left, top);
  for i := 0 to wc.ControlCount - 1 do
    if (wc.controls[i] is TJvSIMConnector) then
    begin
      con := TJvSIMConnector(wc.controls[i]);
      // check for corners in bounds
      Rc := con.BoundsRect;
      // TL
      if ptinrect(R, point(Rc.left, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTL);
      end
        // TR
      else if ptinrect(R, point(Rc.right, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTR);
      end
        // BR
      else if ptinrect(R, point(Rc.right, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBR);
      end
        // BL
      else if ptinrect(R, point(Rc.left, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBL);
      end
    end;
end;

constructor TJvSimLight.Create(AOwner: TComponent);
begin
  inherited;
  FLit := false;
  width := 65;
  height := 65;
  FOnColor := clLime;
  FOffColor := Clgreen;
  connectors := TList.create;
end;

destructor TJvSimLight.Destroy;
begin
  inherited;
  connectors.free;
end;

procedure TJvSimLight.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  mdp := point(x, y);
  doMove := true;
  oldp := point(x, y);
  AnchorConnectors
end;

procedure TJvSimLight.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p := clienttoscreen(point(x, y));
  p := parent.ScreenToClient(p);
  if (ssleft in shift) then
  begin
    if doMove then
    begin
      newleft := p.x - mdp.x;
      newtop := p.y - mdp.y;
      MoveConnectors;
      left := newleft;
      top := newtop;
    end
  end;
end;

procedure TJvSimLight.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  BinCheck(self);
end;

procedure TJvSimLight.MoveConnectors;
var
  i: integer;
  con: TJvSIMConnector;
begin
  if connectors.Count = 0 then exit;
  for i := 0 to connectors.count - 1 do
  begin
    con := TJvSIMConnector(connectors[i]);
    con.MoveConnector(point(newleft, newtop));
  end;
end;

procedure TJvSimLight.Paint;
var
  rgn: Hrgn;
  tlpoly, brpoly: array[0..2] of TPoint;
  clippoly, poly: array[0..7] of TPoint;
  i: integer;
  cr, xw, yh: integer;
  R, RCap: Trect;
  hiColor, loColor, surfcol: Tcolor;
  dx, dy: integer;
  x4: integer;
  bm: TBitmap;

  procedure drawframe;
  begin
    //   rgn :=  CreatePolygonRgn(tlpoly,3,WINDING);
    //   SelectClipRgn(Canvas.handle,rgn);
    with canvas do
    begin
      brush.color := surfCol;
      pen.color := hiColor;
      pen.Width := 2;
      Ellipse(15, 15, xw - 15, yh - 15);
    end;
    //   SelectClipRgn(Canvas.handle,0);
    //   DeleteObject(rgn);
    //   rgn :=  CreatePolygonRgn(brpoly,3,WINDING);
    //   SelectClipRgn(Canvas.handle,rgn);
    with canvas do
    begin
      brush.color := surfCol;
      pen.color := loColor;
      pen.Width := 2;
      arc(15, 15, xw - 15, yh - 15, 0, yh, xw, 0);
      pen.width := 1;
    end;
    //   SelectClipRgn(Canvas.handle,0);
    //   DeleteObject(rgn);
  end;

begin
  if Lit then
    surfcol := Oncolor
  else
    surfcol := OffColor;
  canvas.brush.style := bssolid;
  R := clientRect;
  canvas.brush.color := clsilver;
  canvas.FillRect(R);
  frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
  xw := width - 1;
  yh := height - 1;
  cr := width div 4;
  x4 := width div 4;
  // topleft region
  tlpoly[0] := point(left, top + yh);
  tlpoly[1] := point(left, top);
  tlpoly[2] := point(left + xw, top);
  // bottom right region
  brpoly[0] := point(left + xw, top);
  brpoly[1] := point(left + xw, top + yh);
  brpoly[2] := point(left, top + yh);
  canvas.pen.style := pssolid;
  hiColor := clbtnhighlight;
  locolor := clbtnshadow;
  drawframe;
end;

procedure TJvSimLight.Resize;
begin
  width := 65;
  height := 65;
end;

procedure TJvSimLight.SetLit(const Value: boolean);
begin
  if value <> FLit then
  begin
    FLit := Value;
    invalidate;
  end;
end;

procedure TJvSimLight.SetOffColor(const Value: TColor);
begin
  if value <> FOffColor then
  begin
    FOffColor := Value;
    invalidate;
  end;
end;

procedure TJvSimLight.SetOnColor(const Value: TColor);
begin
  if value <> FonColor then
  begin
    FOnColor := Value;
    invalidate;
  end;
end;

{ TjanSimBin }

constructor TjanSimBin.create(AOwner: Tcomponent);
begin
  inherited;
  width := 65;
  height := 65;
  bmBin := tbitmap.create;
  bmBin.LoadFromResourceName(HInstance, 'RBIN');
end;

destructor TjanSimBin.destroy;
begin
  bmBin.free;
  inherited;

end;

procedure TjanSimBin.paint;
var
  Rf: trect;
begin
  Rf := clientrect;
  canvas.Brush.color := clsilver;
  canvas.fillrect(rect(0, 0, width, height));
  frame3D(canvas, Rf, clbtnhighlight, clbtnshadow, 1);
  canvas.draw(16, 16, bmBin);
end;

procedure TjanSimBin.resize;
begin
  inherited;
  width := 65;
  height := 65;
end;

{ TJvSimLogicBox }

procedure TJvSimLogicBox.cpuOnTimer(sender: TObject);
var
  wc: TWinControl;
  i, j: integer;
  sLight: TJvSimLight;
  sLogic: TJvLogic;
begin
  wc := parent;
  // reset inputs
{  for i:=0 to wc.ControlCount-1 do
    if (wc.controls[i] is TJvLogic) then
    begin
      sLogic:=TJvLogic(wc.controls[i]);
      for j:=0 to 2 do
        sLogic.FGates[j].State:=false;
    end
    else if (wc.controls[i] is TJvSimLight) then
    begin
      sLight:=TJvSimLight(wc.controls[i]);
      sLight.Lit:=false;
    end;}
  // make connections
  for i := 0 to wc.ControlCount - 1 do
    if (wc.controls[i] is TJvSIMConnector) then
      TJvSIMConnector(wc.controls[i]).connect;
end;

constructor TJvSimLogicBox.create(AOwner: Tcomponent);
begin
  inherited;
  width := 130;
  height := 65;
  bmCon := TBitmap.create;
  bmLogic := TBitmap.create;
  bmButton := TBitmap.create;
  bmLight := TBitmap.create;
  bmRev := TBitmap.create;
  bmCon.LoadFromResourceName(HInstance, 'RCON');
  bmLogic.LoadFromResourceName(HInstance, 'RLOGIC');
  bmButton.LoadFromResourceName(HInstance, 'RBUTTON');
  bmLight.LoadFromResourceName(HInstance, 'RLIGHT');
  bmRev.LoadFromResourceName(HInstance, 'RREV');
  bmBin := tbitmap.create;
  bmBin.LoadFromResourceName(HInstance, 'RBIN');
  RCon := rect(0, 0, 32, 32);
  RLogic := rect(33, 0, 64, 32);
  RButton := rect(0, 33, 32, 64);
  RLight := rect(33, 33, 64, 64);
  RRev := rect(65, 0, 97, 32);
  DCon := false;
  DLogic := false;
  DButton := false;
  DLight := false;
  DRev := false;
  cpu := TTimer.Create(self);
  cpu.Enabled := false;
  cpu.OnTimer := cpuOnTimer;
  cpu.Interval := 50;
end;

destructor TJvSimLogicBox.Destroy;
begin
  cpu.free;
  bmCon.free;
  bmLogic.free;
  bmButton.free;
  bmLight.free;
  bmRev.free;
  bmBin.free;
  inherited;

end;

procedure TJvSimLogicBox.Loaded;
begin
  inherited;
  cpu.Enabled := true;
end;

procedure TJvSimLogicBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p := point(x, y);
  DCon := false;
  DLogic := false;
  DButton := false;
  DLight := false;
  if ptinrect(RCon, p) then
    Dcon := true
  else if ptinrect(RLogic, p) then
    DLogic := true
  else if ptinrect(RButton, p) then
    DButton := true
  else if ptinrect(RLight, p) then
    DLight := true
  else if ptinrect(RRev, p) then
    DRev := true;
  invalidate;
end;

procedure TJvSimLogicBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  wc: TWinControl;
  l, t: integer;
begin
  wc := parent;
  l := left;
  t := top + height + 10;
  if Dcon then
    with TJvSIMConnector.create(wc) do
    begin
      parent := wc;
      left := l;
      top := t;
    end
  else if DLogic then
    with TJvLogic.create(wc) do
    begin
      parent := wc;
      left := l;
      top := t;
    end
  else if DButton then
    with TJvSimButton.create(wc) do
    begin
      parent := wc;
      left := l;
      top := t;
    end
  else if DLight then
    with TJvSimLight.create(wc) do
    begin
      parent := wc;
      left := l;
      top := t;
    end
  else if DRev then
    with TJvSimReverse.Create(wc) do
    begin
      parent := wc;
      left := l;
      top := t;
    end;
  DCon := false;
  DLogic := false;
  DButton := false;
  DLight := false;
  DRev := false;
  invalidate;
end;

procedure TJvSimLogicBox.Paint;
var
  R, Rb: TRect;
begin
  with canvas do
  begin
    brush.color := clsilver;
    fillrect(clientrect);
    Rb := RCon;
    if not DCon then
      frame3D(canvas, Rb, clbtnhighlight, clbtnshadow, 1)
    else
      frame3D(canvas, Rb, clbtnshadow, clbtnhighlight, 1);
    draw(4, 4, bmCon);
    Rb := RLogic;
    if not DLogic then
      frame3D(canvas, Rb, clbtnhighlight, clbtnshadow, 1)
    else
      frame3D(canvas, Rb, clbtnshadow, clbtnhighlight, 1);
    draw(36, 4, bmLogic);
    Rb := RButton;
    if not DButton then
      frame3D(canvas, Rb, clbtnhighlight, clbtnshadow, 1)
    else
      frame3D(canvas, Rb, clbtnshadow, clbtnhighlight, 1);
    draw(4, 36, bmButton);
    Rb := RLight;
    if not DLight then
      frame3D(canvas, Rb, clbtnhighlight, clbtnshadow, 1)
    else
      frame3D(canvas, Rb, clbtnshadow, clbtnhighlight, 1);
    draw(36, 36, bmLight);
    Rb := RRev;
    if not DRev then
      frame3D(canvas, Rb, clbtnhighlight, clbtnshadow, 1)
    else
      frame3D(canvas, Rb, clbtnshadow, clbtnhighlight, 1);
    draw(Rb.left + 3, Rb.top + 3, bmRev);

    // draw bin
    draw(100, 16, bmBin);
  end;

end;

procedure TJvSimLogicBox.resize;
begin
  width := 130;
  height := 65;

end;

{ TJvSimReverse }

procedure TJvSimReverse.AnchorConnectors;
var
  wc: TWincontrol;
  i: integer;
  con: TJvSIMConnector;
  R, Rc: TRect;
  p: TPoint;
begin
  wc := parent;
  connectors.Clear;
  R := boundsrect;
  inflateRect(R, 8, 0);
  p := point(left, top);
  for i := 0 to wc.ControlCount - 1 do
    if (wc.controls[i] is TJvSIMConnector) then
    begin
      con := TJvSIMConnector(wc.controls[i]);
      // check for corners in bounds
      Rc := con.BoundsRect;
      // TL
      if ptinrect(R, point(Rc.left, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTL);
      end
        // TR
      else if ptinrect(R, point(Rc.right, Rc.top)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmTR);
      end
        // BR
      else if ptinrect(R, point(Rc.right, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBR);
      end
        // BL
      else if ptinrect(R, point(Rc.left, Rc.bottom)) then
      begin
        connectors.Add(con);
        con.AnchorCorner(p, jcmBL);
      end
    end;
end;

constructor TJvSimReverse.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  width := 42;
  height := 42;
  // initialize Gates
  FGates[0].pos := point(28, 14);
  FGates[1].pos := point(14, 1);
  FGates[2].pos := point(1, 14);
  FGates[3].pos := point(14, 28);
  for i := 0 to 3 do
  begin
    FGates[i].State := false;
    FGates[i].Active := true;
    FGates[i].style := jgsDO;
  end;
  FGates[0].style := jgsDI;
  connectors := TList.create;
end;

destructor TJvSimReverse.Destroy;
begin
  Connectors.Free;
  inherited;
end;

function TJvSimReverse.GetGate(Index: Integer): TjanGate;
begin
  result := FGates[index];
end;

procedure TJvSimReverse.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  mdp := point(x, y);
  oldp := point(x, y);
  doMove := true;
  AnchorConnectors;
end;

procedure TJvSimReverse.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p := clienttoscreen(point(x, y));
  p := parent.ScreenToClient(p);
  if (ssleft in shift) then
  begin
    if doMove then
    begin
      newleft := p.x - mdp.x;
      newtop := p.y - mdp.y;
      MoveConnectors;
      left := newleft;
      top := newtop;
    end
  end;
end;

procedure TJvSimReverse.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  BinCheck(self);
end;

procedure TJvSimReverse.MoveConnectors;
var
  i: integer;
  con: TJvSIMConnector;
begin
  if connectors.Count = 0 then exit;
  for i := 0 to connectors.count - 1 do
  begin
    con := TJvSIMConnector(connectors[i]);
    con.MoveConnector(point(newleft, newtop));
  end;
end;

procedure TJvSimReverse.OutCalc;
begin
  Output1 := Input1;
  OutPut2 := input1;
  Output3 := input1;
end;

procedure TJvSimReverse.Paint;
var
  i: integer;
  p: Tpoint;
  lit: boolean;
  R: TRect;
  poly: array[0..2] of TPoint;
begin
  with canvas do
  begin
    brush.color := clsilver;
    R := ClientRect;
    Fillrect(R);
    Frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
    brush.color := clred;
    for i := 0 to 3 do
      PaintLed(i);
    R := ClientRect;
    // paint triangle
    poly[0] := point(14, 20);
    poly[1] := point(26, 14);
    poly[2] := point(26, 26);
    pen.style := psclear;
    brush.color := clblack;
    polygon(poly);
    pen.style := pssolid;
  end;
end;

procedure TJvSimReverse.PaintLed(index: integer);
var
  surfcol, litcol: Tcolor;
  p: Tpoint;
  x, y: integer;
  lit: boolean;
begin
  if not Gates[index].Active then exit;
  p := Gates[index].pos;
  x := p.x;
  y := p.y;
  if index = 0 then
    lit := FInput1
  else if index = 1 then
    lit := Foutput1
  else if index = 2 then
    lit := FOutput2
  else if index = 3 then
    lit := FOutput3;
  if lit then
  begin
    if Gates[index].Style = jgsDI then
      surfcol := cllime
    else
      surfcol := clred;
    litcol := clwhite
  end
  else
  begin
    if Gates[index].Style = jgsDI then
    begin
      surfcol := clgreen;
      litcol := cllime;
    end
    else
    begin
      surfcol := clmaroon;
      litcol := clred;
    end;
  end;
  with Canvas do
  begin
    brush.color := clsilver;
    fillrect(rect(x, y, x + 12, y + 13));
    brush.style := bsclear;
    pen.color := clgray;
    ellipse(x, y, x + 12, y + 13);
    pen.color := clblack;
    brush.color := surfcol;
    ellipse(x + 1, y + 1, x + 11, y + 12);
    pen.color := clwhite;
    arc(x + 1, y + 1, x + 11, y + 12, x + 0, y + 12, x + 12, y + 0);
    pen.color := litcol;
    arc(x + 3, y + 3, x + 8, y + 9, x + 5, y + 0, x + 0, y + 8);
  end;
end;

procedure TJvSimReverse.Resize;
begin
  width := 42;
  height := 42;
end;

procedure TJvSimReverse.SetInput1(const Value: boolean);
begin
  if value <> FInput1 then
  begin
    FInput1 := Value;
    invalidate;
    OutCalc;
  end;
end;

procedure TJvSimReverse.SetOutPut1(const Value: boolean);
begin
  if value <> FOutput1 then
  begin
    FOutPut1 := Value;
    invalidate;
  end;
end;

procedure TJvSimReverse.SetOutPut2(const Value: boolean);
begin
  if value <> FOutput2 then
  begin
    FOutPut2 := Value;
    invalidate;
  end;
end;

procedure TJvSimReverse.SetOutPut3(const Value: boolean);
begin
  if value <> FOutput3 then
  begin
    FOutPut3 := Value;
    invalidate;
  end;
end;

end.
