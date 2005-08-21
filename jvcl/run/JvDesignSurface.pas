{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesingSurface.pas, released on 2005-08-21.

The Initial Developer of the Original Code is Scott J Miles
Portions created by Scott J Miles are Copyright (C) 2005 Scott J Miles.
All Rights Reserved.

Contributor(s): Olivier Sannier (JVCL Integration)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
unit JvDesignSurface;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
  Contnrs;

type
  TJvDesignSurface = class;
  //
  TJvDesignMessage = function(inSender: TControl; var inMsg: TMessage;
    const inPt: TPoint): Boolean of object;
  //
  TJvDesignCustomMessenger = class
  private
    FContainer: TWinControl;
    FOnDesignMessage: TJvDesignMessage;
  protected
    procedure SetContainer(inValue: TWinControl); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDesignMessage(inSender: TControl;
      var inMessage: TMessage): Boolean; virtual;
    procedure Clear; virtual;
    procedure DesignChildren(inContainer: TWinControl);
    procedure DesignComponent(inComponent: TComponent); virtual;
    property Container: TWinControl read FContainer write SetContainer;
    property OnDesignMessage: TJvDesignMessage read FOnDesignMessage
      write FOnDesignMessage;
  end;
  //
  TJvDesignCustomMessengerClass = class of TJvDesignCustomMessenger;
  //
  TJvDesignMessageHook = class
  private
    FClient: TWinControl;
    FOldProc: TWndMethod;
    FUser: TJvDesignCustomMessenger;
  protected
    procedure HookProc(var inMessage: TMessage);
    procedure Unhook;
  public
    constructor Create(inUser: TJvDesignCustomMessenger; inClient: TWinControl);
    destructor Destroy; override;
    property Client: TWinControl read FClient;
  end;
  //
  TJvDesignCustomController = class
  private
    FSurface: TJvDesignSurface;
  protected
    function GetDragRect: TRect; virtual; abstract;
    function GetShift: TShiftState;
    function KeyDown(inKeycode: Cardinal): Boolean; virtual; abstract;
    function KeyUp(inKeycode: Cardinal): Boolean; virtual; abstract;
    function MouseDown(Button: TMouseButton;
      X, Y: Integer): Boolean; virtual; abstract;
    function MouseMove(X, Y: Integer): Boolean; virtual; abstract;
    function MouseUp(Button: TMouseButton;
      X, Y: Integer): Boolean; virtual; abstract;
  public
    constructor Create(inSurface: TJvDesignSurface); virtual;
    property DragRect: TRect read GetDragRect;
    property Shift: TShiftState read GetShift;
    property Surface: TJvDesignSurface read FSurface;
  end;
  //
  TJvDesignCustomControllerClass = class of TJvDesignCustomController;
  //
  TJvDesignHandleId = ( dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
    dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom );
  //
  TJvDesignCustomSelector = class(TComponent)
  private
    FSurface: TJvDesignSurface;
  protected
    function GetCount: Integer; virtual; abstract;
    function GetSelection(inIndex: Integer): TControl;  virtual; abstract;
    procedure SetSelection(inIndex: Integer; inValue: TControl); virtual; abstract;
  public
    constructor Create(inSurface: TJvDesignSurface); reintroduce; virtual;
    destructor Destroy; override;
    function IsSelected(inValue: TControl): Boolean; virtual; abstract;
    function GetClientControl(inControl: TControl): TControl; virtual; abstract;
    function GetCursor(inX, inY: Integer): TCursor; virtual; abstract;
    function GetHitHandle(inX, inY: Integer): TJvDesignHandleId; virtual; abstract;
    procedure AddToSelection(inValue: TControl); virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure RemoveFromSelection(inValue: TControl); virtual; abstract;
    procedure ToggleSelection(inValue: TControl);
    procedure Update; virtual; abstract;
    property Count: Integer read GetCount;
    property Selection[inIndex: Integer]: TControl read GetSelection
      write SetSelection;
    property Surface: TJvDesignSurface read FSurface;
  end;
  //
  TJvDesignCustomSelectorClass = class of TJvDesignCustomSelector;
  //
  TJvDesignObjectArray = array of TObject;
  TJvDesignGetAddClassEvent = procedure(Sender: TObject;
    var ioClass: string) of object;
{
  TJvDesignOwnerDrawGridEvent = procedure(inSender: TObject; inCanvas: TCanvas;
    inRect: TRect) of object;
}
  //
  TJvDesignSurface = class(TComponent)
  private
    FActive: Boolean;
    FAddClass: string;
    FContainer: TWinControl;
    FContainerHook: TJvDesignMessageHook;
    FController: TJvDesignCustomController;
    FControllerClass: TJvDesignCustomControllerClass;
//    FDrawGrid: Boolean;
    FMessenger: TJvDesignCustomMessenger;
    FMessengerClass: TJvDesignCustomMessengerClass;
    FSelector: TJvDesignCustomSelector;
    FSelectorClass: TJvDesignCustomSelectorClass;
    FUpdateOwner: TComponent;
  protected
    FOnChange: TNotifyEvent;
    FOnGetAddClass: TJvDesignGetAddClassEvent;
//    FOnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent;
    FOnSelectionChange: TNotifyEvent;
    function GetAddBounds: TRect;
    function GetCount: Integer;
    function GetSelected: TJvDesignObjectArray;
    function GetSelectedContainer: TWinControl;
    function GetSelection(inIndex: Integer): TControl;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedContainer;
    procedure NeedController;
    procedure NeedMessenger;
    procedure NeedSelector;
    //procedure PaintContainerBkgnd(inDC: HDC);
    procedure ReaderError(Reader: TReader; const Message: string;
      var Handled: Boolean);
    procedure SetActive(inValue: Boolean);
    procedure SetContainer(inValue: TWinControl);
    //procedure SetDrawGrid(const Value: Boolean);
    procedure SetSelection(inIndex: Integer; inValue: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clear: TJvDesignSurface;
    function ContainerToSelectedContainer(const inPt: TPoint): TPoint;
    function FindControl(inX, inY: Integer): TControl; virtual;
    function GetCursor(inX, inY: Integer): TCursor; virtual;
    function GetHitHandle(inX, inY: Integer): TJvDesignHandleId; virtual;
    function IsDesignMessage(inSender: TControl; var inMsg: TMessage;
      const inPt: TPoint): Boolean;
    function LoadFromFile(const inFilename: string): TJvDesignSurface;
    function LoadFromStream(inStream: TStream): TJvDesignSurface;
    procedure AddComponent;
    procedure Change;
    procedure ClearSelection;
    procedure CopyComponents;
    procedure CutComponents;
    procedure DeleteComponents;
    procedure GetAddClass;
    procedure GrowComponents(inGrowWidth, inGrowHeight: Integer);
    procedure NudgeComponents(inNudgeLeft, inNudgeTop: Integer);
    procedure PasteComponents;
    procedure SaveToFile(const inFilename: string);
    procedure SaveToStream(inStream: TStream);
    procedure Select(inControl: TControl);
    procedure SelectionChange;
    procedure SelectParent;
    procedure SetSelected(const inValue: array of TObject);
    procedure UpdateDesigner; virtual;
    property Active: Boolean read FActive write SetActive;
    property AddClass: string read FAddClass write FAddClass;
    property Controller: TJvDesignCustomController read FController;
    property ControllerClass: TJvDesignCustomControllerClass read FControllerClass
      write FControllerClass;
    property Count: Integer read GetCount;
    property Messenger: TJvDesignCustomMessenger read FMessenger;
    property MessengerClass: TJvDesignCustomMessengerClass read FMessengerClass
      write FMessengerClass;
    property Selected: TJvDesignObjectArray read GetSelected;
    property SelectedContainer: TWinControl read GetSelectedContainer;
    property Selection[inIndex: Integer]: TControl read GetSelection
      write SetSelection;
    property Selector: TJvDesignCustomSelector read FSelector;
    property SelectorClass: TJvDesignCustomSelectorClass read FSelectorClass
      write FSelectorClass;
  published
    property Container: TWinControl read FContainer write SetContainer;
//    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent read FOnGetAddClass
      write FOnGetAddClass;
{
    property OnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent
      read FOnOwnerDrawGrid write FOnOwnerDrawGrid;
}
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange
      write FOnSelectionChange;
  end;
  //
  TJvDesignScrollBox = class(TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl); override;
  end;
  //
  TJvDesignPanel = class(TPanel)
  private
    FSurface: TJvDesignSurface;
    FOnPaint: TNotifyEvent;
    FDrawRules: Boolean;
    function GetActive: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnGetAddClass: TJvDesignGetAddClassEvent;
    function GetOnSelectionChange: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnGetAddClass(const Value: TJvDesignGetAddClassEvent);
    procedure SetOnSelectionChange(const Value: TNotifyEvent);
  public
    constructor Create(inOwner: TComponent); override;
    procedure Clear;
    procedure LoadFromFile(const inFilename: string);
    procedure LoadFromStream(inStream: TStream);
    procedure Paint; override;
    procedure SaveToFile(const inFilename: string);
    procedure SaveToStream(inStream: TStream);
    procedure SetDrawRules(const Value: Boolean);
    property Active: Boolean read GetActive write SetActive;
    property Canvas;
    property Surface: TJvDesignSurface read FSurface;
  published
    property DrawRules: Boolean read FDrawRules write SetDrawRules default true;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent read GetOnGetAddClass
      write SetOnGetAddClass;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange
      write SetOnSelectionChange;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Clipbrd, JvDesignUtils, JvDesignClip, JvDesignImp;

{ TJvDesignCustomMessenger }

constructor TJvDesignCustomMessenger.Create;
begin
  //
end;

destructor TJvDesignCustomMessenger.Destroy;
begin
  //
end;

procedure TJvDesignCustomMessenger.Clear;
begin
  //
end;

procedure TJvDesignCustomMessenger.DesignComponent(inComponent: TComponent);
begin
  //
end;

procedure TJvDesignCustomMessenger.DesignChildren(inContainer: TWinControl);
var
  i: Integer;
begin
  for i := 0 to Pred(inContainer.ControlCount) do
    DesignComponent(inContainer.Controls[i]);
end;

procedure TJvDesignCustomMessenger.SetContainer(inValue: TWinControl);
begin
  FContainer := inValue;
end;

function TJvDesignCustomMessenger.IsDesignMessage(inSender: TControl;
  var inMessage: TMessage): Boolean;

  function MousePoint: TPoint;
  begin
    with TWMMouse(inMessage) do
      MousePoint := Point(XPos, YPos);
    Result := DesignClientToParent(Result, inSender, Container);
  end;

begin
  if not Assigned(OnDesignMessage) then
    Result := false
  else
    case inMessage.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        Result := OnDesignMessage(inSender, inMessage, MousePoint);
      WM_KEYDOWN..WM_KEYUP, WM_PAINT, WM_ERASEBKGND:
        Result := OnDesignMessage(inSender, inMessage, Point(0, 0));
      else Result := false;
    end;
end;

{ TJvDesignMessageHook }

constructor TJvDesignMessageHook.Create(inUser: TJvDesignCustomMessenger;
  inClient: TWinControl);
begin
  FUser := inUser;
  FClient := inClient;
  FOldProc := FClient.WindowProc;
  FClient.WindowProc := HookProc;
end;

destructor TJvDesignMessageHook.Destroy;
begin
  Unhook;
  inherited;
end;

procedure TJvDesignMessageHook.Unhook;
begin
  FClient.WindowProc := FOldProc;
end;

procedure TJvDesignMessageHook.HookProc(var inMessage: TMessage);
begin
  if not FUser.IsDesignMessage(FClient, inMessage) then
    FOldProc(inMessage);
end;

{ TJvDesignCustomController }

constructor TJvDesignCustomController.Create(inSurface: TJvDesignSurface);
begin
  FSurface := inSurface;
end;

function TJvDesignCustomController.GetShift: TShiftState;
// obones: For C5/D5 compatibility, we must use a local variable
// as KeyboardStateToShiftState with no parameters was introduced
// in D6/C6 
var
  KeyState: TKeyBoardState;
begin
  GetKeyboardState(KeyState);
  Result := KeyboardStateToShiftState(KeyState);
end;

{ TJvDesignCustomSelector }

constructor TJvDesignCustomSelector.Create(inSurface: TJvDesignSurface);
begin
  inherited Create(nil);
  FSurface := inSurface;
end;

destructor TJvDesignCustomSelector.Destroy;
begin
  inherited;
end;

procedure TJvDesignCustomSelector.ToggleSelection(inValue: TControl);
begin
  if IsSelected(inValue) then
    RemoveFromSelection(inValue)
  else
    AddToSelection(inValue);
end;

{ TJvDesignSurface }

constructor TJvDesignSurface.Create(AOwner: TComponent);
begin
  inherited;
  FMessengerClass := TJvDesignDesignerMessenger;
  FControllerClass := TJvDesignController;
  FSelectorClass := TJvDesignSelector;
  //FDrawGrid := true;
end;

destructor TJvDesignSurface.Destroy;
begin
  FContainerHook.Free;
  Messenger.Free;
  Controller.Free;
  Selector.Free;
  inherited;
end;

procedure TJvDesignSurface.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TJvDesignSurface.SetContainer(inValue: TWinControl);
begin
  FContainer := inValue;
end;

procedure TJvDesignSurface.NeedContainer;
begin
  if (Container = nil) and (Owner is TWinControl) then
    Container := TWinControl(Owner);
  if Container = nil then
    raise Exception.Create(ClassName + ': Container is nil');
end;

procedure TJvDesignSurface.NeedController;
begin
  if (Controller = nil) and (ControllerClass <> nil) then
    FController := ControllerClass.Create(Self);
  if Controller = nil then
    raise Exception.Create(ClassName + ': Controller is nil');
end;

procedure TJvDesignSurface.NeedMessenger;
begin
  if (Messenger = nil) and (MessengerClass <> nil) then
  begin
    FMessenger := MessengerClass.Create;
    Messenger.OnDesignMessage := IsDesignMessage;
  end;
  if Messenger = nil then
    raise Exception.Create(ClassName + ': Messenger is nil');
end;

procedure TJvDesignSurface.NeedSelector;
begin
  if (Selector = nil) and (SelectorClass <> nil) then
    FSelector := SelectorClass.Create(Self);
  if Selector = nil then
    raise Exception.Create(ClassName + ': Selector is nil');
end;

procedure TJvDesignSurface.SetActive(inValue: Boolean);

  procedure Activate;
  begin
    NeedContainer;
    NeedController;
    NeedSelector;
    NeedMessenger;
    Messenger.Container := Container;
    FContainerHook := TJvDesignMessageHook.Create(Messenger, Container);
  end;

  procedure Deactivate;
  begin
    FreeAndNil(FContainerHook);
    Selector.ClearSelection;
    FreeAndNil(FMessenger);
  end;

begin
  if FActive <> inValue then
  begin
    if inValue then
      Activate
    else
      Deactivate;
    FActive := inValue;
    SelectionChange;
  end;
end;

procedure TJvDesignSurface.UpdateDesigner;
begin
  Selector.Update;
end;

function TJvDesignSurface.GetCount: Integer;
begin
  Result := Selector.Count;
end;

function TJvDesignSurface.GetSelection(inIndex: Integer): TControl;
begin
  Result := Selector.Selection[inIndex];
end;

procedure TJvDesignSurface.SetSelection(inIndex: Integer; inValue: TControl);
begin
  Selector.Selection[inIndex] := inValue;
end;

procedure TJvDesignSurface.ClearSelection;
begin
  Selector.ClearSelection;
end;

procedure TJvDesignSurface.SelectionChange;
begin
  if not (csDestroying in ComponentState) and Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TJvDesignSurface.GetSelected: TJvDesignObjectArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Pred(Count) do
    Result[i] := Selector.Selection[i];
end;

procedure TJvDesignSurface.SetSelected(const inValue: array of TObject);
var
  i: Integer;
begin
  ClearSelection;
  for i := 0 to Pred(Length(inValue)) do
    if inValue[i] is TControl then
      Selector.AddToSelection(TControl(inValue[i]));
end;

procedure TJvDesignSurface.Select(inControl: TControl);
begin
  ClearSelection;
  if inControl <> nil then
    Selector.AddToSelection(inControl);
end;

function TJvDesignSurface.FindControl(inX, inY: Integer): TControl;
var
  c, c0: TControl;
  p: TPoint;
begin
  p := Point(inX, inY);
  c := Container.ControlAtPos(p, true, true);
  while (c <> nil) and (c is TWinControl) do
  begin
    Dec(p.X, c.Left);
    Dec(p.Y, c.Top);
    c0 := TWinControl(c).ControlAtPos(p, true, true);
    if (c0 = nil) or (c0.Owner <> c.Owner) then
      break;
    c := c0;
  end;
  if c = nil then
    c := Container;
  Result := Selector.GetClientControl(c);
end;

function TJvDesignSurface.GetSelectedContainer: TWinControl;
begin
  if (Count <> 1) then
    Result := Container
  else if (Selection[0] is TWinControl) and
    (csAcceptsControls in Selection[0].ControlStyle) then
      Result := TWinControl(Selection[0])
  else
    Result := Selection[0].Parent;
end;

function TJvDesignSurface.ContainerToSelectedContainer(
  const inPt: TPoint): TPoint;
var
  c: TControl;
begin
  Result := inPt;
  c := SelectedContainer;
  while (c <> Container) and (c <> nil) do
  begin
    Dec(Result.X, c.Left);
    Dec(Result.Y, c.Top);
    c := c.Parent;
  end;
end;

function TJvDesignSurface.GetAddBounds: TRect;
begin
  with Result, Controller do
  begin
    TopLeft := ContainerToSelectedContainer(DragRect.TopLeft);
    BottomRight := ContainerToSelectedContainer(DragRect.BottomRight);
  end;
end;

procedure TJvDesignSurface.GetAddClass;
begin
  if Assigned(OnGetAddClass) then
    OnGetAddClass(Self, FAddClass);
end;

procedure TJvDesignSurface.AddComponent;
var
  cc: TComponentClass;
  c: TComponent;
  co: TControl;

  function GetBounds: TRect;
  begin
    Result := GetAddBounds;
    if DesignRectWidth(Result) = 0 then
      Result.Right := Result.Left + co.Width;
    if DesignRectHeight(Result) = 0 then
      Result.Bottom := Result.Top + co.Height;
  end;

begin
  cc := TComponentClass(GetClass(AddClass));
  if (cc <> nil) and (SelectedContainer <> nil) then
  begin
    //c := cc.Create(Owner);
    //c.Name := DesignUniqueName(Owner, AddClass);
    c := cc.Create(Container);
    c.Name := DesignUniqueName(Container, AddClass);
    if (c is TControl) then
    begin
      co := TControl(c);
      co.Parent := SelectedContainer;
      co.BoundsRect := GetBounds;
      Select(co);
    end;
    Messenger.DesignComponent(c);
    SelectionChange;
    Change;
    AddClass := '';
  end;
end;

procedure TJvDesignSurface.NudgeComponents(inNudgeLeft, inNudgeTop: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    with Selection[i] do
    begin
      Left := Left + inNudgeLeft;
      Top := Top + inNudgeTop;
    end;
  Change;
end;

procedure TJvDesignSurface.GrowComponents(inGrowWidth, inGrowHeight: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    with Selection[i] do
    begin
      Width := DesignMax(1, Width + inGrowWidth);
      Height := DesignMax(1, Height + inGrowHeight);
    end;
  Change;
end;

procedure TJvDesignSurface.DeleteComponents;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Pred(Count) do
      Selection[i].Free;
    ClearSelection;
    SelectionChange;
    Change;
  end;
end;

procedure TJvDesignSurface.CopyComponents;
var
  i: Integer;
begin
  with TJvDesignComponentClipboard.Create do
  try
    OpenWrite;
    try
      for i := 0 to Pred(Count) do
        SetComponent(Selection[i]);
    finally
      CloseWrite;
    end;
  finally
    Free;
  end;
end;

procedure TJvDesignSurface.CutComponents;
begin
  CopyComponents;
  DeleteComponents;
end;

procedure TJvDesignSurface.PasteComponents;
var
  co: TControl;
  c: TComponent;
  p: TWinControl;

  procedure KeepInParent;
  begin
    with p do
    begin
      if co.Left > ClientWidth then
        co.Left := ClientWidth - co.Width;
      if co.Top > ClientHeight then
        co.Top := ClientHeight - co.Height;
    end;
  end;

  procedure PasteComponent;
  begin
    c.Name := DesignUniqueName(Owner, c.ClassName);
    Owner.InsertComponent(c);
    if c is TControl then
    begin
      co := TControl(c);
      KeepInParent;
      co.Parent := p;
      Selector.AddToSelection(co);
    end;
  end;

begin
  with TJvDesignComponentClipboard.Create do
  try
    OpenRead;
    try
      c := GetComponent;
      if (c <> nil) then
      begin
        p := SelectedContainer;
        ClearSelection;
        repeat
          PasteComponent;
          c := GetComponent;
        until (c = nil);
        SelectionChange;
        Change;
      end;
    finally
      CloseRead;
    end;
  finally
    Free;
  end;
end;

procedure TJvDesignSurface.SelectParent;
begin
  if (Count > 0) then
    Select(Selection[0].Parent);
end;

{
procedure TJvDesignSurface.PaintContainerBkgnd(inDC: HDC);
var
  r: TRect;
  canvas: TCanvas;
begin
  if DrawGrid then
  begin
    canvas := TCanvas.Create;
    try
      SelectClipRgn(inDC, 0);
      canvas.Handle := inDC;
      canvas.Brush.Color := Container.Brush.Color;
      r := canvas.ClipRect;
      if Assigned(OnOwnerDrawGrid) then
        OnOwnerDrawGrid(Self, canvas, Container.ClientRect)
      else begin
        canvas.FillRect(Container.ClientRect);
        DesignPaintRules(canvas, Container.ClientRect);
      end;
    finally
      canvas.Free;
    end;
  end;
end;
}

function TJvDesignSurface.IsDesignMessage(inSender: TControl;
  var inMsg: TMessage; const inPt: TPoint): Boolean;

  function VirtKey: Cardinal;
  begin
    Result := inMsg.WParam;
  end;

{
  function HandlePaint: Boolean;
  begin
    Result := false;
  end;

  function HandleEraseBkgnd: Boolean;
  begin
    if (inSender <> Container) then
      Result := false
    else begin
       PaintContainerBkgnd(TWMPaint(inMsg).DC);
       inMsg.Result := 1;
       Result := true;
    end;
  end;
}

begin
  if not Active then
    Result := false
  else
    case inMsg.Msg of
{
      WM_ERASEBKGND: Result := HandleEraseBkgnd;
      WM_PAINT: Result := HandlePaint;
}
      WM_LBUTTONDOWN: Result := Controller.MouseDown(mbLeft, inPt.X, inPt.Y);
      WM_LBUTTONUP: Result := Controller.MouseUp(mbLeft, inPt.X, inPt.Y);
      WM_MOUSEMOVE: Result := Controller.MouseMove(inPt.X, inPt.Y);
      WM_KEYDOWN: Result := Controller.KeyDown(VirtKey);
      WM_KEYUP: Result := Controller.KeyUp(VirtKey);
      else Result := false;
    end;
end;

function TJvDesignSurface.GetCursor(inX, inY: Integer): TCursor;
begin
  // Using FindControl is inefficient.
  // All we really want to know is if Selected[0] contains (inX, inY)  
  if (Count > 0) and (FindControl(inX, inY) = Selected[0]) then
    Result := Selector.GetCursor(inX, inY)
  else
    Result := crDefault;
end;

function TJvDesignSurface.GetHitHandle(inX, inY: Integer): TJvDesignHandleId;
begin
  Result := Selector.GetHitHandle(inX, inY);
end;

procedure TJvDesignSurface.BeginUpdate;
begin
  Active := false;
  FUpdateOwner := Owner;
  Owner.RemoveComponent(Self);
end;

procedure TJvDesignSurface.EndUpdate;
begin
  FUpdateOwner.InsertComponent(Self);
  Active := true;
end;

procedure TJvDesignSurface.ReaderError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := true;
end;

function TJvDesignSurface.Clear: TJvDesignSurface;
begin
  BeginUpdate;
  Container.DestroyComponents;
  EndUpdate;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToStream(inStream: TStream);
begin
  BeginUpdate;
  DesignSaveComponentToStream(Container, inStream);
  EndUpdate;
end;

function TJvDesignSurface.LoadFromStream(inStream: TStream): TJvDesignSurface;
begin
  BeginUpdate;
  Container.DestroyComponents;
  DesignLoadComponentFromStream(Container, inStream, ReaderError);
  EndUpdate;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToFile(const inFilename: string);
begin
  BeginUpdate;
  DesignSaveComponentToFile(Container, inFilename);
  EndUpdate;
end;

function TJvDesignSurface.LoadFromFile(
  const inFilename: string): TJvDesignSurface;
begin
  BeginUpdate;
  Container.DestroyComponents;
  DesignLoadComponentFromFile(Container, inFilename, ReaderError);
  EndUpdate;
  Result := Self;
end;

{
procedure TJvDesignSurface.SetDrawGrid(const Value: Boolean);
begin
  FDrawGrid := Value;
  if Active then
    Container.Invalidate;
end;
}

{ TJvDesignScrollBox }

procedure TJvDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
  //
end;

{ TJvDesignPanel }

constructor TJvDesignPanel.Create(inOwner: TComponent);
begin
  inherited;
  FDrawRules := true;
  FSurface := TJvDesignSurface.Create(Self);
  Surface.Name := 'Surface';
  Surface.Container := Self;
end;

procedure TJvDesignPanel.SetDrawRules(const Value: Boolean);
begin
  FDrawRules := Value;
  Invalidate;
end;

procedure TJvDesignPanel.Paint;
begin
  inherited;
  if Surface.Active or (csDesigning in ComponentState) then
  begin
    if DrawRules then
      DesignPaintRules(Canvas, ClientRect);
    if Assigned(OnPaint) then
      OnPaint(Self);
  end;
end;

procedure TJvDesignPanel.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from Clear.
  FSurface := Surface.Clear;
end;

procedure TJvDesignPanel.SaveToStream(inStream: TStream);
begin
  Surface.SaveToStream(inStream);
end;

procedure TJvDesignPanel.LoadFromStream(inStream: TStream);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromStream.
  FSurface := Surface.LoadFromStream(inStream);
end;

procedure TJvDesignPanel.SaveToFile(const inFilename: string);
begin
  Surface.SaveToFile(inFilename);
end;

procedure TJvDesignPanel.LoadFromFile(const inFilename: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  FSurface := Surface.LoadFromFile(inFilename);
end;

function TJvDesignPanel.GetActive: Boolean;
begin
  Result := Surface.Active;
end;

function TJvDesignPanel.GetOnChange: TNotifyEvent;
begin
  Result := Surface.OnChange;
end;

function TJvDesignPanel.GetOnGetAddClass: TJvDesignGetAddClassEvent;
begin
  Result := Surface.OnGetAddClass;
end;

function TJvDesignPanel.GetOnSelectionChange: TNotifyEvent;
begin
  Result := Surface.OnSelectionChange;
end;

procedure TJvDesignPanel.SetActive(const Value: Boolean);
begin
  Surface.Active := Active;
end;

procedure TJvDesignPanel.SetOnChange(const Value: TNotifyEvent);
begin
  Surface.OnChange := Value;
end;

procedure TJvDesignPanel.SetOnGetAddClass(const Value: TJvDesignGetAddClassEvent);
begin
  Surface.OnGetAddClass := Value;
end;

procedure TJvDesignPanel.SetOnSelectionChange(const Value: TNotifyEvent);
begin
  Surface.OnSelectionChange := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
