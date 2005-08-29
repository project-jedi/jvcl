{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesingImp.pas, released on 2005-08-21.

The Initial Developer of the Original Code is Scott J Miles
Portions created by Scott J Miles are Copyright (C) 2005 Scott J Miles.
All Rights Reserved.

Contributor(s): Olivier Sannier (JVCL Integration)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
unit JvDesignImp;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
  Contnrs,
  JvDesignSurface;

const
  cJvDesignDefaultHandleWidth = 8;

type
  TJvDesignHandle = class(TCustomControl)
  private
    FResizeable: Boolean;
  protected
    function HandleRect(inIndex: Integer): TRect;
    function HitRect(inPoint: TPoint): Integer;
    procedure Paint; override;
    procedure PaintEdge(const inRect: TRect);
    procedure PaintHandle(const inRect: TRect);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    property Resizeable: Boolean read FResizeable write FResizeable;
  end;

  TJvDesignHandles = class(TComponent)
  private
    FContainer: TWinControl;
    FSelected: TControl;
    FResizeable: Boolean;
  protected
    function GetHandleWidth: Integer;
    function GetSelectionRect: TRect;
    function SelectedToScreenRect(const inRect: TRect): TRect;
    procedure CreateHandles;
    procedure SetContainer(const Value: TWinControl);
    procedure SetHandleRects(const inRect: TRect);
    procedure SetResizeable(const Value: Boolean);
    procedure SetSelected(const Value: TControl);
    procedure ShowHideHandles(inShow: Boolean);
  public
    Handles: array[0..3] of TJvDesignHandle;
    constructor Create(AOwner: TComponent); override;
    function HitRect(X, Y: Integer): TJvDesignHandleId;
    function SelectedToContainer(const inPt: TPoint): TPoint;
    procedure RepaintHandles;
    procedure UpdateHandles;
    property Container: TWinControl read FContainer write SetContainer;
    property HandleWidth: Integer read GetHandleWidth;
    property Resizeable: Boolean read FResizeable write SetResizeable;
    property Selected: TControl read FSelected write SetSelected;
  end;

  TJvDesignSelector = class(TJvDesignCustomSelector)
  private
    FHandles: TObjectList;
    FHandleWidth: Integer;
  protected
    function FindHandles(inValue: TControl): TJvDesignHandles;
    function GetCount: Integer; override;
    function GetHandles(inIndex: Integer): TJvDesignHandles;
    function GetSelection(inIndex: Integer): TControl; override;
    procedure SetHandles(inIndex: Integer; inValue: TJvDesignHandles);
    procedure SetHandleWidth(inValue: Integer);
    procedure SetSelection(inIndex: Integer; inValue: TControl); override;
    procedure ShowHideResizeHandles;
    property Handles[inIndex: Integer]: TJvDesignHandles read GetHandles
      write SetHandles;
  public
    constructor Create(ASurface: TJvDesignSurface); override;
    destructor Destroy; override;
    function GetClientControl(inControl: TControl): TControl; override;
    function GetCursor(inX, inY: Integer): TCursor; override;
    function GetHitHandle(inX, inY: Integer): TJvDesignHandleId; override;
    function IsSelected(inValue: TControl): Boolean; override;
    procedure AddToSelection(inValue: TControl); override;
    procedure ClearSelection; override;
    procedure RemoveFromSelection(inValue: TControl); override;
    procedure Update; override;
  published
    property HandleWidth: Integer read FHandleWidth
      write SetHandleWidth default cJvDesignDefaultHandleWidth;
  end;

  TJvDesignCustomMouseTool = class
  protected
    FDragRect: TRect;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);  virtual; abstract;
    property DragRect: TRect read FDragRect write FDragRect;
  end;

  TJvDesignDragMode = ( dmNone, dmMove, dmResize, dmSelect, dmCreate );

  TJvDesignAction = ( daSelectParent, daDelete, daCopy, daCut, daPaste,
    daNudgeLeft, daNudgeRight, daNudgeUp, daNudgeDown, daGrowWidth,
    daShrinkWidth, daGrowHeight, daShrinkHeight, daLastAction {$IFDEF COMPILER6_UP}= MAXINT{$ENDIF COMPILER6_UP} );

  TJvDesignController = class(TJvDesignCustomController)
  private
    FClicked: TControl;
    FDragMode: TJvDesignDragMode;
    FDragRect: TRect;
    FKeyDownShift: TShiftState;
    FMouseIsDown: Boolean;
    FMouseTool: TJvDesignCustomMouseTool;
  protected
    function GetDragRect: TRect; override;
    function KeyDown(inKeycode: Cardinal): Boolean; override;
    function KeyUp(inKeycode: Cardinal): Boolean; override;
    function MouseDown(Button: TMouseButton; X, Y: Integer): Boolean; override;
    function MouseMove(X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer): Boolean; override;
    procedure Action(inAction: TJvDesignAction);
  end;

  TJvDesignMouseTool = class(TJvDesignCustomMouseTool)
  private
    FSurface: TJvDesignSurface;
    FMouseLast: TPoint;
    FMouseStart: TPoint;
  protected
    function GetMouseDelta: TPoint; virtual;
  public
    constructor Create(AOwner: TJvDesignSurface); virtual;
    property Surface: TJvDesignSurface read FSurface write FSurface;
  end;

  TJvDesignMover = class(TJvDesignMouseTool)
  private
    FDragRects: array of TRect;
  protected
    procedure ApplyDragRects;
    procedure CalcDragRects;
    procedure CalcPaintRects;
    procedure PaintDragRects;
  public
    constructor Create(AOwner: TJvDesignSurface); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TJvDesignBander = class(TJvDesignMouseTool)
  protected
    function GetClient: TControl; virtual;
    function GetPaintRect: TRect;
    procedure CalcDragRect; virtual;
    procedure PaintDragRect; virtual;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TJvDesignSizer = class(TJvDesignBander)
  private
    FHandleId: TJvDesignHandleId;
  protected
    function GetClient: TControl; override;
    procedure ApplyDragRect;
    procedure ApplyMouseDelta(X, Y: Integer);
    procedure CalcDragRect; override;
  public
    constructor CreateSizer(AOwner: TJvDesignSurface;
      inHandle: TJvDesignHandleId);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TJvDesignDesigner = class(TComponent,
                            {$IFDEF COMPILER5}
                            IDesigner
                            {$ELSE}
                            IDesignerHook
                            {$ENDIF COMPILER5}
                           )
  private
    FMessenger: TJvDesignCustomMessenger;
  public
    constructor Create(inMessenger: TJvDesignCustomMessenger); reintroduce;

    property Messenger: TJvDesignCustomMessenger read FMessenger write FMessenger;

    // IDesignerNotify interface
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation); reintroduce;

    // IDesigner, IDesignerHook interface
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    procedure PaintGrid;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); reintroduce;
    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
    {$IFDEF COMPILER9_UP}
    procedure PaintMenu;
    {$ENDIF COMPILER9_UP}
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
  end;

  TJvDesignDesignerMessenger = class(TJvDesignCustomMessenger)
  private
    FDesignedForm: TCustomForm;
    FDesigner: TJvDesignDesigner;
  protected
    procedure SetComponentDesigning(inComponent: TComponent;
      inDesigning: Boolean);
    procedure SetContainer(inValue: TWinControl); override;
    procedure UndesignComponent(inComponent: TComponent);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DesignComponent(inComponent: TComponent); override;
  end;

  TJvDesignMessageHookList = class(TComponent)
  private
    FHooks: TObjectList;
    FUser: TJvDesignCustomMessenger;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(inUser: TJvDesignCustomMessenger); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Hook(inClient: TWinControl);
    procedure Unhook(inComponent: TComponent);
  end;

  TJvDesignWinControlHookMessenger = class(TJvDesignCustomMessenger)
  private
    FHooks: TJvDesignMessageHookList;
  protected
    procedure HookWinControl(inWinControl: TWinControl);
    procedure SetContainer(inValue: TWinControl); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure DesignComponent(inComponent: TComponent); override;
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
  JvDesignUtils;

var
  ShadedBits: TBitmap;

function NeedShadedBits: TBitmap;
begin
  if ShadedBits = nil then
  begin
    ShadedBits := TBitmap.Create;
    with ShadedBits do
    begin
      Width := 4;
      Height := 2;
      Canvas.Pixels[0, 0] := clGray;
      Canvas.Pixels[1, 0] := clBtnFace;
      Canvas.Pixels[2, 0] := clBtnFace;
      Canvas.Pixels[3, 0] := clBtnFace;
      Canvas.Pixels[0, 1] := clBtnFace;
      Canvas.Pixels[1, 1] := clBtnFace;
      Canvas.Pixels[2, 1] := clGray;
      Canvas.Pixels[3, 1] := clBtnFace;
    end;
  end;
  Result := ShadedBits;
end;

procedure FreeShadedBits;
begin
  FreeAndNil(ShadedBits);
end;

{ TJvDesignHandle }

function TJvDesignHandle.HandleRect(inIndex: Integer): TRect;
var
  w: Integer;
begin
  w := TJvDesignHandles(Owner).HandleWidth;
  case inIndex of
    0: Result := Rect(0, 0, w, w); // left-top
    1: Result := Rect((Width - w) div 2, 0, (Width + w) div 2, w); // middle-top
    2: Result := Rect(Width - w, 0, Width, w); // right-top
    3: Result := Rect(0, (Height - w) div 2, w, (Height + w) div 2); // left-center
  end;
end;

procedure TJvDesignHandle.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TJvDesignHandle.PaintHandle(const inRect: TRect);
begin
  Canvas.Rectangle(inRect);
end;

procedure TJvDesignHandle.PaintEdge(const inRect: TRect);
begin
  Canvas.FillRect(ClientRect);
end;

procedure TJvDesignHandle.Paint;
begin
  with Canvas do
  begin
    Brush.Bitmap := NeedShadedBits;
    PaintEdge(ClientRect);
    Brush.Bitmap := nil;
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    if Resizeable then
      if (Width > Height) then
      begin
        PaintHandle(HandleRect(0));
        PaintHandle(HandleRect(1));
        PaintHandle(HandleRect(2));
      end
      else
        PaintHandle(HandleRect(3));
  end;
end;

function TJvDesignHandle.HitRect(inPoint: TPoint): Integer;
begin
  Result := -1;
  if Width > Height then
    if PtInRect(HandleRect(0), inPoint) then
      Result := 0
    else if PtInRect(HandleRect(1), inPoint) then
      Result := 1
    else if PtInRect(HandleRect(2), inPoint) then
      Result := 2;
  if Result < 0 then
    if PtInRect(HandleRect(3), inPoint) then
      Result := 3;
end;

{ TJvDesignHandles }

constructor TJvDesignHandles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateHandles;
  Resizeable := true;
end;

procedure TJvDesignHandles.CreateHandles;
var
  i: Integer;
begin
  for i := 0 to 3 do
    Handles[i] := TJvDesignHandle.Create(Self);
end;

function TJvDesignHandles.GetHandleWidth: Integer;
begin
  Result := TJvDesignSelector(Owner).HandleWidth;
end;

procedure TJvDesignHandles.SetContainer(const Value: TWinControl);
var
  i: Integer;
begin
  FContainer := Value;
  for i := 0 to 3 do
    with Handles[i] do
    begin
      Visible := false;
      Parent := Container;
    end;
end;

procedure TJvDesignHandles.SetSelected(const Value: TControl);
begin
  if (Selected <> Value) then
  begin
    if (Value is TJvDesignHandle) then
      FSelected := nil
    else
      FSelected := Value;
    UpdateHandles;
  end;
end;

procedure TJvDesignHandles.SetResizeable(const Value: Boolean);
var
  i: Integer;
begin
  FResizeable := Value;
  for i := 0 to 3 do
    Handles[i].Resizeable := Value;
end;

procedure TJvDesignHandles.ShowHideHandles(inShow: Boolean);
var
  i: Integer;
begin
  for i := 0 to 3 do
    with Handles[i] do
    begin
      Visible := inShow;
      if inShow then
        BringToFront;
      Update;
    end;
end;

procedure TJvDesignHandles.UpdateHandles;
begin
  if (Selected <> nil) and (Container <> nil) and (Selected <> Container) then
  begin
    SetHandleRects(GetSelectionRect);
    ShowHideHandles(true);
  end else
    ShowHideHandles(false)
end;

procedure TJvDesignHandles.RepaintHandles;
var
  i: Integer;
begin
  for i := 0 to 3 do
    Handles[i].Repaint;
end;

function TJvDesignHandles.HitRect(X, Y: Integer): TJvDesignHandleId;
const
  cRectIds: array[0..3, 0..3] of TJvDesignHandleId = (
    ( dhLeftTop, dhMiddleTop, dhRightTop, dhNone ),
    ( dhNone, dhNone, dhNone, dhLeftMiddle ),
    ( dhNone, dhNone, dhNone, dhRightMiddle ),
    ( dhLeftBottom, dhMiddleBottom, dhRightBottom, dhNone )
  );
var
  i, r: Integer;
begin
  for i := 0 to 3 do
  begin
    with Handles[i] do
      r := HitRect(Point(X - Left, Y - Top));
    if (r >= 0) then
    begin
      Result := cRectIds[i][r];
      exit;
    end;
  end;
  Result := dhNone;
end;

function TJvDesignHandles.SelectedToContainer(const inPt: TPoint): TPoint;
var
  c: TControl;
begin
  Result := inPt;
  c := Selected.Parent;
  while (c <> Container) and (c <> nil) do
  begin
    Inc(Result.X, c.Left);
    Inc(Result.Y, c.Top);
    c := c.Parent;
  end;
end;

function TJvDesignHandles.SelectedToScreenRect(const inRect: TRect): TRect;
var
  p: TWinControl;
begin
  if Selected = Container then
    p := Container
  else
    p := Selected.Parent;
  Result.topLeft := p.ClientToScreen(inRect.topLeft);
  Result.bottomRight := p.ClientToScreen(inRect.bottomRight);
end;

function TJvDesignHandles.GetSelectionRect: TRect;
var
  p: TPoint;
begin
  if (Selected = Container) then
    p := Point(0, 0)
  else
    p := SelectedToContainer(Selected.BoundsRect.topLeft);
  Result := Rect(p.X, p.Y, p.X + Selected.Width, p.y + Selected.Height);
  InflateRect(Result, -HandleWidth div 2, -HandleWidth div 2);
end;

procedure TJvDesignHandles.SetHandleRects(const inRect: TRect);
var
  w: Integer;
begin
  w := HandleWidth;
  with inRect do
  begin
    Handles[0].BoundsRect := Rect(Left - w, Top - w, Right + w, Top);
    Handles[1].BoundsRect := Rect(Left - w, Top, Left, Bottom);
    Handles[2].BoundsRect := Rect(Right, Top, Right + w, Bottom);
    Handles[3].BoundsRect := Rect(Left - w, Bottom, Right + w, Bottom + w);
  end;
end;

{ TJvDesignSelector }

constructor TJvDesignSelector.Create(ASurface: TJvDesignSurface);
begin
  inherited Create(ASurface);
  //ControllerClass := TJvDesignController;
  FHandleWidth := cJvDesignDefaultHandleWidth;
  FHandles := TObjectList.Create;
end;

destructor TJvDesignSelector.Destroy;
begin
  FHandles.Free;
  inherited Destroy;
end;

procedure TJvDesignSelector.SetHandleWidth(inValue: Integer);
begin
  FHandleWidth := inValue;
  Update;
end;

function TJvDesignSelector.GetCount: Integer;
begin
  Result := FHandles.Count;
end;

function TJvDesignSelector.GetHandles(inIndex: Integer): TJvDesignHandles;
begin
  Result := TJvDesignHandles(FHandles[inIndex]);
end;

procedure TJvDesignSelector.SetHandles(inIndex: Integer; inValue: TJvDesignHandles);
begin
  FHandles[inIndex] := inValue;
end;

function TJvDesignSelector.GetSelection(inIndex: Integer): TControl;
begin
  Result := Handles[inIndex].Selected;
end;

procedure TJvDesignSelector.SetSelection(inIndex: Integer; inValue: TControl);
begin
  Handles[inIndex].Selected := inValue;
end;

function TJvDesignSelector.FindHandles(inValue: TControl): TJvDesignHandles;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
  begin
    Result := Handles[i];
    if Result.Selected = inValue then
      exit;
  end;
  Result := nil;
end;

function TJvDesignSelector.IsSelected(inValue: TControl): Boolean;
begin
  Result := FindHandles(inValue) <> nil;
end;

procedure TJvDesignSelector.ClearSelection;
begin
  //if not (csDestroying in ComponentState) then
  FHandles.Clear;
end;

procedure TJvDesignSelector.ShowHideResizeHandles;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    with Handles[i] do
    begin
      Resizeable := (Count = 1);
      RepaintHandles;
    end;
end;

procedure TJvDesignSelector.AddToSelection(inValue: TControl);
var
  h: TJvDesignHandles;
begin
  if inValue = nil then
    raise Exception.Create('Cannot add a nil selection.');
  if not IsSelected(inValue) then
  begin
    h := TJvDesignHandles.Create(Self);
    h.Container := Surface.Container;
    h.Resizeable := Count = 0;
    FHandles.Add(h);
    h.Selected := inValue;
    if (Count = 2) then
      ShowHideResizeHandles
    else
      h.UpdateHandles;
    Surface.Messenger.DesignComponent(h.Handles[0]);
    Surface.Messenger.DesignComponent(h.Handles[1]);
    Surface.Messenger.DesignComponent(h.Handles[2]);
    Surface.Messenger.DesignComponent(h.Handles[3]);
  end;
end;

procedure TJvDesignSelector.RemoveFromSelection(inValue: TControl);
begin
  if IsSelected(inValue) then
  begin
    FHandles.Remove(FindHandles(inValue));
    Surface.SelectionChange;
  end;
end;

function TJvDesignSelector.GetClientControl(inControl: TControl): TControl;
begin
  if inControl is TJvDesignHandle then
    Result := TJvDesignHandles(inControl.Owner).Selected
  else
    Result := inControl;
end;

procedure TJvDesignSelector.Update;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    Handles[i].UpdateHandles;
end;

function TJvDesignSelector.GetHitHandle(inX, inY: Integer): TJvDesignHandleId;
begin
  if Count > 0 then
    Result := Handles[0].HitRect(inX, inY)
  else
    Result := dhNone;
end;

function TJvDesignSelector.GetCursor(inX, inY: Integer): TCursor;
const
  cCurs: array[TJvDesignHandleId] of TCursor =
    ( crHandPoint, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeWE,
      crSizeNESW, crSizeNS, crSizeNWSE );
begin
  Result := cCurs[GetHitHandle(inX, inY)]
end;

{ TJvDesignController }

procedure TJvDesignController.Action(inAction: TJvDesignAction);
begin
  with Surface do
    case inAction of
      daSelectParent: SelectParent;
      daDelete: DeleteComponents;
      daCopy: CopyComponents;
      daCut: CutComponents;
      daPaste: PasteComponents;
      daNudgeLeft: NudgeComponents(-1, 0);
      daNudgeRight: NudgeComponents(1, 0);
      daNudgeUp: NudgeComponents(0, -1);
      daNudgeDown: NudgeComponents(0, 1);
      daGrowWidth: GrowComponents(1, 0);
      daShrinkWidth: GrowComponents(-1, 0);
      daGrowHeight: GrowComponents(0, 1);
      daShrinkHeight: GrowComponents(0, -1);
    end;
  Surface.UpdateDesigner;
end;

function TJvDesignController.GetDragRect: TRect;
begin
  Result := FDragRect;
end;

function TJvDesignController.KeyDown(inKeycode: Cardinal): Boolean;

  function CtrlKeys: Boolean;
  begin
    Result := true;
    case inKeycode of
      VK_LEFT:  Action(daNudgeLeft);
      VK_RIGHT: Action(daNudgeRight);
      VK_UP:    Action(daNudgeUp);
      VK_DOWN:  Action(daNudgeDown);
      else Result := false;
    end;
  end;

  function ShiftKeys: Boolean;
  begin
    Result := true;
    case inKeycode of
      VK_LEFT:  Action(daShrinkWidth);
      VK_RIGHT: Action(daGrowWidth);
      VK_UP:    Action(daShrinkHeight);
      VK_DOWN:  Action(daGrowHeight);
      else Result := false;
    end;
  end;

begin
  FKeyDownShift := Shift;
  if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := false
end;

function TJvDesignController.KeyUp(inKeycode: Cardinal): Boolean;

  function Keys: Boolean;
  begin
    Result := true;
    case inKeycode of
      VK_ESCAPE: Action(daSelectParent);
      VK_DELETE: Action(daDelete);
      else Result := false;
    end;
  end;

  function CtrlKeys: Boolean;
  begin
    Result := true;
    case inKeycode of
      Ord('C'): Action(daCopy);
      Ord('X'): Action(daCut);
      Ord('V'): Action(daPaste);
      else Result := false;
    end;
  end;

  function ShiftKeys: Boolean;
  begin
    Result := false;
  end;

begin
  FKeyDownShift := FKeyDownShift + Shift;
  if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := Keys;
  FKeyDownShift := [];
end;

function TJvDesignController.MouseDown(Button: TMouseButton;
  X, Y: Integer): Boolean;
var
  handleId: TJvDesignHandleId;

  procedure CaptureMouse;
  begin
    FMouseIsDown := true;
    Mouse.Capture := Surface.Container.Handle;
  end;

  procedure FocusSurface;
  begin
    if not Surface.Container.Focused and Surface.Container.CanFocus then
      Surface.Container.SetFocus;
  end;

  procedure SelectDragMode;
  begin
    handleId := dhNone;
    if (ssCtrl in Shift) then
      // Ctrl-drag selection has highest priority
      FDragMode := dmSelect
    else begin
      handleId := Surface.GetHitHandle(X, Y);
      if (handleId <> dhNone) then
      begin
        FClicked := Surface.Selection[0];
        FDragMode := dmResize;
      end
      else begin
        FClicked := Surface.FindControl(X, Y);
        if (FClicked = Surface.Container) or (FClicked is TJvDesignHandle) then
          FClicked := nil;
        Surface.GetAddClass;
        if (Surface.AddClass <> '') then
          // then object creation
          FDragMode := dmCreate
        else if FClicked <> nil then
          // moving is last
          FDragMode := dmMove
        else
          // select by default
          FDragMode := dmSelect;
      end;
    end;
    if FClicked = nil then
      FClicked := Surface.Container;
    FClicked.Parent.DisableAlign;
  end;

  procedure CreateMouseTool;
  begin
    case FDragMode of
      dmSelect, dmCreate:
      begin
        Surface.ClearSelection;
        FMouseTool := TJvDesignBander.Create(Surface);
      end;
      //
      dmMove:
      begin
        if (ssShift in Shift) then
          Surface.Selector.AddToSelection(FClicked)
        else if not Surface.Selector.IsSelected(FClicked) then
          Surface.Select(FClicked);
        FMouseTool := TJvDesignMover.Create(Surface);
      end;
      //
      dmResize:
      begin
        if not Surface.Selector.IsSelected(FClicked) then
          Surface.Select(FClicked);
        FMouseTool := TJvDesignSizer.CreateSizer(Surface, handleId);
      end;
    end;
    if FMouseTool <> nil then
      FMouseTool.MouseDown(Button, Shift, X, Y);
  end;

begin
  FocusSurface;
  CaptureMouse;
  SelectDragMode;
  CreateMouseTool;
  Result := true;
end;

function TJvDesignController.MouseMove(X, Y: Integer): Boolean;
begin
  if not FMouseIsDown then
    Windows.SetCursor(Screen.Cursors[Surface.GetCursor(X, Y)])
  else
    if FMouseTool <> nil then
      FMouseTool.MouseMove(Shift, X, Y);
  Result := true;
end;

function TJvDesignController.MouseUp(Button: TMouseButton;
  X, Y: Integer): Boolean;

  procedure ReleaseMouse;
  begin
    FMouseIsDown := false;
    Mouse.Capture := 0;
  end;

  procedure EnableAlign;
  begin
    // If the debugger breaks in during a mouse operation,
    // AlignDisabled can become stuck.
    // This routine is to aid debugging only.
    if FClicked <> nil then
{$IFDEF COMPILER6_UP}
      while FClicked.Parent.AlignDisabled do
{$ENDIF COMPILER6_UP}
        FClicked.Parent.EnableAlign;
  end;

  procedure FinishMouseTool;
  begin
    if FMouseTool <> nil then
    try
      FMouseTool.MouseUp(Button, Shift, X, Y);
      FDragRect := DesignValidateRect(FMouseTool.DragRect);
      case FDragMode of
        dmCreate:
        begin
          if FClicked <> nil then
            Surface.Select(FClicked);
          Surface.AddComponent;
        end;
        else Surface.SelectionChange;
      end;
    finally
      FreeAndNil(FMouseTool);
    end;
  end;

begin
  if FMouseIsDown then
  begin
    ReleaseMouse;
    EnableAlign;
    FinishMouseTool;
    FClicked := nil;
  end;
  Result := true;
end;

{ TJvDesignMouseTool }

constructor TJvDesignMouseTool.Create(AOwner: TJvDesignSurface);
begin
  Surface := AOwner;
end;

function TJvDesignMouseTool.GetMouseDelta: TPoint;
const
  GridX = 4;
  GridY = 4;
begin
  with Result do
  begin
    X := FMouseLast.X - FMouseStart.X;
    Dec(X, X mod GridX);
    Y := FMouseLast.Y - FMouseStart.Y;
    Dec(Y, Y mod GridY);
  end;
end;

{ TJvDesignMover }

constructor TJvDesignMover.Create(AOwner: TJvDesignSurface);
begin
  inherited;
  SetLength(FDragRects, Surface.Count);
end;

procedure TJvDesignMover.CalcDragRects;
var
  delta: TPoint;
  i: Integer;
begin
  delta := GetMouseDelta;
  for i := 0 to Pred(Surface.Count) do
    with Surface.Selection[i] do
    begin
      FDragRects[i] := BoundsRect;
      OffsetRect(FDragRects[i], delta.X, delta.Y);
    end;
end;

procedure TJvDesignMover.CalcPaintRects;
var
  i: Integer;
begin
  CalcDragRects;
  for i := 0 to Pred(Surface.Count) do
    with Surface.Selection[i] do
      with Parent.ClientToScreen(Point(0, 0)) do
        OffsetRect(FDragRects[i], X, Y);
end;

procedure TJvDesignMover.PaintDragRects;
var
  i: Integer;
begin
  for i := 0 to Pred(Surface.Count) do
    DesignPaintRubberbandRect(FDragRects[i], psDot);
end;

procedure TJvDesignMover.ApplyDragRects;
var
  i: Integer;
begin
  if (GetMouseDelta.X <> 0) or (GetMouseDelta.Y <> 0) then
  begin
    CalcDragRects;
    for i := 0 to Pred(Surface.Count) do
      Surface.Selection[i].BoundsRect := FDragRects[i];
    Surface.UpdateDesigner;
    Surface.Change;
  end;
end;

procedure TJvDesignMover.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseStart := Point(X, Y);
  FMouseLast := FMouseStart;
  CalcPaintRects;
  PaintDragRects;
end;

procedure TJvDesignMover.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  PaintDragRects;
  FMouseLast := Point(X, Y);
  CalcPaintRects;
  PaintDragRects;
end;

procedure TJvDesignMover.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  PaintDragRects;
  FMouseLast := Point(X, Y);
  ApplyDragRects;
end;

{ TJvDesignBander }

procedure TJvDesignBander.CalcDragRect;
begin
  with GetMouseDelta do
  begin
    DragRect := Rect(0, 0, X, Y);
    OffsetRect(FDragRect, FMouseStart.X, FMouseStart.Y);
  end;
end;

function TJvDesignBander.GetClient: TControl;
begin
  Result := Surface.Container;
end;

function TJvDesignBander.GetPaintRect: TRect;
begin
  Result := FDragRect;
  with GetClient.ClientToScreen(Point(0, 0)) do
    OffsetRect(Result, X, Y);
end;

procedure TJvDesignBander.PaintDragRect;
begin
  DesignPaintRubberbandRect(GetPaintRect, psDot);
end;

procedure TJvDesignBander.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseStart := Point(X, Y);
  FMouseLast := FMouseStart;
  CalcDragRect;
  PaintDragRect;
end;

procedure TJvDesignBander.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  PaintDragRect;
  FMouseLast := Point(X, Y);
  CalcDragRect;
  PaintDragRect;
end;

procedure TJvDesignBander.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  PaintDragRect;
  CalcDragRect;
end;

{ TJvDesignSizer }

constructor TJvDesignSizer.CreateSizer(AOwner: TJvDesignSurface;
  inHandle: TJvDesignHandleId);
begin
  inherited Create(AOwner);
  FHandleId := inHandle;
end;

procedure TJvDesignSizer.ApplyMouseDelta(X, Y: Integer);
begin
  case FHandleId of
    dhLeftTop, dhMiddleTop, dhRightTop: Inc(FDragRect.Top, Y);
    dhLeftBottom, dhMiddleBottom, dhRightBottom: Inc(FDragRect.Bottom, Y);
  end;
  case FHandleId of
    dhLeftTop, dhLeftMiddle, dhLeftBottom: Inc(FDragRect.Left, X);
    dhRightTop, dhRightMiddle, dhRightBottom: Inc(FDragRect.Right, X);
  end;
end;

procedure TJvDesignSizer.CalcDragRect;
begin
  FDragRect := Surface.Selection[0].BoundsRect;
  with GetMouseDelta do
    ApplyMouseDelta(X, Y);
  FDragRect := DesignValidateRect(FDragRect);
end;

function TJvDesignSizer.GetClient: TControl;
begin
  Result := Surface.Selection[0].Parent;
end;

procedure TJvDesignSizer.ApplyDragRect;
begin
  Surface.Selection[0].BoundsRect := FDragRect;
  Surface.UpdateDesigner;
  Surface.Change;
end;

procedure TJvDesignSizer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  ApplyDragRect;
end;

{ TJvDesignDesigner }

constructor TJvDesignDesigner.Create(inMessenger: TJvDesignCustomMessenger);
begin
  inherited Create(nil);
  FMessenger := inMessenger;
end;

function TJvDesignDesigner.GetCustomForm: TCustomForm;
begin
  Result := nil;
end;

function TJvDesignDesigner.GetIsControl: Boolean;
begin
  Result := false;
end;

function TJvDesignDesigner.GetRoot: TComponent;
begin
  Result := nil;
end;

function TJvDesignDesigner.IsDesignMsg(Sender: TControl;
  var Message: TMessage): Boolean;
begin
  Result := Messenger.IsDesignMessage(Sender, Message);
end;

procedure TJvDesignDesigner.Modified;
begin
  //
end;

procedure TJvDesignDesigner.Notification(AnObject: TPersistent;
  Operation: TOperation);
begin
  //
end;

procedure TJvDesignDesigner.PaintGrid;
begin
  //
end;

procedure TJvDesignDesigner.SetCustomForm(Value: TCustomForm);
begin
  //
end;

procedure TJvDesignDesigner.SetIsControl(Value: Boolean);
begin
  //
end;

function TJvDesignDesigner.UniqueName(const BaseName: string): string;
begin
  //
end;

procedure TJvDesignDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  //
end;

{$IFDEF COMPILER9_UP}
procedure TJvDesignDesigner.PaintMenu;
begin
  //
end;
{$ENDIF COMPILER9_UP}

{ TJvDesignDesignerMessenger }

constructor TJvDesignDesignerMessenger.Create;
begin
  FDesigner := TJvDesignDesigner.Create(Self);
end;

destructor TJvDesignDesignerMessenger.Destroy;
begin
  if Container <> nil then
    SetComponentDesigning(Container, false);
  if (FDesignedForm <> nil) then
    FDesignedForm.Designer := nil;
  FDesigner.Free;
  inherited;
end;

type
  TCrackedComponent = class(TComponent)
  end;

procedure TJvDesignDesignerMessenger.SetComponentDesigning(inComponent: TComponent;
  inDesigning: Boolean);
begin
  TCrackedComponent(inComponent).SetDesigning(inDesigning);
end;

procedure TJvDesignDesignerMessenger.UndesignComponent(inComponent: TComponent);
begin
  SetComponentDesigning(inComponent, false);
end;

procedure TJvDesignDesignerMessenger.DesignComponent(inComponent: TComponent);
begin
  SetComponentDesigning(inComponent, true);
end;

procedure TJvDesignDesignerMessenger.SetContainer(inValue: TWinControl);

  function FindParentForm: TCustomForm;
  var
    p: TWinControl;
  begin
    p := Container;
    while (p.Parent <> nil) do
      p := p.Parent;
    if not (p is TCustomForm) then
      raise Exception.Create(ClassName + ': Oldest ancestor of Container must be a form.');
    Result := TCustomForm(p);
  end;

begin
  inherited;
  if (Container <> nil) then
  begin
    FDesignedForm := FindParentForm;
    FDesignedForm.Designer := FDesigner;
    DesignChildren(Container);
  end;
end;

{ TJvDesignMessageHookList }

constructor TJvDesignMessageHookList.Create(inUser: TJvDesignCustomMessenger);
begin
  inherited Create(nil);
  FUser := inUser;
  FHooks := TObjectList.Create;
  FHooks.OwnsObjects := true;
end;

destructor TJvDesignMessageHookList.Destroy;
begin
  FHooks.Free;
  inherited;
end;

procedure TJvDesignMessageHookList.Clear;
begin
  FHooks.Clear;
end;

procedure TJvDesignMessageHookList.Hook(inClient: TWinControl);
begin
  inClient.FreeNotification(Self);
  FHooks.Add(TJvDesignMessageHook.Create(FUser, inClient));
end;

procedure TJvDesignMessageHookList.Unhook(inComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Pred(FHooks.Count) do
    if TJvDesignMessageHook(FHooks[i]).Client = inComponent then
    begin
      FHooks.Delete(i);
      break;
    end;
end;

procedure TJvDesignMessageHookList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    Unhook(AComponent);
end;

{ TJvDesignWinControlHookMessenger }

constructor TJvDesignWinControlHookMessenger.Create;
begin
  inherited Create;
  FHooks := TJvDesignMessageHookList.Create(Self);
end;

destructor TJvDesignWinControlHookMessenger.Destroy;
begin
  FHooks.Free;
  inherited Destroy;
end;

procedure TJvDesignWinControlHookMessenger.Clear;
begin
  FHooks.Clear;
end;

procedure TJvDesignWinControlHookMessenger.DesignComponent(inComponent: TComponent);
begin
  if inComponent is TWinControl then
    HookWinControl(TWinControl(inComponent));
end;

procedure TJvDesignWinControlHookMessenger.HookWinControl(inWinControl: TWinControl);
begin
  FHooks.Hook(inWinControl);
  DesignChildren(inWinControl);
end;

procedure TJvDesignWinControlHookMessenger.SetContainer(inValue: TWinControl);
begin
  inherited;
  if (Container <> nil) then
    DesignChildren(Container);
end;

initialization
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
  FreeShadedBits;
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.
