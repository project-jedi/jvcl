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
home page, located at http://jvcl.delphi-jedi.org

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
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  Forms, Contnrs,
  JvDesignSurface;

const
  cJvDesignDefaultHandleWidth = 8;

type
  TJvDesignHandle = class(TCustomControl)
  private
    FResizeable: Boolean;
  protected
    function HandleRect(AIndex: Integer): TRect;
    function HitRect(APoint: TPoint): Integer;
    procedure Paint; override;
    procedure PaintEdge(const ARect: TRect);
    procedure PaintHandle(const ARect: TRect);
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
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
    function SelectedToScreenRect(const ARect: TRect): TRect;
    procedure CreateHandles;
    procedure SetContainer(const Value: TWinControl);
    procedure SetHandleRects(const ARect: TRect);
    procedure SetResizeable(const Value: Boolean);
    procedure SetSelected(const Value: TControl);
    procedure ShowHideHandles(AShow: Boolean);
  public
    Handles: array [0..3] of TJvDesignHandle;
    constructor Create(AOwner: TComponent); override;
    function HitRect(X, Y: Integer): TJvDesignHandleId;
    function SelectedToContainer(const APt: TPoint): TPoint;
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
    function FindHandles(AValue: TControl): TJvDesignHandles;
    function GetCount: Integer; override;
    function GetHandles(AIndex: Integer): TJvDesignHandles;
    function GetSelection(AIndex: Integer): TControl; override;
    procedure SetHandles(AIndex: Integer; AValue: TJvDesignHandles);
    procedure SetHandleWidth(AValue: Integer);
    procedure SetSelection(AIndex: Integer; AValue: TControl); override;
    procedure ShowHideResizeHandles;
    property Handles[AIndex: Integer]: TJvDesignHandles read GetHandles write SetHandles;
  public
    constructor Create(ASurface: TJvDesignSurface); override;
    destructor Destroy; override;
    function GetClientControl(AControl: TControl): TControl; override;
    function GetCursor(AX, AY: Integer): TCursor; override;
    function GetHitHandle(AX, AY: Integer): TJvDesignHandleId; override;
    function IsSelected(AValue: TControl): Boolean; override;
    procedure AddToSelection(AValue: TControl); override;
    procedure ClearSelection; override;
    procedure RemoveFromSelection(AValue: TControl); override;
    procedure Update; override;
  published
    property HandleWidth: Integer read FHandleWidth write SetHandleWidth default cJvDesignDefaultHandleWidth;
  end;

  TJvDesignCustomMouseTool = class(TObject)
  protected
    FDragRect: TRect;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  virtual; abstract;
    property DragRect: TRect read FDragRect write FDragRect;
  end;

  TJvDesignDragMode = (dmNone, dmMove, dmResize, dmSelect, dmCreate);

  TJvDesignAction = (daSelectParent, daDelete, daCopy, daCut, daPaste,
    daNudgeLeft, daNudgeRight, daNudgeUp, daNudgeDown, daGrowWidth,
    daShrinkWidth, daGrowHeight, daShrinkHeight, daLastAction = MaxInt);

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
    function KeyDown(AKeyCode: Cardinal): Boolean; override;
    function KeyUp(AKeyCode: Cardinal): Boolean; override;
    function MouseDown(Button: TMouseButton; X, Y: Integer): Boolean; override;
    function MouseMove(X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer): Boolean; override;
    procedure Action(AAction: TJvDesignAction);
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TJvDesignBander = class(TJvDesignMouseTool)
  protected
    function GetClient: TControl; virtual;
    function GetPaintRect: TRect;
    procedure CalcDragRect; virtual;
    procedure PaintDragRect; virtual;
  public
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
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
    constructor CreateSizer(AOwner: TJvDesignSurface; AHandle: TJvDesignHandleId);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TJvDesignDesigner = class(TComponent, IDesignerHook)
  private
    FMessenger: TJvDesignCustomMessenger;
  public
    constructor Create(AMessenger: TJvDesignCustomMessenger); reintroduce;
    // IDesignerNotify interface
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation); reintroduce;

    // IDesigner, IDesignerHook interface
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Msg: TMessage): Boolean;
    {$IFDEF RTL240_UP}
    procedure DrawSelectionMarks(AControl: TControl);
    function IsSelected(AControl: TControl): Boolean;
    {$ENDIF RTL240_UP}
    procedure PaintGrid;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string); reintroduce;
    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
    {$IFDEF COMPILER9_UP}
    procedure PaintMenu;
    {$ENDIF COMPILER9_UP}
    property Messenger: TJvDesignCustomMessenger read FMessenger write FMessenger;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
  end;

  TJvDesignDesignerMessenger = class(TJvDesignCustomMessenger)
  private
    FDesignedForm: TCustomForm;
    FDesigner: TJvDesignDesigner;
  protected
    procedure SetComponentDesigning(AComponent: TComponent; ADesigning: Boolean);
    procedure SetContainer(AValue: TWinControl); override;
    procedure UndesignComponent(AComponent: TComponent);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); override;
  end;

  TJvDesignMessageHookList = class(TComponent)
  private
    FHooks: TObjectList;
    FUser: TJvDesignCustomMessenger;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AUser: TJvDesignCustomMessenger); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Hook(AClient: TWinControl);
    procedure Unhook(AComponent: TComponent);
  end;

  TJvDesignWinControlHookMessenger = class(TJvDesignCustomMessenger)
  private
    FHooks: TJvDesignMessageHookList;
  protected
    procedure HookWinControl(AWinControl: TWinControl);
    procedure UnhookWinControl(AWinControl: TWinControl);
    procedure SetContainer(AValue: TWinControl); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Types,
  JvDesignUtils, JvResources, JvTypes;

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

//=== { TJvDesignHandle } ====================================================

function TJvDesignHandle.HandleRect(AIndex: Integer): TRect;
var
  W: Integer;
begin
  W := TJvDesignHandles(Owner).HandleWidth;
  case AIndex of
    0:
      Result := Rect(0, 0, W, W); // left-top
    1:
      Result := Rect((Width - W) div 2, 0, (Width + W) div 2, W); // middle-top
    2:
      Result := Rect(Width - W, 0, Width, W); // right-top
    3:
      Result := Rect(0, (Height - W) div 2, W, (Height + W) div 2); // left-center
  end;
end;

procedure TJvDesignHandle.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TJvDesignHandle.PaintHandle(const ARect: TRect);
begin
  Canvas.Rectangle(ARect);
end;

procedure TJvDesignHandle.PaintEdge(const ARect: TRect);
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
      if Width > Height then
      begin
        PaintHandle(HandleRect(0));
        PaintHandle(HandleRect(1));
        PaintHandle(HandleRect(2));
      end
      else
        PaintHandle(HandleRect(3));
  end;
end;

function TJvDesignHandle.HitRect(APoint: TPoint): Integer;
begin
  Result := -1;
  if Width > Height then
    if PtInRect(HandleRect(0), APoint) then
      Result := 0
    else
    if PtInRect(HandleRect(1), APoint) then
      Result := 1
    else
    if PtInRect(HandleRect(2), APoint) then
      Result := 2;
  if Result < 0 then
    if PtInRect(HandleRect(3), APoint) then
      Result := 3;
end;

//=== { TJvDesignHandles } ===================================================

constructor TJvDesignHandles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateHandles;
  Resizeable := True;
end;

procedure TJvDesignHandles.CreateHandles;
var
  I: Integer;
begin
  for I := Low(Handles) to High(Handles) do
    Handles[I] := TJvDesignHandle.Create(Self);
end;

function TJvDesignHandles.GetHandleWidth: Integer;
begin
  Result := TJvDesignSelector(Owner).HandleWidth;
end;

procedure TJvDesignHandles.SetContainer(const Value: TWinControl);
var
  I: Integer;
begin
  FContainer := Value;
  for I := Low(Handles) to High(Handles) do
    with Handles[I] do
    begin
      Visible := False;
      Parent := Container;
    end;
end;

procedure TJvDesignHandles.SetSelected(const Value: TControl);
begin
  if Selected <> Value then
  begin
    if Value is TJvDesignHandle then
      FSelected := nil
    else
      FSelected := Value;
    UpdateHandles;
  end;
end;

procedure TJvDesignHandles.SetResizeable(const Value: Boolean);
var
  I: Integer;
begin
  FResizeable := Value;
  for I := Low(Handles) to High(Handles) do
    Handles[I].Resizeable := Value;
end;

procedure TJvDesignHandles.ShowHideHandles(AShow: Boolean);
var
  I: Integer;
begin
  for I := Low(Handles) to High(Handles) do
    with Handles[I] do
    begin
      Visible := AShow;
      if AShow then
        BringToFront;
      Update;
    end;
end;

procedure TJvDesignHandles.UpdateHandles;
begin
  if (Selected <> nil) and (Container <> nil) and (Selected <> Container) then
  begin
    SetHandleRects(GetSelectionRect);
    ShowHideHandles(True);
  end
  else
    ShowHideHandles(False)
end;

procedure TJvDesignHandles.RepaintHandles;
var
  I: Integer;
begin
  for I := Low(Handles) to High(Handles) do
    Handles[I].Repaint;
end;

function TJvDesignHandles.HitRect(X, Y: Integer): TJvDesignHandleId;
const
  cRectIds: array [0..3, 0..3] of TJvDesignHandleId =
   (
    (dhLeftTop, dhMiddleTop, dhRightTop, dhNone),
    (dhNone, dhNone, dhNone, dhLeftMiddle),
    (dhNone, dhNone, dhNone, dhRightMiddle),
    (dhLeftBottom, dhMiddleBottom, dhRightBottom, dhNone)
   );
var
  I, R: Integer;
begin
  for I := 0 to 3 do
  begin
    with Handles[I] do
      R := HitRect(Point(X - Left, Y - Top));
    if R >= 0 then
    begin
      Result := cRectIds[I][R];
      Exit;
    end;
  end;
  Result := dhNone;
end;

function TJvDesignHandles.SelectedToContainer(const APt: TPoint): TPoint;
var
  C: TControl;
begin
  Result := APt;
  C := Selected.Parent;
  while (C <> Container) and (C <> nil) do
  begin
    Inc(Result.X, C.Left);
    Inc(Result.Y, C.Top);
    C := C.Parent;
  end;
end;

function TJvDesignHandles.SelectedToScreenRect(const ARect: TRect): TRect;
var
  P: TWinControl;
begin
  if Selected = Container then
    P := Container
  else
    P := Selected.Parent;
  Result.TopLeft := P.ClientToScreen(ARect.TopLeft);
  Result.BottomRight := P.ClientToScreen(ARect.BottomRight);
end;

function TJvDesignHandles.GetSelectionRect: TRect;
var
  P: TPoint;
begin
  if Selected = Container then
    P := Point(0, 0)
  else
    P := SelectedToContainer(Selected.BoundsRect.TopLeft);
  Result := Rect(P.X, P.Y, P.X + Selected.Width, P.Y + Selected.Height);
  InflateRect(Result, -HandleWidth div 2, -HandleWidth div 2);
end;

procedure TJvDesignHandles.SetHandleRects(const ARect: TRect);
var
  W: Integer;
begin
  W := HandleWidth;
  Handles[0].BoundsRect := Rect(ARect.Left - W, ARect.Top - W, ARect.Right + W, ARect.Top);
  Handles[1].BoundsRect := Rect(ARect.Left - W, ARect.Top, ARect.Left, ARect.Bottom);
  Handles[2].BoundsRect := Rect(ARect.Right, ARect.Top, ARect.Right + W, ARect.Bottom);
  Handles[3].BoundsRect := Rect(ARect.Left - W, ARect.Bottom, ARect.Right + W, ARect.Bottom + W);
end;

//=== { TJvDesignSelector } ==================================================

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

procedure TJvDesignSelector.SetHandleWidth(AValue: Integer);
begin
  FHandleWidth := AValue;
  Update;
end;

function TJvDesignSelector.GetCount: Integer;
begin
  Result := FHandles.Count;
end;

function TJvDesignSelector.GetHandles(AIndex: Integer): TJvDesignHandles;
begin
  Result := TJvDesignHandles(FHandles[AIndex]);
end;

procedure TJvDesignSelector.SetHandles(AIndex: Integer; AValue: TJvDesignHandles);
begin
  FHandles[AIndex] := AValue;
end;

function TJvDesignSelector.GetSelection(AIndex: Integer): TControl;
begin
  Result := Handles[AIndex].Selected;
end;

procedure TJvDesignSelector.SetSelection(AIndex: Integer; AValue: TControl);
begin
  Handles[AIndex].Selected := AValue;
end;

function TJvDesignSelector.FindHandles(AValue: TControl): TJvDesignHandles;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Handles[I];
    if Result.Selected = AValue then
      Break
    else
      Result := nil;
  end;
end;

function TJvDesignSelector.IsSelected(AValue: TControl): Boolean;
begin
  Result := FindHandles(AValue) <> nil;
end;

procedure TJvDesignSelector.ClearSelection;
begin
  //if not (csDestroying in ComponentState) then
  FHandles.Clear;
end;

procedure TJvDesignSelector.ShowHideResizeHandles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Handles[I] do
    begin
      Resizeable := (Count = 1);
      RepaintHandles;
    end;
end;

procedure TJvDesignSelector.AddToSelection(AValue: TControl);
var
  H: TJvDesignHandles;
begin
  if AValue = nil then
    raise EJVCLException.CreateRes(@RsEDesignCannotSelect);
  if not IsSelected(AValue) then
  begin
    H := TJvDesignHandles.Create(Self);
    H.Container := Surface.Container;
    H.Resizeable := Count = 0;
    FHandles.Add(H);
    H.Selected := AValue;
    if Count = 2 then
      ShowHideResizeHandles
    else
      H.UpdateHandles;
    Surface.Messenger.DesignComponent(H.Handles[0], True);
    Surface.Messenger.DesignComponent(H.Handles[1], True);
    Surface.Messenger.DesignComponent(H.Handles[2], True);
    Surface.Messenger.DesignComponent(H.Handles[3], True);
  end;
end;

procedure TJvDesignSelector.RemoveFromSelection(AValue: TControl);
begin
  if IsSelected(AValue) then
  begin
    FHandles.Remove(FindHandles(AValue));
    Surface.SelectionChange;
  end;
end;

function TJvDesignSelector.GetClientControl(AControl: TControl): TControl;
begin
  if AControl is TJvDesignHandle then
    Result := TJvDesignHandles(AControl.Owner).Selected
  else
    Result := AControl;
end;

procedure TJvDesignSelector.Update;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Handles[I].UpdateHandles;
end;

function TJvDesignSelector.GetHitHandle(AX, AY: Integer): TJvDesignHandleId;
begin
  if Count > 0 then
    Result := Handles[0].HitRect(AX, AY)
  else
    Result := dhNone;
end;

function TJvDesignSelector.GetCursor(AX, AY: Integer): TCursor;
const
  cCurs: array[TJvDesignHandleId] of TCursor =
   (crHandPoint, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeWE,
    crSizeNESW, crSizeNS, crSizeNWSE);
begin
  Result := cCurs[GetHitHandle(AX, AY)];
end;

//=== { TJvDesignController } ================================================

procedure TJvDesignController.Action(AAction: TJvDesignAction);
begin
  with Surface do
    case AAction of
      daSelectParent:
        SelectParent;
      daDelete:
        DeleteComponents;
      daCopy:
        CopyComponents;
      daCut:
        CutComponents;
      daPaste:
        PasteComponents;
      daNudgeLeft:
        NudgeComponents(-1, 0);
      daNudgeRight:
        NudgeComponents(1, 0);
      daNudgeUp:
        NudgeComponents(0, -1);
      daNudgeDown:
        NudgeComponents(0, 1);
      daGrowWidth:
        GrowComponents(1, 0);
      daShrinkWidth:
        GrowComponents(-1, 0);
      daGrowHeight:
        GrowComponents(0, 1);
      daShrinkHeight:
        GrowComponents(0, -1);
    end;
  Surface.UpdateDesigner;
end;

function TJvDesignController.GetDragRect: TRect;
begin
  Result := FDragRect;
end;

function TJvDesignController.KeyDown(AKeyCode: Cardinal): Boolean;

  function CtrlKeys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      VK_LEFT:
        Action(daNudgeLeft);
      VK_RIGHT:
        Action(daNudgeRight);
      VK_UP:
        Action(daNudgeUp);
      VK_DOWN:
        Action(daNudgeDown);
      else
        Result := False;
    end;
  end;

  function ShiftKeys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      VK_LEFT:
        Action(daShrinkWidth);
      VK_RIGHT:
        Action(daGrowWidth);
      VK_UP:
        Action(daShrinkHeight);
      VK_DOWN:
        Action(daGrowHeight);
      else
        Result := False;
    end;
  end;

begin
  FKeyDownShift := Shift;
  if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else
  if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := False;
end;

function TJvDesignController.KeyUp(AKeyCode: Cardinal): Boolean;

  function Keys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      VK_ESCAPE:
        Action(daSelectParent);
      VK_DELETE:
        Action(daDelete);
      else
        Result := False;
    end;
  end;

  function CtrlKeys: Boolean;
  begin
    Result := True;
    case AKeyCode of
      Ord('C'):
        Action(daCopy);
      Ord('X'):
        Action(daCut);
      Ord('V'):
        Action(daPaste);
      else
        Result := False;
    end;
  end;

  function ShiftKeys: Boolean;
  begin
    Result := False;
  end;

begin
  FKeyDownShift := FKeyDownShift + Shift;
  if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else
  if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := Keys;
  FKeyDownShift := [];
end;

function TJvDesignController.MouseDown(Button: TMouseButton; X, Y: Integer): Boolean;
var
  HandleId: TJvDesignHandleId;

  procedure CaptureMouse;
  begin
    FMouseIsDown := True;
    Mouse.Capture := Surface.Container.Handle;
  end;

  procedure FocusSurface;
  var
    WasActive: Boolean;
  begin
    if not Surface.Container.Focused and Surface.Container.CanFocus then
    begin
      // Mantis 4732: deactivate the container otherwise SetFocus does not work
      // This bug apparently only happens under certain rare conditions
      // under windows but its fix does not seem to have any negative impact
      // on systems where it does not happen.
      WasActive := TJvDesignPanel(Surface.Container).Active;
      if WasActive then
        TJvDesignPanel(Surface.Container).Active := False;

      Surface.Container.SetFocus;

      if WasActive then
        TJvDesignPanel(Surface.Container).Active := True;
    end;
  end;

  procedure SelectDragMode;
  begin
    HandleId := dhNone;
    if ssCtrl in Shift then
      // Ctrl-drag selection has highest priority
      FDragMode := dmSelect
    else
    begin
      HandleId := Surface.GetHitHandle(X, Y);
      if HandleId <> dhNone then
      begin
        FClicked := Surface.Selection[0];
        FDragMode := dmResize;
      end
      else
      begin
        FClicked := Surface.FindControl(X, Y);
        if (FClicked = Surface.Container) or (FClicked is TJvDesignHandle) then
          FClicked := nil;
        Surface.GetAddClass;
        if Surface.AddClass <> '' then
          // then object creation
          FDragMode := dmCreate
        else
        if FClicked <> nil then
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
      dmMove:
        begin
          if ssShift in Shift then
            Surface.Selector.AddToSelection(FClicked)
          else
          if not Surface.Selector.IsSelected(FClicked) then
            Surface.Select(FClicked);
          FMouseTool := TJvDesignMover.Create(Surface);
        end;
      dmResize:
        begin
          if not Surface.Selector.IsSelected(FClicked) then
            Surface.Select(FClicked);
          FMouseTool := TJvDesignSizer.CreateSizer(Surface, HandleId);
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
  Result := True;
end;

function TJvDesignController.MouseMove(X, Y: Integer): Boolean;
begin
  if not FMouseIsDown then
    Windows.SetCursor(Screen.Cursors[Surface.GetCursor(X, Y)])
  else
  if FMouseTool <> nil then
    FMouseTool.MouseMove(Shift, X, Y);
  Result := True;
end;

function TJvDesignController.MouseUp(Button: TMouseButton; X, Y: Integer): Boolean;

  procedure ReleaseMouse;
  begin
    FMouseIsDown := False;
    Mouse.Capture := 0;
  end;

  procedure EnableAlign;
  begin
    // If the debugger breaks in during a mouse operation,
    // AlignDisabled can become stuck.
    // This routine is to aid debugging only.
    if FClicked <> nil then
      while FClicked.Parent.AlignDisabled do
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
        else
          Surface.SelectionChange;
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
    // We have to call UpdateDesigner for GraphicControls because they don't get
    // WM_WINDOWPOSCHANGED messages that update the designer handles.
    if FClicked is TGraphicControl then
      Surface.UpdateDesigner;
    FClicked := nil;
  end;
  Result := True;
end;

//=== { TJvDesignMouseTool } =================================================

constructor TJvDesignMouseTool.Create(AOwner: TJvDesignSurface);
begin
  Surface := AOwner;
end;

function TJvDesignMouseTool.GetMouseDelta: TPoint;
const
  GridX = 4;
  GridY = 4;
begin
  Result.X := FMouseLast.X - FMouseStart.X;
  Dec(Result.X, Result.X mod GridX);
  Result.Y := FMouseLast.Y - FMouseStart.Y;
  Dec(Result.Y, Result.Y mod GridY);
end;

//=== { TJvDesignMover } =====================================================

constructor TJvDesignMover.Create(AOwner: TJvDesignSurface);
begin
  inherited Create(AOwner);
  SetLength(FDragRects, Surface.Count);
end;

procedure TJvDesignMover.CalcDragRects;
var
  Delta: TPoint;
  I: Integer;
begin
  Delta := GetMouseDelta;
  for I := 0 to Surface.Count - 1 do
    with Surface.Selection[I] do
    begin
      FDragRects[I] := BoundsRect;
      OffsetRect(FDragRects[I], Delta.X, Delta.Y);
    end;
end;

procedure TJvDesignMover.CalcPaintRects;
var
  I: Integer;
  Pt: TPoint;
begin
  CalcDragRects;
  for I := 0 to Surface.Count - 1 do
    with Surface.Selection[I] do
    begin
      Pt := Parent.ClientToScreen(Point(0, 0));
      OffsetRect(FDragRects[I], Pt.X, Pt.Y);
    end;
end;

procedure TJvDesignMover.PaintDragRects;
var
  I: Integer;
begin
  for I := 0 to Surface.Count - 1 do
    DesignPaintRubberbandRect(Surface.Container, FDragRects[I], psDot);
end;

procedure TJvDesignMover.ApplyDragRects;
var
  I: Integer;
begin
  if (GetMouseDelta.X <> 0) or (GetMouseDelta.Y <> 0) then
  begin
    CalcDragRects;
    for I := 0 to Surface.Count - 1 do
      Surface.Selection[I].BoundsRect := FDragRects[I];
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

//=== { TJvDesignBander } ====================================================

procedure TJvDesignBander.CalcDragRect;
var
  Pt: TPoint;
begin
  Pt := GetMouseDelta;
  FDragRect := Rect(0, 0, Pt.X, Pt.Y);
  OffsetRect(FDragRect, FMouseStart.X, FMouseStart.Y);
end;

function TJvDesignBander.GetClient: TControl;
begin
  Result := Surface.Container;
end;

function TJvDesignBander.GetPaintRect: TRect;
var
  Pt: TPoint;
begin
  Result := FDragRect;
  Pt := GetClient.ClientToScreen(Point(0, 0));
  OffsetRect(Result, Pt.X, Pt.Y);
end;

procedure TJvDesignBander.PaintDragRect;
begin
  DesignPaintRubberbandRect(Surface.Container, GetPaintRect, psDot);
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

//=== { TJvDesignSizer } =====================================================

constructor TJvDesignSizer.CreateSizer(AOwner: TJvDesignSurface; AHandle: TJvDesignHandleId);
begin
  inherited Create(AOwner);
  FHandleId := AHandle;
end;

procedure TJvDesignSizer.ApplyMouseDelta(X, Y: Integer);
begin
  case FHandleId of
    dhLeftTop, dhMiddleTop, dhRightTop:
      Inc(FDragRect.Top, Y);
    dhLeftBottom, dhMiddleBottom, dhRightBottom:
      Inc(FDragRect.Bottom, Y);
  end;
  case FHandleId of
    dhLeftTop, dhLeftMiddle, dhLeftBottom:
      Inc(FDragRect.Left, X);
    dhRightTop, dhRightMiddle, dhRightBottom:
      Inc(FDragRect.Right, X);
  end;
end;

procedure TJvDesignSizer.CalcDragRect;
var
  Pt: TPoint;
begin
  FDragRect := Surface.Selection[0].BoundsRect;
  Pt := GetMouseDelta;
  ApplyMouseDelta(Pt.X, Pt.Y);
  FDragRect := DesignValidateRect(FDragRect);
end;

function TJvDesignSizer.GetClient: TControl;
begin
  Result := Surface.Selection[0].Parent;
end;

procedure TJvDesignSizer.ApplyDragRect;
begin
  Surface.Selection[0].BoundsRect := FDragRect;
  Surface.Change;
end;

procedure TJvDesignSizer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  ApplyDragRect;
end;

//=== { TJvDesignDesigner } ==================================================

constructor TJvDesignDesigner.Create(AMessenger: TJvDesignCustomMessenger);
begin
  inherited Create(nil);
  FMessenger := AMessenger;
end;

function TJvDesignDesigner.GetCustomForm: TCustomForm;
begin
  Result := nil;
end;

function TJvDesignDesigner.GetIsControl: Boolean;
begin
  Result := False;
end;

function TJvDesignDesigner.GetRoot: TComponent;
begin
  Result := nil;
end;

function TJvDesignDesigner.IsDesignMsg(Sender: TControl; var Msg: TMessage): Boolean;
begin
  Result := Messenger.IsDesignMessage(Sender, Msg);
end;

{$IFDEF RTL240_UP}
procedure TJvDesignDesigner.DrawSelectionMarks(AControl: TControl);
begin
  {$MESSAGE WARN 'Check and implement TJvDesignDesigner.DrawSelectionMarks if necessary'}
end;

function TJvDesignDesigner.IsSelected(AControl: TControl): Boolean;
begin
  {$MESSAGE WARN 'Check and implement TJvDesignDesigner.IsSelected if necessary'}
  Result := False;
end;
{$ENDIF RTL240_UP}

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

//=== { TJvDesignDesignerMessenger } =========================================

constructor TJvDesignDesignerMessenger.Create;
begin
  FDesigner := TJvDesignDesigner.Create(Self);
end;

destructor TJvDesignDesignerMessenger.Destroy;
begin
  if Container <> nil then
    DesignChildren(Container, False);
  if FDesignedForm <> nil then
    FDesignedForm.Designer := nil;
  FDesigner.Free;
  inherited Destroy;
end;

type
  TAccessComponent = class(TComponent);

procedure TJvDesignDesignerMessenger.SetComponentDesigning(AComponent: TComponent; ADesigning: Boolean);
begin
  TAccessComponent(AComponent).SetDesigning(ADesigning);
end;

procedure TJvDesignDesignerMessenger.UndesignComponent(AComponent: TComponent);
begin
  SetComponentDesigning(AComponent, False);
end;

procedure TJvDesignDesignerMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  SetComponentDesigning(AComponent, ADesigning);
end;

procedure TJvDesignDesignerMessenger.SetContainer(AValue: TWinControl);

  function FindParentForm: TCustomForm;
  var
    P: TWinControl;
  begin
    P := Container;
    while P.Parent <> nil do
      P := P.Parent;
    if not (P is TCustomForm) then
      raise EJVCLException.CreateResFmt(@RsEOldestFmt , [ClassName]);
    Result := TCustomForm(P);
  end;

begin
  inherited SetContainer(AValue);
  if Container <> nil then
  begin
    FDesignedForm := FindParentForm;
    FDesignedForm.Designer := FDesigner;
    DesignChildren(Container, True);
  end;
end;

//=== { TJvDesignMessageHookList } ===========================================

constructor TJvDesignMessageHookList.Create(AUser: TJvDesignCustomMessenger);
begin
  inherited Create(nil);
  FUser := AUser;
  FHooks := TObjectList.Create;
  FHooks.OwnsObjects := True;
end;

destructor TJvDesignMessageHookList.Destroy;
begin
  FHooks.Free;
  inherited Destroy;
end;

procedure TJvDesignMessageHookList.Clear;
begin
  FHooks.Clear;
end;

procedure TJvDesignMessageHookList.Hook(AClient: TWinControl);
begin
  AClient.FreeNotification(Self);
  FHooks.Add(TJvDesignMessageHook.Create(FUser, AClient));
end;

procedure TJvDesignMessageHookList.Unhook(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to FHooks.Count - 1 do
    if TJvDesignMessageHook(FHooks[I]).Client = AComponent then
    begin
      FHooks.Delete(I);
      Break;
    end;
end;

procedure TJvDesignMessageHookList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    Unhook(AComponent);
end;

//=== { TJvDesignWinControlHookMessenger } ===================================

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

procedure TJvDesignWinControlHookMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  if (AComponent is TWinControl) then
    if ADesigning then
      HookWinControl(TWinControl(AComponent))
    else
      UnhookWinControl(TWinControl(AComponent))
end;

procedure TJvDesignWinControlHookMessenger.HookWinControl(AWinControl: TWinControl);
begin
  FHooks.Hook(AWinControl);
  DesignChildren(AWinControl, True);
end;

procedure TJvDesignWinControlHookMessenger.UnhookWinControl(AWinControl: TWinControl);
begin
  FHooks.Unhook(AWinControl);
  DesignChildren(AWinControl, False);
end;

procedure TJvDesignWinControlHookMessenger.SetContainer(AValue: TWinControl);
begin
  inherited SetContainer(AValue);
  if Container <> nil then
    DesignChildren(Container, True);
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
