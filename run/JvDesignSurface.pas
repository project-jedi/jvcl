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
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
  Mantis 3963: When a design surface is active, the ENTIRE form where it is
               located suffers impacts from being in design mode. This can not
               be circumvented because the Designer property is to be set on
               the parent form and it MUST be set for the design mode to be
               effective. The only workaround is to not have anything else
               on the form being designed.

-----------------------------------------------------------------------------}
// $Id$

unit JvDesignSurface;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  Forms, ExtCtrls;

type
  TJvDesignSurface = class;

  TJvDesignMessage = function(ASender: TControl; var AMsg: TMessage;
    const APt: TPoint): Boolean of object;

  TJvDesignCustomMessenger = class(TObject)
  private
    FContainer: TWinControl;
    FOnDesignMessage: TJvDesignMessage;
  protected
    procedure SetContainer(AValue: TWinControl); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDesignMessage(ASender: TControl; var AMessage: TMessage): Boolean; virtual;
    procedure Clear; virtual;
    procedure DesignChildren(AContainer: TWinControl; ADesigning: Boolean);
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); virtual;
    property Container: TWinControl read FContainer write SetContainer;
    property OnDesignMessage: TJvDesignMessage read FOnDesignMessage write FOnDesignMessage;
  end;

  TJvDesignCustomMessengerClass = class of TJvDesignCustomMessenger;

  TJvDesignMessageHook = class(TObject)
  private
    FClient: TWinControl;
    FOldProc: TWndMethod;
    FUser: TJvDesignCustomMessenger;
  protected
    procedure HookProc(var AMessage: TMessage);
    procedure Unhook;
  public
    constructor Create(AUser: TJvDesignCustomMessenger; AClient: TWinControl);
    destructor Destroy; override;
    property Client: TWinControl read FClient;
  end;

  TJvDesignCustomController = class(TObject)
  private
    FSurface: TJvDesignSurface;
  protected
    function GetDragRect: TRect; virtual; abstract;
    function GetShift: TShiftState;
    function KeyDown(AKeyCode: Cardinal): Boolean; virtual; abstract;
    function KeyUp(AKeyCode: Cardinal): Boolean; virtual; abstract;
    function MouseDown(Button: TMouseButton; X, Y: Integer): Boolean; virtual; abstract;
    function MouseMove(X, Y: Integer): Boolean; virtual; abstract;
    function MouseUp(Button: TMouseButton; X, Y: Integer): Boolean; virtual; abstract;
  public
    constructor Create(ASurface: TJvDesignSurface); virtual;
    property DragRect: TRect read GetDragRect;
    property Shift: TShiftState read GetShift;
    property Surface: TJvDesignSurface read FSurface;
  end;

  TJvDesignCustomControllerClass = class of TJvDesignCustomController;

  TJvDesignHandleId = (dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
    dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom);

  TJvDesignCustomSelector = class(TComponent)
  private
    FSurface: TJvDesignSurface;
  protected
    function GetCount: Integer; virtual; abstract;
    function GetSelection(AIndex: Integer): TControl;  virtual; abstract;
    procedure SetSelection(AIndex: Integer; AValue: TControl); virtual; abstract;
  public
    constructor Create(ASurface: TJvDesignSurface); reintroduce; virtual;
    destructor Destroy; override;
    function IsSelected(AValue: TControl): Boolean; virtual; abstract;
    function GetClientControl(AControl: TControl): TControl; virtual; abstract;
    function GetCursor(AX, AY: Integer): TCursor; virtual; abstract;
    function GetHitHandle(AX, AY: Integer): TJvDesignHandleId; virtual; abstract;
    procedure AddToSelection(AValue: TControl); virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure RemoveFromSelection(AValue: TControl); virtual; abstract;
    procedure ToggleSelection(AValue: TControl);
    procedure Update; virtual; abstract;
    property Count: Integer read GetCount;
    property Selection[AIndex: Integer]: TControl read GetSelection write SetSelection;
    property Surface: TJvDesignSurface read FSurface;
  end;

  TJvDesignCustomSelectorClass = class of TJvDesignCustomSelector;

  TJvDesignObjectArray = array of TObject;
  TJvDesignGetAddClassEvent = procedure(Sender: TObject; var ioClass: string) of object;
{
  TJvDesignOwnerDrawGridEvent = procedure(ASender: TObject; ACanvas: TCanvas;
    ARect: TRect) of object;
}

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
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
    function GetSelection(AIndex: Integer): TControl;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedContainer;
    procedure NeedController;
    procedure NeedMessenger;
    procedure NeedSelector;
    //procedure PaintContainerBkgnd(ADC: HDC);
    procedure ReaderError(Reader: TReader; const Msg: string; var Handled: Boolean);
    procedure SetActive(AValue: Boolean);
    procedure SetContainer(AValue: TWinControl);
    //procedure SetDrawGrid(const Value: Boolean);
    procedure SetSelection(AIndex: Integer; AValue: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clear: TJvDesignSurface;
    function ContainerToSelectedContainer(const APt: TPoint): TPoint;
    function FindControl(AX, AY: Integer): TControl; virtual;
    function GetCursor(AX, AY: Integer): TCursor; virtual;
    function GetHitHandle(AX, AY: Integer): TJvDesignHandleId; virtual;
    function IsDesignMessage(ASender: TControl; var AMsg: TMessage; const APt: TPoint): Boolean;
    function LoadFromFile(const AFileName: string): TJvDesignSurface;
    function LoadFromStream(AStream: TStream): TJvDesignSurface;
    procedure AddComponent;
    procedure Change;
    procedure ClearSelection;
    procedure CopyComponents;
    procedure CutComponents;
    procedure DeleteComponents;
    procedure GetAddClass;
    procedure GrowComponents(AGrowWidth, AGrowHeight: Integer);
    procedure NudgeComponents(ANudgeLeft, ANudgeTop: Integer);
    procedure PasteComponents;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure Select(AControl: TControl);
    procedure SelectionChange;
    procedure SelectParent;
    procedure SetSelected(const AValue: array of TObject);
    procedure UpdateDesigner; virtual;
    property Active: Boolean read FActive write SetActive;
    property AddClass: string read FAddClass write FAddClass;
    property Controller: TJvDesignCustomController read FController;
    property ControllerClass: TJvDesignCustomControllerClass read FControllerClass write FControllerClass;
    property Count: Integer read GetCount;
    property Messenger: TJvDesignCustomMessenger read FMessenger;
    property MessengerClass: TJvDesignCustomMessengerClass read FMessengerClass write FMessengerClass;
    property Selected: TJvDesignObjectArray read GetSelected;
    property SelectedContainer: TWinControl read GetSelectedContainer;
    property Selection[AIndex: Integer]: TControl read GetSelection write SetSelection;
    property Selector: TJvDesignCustomSelector read FSelector;
    property SelectorClass: TJvDesignCustomSelectorClass read FSelectorClass write FSelectorClass;
  published
    property Container: TWinControl read FContainer write SetContainer;
//    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent read FOnGetAddClass write FOnGetAddClass;
//    property OnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent read FOnOwnerDrawGrid write FOnOwnerDrawGrid;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDesignScrollBox = class(TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
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
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure Paint; override;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SetDrawRules(const Value: Boolean);
    property Active: Boolean read GetActive write SetActive;
    property Canvas;
    property Surface: TJvDesignSurface read FSurface;
  published
    property DrawRules: Boolean read FDrawRules write SetDrawRules default True;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent read GetOnGetAddClass write SetOnGetAddClass;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
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
  JvDesignUtils, JvDesignClip, JvDesignImp, JvResources, JvTypes;

//=== { TJvDesignCustomMessenger } ===========================================

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

procedure TJvDesignCustomMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  //
end;

procedure TJvDesignCustomMessenger.DesignChildren(AContainer: TWinControl; ADesigning: Boolean);
var
  I: Integer;
begin
  for I := 0 to AContainer.ControlCount - 1 do
    DesignComponent(AContainer.Controls[I], ADesigning);
end;

procedure TJvDesignCustomMessenger.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

function TJvDesignCustomMessenger.IsDesignMessage(ASender: TControl;
  var AMessage: TMessage): Boolean;

  function MousePoint: TPoint;
  begin
    with TWMMouse(AMessage) do
      MousePoint := Point(XPos, YPos);
    Result := DesignClientToParent(Result, ASender, Container);
  end;

begin
  if not Assigned(FOnDesignMessage) then
    Result := False
  else
    case AMessage.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        Result := FOnDesignMessage(ASender, AMessage, MousePoint);
      WM_KEYDOWN..WM_KEYUP, WM_PAINT, WM_ERASEBKGND, WM_WINDOWPOSCHANGED, CN_KEYDOWN..CN_KEYUP:
        Result := FOnDesignMessage(ASender, AMessage, Point(0, 0));
      else
        Result := False;
    end;
end;

//=== { TJvDesignMessageHook } ===============================================

constructor TJvDesignMessageHook.Create(AUser: TJvDesignCustomMessenger;
  AClient: TWinControl);
begin
  FUser := AUser;
  FClient := AClient;
  FOldProc := FClient.WindowProc;
  FClient.WindowProc := HookProc;
end;

destructor TJvDesignMessageHook.Destroy;
begin
  Unhook;
  inherited Destroy;
end;

procedure TJvDesignMessageHook.Unhook;
begin
  FClient.WindowProc := FOldProc;
end;

procedure TJvDesignMessageHook.HookProc(var AMessage: TMessage);
begin
  if not FUser.IsDesignMessage(FClient, AMessage) then
    FOldProc(AMessage);
end;

//=== { TJvDesignCustomController } ==========================================

constructor TJvDesignCustomController.Create(ASurface: TJvDesignSurface);
begin
  FSurface := ASurface;
end;

function TJvDesignCustomController.GetShift: TShiftState;
begin
  Result := KeyboardStateToShiftState;
end;

//=== { TJvDesignCustomSelector } ============================================

constructor TJvDesignCustomSelector.Create(ASurface: TJvDesignSurface);
begin
  inherited Create(nil);
  FSurface := ASurface;
end;

destructor TJvDesignCustomSelector.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDesignCustomSelector.ToggleSelection(AValue: TControl);
begin
  if IsSelected(AValue) then
    RemoveFromSelection(AValue)
  else
    AddToSelection(AValue);
end;

//=== { TJvDesignSurface } ===================================================

constructor TJvDesignSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessengerClass := TJvDesignDesignerMessenger;
  FControllerClass := TJvDesignController;
  FSelectorClass := TJvDesignSelector;
  //FDrawGrid := True;
end;

destructor TJvDesignSurface.Destroy;
begin
  FContainerHook.Free;
  Messenger.Free;
  Controller.Free;
  Selector.Free;
  inherited Destroy;
end;

procedure TJvDesignSurface.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDesignSurface.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

procedure TJvDesignSurface.NeedContainer;
begin
  if (Container = nil) and (Owner is TWinControl) then
    Container := TWinControl(Owner);
  if Container = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Container']);
end;

procedure TJvDesignSurface.NeedController;
begin
  if (Controller = nil) and (ControllerClass <> nil) then
    FController := ControllerClass.Create(Self);
  if Controller = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Controller']);
end;

procedure TJvDesignSurface.NeedMessenger;
begin
  if (Messenger = nil) and (MessengerClass <> nil) then
  begin
    FMessenger := MessengerClass.Create;
    Messenger.OnDesignMessage := IsDesignMessage;
  end;
  if Messenger = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Messenger']);
end;

procedure TJvDesignSurface.NeedSelector;
begin
  if (Selector = nil) and (SelectorClass <> nil) then
    FSelector := SelectorClass.Create(Self);
  if Selector = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Selector']);
end;

procedure TJvDesignSurface.SetActive(AValue: Boolean);

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
  if FActive <> AValue then
  begin
    if AValue then
      Activate
    else
      Deactivate;
    FActive := AValue;
    SelectionChange;
    if Assigned(Container) then
      Container.Invalidate;
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

function TJvDesignSurface.GetSelection(AIndex: Integer): TControl;
begin
  Result := Selector.Selection[AIndex];
end;

procedure TJvDesignSurface.SetSelection(AIndex: Integer; AValue: TControl);
begin
  Selector.Selection[AIndex] := AValue;
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
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Selector.Selection[I];
end;

procedure TJvDesignSurface.SetSelected(const AValue: array of TObject);
var
  I: Integer;
begin
  ClearSelection;
  for I := 0 to Length(AValue) - 1 do
    if AValue[I] is TControl then
      Selector.AddToSelection(TControl(AValue[I]));
end;

procedure TJvDesignSurface.Select(AControl: TControl);
begin
  ClearSelection;
  if AControl <> nil then
    Selector.AddToSelection(AControl);
end;

function TJvDesignSurface.FindControl(AX, AY: Integer): TControl;
var
  C, C0: TControl;
  P: TPoint;
begin
  P := Point(AX, AY);
  C := Container.ControlAtPos(P, True, True);
  while (C <> nil) and (C is TWinControl) do
  begin
    Dec(P.X, C.Left);
    Dec(P.Y, C.Top);
    C0 := TWinControl(C).ControlAtPos(P, True, True);
    if (C0 = nil) or (C0.Owner <> C.Owner) then
      Break;
    C := C0;
  end;
  if C = nil then
    C := Container;
  Result := Selector.GetClientControl(C);
end;

function TJvDesignSurface.GetSelectedContainer: TWinControl;
begin
  if Count <> 1 then
    Result := Container
  else
  if (Selection[0] is TWinControl) and
    (csAcceptsControls in Selection[0].ControlStyle) then
    Result := TWinControl(Selection[0])
  else
    Result := Selection[0].Parent;
end;

function TJvDesignSurface.ContainerToSelectedContainer(const APt: TPoint): TPoint;
var
  C: TControl;
begin
  Result := APt;
  C := SelectedContainer;
  while (C <> Container) and (C <> nil) do
  begin
    Dec(Result.X, C.Left);
    Dec(Result.Y, C.Top);
    C := C.Parent;
  end;
end;

function TJvDesignSurface.GetAddBounds: TRect;
begin
  with Controller do
  begin
    Result.TopLeft := ContainerToSelectedContainer(DragRect.TopLeft);
    Result.BottomRight := ContainerToSelectedContainer(DragRect.BottomRight);
  end;
end;

procedure TJvDesignSurface.GetAddClass;
begin
  if Assigned(FOnGetAddClass) then
    FOnGetAddClass(Self, FAddClass);
end;

procedure TJvDesignSurface.AddComponent;
var
  CC: TComponentClass;
  C: TComponent;
  CO: TControl;

  function GetBounds: TRect;
  begin
    Result := GetAddBounds;
    if DesignRectWidth(Result) = 0 then
      Result.Right := Result.Left + CO.Width;
    if DesignRectHeight(Result) = 0 then
      Result.Bottom := Result.Top + CO.Height;
  end;

begin
  CC := TComponentClass(GetClass(AddClass));
  if (CC <> nil) and (SelectedContainer <> nil) then
  begin
    //C := CC.Create(Owner);
    //C.Name := DesignUniqueName(Owner, AddClass);
    C := CC.Create(Container);
    C.Name := DesignUniqueName(Container, AddClass);
    if C is TControl then
    begin
      CO := TControl(C);
      CO.Parent := SelectedContainer;
      CO.BoundsRect := GetBounds;
      Select(CO);
    end;
    Messenger.DesignComponent(C, Active);
    SelectionChange;
    Change;
    AddClass := '';
  end;
end;

procedure TJvDesignSurface.NudgeComponents(ANudgeLeft, ANudgeTop: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Left := Left + ANudgeLeft;
      Top := Top + ANudgeTop;
    end;
  Change;
end;

procedure TJvDesignSurface.GrowComponents(AGrowWidth, AGrowHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Width := DesignMax(1, Width + AGrowWidth);
      Height := DesignMax(1, Height + AGrowHeight);
    end;
  Change;
end;

procedure TJvDesignSurface.DeleteComponents;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
      Selection[I].Free;
    ClearSelection;
    SelectionChange;
    Change;
  end;
end;

procedure TJvDesignSurface.CopyComponents;
var
  I: Integer;
begin
  with TJvDesignComponentClipboard.Create(Container) do
  try
    OpenWrite;
    try
      for I := 0 to Count - 1 do
        SetComponent(Selection[I]);
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
  CO: TControl;
  C: TComponent;
  P: TWinControl;

  procedure KeepInParent;
  begin
    if CO.Left > P.ClientWidth then
      CO.Left := P.ClientWidth - CO.Width;
    if CO.Top > P.ClientHeight then
      CO.Top := P.ClientHeight - CO.Height;
  end;

  procedure PasteComponent;
  begin
    C.Name := DesignUniqueName(Owner, C.ClassName);
    Owner.InsertComponent(C);
    if C is TControl then
    begin
      CO := TControl(C);
      KeepInParent;
      CO.Parent := P;
      Selector.AddToSelection(CO);
    end;
  end;

begin
  with TJvDesignComponentClipboard.Create(Container) do
  try
    OpenRead;
    try
      C := GetComponent;
      if (C <> nil) then
      begin
        P := SelectedContainer;
        ClearSelection;
        repeat
          PasteComponent;
          C := GetComponent;
        until C = nil;
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
  if Count > 0 then
    Select(Selection[0].Parent);
end;

{
procedure TJvDesignSurface.PaintContainerBkgnd(ADC: HDC);
var
  r: TRect;
  canvas: TCanvas;
begin
  if DrawGrid then
  begin
    canvas := TCanvas.Create;
    try
      SelectClipRgn(ADC, 0);
      canvas.Handle := ADC;
      canvas.Brush.Color := Container.Brush.Color;
      r := canvas.ClipRect;
      if Assigned(FOnOwnerDrawGrid) then
        FOnOwnerDrawGrid(Self, canvas, Container.ClientRect)
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

type
  TAccessWinControl = class(TWinControl);

function TJvDesignSurface.IsDesignMessage(ASender: TControl;
  var AMsg: TMessage; const APt: TPoint): Boolean;

  function VirtKey: Cardinal;
  begin
    Result := AMsg.WParam;
  end;

{
  function HandlePaint: Boolean;
  begin
    Result := False;
  end;

  function HandleEraseBkgnd: Boolean;
  begin
    if (ASender <> Container) then
      Result := False
    else begin
       PaintContainerBkgnd(TWMPaint(AMsg).DC);
       AMsg.Result := 1;
       Result := True;
    end;
  end;
}
var
  PosChangedHandle: HWND;
  I: Integer;
  Control: TAccessWinControl;
begin
  if not Active then
    Result := False
  else
    case AMsg.Msg of
{
      WM_ERASEBKGND:
        Result := HandleEraseBkgnd;
      WM_PAINT:
        Result := HandlePaint;
}
      WM_LBUTTONDOWN:
        Result := Controller.MouseDown(mbLeft, APt.X, APt.Y);
      WM_LBUTTONUP:
        Result := Controller.MouseUp(mbLeft, APt.X, APt.Y);
      WM_MOUSEMOVE:
        Result := Controller.MouseMove(APt.X, APt.Y);
      WM_KEYDOWN, CN_KEYDOWN:
        Result := Controller.KeyDown(VirtKey);
      WM_KEYUP, CN_KEYUP:
        Result := Controller.KeyUp(VirtKey);
      WM_WINDOWPOSCHANGED:
        begin
          if AMsg.lParam <> 0 then
          begin
            PosChangedHandle := PWindowPos(AMsg.lParam).hwnd;

            // If the window that has changed is a control owned by our container
            // then we must update the designer. This allows to programatically
            // change the location of a control while making the designer handles
            // follow it around (Mantis 4693).
            // For this to work properly, we MUST update the bounds of the
            // control before calling UpdateDesigner because the VCL has not yet
            // processed the WM_WINDOWPOSCHANGED message when this code executes.
            // If we did not, the designer would use the previous position of the
            // control to display the handles.
            // Additionnaly, we must not work with controls that don't have their
            // handle allocated. In some instances, creating the handle may trigger
            // a second WM_WINDOWPOSCHANGED message, thus leading to an infinite
            // loop and a crash (Mantis 5225)
            for I := 0 to Container.ComponentCount - 1 do
            begin
              if Container.Components[I] is TWinControl then
              begin
                Control := TAccessWinControl(Container.Components[I]);
                if Control.HandleAllocated and (PosChangedHandle = Control.Handle) then
                begin
                  if not (csDestroyingHandle in Control.ControlState) then
                    {$IFDEF DELPHI10_UP}
                    Control.UpdateBounds;
                    {$ELSE}
                    Control.Dispatch(AMsg);
                    {$ENDIF DELPHI10_UP}

                  UpdateDesigner;
                end;
              end;
            end;
          end;

          // Must return False to let the VCL do its own work of placing the window
          Result := False;
        end;
      else
        Result := False;
    end;
end;

function TJvDesignSurface.GetCursor(AX, AY: Integer): TCursor;
begin
  // Using FindControl is inefficient.
  // All we really want to know is if Selected[0] contains (AX, AY)
  if (Count > 0) and (FindControl(AX, AY) = Selected[0]) then
    Result := Selector.GetCursor(AX, AY)
  else
    Result := crDefault;
end;

function TJvDesignSurface.GetHitHandle(AX, AY: Integer): TJvDesignHandleId;
begin
  Result := Selector.GetHitHandle(AX, AY);
end;

procedure TJvDesignSurface.BeginUpdate;
begin
  Active := False;
  FUpdateOwner := Owner;
  Owner.RemoveComponent(Self);
end;

procedure TJvDesignSurface.EndUpdate;
begin
  FUpdateOwner.InsertComponent(Self);
  Active := True;
end;

procedure TJvDesignSurface.ReaderError(Reader: TReader; const Msg: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

function TJvDesignSurface.Clear: TJvDesignSurface;
begin
  BeginUpdate;
  try
    Container.DestroyComponents;
  finally
    EndUpdate;
  end;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToStream(AStream: TStream);
begin
  BeginUpdate;
  try
    DesignSaveComponentToStream(Container, AStream);
  finally
    EndUpdate;
  end;
end;

function TJvDesignSurface.LoadFromStream(AStream: TStream): TJvDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromStream(Container, AStream, ReaderError);
    Container.Name := SavedName;
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToFile(const AFileName: string);
begin
  BeginUpdate;
  try
    DesignSaveComponentToFile(Container, AFileName);
  finally
    EndUpdate;
  end;
end;

function TJvDesignSurface.LoadFromFile(const AFileName: string): TJvDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromFile(Container, AFileName, ReaderError);
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
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

//=== { TJvDesignScrollBox } =================================================

procedure TJvDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
  //
end;

//=== { TJvDesignPanel } =====================================================

constructor TJvDesignPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawRules := True;
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
  inherited Paint;
  if Surface.Active or (csDesigning in ComponentState) then
  begin
    if DrawRules then
      DesignPaintRules(Canvas, ClientRect);
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  end;
end;

procedure TJvDesignPanel.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from Clear.
  FSurface := Surface.Clear;
end;

procedure TJvDesignPanel.SaveToStream(AStream: TStream);
begin
  Surface.SaveToStream(AStream);
end;

procedure TJvDesignPanel.LoadFromStream(AStream: TStream);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromStream.
  FSurface := Surface.LoadFromStream(AStream);
end;

procedure TJvDesignPanel.SaveToFile(const AFileName: string);
begin
  Surface.SaveToFile(AFileName);
end;

procedure TJvDesignPanel.LoadFromFile(const AFileName: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  FSurface := Surface.LoadFromFile(AFileName);
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
  Surface.Active := Value;
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
