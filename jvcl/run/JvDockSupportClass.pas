{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockSupportClass.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-12-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDockSupportClass;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms;

type
  TJvDockBaseTree = class;

  TJvDockBaseZone = class(TObject)
  private
    FBaseTree: TJvDockBaseTree;
    FChildZone: TJvDockBaseZone;
    FNextSibling: TJvDockBaseZone;
    FPrevSibling: TJvDockBaseZone;
    FParentZone: TJvDockBaseZone;
  protected
    function GetNextSibingCount: Integer;
    function GetPrevSibingCount: Integer;
  public
    constructor Create(BaseTree: TJvDockBaseTree); virtual;
    destructor Destroy; override;

    function CreateChildZone: TJvDockBaseZone;
    function GetParentZone: TJvDockBaseZone; virtual;
    function GetChildCount: Integer;
    function GetChildZone(Index: Word): TJvDockBaseZone;
    property BaseTree: TJvDockBaseTree read FBaseTree write FBaseTree;
    property ChildZone: TJvDockBaseZone read FChildZone write FChildZone;
    property NextSibling: TJvDockBaseZone read FNextSibling write FNextSibling;
    property PrevSibling: TJvDockBaseZone read FPrevSibling write FPrevSibling;
    property ParentZone: TJvDockBaseZone read FParentZone write FParentZone;
  end;

  TJvDockScanZoneNotification = (snNone, snAdded, snExtracted, snDeleted);

  TJvDockTreeZoneClass = class of TJvDockBaseZone;

  TJvDockScanTreeZoneProc = procedure(TreeZone: TJvDockBaseZone);
  
  TJvDockBaseTree = class(TObject)
  private
    FScanAction: TJvDockScanZoneNotification;
    FTreeZoneClass: TJvDockTreeZoneClass;
    FTopTreeZone: TJvDockBaseZone;        
    FCurrTreeZone: TJvDockBaseZone;       
    FScanZoneProc: TJvDockScanTreeZoneProc; 
  protected
    procedure ForwardScanTree(TreeZone: TJvDockBaseZone); virtual; 
    procedure BackwardScanTree(TreeZone: TJvDockBaseZone); virtual;
    procedure MiddleScanTree(TreeZone: TJvDockBaseZone); virtual;  
    procedure ScanTreeZone(TreeZone: TJvDockBaseZone); virtual;
  public
    constructor Create(TreeZone: TJvDockTreeZoneClass); virtual;
    destructor Destroy; override;
    function AddChildZone(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone; virtual;
    function AddNextSibling(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone; virtual;
    function AddPrevSibling(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone; virtual;
    function AddParentZone(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone; virtual;
    procedure RemoveChildZone(TreeZone: TJvDockBaseZone); virtual;
    procedure RemoveNextSibling(TreeZone: TJvDockBaseZone); virtual;
    procedure RemovePrevSibling(TreeZone: TJvDockBaseZone); virtual;
    procedure RemoveParentZone(TreeZone: TJvDockBaseZone); virtual;
    property TreeZoneClass: TJvDockTreeZoneClass read FTreeZoneClass write FTreeZoneClass;
    property TopTreeZone: TJvDockBaseZone read FTopTreeZone write FTopTreeZone;
    property CurrTreeZone: TJvDockBaseZone read FCurrTreeZone write FCurrTreeZone;
    property ScanZoneProc: TJvDockScanTreeZoneProc read FScanZoneProc write FScanZoneProc;
  end;

  TJvDockBaseGetFormEventComponent = class(TComponent)
  private
    FOldOnActivate: TNotifyEvent;
    FOldOnClose: TCloseEvent;
    FOldOnCloseQuery: TCloseQueryEvent;
    FOldOnCreate: TNotifyEvent;
    FOldOnDeactivate: TNotifyEvent;
    FOldOnDestroy: TNotifyEvent;
    FOldOnHelp: THelpEvent;
    FOldOnHide: TNotifyEvent;
    FOldOnPaint: TNotifyEvent;
    FOldOnShortCut: TShortCutEvent;
    FOldOnShow: TNotifyEvent;
    FOldOnDockDrop: TDockDropEvent;
    FOldOnDockOver: TDockOverEvent;
    FOldOnExit: TNotifyEvent;
    FOldOnGetSiteInfo: TGetSiteInfoEvent;
    FOldOnKeyDown: TKeyEvent;
    FOldOnKeyPress: TKeyPressEvent;
    FOldOnKeyUp: TKeyEvent;
    FOldOnMouseWheel: TMouseWheelEvent;
    FOldOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOldOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOldOnUndock: TUnDockEvent;
    FOldOnCanResize: TCanResizeEvent;
    FOldOnClick: TNotifyEvent;
    FOldOnConstrainedResize: TConstrainedResizeEvent;
    FOldOnContextPopup: TContextPopupEvent;
    FOldOnDblClick: TNotifyEvent;
    FOldOnDragDrop: TDragDropEvent;
    FOldOnDragOver: TDragOverEvent;
    FOldOnEndDock: TEndDragEvent;
    FOldOnEndDrag: TEndDragEvent;
    FOldOnMouseDown: TMouseEvent;
    FOldOnMouseMove: TMouseMoveEvent;
    FOldOnMouseUp: TMouseEvent;
    FOldOnResize: TNotifyEvent;
    FOldOnStartDock: TStartDockEvent;
    FParentForm: TForm;
    FOldWindowProc: TWndMethod;
  protected
    procedure DoFormOnActivate(Sender: TObject); virtual;
    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure DoFormOnCloseQuery(Sender: TObject; var CanClose: Boolean); virtual;
    procedure DoFormOnCreate(Sender: TObject); virtual;
    procedure DoFormOnDeactivate(Sender: TObject); virtual;
    procedure DoFormOnDestroy(Sender: TObject); virtual;
    function DoFormOnHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
    procedure DoFormOnHide(Sender: TObject); virtual;
    procedure DoFormOnPaint(Sender: TObject); virtual;
    procedure DoFormOnShortCut(var Msg: TWMKey; var Handled: Boolean); virtual;
    procedure DoFormOnShow(Sender: TObject); virtual;
    procedure DoFormOnDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer); virtual;
    procedure DoFormOnDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean); virtual;
    procedure DoFormOnExit(Sender: TObject); virtual;
    procedure DoFormOnGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    procedure DoFormOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure DoFormOnKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure DoFormOnKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure DoFormOnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoFormOnMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoFormOnMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoFormOnUndock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean); virtual;
    procedure DoFormOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean); virtual;
    procedure DoFormOnClick(Sender: TObject); virtual;
    procedure DoFormOnConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer); virtual;
    procedure DoFormOnContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean); virtual;
    procedure DoFormOnDblClick(Sender: TObject); virtual;
    procedure DoFormOnDragDrop(Sender, Source: TObject;
      X, Y: Integer); virtual;
    procedure DoFormOnDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure DoFormOnEndDock(Sender, Target: TObject;
      X, Y: Integer); virtual;
    procedure DoFormOnEndDrag(Sender, Target: TObject;
      X, Y: Integer); virtual;
    procedure DoFormOnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoFormOnMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure DoFormOnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoFormOnResize(Sender: TObject); virtual;
    procedure DoFormOnStartDock(Sender: TObject;
      var DragObject: TDragDockObject); virtual;
    procedure WindowProc(var Msg: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ParentForm: TForm read FParentForm;
  end;

implementation

//=== { TJvDockBaseZone } ====================================================

constructor TJvDockBaseZone.Create(BaseTree: TJvDockBaseTree);
begin
  // (rom) added inherited Create
  inherited Create;
  FBaseTree := BaseTree;
  FChildZone := nil;
  FNextSibling := nil;
  FPrevSibling := nil;
  FParentZone := nil;
end;

function TJvDockBaseZone.CreateChildZone: TJvDockBaseZone;
begin
  Result := nil;
end;

destructor TJvDockBaseZone.Destroy;
begin
  FBaseTree := nil;
  FChildZone := nil;
  FNextSibling := nil;
  FPrevSibling := nil;
  FParentZone := nil;
  inherited;
end;

function TJvDockBaseZone.GetChildCount: Integer;
var
  Zone: TJvDockBaseZone;
begin
  Result := 0;
  if FChildZone <> nil then
  begin
    Inc(Result);
    Zone := FChildZone;
    while Zone.NextSibling <> nil do
    begin
      Zone := Zone.NextSibling;
      Inc(Result);
    end;
  end;
end;

function TJvDockBaseZone.GetChildZone(Index: Word): TJvDockBaseZone;
begin
  Result := FChildZone;
  while (Index > 0) and (Result <> nil) do
  begin
    Result := Result.NextSibling;
    Dec(Index);
  end;
end;

function TJvDockBaseZone.GetNextSibingCount: Integer;
var
  Zone: TJvDockBaseZone;
begin
  Result := 0;
  Zone := Self;
  while Zone.NextSibling <> nil do
  begin
    Zone := Zone.NextSibling;
    Inc(Result);
  end;
end;

function TJvDockBaseZone.GetParentZone: TJvDockBaseZone;
var
  TreeZone: TJvDockBaseZone;
begin
  TreeZone := Self;
  while (TreeZone <> nil) and (TreeZone.ParentZone = nil) and
    (TreeZone.PrevSibling <> nil) do
    TreeZone := TreeZone.PrevSibling;
  if TreeZone <> nil then
    Result := TreeZone.ParentZone
  else
    Result := nil;
end;

function TJvDockBaseZone.GetPrevSibingCount: Integer;
var
  Zone: TJvDockBaseZone;
begin
  Result := 0;
  Zone := Self;
  while Zone.PrevSibling <> nil do
  begin
    Zone := Zone.PrevSibling;
    Inc(Result);
  end;
end;

//=== { TJvDockBaseTree } ====================================================

constructor TJvDockBaseTree.Create(TreeZone: TJvDockTreeZoneClass);
begin
  // (rom) added inherited Create
  inherited Create;
  FTreeZoneClass := TreeZone;
  FTopTreeZone := FTreeZoneClass.Create(Self);
  FCurrTreeZone := FTopTreeZone;
  FScanZoneProc := nil;
  FScanAction := snNone;
end;

destructor TJvDockBaseTree.Destroy;
begin
  FScanAction := snDeleted;
  BackwardScanTree(TopTreeZone);
  FScanAction := snNone;
  inherited Destroy;
end;

function TJvDockBaseTree.AddChildZone(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone;
begin
  if TreeZone.ChildZone <> nil then
    Result := AddNextSibling(TreeZone.ChildZone, NewZone)
  else
  begin
    if NewZone = nil then
      Result := FTreeZoneClass.Create(Self)
    else
      Result := NewZone;
    TreeZone.ChildZone := Result;
    Result.ParentZone := TreeZone;
  end;
end;

function TJvDockBaseTree.AddNextSibling(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone;
begin
  while TreeZone.NextSibling <> nil do
    TreeZone := TreeZone.NextSibling;
  if NewZone = nil then
    Result := FTreeZoneClass.Create(Self)
  else
    Result := NewZone;
  TreeZone.NextSibling := Result;
  Result.PrevSibling := TreeZone;
  Result.ParentZone := TreeZone.ParentZone;
end;

function TJvDockBaseTree.AddParentZone(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone;
begin
  if NewZone = nil then
    Result := FTreeZoneClass.Create(Self)
  else
    Result := NewZone;
  while TreeZone.PrevSibling <> nil do
    TreeZone := TreeZone.PrevSibling;
  if TreeZone.ParentZone <> nil then
    TreeZone.ParentZone.ChildZone := Result
  else
    TopTreeZone := Result;
  Result.ParentZone := TreeZone.ParentZone;
  TreeZone.ParentZone := Result;
end;

function TJvDockBaseTree.AddPrevSibling(TreeZone, NewZone: TJvDockBaseZone): TJvDockBaseZone;
begin
  if NewZone = nil then
    Result := FTreeZoneClass.Create(Self)
  else
    Result := NewZone;
  if TreeZone.PrevSibling <> nil then
  begin
    TreeZone.PrevSibling.NextSibling := Result;
    Result.PrevSibling := TreeZone.PrevSibling;
    TreeZone.PrevSibling := Result;
    Result.NextSibling := TreeZone;
    Result.ParentZone := TreeZone.ParentZone;
  end
  else
  begin
    if TreeZone.ParentZone <> nil then
      TreeZone.ParentZone.ChildZone := Result
    else
      TopTreeZone := Result;
    Result.ParentZone := TreeZone.ParentZone;
    Result.NextSibling := TreeZone;
    TreeZone.PrevSibling := Result;
  end;
end;

procedure TJvDockBaseTree.ForwardScanTree(TreeZone: TJvDockBaseZone);
begin
  if TreeZone <> nil then
  begin
    ScanTreeZone(TreeZone);
    ForwardScanTree(TreeZone.ChildZone);
    ForwardScanTree(TreeZone.NextSibling);
  end;
end;

procedure TJvDockBaseTree.MiddleScanTree(TreeZone: TJvDockBaseZone);
begin
  if TreeZone <> nil then
  begin
    MiddleScanTree(TreeZone.ChildZone);
    ScanTreeZone(TreeZone);
    MiddleScanTree(TreeZone.NextSibling);
  end;
end;

procedure TJvDockBaseTree.BackwardScanTree(TreeZone: TJvDockBaseZone);
begin
  if TreeZone <> nil then
  begin
    BackwardScanTree(TreeZone.ChildZone);
    BackwardScanTree(TreeZone.NextSibling);
    ScanTreeZone(TreeZone);
  end;
end;

procedure TJvDockBaseTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
begin
  if Assigned(FScanZoneProc) then
    FScanZoneProc(TreeZone);
  if FScanAction = snDeleted then
    TreeZone.Free;
end;

procedure TJvDockBaseTree.RemoveChildZone(TreeZone: TJvDockBaseZone);
begin
  if TreeZone.ChildZone <> nil then
  begin
    FScanAction := snDeleted;
    BackwardScanTree(TreeZone.ChildZone);
    FScanAction := snNone;
  end;
end;

procedure TJvDockBaseTree.RemoveNextSibling(TreeZone: TJvDockBaseZone);
begin
  if TreeZone.NextSibling <> nil then
  begin
    FScanAction := snDeleted;
    BackwardScanTree(TreeZone.NextSibling);
    FScanAction := snNone;
  end;
end;

procedure TJvDockBaseTree.RemoveParentZone(TreeZone: TJvDockBaseZone);
begin
end;

procedure TJvDockBaseTree.RemovePrevSibling(TreeZone: TJvDockBaseZone);
begin
  if TreeZone.PrevSibling <> nil then
  begin
    FScanAction := snDeleted;
    BackwardScanTree(TreeZone.PrevSibling);
    FScanAction := snNone;
  end;
end;

//=== { TJvDockBaseGetFormEventComponent } ===================================

constructor TJvDockBaseGetFormEventComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentForm := TForm(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    FOldOnActivate := FParentForm.OnActivate;
    FParentForm.OnActivate := DoFormOnActivate;
    FOldOnClose := FParentForm.OnClose;
    FParentForm.OnClose := DoFormOnClose;
    FOldOnCloseQuery := FParentForm.OnCloseQuery;
    FParentForm.OnCloseQuery := DoFormOnCloseQuery;
    FOldOnCreate := FParentForm.OnCreate;
    FParentForm.OnCreate := DoFormOnCreate;
    FOldOnDeactivate := FParentForm.OnDeactivate;
    FParentForm.OnDeactivate := DoFormOnDeactivate;
    FOldOnDestroy := FParentForm.OnDestroy;
    FParentForm.OnDestroy := DoFormOnDestroy;
    FOldOnHelp := FParentForm.OnHelp;
    FParentForm.OnHelp := DoFormOnHelp;
    FOldOnHide := FParentForm.OnHide;
    FParentForm.OnHide := DoFormOnHide;
    FOldOnPaint := FParentForm.OnPaint;
    FParentForm.OnPaint := DoFormOnPaint;
    FOldOnShortCut := FParentForm.OnShortCut;
    FParentForm.OnShortCut := DoFormOnShortCut;
    FOldOnShow := FParentForm.OnShow;
    FParentForm.OnShow := DoFormOnShow;
    FOldOnDockDrop := FParentForm.OnDockDrop;
    FParentForm.OnDockDrop := DoFormOnDockDrop;
    FOldOnDockOver := FParentForm.OnDockOver;
    FParentForm.OnDockOver := DoFormOnDockOver;
    FOldOnGetSiteInfo := FParentForm.OnGetSiteInfo;
    FParentForm.OnGetSiteInfo := DoFormOnGetSiteInfo;
    FOldOnKeyDown := FParentForm.OnKeyDown;
    FParentForm.OnKeyDown := DoFormOnKeyDown;
    FOldOnKeyPress := FParentForm.OnKeyPress;
    FParentForm.OnKeyPress := DoFormOnKeyPress;
    FOldOnKeyUp := FParentForm.OnKeyUp;
    FParentForm.OnKeyUp := DoFormOnKeyUp;
    FOldOnMouseWheel := FParentForm.OnMouseWheel;
    FParentForm.OnMouseWheel := DoFormOnMouseWheel;
    FOldOnMouseWheelDown := FParentForm.OnMouseWheelDown;
    FParentForm.OnMouseWheelDown := DoFormOnMouseWheelDown;
    FOldOnMouseWheelUp := FParentForm.OnMouseWheelUp;
    FParentForm.OnMouseWheelUp := DoFormOnMouseWheelUp;
    FOldOnUndock := FParentForm.OnUnDock;
    FParentForm.OnUnDock := DoFormOnUnDock;
    FOldOnCanResize := FParentForm.OnCanResize;
    FParentForm.OnCanResize := DoFormOnCanResize;
    FOldOnClick := FParentForm.OnClick;
    FParentForm.OnClick := DoFormOnClick;
    FOldOnConstrainedResize := FParentForm.OnConstrainedResize;
    FParentForm.OnConstrainedResize := DoFormOnConstrainedResize;
    FOldOnContextPopup := FParentForm.OnContextPopup;
    FParentForm.OnContextPopup := DoFormOnContextPopup;
    FOldOnDblClick := FParentForm.OnDblClick;
    FParentForm.OnDblClick := DoFormOnDblClick;
    FOldOnDragDrop := FParentForm.OnDragDrop;
    FParentForm.OnDragDrop := DoFormOnDragDrop;
    FOldOnDragOver := FParentForm.OnDragOver;
    FParentForm.OnDragOver := DoFormOnDragOver;
    FOldOnEndDock := FParentForm.OnEndDock;
    FParentForm.OnEndDock := DoFormOnEndDock;
    FOldOnMouseDown := FParentForm.OnMouseDown;
    FParentForm.OnMouseDown := DoFormOnMouseDown;
    FOldOnMouseMove := FParentForm.OnMouseMove;
    FParentForm.OnMouseMove := DoFormOnMouseMove;
    FOldOnMouseUp := FParentForm.OnMouseUp;
    FParentForm.OnMouseUp := DoFormOnMouseUp;
    FOldOnResize := FParentForm.OnResize;
    FParentForm.OnResize := DoFormOnResize;
    FOldOnStartDock := FParentForm.OnStartDock;
    FParentForm.OnStartDock := DoFormOnStartDock;
    FOldWindowProc := FParentForm.WindowProc;
    FParentForm.WindowProc := WindowProc;
  end;
end;

destructor TJvDockBaseGetFormEventComponent.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;

    FParentForm.OnActivate := FOldOnActivate;
    FOldOnActivate := nil;
    FParentForm.OnClose := FOldOnClose;
    FOldOnClose := nil;
    FParentForm.OnCloseQuery := FOldOnCloseQuery;
    FOldOnCloseQuery := nil;
    FParentForm.OnCreate := FOldOnCreate;
    FOldOnCreate := nil;
    FParentForm.OnDeactivate := FOldOnDeactivate;
    FOldOnDeactivate := nil;
    FParentForm.OnDestroy := FOldOnDestroy;
    FOldOnDestroy := nil;
    FParentForm.OnHelp := FOldOnHelp;
    FOldOnHelp := nil;
    FParentForm.OnHide := FOldOnHide;
    FOldOnHide := nil;
    FParentForm.OnPaint := FOldOnPaint;
    FOldOnPaint := nil;
    FParentForm.OnShortCut := FOldOnShortCut;
    FOldOnShortCut := nil;
    FParentForm.OnShow := FOldOnShow;
    FOldOnShow := nil;
    FParentForm.OnDockDrop := FOldOnDockDrop;
    FOldOnDockDrop := nil;
    FParentForm.OnDockOver := FOldOnDockOver;
    FOldOnDockOver := nil;
    FParentForm.OnGetSiteInfo := FOldOnGetSiteInfo;
    FOldOnGetSiteInfo := nil;
    FParentForm.OnKeyDown := FOldOnKeyDown;
    FOldOnKeyDown := nil;
    FParentForm.OnKeyPress := FOldOnKeyPress;
    FOldOnKeyPress := nil;
    FParentForm.OnKeyUp := FOldOnKeyUp;
    FOldOnKeyUp := nil;
    FParentForm.OnMouseWheel := FOldOnMouseWheel;
    FOldOnMouseWheel := nil;
    FParentForm.OnMouseWheelDown := FOldOnMouseWheelDown;
    FOldOnMouseWheelDown := nil;
    FParentForm.OnMouseWheelUp := FOldOnMouseWheelUp;
    FOldOnMouseWheelUp := nil;
    FParentForm.OnUndock := FOldOnUndock;
    FOldOnUndock := nil;
    FParentForm.OnCanResize := FOldOnCanResize;
    FOldOnCanResize := nil;
    FParentForm.OnClick := FOldOnClick;
    FOldOnClick := nil;
    FParentForm.OnConstrainedResize := FOldOnConstrainedResize;
    FOldOnConstrainedResize := nil;
    FParentForm.OnContextPopup := FOldOnContextPopup;
    FOldOnContextPopup := nil;
    FParentForm.OnDblClick := FOldOnDblClick;
    FOldOnDblClick := nil;
    FParentForm.OnDragDrop := FOldOnDragDrop;
    FOldOnDragDrop := nil;
    FParentForm.OnDragOver := FOldOnDragOver;
    FOldOnDragOver := nil;
    FParentForm.OnEndDock := FOldOnEndDock;
    FOldOnEndDock := nil;
    FParentForm.OnMouseDown := FOldOnMouseDown;
    FOldOnMouseDown := nil;
    FParentForm.OnMouseMove := FOldOnMouseMove;
    FOldOnMouseMove := nil;
    FParentForm.OnMouseUp := FOldOnMouseUp;
    FOldOnMouseUp := nil;
    FParentForm.OnResize := FOldOnResize;
    FOldOnResize := nil;
    FParentForm.OnStartDock := FOldOnStartDock;
    FOldOnStartDock := nil;
    FParentForm := nil;
  end;
  inherited Destroy;
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnActivate(Sender: TObject);
begin
  if Assigned(FOldOnActivate) then
    FOldOnActivate(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnCanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  if Assigned(FOldOnCanResize) then
    FOldOnCanResize(Sender, NewWidth, NewHeight, Resize);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnClick(Sender: TObject);
begin
  if Assigned(FOldOnClick) then
    FOldOnClick(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOldOnCloseQuery) then
    FOldOnCloseQuery(Sender, CanClose);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnConstrainedResize(
  Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  if Assigned(FOldOnConstrainedResize) then
    FOldOnConstrainedResize(Sender, MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOldOnContextPopup) then
    FOldOnContextPopup(Sender, MousePos, Handled);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnCreate(Sender: TObject);
begin
  if Assigned(FOldOnCreate) then
    FOldOnCreate(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDblClick(Sender: TObject);
begin
  if Assigned(FOldOnDblClick) then
    FOldOnDblClick(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDeactivate(Sender: TObject);
begin
  if Assigned(FOldOnDeactivate) then
    FOldOnDeactivate(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDestroy(Sender: TObject);
begin
  if Assigned(FOldOnDestroy) then
    FOldOnDestroy(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  if Assigned(FOldOnDockDrop) then
    FOldOnDockDrop(Sender, Source, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if Assigned(FOldOnDockOver) then
    FOldOnDockOver(Sender, Source,  X, Y, State, Accept);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if Assigned(FOldOnDragDrop) then
    FOldOnDragDrop(Sender, Source, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOldOnDragOver) then
    FOldOnDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnEndDock(Sender,
  Target: TObject; X, Y: Integer);
begin
  if Assigned(FOldOnEndDock) then
    FOldOnEndDock(Sender, Target, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnEndDrag(Sender,
  Target: TObject; X, Y: Integer);
begin
  if Assigned(FOldOnEndDrag) then
    FOldOnEndDrag(Sender, Target, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnExit(Sender: TObject);
begin
  if Assigned(FOldOnExit) then
    FOldOnExit(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  if Assigned(FOldOnGetSiteInfo) then
    FOldOnGetSiteInfo(Sender, DockClient, InfluenceRect, MousePos, CanDock);
end;

function TJvDockBaseGetFormEventComponent.DoFormOnHelp(Command: Word;
  Data: Integer; var CallHelp: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOldOnHelp) then
    Result := FOldOnHelp(Command, Data, CallHelp);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnHide(Sender: TObject);
begin
  if Assigned(FOldOnHide) then
    FOldOnHide(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOldOnKeyDown) then
    FOldOnKeyDown(Sender, Key, Shift);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Assigned(FOldOnKeyPress) then
    FOldOnKeyPress(Sender, Key);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOldOnKeyUp) then
    FOldOnKeyUp(Sender, Key, Shift);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldOnMouseDown) then
    FOldOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldOnMouseMove) then
    FOldOnMouseMove(Sender, Shift, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOldOnMouseUp) then
    FOldOnMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(FOldOnMouseWheel) then
    FOldOnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnMouseWheelDown(
  Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(FOldOnMouseWheelDown) then
    FOldOnMouseWheelDown(Sender, Shift, MousePos, Handled);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOldOnMouseWheelUp) then
    FOldOnMouseWheelUp(Sender, Shift, MousePos, Handled);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnPaint(Sender: TObject);
begin
  if Assigned(FOldOnPaint) then
    FOldOnPaint(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnResize(Sender: TObject);
begin
  if Assigned(FOldOnResize) then
    FOldOnResize(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  if Assigned(FOldOnShortCut) then
    FOldOnShortCut(Msg, Handled);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnShow(Sender: TObject);
begin
  if Assigned(FOldOnShow) then
    FOldOnShow(Sender);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  if Assigned(FOldOnStartDock) then
    FOldOnStartDock(Sender, DragObject);
end;

procedure TJvDockBaseGetFormEventComponent.DoFormOnUndock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  if Assigned(FOldOnUndock) then
    FOldOnUndock(Sender, Client, NewTarget, Allow);
end;

procedure TJvDockBaseGetFormEventComponent.WindowProc(var Msg: TMessage);
begin
  if Assigned(FOldWindowProc) then
    FOldWindowProc(Msg);
end;

end.



