{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockVCStyle.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockVCStyle;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls,
  JvDockControlForm, JvDockSupportControl, JvDockTree, JvDockAdvTree;

type
  TJvDockVCConjoinServerOption = class(TJvDockBasicConjoinServerOption)
  private
    FBorderWidth: Integer;
    procedure SetBorderWidth(const Value: Integer);
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 4;
  end;

  TJvDockVCTabServerOption = class(TJvDockBasicTabServerOption);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDockVCStyle = class(TJvDockAdvStyle)
  private
    FOldEachOtherDock: Boolean;
  protected
    procedure FormGetDockEdge(DockClient: TJvDockClient; Source: TJvDockDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); override;
    procedure FormStartDock(DockClient: TJvDockClient;
      var Source: TJvDockDragDockObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CanSetEachOtherDocked(ADockBaseControl: TJvDockBaseControl): Boolean; override;
    procedure SetDockBaseControl(IsCreate: Boolean;
      DockBaseControl: TJvDockBaseControl); override;
  published
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TJvDockVCSplitter = class(TJvDockSplitter)
  private
    FOldSize: Integer;
  protected
    function DoCanResize(var NewSize: Integer): Boolean; override;
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVCPanel = class(TJvDockAdvPanel)
  protected
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function GetDockEdge(MousePos: TPoint): TAlign; override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
  end;

  TJvDockVCConjoinPanel = class(TJvDockConjoinPanel);
  TJvDockVCTabPageControl = class(TJvDockAdvTabPageControl);

  TJvDockZoneSizeStyle = (zssMinimum, zssNormal, zssMaximum);

  TJvDockVCZone = class(TJvDockAdvZone)
  private
    FZoneSizeStyle: TJvDockZoneSizeStyle;
    FExpandButtonDown: Boolean;
    procedure DoSetChildSizeStyle(ZoneSizeStyle: TJvDockZoneSizeStyle);
  public
    constructor Create(Tree: TJvDockTree); override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
    procedure InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean); override;
    procedure SetZoneSize(Size: Integer; Show: Boolean); override;
    property ZoneSizeStyle: TJvDockZoneSizeStyle read FZoneSizeStyle write FZoneSizeStyle;
    property ExpandButtonDown: Boolean read FExpandButtonDown write FExpandButtonDown;
  end;

  TJvDockVCTree = class(TJvDockAdvTree)
  private
    FExpandBtnZone: TJvDockVCZone;
  protected
    procedure WindowProc(var Msg: TMessage); override;
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); override;
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean); override;
    function GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign; override;
    function DoLButtonDown(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoOtherHint(Zone: TJvDockZone;
      HTFlag: Integer; var HintStr: string); override;
    procedure CustomSaveZone(Stream: TStream; Zone: TJvDockZone); override;
    procedure CustomLoadZone(Stream: TStream; var Zone: TJvDockZone); override;
    procedure CalcSplitterPos; override;
    function GetDropOnZone(Orient: TDockOrientation; DockRect: TRect; var DropAlign: TAlign): TJvDockZone; virtual;
    function GetDropOnControl(Orient: TDockOrientation; Zone: TJvDockZone; DockRect: TRect;
      var DropAlign: TAlign; Control: TControl): TControl; virtual;
    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; override;
    function GetLeftGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign; DropCtl: TControl); override;
    procedure InsertNewParent(NewZone, SiblingZone: TJvDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure InsertSibling(NewZone, SiblingZone: TJvDockZone;
      InsertLast, Update: Boolean); override;
    procedure DrawDockGrabber(Control: TWinControl; const ARect: TRect); override;
    procedure DrawDockSiteRect; override;
    procedure DrawSplitterRect(const ARect: TRect); override;
    procedure GetCaptionRect(var Rect: TRect); override;
    procedure RemoveControl(Control: TControl); override;
    procedure RemoveZone(Zone: TJvDockZone; Hide: Boolean = True); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure ResetDockZoneSizeStyle(Parent: TJvDockZone;
      ZoneSizeStyle: TJvDockZoneSizeStyle; Exclude: TJvDockZone);
    procedure ScaleZone(Zone: TJvDockZone); override;
    procedure ScaleChildZone(Zone: TJvDockZone); override;
    procedure ScaleSiblingZone(Zone: TJvDockZone); override;
    procedure ShiftZone(Zone: TJvDockZone); override;
    procedure SplitterMouseUp; override;
    procedure SyncWithStyle; override;
  public
    constructor Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass; ADockStyle: TJvDockObservableStyle); override;
  end;

  TJvDockVCDragDockObject = class(TJvDockDragDockObject)
  private
    FDockOverBrush: TBrush;
    FDockOverFrameWidth: Integer;
    FCurrentState: TDragState;
    FPreviousState: TDragState;
    FPreviousTarget: Pointer;
    procedure SetPreviousState(const Value: TDragState);
    procedure SetCurrentState(const Value: TDragState);
  protected
    procedure GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean); override;
    procedure SetDefaultBrushStyle; virtual;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    function DragFindWindow(const Pos: TPoint): THandle; override;
    function GetDropCtl: TControl; override;
    property CurrentState: TDragState read FCurrentState write SetCurrentState;
    property PreviousState: TDragState read FPreviousState write SetPreviousState;
    property PreviousTarget: Pointer read FPreviousTarget write FPreviousTarget;
    property DockOverFrameWidth: Integer read FDockOverFrameWidth write FDockOverFrameWidth;
    property DockOverBrush: TBrush read FDockOverBrush;
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
  {$IFDEF JVCLThemesEnabled}
  JvThemes,
  {$ENDIF JVCLThemesEnabled}
  SysUtils, ExtCtrls,
  JvDockSupportProc, JvDockGlobals;

type
  TWinControlAccessProtected = class(TWinControl);

const
  DefaultFrameWidth = 3;
  DefaultDockOverFrameWidth = 1;
  DefaultDockOverBrushStyle = bsSolid;

  ScaleMaximum = 9999;

  DropAlignArray: array [TDockOrientation, Boolean] of TAlign =
    ((alNone, alNone), (alTop, alBottom), (alLeft, alRight));

//=== { TJvDockVCConjoinServerOption } =======================================

constructor TJvDockVCConjoinServerOption.Create(ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(ADockStyle);
  FBorderWidth := 4;
end;

procedure TJvDockVCConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TJvDockVCConjoinServerOption then
  begin
    BeginUpdate;
    try
      BorderWidth := TJvDockVCConjoinServerOption(Source).FBorderWidth;
      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockVCConjoinServerOption.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

//=== { TJvDockVCDragDockObject } ============================================

constructor TJvDockVCDragDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FrameWidth := DefaultFrameWidth;

  FDockOverFrameWidth := DefaultDockOverFrameWidth;

  FDockOverBrush := TBrush.Create;
  SetDefaultBrushStyle;

  CurrentState := dsDragEnter;
  PreviousState := CurrentState;
end;

destructor TJvDockVCDragDockObject.Destroy;
begin
  FDockOverBrush.Free;
  inherited Destroy;
end;

function TJvDockVCDragDockObject.DragFindWindow(const Pos: TPoint): THandle;
begin
  Result := 0;
end;

procedure TJvDockVCDragDockObject.GetBrush_PenSize_DrawRect(var ABrush: TBrush;
  var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);
var
  DockOver: Boolean;

  procedure GetBrushAndFrameWidth;
  begin
    if DockOver then
    begin
      PenSize := FDockOverFrameWidth;
      ABrush := FDockOverBrush;
    end
    else
    begin
      PenSize := FrameWidth;
      ABrush := Brush;
    end;
  end;

begin
  DockOver :=
    ((PreviousState = dsDragEnter) and (CurrentState = dsDragMove) and (not Erase or (PreviousTarget <> nil))) or
    ((PreviousState = dsDragMove) and (CurrentState = dsDragMove)) or
    ((PreviousState = dsDragMove) and (CurrentState = dsDragLeave) and Erase);

  GetBrushAndFrameWidth;

  if (PreviousState = dsDragMove) and (CurrentState = dsDragLeave) then
  begin
    PreviousState := dsDragEnter;
    PreviousTarget := nil;
  end
  else
    PreviousTarget := DragTarget;

  if Erase then
    DrawRect := EraseDockRect
  else
    DrawRect := DockRect;
end;

function TJvDockVCDragDockObject.GetDropCtl: TControl;
begin
  Result := DropOnControl;
end;

procedure TJvDockVCDragDockObject.SetCurrentState(const Value: TDragState);
begin
  FCurrentState := Value;
end;

procedure TJvDockVCDragDockObject.SetDefaultBrushStyle;
begin
  FDockOverBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  FDockOverBrush.Style := bsSolid;
end;

procedure TJvDockVCDragDockObject.SetPreviousState(const Value: TDragState);
begin
  FPreviousState := Value;
end;

//=== { TJvDockVCPanel } =====================================================

procedure TJvDockVCPanel.CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer);
begin
  if Source is TJvDockVCDragDockObject then
  begin
    TJvDockVCDragDockObject(Source).CurrentState := dsDragEnter;
    TJvDockVCDragDockObject(Source).PreviousState := dsDragEnter;
  end;

  if Source.DropOnControl <> Source.Control then
    inherited CustomDockDrop(Source, X, Y);
end;

procedure TJvDockVCPanel.CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DropAlign: TAlign;
  VCSource: TJvDockVCDragDockObject;
  SysCaptionHeight: Integer;
  PanelScreenRect: TRect;
  R: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);

  if Source is TJvDockVCDragDockObject then
  begin
    VCSource := TJvDockVCDragDockObject(Source);
    VCSource.PreviousState := VCSource.CurrentState;
    VCSource.CurrentState := State;
  end;

  if State = dsDragMove then
  begin
    DropAlign := Source.DropAlign;
    Source.DropOnControl := JvDockManager.GetDockEdge(Source.EraseDockRect,
      Source.DragPos, DropAlign, Source.Control);
    Source.DropAlign := DropAlign;

    SysCaptionHeight := Ord(Source.Control.Floating) * JvDockGetSysCaptionHeight;

    PanelScreenRect := BoundsRect;
    MapWindowPoints(Parent.Handle, 0, PanelScreenRect, 2);

    if ((Align in [alTop, alBottom]) and
      (Source.DockRect.Right = PanelScreenRect.Right) and
      (Source.DockRect.Left = PanelScreenRect.Left)) or
      ((Align in [alLeft, alRight]) and
      (Source.DockRect.Top = PanelScreenRect.Top) and
      (Source.DockRect.Bottom = PanelScreenRect.Bottom)) then
      Exit;

    if ((Source.DropOnControl <> nil) and (Source.DropOnControl <> Source.Control)) and
      (Source.DropOnControl.HostDockSite <> Source.Control.HostDockSite) then
    begin
      if DropAlign in [alTop, alBottom] then
      begin
        if ((Source.Control.DockOrientation = doVertical) or (Source.Control.HostDockSite = nil)) then
        begin
          R := Source.DockRect;
          R.Bottom := Source.DockRect.Top + Source.Control.UndockHeight - SysCaptionHeight;
          Source.DockRect := R;
        end;
      end
      else
      if DropAlign in [alLeft, alRight] then
        if (Source.Control.DockOrientation = doHorizontal) or (Source.Control.HostDockSite = nil) then
        begin
          R := Source.DockRect;
          R.Right := Source.DockRect.Left + Source.Control.UndockWidth - SysCaptionHeight;
          Source.DockRect := R;
        end;
    end;
  end;
end;

procedure TJvDockVCPanel.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TJvDockVCPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(Handle, InfluenceRect);
  with Client, JvGlobalDockManager.DragObject do
    case Self.Align of
      alTop:
        if MousePos.Y >= InfluenceRect.Top then
          InflateRect(InfluenceRect, 0,
            JvDockGetMinOffset(TBDockHeight, Height, MouseDeltaY));
      alBottom:
        if MousePos.Y <= InfluenceRect.Top then
          InflateRect(InfluenceRect, 0,
            JvDockGetMinOffset(TBDockHeight, Height, 1 - MouseDeltaY));
      alLeft:
        if MousePos.X >= InfluenceRect.Left then
          InflateRect(InfluenceRect,
            JvDockGetMinOffset(LRDockWidth, Width, MouseDeltaX), 0);
      alRight:
        if MousePos.X <= InfluenceRect.Left then
          InflateRect(InfluenceRect,
            JvDockGetMinOffset(LRDockWidth, Width, 1 - MouseDeltaX), 0);
    end;
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockVCPanel.CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer);
var
  R: TRect;
  BorderWidth: Integer;
  GrabberSize: Integer;
  PanelScreenRect: TRect;
  SysCaptionHeight: Integer;
  DockSize: Integer;

  procedure GetDockSize;
  begin
    if Align in [alLeft, alRight] then
    begin
      if (Source.Control.HostDockSite <> nil) and
        (Source.Control.HostDockSite <> Source.TargetControl) then
        DockSize := Source.Control.UndockHeight
      else
        DockSize := Source.Control.Height;
    end
    else
    begin
      if (Source.Control.HostDockSite <> nil) and
        (Source.Control.HostDockSite <> Source.TargetControl) then
        DockSize := Source.Control.UndockWidth
      else
        DockSize := Source.Control.Width;
    end;
  end;

  procedure SetMaxDockSize(Align: TAlign);
  begin
    if Align in [alLeft, alRight] then
    begin
      if R.Bottom - R.Top >= PanelScreenRect.Bottom - PanelScreenRect.Top then
      begin
        R.Top := PanelScreenRect.Top;
        R.Bottom := PanelScreenRect.Bottom;
      end;
    end
    else
    begin
      if R.Right - R.Left >= PanelScreenRect.Right - PanelScreenRect.Left then
      begin
        R.Left := PanelScreenRect.Left;
        R.Right := PanelScreenRect.Right;
      end;
    end;
  end;

begin
  if Source.Control.HostDockSite is TJvDockCustomPanel then
  begin
    BorderWidth := TJvDockCustomPanel(Source.Control.HostDockSite).JvDockManager.BorderWidth;
    GrabberSize := TJvDockCustomPanel(Source.Control.HostDockSite).JvDockManager.GrabberSize;
  end
  else
  begin
    BorderWidth := 0;
    GrabberSize := 0;
  end;

  PanelScreenRect := BoundsRect;
  MapWindowPoints(Parent.Handle, 0, PanelScreenRect, 2);

  SysCaptionHeight := Ord(Source.Control.Floating) * JvDockGetSysCaptionHeight;

  GetDockSize;

  with Source.Control do
  begin
    case Self.Align of
      alTop:
        begin
          R.TopLeft := Self.ClientToScreen(Point(0, 0));
          R.BottomRight := Self.ClientToScreen(Point(Self.Width, TBDockHeight));
          R.Top := R.Top + Y -
            JvDockGetMinOffset(TBDockHeight, Height + 2 * BorderWidth, Source.MouseDeltaY);
          R.Bottom := R.Top + TBDockHeight;
          if (Self.Height > 0) and (R.Top + TBDockHeight div 2 < PanelScreenRect.Bottom) and
            (R.Bottom - TBDockHeight div 2 > PanelScreenRect.Top) then
          begin
            R.Left := R.Left + X - Round((Width + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaX);
            R.Right := R.Left + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
          end;
        end;
      alBottom:
        begin
          R.TopLeft := Self.ClientToScreen(Point(0, -TBDockHeight));
          R.BottomRight := Self.ClientToScreen(Point(Self.Width, 0));
          R.Top := R.Top + Y +
            JvDockGetMinOffset(TBDockHeight, Height + 2 * BorderWidth, 1 - Source.MouseDeltaY);
          R.Bottom := R.Top + TBDockHeight;
          if (Self.Height > 0) and (R.Top + TBDockHeight div 2 < PanelScreenRect.Bottom) and
            (R.Bottom - TBDockHeight div 2 > PanelScreenRect.Top) then
          begin
            R.Left := R.Left + X - Round((Width + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaX);
            R.Right := R.Left + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
          end;
        end;
      alLeft:
        begin
          R.TopLeft := Self.ClientToScreen(Point(0, 0));
          R.BottomRight := Self.ClientToScreen(Point(LRDockWidth, Self.Height));
          R.Left := R.Left + X -
            JvDockGetMinOffset(LRDockWidth, Width + 2 * BorderWidth, Source.MouseDeltaX);
          R.Right := R.Left + LRDockWidth;
          if (Self.Width > 0) and ((R.Left + LRDockWidth div 2 < PanelScreenRect.Right) and
            (R.Right - LRDockWidth div 2 > PanelScreenRect.Left)) then
          begin
            R.Top := R.Top + Y - Round((Height + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaY);
            R.Bottom := R.Top + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
          end;
        end;
      alRight:
        begin
          R.TopLeft := Self.ClientToScreen(Point(-LRDockWidth, 0));
          R.BottomRight := Self.ClientToScreen(Point(Self.Width, Self.Height));
          R.Left := R.Left + X +
            JvDockGetMinOffset(LRDockWidth, Width + 2 * BorderWidth, 1 - Source.MouseDeltaX);
          R.Right := R.Left + LRDockWidth;
          if (Self.Width > 0) and (R.Left + LRDockWidth div 2 > PanelScreenRect.Left) and
            (R.Right - LRDockWidth div 2 < PanelScreenRect.Right) then
          begin
            R.Top := R.Top + Y - Round((Height + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaY);
            R.Bottom := R.Top + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
          end;
        end;
    end;
  end;
  SetMaxDockSize(Align);
  Inc(R.Left);
  Source.DockRect := R;
end;

function TJvDockVCPanel.CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
var
  DropAlign: TAlign;
  MousePos: TPoint;
begin
  if (NewTarget = nil) or (NewTarget = Client.HostDockSite) then
  begin
    DropAlign := Source.DropAlign;
    Source.DropOnControl := JvDockManager.GetDockEdge(
      Source.DockRect, Source.DragPos, DropAlign, Source.Control);
    Source.DropAlign := DropAlign;
  end;
  MousePos := ScreenToClient(Source.DragPos);
  if ((Align in [alTop, alBottom]) and ((0 > MousePos.X) or (Width < MousePos.X))) or
    ((Align in [alLeft, alRight]) and ((0 > MousePos.Y) or (Height < MousePos.Y))) or
    (Source.CtrlDown) or Source.Floating then
    Result := inherited CustomUnDock(Source, NewTarget, Client)
  else
  if Source.DropOnControl <> Source.Control then
    Result := inherited CustomUnDock(Source, NewTarget, Client)
  else
    Result := True;
end;

function TJvDockVCPanel.GetDockEdge(MousePos: TPoint): TAlign;
begin
  Result := inherited GetDockEdge(MousePos);
end;

//=== { TJvDockVCSplitter } ==================================================

constructor TJvDockVCSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldSize := MinSize;
end;

function TJvDockVCSplitter.DoCanResize(var NewSize: Integer): Boolean;
var
  DockPanel: TJvDockPanel;
  Limit, MinSize: Integer;
begin
  Result := inherited DoCanResize(NewSize);
  if Result and (FOldSize > NewSize) then
  begin
    DockPanel := DockServer.DockPanelWithAlign[Align];
    if DockPanel = nil then
    begin
      Result := False;
      Exit;
    end;
    Limit := DockPanel.JvDockManager.GetDockClientLimit(JvDockGetControlOrient(DockPanel),
      Align in [alLeft, alTop]);
    MinSize := DockPanel.JvDockManager.MinSize;

    if DockPanel.Align in [alLeft, alTop] then
    begin
      if NewSize < Limit + MinSize then
        Result := False;
    end
    else
    begin
      if NewSize < JvDockGetControlSize(DockPanel) - Limit + MinSize then
        Result := False;
    end;
  end;
  if Result then
    FOldSize := NewSize;
end;

procedure TJvDockVCSplitter.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DockPanel: TJvDockPanel;
begin
  DockPanel := DockServer.DockPanelWithAlign[Align];
  if Assigned(DockPanel) then
  begin
    DockPanel.JvDockManager.BeginResizeDockSite;
    try
      inherited MouseUp(Button, Shift, X, Y);
    finally
      DockPanel.JvDockManager.EndResizeDockSite;
    end;
  end;
end;

procedure TJvDockVCSplitter.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  Inc(Rect.Right, 2);
  case Align of
    alLeft:
      InflateRect(Rect, 0, 2);
    alRight:
      begin
        OffsetRect(Rect, -1, 0);
        InflateRect(Rect, 0, 2);
      end;
    alTop:
      begin
        Inc(Rect.Bottom, 2);
        InflateRect(Rect, 2, 0);
      end;
    alBottom:
      begin
        Dec(Rect.Top, 2);
        InflateRect(Rect, 2, 1);
      end;
  end;
  Canvas.Brush.Color := Color;
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

//=== { TJvDockVCStyle } =====================================================

constructor TJvDockVCStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DockPanelClass := TJvDockVCPanel;
  DockSplitterClass := TJvDockVCSplitter;
  ConjoinPanelClass := TJvDockVCConjoinPanel;
  TabDockClass := TJvDockVCTabPageControl;
  DockPanelTreeClass := TJvDockVCTree;
  DockPanelZoneClass := TJvDockVCZone;
  ConjoinPanelTreeClass := TJvDockVCTree;
  ConjoinPanelZoneClass := TJvDockVCZone;
  ConjoinServerOptionClass := TJvDockVCConjoinServerOption;
  TabServerOptionClass := TJvDockVCTabServerOption;
end;

function TJvDockVCStyle.CanSetEachOtherDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := False;
end;

procedure TJvDockVCStyle.FormGetDockEdge(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TJvDockVCStyle.FormStartDock(DockClient: TJvDockClient;
  var Source: TJvDockDragDockObject);
begin
  inherited FormStartDock(DockClient, Source);
  Source := TJvDockVCDragDockObject.Create(DockClient.ParentForm);
end;

procedure TJvDockVCStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
var
  ADockClient: TJvDockClient;
begin
  if DockBaseControl is TJvDockClient then
  begin
    ADockClient := TJvDockClient(DockBaseControl);
    if IsCreate then
    begin
      FOldEachOtherDock := ADockClient.EachOtherDock;
      ADockClient.EachOtherDock := False;
      ADockClient.DirectDrag := True;
    end
    else
      ADockClient.EachOtherDock := FOldEachOtherDock;
  end;
end;

//=== { TJvDockVCTree } ======================================================

constructor TJvDockVCTree.Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass;
  ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(DockSite, DockZoneClass, ADockStyle);
  Version := RsDockVCDockTreeVersion;
  BorderWidth := 4;
  MinSize := 20;
end;

procedure TJvDockVCTree.BeginDrag(Control: TControl; Immediate: Boolean; Threshold: Integer);
begin
  JvGlobalDockManager.BeginDrag(Control, True, 0);
end;

procedure TJvDockVCTree.CalcSplitterPos;
var
  TestLimit: Integer;
  TempPos: TPoint;
begin
  TempPos := SizePos;
  if SizingZone.ParentZone.Orientation = doHorizontal then
  begin
    TestLimit := SizingZone.Top + MinSize;
    if TempPos.Y <= TestLimit then
    begin
      if DockSiteOrientation = doVertical then
      begin
        if TempPos.Y <= (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2 then
          TempPos.Y := (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2;
      end
      else
        TempPos.Y := TestLimit;
    end;

    TestLimit := GetSplitterLimit(SizingZone, False, True) - MinSize;
    if TempPos.Y >= TestLimit then
    begin
      if DockSiteOrientation = doVertical then
      begin
        if TempPos.Y >= DockSiteSizeAlternate - SizingZone.VisibleNextSiblingCount * MinSize then
          TempPos.Y := DockSiteSizeAlternate - SizingZone.VisibleNextSiblingCount * MinSize;
      end
      else
        TempPos.Y := TestLimit;
    end;
  end
  else
  begin
    TestLimit := SizingZone.Left + MinSize;
    if TempPos.X <= TestLimit then
    begin
      if DockSiteOrientation = doHorizontal then
      begin
        if TempPos.X <= (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2 then
          TempPos.X := (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2;
      end
      else
        TempPos.X := TestLimit;
    end;

    TestLimit := GetSplitterLimit(SizingZone, False, True) - MinSize;
    if TempPos.X >= TestLimit then
    begin
      if DockSiteOrientation = doHorizontal then
      begin
        if TempPos.X >= DockSiteSizeAlternate - SizingZone.VisibleNextSiblingCount * MinSize then
          TempPos.X := DockSiteSizeAlternate - SizingZone.VisibleNextSiblingCount * MinSize;
      end
      else
        TempPos.X := TestLimit;
    end;
  end;
  SizePos := TempPos;
end;

procedure TJvDockVCTree.ControlVisibilityChanged(Control: TControl; Visible: Boolean);
begin
  inherited ControlVisibilityChanged(Control, Visible);
end;

procedure TJvDockVCTree.CustomLoadZone(Stream: TStream; var Zone: TJvDockZone);
begin
  Stream.Read(TJvDockVCZone(Zone).FZoneSizeStyle, SizeOf(TJvDockZoneSizeStyle));
  inherited CustomLoadZone(Stream, Zone);
end;

procedure TJvDockVCTree.CustomSaveZone(Stream: TStream; Zone: TJvDockZone);
begin
  Stream.Write(TJvDockVCZone(Zone).FZoneSizeStyle, SizeOf(TJvDockZoneSizeStyle));
  inherited CustomSaveZone(Stream, Zone);
end;

function TJvDockVCTree.DoLButtonDown(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer): Boolean;
var
  TempZone: TJvDockVCZone;
  Active: Boolean;
begin
  Result := inherited DoLButtonDown(Msg, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTEXPAND) then
  begin
    TempZone := TJvDockVCZone(Zone);
    Active := ((TempZone.ParentZone.Orientation <> DockSiteOrientation) and
      (TempZone.ParentZone.VisibleChildCount >= 2));
    if Active then
    begin
      TempZone.ExpandButtonDown := True;
      TempZone.MouseDown := True;
      FExpandBtnZone := TempZone;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockVCTree.DoLButtonUp(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
var
  TempZone: TJvDockVCZone;
begin
  inherited DoLButtonUp(Msg, Zone, HTFlag);
  if (SizingZone = nil) and (FExpandBtnZone <> nil) then
  begin
    FExpandBtnZone := nil;
    if (Zone <> nil) and (HTFlag = HTEXPAND) then
    begin
      TempZone := TJvDockVCZone(Zone);
      TempZone.ExpandButtonDown := False;
      if TempZone.ZoneSizeStyle in [zssMaximum] then
        TJvDockVCZone(TempZone.ParentZone).DoSetChildSizeStyle(zssNormal)
      else
      begin
        TJvDockVCZone(TempZone.ParentZone).DoSetChildSizeStyle(zssMinimum);
        TempZone.ZoneSizeStyle := zssMaximum;
      end;
      ResetDockZoneSizeStyle(TempZone.ParentZone, TempZone.ZoneSizeStyle, nil);
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockVCTree.DoMouseMove(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
var
  TempZone: TJvDockVCZone;
begin
  inherited DoMouseMove(Msg, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TJvDockVCZone(Zone);
    if ((TempZone <> nil) and (TempZone.ExpandButtonDown <> (HTFlag = HTEXPAND)) and
      ((FExpandBtnZone = TempZone) and FExpandBtnZone.MouseDown)) then
    begin
      TempZone.ExpandButtonDown := (HTFlag = HTEXPAND) and FExpandBtnZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockVCTree.DoOtherHint(Zone: TJvDockZone;
  HTFlag: Integer; var HintStr: string);
begin
  inherited DoOtherHint(Zone, HTFlag, HintStr);
  if HTFlag = HTEXPAND then
    HintStr := RsDockVCDockTreeExpandBtnHint;
end;

procedure TJvDockVCTree.DrawDockGrabber(Control: TWinControl; const ARect: TRect);
var
  VCDockZone: TJvDockVCZone;
  DrawRect: TRect;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  CurrentThemeTypeBtn: TThemedWindow;
  CurrentThemeTypeSB: TThemedScrollBar;
  {$ENDIF JVCLThemesEnabled}

  procedure DrawCloseButton(Left, Top: Integer);
  var
    ADockClient: TJvDockClient;
  begin
    if VCDockZone <> nil then
    begin
      ADockClient := FindDockClient(Control);
      if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
        Exit;
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.{$IFDEF RTL230_UP}Available{$ELSE}ThemesAvailable{$ENDIF RTL230_UP} and ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
      begin
        CurrentThemeTypeBtn := twSmallCloseButtonNormal;
        if VCDockZone.CloseBtnDown then
          CurrentThemeTypeBtn := twSmallCloseButtonPushed;
        Details := ThemeServices.GetElementDetails(CurrentThemeTypeBtn);
        ThemeServices.DrawElement(Canvas.Handle, Details, Classes.Rect(Left, Top,
          Left + ButtonWidth, Top + ButtonHeight));
      end
      else
      {$ENDIF JVCLThemesEnabled}
        DrawFrameControl(Canvas.Handle, Classes.Rect(Left, Top, Left + ButtonWidth,
          Top + ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Ord(VCDockZone.CloseBtnDown) * DFCS_PUSHED);
    end;
  end;

  procedure DrawExpendBotton(Left, Top: Integer);
  const
    ArrowOrient: array [TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0, 0);
    {$IFDEF JVCLThemesEnabled}
    ArrowOrientTheme: array [TAlign] of TThemedScrollBar =
     (tsScrollBarDontCare, tsArrowBtnUpNormal, tsArrowBtnDownNormal, tsArrowBtnLeftNormal,
      tsArrowBtnRightNormal, tsScrollBarDontCare, tsScrollBarDontCare);
    {$ENDIF JVCLThemesEnabled}
    CurrArrow: array [Boolean, TDockOrientation] of TAlign =
      ((alNone, alLeft, alTop), (alNone, alRight, alBottom));
  var
    InActive: Boolean;
    IsMaximum: Boolean;
  begin
    if VCDockZone <> nil then
    begin
      InActive := not ((VCDockZone.ParentZone.Orientation <> DockSiteOrientation) and
        (VCDockZone.ParentZone.VisibleChildCount >= 2));
      IsMaximum := VCDockZone.ZoneSizeStyle in [zssMaximum];
      {$IFDEF JVCLThemesEnabled}
      if ThemeServices.{$IFDEF RTL230_UP}Available{$ELSE}ThemesAvailable{$ENDIF RTL230_UP} and ThemeServices.{$IFDEF RTL230_UP}Enabled{$ELSE}ThemesEnabled{$ENDIF RTL230_UP} then
      begin
        CurrentThemeTypeSB := ArrowOrientTheme[CurrArrow[IsMaximum, DockSiteOrientation]];
        if VCDockZone.ExpandButtonDown then
          CurrentThemeTypeSB := TThemedScrollBar(Ord(CurrentThemeTypeSB) + 2);
        if InActive then
          CurrentThemeTypeSB := TThemedScrollBar(Ord(CurrentThemeTypeSB) + 3);
        Details := ThemeServices.GetElementDetails(CurrentThemeTypeSB);
        ThemeServices.DrawElement(Canvas.Handle, Details, Classes.Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight));
      end
      else
      {$ENDIF JVCLThemesEnabled}
        DrawFrameControl(Canvas.Handle, Classes.Rect(Left, Top, Left + ButtonWidth,
          Top + ButtonHeight), DFC_SCROLL,
          ArrowOrient[CurrArrow[IsMaximum, DockSiteOrientation]] +
            Cardinal(Ord(InActive)) * DFCS_INACTIVE +
            Cardinal(Ord(VCDockZone.ExpandButtonDown)) * DFCS_PUSHED);
    end;
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    if (Left >= Right) or (Top >= Bottom) then
      Exit;
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(Right, Top);
    Canvas.LineTo(Left, Top);
    Canvas.LineTo(Left, Bottom);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.LineTo(Right, Bottom);
    Canvas.LineTo(Right, Top - 1);
  end;

begin
  VCDockZone := TJvDockVCZone(FindControlZone(Control));
  DrawRect := ARect;
  Canvas.Brush.Color := TWinControlAccessProtected(DockSite).Color;
  Canvas.FillRect(DrawRect);
  case GrabbersPosition of
    gpLeft:
      begin
        DrawExpendBotton(ARect.Left + BorderWidth + LeftOffset,
          ARect.Top + TopOffset + ButtonHeight + ButtonSplitter + BorderWidth);
        DrawCloseButton(ARect.Left + BorderWidth + LeftOffset, ARect.Top + TopOffset + BorderWidth);
        DrawGrabberLine(ARect.Left + BorderWidth + LeftOffset + 3,
          ARect.Top + 2 * ButtonHeight + TopOffset + ButtonSplitter + BottomOffset + BorderWidth + 3,
          ARect.Left + BorderWidth + LeftOffset + 5, ARect.Bottom - BorderWidth - 2);
        DrawGrabberLine(ARect.Left + BorderWidth + LeftOffset + 7,
          ARect.Top + 2 * ButtonHeight + TopOffset + ButtonSplitter + BottomOffset + BorderWidth + 3,
          ARect.Left + BorderWidth + LeftOffset + 9, ARect.Bottom - BorderWidth - 2);
      end;
    gpTop:
      begin
        DrawExpendBotton(ARect.Right - LeftOffset - 2 * ButtonWidth - ButtonSplitter - BorderWidth,
          ARect.Top + TopOffset + BorderWidth);
        DrawCloseButton(ARect.Right - LeftOffset - ButtonWidth - BorderWidth, ARect.Top + TopOffset + BorderWidth);
        DrawGrabberLine(ARect.Left + BorderWidth, ARect.Top + BorderWidth + TopOffset + 3,
          ARect.Right - 2 * ButtonWidth - RightOffset - ButtonSplitter - LeftOffset - BorderWidth - 3,
          ARect.Top + BorderWidth + TopOffset + 5);
        DrawGrabberLine(ARect.Left + BorderWidth, ARect.Top + BorderWidth + TopOffset + 7,
          ARect.Right - 2 * ButtonWidth - RightOffset - ButtonSplitter - LeftOffset - BorderWidth - 3,
          ARect.Top + BorderWidth + TopOffset + 9);
      end;
    gpBottom:
      begin
      end;
    gpRight:
      begin
      end;
  end;
end;

procedure TJvDockVCTree.DrawDockSiteRect;
var
  Rect: TRect;
begin
  inherited DrawDockSiteRect;
  Rect := DockSite.ClientRect;
  InflateRect(Rect, BorderWidth, 0);
  if DockSite.Align = alTop then
    Inc(Rect.Bottom, BorderWidth)
  else
  if DockSite.Align = alBottom then
    Dec(Rect.Top, BorderWidth);
  Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
  Frame3D(Canvas, Rect, clBtnHighlight, clBtnShadow, 1);

  Canvas.Pen.Color := clBlack;
  if DockSite.Align = alRight then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, DockSite.Height);
  end
  else
  if DockSite.Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(DockSite.Width, 0);
  end;
end;

procedure TJvDockVCTree.DrawSplitterRect(const ARect: TRect);
var
  Rect: TRect;
begin
  inherited DrawSplitterRect(ARect);
  Rect := ARect;
  InflateRect(Rect, 1, 1);
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

procedure TJvDockVCTree.GetCaptionRect(var Rect: TRect);
begin
  case GrabbersPosition of
    gpTop:
      Rect.Bottom := Rect.Top + GrabberSize + 2;
    gpLeft:
      Rect.Right := Rect.Left + GrabberSize + 2;
  end;
end;

function TJvDockVCTree.GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign;
begin
  Result := inherited GetDockAlign(Client, DropCtl);
  case DockSite.Align of
    alLeft, alRight:
      if (Result in [alLeft, alRight]) and (DropCtl <> nil) then
        DropCtl := nil;
    alTop, alBottom:
      if (Result in [alTop, alBottom]) and (DropCtl <> nil) then
        DropCtl := nil;
  end;
end;

function TJvDockVCTree.GetDockEdge(DockRect: TRect;
  MousePos: TPoint; var DropAlign: TAlign; Control: TControl): TControl;
var
  Zone: TJvDockZone;
  TempOrient: TDockOrientation;
begin
  inherited GetDockEdge(DockRect, MousePos, DropAlign, Control);

  MapWindowPoints(0, DockSite.Handle, DockRect, 2);
  InitDockHeightWidth(0, DockSite.Height, DockSite.Width);
  InitDockRectangles(DockRect);

  TempOrient := DockSiteOrientation;
  Zone := GetDropOnZone(TempOrient, DockRect, DropAlign);
  TempOrient := JvDockExchangeOrient(TempOrient);
  Result := GetDropOnControl(TempOrient, Zone, DockRect, DropAlign, Control);
  DropDockSize := DockRectangles[TempOrient, True] - DockRectangles[TempOrient, False];
end;

function TJvDockVCTree.GetDropOnControl(Orient: TDockOrientation; Zone: TJvDockZone;
  DockRect: TRect; var DropAlign: TAlign; Control: TControl): TControl;
var
  TempZone: TJvDockZone;
  Scale: Double;
  BeginBorderLimit: Integer;
  EndBorderLimit: Integer;

  procedure GetBeginBorderControl(Zone: TJvDockZone);
  begin
    BeginBorderLimit := Zone.TopLeft[Orient];

    if DockRectangles[Orient, False] < BeginBorderLimit then
    begin
      Result := Zone.ChildControl;
      DropAlign := DropAlignArray[Orient, False];
    end;
  end;

  procedure GetEndBorderControl(Zone: TJvDockZone);
  begin
    BeginBorderLimit := Zone.TopLeft[Orient];
    EndBorderLimit := BeginBorderLimit + Zone.HeightWidth[Orient];

    if DockRectangles[Orient, False] < EndBorderLimit then
    begin
      Result := Zone.ChildControl;
      if DockRectangles[Orient, False] = BeginBorderLimit then
        Scale := ScaleMaximum
      else
        Scale := (EndBorderLimit - DockRectangles[Orient, True]) / (DockRectangles[Orient, False] - BeginBorderLimit);
      if Scale >= 1 then
        DropAlign := DropAlignArray[Orient, False]
      else
      begin
        if (Zone.AfterClosestVisibleZone <> nil) and (Zone.AfterClosestVisibleZone.ChildControl = Control) then
        begin
          Result := Zone.AfterClosestVisibleZone.ChildControl;
          DropAlign := DropAlignArray[Orient, False];
        end
        else
          DropAlign := DropAlignArray[Orient, True];
      end;
    end;
  end;

begin
  Result := nil;
  Scale := 0;
  if Zone <> nil then
  begin
    if Zone.ChildCount = 0 then
    begin
      GetBeginBorderControl(Zone);
      if Result = nil then
        GetEndBorderControl(Zone);
    end
    else
    begin
      TempZone := Zone.ChildZones;
      if TempZone <> nil then
        GetBeginBorderControl(TempZone);
      while (TempZone <> nil) and (Result = nil) do
      begin
        GetEndBorderControl(TempZone);
        TempZone := TempZone.AfterClosestVisibleZone;
      end;
    end;
  end;
end;

function TJvDockVCTree.GetDropOnZone(Orient: TDockOrientation; DockRect: TRect;
  var DropAlign: TAlign): TJvDockZone;
var
  TempZone: TJvDockZone;
  Scale: Double;
  TempOrient: TDockOrientation;

  procedure GetBeginBorderZone(BorderLimit: Integer);
  begin
    if DockRectangles[Orient, True] = BorderLimit then
      Scale := ScaleMaximum
    else
      Scale := (BorderLimit - DockRectangles[Orient, False]) / (DockRectangles[Orient, True] - BorderLimit);
    if Scale >= 0 then
    begin
      if Scale >= 1 then
        Result := TempZone.BeforeClosestVisibleZone
      else
        Result := TempZone;
    end;
  end;

  procedure GetEndBorderZone(BorderLimit: Integer);
  begin
    if (DockRectangles[Orient, True] <= BorderLimit) then
      Scale := ScaleMaximum
    else
      Scale := (BorderLimit - DockRectangles[Orient, False]) / (DockRectangles[Orient, True] - BorderLimit);
    if Scale >= 0 then
    begin
      if Scale < 1 then
        Result := TempZone.AfterClosestVisibleZone
      else
        Result := TempZone;
    end;
  end;

begin
  Result := nil;
  TempOrient := JvDockExchangeOrient(Orient);
  if (DockRectangles[TempOrient, False] > DockHeightWidth[TempOrient]) or
    (DockRectangles[TempOrient, True] < 0) then
    Exit;

  if (DockRectangles[Orient, False] + DockRectangles[Orient, True]) div 2 <= 0 then
    DropAlign := DropAlignArray[Orient, False]
  else
  if (DockRectangles[Orient, False] + DockRectangles[Orient, True]) div 2 >= DockHeightWidth[Orient] then
    DropAlign := DropAlignArray[Orient, True]
  else
  begin
    if (TopZone.ChildCount <= 1) or (TopZone.Orientation <> Orient) then
      Result := TopZone
    else
    begin
      Scale := 0;
      TempZone := TopZone.ChildZones;
      GetBeginBorderZone(0);
      while (TempZone <> nil) and (Scale <= 0) do
      begin
        GetEndBorderZone(TempZone.ZoneLimit);
        TempZone := TempZone.AfterClosestVisibleZone;
      end;
    end;
  end;
end;

function TJvDockVCTree.GetLeftGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
begin
  if (MousePos.X >= Zone.Left + BorderWidth) and (MousePos.X <= Zone.Left + BorderWidth + GrabberSize) and
    (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + Zone.Height) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Classes.Rect(
        Left - GrabberSize + LeftOffset,
        Top + TopOffset,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else
      if PtInRect(Classes.Rect(
        Left - GrabberSize + LeftOffset,
        Top + ButtonHeight + TopOffset + ButtonSplitter,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + 2 * ButtonHeight + TopOffset + ButtonSplitter), MousePos) then
        HTFlag := HTEXPAND
      else
        HTFlag := HTCAPTION;
    end;
  end
  else
    Result := nil;
end;

function TJvDockVCTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
begin
  if (MousePos.Y >= Zone.Top + BorderWidth) and (MousePos.Y <= Zone.Top + BorderWidth + GrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Classes.Rect(
        Left + Width - ButtonWidth - RightOffset,
        Top - GrabberSize + TopOffset,
        Left + Width - RightOffset,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else
      if PtInRect(Classes.Rect(
        Left + Width - 2 * ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTEXPAND
      else
        HTFlag := HTCAPTION;
    end;
  end
  else
    Result := nil;
end;

procedure TJvDockVCTree.InsertControl(Control: TControl;
  InsertAt: TAlign; DropCtl: TControl);
begin
  inherited InsertControl(Control, InsertAt, DropCtl);
end;

procedure TJvDockVCTree.InsertNewParent(NewZone, SiblingZone: TJvDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
begin
  inherited InsertNewParent(NewZone, SiblingZone,
    ParentOrientation, InsertLast, Update);
end;

procedure TJvDockVCTree.InsertSibling(NewZone, SiblingZone: TJvDockZone;
  InsertLast, Update: Boolean);
begin
  inherited InsertSibling(NewZone, SiblingZone, InsertLast, Update);
end;

procedure TJvDockVCTree.RemoveControl(Control: TControl);
var
  DockRect: TRect;
  OldDockSize: Integer;
begin
  OldDockSize := DropDockSize;

  DockRect := GetFrameRect(Control);
  if DockSiteOrientation = doHorizontal then
    DropDockSize := DockRect.Right - DockRect.Left
  else
  if DockSiteOrientation = doVertical then
    DropDockSize := DockRect.Bottom - DockRect.Top;

  inherited RemoveControl(Control);

  DropDockSize := OldDockSize;
end;

procedure TJvDockVCTree.RemoveZone(Zone: TJvDockZone; Hide: Boolean);
begin
  inherited RemoveZone(Zone, Hide);
end;

procedure TJvDockVCTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  BeginUpdate;
  try
    if not JvGlobalDockIsLoading then
    begin
      R := DockSite.ClientRect;

      if ResizeCount > 0 then
      begin
        if TopZone.ChildZones <> nil then
        begin
          if (DockSite.Align = alRight) and (R.Right <> PreviousRect.Right) then
          begin
            ShiftBy := -PreviousRect.Right + R.Right;
            ShiftScaleOrientation := doVertical;
            ForEachAt(TopZone.ChildZones, ShiftZone, tskForward);
            SetNewBounds(nil);
          end;
          if (DockSite.Align = alBottom) and (R.Bottom <> PreviousRect.Bottom) then
          begin
            ShiftBy := -PreviousRect.Bottom + R.Bottom;
            ShiftScaleOrientation := doHorizontal;
            ForEachAt(TopZone.ChildZones, ShiftZone, tskForward);
            SetNewBounds(nil);
          end;
        end;
      end;

      if (DockSiteOrientation = doVertical) and (R.Bottom <> PreviousRect.Bottom) then
      begin
        if PreviousRect.Bottom - PreviousRect.Top = 0 then
          ScaleBy := R.Bottom - R.Top
        else
        if PreviousRect.Bottom - PreviousRect.Top > 0 then
          ScaleBy := (R.Bottom - R.Top) / (PreviousRect.Bottom - PreviousRect.Top)
        else
          ScaleBy := 1;
        ShiftScaleOrientation := doHorizontal;
        if ScaleBy <> 1 then
          ForEachAt(nil, ScaleZone, tskForward);
      end;
      if (DockSiteOrientation = doHorizontal) and (R.Right <> PreviousRect.Right) then
      begin
        if PreviousRect.Right - PreviousRect.Left = 0 then
          ScaleBy := R.Right - R.Left
        else
        if PreviousRect.Right - PreviousRect.Left > 0 then
          ScaleBy := (R.Right - R.Left) / (PreviousRect.Right - PreviousRect.Left)
        else
          ScaleBy := 1;
        ShiftScaleOrientation := doVertical;
        if ScaleBy <> 1 then
          ForEachAt(nil, ScaleZone, tskForward);
      end;
    end;
    inherited ResetBounds(Force);
  finally
    EndUpdate;
  end;
end;

procedure TJvDockVCTree.ResetDockZoneSizeStyle(Parent: TJvDockZone;
  ZoneSizeStyle: TJvDockZoneSizeStyle; Exclude: TJvDockZone);
var
  Zone: TJvDockVCZone;
  ChildCount: Integer;
  AverageSize: Integer;
begin
  ChildCount := Parent.VisibleChildCount - Ord((Exclude <> nil) and (Exclude.ParentZone = Parent));
  if ChildCount = 0 then
    Exit;
  AverageSize := DockSiteSizeAlternate div ChildCount;
  Assert(AverageSize > 0);
  Zone := TJvDockVCZone(Parent.FirstVisibleChildZone);
  while Zone <> nil do
  begin
    if Exclude <> Zone then
    begin
      Dec(ChildCount);
      if ZoneSizeStyle in [zssMaximum] then
      begin
        if Zone.ZoneSizeStyle = zssMinimum then
          Zone.ZoneLimit := Zone.LimitBegin + MinSize
        else
        if Zone.ZoneSizeStyle = zssMaximum then
          Zone.ZoneLimit := DockSiteSizeAlternate - ChildCount * MinSize;
      end
      else
      if ZoneSizeStyle in [zssNormal] then
        Zone.ZoneLimit := Zone.LimitBegin + AverageSize;
    end
    else
    if Exclude <> nil then
      Exclude.ZoneLimit := Exclude.LimitBegin;

    Zone := TJvDockVCZone(Zone.AfterClosestVisibleZone);
  end;
  SetNewBounds(Parent);
  ForEachAt(Parent, UpdateZone, tskForward);
end;

procedure TJvDockVCTree.ScaleChildZone(Zone: TJvDockZone);
begin
  if Zone <> nil then
    case TJvDockVCZone(Zone).ZoneSizeStyle of
      zssMinimum:
        begin
          Zone.ZoneLimit := Zone.LimitBegin + MinSize;
          Exit;
        end;
      zssMaximum:
        begin
          Zone.ZoneLimit := DockSiteSizeAlternate - Zone.VisibleNextSiblingCount * MinSize;
          Exit;
        end;
    end;
  inherited ScaleChildZone(Zone);

  if (Zone <> nil) and (Zone.ParentZone <> nil) and Zone.Visibled and
    (Zone.ParentZone.Orientation = ShiftScaleOrientation) then
  begin
    if Zone.LimitSize < MinSize then
      Zone.ZoneLimit := Zone.LimitBegin + MinSize;

    if (Zone.BeforeClosestVisibleZone <> nil) and
      (Zone.LimitBegin > DockSiteSizeWithOrientation[Zone.ParentZone.Orientation] -
        (Zone.VisibleNextSiblingCount + 1) * MinSize + SplitterWidth div 2) then
      Zone.BeforeClosestVisibleZone.ZoneLimit := DockSiteSizeWithOrientation[Zone.ParentZone.Orientation] -
        (Zone.VisibleNextSiblingCount + 1) * MinSize + SplitterWidth div 2;
  end;
end;

procedure TJvDockVCTree.ScaleSiblingZone(Zone: TJvDockZone);
begin
  inherited ScaleSiblingZone(Zone);
end;

procedure TJvDockVCTree.ScaleZone(Zone: TJvDockZone);
begin
  if Zone <> nil then
    case TJvDockVCZone(Zone).ZoneSizeStyle of
      zssMinimum:
        Zone.ZoneLimit := Zone.LimitBegin + MinSize;
      zssMaximum:
        Zone.ZoneLimit := DockSiteSizeAlternate - Zone.VisibleNextSiblingCount * MinSize;
    else
      inherited ScaleZone(Zone);
    end
  else
    inherited ScaleZone(Zone);
end;

procedure TJvDockVCTree.ShiftZone(Zone: TJvDockZone);
begin
  inherited ShiftZone(Zone);
  if (Zone <> nil) and (Zone <> TopZone) and
    (Zone.ParentZone.Orientation = ShiftScaleOrientation) then
  begin
    if Zone.LimitSize < MinSize then
      Zone.ZoneLimit := Zone.LimitBegin + MinSize;
  end;
end;

procedure TJvDockVCTree.SplitterMouseUp;
begin
  BeginUpdate;
  try
    ShiftBy := 0;

    if (DockSiteOrientation = doVertical) and
      (SizingZone.ParentZone.Orientation = doVertical) then
      ShiftBy := SizePos.X + (SplitterWidth div 2) - SizingZone.ZoneLimit
    else
    if (DockSiteOrientation = doHorizontal) and
      (SizingZone.ParentZone.Orientation = doHorizontal) then
      ShiftBy := SizePos.Y + (SplitterWidth div 2) - SizingZone.ZoneLimit;

    if (ShiftBy <> 0) and (SizingZone.AfterClosestVisibleZone <> nil) then
    begin
      if (DockSite.Align in [alLeft, alTop]) then
      begin
        ShiftScaleOrientation := DockSiteOrientation;
        ForEachAt(SizingZone.AfterClosestVisibleZone, ShiftZone, tskForward);
        inherited SplitterMouseUp;
      end
      else
      begin
        ShiftBy := -ShiftBy;
        ShiftScaleOrientation := DockSiteOrientation;
        ForEachAt(SizingZone.AfterClosestVisibleZone, ShiftZone, tskForward);
        SizePos := Point(SizePos.X + ShiftBy, SizePos.Y + ShiftBy);
        inherited SplitterMouseUp;
      end;

      DockSiteSize := DockSiteSize + ShiftBy;
    end
    else
    begin
      TJvDockVCZone(SizingZone.ParentZone).DoSetChildSizeStyle(zssNormal);
      inherited SplitterMouseUp;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvDockVCTree.WindowProc(var Msg: TMessage);
begin
  inherited WindowProc(Msg);
end;

//=== { TJvDockVCZone } ======================================================

constructor TJvDockVCZone.Create(Tree: TJvDockTree);
begin
  inherited Create(Tree);
  FZoneSizeStyle := zssNormal;
  FExpandButtonDown := False;
end;

procedure TJvDockVCZone.DoSetChildSizeStyle(ZoneSizeStyle: TJvDockZoneSizeStyle);
var
  Zone: TJvDockVCZone;
begin
  Zone := TJvDockVCZone(ChildZones);
  while Zone <> nil do
  begin
    Zone.ZoneSizeStyle := ZoneSizeStyle;
    Zone := TJvDockVCZone(Zone.AfterClosestVisibleZone);
  end;
end;

procedure TJvDockVCZone.Insert(DockSize: Integer; Hide: Boolean);
var
  PrevShift, NextShift: Integer;
  TempSize: Integer;
  BorderSize: Integer;
  BeforeVisibleZone, AfterVisibleZone: TJvDockZone;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := True;
    Exit;
  end;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone := AfterClosestVisibleZone;

  BorderSize := TJvDockVCTree(Tree).BorderWidth * Ord(AfterClosestVisibleZone = nil);

  if ParentZone.Orientation <> TJvDockVCTree(Tree).DockSiteOrientation then
  begin
    if ((BeforeVisibleZone <> nil) and (TJvDockVCZone(BeforeVisibleZone).ZoneSizeStyle in [zssMaximum, zssMinimum])) or
      ((AfterVisibleZone <> nil) and (TJvDockVCZone(AfterVisibleZone).ZoneSizeStyle in [zssMaximum, zssMinimum])) then
    begin
      ZoneSizeStyle := zssMinimum;
      TJvDockVCTree(Tree).ResetDockZoneSizeStyle(ParentZone, zssMaximum, nil);
      Visibled := True;
      Exit;
    end;
    case TJvDockVCTree(Tree).DockSiteOrientation of
      doVertical:
        TempSize := Tree.DockSite.Height;
      doHorizontal:
        TempSize := Tree.DockSite.Width;
    else
      raise Exception.CreateRes(@RsEInvalidDockSiteOrientationValue);
    end;

    if DockSize >= TempSize - (ParentZone.VisibleChildCount) * TJvDockVCTree(Tree).MinSize then
      DockSize := (TempSize - (ParentZone.VisibleChildCount) * TJvDockVCTree(Tree).MinSize) div 2;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    Visibled := False;

    if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
    begin
      PrevShift := 0;
      NextShift := 0;
    end
    else
    if BeforeVisibleZone = nil then
    begin
      PrevShift := 0;
      NextShift := DockSize + BorderSize;
      ZoneLimit := DockSize + BorderSize;
      if ParentZone.VisibleChildCount = 1 then
        AfterVisibleZone.ZoneLimit := TempSize;
    end
    else
    if AfterVisibleZone = nil then
    begin
      PrevShift := DockSize + BorderSize;
      NextShift := 0;
      if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
        BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
      else
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := TempSize;
    end
    else
    begin
      PrevShift := Round((BeforeVisibleZone.ZoneLimit) * (DockSize + BorderSize) / TempSize);
      NextShift := DockSize - PrevShift;
      if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
        BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
      else
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize;
    end;

    Visibled := True;

    if PrevShift <> 0 then
    begin
      with TJvDockVCTree(Tree) do
      begin
        ReplacementZone := BeforeVisibleZone;
        try
          if BeforeVisibleZone.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (BeforeVisibleZone.ZoneLimit + PrevShift)
          else
            ScaleBy := 1;
          ShiftScaleOrientation := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;

      if BeforeVisibleZone.LimitSize < TJvDockVCTree(Tree).MinSize then
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TJvDockVCTree(Tree).MinSize;
    end;

    if NextShift <> 0 then
    begin
      with TJvDockVCTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else
          ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
      end;
      if AfterVisibleZone.LimitSize < TJvDockVCTree(Tree).MinSize then
        AfterVisibleZone.ZoneLimit := AfterVisibleZone.LimitBegin + TJvDockVCTree(Tree).MinSize;
    end;
  end
  else
  begin
    with TJvDockVCTree(Tree) do
    begin
      TempSize := DockHeightWidth[DockSiteOrientation] - BorderSize;

      if BeforeVisibleZone <> nil then
      begin
        if (Tree.TopZone.VisibleChildCount = 2) and Visibled then
          BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit + BorderSize;
        if Visibled then
          ZoneLimit := BeforeVisibleZone.ZoneLimit + TempSize
        else
          ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize + BorderSize;

        TempSize := ZoneLimit;
      end;

      if AfterVisibleZone <> nil then
      begin
        if Visibled then
          ZoneLimit := LimitBegin + TempSize
        else
          ZoneLimit := LimitBegin + DockSize - BorderSize;

        ShiftBy := ZoneLimit;
        ShiftScaleOrientation := DockSiteOrientation;
        ForEachAt(AfterVisibleZone, ShiftZone, tskForward);
        TempSize := DockSiteSize + ZoneLimit - LimitBegin;
      end;
      Visibled := True;
      DockSiteSize := TempSize;
      TJvDockPanel(DockSite).DockServer.GetClientAlignControl(DockSite.Align);
    end;
  end;
  Visibled := True;
end;

procedure TJvDockVCZone.InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean);
var
  PrevShift, NextShift: Integer;
  TempSize: Integer;
  BorderWidth: Integer;
begin
  if not Insert then
    Visibled := False;

  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) and (ParentZone <> Tree.TopZone) then
  begin
    if Insert then
      TempSize := ParentZone.VisibleSize
    else
      TempSize := ParentZone.LimitSize;

    ParentZone.InsertOrRemove(TempSize, Insert, Hide);
  end;
  if ParentZone = nil then
    Exit;

  if ParentZone.Orientation <> TJvDockVCTree(Tree).DockSiteOrientation then
  begin
    if TJvDockVCZone(ParentZone.ChildZones).ZoneSizeStyle in [zssMaximum, zssMinimum] then
    begin
      if Insert then
      begin
        ZoneSizeStyle := zssMinimum;
        TJvDockVCTree(Tree).ResetDockZoneSizeStyle(ParentZone, zssMaximum, nil);
      end
      else
      begin
        if ZoneSizeStyle = zssMinimum then
          TJvDockVCTree(Tree).ResetDockZoneSizeStyle(ParentZone, zssMaximum, Self)
        else
        if ZoneSizeStyle = zssMaximum then
        begin
          TJvDockVCTree(Tree).ResetDockZoneSizeStyle(ParentZone, zssNormal, Self);
          TJvDockVCZone(ParentZone).DoSetChildSizeStyle(zssNormal);
        end;
      end;
      Exit;
    end;

    case TJvDockVCTree(Tree).DockSiteOrientation of
      doVertical:
        TempSize := Tree.DockSite.Height;
      doHorizontal:
        TempSize := Tree.DockSite.Width;
    else
      raise Exception.CreateRes(@RsEInvalidDockSiteOrientationValue);
    end;

    if DockSize > TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVCTree(Tree).MinSize then
      DockSize := TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVCTree(Tree).MinSize;

    BorderWidth := TJvDockVCTree(Tree).BorderWidth;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderWidth;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    if BeforeClosestVisibleZone = nil then
    begin
      PrevShift := 0;
      NextShift := (2 * Ord(Insert) - 1) * (DockSize + BorderWidth);

      ZoneLimit := Ord(Insert) * (DockSize + BorderWidth);
      if ParentZone.VisibleChildCount = 2 then
        NextSibling.ZoneLimit := TempSize;
    end
    else
    if AfterClosestVisibleZone = nil then
    begin
      PrevShift := (2 * Ord(Insert) - 1) * (DockSize + BorderWidth);
      NextShift := 0;
      begin
        if ParentZone.ChildCount = 2 then
          PrevSibling.ZoneLimit := TempSize - Ord(Insert) * PrevShift
        else
          PrevSibling.ZoneLimit := PrevSibling.ZoneLimit - PrevShift;
      end;
      ZoneLimit := TempSize;
    end
    else
    begin
      PrevShift := (2 * Ord(Insert) - 1) * Round((PrevSibling.ZoneLimit) *
        (DockSize + BorderWidth) / (TempSize - Ord(not Insert) * (DockSize + BorderWidth)));
      NextShift := (2 * Ord(Insert) - 1) * DockSize - PrevShift;
      PrevSibling.ZoneLimit := PrevSibling.ZoneLimit - PrevShift;
      ZoneLimit := Ord(Insert) * (DockSize + BorderWidth) + PrevSibling.ZoneLimit;
    end;

    if PrevShift <> 0 then
    begin
      with TJvDockVCTree(Tree) do
      begin
        ReplacementZone := PrevSibling;
        try
          if PrevSibling.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (PrevSibling.ZoneLimit + PrevShift)
          else
            ScaleBy := 1;
          ShiftScaleOrientation := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;

      if PrevSibling.LimitSize < TJvDockVCTree(Tree).MinSize then
        PrevSibling.ZoneLimit := PrevSibling.LimitBegin + TJvDockVCTree(Tree).MinSize;
    end;

    if NextShift <> 0 then
    begin
      with TJvDockVCTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else
          ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(NextSibling, ScaleSiblingZone, tskForward);
      end;
    end;

    ParentZone.Update;
  end
  else
  begin
    with TJvDockVCTree(Tree) do
    begin
      if Insert then
      begin
        if AfterClosestVisibleZone = nil then
        begin
          ZoneLimit := LimitBegin + DockHeightWidth[DockSiteOrientation];
          DockSiteSize := ZoneLimit;
        end
        else
        if BeforeClosestVisibleZone = nil then
        begin
          ZoneLimit := DockHeightWidth[DockSiteOrientation] + BorderWidth;
          ShiftBy := ZoneLimit;
          ShiftScaleOrientation := DockSiteOrientation;
          ForEachAt(AfterClosestVisibleZone, ShiftZone, tskForward);
          DockSiteSize := DockSiteSize + ZoneLimit;
        end;

        TJvDockPanel(DockSite).DockServer.GetClientAlignControl(DockSite.Align);
      end
      else
      begin
        ZoneLimit := LimitBegin;
        ShiftBy := -DockSize;
        ShiftScaleOrientation := DockSiteOrientation;

        if PrevSibling <> nil then
          DockSiteSize := DockSiteSize - DockSize - 5
        else
        if NextSibling <> nil then
        begin
          ForEachAt(NextSibling, ShiftZone, tskForward);
          DockSiteSize := DockSiteSize - DockSize;
        end;
      end;
    end;
  end;
  if Insert then
    Visibled := True;
end;

procedure TJvDockVCZone.Remove(DockSize: Integer; Hide: Boolean);
var
  PrevShift, NextShift: Integer;
  TempSize: Integer;
  BorderSize: Integer;
  BeforeVisibleZone, AfterVisibleZone: TJvDockZone;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 1) and (ParentZone <> Tree.TopZone) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := False;
    Exit;
  end;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone := AfterClosestVisibleZone;

  BorderSize := TJvDockVCTree(Tree).BorderWidth * Ord(AfterClosestVisibleZone = nil);

  if ParentZone.Orientation <> TJvDockVCTree(Tree).DockSiteOrientation then
  begin
    if ZoneSizeStyle in [zssMaximum, zssMinimum] then
    begin
      if ZoneSizeStyle = zssMinimum then
        TJvDockVCTree(Tree).ResetDockZoneSizeStyle(ParentZone, zssMaximum, Self)
      else
      if ZoneSizeStyle = zssMaximum then
      begin
        TJvDockVCTree(Tree).ResetDockZoneSizeStyle(ParentZone, zssNormal, Self);
        TJvDockVCZone(ParentZone).DoSetChildSizeStyle(zssNormal);
      end;
      Visibled := False;
      Exit;
    end;

    case TJvDockVCTree(Tree).DockSiteOrientation of
      doVertical:
        TempSize := Tree.DockSite.Height;
      doHorizontal:
        TempSize := Tree.DockSite.Width;
    else
      raise Exception.CreateRes(@RsEInvalidDockSiteOrientationValue);
    end;

    if DockSize > TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVCTree(Tree).MinSize then
      DockSize := TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVCTree(Tree).MinSize;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    Visibled := False;

    if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
      Exit;

    if BeforeVisibleZone = nil then
    begin
      PrevShift := 0;
      NextShift := -DockSize + BorderSize;
      ZoneLimit := -DockSize + BorderSize;
    end
    else
    if AfterVisibleZone = nil then
    begin
      PrevShift := -DockSize + BorderSize;
      NextShift := 0;
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := TempSize;
    end
    else
    begin
      PrevShift := -Round((BeforeVisibleZone.ZoneLimit) *
        (DockSize + BorderSize) / (TempSize - (DockSize + BorderSize)));
      NextShift := -DockSize - PrevShift;
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := BeforeVisibleZone.ZoneLimit;
    end;

    if PrevShift <> 0 then
    begin
      with TJvDockVCTree(Tree) do
      begin
        ReplacementZone := BeforeVisibleZone;
        try
          if BeforeVisibleZone.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (BeforeVisibleZone.ZoneLimit + PrevShift)
          else
            ScaleBy := 1;
          ShiftScaleOrientation := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;

      if BeforeVisibleZone.LimitSize < TJvDockVCTree(Tree).MinSize then
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TJvDockVCTree(Tree).MinSize;
    end;

    if NextShift <> 0 then
    begin
      with TJvDockVCTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else
          ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
      end;
      if AfterVisibleZone.LimitSize < TJvDockVCTree(Tree).MinSize then
        AfterVisibleZone.ZoneLimit := AfterVisibleZone.LimitBegin + TJvDockVCTree(Tree).MinSize;
    end;
  end
  else
  begin
    Visibled := False;
    with TJvDockVCTree(Tree) do
    begin
      ZoneLimit := LimitBegin - BorderSize;
      ShiftBy := -DockSize - BorderSize;
      ShiftScaleOrientation := DockSiteOrientation;

      if BeforeClosestVisibleZone <> nil then
        DockSiteSize := DockSiteSize - DockSize - BorderSize
      else
      if AfterClosestVisibleZone <> nil then
      begin
        ForEachAt(AfterClosestVisibleZone, ShiftZone, tskForward);
        DockSiteSize := DockSiteSize - DockSize - BorderSize;
      end;
    end;
  end;
end;

procedure TJvDockVCZone.SetZoneSize(Size: Integer; Show: Boolean);
begin
  inherited SetZoneSize(Size, Show);
end;

procedure TJvDockVCTree.SyncWithStyle;
begin
  inherited SyncWithStyle;
  if DockStyle.ConjoinServerOption is TJvDockVCConjoinServerOption then
    BorderWidth := TJvDockVCConjoinServerOption(DockStyle.ConjoinServerOption).BorderWidth;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
