{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollMax.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvScrollMax
description : scrollable panels

History:
  1.20:
    - first version;
  2.00:
    - new property ScrollbarVisible;
Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}
// $Id$

unit JvScrollMax;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Forms, ExtCtrls, Controls, Buttons,
  JvButtons, JvComponent;

const
  CM_PARENTBEVELEDCHANGED = WM_USER + 1;
  CM_PARENTBUTTONFONTCHANGED = WM_USER + 2;
  CM_PARENTBUTTONVISIBLECHANGED = WM_USER + 3;

type
  TOnCanExpand = procedure(Sender: TObject; var CanExpand: Boolean) of object;
  TOnCanCollapse = procedure(Sender: TObject; var CanCollapse: Boolean) of object;

  TJvScrollMax = class;

  TJvScrollMaxBand = class(TJvCustomControl)
  private
    FData: Pointer;
    FExpandedHeight: Integer;
    FButton: TSpeedButton;
    FExpanded: Boolean;
    FOrder: Integer;
    FBeveled: Boolean;
    FBorderWidth: Integer;
    FParentBeveled: Boolean;
    FParentButtonFont: Boolean;
    FParentButtonVisible: Boolean;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FOnCanCollapse: TOnCanCollapse;
    FOnCanExpand: TOnCanExpand;
    procedure ButtonClick(Sender: TObject);
    procedure SetExpanded(const Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
    procedure SetParentBeveled(const Value: Boolean);
    procedure SetButtonFont(Value: TFont);
    function GetButtonFont: TFont;
    procedure SetBeveled(const Value: Boolean);
    procedure SetBorderWidth(const Value: Integer);
    function IsBeveledStored: Boolean;
    procedure SetParentButtonFont(const Value: Boolean);
    function IsButtonFontStored: Boolean;
    function GetButtonVisible: Boolean;
    procedure SetButtonVisible(const Value: Boolean);
    function IsButtonVisibleStored: Boolean;
    procedure SetParentButtonVisible(const Value: Boolean);
    procedure CMParentBeveledChanged(var Msg: TMessage); message CM_PARENTBEVELEDCHANGED;
    procedure CMParentButtonFontChanged(var Msg: TMessage); message CM_PARENTBUTTONFONTCHANGED;
    procedure CMParentButtonVisibleChanged(var Msg: TMessage); message CM_PARENTBUTTONVISIBLECHANGED;
  protected
    procedure TextChanged; override;
    procedure DoBoundsChanged; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure SetParent({$IFDEF VisualCLX} const {$ENDIF} AParent: TWinControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetZOrder(TopMost: Boolean); override;
    function ScrollMax: TJvScrollMax;
    procedure UpdateSize(ATop: Integer);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CollapsedHeight: Integer;
    procedure ChangeScale(M, D {$IFDEF VisualCLX}, MH, DH {$ENDIF}: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: Pointer read FData write FData;
  published
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Caption;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight;
    property Order: Integer read GetOrder write SetOrder stored False;
    property ButtonVisible: Boolean read GetButtonVisible write SetButtonVisible stored IsButtonVisibleStored;
    property ButtonFont: TFont read GetButtonFont write SetButtonFont stored IsButtonFontStored;
    property Beveled: Boolean read FBeveled write SetBeveled default True;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property ParentBeveled: Boolean read FParentBeveled write SetParentBeveled stored IsBeveledStored;
    property ParentButtonVisible: Boolean read FParentButtonVisible write SetParentButtonVisible default True;
    property ParentButtonFont: Boolean read FParentButtonFont write SetParentButtonFont default True;
    property OnResize;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnCanExpand: TOnCanExpand read FOnCanExpand write FOnCanExpand;
    property OnCanCollapse: TOnCanCollapse read FOnCanCollapse write FOnCanCollapse;
    property Left stored False;
    property Top stored False;
    property Width;
    property Height;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    {$IFDEF VCL}
    property BiDiMode;
    property ParentBiDiMode;
    {$ENDIF VCL}
  end;

  TJvScrollMaxBands = class(TJvCustomControl)
  private
    FScrolling: Boolean;
  protected
    procedure DoFocusChanged(Control: TWinControl); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure ScrollControls(const DeltaY: Integer);
    procedure Paint; override;
  end;

  TJvPanelScrollBar = class(TJvCustomPanel)
  private
    FMin: Integer;
    FMax: Integer;
    FPos: Integer;
    FPage: Integer;
    Scroll: TPanel;
    FDesignInteractive: Boolean;
    FInclusive: Boolean;
    FOnChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    procedure SetParam(Index, Value: Integer);
    procedure SetInclusive(Value: Boolean);
  protected
    {$IFDEF VCL}
    procedure CreateWnd; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure CreateWidget; override;
    {$ENDIF VisualCLX}
    procedure SetTrackBar;
    procedure Loaded; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(const AMin, AMax, APage, APos: Integer);
    property Pos: Integer index 3 read FPos write SetParam;
    property DesignInteractive: Boolean read FDesignInteractive write FDesignInteractive;
    property Scroller: TPanel read Scroll;
  published
    property Color;
    property Align;
    property Min: Integer index 0 read FMin write SetParam;
    property Max: Integer index 1 read FMax write SetParam;
    property Page: Integer index 2 read FPage write SetParam;
    property Position: Integer index 3 read FPos write SetParam;
    property Inclusive: Boolean read FInclusive write SetInclusive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;

  TJvScrollMax = class(TJvCustomPanel)
  private
    FPnlEdit: TJvScrollMaxBands;
    FScrollBar: TJvPanelScrollBar;
    FScrollPos: Integer;
    FY: Integer;
    FButtonFont: TFont;
    FOnScroll: TNotifyEvent;
    FBeveled: Boolean;
    FButtonVisible: Boolean;
    FAutoHeight: Boolean;
    FExpandedHeight: Integer;
    FOneExpanded: Boolean;
    procedure Correct;
    procedure CorrectHeight;
    procedure BandMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BandMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BandMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBarScroll(Sender: TObject);
    procedure SetButtonFont(Value: TFont);
    procedure ButtonFontChanged(Sender: TObject);
    function GetBand(Index: Integer): TJvScrollMaxBand;
    function GetBandCount: Integer;
    procedure SetScrollPos(const Value: Integer);
    procedure SetButtonVisible(const Value: Boolean);
    procedure SetBeveled(const Value: Boolean);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    function GetScrollBarWidth: Cardinal;
    procedure SetScrollBarWidth(const Value: Cardinal);
    function GetScrollBarVisible: Boolean;
    procedure SetScrollBarVisible(const Value: Boolean);
    procedure SetOneExpanded(const Value: Boolean);
  protected
    {$IFDEF JVCLThemesEnabled}
    procedure SetParentBackground(Value: Boolean); override;
    {$ENDIF JVCLThemesEnabled}
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildParent: TComponent; override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollInView(AControl: TControl);
    procedure MouseControls(AControls: array of TControl);
    procedure MouseClasses(AControlClasses: array of TControlClass);
    function AllCollapsed: Boolean;
    function AllExpanded: Boolean;
    procedure AddBand(Band: TJvScrollMaxBand);
    property BandCount: Integer read GetBandCount;
    property Bands[Index: Integer]: TJvScrollMaxBand read GetBand;
  published
    property ScrollPos: Integer read FScrollPos write SetScrollPos default 0;
    property BorderWidth default 3;
    property Beveled: Boolean read FBeveled write SetBeveled default True;
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible default True;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight default -1;
    property ScrollBarWidth: Cardinal read GetScrollBarWidth write SetScrollBarWidth default 7;
    property ScrollBarVisible: Boolean read GetScrollBarVisible write SetScrollBarVisible default True;
    property OneExpanded: Boolean read FOneExpanded write SetOneExpanded default False;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Color;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  public
    {$IFDEF VCL}
    property DockManager;
    {$ENDIF VCL}
  published
    property Anchors;
    //property AutoSize;
    property Constraints;
    {$IFDEF VCL}
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
    {$ENDIF VCL}
  end;

  EJvScrollMaxError = class(Exception);

var
  crRAHand: Integer;
  crRAHandMove: Integer;

implementation

uses
  JvDsgnIntf, JvJVCLUtils, JvConsts, JvThemes, JvResources;

{ Cursors resources }
{$IFDEF MSWINDOWS}
{$R ..\Resources\JvScrollMax.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvScrollMax.res}
{$ENDIF LINUX}

function PanelBorder(Panel: TCustomPanel): Integer;
begin
  Result := TPanel(Panel).BorderWidth;
  if TPanel(Panel).BevelOuter <> bvNone then
    Inc(Result, TPanel(Panel).BevelWidth);
  if TPanel(Panel).BevelInner <> bvNone then
    Inc(Result, TPanel(Panel).BevelWidth);
end;

{ function DefineCursor was typed from
  book "Secrets of Delphi 2" by Ray Lischner }

{ (rom) deactivated  see end of file
function DefineCursor(Identifier: PChar): TCursor;
var
  Handle: HCURSOR;
begin
  Handle := LoadCursor(HInstance, Identifier);
  if Handle = 0 then
    raise EOutOfResources.CreateRes(@RsECannotLoadCursorResource);
  for Result := 1 to High(TCursor) do
    if Screen.Cursors[Result] = Screen.Cursors[crDefault] then
    begin
      Screen.Cursors[Result] := Handle;
      Exit;
    end;
  raise EOutOfResources.CreateRes(@RsETooManyUserdefinedCursors);
end;
}

//=== { TJvScroller } ========================================================

type
  TJvScroller = class(TPanel)
  private
    FY: Integer;
    {$IFDEF VCL}
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF VCL}
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  end;

procedure TJvScroller.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FY := Y;
end;

procedure TJvScroller.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Sm, T, OldPos: Integer;
begin
  if Shift = [ssLeft] then
  begin
    Sm := FY - Y;
    T := Top;
    if Sm <> 0 then
    begin
      with Parent as TJvPanelScrollBar do
      begin
        OldPos := Pos;
        Pos := Pos - Round(Sm * (FMax - FMin + 1) / ClientHeight);
        if (Pos <> OldPos) and Assigned(FOnScroll) then
          FOnScroll(Parent);
      end;
    end;
    FY := Y - Top + T;
  end;
end;

{$IFDEF VCL}
procedure TJvScroller.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  with (Owner as TJvPanelScrollBar) do
    Msg.Result := Integer(FDesignInteractive and (FPage <> FMax - FMin + 1));
end;
{$ENDIF VCL}

//=== { TJvPanelScrollBar } ==================================================

constructor TJvPanelScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvLowered;
  Color := clAppWorkSpace;
  Caption := '';
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Scroll := TJvScroller.Create(Self);
  Scroll.Parent := Self;
  Scroll.Caption := '';
  Scroll.ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  FMax := 100;
  FPage := 10;
  Width := 20;
  Height := 100;
end;

procedure TJvPanelScrollBar.Loaded;
begin
  inherited Loaded;
  Resize;
end;

procedure TJvPanelScrollBar.Resize;
begin
  inherited Resize;
  with Scroll do
  begin
    Top := BevelWidth;
    Left := BevelWidth;
    Width := Self.Width - 2 * BevelWidth;
  end;
  SetTrackBar;
end;

procedure TJvPanelScrollBar.SetTrackBar;
var
  CH, H, T: Integer;
  L, FP, P, P1: Integer;
begin
  { Before change of the code necessarily make a copy! [translated] }
  if FMin > FMax then
    FMin := FMax;
  if FPage > FMax - FMin + 1 then
    FPage := FMax - FMin + 1;
  if FInclusive then
    P := FPage
  else
    P := 0;
  P1 := FPage - P;
  if FPos > FMax - P then
    FPos := FMax - P;
  if FPos < FMin then
    FPos := FMin;
  L := FMax - FMin + 1;
  CH := Height - 2 * BevelWidth;
  H := Trunc(CH * FPage / L) + 1;
  FP := Trunc((FPos - FMin) / L * (L - P1)) + 1;
  T := Round(CH * FP / L);
  if H < 7 then
    H := 7;
  if H > CH then
    H := CH;
  if T < BevelWidth then
    T := BevelWidth;
  if T + H > Height - BevelWidth then
    T := Height - BevelWidth - H;
  if FPos = FMax - P then
    T := Height - BevelWidth - H;

  with Scroll do
    SetBounds(Left, T, Width, H);
end;

procedure TJvPanelScrollBar.SetParam(Index, Value: Integer);
begin
  case Index of
    0:
      FMin := Value;
    1:
      FMax := Value;
    2:
      FPage := Value;
    3:
      FPos := Value;
  end;
  SetParams(FMin, FMax, FPage, FPos);
end;

procedure TJvPanelScrollBar.SetParams(const AMin, AMax, APage, APos: Integer);
begin
  FMin := AMin;
  FMax := AMax;
  FPage := APage;
  FPos := APos;
  if Assigned(FOnChange) then
    FOnChange(Self);
  SetTrackBar;
end;

procedure TJvPanelScrollBar.SetInclusive(Value: Boolean);
begin
  FInclusive := Value;
  SetTrackBar;
end;

{$IFDEF VCL}
procedure TJvPanelScrollBar.CreateWnd;
begin
  inherited CreateWnd;
  SetTrackBar;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvPanelScrollBar.CreateWidget;
begin
  inherited CreateWidget;
  SetTrackBar;
end;
{$ENDIF VisualCLX}

//=== { TJvBandBtn } =========================================================

type
  TJvBandBtn = class(TJvNoFrameButton)
  private
    {$IFDEF VCL}
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF VCL}
  protected
    procedure FontChanged; override;
  end;

{$IFDEF VCL}
procedure TJvBandBtn.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := 1;
end;
{$ENDIF VCL}

procedure TJvBandBtn.FontChanged;
begin
  inherited FontChanged;
  if Parent <> nil then
    with Parent as TJvScrollMaxBand do
    begin
      FParentButtonFont := False;
      Canvas.Font := Self.Font;
      // (rom) please check this change
      //FButton.Height := Canvas.TextHeight('W') + 4;
      FButton.Height := CanvasMaxTextHeight(Canvas) + 4;
      Invalidate;
    end;
end;

//=== { TJvScrollMaxBand } ===================================================

constructor TJvScrollMaxBand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption, csAcceptsControls];
  IncludeThemeStyle(Self, [csParentBackground]);
  Height := 50;
  FExpandedHeight := 50;
  ParentColor := True;
  FParentButtonFont := True;
  FParentButtonVisible := True;
  FParentBeveled := True;
  FButton := TJvBandBtn.Create(Self);
  with FButton as TJvBandBtn do
  begin
    SetDesigning(False);
    Parent := Self;
    Top := 2;
    Left := 4;
    Cursor := crArrow;
    OnClick := ButtonClick;
    Margin := 4;
    Spacing := -1;
    NoBorder := False;
    ParentColor := True;
    {o}
    {$IFDEF VCL}
    FButton.ParentBiDiMode := True;
    {$ENDIF VCL}
  end;
  Expanded := True;
end;

destructor TJvScrollMaxBand.Destroy;
begin
  inherited Destroy;
end;

procedure TJvScrollMaxBand.Loaded;
begin
  inherited Loaded;
  Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
end;

procedure TJvScrollMaxBand.DoBoundsChanged;
begin
  if FExpanded then
    ExpandedHeight := Height;
  inherited DoBoundsChanged;
  if Parent <> nil then
    ScrollMax.CorrectHeight;
end;

procedure TJvScrollMaxBand.TextChanged;
begin
  inherited TextChanged;
  FButton.Caption := Caption;
end;

procedure TJvScrollMaxBand.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    if FExpanded then
      FButton.Glyph.LoadFromResourceName(HInstance, 'RASCROLLMAXBTNMINUS')
    else
      FButton.Glyph.LoadFromResourceName(HInstance, 'RASCROLLMAXBTNPLUS');
    if FExpanded and Assigned(FOnExpand) then
      FOnExpand(Self);
    if not FExpanded and Assigned(FOnCollapse) then
      FOnCollapse(Self);
    RequestAlign;
    if Parent <> nil then
      ScrollMax.CorrectHeight;
   { if not (csLoading in ComponentState) and (ScrollMax <> nil) then
      DesignerModified(ScrollMax); }
  end;
end;

procedure TJvScrollMaxBand.SetExpandedHeight(const Value: Integer);
begin
  if FExpandedHeight <> Value then
  begin
    FExpandedHeight := Value;
    if FExpanded then
      Height := FExpandedHeight;
     // RequestAlign - called from SetHeight
  end;
end;

function TJvScrollMaxBand.GetOrder: Integer;
var
  I: Integer;
begin
  Result := FOrder;
  if Parent <> nil then
  begin
    for I := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[I] = Self then
      begin
        Result := I;
        Break;
      end;
  end;
end;

procedure TJvScrollMaxBand.SetOrder(const Value: Integer);
begin
  if FOrder <> Value then
  begin
    if Parent <> nil then
      TJvScrollMaxBands(Parent).SetChildOrder(Self, Value);
    FOrder := GetOrder;
    RequestAlign;
  end;
end;

function TJvScrollMaxBand.GetButtonFont: TFont;
begin
  Result := FButton.Font;
end;

procedure TJvScrollMaxBand.SetButtonFont(Value: TFont);
begin
  FButton.Font := Value;
end;

procedure TJvScrollMaxBand.SetParentButtonFont(const Value: Boolean);
begin
  if FParentButtonFont <> Value then
  begin
    FParentButtonFont := Value;
    if Parent <> nil then
      Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.CMParentButtonFontChanged(var Msg: TMessage);
begin
  if FParentButtonFont then
  begin
    if ScrollMax <> nil then
      SetButtonFont(ScrollMax.FButtonFont);
    FParentButtonFont := True;
  end;
end;

function TJvScrollMaxBand.IsButtonFontStored: Boolean;
begin
  Result := not ParentButtonFont;
end;

function TJvScrollMaxBand.GetButtonVisible: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TJvScrollMaxBand.SetButtonVisible(const Value: Boolean);
begin
  if FButton.Visible <> Value then
  begin
    FParentButtonVisible := False;
    FButton.Visible := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;

function TJvScrollMaxBand.IsButtonVisibleStored: Boolean;
begin
  Result := not ParentButtonVisible;
end;

procedure TJvScrollMaxBand.SetParentButtonVisible(const Value: Boolean);
begin
  if FParentButtonVisible <> Value then
  begin
    FParentButtonVisible := Value;
    if Parent <> nil then
      Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.CMParentButtonVisibleChanged(var Msg: TMessage);
begin
  if FParentButtonVisible then
  begin
    if ScrollMax <> nil then
      SetButtonVisible(ScrollMax.FButtonVisible);
    FParentButtonVisible := True;
  end;
end;

procedure TJvScrollMaxBand.SetBeveled(const Value: Boolean);
begin
  if FBeveled <> Value then
  begin
    FParentBeveled := False;
    FBeveled := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;

function TJvScrollMaxBand.IsBeveledStored: Boolean;
begin
  Result := not ParentBeveled;
end;

procedure TJvScrollMaxBand.SetParentBeveled(const Value: Boolean);
begin
  if FParentBeveled <> Value then
  begin
    FParentBeveled := Value;
    if Parent <> nil then
      Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.CMParentBeveledChanged(var Msg: TMessage);
begin
  if FParentBeveled then
  begin
    if ScrollMax <> nil then
      SetBeveled(ScrollMax.FBeveled);
    FParentBeveled := True;
  end;
end;

procedure TJvScrollMaxBand.ButtonClick(Sender: TObject);
var
  E: Boolean;
begin
  E := True;
  if FExpanded then
  begin
    if Assigned(FOnCanCollapse) then
      FOnCanCollapse(Self, E);
  end
  else
  if Assigned(FOnCanExpand) then
    FOnCanExpand(Self, E);
  if E then
    Expanded := not FExpanded;
  DesignerModified(Self);
end;

procedure TJvScrollMaxBand.SetParent({$IFDEF VisualCLX} const {$ENDIF} AParent: TWinControl);
begin
  if not ((AParent is TJvScrollMaxBands) or (AParent = nil)) then
    raise EJvScrollMaxError.CreateRes(@RsETJvScrollMaxBandCanBePutOnlyIntoTJv);
  inherited SetParent(AParent);
  if not (csLoading in ComponentState) then
  begin
    Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
    Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
    Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.SetZOrder(TopMost: Boolean);
begin
  inherited SetZOrder(TopMost);
  RequestAlign;
end;

procedure TJvScrollMaxBand.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  ScrollMax.BandMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvScrollMaxBand.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  ScrollMax.BandMouseMove(Self, Shift, X, Y);
end;

procedure TJvScrollMaxBand.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  ScrollMax.BandMouseUp(Self, Button, Shift, X, Y);
end;

function TJvScrollMaxBand.ScrollMax: TJvScrollMax;
begin
  if (Parent <> nil) and ((Parent as TJvScrollMaxBands).Parent <> nil) then
    Result := (Parent as TJvScrollMaxBands).Parent as TJvScrollMax
  else
    Result := nil;
end;

function TJvScrollMaxBand.CollapsedHeight: Integer;
begin
  Result := FButton.BoundsRect.Bottom + FButton.Top;
end;

procedure TJvScrollMaxBand.UpdateSize(ATop: Integer);
var
  W, H: Integer;
begin
  if FExpanded then
    H := FExpandedHeight
  else
    H := CollapsedHeight;
  if ScrollMax <> nil then
  begin
    W := Parent.Width;
    if ScrollMax.ScrollBarVisible then
      W := W - 3;
  end
  else
    W := Width;
  SetBounds(0, ATop, W, H);
  if FBeveled then
    FButton.Left := 16
  else
    FButton.Left := 4;
  FButton.Width := Width - FButton.Left * 2;
end;

procedure TJvScrollMaxBand.Paint;
const
  Ex: array [Boolean] of Integer = (BF_TOP, BF_RECT);
var
  R: TRect;
begin
  {$IFDEF VCL}
  if Canvas.Handle <> 0 then
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Canvas.Handle <> nil then
  {$ENDIF VisualCLX}
  begin
    if csDesigning in ComponentState then
      DrawDesignFrame(Canvas, ClientRect);
    if FBeveled then
    begin
      R.Left := 1;
      if ButtonVisible then
        R.Top := FButton.Top + FButton.Height div 2
      else
        R.Top := 1;
      R.Right := Width - R.Left;
      R.Bottom := Height - 1;
      {$IFDEF VisualCLX} QWindows.{$ENDIF}DrawEdge(Canvas.Handle, R, EDGE_ETCHED, Ex[FExpanded]);
      if ButtonVisible then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(Bounds(FButton.Left - 2, R.Top, FButton.Width + 4, 2));
      end;
    end;
  end;
end;

procedure TJvScrollMaxBand.AlignControls(AControl: TControl; var Rect: TRect);
var
  BevelSize: Integer;
begin
  BevelSize := FBorderWidth;
  if FBeveled then
    Inc(BevelSize, 3);
  InflateRect(Rect, -BevelSize, -BevelSize);
  if ButtonVisible then
  begin
    Inc(Rect.Top, FButton.Height);
    if FButton.Top > FBorderWidth then
      Inc(Rect.Top, FButton.Top);
  end;
  inherited AlignControls(AControl, Rect);
end;

procedure TJvScrollMaxBand.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Realign;
  end;
end;

procedure TJvScrollMaxBand.ChangeScale(M, D {$IFDEF VisualCLX}, MH, DH {$ENDIF}: Integer);
begin
  inherited ChangeScale(M, D {$IFDEF VisualCLX}, MH, DH {$ENDIF});
  ExpandedHeight := FExpandedHeight * M div D;
end;

//=== { TJvScrollMaxBands } ==================================================

procedure TJvScrollMaxBands.AlignControls(AControl: TControl; var Rect: TRect);
var
  I: Integer;
  ScrollMax: TJvScrollMax;
  T: Integer;
  SMax, SPage, SPos: Integer;

  procedure AdjustBottom;
  begin
    if (Controls[ControlCount - 1].BoundsRect.Bottom < Height) and
      (Controls[0].Top < 0) then
    begin
      if Height - (Controls[ControlCount - 1].BoundsRect.Bottom - Controls[0].Top) > 0 then
        ScrollControls(-Controls[0].Top)
      else
        ScrollControls(Height - Controls[ControlCount - 1].BoundsRect.Bottom);
    end;
  end;

  procedure AdjustBand;
  var
    Band: TJvScrollMaxBand;
  begin
    Band := AControl as TJvScrollMaxBand;
    if (Band <> nil) and Band.FExpanded and
      (Band.BoundsRect.Bottom > Height) and
      (Band.Top > 0) and
      not (csLoading in Band.ComponentState) then
    begin
      ScrollControls(Height - Band.BoundsRect.Bottom);
    end;
  end;

  procedure SetCursor;
  var
    I: Integer;
    Cursor: TCursor;
  begin
    if (Controls[ControlCount - 1].BoundsRect.Bottom > ClientHeight) or
      (Controls[0].Top < 0) then
      Cursor := crRAHand
    else
      Cursor := crDefault;
    for I := 0 to ControlCount - 1 do
      Controls[I].Cursor := Cursor;
  end;

begin
  if FScrolling then
    Exit;
  if (Parent <> nil) and (csLoading in Parent.ComponentState) then
    Exit;
  ScrollMax := Parent as TJvScrollMax;
  if (AControl <> nil) and
    (AControl as TJvScrollMaxBand).FExpanded and
    ScrollMax.FOneExpanded then
    for I := 0 to ControlCount - 1 do
      if not (Controls[I] is TJvScrollMaxBand) then
        raise EJvScrollMaxError.CreateRes(@RsETJvScrollMaxCanContainOnlyTJvScroll)
      else
      if Controls[I] <> AControl then
        (Controls[I] as TJvScrollMaxBand).Expanded := False;
  SPos := ScrollMax.FScrollPos;
  if ControlCount > 0 then
  begin
    for I := 0 to ControlCount - 1 do
    begin
      if not (Controls[I] is TJvScrollMaxBand) then
        raise EJvScrollMaxError.CreateRes(@RsETJvScrollMaxCanContainOnlyTJvScroll);
      if I > 0 then
        T := Controls[I - 1].BoundsRect.Bottom
      else
        T := -ScrollMax.FScrollPos;
      (Controls[I] as TJvScrollMaxBand).UpdateSize(T);
    end;
    AdjustBottom;
    AdjustBand;
    SMax := Controls[ControlCount - 1].BoundsRect.Bottom - Controls[0].Top;
    SPos := -Controls[0].Top;
    ScrollMax.FScrollPos := SPos;
    SetCursor;
  end
  else
    SMax := Height;
  SPage := Height;
  ScrollMax.FScrollBar.SetParams(0, SMax, SPage, SPos);
end;

procedure TJvScrollMaxBands.ScrollControls(const DeltaY: Integer);
begin
  FScrolling := True;
  try
    ScrollBy(0, DeltaY);
  finally
    FScrolling := False;
  end;
end;

procedure TJvScrollMaxBands.DoFocusChanged(Control: TWinControl);
begin
  inherited DoFocusChanged(Control);
  if (Control <> nil) and
    ContainsControl(Control) and
    (Parent <> nil) then
    (Parent as TJvScrollMax).ScrollInView(Control);
end;

procedure TJvScrollMaxBands.Paint;
var
  R: TRect;
  S1: string;
begin
  if (csDesigning in ComponentState) and
    (ControlCount = 0) and
    {$IFDEF VCL}
    (Canvas.Handle <> 0) then
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    (Canvas.Handle <> nil) then
    {$ENDIF VisualCLX}
  begin
    R := ClientRect;
    Canvas.Font.Color := clAppWorkSpace;
    S1 := RsRightClickAndChooseAddBand;
    {$IFDEF VCL}
    DrawText(Canvas.Handle, PChar(S1),
      -1, R, DT_WORDBREAK {or DT_CENTER or DT_VCENTER});
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    DrawText(Canvas, S1,
      -1, R, DT_WORDBREAK {or DT_CENTER or DT_VCENTER});
    {$ENDIF VisualCLX}
  end;
end;

//=== { TJvScrollMax } =======================================================

constructor TJvScrollMax.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Caption := '';
  Width := 250;
  Height := 150;
  BorderWidth := 3;
  FExpandedHeight := -1;
  FButtonFont := TFont.Create;
  FButtonFont.Name := 'Small Fonts';
  FButtonFont.Size := 7;
  FButtonFont.OnChange := ButtonFontChanged;
  FButtonVisible := True;
  FBeveled := True;
  ParentColor := True;
  FPnlEdit := TJvScrollMaxBands.Create(Self);
  with FPnlEdit do
  begin
    Align := alClient;
    Parent := Self;
    ControlStyle := ControlStyle + [csAcceptsControls];
    ParentColor := True;
  end;
  FScrollBar := TJvPanelScrollBar.Create(Self);
  with FScrollBar do
  begin
    Inclusive := True;
    Parent := Self;
    Width := 7;
    Align := alRight;
    Max := FPnlEdit.Height;
    Page := Self.Height;
    OnScroll := ScrollBarScroll;
    ParentColor := True;
    Visible := True;
    DesignInteractive := True;
  end;
  {$IFDEF JVCLThemesEnabled}
  // (ahuser) FPnlEdit and FScrollBar must be created
  ParentBackground := True;
  {$ENDIF JVCLThemesEnabled}
end;

destructor TJvScrollMax.Destroy;
begin
  FButtonFont.Free;
  inherited Destroy;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvScrollMax.SetParentBackground(Value: Boolean);
begin
  inherited SetParentBackground(Value);
  FPnlEdit.ParentBackground := Value;
  FScrollBar.ParentBackground := Value;
end;
{$ENDIF JVCLThemesEnabled}

{$IFDEF VCL}
procedure TJvScrollMax.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    ExStyle := ExStyle or WS_EX_CONTROLPARENT;
  end;
end;
{$ENDIF VCL}

procedure TJvScrollMax.Loaded;
begin
  inherited Loaded;
  Resize;
  FPnlEdit.Realign;
end;

procedure TJvScrollMax.SetButtonFont(Value: TFont);
begin
  FButtonFont.Assign(Value);
end;

procedure TJvScrollMax.SetButtonVisible(const Value: Boolean);
begin
  if FButtonVisible <> Value then
  begin
    FButtonVisible := Value;
    FPnlEdit.NotifyControls(CM_PARENTBUTTONVISIBLECHANGED);
  end;
end;

procedure TJvScrollMax.SetBeveled(const Value: Boolean);
begin
  if FBeveled <> Value then
  begin
    FBeveled := Value;
    FPnlEdit.NotifyControls(CM_PARENTBEVELEDCHANGED);
  end;
end;

procedure TJvScrollMax.ButtonFontChanged(Sender: TObject);
begin
  FPnlEdit.NotifyControls(CM_PARENTBUTTONFONTCHANGED);
end;

procedure TJvScrollMax.MouseControls(AControls: array of TControl);
var
  I: Integer;
begin
  for I := Low(AControls) to High(AControls) do
  begin
    TJvScrollMax(AControls[I]).OnMouseDown := BandMouseDown;
    TJvScrollMax(AControls[I]).OnMouseMove := BandMouseMove;
    TJvScrollMax(AControls[I]).OnMouseUp := BandMouseUp;
  end;
end;

procedure TJvScrollMax.MouseClasses(AControlClasses: array of TControlClass);
var
  I, iB, iC: Integer;
begin
  for I := Low(AControlClasses) to High(AControlClasses) do
    for iB := 0 to BandCount - 1 do
      for iC := 0 to Bands[iB].ControlCount - 1 do
        if Bands[iB].Controls[iC] is AControlClasses[I] then
        begin
          TJvScrollMax(Bands[iB].Controls[iC]).OnMouseDown := BandMouseDown;
          TJvScrollMax(Bands[iB].Controls[iC]).OnMouseMove := BandMouseMove;
          TJvScrollMax(Bands[iB].Controls[iC]).OnMouseUp := BandMouseUp;
        end;
end;

procedure TJvScrollMax.Correct;
var
  Sm: Integer;
  CH: Integer;
begin
  if BandCount > 0 then
  begin
    Sm := 0;
    CH := FPnlEdit.Height;
    if (Bands[BandCount - 1].BoundsRect.Bottom < CH) and (Bands[0].Top < 0) then
      Sm := (CH - Bands[BandCount - 1].BoundsRect.Bottom);
    if Bands[0].Top + Sm > 0 then
      Sm := -Bands[0].Top;
    if Sm <> 0 then
    begin
      FPnlEdit.ScrollControls(Sm);
      FScrollBar.Pos := -Bands[0].Top;
      FScrollPos := FScrollBar.Pos;
    end;
  end;
end;

procedure TJvScrollMax.BandMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CH: Integer;
begin
  if (Button = mbLeft) and (BandCount > 0) then
  begin
    FY := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := FPnlEdit.Height;
    if (Bands[BandCount - 1].BoundsRect.Bottom > CH) or
      (Bands[0].Top < 0) then
      Screen.Cursor := crRAHandMove
    else
      Screen.Cursor := crDefault;
  end;
end;

procedure TJvScrollMax.BandMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Sm: Integer;
  CH: Integer;
begin
  if (ssLeft in Shift) and (BandCount > 0) then
  begin
    Y := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := FPnlEdit.Height;
    if not (Sender = FScrollBar.Scroller) then
      Sm := Y - FY
    else
      Sm := FY - Y;
    if Sm < 0 then {Up}
    begin
      if not (Bands[BandCount - 1].BoundsRect.Bottom > CH) then
        Sm := 0
      else
      if Bands[BandCount - 1].BoundsRect.Bottom + Sm < CH then
        Sm := CH - Bands[BandCount - 1].BoundsRect.Bottom;
    end
    else
    if Sm > 0 then {Down}
    begin
      if not (Bands[0].Top < 0) then
        Sm := 0
      else
      if Bands[0].Top + Sm > 0 then
        Sm := -Bands[0].Top;
    end;
    if Sm <> 0 then
    begin
      FPnlEdit.ScrollControls(Sm);
      FScrollBar.Pos := -Bands[0].Top;
      FScrollPos := FScrollBar.Pos;
    end;
    FY := Y;
    Correct;
  end;
end;

procedure TJvScrollMax.BandMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

function TJvScrollMax.GetBand(Index: Integer): TJvScrollMaxBand;
begin
  Result := TJvScrollMaxBand(FPnlEdit.Controls[Index]);
end;

function TJvScrollMax.GetBandCount: Integer;
begin
  Result := FPnlEdit.ControlCount;
end;

procedure TJvScrollMax.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FPnlEdit.GetChildren(Proc, Root);
end;

function TJvScrollMax.GetChildParent: TComponent;
begin
  Result := FPnlEdit;
end;

procedure TJvScrollMax.SetScrollPos(const Value: Integer);
begin
  if FScrollPos <> Value then
  begin
    FScrollPos := Value;
    if not (csLoading in ComponentState) then
    begin
      if FScrollPos > FScrollBar.Max - FScrollBar.Page then
        FScrollPos := FScrollBar.Max - FScrollBar.Page;
      if FScrollPos < 0 then
        FScrollPos := 0;
      DesignerModified(Self);
      FPnlEdit.Realign;
    end;
  end;
end;

procedure TJvScrollMax.ScrollBarScroll(Sender: TObject);
begin
  ScrollPos := FScrollBar.Pos;
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TJvScrollMax.ScrollInView(AControl: TControl);
var
  I: Integer;
  Band: TJvScrollMaxBand;
  Rect: TRect;
begin
  Band := nil;
  for I := 0 to FPnlEdit.ControlCount - 1 do
    if (FPnlEdit.Controls[I] as TJvScrollMaxBand).ContainsControl(AControl) then
    begin
      Band := FPnlEdit.Controls[I] as TJvScrollMaxBand;
      Break;
    end;
  if Band = nil then
    raise EJvScrollMaxError.CreateResFmt(@RsEControlsNotAChildOfs, [AControl.Name, Parent.Name]);
  Band.Expanded := True;
  Rect := AControl.ClientRect;
  Dec(Rect.Top, BevelWidth + BorderWidth + 4);
  Inc(Rect.Bottom, BevelWidth + BorderWidth + 4);
  Rect.TopLeft := ScreenToClient(AControl.ClientToScreen(Rect.TopLeft));
  Rect.BottomRight := ScreenToClient(AControl.ClientToScreen(Rect.BottomRight));
  if Rect.Top < 0 then
    ScrollPos := ScrollPos + Rect.Top
  else
  if Rect.Bottom > ClientHeight then
  begin
    if Rect.Bottom - Rect.Top > ClientHeight then
      Rect.Bottom := Rect.Top + ClientHeight;
    ScrollPos := ScrollPos + Rect.Bottom - ClientHeight;
  end;
end;

procedure TJvScrollMax.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if FAutoHeight then
      CorrectHeight;
  end;
end;

procedure TJvScrollMax.SetExpandedHeight(const Value: Integer);
begin
  if FExpandedHeight <> Value then
  begin
    FExpandedHeight := Value;
    if FAutoHeight then
      CorrectHeight;
  end;
end;

procedure TJvScrollMax.Resize;
begin
  inherited Resize;
  if FAutoHeight and (BandCount > 0) and
    not AllCollapsed and (FExpandedHeight > -1) then
    FExpandedHeight := Height;
  if FAutoHeight then
    CorrectHeight;
end;

procedure TJvScrollMax.CorrectHeight;
var
  I, H: Integer;
  Band: TJvScrollMaxBand;
begin
  if not FAutoHeight or (BandCount = 0) then
    Exit;
  if AllCollapsed then
  begin
    H := 0;
    for I := 0 to BandCount - 1 do
      Inc(H, Bands[I].Height);
    ClientHeight := H + 2 * PanelBorder(Self);
  end
  else
  if FExpandedHeight <> -1 then
    Height := FExpandedHeight
  else
  begin
    H := 0;
    Band := nil;
    for I := 0 to BandCount - 1 do
      if Bands[I].Height > H then
      begin
        Band := Bands[I];
        H := Band.Height;
      end;
    H := 0;
    for I := 0 to BandCount - 1 do
      if Bands[I] = Band then
        Inc(H, Bands[I].Height)
      else
        Inc(H, Bands[I].CollapsedHeight);
    ClientHeight := H + 2 * PanelBorder(Self);
  end;
end;

function TJvScrollMax.AllCollapsed: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to BandCount - 1 do
    if Bands[I].Expanded then
      Exit;
  Result := True;
end;

function TJvScrollMax.AllExpanded: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to BandCount - 1 do
    if not Bands[I].Expanded then
      Exit;
  Result := True;
end;

procedure TJvScrollMax.AddBand(Band: TJvScrollMaxBand);
begin
  Band.Parent := GetChildParent as TWinControl;
end;

function TJvScrollMax.GetScrollBarWidth: Cardinal;
begin
  Result := FScrollBar.Width;
end;

procedure TJvScrollMax.SetScrollBarWidth(const Value: Cardinal);
begin
  if Value >= 4 then
    FScrollBar.Width := Value;
end;

function TJvScrollMax.GetScrollBarVisible: Boolean;
begin
  Result := FScrollBar.Visible;
end;

procedure TJvScrollMax.SetScrollBarVisible(const Value: Boolean);
begin
  FScrollBar.Visible := Value;
  if csDesigning in ComponentState then
    if not Value then
      FScrollBar.Parent := nil
    else
      FScrollBar.Parent := Self;
end;

procedure TJvScrollMax.SetOneExpanded(const Value: Boolean);
begin
  if FOneExpanded <> Value then
  begin
    FOneExpanded := Value;
    { .. }
  end;
end;

{ (rom) deactivated  can cause problems
initialization
  crRAHand := DefineCursor('RAHAND');
  crRAHandMove := DefineCursor('RAHANDMOVE');
}

end.

