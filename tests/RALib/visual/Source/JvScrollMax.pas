{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvScrollMax
description : scrollable panels

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

{ History:
  1.20:
    - first version;
  2.00:
    - new property ScrollbarVisible;
}

unit JvScrollMax;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs,
  ExtCtrls, StdCtrls, Controls, Buttons, JvButtons;


 { TJvScrollMax }

const
  CM_PARENTBEVELEDCHANGED       = WM_User + 1;
  CM_PARENTBUTTONFONTCHANGED    = WM_User + 2;
  CM_PARENTBUTTONVISIBLECHANGED = WM_User + 3;

type

  TOnCanExpand = procedure(Sender : TObject; var CanExpand : boolean) of object;
  TOnCanCollapse = procedure(Sender : TObject; var CanCollapse : boolean) of object;

  TJvScrollMax = class;

  TJvScrollMaxBand = class(TCustomControl)
  private
    FData : Pointer;
    FExpandedHeight : integer;
    FButton : TSpeedButton;
    FExpanded : boolean;
    FOrder : integer;
    FBeveled : boolean;
    FBorderWidth : integer;
    FParentBeveled : boolean;
    FParentButtonFont : boolean;
    FParentButtonVisible : boolean;

    FOnResize: TNotifyEvent;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FOnCanCollapse : TOnCanCollapse;
    FOnCanExpand : TOnCanExpand;


    procedure ButtonClick(Sender : TObject);
    procedure SetExpanded(const Value: boolean);
    procedure SetExpandedHeight(const Value: integer);
    function GetOrder: integer;
    procedure SetOrder(const Value: integer);
    procedure SetParentBeveled(const Value: boolean);
    procedure SetButtonFont(Value: TFont);
    function GetButtonFont: TFont;
    procedure SetBeveled(const Value : boolean);
    procedure SetBorderWidth(const Value: integer);
    function IsBeveledStored : boolean;
    procedure SetParentButtonFont(const Value: Boolean);
    function IsButtonFontStored: Boolean;
    function GetButtonVisible: boolean;
    procedure SetButtonVisible(const Value: boolean);
    function IsButtonVisibleStored : boolean;
    procedure SetParentButtonVisible(const Value: boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMParentBeveledChanged(var Message: TMessage); message CM_PARENTBEVELEDCHANGED;
    procedure CMParentButtonFontChanged(var Message: TMessage); message CM_PARENTBUTTONFONTCHANGED;
    procedure CMParentButtonVisibleChanged(var Message: TMessage); message CM_PARENTBUTTONVISIBLECHANGED;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure SetZOrder(TopMost: Boolean); override;
    function JvScrollMax : TJvScrollMax;
    procedure UpdateSize(ATop : integer);
   {$IFDEF COMPILER2}
    procedure RequestAlign;
   {$ENDIF COMPILER2}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CollapsedHeight : integer;
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Data : Pointer read FData write FData;
  published
    property Expanded : boolean read FExpanded write SetExpanded default true;
    property Caption;
    property ExpandedHeight : integer read FExpandedHeight write SetExpandedHeight;
    property Order : integer read GetOrder write SetOrder stored false;
    property ButtonVisible : boolean read GetButtonVisible write SetButtonVisible stored IsButtonVisibleStored;
    property ButtonFont : TFont read GetButtonFont write SetButtonFont stored IsButtonFontStored;
    property Beveled : boolean read FBeveled write SetBeveled default true;
    property BorderWidth : integer read FBorderWidth write SetBorderWidth default 0;
    property ParentBeveled : boolean read FParentBeveled write SetParentBeveled stored IsBeveledStored;
    property ParentButtonVisible : boolean read FParentButtonVisible write SetParentButtonVisible default true;
    property ParentButtonFont : boolean read FParentButtonFont write SetParentButtonFont default true;

    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnExpand : TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse : TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnCanExpand : TOnCanExpand read FOnCanExpand write FOnCanExpand;
    property OnCanCollapse : TOnCanCollapse read FOnCanCollapse write FOnCanCollapse;

    property Left   stored false;
    property Top    stored false;
    property Width  stored false;
    property Height stored false;

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
   {$IFDEF COMPILER4_UP}
    property BiDiMode;
    property ParentBiDiMode;
   {$ENDIF COMPILER4_UP}
  end;

  TJvScrollMaxBands = class(TCustomControl)
  private
    Scrolling : boolean;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure ScrollControls(const DeltaY : integer);
    procedure Paint; override;
  end;

  TJvScrollBar = class(TCustomPanel)
  private
    FMin, FMax : integer;
    FPos  : integer;
    FPage : integer;
    Scroll : TPanel;
    FDesignInteractive : boolean;
    FInclusive : boolean;
    FOnChange : TNotifyEvent;
    FOnScroll : TNotifyEvent;
    procedure SetParam(index, Value : integer);
    procedure SetInclusive(Value : boolean);
  protected
    procedure CreateWnd; override;
    procedure SetTrackBar;
    procedure Loaded; override;
    procedure Resize; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure SetParams(const AMin, AMax, APage, APos : integer);
    property Pos : integer index 3 read FPos write SetParam;
    property DesignInteractive : boolean read FDesignInteractive write FDesignInteractive;
    property Scroller : TPanel read Scroll;
  published
    property Color;
    property Align;
    property Min  : integer index 0 read FMin write SetParam;
    property Max  : integer index 1 read FMax write SetParam;
    property Page : integer index 2 read FPage write SetParam;
    property Position : integer index 3 read FPos write SetParam;
    property Inclusive : boolean read FInclusive write SetInclusive;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnScroll : TNotifyEvent read FOnScroll write FOnScroll;
  end;

  TJvScrollMax = class(TCustomPanel)
  private
    pnlEdit    : TJvScrollMaxBands;
    ScrollBar  : TJvScrollBar;
    FScrollPos : integer;
    Yy         : integer;
    FButtonFont : TFont;
    FOnScroll: TNotifyEvent;
    FBeveled : boolean;
    FButtonVisible : boolean;
    FAutoHeight : boolean;
    FExpandedHeight : integer;
    FOneExpanded : boolean;

    procedure Correct;
    procedure CorrectHeight;
    procedure BandMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure BandMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure BandMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure ScrollBarScroll(Sender : TObject);

    procedure SetButtonFont(Value: TFont);
    procedure ButtonFontChanged(Sender: TObject);
    function GetBand(index: integer): TJvScrollMaxBand;
    function GetBandCount: integer;
    procedure SetScrollPos(const Value : integer);
    procedure SetButtonVisible(const Value: boolean);
    procedure SetBeveled(const Value: boolean);
    procedure SetAutoHeight(const Value: boolean);
    procedure SetExpandedHeight(const Value : integer);
    function GetScrollBarWidth : cardinal;
    procedure SetScrollBarWidth(const Value : cardinal);
    function GetScrollBarVisible : boolean;
    procedure SetScrollBarVisible(const Value : boolean);
    procedure SetOneExpanded(const Value: boolean);
  protected
    procedure Loaded; override;
   {$IFDEF COMPILER2}
    procedure GetChildren(Proc: TGetChildProc); override;
   {$ELSE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
   {$ENDIF  COMPILER2}
    function GetChildParent: TComponent; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ScrollInView(AControl : TControl);
    procedure MouseControls(AControls : array of TControl);
    procedure MouseClasses(AControlClasses : array of TControlClass);
    function AllCollapsed : boolean;
    function AllExpanded : boolean;
    procedure AddBand(Band: TJvScrollMaxBand);

    property BandCount : integer read GetBandCount;
    property Bands[index : integer] : TJvScrollMaxBand read GetBand;
  published
    property ScrollPos : integer read FScrollPos write SetScrollPos default 0;
    property BorderWidth default 3;
    property Beveled : boolean read FBeveled write SetBeveled default true;
    property ButtonFont : TFont read FButtonFont write SetButtonFont;
    property ButtonVisible : boolean read FButtonVisible write SetButtonVisible default true;
    property AutoHeight : boolean read FAutoHeight write SetAutoHeight;
    property ExpandedHeight : integer read FExpandedHeight write SetExpandedHeight default -1;
    property ScrollBarWidth : cardinal read GetScrollBarWidth write SetScrollBarWidth default 7;
    property ScrollBarVisible: boolean read GetScrollBarVisible write SetScrollBarVisible default true;
    property OneExpanded : boolean read FOneExpanded write SetOneExpanded default false;
    property OnScroll : TNotifyEvent read FOnScroll write FOnScroll;

    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
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
 {$IFDEF COMPILER4_UP}
  public
    property DockManager;
  published
    property Anchors;
    //property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
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
 {$ENDIF COMPILER4_UP}
  end;

  EJvScrollMaxError  = class(Exception);

var
  crRAHand : integer;
  crRAHandMove : integer;

implementation

uses JvDsgnIntf;

{ Cursors resources }
{$R *.RES}

function PanelBorder(Panel : TCustomPanel) : integer;
begin
  Result := TPanel(Panel).BorderWidth;
  if TPanel(Panel).BevelOuter <> bvNone then Inc(Result, TPanel(Panel).BevelWidth);
  if TPanel(Panel).BevelInner <> bvNone then Inc(Result, TPanel(Panel).BevelWidth);
end;

{ function DefineCursor was typed from
  book "Secrets of Delphi 2" by Ray Lischner }

function DefineCursor(Identifer : PChar) : TCursor;
var
  Handle : HCursor;
begin
  Handle := LoadCursor(hInstance, Identifer);
  if Handle = 0 then
    raise EOutOfResources.Create('Cannot load cursor resource');
  for Result := 1 to High(TCursor) do
    if Screen.Cursors[Result] = Screen.Cursors[crDefault] then
    begin
      Screen.Cursors[Result] := Handle;
      exit;
    end;
  raise EOutOfResources.Create('Too many user-defined cursors');
end;

{********************** TJvScrollBar **********************}
type

  TJvScroller = class(TPanel)
  private
    Yy : integer;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
  end;

procedure TJvScroller.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if (Button = mbLeft) then
    Yy := Y;
end;

procedure TJvScroller.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  Sm, T, OldPos : integer;
begin
  if Shift = [ssLeft] then
  begin
    Sm := Yy - Y;
    T := Top;
    if (Sm <> 0) then
    begin
      with Parent as TJvScrollBar do
      begin
        OldPos := Pos;
        Pos := Pos - Round(Sm * (FMax - FMin + 1) / ClientHeight);
        if (Pos <> OldPos) and Assigned(FOnScroll) then
          FOnScroll(Parent);
      end;
    end;
    Yy := Y - Top + T;
  end;
end;

procedure TJvScroller.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  with (Owner as TJvScrollBar) do
    Message.Result := integer(FDesignInteractive and (FPage <> FMax - FMin + 1)) ;
end;

constructor TJvScrollBar.Create(AOwner : TComponent);
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

procedure TJvScrollBar.Loaded;
begin
  inherited Loaded;
  Resize;
end;

procedure TJvScrollBar.Resize;
begin
  inherited Resize;
  with Scroll do begin
    Top := BevelWidth;
    Left := BevelWidth;
    Width := Self.Width - 2*BevelWidth;
  end;
  SetTrackBar;
end;

procedure TJvScrollBar.SetTrackBar;
var
  CH, H, T : integer;
  L, FP, P, P1 : integer;
begin
 {перед изменением кода обязательно сделайте копию!}
  if FMin > FMax then FMin := FMax;
  if FPage > FMax-FMin+1 then FPage := FMax-FMin+1;
  if FInclusive then P := FPage else P := 0;
  P1 := FPage - P;
  if FPos > FMax-P then FPos := FMax-P;
  if FPos < FMin then FPos := FMin;
  L := FMax - FMin +1;
  CH := Height - 2*BevelWidth;
  H := Trunc(CH * FPage / L) +1;
  FP := Trunc((FPos-FMin) / L * (L - P1)) +1;
  T := Round(CH * FP / L);
  if H < 7 then H := 7;
  if H > CH then H := CH;
  if T < BevelWidth then T := BevelWidth;
  if T + H > Height - BevelWidth then
    T := Height - BevelWidth - H;
  if FPos = FMax - P then T := Height - BevelWidth - H;

  with Scroll do
    SetBounds(Left, T, Width, H);
end;

procedure TJvScrollBar.SetParam(index, Value : integer);
begin
  case index of
    0 : FMin  := Value;
    1 : FMax  := Value;
    2 : FPage := Value;
    3 : FPos  := Value;
  end;
  SetParams(FMin, FMax, FPage, FPos);
end;

procedure TJvScrollBar.SetParams(const AMin, AMax, APage, APos : integer);
begin
  FMin  := AMin ;
  FMax  := AMax ;
  FPage := APage;
  FPos  := APos ;
  if Assigned(FOnChange) then FOnChange(Self);
  SetTrackBar;
end;

procedure TJvScrollBar.SetInclusive(Value : boolean);
begin
  FInclusive := Value;
  SetTrackBar;
end;

procedure TJvScrollBar.CreateWnd;
begin
  inherited CreateWnd;
  SetTrackBar;
end;
{###################### TJvScrollBar ######################}

{********************** TJvScrollMax **********************}

type

  TJvBandBtn = class(TJvNoFrameButton)
  private
   {$IFDEF COMPILER3_UP}
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
   {$ENDIF COMPILER3_UP}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  end;

{$IFDEF COMPILER3_UP}
procedure TJvBandBtn.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;
{$ENDIF COMPILER3_UP}

procedure TJvBandBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Parent <> nil then
    with Parent as TJvScrollMaxBand do
    begin
      FParentButtonFont := false;
      Canvas.Font := Self.Font;
      FButton.Height := Canvas.TextHeight('W') + 4;
      Invalidate;
    end;
end;

{ ****** TJvScrollMaxBand ****** }

constructor TJvScrollMaxBand.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption, csAcceptsControls];
  Height := 50;
  FExpandedHeight := 50;
  ParentColor := true;
  FParentButtonFont := true;
  FParentButtonVisible := true;
  FParentBeveled := true;
  FButton := TJvBandBtn.Create(Self);
  with FButton as TJvBandBtn do begin
    SetDesigning(false);
    Parent := Self;
    Top     := 2;
    Left    := 4;
    Cursor := crArrow;
    OnClick := ButtonClick;
    Margin := 4;
    Spacing := -1;
    NoBorder := false;
    ParentColor := true;
    {o}
    {$IFDEF COMPILER4_UP}
    FButton.ParentBiDiMode := True;
    {$ENDIF COMPILER4_UP}
  end;
  Expanded := true;
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

procedure TJvScrollMaxBand.WMSize(var Message: TWMSize);
begin
  if FExpanded then ExpandedHeight := Height;
  inherited;
  if Assigned(FOnResize) then FOnResize(Self);
  if Parent <> nil then JvScrollMax.CorrectHeight;
end;

procedure TJvScrollMaxBand.CMTextChanged(var Message: TMessage);
begin
  FButton.Caption := Caption;
end;

procedure TJvScrollMaxBand.SetExpanded(const Value: boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    if FExpanded then
      FButton.Glyph.LoadFromResourceName(hInstance, 'RASCROLLMAXBTNMINUS')
    else
      FButton.Glyph.LoadFromResourceName(hInstance, 'RASCROLLMAXBTNPLUS');
    if FExpanded and Assigned(FOnExpand) then FOnExpand(Self);
    if not FExpanded and Assigned(FOnCollapse) then FOnCollapse(Self);
    RequestAlign;
    if Parent <> nil then JvScrollMax.CorrectHeight;
   { if not (csLoading in ComponentState) and (JvScrollMax <> nil) then
      DesignerModified(JvScrollMax); }
  end;
end;

procedure TJvScrollMaxBand.SetExpandedHeight(const Value: integer);
begin
  if FExpandedHeight <> Value then
  begin
    FExpandedHeight := Value;
    if FExpanded then
      Height := FExpandedHeight;
     // RequestAlign - called from SetHeight
  end;
end;

function TJvScrollMaxBand.GetOrder: integer;
var
  i : integer;
begin
  Result := FOrder;
  if Parent <> nil then
  begin
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] = Self then
      begin
        Result := i;
        break;
      end;
  end;
end;

procedure TJvScrollMaxBand.SetOrder(const Value: integer);
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
    if Parent <> nil then Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.CMParentButtonFontChanged(var Message: TMessage);
begin
  if FParentButtonFont then
  begin
    if JvScrollMax <> nil then
      SetButtonFont(JvScrollMax.FButtonFont);
    FParentButtonFont := true;
  end;
end;

function TJvScrollMaxBand.IsButtonFontStored: Boolean;
begin
  Result := not ParentButtonFont;
end;

function TJvScrollMaxBand.GetButtonVisible: boolean;
begin
  Result := FButton.Visible;
end;

procedure TJvScrollMaxBand.SetButtonVisible(const Value: boolean);
begin
  if FButton.Visible <> Value then
  begin
    FParentButtonVisible := false;
    FButton.Visible := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;

function TJvScrollMaxBand.IsButtonVisibleStored : boolean;
begin
  Result := not ParentButtonVisible;
end;

procedure TJvScrollMaxBand.SetParentButtonVisible(const Value: boolean);
begin
  if FParentButtonVisible <> Value then
  begin
    FParentButtonVisible := Value;
    if Parent <> nil then Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.CMParentButtonVisibleChanged(var Message: TMessage);
begin
  if FParentButtonVisible then
  begin
    if JvScrollMax <> nil then
      SetButtonVisible(JvScrollMax.FButtonVisible);
    FParentButtonVisible := true;
  end;
end;

procedure TJvScrollMaxBand.SetBeveled(const Value: boolean);
begin
  if FBeveled <> Value then
  begin
    FParentBeveled := false;
    FBeveled := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;

function TJvScrollMaxBand.IsBeveledStored : boolean;
begin
  Result := not ParentBeveled;
end;

procedure TJvScrollMaxBand.SetParentBeveled(const Value: boolean);
begin
  if FParentBeveled <> Value then
  begin
    FParentBeveled := Value;
    if Parent <> nil then Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  end;
end;

procedure TJvScrollMaxBand.CMParentBeveledChanged(var Message: TMessage);
begin
  if FParentBeveled then
  begin
    if JvScrollMax <> nil then
      SetBeveled(JvScrollMax.FBeveled);
    FParentBeveled := true;
  end;
end;

procedure TJvScrollMaxBand.ButtonClick(Sender : TObject);
var
  E : boolean;
begin
  E := true;
  if FExpanded then
    if Assigned(FOnCanCollapse) then FOnCanCollapse(Self, E) else
  else
    if Assigned(FOnCanExpand) then FOnCanExpand(Self, E);
  if E then
    Expanded := not FExpanded;
  DesignerModified(Self);
end;

procedure TJvScrollMaxBand.SetParent(AParent: TWinControl);
begin
  if not ((AParent is TJvScrollMaxBands) or (AParent = nil)) then
    raise EJvScrollMaxError .Create('TJvScrollMaxBand can be putted only into TJvScrollMax component');
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

procedure TJvScrollMaxBand.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  JvScrollMax.BandMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvScrollMaxBand.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseMove(Shift, X, Y);
  JvScrollMax.BandMouseMove(Self, Shift, X, Y);
end;

procedure TJvScrollMaxBand.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  JvScrollMax.BandMouseUp(Self, Button, Shift, X, Y);
end;

function TJvScrollMaxBand.JvScrollMax : TJvScrollMax;
begin
  if (Parent <> nil) and ((Parent as TJvScrollMaxBands).Parent <> nil) then
    Result := (Parent as TJvScrollMaxBands).Parent as TJvScrollMax
  else
    Result := nil;
end;

function TJvScrollMaxBand.CollapsedHeight : integer;
begin
  Result := FButton.BoundsRect.Bottom + FButton.Top;
end;

procedure TJvScrollMaxBand.UpdateSize(ATop : integer);
var
  W, H : integer;
begin
  if FExpanded then H := FExpandedHeight
  else H := CollapsedHeight;
  if JvScrollMax <> nil then
  begin
    W := Parent.Width;
    if JvScrollMax.ScrollBarVisible then
      W := W - 3;
  end
  else
    W := Width;
  SetBounds(0, ATop, W, H);
  if FBeveled then FButton.Left := 16
  else FButton.Left := 4;
  FButton.Width := Width - FButton.Left * 2;
end;

{$IFDEF COMPILER2}
procedure TJvScrollMaxBand.RequestAlign;
begin
  if Parent <> nil then Left := Left - 1;
end;
{$ENDIF COMPILER2}

procedure TJvScrollMaxBand.Paint;
var
  R : TRect;
const
  Ex : array[boolean] of integer = (BF_TOP, BF_RECT);
begin
  inherited Paint;
  if Canvas.Handle <> 0 then
  begin
    if (csDesigning in ComponentState) then
      DrawDesignFrame(Canvas, ClientRect);
    if FBeveled then
    begin
      R.Left := 1;
      if ButtonVisible then
        R.Top := FButton.Top + FButton.Height div 2 else
        R.Top := 1;
      R.Right := Width - R.Left;
      R.Bottom := Height - 1;
      DrawEdge(Canvas.Handle, R, EDGE_ETCHED, Ex[FExpanded]);
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
  if FBeveled then inc(BevelSize, 3);
  InflateRect(Rect, -BevelSize, -BevelSize);
  if ButtonVisible then begin
    inc(Rect.Top, FButton.Height);
    if FButton.Top > FBorderWidth then inc(Rect.Top, FButton.Top);
  end;
  inherited AlignControls(AControl, Rect);
end;

procedure TJvScrollMaxBand.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Realign;
  end;
end;

procedure TJvScrollMaxBand.ChangeScale(M, D: Integer); 
begin
  inherited ChangeScale(M, D);
  ExpandedHeight := FExpandedHeight * M div D;
end;
{ ******* TJvScrollMaxBands ******* }

procedure TJvScrollMaxBands.AlignControls(AControl: TControl; var Rect: TRect);

  procedure AdjustBottom;
  begin
    if (Controls[ControlCount -1].BoundsRect.Bottom < Height) and
       (Controls[0].Top < 0) then
    begin
      if Height - (Controls[ControlCount -1].BoundsRect.Bottom - Controls[0].Top) > 0 then
        ScrollControls(-Controls[0].Top)
      else
        ScrollControls(Height - Controls[ControlCount -1].BoundsRect.Bottom);
    end;
  end;

  procedure AdjustBand;
  var
    Band : TJvScrollMaxBand;
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
    i : integer;
    Cursor : TCursor;
  begin
    if (Controls[ControlCount -1].BoundsRect.Bottom > ClientHeight) or
       (Controls[0].Top < 0) then
      Cursor := crRAHand else
      Cursor := crDefault;
    for i := 0 to ControlCount -1 do
      Controls[i].Cursor := Cursor;
  end;

var
  i : integer;
  JvScrollMax : TJvScrollMax;
  T : integer;
  SMax, SPage, SPos : integer;
begin
  if Scrolling then exit;
  if (Parent <> nil) and (csLoading in Parent.ComponentState) then exit;
  JvScrollMax := Parent as TJvScrollMax;
  if (AControl <> nil) and
     (AControl as TJvScrollMaxBand).FExpanded and
     JvScrollMax.FOneExpanded then
    for i := 0 to ControlCount -1 do
      if not (Controls[i] is TJvScrollMaxBand) then
        raise EJvScrollMaxError .Create('TJvScrollMax can contains only TJvScrollMaxBand components')
      else if Controls[i] <> AControl then
        (Controls[i] as TJvScrollMaxBand).Expanded := false;
  SPos := JvScrollMax.FScrollPos;
  if ControlCount > 0 then
  begin
    for i := 0 to ControlCount -1 do
    begin
      if not (Controls[i] is TJvScrollMaxBand) then
        raise EJvScrollMaxError .Create('TJvScrollMax can contains only TJvScrollMaxBand components');
      if i > 0 then
        T := Controls[i - 1].BoundsRect.Bottom else
        T := -JvScrollMax.FScrollPos;
      (Controls[i] as TJvScrollMaxBand).UpdateSize(T);
    end;
    AdjustBottom;
    AdjustBand;
    SMax := Controls[ControlCount-1].BoundsRect.Bottom - Controls[0].Top;
    SPos := -Controls[0].Top;
    JvScrollMax.FScrollPos := SPos;
    SetCursor;
  end else
    SMax := Height;
  SPage := Height;
  JvScrollMax.ScrollBar.SetParams(0, SMax, SPage, SPos);
end;

procedure TJvScrollMaxBands.ScrollControls(const DeltaY : integer);
begin
  Scrolling := true;
  try
    ScrollBy(0, DeltaY);
  finally
    Scrolling := false;
  end;
end;

procedure TJvScrollMaxBands.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if (Message.Sender <> nil) and
     ContainsControl(Message.Sender) and
     (Parent <> nil) then
    (Parent as TJvScrollMax).ScrollInView(Message.Sender);
end;

procedure TJvScrollMaxBands.Paint;
var
  R : TRect;
begin
  inherited Paint;
  if (csDesigning in ComponentState) and
     (ControlCount = 0) and
     (Canvas.Handle <> 0) then
  begin
    R := ClientRect;
    Canvas.Font.Color := clAppWorkSpace;
    DrawText(Canvas.Handle, 'Right click and choose "Add band"',
       -1, R, DT_WORDBREAK {or DT_CENTER or DT_VCENTER});
  end;
end;


{ ******* TJvScrollMax ******* }

constructor TJvScrollMax.Create(AOwner : TComponent);
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
  FButtonVisible := true;
  FBeveled := true;
  ParentColor := true;
  pnlEdit := TJvScrollMaxBands.Create(Self);
  with pnlEdit do begin
    Align       := alClient;
    Parent      := Self;
    ControlStyle := ControlStyle + [csAcceptsControls];
    ParentColor := true;
  end;
  ScrollBar := TJvScrollBar.Create(Self);
  with ScrollBar do begin
    Inclusive := true;
    Parent := Self;
    Width  := 7;
    Align  := alRight;
    Max    := pnlEdit.Height;
    Page   := Self.Height;
    OnScroll := ScrollBarScroll;
    ParentColor := true;
    Visible := True;
   {$IFDEF COMPILER3_UP}
    DesignInteractive := true;
   {$ENDIF COMPILER3_UP}
  end;
end;

destructor TJvScrollMax.Destroy;
begin
  FButtonFont.Free;
  inherited Destroy;
end;

procedure TJvScrollMax.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    ExStyle := ExStyle or WS_EX_CONTROLPARENT;
  end;
end;

procedure TJvScrollMax.Loaded;
begin
  inherited Loaded;
  Resize;
  pnlEdit.Realign;
end;

procedure TJvScrollMax.SetButtonFont(Value: TFont);
begin
  FButtonFont.Assign(Value);
end;

procedure TJvScrollMax.SetButtonVisible(const Value: boolean);
begin
  if FButtonVisible <> Value then
  begin
    FButtonVisible := Value;
    pnlEdit.NotifyControls(CM_PARENTBUTTONVISIBLECHANGED);
  end;
end;

procedure TJvScrollMax.SetBeveled(const Value: boolean);
begin
  if FBeveled <> Value then
  begin
    FBeveled := Value;
    pnlEdit.NotifyControls(CM_PARENTBEVELEDCHANGED);
  end;
end;

procedure TJvScrollMax.ButtonFontChanged(Sender: TObject);
begin
  pnlEdit.NotifyControls(CM_PARENTBUTTONFONTCHANGED);
end;

procedure TJvScrollMax.MouseControls(AControls : array of TControl);
var
  i : integer;
begin
  for i := Low(AControls) to High(AControls) do
  begin
    TJvScrollMax(AControls[i]).OnMouseDown := BandMouseDown;
    TJvScrollMax(AControls[i]).OnMouseMove := BandMouseMove;
    TJvScrollMax(AControls[i]).OnMouseUp   := BandMouseUp  ;
  end;
end;

procedure TJvScrollMax.MouseClasses(AControlClasses : array of TControlClass);
var
  i, iB, iC : integer;
begin
  for i := Low(AControlClasses) to High(AControlClasses) do
    for iB := 0 to BandCount - 1 do
      for iC := 0 to Bands[iB].ControlCount -1 do
        if Bands[iB].Controls[iC] is AControlClasses[i] then
        begin
          TJvScrollMax(Bands[iB].Controls[iC]).OnMouseDown := BandMouseDown;
          TJvScrollMax(Bands[iB].Controls[iC]).OnMouseMove := BandMouseMove;
          TJvScrollMax(Bands[iB].Controls[iC]).OnMouseUp   := BandMouseUp  ;
        end;
end;

procedure TJvScrollMax.Correct;
var
  Sm : integer;
  CH : integer;
begin
  if BandCount > 0 then
  begin
    Sm := 0;
    CH := pnlEdit.Height;
    if (Bands[BandCount -1].BoundsRect.Bottom < CH) and (Bands[0].Top < 0) then
      Sm := (CH - Bands[BandCount -1].BoundsRect.Bottom);
    if Bands[0].Top + Sm > 0 then Sm := -Bands[0].Top;
    if Sm <> 0 then begin
      pnlEdit.ScrollControls(Sm);
      ScrollBar.Pos := -Bands[0].Top;
      FScrollPos := ScrollBar.Pos;
    end;
  end;
end;

procedure TJvScrollMax.BandMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  CH : integer;
begin
  if (Button = mbLeft) and (BandCount > 0) then
  begin
    Yy := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := pnlEdit.Height;
    if (Bands[BandCount -1].BoundsRect.Bottom > CH) or
       (Bands[0].Top < 0) then
      Screen.Cursor := crRAHandMove else
      Screen.Cursor := crDefault;
  end;
end;

procedure TJvScrollMax.BandMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
var
  Sm : integer;
  CH : integer;
begin
  if (ssLeft in Shift) and (BandCount > 0) then
  begin
    Y := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := pnlEdit.Height;
    if not (Sender = ScrollBar.Scroller) then
      Sm := Y-Yy else
      Sm := Yy-Y;
    if {Up} Sm < 0 then begin
      if not (Bands[BandCount -1].BoundsRect.Bottom > CH) then Sm := 0
      else if (Bands[BandCount -1].BoundsRect.Bottom + Sm < CH) then
        Sm := CH - Bands[BandCount -1].BoundsRect.Bottom;
    end else if{Down} Sm > 0 then begin
      if not (Bands[0].Top < 0) then Sm := 0
      else if (Bands[0].Top + Sm > 0) then Sm := -Bands[0].Top;
    end;
    if Sm <> 0 then begin
      pnlEdit.ScrollControls(Sm);
      ScrollBar.Pos := -Bands[0].Top;
      FScrollPos := ScrollBar.Pos;
    end;
    Yy := Y;
    Correct;
  end;
end;

procedure TJvScrollMax.BandMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  Screen.Cursor := crDefault;
end;

function TJvScrollMax.GetBand(index: integer): TJvScrollMaxBand;
begin
  Result := TJvScrollMaxBand(pnlEdit.Controls[index]);
end;

function TJvScrollMax.GetBandCount: integer;
begin
  Result := pnlEdit.ControlCount;
end;


{$IFDEF COMPILER2}
procedure TJvScrollMax.GetChildren(Proc: TGetChildProc);
begin
  pnlEdit.GetChildren(Proc);
end;
{$ELSE}
procedure TJvScrollMax.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  pnlEdit.GetChildren(Proc, Root);
end;
{$ENDIF  COMPILER2}

function TJvScrollMax.GetChildParent: TComponent;
begin
  Result := pnlEdit;
end;

procedure TJvScrollMax.SetScrollPos(const Value : integer);
begin
  if FScrollPos <> Value then
  begin
    FScrollPos := Value;
    if not (csLoading in ComponentState) then
    begin
      if FScrollPos > ScrollBar.Max - ScrollBar.Page then
        FScrollPos := ScrollBar.Max - ScrollBar.Page;
      if FScrollPos < 0 then FScrollPos := 0;
      DesignerModified(Self);
      pnlEdit.Realign;
    end;
  end;
end;

procedure TJvScrollMax.ScrollBarScroll(Sender : TObject);
begin
  ScrollPos := ScrollBar.Pos;
  if Assigned(FOnScroll) then FOnScroll(Self);
end;

procedure TJvScrollMax.ScrollInView(AControl : TControl);
var
  i : integer;
  Band : TJvScrollMaxBand;
  Rect : TRect;
begin
  Band := nil;
  for i := 0 to pnlEdit.ControlCount - 1 do
    if (pnlEdit.Controls[i] as TJvScrollMaxBand).ContainsControl(AControl) then
    begin
      Band := pnlEdit.Controls[i] as TJvScrollMaxBand;
      break;
    end;
  if Band = nil then raise EJvScrollMaxError .CreateFmt('Control %s not a child of %s', [AControl.Name, Parent.Name]);
  Band.Expanded := true;
  Rect := AControl.ClientRect;
  Dec(Rect.Top, BevelWidth + BorderWidth + 4);
  Inc(Rect.Bottom, BevelWidth + BorderWidth + 4);
  Rect.TopLeft := ScreenToClient(AControl.ClientToScreen(Rect.TopLeft));
  Rect.BottomRight := ScreenToClient(AControl.ClientToScreen(Rect.BottomRight));
  if Rect.Top < 0 then
    ScrollPos := ScrollPos + Rect.Top
  else if Rect.Bottom > ClientHeight then
  begin
    if Rect.Bottom - Rect.Top > ClientHeight then
      Rect.Bottom := Rect.Top + ClientHeight;
    ScrollPos := ScrollPos + Rect.Bottom - ClientHeight;
  end;
end;

procedure TJvScrollMax.SetAutoHeight(const Value: boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if FAutoHeight then CorrectHeight;
  end;
end;

procedure TJvScrollMax.SetExpandedHeight(const Value : integer);
begin
  if FExpandedHeight <> Value then
  begin
    FExpandedHeight := Value;
    if FAutoHeight then CorrectHeight;
  end;
end;

procedure TJvScrollMax.Resize;
begin
  inherited Resize;
  if FAutoHeight and (BandCount > 0) and
    not AllCollapsed and (FExpandedHeight > -1) then
    FExpandedHeight := Height;
  if FAutoHeight then CorrectHeight;
end;

procedure TJvScrollMax.CorrectHeight;
var
  i, H : integer;
  Band : TJvScrollMaxBand;
begin
  if not FAutoHeight or (BandCount = 0) then exit;
  if AllCollapsed then
  begin
    H := 0;
    for i := 0 to BandCount - 1 do
      inc(H, Bands[i].Height);
    ClientHeight := H + 2 * PanelBorder(Self);
  end else
    if FExpandedHeight <> -1 then
      Height := FExpandedHeight
    else
    begin
      H := 0;
      Band := nil;
      for i := 0 to BandCount - 1 do
        if Bands[i].Height > H then
        begin
          Band := Bands[i];
          H := Band.Height;
        end;
      H := 0;
      for i := 0 to BandCount - 1 do
        if Bands[i] = Band then
          inc(H, Bands[i].Height)
        else
          inc(H, Bands[i].CollapsedHeight);
      ClientHeight := H + 2 * PanelBorder(Self);
    end;
end;

function TJvScrollMax.AllCollapsed : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to BandCount - 1 do
    if Bands[i].Expanded then exit;
  Result := true;
end;

function TJvScrollMax.AllExpanded : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to BandCount - 1 do
    if not Bands[i].Expanded then exit;
  Result := true;
end;

procedure TJvScrollMax.AddBand(Band: TJvScrollMaxBand);
begin
  Band.Parent := GetChildParent as TWinControl;
end;

function TJvScrollMax.GetScrollBarWidth : cardinal;
begin
  Result := ScrollBar.Width;
end;

procedure TJvScrollMax.SetScrollBarWidth(const Value : cardinal);
begin
  if Value >= 4 then
    ScrollBar.Width := Value;
end;

function TJvScrollMax.GetScrollBarVisible : boolean;
begin
  Result := ScrollBar.Visible;
end;

procedure TJvScrollMax.SetScrollBarVisible(const Value : boolean);
begin
  ScrollBar.Visible := Value;
  if (csDesigning in ComponentState) then
    if not Value then
      ScrollBar.Parent := nil
    else
      ScrollBar.Parent := Self;
end;

procedure TJvScrollMax.SetOneExpanded(const Value: boolean);
begin
  if FOneExpanded <> Value then
  begin
    FOneExpanded := Value;
    { .. }
  end;
end;

initialization
  crRAHand := DefineCursor('RAHAND');
  crRAHandMove := DefineCursor('RAHANDMOVE');
end.
