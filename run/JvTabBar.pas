{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTabBar.pas, released on 2004-12-23.

The Initial Developer of the Original Code is Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTabBar;

{$I jvcl.inc}
{.$DEFINE WINFORMS}

interface

uses
 {$IFDEF WINFORMS}
  System.Windows.Forms, System.Drawing, 
  Borland.Vcl.Windows, Borland.Vcl.Messages, Borland.VCL.SysUtils,
  Borland.Vcl.Classes, Borland.Vcl.Types,
  Jedi.WinForms.VCL.Graphics, Jedi.WinForms.VCL.Controls,
  Jedi.WinForms.VCL.Forms, Jedi.WinForms.VCL.ImgList, Jedi.WinForms.VCL.Menus,
  Jedi.WinForms.VCL.Buttons;
 {$ELSE}
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ImgList, Menus, Buttons,
  {$IFDEF CLR}
  Types,
  {$ENDIF CLR}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, Qt, QTypes, QGraphics, QControls, QForms, QImgList, QMenus, QButtons,
  {$ENDIF VisualCLX}
  SysUtils, Classes;
 {$ENDIF WINFORMS}

type
  TJvCustomTabBar = class;
  TJvTabBarItem = class;

  TJvGetModifiedEvent = procedure(Sender: TJvTabBarItem; var Modified: Boolean) of object;
  TJvGetEnabledEvent = procedure(Sender: TJvTabBarItem; var Enabled: Boolean) of object;

  {$IFDEF COMPILER5}
  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);

  TOwnedCollection = class(Classes.TOwnedCollection)
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
  end;
  {$ENDIF COMPILER5}

  TJvTabBarItem = class(TCollectionItem)
  private
    FLeft: Integer; // used for calculating DisplayRect

    FImageIndex: TImageIndex;
    FEnabled: Boolean;
    FVisible: Boolean;
    FTag: Integer;
    FData: TObject;
    FHint: TCaption;
    FName: string;
    FCaption: TCaption;
    FImages: TCustomImageList;
    FModified: Boolean;
    FPopupMenu: TPopupMenu;
    FOnGetEnabled: TJvGetEnabledEvent;
    FOnGetModified: TJvGetModifiedEvent;
    FShowHint: Boolean;
    function GetEnabled: Boolean;
    function GetModified: Boolean;

    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetClosing: Boolean;
    procedure SetModified(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetSelected(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetName(const Value: string);
    procedure SetVisible(const Value: Boolean);
    function GetTabBar: TJvCustomTabBar;
    function GetSelected: Boolean;
    function GetDisplayRect: TRect;
    function GetHot: Boolean;
  protected
    procedure Changed; virtual;

    procedure SetIndex(Value: Integer); override;
    procedure Notification(Component: TComponent; Operation: TOperation); virtual;
    property Name: string read FName write SetName;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetImages: TCustomImageList;
    function CanSelect: Boolean;
    function GetNextVisible: TJvTabBarItem;
    function GetPreviousVisible: TJvTabBarItem;
    procedure MakeVisible;

    property Data: TObject read FData write FData;
    property TabBar: TJvCustomTabBar read GetTabBar;
    property DisplayRect: TRect read GetDisplayRect;
    property Hot: Boolean read GetHot;
    property Closing: Boolean read GetClosing;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Selected: Boolean read GetSelected write SetSelected stored False;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Modified: Boolean read GetModified write SetModified default False;
    property Hint: TCaption read FHint write FHint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowHint: Boolean read FShowHint write FShowHint default True;

    property OnGetModified: TJvGetModifiedEvent read FOnGetModified write FOnGetModified;
    property OnGetEnabled: TJvGetEnabledEvent read FOnGetEnabled write FOnGetEnabled;
  end;

  TJvTabBarItems = class(TOwnedCollection)
  private
    function GetTabBar: TJvCustomTabBar;
    function GetItem(Index: Integer): TJvTabBarItem;
    procedure SetItem(Index: Integer; const Value: TJvTabBarItem);
  protected
    function Find(const AName: string): TJvTabBarItem;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    function IndexOf(Item: TJvTabBarItem): Integer;
    property Items[Index: Integer]: TJvTabBarItem read GetItem write SetItem; default;

    property TabBar: TJvCustomTabBar read GetTabBar;
  end;

  TJvTabBarPainterOptionType = (poPaintsHotTab);
  TJvTabBarPainterOptions = set of TJvTabBarPainterOptionType;

  TJvTabBarPainter = class(TComponent)
  private
    FOnChange: TNotifyEvent;
  protected
    procedure Changed; virtual;

    procedure DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect); virtual; abstract;
    procedure DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect); virtual; abstract;
    procedure DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect); virtual; abstract;
    function GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer; virtual; abstract;
    function GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize; virtual; abstract;
    function GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect): TRect; virtual; abstract;
    function Options: TJvTabBarPainterOptions; virtual; abstract;
  end;

  TJvModernTabBarPainter = class(TJvTabBarPainter)
  private
    FFont: TFont;
    FDisabledFont: TFont;
    FColor: TColor;
    FTabColor: TColor;
    FControlDivideColor: TColor;
    FBorderColor: TColor;
    FModifiedCrossColor: TColor;
    FCloseRectColor: TColor;
    FCloseRectColorDisabled: TColor;
    FCloseCrossColorDisabled: TColor;
    FCloseCrossColorSelected: TColor;
    FCloseCrossColor: TColor;
    FCloseColor: TColor;
    FCloseColorSelected: TColor;
    FDividerColor: TColor;
    procedure SetCloseRectColorDisabled(const Value: TColor);
    procedure SetCloseColor(const Value: TColor);
    procedure SetCloseColorSelected(const Value: TColor);
    procedure SetCloseCrossColor(const Value: TColor);
    procedure SetCloseCrossColorDisabled(const Value: TColor);
    procedure SetCloseRectColor(const Value: TColor);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetFont(const Value: TFont);

    procedure SetModifiedCrossColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetControlDivideColor(const Value: TColor);

    procedure SetTabColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    procedure SetDividerColor(const Value: TColor);
    procedure SetCloseCrossColorSelected(const Value: TColor);
  protected
    procedure DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect); override;
    procedure DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect); override;
    procedure DrawDivider(Canvas: TCanvas; LeftTab: TJvTabBarItem; R: TRect); override;
    function GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer; override;
    function GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize; override;
    function GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect): TRect; override;
    function Options: TJvTabBarPainterOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;
    property Color: TColor read FColor write SetColor default clWindow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property ControlDivideColor: TColor read FControlDivideColor write SetControlDivideColor default clBlack;
    property ModifiedCrossColor: TColor read FModifiedCrossColor write SetModifiedCrossColor default clRed;
    property CloseColorSelected: TColor read FCloseColorSelected write SetCloseColorSelected default $F4F4F4;
    property CloseColor: TColor read FCloseColor write SetCloseColor default clWhite;
    property CloseCrossColorSelected: TColor read FCloseCrossColorSelected write SetCloseCrossColorSelected default clBlack;
    property CloseCrossColor: TColor read FCloseCrossColor write SetCloseCrossColor default $5D5D5D;
    property CloseCrossColorDisabled: TColor read FCloseCrossColorDisabled write SetCloseCrossColorDisabled default $ADADAD;
    property CloseRectColor: TColor read FCloseRectColor write SetCloseRectColor default $868686;
    property CloseRectColorDisabled: TColor read FCloseRectColorDisabled write SetCloseRectColorDisabled default $D6D6D6;
    property DividerColor: TColor read FDividerColor write SetDividerColor default $99A8AC;

    property Font: TFont read FFont write SetFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
  end;

  TJvTabBarItemEvent = procedure(Sender: TObject; Item: TJvTabBarItem) of object;
  TJvTabBarSelectingEvent = procedure(Sender: TObject; Item: TJvTabBarItem; var AllowSelect: Boolean) of object;
  TJvTabBarClosingEvent = procedure(Sender: TObject; Item: TJvTabBarItem; var AllowClose: Boolean) of object;

  TJvCustomTabBar = class(TCustomControl)
  private
    FTabs: TJvTabBarItems;
    FPainter: TJvTabBarPainter;
    FDefaultPainter: TJvTabBarPainter;
    FChangeLink: TChangeLink;
    FCloseButton: Boolean;
    FRightClickSelect: Boolean;
    FImages: TImageList;
    FHotTracking: Boolean;
    FHotTab: TJvTabBarItem;
    FSelectedTab: TJvTabBarItem;
    FClosingTab: TJvTabBarItem;
    FMouseDownClosingTab: TJvTabBarItem;
    FMargin: Integer;
    FAutoFreeClosed: Boolean;
    FAllowUnselected: Boolean;

    FOnTabClosing: TJvTabBarClosingEvent;
    FOnTabSelected: TJvTabBarItemEvent;
    FOnTabSelecting: TJvTabBarSelectingEvent;
    FOnTabClosed: TJvTabBarItemEvent;
    FOnChange: TNotifyEvent;

    // scrolling
    FLeftIndex: Integer;
    FLastTabRight: Integer;
    FRequiredWidth: Integer;
    FBarWidth: Integer;
    FBtnLeftScroll: TSpeedButton;
    FBtnRightScroll: TSpeedButton;
    FBmpLeftScroll: TBitmap;
    FBmpRightScroll: TBitmap;
    FHint: TCaption;
    FFlatScrollButtons: Boolean;

    function GetLeftTab: TJvTabBarItem;
    procedure SetLeftTab(Value: TJvTabBarItem);
    procedure SetSelectedTab(Value: TJvTabBarItem);
    procedure SetTabs(Value: TJvTabBarItems);
    procedure SetPainter(Value: TJvTabBarPainter);
    procedure SetImages(Value: TImageList);
    procedure SetCloseButton(Value: Boolean);
    procedure SetMargin(Value: Integer);

    procedure SetHotTab(Tab: TJvTabBarItem);
    procedure SetClosingTab(Tab: TJvTabBarItem);
    procedure UpdateScrollButtons;
    function GetScrollBarGlyph(Left: Boolean): TBitmap;
    function FindSelectableTab(Tab: TJvTabBarItem): TJvTabBarItem;
    procedure SetHint(const Value: TCaption);
    procedure SetFlatScrollButtons(const Value: Boolean);
  protected
    procedure Resize; override;
    procedure CalcTabsRects;
    procedure Paint; override;
    procedure PaintTab(Canvas: TCanvas; Tab: TJvTabBarItem); virtual;

    function GetTabWidth(Tab: TJvTabBarItem): Integer;
    function GetTabHeight(Tab: TJvTabBarItem): Integer;

    function CurrentPainter: TJvTabBarPainter;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    function TabClosing(Tab: TJvTabBarItem): Boolean; virtual;
    procedure TabClosed(Tab: TJvTabBarItem); virtual;
    function TabSelecting(Tab: TJvTabBarItem): Boolean; virtual;
    procedure TabSelected(Tab: TJvTabBarItem); virtual;
    procedure Changed; virtual;
    procedure ImagesChanged(Sender: TObject); virtual;
    procedure ScrollButtonClicked(Sender: TObject); virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    {$IFDEF VCL}
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure MouseLeave(AControl: TControl); override;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddTab(const Caption: string): TJvTabBarItem;
    function TabAt(X, Y: Integer): TJvTabBarItem;
    function MakeVisible(Tab: TJvTabBarItem): Boolean;

    property Tabs: TJvTabBarItems read FTabs write SetTabs;
    property Painter: TJvTabBarPainter read FPainter write SetPainter;
    property Images: TImageList read FImages write SetImages;

    // Status
    property SelectedTab: TJvTabBarItem read FSelectedTab write SetSelectedTab;
    property LeftTab: TJvTabBarItem read GetLeftTab write SetLeftTab;
    property HotTab: TJvTabBarItem read FHotTab;
    property ClosingTab: TJvTabBarItem read FClosingTab;

    // Options
    property CloseButton: Boolean read FCloseButton write SetCloseButton default True;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default True;
    property HotTracking: Boolean read FHotTracking write FHotTracking default False;
    property AutoFreeClosed: Boolean read FAutoFreeClosed write FAutoFreeClosed default True;
    property AllowUnselected: Boolean read FAllowUnselected write FAllowUnselected default False;
    property Margin: Integer read FMargin write SetMargin default 6;
    property FlatScrollButtons: Boolean read FFlatScrollButtons write SetFlatScrollButtons default False;
    property Hint: TCaption read FHint write SetHint;

    // Events
    property OnTabClosing: TJvTabBarClosingEvent read FOnTabClosing write FOnTabClosing;
    property OnTabClosed: TJvTabBarItemEvent read FOnTabClosed write FOnTabClosed;
    property OnTabSelecting: TJvTabBarSelectingEvent read FOnTabSelecting write FOnTabSelecting;
    property OnTabSelected: TJvTabBarItemEvent read FOnTabSelected write FOnTabSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvTabBar = class(TJvCustomTabBar)
  published
    property Align default alTop;
    property Cursor;
    property PopupMenu;
    property ShowHint default False;
    property Height default 23;
    property Hint;

    property CloseButton;
    property RightClickSelect;
    property HotTracking;
    property AutoFreeClosed;
    property AllowUnselected;
    property Margin;

    property Tabs;
    property Painter;
    property Images;

    property OnTabClosing;
    property OnTabClosed;
    property OnTabSelecting;
    property OnTabSelected;
    property OnChange;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;

    property OnClick;
    property OnDblClick;

    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;

    {$IFDEF VCL}
    {$IFNDEF WINFORMS}
    property OnStartDock;
    property OnEndDock;
    {$ENDIF ~WINFORMS}
    {$ENDIF VCL}
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

type
{$IFDEF VCL}
  TCanvasX = TCanvas;
{$ENDIF VCL}
{$IFDEF VisualCLX}
  TCanvasX = class(TCanvas)
    // LineTo under CLX draws the last point, Windows doesn't. This wrapper
    // restores the last point.
    procedure LineTo(X, Y: Integer);
  end;

procedure TCanvasX.LineTo(X, Y: Integer);
var
  C: TColor;
begin
  C := Pixels[X, Y];
  inherited LineTo(X, Y);
  Pixels[X, Y] := C;
end;
{$ENDIF VisualCLX}

{$IFDEF COMPILER5}
{ TOwnedCollection }

procedure TOwnedCollection.Update(Item: TCollectionItem);
begin
  Notify(Item, cnAdded);
end;

procedure TOwnedCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
end;
{$ENDIF COMPILER5}

{ TJvCustomTabBar }

constructor TJvCustomTabBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  
  FTabs := TJvTabBarItems.Create(Self, TJvTabBarItem);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChanged;

  FRightClickSelect := True;
  FCloseButton := True;
  FAutoFreeClosed := True;

  FMargin := 6;

  Align := alTop;
  Height := 23;
end;

destructor TJvCustomTabBar.Destroy;
begin
  // these events are too dangerous while object destruction
  FOnTabSelected := nil;
  FOnTabSelecting := nil;
  FOnChange := nil;

  Painter := nil;
  Images := nil;
  FChangeLink.Free;
  FreeAndNil(FTabs);
  FreeAndNil(FBmpLeftScroll);
  FreeAndNil(FBmpRightScroll);
  inherited Destroy;
end;

procedure TJvCustomTabBar.Notification(Component: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(Component, Operation);
  if Operation = opRemove then
  begin
    if Component = FPainter then
      Painter := nil
    else
    if Component = FImages then
      Images := nil;
  end;
  if Assigned(FTabs) then
    for I := Tabs.Count - 1 downto 0 do
      Tabs[I].Notification(Component, Operation);
end;

function TJvCustomTabBar.GetScrollBarGlyph(Left: Boolean): TBitmap;
const
  W = 6;
  H = 6;
var
  Pts: array[0..2] of TPoint;
begin
  if Left then
    Result := FBmpLeftScroll
  else
    Result := FBmpRightScroll;

  if not Assigned(Result) then
  begin
    Result := TBitmap.Create;
    Result.Width := 8;
    Result.Height := 8;

    if Left then
    begin
      Pts[0] := Point(W div 2 + 1, 0);
      Pts[1] := Point(W div 2 + 1, H);
      Pts[2] := Point(0 + 1, H div 2);
    end
    else
    begin
      Pts[0] := Point(W div 2 - 1, 0);
      Pts[1] := Point(W div 2 - 1, H);
      Pts[2] := Point(W - 1, H div 2);
    end;
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := clBlack;
    Result.Canvas.Pen.Color := Result.Canvas.Brush.Color;
    Result.Canvas.Polygon(Pts)
  end;

  if Left then
    FBmpLeftScroll := Result
  else
    FBmpRightScroll := Result;
end;

procedure TJvCustomTabBar.SetTabs(Value: TJvTabBarItems);
begin
  if Value <> FTabs then
    FTabs.Assign(Value);
end;

procedure TJvCustomTabBar.SetPainter(Value: TJvTabBarPainter);
begin
  if Value <> FPainter then
  begin
    if Assigned(FPainter) then
    begin
      FPainter.FOnChange := nil;
      FPainter.RemoveFreeNotification(Self);
    end;
    FPainter := Value;
    if Assigned(FPainter) then
    begin
      FreeAndNil(FDefaultPainter);
      FPainter.FreeNotification(Self);
      FPainter.FOnChange := ImagesChanged;
    end;

    if not (csDestroying in ComponentState) then
      Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetImages(Value: TImageList);
begin
  if Value <> FImages then
  begin
    if Assigned(FImages) then
    begin
      FImages.UnregisterChanges(FChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FChangeLink);
      FImages.FreeNotification(Self);
    end;

    if not (csDestroying in ComponentState) then
      Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetCloseButton(Value: Boolean);
begin
  if Value <> FCloseButton then
  begin
    FCloseButton := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetMargin(Value: Integer);
begin
  if Value <> FMargin then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetSelectedTab(Value: TJvTabBarItem);
begin
  if Value <> FSelectedTab then
  begin
    if Assigned(Value) and not Value.CanSelect then
      Exit;

    if TabSelecting(Value) then
    begin
      FSelectedTab := Value;
      if not (csDestroying in ComponentState) then
        Invalidate;
      MakeVisible(FSelectedTab);
      TabSelected(FSelectedTab);
    end;
  end;
end;

function TJvCustomTabBar.CurrentPainter: TJvTabBarPainter;
begin
  Result := FPainter;
  if not Assigned(Result) then
  begin
    if not Assigned(FDefaultPainter) then
      FDefaultPainter := TJvModernTabBarPainter.Create(Self);
    Result := FDefaultPainter;
  end;
end;

function TJvCustomTabBar.TabClosing(Tab: TJvTabBarItem): Boolean;
begin
  Result := True;
  if Assigned(FOnTabClosing) then
    FOnTabClosing(Self, Tab, Result);
end;

procedure TJvCustomTabBar.TabClosed(Tab: TJvTabBarItem);
begin
  if AutoFreeClosed then
    Tab.Visible := False;
  try
    if Assigned(FOnTabClosed) then
      FOnTabClosed(Self, Tab);
  finally
    if AutoFreeClosed then
      Tab.Free;
  end;
end;

function TJvCustomTabBar.TabSelecting(Tab: TJvTabBarItem): Boolean;
begin
  Result := True;
  if Assigned(FOnTabSelecting) then
    FOnTabSelecting(Self, Tab, Result);
end;

procedure TJvCustomTabBar.TabSelected(Tab: TJvTabBarItem);
begin
  if Assigned(FOnTabSelected) then
    FOnTabSelected(Self, Tab);
end;

function TJvCustomTabBar.FindSelectableTab(Tab: TJvTabBarItem): TJvTabBarItem;
var
  Index: Integer;
begin
  Result := Tab;
  if Assigned(Result) and not Result.CanSelect then
  begin
    if AllowUnselected then
      Result := nil
    else
    begin
      Index := Result.Index + 1;
      while Index < Tabs.Count do
      begin
        if Tabs[Index].CanSelect then
          Break;
        Inc(Index);
      end;
      if Index >= Tabs.Count then
      begin
        Index := Result.Index - 1;
        while Index >= 0 do
        begin
          if Tabs[Index].CanSelect then
            Break;
          Dec(Index);
        end;
      end;
      if Index >= 0 then
        Result := Tabs[Index]
      else
        Result := nil;
    end;
  end;
  if not AllowUnselected and not Assigned(Result) then
  begin
    // try to find a selectable tab
    for Index := 0 to Tabs.Count - 1 do
      if Tabs[Index].CanSelect then
      begin
        Result := Tabs[Index];
        Break;
      end;
  end;
end;

procedure TJvCustomTabBar.Changed;
begin
  if not (csDestroying in ComponentState) then
  begin
    // The TabSelected tab is now no more selectable
    SelectedTab := FindSelectableTab(SelectedTab);

    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
    UpdateScrollButtons;
  end;
end;

procedure TJvCustomTabBar.ImagesChanged(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

{$IFDEF VCL}
procedure TJvCustomTabBar.CMMouseLeave(var Message: TMessage);
begin
  SetHotTab(nil);
  inherited;
end;

procedure TJvCustomTabBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomTabBar.MouseLeave(AControl: TControl);
begin
  SetHotTab(nil);
  inherited MouseLeave(AControl);
end;
{$ENDIF VisualCLX}

procedure TJvCustomTabBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Tab: TJvTabBarItem;
begin
  if Button = mbLeft then
  begin
    FMouseDownClosingTab := nil;
    SetClosingTab(nil); // no tab should be closed

    Tab := TabAt(X, Y);
    if Assigned(Tab) then
      SelectedTab := Tab;

    if Assigned(Tab) and (Tab = SelectedTab) then
    begin
      if CloseButton and PtInRect(CurrentPainter.GetCloseRect(Canvas, Tab, Tab.DisplayRect), Point(X, Y)) then
      begin
        if TabClosing(Tab) then
        begin
          FMouseDownClosingTab := Tab;
          SetClosingTab(Tab);
        end;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomTabBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Pt: TPoint;
  Tab: TJvTabBarItem;
begin
  try
    if RightClickSelect and not Assigned(PopupMenu) and (Button = mbRight) then
    begin
      Tab := TabAt(X, Y);
      if Assigned(Tab) then
        SelectedTab := Tab;
      if Assigned(Tab) and Assigned(Tab.PopupMenu) then
      begin
        Pt := ClientToScreen(Point(X, Y));
        Tab.PopupMenu.Popup(Pt.X, Pt.Y);
      end;
    end
    else
    if Button = mbLeft then
    begin
      if Assigned(FClosingTab) and CloseButton then
      begin
        CalcTabsRects;
        if PtInRect(CurrentPainter.GetCloseRect(Canvas, FClosingTab, FClosingTab.DisplayRect), Point(X, Y)) then
          TabClosed(FClosingTab);
      end;
    end;
  finally
    FMouseDownClosingTab := nil;
    SetClosingTab(nil);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomTabBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Tab: TJvTabBarItem;
  NewHint: TCaption;
begin
  CalcTabsRects;
  Tab := TabAt(X, Y);
  if HotTracking and ([ssLeft, ssMiddle, ssRight] * Shift = []) then
    SetHotTab(Tab);

  if CloseButton and Assigned(FMouseDownClosingTab) and (ssLeft in Shift) then
  begin
    if PtInRect(CurrentPainter.GetCloseRect(Canvas, FMouseDownClosingTab, FMouseDownClosingTab.DisplayRect), Point(X, Y)) then
      SetClosingTab(FMouseDownClosingTab)
    else
      SetClosingTab(nil)
  end;

  if Assigned(Tab) and Tab.ShowHint then
    NewHint := Tab.Hint
  else
    NewHint := FHint;

  if NewHint <> inherited Hint then
  begin
    Application.CancelHint;
    ShowHint := False;
    ShowHint := True;
    inherited Hint := NewHint;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TJvCustomTabBar.SetHotTab(Tab: TJvTabBarItem);
begin
  if (csDestroying in ComponentState) or not HotTracking then
  begin
    FHotTab := nil;
    Exit;
  end;

  if Tab <> FHotTab then
  begin
    FHotTab := Tab;
    Paint;
  end;
end;

function TJvCustomTabBar.AddTab(const Caption: string): TJvTabBarItem;
begin
  Result := TJvTabBarItem(Tabs.Add);
  Result.Caption := Caption;
end;

procedure TJvCustomTabBar.CalcTabsRects;
var
  I, X: Integer;
  Tab: TJvTabBarItem;
  Offset: Integer;
  Index: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  Offset := 0;
  X := Margin;  // adjust for scrolled area
  Index := 0;
  for I := 0 to Tabs.Count - 1 do
  begin
    Tab := Tabs[I];
    if Tab.Visible then
    begin
      Tab.FLeft := X;
      Inc(X, GetTabWidth(Tab));
      Inc(X, CurrentPainter.GetDividerWidth(Canvas, Tab));
      if Index < FLeftIndex then
      begin
        Inc(Offset, X); // this tab is placed too left.
        X := 0;
        Tab.FLeft := -Offset - 10;
      end;
      Inc(Index);
    end
    else
      Tab.FLeft := -1;
  end;

  FRequiredWidth := X + Offset;
  FLastTabRight := X;
end;

type
  TSpeedButtonAccess = class(TSpeedButton);
  
procedure TJvCustomTabBar.Paint;
var
  I: Integer;
  Bmp: TBitmap;
  R: TRect;
begin
  CalcTabsRects;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    CurrentPainter.DrawBackground(Bmp.Canvas, Self, ClientRect);
    if Assigned(FBtnLeftScroll) and Assigned(FBtnRightScroll) then
    begin
      // paint scroll button's background and the buttons
      R := Rect(FBarWidth, 0, Bmp.Width, Bmp.Height);
      Canvas.CopyRect(R, Bmp.Canvas, R);
      TSpeedButtonAccess(FBtnLeftScroll).Paint;
      TSpeedButtonAccess(FBtnRightScroll).Paint;
      if FBarWidth > 0 then
        Bmp.Width := FBarWidth;
    end;

    if FBarWidth > 0 then
      for I := 0 to Tabs.Count - 1 do
        if Tabs[I].Visible then
          PaintTab(Bmp.Canvas, Tabs[I]);
    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJvCustomTabBar.PaintTab(Canvas: TCanvas; Tab: TJvTabBarItem);
var
  R: TRect;
begin
  if csDestroying in ComponentState then
    Exit;

  if Tab.Visible then
  begin
    R := Tab.DisplayRect;
    if (R.Right >= 0) and (R.Left < FBarWidth) then
    begin
      CurrentPainter.DrawTab(Canvas, Tab, R);
      R.Left := R.Right;
      R.Right := R.Left + CurrentPainter.GetDividerWidth(Canvas, Tab) - 1;
      CurrentPainter.DrawDivider(Canvas, Tab, R);
    end;
  end;
end;

function TJvCustomTabBar.GetTabHeight(Tab: TJvTabBarItem): Integer;
begin
  Result := CurrentPainter.GetTabSize(Canvas, Tab).cy;
end;

function TJvCustomTabBar.GetTabWidth(Tab: TJvTabBarItem): Integer;
begin
  Result := CurrentPainter.GetTabSize(Canvas, Tab).cx;
end;

function TJvCustomTabBar.TabAt(X, Y: Integer): TJvTabBarItem;
var
  I: Integer;
  Pt: TPoint;
begin
  if not Assigned(FBtnLeftScroll) or (X < FBarWidth) then
  begin
    CalcTabsRects;
    Pt := Point(X, Y);
    for I := 0 to Tabs.Count - 1 do
      if PtInRect(Tabs[i].DisplayRect, Pt) then
      begin
        Result := Tabs[i];
        Exit;
      end;
  end;
  Result := nil;
end;

procedure TJvCustomTabBar.SetClosingTab(Tab: TJvTabBarItem);
begin
  if Tab <> FClosingTab then
  begin
    FClosingTab := Tab; // this tab should be TabClosed
    Paint;
  end;
end;

function TJvCustomTabBar.GetLeftTab: TJvTabBarItem;
begin
  if FLeftIndex < Tabs.Count then
  begin
    Result := Tabs[FLeftIndex];
    if not Result.Visible then
      Result := Result.GetNextVisible;
  end
  else
    Result := nil;
end;

procedure TJvCustomTabBar.SetLeftTab(Value: TJvTabBarItem);
var
  Index: Integer;
  Tab: TJvTabBarItem;
begin
  Index := 0;
  if Assigned(Value) then
  begin
    // find first visible before or at Value.Index
    Index := 0;
    if (Tabs.Count > 0) and (Value <> Tabs[0]) then
    begin
      while Index < Tabs.Count do
      begin
        Tab := Tabs[Index].GetNextVisible;
        if Tab = nil then
        begin
          Index := FLeftIndex; // do not change
          Break;
        end
        else
        begin
          Index := Tab.Index;
          if Tab.Index >= Value.Index then
            Break;
        end;
      end;
      if Index >= Tabs.Count then
        Index := FLeftIndex; // do not change
    end;
  end;
  if Index <> FLeftIndex then
  begin
    FLeftIndex := Index;
    Invalidate;
  end;
end;

procedure TJvCustomTabBar.UpdateScrollButtons;
const
  BtnSize = 12;
begin
  CalcTabsRects;
  if (FRequiredWidth < ClientWidth) or ((FLeftIndex = 0) and (FLastTabRight <= ClientWidth)) then
  begin
    FreeAndNil(FBtnLeftScroll);
    FreeAndNil(FBtnRightScroll);
    FLeftIndex := 0;
    FBarWidth := ClientWidth;
    Invalidate;
  end
  else
  begin
    if not Assigned(FBtnLeftScroll) then
    begin
      FBtnLeftScroll := TSpeedButton.Create(Self);
      FBtnLeftScroll.Caption := '';
      FBtnLeftScroll.Flat := FlatScrollButtons;
      FBtnLeftScroll.Parent := Self;
      FBtnLeftScroll.OnClick := ScrollButtonClicked;
      FBtnLeftScroll.Glyph := GetScrollBarGlyph(True);
    end;
    if not Assigned(FBtnRightScroll) then
    begin
      FBtnRightScroll := TSpeedButton.Create(Self);
      FBtnRightScroll.Caption := '';
      FBtnRightScroll.Flat := FlatScrollButtons;
      FBtnRightScroll.Parent := Self;
      FBtnRightScroll.OnClick := ScrollButtonClicked;
      FBtnRightScroll.Glyph := GetScrollBarGlyph(False);
    end;

    FBtnLeftScroll.SetBounds(ClientWidth - BtnSize * 2 - 1 - 1, ClientHeight - BtnSize - 2, BtnSize, BtnSize);
    FBtnRightScroll.SetBounds(ClientWidth - BtnSize - 1 - 1, ClientHeight - BtnSize - 2, BtnSize, BtnSize);

    FBarWidth := FBtnLeftScroll.Left - 2;

    FBtnLeftScroll.Enabled := FLeftIndex > 0;
    FBtnRightScroll.Enabled := FLastTabRight > ClientWidth;
  end;
end;

procedure TJvCustomTabBar.Resize;
begin
  UpdateScrollButtons;
  inherited Resize;
end;

procedure TJvCustomTabBar.ScrollButtonClicked(Sender: TObject);
begin
  if Sender = FBtnLeftScroll then
    Dec(FLeftIndex)
  else
  if Sender = FBtnRightScroll then
    Inc(FLeftIndex);
  UpdateScrollButtons;
  Invalidate;
end;

function TJvCustomTabBar.MakeVisible(Tab: TJvTabBarItem): Boolean;
var
  R: TRect;
  LastLeftIndex: Integer;
begin
  Result := False;
  if not Assigned(Tab) or not Tab.Visible then
    Exit;

  LastLeftIndex := FLeftIndex;
  repeat
    CalcTabsRects;
    R := Tab.DisplayRect;
    if R.Right > FBarWidth then
      Inc(FLeftIndex)
    else
      Break;
  until FLeftIndex = Tabs.Count - 1;
  if (R.Left < 0) and (FLeftIndex > 0) then
    Dec(FLeftIndex); // bar is too small
  if FLeftIndex <> LastLeftIndex then
  begin
    UpdateScrollButtons;
    Invalidate;
  end;
end;

procedure TJvCustomTabBar.SetHint(const Value: TCaption);
begin
  if Value <> FHint then
    FHint := Value;
end;

procedure TJvCustomTabBar.SetFlatScrollButtons(const Value: Boolean);
begin
  if Value <> FFlatScrollButtons then
  begin
    FFlatScrollButtons := Value;
    FreeAndNil(FBtnLeftScroll);
    FreeAndNil(FBtnRightScroll);
    UpdateScrollButtons;
  end;
end;

{ TJvTabBarItem }

constructor TJvTabBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FEnabled := True;
  FVisible := True;
  FShowHint := True;
end;

destructor TJvTabBarItem.Destroy;
begin
  PopupMenu := nil;
  {$IFDEF COMPILER5}
  TOwnedCollection(GetOwner).Notify(Self, cnDeleting);
  {$ENDIF COMPILER5}
  inherited Destroy;
end;

procedure TJvTabBarItem.Assign(Source: TPersistent);
begin
  if Source is TJvTabBarItem then
  begin
    with TJvTabBarItem(Source) do
    begin
      Self.FImageIndex := FImageIndex;
      Self.FEnabled := FEnabled;
      Self.FVisible := FVisible;
      Self.FTag := FTag;
      Self.FData := FData;
      Self.FHint := FHint;
      Self.FShowHint := FShowHint;
      Self.FName := FName;
      Self.FCaption := FCaption;
      Self.FModified := FModified;
      Self.FImages := FImages;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTabBarItem.Notification(Component: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Component = PopupMenu then
      PopupMenu := nil;
  end;
end;

procedure TJvTabBarItem.Changed;
begin
  TabBar.Changed;
end;

function TJvTabBarItem.GetDisplayRect: TRect;
begin
  if not Visible then
    Result := Rect(-1, -1, -1, -1)
  else
  begin
    if FLeft = -1 then
      TabBar.CalcTabsRects; // not initialized

    case TabBar.Align of
      alBottom:
          Result := Rect(FLeft, 0,
            FLeft + TabBar.GetTabWidth(Self), 0 + TabBar.GetTabHeight(Self));
    else
      // Top
      Result := Rect(FLeft, TabBar.ClientHeight - TabBar.GetTabHeight(Self),
          FLeft + TabBar.GetTabWidth(Self), TabBar.ClientHeight);
    end;
  end;
end;

function TJvTabBarItem.GetHot: Boolean;
begin
  Result := TabBar.HotTab = Self;
end;

function TJvTabBarItem.GetImages: TCustomImageList;
begin
  Result := TabBar.Images;
end;

function TJvTabBarItem.GetSelected: Boolean;
begin
  Result := TabBar.SelectedTab = Self;
end;

function TJvTabBarItem.GetTabBar: TJvCustomTabBar;
begin
  Result := (GetOwner as TJvTabBarItems).TabBar;
end;

procedure TJvTabBarItem.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetImageIndex(const Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetName(const Value: string);
begin
  if (Value <> FName) and (TJvTabBarItems(Collection).Find(Value) = nil) then
    FName := Value;
end;

procedure TJvTabBarItem.SetSelected(const Value: Boolean);
begin
  if Value then
    TabBar.SelectedTab := Self;
end;

procedure TJvTabBarItem.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    FLeft := -1; // discard
    Changed;
  end;
end;

function TJvTabBarItem.CanSelect: Boolean;
begin
  Result := Visible and Enabled;
end;

function TJvTabBarItem.GetNextVisible: TJvTabBarItem;
var
  I: Integer;
begin
  for I := Index + 1 to TabBar.Tabs.Count - 1 do
    if TabBar.Tabs[I].Visible then
    begin
      Result := TabBar.Tabs[I];
      Exit;
    end;
  Result := nil;
end;

function TJvTabBarItem.GetPreviousVisible: TJvTabBarItem;
var
  I: Integer;
begin
  for I := Index - 1 downto 0 do
    if TabBar.Tabs[I].Visible then
    begin
      Result := TabBar.Tabs[I];
      Exit;
    end;
  Result := nil;
end;

function TJvTabBarItem.GetClosing: Boolean;
begin
  Result := TabBar.ClosingTab = Self;
end;

procedure TJvTabBarItem.SetModified(const Value: Boolean);
begin
  if Value <> FModified then
  begin
    FModified := Value;
    Changed;
  end;
end;

procedure TJvTabBarItem.SetPopupMenu(const Value: TPopupMenu);
begin
  if Value <> FPopupMenu then
  begin
    if Assigned(FPopupMenu) then
      FPopupMenu.RemoveFreeNotification(TabBar);
    FPopupMenu := Value;
    if Assigned(FPopupMenu) then
      FPopupMenu.FreeNotification(TabBar);
  end;
end;

procedure TJvTabBarItem.MakeVisible;
begin
  TabBar.MakeVisible(Self);
end;

function TJvTabBarItem.GetEnabled: Boolean;
begin
  Result := FEnabled;
  if Assigned(FOnGetEnabled) then
    FOnGetEnabled(Self, Result);
end;

function TJvTabBarItem.GetModified: Boolean;
begin
  Result := FModified;
  if Assigned(FOnGetModified) then
    FOnGetModified(Self, Result);
end;

procedure TJvTabBarItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  Changed;
end;

{ TJvTabBarItems }

function TJvTabBarItems.Find(const AName: string): TJvTabBarItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJvTabBarItems.GetTabBar: TJvCustomTabBar;
begin
  Result := GetOwner as TJvTabBar;
end;

function TJvTabBarItems.GetItem(Index: Integer): TJvTabBarItem;
begin
  Result := TJvTabBarItem(inherited Items[Index]);
end;

procedure TJvTabBarItems.SetItem(Index: Integer; const Value: TJvTabBarItem);
begin
  if Value <> GetItem(Index) then
    GetItem(Index).Assign(Value);
end;

procedure TJvTabBarItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action in [cnExtracting, cnDeleting] then
  begin
    // unselect the item to delete
    if TabBar.SelectedTab = Item then
      TabBar.SelectedTab := nil;
    if TabBar.HotTab = Item then
      TabBar.SetHotTab(nil);
    if TabBar.ClosingTab = Item then
      TabBar.FClosingTab := nil;
  end;
  TabBar.Changed;
end;

function TJvTabBarItems.IndexOf(Item: TJvTabBarItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = Item then
      Exit;
  Result := -1;
end;

{ TJvTabBarPainter }

procedure TJvTabBarPainter.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TJvModernTabBarPainter }

constructor TJvModernTabBarPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FDisabledFont := TFont.Create;

  FFont.Color := clWindowText;
  FDisabledFont.Color := clGrayText;

  FFont.OnChange := FontChanged;
  FDisabledFont.OnChange := FontChanged;

  FTabColor := clBtnFace;
  FColor := clWindow;
  FBorderColor := clSilver;
  FControlDivideColor := clBlack;

  FModifiedCrossColor := clRed;
  FCloseColorSelected := $F4F4F4;
  FCloseColor := clWhite;
  FCloseCrossColorSelected := clBlack;
  FCloseCrossColor := $5D5D5D;
  FCloseCrossColorDisabled := $ADADAD;
  FCloseRectColor := $868686;
  FCloseRectColorDisabled := $D6D6D6;
  FDividerColor := $99A8AC;
end;

destructor TJvModernTabBarPainter.Destroy;
begin
  FFont.Free;
  FDisabledFont.Free;
  inherited Destroy;
end;

procedure TJvModernTabBarPainter.DrawBackground(Canvas: TCanvas; TabBar: TJvCustomTabBar; R: TRect);
begin
  with TCanvasX(Canvas) do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(R);

    Brush.Style := bsClear;
    Pen.Color := BorderColor;
    case TabBar.Align of
      alBottom:
        begin
          MoveTo(0, R.Bottom - 1);
          LineTo(0, 0);
          Pen.Color := ControlDivideColor;
          LineTo(R.Right - 1, 0);
          Pen.Color := BorderColor;
          LineTo(R.Right - 1, R.Bottom - 1);
          LineTo(0, R.Bottom - 1);
        end;
    else
      // Top
      MoveTo(0, R.Bottom - 1);
      LineTo(0, 0);
      LineTo(R.Right - 1, 0);
      LineTo(R.Right - 1, R.Bottom - 1);
      Pen.Color := ControlDivideColor;
      LineTo(0, R.Bottom - 1);
    end;
  end;
end;

procedure TJvModernTabBarPainter.DrawDivider(Canvas: TCanvas;
  LeftTab: TJvTabBarItem; R: TRect);
begin
  if not LeftTab.Selected then
  begin
    if not Assigned(LeftTab.TabBar.SelectedTab) or (LeftTab.GetNextVisible <> LeftTab.TabBar.SelectedTab) then
    begin
      with TCanvasX(Canvas) do
      begin
        Pen.Color := DividerColor;
        MoveTo(R.Right - 1, R.Top + 3);
        LineTo(R.Right - 1, R.Bottom - 3);
      end;
    end;
  end;
end;

procedure TJvModernTabBarPainter.DrawTab(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect);
var
  CloseR: TRect;
begin
  with TCanvasX(Canvas) do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;

    if Tab.Selected then
    begin
      Brush.Style := bsSolid;
      Brush.Color := TabColor;
      FillRect(R);

      Pen.Color := ControlDivideColor;
      case Tab.TabBar.Align of
        alBottom:
          begin
            MoveTo(R.Left, R.Top);
            LineTo(R.Left, R.Bottom - 1);
            LineTo(R.Right - 1, R.Bottom - 1);
            LineTo(R.Right - 1, R.Top - 1{end});
          end;
      else
        // Top
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Left, R.Top);
        LineTo(R.Right - 1, R.Top);
        LineTo(R.Right - 1, R.Bottom - 1 + 1{end});
      end;
    end;

    if Tab.TabBar.CloseButton then
    begin
      // close button color
      if Tab.Selected then
        Brush.Color := CloseColorSelected
      else
        Brush.Color := CloseColor;

      if Tab.Enabled and not Tab.Selected and Tab.Hot then
      begin
        // hot
        Pen.Color := DividerColor;
        MoveTo(R.Left, R.Top);
        LineTo(R.Right - 1 - 1, R.Top);
      end;

      CloseR := GetCloseRect(Canvas, Tab, Tab.DisplayRect);
      Pen.Color := CloseRectColor;
      if not Tab.Enabled then
        Pen.Color := CloseRectColorDisabled;

      if Tab.Closing then
        // shrink
        Rectangle(CloseR.Left + 1, CloseR.Top + 1, CloseR.Right - 1, CloseR.Bottom - 1)
      else
        Rectangle(CloseR);

      if Tab.Modified then
        Pen.Color := ModifiedCrossColor
      else
      if Tab.Selected and not Tab.Closing then
        Pen.Color := CloseCrossColorSelected
      else
      if Tab.Enabled then
        Pen.Color := CloseCrossColor
      else
        Pen.Color := CloseCrossColorDisabled;

      // close cross
      MoveTo(CloseR.Left + 3, CloseR.Top + 3);
      LineTo(CloseR.Right - 3, CloseR.Bottom - 3);
      MoveTo(CloseR.Left + 4, CloseR.Top + 3);
      LineTo(CloseR.Right - 4, CloseR.Bottom - 3);

      MoveTo(CloseR.Right - 4, CloseR.Top + 3);
      LineTo(CloseR.Left + 2, CloseR.Bottom - 3);
      MoveTo(CloseR.Right - 5, CloseR.Top + 3);
      LineTo(CloseR.Left + 3, CloseR.Bottom - 3);

      // remove intersection
      if Tab.Modified then
        FillRect(Rect(CloseR.Left + 5, CloseR.Top + 4, CloseR.Right - 5, CloseR.Bottom - 4));

      R.Left := CloseR.Right;
    end;

    InflateRect(R, -1, -1);

    if not Tab.TabBar.CloseButton then
      Inc(R.Left, 2);

    if (Tab.ImageIndex <> -1) and (Tab.GetImages <> nil) then
    begin
      Tab.GetImages.Draw(Canvas, R.Left, R.Top + (R.Bottom - R.Top - Tab.GetImages.Height) div 2,
        Tab.ImageIndex, {$IFDEF VisualCLX}itImage,{$ENDIF} Tab.Enabled);
      Inc(R.Left, Tab.GetImages.Width + 2);
    end;

    if Tab.Enabled then
      Font.Assign(Self.Font)
    else
      Font.Assign(Self.DisabledFont);

    Brush.Style := bsClear;
    TextRect(R, R.Left + 3, R.Top + 3, Tab.Caption);
  end;
end;

function TJvModernTabBarPainter.GetCloseRect(Canvas: TCanvas; Tab: TJvTabBarItem; R: TRect): TRect;
begin
  Result.Left := R.Left + 5;
  Result.Top :=  R.Top + 5;
  Result.Right := Result.Left + 12;
  Result.Bottom := Result.Top + 11;
end;

function TJvModernTabBarPainter.GetDividerWidth(Canvas: TCanvas; LeftTab: TJvTabBarItem): Integer;
begin
  Result := 1;
end;

function TJvModernTabBarPainter.GetTabSize(Canvas: TCanvas; Tab: TJvTabBarItem): TSize;
begin
  if Tab.Enabled then
    Canvas.Font.Assign(Font)
  else
    Canvas.Font.Assign(DisabledFont);

  Result.cx := Canvas.TextWidth(Tab.Caption) + 11;
  Result.cy := Canvas.TextHeight(Tab.Caption + 'Ag') + 7;
  if Tab.TabBar.CloseButton then
    Result.cx := Result.cx + 15;
  if (Tab.ImageIndex <> -1) and (Tab.GetImages <> nil) then
    Result.cx := Result.cx + Tab.GetImages.Width + 2;
end;

function TJvModernTabBarPainter.Options: TJvTabBarPainterOptions;
begin
  Result := [poPaintsHotTab];
end;

procedure TJvModernTabBarPainter.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvModernTabBarPainter.SetBorderColor(const Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetControlDivideColor(const Value: TColor);
begin
  if Value <> FControlDivideColor then
  begin
    FControlDivideColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetModifiedCrossColor(const Value: TColor);
begin
  if Value <> FModifiedCrossColor then
  begin
    FModifiedCrossColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetTabColor(const Value: TColor);
begin
  if Value <> FTabColor then
  begin
    FTabColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseColor(const Value: TColor);
begin
  if Value <> FCloseColor then
  begin
    FCloseColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseColorSelected(const Value: TColor);
begin
  if Value <> FCloseColorSelected then
  begin
    FCloseColorSelected := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseCrossColor(const Value: TColor);
begin
  if Value <> FCloseCrossColor then
  begin
    FCloseCrossColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseCrossColorDisabled(const Value: TColor);
begin
  if Value <> FCloseCrossColorDisabled then
  begin
    FCloseCrossColorDisabled := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseCrossColorSelected(const Value: TColor);
begin
  if Value <> FCloseCrossColorSelected then
  begin
    FCloseCrossColorSelected := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseRectColor(const Value: TColor);
begin
  if Value <> FCloseRectColor then
  begin
    FCloseRectColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetCloseRectColorDisabled(const Value: TColor);
begin
  if Value <> FCloseRectColorDisabled then
  begin
    FCloseRectColorDisabled := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetDividerColor(const Value: TColor);
begin
  if Value <> FDividerColor then
  begin
    FDividerColor := Value;
    Changed;
  end;
end;

procedure TJvModernTabBarPainter.SetDisabledFont(const Value: TFont);
begin
  if Value <> FDisabledFont then
    FDisabledFont.Assign(Value);
end;

procedure TJvModernTabBarPainter.SetFont(const Value: TFont);
begin
  if Value <> FFont then
    FFont.Assign(Value);
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

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
