{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNavigationPane.PAS, released on 2004-03-28.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
{$I jvcl.inc}

unit JvNavigationPane;

interface
uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Menus, ExtCtrls, ImgList,
  JvButton, JvPageList;

type
  TJvCustomNavigationPane = class;
  TJvNavIconButton = class;

  TJvNavPanelHeader = class(TCustomControl)
  private
    FColorFrom: TColor;
    FColorTo: TColor;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FChangeLink: TChangeLink;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender: TObject);
  protected
    procedure Paint; override;

    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property ColorFrom: TColor read FColorFrom write SetColorFrom default $D68652;
    property ColorTo: TColor read FColorTo write SetColorTo default $944110;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Height default 27;
    property Width default 225;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TJvNavPanelDivider = class(TSplitter)
  private
    FColorFrom: TColor;
    FColorTo: TColor;
    FFrameColor: TColor;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
  protected
    procedure Paint; override;
    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // NB! Color is published but not used
    property Align default alNone;
    property Anchors;
    property AutoSnap default False;
    property Caption;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default $FFE7CE;
    property ColorTo: TColor read FColorTo write SetColorTo default $E7A67B;
    property Constraints;
    property Cursor default crSizeNS;
    property Enabled;
    property Font;
    property FrameColor: TColor read FFrameColor write SetFrameColor default $943000;
    property Height default 19;
    property ResizeStyle default rsUpdate;
    property Width default 125;
  end;

  TJvOutlookSplitter = class(TSplitter)
  private
    FColorTo: TColor;
    FColorFrom: TColor;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
  protected
    procedure Paint; override;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // NB! Color is published but not used
    property Align default alBottom;
    property AutoSnap default False;
    property ResizeStyle default rsUpdate;
    property ColorFrom: TColor read FColorFrom write SetColorFrom default $D68652;
    property ColorTo: TColor read FColorTo write SetColorTo default $944110;
    property Height default 7;
    property Cursor default crSizeNS;
    property Enabled;
  end;

  TJvNavPanelColors = class(TPersistent)
  private
    FColorTo: TColor;
    FColorFrom: TColor;
    FFrameColor: TColor;
    FHotColorFrom: TColor;
    FSelectedColorFrom: TColor;
    FSelectedColorTo: TColor;
    FHotColorTo: TColor;
    FOnChange: TNotifyEvent;
    FSplitterColorFrom: TColor;
    FSplitterColorTo: TColor;
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetHotColorFrom(const Value: TColor);
    procedure SetHotColorTo(const Value: TColor);
    procedure SetSelectedColorFrom(const Value: TColor);
    procedure SetSelectedColorTo(const Value: TColor);
    procedure SetSplitterColorFrom(const Value: TColor);
    procedure SetSplitterColorTo(const Value: TColor);
  protected
    procedure Change;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property SplitterColorFrom: TColor read FSplitterColorFrom write SetSplitterColorFrom;
    property SplitterColorTo: TColor read FSplitterColorTo write SetSplitterColorTo;
    property ColorFrom: TColor read FColorFrom write SetColorFrom;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property HotColorFrom: TColor read FHotColorFrom write SetHotColorFrom;
    property HotColorTo: TColor read FHotColorTo write SetHotColorTo;
    property SelectedColorFrom: TColor read FSelectedColorFrom write SetSelectedColorFrom;
    property SelectedColorTo: TColor read FSelectedColorTo write SetSelectedColorTo;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvIconPanel = class(TCustomControl)
  private
    FDropButton: TJvNavIconButton;
    FColors: TJvNavPanelColors;
    procedure SetDropDownMenu(const Value: TPopupMenu);

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    function GetDropDownMenu: TPopupMenu;
    procedure SetColors(const Value: TJvNavPanelColors);
  protected
    procedure DoColorsChange(Sender: TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
  end;

  TJvNavIconButtonType = (nibDropDown, nibImage);

  TJvNavIconButton = class(TJvCustomGraphicButton)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FButtonType: TJvNavIconButtonType;
    FColors: TJvNavPanelColors;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure DoImagesChange(Sender: TObject);
    procedure SetButtonType(const Value: TJvNavIconButtonType);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure DoColorsChange(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AllowAllup;
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Down;
    property DropDownMenu;
    property GroupIndex;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property ButtonType: TJvNavIconButtonType read FButtonType write SetButtonType;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Width default 22;
    property Height default 22;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvNavPanelButton = class(TJvCustomGraphicButton)
  private
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FColors: TJvNavPanelColors;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure DoColorsChange(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property Width default 125;
    property Height default 28;

    property Colors: TJvNavPanelColors read FColors write SetColors;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvNavPanelPage = class(TJvCustomPage)
  private
    FNavPanel: TJvNavPanelButton;
    FIconButton: TJvNavIconButton;
    FOnClick: TNotifyEvent;
    FIconPanel: TJvIconPanel;
    procedure SetCaption(const Value: TCaption);
    procedure SetIconic(const Value: boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    function GetCaption: TCaption;
    function GetIconic: boolean;
    function GetImageIndex: TImageIndex;
    procedure DoButtonClick(Sender: TObject);
    function GetHint: string;
    procedure SetHint(const Value: string);

    procedure SetIconPanel(const Value: TJvIconPanel);
    function GetColors: TJvNavPanelColors;
    procedure SetColors(const Value: TJvNavPanelColors);
  protected
    procedure UpdatePageList;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetPageIndex(Value: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property NavPanel: TJvNavPanelButton read FNavPanel;
    property IconButton: TJvNavIconButton read FIconButton;
    property IconPanel: TJvIconPanel read FIconPanel write SetIconPanel;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
    property Colors: TJvNavPanelColors read GetColors write SetColors;
    property Caption: TCaption read GetCaption write SetCaption;
    property Iconic: boolean read GetIconic write SetIconic default False;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property Hint: string read GetHint write SetHint;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TJvCustomNavigationPane = class(TJvCustomPageList)
  private
    FIconPanel: TJvIconPanel;
    FSplitter: TJvOutlookSplitter;
    FLargeImages: TCustomImageList;
    FSmallImages: TCustomImageList;
    FColors: TJvNavPanelColors;
    FNavPanelFont: TFont;
    FResizable: boolean;
    function GetDropDownMenu: TPopupMenu;
    function GetSmallImages: TCustomImageList;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetLargeImages(const Value: TCustomImageList);
    procedure SetSmallImages(const Value: TCustomImageList);
    function GetMaximizedCount: integer;
    procedure SetMaximizedCount(Value: integer);
    procedure HidePanel(Index: integer);
    procedure ShowPanel(Index: integer);
    procedure SetColors(const Value: TJvNavPanelColors);
    procedure SetResizable(const Value: boolean);
    function GetNavPage(Index: integer): TJvNavPanelPage;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DoSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure SetNavPanelFont(const Value: TFont);
    procedure DoNavPanelFontChange(Sender:TObject);
    function IsNavPanelFontStored: Boolean;
  protected
    procedure SetActivePage(Page: TJvCustomPage); override;
    procedure InsertPage(APage: TJvCustomPage); override;
    procedure RemovePage(APage: TJvCustomPage); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function InternalGetPageClass: TJvCustomPageClass; override;
    property NavPages[Index: integer]: TJvNavPanelPage read GetNavPage;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdatePositions;
  protected
    property BorderWidth default 1;
    property NavPanelFont: TFont read FNavPanelFont write SetNavPanelFont stored IsNavPanelFontStored;
    property ParentColor default False;
    property Color default clWindow;
    property Colors: TJvNavPanelColors read FColors write SetColors;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property Resizable: boolean read FResizable write SetResizable default True;
    property SmallImages: TCustomImageList read GetSmallImages write SetSmallImages;
    property MaximizedCount: integer read GetMaximizedCount write SetMaximizedCount;
  end;

  TJvNavigationPane = class(TJvCustomNavigationPane)
  public
    property NavPages;
  published
    property ActivePage;
    property Align;
    property Anchors;
    property BorderWidth;
    property Caption;
    property Color;
    property Colors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownMenu;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property LargeImages;
    property MaximizedCount;
    property NavPanelFont;
    property Resizable;
    property SmallImages;
  end;

implementation
uses
  Forms, JvJVCLUtils;

const
  cButtonHeight = 28;
  cButtonWidth = 22;
  cSplitterHeight = 7;

{ TJvIconPanel }

constructor TJvIconPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  Height := cButtonHeight;
  FDropButton := TJvNavIconButton.Create(Self);
  FDropButton.Visible := false;
  FDropButton.ButtonType := nibDropDown;
  FDropButton.GroupIndex := 0;
  FDropButton.Width := cButtonWidth;
  FDropButton.Left := Width + 10;
  FDropButton.Align := alRight;
  FDropButton.Parent := self;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
end;

destructor TJvIconPanel.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TJvIconPanel.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

function TJvIconPanel.GetDropDownMenu: TPopupMenu;
begin
  Result := FDropButton.DropDownMenu;
end;

procedure TJvIconPanel.Paint;
begin
  GradientFillRect(Canvas, ClientRect, Colors.ColorFrom, Colors.ColorTo, fdTopToBottom, 32);
  Canvas.Pen.Color := Colors.FrameColor;
  if Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width + 1, 0);
  end
  else
  begin
    Canvas.MoveTo(0, ClientHeight - 1);
    Canvas.LineTo(Width + 1, ClientHeight - 1);
  end;
end;

procedure TJvIconPanel.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvIconPanel.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropButton.DropDownMenu := Value;
  FDropButton.Visible := Value <> nil;
end;

procedure TJvIconPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TJvCustomNavigationPane }

constructor TJvCustomNavigationPane.Create(AOwner: TComponent);
begin
  inherited;
  BorderWidth := 1;
  ParentColor := False;
  Color := clWindow;
  ControlStyle := ControlStyle + [csOpaque];
  FResizable := True;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  FIconPanel := TJvIconPanel.Create(Self);
  FIconPanel.Parent := Self;
  FIconPanel.Align := alBottom;
  FNavPanelFont := TFont.Create;
  FNavPanelFont.Assign(Screen.IconFont);
  FNavPanelFont.Style := [fsBold];
  FNavPanelFont.OnChange := DoNavPanelFontChange;

  FSplitter := TJvOutlookSplitter.Create(Self);
  with FSplitter do
  begin
    ResizeStyle := rsNone;
    MinSize := 1;
    OnCanResize := DoSplitterCanResize;
    Parent := Self;
  end;
end;

destructor TJvCustomNavigationPane.Destroy;
begin
  FColors.Free;
  FNavPanelFont.Free;
  inherited Destroy;
end;

procedure TJvCustomNavigationPane.DoSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
var
  ACount: integer;
begin
  ACount := MaximizedCount;
  if NewSize < cButtonHeight div 2 then
    MaximizedCount := ACount - 1
  else if NewSize > cButtonHeight + cButtonHeight div 2 then
    MaximizedCount := ACount + 1;
  NewSize := 0;
  Accept := False;
end;

function TJvCustomNavigationPane.GetDropDownMenu: TPopupMenu;
begin
  if FIconPanel <> nil then
    Result := FIconPanel.DropDownMenu
  else
    Result := nil;
end;

function TJvCustomNavigationPane.GetSmallImages: TCustomImageList;
begin
  Result := FSmallImages;
end;

function TJvCustomNavigationPane.GetMaximizedCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to PageCount - 1 do
    if not NavPages[i].Iconic then
      Inc(result);
end;

procedure TJvCustomNavigationPane.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = LargeImages then
      LargeImages := nil;
    if AComponent = SmallImages then
      SmallImages := nil;
  end;
end;

procedure TJvCustomNavigationPane.SetDropDownMenu(const Value: TPopupMenu);
begin
  if FIconPanel <> nil then
    FIconPanel.DropDownMenu := Value;
end;

procedure TJvCustomNavigationPane.SetLargeImages(const Value: TCustomImageList);
var
  i: integer;
begin
  if FLargeImages <> Value then
  begin
    FLargeImages := Value;
    for i := 0 to PageCount - 1 do
      NavPages[i].NavPanel.Images := FLargeImages;
  end;
end;

procedure TJvCustomNavigationPane.SetSmallImages(const Value: TCustomImageList);
var
  i: integer;
begin
  if FSmallImages <> Value then
  begin
    FSmallImages := Value;
    for i := 0 to PageCount - 1 do
      NavPages[i].IconButton.Images := FSmallImages;
  end;
end;

procedure TJvCustomNavigationPane.HidePanel(Index: integer);
begin
  if (Index >= 0) and (Index < PageCount) then // don't hide the first panel
    NavPages[Index].Iconic := true;
end;

procedure TJvCustomNavigationPane.ShowPanel(Index: integer);
begin
  if (Index >= 0) and (Index < PageCount) then
    NavPages[Index].Iconic := false;
end;

procedure TJvCustomNavigationPane.SetMaximizedCount(Value: integer);
var
  i, ACount: integer;
begin
  ACount := MaximizedCount;
  if (Value < 0) then Value := 0;
  if (Value > PageCount) then Value := PageCount;
  if Value = MaximizedCount then Exit;
  while ACount > Value do
  begin
    HidePanel(ACount - 1);
    Dec(ACount);
  end;
  if Value > ACount then
    for i := Value downto ACount do
      ShowPanel(i - 1);
  UpdatePositions;
end;

procedure TJvCustomNavigationPane.WMNCPaint(var Message: TWMNCPaint);
var AColor:TCOlor;
begin
  AColor := Color;
  try
    Color := Colors.FrameColor;
    inherited;
  finally
    Color := AColor;
  end;
end;

procedure TJvCustomNavigationPane.UpdatePositions;
var
  i, X, Y: integer;
begin
  if (csDestroying in ComponentState) or (FIconPanel = nil) then Exit;
  DisableAlign;
  FIconPanel.DisableAlign;
  try
    Y := 0;
    X := 0;
    FSplitter.Top := Y;
    FIconPanel.FDropButton.Left := Width;
    FIconPanel.Top := Height - FIconPanel.Height;
    Inc(Y, FSplitter.Height);
    for i := 0 to PageCount - 1 do
    begin
      if NavPages[i].NavPanel = nil then Exit;
      if NavPages[i].Iconic then
      begin
        NavPages[i].IconButton.Left := X;
        Inc(X, NavPages[i].IconButton.Width);
      end
      else
      begin
        NavPages[i].NavPanel.Top := Y;
        Inc(Y, NavPages[i].NavPanel.Height);
      end;
      NavPages[i].Invalidate;
    end;
  finally
    EnableAlign;
    FIconPanel.EnableAlign;
  end;
  Invalidate;
end;

procedure TJvCustomNavigationPane.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvCustomNavigationPane.DoColorsChange(Sender: TObject);
var
  i: integer;
begin
  if FIconPanel <> nil then
    TJvIconPanel(FIconPanel).Colors := Colors;
  for i := 0 to PageCount - 1 do
  begin
    NavPages[i].NavPanel.Colors := Colors;
    NavPages[i].IconButton.Colors := Colors;
  end;
  FSplitter.ColorFrom := Colors.SplitterColorFrom;
  FSplitter.ColorTo := Colors.SplitterColorTo;
end;

procedure TJvCustomNavigationPane.Loaded;
begin
  inherited;
  UpdatePositions;
end;

procedure TJvCustomNavigationPane.SetResizable(const Value: boolean);
begin
  if FResizable <> Value then
  begin
    FResizable := Value;
    FSplitter.Enabled := FResizable;
  end;
end;

function TJvCustomNavigationPane.InternalGetPageClass: TJvCustomPageClass;
begin
  Result := TJvNavPanelPage;
end;

function TJvCustomNavigationPane.GetNavPage(Index: integer): TJvNavPanelPage;
begin
  Result := TJvNavPanelPage(Pages[Index]);
end;

procedure TJvCustomNavigationPane.InsertPage(APage: TJvCustomPage);
begin
  inherited InsertPage(APage);
  if APage <> nil then
  begin
    TJvNavPanelPage(APage).Top := FIconPanel.Top;
    if ActivePage = nil then
      ActivePage := APage;
  end;
  UpdatePositions;
end;

procedure TJvCustomNavigationPane.SetActivePage(Page: TJvCustomPage);
begin
  inherited;
  if (ActivePage <> nil) then
  begin
    TJvNavPanelPage(ActivePage).NavPanel.Down := True;
    TJvNavPanelPage(ActivePage).IconButton.Down := True;
    ActivePage.Refresh;
  end;
end;

procedure TJvCustomNavigationPane.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TJvCustomNavigationPane.Paint;
begin
  if ActivePage = nil then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
end;

procedure TJvCustomNavigationPane.SetNavPanelFont(const Value: TFont);
begin
  FNavPanelFont.Assign(Value);
end;

function TJvCustomNavigationPane.IsNavPanelFontStored: Boolean;
var
  F: TFont;
begin
  F := Screen.IconFont;
  with FNavPanelFont do
    Result := (Name <> F.Name) or (Size <> F.Size) or (Style <> [fsBold])
      or (Color <> F.Color) or (Pitch <> F.Pitch) or (Charset <> F.CharSet);
end;

procedure TJvCustomNavigationPane.DoNavPanelFontChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    NavPages[i].NavPanel.Font := FNavPanelFont;
end;

procedure TJvCustomNavigationPane.RemovePage(APage: TJvCustomPage);
begin
  inherited RemovePage(APage);
  Invalidate;
end;

{ TJvNavIconButton }

constructor TJvNavIconButton.Create(AOwner: TComponent);
begin
  inherited;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  Width := 22;
  Height := 22;
  Font := Screen.IconFont;
  Font.Style := [fsBold];
end;

destructor TJvNavIconButton.Destroy;
begin
  FChangeLink.Free;
  FColors.Free;
  inherited;
end;

procedure TJvNavIconButton.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavIconButton.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavIconButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TJvNavIconButton.Paint;
var
  Rect: TRect;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    Brush.Style := bsClear;
    InflateRect(Rect, 0, -1);
    if (bsMouseInside in MouseStates) then
    begin
      if (bsMouseDown in MouseStates) then
        GradientFillRect(Canvas, Rect, Colors.SelectedColorFrom, Colors.SelectedColorTo, fdTopToBottom, 32)
      else
        GradientFillRect(Canvas, Rect, Colors.HotColorFrom, Colors.HotColorTo, fdTopToBottom, 32)
    end
    else if Down then
      GradientFillRect(Canvas, Rect, Colors.SelectedColorFrom, Colors.SelectedColorTo, fdTopToBottom, 32);
    case ButtonType of
      nibDropDown:
        begin // area should be 7x12
          InflateRect(Rect, -((Rect.Right - Rect.Left) - 7) div 2, -((Rect.Bottom - Rect.Top) - 12) div 2);
          Canvas.Pen.Color := clWindowText;

          // chevron
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, 1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, 1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, -1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, -1, 1);

          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 2, Rect.Top);

          Canvas.MoveTo(Rect.Left + 3, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);

          // drop arrow
          OffsetRect(Rect, 1, 4);
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 5, Rect.Top);
          OffsetRect(Rect, 1, 1);
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 3, Rect.Top);
          OffsetRect(Rect, 1, 1);
          Canvas.MoveTo(Rect.Left, Rect.Top);
          Canvas.LineTo(Rect.Left + 1, Rect.Top);

        end;
      nibImage:
        begin
          if (Images <> nil) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
            // draw image only
            Images.Draw(Canvas,
              (Width - Images.Width) div 2 + Ord(bsMouseDown in MouseStates),
              (Height - Images.Height) div 2 + Ord(bsMouseDown in MouseStates), ImageIndex, Enabled);
        end;
    end;
    if csDesigning in ComponentState then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psDot;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(ClientRect);
    end;
  end;
end;

procedure TJvNavIconButton.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvNavIconButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavIconButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FChangeLink);
    end;
    Invalidate;
  end;
end;

procedure TJvNavIconButton.SetButtonType(const Value: TJvNavIconButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    Invalidate;
  end;
end;

{ TJvNavPanelButton }

procedure TJvNavPanelButton.Click;
begin
  inherited;
end;

procedure TJvNavPanelButton.CMFontchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvNavPanelButton.CMTextchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TJvNavPanelButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csDisplayDragImage];
  Flat := True;
  HotTrack := True;
  Height := cButtonHeight;
  FColors := TJvNavPanelColors.Create;
  FColors.OnChange := DoColorsChange;
  Font := Screen.IconFont;
  Font.Style := [fsBold];
  Width := 125;
  Height := 28;
end;

destructor TJvNavPanelButton.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TJvNavPanelButton.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelButton.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  if bsMouseInside in MouseStates then
  begin
    if (bsMouseDown in MouseStates) then
      GradientFillRect(Canvas, R, Colors.SelectedColorTo, Colors.SelectedColorFrom, fdTopToBottom, 32)
    else
      GradientFillRect(Canvas, R, Colors.HotColorFrom, Colors.HotColorTo, fdTopToBottom, 32);
  end
  else if Down then
    GradientFillRect(Canvas, R, Colors.SelectedColorFrom, Colors.SelectedColorTo, fdTopToBottom, 32)
  else
    GradientFillRect(Canvas, ClientRect, Colors.ColorFrom, Colors.ColorTo, fdTopToBottom, 32);

  if Assigned(Images) then
  begin
    OffsetRect(R, 4, 0);
    Images.Draw(Canvas, R.Left, (Height - Images.Height) div 2, ImageIndex);
    OffsetRect(R, Images.Width + 4, 0);
  end
  else
    OffsetRect(R, 8, 0);
  if Caption <> '' then
  begin
    Canvas.Font := Font;
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
      DT_EDITCONTROL);
  end;
  Canvas.Pen.Color := clGray;
  if Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width + 1, 0);
  end
  else
  begin
    Canvas.MoveTo(0, ClientHeight - 1);
    Canvas.LineTo(Width + 1, ClientHeight - 1);
  end;
end;

procedure TJvNavPanelButton.SetColors(const Value: TJvNavPanelColors);
begin
  FColors.Assign(Value);
end;

procedure TJvNavPanelButton.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

{ TJvNavPanelColors }

procedure TJvNavPanelColors.Assign(Source: TPersistent);
begin
  if (Source is TJvNavPanelColors) and (Source <> Self) then
  begin
    FSplitterColorFrom := TJvNavPanelColors(Source).SplitterColorFrom;
    FSplitterColorTo := TJvNavPanelColors(Source).SplitterColorTo;
    FColorFrom := TJvNavPanelColors(Source).ColorFrom;
    FColorTo := TJvNavPanelColors(Source).ColorTo;
    FHotColorFrom := TJvNavPanelColors(Source).HotColorFrom;
    FHotColorTo := TJvNavPanelColors(Source).HotColorTo;
    FSelectedColorFrom := TJvNavPanelColors(Source).SelectedColorFrom;
    FSelectedColorTo := TJvNavPanelColors(Source).SelectedColorTo;
    FFrameColor := TJvNavPanelColors(Source).FrameColor;
    Change;
    Exit;
  end;
  inherited;
end;

procedure TJvNavPanelColors.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TJvNavPanelColors.Create;
begin
  inherited Create;
  FSplitterColorFrom := $D68652;
  FSplitterColorTo := $944110;
  FColorFrom := $FFE7CE;
  FColorTo := $E7A67B;
  FFrameColor := $943000;
  FHotColorFrom := $8CE7FF;
  FHotColorTo := $1096EF;
  FSelectedColorFrom := $0053DDFF;
  FSelectedColorTo := $000D7BC6;
end;

procedure TJvNavPanelColors.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetHotColorFrom(const Value: TColor);
begin
  if FHotColorFrom <> Value then
  begin
    FHotColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetHotColorTo(const Value: TColor);
begin
  if FHotColorTo <> Value then
  begin
    FHotColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetSelectedColorFrom(const Value: TColor);
begin
  if FSelectedColorFrom <> Value then
  begin
    FSelectedColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetSelectedColorTo(const Value: TColor);
begin
  if FSelectedColorTo <> Value then
  begin
    FSelectedColorTo := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetSplitterColorFrom(const Value: TColor);
begin
  if FSplitterColorFrom <> Value then
  begin
    FSplitterColorFrom := Value;
    Change;
  end;
end;

procedure TJvNavPanelColors.SetSplitterColorTo(const Value: TColor);
begin
  if FSplitterColorTo <> Value then
  begin
    FSplitterColorTo := Value;
    Change;
  end;
end;

{ TJvNavPanelPage }

constructor TJvNavPanelPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNavPanel := TJvNavPanelButton.Create(Self);
  FNavPanel.Visible := true;
  FNavPanel.Align := alBottom;
  FNavPanel.GroupIndex := 113; // use a silly number that no one else is probable to use
  FNavPanel.AllowAllUp := false;

  FIconButton := TJvNavIconButton.Create(Self);
  FIconButton.ButtonType := nibImage;
  FIconButton.Visible := false;
  FIconButton.Align := alRight;
  FIconButton.Width := 0;
  FIconButton.GroupIndex := 113;
  FIconButton.AllowAllUp := false;

  FNavPanel.OnClick := DoButtonClick;
  FIconButton.OnClick := DoButtonClick;

  ImageIndex := -1;
end;

procedure TJvNavPanelPage.DoButtonClick(Sender: TObject);
begin
  if not NavPanel.Down then
  begin
    if Parent <> nil then
      TJvCustomNavigationPane(Parent).ActivePage := Self; // this sets "Down" as well
    if Assigned(FOnClick) then FOnClick(Self);
  end;
end;

function TJvNavPanelPage.GetCaption: TCaption;
begin
  if NavPanel = nil then
    Result := ''
  else
    Result := NavPanel.Caption;
end;

function TJvNavPanelPage.GetColors: TJvNavPanelColors;
begin
  Result := NavPanel.Colors;
end;

function TJvNavPanelPage.GetHint: string;
begin
  if NavPanel = nil then
    Result := ''
  else
    Result := NavPanel.Hint;
end;

function TJvNavPanelPage.GetIconic: boolean;
begin
  if NavPanel = nil then
    Result := false
  else
    Result := not NavPanel.Visible;
end;

function TJvNavPanelPage.GetImageIndex: TImageIndex;
begin
  if NavPanel = nil then
    Result := -1
  else
    Result := NavPanel.ImageIndex;
end;

procedure TJvNavPanelPage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = IconPanel) then
    IconPanel := nil;
end;

procedure TJvNavPanelPage.SetCaption(const Value: TCaption);
begin
  if NavPanel <> nil then
    NavPanel.Caption := Value;
end;

procedure TJvNavPanelPage.SetColors(const Value: TJvNavPanelColors);
begin
  NavPanel.Colors := Value;
  IconButton.Colors := Value;
end;

procedure TJvNavPanelPage.SetHint(const Value: string);
begin
  NavPanel.Hint := Value;
  IconButton.Hint := Value;
end;

procedure TJvNavPanelPage.SetIconic(const Value: boolean);
begin
  NavPanel.Visible := not Value;
  IconButton.Visible := Value;
  NavPanel.Height := cButtonHeight * Ord(NavPanel.Visible);
  IconButton.Width := cButtonWidth * Ord(IconButton.Visible);
  UpdatePageList;
end;

procedure TJvNavPanelPage.SetIconPanel(const Value: TJvIconPanel);
begin
  if (FIconPanel <> Value) and not (csDestroying in ComponentState) then
  begin
    FIconPanel := Value;
    if IconButton <> nil then
    begin
      if FIconPanel <> nil then
      begin
        IconButton.Parent := FIconPanel;
        FIconPanel.FreeNotification(Self);
      end
      else
        IconButton.Parent := nil;
    end;
  end;
end;

procedure TJvNavPanelPage.SetImageIndex(const Value: TImageIndex);
begin
  NavPanel.ImageIndex := Value;
  IconButton.ImageIndex := Value;
end;

procedure TJvNavPanelPage.SetPageIndex(Value: Integer);
begin
  inherited SetPageIndex(Value);
  UpdatePageList;
end;

procedure TJvNavPanelPage.SetParent(AParent: TWinControl);
begin
  inherited;
  if (FNavPanel = nil) or (FIconButton = nil) or (csDestroying in ComponentState) then
    Exit;
  NavPanel.Parent := AParent;
  if AParent is TJvCustomNavigationPane then
  begin
    IconPanel := TJvCustomNavigationPane(AParent).FIconPanel;
    NavPanel.Colors := TJvCustomNavigationPane(AParent).Colors;
    IconButton.Images := TJvCustomNavigationPane(AParent).SmallImages;
    NavPanel.Images := TJvCustomNavigationPane(AParent).LargeImages;
    NavPanel.Font := TJvCustomNavigationPane(AParent).NavPanelFont;
  end
  else
    IconButton.Parent := nil;
end;

procedure TJvNavPanelPage.UpdatePageList;
begin
  if PageList <> nil then
    TJvCustomNavigationPane(PageList).UpdatePositions;
end;

{ TJvOutlookSplitter }

procedure TJvOutlookSplitter.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TJvOutlookSplitter.Create(AOwner: TComponent);
begin
  inherited;
  FColorFrom := $D68652;
  FColorTo := $944110;
  Align := alBottom;
  AutoSnap := False;
  ResizeStyle := rsUpdate;
  Height := cSplitterHeight;
  Cursor := crSizeNS;
end;

procedure TJvOutlookSplitter.Paint;
var
  i: integer;
  R: TRect;
begin
  R := ClientRect;
  if Align in [alTop, alBottom] then
  begin
    GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, R.Bottom - R.Top);
    Inc(R.Left, (R.Right - R.Left) div 2 - 20);
    Inc(R.Top, (R.Bottom - R.Top) div 2 - 1);
    R.Right := R.Left + 2;
    R.Bottom := R.Top + 2;
    if Enabled then
      for i := 0 to 9 do // draw the dots
      begin
        Canvas.Brush.Color := cl3DDkShadow;
        Canvas.FillRect(R);
        OffsetRect(R, 1, 1);
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(R);
        Canvas.Brush.Color := ColorFrom; // (p3) this is probably not the right color, but it's close enough for me...
        Canvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Top + 1));
        OffsetRect(R, 3, -1);
      end;
  end
  else
  begin
    GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdLeftToRight, R.Right - R.Left);
    Inc(R.Top, (R.Bottom - R.Top) div 2 - 20);
    Inc(R.Left, (R.Right - R.Left) div 2 - 2);
    R.Right := R.Left + 2;
    R.Bottom := R.Top + 2;
    if Enabled then
      for i := 0 to 9 do // draw the dots
      begin
        Canvas.Brush.Color := cl3DDkShadow;
        Canvas.FillRect(R);
        OffsetRect(R, 1, 1);
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(R);
        Canvas.Brush.Color := ColorFrom;
        Canvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Top + 1));
        OffsetRect(R, -1, 3);
      end;

  end;
end;

procedure TJvOutlookSplitter.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvOutlookSplitter.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

{ TJvNavPanelHeader }

procedure TJvNavPanelHeader.CMTextchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TJvNavPanelHeader.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImagesChange;
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  FColorFrom := $D68652;
  FColorTo := $944110;
  Font.Name := 'Arial';
  Font.Size := 12;
  Font.Style := [fsBold];
  Font.Color := clWhite;
  Height := 27;
  Width := 225;
end;

destructor TJvNavPanelHeader.Destroy;
begin
  FChangeLink.Free;
  inherited;
end;

procedure TJvNavPanelHeader.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvNavPanelHeader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TJvNavPanelHeader.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, 32);
  if Caption <> '' then
  begin
    Canvas.Font := Font;
    OffsetRect(R, 4, 0);
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
      DT_EDITCONTROL);
  end;
  if (Images <> nil) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    Images.Draw(Canvas,
      ClientWidth - Images.Width - (Height - Images.Height) div 2, (Height - Images.Height) div 2, ImageIndex, True);
  end;
end;

procedure TJvNavPanelHeader.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FChangeLink);
      FImages.FreeNotification(Self);
    end;
    Invalidate;
  end;
end;

procedure TJvNavPanelHeader.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TJvNavPanelDivider }

procedure TJvNavPanelDivider.CMTextchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TJvNavPanelDivider.Create(AOwner: TComponent);
begin
  inherited;
  Align := alNone;
  AutoSnap := False;
  ResizeStyle := rsUpdate;
  ControlStyle := ControlStyle + [csOpaque];
  FColorFrom := $FFE7CE;
  FColorTo := $E7A67B;
  FFrameColor := $943000;
  Cursor := crSizeNS;
  Font := Screen.IconFont;
  Height := 19;
  Width := 125;
end;

procedure TJvNavPanelDivider.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  GradientFillRect(Canvas, R, ColorFrom, ColorTo, fdTopToBottom, 32);
  if Caption <> '' then
  begin
    Canvas.Font := Font;
    OffsetRect(R, 7, 0);
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
      DT_EDITCONTROL);
  end;
  Canvas.Pen.Color := FrameColor;
  Canvas.MoveTo(0, ClientHeight - 1);
  Canvas.LineTo(Width, ClientHeight - 1);
end;

procedure TJvNavPanelDivider.SetColorFrom(const Value: TColor);
begin

end;

procedure TJvNavPanelDivider.SetColorTo(const Value: TColor);
begin

end;

procedure TJvNavPanelDivider.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
end;

initialization
  RegisterClasses([TJvNavPanelPage]);
end.

