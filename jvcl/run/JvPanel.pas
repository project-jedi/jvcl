{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPanel.pas, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
pongtawat
Peter Thornqvist [peter3 at sourceforge dot net]
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPanel;

{$I jvcl.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  {$IFDEF VisualCLX}
  Qt, 
  {$ENDIF VisualCLX}
  JvThemes, JvComponent, JvExControls;

type
  TJvPanelResizeParentEvent = procedure(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer) of object;
  TJvAutoSizePanel = (asNone, asWidth, asHeight, asBoth);

  TJvPanel = class;

  TJvArrangeSettings = class(TPersistent)
  private
    FPanel: TJvPanel;
    FAutoArrange: Boolean;
    FAutoSize: TJvAutoSizePanel;
    FWrapControls: Boolean;
    FBorderLeft: Integer;
    FBorderTop: Integer;
    FDistanceVertical: Integer;
    FDistanceHorizontal: Integer;
    FShowNotVisibleAtDesignTime: Boolean;
    FMaxWidth: Integer;
    procedure SetWrapControls(Value: Boolean);
    procedure SetAutoArrange(Value: Boolean);
    procedure SetAutoSize(Value: TJvAutoSizePanel);
    procedure SetBorderLeft(Value: Integer);
    procedure SetBorderTop(Value: Integer);
    procedure SetDistanceVertical(Value: Integer);
    procedure SetDistanceHorizontal(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure Rearrange;
  public
    constructor Create(APanel: TJvPanel);
    procedure Assign(Source: TPersistent); override;
  published
    property WrapControls: Boolean read FWrapControls write SetWrapControls default True;
    property BorderLeft: Integer read FBorderLeft write SetBorderLeft default 0;
    property BorderTop: Integer read FBorderTop write SetBorderTop default 0;
    property DistanceVertical: Integer read FDistanceVertical write SetDistanceVertical default 0;
    property DistanceHorizontal: Integer read FDistanceHorizontal write SetDistanceHorizontal default 0;
    property ShowNotVisibleAtDesignTime: Boolean read FShowNotVisibleAtDesignTime write FShowNotVisibleAtDesignTime default True;
    property AutoSize: TJvAutoSizePanel read FAutoSize write SetAutoSize default asNone;
    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange default False;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;
  end;

  {$IFDEF VCL}
  TJvPanelMoveEvent = procedure(Sender: TObject; X, Y: Integer; var Allow: Boolean) of object;
  {$ENDIF VCL}

  TJvPanel = class(TJvCustomPanel, IJvDenySubClassing)
  private
    FTransparent: Boolean;
    FFlatBorder: Boolean;
    FFlatBorderColor: TColor;
    FMultiLine: Boolean;
    FOldColor: TColor;
    FHotColor: TColor;
    FSizeable: Boolean;
    FDragging: Boolean;
    FLastPos: TPoint;
    FArrangeSettings: TJvArrangeSettings;
    FEnableArrangeCount: Integer;
    FArrangeControlActive: Boolean;
    FArrangeWidth: Integer;
    FArrangeHeight: Integer;
    FOnResizeParent: TJvPanelResizeParentEvent;
    FOnPaint: TNotifyEvent;
    {$IFDEF VCL}
    FMovable: Boolean;
    FWasMoved: Boolean;
    FOnAfterMove: TNotifyEvent;
    FOnBeforeMove: TJvPanelMoveEvent;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FGripBmp: TBitmap;
    procedure CreateSizeGrip;
    {$ENDIF VisualCLX}
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFlatBorder(const Value: Boolean);
    procedure SetFlatBorderColor(const Value: TColor);
    procedure DrawCaption;
    procedure DrawCaptionTo(ACanvas: TCanvas
  {$IFDEF VisualCLX}; DrawingMask: Boolean = false{$ENDIF});
    procedure DrawBorders;
    procedure SetMultiLine(const Value: Boolean);
    procedure SetHotColor(const Value: TColor);
    procedure SetSizeable(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
    procedure TextChanged; override;
    procedure Paint; override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    {$IFDEF VCL}
    procedure AdjustSize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    function DoBeforeMove(X, Y: Integer): Boolean; dynamic;
    procedure DoAfterMove; dynamic;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure DrawMask(ACanvas: TCanvas); override;
    {$ENDIF VisualCLX}
    procedure Loaded; override;
    procedure Resize; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function GetNextControlByTabOrder(ATabOrder: Integer): TWinControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ArrangeControls;
    procedure EnableArrange;
    procedure DisableArrange;
    function ArrangeEnabled: Boolean;
    property ArrangeWidth: Integer read FArrangeWidth;
    property ArrangeHeight: Integer read FArrangeHeight;
    {$IFDEF VCL}
    property DockManager;
    {$ENDIF VCL}
    property Canvas;
  published
    {$IFDEF VCL}
    property Movable: Boolean read FMovable write FMovable default False;
    {$ENDIF VCL}
    property Sizeable: Boolean read FSizeable write SetSizeable default False;
    property HintColor;
    property HotColor: TColor read FHotColor write SetHotColor default clBtnFace;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine;
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder default False;
    property FlatBorderColor: TColor read FFlatBorderColor write SetFlatBorderColor default clBtnShadow;
    property OnMouseEnter;
    property OnMouseLeave;
    {$IFDEF VCL}
    property OnBeforeMove: TJvPanelMoveEvent read FOnBeforeMove write FOnBeforeMove;
    property OnAfterMove: TNotifyEvent Read FOnAfterMove write FOnAfterMove;
    {$ENDIF VCL}
    property OnParentColorChange;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property OnResizeParent: TJvPanelResizeParentEvent read FOnResizeParent write FOnResizeParent;
    property Align;
    property Alignment;
    property Anchors;
    {$IFDEF VCL}
    property AutoSize;
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property FullRepaint;
    property Locked;
    property ParentBiDiMode;
    property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF VCL}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  JvMouseTimer;

const
  BkModeTransparent = TRANSPARENT;

function IsThemed: Boolean;
begin
  {$IFDEF JVCLThemesEnabled}
  Result := ThemeServices.ThemesEnabled;
  {$ELSE}
  Result := False;
  {$ENDIF JVCLThemesEnabled}
end;

//=== { TJvArrangeSettings } =================================================

constructor TJvArrangeSettings.Create(APanel: TJvPanel);
begin
  inherited Create;
  FPanel := APanel;
  FMaxWidth := 0;
  FBorderLeft := 0;
  FBorderTop := 0;
  FDistanceVertical:= 0;
  FDistanceHorizontal:= 0;
  WrapControls := True;
  ShowNotVisibleAtDesignTime := True;
  FAutoSize := asNone;
  AutoArrange := False;
end;

procedure TJvArrangeSettings.SetWrapControls(Value: Boolean);
begin
  if Value <> FWrapControls then
  begin
    FWrapControls := Value;
    Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetAutoArrange(Value: Boolean);
begin
  if Value <> FAutoArrange then
  begin
    FAutoArrange := Value;
    if Value then
      Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetAutoSize(Value: TJvAutoSizePanel);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    if AutoSize <> asNone then
      Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetBorderLeft(Value: Integer);
begin
  if Value <> FBorderLeft then
  begin
    FBorderLeft := Value;
    Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetBorderTop(Value: Integer);
begin
  if Value <> FBorderTop then
  begin
    FBorderTop := Value;
    Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetDistanceVertical(Value: Integer);
begin
  if Value <> FDistanceVertical then
  begin
    FDistanceVertical := Value;
    Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetDistanceHorizontal(Value: Integer);
begin
  if Value <> FDistanceHorizontal then
  begin
    FDistanceHorizontal := Value;
    Rearrange;
  end;
end;

procedure TJvArrangeSettings.SetMaxWidth(Value: Integer);
begin
  if Value <> FMaxWidth then
  begin
    FMaxWidth := Value;
    Rearrange;
  end;
end;

procedure TJvArrangeSettings.Assign(Source: TPersistent);
var
  A: TJvArrangeSettings;
begin
  if Source is TJvArrangeSettings then
  begin
    A := TJvArrangeSettings(Source);
    FAutoArrange := A.AutoArrange;
    FAutoSize := A.AutoSize;
    FWrapControls := A.WrapControls;
    FBorderLeft := A.BorderLeft;
    FBorderTop := A.BorderTop;
    FDistanceVertical := A.DistanceVertical;
    FDistanceHorizontal := A.DistanceHorizontal;
    FShowNotVisibleAtDesignTime := A.ShowNotVisibleAtDesignTime;
    FMaxWidth := A.MaxWidth;

    Rearrange;
  end
  else
    inherited Assign(Source);
end;

procedure TJvArrangeSettings.Rearrange;
begin
  if (FPanel <> nil) and AutoArrange and
    not (csLoading in FPanel.ComponentState) then
    FPanel.ArrangeControls;
end;

//=== { TJvPanel } ===========================================================

constructor TJvPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csNeedsBorderPaint, csParentBackground]);
  {$ENDIF VCL}
  FTransparent := False;
  FFlatBorder := False;
  FFlatBorderColor := clBtnShadow;
  FHotColor := clBtnFace;

  FArrangeSettings := TJvArrangeSettings.Create(Self);
end;

destructor TJvPanel.Destroy;
begin
  FArrangeSettings.Free;
  {$IFDEF VisualCLX}
  FreeAndNil(FGripBmp);
  {$ENDIF VisualCLX}
  inherited Destroy;
end;

{$IFDEF VCL}

procedure TJvPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Transparent and not IsThemed then
  begin
    if not (csDesigning in ComponentState) then
      Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
    ControlStyle := ControlStyle - [csOpaque];
  end
  else
  begin
    if not (csDesigning in ComponentState) then
      Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
    ControlStyle := ControlStyle + [csOpaque];
  end;
end;

procedure TJvPanel.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if Movable then
  begin
    P := ScreenToClient(SmallPointToPoint(Msg.Pos));
    with P do
      if (X > 5) and (Y > 5) and (X < Width - 5) and (Y < Height - 5) and DoBeforeMove(P.X,P.Y) then
      begin
         Msg.Result := HTCAPTION;
         FWasMoved := True;
      end;
  end;
end;

procedure TJvPanel.DoAfterMove;
begin
  if Assigned(FOnAfterMove) then
    FOnAfterMove(Self);
end;

procedure TJvPanel.WMExitSizeMove(var Msg: TMessage);
begin
  inherited;
  if FWasMoved then
    DoAfterMove;
  FWasMoved := False;
end;

function TJvPanel.DoBeforeMove(X,Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforeMove) then
    FOnBeforeMove(Self, X, Y, Result);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvPanel.DrawMask(ACanvas: TCanvas);
var
  R: TRect;
  I, J, X, Y: Integer;
begin
  inherited DrawMask(ACanvas);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := clDontMask;
  I := 0 ; //BorderWidth;
  if BevelOuter <> bvNone then
    Inc(I, BevelWidth);
  if BevelInner <> bvNone then
    Inc(I, BevelWidth);
  ACanvas.Pen.Width := I;
  R := ClientRect;
  ACanvas.Rectangle(R);
  DrawCaptionTo(ACanvas, true);
  if Sizeable then
  begin
    X := ClientWidth - FGripBmp.Width - I;
    Y := ClientHeight - FGripBmp.Height - I;
    for I := 0 to 2 do
    begin
      for J := 0 to 2 do
      begin
        ACanvas.MoveTo(X + 4 * I + J, Y + FGripBmp.Height);
        ACanvas.LineTo(X + FGripBmp.Width, Y + 4 * I + J);
      end
    end;
  end;
end;
{$ENDIF VisualCLX}

procedure TJvPanel.Paint;
var
  X, Y: Integer;
  {$IFDEF VisualCLX}
  I: Integer;
  {$ENDIF VisualCLX}
begin
  if Assigned(FOnPaint) then
  begin
    FOnPaint(Self);
    Exit;
  end;
  Canvas.Brush.Color := Color;
  if not Transparent or IsThemed then
    DrawThemedBackground(Self, Canvas, ClientRect)
  else
    Canvas.Brush.Style := bsClear;

  if FFlatBorder then
  begin
    Canvas.Brush.Color := FFlatBorderColor;
    {$IFDEF VCL}
    Canvas.FrameRect(ClientRect);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FrameRect(Canvas, ClientRect);
    {$ENDIF VisualCLX}
    Canvas.Brush.Color := Color;
  end
  else
    DrawBorders;
  Self.DrawCaption;
  if Sizeable then
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tsGripper),
        Rect(ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - BevelWidth - 2,
          ClientHeight - GetSystemMetrics(SM_CYHSCROLL) - BevelWidth - 2,
          ClientWidth - BevelWidth - 2, ClientHeight - BevelWidth - 2));
    end
    else
    {$ENDIF JVCLThemesEnabled}
      with Canvas do
      begin
        {$IFDEF VisualCLX}
        I := 0 ; //BorderWidth;
        if BevelOuter <> bvNone then
          Inc(I, BevelWidth);
        if BevelInner <> bvNone then
          Inc(I, BevelWidth);
        X := ClientWidth - FGripBmp.Width - I;
        Y := ClientHeight - FGripBmp.Height - I;
        Draw(X, Y, FGripBmp);
        {$ENDIF VisualCLX}
        {$IFDEF VCL}
        // (rom) Marlett is not a standard Windows font
        Font.Name := 'Marlett';
        Font.Charset := DEFAULT_CHARSET;
        Font.Size := 12;
        Canvas.Font.Style := [];
        Canvas.Font.Color := clBtnShadow;
        Brush.Style := bsClear;
        X := ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - BevelWidth - 2;
        Y := ClientHeight - GetSystemMetrics(SM_CYHSCROLL) - BevelWidth - 2;
        if Transparent and not IsThemed then
          SetBkMode(Handle, BkModeTransparent);
        TextOut(X, Y, 'o');
        {$ENDIF VCL}
      end;
end;

{$IFDEF VCL}
procedure TJvPanel.AdjustSize;
begin
  inherited AdjustSize;
  if Transparent and not IsThemed then
  begin
    // (ahuser) That is the only way to draw the border of the contained controls.
    Width := Width + 1;
    Width := Width - 1;
  end;
end;
{$ENDIF VCL}

procedure TJvPanel.DrawBorders;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;

begin
  Rect := ClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
end;

procedure TJvPanel.DrawCaption;
begin
  DrawCaptionTo(self.Canvas);
end;

procedure TJvPanel.DrawCaptionTo(ACanvas: TCanvas
  {$IFDEF VisualCLX}; DrawingMask: Boolean = false{$ENDIF});
const
  Alignments: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWrap: array [Boolean] of Longint = (DT_SINGLELINE, DT_WORDBREAK);
var
  ATextRect: TRect;
  BevelSize: Integer;
  Flags: Longint;
begin
  with ACanvas do
  begin
    if Caption <> '' then
    begin
      SetBkMode(Handle, BkModeTransparent);
      Font := Self.Font;
      ATextRect := GetClientRect;
      InflateRect(ATextRect, -BorderWidth, -BorderWidth);
      BevelSize := 0;
      if BevelOuter <> bvNone then
        Inc(BevelSize, BevelWidth);
      if BevelInner <> bvNone then
        Inc(BevelSize, BevelWidth);
      InflateRect(ATextRect, -BevelSize, -BevelSize);
      Flags := DT_EXPANDTABS or WordWrap[MultiLine] or Alignments[Alignment];
      Flags := DrawTextBiDiModeFlags(Flags);
      //calculate required rectangle size
      {$IFDEF VCL}
      DrawText(ACanvas.Handle, PChar(Caption), -1, ATextRect, Flags or DT_CALCRECT);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      DrawText(ACanvas, Caption, -1, ATextRect, Flags or DT_CALCRECT);
      {$ENDIF VisualCLX}
      // adjust the rectangle placement
      OffsetRect(ATextRect, 0, -ATextRect.Top + (Height - (ATextRect.Bottom - ATextRect.Top)) div 2);
      case Alignment of
        taRightJustify:
          OffsetRect(ATextRect, -ATextRect.Left + (Width - (ATextRect.Right - ATextRect.Left) - BorderWidth -
            BevelSize), 0);
        taCenter:
          OffsetRect(ATextRect, -ATextRect.Left + (Width - (ATextRect.Right - ATextRect.Left)) div 2, 0);
      end;
      {$IFDEF VisualCLX}
      if DrawingMask then
        Font.Color := clDontMask
      else
      {$ENDIF VisualCLX}
        if not Enabled then
          Font.Color := clGrayText;
      //draw text
      if Transparent and not IsThemed then
        SetBkMode(ACanvas.Handle, BkModeTransparent);
      {$IFDEF VCL}
      DrawText(ACanvas.Handle, PChar(Caption), -1, ATextRect, Flags);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      DrawText(ACanvas, Caption, -1, ATextRect, Flags);
      {$ENDIF VisualCLX}
    end;
  end;
end;

procedure TJvPanel.ParentColorChanged;
begin
  Invalidate;
  inherited ParentColorChanged;
end;

procedure TJvPanel.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver and (Control = nil) then
  begin
    FOldColor := Color;
    if not Transparent or IsThemed then
    begin
      Color := HotColor;
      MouseTimer.Attach(Self);
    end;
  end;
  inherited MouseEnter(Control);
end;

procedure TJvPanel.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if MouseOver and (Control = nil) then
  begin
    if not Transparent or IsThemed then
    begin
      Color := FOldColor;
      MouseTimer.Detach(Self);
    end;
  end;
  inherited MouseLeave(Control);
end;

procedure TJvPanel.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if not IsThemed then
    begin
      {$IFDEF VCL}
      RecreateWnd;
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      Masked := FTransparent;
      {$ENDIF VisualCLX}
    end;
  end;
end;

procedure TJvPanel.SetFlatBorder(const Value: Boolean);
begin
  if Value <> FFlatBorder then
  begin
    FFlatBorder := Value;
    Invalidate;
  end;
end;

procedure TJvPanel.SetFlatBorderColor(const Value: TColor);
begin
  if Value <> FFlatBorderColor then
  begin
    FFlatBorderColor := Value;
    Invalidate;
  end;
end;

function TJvPanel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  if Transparent and not IsThemed then
    Result := True
  else
    Result := inherited DoPaintBackground(Canvas, Param);
end;

procedure TJvPanel.SetMultiLine(const Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    Invalidate;
  end;
end;

procedure TJvPanel.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TJvPanel.Invalidate;
begin
{  if Transparent and Visible and Assigned(Parent) and Parent.HandleAllocated and HandleAllocated then
    RedrawWindow(Parent.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or
      RDW_INVALIDATE or RDW_ERASENOW or RDW_UPDATENOW or RDW_ALLCHILDREN); }
  inherited Invalidate;
end;

procedure TJvPanel.SetHotColor(const Value: TColor);
begin
  if FHotColor <> Value then
  begin
    FHotColor := Value;
    if not Transparent or IsThemed then
      Invalidate;
  end;
end;

procedure TJvPanel.SetSizeable(const Value: Boolean);
begin
  if FSizeable <> Value then
  begin
    FSizeable := Value;
    {$IFDEF VisualCLX}
    if Value then
      CreateSizeGrip
    else
      FreeAndNil(FGripBmp);
    {$ENDIF VisualCLX}
    Invalidate;
  end;
end;

procedure TJvPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Sizeable and (Button = mbLeft) and ((Width - X) < 12) and ((Height - Y) < 12) then
  begin
    FDragging := True;
    FLastPos := Point(X, Y);
    MouseCapture := True;
    Screen.Cursor := crSizeNWSE;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  X1, Y1: Integer;
begin
  if FDragging and Sizeable then
  begin
    R := BoundsRect;
    X1 := R.Right - R.Left + X - FLastPos.X;
    Y1 := R.Bottom - R.Top + Y - FLastPos.Y;
    if (X1 > 1) and (Y1 > 1) then
    begin
      if X1 >= 0 then
        FLastPos.X := X;
      if Y1 >= 0 then
        FLastPos.Y := Y;
      SetBounds(Left, Top, X1, Y1);
      Refresh;
    end;
  end
  else
  begin
    inherited MouseMove(Shift, X, Y);
    if Sizeable and ((Width - X) < 12) and ((Height - Y) < 12) then
      Cursor := crSizeNWSE
    else
      Cursor := crDefault;
  end;
end;

procedure TJvPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging and Sizeable then
  begin
    FDragging := False;
    MouseCapture := False;
    Screen.Cursor := crDefault;
    Refresh;
  end
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Transparent and not IsThemed then
    Invalidate;
end;

procedure TJvPanel.Resize;
begin
  {$IFDEF VisualCLX}
  if Assigned(FArrangeSettings) then
  {$ENDIF VisualCLX}
    if FArrangeSettings.AutoArrange then
      ArrangeControls;
  inherited Resize;
end;

procedure TJvPanel.EnableArrange;
begin
  EnableAlign;
  if FEnableArrangeCount > 0 then
    Dec(FEnableArrangeCount);
end;

procedure TJvPanel.DisableArrange;
begin
  Inc(FEnableArrangeCount);
  DisableAlign;
end;

function TJvPanel.ArrangeEnabled: Boolean;
begin
  Result := FEnableArrangeCount <= 0;
end;

procedure TJvPanel.Loaded;
begin
  inherited Loaded;
  if FArrangeSettings.AutoArrange then
    ArrangeControls;
end;

procedure TJvPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if FArrangeSettings.AutoArrange then
    ArrangeControls;
end;

function TJvPanel.GetNextControlByTabOrder(ATabOrder: Integer): TWinControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TWinControl then
      if TWinControl(Controls[I]).TabOrder = ATabOrder then
      begin
        Result := TWinControl(Controls[I]);
        Break;
      end;
end;

procedure TJvPanel.ArrangeControls;
var
  AktX, AktY, NewX, NewY, MaxY, NewMaxX: Integer;
  ControlMaxX, ControlMaxY: Integer;
  TmpWidth, TmpHeight: Integer;
  LastTabOrder: Integer;
  CurrControl: TWinControl;
  I: Integer;
  OldHeight, OldWidth: Integer;
begin
  if (not ArrangeEnabled) or FArrangeControlActive or (ControlCount = 0) then
    Exit;
  if [csLoading, csReading] * ComponentState <> [] then
    Exit;
  FArrangeWidth := 0;
  FArrangeHeight := 0;
  FArrangeControlActive := True;
  try
    OldHeight := Height;
    OldWidth := Width;
    TmpHeight := Height;
    TmpWidth := Width;
    AktY := FArrangeSettings.BorderTop;
    AktX := FArrangeSettings.BorderLeft;
    LastTabOrder := -1;
    MaxY := -1;
    if (FArrangeSettings.AutoSize in [asWidth, asBoth]) then
      ControlMaxX := TmpWidth - 2 * FArrangeSettings.BorderLeft
    else
      ControlMaxX := -1;
    if (FArrangeSettings.AutoSize in [asHeight, asBoth]) then
      ControlMaxY := TmpHeight - 2 * FArrangeSettings.BorderTop
    else
      ControlMaxY := -1;

    for I := 0 to ControlCount - 1 do
      if Controls[I] is TWinControl then
      begin
        if Controls[I] is TJvPanel then
          TJvPanel(Controls[I]).ArrangeSettings.Rearrange;
        if (Controls[I].Width + 2 * FArrangeSettings.BorderLeft > TmpWidth) then
          TmpWidth := Controls[I].Width + 2 * FArrangeSettings.BorderLeft;
      end;

    if (TmpWidth > FArrangeSettings.MaxWidth) and (FArrangeSettings.MaxWidth > 0) then
      TmpWidth := FArrangeSettings.MaxWidth ;
    CurrControl := GetNextControlByTabOrder(LastTabOrder+1);
    while Assigned(CurrControl) do
    begin
      LastTabOrder := CurrControl.TabOrder;
      if CurrControl.Visible or
        ((csDesigning in ComponentState) and FArrangeSettings.ShowNotVisibleAtDesignTime) then
      begin
        NewMaxX := AktX + CurrControl.Width + FArrangeSettings.DistanceHorizontal +
          FArrangeSettings.BorderLeft;
        if (((NewMaxX > TmpWidth) and not (FArrangeSettings.AutoSize in [asWidth, asBoth])) or
            ((NewMaxX > FArrangeSettings.MaxWidth) and (FArrangeSettings.MaxWidth > 0))) and
           (AktX > FArrangeSettings.BorderLeft) and // Only Valid if there is one control in the current line
           FArrangeSettings.WrapControls then
        begin
          AktX := FArrangeSettings.BorderLeft;
          AktY := AktY + MaxY + FArrangeSettings.DistanceVertical;
          MaxY := -1;
          NewX := AktX;
          NewY := AktY;
        end
        else
        begin
          NewX := AktX;
          NewY := AktY;
        end;
        AktX := AktX + CurrControl.Width;
        if AktX > ControlMaxX then
          ControlMaxX := AktX;
        AktX := AktX + FArrangeSettings.DistanceHorizontal;
        CurrControl.Left := NewX;
        CurrControl.Top := NewY;
        if CurrControl.Height > MaxY then
          MaxY := CurrControl.Height;
        ControlMaxY := AktY + MaxY;
      end;
      CurrControl := GetNextControlByTabOrder(LastTabOrder+1);
    end;

    if not (csLoading in ComponentState) then
    begin
      if (FArrangeSettings.AutoSize in [asWidth, asBoth]) then
        if ControlMaxX >= 0 then
          if (FArrangeSettings.MaxWidth > 0) and (ControlMaxX >= FArrangeSettings.MaxWidth) then
            TmpWidth := FArrangeSettings.MaxWidth
          else
            TmpWidth := ControlMaxX + FArrangeSettings.BorderLeft
        else
          TmpWidth := 0;
      if (FArrangeSettings.AutoSize in [asHeight, asBoth]) then
        if ControlMaxY >= 0 then
          TmpHeight := ControlMaxY + FArrangeSettings.BorderTop
        else
          TmpHeight := 0;
      Width := TmpWidth;
      Height := TmpHeight;
    end;
    FArrangeWidth := ControlMaxX + 2 * FArrangeSettings.BorderLeft;
    FArrangeHeight := ControlMaxY + 2 * FArrangeSettings.BorderTop;
    if (OldWidth <> TmpWidth) or (OldHeight <> Height) then
      {$IFDEF VCL}
      SendMessage(GetFocus, WM_PAINT, 0, 0);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      UpdateWindow(GetFocus);
      {$ENDIF VisualCLX}
  finally
    FArrangeControlActive := False;
  end;
end;

procedure TJvPanel.SetWidth(Value: Integer);
var
  Changed: Boolean;
begin
  Changed := inherited Width <> Value;
  inherited Width := Value;
  if Changed then
    if Assigned(FOnResizeParent) then
      FOnResizeParent(Self, Left, Top, Value, Height)
    else
    if Parent is TJvPanel then
      TJvPanel(Parent).ArrangeSettings.Rearrange;
end;

function TJvPanel.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TJvPanel.SetHeight(Value: Integer);
var
  Changed: Boolean;
begin
  Changed := inherited Height <> Value;
  inherited Height := Value;
  if Changed then
    if Assigned(FOnResizeParent) then
      FOnResizeParent(Self, Left, Top, Width, Value)
    else
    if Parent is TJvPanel then
      TJvPanel(Parent).ArrangeSettings.Rearrange;
end;

function TJvPanel.GetHeight: Integer;
begin
  Result := inherited Height;
end;

procedure TJvPanel.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  if (Value <> nil) and (Value <> FArrangeSettings) then
    FArrangeSettings.Assign(Value);
end;

{$IFDEF VisualCLX}
procedure TJvPanel.CreateSizeGrip;
var
  I: Integer;
begin
  FGripBmp := TBitmap.Create;
  FGripBmp.Width := 13; //GetSystemMetrics(SM_CXVSCROLL);
  FGripBmp.Height := 13; //GetSystemMetrics(SM_CXYSCROLL);
  with FGripBmp.Canvas do
  begin
    Brush.Color := clBackground;
    FillRect(Bounds(0, 0, Width, Height));
    Pen.Width := 1;
    for I := 0 to 2 do
    begin
      Pen.Color := clLight;
      MoveTo(3 * I, FGripBmp.Height);
      LineTo(FGripBmp.Width, 3 * I);
      Pen.Color := clDark;
      MoveTo(3 * I + 1, FGripBmp.Height);
      LineTo(FGripBmp.Width, 3 * I + 1);
//      Pen.Color := clMid;
      MoveTo(3 * I + 2, FGripBmp.Height);
      LineTo(FGripBmp.Width, 3 * I + 2);
    end;
  end;
  FGripBmp.TransparentColor := clBackground;
  FGripBmp.TransparentMode := tmFixed;
  FGripBmp.Transparent := True;
end;
{$ENDIF VisualCLX}

end.

