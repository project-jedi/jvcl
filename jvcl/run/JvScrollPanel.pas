{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A scrolling TToolWindow like the ones in IE 4.0 with popup scrollbuttons
   either on top/bottom or left/right edge.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvScrollPanel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, ToolWin, ExtCtrls,
  JvComponent, JvExForms;

type
  TJvCustomScrollPanel = class;
  TJvDivideKind = (dkDivider, dkSeparator);
  TJvScrollKind = (sbUp, sbDown, sbLeft, sbRight);
  /// (DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
  TJvScrollDirection = (sdHorizontal, sdVertical);
  TJvScrollingEvent = procedure(Sender: TObject; var AllowChange: Boolean; Kind: TJvScrollKind) of object;
  TJvScrolledEvent = procedure(Sender: TObject; Kind: TJvScrollKind) of object;

  TJvDivider = class(TJvGraphicControl)
  private
    FKind: TJvDivideKind;
    FVertical: Boolean;
    procedure SetKind(Value: TJvDivideKind);
    procedure SetVertical(Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Vertical: Boolean read FVertical write SetVertical default True;
    property Kind: TJvDivideKind read FKind write SetKind default dkDivider;
  end;

  TJvScrollButton = class(TJvCustomControl)
  private
    FDown: Boolean;
    FRepeat: Boolean;
    FFlat: Boolean;
    FAutoRepeat: Boolean;
    FIncrement: Word;
    FTimer: TTimer;
    FKind: TJvScrollKind;
    procedure SetKind(Value: TJvScrollKind);
    procedure SetFlat(Value: Boolean);
    procedure OnTime(Sender: TObject);
  protected
    procedure SetPosition; virtual;
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure EnabledChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Kind: TJvScrollKind read FKind write SetKind default sbUp;
    property Increment: Word read FIncrement write FIncrement default 16;
    property Width default 16;
    property Height default 16;
  end;

  TJvCustomScrollPanel = class(TJvExToolWindow)
  private
    FScrollDirection: TJvScrollDirection;
    FScrollAmount: Word;
    FAutoHide: Boolean;
    FAutoRepeat: Boolean;
    FAutoArrange: Boolean;
    FUpLeft: TJvScrollButton;
    FDownRight: TJvScrollButton;
    FOnScrolling: TJvScrollingEvent;
    FOnScrolled: TJvScrolledEvent;
    FFlat: Boolean;
    procedure SetAutoArrange(Value: Boolean);
    procedure SetAutoHide(Value: Boolean);
    procedure SetScrollDirection(Value: TJvScrollDirection);
    procedure SetFlat(Value: Boolean);
    procedure AlignArrows;
    procedure UpdateVisible;
    procedure ArrangeChildren;
    procedure SetupArrows;
    procedure SetScrollAmount(const Value: Word);
  protected
    procedure VisibleChanged; override;
    procedure EnabledChanged; override;
    procedure SetParent( AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property AutoHide: Boolean read FAutoHide write SetAutoHide;
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat;
    property AutoArrange: Boolean read FAutoArrange write SetAutoArrange default False;
    property Flat: Boolean read FFlat write SetFlat;
    property ScrollDirection: TJvScrollDirection read FScrollDirection write SetScrollDirection;
    property ScrollAmount: Word read FScrollAmount write SetScrollAmount default 16;
    property OnScrolling: TJvScrollingEvent read FOnScrolling write FOnScrolling;
    property OnScrolled: TJvScrolledEvent read FOnScrolled write FOnScrolled;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align default alTop;
    property Height default 35;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvScrollingWindow = class(TJvCustomScrollPanel)
  public
    constructor Create(AComponent: TComponent); override;
  published
    property AutoArrange;
    property AutoHide default True;
    property AutoRepeat default False;
    property Flat;
    property ScrollDirection default sdHorizontal;
    property ScrollAmount;
    { inherited ones: }
    property Align;
    property BorderWidth;
    property EdgeInner;
    property EdgeOuter;
    property EdgeBorders;
///    property BevelInner;
///    property BevelOuter;
///    property BevelKind;
///    property BevelWidth;
    property Enabled;
    property ShowHint;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ImeMode;
    property ImeName;
    property Color;
    property ParentColor;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScrolling;
    property OnScrolled;
    property TabOrder;
    property TabStop;
    property HelpContext;
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
  Math,
  JvThemes, JvExControls;

const
  cInitTime = 360;
  cTimeDelay = 100;

{
procedure TileBitmap(Dest: TControl; Source: TBitmap);
var
  X, Y, W, H: Longint;
  DR,SR: TRect;
  Tmp: TBitmap;
  Canvas: TControlCanvas;
begin
  if not Source.Empty then
  begin
    with Source do
    begin
      W := Width;
      H := Height;
    end;

    Tmp := TBitmap.Create;
    Canvas := TControlCanvas.Create;
    Canvas.Control := Dest;
    Tmp.Width := Dest.Width;
    Tmp.Height := Dest.Height;

    Y := 0;
    SR := Rect(0,0,W,H);
    while y < Dest.Height do
    begin
      X := 0;
      while X < Dest.Width do
      begin
        DR := Rect(X,Y,X+W,Y+H);
        Tmp.Canvas.CopyRect(DR,Source.Canvas,SR);
        Inc(X, W);
      end;
      Inc(Y, H);
    end;
    BitBlt(Canvas.Handle,0,0,Dest.Width,Dest.Height,Tmp.Handle,0,0,SRCCOPY);
//    Canvas.Draw(0,0,Tmp);
    Tmp.Free;
    Canvas.Free;
  end;
end;
}

//=== { TJvDivider } =========================================================

constructor TJvDivider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 16, 16);
  FVertical := True;
  FKind := dkDivider;
end;

procedure TJvDivider.SetVertical(Value: Boolean);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    Invalidate;
  end;
end;

procedure TJvDivider.SetKind(Value: TJvDivideKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Invalidate;
  end;
end;

procedure TJvDivider.Paint;
var
  R: TRect;
begin
  if not Visible then
    Exit;
  if FKind = dkDivider then
    with Canvas do
    begin
      if FVertical then
      begin
        R := Rect(Width div 2 - 1, 1, Width, Height - 1);
        DrawEdge(Handle, R, EDGE_ETCHED, BF_LEFT);
      end
      else
      begin
        R := Rect(1, Height div 2 - 1, Width, Height - 1);
        DrawEdge(Handle, R, EDGE_ETCHED, BF_TOP);
      end;
    end;
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
end;

//=== { TJvScrollButton } ====================================================

constructor TJvScrollButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks, csSetCaption];
  FDown := False;
  FIncrement := 16;
  FAutoRepeat := False;
  FFlat := False;
  FKind := sbUp;
  Width := 16;
  Height := 16;
end;

procedure TJvScrollButton.SetKind(Value: TJvScrollKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Invalidate;
  end;
end;

procedure TJvScrollButton.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvScrollButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    inherited MouseEnter(Control);
    if FFlat then
      Invalidate;
  end;
end;

procedure TJvScrollButton.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    FDown := False;
    inherited MouseLeave(Control);
    if FFlat then
      Invalidate;
  end;
end;

procedure TJvScrollButton.Paint;
const
  Kinds: array [TJvScrollKind] of Integer =
    (DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
var
  Flags: Integer;
  R: TRect;
begin
  if not Visible then
    Exit;
  R := GetClientRect;
  Flags := Kinds[FKind];

  if not Enabled then
  begin
    FDown := False;
    MouseOver := False;
    Flags := Flags or DFCS_INACTIVE or DFCS_FLAT;
  end;

  if FDown then
    Flags := Flags or DFCS_PUSHED;

  if FFlat and not MouseOver then
    Flags := Flags or DFCS_FLAT;

  if MouseOver then
  begin
    if FKind in [sbUp, sbDown] then
      OffsetRect(R, 0, 1)
    else
      OffsetRect(R, 1, 0);
  end;

  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
  Frame3D(Canvas, R, clBtnFace, clBtnFace, 1);
  if FDown then
    Frame3D(Canvas, R, clBtnShadow, clBtnHighLight, 1);
end;

procedure TJvScrollButton.Click;
begin
  if Enabled then
  begin
    inherited Click;
    ReleaseCapture;
  end;
end;

procedure TJvScrollButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := True;
  inherited MouseDown(Button, Shift, X, Y);
  { AutoRepeat }
  if Parent is TJvCustomScrollPanel then
    FAutoRepeat := (Parent as TJvCustomScrollPanel).AutoRepeat;
  if FAutoRepeat then
  begin
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(Self);
    with FTimer do
    begin
      OnTimer := OnTime;
      Interval := cInitTime;
      Enabled := True;
    end;
  end;
  Repaint;
end;

procedure TJvScrollButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FRepeat then
    SetPosition;
  FRepeat := False;
  inherited MouseUp(Button, Shift, X, Y);
  FreeAndNil(FTimer);
  FDown := False;
  Repaint;
end;

procedure TJvScrollButton.SetPosition;
var
  AllowScroll: Boolean;
  Sp: TJvCustomScrollPanel;
begin
  if (Parent = nil) or not (Parent is TJvCustomScrollPanel) or not Parent.Visible then
    Exit;
  Sp := TJvCustomScrollPanel(Parent);
  AllowScroll := True;
  if Assigned(Sp.OnScrolling) then
    Sp.OnScrolling(Self, AllowScroll, FKind);
  if not AllowScroll then
    Exit;

  case FKind of
    sbUp:
      Sp.ScrollBy(0, FIncrement);
    sbDown:
      Sp.ScrollBy(0, -FIncrement);
    sbLeft:
      Sp.ScrollBy(FIncrement, 0);
    sbRight:
      Sp.ScrollBy(-FIncrement, 0);
  end;
  if Assigned(Sp.OnScrolled) then
    Sp.OnScrolled(Self, FKind);
  Sp.UpdateVisible;
end;

procedure TJvScrollButton.OnTime(Sender: TObject);
begin
  FTimer.Interval := cTimeDelay;
  if FDown and MouseCapture then
  begin
    SetPosition;
    FRepeat := True;
    if Parent <> nil then
      Parent.Invalidate;
  end;
end;

procedure TJvScrollButton.EnabledChanged;
begin
  inherited EnabledChanged;
  if Assigned(Parent) then
    Enabled := Parent.Enabled;
  Invalidate;
end;

//=== { TJvCustomScrollPanel } ===============================================

constructor TJvCustomScrollPanel.Create(AOwner: TComponent);
begin
  // We must set the csCreating flag ourselves into ControlState so that
  // other functions can use it. In particular, the Notification method called
  // by one of the inherited Create constructors must see it so as to not call
  // Invalidate. If it calls it during creation, it triggers a big AV.
  // Note that it doesn't seem that the VCL uses that flag itself.
  ControlState := ControlState + [csCreating];
  try
    inherited Create(AOwner);
    // this is very strange: without it I get a "Control '' has no parent window" error
    // when dropping it in design-time. Never seen the need before
    // (rom) probably assigning Align causes it. That needs a parent.
    if AOwner is TWinControl then
      Parent := TWinControl(AOwner);
    ControlStyle := ControlStyle + [csAcceptsControls];
    IncludeThemeStyle(Self, [csParentBackground]);
    BevelInner := bvRaised;
    BevelOuter := bvNone;
    BevelKind := bkTile;
    FScrollDirection := sdHorizontal;
    FScrollAmount := 16;
    Align := alTop;
    Height := 35;
    SetupArrows;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

procedure TJvCustomScrollPanel.AlignArrows;
begin
  if FUpLeft = nil then
    Exit;
  case FScrollDirection of
    sdVertical:
      begin
        FUpLeft.Kind := sbUp;
        FUpLeft.SetBounds(0, 0, ClientWidth, 16);
//        FUpLeft.Anchors := [akTop, akLeft, akRight];
        FUpLeft.Align := alTop;

        FDownRight.Kind := sbDown;
        FDownRight.SetBounds(0, ClientHeight - 16, ClientWidth, 16);
//        FDownRight.Anchors := [akLeft, akRight, akBottom];
        FDownRight.Align := alBottom;
      end;
    sdHorizontal:
      begin
        FUpLeft.Kind := sbLeft;
        FUpLeft.SetBounds(0, 0, 16, ClientHeight);
//        FUpLeft.Anchors := [akTop, akLeft, akBottom];
        FUpLeft.Align := alLeft;

        FDownRight.Kind := sbRight;
        FDownRight.SetBounds(ClientWidth - 16, 0, 16, ClientHeight);
//        FDownRight.Anchors := [akTop, akRight, akBottom];
        FDownRight.Align := alRight;
      end;
  end;
  UpdateVisible;
end;

procedure TJvCustomScrollPanel.SetAutoArrange(Value: Boolean);
begin
  if FAutoArrange <> Value then
    FAutoArrange := Value;
end;

procedure TJvCustomScrollPanel.SetAutoHide(Value: Boolean);
begin
  if FAutoHide <> Value then
  begin
    FAutoHide := Value;
    UpdateVisible;
    Invalidate;
  end;
end;

procedure TJvCustomScrollPanel.SetScrollAmount(const Value: Word);
begin
  if FScrollAmount <> Value then
  begin
    FScrollAmount := Value;
    if Assigned(FUpLeft) then
      FUpLeft.Increment := Value;
    if Assigned(FDownRight) then
      FDownRight.Increment := Value;
  end;
end;

procedure TJvCustomScrollPanel.SetScrollDirection(Value: TJvScrollDirection);
begin
  if FScrollDirection <> Value then
  begin
    FScrollDirection := Value;
    if FAutoArrange then
      ArrangeChildren;
    Invalidate;
    AlignArrows;
    UpdateVisible;
    AlignArrows;
  end;
end;

procedure TJvCustomScrollPanel.ArrangeChildren;
var
  I: Integer;
begin
  if FUpLeft = nil then
    Exit;
  DisableAlign;
  try
    for I := 0 to ControlCount - 1 do
      if (Controls[I] <> FUpLeft) and (Controls[I] <> FDownRight) then
        Controls[I].SetBounds(Controls[I].Top, Controls[I].Left, Controls[I].Height, Controls[I].Width);
    if not (csLoading in ComponentState) and (Align = alNone) then
      SetBounds(0, 0, Height, Width);
  finally
    EnableAlign;
  end;
end;

procedure TJvCustomScrollPanel.UpdateVisible;
var
  Less, More, I: Integer;
begin
  if FUpLeft = nil then
    Exit;
  DisableAlign;
  try
    if FAutoHide then
    begin
      if FScrollDirection = sdVertical then
      begin
        Less := ClientWidth;
        More := 0;
        for I := 0 to ControlCount - 1 do
          if (Controls[I] <> FUpLeft) and (Controls[I] <> FDownRight) and Controls[I].Visible then
          begin
            Less := Min(Controls[I].Top, Less);
            More := Max(Controls[I].Top + Controls[I].Height, More);
          end;
        FUpLeft.Visible := Less < 0;
        FDownRight.Visible := More > ClientHeight;
      end
      else
      if FScrollDirection = sdHorizontal then
      begin
        Less := ClientHeight;
        More := 0;
        for I := 0 to ControlCount - 1 do
          if (Controls[I] <> FUpLeft) and (Controls[I] <> FDownRight) and Controls[I].Visible then
          begin
            Less := Min(Controls[I].Left, Less);
            More := Max(Controls[I].Left + Controls[I].Width, More);
          end;
        FUpLeft.Visible := Less < 0;
        FDownRight.Visible := More > ClientWidth;
      end
    end
    else { always show }
    begin
      FUpLeft.Visible := True;
      FDownRight.Visible := True;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TJvCustomScrollPanel.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if FUpLeft <> nil then
    begin
      FUpLeft.Flat := FFlat;
      FDownRight.Flat := FFlat;
    end;
    Invalidate;
  end;
end;

procedure TJvCustomScrollPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  AlignArrows;
  UpdateVisible;
end;

procedure TJvCustomScrollPanel.SetupArrows;
begin
  if FUpLeft <> nil then
    Exit;
  FUpLeft := TJvScrollButton.Create(Self);
  FUpLeft.FreeNotification(Self);
  FUpLeft.Kind := sbLeft;

  FDownRight := TJvScrollButton.Create(Self);
  FDownRight.FreeNotification(Self);
  FDownRight.Kind := sbRight;
end;

procedure TJvCustomScrollPanel.SetParent( AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FUpLeft = nil then
    Exit;
  FUpLeft.Parent := Self;
  FUpLeft.Visible := True;
  FDownRight.Parent := Self;
  FDownRight.Visible := True;
end;

procedure TJvCustomScrollPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FUpLeft then
      FUpLeft := nil;
    if AComponent = FDownRight then
      FDownRight := nil;
  end;

  // If we invalidate while creating, it triggers a series of exceptions
  // leading to an access violation at 0000000
  // Note that csCreating is specifically added to ControlState in the
  // constructor for Notification to be able to use
  if not (csCreating in ControlState) then
    Invalidate;
end;

procedure TJvCustomScrollPanel.EnabledChanged;
begin
  inherited EnabledChanged;
  if FUpLeft = nil then
    Exit;
  FUpLeft.Enabled := Enabled;
  FDownRight.Enabled := Enabled;
  if AutoHide then
    UpdateVisible
  else
    Invalidate;
end;

procedure TJvCustomScrollPanel.VisibleChanged;
begin
  inherited VisibleChanged;
  if FUpLeft = nil then
    Exit;
  FUpLeft.Visible := Visible;
  FDownRight.Visible := Visible;
end;

//=== { TJvScrollingWindow } =================================================

constructor TJvScrollingWindow.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  AutoHide := True;
  AutoRepeat := False;
  ScrollDirection := sdHorizontal;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
