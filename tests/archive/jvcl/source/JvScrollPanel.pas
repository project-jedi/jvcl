{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollPanel.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A scrolling TToolWindow like the ones in IE 4.0 with popup scrollbuttons
   either on top/bottom or left/right edge. }
unit JvScrollPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ExtCtrls, JvComponent, JVCLVer;

type
  TJvCustomScrollPanel = class;
  TJvDivideKind = (dkDivider, dkSeparator);
  TJvScrollKind = (sbUp, sbDown, sbLeft, sbRight);
  /// (DFCS_SCROLLUP,DFCS_SCROLLDOWN,DFCS_SCROLLLEFT,DFCS_SCROLLRIGHT);
  TJvScrollDirection = (sdHorizontal, sdVertical);
  TJvScrollingEvent = procedure(Sender: TObject; var AllowChange: boolean; Kind: TJvScrollKind) of object;
  TJvScrolledEvent = procedure(Sender: TObject; Kind: TJvScrollKind) of object;

  TJvDivider = class(TJvGraphicControl)
  private
    FKind: TJvDivideKind;
    FVertical: boolean;
    procedure SetKind(Value: TJvDivideKind);
    procedure SetVertical(Value: boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Vertical: boolean read FVertical write SetVertical default true;
    property Kind: TJvDivideKind read FKind write SetKind default dkDivider;
  end;

  TJvScrollButton = class(TJvCustomControl)
  private
    FInsideButton: boolean;
    FDown, FRepeat: boolean;
    FFlat: boolean;
    FAutoRepeat: boolean;
    FScrollAmount: word;
    FTimer: TTimer;
    FKind: TJvScrollKind;
    procedure SetKind(Value: TJvScrollKind);
    procedure SetFlat(Value: boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure OnTime(Sender: TObject); protected
    procedure SetPosition; virtual;
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoRepeat: boolean read FAutoRepeat write FAutoRepeat;
    property Flat: boolean read FFlat write SetFlat;
    property Kind: TJvScrollKind read FKind write SetKind;
    property Increment: word read FScrollAmount write FScrollAmount;
    property Width default 16;
    property Height default 16;
  end;

  TJvCustomScrollPanel = class(TToolWindow)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FScrollDirection: TJvScrollDirection;
    FScrollAmount: word;
    FAutoHide: boolean;
    FAutoRepeat: boolean;
    FAutoArrange: boolean;
    FUpLeft: TJvScrollButton;
    FDownRight: TJvScrollButton;
    FOnScrolling: TJvScrollingEvent;
    FOnScrolled: TJvScrolledEvent;
    FFlat: boolean;
    procedure SetAutoArrange(Value: boolean);
    procedure SetAutoHide(Value: boolean);
    procedure SetScrollDirection(Value: TJvScrollDirection);
    procedure SetFlat(Value: boolean);
    procedure AlignArrows;
    procedure UpdateVisible;
    procedure ArrangeChildren;
    procedure SetupArrows;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    property Align default alTop;
    property Height default 35;
    property AutoHide: boolean read FAutoHide write SetAutoHide;
    property AutoRepeat: boolean read FAutoRepeat write FAutoRepeat;
    property AutoArrange: boolean read FAutoArrange write SetAutoArrange default False;
    property Flat: boolean read FFlat write SetFlat;
    property ScrollDirection: TJvScrollDirection read FScrollDirection write SetScrollDirection;
    property ScrollAmount: word read FScrollAmount write FScrollAmount default 16;
    property OnScrolling: TJvScrollingEvent read FOnScrolling write FOnScrolling;
    property OnScrolled: TJvScrolledEvent read FOnScrolled write FOnScrolled;
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;

  end;

  TJvScrollingWindow = class(TJvCustomScrollPanel)
  public
    constructor Create(AComponent:TComponent);override;
  published
    property AboutJVCL;
    property AutoArrange;
    property AutoHide default true;
    property AutoRepeat default false;
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
    property Ctl3D;
    property ShowHint;
    property Hint;
    property ParentShowHint;
    property PopUpMenu;
    property ImeMode;
    property ImeName;
    property Color;
    property ParentColor;
    property ParentCtl3D;
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

implementation

const
  cInitTime = 360;
  cTimeDelay = 100;

  { utility }

function Min(X, Y: integer): integer;
begin
  if X > Y then
    Result := Y
  else
    Result := X;
end;

function Max(X, Y: integer): integer;
begin
  if X > Y then
    Result := X
  else
    Result := Y
end;

{
procedure TileBitmap(Dest:TControl;Source:TBitmap);
var
  X, Y, W, H: LongInt;
  DR,SR:TRect;
  Tmp:TBitmap;
  Canvas:TControlCanvas;
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
    while y < Dest.Height do begin
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

{ TJvDivider }

constructor TJvDivider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 16, 16);
  FVertical := true;
  FKind := dkDivider;
end;

procedure TJvDivider.SetVertical(Value: boolean);
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
var R: TRect;
begin
  if not Visible then Exit;
  if (FKind = dkDivider) then
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
  if (csDesigning in ComponentState) then
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
end;

{ TJvScrollButton }

constructor TJvScrollButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks, csSetCaption];
  FDown := False;
  FScrollAmount := 16;
  FInsideButton := False;
  FAutoRepeat := False;
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

procedure TJvScrollButton.SetFlat(Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvScrollButton.CMMouseEnter(var Message: TMessage);
begin
  FInsideButton := True;
  if FFlat then
    Invalidate;
end;

procedure TJvScrollButton.CMMouseLeave(var Message: TMessage);
begin
  FInsideButton := False;
  FDown := False;
  if FFlat then
    Invalidate;
end;

procedure TJvScrollButton.Paint;
const
  aKind: array[TJvScrollKind] of integer =
  (DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
var Flags: integer; R: TRect;
begin
  if not Visible then Exit;
  R := GetClientRect;
  Flags := aKind[FKind];

  if not Enabled then
  begin
    FDown := False;
    FInsideButton := false;
    Flags := Flags or DFCS_INACTIVE or DFCS_FLAT;
  end;

  if FDown then
    Flags := Flags or DFCS_PUSHED;

  if FFlat and not FInsideButton then
    Flags := Flags or DFCS_FLAT;

  if FInsideButton then
  begin
    if FKind in [sbUp, sbDown] then
      OffsetRect(R, 0, 1)
    else
      OffsetRect(R, 1, 0);
  end;

  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
  Frame3d(Canvas, R, clBtnFace, clBtnFace, 1);
  if FDown then
    Frame3d(Canvas, R, clBtnShadow, clBtnHighLight, 1);
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
      FTimer := TTimer.Create(self);
    with FTimer do
    begin
      OnTimer := OnTime;
      Interval := cInitTime;
      Enabled := True;
    end;
  end;
  Repaint;
end;

procedure TJvScrollButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not FRepeat then
    SetPosition;
  FRepeat := false;
  inherited MouseUp(Button, Shift, X, Y);
  FreeAndNil(FTimer);
  FDown := False;
  Repaint;
end;

procedure TJvScrollButton.SetPosition;
var AllowScroll: boolean; sp: TJvCustomScrollPanel;
begin
  if (Parent = nil) or not (Parent is TJvCustomScrollPanel) or not Parent.Visible then
    Exit;
  sp := TJvCustomScrollPanel(Parent);
  AllowScroll := True;
  if Assigned(sp.OnScrolling) then
    sp.OnScrolling(self, AllowScroll, FKind);
  if not AllowScroll then
    Exit;

  case FKind of
    sbUp:
      sp.ScrollBy(0, FScrollAmount);
    sbDown:
      sp.ScrollBy(0, -FScrollAmount);
    sbLeft:
      sp.ScrollBy(FScrollAmount, 0);
    sbRight:
      sp.ScrollBy(-FScrollAmount, 0);
  end;
  if Assigned(sp.OnScrolled) then
    sp.OnScrolled(self, FKind);
  sp.UpdateVisible;
end;

procedure TJvScrollButton.OnTime(Sender: TObject);
begin
  FTimer.Interval := cTimeDelay;
  if FDown and MouseCapture then
  begin
    SetPosition;
    FRepeat := true;
    if Parent <> nil then
      Parent.Invalidate;
  end;
end;

procedure TJvScrollButton.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(Parent) then
    Enabled := Parent.Enabled;
  Invalidate;
end;

{ TJvScrollingWindow }

constructor TJvCustomScrollPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // this is very strange: without it I get a "Control '' has no parent window" error
  // when dropping it in design-time. Never seen the need before
  if AOwner is TWinControl then
    Parent := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FScrollDirection := sdHorizontal;
  BevelInner := bvRaised;
  BevelOuter := bvNone;
  BevelKind := bkTile;
  FScrollAmount := 16;
  Align := alTop;
  Height := 35;
  SetupArrows;
end;

procedure TJvCustomScrollPanel.AlignArrows;
begin
  if FUpLeft = nil then
    Exit;
  case ScrollDirection of
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

procedure TJvCustomScrollPanel.SetAutoArrange(Value: boolean);
begin
  if FAutoArrange <> Value then
    FAutoArrange := Value;
end;

procedure TJvCustomScrollPanel.SetAutoHide(Value: boolean);
begin
  if FAutoHide <> Value then
  begin
    FAutoHide := Value;
    UpdateVisible;
    Invalidate;
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
    //    AlignArrows;
    //    UpdateVisible;
  end;
end;

procedure TJvCustomScrollPanel.ArrangeChildren;
var i: integer;
begin
  if FUpLeft = nil then
    Exit;
  DisableAlign;
  try
    for i := 0 to ControlCount - 1 do
    begin
      if (Controls[i] = FUpLeft) or (Controls[i] = FDownRight) then
        Continue;
      Controls[i].SetBounds(Controls[i].Top,Controls[i].Left,Controls[i].Height,Controls[i].Width);
    end;
    if not (csLoading in ComponentState) and (Align = alNone) then
      SetBounds(0, 0, Height, Width);
  finally
    EnableAlign;
  end;
end;

procedure TJvCustomScrollPanel.UpdateVisible;
var Less, More, i: integer;
begin
  if FUpLeft = nil then
    Exit;
  DisableAlign;
  try
    if (FAutoHide) then
    begin
      if FScrollDirection = sdVertical then
      begin
        Less := ClientWidth;
        More := 0;
        for i := 0 to ControlCount - 1 do
        begin
          if (Controls[i] = FUpLeft) or (Controls[i] = FDownRight) then
            Continue;
          Less := Min(Controls[i].Top, Less);
          More := Max(Controls[i].Top + Controls[i].Height, More);
        end;
        FUpLeft.Visible := Less < 0;
        FDownRight.Visible := More > ClientHeight;
      end
      else if FScrollDirection = sdHorizontal then
      begin
        Less := ClientHeight;
        More := 0;
        for i := 0 to ControlCount - 1 do
        begin
          if (Controls[i] = FUpLeft) or (Controls[i] = FDownRight) then
            Continue;
          Less := Min(Controls[i].Left, Less);
          More := Max(Controls[i].Left + Controls[i].Width, More);
        end;
        FUpLeft.Visible := Less < 0;
        FDownRight.Visible := More > ClientWidth;
      end
    end
    else { always show }
    begin
      FUpLeft.Visible := true;
      FDownRight.Visible := true;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TJvCustomScrollPanel.SetFlat(Value: boolean);
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

procedure TJvCustomScrollPanel.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  AlignArrows;
  UpdateVisible;
end;

procedure TJvCustomScrollPanel.SetupArrows;
begin
  if FUpLeft <> nil then
    Exit;
  FUpLeft := TJvScrollButton.Create(self);
  FUpLeft.FreeNotification(self);
  FUpLeft.Kind := sbLeft;

  FDownRight := TJvScrollButton.Create(self);
  FDownRight.FreeNotification(self);
  FDownRight.Kind := sbRight;
end;

procedure TJvCustomScrollPanel.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (FUpLeft = nil) then
    Exit;
  FUpLeft.Parent := self;
  FUpLeft.Visible := True;
  FDownRight.Parent := self;
  FDownRight.Visible := True;
end;

procedure TJvCustomScrollPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FUpLeft then
      FUpLeft := nil;
    if AComponent = FDownRight then
      FDownRight := nil;
  end;
end;

procedure TJvCustomScrollPanel.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  FUpLeft.Enabled := Enabled;
  FDownRight.Enabled := Enabled;
  if AutoHide then UpdateVisible else Invalidate;
end;

procedure TJvCustomScrollPanel.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  FUpLeft.Visible := Visible;
  FDownRight.Visible := Visible;
end;

{ TJvScrollingWindow }

constructor TJvScrollingWindow.Create(AComponent: TComponent);
begin
  inherited;
  AutoHide := true;
end;

end.

