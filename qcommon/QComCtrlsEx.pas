{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QToolWin.pas, released on 2004-05-16

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

Known Issues:
----------------------------------------------------------------------------}
// $Id$

unit QComCtrlsEx;

interface

uses
  SysUtils, Classes, Contnrs, Types, Qt, QGraphics, QControls, QForms,
  QStdCtrls, QExtCtrls, QComboEdits, QWindows;

type
  TUDAlignButton = (udLeft, udRight);
  TUDOrientation = (udHorizontal, udVertical);
  TUDBtnType = (btNext, btPrev);
  TUpDownDirection = (updNone, updUp, updDown);
  TUDClickEvent = procedure (Sender: TObject; Button: TUDBtnType) of object;
  TUDChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;
  TUDChangingEventEx = procedure (Sender: TObject; var AllowChange: Boolean;
    NewValue: SmallInt; Direction: TUpDownDirection) of object;

  TCustomUpDown = class(TCustomControl)
  private
    FArrowKeys: Boolean;
    FAssociate: TWidgetControl;
    FMin: SmallInt;
    FMax: SmallInt;
    FIncrement: Integer;
    FNewValue: SmallInt;
    FNewValueDelta: SmallInt;
    FPosition: SmallInt;
    FThousands: Boolean;
    FWrap: Boolean;
    FOnClick: TUDClickEvent;
    FAlignButton: TUDAlignButton;
    FOrientation: TUDOrientation;
    FOnChanging: TUDChangingEvent;
    FOnChangingEx: TUDChangingEventEx;

    FButtonDown: Integer;
    FMouseOverButton: Boolean;
    FRepeatTimer: TTimer;
    FForceAlign: Boolean;

    procedure SetAssociate(Value: TWinControl);
    function GetPosition: SmallInt;
    procedure SetMin(Value: SmallInt);
    procedure SetMax(Value: SmallInt);
    procedure SetPosition(Value: SmallInt);
    procedure SetAlignButton(Value: TUDAlignButton);
    procedure SetOrientation(Value: TUDOrientation);
    procedure SetArrowKeys(Value: Boolean);
    procedure SetThousands(Value: Boolean);
    function GetRepeatInterval: Integer;
    procedure SetRepeatInterval(Value: Integer);
    procedure SetForceAlign(const Value: Boolean);
  protected
    function AssociateHook(Sender: QObjectH; Event: QEventH): Boolean; virtual;
    procedure Paint; override;
    procedure UpdatePosition(Value: SmallInt); virtual;
    procedure UpdateAlignment; virtual;
    function ButtonRect(ButtonIndex: Integer): TRect; virtual;
    procedure RepeatTimer(Sender: TObject); virtual;
    procedure ChangePosition(ButtonIndex: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure BoundsChanged; override;

    function DoCanChange(NewVal: SmallInt; Delta: SmallInt): Boolean; virtual;
    function CanChange: Boolean; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click(Button: TUDBtnType); reintroduce; dynamic;
    property AlignButton: TUDAlignButton read FAlignButton write SetAlignButton default udRight;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property Associate: TWidgetControl read FAssociate write SetAssociate;
    property Min: SmallInt read FMin write SetMin default 0;
    property Max: SmallInt read FMax write SetMax default 100;
    property Increment: Integer read FIncrement write FIncrement default 1;
    property Orientation: TUDOrientation read FOrientation write SetOrientation default udVertical;
    property Position: SmallInt read GetPosition write SetPosition default 0;
    property Thousands: Boolean read FThousands write SetThousands default True;
    property Wrap: Boolean read FWrap write FWrap default False;
    property OnChanging: TUDChangingEvent read FOnChanging write FOnChanging;
    property OnChangingEx: TUDChangingEventEx read FOnChangingEx write FOnChangingEx;
    property OnClick: TUDClickEvent read FOnClick write FOnClick;

    // exclusive for VisualCLX
    property RepeatInterval: Integer read GetRepeatInterval write SetRepeatInterval default 125;
    property ForceAlign: Boolean read FForceAlign write SetForceAlign default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TUpDown = class(TCustomUpDown)
  published
    property AlignButton;
    property Anchors;
    property Associate;
    property ArrowKeys;
    property Enabled;
    property Hint;
    property Min;
    property Max;
    property Increment;
    property Constraints;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Thousands;
    property Visible;
    property Wrap;
    property OnChanging;
    property OnChangingEx;
    property OnContextPopup;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  Math;

function ExcludeThousands(const Text: string): string;
var
  Len, i: Integer;
begin
  Result := Text;
  Len := Length(Result);
  if Len > 4 then
  begin
    i := Len - 3;
    while (i > 1) do
    begin
      if Result[i] = ThousandSeparator then
        Delete(Result, i, 1);
      Dec(i, 2);
    end;
  end;
end;

function IncludeThousands(const Text: string): string;
var
  Len, i: Integer;
begin
  Result := Text;
  Len := Length(Result);
  if Len > 3 then
  begin
    i := Len - 2;
    while (i > 1) do
    begin
      Insert(ThousandSeparator, Result, i);
      Dec(i, 3);
    end;
  end;
end;

{ TCustomUpDown }

constructor TCustomUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  FThousands := True;
  FButtonDown := -1;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FIncrement := 1;

  FAlignButton := udRight;
  FOrientation := udVertical;
  Width := GetSystemMetrics(SM_CXVSCROLL);
  Height := GetSystemMetrics(SM_CYVSCROLL) - 2; // is SM_CYVSCROLL wrong or is it just Qt
  Height := Height + (Height div 2);

  FRepeatTimer := TTimer.Create(nil);
  FRepeatTimer.Interval := 125;
  FRepeatTimer.OnTimer := RepeatTimer;

  // install hook
  FArrowKeys := False;
  SetArrowKeys(True);
end;

destructor TCustomUpDown.Destroy;
begin
  SetArrowKeys(False); // uninstall hook
  FRepeatTimer.Free;
  inherited Destroy;
end;

function TCustomUpDown.CanChange: Boolean;
var
  Direction: TUpDownDirection;
begin
  Result := True;
  Direction := updNone;

  if (FNewValue < Min) and (FNewValueDelta < 0) or
     (FNewValue > Max) and (FNewValueDelta > 0) then
    Direction := updNone
  else if FNewValueDelta < 0 then
    Direction := updDown
  else if FNewValueDelta > 0 then
    Direction := updUp;

  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);
  if Assigned(FOnChangingEx) then
    FOnChangingEx(Self, Result, FNewValue, Direction);
end;

procedure TCustomUpDown.Click(Button: TUDBtnType);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button);
end;

function TCustomUpDown.DoCanChange(NewVal, Delta: SmallInt): Boolean;
begin
  FNewValue := NewVal;
  FNewValueDelta := Delta;
  Result := CanChange;
end;

function TCustomUpDown.GetPosition: SmallInt;
var
  ErrCode: Integer;
begin
  if Associate is TCustomEdit then
  begin
    if Thousands then
      Val(ExcludeThousands(TCustomEdit(Associate).Text), Result, ErrCode)
    else
      Val(TCustomEdit(Associate).Text, Result, ErrCode);
    if ErrCode = 0 then
      FPosition := Result;
  end;
  Result := FPosition;
end;

procedure TCustomUpDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Associate) then
    Associate := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TCustomUpDown.SetAlignButton(Value: TUDAlignButton);
begin
  if Value <> FAlignButton then
  begin
    FAlignButton := Value;
    UpdateAlignment;
  end;
end;

procedure TCustomUpDown.SetArrowKeys(Value: Boolean);
begin
  if Value <> FArrowKeys then
  begin
    if FArrowKeys then
      UninstallApplicationHook(AssociateHook);
    FArrowKeys := Value;
    if FArrowKeys then
      InstallApplicationHook(AssociateHook);
  end;
end;

procedure TCustomUpDown.SetAssociate(Value: TWinControl);
begin
  if Value <> FAssociate then
  begin
    if FAssociate <> nil then
      FAssociate.RemoveFreeNotification(Self);
    FAssociate := Value;
    if FAssociate <> nil then
    begin
      FAssociate.FreeNotification(Self);
      UpdateAlignment;
      UpdatePosition(Position);
    end;
  end;
end;

procedure TCustomUpDown.SetMax(Value: SmallInt);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    UpdatePosition(Position);
  end;
end;

procedure TCustomUpDown.SetMin(Value: SmallInt);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    UpdatePosition(Position);
  end;
end;

procedure TCustomUpDown.SetOrientation(Value: TUDOrientation);
begin
  if Value <> FOrientation then
  begin
    FOrientation := Value;
    Invalidate;
  end;
end;

procedure TCustomUpDown.SetPosition(Value: SmallInt);
begin
  if Value <> FPosition then
  begin
    if (Max > Min) and (Value > Max) and Wrap then
      Value := Min;
    if (Max > Min) and (Value < Min) and Wrap then
      Value := Max;
    UpdatePosition(Value);
  end;
end;

procedure TCustomUpDown.UpdatePosition(Value: SmallInt);
begin
  if Value < Min then
    Value := Min;
  if (Max > Min) and (Value > Max) then
    Value := Max;

  if DoCanChange(Value, Value - FPosition) then
  begin
    FPosition := Value;

    if Associate is TCustomEdit then
    begin
      if Thousands then
        TCustomEdit(Associate).Text := IncludeThousands(IntToStr(FPosition))
      else
        TCustomEdit(Associate).Text := IntToStr(FPosition);
    end;
  end;
end;

procedure TCustomUpDown.SetThousands(Value: Boolean);
var
  Pos: Integer;
begin
  if Value <> FThousands then
  begin
    Pos := Position;
    FThousands := Value;
    UpdatePosition(Pos);
  end;
end;

function TCustomUpDown.ButtonRect(ButtonIndex: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if Orientation = udVertical then
  begin
    case ButtonIndex of
      0: Result := Rect(0, 0, Width, Height div 2);
      1: Result := Rect(0, Height div 2, Width, Height);
    end;
  end
  else
  begin
    case ButtonIndex of
      0: Result := Rect(0, 0, Width div 2, Height);
      1: Result := Rect(Width div 2, 0, Width, Height);
    end;
  end;
end;

procedure TCustomUpDown.Paint;
const
  BtnType: array[0..1, TUDOrientation] of Integer = (
    (DFCS_SCROLLLEFT, DFCS_SCROLLUP),
    (DFCS_SCROLLRIGHT, DFCS_SCROLLDOWN)
  );
var
  Flags, BtnFlags: Integer;
  R: TRect;
  Pixmap: QPixmapH;
  Painter: QPainterH;
  i: Integer;
begin
  Pixmap := QPixmap_create(Width, Height, -1, QPixmapOptimization_DefaultOptim);
  Painter := QPainter_create(Pixmap);
  try
    Flags := 0;
    if not Enabled then
      Flags := DFCS_INACTIVE;

    for i := 0 to 1 do
    begin
      R := ButtonRect(i);
      BtnFlags := 0;
      if (FButtonDown = i) and FMouseOverButton then
        BtnFlags := BtnFlags or DFCS_PUSHED;
      DrawFrameControl(Painter, R, DFC_SCROLL, BtnType[i, Orientation] or Flags or BtnFlags);
    end;
  finally
    QPainter_destroy(Painter);
    Canvas.Start;
    QPainter_drawPixmap(Canvas.Handle, 0, 0, Pixmap, 0, 0, Width, Height);
    Canvas.Stop;
  end;
end;

procedure TCustomUpDown.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
begin
  if Button = mbLeft then
  begin
    for i := 0 to 1 do
      if PtInRect(ButtonRect(i), Point(X, Y)) then
      begin
        MouseCapture := True;
        FMouseOverButton := True;
        FButtonDown := i;
        ChangePosition(FButtonDown);

        FRepeatTimer.Tag := 1;
        FRepeatTimer.Enabled := True;
        Exit;
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomUpDown.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FButtonDown <> -1) then
  begin
    FRepeatTimer.Enabled := False;
    FButtonDown := -1;
    MouseCapture := False;
    Paint;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomUpDown.RepeatTimer(Sender: TObject);
begin
  if FRepeatTimer.Tag = 1 then
  begin
    FRepeatTimer.Tag := 0;
    Exit;
  end;
  if (FButtonDown <> -1) then
  begin
    FMouseOverButton := PtInRect(ButtonRect(FButtonDown), ScreenToClient(Mouse.CursorPos));
    if FMouseOverButton then
      ChangePosition(FButtonDown)
    else
      Paint;
  end;
end;

function TCustomUpDown.GetRepeatInterval: Integer;
begin
  Result := FRepeatTimer.Interval;
end;

procedure TCustomUpDown.SetRepeatInterval(Value: Integer);
begin
  if Value < 10 then
    Value := 10;
  FRepeatTimer.Interval := Value;
end;

function TCustomUpDown.AssociateHook(Sender: QObjectH;
  Event: QEventH): Boolean;
begin
  Result := False;
  if (Associate <> nil) and (Associate.HandleAllocated) then
  begin
    if Sender = Associate.Handle then
    begin
      case QEvent_type(Event) of
        QEventType_KeyPress:
          begin
            if QKeyEvent_key(QKeyEventH(Event)) = Key_Up then
            begin
              FMouseOverButton := True;
              FButtonDown := 0;
              ChangePosition(FButtonDown);
              FButtonDown := -1;
              Result := True;
            end
            else
            if QKeyEvent_key(QKeyEventH(Event)) = Key_Down then
            begin
              FMouseOverButton := True;
              FButtonDown := 1;
              ChangePosition(FButtonDown);
              FButtonDown := -1;
              Result := True;
            end;
          end;
        QEventType_KeyRelease:
          begin
            if not (QKeyEvent_isAutoRepeat(QKeyEventH(Event))) then
            begin
              if QKeyEvent_key(QKeyEventH(Event)) = Key_Up then
              begin
                FButtonDown := -1;
                Paint;
                Result := True;
              end
              else
              if QKeyEvent_key(QKeyEventH(Event)) = Key_Down then
              begin
                FButtonDown := -1;
                Paint;
                Result := True;
              end;
            end;
          end;
      end;
    end;
  end;
end;

procedure TCustomUpDown.ChangePosition(ButtonIndex: Integer);
begin
  if Orientation = udVertical then
  begin
    case ButtonIndex of
      0:
        begin
          Position := Position + Increment;
          Click(btNext);
        end;
      1:
        begin
          Position := Position - Increment;
          Click(btPrev);
        end;
    end;
  end
  else
  begin
    case ButtonIndex of
      0:
        begin
          Position := Position - Increment;
          Click(btPrev);
        end;
      1:
        begin
          Position := Position + Increment;
          Click(btNext);
        end;
    end;
  end;
  Paint;
end;

procedure TCustomUpDown.UpdateAlignment;
begin
  if Associate = nil then
    Exit;

  if AlignButton = udRight then
  begin
    Top := Associate.Top;
    Left := Associate.BoundsRect.Right + 1;
  end
  else // udLeft
  begin
    Top := Associate.Top;
    Left := Associate.Left - 1 - Width;
  end;
end;

procedure TCustomUpDown.BoundsChanged;
begin
  inherited BoundsChanged;
  if ForceAlign then
    UpdateAlignment;
end;

procedure TCustomUpDown.SetForceAlign(const Value: Boolean);
begin
  FForceAlign := Value;
  if FForceAlign then
    UpdateAlignment;
end;

end.
