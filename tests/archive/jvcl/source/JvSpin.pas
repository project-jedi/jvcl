{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpin.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSpin;

interface

uses {$IFDEF WIN32} Windows, ComCtrls, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Controls, ExtCtrls, Classes, Graphics, Messages, Forms, StdCtrls, Menus,
  SysUtils;

type

{ TJvSpinButton }

  TSpinButtonState = (sbNotDown, sbTopDown, sbBottomDown);

  TJvSpinButton = class(TGraphicControl)
  private
    FDown: TSpinButtonState;
    FUpBitmap: TBitmap;
    FDownBitmap: TBitmap;
    FDragging: Boolean;
    FInvalidate: Boolean;
    FTopDownBtn: TBitmap;
    FBottomDownBtn: TBitmap;
    FRepeatTimer: TTimer;
    FNotDownBtn: TBitmap;
    FLastDown: TSpinButtonState;
    FFocusControl: TWinControl;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    procedure TopClick;
    procedure BottomClick;
    procedure GlyphChanged(Sender: TObject);
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    procedure SetDown(Value: TSpinButtonState);
    procedure SetFocusControl(Value: TWinControl);
    procedure DrawAllBitmap;
    procedure DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
    procedure TimerExpired(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Down: TSpinButtonState read FDown write SetDown default sbNotDown;
  published
    property DragCursor;
    property DragMode;
    property Enabled;
    property Visible;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowHint;
    property ParentShowHint;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvxSpinEdit }

{$IFDEF CBUILDER}
  TValueType = (vtInt, vtFloat, vtHex);
{$ELSE}
  TValueType = (vtInteger, vtFloat, vtHex);
{$ENDIF}

{$IFDEF WIN32}
  TSpinButtonKind = (bkStandard, bkDiagonal);
{$ENDIF}

  TJvxSpinEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FDecimal: Byte;
    FChanging: Boolean;
    FEditorEnabled: Boolean;
    FValueType: TValueType;
    FButton: TJvSpinButton;
    FBtnWindow: TWinControl;
    FArrowKeys: Boolean;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
{$IFDEF WIN32}
    FButtonKind: TSpinButtonKind;
    FUpDown: TCustomUpDown;
    function GetButtonKind: TSpinButtonKind;
    procedure SetButtonKind(Value: TSpinButtonKind);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
{$ENDIF}
    function GetMinHeight: Integer;
    procedure GetTextHeight(var SysHeight, Height: Integer);
    function GetValue: Extended;
    function CheckValue(NewValue: Extended): Extended;
    function GetAsInteger: Longint;
    function IsIncrementStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetArrowKeys(Value: Boolean);
    procedure SetAsInteger(NewValue: Longint);
    procedure SetValue(NewValue: Extended);
    procedure SetValueType(NewType: TValueType);
    procedure SetDecimal(NewValue: Byte);
    function GetButtonWidth: Integer;
    procedure RecreateButton;
    procedure ResizeButton;
    procedure SetEditRect;
    procedure SetAlignment(Value: TAlignment);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
{$IFDEF COMPILER4_UP}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
{$ENDIF}
  protected
    procedure Change; override;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsInteger: Longint read GetAsInteger write SetAsInteger default 0;
    property Text;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
{$IFDEF WIN32}
    property ButtonKind: TSpinButtonKind read FButtonKind write SetButtonKind
      default bkDiagonal;
{$ENDIF}
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write FMaxValue stored IsMaxStored;
    property MinValue: Extended read FMinValue write FMinValue stored IsMinStored;
    property ValueType: TValueType read FValueType write SetValueType
      default {$IFDEF CBUILDER} vtInt {$ELSE} vtInteger {$ENDIF};
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

implementation

uses {$IFDEF WIN32} CommCtrl, {$ENDIF} JvVCLUtils;

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

const
  sSpinUpBtn = 'JVSPINUP';
  sSpinDownBtn = 'JVSPINDOWN';

const
  InitRepeatPause = 400; { pause before repeat timer (ms) }
  RepeatPause     = 100;

{ TJvSpinButton }

constructor TJvSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpBitmap := TBitmap.Create;
  FDownBitmap := TBitmap.Create;
  FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
  FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);
  FUpBitmap.OnChange := GlyphChanged;
  FDownBitmap.OnChange := GlyphChanged;
  Height := 20;
  Width := 20;
  FTopDownBtn := TBitmap.Create;
  FBottomDownBtn := TBitmap.Create;
  FNotDownBtn := TBitmap.Create;
  DrawAllBitmap;
  FLastDown := sbNotDown;
end;

destructor TJvSpinButton.Destroy;
begin
  FTopDownBtn.Free;
  FBottomDownBtn.Free;
  FNotDownBtn.Free;
  FUpBitmap.Free;
  FDownBitmap.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TJvSpinButton.GlyphChanged(Sender: TObject);
begin
  FInvalidate := True;
  Invalidate;
end;

function TJvSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpBitmap;
end;

procedure TJvSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then FUpBitmap.Assign(Value)
  else FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
end;

function TJvSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownBitmap;
end;

procedure TJvSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then FDownBitmap.Assign(Value)
  else FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);
end;

procedure TJvSpinButton.SetDown(Value: TSpinButtonState);
var
  OldState: TSpinButtonState;
begin
  OldState := FDown;
  FDown := Value;
  if OldState <> FDown then Repaint;
end;

procedure TJvSpinButton.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TJvSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TJvSpinButton.Paint;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  if (FNotDownBtn.Height <> Height) or (FNotDownBtn.Width <> Width) or
    FInvalidate then DrawAllBitmap;
  FInvalidate := False;
  with Canvas do
    case FDown of
      sbNotDown: Draw(0, 0, FNotDownBtn);
      sbTopDown: Draw(0, 0, FTopDownBtn);
      sbBottomDown: Draw(0, 0, FBottomDownBtn);
    end;
end;

procedure TJvSpinButton.DrawAllBitmap;
begin
  DrawBitmap(FTopDownBtn, sbTopDown);
  DrawBitmap(FBottomDownBtn, sbBottomDown);
  DrawBitmap(FNotDownBtn, sbNotDown);
end;

procedure TJvSpinButton.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
var
  R, RSrc: TRect;
  dRect: Integer;
  {Temp: TBitmap;}
begin
  ABitmap.Height := Height;
  ABitmap.Width := Width;
  with ABitmap.Canvas do begin
    R := Bounds(0, 0, Width, Height);
    Pen.Width := 1;
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    FillRect(R);
    { buttons frame }
    Pen.Color := clWindowFrame;
    Rectangle(0, 0, Width, Height);
    MoveTo(-1, Height);
    LineTo(Width, -1);
    { top button }
    if ADownState = sbTopDown then Pen.Color := clBtnShadow
    else Pen.Color := clBtnHighlight;
    MoveTo(1, Height - 4);
    LineTo(1, 1);
    LineTo(Width - 3, 1);
    if ADownState = sbTopDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    if ADownState <> sbTopDown then begin
      MoveTo(1, Height - 3);
      LineTo(Width - 2, 0);
    end;
    { bottom button }
    if ADownState = sbBottomDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    MoveTo(2, Height - 2);
    LineTo(Width - 2, Height - 2);
    LineTo(Width - 2, 1);
    if ADownState = sbBottomDown then Pen.Color := clBtnShadow
      else Pen.Color := clBtnHighlight;
    MoveTo(2, Height - 2);
    LineTo(Width - 1, 1);
    { top glyph }
    dRect := 1;
    if ADownState = sbTopDown then Inc(dRect);
    R := Bounds(Round((Width / 4) - (FUpBitmap.Width / 2)) + dRect,
      Round((Height / 4) - (FUpBitmap.Height / 2)) + dRect, FUpBitmap.Width,
      FUpBitmap.Height);
    RSrc := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FUpBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor);
    { bottom glyph }
    R := Bounds(Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
      Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1,
      FDownBitmap.Width, FDownBitmap.Height);
    RSrc := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FDownBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor);
    if ADownState = sbBottomDown then begin
      Pen.Color := clBtnShadow;
      MoveTo(3, Height - 2);
      LineTo(Width - 1, 2);
    end;
  end;
end;

procedure TJvSpinButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FInvalidate := True;
  Invalidate;
end;

procedure TJvSpinButton.TopClick;
begin
  if Assigned(FOnTopClick) then begin
    FOnTopClick(Self);
    if not (csLButtonDown in ControlState) then FDown := sbNotDown;
  end;
end;

procedure TJvSpinButton.BottomClick;
begin
  if Assigned(FOnBottomClick) then begin
    FOnBottomClick(Self);
    if not (csLButtonDown in ControlState) then FDown := sbNotDown;
  end;
end;

procedure TJvSpinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then begin
    if (FFocusControl <> nil) and FFocusControl.TabStop and
      FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
        FFocusControl.SetFocus;
    if FDown = sbNotDown then begin
      FLastDown := FDown;
      if Y > (-(Height/Width) * X + Height) then begin
        FDown := sbBottomDown;
        BottomClick;
      end
      else begin
        FDown := sbTopDown;
        TopClick;
      end;
      if FLastDown <> FDown then begin
        FLastDown := FDown;
        Repaint;
      end;
      if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Interval := InitRepeatPause;
      FRepeatTimer.Enabled := True;
    end;
    FDragging := True;
  end;
end;

procedure TJvSpinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TSpinButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then begin
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then begin
      NewState := FDown;
      if Y > (-(Width / Height) * X + Height) then begin
        if (FDown <> sbBottomDown) then begin
          if FLastDown = sbBottomDown then FDown := sbBottomDown
          else FDown := sbNotDown;
          if NewState <> FDown then Repaint;
        end;
      end
      else begin
        if (FDown <> sbTopDown) then begin
          if (FLastDown = sbTopDown) then FDown := sbTopDown
          else FDown := sbNotDown;
          if NewState <> FDown then Repaint;
        end;
      end;
    end else
      if FDown <> sbNotDown then begin
        FDown := sbNotDown;
        Repaint;
      end;
  end;
end;

procedure TJvSpinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then begin
    FDragging := False;
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then begin
      FDown := sbNotDown;
      FLastDown := sbNotDown;
      Repaint;
    end;
  end;
end;

procedure TJvSpinButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FDown <> sbNotDown) and MouseCapture then begin
    try
      if FDown = sbBottomDown then BottomClick else TopClick;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

function DefBtnWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
  if Result > 15 then Result := 15;
end;

{$IFDEF WIN32}

type
  TJvUpDown = class(TCustomUpDown)
  private
    FChanging: Boolean;
    procedure ScrollMessage(var Message: TWMVScroll);
    procedure WMHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
  end;

constructor TJvUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Orientation := udVertical;
  Min := -1;
  Max := 1;
  Position := 0;
end;

destructor TJvUpDown.Destroy;
begin
  OnClick := nil;
  inherited Destroy;
end;

procedure TJvUpDown.ScrollMessage(var Message: TWMVScroll);
begin
  if Message.ScrollCode = SB_THUMBPOSITION then begin
    if not FChanging then begin
      FChanging := True;
      try
        if Message.Pos > 0 then Click(btNext)
        else if Message.Pos < 0 then Click(btPrev);
        if HandleAllocated then
          SendMessage(Handle, UDM_SETPOS, 0, 0);
      finally
        FChanging := False;
      end;
    end;
  end;
end;

procedure TJvUpDown.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(TWMVScroll(Message));
end;

procedure TJvUpDown.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message);
end;

procedure TJvUpDown.WMSize(var Message: TWMSize);
begin
  inherited;
  if Width <> DefBtnWidth then Width := DefBtnWidth;
end;
{$ENDIF WIN32}

{ TJvxSpinEdit }

constructor TJvxSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1.0;
  FDecimal := 2;
  FEditorEnabled := True;
{$IFDEF WIN32}
  FButtonKind := bkDiagonal;
{$ENDIF}
  FArrowKeys := True;
  RecreateButton;
end;

destructor TJvxSpinEdit.Destroy;
begin
  Destroying;
  FChanging := True;
  if FButton <> nil then begin
    FButton.Free;
    FButton := nil;
    FBtnWindow.Free;
    FBtnWindow := nil;
  end;
{$IFDEF WIN32}
  if FUpDown <> nil then begin
    FUpDown.Free;
    FUpDown := nil;
  end;
{$ENDIF}
  inherited Destroy;
end;

procedure TJvxSpinEdit.RecreateButton;
begin
  if (csDestroying in ComponentState) then Exit;
  FButton.Free;
  FButton := nil;
  FBtnWindow.Free;
  FBtnWindow := nil;
{$IFDEF WIN32}
  FUpDown.Free;
  FUpDown := nil;
  if GetButtonKind = bkStandard then begin
    FUpDown := TJvUpDown.Create(Self);
    with TJvUpDown(FUpDown) do begin
      Visible := True;
      SetBounds(0, 0, DefBtnWidth, Self.Height);
{$IFDEF COMPILER4_UP}
      if (BiDiMode = bdRightToLeft) then Align := alLeft else
{$ENDIF}
      Align := alRight;
      Parent := Self;
      OnClick := UpDownClick;
    end;
  end
  else begin
{$ENDIF}
    FBtnWindow := TWinControl.Create(Self);
    FBtnWindow.Visible := True;
    FBtnWindow.Parent := Self;
    FBtnWindow.SetBounds(0, 0, Height, Height);
    FButton := TJvSpinButton.Create(Self);
    FButton.Visible := True;
    FButton.Parent := FBtnWindow;
    FButton.FocusControl := Self;
    FButton.OnTopClick := UpClick;
    FButton.OnBottomClick := DownClick;
    FButton.SetBounds(0, 0, FBtnWindow.Width, FBtnWindow.Height);
{$IFDEF WIN32}
  end;
{$ENDIF}
end;

procedure TJvxSpinEdit.SetArrowKeys(Value: Boolean);
begin
  FArrowKeys := Value;
{$IFDEF WIN32}
  ResizeButton;
{$ENDIF}
end;

{$IFDEF WIN32}
function TJvxSpinEdit.GetButtonKind: TSpinButtonKind;
begin
  if NewStyleControls then Result := FButtonKind
  else Result := bkDiagonal;
end;

procedure TJvxSpinEdit.SetButtonKind(Value: TSpinButtonKind);
var
  OldKind: TSpinButtonKind;
begin
  OldKind := FButtonKind;
  FButtonKind := Value;
  if OldKind <> GetButtonKind then begin
    RecreateButton;
    ResizeButton;
    SetEditRect;
  end;
end;

procedure TJvxSpinEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if TabStop and CanFocus then SetFocus;
  case Button of
    btNext: UpClick(Sender);
    btPrev: DownClick(Sender);
  end;
end;
{$ENDIF WIN32}

function TJvxSpinEdit.GetButtonWidth: Integer;
begin
{$IFDEF WIN32}
  if FUpDown <> nil then Result := FUpDown.Width else
{$ENDIF}
  if FButton <> nil then Result := FButton.Width
  else Result := DefBtnWidth;
end;

procedure TJvxSpinEdit.ResizeButton;
{$IFDEF WIN32}
var
  R: TRect;
{$ENDIF}
begin
{$IFDEF WIN32}
  if FUpDown <> nil then begin
    FUpDown.Width := DefBtnWidth;
 {$IFDEF COMPILER4_UP}
    if (BiDiMode = bdRightToLeft) then FUpDown.Align := alLeft else
 {$ENDIF}
    FUpDown.Align := alRight;
  end
  else if FButton <> nil then begin { bkDiagonal }
    if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
      R := Bounds(Width - Height - 1, -1, Height - 3, Height - 3)
    else
      R := Bounds(Width - Height, 0, Height, Height);
 {$IFDEF COMPILER4_UP}
    if (BiDiMode = bdRightToLeft) then begin
      if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then begin
        R.Left := -1;
        R.Right := Height - 4;
      end
      else begin
        R.Left := 0;
        R.Right := Height;
      end;
    end;
 {$ENDIF}
    with R do
      FBtnWindow.SetBounds(Left, Top, Right - Left, Bottom - Top);
    FButton.SetBounds(0, 0, FBtnWindow.Width, FBtnWindow.Height);
  end;
{$ELSE}
  if FButton <> nil then begin
    FBtnWindow.SetBounds(Width - Height, 0, Height, Height);
    FButton.SetBounds(0, 0, FBtnWindow.Width, FBtnWindow.Height);
  end;
{$ENDIF}
end;

procedure TJvxSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ArrowKeys and (Key in [VK_UP, VK_DOWN]) then begin
    if Key = VK_UP then UpClick(Self)
    else if Key = VK_DOWN then DownClick(Self);
    Key := 0;
  end;
end;

procedure TJvxSpinEdit.Change;
begin
  if not FChanging then inherited Change;
end;

procedure TJvxSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then begin
    inherited KeyPress(Key);
    if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then Key := #0;
    end;
  end;
end;

function TJvxSpinEdit.IsValidChar(Key: Char): Boolean;
var
  ValidChars: set of Char;
begin
  ValidChars := ['+', '-', '0'..'9'];
  if ValueType = vtFloat then begin
    if Pos(DecimalSeparator, Text) = 0 then
      ValidChars := ValidChars + [DecimalSeparator];
    if Pos('E', AnsiUpperCase(Text)) = 0 then
      ValidChars := ValidChars + ['e', 'E'];
  end
  else if ValueType = vtHex then begin
    ValidChars := ValidChars + ['A'..'F', 'a'..'f'];
  end;
  Result := (Key in ValidChars) or (Key < #32);
  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TJvxSpinEdit.CreateParams(var Params: TCreateParams);
const
{$IFDEF COMPILER4_UP}
  Alignments: array[Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER));
{$ELSE}
  Alignments: array[TAlignment] of Longint = (ES_LEFT, ES_RIGHT, ES_CENTER);
{$ENDIF}
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or
{$IFDEF COMPILER4_UP}
    Alignments[UseRightToLeftAlignment, FAlignment];
{$ELSE}
    Alignments[FAlignment];
{$ENDIF}
end;

procedure TJvxSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TJvxSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
{$IFDEF COMPILER4_UP}
  if (BiDiMode = bdRightToLeft) then
    SetRect(Loc, GetButtonWidth + 1, 0, ClientWidth - 1,
      ClientHeight + 1) else
{$ENDIF COMPILER4_UP}
  SetRect(Loc, 0, 0, ClientWidth - GetButtonWidth - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Loc));
end;

procedure TJvxSpinEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvxSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else begin
    ResizeButton;
    SetEditRect;
  end;
end;

procedure TJvxSpinEdit.GetTextHeight(var SysHeight, Height: Integer);
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  SysHeight := SysMetrics.tmHeight;
  Height := Metrics.tmHeight;
end;

function TJvxSpinEdit.GetMinHeight: Integer;
var
  I, H: Integer;
begin
  GetTextHeight(I, H);
  if I > H then I := H;
  Result := H + {$IFNDEF WIN32} (I div 4) + {$ENDIF}
    (GetSystemMetrics(SM_CYBORDER) * 4) + 1;
end;

procedure TJvxSpinEdit.UpClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then MessageBeep(0)
  else begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value + FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnTopClick) then FOnTopClick(Self);
  end;
end;

procedure TJvxSpinEdit.DownClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then MessageBeep(0)
  else begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value - FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnBottomClick) then FOnBottomClick(Self);
  end;
end;

{$IFDEF COMPILER4_UP}
procedure TJvxSpinEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;
{$ENDIF}

procedure TJvxSpinEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TJvxSpinEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TJvxSpinEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
{$IFDEF WIN32}
  if FUpDown <> nil then begin
    FUpDown.Enabled := Enabled;
    ResizeButton;
  end;
{$ENDIF}
  if FButton <> nil then FButton.Enabled := Enabled;
end;

procedure TJvxSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TJvxSpinEdit.WMCut(var Message: TWMCut);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TJvxSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue(Value) <> Value then SetValue(Value);
end;

procedure TJvxSpinEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

function TJvxSpinEdit.GetValue: Extended;
begin
  try
    if ValueType = vtFloat then Result := StrToFloat(Text)
    else if ValueType = vtHex then Result := StrToInt('$' + Text)
    else Result := StrToInt(Text);
  except
    if ValueType = vtFloat then Result := FMinValue
    else Result := Trunc(FMinValue);
  end;
end;

procedure TJvxSpinEdit.SetValue(NewValue: Extended);
begin
  if ValueType = vtFloat then
    Text := FloatToStrF(CheckValue(NewValue), ffFixed, 15, FDecimal)
  else if ValueType = vtHex then
    Text := IntToHex(Round(CheckValue(NewValue)), 1)
  else
    Text := IntToStr(Round(CheckValue(NewValue)));
end;

function TJvxSpinEdit.GetAsInteger: Longint;
begin
  Result := Trunc(GetValue);
end;

procedure TJvxSpinEdit.SetAsInteger(NewValue: Longint);
begin
  SetValue(NewValue);
end;

procedure TJvxSpinEdit.SetValueType(NewType: TValueType);
begin
  if FValueType <> NewType then begin
    FValueType := NewType;
    Value := GetValue;
    if FValueType in [{$IFDEF CBUILDER} vtInt {$ELSE} vtInteger {$ENDIF}, vtHex] then
    begin
      FIncrement := Round(FIncrement);
      if FIncrement = 0 then FIncrement := 1;
    end;
  end;
end;

function TJvxSpinEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

function TJvxSpinEdit.IsMaxStored: Boolean;
begin
  Result := (MaxValue <> 0.0);
end;

function TJvxSpinEdit.IsMinStored: Boolean;
begin
  Result := (MinValue <> 0.0);
end;

function TJvxSpinEdit.IsValueStored: Boolean;
begin
  Result := (GetValue <> 0.0);
end;

procedure TJvxSpinEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then begin
    FDecimal := NewValue;
    Value := GetValue;
  end;
end;

function TJvxSpinEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

end.
