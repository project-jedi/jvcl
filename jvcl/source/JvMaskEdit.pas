{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMaskEdit.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com],
                Rob den Braasem [rbraasem@xs4all.nl],
                Oliver Giesen [ogware@gmx.net],
                Peter Thornqvist [peter3@peter3.com].

Last Modified: 2002-12-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMaskEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Mask, Forms,
  JvCaret, JvComponent, JvTypes, JVCLVer, JvToolEdit;

type
  TJvCustomMaskEdit = class(TCustomMaskEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnEnabledChanged: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnSetFocus: TJvFocusChangeEvent;
    FOnKillFocus: TJvFocusChangeEvent;
    FSaved: TColor;
    FHintColor: TColor;
    FOver: Boolean;
    FEffect: Boolean;
    FCaret: TJvCaret;
    FEntering: Boolean;
    FLeaving: Boolean;
    (* ++ RDB ++ *)
    FClipboardCommands: TJvClipboardCommands;
    FGroupIndex: Integer;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FProtectPassword: Boolean;
    (* -- RDB -- *)
    procedure SetCtl3D(Value: Boolean);
    (* ++ RDB ++ *)
    procedure UpdateEdit;
    function GetPasswordChar: Char;
    function GetText: string;
    procedure SetPasswordChar(const Value: Char);
    procedure SetText(const Value: string);
    (* -- RDB -- *)
  protected
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CaretChanged(sender: TObject); dynamic;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    (* ++ RDB ++ *)
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;
    (* -- RDB -- *)
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoCtl3DChanged; virtual;
    procedure DoEnabledChanged; virtual;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure DoParentColorChanged; dynamic;
    procedure DoKillFocus(const ANextControl: TWinControl); virtual;
    procedure DoSetFocus(const APreviousControl: TWinControl); virtual;
    procedure SetCaret(const Value: TJvCaret);
    (* ++ RDB ++ *)
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands);
    procedure SetGroupIndex(const Value: Integer);
    (* -- RDB -- *)
  public
    procedure DefaultHandler(var Msg); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Entering: Boolean read FEntering;
    property Leaving: Boolean read FLeaving;
  protected
    property Text: string read GetText write SetText;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property HotTrack: Boolean read FEffect write SetCtl3D default False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property Caret: TJvCaret read FCaret write SetCaret;
    (* ++ RDB ++ *)
    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands
      write SetClipboardCommands default [caCopy..caUndo];
    property DisabledTextColor: TColor read FDisabledTextColor write
      SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor
      default clWindow;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    (* -- RDB -- *)
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write
      FOnParentColorChanged;
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvMaskEdit = class(TJvCustomMaskEdit)
  published
    property AboutJVCL;
    property Caret;
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    property GroupIndex;
    property HintColor;
    property HotTrack;
    property ProtectPassword;
    property OnCtl3DChanged;
    property OnEnabledChanged;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnKillFocus;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSetFocus;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

procedure TJvCustomMaskEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  DoCtl3DChanged;
end;

procedure TJvCustomMaskEdit.DoCtl3DChanged;
begin
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomMaskEdit.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  DoParentColorChanged;
end;

procedure TJvCustomMaskEdit.DoParentColorChanged;
begin
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

constructor TJvCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEffect := False;
  FHintColor := clInfoBk;
  FOver := False;
  FCaret := TJvCaret.Create(Self);
  FCaret.OnChanged := CaretChanged;
  ControlStyle := ControlStyle + [csAcceptsControls];
  (* ++ RDB ++ *)
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FClipboardCommands := [caCopy..caUndo];
  FGroupIndex := -1;
  (* -- RDB -- *)
  FEntering := False;
  FLeaving := False;
end;

destructor TJvCustomMaskEdit.Destroy;
begin
  FCaret.OnChanged := nil;
  FreeAndNil(FCaret);
  inherited Destroy;
end;

procedure TJvCustomMaskEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
  DoEnabledChanged;
end;

procedure TJvCustomMaskEdit.DoEnabledChanged;
begin
  if Assigned(FOnEnabledChanged) then
    FOnEnabledChanged(Self);
end;

procedure TJvCustomMaskEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    if HotTrack then
      Ctl3D := True;
    FOver := True;
  end;
  DoMouseEnter;
end;

procedure TJvCustomMaskEdit.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomMaskEdit.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FEffect then
      Ctl3D := False;
    FOver := False;
  end;
  inherited;
  DoMouseLeave;
end;

procedure TJvCustomMaskEdit.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomMaskEdit.SetCtl3D(Value: Boolean);
begin
  FEffect := Value;
  if Value then
    Ctl3D := False;
end;

procedure TJvCustomMaskEdit.CaretChanged(Sender: TObject);
begin
  FCaret.CreateCaret;
end;

procedure TJvCustomMaskEdit.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomMaskEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  if FClipboardCommands <> Value then
  begin
    FClipboardCommands := Value;
    ReadOnly := FClipboardCommands <= [caCopy];
  end;
end;

procedure TJvCustomMaskEdit.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateEdit;
end;

procedure TJvCustomMaskEdit.UpdateEdit;
var
  I: Integer;
begin
  if Assigned(Owner) then
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      if (Owner.Components[i] is TJvCustomMaskEdit) then
        with TJvCustomMaskEdit(Owner.Components[i]) do
        begin
          if (Name <> Self.Name) and (GroupIndex <> -1) and
          (GroupIndex = Self.GroupIndex) then
            Clear;
        end;
    end;
end;

procedure TJvCustomMaskEdit.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TJvCustomMaskEdit.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TJvCustomMaskEdit.WMCopy(var Msg: TWMCopy);
begin
  if caCopy in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMaskEdit.WMCut(var Msg: TWMCut);
begin
  if caCut in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMaskEdit.WMPaste(var Msg: TWMPaste);
begin
  if caPaste in ClipboardCommands then
    inherited;
  UpdateEdit;
end;

procedure TJvCustomMaskEdit.WMUndo(var Msg: TWMUndo);
begin
  if caUndo in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMaskEdit.WMPaint(var Msg: TWMPaint);
const
  AlignmentValues: array[False..True, TAlignment] of TAlignment = (
    (taLeftJustify, taRightJustify, taCenter),
    (taRightJustify, taLeftJustify, taCenter)
  );
{$IFNDEF COMPILER4UP}
  UseRightToLeftAlignment = False;
{$ENDIF}
var
  Canvas: TControlCanvas;
  Style: Integer;
  AAlignment: TAlignment;
  R: TRect;
  ButtonWidth: Integer;
begin
  if Enabled then
    inherited
  else
  begin
    Style := GetWindowLong(Handle, GWL_STYLE);
    if (Style and ES_RIGHT) <> 0 then
      AAlignment := AlignmentValues[UseRightToLeftAlignment, taRightJustify]
    else if (Style and ES_CENTER) <> 0 then
      AAlignment := taCenter
    else
      AAlignment := AlignmentValues[UseRightToLeftAlignment, taLeftJustify];

    SendMessage(Handle, EM_GETRECT, 0, Integer(@R));
    {$IFDEF COMPILER4_UP}
    if BiDiMode = bdRightToLeft then
      ButtonWidth := R.Left - 1
    else
    {$ENDIF COMPILER4_UP}
      ButtonWidth := ClientWidth - R.Right - 2;
    if ButtonWidth < 0 then ButtonWidth := 0;

    Canvas := nil;
    if not PaintEdit(Self, Text, AAlignment, False, ButtonWidth, FDisabledTextColor,
       Focused, Canvas, Msg) then
      inherited;
    Canvas.Free;
  end;
end;

procedure TJvCustomMaskEdit.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  Canvas: TCanvas;
begin
  if Enabled then
    inherited
  else
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := Msg.DC;
      SaveDC(Msg.DC);
      try
        Canvas.Brush.Color := FDisabledColor;
        Canvas.Brush.Style := bsSolid;
        Canvas.Fillrect(clientrect);
        Msg.Result := 1;
      finally
        RestoreDC(Msg.DC, -1);
      end;
    finally
      Canvas.Free;
    end;
  end;
end;

procedure TJvCustomMaskEdit.WMSetFocus(var Msg: TMessage);
begin
  FEntering := True;
  try
    inherited;
    FCaret.CreateCaret;
    DoSetFocus(FindControl(Msg.WParam));
  finally
    FEntering := False;
  end;
end;

procedure TJvCustomMaskEdit.WMKillFocus(var Msg: TMessage);
begin
  FLeaving := True;
  try
    FCaret.DestroyCaret;
    inherited;
    DoKillFocus(FindControl(Msg.WParam));
  finally
    FLeaving := False;
  end;
end;

procedure TJvCustomMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  UpdateEdit;
  inherited KeyDown(Key, Shift);
end;

function TJvCustomMaskEdit.GetPasswordChar: Char;
begin
  Result := inherited PasswordChar;
end;

function TJvCustomMaskEdit.GetText: string;
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    Result := inherited Text;
  finally
    ProtectPassword := Tmp;
  end;
end;

procedure TJvCustomMaskEdit.SetPasswordChar(const Value: Char);
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    inherited PasswordChar := Value;
  finally
    ProtectPassword := Tmp;
  end;
end;

procedure TJvCustomMaskEdit.SetText(const Value: string);
begin
  inherited Text := Value;
end;

procedure TJvCustomMaskEdit.DefaultHandler(var Msg);
begin
  case TMessage(Msg).Msg of
    WM_CUT, WM_PASTE, EM_SETPASSWORDCHAR, WM_GETTEXT, WM_GETTEXTLENGTH:
      if not ProtectPassword then
        inherited DefaultHandler(Msg);
  else
    inherited DefaultHandler(Msg);
  end;
end;

procedure TJvCustomMaskEdit.DoKillFocus(const ANextControl: TWinControl);
begin
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Self, ANextControl);
end;

procedure TJvCustomMaskEdit.DoSetFocus(
  const APreviousControl: TWinControl);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, APreviousControl);
end;

end.

