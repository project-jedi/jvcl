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

{$I jvcl.inc}

unit JvMaskEdit;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFDEF VCL}
  Graphics, Controls, Mask, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QMask, QForms, QWindows,
  {$ENDIF VisualCLX}
  JvComponent, JvTypes, JVCLVer, JvCaret, JvToolEdit, JvExMask;

type
  TJvCustomMaskEdit = class(TJvExCustomMaskEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnEnabledChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    {$IFDEF VCL}
    FOnSetFocus: TJvFocusChangeEvent;
    FOnKillFocus: TJvFocusChangeEvent;
    {$ENDIF VCL}
    FSaved: TColor;
    FHintColor: TColor;
    FOver: Boolean;
    FHotTrack: Boolean;
    FCaret: TJvCaret;
    FEntering: Boolean;
    FLeaving: Boolean;
    FClipboardCommands: TJvClipboardCommands;
    FGroupIndex: Integer;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FProtectPassword: Boolean;
    FLastNotifiedText: String;
    procedure SetHotTrack(Value: Boolean);
    procedure UpdateEdit;
    function GetPasswordChar: Char;
    function GetText: string;
    procedure SetPasswordChar(const Value: Char);
    procedure SetText(const Value: string);
  protected
    procedure CaretChanged(Sender: TObject); dynamic;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    {$IFDEF VCL}
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;
    procedure DoKillFocus(const ANextControl: TWinControl); virtual;
    procedure DoSetFocus(const APreviousControl: TWinControl); virtual;
    {$ENDIF VCL}
    procedure EnabledChanged; override;
    procedure MouseEnter(Control :TControl); override;
    procedure MouseLeave(Control :TControl); override;
    procedure ParentColorChanged; override;
    {$IFDEF VisualCLX}
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    {$ENDIF VisualCLX}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetCaret(const Value: TJvCaret);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands);
    procedure SetGroupIndex(const Value: Integer);
    procedure NotifyIfChanged;
    procedure Change; override;
  public
    {$IFDEF VCL}
    procedure DefaultHandler(var Msg); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    procedure PasteFromClipboard; override;
    {$ENDIF VisualCLX}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Entering: Boolean read FEntering;
    property Leaving: Boolean read FLeaving;
  protected
    property Text: string read GetText write SetText;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property Caret: TJvCaret read FCaret write SetCaret;

    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands
      write SetClipboardCommands default [caCopy..caUndo];
    property DisabledTextColor: TColor read FDisabledTextColor write
      SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default -1;

    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    {$IFDEF VCL}
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
    {$ENDIF VCL}
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
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF VCL}
    property OnSetFocus;
    property OnKillFocus;
    property OnStartDock;
    property OnEndDock;
    {$ENDIF VCL}
    property OnStartDrag;
  end;

implementation

constructor TJvCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  FHintColor := clInfoBk;
  FOver := False;
  FCaret := TJvCaret.Create(Self);
  FCaret.OnChanged := CaretChanged;
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FClipboardCommands := [caCopy..caUndo];
  FGroupIndex := -1;
  FEntering := False;
  FLeaving := False;
end;

destructor TJvCustomMaskEdit.Destroy;
begin
  FCaret.OnChanged := nil;
  FreeAndNil(FCaret);
  inherited Destroy;
end;

procedure TJvCustomMaskEdit.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomMaskEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
  if Assigned(FOnEnabledChanged) then
    FOnEnabledChanged(Self);
end;

procedure TJvCustomMaskEdit.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if HotTrack then
      {$IFDEF VCL}
      Ctl3D := True;
      {$ELSE}
      BorderStyle := bsSunken3d;
      {$ENDIF VCL}
    FOver := True;
  end;
  inherited MouseEnter(Control);
end;

procedure TJvCustomMaskEdit.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      {$IFDEF VCL}
      Ctl3D := False;
      {$ELSE}
      BorderStyle := bsSingle; // maybe bsNone
      {$ENDIF VCL}
    FOver := False;
  end;
  inherited MouseLeave(Control);
end;

procedure TJvCustomMaskEdit.SetHotTrack(Value: Boolean);
begin
  FHotTrack := Value;
  if Value then
    {$IFDEF VCL}
    Ctl3D := False;
    {$ELSE}
    BorderStyle := bsSingle; // maybe bsNone
    {$ENDIF VCL}
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
      if (Owner.Components[i] is TJvCustomMaskEdit) then
        with TJvCustomMaskEdit(Owner.Components[i]) do
          if (Name <> Self.Name) and (GroupIndex <> -1) and
            (GroupIndex = Self.GroupIndex) then
            Clear;
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

{$IFDEF VCL}
procedure TJvCustomMaskEdit.WMCopy(var Msg: TWMCopy);
begin
  if caCopy in ClipboardCommands then
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomMaskEdit.CopyToClipboard;
begin
  if caCopy in ClipboardCommands then
    inherited CopyToClipboard;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvCustomMaskEdit.WMCut(var Msg: TWMCut);
begin
  if caCut in ClipboardCommands then
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomMaskEdit.CutToClipboard;
begin
  if caCut in ClipboardCommands then
    inherited CutToClipboard;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvCustomMaskEdit.WMPaste(var Msg: TWMPaste);
begin
  if caPaste in ClipboardCommands then
    inherited;
  UpdateEdit;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomMaskEdit.PasteFromClipboard;
begin
  if caPaste in ClipboardCommands then
    inherited PasteFromClipboard;
  UpdateEdit;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}

procedure TJvCustomMaskEdit.WMUndo(var Msg: TWMUndo);
begin
  if caUndo in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMaskEdit.WMPaint(var Msg: TWMPaint);
const
  AlignmentValues: array [False..True, TAlignment] of TAlignment =
    ((taLeftJustify, taRightJustify, taCenter),
     (taRightJustify, taLeftJustify, taCenter));
var
  Canvas: TControlCanvas;
  Style: Integer;
  AAlignment: TAlignment;
  //R: TRect;
  //ButtonWidth: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    inherited
  else
  begin
    { (rb) Alignment switching is already in PaintEdit ?? }
    { (rb) implementation VCL <> VisualCLX }
    Style := GetWindowLong(Handle, GWL_STYLE);
    if (Style and ES_RIGHT) <> 0 then
      AAlignment := AlignmentValues[UseRightToLeftAlignment, taRightJustify]
    else
    if (Style and ES_CENTER) <> 0 then
      AAlignment := taCenter
    else
      AAlignment := AlignmentValues[UseRightToLeftAlignment, taLeftJustify];

    {SendMessage(Handle, EM_GETRECT, 0, Integer(@R));
    if BiDiMode = bdRightToLeft then
      ButtonWidth := R.Left - 1
    else
      ButtonWidth := ClientWidth - R.Right - 2;
    if ButtonWidth < 0 then ButtonWidth := 0;}

    Canvas := nil;
    if not PaintEdit(Self, Text, AAlignment, False, {ButtonWidth,}
       FDisabledTextColor, Focused, Canvas, Msg) then
      inherited;
    Canvas.Free;
  end;
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomMaskEdit.Paint;
begin
  with Canvas do
  begin
   // Paint
    if Enabled then
      inherited Paint
    else
    begin
      ACanvas := nil;
      if not PaintEdit(Self, Text, taLeftJustify, False, {0,}
         FDisabledTextColor, Focused, ACanvas) then
        inherited Paint;
      ACanvas.Free;
    end;
  end;
end;
{$ENDIF VisualCLX}

function TJvCustomMaskEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := False;
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    Result := inherited DoPaintBackground(Canvas, Param)
  else
  begin
    SaveDC(Canvas.Handle);
    try
      Canvas.Brush.Color := FDisabledColor;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(ClientRect);
      Result := True;
    finally
      RestoreDC(Canvas.Handle, -1);
    end;
  end;
end;

{$IFDEF VCL}
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
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomMaskEdit.DoEnter;
begin
  FEntering := True;
  try
    inherited DoEnter;
    FCaret.CreateCaret;
  finally
    FEntering := False;
  end;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
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
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomMaskEdit.DoExit;
begin
  FLeaving := True;
  try
    FCaret.DestroyCaret;
    inherited DoExit;
  finally
    FLeaving := False;
  end;
end;
{$ENDIF VisualCLX}

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

{$IFDEF VCL}

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
  NotifyIfChanged;
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Self, ANextControl);
end;

procedure TJvCustomMaskEdit.DoSetFocus(const APreviousControl: TWinControl);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, APreviousControl);
end;

{$ENDIF VCL}

procedure TJvCustomMaskEdit.Change;
begin
  FLastNotifiedText := Text;
  inherited Change;
end;

procedure TJvCustomMaskEdit.NotifyIfChanged;
begin
  if FLastNotifiedText <> Text then
  begin
    FLastNotifiedText := Text;
    inherited Change;
  end;
end;

end.

