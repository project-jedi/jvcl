{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNewEdit.PAS, released on 2002-mm-dd.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TJvEdit, TJvExEdit, TJvCaretEdit,TJvAlignedEdit,
TJvSingleLineMemo.
Merging done 2002-06-05 by Peter Thornqvist [peter3@peter3.com]

  MERGE NOTES:
    * TjvCustomEdit has been removed from JvComponent and put here instead.
    * The HotTrack property only works if BorderStyle := bsSingle and BevekKind := bvNone
    * Added ClipboardCommands

Contributor(s):
  Anthony Steele [asteele@iafrica.com]
  Peter Below [100113.1101@compuserve.com]

Last Modified: 2002-06-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvEdit;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Dialogs, Forms, JvComponent,
  JvPropAutoSave, JvMaxPixel, JVCLVer;

type
  TJvCustomEdit = class(TCustomEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FAlignment: TAlignment;
    FHotTrack: Boolean;
    FOver: Boolean;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FAutoSave: TJvAutoSave;
    FMaxPixel: TJvMaxPixel;
    FCaret: TJvCaret;
    FClipBoardCommands: TJvClipboardCommands;
    procedure SetCaret(const Value: TJvCaret);
    procedure CaretChanged(sender: TObject); dynamic;
    procedure WMSetFocus(var msg: TMessage); message WM_SETFOCUS;
    procedure WMPaint(var msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetHotTrack(const Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    procedure SetClipBoardCommands(const Value: TJvClipboardCommands);
  protected
    procedure Change; override;
    procedure MaxPixelChanged(Sender: TObject);
  public
    function IsEmpty: Boolean;
    procedure Loaded; override;
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoSave: TJvAutoSave read FAutoSave write FAutoSave;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caret: TJvCaret read FCaret write SetCaret;
    property ClipBoardCommands: TJvClipboardCommands read FClipBoardCommands write SetClipBoardCommands default [caCopy..caUndo];
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;

    property HintColor: TColor read FColor write FColor default clInfoBk;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
  end;

  TJvEdit = class(TJvCustomEdit)
  published
    property AboutJVCL;
    property Align;
    property AutoSave;
    property Alignment;
{$IFDEF COMPILER6_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
{$ENDIF}
    property Caret;
    property ClipBoardCommands;
    property DisabledTextColor;
    property DisabledColor;
    property HintColor;
    property HotTrack;
    property MaxPixel;
    property Modified;
    property SelStart;
    property SelText;
    property SelLength;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
    property OnRestored;

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
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
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
    property OnContextPopup;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

  end;

implementation

{ TJvCustomEdit }

constructor TJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // (p3) this is a hack to avoid (Control "" has no parent window) - fixed by checking for Parent in Change
//  Parent := TWinControl(AOwner);
  FColor := clInfoBk;
  FHotTrack := False;
  FOver := False;
  FAlignment := taLeftJustify;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FClipBoardCommands := [caCopy..caUndo];
  FCaret := TJvCaret.Create(self);
  FCaret.OnChanged := CaretChanged;
  FAutoSave := TJvAutoSave.Create(Self);
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
end;

procedure TJvCustomEdit.Loaded;
var
  st: string;
begin
  inherited;
  if FAutoSave.LoadValue(st) then
  begin
    Text := st;
    if Assigned(FOnRestored) then
      FOnRestored(Self);
  end;
end;

destructor TJvCustomEdit.Destroy;
begin
  FAutoSave.Free;
  FMaxPixel.Free;
  FCaret.Free;
  inherited;
end;

procedure TJvCustomEdit.Change;
var
  st: string;
begin
  inherited;
  if not HasParent then
    Exit;
  st := Text;
  FMaxPixel.Test(st, Font);
  if st <> Text then
    Text := st;
  SelStart := Length(Text);
  FAutoSave.SaveValue(Text);
end;

procedure TJvCustomEdit.CreateParams(var Params: TCreateParams);
const
  Styles: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Styles[FAlignment];
  if (FAlignment <> taLeftJustify) and (Win32Platform = VER_PLATFORM_WIN32_WINDOWS)
    and (Win32MajorVersion = 4) and (Win32MinorVersion = 0) then
    Params.Style := Params.Style or ES_MULTILINE; // needed for Win95
end;

procedure TJvCustomEdit.CMMouseEnter(var Msg: TMessage);
var i, j: integer;
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      i := SelStart;
      j := SelLength;
      Ctl3d := True;
      SelStart := i;
      SelLength := j;
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomEdit.CMMouseLeave(var Msg: TMessage);
var i, j: integer;
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
    begin
      i := SelStart;
      j := SelLength;
      Ctl3d := False;
      SelStart := i;
      SelLength := j;
    end;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomEdit.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomEdit.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Ctl3d := not FHotTrack;
end;

function TJvCustomEdit.IsEmpty: Boolean;
begin
  Result := Length(Caption) = 0;
end;

procedure TJvCustomEdit.SetAlignment(Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;

procedure TJvCustomEdit.MaxPixelChanged(Sender: TObject);
var
  st: string;
begin
  st := Text;
  FMaxPixel.Test(st, Font);
  if st <> Text then
    Text := st;
  SelStart := Length(Text);
end;

procedure TJvCustomEdit.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TJvCustomEdit.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TJvCustomEdit.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  canvas: TCanvas;
begin
  if Enabled then
    inherited
  else
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := msg.DC;
      SaveDC(msg.DC);
      try
        Canvas.Brush.Color := FDisabledColor;
        Canvas.Brush.Style := bsSolid;
        Canvas.Fillrect(clientrect);
        Msg.result := 1;
      finally
        RestoreDC(msg.DC, -1);
      end;
    finally
      canvas.free
    end;
  end; { Else }
end;

procedure TJvCustomEdit.WMPaint(var msg: TWMPaint);
var
  canvas: TCanvas;
  ps: TPaintStruct;
  callEndPaint: Boolean;
begin
  if Enabled then
    inherited
  else
  begin
    callEndPaint := False;
    canvas := TCanvas.Create;
    try
      if msg.DC <> 0 then
      begin
        canvas.Handle := msg.DC;
        ps.fErase := true;
      end
      else
      begin
        BeginPaint(handle, ps);
        callEndPaint := true;
        canvas.handle := ps.hdc;
      end;

      if ps.fErase then
        Perform(WM_ERASEBKGND, canvas.handle, 0);

      SaveDC(canvas.handle);
      try
        canvas.Brush.Style := bsClear;
        canvas.Font := Font;
        canvas.Font.Color := FDisabledTextColor;
        canvas.TextOut(1, 1, Text);
      finally
        RestoreDC(canvas.handle, -1);
      end;
    finally
      if callEndPaint then
        EndPaint(handle, ps);
      canvas.free
    end;
  end;
end;

procedure TJvCustomEdit.CaretChanged(sender: TObject);
begin
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomEdit.WMSetFocus(var msg: TMessage);
begin
  inherited;
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomEdit.WMCopy(var Msg: TWMCopy);
begin
  if caCopy in ClipBoardCommands then
    inherited;
end;

procedure TJvCustomEdit.WMCut(var Msg: TWMCut);
begin
  if caCut in ClipBoardCommands then
    inherited;
end;

procedure TJvCustomEdit.WMPaste(var Msg: TWMPaste);
begin
  if caPaste in ClipBoardCommands then
    inherited;
end;

procedure TJvCustomEdit.WMUndo(var Msg: TWMUndo);
begin
  if caUndo in ClipBoardCommands then
    inherited;
end;

function TJvCustomEdit.GetReadOnly: boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TJvCustomEdit.SetReadOnly(const Value: boolean);
begin
  inherited ReadOnly := Value;
  if Value then
    FClipBoardCommands := [caCopy];
end;

procedure TJvCustomEdit.SetClipBoardCommands(
  const Value: TJvClipboardCommands);
begin
  if FClipBoardCommands <> Value then
  begin
    FClipBoardCommands := Value;
    ReadOnly := FClipBoardCommands <= [caCopy];
  end;
end;

end.

