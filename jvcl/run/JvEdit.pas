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
    * The HotTrack property only works if BorderStyle := bsSingle and BevelKind := bvNone
    * Added ClipboardCommands

Contributor(s):
  Anthony Steele [asteele@iafrica.com]
  Peter Below [100113.1101@compuserve.com]
  Rob den Braasem [rbraasem@xs4all.nl] (GroupIndex property - using several TJvEdits with the same GroupIndex
    will clear the text from the other edits when something is typed into one of them.
    To disable GroupIndex, set it to -1)

Last Modified: 2002-11-18

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  JvCaret, JvComponent, JvPropAutoSave, JvMaxPixel, JVCLVer, JvToolEdit;

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
    FClipboardCommands: TJvClipboardCommands;
    FOldCommands: TJvClipboardCommands;
    FGroupIndex: Integer;
    FProtectPassword: Boolean;
    FStreamedSelLength: Integer;
    FStreamedSelStart: Integer;
    procedure SetCaret(const Value: TJvCaret);
    procedure CaretChanged(Sender: TObject); dynamic;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
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
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure UpdateEdit;

    procedure SetClipboardCommands(const Value: TJvClipboardCommands);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetPasswordChar(Value: Char);
    function GetPasswordChar: Char;
  protected
    procedure Change; override;
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TSHiftState);override;
  public
    procedure DefaultHandler(var Msg); override;
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
    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands write SetClipboardCommands default
      [caCopy..caUndo];
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;
    property Text: string read GetText write SetText;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;

    property HintColor: TColor read FColor write FColor default clInfoBk;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
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
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    property ProtectPassword;
    property HintColor;
    property HotTrack;
    property GroupIndex;
    property MaxPixel;
    property Modified;
    // property SelStart; (p3) why published?
    //property SelText;
    // property SelLength; (p3) why published?
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

uses
  Math;

constructor TJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FHotTrack := False;
  FOver := False;
  FAlignment := taLeftJustify;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  ClipboardCommands := [caCopy..caUndo];
  FCaret := TJvCaret.Create(Self);
  FCaret.OnChanged := CaretChanged;
  FAutoSave := TJvAutoSave.Create(Self);
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FGroupIndex := -1;
  FStreamedSelLength := 0;
  FStreamedSelStart := 0;
end;

destructor TJvCustomEdit.Destroy;
begin
  FAutoSave.Free;
  FMaxPixel.Free;
  FCaret.Free;
  inherited Destroy;
end;

procedure TJvCustomEdit.Loaded;
var
  St: string;
begin
  inherited Loaded;
  if FAutoSave.LoadValue(St) then
  begin
    Text := St;
    if Assigned(FOnRestored) then
      FOnRestored(Self);
  end;
  SelStart := FStreamedSelStart;
  SelLength := FStreamedSelLength;
end;

procedure TJvCustomEdit.Change;
var
  St: string;
begin
  inherited Change;
  if not HasParent then
    Exit;
  St := Text;
  FMaxPixel.Test(St, Font);
  if St <> Text then
  begin
    Text := St;
    SelStart := Min(SelStart, Length(Text));
  end;
  FAutoSave.SaveValue(Text);
end;

procedure TJvCustomEdit.CreateParams(var Params: TCreateParams);
const
  Styles: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Styles[FAlignment];
  if (FAlignment <> taLeftJustify) and (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
    (Win32MajorVersion = 4) and (Win32MinorVersion = 0) then
    Params.Style := Params.Style or ES_MULTILINE; // needed for Win95
end;

procedure TJvCustomEdit.CMMouseEnter(var Msg: TMessage);
var
  I, J: Integer;
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Ctl3D := True;
      SelStart := I;
      SelLength := J;
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomEdit.CMMouseLeave(var Msg: TMessage);
var
  I, J: Integer;
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Ctl3D := False;
      SelStart := I;
      SelLength := J;
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
  Ctl3D := not FHotTrack;
end;

function TJvCustomEdit.IsEmpty: Boolean;
begin
  Result := (Length(Text) = 0);
end;

procedure TJvCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;

procedure TJvCustomEdit.MaxPixelChanged(Sender: TObject);
var
  St: string;
begin
  St := Text;
  FMaxPixel.Test(St, Font);
  if St <> Text then
  begin
    Text := St;
    SelStart := Min(SelStart, Length(Text));
  end;
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

procedure TJvCustomEdit.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
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
        Canvas.FillRect(ClientRect);
        Msg.Result := 1;
      finally
        RestoreDC(Msg.DC, -1);
      end;
    finally
      Canvas.Free
    end;
  end;
end;

function StrFillChar(Ch: Char; Length: Cardinal): string;
begin
  SetLength(Result, Length);
  if Length > 0 then
    FillChar(Result[1], Length, Ch);
end;

procedure TJvCustomEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TControlCanvas;
  S: string;
begin
  if Enabled then
    inherited
  else
  begin
    if PasswordChar = #0 then
      S := Text
    else
      S := StrFillChar(PasswordChar, Length(Text));
    Canvas := nil;
    if not PaintEdit(Self, S, FAlignment, False, 0, FDisabledTextColor,
       Focused, Canvas, Msg) then
      inherited;
    Canvas.Free;
  end;
end;

procedure TJvCustomEdit.CaretChanged(Sender: TObject);
begin
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.WMKillFocus(var Msg: TMessage);
begin
  FCaret.DestroyCaret;
  inherited;
end;

procedure TJvCustomEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomEdit.WMCopy(var Msg: TWMCopy);
begin
  if caCopy in ClipboardCommands then
    inherited;
end;

procedure TJvCustomEdit.WMCut(var Msg: TWMCut);
begin
  if caCut in ClipboardCommands then
    inherited;
end;

procedure TJvCustomEdit.WMPaste(var Msg: TWMPaste);
begin
  if caPaste in ClipboardCommands then
    inherited;
  UpdateEdit;
end;

procedure TJvCustomEdit.WMUndo(var Msg: TWMUndo);
begin
  if caUndo in ClipboardCommands then
    inherited;
end;

function TJvCustomEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TJvCustomEdit.SetReadOnly(const Value: Boolean);
begin
  inherited ReadOnly := Value;
  if Value then
  begin
    if caCopy in FClipboardCommands then
      FClipboardCommands := [caCopy]
    else
      FClipboardCommands := [];
  end
  else
    FClipboardCommands := FOldCommands;
end;

procedure TJvCustomEdit.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateEdit;
end;

procedure TJvCustomEdit.UpdateEdit;
var
  I: Integer;
begin
  for I := 0 to Self.Owner.ComponentCount - 1 do
    if Self.Owner.Components[I] is TJvCustomEdit then
      if ((Self.Owner.Components[I].Name <> Self.Name) and
        ((Self.Owner.Components[I] as TJvCustomEdit).GroupIndex <> -1) and
        ((Self.Owner.Components[I] as TJvCustomEdit).fGroupIndex = Self.FGroupIndex)) then
        (Self.Owner.Components[I] as TJvCustomEdit).Caption := '';
end;

procedure TJvCustomEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  if FClipboardCommands <> Value then
  begin
    FClipboardCommands := Value;
    FOldCommands := Value;
  end;
end;

procedure TJvCustomEdit.SetText(const Value: string);
begin
  inherited Text := Value;
end;

function TJvCustomEdit.GetText: string;
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

procedure TJvCustomEdit.SetPasswordChar(Value: Char);
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    if HandleAllocated then
      inherited PasswordChar := Char(SendMessage(Handle, EM_GETPASSWORDCHAR, 0, 0));
    inherited PasswordChar := Value;
  finally
    ProtectPassword := Tmp;
  end;
end;

procedure TJvCustomEdit.DefaultHandler(var Msg);
begin
  if ProtectPassword then
    with TMessage(Msg) do
      case Msg of
        WM_CUT, WM_COPY, WM_GETTEXT, WM_GETTEXTLENGTH, EM_SETPASSWORDCHAR:
          Result := 0;
      else
        inherited
      end
  else
    inherited;
end;

function TJvCustomEdit.GetPasswordChar: Char;
begin
  if HandleAllocated then
    Result := Char(Sendmessage(Handle, EM_GETPASSWORDCHAR, 0, 0))
  else
    Result := inherited PasswordChar;
end;

procedure TJvCustomEdit.SetSelLength(Value: Integer);
begin
  if csReading in ComponentState then
    FStreamedSelLength := Value
  else
    inherited;
end;

procedure TJvCustomEdit.SetSelStart(Value: Integer);
begin
  if csReading in ComponentState then
    FStreamedSelStart := Value
  else
    inherited;
end;

procedure TJvCustomEdit.KeyDown(var Key: Word; Shift: TSHiftState);
begin
  UpdateEdit;
  inherited;
end;

end.

