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
  André Snepvangers [asn@xs4all.nl] ( clx compatible version )

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvEdit;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, StdCtrls, Forms, Menus,
  JvCaret, JvToolEdit,
  {$ENDIF}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QStdCtrls, QDialogs, QForms,
  {$ENDIF}
  JvComponent, JvMaxPixel, JVCLVer;

type
  TJvCustomEdit = class(TCustomEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOver: Boolean;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FMaxPixel: TJvMaxPixel;
    FClipboardCommands: TJvClipboardCommands;
    FOldCommands: TJvClipboardCommands;
    FGroupIndex: Integer;
  {$IFDEF VCL}
    FAlignment: TAlignment;
    FCaret: TJvCaret;
    FHotTrack: Boolean;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FProtectPassword: Boolean;
    FStreamedSelLength: Integer;
    FStreamedSelStart: Integer;
    FUseFixedPopup: boolean;
    procedure CaretChanged(Sender: TObject); dynamic;
    function GetPasswordChar: Char;
    function GetReadOnly: Boolean;
    function GetText: string;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaret(const Value: TJvCaret);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetPasswordChar(Value: Char);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  {$ENDIF}
    procedure SetGroupIndex(const Value: Integer);
    procedure UpdateEdit;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands);
  protected
    procedure Change; override;
    procedure KeyDown(var Key:Word;Shift:TSHiftState);override;
    procedure MaxPixelChanged(Sender: TObject);
  {$IFDEF VCL}
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    function GetPopupMenu: TPopupMenu; override;
  {$ENDIF}
  {$IFDEF VisualCLX}
    procedure EnabledChanged; override;
    procedure ParentColorChanged; override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
  {$ENDIF}
  public
    function IsEmpty: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF VCL}
    procedure DefaultHandler(var Msg); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
  {$ENDIF}
  protected
  {$IFDEF VCL}
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caret: TJvCaret read FCaret write SetCaret;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
    property UseFixedPopup:boolean read FUseFixedPopup write FUseFixedPopup default true;
  {$ENDIF}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands write SetClipboardCommands default
      [caCopy..caUndo];
    // set to True to disable read/write of PasswordChar and read of Text
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

  TJvEdit = class(TJvCustomEdit)
  published
  {$IFDEF VCL}
    {$IFDEF COMPILER6_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    property Caret;
    property DragCursor;
    property DragKind;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property DisabledTextColor;
    property DisabledColor;
    property HotTrack;
    property PasswordChar;
    property PopupMenu;
    property ProtectPassword;
    property UseFixedPopup; // asn: clx not implemented yet
  {$ENDIF}
  {$IFDEF VisualCLX}
    property EchoMode;
    property InputKeys;
  {$ENDIF}
    property AboutJVCL;
    property Align;
    property Alignment;
    property ClipboardCommands;
    property HintColor;
    property GroupIndex;
    property MaxPixel;
    property Modified;
    // property SelStart; (p3) why published?
    // property SelText;
    // property SelLength; (p3) why published?
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
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
  {$IFDEF VCL}
  JvFixedEditPopup,
  {$ENDIF}
  Math;

constructor TJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  FAlignment := taLeftJustify;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  ClipboardCommands := [caCopy..caUndo];
  {$IFDEF VCL}
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FHotTrack := False;
  FCaret := TJvCaret.Create(Self);
  FCaret.OnChanged := CaretChanged;
  FStreamedSelLength := 0;
  FStreamedSelStart := 0;
  FUseFixedPopup := True;  // asn: clx not implemented yet
  {$ENDIF}
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FGroupIndex := -1;
end;

destructor TJvCustomEdit.Destroy;
begin
  FMaxPixel.Free;
  {$IFDEF VCL}
  FCaret.Free;
  {$ENDIF}
  inherited Destroy;
end;

{$IFDEF VCL}
procedure TJvCustomEdit.Loaded;
begin
  inherited Loaded;
  SelStart := FStreamedSelStart;
  SelLength := FStreamedSelLength;
end;
{$ENDIF}

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
end;

{$IFDEF VCL}
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
{$ENDIF}

{$IFDEF VisualCLX}
procedure TJvCustomEdit.MouseEnter(AControl: TControl);
{$ENDIF}
{$IFDEF VCL}
procedure TJvCustomEdit.CMMouseEnter(var Msg: TMessage);
var
  I, J: Integer;
{$ENDIF}
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
  {$IFDEF VCL}
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Ctl3D := True;
      SelStart := I;
      SelLength := J;
    end;
  {$ENDIF}
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;


{$IFDEF VCL}
procedure TJvCustomEdit.CMMouseLeave(var Msg: TMessage);
var I, J: Integer;
{$ENDIF}
{$IFDEF VisualCLX}
procedure TJvCustomEdit.MouseLeave(AControl: TControl);
{$ENDIF}
begin
  {$IFDEF VisualCLX}
  inherited;
  {$ENDIF}
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    {$IFDEF VCL}
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Ctl3D := False;
      SelStart := I;
      SelLength := J;
    end;
    {$ENDIF}
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFDEF VCL}
procedure TJvCustomEdit.CMParentColorChanged(var Msg: TMessage);
{$ENDIF}
{$IFDEF VisualCLX}
procedure TJvCustomEdit.ParentColorChanged;
{$ENDIF}
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{$IFDEF VCL}
procedure TJvCustomEdit.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Ctl3D := not FHotTrack;  // asn: in clx not implemented
end;
{$ENDIF}

function TJvCustomEdit.IsEmpty: Boolean;
begin
  Result := (Length(Text) = 0);
end;

{$IFDEF VCL}  // asn: clx version version has alignment prpoerty
procedure TJvCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;
{$ENDIF}

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

{$IFDEF VCL}
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
      Canvas.Free;
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
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    inherited
  else
  begin
    if PasswordChar = #0 then
      S := Text
    else
      S := StrFillChar(PasswordChar, Length(Text));
    Canvas := nil;
    if not PaintEdit(Self, S, FAlignment, False, {0,} FDisabledTextColor,
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
{$ENDIF}

{$IFDEF VCL}
procedure TJvCustomEdit.CMEnabledchanged(var Message: TMessage);
{$ENDIF}
{$IFDEF VisualCLX}
procedure TJvCustomEdit.Enabledchanged;
{$ENDIF}
begin
  inherited;
  Invalidate;
end;

{$IFDEF VisualCLX}
procedure TJvCustomEdit.CopyToClipboard;
{$ENDIF}
{$IFDEF VCL}
procedure TJvCustomEdit.WMCopy(var Msg: TWMCopy);
{$ENDIF}
begin
  if caCopy in ClipBoardCommands then
    inherited;
end;

{$IFDEF VisualCLX}
procedure TJvCustomEdit.CutToClipboard;
{$ENDIF}
{$IFDEF VCL}
procedure TJvCustomEdit.WMCut(var Msg: TWMCut);
{$ENDIF}
begin
  if caCut in ClipBoardCommands then
    inherited;
end;

{$IFDEF VisualCLX}
procedure TJvCustomEdit.PasteFromClipboard;
{$ENDIF}
{$IFDEF VCL}
procedure TJvCustomEdit.WMPaste(var Msg: TWMPaste);
{$ENDIF}
begin
  if caPaste in ClipBoardCommands then
    inherited;
  UpdateEdit;
end;

{$IFDEF VisualCLX}
procedure TJvCustomEdit.Undo;
{$ENDIF}
{$IFDEF VCL}
procedure TJvCustomEdit.WMUndo(var Msg: TWMUndo);
{$ENDIF}
begin
  if caUndo in ClipBoardCommands then
    inherited;
end;

{$IFDEF VCL}
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
{$ENDIF}

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

{$IFDEF VCL}
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
{$ENDIF}

procedure TJvCustomEdit.KeyDown(var Key: Word; Shift: TSHiftState);
begin
  UpdateEdit;
  inherited;
end;

{$IFDEF VCL}
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

function TJvCustomEdit.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
 // user has not assigned his own popup menu, so use fixed default 
  if (Result = nil) and UseFixedPopup then
    Result := FixedDefaultEditPopUp(self);
end;
{$ENDIF}

end.

