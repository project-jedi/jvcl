{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNewEdit.PAS, released on 2002-mm-dd.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TJvEdit, TJvExEdit, TJvCaretEdit,TJvAlignedEdit,
TJvSingleLineMemo.
Merging done 2002-06-05 by Peter Thornqvist [peter3 at sourceforge dot net]

  MERGE NOTES:
    * TJvCustomEdit has been removed from JvComponent and put here instead.
    * The HotTrack property only works if BorderStyle := bsSingle and BevelKind := bvNone
    * Added ClipboardCommands

Contributor(s):
  Anthony Steele [asteele att iafrica dott com]
  Peter Below [100113 dott 1101 att compuserve dott com]
  Rob den Braasem [rbraasem att xs4all dott nl] (GroupIndex property - using several TJvEdits with the same GroupIndex
    will clear the text from the other edits when something is typed into one of them.
    To disable GroupIndex, set it to -1)
  André Snepvangers [asn att xs4all dott nl] ( clx compatible version )

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvEdit;

{$I jvcl.inc}

interface

uses
  Windows, Messages,
  {$IFDEF VisualCLX}
  Qt,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Menus,
  JvCaret, JvMaxPixel, JvTypes, JvExStdCtrls;

{$IFDEF VisualCLX}
const
//  clGrayText = clDark; // (ahuser) This is wrong in QGraphics.
                         //          Since when is clGrayText = clLight = clWhite?
  clGrayText = clDisabledText;
{$ENDIF VisualCLX}

type
  TJvCustomEdit = class(TJvExCustomEdit)
  private
    FFlat: Boolean;
    FMaxPixel: TJvMaxPixel;
    FGroupIndex: Integer;
    FAlignment: TAlignment;
    FCaret: TJvCaret;
    FHotTrack: Boolean;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FProtectPassword: Boolean;
    FStreamedSelLength: Integer;
    FStreamedSelStart: Integer;
    FUseFixedPopup: Boolean;
    FAutoHint: Boolean;
    {$IFDEF VisualCLX}
    FPasswordChar: Char;
    FNullPixmap: QPixmapH;
    {$ENDIF VisualCLX}
    FEmptyValue: string;
    FIsEmptyValue: Boolean;
    FEmptyFontColor: TColor;
    FOldFontColor: TColor;
    {$IFDEF JVCLThemesEnabled}
    FThemedPassword: Boolean;
    {$ENDIF JVCLThemesEnabled}
    function GetPasswordChar: Char;
    function IsPasswordCharStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaret(const Value: TJvCaret);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetPasswordChar(Value: Char);
    procedure SetHotTrack(const Value: Boolean);
    {$IFDEF VCL}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    {$ENDIF VCL}
    procedure SetEmptyValue(const Value: string);
    procedure SetGroupIndex(Value: Integer);
    function GetFlat: Boolean;
    {$IFDEF JVCLThemesEnabled}
    procedure SetThemedPassword(const Value: Boolean);
    {$ENDIF JVCLThemesEnabled}
  protected
    procedure DoClipboardCut; override;
    procedure DoClipboardPaste; override;
    procedure DoClearText; override;
    procedure DoUndo; override;

    { (rb) renamed from UpdateEdit }
    procedure UpdateGroup; virtual;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands); override;
    procedure CaretChanged(Sender: TObject); dynamic;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    function GetPopupMenu: TPopupMenu; override;

    {$IFDEF VCL}
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure CreateHandle; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function GetText: TCaption; override;
    {$ENDIF VisualCLX}
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEmptyValueEnter; virtual;
    procedure DoEmptyValueExit; virtual;
    {$IFDEF VisualCLX}
    procedure InitWidget; override;
    procedure Paint; override;
//    procedure TextChanged; override;
//    procedure KeyPress(var Key: Char); override;
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$ENDIF VisualCLX}
    procedure DoSetFocus(FocusedWnd: HWND); override;
    procedure DoKillFocus(FocusedWnd: HWND); override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure EnabledChanged; override;
    procedure SetFlat(Value: Boolean); virtual;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;

    procedure Loaded; override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
  public
    function IsEmpty: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF VCL}
    procedure DefaultHandler(var Msg); override;
    {$ENDIF VCL}
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False;
    property Caret: TJvCaret read FCaret write SetCaret;
    property EmptyValue: string read FEmptyValue write SetEmptyValue;
    property EmptyFontColor: TColor read FEmptyFontColor write FEmptyFontColor default clGrayText;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar stored IsPasswordCharStored;
    {$IFDEF JVCLThemesEnabled}
    property ThemedPassword: Boolean read FThemedPassword write SetThemedPassword default False;
    {$ENDIF JVCLThemesEnabled}
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;
    {$IFDEF VCL}
    property Text: TCaption read GetText write SetText;
    {$ENDIF VCL}
    property UseFixedPopup: Boolean read FUseFixedPopup write FUseFixedPopup default True;
    property HintColor;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default -1;
    property OnParentColorChange;
    property Flat: Boolean read GetFlat write SetFlat default False;
  end;

  TJvEdit = class(TJvCustomEdit)
  published
    {$IFDEF VCL}
    {$IFDEF COMPILER6_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF COMPILER6_UP}
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property EmptyValue; // p3: clx not implemented yet
    property EmptyFontColor; // p3: clx not implemented yet
    property Flat;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property UseFixedPopup; // asn: clx not implemented yet
    {$ENDIF VCL}
    property Caret;
    property DisabledTextColor;
    property DisabledColor;
    property HotTrack;
    property PasswordChar;
    property PopupMenu;
    property ProtectPassword;
    {$IFDEF VisualCLX}
    property EchoMode;
    property InputKeys;
    {$ENDIF VisualCLX}
    property Align;
    property Alignment;
    property ClipboardCommands;
    property HintColor;
    property GroupIndex;
    property MaxPixel;
    property Modified; { (rb) why published/stored? }
    {$IFDEF JVCLThemesEnabled}
    property ThemedPassword;
    {$ENDIF JVCLThemesEnabled}
    // property SelStart; (p3) why published?
    // property SelText;
    // property SelLength; (p3) why published?
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property AutoHint;
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
    {$IFDEF VCL}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Math, Forms,
  {$IFDEF VCL}
  JvFixedEditPopup,
  {$ENDIF VCL}
  JvToolEdit;

//=== Local procedures =======================================================

// (rom) StrFillChar replaced by StringOfChar

function TextFitsInCtrl(Control: TControl; const Text: string): Boolean;
var
  C: TControlCanvas;
  Size: TSize;
begin
  C := TControlCanvas.Create;
  try
    C.Control := Control;
    {$IFDEF VisualCLX}
    C.StartPaint;
    {$ENDIF VisualCLX}
    Result :=
      not GetTextExtentPoint32(C.Handle, PChar(Text), Length(Text), Size) or
      { (rb) ClientWidth is too big, should be EM_GETRECT, don't know the Clx variant }
      (Control.ClientWidth > Size.cx);
    {$IFDEF VisualCLX}
    C.StopPaint;
    {$ENDIF VisualCLX}
  finally
    C.Free;
  end;
end;

//=== { TJvCustomEdit } ======================================================

constructor TJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF VisualCLX}
  FNullPixmap := QPixmap_create(1, 1, 1, QPixmapOptimization_DefaultOptim);
  {$ENDIF VisualCLX}
  FAlignment := taLeftJustify;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  ClipboardCommands := [caCopy..caUndo];
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FHotTrack := False;
  FCaret := TJvCaret.Create(Self);
  FCaret.OnChanged := CaretChanged;
  FStreamedSelLength := 0;
  FStreamedSelStart := 0;
  FUseFixedPopup := True; // asn: clx not implemented yet
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FGroupIndex := -1;
  FEmptyFontColor := clGrayText;
end;

destructor TJvCustomEdit.Destroy;
begin
  FMaxPixel.Free;
  FCaret.Free;
  {$IFDEF VisualCLX}
  QPixmap_destroy(FNullPixmap);
  {$ENDIF VisualCLX}
  inherited Destroy;
end;

procedure TJvCustomEdit.CaretChanged(Sender: TObject);
begin
  FCaret.CreateCaret;
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
end;

{$IFDEF VCL}
procedure TJvCustomEdit.CMHintShow(var Msg: TMessage);
begin
  if AutoHint and not TextFitsInCtrl(Self, Self.Text) then
    with TCMHintShow(Msg) do
    begin
      HintInfo.HintPos := Self.ClientToScreen(Point(-2, Height - 2));
      HintInfo.HintStr := Self.Text;
      Result := 0;
    end
  else
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvCustomEdit.HintShow(var HintInfo : THintInfo): Boolean;
begin
  if AutoHint and not TextFitsInCtrl(Self, Self.Text) then
  begin
    HintInfo.HintPos := Self.ClientToScreen(Point(-2, Height - 2));
    HintInfo.HintStr := Self.Text;
  end;
  Result := inherited HintShow(HintInfo);
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvCustomEdit.CreateHandle;
begin
  inherited CreateHandle;
  if Focused then
    DoEmptyValueEnter
  else
    DoEmptyValueExit;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvCustomEdit.InitWidget;
begin
  inherited InitWidget;
  if Focused then
    DoEmptyValueEnter
  else
    DoEmptyValueExit;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}

procedure TJvCustomEdit.CreateParams(var Params: TCreateParams);
const
  {$IFDEF JVCLThemesEnabled}
  Passwords: array [Boolean] of DWORD = (0, ES_PASSWORD);
  {$ENDIF JVCLThemesEnabled}
  Styles: array [TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Styles[FAlignment];
  {$IFDEF JVCLThemesEnabled}
  Params.Style := Params.Style or Passwords[ThemedPassword];
  {$ENDIF JVCLThemesEnabled}
  if (FAlignment <> taLeftJustify) and (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
    (Win32MajorVersion = 4) and (Win32MinorVersion = 0) then
    Params.Style := Params.Style or ES_MULTILINE; // needed for Win95
end;

procedure TJvCustomEdit.DefaultHandler(var Msg);
begin
  if ProtectPassword then
    with TMessage(Msg) do
      case Msg of
        WM_CUT, WM_COPY, WM_GETTEXT, WM_GETTEXTLENGTH, EM_SETPASSWORDCHAR:
          Result := 0;
      else
        inherited DefaultHandler(Msg);
      end
  else
    inherited DefaultHandler(Msg);
end;

{$ENDIF VCL}

procedure TJvCustomEdit.DoClearText;
begin
  if not ReadOnly then
    inherited DoClearText;
end;

procedure TJvCustomEdit.DoClipboardCut;
begin
  if not ReadOnly then
    inherited DoClipboardCut;
end;

procedure TJvCustomEdit.DoClipboardPaste;
begin
  if not ReadOnly then
  begin
    inherited DoClipboardPaste;
    UpdateGroup;
  end;
end;

procedure TJvCustomEdit.DoEmptyValueEnter;
begin
  if EmptyValue <> '' then
  begin
    if (inherited Text) = EmptyValue then
    begin
      inherited Text := '';
      FIsEmptyValue := False;
      if not (csDesigning in ComponentState) then
        Font.Color := FOldFontColor;
    end;
  end
  else
    if not (csDesigning in ComponentState) then
      Font.Color := FOldFontColor;
end;

procedure TJvCustomEdit.DoEmptyValueExit;
begin
  if EmptyValue <> '' then
  begin
    if Text = '' then
    begin
      Text := EmptyValue;
      FIsEmptyValue := True;
      if not (csDesigning in ComponentState) then
      begin
        FOldFontColor := Font.Color;
        Font.Color := FEmptyFontColor;
      end;
    end;
  end;
end;

procedure TJvCustomEdit.DoEnter;
begin
  inherited DoEnter;
  DoEmptyValueEnter;
end;

procedure TJvCustomEdit.DoExit;
begin
  inherited DoExit;
  DoEmptyValueExit;
end;

procedure TJvCustomEdit.DoKillFocus(FocusedWnd: HWND);
begin
  FCaret.DestroyCaret;
  inherited DoKillFocus(FocusedWnd);
end;

function TJvCustomEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
var
  R: TRect;
begin
  if Enabled then
    Result := inherited DoPaintBackground(Canvas, Param)
  else
  begin
    Canvas.Brush.Color := FDisabledColor;
    Canvas.Brush.Style := bsSolid;
    R := ClientRect;
    Canvas.FillRect(R);
    Result := True;
    {$IFDEF VisualCLX}
    // paint Border
    if BorderStyle = bsSingle then
      QGraphics.DrawEdge(Canvas, R, esLowered, esLowered, ebRect);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomEdit.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.DoUndo;
begin
  if not ReadOnly then
    inherited DoUndo;
end;

procedure TJvCustomEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

function TJvCustomEdit.GetFlat: Boolean;
begin
  {$IFDEF VCL}
  FFlat := not Ctl3D; // update
  {$ENDIF VCL}
  Result := FFlat;
end;

function TJvCustomEdit.GetPasswordChar: Char;
begin
  {$IFDEF VCL}
  if HandleAllocated then
    Result := Char(SendMessage(Handle, EM_GETPASSWORDCHAR, 0, 0))
  else
    Result := inherited PasswordChar;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Result := FPasswordChar;
  {$ENDIF VisualCLX}
end;

function TJvCustomEdit.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  {$IFDEF VCL}
  // user has not assigned his own popup menu, so use fixed default
  if (Result = nil) and UseFixedPopup then
    Result := FixedDefaultEditPopUp(Self);
  {$ENDIF VCL}
end;


function TJvCustomEdit.GetText: TCaption;
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    {$IFDEF VCL}
    Result := inherited Text;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Result := inherited GetText;
    {$ENDIF VisualCLX}
  finally
    ProtectPassword := Tmp;
  end;

  if (Result = EmptyValue) and (EmptyValue <> '') then
    Result := '';
end;


function TJvCustomEdit.IsEmpty: Boolean;
begin
  Result := (Length(Text) = 0);
end;

function TJvCustomEdit.IsPasswordCharStored: Boolean;
begin
  Result := (PasswordChar <> #0) {$IFDEF JVCLThemesEnabled} and not ThemedPassword {$ENDIF};
end;

procedure TJvCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  UpdateGroup;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomEdit.Loaded;
begin
  inherited Loaded;
  FOldFontColor := Font.Color;
  SelStart := FStreamedSelStart;
  SelLength := FStreamedSelLength;
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

procedure TJvCustomEdit.MouseEnter(AControl: TControl);
var
  I, J: Integer;
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Flat := False;
      SelStart := I;
      SelLength := J;
    end;
//    UpdateAutoHint;
    inherited MouseEnter(AControl);
  end;
end;

procedure TJvCustomEdit.MouseLeave(AControl: TControl);
var
  I, J: Integer;
begin
  if MouseOver then
  begin
    if FHotTrack then
    begin
      I := SelStart;
      J := SelLength;
      Flat := True;
      SelStart := I;
      SelLength := J;
    end;
    inherited MouseLeave(AControl);
  end;
end;

{$IFDEF VisualCLX}

procedure TJvCustomEdit.Paint;
var
  S: TCaption;
begin
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    inherited Paint
  else
  begin
    if PasswordChar = #0 then
      S := Text
    else
      S := StringOfChar(PasswordChar, Length(Text));
    if not PaintEdit(Self, S, FAlignment, False, {0,} FDisabledTextColor,
      Focused, Flat, Canvas) then
      inherited Paint;
  end;
end;

{$ENDIF VisualCLX}

procedure TJvCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    {$IFDEF VCL}
    RecreateWnd;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    inherited Alignment := FAlignment;
    Invalidate;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomEdit.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  if ClipboardCommands <> Value then
  begin
    inherited SetClipboardCommands(Value);
    ReadOnly := ClipboardCommands <= [caCopy];
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

procedure TJvCustomEdit.SetEmptyValue(const Value: string);
begin
  FEmptyValue := Value;
  if HandleAllocated then
  begin
    if Focused then
      DoEmptyValueEnter
    else
      DoEmptyValueExit;
  end;
end;

procedure TJvCustomEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    {$IFDEF VCL}
    Ctl3D := not FFlat;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    if FFlat then
      BorderStyle := bsNone
    else
      BorderStyle := bsSingle;
    Invalidate;
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomEdit.SetGroupIndex(Value: Integer);
begin
  if Value <> FGroupIndex then
  begin
    FGroupIndex := Value;
    UpdateGroup;
  end;
end;

procedure TJvCustomEdit.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Flat := FHotTrack;
end;

procedure TJvCustomEdit.SetPasswordChar(Value: Char);
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;
    {$IFDEF VCL}
    if HandleAllocated then
      inherited PasswordChar := Char(SendMessage(Handle, EM_GETPASSWORDCHAR, 0, 0));
    inherited PasswordChar := Value;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FPasswordChar := Value;
    Invalidate;
    {$ENDIF VisualCLX}
  finally
    ProtectPassword := Tmp;
  end;
end;

procedure TJvCustomEdit.SetSelLength(Value: Integer);
begin
  if csReading in ComponentState then
    FStreamedSelLength := Value
  else
    inherited SetSelLength(Value);
end;

procedure TJvCustomEdit.SetSelStart(Value: Integer);
begin
  if csReading in ComponentState then
    FStreamedSelStart := Value
  else
    inherited SetSelStart(Value);
end;

{$IFDEF VCL}

// (ahuser) ProtectPassword has no function under CLX

procedure TJvCustomEdit.SetText(const Value: TCaption);
begin
  if csLoading in ComponentState then
  begin
    inherited Text := Value;
    Exit;
  end;
  FIsEmptyValue := (Value = '');
  if not FIsEmptyValue then
  begin
    Font.Color := FOldFontColor;
    inherited Text := Value;
  end
  else
  begin
    Font.Color := FEmptyFontColor;
    inherited Text := EmptyValue;
  end;
end;

{$ENDIF VCL}

{$IFDEF JVCLThemesEnabled}

procedure TJvCustomEdit.SetThemedPassword(const Value: Boolean);
begin
  if FThemedPassword <> Value then
  begin
    FThemedPassword := Value;
    PasswordChar := #0;
    RecreateWnd;
  end;
end;

{$ENDIF JVCLThemesEnabled}

procedure TJvCustomEdit.UpdateGroup;
var
  I: Integer;
begin
  if (FGroupIndex <> -1) and (Owner <> nil) then
    for I := 0 to Owner.ComponentCount - 1 do
      if (Owner.Components[I] is TJvCustomEdit) and (Owner.Components[I] <> Self) and
        (TJvCustomEdit(Owner.Components[I]).GroupIndex = Self.GroupIndex) then
        TJvCustomEdit(Owner.Components[I]).Clear;
end;

{$IFDEF VCL}
procedure TJvCustomEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TControlCanvas;
  S: TCaption;
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
      S := StringOfChar(PasswordChar, Length(Text));
    Canvas := nil;
    try
      if not PaintEdit(Self, S, FAlignment, False, {0,} FDisabledTextColor,
        Focused, Canvas, Msg) then
        inherited;
    finally
      Canvas.Free;
    end;
  end;
end;
{$ENDIF VCL}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

