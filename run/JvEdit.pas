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
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  Types,
  {$ENDIF CLR}
  Windows, Messages,
  {$IFDEF VisualCLX}
  Qt,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Menus,
  JvCaret, JvMaxPixel, JvTypes, JvExStdCtrls;

{$IFDEF VisualCLX}
const
  clGrayText = clDisabledText; // Since when is clGrayText = clLight = clWhite?
{$ENDIF VisualCLX}

type
  TJvCustomEdit = class(TJvExCustomEdit)
  private
    {$IFDEF VisualCLX}
    FFlat: Boolean;
    {$ENDIF VisualCLX}
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
    FIsLoaded: Boolean;
    {$IFDEF JVCLThemesEnabled}
    FThemedPassword: Boolean;
    FThemedFont: TFont;
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
    function IsFlatStored: Boolean;
    {$ENDIF VCL}
    procedure SetEmptyValue(const Value: string);
    procedure SetGroupIndex(Value: Integer);
    function GetFlat: Boolean;
    {$IFDEF JVCLThemesEnabled}
    procedure SetThemedPassword(const Value: Boolean);
    procedure WMSetFont(var Msg: TWMSetFont); message WM_SETFONT;
    function GetThemedFontHandle: HFONT;
    {$ENDIF JVCLThemesEnabled}
  protected
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;

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
    procedure SetText(const Value: TCaption); {$IFDEF CLR}reintroduce;{$ENDIF} virtual;
    procedure CreateHandle; override;
    procedure DestroyWnd; override;
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
    function HintShow(var HintInfo: THintInfo): Boolean; override;
    {$ENDIF VisualCLX}
    procedure FocusSet(PrevWnd: THandle); override;
    procedure FocusKilled(NextWnd: THandle); override;
    function DoEraseBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
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
    property Flat: Boolean read GetFlat write SetFlat {$IFDEF VisualCLX}default False;{$ENDIF VisualCLX}{$IFDEF VCL}stored IsFlatStored;{$ENDIF VCL}
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
    property ParentCtl3D;
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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
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
      {$IFDEF CLR}
      not GetTextExtentPoint32(C.Handle, Text, Length(Text), Size) or
      {$ELSE}
      not GetTextExtentPoint32(C.Handle, PChar(Text), Length(Text), Size) or
      {$ENDIF CLR}
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
  {$IFDEF JVCLThemesEnabled}
  FThemedFont.Free;
  {$ENDIF JVCLThemesEnabled}
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
  Sel: Integer;
begin
  inherited Change;
  if not HasParent then
    Exit;
  St := Text;
  FMaxPixel.Test(St, Font);
  if St <> Text then
  begin
    Sel := SelStart;
    Text := St;
    SelStart := Min(Sel, Length(Text));
  end;
end;

{$IFDEF VCL}
procedure TJvCustomEdit.CMHintShow(var Msg: TMessage);
{$IFDEF CLR}
var
  Info: THintInfo;
{$ENDIF CLR}
begin
  if AutoHint and not TextFitsInCtrl(Self, Self.Text) then
    {$IFDEF CLR}
    with TCMHintShow.Create(Msg) do
    begin
      Info := HintInfo;
      Info.HintPos := Self.ClientToScreen(Point(-2, Height - 2));
      Info.HintStr := Self.Text;
      HintInfo := Info;
    end
    {$ELSE}
    with TCMHintShow(Msg) do
    begin
      HintInfo.HintPos := Self.ClientToScreen(Point(-2, Height - 2));
      HintInfo.HintStr := Self.Text;
      Result := 0;
    end
    {$ENDIF CLR}
  else
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvCustomEdit.HintShow(var HintInfo: THintInfo): Boolean;
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

procedure TJvCustomEdit.DestroyWnd;
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    // TWinControl.DestroyWnd sends WM_GETTEXTLENGTH & WM_GETTEXT messages,
    // thus we have to temporarily set ProtectPassword to False.
    ProtectPassword := False;
    inherited DestroyWnd;
  finally
    ProtectPassword := Tmp;
  end;
end;

{$ENDIF VCL}

procedure TJvCustomEdit.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomEdit.WMCut(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomEdit.WMPaste(var Msg: TMessage);
begin
  if not ReadOnly then
  begin
    inherited;
    UpdateGroup;
  end;
end;

procedure TJvCustomEdit.DoEmptyValueEnter;
begin
  if (csDesigning in ComponentState) or not FIsLoaded or (EmptyValue = '') then
    Exit;
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
  if (csDesigning in ComponentState) or not FIsLoaded or (EmptyValue = '') then
    Exit;
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
  end
  else
  if not (csDesigning in ComponentState) then
    Font.Color := FOldFontColor;
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

procedure TJvCustomEdit.FocusKilled(NextWnd: THandle);
begin
  FCaret.DestroyCaret;
  inherited FocusKilled(NextWnd);
end;

function TJvCustomEdit.DoEraseBackground(Canvas: TCanvas; Param: Integer): Boolean;
var
  R: TRect;
begin
  if Enabled then
    Result := inherited DoEraseBackground(Canvas, Param)
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

procedure TJvCustomEdit.FocusSet(PrevWnd: THandle);
begin
  inherited FocusSet(PrevWnd);
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.WMUndo(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

function TJvCustomEdit.GetFlat: Boolean;
begin
  {$IFDEF VCL}
  Result := not Ctl3D;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Result := FFlat;
  {$ENDIF VisualClx}
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

// (ahuser) ProtectPassword has no function under CLX
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

{$IFDEF JVCLThemesEnabled}
function TJvCustomEdit.GetThemedFontHandle: HFONT;
var
  AFont: TLogFont;
begin
  GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(AFont), @AFont);
  AFont.lfHeight := Self.Font.Height;
  Result := CreateFontIndirect(AFont);
end;
{$ENDIF JVCLThemesEnabled}

function TJvCustomEdit.IsEmpty: Boolean;
begin
  Result := (Length(Text) = 0);
end;

{$IFDEF VCL}
function TJvCustomEdit.IsFlatStored: Boolean;
begin
  { Same as IsCtl3DStored }
  Result := not ParentCtl3D;
end;
{$ENDIF VCL}

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
  { (rb) I think that csLoading flag can be used instead of FIsLoaded.
         FIsLoaded is set a bit later to true than csLoading but that
         does not matter AFAICS
  }
  FIsLoaded := True;
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
    if Focused then
      DoEmptyValueEnter
    else
      DoEmptyValueExit;
end;

procedure TJvCustomEdit.SetFlat(Value: Boolean);
begin
  {$IFDEF VCL}
  Ctl3D := not Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Value <> FFlat then
  begin
    FFlat := Value;
    if FFlat then
      BorderStyle := bsNone
    else
      BorderStyle := bsSingle;
    Invalidate;
  end;
  {$ENDIF VisualCLX}
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
  if (csLoading in ComponentState) or not FIsLoaded then
  begin
    inherited Text := Value;
    Exit;
  end;
  FIsEmptyValue := (Value = '') and (EmptyValue <> '');
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
    if not FThemedPassword then
      FreeAndNil(FThemedFont);
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
  { PaintEdit does not work well when the edit is themed (and ThemedPassword=true),
    as a workaround check if the disabled colors are set to the default so
    the edit can paint itself (We must check both colors, although only
    DisabledTextColor is passed on to PaintEdit; PaintEdit triggers a
    DoEraseBackground call) }
  if Enabled or ((DisabledTextColor = clGrayText) and (DisabledColor = clWindow)) then
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

{$IFDEF JVCLThemesEnabled}
procedure TJvCustomEdit.WMSetFont(var Msg: TWMSetFont);
begin
  if ThemedPassword then
  begin
    // Retrieves MS Shell Dlg.
    // Other way is to use Screen.IconFont
    if FThemedFont = nil then
      FThemedFont := TFont.Create;
    FThemedFont.Handle := GetThemedFontHandle;
    Msg.Font := FThemedFont.Handle;
  end;
  inherited;
end;
{$ENDIF JVCLThemesEnabled}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

