{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQEdit;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, 
  Qt, 
  QForms, Classes, QGraphics, QControls, QMenus, 
  JvQMaxPixel, JvQTypes, JvQExStdCtrls;


const
  clGrayText = clDisabledText;


type
  TJvCustomEdit = class(TJvExCustomEdit)
  private
    FFlat: Boolean;
    FMaxPixel: TJvMaxPixel;
    FGroupIndex: Integer;
    FAlignment: TAlignment; 
    FHotTrack: Boolean;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FProtectPassword: Boolean;
    FStreamedSelLength: Integer;
    FStreamedSelStart: Integer;
    FUseFixedPopup: Boolean;
    FAutoHint: Boolean; 
    FPasswordChar: Char; 
    FEmptyValue: string;
    FIsEmptyValue: Boolean;
    FEmptyFontColor: TColor;
    FOldFontColor: TColor;
    FIsLoaded:boolean; 
    function GetPasswordChar: Char;
    function IsPasswordCharStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetPasswordChar(Value: Char);
    procedure SetHotTrack(const Value: Boolean); 
    procedure SetEmptyValue(const Value: string);
    procedure SetGroupIndex(Value: Integer);
    function GetFlat: Boolean; 
    procedure WMPaste(var Mesg: TMessage); message WM_PASTE;
    procedure WMUndo(var Mesg: TMessage); message WM_UNDO;
  protected
    { (rb) renamed from UpdateEdit }
    procedure UpdateGroup; virtual;
//    procedure CaretChanged(Sender: TObject); dynamic;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    function GetPopupMenu: TPopupMenu; override;
  
    function GetText: TCaption; override; 
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEmptyValueEnter; virtual;
    procedure DoEmptyValueExit; virtual; 
    procedure InitWidget; override;
    procedure Paint; override;
//    procedure TextChanged; override;
//    procedure KeyPress(var Key: Char); override; 
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    function DoEraseBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure EnabledChanged; override;
    procedure SetFlat(Value: Boolean); virtual;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;

    procedure Loaded; override; 
  public
    function IsEmpty: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; 
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False; 
    property EmptyValue: string read FEmptyValue write SetEmptyValue;
    property EmptyFontColor: TColor read FEmptyFontColor write FEmptyFontColor default clGrayText;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar stored IsPasswordCharStored; 
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow; 
    property UseFixedPopup: Boolean read FUseFixedPopup write FUseFixedPopup default True;
    property HintColor;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default -1;
    property OnParentColorChange;
    property Flat: Boolean read GetFlat write SetFlat default False;
  end;

  TJvEdit = class(TJvCustomEdit)
  published 
//    property Caret;
    property DisabledTextColor;
    property DisabledColor;
    property HotTrack;
    property PasswordChar;
    property PopupMenu;
    property ProtectPassword; 
    property EchoMode; 
    property Align;
    property Alignment;
    property ClipboardCommands;
    property HintColor;
    property GroupIndex;
    property MaxPixel; 
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
  SysUtils, Math, 
  JvQToolEdit;

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
    C.StartPaint; 
    Result :=
      not GetTextExtentPoint32(C.Handle, PChar(Text), Length(Text), Size) or
      { (rb) ClientWidth is too big, should be EM_GETRECT, don't know the Clx variant }
      (Control.ClientWidth > Size.cx); 
    C.StopPaint; 
  finally
    C.Free;
  end;
end;

//=== { TJvCustomEdit } ======================================================

constructor TJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taLeftJustify;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  ClipboardCommands := [caCopy..caUndo];
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FHotTrack := False; 
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
  inherited Destroy;
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

(*
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
*)

function TJvCustomEdit.HintShow(var HintInfo: THintInfo): Boolean;
begin
  if AutoHint and not TextFitsInCtrl(Self, Self.Text) then
  begin
    HintInfo.HintPos := Self.ClientToScreen(Point(-2, Height - 2));
    HintInfo.HintStr := Self.Text;
  end;
  Result := inherited HintShow(HintInfo);
end;



procedure TJvCustomEdit.InitWidget;
begin
  inherited InitWidget;
  if Focused then
    DoEmptyValueEnter
  else
    DoEmptyValueExit;
end;





procedure TJvCustomEdit.WMPaste(var Mesg: TMessage);
begin
  inherited;
  UpdateGroup;
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
    // paint Border
    if BorderStyle = bsSingle then
      QGraphics.DrawEdge(Canvas, R, esLowered, esLowered, ebRect); 
  end;
end;

procedure TJvCustomEdit.WMUndo(var Mesg: TMessage);
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
  Result := FFlat;
end;

function TJvCustomEdit.GetPasswordChar: Char;
begin  
  Result := FPasswordChar; 
end;

function TJvCustomEdit.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu; 
end;

// (ahuser) ProtectPassword has no function under CLX
function TJvCustomEdit.GetText: TCaption;
var
  Tmp: Boolean;
begin
  Tmp := ProtectPassword;
  try
    ProtectPassword := False;  
    Result := inherited GetText; 
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
  Result := (PasswordChar <> #0) ;
end;

procedure TJvCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  UpdateGroup;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomEdit.Loaded;
begin
  inherited Loaded;
  FIsLoaded := true;
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
    Canvas.Font := Font;  
    if not PaintEdit(Self, S, FAlignment, False, {0,} FDisabledTextColor,
      Focused, Flat, Canvas) then
      inherited Paint;
  end;
end;



procedure TJvCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;  
    inherited Alignment := FAlignment;
    Invalidate; 
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
    if FFlat then
      BorderStyle := bsNone
    else
      BorderStyle := bsSingle;
    Invalidate; 
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
    FPasswordChar := Value;
    Invalidate; 
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

