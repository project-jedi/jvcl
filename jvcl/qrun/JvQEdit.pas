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

{$I jvcl.inc}

unit JvQEdit;

interface

uses
  SysUtils, Classes,  
  Qt, QTypes, QGraphics, QControls, QStdCtrls, QDialogs, QForms, QMenus, Types,
  QWindows, 
  JvQCaret, JvQMaxPixel, JvQTypes, JvQToolEdit, JvQExStdCtrls;


const
  clGrayText = clDark; // (ahuser) This is wrong in QGraphics.
                       //          Since when is clGrayText = clLight = clWhite?


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
    FOldHint: TCaption; 
    FPasswordChar: Char;
    FNullPixmap: QPixmapH; 
    FEmptyValue: string;
    FIsEmptyValue: boolean;
    FEmptyFontColor, FOldFontColor: TColor;
    function GetPasswordChar: Char;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaret(const Value: TJvCaret);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetPasswordChar(Value: Char);
    procedure SetHotTrack(const Value: Boolean); 
    procedure SetEmptyValue(const Value: string);
    procedure SetGroupIndex(Value: Integer);
    function GetFlat: Boolean;
    procedure SetAutoHint(Value: Boolean);
  protected
    procedure DoClipboardCut; override;
    procedure DoClipboardPaste; override;
    procedure DoClearText; override;
    procedure DoUndo; override;

    procedure UpdateEdit; virtual;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands); override;
    procedure CaretChanged(Sender: TObject); dynamic;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    function GetPopupMenu: TPopupMenu; override;
 
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEmptyValueEnter; virtual;
    procedure DoEmptyValueExit; virtual; 
    procedure InitWidget; override;
    procedure Paint; override;
    procedure TextChanged; override;
    procedure KeyPress(var Key: Char); override; 
    procedure DoSetFocus(FocusedWnd: HWND); override;
    procedure DoKillFocus(FocusedWnd: HWND); override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure EnabledChanged; override;
    procedure SetFlat(Value: Boolean); virtual;
    procedure UpdateAutoHint; dynamic;
    procedure Resize; override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
  public
    function IsEmpty: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
 
    procedure Loaded; override;
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoHint: Boolean read FAutoHint write SetAutoHint default False;
    property Caret: TJvCaret read FCaret write SetCaret;
    property EmptyValue: string read FEmptyValue write SetEmptyValue;
    property EmptyFontColor:TColor read FEmptyFontColor write FEmptyFontColor default clGrayText;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    // set to True to disable read/write of PasswordChar and read of Text
    property ProtectPassword: Boolean read FProtectPassword write FProtectPassword default False;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow; 
    property UseFixedPopup: Boolean read FUseFixedPopup write FUseFixedPopup default True;
    property HintColor;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property OnParentColorChange;
    property Flat: Boolean read GetFlat write SetFlat;
  end;

  TJvEdit = class(TJvCustomEdit)
  published 
    property Caret;
    property DisabledTextColor;
    property DisabledColor;
    property HotTrack;
    property PasswordChar;
    property PopupMenu;
    property ProtectPassword; 
    property EchoMode;
    property InputKeys; 
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
  Math;

constructor TJvCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  FNullPixmap := QPixmap_create(1, 1, 1, QPixmapOptimization_DefaultOptim); 
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
  QPixmap_destroy(FNullPixmap); 
  inherited Destroy;
end;

procedure TJvCustomEdit.Loaded;
begin
  inherited Loaded;
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
  UpdateAutoHint;
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

procedure TJvCustomEdit.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Flat := FHotTrack;
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
    inherited Alignment := FAlignment;
    Invalidate; 
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

function StrFillChar(Ch: Char; Length: Cardinal): string;
begin
  SetLength(Result, Length);
  if Length > 0 then
    FillChar(Result[1], Length, Ch);
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
   // paint Border
    if (BorderStyle = bsSingle) then
      QGraphics.DrawEdge(Canvas, R, esLowered, esLowered, ebRect); 
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
      S := StrFillChar(PasswordChar, Length(Text));
    if not PaintEdit(Self, S, FAlignment, False, {0,} FDisabledTextColor,
      Focused, Flat, Canvas) then
      inherited Paint;
  end;
end;

procedure TJvCustomEdit.TextChanged;
begin
  inherited TextChanged;
  UpdateAutoHint;
end;

procedure TJvCustomEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  UpdateAutoHint;
end;



procedure TJvCustomEdit.CaretChanged(Sender: TObject);
begin
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomEdit.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  FCaret.CreateCaret;
end;

procedure TJvCustomEdit.DoKillFocus(FocusedWnd: HWND);
begin
  FCaret.DestroyCaret;
  inherited DoKillFocus(FocusedWnd);
end;

procedure TJvCustomEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

procedure TJvCustomEdit.DoClearText;
begin
  if not ReadOnly then
    inherited DoClearText;
end;

procedure TJvCustomEdit.DoUndo;
begin
  if not ReadOnly then
    inherited DoUndo;
end;

procedure TJvCustomEdit.DoClipboardCut;
begin
  if not ReadOnly then
    inherited DoClipboardCut;
end;

procedure TJvCustomEdit.DoClipboardPaste;
begin
  if not ReadOnly then
    inherited DoClipboardPaste;
  UpdateEdit;
end;

procedure TJvCustomEdit.SetGroupIndex(Value: Integer);
begin
  FGroupIndex := Value;
  UpdateEdit;
end;

procedure TJvCustomEdit.UpdateEdit;
var
  I: Integer;
begin
  if Assigned(Owner) then
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TJvCustomEdit then
        if ({(Owner.Components[I].Name <> Self.Name)}
          (Owner.Components[I] <> Self) and // (ahuser) this is better and faster
          ((Owner.Components[I] as TJvCustomEdit).GroupIndex <> -1) and
          ((Owner.Components[I] as TJvCustomEdit).GroupIndex = FGroupIndex)) then
          (Owner.Components[I] as TJvCustomEdit).Caption := '';
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



function TJvCustomEdit.GetPasswordChar: Char;
begin  
  Result := FPasswordChar; 
end;

procedure TJvCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  UpdateEdit;
  inherited KeyDown(Key, Shift);
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

function TJvCustomEdit.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu; 
end;

function TJvCustomEdit.GetFlat: Boolean;
begin 
  Result := FFlat;
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

procedure TJvCustomEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  if ClipboardCommands <> Value then
  begin
    inherited SetClipboardCommands(Value);
    ReadOnly := ClipboardCommands <= [caCopy];
  end;
end;

procedure TJvCustomEdit.UpdateAutoHint;
var
  C: TControlCanvas;
  Size: TSize;
begin
  if AutoHint and HandleAllocated then
  begin
    // (p3) empty original Hint indicates that we always want to replace Hint with Text
    if FOldHint = '' then
    begin
      Hint := Text;
      Exit;
    end;
    C := TControlCanvas.Create;
    try
      C.Control := Self;  
        if GetTextExtentPoint32W(C.Handle, PWideChar(Text), Length(Text), Size) then 
        begin
          if (ClientWidth <= Size.cx) then
            Hint := Text
          else
            Hint := FOldHint;
        end
        else
          Hint := FOldHint;
    finally
      C.Free;
    end;
  end;
end;

procedure TJvCustomEdit.SetAutoHint(Value: Boolean);
begin
  if FAutoHint <> Value then
  begin
    if Value then
      FOldHint := Hint
    else
      Hint := FOldHint;
    FAutoHint := Value;
    UpdateAutoHint;
  end;
end;

procedure TJvCustomEdit.Resize;
begin
  inherited Resize;
  UpdateAutoHint;
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

procedure TJvCustomEdit.DoEmptyValueEnter;
begin
  if EmptyValue <> '' then
  begin
    if FIsEmptyValue then
    begin
      Text := '';
      FIsEmptyValue := false;
      if not (csDesigning in ComponentState) then
        Font.Color := FOldFontColor;
    end;
  end;
end;

procedure TJvCustomEdit.DoEmptyValueExit;
begin
  if EmptyValue <> '' then
  begin
    if Text = '' then
    begin
      Text := EmptyValue;
      FIsEmptyValue := true;
      if not (csDesigning in ComponentState) then
      begin
        FOldFontColor := Font.Color;
        Font.Color := FEmptyFontColor;
      end;
    end;
  end;
end;



procedure TJvCustomEdit.InitWidget;

begin
  inherited;
  if Focused then
    DoEmptyValueEnter
  else
    DoEmptyValueExit;
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


end.

