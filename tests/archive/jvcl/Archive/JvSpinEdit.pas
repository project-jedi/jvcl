{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpinEdit.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): _________________________________.

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSpinEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Forms, JvUpDown,
  JvEdit, Graphics, Dialogs, ComCtrls, JvComponent;

type
  TJvSpinEdit = class(TJvCustomEdit)
  private
    FEdit: TJvEdit;
    FUpDown: TJvUpDown;
    FOnChange: TNotifyEvent;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPosition: Integer;
    function GetThousands: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetThousands(const Value: Boolean);
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetHotTrack: Boolean;
    procedure SetHotTrack(const Value: Boolean);
    function GetTabStop: Boolean;
    procedure SetTabStop(const Value: Boolean);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
  protected
    procedure DoUpClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEditChange(Sender: TObject);
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property Enabled;
    property TabOrder;
    property Anchors;
    property Color:TColor read GetColor write SetColor;
    property Font:TFont read GetFont write SetFont;
    property Max:Integer read GetMax write SetMax default 100;
    property Min:Integer read GetMin write SetMin default 0;
    property Value:Integer read GetPosition write SetPosition default 0;
    property Thousands:Boolean read GetThousands write SetThousands default false;
    property Alignment:TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Text:string read GetText write SetText;
    property HotTrack:Boolean read GetHotTrack write SetHotTrack default false;
    property TabStop:Boolean read GetTabStop write SetTabStop;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{**************************************************************}
constructor TJvSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csSetCaption];

  Height := 21;
  Width := 121;
  BorderStyle := bsSingle;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  inherited Color := clBtnFace;

  FEdit := TJvEdit.Create(self);
  FEdit.Parent := self;
  FEdit.TabStop := true;
  FEdit.BorderStyle := bsNone;
 {$IFDEF COMPILER6_UP}
  FEdit.BevelInner := bvSpace;
  FEdit.BevelOuter := bvNone;
  FEdit.BevelKind := bkFlat;
{$ENDIF}


  FEdit.OnExit := DoEditChange;
  FEdit.OnEnter := DoEditChange;
  FEdit.TabOrder := 0;
  FEdit.Align := alClient;

  FUpDown := TJvUpDown.Create(self);
  FUpDown.Parent := self;
  FUpDown.Align := alRight;
  FUpDown.Thousands := false;
  FUpDown.TabStop := false;
  FUpDown.OnMouseDown := DoUpClick;
  FUpDown.HotTrack :=  true;
  FUpDown.AlignButton := abNone;
  FUpDown.Associate := FEdit;

  inherited TabStop := false;
end;
{**************************************************************}
destructor TJvSpinEdit.Destroy;
begin
  FEdit.Free;
  FUpDown.Free;
  inherited;
end;
{**************************************************************}
procedure TJvSpinEdit.DoEditChange(Sender: TObject);
begin
  if Thousands and (FUpDown.Position > 999) then
    FEdit.Text := FloatToStrF(FUpDown.Position, ffNumber, 18, 0)
  else
    FEdit.Text := IntToStr(FUpDown.Position);
  if Assigned(FOnChange) then
    FOnChange(self);
end;
{**************************************************************}
procedure TJvSpinEdit.DoUpClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FEdit.SetFocus;
end;
{**************************************************************}
function TJvSpinEdit.GetAlignment: TAlignment;
begin
  result := FEdit.Alignment;
end;
{**************************************************************}
function TJvSpinEdit.GetColor: TColor;
begin
  result := FEdit.Color;
end;
{**************************************************************}
function TJvSpinEdit.GetFont: TFont;
begin
  result := FEdit.Font;
end;
{**************************************************************}
function TJvSpinEdit.GetHotTrack: Boolean;
begin
  result := FUpDown.HotTrack;
end;
{**************************************************************}
function TJvSpinEdit.GetMax: Integer;
begin
  result := FUpDown.Max;
end;
{**************************************************************}
function TJvSpinEdit.GetMin: Integer;
begin
  result := FUpDown.Min;
end;
{**************************************************************}
function TJvSpinEdit.GetPosition: Integer;
begin
  result := FUpDown.Position;
end;
{**************************************************************}
function TJvSpinEdit.GetTabStop: Boolean;
begin
  result := FEdit.TabStop;
end;
{**************************************************************}
function TJvSpinEdit.GetText: string;
begin
  result := FEdit.Text;
end;
{**************************************************************}
function TJvSpinEdit.GetThousands: Boolean;
begin
  result := FUpDown.Thousands;
end;
{**************************************************************}
procedure TJvSpinEdit.SetAlignment(const Value: TAlignment);
begin
  FEdit.Alignment := Value;
end;
{**************************************************************}
procedure TJvSpinEdit.SetColor(const Value: TColor);
begin
  FEdit.Color := Value;
end;
{**************************************************************}
procedure TJvSpinEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FEdit.Enabled := Value;
  FUpDown.Enabled := Value;
end;
{**************************************************************}
procedure TJvSpinEdit.SetFont(const Value: TFont);
begin
  FEdit.Font.Assign(Value);
end;
{**************************************************************}
procedure TJvSpinEdit.SetHotTrack(const Value: Boolean);
begin
  FUpDown.HotTrack := Value;
  Realign;
end;
{**************************************************************}
procedure TJvSpinEdit.SetMax(const Value: Integer);
begin
  FUpDown.Max := Value;
end;
{**************************************************************}
procedure TJvSpinEdit.SetMin(const Value: Integer);
begin
  FUpDown.Min := Value;
end;
{**************************************************************}
procedure TJvSpinEdit.SetPosition(const Value: Integer);
begin
  FUpDown.Position := Value;
  if Thousands and (FUpDown.Position > 999) then
    FEdit.Text := FloatToStrF(FUpDown.Position, ffNumber, 18, 0)
  else
    FEdit.Text := IntToStr(FUpDown.Position);
end;
{**************************************************************}
procedure TJvSpinEdit.SetTabStop(const Value: Boolean);
begin
  FEdit.TabStop := Value;
end;
{**************************************************************}
procedure TJvSpinEdit.SetText(const Value: string);
begin
  FUpDown.Position := StrToIntDef(Value,FUpDown.Position);
  if Thousands and (FUpDown.Position > 999) then
    FEdit.Text := FloatToStrF(FUpDown.Position, ffNumber, 18, 0)
  else
    FEdit.Text := IntToStr(FUpDown.Position);
end;
{**************************************************************}
procedure TJvSpinEdit.SetThousands(const Value: Boolean);
begin
  FUpDown.Thousands := Value;
end;
{**************************************************************}
end.
