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

The Original Code is: JvGroupBox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQGroupBox;

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}  
  Types, Qt, QWindows, 
  QGraphics, QControls, QForms, QStdCtrls,
  JvQThemes, JvQExControls, JvQExStdCtrls;

type
  TJvGroupBox = class(TJvExGroupBox, IJvDenySubClassing)
  private
    FOnHotKey: TNotifyEvent;
    FPropagateEnable: Boolean;
    procedure SetPropagateEnable(const Value: Boolean);
  protected
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure EnabledChanged; override;
    procedure DoHotKey; dynamic;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property HintColor; 
    property PropagateEnable: Boolean read FPropagateEnable write SetPropagateEnable default False;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnHotKey: TNotifyEvent read FOnHotKey write FOnHotKey;
  end;

implementation

uses
  Math;

constructor TJvGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPropagateEnable := False;
  ControlStyle := ControlStyle + [csAcceptsControls]; 
end;

procedure TJvGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint; 
  LastBkMode: Integer; 
begin 
  with Canvas do
  begin 
    LastBkMode := GetBkMode(Handle);
    try
      Font := Self.Font;
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height); 
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;  
        QWindows.FrameRect(Canvas, R); 
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;  
      QWindows.FrameRect(Canvas, R); 
      if Text <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        // calculate text rect
        SetBkMode(Handle, OPAQUE);  
        DrawText(Canvas, Text, Length(Text), R, Flags or DT_CALCRECT); 
        Brush.Color := Color;
        if not Enabled then
        begin
          OffsetRect(R, 1, 1);
          Font.Color := clBtnHighlight;  
          DrawText(Canvas, Text, Length(Text), R, Flags); 
          OffsetRect(R, -1, -1);
          Font.Color := clBtnShadow;
          SetBkMode(Handle, TRANSPARENT);  
          DrawText(Canvas, Text, Length(Text), R, Flags); 
        end
        else  
          DrawText(Canvas, Text, Length(Text), R, Flags); 
      end;
    finally
      SetBkMode(Handle, LastBkMode); 
      Stop; 
    end;
  end;
end;

function TJvGroupBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := inherited WantKey(Key, Shift, KeyText);
  if Result then
    DoHotKey;
end;

procedure TJvGroupBox.EnabledChanged;
var
  I: Integer;
begin
  inherited EnabledChanged;
  if PropagateEnable then
    for I := 0 to ControlCount - 1 do
      Controls[I].Enabled := Enabled;
  Invalidate;
end;

procedure TJvGroupBox.DoHotKey;
begin
  if Assigned(FOnHotKey) then
    FOnHotKey(Self);
end;

procedure TJvGroupBox.SetPropagateEnable(const Value: Boolean);
var
  I: Integer;
begin
  FPropagateEnable := Value;
  for I := 0 to ControlCount - 1 do
    Controls[I].Enabled := Enabled;
end;

end.

