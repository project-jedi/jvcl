{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Buttons.pas, released on 2005-02-11.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_Buttons;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Windows, Classes, Graphics, Buttons;

{ TSpeedButtonActionLink }

{ TSpeedButton }

{ constructor Create(AOwner: TComponent) }

procedure TSpeedButton_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSpeedButton.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Click; }

procedure TSpeedButton_Click(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Click;
end;

{ property Read AllowAllUp: Boolean }

procedure TSpeedButton_Read_AllowAllUp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).AllowAllUp;
end;

{ property Write AllowAllUp(Value: Boolean) }

procedure TSpeedButton_Write_AllowAllUp(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).AllowAllUp := Value;
end;

{ property Read GroupIndex: Integer }

procedure TSpeedButton_Read_GroupIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).GroupIndex;
end;

{ property Write GroupIndex(Value: Integer) }

procedure TSpeedButton_Write_GroupIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).GroupIndex := Value;
end;

{ property Read Down: Boolean }

procedure TSpeedButton_Read_Down(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).Down;
end;

{ property Write Down(Value: Boolean) }

procedure TSpeedButton_Write_Down(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Down := Value;
end;

{ property Read Flat: Boolean }

procedure TSpeedButton_Read_Flat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).Flat;
end;

{ property Write Flat(Value: Boolean) }

procedure TSpeedButton_Write_Flat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Flat := Value;
end;

{ property Read Glyph: TBitmap }

procedure TSpeedButton_Read_Glyph(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSpeedButton(Args.Obj).Glyph);
end;

{ property Write Glyph(Value: TBitmap) }

procedure TSpeedButton_Write_Glyph(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Glyph := V2O(Value) as TBitmap;
end;

{ property Read Layout: TButtonLayout }

procedure TSpeedButton_Read_Layout(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).Layout;
end;

{ property Write Layout(Value: TButtonLayout) }

procedure TSpeedButton_Write_Layout(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Layout := Value;
end;

{ property Read Margin: Integer }

procedure TSpeedButton_Read_Margin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).Margin;
end;

{ property Write Margin(Value: Integer) }

procedure TSpeedButton_Write_Margin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Margin := Value;
end;

{ property Read NumGlyphs: TNumGlyphs }

procedure TSpeedButton_Read_NumGlyphs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).NumGlyphs;
end;

{ property Write NumGlyphs(Value: TNumGlyphs) }

procedure TSpeedButton_Write_NumGlyphs(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).NumGlyphs := Value;
end;

{ property Read Spacing: Integer }

procedure TSpeedButton_Read_Spacing(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).Spacing;
end;

{ property Write Spacing(Value: Integer) }

procedure TSpeedButton_Write_Spacing(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Spacing := Value;
end;

{ property Read Transparent: Boolean }

procedure TSpeedButton_Read_Transparent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSpeedButton(Args.Obj).Transparent;
end;

{ property Write Transparent(Value: Boolean) }

procedure TSpeedButton_Write_Transparent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSpeedButton(Args.Obj).Transparent := Value;
end;

{ TBitBtn }

{ constructor Create(AOwner: TComponent) }

procedure TBitBtn_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBitBtn.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Click; }

procedure TBitBtn_Click(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Click;
end;

{ property Read Glyph: TBitmap }

procedure TBitBtn_Read_Glyph(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBitBtn(Args.Obj).Glyph);
end;

{ property Write Glyph(Value: TBitmap) }

procedure TBitBtn_Write_Glyph(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Glyph := V2O(Value) as TBitmap;
end;

{ property Read Kind: TBitBtnKind }

procedure TBitBtn_Read_Kind(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitBtn(Args.Obj).Kind;
end;

{ property Write Kind(Value: TBitBtnKind) }

procedure TBitBtn_Write_Kind(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Kind := Value;
end;

{ property Read Layout: TButtonLayout }

procedure TBitBtn_Read_Layout(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitBtn(Args.Obj).Layout;
end;

{ property Write Layout(Value: TButtonLayout) }

procedure TBitBtn_Write_Layout(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Layout := Value;
end;

{ property Read Margin: Integer }

procedure TBitBtn_Read_Margin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitBtn(Args.Obj).Margin;
end;

{ property Write Margin(Value: Integer) }

procedure TBitBtn_Write_Margin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Margin := Value;
end;

{ property Read NumGlyphs: TNumGlyphs }

procedure TBitBtn_Read_NumGlyphs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitBtn(Args.Obj).NumGlyphs;
end;

{ property Write NumGlyphs(Value: TNumGlyphs) }

procedure TBitBtn_Write_NumGlyphs(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).NumGlyphs := Value;
end;

{ property Read Style: TButtonStyle }

procedure TBitBtn_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitBtn(Args.Obj).Style;
end;

{ property Write Style(Value: TButtonStyle) }

procedure TBitBtn_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Style := Value;
end;

{ property Read Spacing: Integer }

procedure TBitBtn_Read_Spacing(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBitBtn(Args.Obj).Spacing;
end;

{ property Write Spacing(Value: Integer) }

procedure TBitBtn_Write_Spacing(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBitBtn(Args.Obj).Spacing := Value;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cButtons = 'Buttons';
begin
  with JvInterpreterAdapter do
  begin
    { TSpeedButtonActionLink }
    {$IFDEF COMPILER7_UP}
    AddClass(cButtons, TSpeedButtonActionLink, 'TSpeedButtonActionLink');
    {$ENDIF COMPILER7_UP}
    { TSpeedButton }
    AddClass(cButtons, TSpeedButton, 'TSpeedButton');
    AddGet(TSpeedButton, 'Create', TSpeedButton_Create, 1, [varEmpty], varObject);
    AddGet(TSpeedButton, 'Click', TSpeedButton_Click, 0, [varEmpty], varObject);
    AddGet(TSpeedButton, 'AllowAllUp', TSpeedButton_Read_AllowAllUp, 0, [varEmpty], varBoolean);
    AddSet(TSpeedButton, 'AllowAllUp', TSpeedButton_Write_AllowAllUp, 0, [varBoolean]);
    AddGet(TSpeedButton, 'GroupIndex', TSpeedButton_Read_GroupIndex, 0, [varEmpty], varInteger);
    AddSet(TSpeedButton, 'GroupIndex', TSpeedButton_Write_GroupIndex, 0, [varInteger]);
    AddGet(TSpeedButton, 'Down', TSpeedButton_Read_Down, 0, [varEmpty], varBoolean);
    AddSet(TSpeedButton, 'Down', TSpeedButton_Write_Down, 0, [varBoolean]);
    AddGet(TSpeedButton, 'Flat', TSpeedButton_Read_Flat, 0, [varEmpty], varBoolean);
    AddSet(TSpeedButton, 'Flat', TSpeedButton_Write_Flat, 0, [varBoolean]);
    AddGet(TSpeedButton, 'Glyph', TSpeedButton_Read_Glyph, 0, [varEmpty], varEmpty);
    AddSet(TSpeedButton, 'Glyph', TSpeedButton_Write_Glyph, 0, [varEmpty]);
    AddGet(TSpeedButton, 'Layout', TSpeedButton_Read_Layout, 0, [varEmpty], varEmpty);
    AddSet(TSpeedButton, 'Layout', TSpeedButton_Write_Layout, 0, [varEmpty]);
    AddGet(TSpeedButton, 'Margin', TSpeedButton_Read_Margin, 0, [varEmpty], varInteger);
    AddSet(TSpeedButton, 'Margin', TSpeedButton_Write_Margin, 0, [varInteger]);
    AddGet(TSpeedButton, 'NumGlyphs', TSpeedButton_Read_NumGlyphs, 0, [varEmpty], varEmpty);
    AddSet(TSpeedButton, 'NumGlyphs', TSpeedButton_Write_NumGlyphs, 0, [varEmpty]);
    AddGet(TSpeedButton, 'Spacing', TSpeedButton_Read_Spacing, 0, [varEmpty], varInteger);
    AddSet(TSpeedButton, 'Spacing', TSpeedButton_Write_Spacing, 0, [varInteger]);
    AddGet(TSpeedButton, 'Transparent', TSpeedButton_Read_Transparent, 0, [varEmpty], varBoolean);
    AddSet(TSpeedButton, 'Transparent', TSpeedButton_Write_Transparent, 0, [varBoolean]);
    { TBitBtn }
    AddClass(cButtons, TBitBtn, 'TBitBtn');
    AddGet(TBitBtn, 'Create', TBitBtn_Create, 1, [varEmpty], varBoolean);
    AddGet(TBitBtn, 'Click', TBitBtn_Click, 0, [varEmpty], varBoolean);
    AddGet(TBitBtn, 'Glyph', TBitBtn_Read_Glyph, 0, [varEmpty], varEmpty);
    AddSet(TBitBtn, 'Glyph', TBitBtn_Write_Glyph, 0, [varEmpty]);
    AddGet(TBitBtn, 'Kind', TBitBtn_Read_Kind, 0, [varEmpty], varEmpty);
    AddSet(TBitBtn, 'Kind', TBitBtn_Write_Kind, 0, [varEmpty]);
    AddGet(TBitBtn, 'Layout', TBitBtn_Read_Layout, 0, [varEmpty], varEmpty);
    AddSet(TBitBtn, 'Layout', TBitBtn_Write_Layout, 0, [varEmpty]);
    AddGet(TBitBtn, 'Margin', TBitBtn_Read_Margin, 0, [varEmpty], varInteger);
    AddSet(TBitBtn, 'Margin', TBitBtn_Write_Margin, 0, [varInteger]);
    AddGet(TBitBtn, 'NumGlyphs', TBitBtn_Read_NumGlyphs, 0, [varEmpty], varEmpty);
    AddSet(TBitBtn, 'NumGlyphs', TBitBtn_Write_NumGlyphs, 0, [varEmpty]);
    AddGet(TBitBtn, 'Style', TBitBtn_Read_Style, 0, [varEmpty], varEmpty);
    AddSet(TBitBtn, 'Style', TBitBtn_Write_Style, 0, [varEmpty]);
    AddGet(TBitBtn, 'Spacing', TBitBtn_Read_Spacing, 0, [varEmpty], varInteger);
    AddSet(TBitBtn, 'Spacing', TBitBtn_Write_Spacing, 0, [varInteger]);
  end;
  RegisterClasses([TSpeedButton, TBitBtn]);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.