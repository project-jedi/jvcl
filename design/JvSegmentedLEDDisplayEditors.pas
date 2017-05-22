{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSegmentedLEDDisplayEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSegmentedLEDDisplayEditors;

{$I jvcl.inc}

interface

uses
  Classes, Graphics, Menus, Windows,
  DesignEditors, DesignIntf, DesignMenus, VCLEditors, 
  JvColorEditor, JvSegmentedLEDDisplay;

type
  TJvTClassProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
  end;

  TJvSegmentedLEDDigitClassProperty = class(TJvTClassProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvSegmentedLEDDisplayEditor = class(TDefaultEditor)
  protected
    function Display: TJvCustomSegmentedLEDDisplay;
    procedure AddDigit;
    procedure RemoveDigit;
    function DigitCount: Integer;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  TJvUnlitColorProperty = class(TColorProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
    procedure ICustomPropertyListDrawing.ListDrawValue = ListDrawValue;
    procedure ICustomPropertyDrawing.PropDrawValue = PropDrawValue;
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); 
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

implementation

uses
  SysUtils,
  JclRTTI,
  JvSegmentedLEDDisplayMappingForm, JvDsgnConsts;

const
  cDefaultBackground = 'clDefaultBackground';
  cDefaultLitColor = 'clDefaultLitColor';

//=== { TJvTClassProperty } ==================================================

function TJvTClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TJvTClassProperty.GetName: string;
begin
  Result := inherited GetName;
  if AnsiSameStr(Copy(Result, Length(Result) - 3, 4), 'Name') then
    SetLength(Result, Length(Result) - 4);
end;

//=== { TJvSegmentedLEDDigitClassProperty } ==================================

procedure TJvSegmentedLEDDigitClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with DigitClassList.LockList do
    try
      for I := 0 to Count - 1 do
        Proc(TClass(Items[I]).ClassName);
    finally
      DigitClassList.UnlockList;
    end;
end;

//=== { TJvSegmentedLEDDisplayEditor } =======================================

type
  TOpenDisplay = class(TJvCustomSegmentedLEDDisplay);

function TJvSegmentedLEDDisplayEditor.Display: TJvCustomSegmentedLEDDisplay;
begin
  Result := TJvCustomSegmentedLEDDisplay(Component);
end;

procedure TJvSegmentedLEDDisplayEditor.AddDigit;
begin
  TOpenDisplay(Display).Digits.Add;
  Designer.Modified;
end;

procedure TJvSegmentedLEDDisplayEditor.RemoveDigit;
begin
  TOpenDisplay(Display).Digits.Delete(DigitCount - 1);
  Designer.Modified;
end;

function TJvSegmentedLEDDisplayEditor.DigitCount: Integer;
begin
  Result := TOpenDisplay(Display).Digits.Count;
end;

procedure TJvSegmentedLEDDisplayEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      AddDigit;
    1:
      RemoveDigit;
    3:
      EditSLDMapping(Display, Designer);
  end;
end;

function TJvSegmentedLEDDisplayEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsAddDigit;
    1:
      Result := RsRemoveDigit;
    2:
      Result := '-'; // do not localize
    3:
      Result := RsEditMappingEllipsis;
  end;
end;

function TJvSegmentedLEDDisplayEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure TJvSegmentedLEDDisplayEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  if (Index = 1) and (DigitCount = 0) then
    AItem.Enabled := False;
  if (Index = 0) and (TOpenDisplay(Display).DigitClass = nil) then
    AItem.Enabled := False;
end;

//=== { TJvUnlitColorProperty } ==============================================

function TJvUnlitColorProperty.GetValue: string;
begin
  case GetOrdValue of
    clDefaultBackground:
      Result := cDefaultBackground;
    clDefaultLitColor:
      Result := cDefaultLitColor;
  else
    Result := inherited GetValue;
  end;
end;

procedure TJvUnlitColorProperty.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
  Proc(cDefaultBackground);
  Proc(cDefaultLitColor);
end;

procedure TJvUnlitColorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToUnlitColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

procedure TJvUnlitColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  LRight: Integer;
  OldPenColor, OldBrushColor, TmpColor: TColor;
  TmpRect: TRect;
begin
  LRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
    try
      OldPenColor := Pen.Color;
      OldBrushColor := Brush.Color;
      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, LRight, ARect.Bottom);
      IdentToUnlitColor(Value, Integer(TmpColor));
      Brush.Color := TmpColor;
      Pen.Color := JvColorToBorderColor(ColorToRGB(Brush.Color), ASelected);
      Rectangle(ARect.Left + 1, ARect.Top + 1, LRight - 1, ARect.Bottom - 1);
      Brush.Color := OldBrushColor;
      Pen.Color := OldPenColor;
    finally
      TmpRect := ARect;
      TmpRect.Left := LRight;
      ACanvas.TextRect(TmpRect, TmpRect.Left + 1, TmpRect.Top + 1, Value);
    end;
end;

procedure TJvUnlitColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True {ASelected})
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

end.