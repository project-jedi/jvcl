{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInspExtraEditors.pas, released on 2001-02-28.

The Initial Developer of the Original Code is Marcel Bestebroer
  <jedi_mbe (at) users (dot) sf (dot) net>.
Portions created by Marcel Bestebroer are Copyright (C) 2000 - 2001 mbeSoft.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com], Markus Spoettl.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvInspExtraEditors;

interface

{ Additional editors for JvInspector. }

uses
  SysUtils, Classes,
  Windows, Graphics, Controls, StdCtrls, ImgList,
  JvInspector;

type
  // In the same spirit as the TJvTypeInfoHelper class in JvInspector.pas
  // we define here a TJvTypeInfoExtraHelper class for the types that
  // are used by the editors in this unit. Please refer to JvInspector.pas
  // for more details on this C++ Builder compatibility issue.
  TJvTypeInfoExtraHelper = class(TJvTypeInfoHelper)
  private
    FTAlignProp: TAlign;
    FTAnchorsProp: TAnchors;
    FTColorProp: TColor;
    FTImageIndexProp: TImageIndex;
  published
    property TAlignProp: TAlign read FTAlignProp;
    property TAnchorsProp: TAnchors read FTAnchorsProp;
    property TColorProp: TColor read FTColorProp;
    property TImageIndexProp: TImageIndex read FTImageIndexProp;
  end;

  { TAlign item editor. Descents from the enumeration item to keep DisplayValue available }
  TJvInspectorAlignItem = class(TJvInspectorEnumItem)
  private
    FUnassignedColor: TColor;
    FNormalColor: TColor;
    FActiveColor: TColor;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintAlignBox(const Align: TAlign; const ACanvas: TCanvas; const ARect: TRect;
      const UseUnassigned: Boolean);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    procedure InitEdit; override;
    class procedure RegisterAsDefaultItem;
    class procedure UnregisterAsDefaultItem;
    property UnassignedColor: TColor read FUnassignedColor;
    property NormalColor: TColor read FNormalColor;
    property ActiveColor: TColor read FActiveColor;
  end;

  { TAnchors item editor. Descents from the set item to keep DisplayValue available }
  TJvInspectorAnchorsItem = class(TJvInspectorSetItem)
  private
    FUnassignedColor: TColor;
    FNormalColor: TColor;
    FActiveColor: TColor;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetItemSetFlags(const Value: TInspectorSetFlags); override;
    procedure PaintAnchorsBox(const Anchors: TAnchors; const ACanvas: TCanvas; const ARect: TRect;
      const UseUnassigned: Boolean);
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    procedure DoneEdit(const CancelEdits: Boolean = False); override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    procedure InitEdit; override;
    class procedure RegisterAsDefaultItem;
    class procedure UnregisterAsDefaultItem;
    property UnassignedColor: TColor read FUnassignedColor;
    property NormalColor: TColor read FNormalColor;
    property ActiveColor: TColor read FActiveColor;
  end;

  { TColor item editor. Will render the color in a box, together with the name/value }
  TJvInspectorColorItem = class(TJvCustomInspectorItem)
  private
    FColors: TStringList;
    FStdColors: TStringList;
    FIncludeStdColors: Boolean;
  protected
    procedure AddStdColor(const S: string);
    function BorderColor(const ABackgroundColor, AInternalColor: TColor): TColor;
    function NameForColor(const Color: TColor): string;
    procedure PaintValue(const Color: TColor; const ColorName: string; const ACanvas: TCanvas;
      const ARect: TRect);
    {$IFDEF VCL}
    procedure DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure DoDrawListItem(Control: TObject; Index: Integer; Rect: TRect;
      State: TOwnerDrawState; var Handled: Boolean); override;
    {$ENDIF VisualCLX}

    procedure DoMeasureListItem(Control: TWinControl; Index: Integer; var Height: Integer); override;
    procedure DoMeasureListItemWidth(Control: TWinControl; Index: Integer; var Width: Integer); override;
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); override;
  public
    constructor Create(const AParent: TJvCustomInspectorItem;
      const AData: TJvCustomInspectorData); override;
    procedure BeforeDestruction; override;
    procedure DrawValue(const ACanvas: TCanvas); override;
    class procedure RegisterAsDefaultItem;
    class procedure UnregisterAsDefaultItem;
    property IncludeStdColors: Boolean read FIncludeStdColors write FIncludeStdColors;
  end;

  { TImageList image index editor. Will render the image next to the value }
  TJvInspectorTImageIndexItem = class(TJvCustomInspectorItem)
  private
    FImageList: TCustomImageList;
  protected
    procedure PaintValue(const ImgNum: Integer; const ImgName: string; const ACanvas: TCanvas;
      const ARect: TRect);
    {$IFDEF VCL}
    procedure DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure DoDrawListItem(Control: TObject; Index: Integer; Rect: TRect;
      State: TOwnerDrawState; var Handled: Boolean); override;
    {$ENDIF VisualCLX}

    procedure DoMeasureListItem(Control: TWinControl; Index: Integer; var Height: Integer); override;
    procedure DoMeasureListItemWidth(Control: TWinControl; Index: Integer; var Width: Integer); override;
    function GetDisplayValue: string; override;
    procedure GetValueList(const Strings: TStrings); override;
    procedure SetDisplayValue(const Value: string); override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
    procedure SetRects(const RectKind: TInspectorPaintRect; Value: TRect); override;
  public
    procedure DrawValue(const ACanvas: TCanvas); override;
    class procedure RegisterAsDefaultItem;
    class procedure UnregisterAsDefaultItem;
    property Images: TCustomImageList read FImageList write FImageList;
  end;

implementation

uses
  TypInfo,
  JclRTTI,
  JvResources;

type
  TOpenInspector = class(TJvCustomInspector);
  TOpenPainter = class(TJvInspectorPainter);

  TColorQuad = packed record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
    Alpha: Byte;
  end;

//=== TJvInspectorAlignItem ==================================================

constructor TJvInspectorAlignItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FUnassignedColor := clGrayText;
  FNormalColor := clWindowText;
  FActiveColor := clBlue;
end;

procedure TJvInspectorAlignItem.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  NewAlign: TAlign;
begin
  if Editing and (Shift = [ssCtrl]) then
  begin
    if Data.IsAssigned then
      NewAlign := TAlign(Data.AsOrdinal)
    else
      NewAlign := alNone;
    case Key of
    VK_UP, VK_NUMPAD8:
      begin
        if NewAlign = alTop then
          NewAlign := alNone
        else
          NewAlign := alTop;
      end;
    VK_RIGHT, VK_NUMPAD6:
    begin
      if NewAlign = alRight then
        NewAlign := alNone
      else
        NewAlign := alRight;
    end;
    VK_DOWN, VK_NUMPAD2:
    begin
      if NewAlign = alBottom then
        NewAlign := alNone
      else
        NewAlign := alBottom;
    end;
    VK_LEFT, VK_NUMPAD4:
    begin
      if NewAlign = alLeft then
        NewAlign := alNone
      else
        NewAlign := alLeft;
    end;
    VK_NUMPAD5, VK_HOME, VK_NUMPAD7:
    begin
      if NewAlign = alClient then
        NewAlign := alNone
      else
        NewAlign := alClient;
    end;
    end;
    case Key of
    VK_UP, VK_NUMPAD8, VK_RIGHT, VK_NUMPAD6, VK_DOWN, VK_NUMPAD2,
    VK_LEFT, VK_NUMPAD4, VK_NUMPAD5, VK_HOME, VK_NUMPAD7:
    begin
      Data.AsOrdinal := Ord(NewAlign);
      Key := 0;
    end;
    else
      inherited EditKeyDown(Sender, Key, Shift);
    end;
  end;
end;

procedure TJvInspectorAlignItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewAlign: TAlign;
  ValueRect: TRect;
begin
  if Editing and (Shift = [ssLeft]) then
  begin
    if Data.IsAssigned then
      NewAlign := TAlign(Data.AsOrdinal)
    else
      NewAlign := alNone;
    ValueRect := Rects[iprValueArea];
    with ValueRect do
    begin
      if PtInRect(Rect(Left + 5, Top, Right - 5, Top + 5), Point(X, Y)) then
      begin
        if NewAlign = alTop then
          NewAlign := alNone
        else
          NewAlign := alTop;
      end
      else
      if PtInRect(Rect(Right - 5, Top + 5, Right, Bottom - 5), Point(X, Y)) then
      begin
        if NewAlign = alRight then
          NewAlign := alNone
        else
          NewAlign := alRight;
      end
      else
      if PtInRect(Rect(Left + 5, Bottom - 5, Right - 5, Bottom), Point(X, Y)) then
      begin
        if NewAlign = alBottom then
          NewAlign := alNone
        else
          NewAlign := alBottom;
      end
      else
      if PtInRect(Rect(Left, Top + 5, Left + 5, Bottom - 5), Point(X, Y)) then
      begin
        if NewAlign = alLeft then
          NewAlign := alNone
        else
          NewAlign := alLeft;
      end
      else
      if PtInRect(ValueRect, Point(X, Y)) then
      begin
        if NewAlign = alClient then
          NewAlign := alNone
        else
          NewAlign := alClient;
      end;
    end;
    Data.AsOrdinal := Ord(NewAlign);
  end;
end;

procedure TJvInspectorAlignItem.PaintAlignBox(const Align: TAlign; const ACanvas: TCanvas;
  const ARect: TRect; const UseUnassigned: Boolean);
var
  NoAlignColor: TColor;

  procedure RenderAlign(const Check: TAlign; const X, Y : Integer);
  begin
    if (Align = alClient) or (Align = Check) then
      ACanvas.Pen.Color := ActiveColor
    else
      ACanvas.Pen.Color := NoAlignColor;
    ACanvas.LineTo(X, Y);
  end;

begin
  if UseUnassigned then
    NoAlignColor := UnassignedColor
  else
    NoAlignColor := NormalColor;
  ACanvas.Pen.Width := 2;

  ACanvas.MoveTo(ARect.Left + 2, ARect.Top + 2);
  RenderAlign(alTop, ARect.Right - 3, ARect.Top + 2);
  RenderAlign(alRight, ARect.Right - 3, ARect.Bottom - 3);
  RenderAlign(alBottom, ARect.Left + 2, ARect.Bottom - 3);
  RenderAlign(alLeft, ARect.Left + 2, ARect.Top + 1);
end;

procedure TJvInspectorAlignItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  SetEditing(False);
end;

procedure TJvInspectorAlignItem.DrawValue(const ACanvas: TCanvas);
var
  IsValid: Boolean;
  Align: TAlign;
  ARect: TRect;

begin
  IsValid := Data.IsInitialized and Data.IsAssigned and Data.HasValue;
  if IsValid then
    Align := TAlign(Data.AsOrdinal)
  else
    Align := alNone;

  if Editing and Data.IsAssigned then
    ACanvas.Brush.Color := clWindow;

  ARect := Rects[iprValueArea];
  ACanvas.FillRect(ARect);
  PaintAlignBox(Align, ACanvas, ARect, not IsValid);
end;

procedure TJvInspectorAlignItem.InitEdit;
begin
  SetEditing(CanEdit);
end;

class procedure TJvInspectorAlignItem.RegisterAsDefaultItem;
begin
  with TJvCustomInspectorData.ItemRegister do
  begin
    if IndexOf(Self) = -1 then
      Add(TJvInspectorTypeInfoRegItem.Create(Self, TypeInfo(TAlign)));
  end;
end;

class procedure TJvInspectorAlignItem.UnregisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Delete(Self);
end;

//=== TJvInspectorColorItem ==================================================

constructor TJvInspectorColorItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FColors := TStringList.Create;
  FStdColors := TStringList.Create;
  GetColorValues(AddStdColor);
  FStdColors.Sort;
  IncludeStdColors := True;
  Flags := [iifVisible, iifValueList, iifAllowNonListValues, iifOwnerDrawListVariable];
end;

procedure TJvInspectorColorItem.AddStdColor(const S: string);
begin
  FStdColors.AddObject(S, TObject(JclStrToTypedInt(S, TypeInfo(TColor))));
end;

function TJvInspectorColorItem.BorderColor(const ABackgroundColor, AInternalColor: TColor): TColor;
var
  BckRGB: TColor;
  ColRGB: TColor;

  function IsLightColor(const RGB: TColor): Boolean;
  begin
    with TColorQuad(RGB) do
      Result := (Red > 192) or (Green > 192) or (Blue > 192);
  end;

begin
  BckRGB := ColorToRGB(ABackgroundColor);
  ColRGB := ColorToRGB(AInternalColor);
  if IsLightColor(BckRGB) and IsLightColor(ColRGB) then
    Result := clBlack
  else
  if not IsLightColor(BckRGB) and not IsLightColor(ColRGB) then
    Result := clWhite
  else
    Result := AInternalColor;
end;

function TJvInspectorColorItem.NameForColor(const Color: TColor): string;
begin
  Result := JclTypedIntToStr(Color, TypeInfo(TColor));
end;

procedure TJvInspectorColorItem.PaintValue(const Color: TColor; const ColorName: string;
  const ACanvas: TCanvas; const ARect: TRect);
var
  TH: Integer;
  BoxRect: TRect;
  bc: TColor;
  pc: TColor;
  txtRect: TRect;
begin
  TH := Rects[iprValue].Bottom - Rects[iprValue].Top - 2;
  BoxRect.Left := ARect.Left + (ARect.Bottom - ARect.Top - TH) div 2;
  BoxRect.Top := ARect.Top + BoxRect.Left - ARect.Left;
  BoxRect.Right := BoxRect.Left + TH;
  BoxRect.Bottom := BoxRect.Top + TH;
  with ACanvas do
  begin
    if Color <> clNone then
    begin
      bc := Brush.Color;
      pc := Pen.Color;
      try
        Brush.Color := Color;
        Pen.Color := BorderColor(bc, Color);
        Rectangle(BoxRect);
      finally
        Pen.Color := pc;
        Brush.Color := bc;
      end;
    end;
   txtRect := ARect;
   txtRect.Left := txtRect.Left + (txtRect.Bottom-txtRect.Top)+ 1;
   TextRect(txtRect, txtRect.Left, BoxRect.Top, ColorName);
  end;
end;

{$IFDEF VCL}
procedure TJvInspectorColorItem.DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvInspectorColorItem.DoDrawListItem(Control: TObject; Index: Integer; Rect: TRect;
      State: TOwnerDrawState; var Handled: Boolean);
{$ENDIF VisualCLX}
begin
  with TListBox(Control) do
  begin
    if odSelected in State then
      Canvas.Brush.Color := clHighlight;
    Canvas.FillRect(Rect);
    Rect.Top := Rect.Top + 1;
    Rect.Bottom := Rect.Bottom - 1;
    PaintValue(TColor(Items.Objects[Index]), Items[Index], Canvas, Rect);
  end;
  {$IFDEF VisualCLX}
  Handled := True;
  {$ENDIF VisualCLX}
end;

procedure TJvInspectorColorItem.DoMeasureListItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  with Rects[iprValueArea] do
    Height := Bottom - Top + 2;
end;

procedure TJvInspectorColorItem.DoMeasureListItemWidth(Control: TWinControl; Index: Integer;
  var Width: Integer);
begin
  with Rects[iprValueArea] do
    Width := Width + Bottom - Top + 2;
end;

function TJvInspectorColorItem.GetDisplayValue: string;
var
  TempSL: TStringList;
  I: Integer;
begin
  TempSL := TStringList.Create;
  try
    GetValueList(TempSL);
    I := TempSL.IndexOfObject(TObject(Data.AsOrdinal));
    if I = -1 then
      Result := JclTypedIntToStr(Data.AsOrdinal, TypeInfo(TColor))
    else
      Result := TempSL[I];
  finally
    TempSL.Free;
  end;
end;

procedure TJvInspectorColorItem.GetValueList(const Strings: TStrings);
var
  TempSL: TStringList;
begin
  TempSL := TStringList.Create;
  try
    if IncludeStdColors then
      TempSL.AddStrings(FStdColors);
    TempSL.AddStrings(FColors);
    DoGetValueList(Strings);
    TempSL.AddStrings(Strings);
    TempSL.Sort;
    Strings.Assign(TempSL);
  finally
    TempSL.Free;
  end;
end;

procedure TJvInspectorColorItem.SetDisplayValue(const Value: string);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    GetValueList(SL);
    I := SL.IndexOf(Value);
    if I > -1 then
      I := Integer(SL.Objects[I])
    else
      I := JclStrToTypedInt(Value, TypeInfo(TColor));
    Data.AsOrdinal := I;
  finally
    SL.Free;
  end;
end;

procedure TJvInspectorColorItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value + [iifValueList, iifAllowNonListValues, iifOwnerDrawListFixed] -
    [iifOwnerDrawListVariable]);
end;

procedure TJvInspectorColorItem.SetRects(const RectKind: TInspectorPaintRect; Value: TRect);
begin
  if RectKind = iprValue then
    Value.Left := Value.Left + (Value.Bottom - Value.Top) + 2;
  inherited SetRects(RectKind, Value);
end;

procedure TJvInspectorColorItem.BeforeDestruction;
begin
  FStdColors.Free;
  FColors.Free;
  inherited BeforeDestruction;
end;

procedure TJvInspectorColorItem.DrawValue(const ACanvas: TCanvas);
var
  Color: TColor;
  S: string;
  ARect: TRect;
  SafeColor: TColor;
begin
  Color := clNone;
  if Data = nil then
    S := RsJvInspItemUnInitialized
  else
  try
    if not Data.IsInitialized then
      S := RsJvInspItemUnInitialized
    else
    if not Data.HasValue then
      S := RsJvInspItemNoValue
    else
    if not Data.IsAssigned then
      S := RsJvInspItemUnassigned
    else
    begin
      S := DisplayValue;
      Color := Data.AsOrdinal;
    end;
  except
    S := RsJvInspItemValueException + ExceptObject.ClassName + ': ' +
      Exception(ExceptObject).Message;
  end;
  ARect := Rects[iprValueArea];
  SafeColor := ACanvas.Brush.Color;
  if Editing then
    ACanvas.Brush.Color := clWindow;
  try
    ACanvas.FillRect(ARect);
    PaintValue(Color, S, ACanvas, ARect);
    if Editing then
      DrawEditor(ACanvas);
  finally
    if Editing then
      ACanvas.Brush.Color := SafeColor;
  end;
end;

class procedure TJvInspectorColorItem.RegisterAsDefaultItem;
begin
  with TJvCustomInspectorData.ItemRegister do
    if IndexOf(Self) = -1 then
      Add(TJvInspectorTypeInfoRegItem.Create(Self, TypeInfo(TColor)));
end;

class procedure TJvInspectorColorItem.UnregisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Delete(Self);
end;

//=== TJvInspectorAnchorsItem ================================================

constructor TJvInspectorAnchorsItem.Create(const AParent: TJvCustomInspectorItem;
  const AData: TJvCustomInspectorData);
begin
  inherited Create(AParent, AData);
  FUnassignedColor := clGrayText;
  FNormalColor := clWindowText;
  FActiveColor := clBlue;
end;

procedure TJvInspectorAnchorsItem.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  NewAnchors: TAnchors;

  procedure Toggle(const Side: TAnchorKind);
  begin
    if Side in NewAnchors then
      Exclude(NewAnchors, Side)
    else
      Include(NewAnchors, Side)
  end;

begin
  if Editing and (Shift = [ssCtrl]) then
  begin
    if Data.IsAssigned then
      Data.GetAsSet(NewAnchors)
    else
      NewAnchors := [];
    case Key of
    VK_UP, VK_NUMPAD8:
      Toggle(akTop);
    VK_RIGHT, VK_NUMPAD6:
      Toggle(akRight);
    VK_DOWN, VK_NUMPAD2:
      Toggle(akBottom);
    VK_LEFT, VK_NUMPAD4:
      Toggle(akLeft);
    VK_NUMPAD5, VK_HOME, VK_NUMPAD7:
      begin
        if NewAnchors <> [] then
          NewAnchors := []
        else
          NewAnchors := [akLeft, akTop, akRight, akBottom];
      end;
    end;
    case Key of
    VK_UP, VK_NUMPAD8, VK_RIGHT, VK_NUMPAD6, VK_DOWN, VK_NUMPAD2, VK_LEFT,
    VK_NUMPAD4, VK_NUMPAD5, VK_HOME, VK_NUMPAD7:
      begin
        Data.SetAsSet(NewAnchors);
        Key := 0;
      end;
    else
      inherited EditKeyDown(Sender, Key, Shift);
    end;
  end;
end;

procedure TJvInspectorAnchorsItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewAnchors: TAnchors;
  OrgAnchors: TAnchors;
  ValueRect: TRect;

  procedure Toggle(const Side: TAnchorKind);
  begin
    if Side in NewAnchors then
      Exclude(NewAnchors, Side)
    else
      Include(NewAnchors, Side);
  end;

begin
  if Editing and (Shift = [ssLeft]) then
  begin
    if Data.IsAssigned then
      Data.GetAsSet(NewAnchors)
    else
      NewAnchors := [];
    OrgAnchors := NewAnchors;
    ValueRect := Rects[iprValueArea];
    with ValueRect do
    begin
      if PtInRect(Rect(Left + 5, Top, Right - 5, Top + 5), Point(X, Y)) then
        Toggle(akTop)
      else
      if PtInRect(Rect(Right - 5, Top + 5, Right, Bottom - 5), Point(X, Y)) then
        Toggle(akRight)
      else
      if PtInRect(Rect(Left + 5, Bottom - 5, Right - 5, Bottom), Point(X, Y)) then
        Toggle(akBottom)
      else
      if PtInRect(Rect(Left, Top + 5, Left + 5, Bottom - 5), Point(X, Y)) then
        Toggle(akLeft)
      else
      if PtInRect(ValueRect, Point(X, Y)) then
      begin
        if NewAnchors <> [] then
          NewAnchors := []
        else
          NewAnchors := [akLeft, akTop, akRight, akBottom];
      end;
    end;
    if OrgAnchors <> NewAnchors then
      Data.SetAsSet(NewAnchors);
  end;
end;

procedure TJvInspectorAnchorsItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value);
end;

procedure TJvInspectorAnchorsItem.SetItemSetFlags(const Value: TInspectorSetFlags);
begin
  inherited SetItemSetFlags(Value - [isfCreateMemberItems] + [isfEditString]);
end;

procedure TJvInspectorAnchorsItem.PaintAnchorsBox(const Anchors: TAnchors; const ACanvas: TCanvas;
  const ARect: TRect; const UseUnassigned: Boolean);
var
  NoAnchorsColor: TColor;

  procedure RenderAnchors(const Check: TAnchorKind; const X, Y : Integer);
  begin
    if Check in Anchors then
      ACanvas.Pen.Color := ActiveColor
    else
      ACanvas.Pen.Color := NoAnchorsColor;
    ACanvas.LineTo(X, Y);
  end;

begin
  if UseUnassigned then
    NoAnchorsColor := UnassignedColor
  else
    NoAnchorsColor := NormalColor;
  ACanvas.Pen.Width := 2;

  ACanvas.MoveTo(ARect.Left + 2, ARect.Top + 2);
  RenderAnchors(akTop, ARect.Right - 3, ARect.Top + 2);
  RenderAnchors(akRight, ARect.Right - 3, ARect.Bottom - 3);
  RenderAnchors(akBottom, ARect.Left + 2, ARect.Bottom - 3);
  RenderAnchors(akLeft, ARect.Left + 2, ARect.Top + 1);
end;

procedure TJvInspectorAnchorsItem.DoneEdit(const CancelEdits: Boolean = False);
begin
  SetEditing(False);
end;

procedure TJvInspectorAnchorsItem.DrawValue(const ACanvas: TCanvas);
var
  IsValid: Boolean;
  Anchors: TAnchors;
  ARect: TRect;
begin
  IsValid := Data.IsInitialized and Data.IsAssigned and Data.HasValue;
  if IsValid then
    Data.GetAsSet(Anchors)
  else
    Anchors := [];

  if Editing and Data.IsAssigned then
    ACanvas.Brush.Color := clWindow;

  ARect := Rects[iprValueArea];
  ACanvas.FillRect(ARect);
  PaintAnchorsBox(Anchors, ACanvas, ARect, not IsValid);
end;

procedure TJvInspectorAnchorsItem.InitEdit;
begin
  SetEditing(CanEdit);
end;

class procedure TJvInspectorAnchorsItem.RegisterAsDefaultItem;
begin
  with TJvCustomInspectorData.ItemRegister do
    if IndexOf(Self) = -1 then
      Add(TJvInspectorTypeInfoRegItem.Create(Self, TypeInfo(TAnchors)));
end;

class procedure TJvInspectorAnchorsItem.UnregisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Delete(Self);
end;

//=== TJvInspectorTImageIndexItem ============================================

procedure TJvInspectorTImageIndexItem.PaintValue(const ImgNum: Integer; const ImgName: string;
  const ACanvas: TCanvas; const ARect: TRect);
var
  TH: Integer;
  BoxRect: TRect;
  Bmp: TBitmap;
begin
  TH := Rects[iprValue].Bottom - Rects[iprValue].Top - 2;
  BoxRect.Left := ARect.Left + (ARect.Bottom - ARect.Top - TH) div 2;
  BoxRect.Top := ARect.Top + BoxRect.Left - ARect.Left;
  BoxRect.Right := BoxRect.Left + TH;
  BoxRect.Bottom := BoxRect.Top + TH;
  with ACanvas do
  begin
    if (ImgNum > -1) and (Images <> nil) and (ImgNum < Images.Count) then
    begin
      Bmp := TBitmap.Create;
      try
        Images.GetBitmap(ImgNum, Bmp);
        StretchDraw(BoxRect, Bmp);
      finally
        Bmp.Free;
      end;
    end;
    TextOut(ARect.Left + (ARect.Bottom - ARect.Top) + 1, BoxRect.Top, ImgName);
  end;
end;

{$IFDEF VCL}
procedure TJvInspectorTImageIndexItem.DoDrawListItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvInspectorTImageIndexItem.DoDrawListItem(Control: TObject; Index: Integer; Rect: TRect;
      State: TOwnerDrawState; var Handled: Boolean);
{$ENDIF VisualCLX}
begin
  with TListBox(Control) do
  begin
    if odSelected in State then
      Canvas.Brush.Color := clHighlight;
    Canvas.FillRect(Rect);
    Rect.Top := Rect.Top + 1;
    Rect.Bottom := Rect.Bottom - 1;
    PaintValue(Integer(Items.Objects[Index]), Items[Index], Canvas, Rect);
  end;
  {$IFDEF VisualCLX}
  Handled := True;
  {$ENDIF VisualCLX}
end;

procedure TJvInspectorTImageIndexItem.DoMeasureListItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  with Rects[iprValueArea] do
    Height := Bottom - Top + 2;
end;

procedure TJvInspectorTImageIndexItem.DoMeasureListItemWidth(Control: TWinControl; Index: Integer;
  var Width: Integer);
begin
  with Rects[iprValueArea] do
    Width := Width + Bottom - Top + 2;
end;

function TJvInspectorTImageIndexItem.GetDisplayValue: string;
begin
  Result := JclTypedIntToStr(Integer(Data.AsOrdinal), Data.TypeInfo);
end;

procedure TJvInspectorTImageIndexItem.GetValueList(const Strings: TStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.AddObject('-1', TObject(-1000));
    for I := 0 to FImageList.Count - 1 do
      Strings.AddObject(IntToStr(I), TObject(I));
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvInspectorTImageIndexItem.SetDisplayValue(const Value: string);
var
  TmpOrd: Integer;
begin
  TmpOrd := JclStrToTypedInt(Value, Data.TypeInfo);
  if (JclTypeInfo(Data.TypeInfo) as IJclOrdinalRangeTypeInfo).OrdinalType = otULong then
    Data.AsOrdinal := Cardinal(TmpOrd)
  else
    Data.AsOrdinal := TmpOrd;
end;

procedure TJvInspectorTImageIndexItem.SetFlags(const Value: TInspectorItemFlags);
begin
  inherited SetFlags(Value + [iifValueList, iifAllowNonListValues, iifOwnerDrawListVariable] -
    [iifOwnerDrawListFixed]);
end;

procedure TJvInspectorTImageIndexItem.SetRects(const RectKind: TInspectorPaintRect; Value: TRect);
begin
  if RectKind = iprValue then
    Value.Left := Value.Left + (Value.Bottom  -Value.Top) + 2;
  inherited SetRects(RectKind, Value);
end;

procedure TJvInspectorTImageIndexItem.DrawValue(const ACanvas: TCanvas);
var
  Idx: Integer;
  S: string;
  ARect: TRect;
  SafeColor: TColor;
begin
  Idx := -1;
  if Data = nil then
    S := RsJvInspItemUnInitialized
  else
  try
    if not Data.IsInitialized then
      S := RsJvInspItemUnInitialized
    else
    if not Data.HasValue then
      S := RsJvInspItemNoValue
    else
    if not Data.IsAssigned then
      S := RsJvInspItemUnassigned
    else
    begin
      S := DisplayValue;
      Idx := Data.AsOrdinal;
    end;
  except
    S := RsJvInspItemValueException + ExceptObject.ClassName + ': ' +
      Exception(ExceptObject).Message;
  end;
  ARect := Rects[iprValueArea];
  SafeColor := ACanvas.Brush.Color;
  if Editing then
    ACanvas.Brush.Color := clWindow;
  try
    ACanvas.FillRect(ARect);
    PaintValue(Idx, S, ACanvas, ARect);
    if Editing then
      DrawEditor(ACanvas);
  finally
    if Editing then
      ACanvas.Brush.Color := SafeColor;
  end;
end;

class procedure TJvInspectorTImageIndexItem.RegisterAsDefaultItem;
begin
  with TJvCustomInspectorData.ItemRegister do
  begin
    if IndexOf(Self) = -1 then
      Add(TJvInspectorTypeInfoRegItem.Create(Self, TypeInfo(TImageIndex)));
  end;
end;

class procedure TJvInspectorTImageIndexItem.UnregisterAsDefaultItem;
begin
  TJvCustomInspectorData.ItemRegister.Delete(Self);
end;

initialization
  // Register our Extra TypeInfo helper class for BCB
  RegisterTypeInfoHelper(TJvTypeInfoExtraHelper);

end.
