{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQStdDsgnEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Added editors for JvFooter and JvGroupHeader

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQStdDsgnEditors;

interface

uses
  Classes, SysUtils,
  QForms, QControls, Types, QGraphics, QExtCtrls, {Tabs,} QDialogs,
  QExtDlgs, QMenus, QStdCtrls, QImgList, Qt,
  DsnConst,
  RTLConsts, DesignIntf, DesignEditors, DesignMenus,
  ClxEditors, ClxImgEdit;

type
  TJvDateTimeExProperty = class(TDateTimeProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvDateExProperty = class(TDateProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvTimeExProperty = class(TTimeProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

//
// asn: taken from VCLEditors
//
type
{ ICustomPropertyDrawing
  Implementing this interface allows a property editor to take over the object
  inspector's drawing of the name and the value. If paFullWidthName is returned
  by IProperty.GetAttributes then only PropDrawName will be called. Default
  implementation of both these methods are provided in DefaultPropDrawName
  and DefaultPropDrawValue in this unit. }
  ICustomPropertyDrawing = interface
    ['{E1A50419-1288-4B26-9EFA-6608A35F0824}']   // vcl
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

  { ICustomPropertyListDrawing
  Implemention this interface allows a property editor to take over the drawing
  of the drop down list box displayed by the property editor. This is only
  meaningful to implement if the property editor returns paValueList from
  IProperty.GetAttributes. The Value parameter is the result of
  IProperty.GetValue. The implementations ListMeasureWidth and ListMeasureHeight
  can be left blank since the var parameter is filled in to reasonable defaults
  by the object inspector. A default implementation of ListDrawValue is supplied
  in the DefaultPropertyListDrawValue procedure included in this unit }
  ICustomPropertyListDrawing = interface
    ['{BE2B8CF7-DDCA-4D4B-BE26-2396B969F8E0}'] // vcl
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

  TColorPropertyEx = class(TColorProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  public
    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); dynamic;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); dynamic;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); dynamic;
    { CustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); dynamic;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); dynamic;
  end;

  TJvDefaultImageIndexProperty = class(TIntegerProperty)
  protected
    function ImageList: TCustomImageList; virtual;
  public
//    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure DefaultPropertyDrawName(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
procedure DefaultPropertyDrawValue(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
procedure DefaultPropertyListDrawValue(const Value: string; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean);

implementation

uses
  TypInfo, Math,
  JvQTypes, JvQDateTimeForm, JvQDsgnConsts;

//=== TJvDateTimeExProperty ==================================================

procedure TJvDateTimeExProperty.Edit;
var
  D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TFrmSelectDateTimeDlg.SelectDateTime(D, dstDateTime) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TJvDateTimeExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

//=== TJvDateExProperty ======================================================

procedure TJvDateExProperty.Edit;
var
  D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TFrmSelectDateTimeDlg.SelectDateTime(D, dstDate) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TJvDateExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

//=== TJvTimeExProperty ======================================================

procedure TJvTimeExProperty.Edit;
var
  D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now
  else // (p3) we need the date part or we might get a "Must be in ShowCheckBox mode" error 
    D := SysUtils.Date + Frac(D);
  if TFrmSelectDateTimeDlg.SelectDateTime(D, dstTime) then
  begin
    SetFloatValue(Frac(D)); // (p3) only return the time portion
    Designer.Modified;
  end;
end;

function TJvTimeExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

//=== { TJvDefaultImageIndexProperty } =======================================

procedure GetImages(ImageList: TImageList; Index: Integer; Image, Mask: TBitmap);
var
  R: TRect;
begin
  with ImageList do
  begin
    R := Rect(0, 0, Width, Height);
    Image.Width := Width;
    Image.Height := Height;
    Mask.Width := Width;
    Mask.Height := Height;
  end;
  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList.Draw(Image.Canvas, 0, 0, Index);
  end;
  with Mask.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList.Draw(Mask.Canvas, 0, 0, Index, itMask);
  end;
end;

(*
function EditImageIndex(AImageList: TImageList; Index: integer): integer;
var
  I: Integer;
  ListItems: TClxImageListEditor;
begin
  Result := -1;
  ListItems := TClxImageListEditor.CreateImgListEditor(Application, AImageList);
  with ListItems do
    try
      Screen.Cursor := crHourglass;
      try
        if AImageList.Width = 0 then
          FImageXDivOffset := 1
        else
          FImageXDivOffset := MainImage.Width div AImageList.Width;
        if AImageList.Height = 0 then
          FImageYDivOffset := 1
        else
          FImageYDivOffset := MainImage.Height div AImageList.Height;
        if FImageXDivOffset = 0 then
          FImageXDivOffset := 1;
        if FImageYDivOffset = 0 then
          FImageYDivOffset := 1;
        FComponentList := AImageList;
        FImageList.Assign(ComponentList);
        with ImageListView do
        begin
          Assign(ImageList);
          if (Width > 24) or (Height > 24) then StretchImageList(ImageList,
            FImageListView, 24, 24)
          else if (Width < 24) and (Height < 24) then CenterImageList(FImageList,
            FImageListView, 24, 24);
        end;
        ImageList.Clear;
        with ImageBitmap do
        begin
          Height := ImageList.Height;
          Width := ImageList.Width;
        end;
        for I := 0 to ComponentList.Count - 1 do
          with TImageInfo.Create(InfoList, ListItems) do
          begin
            FNew := False;
            FAutoOp := False;
            GetImages(ComponentList, I, FBitmap, FMask);
            TransparentColor := clDefault;
            Change;
          end;
        ActiveControl := ImageView;
        with ImageView do
        begin
          Selected := nil;
          Item := nil;
          if (Items.Count > 0) and (Index >= 0) and (Index < Items.Count)  then
          begin
            Item := Items[Index];
            Selected := Item;
            if Assigned(Item) then
              Item.MakeVisible;
            end;
          end;
        end;
        UpdateImageView;
        SelectImage(Index);
        Help.Top := Apply.Top;
        Apply.Visible := False;
      finally
        Screen.Cursor := crDefault;
      end;
      Caption := Format('Select index from ' + SImageListEditorCaption, [AImageList.Owner.Name, DotSep,
        AImageList.Name]);
      if ShowModal = mrOk then
      begin
        with ListView do
        for i := 0 to items.count - 1 do
        begin
          if Items[i] = Selected then
          begin
            Result := i;
            break;
          end;
        end;
      end
    finally
      Free;
    end;
  end;
end;
*)

function TJvDefaultImageIndexProperty.ImageList: TCustomImageList;
const
  cImageList = 'ImageList';
  cImages = 'Images';
begin
  if TypInfo.GetPropInfo(GetComponent(0), cImageList) <> nil then
    Result := TCustomImageList(TypInfo.GetObjectProp(GetComponent(0), cImageList))
  else
  if TypInfo.GetPropInfo(GetComponent(0), cImages) <> nil then
    Result := TCustomImageList(TypInfo.GetObjectProp(GetComponent(0), cImages))
  else
    Result := nil;
end;

(*
procedure TJvDefaultImageIndexProperty.Edit;
var
  mresult: integer;
begin
  mresult := EditImageIndex(ImagesList, GetValue);
  if mresult >= 0 then
    SetValue(mresult);
end;
*)

function TJvDefaultImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable, paDialog];
end;

(*
function TJvDefaultImageIndexProperty.GetValue: string;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TJvDefaultImageIndexProperty.SetValue(const Value: string);
var
  XValue: Integer;
begin
  try
    XValue := StrToInt(Value);
    SetOrdValue(XValue);
  except
    inherited SetValue(Value);
  end;
end;

procedure TJvDefaultImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  Tmp: TCustomImageList;
  I: Integer;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    for I := 0 to Tmp.Count - 1 do
      Proc(IntToStr(I));
end;
*)

//=== { TColorPropertyEx } ==================================================

procedure DebugMessage(functionname: string; ACanvas: TCanvas);
var
  StdTxt: string;
begin
  StdTxt := functionname + ', ' + ACanvas.ClassName + ': ' ;
  if ACanvas = nil then
    ShowMessage(StdTxt +  'Canvas Not Assigned')
  else if not assigned(ACanvas.Handle) then
    ShowMessage(StdTxt + 'Canvas.Handle Not Assigned')
  else if not QPainter_isActive(ACanvas.Handle) then
    ShowMessage(StdTxt + 'Painter is not active');
end;

procedure TColorPropertyEx.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DebugMessage('PropDrawValue', ACanvas);
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True) // ASelected
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TColorPropertyEx.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red: Byte;
      Green: Byte;
      Blue: Byte;
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
      (TColorQuad(AColor).Green > 192) or
      (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else
    if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;
var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  DebugMessage('ListDrawValue', ACanvas);
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // set things up and do the work
    Brush.Color := StringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top, ARect.Right,
      ARect.Bottom), ASelected);
  end;
end;

procedure TColorPropertyEx.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  DebugMessage('ListMeasureWidth', ACanvas);//: $%p, Handle: $%p', [ACanvas,ACanvas.Handle]));
  if ACanvas <> nil then
    AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

procedure TColorPropertyEx.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implemenation necessary
  DebugMessage('ListMeasureHeight', ACanvas);//: $%p, Handle: $%p', [ACanvas,ACanvas.Handle]));
end;

procedure TColorPropertyEx.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DebugMessage('PropDrawName', ACanvas);//: $%p, Handle: $%p', [ACanvas,ACanvas.Handle]));
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure DefaultPropertyDrawName(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
begin
  Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Prop.GetName);
end;

procedure DefaultPropertyDrawValue(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
begin
  Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Prop.GetVisualValue);
end;

procedure DefaultPropertyListDrawValue(const Value: string; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean);
begin
  Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Value);
end;

end.
