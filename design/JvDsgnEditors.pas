{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnEditors.PAS, released on 2002-05-26.

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

unit JvDsgnEditors;

{ Various property editors }

interface

uses
  Windows, Forms, Controls, Graphics, ExtCtrls, Dialogs,
  ExtDlgs, Menus, StdCtrls, ImgList,
  {$IFDEF VCL}
  Tabs,
  {$ENDIF VCL}
  ImgEdit, DsnConst,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$IFDEF VCL}
  FiltEdit,
  {$ENDIF VCL}
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF COMPILER6_UP}
  Classes, SysUtils;

{$IFDEF VisualCLX}
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
    ['{E1A50419-1288-4B26-9EFA-6608A35F0824}']
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

{ ICustomPropertyDrawing
  Implemention this interface allows a property editor to take over the drawing
  of the drop down list box displayed by the property editor. This is only
  meaningful to implement if the property editor returns paValueList from
  IProperty.GetAttributes. The Value parameter is the result of
  IProperty.GetValue. The implementations ListMeasureWidth and ListMeasureHeight
  can be left blank since the var parameter is filled in to reasonable defaults
  by the object inspector. A default implementation of ListDrawValue is supplied
  in the DefaultPropertyListDrawValue procedure included in this unit }
  ICustomPropertyListDrawing = interface
    ['{BE2B8CF7-DDCA-4D4B-BE26-2396B969F8E0}']
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

  TColorPropertyEx = class(TIntegerProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { CustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;
{$ENDIF VisualCLX}

type
  // Special TClassProperty, that show events along with all other properties
  // This is only useful with version 5 and before
  {$IFNDEF COMPILER6_UP}
  TJvPersistentProperty = class(TClassProperty)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
  end;
  {$ENDIF COMPILER6_UP}

  TJvHintProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TJvFilenameProperty = class(TStringProperty)
  protected
    procedure OnDialogShow(Sender: TObject); virtual;
    function GetFilter: string; virtual;
    function GetOptions: TOpenOptions; virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvExeNameProperty = class(TJvFilenameProperty)
  protected
    function GetFilter: string; override;
  end;

  TJvDirectoryProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvStringsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TJvStringsEditor = class(TDefaultEditor)
  protected
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
    {$ENDIF COMPILER6_UP}
    function GetStringsName: string; virtual;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {$IFDEF VCL}

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

  {$ENDIF VCL}

  TJvShortCutProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  {$IFDEF COMPILER6_UP}
  TJvDefaultImageIndexProperty = class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
  protected
    function ImageList: TCustomImageList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure ListMeasureWidth(const Value: string;
      ACanvas: TCanvas; var AWidth: Integer); virtual;
    procedure ListMeasureHeight(const Value: string;
      ACanvas: TCanvas; var AHeight: Integer); virtual;
    procedure ListDrawValue(const Value: string;
      ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;
  {$ENDIF COMPILER6_UP}

  {$IFDEF COMPILER5}
  TJvDefaultImageIndexProperty = class(TIntegerProperty)
  protected
    function ImageList: TCustomImageList; virtual;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;
  end;
  {$ENDIF COMPILER5}

  TJvNosortEnumProperty = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvIntegerProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TJvFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;
                  
  TJvImageListEditor = class(TComponentEditor)
  private
    procedure SaveAsBitmap(ImageList: TImageList);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvWeekDayProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvComponentFormProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{$IFDEF VisualCLX}
procedure DefaultPropertyDrawName(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
procedure DefaultPropertyDrawValue(Prop: TPropertyEditor; Canvas: TCanvas;
  const Rect: TRect);
procedure DefaultPropertyListDrawValue(const Value: string; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean);
{$ENDIF VisualCLX}

implementation

{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF COMPILER6_UP}

uses
  TypInfo, Math, FileCtrl, Consts,
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Dlgs, JvDateTimeForm,
  {$ENDIF VCL}
  {$IFDEF LINUX}
	JvQRegistryIniFile,
  {$ENDIF LINUX}
  JvTypes, JvStringsForm, JvDsgnConsts, JvConsts;

function ValueName(E: Extended): string;
begin
  if E = High(Integer) then
    Result := RsMaxInt
  else
  if E = Low(Integer) then
    Result := RsMinInt
  else
  if E = High(Longint) then
    Result := RsMaxLong
  else
  if E = Low(Longint) then
    Result := RsMinLong
  else
  if E = High(ShortInt) then
    Result := RsMaxShort
  else
  if E = Low(ShortInt) then
    Result :=RsMinShort
  else
  if E = High(Word) then
    Result := RsMaxWord
  else
    Result := '';
end;

function StrToValue(const S: string): Longint;
begin
  if CompareText(S, RsMaxLong) = 0 then
    Result := High(Longint)
  else
  if CompareText(S, RsMinLong) = 0 then
    Result := Low(Longint)
  else
  if CompareText(S, RsMaxInt) = 0 then
    Result := High(Integer)
  else
  if CompareText(S, RsMinInt) = 0 then
    Result := Low(Integer)
  else
  if CompareText(S, RsMaxShort) = 0 then
    Result := High(Shortint)
  else
  if CompareText(S, RsMinShort) = 0 then
    Result := Low(Shortint)
  else
  if CompareText(S, RsMaxWord) = 0 then
    Result := High(Word)
  else
    Result := 0;
end;
  
//=== { TJvFilenameProperty } ================================================

procedure TJvFilenameProperty.Edit;
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := GetStrValue;
    Filter := GetFilter;
    Options := GetOptions;
    OnShow := OnDialogShow;
    if Execute then
      SetStrValue(FileName);
  finally
    Free;
  end;
end;

function TJvFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TJvFilenameProperty.GetFilter: string;
begin
  Result := RsAllFilesFilter;
end;

function TJvFilenameProperty.GetOptions: TOpenOptions;
begin
  Result := [ofHideReadOnly, ofEnableSizing];
end;

function TJvFilenameProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '' then
    Result := RsFileName;
end;

//=== { TJvDirectoryProperty } ===============================================

procedure TJvDirectoryProperty.Edit;
var
  AName: string;
  FolderName: THintString; // (ahuser) TCaption is "type Xxxstring", THintString is "Xxxstring"
  C: TPersistent;
begin
  C := GetComponent(0);
  if C is TComponent then
    AName := TComponent(C).Name
  else
  if C is TCollectionItem then
    AName := TCollectionItem(C).GetNamePath
  else
    AName := C.ClassName;
  if SelectDirectory(AName + '.' + GetName, '', FolderName) then
    SetValue(FolderName);
end;

function TJvDirectoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TJvDirectoryProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '' then
    Result := RsDirectory;
end;

//=== { TJvHintProperty } ====================================================

function TJvHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := {inherited GetAttributes +} [paDialog];
end;

procedure TJvHintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
//  I, Cnt: Integer;
begin
  with TJvStrEditDlg.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else
      Caption := GetName;
    Temp := GetStrValue;
    Memo.Lines.Text := Temp;
    UpdateStatus(nil);
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

//=== { TJvStringsProperty } =================================================

procedure TJvStringsProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
//  I, Cnt: Integer;
begin
  with TJvStrEditDlg.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else
      Caption := GetName;
    Temp := GetStrValue;
    Memo.Lines.Text := Temp;
    UpdateStatus(nil);
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

function TJvStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;


//=== { TJvStringsProperty } =================================================

{$IFDEF COMPILER6_UP}
procedure TJvStringsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, GetStringsName) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;
{$ELSE}
procedure TJvStringsEditor.EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if SameText(PropName, RsItems) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF COMPILER6_UP}

procedure TJvStringsEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

function TJvStringsEditor.GetStringsName: string;
begin
  Result := RsItems;
end;

function TJvStringsEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := Format(RsFmtEditEllipsis, [GetStringsName])
  else
    Result := '';
end;

function TJvStringsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VCL}

//=== { TJvDateTimeExProperty } ==============================================

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

//=== { TJvDateExProperty } ==================================================

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

//=== { TJvTimeExProperty } ==================================================

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

{$ENDIF VCL}

//=== { TJvDefaultImageIndexProperty } =======================================

{$IFDEF COMPILER6_UP}

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

function TJvDefaultImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

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

procedure TJvDefaultImageIndexProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AWidth := Tmp.Width + ACanvas.TextHeight(Value) + 4;
end;

procedure TJvDefaultImageIndexProperty.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AHeight := Max(Tmp.Height + 2, ACanvas.TextHeight(Value) + 2);
end;

procedure TJvDefaultImageIndexProperty.ListDrawValue(const Value: string; ACanvas:
  TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Tmp: TCustomImageList;
  R: TRect;
begin
  DefaultPropertyListDrawValue(Value, ACanvas, ARect, ASelected);
  Tmp := ImageList;
  if Tmp <> nil then
  begin
    R := ARect;
    ACanvas.FillRect(ARect);
    Tmp.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(Value));
    OffsetRect(R, Tmp.Width + 2, 0);
    DrawText(ACanvas.Handle, PChar(Value), -1, R, 0);
  end;
end;

procedure TJvDefaultImageIndexProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TJvDefaultImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if (GetVisualValue <> '') and Assigned(Tmp) then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

{$ENDIF COMPILER6_UP}

{$IFDEF COMPILER5}

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

function TJvDefaultImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

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

procedure TJvDefaultImageIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Tmp: TCustomImageList;
  R: TRect;
begin
  inherited ListDrawValue(Value, ACanvas, ARect, ASelected);
  Tmp := ImageList;
  if Tmp <> nil then
  begin
    R := ARect;
    ACanvas.FillRect(ARect);
    Tmp.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(Value));
    OffsetRect(R, Tmp.Width + 2, 0);
    DrawText(ACanvas.Handle, PChar(Value), -1, R, 0);
  end;
end;

procedure TJvDefaultImageIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AHeight := Max(Tmp.Height + 2, ACanvas.TextHeight(Value) + 2);
end;

procedure TJvDefaultImageIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AWidth := Tmp.Width + ACanvas.TextHeight(Value) + 4;
end;

procedure TJvDefaultImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
//  if GetVisualValue <> '' then
//    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
//  else
  inherited PropDrawValue(ACanvas, ARect, ASelected);
end;

{$ENDIF COMPILER5}

//=== { TJvShortCutProperty } ==================================================

function TJvShortCutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TJvShortCutProperty.GetValue: string;
begin
  try
    Result := ShortCutToText(GetOrdValue);
    if Result = '' then
      Result := RsNone;
  except
    Result := inherited GetValue;
  end;
end;

procedure TJvShortCutProperty.GetValues(Proc: TGetStrProc);
var
  Key: Word;
  Shift: TShiftState;
begin
  Proc(RsNone);

  Shift := [ssCtrl];
  for Key := Ord('A') to Ord('Z') do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Shift := [ssAlt, ssCtrl];
  for Key := Ord('A') to Ord('Z') do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Shift := [];
  for Key := VK_F1 to VK_F10 do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Shift := [ssCtrl];
  for Key := VK_F1 to VK_F10 do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Shift := [ssShift];
  for Key := VK_F1 to VK_F10 do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Shift := [ssShift, ssCtrl];
  for Key := VK_F1 to VK_F10 do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Shift := [ssShift, ssAlt, ssCtrl];
  for Key := VK_F1 to VK_F10 do
    Proc(ShortCutToText(ShortCut(Key, Shift)));

  Proc(ShortCutToText(ShortCut(VK_INSERT, [])));
  Proc(ShortCutToText(ShortCut(VK_INSERT, [ssShift])));
  Proc(ShortCutToText(ShortCut(VK_INSERT, [ssCtrl])));

  Proc(ShortCutToText(ShortCut(VK_DELETE, [])));
  Proc(ShortCutToText(ShortCut(VK_DELETE, [ssShift])));
  Proc(ShortCutToText(ShortCut(VK_DELETE, [ssCtrl])));

  Proc(ShortCutToText(ShortCut(VK_BACK, [ssAlt])));
  Proc(ShortCutToText(ShortCut(VK_BACK, [ssAlt, ssShift])));
end;

procedure TJvShortCutProperty.SetValue(const Value: string);
begin
  try
    SetOrdValue(TextToShortCut(Value));
  except
    inherited SetValue(Value);
  end;
end;

//=== { TJvNosortEnumProperty } ==============================================

function TJvNosortEnumProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paSortList];
end;

procedure TJvFilenameProperty.OnDialogShow(Sender: TObject);
begin
  {$IFDEF VCL}
  SetDlgItemText(GetParent(TOpenDialog(Sender).Handle), chx1, PChar(RsStripFilePath));
  {$ENDIF VCL}
end;

//=== { TJvExeNameProperty } =================================================

function TJvExeNameProperty.GetFilter: string;
begin
  Result := RsExecutableFilesExeExeAllFiles;
end;

//=== { TJvIntegerProperty } =================================================

function TJvIntegerProperty.GetValue: string;
begin
  Result := ValueName(GetOrdValue);
  if Result = '' then
    Result := IntToStr(GetOrdValue);
end;

procedure TJvIntegerProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L = 0 then
    L := StrToInt(Value);
  inherited SetValue(IntToStr(L));
end;

//=== { TJvFloatProperty } ===================================================

function TJvFloatProperty.GetValue: string;
const
  Precisions: array [TFloatType] of Integer = (7, 15, 18, 18, 18);
begin
  Result := ValueName(GetFloatValue);
  if Result = '' then
    Result := FloatToStrF(GetFloatValue, ffGeneral,
      Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TJvFloatProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L <> 0 then
    SetFloatValue(L)
  else
    SetFloatValue(StrToFloat(Value));
end;

procedure TJvImageListEditor.SaveAsBitmap(ImageList: TImageList);
var
  Bitmap: TBitmap;
  SaveDlg: TOpenDialog;
  I: Integer;
begin
  if ImageList.Count > 0 then
  begin
    SaveDlg := TSavePictureDialog.Create(Application);
    with SaveDlg do
    try
      Options := [ofHideReadOnly, ofOverwritePrompt];
      DefaultExt := GraphicExtension(TBitmap);
      Filter := GraphicFilter(TBitmap);
      if Execute then
      begin
        Bitmap := TBitmap.Create;
        try
          with Bitmap do
          begin
            Width := ImageList.Width * ImageList.Count;
            Height := ImageList.Height;
            if ImageList.BkColor <> clNone then
              Canvas.Brush.Color := ImageList.BkColor
            else
              Canvas.Brush.Color := clWindow;
            Canvas.FillRect(Bounds(0, 0, Width, Height));
            for I := 0 to ImageList.Count - 1 do
              ImageList.Draw(Canvas, ImageList.Width * I, 0, I);
            {$IFDEF VCL}
            HandleType := bmDIB;
            if PixelFormat in [pf15bit, pf16bit] then
            {$ENDIF VCL}
            {$IFDEF VisualCLX}
            if PixelFormat = pf16bit then
            {$ENDIF VisualCLX}
            try
              PixelFormat := pf24bit;
            except
            end;
          end;
          Bitmap.SaveToFile(FileName);
        finally
          Bitmap.Free;
        end;
      end;
    finally
      Free;
    end;
  end
  else
    Beep;
end;

procedure TJvImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Designer <> nil then
    case Index of
      0:
        if EditImageList(Component as TImageList) then
          Designer.Modified;
      1:
        SaveAsBitmap(TImageList(Component));
    end;
end;

function TJvImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := SImageListEditor;
    1:
      Result := RsSaveImageList;
  else
    Result := '';
  end;
end;

function TJvImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

//=== { TJvWeekDayProperty } =================================================

function TJvWeekDayProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

//=== { TJvComponentFormProperty } ===========================================

procedure TJvComponentFormProperty.GetValues(Proc: TGetStrProc);
var
  Form: TComponent;
begin
  inherited GetValues(Proc);
  Form := Designer.{$IFDEF COMPILER6_UP} Root {$ELSE} Form {$ENDIF};
  if (Form is GetTypeData(GetPropType)^.ClassType) and (Form.Name <> '') then
    Proc(Form.Name);
end;

procedure TJvComponentFormProperty.SetValue(const Value: string);
var
  Component: TComponent;
  Form: TComponent;
begin
  Component := Designer.GetComponent(Value);
  Form := Designer.{$IFDEF COMPILER6_UP} Root {$ELSE} Form {$ENDIF};
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType)) and
    (CompareText(Form.Name, Value) = 0) then
  begin
    if not (Form is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
    SetOrdValue(Longint(Form));
  end
  else
    inherited SetValue(Value);
end;

//=== { TJvPersistentProperty } ==============================================

{$IFNDEF COMPILER6_UP}

function TJvPersistentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

function TJvPersistentProperty.GetEditLimit: Integer;
begin
  Result := 127;
end;

procedure TJvPersistentProperty.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
  J: Integer;
  JvPersistents: TDesignerSelectionList;
begin
  JvPersistents := TDesignerSelectionList.Create;
  for I := 0 to PropCount - 1 do
  begin
    J := GetOrdValueAt(I);
    if J <> 0 then
      JvPersistents.Add(TJvPersistent(GetOrdValueAt(I)));
  end;
  if JvPersistents.Count > 0 then
    GetComponentProperties(JvPersistents, tkAny, Designer, Proc);
end;

{$ENDIF COMPILER6_UP}

//=== { TColorPropertyEx } ==================================================

{$IFDEF VisualCLX}

const
  { context ids for the Color Editor, from VCLEditors }
  hcDColorEditor      = 25010;

procedure TColorPropertyEx.Edit;
var
  ColorDialog: TColorDialog;
  {$IFDEF MSWINDOWS}
  IniFile: TRegIniFile;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  IniFile: TJvRegistryIniFile;
  {$ENDIF LINUX}

  procedure GetCustomColors;
  begin
    {$IFDEF MSWINDOWS}
    IniFile := TRegIniFile.Create(sDelphiKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors reading values }
    end;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    IniFile := TJvRegistryIniFile.Create(sDelphiKey);
    try
      if IniFile.OpenKey(SCustomColors, false) then
      begin
        IniFile.ReadSectionValues(ColorDialog.CustomColors);
        IniFile.CloseKey;
      end;
    except
      { Ignore errors reading values }
    end;
    {$ENDIF LINUX}
	end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    {$IFDEF LINUX}
    if IniFile <> nil then
    with IniFile, ColorDialog do
      if OpenKey(SCustomColors, true) then
      begin
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            WriteString( S, CustomColors.Values[S]);
          end;
        end;
        CloseKey;
      end;
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
    if IniFile <> nil then
    with IniFile, ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            WriteString(SCustomColors, S, CustomColors.Values[S]);
          end;
        end;
     {$ENDIF MSWINDOWS}
  end;

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  try
    GetCustomColors;
    ColorDialog.Color := GetOrdValue;
    ColorDialog.HelpContext := hcDColorEditor;
    if ColorDialog.Execute then
      SetOrdValue(ColorDialog.Color);
    SaveCustomColors;
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;

function TColorPropertyEx.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TColorPropertyEx.GetValue: string;
begin
  Result := ColorToString(TColor(GetOrdValue));
end;

procedure TColorPropertyEx.GetValues(Proc: TGetStrProc);
begin
  GetColorValues(Proc);
end;

procedure TColorPropertyEx.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
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
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  if ACanvas = nil then exit;
  with ACanvas do
  begin
    Start;
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
    Stop;
  end;
end;

procedure TColorPropertyEx.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  if ACanvas <> nil then
    AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

procedure TColorPropertyEx.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

procedure TColorPropertyEx.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implemenation necessary
end;

procedure TColorPropertyEx.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
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
{$ENDIF VisualCLX}

end.

