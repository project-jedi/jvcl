{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
{$I JVCL.INC}


unit JvCtlReg;

interface

{ Register custom useful controls }

procedure Register;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.D16}
{$ENDIF}

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, {$ENDIF} Classes, SysUtils,
  {$IFDEF Delphi6_Up}
RTLConsts,  DesignIntf, DesignEditors,  VCLEditors,
{$ELSE}
  LibIntf, DsgnIntf,
{$ENDIF}


  TypInfo, Controls, Graphics, ExtCtrls, Tabs, Dialogs, Forms,
  {$IFDEF Delphi3_Up} DsnConst, ExtDlgs, {$ELSE} LibConst, {$ENDIF} 
{$IFDEF DCS}
  {$IFDEF Delphi4_Up} ImgEdit, {$ENDIF} {$IFDEF WIN32} ImgList, {$ENDIF}
{$ENDIF DCS}
  {$IFDEF WIN32} JvRichEd, {$ENDIF} Menus, FiltEdit, StdCtrls, Buttons,
  JvLConst, JvxCtrls, JvGrids, JvCurrEdit, JvToolEdit, JvHintProp, JvDateUtil,
  JvPickDate, JvSplit, JvxSlider, JvxClock, JvxAnimate, JvCombos, JvSpin, Consts,
  JvDice, JvSwitch, JvCheckItm, JvVCLUtils, JvColors, JvAniFile, JvGraph,
  {$IFDEF USE_Jv_GIF} JvGIF, JvGIFCtrl, {$ENDIF} JvHints, JvExcptDlg, JvCConst,
  JvFileUtil, JvDsgn;

{$IFNDEF Delphi3_Up}

{ TJvDateProperty }

type
  TJvDateProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TJvDateProperty.GetValue: string;
begin
  if GetFloatValue = NullDate then Result := ''
  else Result := FormatDateTime(ShortDateFormat, GetFloatValue);
end;

procedure TJvDateProperty.SetValue(const Value: string);
begin
  if Value = '' then SetFloatValue(NullDate)
  else SetFloatValue(StrToDateFmt(ShortDateFormat, Value));
end;

{ TJvModalResultProperty }

type
  TJvModalResultProperty = class(TModalResultProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

const
  ModalResults: array[mrAll..mrYesToAll] of string = (
    'mrAll',
    'mrNoToAll',
    'mrYesToAll');

function TJvModalResultProperty.GetValue: string;
var
  CurValue: Longint;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
    else Result := inherited GetValue;
  end;
end;

procedure TJvModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited GetValues(Proc);
  for I := Low(ModalResults) to High(ModalResults) do
    Proc(ModalResults[I]);
end;

procedure TJvModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if (Value <> '') then
    for I := Low(ModalResults) to High(ModalResults) do
      if CompareText(ModalResults[I], Value) = 0 then begin
        SetOrdValue(I);
        Exit;
      end;
  inherited SetValue(Value);
end;

{$ENDIF Delphi3_Up}

function ValueName(E: Extended): string;
begin
  if E = High(Integer) then Result := 'MaxInt'
  else if E = Low(Integer) then Result := 'MinInt'
  else if E = High(Longint) then Result := 'MaxLong'
  else if E = Low(Longint) then Result := 'MinLong'
  else if E = High(ShortInt) then Result := 'MaxShort'
  else if E = Low(ShortInt) then Result := 'MinShort'
  else if E = High(Word) then Result := 'MaxWord'
  else Result := '';
end;

function StrToValue(const S: string): Longint;
begin
  if CompareText(S, 'MaxLong') = 0 then Result := High(Longint)
  else if CompareText(S, 'MinLong') = 0 then Result := Low(Longint)
  else if CompareText(S, 'MaxInt') = 0 then Result := High(Integer)
  else if CompareText(S, 'MinInt') = 0 then Result := Low(Integer)
  else if CompareText(S, 'MaxShort') = 0 then Result := High(ShortInt)
  else if CompareText(S, 'MinShort') = 0 then Result := Low(ShortInt)
  else if CompareText(S, 'MaxWord') = 0 then Result := High(Word)
  else Result := 0;
end;

{ TJvIntegerProperty }

type
  TJvIntegerProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TJvIntegerProperty.GetValue: string;
begin
  Result := ValueName(GetOrdValue);
  if Result = '' then Result := IntToStr(GetOrdValue);
end;

procedure TJvIntegerProperty.SetValue(const Value: String);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L = 0 then L := StrToInt(Value);
  inherited SetValue(IntToStr(L));
end;

{ TJvFloatProperty }

type
  TJvFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TJvFloatProperty.GetValue: string;
const
{$IFDEF WIN32}
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);
{$ELSE}
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18);
{$ENDIF}
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
  if L <> 0 then SetFloatValue(L)
  else SetFloatValue(StrToFloat(Value));
end;

{ TJvPaintBoxEditor }

type
  TJvPaintBoxEditor = class(TDefaultEditor)
  public
{$IFDEF Delphi6_Up}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
{$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
{$ENDIF}
  end;

{$IFDEF Delphi6_Up}
procedure TJvPaintBoxEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TJvPaintBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then begin
    PropertyEditor.Edit;
    Continue := False;
  end
  else inherited;
end;

{ TJvAnimatedEditor }

type
  TJvAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
{$IFDEF Delphi6_Up}
    procedure CheckEdit(const PropertyEditor: IProperty);
{$ELSE}
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
{$ENDIF}
    procedure EditImage(Image: TJvAnimatedImage);
    procedure LoadAniFile(Image: TJvAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$IFDEF Delphi6_Up}
procedure TJvAnimatedEditor.CheckEdit(const PropertyEditor: IProperty);
begin
{$ELSE}
procedure TJvAnimatedEditor.CheckEdit(PropertyEditor: TPropertyEditor);
begin
  try
{$ENDIF}
    if FContinue and (CompareText(PropertyEditor.GetName, 'GLYPH') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
{$IFNDEF Delphi6_Up}
  finally
    PropertyEditor.Free;
  end;
{$ENDIF}
end;

{$IFDEF Delphi6_Up}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}



procedure TJvAnimatedEditor.EditImage(Image: TJvAnimatedImage);
var
  Components: TDesignerSelectionList;
begin
  Components := {$IFDEF Delphi6_Up}TDesignerSelections{$ELSE}TDesignerSelectionList{$ENDIF}.Create;
{$IFNDEF Delphi6_Up}
  try
{$ENDIF}
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
{$IFNDEF Delphi6_Up}
  finally
    Components.Free;
  end;
{$ENDIF}
end;


procedure TJvAnimatedEditor.LoadAniFile(Image: TJvAnimatedImage);
var
  Dialog: TOpenDialog;
  AniCursor: TJvAnimatedCursorImage;
  CurDir: string;
begin
  CurDir := GetCurrentDir;
  Dialog := TOpenDialog.Create(Application);
  try
    with Dialog do begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := 'ani';
      Filter := LoadStr(srAniCurFilter);
      if Execute then begin
        AniCursor := TJvAnimatedCursorImage.Create;
        try
          AniCursor.LoadFromFile(FileName);
          AniCursor.AssignToBitmap(Image.Glyph, clFuchsia, True,
            Image.Orientation = goVertical);
          Image.Interval := AniCursor.DefaultRate;
          Image.TransparentColor := clFuchsia;
          Designer.Modified;
        finally
          AniCursor.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
    SetCurrentDir(CurDir);
  end;
end;

procedure TJvAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 1) then
    LoadAniFile(TJvAnimatedImage(Component))
  else if (Index = GetVerbCount - 2) then
    EditImage(TJvAnimatedImage(Component))
  else inherited ExecuteVerb(Index);
end;

function TJvAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 1) then Result := LoadStr(srLoadAniCursor)
  else if (Index = GetVerbCount - 2) then Result := LoadStr(srEditPicture)
  else Result := inherited GetVerb(Index);
end;

function TJvAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{$IFDEF DCS}
{$IFDEF WIN32}

type
  TJvImageListEditor = class(TComponentEditor)
  private
    procedure SaveAsBitmap(ImageList: TImageList);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TJvImageListEditor.SaveAsBitmap(ImageList: TImageList);
var
  Bitmap: TBitmap;
  SaveDlg: TOpenDialog;
  I: Integer;
begin
  if ImageList.Count > 0 then begin
{$IFDEF Delphi3_Up}
    SaveDlg := TSavePictureDialog.Create(Application);
{$ELSE}
    SaveDlg := TSaveDialog.Create(Application);
{$ENDIF}
    with SaveDlg do
    try
      Options := [ofHideReadOnly, ofOverwritePrompt];
      DefaultExt := GraphicExtension(TBitmap);
      Filter := GraphicFilter(TBitmap);
      if Execute then begin
        Bitmap := TBitmap.Create;
        try
          with Bitmap do begin
            Width := ImageList.Width * ImageList.Count;
            Height := ImageList.Height;
            if ImageList.BkColor <> clNone then
              Canvas.Brush.Color := ImageList.BkColor
            else Canvas.Brush.Color := clWindow;
            Canvas.FillRect(Bounds(0, 0, Width, Height));
            for I := 0 to ImageList.Count - 1 do
              ImageList.Draw(Canvas, ImageList.Width * I, 0, I);
{$IFDEF Delphi3_Up}
            HandleType := bmDIB;
            if PixelFormat in [pf15bit, pf16bit] then try
              PixelFormat := pf24bit;
            except {} end;
{$ENDIF}
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
  else Beep;
end;

procedure TJvImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Designer <> nil then
    case Index of
      0: if EditImageList(Component as TImageList) then Designer.Modified;
      1: SaveAsBitmap(TImageList(Component));
    end;
end;

function TJvImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
{$IFDEF Delphi3_Up}
    0: Result := SImageListEditor;
{$ELSE}
    0: Result := LoadStr(SImageEditor);
{$ENDIF}
    1: Result := LoadStr(srSaveImageList);
    else Result := '';
  end;
end;

function TJvImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$ENDIF WIN32}
{$ENDIF DCS}

{ TJvWeekDayProperty }

type
  TJvWeekDayProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

function TJvWeekDayProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

{$IFDEF Delphi3_Up}
resourcestring
  srSamples = 'Samples';
{$ENDIF}

procedure Register;
const
{$IFDEF Delphi3_Up}
  BaseClass: TClass = TPersistent;
{$ELSE}
  BaseClass: TClass = TComponent;
{$ENDIF}
begin
  RegisterComponents({LoadStr(srRXControls)}'JvX Controls', [TJvComboEdit, TJvFilenameEdit,
    TJvDirectoryEdit, TJvDateEdit, TJvCalcEdit, TJvxCurrencyEdit, TJvTextListBox,
    TJvxCheckListBox, TJvxFontComboBox, TJvxColorComboBox, TJvxSplitter, TJvxSlider,
    TJvxLabel, {$IFDEF WIN32} TJvxRichEdit, {$ENDIF}
    TJvxClock, TJvAnimatedImage, TJvxDrawGrid, TJvxSpeedButton,
    {$IFDEF USE_Jv_GIF} TJvGIFAnimator, {$ENDIF} TJvSpinButton, TJvxSpinEdit,
    TJvSwitch, TJvDice]);
{$IFDEF CBUILDER}
 {$IFNDEF COMPILER35_UP} { C++Builder 1.0 }
  RegisterComponents(ResStr(srAdditional), [TScroller]);
 {$ELSE}
  RegisterComponents(ResStr(srSamples), [TScroller]);
 {$ENDIF}
{$ELSE}
  RegisterComponents(ResStr(srSamples), [TScroller]);
{$ENDIF}

{$IFDEF Delphi3_Up}
  RegisterNonActiveX([TJvCustomComboEdit, TJvCustomDateEdit, TJvCustomNumEdit,
    TJvFileDirEdit, TJvxCustomListBox, TJvxRichEdit], axrComponentOnly);
  RegisterNonActiveX([TScroller], axrComponentOnly);
{$ENDIF Delphi3_Up}

  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TJvWeekDayProperty);
{$IFDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, 'Text', nil);
{$ELSE}
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, 'Text', TStringProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TJvFileDirEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomDateEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, 'Text', TJvDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TJvDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TJvDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', THintProperty);
  RegisterPropertyEditor(TypeInfo(string), TMenuItem, 'Hint', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', THintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
{$IFDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(Boolean), TJvxFontComboBox, 'TrueTypeOnly', nil);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);
{$ELSE}
  RegisterPropertyEditor(TypeInfo(TDateTime), TPersistent, '', TJvDateProperty);
  RegisterPropertyEditor(TypeInfo(TModalResult), TPersistent, '', TJvModalResultProperty);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCaption), TLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvxLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvxSpeedButton, 'Caption', THintProperty);

  RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(ShortInt), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(SmallInt), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Longint), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TJvIntegerProperty);

  RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TJvFloatProperty);
  RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TJvFloatProperty);
  RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TJvFloatProperty);
{$IFDEF WIN32}
  RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TJvFloatProperty);
{$ENDIF}

  RegisterComponentEditor(TPaintBox, TJvPaintBoxEditor);
  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
{$IFDEF WIN32}
{$IFDEF DCS}
  RegisterComponentEditor(TCustomImageList, TJvImageListEditor);
  RegisterComponentEditor(TImageList, TJvImageListEditor);
{$ENDIF}
{$ENDIF}
  RegisterRxColors;
end;

end.
