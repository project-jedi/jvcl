{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxCtlReg;

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
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, TypInfo, Controls, Graphics, ExtCtrls, Tabs, Dialogs, Forms,
  {$IFDEF Delphi3_Up} DsnConst, ExtDlgs, {$ELSE} LibConst, {$ENDIF} 
{$IFDEF DCS}
  {$IFDEF Delphi4_Up} ImgEdit, {$ENDIF} {$IFDEF WIN32} ImgList, {$ENDIF}
{$ENDIF DCS}
  {$IFDEF WIN32} JvxRichEd, {$ENDIF} Menus, FiltEdit, StdCtrls, Buttons,
  JvxLConst, JvxCtrls, JvxGrids, JvxCurrEdit, JvxToolEdit, JvxHintProp, JvxDateUtil,
  JvxPickDate, JvxSplit, JvxSlider, JvxClock, JvxAnimate, JvxCombos, JvxSpin, Consts,
  JvxDice, JvxSwitch, JvxCheckItm, JvxVCLUtils, JvxColors, JvxAniFile, JvxGraph,
  {$IFDEF USE_RX_GIF} JvxGIF, JvxGIFCtrl, {$ENDIF} JvxHints, JvxExcptDlg, JvxCConst,
  JvxFileUtil, JvxDsgn;

{$IFNDEF Delphi3_Up}

{ TJvxDateProperty }

type
  TJvxDateProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TJvxDateProperty.GetValue: string;
begin
  if GetFloatValue = NullDate then Result := ''
  else Result := FormatDateTime(ShortDateFormat, GetFloatValue);
end;

procedure TJvxDateProperty.SetValue(const Value: string);
begin
  if Value = '' then SetFloatValue(NullDate)
  else SetFloatValue(StrToDateFmt(ShortDateFormat, Value));
end;

{ TJvxModalResultProperty }

type
  TJvxModalResultProperty = class(TModalResultProperty)
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

function TJvxModalResultProperty.GetValue: string;
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

procedure TJvxModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited GetValues(Proc);
  for I := Low(ModalResults) to High(ModalResults) do
    Proc(ModalResults[I]);
end;

procedure TJvxModalResultProperty.SetValue(const Value: string);
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

{ TJvxIntegerProperty }

type
  TJvxIntegerProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TJvxIntegerProperty.GetValue: string;
begin
  Result := ValueName(GetOrdValue);
  if Result = '' then Result := IntToStr(GetOrdValue);
end;

procedure TJvxIntegerProperty.SetValue(const Value: String);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L = 0 then L := StrToInt(Value);
  inherited SetValue(IntToStr(L));
end;

{ TJvxFloatProperty }

type
  TJvxFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TJvxFloatProperty.GetValue: string;
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

procedure TJvxFloatProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  L := StrToValue(Value);
  if L <> 0 then SetFloatValue(L)
  else SetFloatValue(StrToFloat(Value));
end;

{ TJvxPaintBoxEditor }

type
  TJvxPaintBoxEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  end;

procedure TJvxPaintBoxEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'OnPaint') = 0 then begin
    Prop.Edit;
    Continue := False;
  end
  else inherited EditProperty(Prop, Continue);
end;

{ TJvxAnimatedEditor }

type
  TJvxAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
    procedure CheckEdit(const Prop: IProperty);
    procedure EditImage(Image: TJvxAnimatedImage);
    procedure LoadAniFile(Image: TJvxAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TJvxAnimatedEditor.CheckEdit(const Prop: IProperty);
begin
  try
    if FContinue and (CompareText(Prop.GetName, 'GLYPH') = 0) then
    begin
      Prop.Edit;
      FContinue := False;
    end;
  finally
    //Prop.Free;
  end;
end;

procedure TJvxAnimatedEditor.EditImage(Image: TJvxAnimatedImage);
var
  Components: IDesignerSelections;
begin
  Components := CreateSelectionList;
  try
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
  finally
    //Components.Free;
  end;
end;

procedure TJvxAnimatedEditor.LoadAniFile(Image: TJvxAnimatedImage);
var
  Dialog: TOpenDialog;
  AniCursor: TJvxAnimatedCursorImage;
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
        AniCursor := TJvxAnimatedCursorImage.Create;
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

procedure TJvxAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 1) then
    LoadAniFile(TJvxAnimatedImage(Component))
  else if (Index = GetVerbCount - 2) then
    EditImage(TJvxAnimatedImage(Component))
  else inherited ExecuteVerb(Index);
end;

function TJvxAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 1) then Result := LoadStr(srLoadAniCursor)
  else if (Index = GetVerbCount - 2) then Result := LoadStr(srEditPicture)
  else Result := inherited GetVerb(Index);
end;

function TJvxAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{$IFDEF DCS}
{$IFDEF WIN32}

type
  TJvxImageListEditor = class(TComponentEditor)
  private
    procedure SaveAsBitmap(ImageList: TImageList);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TJvxImageListEditor.SaveAsBitmap(ImageList: TImageList);
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

procedure TJvxImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Designer <> nil then
    case Index of
      0: if EditImageList(Component as TImageList) then Designer.Modified;
      1: SaveAsBitmap(TImageList(Component));
    end;
end;

function TJvxImageListEditor.GetVerb(Index: Integer): string;
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

function TJvxImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$ENDIF WIN32}
{$ENDIF DCS}

{ TJvxWeekDayProperty }

type
  TJvxWeekDayProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

function TJvxWeekDayProperty.GetAttributes: TPropertyAttributes;
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
//  RegisterComponents(LoadStr(srRXControls), [TJvxComboEdit, TJvxFilenameEdit,
  RegisterComponents('JVX', [TJvxComboEdit, TJvxFilenameEdit,
    TJvxDirectoryEdit, TJvxDateEdit, TJvxCalcEdit, TJvxCurrencyEdit, TJvxTextListBox,
    TJvxCheckListBox, TJvxFontComboBox, TJvxColorComboBox, TJvxSplitter, TJvxSlider,
    TJvxLabel, {$IFDEF WIN32} TJvxRichEdit, {$ENDIF}
    TJvxClock, TJvxAnimatedImage, TJvxDrawGrid, TJvxSpeedButton,
    {$IFDEF USE_RX_GIF} TJvxGIFAnimator, {$ENDIF} TJvxSpinButton, TJvxSpinEdit,
    TJvxSwitch, TJvxDice]);
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
  RegisterNonActiveX([TJvxCustomComboEdit, TJvxCustomDateEdit, TJvxCustomNumEdit,
    TJvxFileDirEdit, TJvxCustomListBox, TJvxRichEdit], axrComponentOnly);
  RegisterNonActiveX([TScroller], axrComponentOnly);
{$ENDIF Delphi3_Up}

  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TJvxWeekDayProperty);
{$IFDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(string), TJvxCustomNumEdit, 'Text', nil);
{$ELSE}
  RegisterPropertyEditor(TypeInfo(string), TJvxCustomNumEdit, 'Text', TStringProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TJvxFileDirEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxCustomDateEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxFilenameEdit, 'FileName', TJvxFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxDirectoryEdit, 'Text', TJvxDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TJvxDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TJvxDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', THintProperty);
  RegisterPropertyEditor(TypeInfo(string), TMenuItem, 'Hint', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxCustomComboEdit, 'ButtonHint', THintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvxCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvxProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvxProgressControlProperty);
{$IFDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(Boolean), TJvxFontComboBox, 'TrueTypeOnly', nil);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);
{$ELSE}
  RegisterPropertyEditor(TypeInfo(TDateTime), TPersistent, '', TJvxDateProperty);
  RegisterPropertyEditor(TypeInfo(TModalResult), TPersistent, '', TJvxModalResultProperty);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCaption), TLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvxLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvxSpeedButton, 'Caption', THintProperty);

  RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TJvxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(ShortInt), BaseClass, '', TJvxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(SmallInt), BaseClass, '', TJvxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Longint), BaseClass, '', TJvxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TJvxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TJvxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TJvxIntegerProperty);

  RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TJvxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TJvxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TJvxFloatProperty);
{$IFDEF WIN32}
  RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TJvxFloatProperty);
{$ENDIF}

  RegisterComponentEditor(TPaintBox, TJvxPaintBoxEditor);
  RegisterComponentEditor(TJvxAnimatedImage, TJvxAnimatedEditor);
{$IFDEF WIN32}
{$IFDEF DCS}
  RegisterComponentEditor(TCustomImageList, TJvxImageListEditor);
  RegisterComponentEditor(TImageList, TJvxImageListEditor);
{$ENDIF}
{$ENDIF}
  RegisterRxColors;
end;

end.
