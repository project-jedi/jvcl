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

{$I JVCL.INC}

unit JvCtlReg;

interface

{ Register custom useful controls }

procedure Register;

implementation

uses
  Classes, SysUtils,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  TypInfo, Controls, Graphics, ExtCtrls, Tabs, Dialogs, Forms,
  DsnConst, ExtDlgs,
  {$IFNDEF DelphiPersonalEdition}
  ImgEdit,
  ImgList,
  {$ENDIF DelphiPersonalEdition}
  JvRichEd,
  Menus, FiltEdit, StdCtrls,
  JvxDConst, JvxCtrls, JvGrids, JvCurrEdit, JvToolEdit, JvDateUtil,
  JvPickDate, JvSplit, JvxSlider, JvxClock, JvxAnimate, JvSpin,
  JvDsgnEditors, JvDice, JvSwitch, JvCheckItm, JvVCLUtils, JvColors, JvAniFile, JvGraph,
  {$IFDEF USE_JV_GIF}
  JvGIF, JvGIFCtrl,
  {$ENDIF}
  JvHints, JvExcptDlg,
  JvFileUtil, JvDsgn;

function ValueName(E: Extended): string;
begin
  if E = High(Integer) then
    Result := 'MaxInt'
  else
  if E = Low(Integer) then
    Result := 'MinInt'
  else
  if E = High(Longint) then
    Result := 'MaxLong'
  else
  if E = Low(Longint) then
    Result := 'MinLong'
  else
  if E = High(ShortInt) then
    Result := 'MaxShort'
  else
  if E = Low(ShortInt) then
    Result := 'MinShort'
  else
  if E = High(Word) then
    Result := 'MaxWord'
  else
    Result := '';
end;

function StrToValue(const S: string): Longint;
begin
  if CompareText(S, 'MaxLong') = 0 then
    Result := High(Longint)
  else
  if CompareText(S, 'MinLong') = 0 then
    Result := Low(Longint)
  else
  if CompareText(S, 'MaxInt') = 0 then
    Result := High(Integer)
  else
  if CompareText(S, 'MinInt') = 0 then
    Result := Low(Integer)
  else
  if CompareText(S, 'MaxShort') = 0 then
    Result := High(ShortInt)
  else
  if CompareText(S, 'MinShort') = 0 then
    Result := Low(ShortInt)
  else
  if CompareText(S, 'MaxWord') = 0 then
    Result := High(Word)
  else
    Result := 0;
end;

//=== TJvIntegerProperty =====================================================

type
  TJvIntegerProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

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

//=== TJvFloatProperty =======================================================

type
  TJvFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

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

//=== TJvPaintBoxEditor ======================================================

type
  TJvPaintBoxEditor = class(TDefaultEditor)
  public
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}
  end;

{$IFDEF COMPILER6_UP}
procedure TJvPaintBoxEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TJvPaintBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end
  else
    inherited;
end;

//=== TJvAnimatedEditor ======================================================

type
  TJvAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
    {$IFDEF COMPILER6_UP}
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

{$IFDEF COMPILER6_UP}
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
{$IFNDEF COMPILER6_UP}
  finally
    PropertyEditor.Free;
  end;
{$ENDIF}
end;

{$IFDEF COMPILER6_UP}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

procedure TJvAnimatedEditor.EditImage(Image: TJvAnimatedImage);
var
  Components: TDesignerSelectionList;
begin
  {$IFDEF COMPILER6_UP}
  Components := TDesignerSelections.Create;
  {$ELSE}
  Components := TDesignerSelectionList.Create;
  {$ENDIF}
  {$IFNDEF COMPILER6_UP}
  try
  {$ENDIF}
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
  {$IFNDEF COMPILER6_UP}
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
    with Dialog do
    begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := 'ani';
      Filter := srAniCurFilter;
      if Execute then
      begin
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
  if Index = GetVerbCount - 1 then
    LoadAniFile(TJvAnimatedImage(Component))
  else
  if Index = GetVerbCount - 2 then
    EditImage(TJvAnimatedImage(Component))
  else
    inherited ExecuteVerb(Index);
end;

function TJvAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := srLoadAniCursor
  else
  if Index = GetVerbCount - 2 then
    Result := srEditPicture
  else
    Result := inherited GetVerb(Index);
end;

function TJvAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

//=== TJvImageListEditor =====================================================

{$IFNDEF DelphiPersonalEdition}
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
            HandleType := bmDIB;
            if PixelFormat in [pf15bit, pf16bit] then
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
      Result := srSaveImageList;
  else
    Result := '';
  end;
end;

function TJvImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$ENDIF DelphiPersonalEdition}

//=== TJvWeekDayProperty =====================================================

type
  TJvWeekDayProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

function TJvWeekDayProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure Register;
const
  cText = 'Text';
  cCaption = 'Caption';
  cHint = 'Hint';
const
  BaseClass: TClass = TComponent;
begin
  RegisterComponents(srJvCompositesPalette,
    [TJvComboEdit, TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvCalcEdit]);

  RegisterComponents(srJvConvertPalette, [TJvxCurrencyEdit]);

  RegisterComponents(srJvControlsPalette, [TJvTextListBox,
    TJvxCheckListBox, TJvxSplitter, TJvxSlider,
    TJvxLabel,
    TJvxRichEdit,
    TJvxClock, TJvAnimatedImage, TJvxDrawGrid, TJvxSpeedButton,
    {$IFDEF USE_JV_GIF}
    TJvGIFAnimator,
    {$ENDIF}
    TJvSpinButton,
    TJvSwitch, TJvDice]);
  {$IFDEF CBUILDER}
    RegisterComponents(ResStr(srSamplesPalette), [TScroller]);
  {$ELSE}
  RegisterComponents(ResStr(srSamplesPalette), [TScroller]);
  {$ENDIF}

  RegisterNonActiveX([TJvCustomComboEdit, TJvCustomDateEdit, TJvCustomNumEdit,
    TJvFileDirEdit, TJvxCustomListBox, TJvxRichEdit], axrComponentOnly);
  RegisterNonActiveX([TScroller], axrComponentOnly);

  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TJvWeekDayProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, cText, nil);
  RegisterPropertyEditor(TypeInfo(string), TJvFileDirEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomDateEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, cText, TJvDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TJvDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TJvDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', THintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);

  RegisterPropertyEditor(TypeInfo(TCaption), TJvxLabel, cCaption, THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvxSpeedButton, cCaption, THintProperty);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(TypeInfo(string), TMenuItem, cHint, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, cHint, THintProperty);

  RegisterPropertyEditor(TypeInfo(TCaption), TLabel, cCaption, THintProperty);

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
  RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TJvFloatProperty);
    {$IFNDEF DelphiPersonalEdition}
      RegisterComponentEditor(TPaintBox, TJvPaintBoxEditor);
      RegisterComponentEditor(TCustomImageList, TJvImageListEditor);
      RegisterComponentEditor(TImageList, TJvImageListEditor);
    {$ENDIF}
    RegisterJvColors;
  {$ENDIF}

end;

end.

