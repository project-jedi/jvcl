{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorCombo.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
Brian Cook (borland.public.vcl.components.writing)

Last Modified: 2002-11-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Comboboxes for displaying colors and fonts

Known Issues:
  If you set AutoComplete in TJvColorComboBox to True and use the same text for
  all Custom colors, the inherited Change behaviour from TJvComboBox makes the *first*
  custom color selected, not the last added as it should be thus AutoComplete is
  set to default to False. (p3)
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvColorCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs,
  Graphics, StdCtrls, Printers,
  JvComboBox;

type
  TJvNewColorEvent = procedure(Sender: TObject; Color: TColor; var DisplayName: string;
    var AllowAdd: Boolean) of object;
  TJvGetColorNameEvent = procedure(Sender: TObject; Index: Integer; Color: TColor;
    var DisplayName: string) of object;
  TJvColorComboOption = (coText, coHex, coRGB, coSysColors, coCustomColors);
  TJvColorComboOptions = set of TJvColorComboOption;

  TJvColorComboBox = class(TJvCustomComboBox)
  private
    FColorValue: TColor;
    FCustCnt: Integer;
    FHiliteColor: TColor;
    FHiliteText: TColor;
    FOptions: TJvColorComboOptions;
    FPrefix: string;
    FOther: string;
    FColWidth: Integer;
    FExecutingDialog: Boolean;
    FNewColor: TJvNewColorEvent;
    FOnGetDisplayName: TJvGetColorNameEvent;
    FColorNameMap: TStringList;
    FOnInsertColor: TJvNewColorEvent;
    FOnBeforeCustom: TNotifyEvent;
    procedure SetOptions(Value: TJvColorComboOptions);
    procedure SetOther(Value: string);
    procedure SetColWidth(Value: Integer);
    procedure SetColorValue(Value: TColor);
    procedure ResetItemHeight;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure SetPrefix(const Value: string);
    function GetColorNameMap: TStrings;
    procedure SetColorNameMap(const Value: TStrings);
    procedure InitColorNames;
    function GetDropDownWidth: Integer;
    procedure SetDropDownWidth(const Value: Integer);
  protected
    procedure FontChanged; override;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure Click; override;

    function GetColorName(AColor: TColor; const Default: string): string;
    function DoNewColor(Color: TColor; var DisplayName: string): Boolean; virtual;
    procedure DoGetDisplayName(Index: Integer; AColor: TColor; var DisplayName: string); virtual;
    function DoInsertColor(AIndex: Integer; AColor: TColor; var DisplayName: string): Boolean; virtual;
    procedure DoBeforeCustom;
    procedure InternalInsertColor(AIndex: Integer; AColor: TColor; const DisplayName: string); virtual;
    procedure DoNameMapChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure GetColors; virtual;
    procedure GetCustomColors(AList: TList);
    // Returns the current name for AColor. Note that this implicitly might call the
    // OnGetDisplayName event if the protected GetColorName returns an emtpy string
    function ColorName(AColor: TColor): string;
    // returns the index of a specific color or -1 if not found
    function FindColor(AColor: TColor): Integer;

    procedure AddColor(AColor: TColor; const DisplayName: string);
    procedure ChangeColor(AIndex: Integer; AColor: TColor; const DisplayName: string);
    procedure InsertColor(AIndex: Integer; AColor: TColor; const DisplayName: string);
    property Text;
    property CustomColorCount: Integer read FCustCnt;
  published
    property Anchors;
    property AutoComplete default False;
    {$IFDEF COMPILER6_UP}
    property AutoDropDown;
    {$ENDIF COMPILER6_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    // color name map is a Tstrings property that can contain name/value mappings on the form
    // colorName=DisplayName
    // if the component finds a matching mapping, it will substitute the default value
    // with the value in the list, otherwise the default value wil be used
    // Example:
    // clBlack=Black
    property ColorNameMap: TStrings read GetColorNameMap write SetColorNameMap;
    property ColorValue: TColor read FColorValue write SetColorValue default clBlack;
    property ColorDialogText: string read FOther write SetOther;
    property ColorWidth: Integer read FColWidth write SetColWidth default 21;
    property DroppedDownWidth: Integer read GetDropDownWidth write SetDropDownWidth;

    property NewColorText: string read FPrefix write SetPrefix;
    property Options: TJvColorComboOptions read FOptions write SetOptions default [coText];

    property HiliteColor: TColor read FHiliteColor write FHiliteColor default clHighlight;
    property HiliteText: TColor read FHiliteText write FHiliteText default clHighlightText;
    // called before a new color is inserted as a result of displaying the Custom Colors dialog
    property OnNewColor: TJvNewColorEvent read FNewColor write FNewColor;
    // called before any color is inserted
    property OnInsertColor: TJvNewColorEvent read FOnInsertColor write FOnInsertColor;
    // called whenever the displayname of an item is needed
    property OnGetDisplayName: TJvGetColorNameEvent read FOnGetDisplayName write FOnGetDisplayName;
    // called just before the '(Other)' item is added at the bottom of the list
    property OnBeforeCustom: TNotifyEvent read FOnBeforeCustom write FOnBeforeCustom;

    property Color;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

  //  TFontDialogDevice = (fdScreen, fdPrinter, fdBoth); { already in Dialogs }
  TJvFontComboOption = (foAnsiOnly, foTrueTypeOnly, foFixedPitchOnly,
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly, foWysiWyg, foDisableVerify);
  // foDisableVerify: if True, allows you to insert a font name that doesn't exist (by assigning to FontName
  TJvFontComboOptions = set of TJvFontComboOption;

  TJvFontComboBox = class(TJvCustomComboBox)
  private
    FTrueTypeBmp: TBitmap;
    FFixBmp: TBitmap;
    FDeviceBmp: TBitmap;
    FDevice: TFontDialogDevice;
    FHiliteColor: TColor;
    FHiliteText: TColor;
    FUseImages: Boolean;
    FOptions: TJvFontComboOptions;
    procedure SetUseImages(Value: Boolean);
    procedure SetDevice(Value: TFontDialogDevice);
    procedure SetOptions(Value: TJvFontComboOptions);
    procedure ResetItemHeight;
    procedure Reset;
      // (ahuser) why both WM_FONTCHANGE and CM_FONTCHANGED ?
    //procedure WMFontChange(var Msg: TMessage); message WM_FONTCHANGE;
    function GetFontName: string;
    procedure SetFontName(const Value: string);
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
    function GetDropDownWidth: Integer;
    procedure SetDropDownWidth(const Value: Integer);
  protected
    procedure FontChanged; override;
    procedure Loaded; override;
    procedure GetFonts; virtual;
    procedure Click; override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FontSubstitute(const FontName: string): string;
    property Text;
  published
    property Anchors;
    property AutoComplete default False;
    {$IFDEF COMPILER6_UP}
    property AutoDropDown;
    {$ENDIF COMPILER6_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    property Color;
    property DroppedDownWidth: Integer read GetDropDownWidth write SetDropDownWidth;
    property FontName: string read GetFontName write SetFontName;
    property Device: TFontDialogDevice read FDevice write SetDevice default fdScreen;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ItemIndex;
    property HiliteColor: TColor read FHiliteColor write FHiliteColor default clHighlight;
    property HiliteText: TColor read FHiliteText write FHiliteText default clHighlightText;
    property Options: TJvFontComboOptions read FOptions write SetOptions default [];
    property UseImages: Boolean read FUseImages write SetUseImages default True;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted: Boolean read GetSorted write SetSorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

implementation

uses
  Math,
  JvConsts, JvResources, JvTypes;

{$R ..\resources\JvColorCombo.res}

function LoadInternalBitmap(ResName: string): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Handle := LoadBitmap(hInstance, PChar(ResName));
end;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  aFont: HFont;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    aFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    SelectObject(DC, aFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := TM.tmHeight + 1;
end;

function IncludeFont(Options: TJvFontComboOptions; LogFont: TLogFont; FontType: Integer): Boolean;
begin
  Result := True;
  if foAnsiOnly in Options then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if foTrueTypeOnly in Options then
    Result := Result and (FontType and TRUETYPE_FONTTYPE > 0);
  if foFixedPitchOnly in Options then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH > 0);
  if foOEMFontsOnly in Options then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if foNoOEMFonts in Options then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if foScalableOnly in Options then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; FontCombo: TJvFontComboBox): Integer; stdcall;
begin
  Result := 0;
  if FontCombo = nil then
    Exit;
  if IncludeFont(FontCombo.Options, LogFont, FontType) then
  begin
    if FontCombo.Items.IndexOf(string(LogFont.lfFaceName)) = -1 then
      FontCombo.Items.AddObject(string(LogFont.lfFaceName), TObject(FontType));
  end;
  Result := 1;
end;

function ItemStateToOwnerDrawState(State: Integer): TOwnerDrawState;
begin
  Result := [];
  if (State and ODS_CHECKED) <> 0 then
    Include(Result, odChecked);
  if (State and ODS_COMBOBOXEDIT) <> 0 then
    Include(Result, odComboBoxEdit);
  if (State and ODS_DEFAULT) <> 0 then
    Include(Result, odDefault);
  if (State and ODS_DISABLED) <> 0 then
    Include(Result, odDisabled);
  if (State and ODS_FOCUS) <> 0 then
    Include(Result, odFocused);
  if (State and ODS_GRAYED) <> 0 then
    Include(Result, odGrayed);
  if (State and ODS_SELECTED) <> 0 then
    Include(Result, odSelected);
end;

// === TJvColorComboBox ======================================================

constructor TJvColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorNameMap := TStringList.Create;
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;
  FColWidth := 21;
  FPrefix := RsNewColorPrefix;
  FOther := RsCustomCaption;
  FOptions := [coText];
  FHiliteColor := clHighlight;
  FHiliteText := clHighlightText;
  AutoComplete := False;
  // make sure that if this is the first time the component is dropped on the form,
  // the default Name/Value map is created (thanks to Brian Cook on the borland NG's):
  if (Owner <> nil) and ([csDesigning, csLoading] * Owner.ComponentState = [csDesigning]) then
    InitColorNames;
  FColorNameMap.OnChange := DoNameMapChange;
end;

destructor TJvColorComboBox.Destroy;
begin
  FColorNameMap.Free;
  inherited Destroy;
end;

procedure TJvColorComboBox.GetColors;
var
  I: Integer;
  ColorName: string;
begin
  Clear;
  FCustCnt := 0;
  for I := Low(ColorValues) to High(ColorValues) do
  begin
    ColorName := GetColorName(ColorValues[I].Value, '');
    InternalInsertColor(Items.Count, ColorValues[I].Value, ColorName);
  end;
  if coSysColors in FOptions then
    for I := Low(SysColorValues) to High(SysColorValues) do
    begin
      ColorName := GetColorName(SysColorValues[I].Value, '');
      InternalInsertColor(Items.Count, SysColorValues[I].Value, ColorName);
    end;
  DoBeforeCustom;
  if coCustomColors in FOptions then
    InternalInsertColor(Items.Count, $000001, FOther);
  SetColorValue(FColorValue);
end;

procedure TJvColorComboBox.SetOptions(Value: TJvColorComboOptions);
begin
  if FOptions <> Value then
  begin
    if (coCustomColors in FOptions) and (Items.Count > 0) then
      Items.Delete(Items.Count - 1);
    FOptions := Value;
{    if coText in FOptions then
    begin
      Exclude(FOptions,coHex);
      Exclude(FOptions,coRGB);
    end
    else
    if coHex in Value then
      Exclude(FOptions,coRGB); }
    if coCustomColors in FOptions then
      InternalInsertColor(Items.Count, $000001, FOther);
    Invalidate;
  end;
end;

procedure TJvColorComboBox.SetOther(Value: string);
var
  I: Integer;
begin
  if FOther <> Value then
  begin
    I := Items.IndexOf(FOther);
    while I > -1 do
    begin
      Items[I] := Value;
      I := Items.IndexOf(FOther);
    end;
    FOther := Value;
  end;
end;

procedure TJvColorComboBox.SetColWidth(Value: Integer);
begin
  if FColWidth <> Value then
  begin
    FColWidth := Value;
    Invalidate;
  end;
end;

procedure TJvColorComboBox.SetColorValue(Value: TColor);
var
  I: Integer;
begin
  I := FindColor(Value);
  if I >= 0 then
  begin
    FColorValue := Value;
    if ItemIndex <> I then
    begin
      ItemIndex := I;
      Change;
    end;
    Exit;
  end
  else
  if coCustomColors in Options then
  begin
    InsertColor(Items.Count - 1, Value, Format(FPrefix, [FCustCnt]));
        //      Items.InsertObject(Items.Count, FPrefix + IntToStr(FCustCnt), TObject(Value))
    FColorValue := Value;
    ItemIndex := Items.Count - 2;
  end
  else
  begin
    AddColor(Value, Format(FPrefix, [FCustCnt]));
    FColorValue := Value;
    ItemIndex := Items.Count - 1;
    Change;
  end;
    //      Items.AddObject(FPrefix + IntToStr(FCustCnt), TObject(Value));
end;

function TJvColorComboBox.DoNewColor(Color: TColor; var DisplayName: string): Boolean;
begin
  if Assigned(FNewColor) then
    FNewColor(Self, Color, DisplayName, Result)
  else
    Result := FindColor(Color) = -1;
  if Result then
    Inc(FCustCnt);
end;

procedure TJvColorComboBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := ItemStateToOwnerDrawState(itemState);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := FHiliteColor;
      Canvas.Font.Color := FHiliteText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TJvColorComboBox.DrawItem(Index: Integer; R: TRect;
  State: TOwnerDrawState);
var
  aRect: TRect;
  aColor: TColor;
  S: string;
begin
  aRect := R;
  Inc(aRect.Top, 2);
  Inc(aRect.Left, 2);
  Dec(aRect.Bottom, 2);
  if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) or
    ((coCustomColors in FOptions) and (Index = Items.Count - 1)) then
    aRect.Right := aRect.Left + FColWidth

  else
    Dec(aRect.Right, 3);

  with Canvas do
  begin
    aColor := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := clGray;
    OffsetRect(aRect, 2, 2);
    FillRect(aRect);
    OffsetRect(aRect, -2, -2);
    Brush.Color := TColor(Items.Objects[Index]);
    try
      Rectangle(aRect);
    finally
      Brush.Color := aColor;
    end;
    if (coCustomColors in FOptions) and (Index = Items.Count - 1) then
    begin
      S := FOther;
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      Brush.Color := Self.Color;
      FillRect(R);
      R.Left := R.Left + 2;
      R.Right := R.Left + TextWidth(S) + 2;
      Brush.Color := aColor;
      FillRect(R);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    end
    else
    if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) then
    begin
      S := Items[Index];
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      if S <> FOther then
      begin
        if coHex in FOptions then
          S := Format('0x%.6x', [ColorToRGB(TColor(Items.Objects[Index]))])
        else
        if coRGB in Foptions then
          S := Format('(%d,%d,%d)', [GetRValue(TColor(Items.Objects[Index])), GetGValue(TColor(Items.Objects[Index])),
            GetBValue(TColor(Items.Objects[Index]))]);
      end;
      R.Left := R.Left + FColWidth + 6;
      R.Right := R.Left + TextWidth(S) + 6;
      FillRect(R);
      OffsetRect(R, 2, 0);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or
        DT_VCENTER or DT_NOPREFIX);
      OffsetRect(R, -2, 0);
    end
    else
      FrameRect(R);
    if odSelected in State then
      DrawFocusRect(R);
  end;
end;

procedure TJvColorComboBox.Click;
var
  S: string;
  CD: TColorDialog;
begin
  if FExecutingDialog then
    Exit;
  if (ItemIndex = Items.Count - 1) and (coCustomColors in FOptions) then
  begin
    FExecutingDialog := True;
    CD := TColorDialog.Create(Self);
    with CD do
    try
      CD.Color := ColorValue;
      Options := Options + [cdFullOpen, cdPreventFullOpen];
      S := FPrefix;
      if Execute and DoNewColor(CD.Color, S) then
      begin
        InternalInsertColor(Items.Count - 1, CD.Color, S);
        ItemIndex := Items.Count - 2;
        Change;
      end
      else
        ColorValue := CD.Color;
    finally
      Free;
    end // with
  end
  else
  if ItemIndex >= 0 then
    ColorValue := TColor(Items.Objects[ItemIndex]);
  inherited Click;
  FExecutingDialog := False;
end;

procedure TJvColorComboBox.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvColorComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), 9);
end;

procedure TJvColorComboBox.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TJvColorComboBox.AddColor(AColor: TColor;
  const DisplayName: string);
var
  S: string;
begin
  S := DisplayName;
  if DoNewColor(AColor, S) then
    InternalInsertColor(Items.Count - Ord(coCustomColors in Options), AColor, S);
end;

procedure TJvColorComboBox.DoGetDisplayName(Index: Integer; AColor: TColor;
  var DisplayName: string);
begin
  if Assigned(FOnGetDisplayName) then
    FOnGetDisplayName(Self, Index, AColor, DisplayName)
  else
    DisplayName := GetColorName(AColor, DisplayName);
end;

procedure TJvColorComboBox.InsertColor(AIndex: Integer; AColor: TColor;
  const DisplayName: string);
var
  S: string;
begin
  S := DisplayName;
  if DoInsertColor(AIndex, AColor, S) then
    InternalInsertColor(AIndex, AColor, S);
end;

function TJvColorComboBox.GetColorNameMap: TStrings;
begin
  Result := FColorNameMap;
end;

procedure TJvColorComboBox.SetColorNameMap(const Value: TStrings);
begin
  FColorNameMap.Assign(Value);
  Invalidate;
end;

function TJvColorComboBox.GetColorName(AColor: TColor; const Default: string): string;
var
  Tmp: string;
begin
  Tmp := ColorToString(AColor);
  Result := FColorNameMap.Values[Tmp];
  if Result = '' then
    Result := FColorNameMap.Values['cl' + Tmp];
  if Result = '' then
  begin
    if Default = '' then
    begin
      if (Length(Tmp) > 1) and AnsiSameText(Tmp[1], 'c') and AnsiSameText(Tmp[2], 'l') then
        Result := Copy(Tmp, 3, MaxInt)
      else
        Result := Tmp;
    end
    else
      Result := Default;
  end;
end;

procedure TJvColorComboBox.InitColorNames;
var
  I: Integer;
begin
  FColorNameMap.BeginUpdate;
  try
    FColorNameMap.Clear;
    for I := Low(ColorValues) to High(ColorValues) do
      FColorNameMap.Add(ColorValues[I].Constant + '=' + ColorValues[I].Description);
    for I := Low(SysColorValues) to High(SysColorValues) do
      FColorNameMap.Add(SysColorValues[I].Constant + '=' + SysColorValues[I].Description);
  finally
    FColorNameMap.EndUpdate;
  end;
end;

function TJvColorComboBox.DoInsertColor(AIndex: Integer; AColor: TColor;
  var DisplayName: string): Boolean;
begin
  Result := True;
  if Assigned(FOnInsertColor) then
    FOnInsertColor(Self, AColor, DisplayName, Result);
end;

procedure TJvColorComboBox.DoBeforeCustom;
begin
  if Assigned(FOnBeforeCustom) then
    FOnBeforeCustom(Self);
end;

procedure TJvColorComboBox.ChangeColor(AIndex: Integer; AColor: TColor;
  const DisplayName: string);
begin
  // raise Exception ?
  if (AIndex >= 0) and (AIndex < Items.Count - Ord(coCustomColors in Options)) then
  begin
    Items[AIndex] := DisplayName;
    Items.Objects[AIndex] := TObject(AColor);
  end;
end;

function TJvColorComboBox.ColorName(AColor: TColor): string;
begin
  Result := GetColorName(AColor, '');
  if Result = '' then
    DoGetDisplayName(-1, AColor, Result);
end;

function TJvColorComboBox.FindColor(AColor: TColor): Integer;
begin
  Result := Items.IndexOfObject(TObject(AColor));
  if (coCustomColors in Options) and (Result = Items.Count - 1) then
    Result := -1;
end;

procedure TJvColorComboBox.GetCustomColors(AList: TList);
var
  I, J: Integer;
begin
  if AList = nil then
    Exit;
  J := Ord((coCustomColors in Options));
  for I := Items.Count - (CustomColorCount + J) to pred(Items.Count - J) do
    AList.Add(Items.Objects[I]);
end;

procedure TJvColorComboBox.InternalInsertColor(AIndex: Integer;
  AColor: TColor; const DisplayName: string);
begin
  Items.InsertObject(AIndex, DisplayName, TObject(AColor));
end;

procedure TJvColorComboBox.DoNameMapChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvColorComboBox.Loaded;
begin
  inherited;
  HandleNeeded;
  if HandleAllocated then
    GetColors;
end;

function TJvColorComboBox.GetDropDownWidth: Integer;
begin
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

procedure TJvColorComboBox.SetDropDownWidth(const Value: Integer);
begin
  SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

// === TJvFontComboBox =======================================================

constructor TJvFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrueTypeBmp := LoadInternalBitmap('TTF_FONT');
  FFixBmp := LoadInternalBitmap('FIX_FONT');
  FDeviceBmp := LoadInternalBitmap('PRN_FONT');
  FHiliteColor := clHighlight;
  FHiliteText := clHighlightText;
  FDevice := fdScreen;
  FUseImages := True;
  Style := csOwnerDrawFixed;
  AutoComplete := False;
  ResetItemHeight;
end;

destructor TJvFontComboBox.Destroy;
begin
  FTrueTypeBmp.Free;
  FDeviceBmp.Free;
  FFixBmp.Free;
  inherited Destroy;
end;

procedure TJvFontComboBox.GetFonts;
var
  DC: HDC;
begin
  HandleNeeded;
  if not HandleAllocated then
    Exit;
  Clear;
  DC := GetDC(0);
  try
    if FDevice in [fdScreen, fdBoth] then
      EnumFonts(DC, nil, @EnumFontsProc, Pointer(Self));
    if FDevice in [fdPrinter, fdBoth] then
    try
      EnumFonts(Printer.Handle, nil, @EnumFontsProc, Pointer(Self));
    except
        // (p3) exception might be raised if no printer is installed, but ignore it here
    end;
  finally
    ReleaseDC(0, DC);
  end;
  ItemIndex := 0;
end;

procedure TJvFontComboBox.SetOptions(Value: TJvFontComboOptions);
begin
  if Value <> Options then
  begin
    FOptions := Value;
    Reset;
  end;
end;

procedure TJvFontComboBox.SetUseImages(Value: Boolean);
begin
  if FUseImages <> Value then
  begin
    FUseImages := Value;
    Invalidate;
  end;
end;

procedure TJvFontComboBox.SetDevice(Value: TFontDialogDevice);
begin
  if Value <> FDevice then
  begin
    FDevice := Value;
    Reset;
  end;
end;

procedure TJvFontComboBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := ItemStateToOwnerDrawState(itemState);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := FHiliteColor;
      Canvas.Font.Color := FHiliteText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TJvFontComboBox.DrawItem(Index: Integer; R: TRect;
  State: TOwnerDrawState);
var
  aBmp: TBitmap;
  aColor: TColor;
  aWidth: Integer;
  aName: string;
begin
  with Canvas do
  begin
    aColor := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    //    aWidth  := 20;
    if (Integer(Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      aBmp := FTrueTypeBmp
    else
    if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      aBmp := FDeviceBmp
    else
      aBmp := FFixBmp;
    if not FUseImages then
      aBmp := nil;

    if aBmp <> nil then
    begin
      aWidth := aBmp.Width;
      BrushCopy(Bounds(R.Left + 2, (R.Top + R.Bottom - aBmp.Height) div 2,
        aBmp.Width, aBmp.Height), aBmp, Bounds(0, 0, aBmp.Width, aBmp.Height), clFuchsia);
      R.Left := R.Left + aWidth + 6;
    end;
    Brush.Color := aColor;
    aName := Canvas.Font.Name;
    if foWysiWyg in FOptions then
      Canvas.Font.Name := Items[Index];
    R.Right := R.Left + TextWidth(Items[Index]) + 6;
    FillRect(R);
    OffsetRect(R, 2, 0);
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    Canvas.Font.Name := aName;
    OffsetRect(R, -2, 0);
    if odSelected in State then
      DrawFocusRect(R);
  end;
end;

{procedure TJvFontComboBox.WMFontChange(var Msg: TMessage);
begin
  inherited;
  Reset;
end;}

procedure TJvFontComboBox.FontChanged;
begin
  inherited FontChanged;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvFontComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), FTrueTypeBmp.Height);
end;

procedure TJvFontComboBox.Click;
begin
  inherited Click;
  Change;
end;

procedure TJvFontComboBox.Reset;
var
  S: string;
begin
  if HandleAllocated then
  begin
    S := FontName;
    GetFonts;
    FontName := S;
  end;
end;

function TJvFontComboBox.GetFontName: string;
begin
  Result := inherited Text;
end;

procedure TJvFontComboBox.SetFontName(const Value: string);
begin
  if Value = '' then Exit;
  ItemIndex := Items.IndexOf(Value);
  if ItemIndex = -1 then // try to find the font substitute name
    ItemIndex := Items.IndexOf(FontSubstitute(Value));
  if (ItemIndex = -1) and (foDisableVerify in Options) then // add if allowed to
    ItemIndex := Items.AddObject(Value, TObject(TRUETYPE_FONTTYPE));
end;

procedure TJvFontComboBox.Loaded;
begin
  inherited Loaded;
  HandleNeeded;
  if HandleAllocated then
  begin
    GetFonts;
  end;
end;

function TJvFontComboBox.GetSorted: Boolean;
begin
  Result := inherited Sorted;
end;

procedure TJvFontComboBox.SetSorted(const Value: Boolean);
var
  S: string;
begin
  S := FontName;
  inherited Sorted := Value;
  FontName := S;
end;

function TJvFontComboBox.FontSubstitute(const FontName: string): string;
var
  aSize: DWORD;
  AKey: HKey;
begin
  Result := FontName;
  if FontName = '' then Exit;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar('SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes'), 0, KEY_QUERY_VALUE, AKey) = ERROR_SUCCESS then
  try
    if (RegQueryValueEx(AKey, PChar(FontName),
      nil, nil, nil, @aSize) = ERROR_SUCCESS) and (aSize > 0) then
    begin
      SetLength(Result, aSize);
      if RegQueryValueEx(AKey, PChar(FontName), nil, nil, PByte(@Result[1]), @aSize) = ERROR_SUCCESS then
        Result := string(Result)
      else
        Result := FontName;
    end;
  finally
    RegCloseKey(AKEy);
  end
  else
    Result := FontName;
end;

function TJvFontComboBox.GetDropDownWidth: Integer;
begin
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

procedure TJvFontComboBox.SetDropDownWidth(const Value: Integer);
begin
  SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

end.

