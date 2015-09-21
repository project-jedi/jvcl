{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorCombo.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thrnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thrnqvist are Copyright (C) 2002 Peter Thrnqvist.
All Rights Reserved.

Contributor(s):
Brian Cook (borland.public.vcl.components.writing)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Comboboxes for displaying colors and fonts

Known Issues:
  If you set AutoComplete in TJvColorComboBox to True and use the same text for
  all Custom colors, the inherited Change behaviour from TJvComboBox makes the *first*
  custom color selected, not the last added as it should be thus AutoComplete is
  set to default to False. (p3)
-----------------------------------------------------------------------------}
// $Id$

unit JvColorCombo;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  Classes, Controls, Dialogs, Graphics,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}  
  JvCombobox;

type
  TJvNewColorEvent = procedure(Sender: TObject; Color: TColor; var DisplayName: string;
    var AllowAdd: Boolean) of object;
  TJvGetColorNameEvent = procedure(Sender: TObject; Index: Integer; Color: TColor;
    var DisplayName: string) of object;
  TJvColorComboOption = (coText, coHex, coRGB, coStdColors, coSysColors, coCustomColors);
  TJvColorComboOptions = set of TJvColorComboOption;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvColorComboBox = class(TJvCustomComboBox)
  private
    FColorValue: TColor;
    FCustomColorCount: Integer;
    FHiliteColor: TColor;
    FHiliteText: TColor;
    FOptions: TJvColorComboOptions;
    FNewColorText: string;
    FColorDialogText: string;
    FColorWidth, FUpdateCount: Integer;
    FExecutingDialog: Boolean;
    FNewColor: TJvNewColorEvent;
    FOnGetDisplayName: TJvGetColorNameEvent;
    FColorNameMap: TStringList;
    FOnInsertColor: TJvNewColorEvent;
    FOnBeforeCustom: TNotifyEvent;
    FCustomColors: TStrings;
    procedure SetOptions(Value: TJvColorComboOptions);
    procedure SetColorDialogText(Value: string);
    procedure SetColorWidth(Value: Integer);
    procedure SetColorValue(Value: TColor);
    procedure ResetItemHeight;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    function GetColorNameMap: TStrings;
    procedure SetColorNameMap(const Value: TStrings);
    procedure InitColorNames;
    function GetDropDownWidth: Integer;
    procedure SetDropDownWidth(const Value: Integer);
    function GetColor(Index: Integer): TColor;
    function IsColorNameMapStored: Boolean;
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
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BeginUpdate: Integer;
    function EndUpdate: Integer;
    procedure GetColors; virtual;
    function GetCustomColorsStrings: TStrings;
    procedure SetCustomColorsStrings(const Value: TStrings);
    procedure GetCustomColors(AList: TList);
    procedure SetCustomColors(AList: TList);
    // Returns the current name for AColor. Note that this implicitly might call the
    // OnGetDisplayName event if the protected GetColorName returns an empty string
    function ColorName(AColor: TColor): string;
    // returns the index of a specific color or -1 if not found
    function FindColor(AColor: TColor): Integer;

    procedure AddColor(AColor: TColor; const DisplayName: string);
    procedure ChangeColor(AIndex: Integer; AColor: TColor; const DisplayName: string);
    procedure InsertColor(AIndex: Integer; AColor: TColor; const DisplayName: string);
    property Text;
    property CustomColorCount: Integer read FCustomColorCount;
    property CustomColors: TStrings read GetCustomColorsStrings write SetCustomColorsStrings;

    property Colors[Index: Integer]: TColor read GetColor;
  published
    property Anchors;
    property AutoComplete default False;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    // color name map is a TStrings property that can contain name/value mappings on the form
    // ColorName=DisplayName
    // If the component finds a matching mapping, it will substitute the default value
    // with the value in the list, otherwise the default value wil be used
    // Example:
    // clBlack=Black
    property ColorNameMap: TStrings read GetColorNameMap write SetColorNameMap stored IsColorNameMapStored;
    property ColorValue: TColor read FColorValue write SetColorValue default clBlack;
    property ColorDialogText: string read FColorDialogText write SetColorDialogText;
    property ColorWidth: Integer read FColorWidth write SetColorWidth default 21;
    property DroppedDownWidth: Integer read GetDropDownWidth write SetDropDownWidth;
    property HiliteColor: TColor read FHiliteColor write FHiliteColor default clHighlight;
    property HiliteText: TColor read FHiliteText write FHiliteText default clHighlightText;
    property NewColorText: string read FNewColorText write FNewColorText;
    property Options: TJvColorComboOptions read FOptions write SetOptions default [coText, coStdColors];
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
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly, foWysiWyg, foDisableVerify,
    foPreviewFont, foMRU);
  // foDisableVerify: if True, allows you to insert a font name that doesn't exist (by assigning to FontName)
  TJvFontComboOptions = set of TJvFontComboOption;
  TJvDrawPreviewEvent = procedure(Sender: TObject; const AFontName: string;
    var APreviewText: string; ATextWidth: Integer; var DrawPreview: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
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
    FMRUCount: Integer;
    FWasMouse: Boolean;
    FShowMRU: Boolean;
    FMaxMRUCount, FUpdateCount: Integer;
    FOnDrawPreviewEvent: TJvDrawPreviewEvent;
    FFontSizes:TStrings;
    FEnumeratorDC:HDC;
    FSampleText: string;
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
    procedure SetShowMRU(const Value: Boolean);
    procedure SetMaxMRUCount(const Value: Integer);
    function GetFontSizes: TStrings;
    procedure SetSampleText(const Value: string);
    function GetSampleTextStored: Boolean;
  protected
    procedure FontChanged; override;
    procedure Loaded; override;
    procedure GetFonts; virtual;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure CloseUp; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetParent(AParent: TWinControl); override;
    function DoDrawPreview(const AFontName: string; var APreviewText: string;
      ATextWidth: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddToMRU: Integer;
    procedure ClearMRU;
    procedure Click; override;
    function BeginUpdate: Integer;
    function EndUpdate: Integer;
    function FontSubstitute(const AFontName: string): string;
    procedure FontSizeList(SizeList: TList);
    function IsTrueType: Boolean;
    property Text;
    property MRUCount: Integer read FMRUCount;
    // returns the supported font sizes or a set of default sizes for TrueType fonts
    property FontSizes: TStrings read GetFontSizes;
  published
    property Anchors;
    property AutoComplete default False;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    property Color;
    property DroppedDownWidth: Integer read GetDropDownWidth write SetDropDownWidth;
    property MaxMRUCount: Integer read FMaxMRUCount write SetMaxMRUCount;
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
    property SampleText: string read FSampleText write SetSampleText stored GetSampleTextStored;
    property OnDrawPreviewEvent: TJvDrawPreviewEvent read FOnDrawPreviewEvent write FOnDrawPreviewEvent;
  end;

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
  SysUtils, Math, StdCtrls, Printers,
  JvConsts, JvResources, JvTypes;

const
  DefaultSampleText = 'AbCdEfGhIj';

{$R JvColorCombo.res}

function LoadInternalBitmap(ResName: string): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Handle := LoadBitmap(HInstance, PChar(ResName));
end;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  AFont: HFONT;
  TM: TTextMetric;
begin
  DC := GetDC(HWND_DESKTOP);
  try
    AFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    SelectObject(DC, AFont);
  finally
    ReleaseDC(HWND_DESKTOP, DC);
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
  FontType: DWORD; FontCombo: TJvFontComboBox): Integer; stdcall;
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

//=== { TJvColorComboBox } ===================================================

constructor TJvColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomColors := TStringList.Create;
  FColorNameMap := TStringList.Create;
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;
  FColorWidth := 21;
  FNewColorText := RsNewColorPrefix;
  FColorDialogText := RsCustomCaption;
  FOptions := [coText, coStdColors];
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
  FCustomColors.Free;
  inherited Destroy;
end;

function TJvColorComboBox.BeginUpdate: Integer;
begin
  Inc(FUpdateCount);
  Result := FUpdateCount;
end;

function TJvColorComboBox.EndUpdate: Integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    GetColors
  else
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  Result := FUpdateCount;
end;

procedure TJvColorComboBox.GetColors;
var
  I: Integer;
  ColorName: string;
begin
  if FUpdateCount = 0 then
  begin
    Items.BeginUpdate;
    try
      Clear;
      FCustomColorCount := 0;
      if coStdColors in FOptions then
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
        InternalInsertColor(Items.Count, $000001, FColorDialogText);
      if Items.Count > 0 then
        SetColorValue(FColorValue);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TJvColorComboBox.SetOptions(Value: TJvColorComboOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    GetColors;
  end;
end;

procedure TJvColorComboBox.SetColorDialogText(Value: string);
var
  I: Integer;
begin
  if FColorDialogText <> Value then
  begin
    I := Items.IndexOf(FColorDialogText);
    while I > -1 do
    begin
      Items[I] := Value;
      I := Items.IndexOf(FColorDialogText);
    end;
    FColorDialogText := Value;
  end;
end;

procedure TJvColorComboBox.SetColorWidth(Value: Integer);
begin
  if FColorWidth <> Value then
  begin
    FColorWidth := Value;
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
    InsertColor(Items.Count - 1, Value, Format(FNewColorText, [FCustomColorCount]));
    // If we are executing the dialog, the FCustomColorCount value has already been incremented (see the Click method)
    if not FExecutingDialog then
      Inc(FCustomColorCount);
    FColorValue := Value;
    ItemIndex := Items.Count - 2;
  end
  else
  begin
    AddColor(Value, Format(FNewColorText, [FCustomColorCount]));
    FColorValue := Value;
    ItemIndex := Items.Count - 1;
    Change;
  end;
end;

function TJvColorComboBox.DoNewColor(Color: TColor; var DisplayName: string): Boolean;
begin
  Result := FindColor(Color) = -1;
  if Assigned(FNewColor) then
    FNewColor(Self, Color, DisplayName, Result);
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
  LRect: TRect;
  AColor: TColor;
  S: string;
begin
  if Index >= Items.Count then
    Exit;
  LRect := R;
  Inc(LRect.Top, 2);
  Inc(LRect.Left, 2);
  Dec(LRect.Bottom, 2);
  if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) or
    ((coCustomColors in FOptions) and (Index = Items.Count - 1)) then
    LRect.Right := LRect.Left + FColorWidth

  else
    Dec(LRect.Right, 3);

  with Canvas do
  begin
    AColor := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := clGray;
    OffsetRect(LRect, 2, 2);
    FillRect(LRect);
    OffsetRect(LRect, -2, -2);
    Brush.Color := TColor(Items.Objects[Index]);
    try
      Rectangle(LRect);
    finally
      Brush.Style := bsSolid;
      Brush.Color := AColor;
    end;
    if (coCustomColors in FOptions) and (Index = Items.Count - 1) then
    begin
      S := FColorDialogText;
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      Brush.Color := Self.Color;
      FillRect(R);
      R.Left := R.Left + 2;
      R.Right := R.Left + TextWidth(S) + 2;
      Brush.Color := AColor;
      if AColor = clNone then
        Brush.Style := bsFDiagonal
      else
      if AColor = clDefault then
        Brush.Style := bsBDiagonal;
      FillRect(R);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    end
    else
    if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) then
    begin
      S := Items[Index];
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      if S <> FColorDialogText then
      begin
        if coHex in FOptions then
          S := Format('0x%.6x', [ColorToRGB(TColor(Items.Objects[Index]))])
        else
        if coRGB in FOptions then
          S := Format('(%d,%d,%d)', [GetRValue(TColor(Items.Objects[Index])),
            GetGValue(TColor(Items.Objects[Index])), GetBValue(TColor(Items.Objects[Index]))]);
      end;
      R.Left := R.Left + FColorWidth + 6;
      R.Right := R.Left + TextWidth(S) + 6;
      if AColor = clNone then
        Brush.Style := bsFDiagonal
      else
      if AColor = clDefault then
        Brush.Style := bsBDiagonal;
      FillRect(R);
      OffsetRect(R, 2, 0);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
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
  S, Tmp: string;
  CD: TColorDialog;
begin
  if FExecutingDialog then
    Exit;
  try
    if (ItemIndex = Items.Count - 1) and (coCustomColors in FOptions) then
    begin
      FExecutingDialog := True;
      CD := TColorDialog.Create(Self);
      with CD do
      try
        CD.Color := ColorValue;
        CD.CustomColors := Self.CustomColors;
        Options := Options + [cdFullOpen, cdPreventFullOpen];
        S := FNewColorText;
        if Execute then
        begin
          Self.CustomColors := CD.CustomColors;
          if DoNewColor(CD.Color, S) then
            Inc(FCustomColorCount);
          Tmp := FNewColorText;
          try
            FNewColorText := S;
            ColorValue := CD.Color;
          finally
            FNewColorText := Tmp;
          end;
          Change;
        end
        else
          ItemIndex := Items.Count - 2;
      finally
        Free;
      end;
    end
    else
    if ItemIndex >= 0 then
      ColorValue := TColor(Items.Objects[ItemIndex]);
    inherited Click;
  finally
    FExecutingDialog := False;
  end;
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

procedure TJvColorComboBox.AddColor(AColor: TColor; const DisplayName: string);
var
  S: string;
begin
  S := DisplayName;
  if DoNewColor(AColor, S) then
  begin
    if coCustomColors in Options then
      Inc(FCustomColorCount);
    InternalInsertColor(Items.Count - Ord(coCustomColors in Options), AColor, S);
    if ItemIndex < 0 then ItemIndex := 0;
  end;
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

function TJvColorComboBox.GetColor(Index: Integer): TColor;
begin
  Result := TColor(Items.Objects[Index]);
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

function TJvColorComboBox.IsColorNameMapStored: Boolean;
var
  I, Offset: Integer;
begin
  Result := FColorNameMap.Count <> Length(ColorValues) + Length(SysColorValues);
  if not Result then
  begin
    Result := True;
    for I := Low(ColorValues) to High(ColorValues) do
      if FColorNameMap[I - Low(ColorValues)] <> ColorValues[I].Constant + '=' + ColorValues[I].Description then
        Exit;
    Offset := High(ColorValues) - Low(ColorValues) + 1;
    for I := Low(SysColorValues) to High(SysColorValues) do
      if FColorNameMap[Offset + I - Low(SysColorValues)] <> SysColorValues[I].Constant + '=' + SysColorValues[I].Description then
        Exit;
    Result := False;
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
  Items.BeginUpdate;
  try
    J := Ord((coCustomColors in Options));
    for I := Items.Count - (CustomColorCount + J) to pred(Items.Count - J) do
      AList.Add(Items.Objects[I]);
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvColorComboBox.SetCustomColors(AList: TList);
var
  I: Integer;
  AColor: TColor;
  S: string;
begin
  if AList = nil then
    Exit;
  Items.BeginUpdate;
  try
    for I := 0 to AList.Count - 1 do
    begin
      AColor := TColor(AList[I]);
      if AColor <> -1 then
      begin
        S := FNewColorText;
        if DoNewColor(AColor, S) then
        begin
          InsertColor(Items.Count - 1, AColor, Format(S, [FCustomColorCount]));
          Inc(FCustomColorCount);
        end;
      end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

function TJvColorComboBox.GetCustomColorsStrings: TStrings;
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  FCustomColors.BeginUpdate;
  try
    FCustomColors.Clear;
    GetCustomColors(AList);
    for I := 0 to AList.Count - 1 do
      FCustomColors.Values['Color' + Char(Ord('A') + I)] := Format('%.6x', [Integer(AList[I])]);
  finally
    AList.Free;
    FCustomColors.EndUpdate;
  end;
  Result := FCustomColors;
end;

procedure TJvColorComboBox.SetCustomColorsStrings(const Value: TStrings);
var
  AList: TList;
  AValue: string;
  I: Integer;
begin
  FCustomColors.Assign(Value);
  AList := TList.Create;
  FCustomColors.BeginUpdate;
  try
    for I := 0 to FCustomColors.Count - 1 do
    begin
      AValue := FCustomColors.Values['Color' + Char(Ord('A') + I)];
      if (AValue <> '') and (AValue <> 'FFFFFF') then
        AList.Add(Pointer(StrToInt('$' + AValue)));
    end;
    SetCustomColors(AList);
  finally
    AList.Free;
    FCustomColors.EndUpdate;
  end;
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
  inherited Loaded;
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

procedure TJvColorComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (Parent <> nil) and HandleAllocated then
    GetColors;
end;

//=== { TJvFontComboBox } ====================================================

constructor TJvFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrueTypeBmp := LoadInternalBitmap('JvFontComboBoxTTF');
  FFixBmp := LoadInternalBitmap('JvFontComboBoxFIX');
  FDeviceBmp := LoadInternalBitmap('JvFontComboBoxPRN');
  FSampleText := DefaultSampleText;
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
  FFontSizes.Free;
  inherited Destroy;
end;

procedure TJvFontComboBox.GetFonts;
var
  DC: HDC;
  MRUItems: TStringList;
  I: Integer;
begin
  if FUpdateCount = 0 then
  begin
    HandleNeeded;
    if not HandleAllocated then
      Exit;
    Items.BeginUpdate;
    MRUItems := TStringList.Create;
    try
      if FShowMRU then
        for I := 0 to MRUCount - 1 do
          MRUItems.AddObject(Items[I], Items.Objects[I]);
      Clear;
      DC := GetDC(HWND_DESKTOP);
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
        ReleaseDC(HWND_DESKTOP, DC);
      end;
      if FShowMRU then
        for I := MRUCount - 1 downto 0 do
        begin
          Items.InsertObject(0, MRUItems[I], MRUItems.Objects[I]);
        end;
    finally
      MRUItems.Free;
      Items.EndUpdate;
    end;
  end;
end;

procedure TJvFontComboBox.SetOptions(Value: TJvFontComboOptions);
begin
  if Value <> Options then
  begin
    FOptions := Value;
    if (foPreviewFont in FOptions) then
      Exclude(FOptions, foWysiWyg);
    SetShowMRU(foMRU in FOptions);
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

function TJvFontComboBox.DoDrawPreview(const AFontName: string;
  var APreviewText: string; ATextWidth: Integer): Boolean;
begin
  Result := ATextWidth < ClientWidth;
  if Assigned(FOnDrawPreviewEvent) then
    FOnDrawPreviewEvent(Self, AFontName, APreviewText, ATextWidth, Result);
end;

procedure TJvFontComboBox.DrawItem(Index: Integer; R: TRect;
  State: TOwnerDrawState);
var
  ABmp: TBitmap;
  AColor: TColor;
  AWidth: Integer;
  TmpRect: TRect;
  S, AName: string;
begin
  with Canvas do
  begin
    AColor := Brush.Color;
    Brush.Color := Color;
    Pen.Color := Font.Color;
    FillRect(R);
    Inc(R.Top);
    //    AWidth  := 20;
    if (Integer(Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      ABmp := FTrueTypeBmp
    else
    if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      ABmp := FDeviceBmp
    else
      ABmp := FFixBmp;
    if not FUseImages then
      ABmp := nil;

    if ABmp <> nil then
    begin
      AWidth := ABmp.Width;
      BrushCopy(Bounds(R.Left + 2, (R.Top + R.Bottom - ABmp.Height) div 2,
        ABmp.Width, ABmp.Height), ABmp, Bounds(0, 0, ABmp.Width, ABmp.Height), clFuchsia);
      R.Left := R.Left + AWidth + 6;
    end
    else
      AWidth := 4;
    Brush.Color := AColor;
    AName := Canvas.Font.Name;
    if foWysiWyg in FOptions then
    begin
      if (foPreviewFont in Options) then
        Canvas.Font.Name := Self.Font.Name
      else
        Canvas.Font.Name := Items[Index];
    end;
    if not (foPreviewFont in Options) then
      R.Right := R.Left + TextWidth(Items[Index]) + 6;
    FillRect(R);
    OffsetRect(R, 2, 0);
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    if (foPreviewFont in Options) then
    begin
      Inc(AWidth, TextWidth(Items[Index]) + 36);
      Canvas.Font.Name := Items[Index];
      S := FSampleText;
      Inc(AWidth, TextWidth(S));
      if DoDrawPreview(Items[Index], S, AWidth) then
      begin
        TmpRect := R;
        TmpRect.Left := 0;
        TmpRect.Right := ClientWidth - (GetSystemMetrics(SM_CXVSCROLL) + 8);
        R.Right := ClientWidth;
        DrawText(Canvas.Handle, PChar(S), -1, TmpRect, DT_SINGLELINE or DT_VCENTER or DT_RIGHT or DT_NOPREFIX);
      end;
    end;
    Canvas.Font.Name := AName;
    OffsetRect(R, -2, 0);
    if odSelected in State then
      DrawFocusRect(R);
    if FShowMRU and not (odComboBoxEdit in State) then
    begin
      // draw MRU separator
      Dec(R.Top);
      if (Index = MRUCount - 1) then
      begin
        Canvas.Pen.Color := clGray;
        Canvas.Pen.Width := 1;
        Canvas.MoveTo(0, R.Bottom - 1);
        Canvas.LineTo(ClientWidth, R.Bottom - 1);
      end
      else
      if (Index = MRUCount) and (Index > 0) then
      begin
        Canvas.Pen.Color := clGray;
        Canvas.Pen.Width := 1;
        Canvas.MoveTo(0, R.Top + 1);
        Canvas.LineTo(ClientWidth, R.Top + 1);
      end;
    end;
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

function TJvFontComboBox.BeginUpdate: Integer;
begin
  Inc(FUpdateCount);
  Result := FUpdateCount;
end;

function TJvFontComboBox.EndUpdate: Integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    GetFonts
  else
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  Result := FUpdateCount;
end;

procedure TJvFontComboBox.Click;
begin
  inherited Click;
  Change;
  if FShowMRU and FWasMouse and not DroppedDown then
  begin
    ItemIndex := AddToMRU;
    FWasMouse := False;
  end;
end;

procedure TJvFontComboBox.Reset;
var
  S: string;
begin
  HandleNeeded;
  if HandleAllocated then
  begin
    FreeAndNil(FFontSizes);
    S := FontName;
    GetFonts;
    if S <> '' then
      FontName := S
    else
      FontName := Font.Name;
  end;
end;

function TJvFontComboBox.GetFontName: string;
begin
  Result := inherited Text;
end;

procedure TJvFontComboBox.SetFontName(const Value: string);
begin
  HandleNeeded;
  if HandleAllocated and (Value <> '') then
  begin
    if Items.Count = 0 then
      GetFonts;
    ItemIndex := Items.IndexOf(Value);
    if ItemIndex = -1 then // try to find the font substitute name
      ItemIndex := Items.IndexOf(FontSubstitute(Value));
    if (ItemIndex = -1) and (foDisableVerify in Options) then // add if allowed to
      ItemIndex := Items.AddObject(Value, TObject(TRUETYPE_FONTTYPE));
    FreeAndNil(FFontSizes);
  end;
end;

procedure TJvFontComboBox.Loaded;
begin
  inherited Loaded;
  Reset;
end;

function TJvFontComboBox.GetSampleTextStored: Boolean;
begin
  result := FSampleText <> DefaultSampleText;
end;

function TJvFontComboBox.GetSorted: Boolean;
begin
  Result := inherited Sorted;
end;

procedure TJvFontComboBox.SetSorted(const Value: Boolean);
var
  S: string;
begin
  if Value <> inherited Sorted then
  begin
    S := FontName;
    if not FShowMRU then
      inherited Sorted := Value
    else
      inherited Sorted := False;
    FontName := S;
  end;
end;

function TJvFontComboBox.FontSubstitute(const AFontName: string): string;
var
  Size: DWORD;
  AKey: HKey;
begin
  Result := AFontName;
  if AFontName = '' then
    Exit;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes',
    0, KEY_QUERY_VALUE, AKey) = ERROR_SUCCESS then
  try
    if (RegQueryValueEx(AKey, PChar(AFontName), nil, nil, nil, @Size) = ERROR_SUCCESS) and
       (Size > 0) then
    begin
      SetLength(Result, Size);
      if RegQueryValueEx(AKey, PChar(AFontName), nil, nil, PByte(@Result[1]), @Size) = ERROR_SUCCESS then
        Result := string(Result)
      else
        Result := AFontName;
    end;
  finally
    RegCloseKey(AKey);
  end
  else
    Result := AFontName;
end;

function TJvFontComboBox.GetDropDownWidth: Integer;
begin
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

procedure TJvFontComboBox.SetDropDownWidth(const Value: Integer);
begin
  SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

procedure TJvFontComboBox.SetSampleText(const Value: string);
begin
  if value <> FSampleText then
  begin
    FSampleText := value;
    Invalidate;
  end;
end;

procedure TJvFontComboBox.SetShowMRU(const Value: Boolean);
begin
  if FShowMRU <> Value then
  begin
    if FShowMRU then
      ClearMRU;
    FShowMRU := Value;
    if FShowMRU and Sorted then
      Sorted := False;
  end;
end;

var
  FPixelsPerInch: Integer = 96;

function GetFontSizesEnum(var lpelf: TEnumLogFont; var lpntm: TNewTextMetric;
  FontType: Integer; lParam: Integer): Integer; stdcall;
var
  aSize: Integer;
begin
  aSize := MulDiv(lpelf.elfLogFont.lfHeight, 72, FPixelsPerInch);
  if TList(lParam).IndexOf(Pointer(aSize)) < 0 then
    TList(lParam).Add(Pointer(aSize));
  Result := 1;
end;

function SizeSort(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(Item1) - Integer(Item2); // The list contains integers, casted to pointers
end;

function TJvFontComboBox.IsTrueType: Boolean;
begin
  if ItemIndex >= 0 then
    Result := (Integer(Items.Objects[ItemIndex]) and TRUETYPE_FONTTYPE) <> 0
  else
    Result := False;
end;

procedure TJvFontComboBox.FontSizeList(SizeList: TList);
const
  cTTSizes: array [0..15] of Integer =
    (8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72);
var
  DC: HDC;
  I:Integer;
begin
  if SizeList = nil then
    Exit;
  SizeList.Clear;
  if IsTrueType then
  begin
    // fill in constant sizes for true type fonts
    SizeList.Clear;
    for I := Low(cTTSizes) to High(cTTSizes) do
      SizeList.Add(Pointer(cTTSizes[I]));
  end
  else
  begin
    DC := GetDC(HWND_DESKTOP);
    try
      FPixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
      EnumFontFamilies(DC, PChar(FontName), @GetFontSizesEnum, LPARAM(SizeList));
      SizeList.Sort(SizeSort);
    finally
      ReleaseDC(HWND_DESKTOP, DC);
    end;
  end;
end;

function TJvFontComboBox.AddToMRU: Integer;
var
  I: Integer;
begin
  Result := ItemIndex;
  if csDesigning in ComponentState then
    Exit;
  if (MaxMRUCount = 0) or (MaxMRUCount > MRUCount) then
  begin
    I := Items.IndexOf(Text);
    if (I > MRUCount - 1) and (I >= 0) then
    begin
      Items.InsertObject(0, Items[I], Items.Objects[I]);
      Inc(FMRUCount);
    end
    else
    if I < 0 then
    begin
      Items.InsertObject(0, Text, TObject(TRUETYPE_FONTTYPE));
      Inc(FMRUCount);
    end;
    Result := 0;
  end
  else
  if (MRUCount > 0) and (ItemIndex > 0) then
  begin
    Items[0] := Items[ItemIndex];
    Items.Objects[0] := Items.Objects[ItemIndex];
    Result := 0;
  end;
end;

procedure TJvFontComboBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FWasMouse := False;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvFontComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FWasMouse := True;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvFontComboBox.CloseUp;
begin
  inherited CloseUp;
  if FShowMRU then
  begin
    AddToMRU;
    ItemIndex := Items.IndexOf(Text);
    FWasMouse := False;
  end;
end;

procedure TJvFontComboBox.ClearMRU;
begin
  while FMRUCount > 0 do
  begin
    Items.Delete(0);
    Dec(FMRUCount);
  end;
end;

procedure TJvFontComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // (rom) only accept without Shift, Alt or Ctrl down
  if (Shift * KeyboardShiftStates = []) and
    (Key = VK_RETURN) and FShowMRU then
    ItemIndex := AddToMRU;
  inherited KeyDown(Key, Shift);
end;

procedure TJvFontComboBox.SetMaxMRUCount(const Value: Integer);
var
  S: string;
begin
  if FMaxMRUCount <> Value then
  begin
    FMaxMRUCount := Value;
    if (FMaxMRUCount > 0) and (FMRUCount > 0) then
    begin
      S := Text;
      while FMRUCount > FMaxMRUCount do
      begin
        Items.Delete(0);
        Dec(FMRUCount);
      end;
      ItemIndex := Items.IndexOf(S);
      if ItemIndex < 0 then
        ItemIndex := 0;
    end;
  end;
end;

procedure TJvFontComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if (Parent <> nil) then
    FontName := Font.Name;
end;

function EnumFontSizeProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; FontCombo: TJvFontComboBox): Integer; stdcall;
var
  tmp: Integer;
begin
  if FontType and TRUETYPE_FONTTYPE <> TRUETYPE_FONTTYPE then // TTF's don't have size info
  begin
    tmp := Round(((TextMetric.tmHeight - TextMetric.tmInternalLeading) * 72) / GetDeviceCaps(FontCombo.FEnumeratorDC, LOGPIXELSY));
    FontCombo.FFontSizes.AddObject(IntToStr(tmp), TObject(tmp));
    Result := 1;
  end
  else
    Result := 0;
end;

function IntegerSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToIntDef(List[Index1], 0) - StrToIntDef(List[Index2], 0);
end;

function TJvFontComboBox.GetFontSizes: TStrings;
begin
  if FFontSizes = nil then
    FFontSizes := TStringlist.Create;
  FFontSizes.Clear;
  TStringList(FFontSizes).Sorted := True;

  FEnumeratorDC := GetDC(HWND_DESKTOP);
  try
    if FDevice in [fdScreen, fdBoth] then
      EnumFonts(FEnumeratorDC, PChar(FontName), @EnumFontSizeProc, Pointer(Self));
  finally
    ReleaseDC(HWND_DESKTOP, FEnumeratorDC);
  end;
  if FDevice in [fdPrinter, fdBoth] then
  try
    FEnumeratorDC := Printer.Handle;
    EnumFonts(FEnumeratorDC,  PChar(FontName), @EnumFontSizeProc, Pointer(Self));
  except
    // ignore exceptions (printer may not be installed)
  end;

  TStringlist(FFontSizes).Sorted := False;
  if FFontSizes.Count > 1 then
    TStringList(FFontSizes).CustomSort(IntegerSort)
  else // true type font or font with only one size, so fake it:
  begin
    FFontSizes.Clear;
    FFontSizes.AddObject('8', TObject(8));
    FFontSizes.AddObject('9', TObject(9));
    FFontSizes.AddObject('10', TObject(10));
    FFontSizes.AddObject('11', TObject(11));
    FFontSizes.AddObject('12', TObject(12));
    FFontSizes.AddObject('14', TObject(14));
    FFontSizes.AddObject('16', TObject(16));
    FFontSizes.AddObject('18', TObject(18));
    FFontSizes.AddObject('20', TObject(20));
    FFontSizes.AddObject('22', TObject(22));
    FFontSizes.AddObject('24', TObject(24));
    FFontSizes.AddObject('26', TObject(26));
    FFontSizes.AddObject('28', TObject(28));
    FFontSizes.AddObject('36', TObject(36));
    FFontSizes.AddObject('48', TObject(48));
    FFontSizes.AddObject('72', TObject(72));
  end;
  Result := FFontSizes;
end;

{$IFDEF UNITVERSIONING}


initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

