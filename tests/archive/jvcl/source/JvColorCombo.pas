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

Known Issues:
If you set AutoComplete in TJvColorComboBox to True and use the same text for
all Custom colors, the inherited Change behaviour from TJvComboBox makes the *first*
custom color selected, not the last added as it should be thus AutoComplete is
set to default to False. (p3)

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvColorCombo;

{ Comboboxes for displaying colors and fonts }

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs, Graphics, StdCtrls,
  Printers, JvComboBox;

type
  TJvNewColorEvent = procedure(Sender: TObject; Color: TColor; var DisplayName: string; var AllowAdd: Boolean) of
    object;
  TJvGetColorNameEvent = procedure(Sender: TObject; Index: Integer; Color: TColor; var DisplayName: string) of object;
  TJvColorComboOption = (coText, coHex, coRGB, coSysColors, coCustomColors);
  TJvColorComboOptions = set of TJvColorComboOption;

  TJvColorComboBox = class(TJvCustomComboBox)
  private
    FColorValue: TColor;
    FCustCnt: Integer;
    FHiLiteColor: TColor;
    FHiLiteText: TColor;
    FOptions: TJvColorComboOptions;
    FPrefix: string;
    FOther: string;
    FColWidth: Integer;
    FExecutingDialog: Boolean;
    FNewColor: TJvNewColorEvent;
    FOnGetDisplayName: TJvGetColorNameEvent;
    FColorNameMap: TStrings;
    FOnInsertColor: TJvNewColorEvent;
    FOnBeforeCustom: TNotifyEvent;
    procedure SetOptions(Value: TJvColorComboOptions);
    procedure SetOther(Value: string);
    procedure SetColWidth(Value: Integer);
    procedure SetColorValue(Value: TColor);
    procedure SetDroppedWidth(Value: Integer);
    function GetDroppedWidth: Integer;
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetPrefix(const Value: string);
    procedure SetColorNameMap(const Value: TStrings);
    procedure InitColorNames;
  protected
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure Click; override;

    function GetColorName(AColor: TColor; const Default: string): string;
    function DoNewColor(Color: TColor; var DisplayName: string): Boolean; virtual;
    procedure DoGetDisplayName(Index: Integer; AColor: TColor; var DisplayName: string); virtual;
    function DoInsertColor(AIndex: Integer; AColor: TColor; var DisplayName: string): Boolean; virtual;
    procedure DoBeforeCustom;
    procedure Change; override;
    procedure InternalInsertColor(AIndex: Integer; AColor: TColor; const DisplayName: string); virtual;
    procedure DoNameMapChange(Sender:TObject);
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
    {$ENDIF}
    property AutoSave;
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
    property ColorNameMap: TStrings read FColorNameMap write SetColorNameMap;
    property ColorValue: TColor read FColorValue write SetColorValue default clBlack;
    property ColorDialogText: string read FOther write SetOther;
    property ColorWidth: Integer read FColWidth write SetColWidth default 21;

    property NewColorText: string read FPrefix write SetPrefix;
    property Options: TJvColorComboOptions read FOptions write SetOptions default [coText];

    property DroppedDownWidth: Integer read GetDroppedWidth write SetDroppedWidth;
    property HiliteColor: TColor read FHiliteColor write FHiLiteColor default clHighLight;
    property HiliteText: TColor read FHiliteText write FHiLiteText default clHighLightText;
    // called before a new color is inserted as a result of displaying the Custom Colors dialog
    property OnNewColor: TJvNewColorEvent read FNewColor write FNewColor;
    // called before any color is inserted
    property OnInsertColor: TJvNewColorEvent read FOnInsertColor write FOnInsertColor;
    // called whenever the displayname of an item is needed
    property OnGetDisplayName: TJvGetColorNameEvent read FOnGetDisplayName write FOnGetDisplayName;
    // called just before the '(Other)' item is added at the bottom of the list
    property OnBeforeCustom: TNotifyEvent read FOnBeforeCustom write FOnBeforeCustom;

    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
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
    TrueTypeBMP: TBitmap;
    FixBMP: TBitmap;
    DeviceBMP: TBitmap;
    FDevice: TFontDialogDevice;
    FHiLiteColor: TColor;
    FHiLiteText: TColor;
    FUseImages: Boolean;
    FOptions: TJvFontComboOptions;
    procedure SetUseImages(Value: Boolean);
    procedure SetDevice(Value: TFontDialogDevice);
    procedure SetOptions(Value: TJvFontComboOptions);
    procedure ResetItemHeight;
    procedure Reset;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    function GetFontName: string;
    procedure SetFontName(const Value: string);
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  protected
    procedure GetFonts; virtual;
    procedure Click; override;

    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Text;
  published
    property Anchors;
    property AutoComplete default False;
    {$IFDEF COMPILER6_UP}
    property AutoDropDown;
    {$ENDIF}
    property AutoSave;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    property Color;
    property Ctl3D;
    property FontName: string read GetFontName write SetFontName;
    property Device: TFontDialogDevice read FDevice write SetDevice default fdScreen;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ItemIndex;
    property HiliteColor: TColor read FHiliteColor write FHiLiteColor default clHighLight;
    property HiliteText: TColor read FHiliteText write FHiLiteText default clHighLightText;
    property Options: TJvFontComboOptions read FOptions write SetOptions default [];
    property UseImages: Boolean read FUseImages write SetUseImages default True;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
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

resourcestring
  SOtherCaption = 'Custom...';
  SNewColorPrefix = 'Custom';

implementation
{$R *.RES}

const
  {$IFNDEF COMPILER6_UP}
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($F0CAA6);
  clCream = TColor($F0FBFF);
  clMedGray = TColor($A4A0A0);
  {$ENDIF}

  ColCount = 20;
  SysColCount = 25;
  ColorValues: array [1..ColCount] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clMoneyGreen, clSkyBlue, clCream, clMedGray);

  SysColorValues: array [1..SysColCount] of TColor = (
    clScrollBar, clBackground, clActiveCaption, clInactiveCaption, clMenu,
    clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText, clActiveBorder,
    clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText, clBtnFace,
    clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight,
    cl3DDkShadow, cl3DLight, clInfoText, clInfoBk);

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

function Max(Val1, Val2: Integer): Integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
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
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  aC: TJvFontComboBox;
begin
  aC := TJvFontComboBox(Data);
  Result := 0;
  if aC = nil then
    Exit;
  if IncludeFont(aC.Options, LogFont, FontType) then
  begin
    if aC.Items.IndexOf(string(LogFont.lfFaceName)) = -1 then
      aC.Items.AddObject(string(LogFont.lfFaceName), TObject(FontType));
  end;
  Result := 1;
end;

// === TJvColorComboBox ======================================================

constructor TJvColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorNameMap := TStringlist.Create;
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;
  FColWidth := 21;
  FPrefix := SNewColorPrefix;
  FOther := SOtherCaption;
  FOptions := [coText];
  FHiLiteColor := clHighLight;
  FHiLiteText := clHighLightText;
  AutoComplete := False;
  // make sure that if this is the first time the component is dropped on the form,
  // the default Name/Value map is created (thanks to Brian Cook on the borland NG's):
  if (Owner <> nil) and ([csDesigning, csLoading] * Owner.ComponentState = [csDesigning]) then
    InitColorNames;
  TStringlist(FColorNameMap).OnChange := DoNameMapChange;
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
  for I := 1 to ColCount do
  begin
    ColorName := GetColorName(ColorValues[I], '');
    InternalInsertColor(Items.Count, ColorValues[I], ColorName);
  end;
  if coSysColors in FOptions then
    for I := 1 to SysColCount do
    begin
      ColorName := GetColorName(SysColorValues[I], '');
      InternalInsertColor(Items.Count, SysColorValues[I], ColorName);
    end;
  DoBeforeCustom;
  if coCustomColors in FOptions then
    InternalInsertColor(Items.Count, $000001, FOther);
  SetColorValue(FColorValue);
end;

procedure TJvColorComboBox.SetDroppedWidth(Value: Integer);
begin
  SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

function TJvColorComboBox.GetDroppedWidth: Integer;
begin
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
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
    else if coHex in Value then
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
      ItemIndex := I;
    Change;
    Exit;
  end
  else
  if coCustomColors in Options then
  begin
    InsertColor(Items.Count - 1, Value, Format(FPrefix, [FCustCnt]));
        //      Items.InsertObject(Items.Count, FPrefix + IntToStr(FCustCnt), TObject(Value))
  end
  else
    AddColor(Value, Format(FPrefix, [FCustCnt]));
    //      Items.AddObject(FPrefix + IntToStr(FCustCnt), TObject(Value));
  ItemIndex := Items.Count - 2;
  FColorValue := Value;
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

procedure TJvColorComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State, odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State, odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State, odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State, odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State, odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State, odGrayed);
    if bool(itemState and ODS_SELECTED) then
      Include(State, odSelected);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := FHiLiteColor;
      Canvas.Font.Color := FHiLiteText;
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
    if ((coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions)) then
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

procedure TJvColorComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
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
    Result := FColorNameMap.Values['cl'+Tmp];
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
begin
  FColorNameMap.BeginUpdate;
  try
    FColorNameMap.Clear;
    FColorNameMap.Add('clBlack=Black');
    FColorNameMap.Add('clMaroon=Maroon');
    FColorNameMap.Add('clGreen=Green');
    FColorNameMap.Add('clOlive=Olive');
    FColorNameMap.Add('clNavy=Navy');
    FColorNameMap.Add('clPurple=Purple');
    FColorNameMap.Add('clTeal=Teal');
    FColorNameMap.Add('clGray=Gray');
    FColorNameMap.Add('clSilver=Silver');
    FColorNameMap.Add('clRed=Red');
    FColorNameMap.Add('clLime=Lime');
    FColorNameMap.Add('clYellow=Yellow');
    FColorNameMap.Add('clBlue=Blue');
    FColorNameMap.Add('clFuchsia=Fuchsia');
    FColorNameMap.Add('clAqua=Aqua');
    FColorNameMap.Add('clLtGray=Light Gray');
    FColorNameMap.Add('clDkGray=Dark Gray');
    FColorNameMap.Add('clWhite=White');
    FColorNameMap.Add('clMoneyGreen=Money Green');
    FColorNameMap.Add('clSkyBlue=Sky Blue');
    FColorNameMap.Add('clCream=Cream');
    FColorNameMap.Add('clMedGray=Medium Gray');

    FColorNameMap.Add('clScrollBar=ScrollBar');
    FColorNameMap.Add('clBackground=Background');
    FColorNameMap.Add('clActiveCaption=Active Caption');
    FColorNameMap.Add('clInactiveCaption=Inactive Caption');
    FColorNameMap.Add('clMenu=Menu');
    FColorNameMap.Add('clWindow=Window');
    FColorNameMap.Add('clWindowFrame=Window Frame');
    FColorNameMap.Add('clMenuText=Menu Text');
    FColorNameMap.Add('clWindowText=Window Text');
    FColorNameMap.Add('clCaptionText=Caption Text');
    FColorNameMap.Add('clActiveBorder=Active Border');
    FColorNameMap.Add('clInactiveBorder=Inactive Border');
    FColorNameMap.Add('clAppWorkSpace=Application Workspace');
    FColorNameMap.Add('clHighlight=Highlight');
    FColorNameMap.Add('clHighlightText=Highlight Text');
    FColorNameMap.Add('clBtnFace=Button Face');
    FColorNameMap.Add('clBtnShadow=Button Shadow');
    FColorNameMap.Add('clGrayText=Gray Text');
    FColorNameMap.Add('clBtnText=Button Text');
    FColorNameMap.Add('clInactiveCaptionText=Inactive Caption Text');
    FColorNameMap.Add('clBtnHighlight=Button Highlight');
    FColorNameMap.Add('cl3DDkShadow=3D Dark Shadow');
    FColorNameMap.Add('cl3DLight=3D Light');
    FColorNameMap.Add('clInfoText=Info Text');
    FColorNameMap.Add('clInfoBk=Info Background');
    FColorNameMap.Add('clHotLight=Hot Light');
    FColorNameMap.Add('clGradientActiveCaption=Gradient Active Caption');
    FColorNameMap.Add('clGradientInactiveCaption=Gradient Inactive Caption');
    FColorNameMap.Add('clMenuHighlight=Menu Highlight');
    FColorNameMap.Add('clMenuBar=MenuBar');
    FColorNameMap.Add('clNone=None');
    FColorNameMap.Add('clDefault=Default');
  finally
    FColorNameMap.EndUpdate;
  end;
end;

procedure TJvColorComboBox.Loaded;
begin
  inherited;
  HandleNeeded;
  if HandleAllocated then
  begin
    AutoSave.LoadValue(Integer(FColorValue));
    GetColors;
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

procedure TJvColorComboBox.Change;
begin
  if HandleAllocated then
  begin
    inherited;
    AutoSave.SaveValue(ColorValue);
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

// === TJvFontComboBox =======================================================

constructor TJvFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TrueTypeBMP := LoadInternalBitmap('TTF_FONT');
  FixBMP := LoadInternalBitmap('FIX_FONT');
  DeviceBMP := LoadInternalBitmap('PRN_FONT');
  FHiliteColor := clHighLight;
  FHiLiteText := clHighLightText;
  FDevice := fdScreen;
  FUseImages := True;
  Style := csOwnerDrawFixed;
  AutoComplete := False;
  ResetItemHeight;
end;

destructor TJvFontComboBox.Destroy;
begin
  TrueTypeBMP.Free;
  DeviceBMP.Free;
  FixBMP.Free;
  inherited Destroy;
end;

procedure TJvFontComboBox.GetFonts;
var
  DC: HDC;
  Proc: TFarProc;
begin
  HandleNeeded;
  if not HandleAllocated then
    Exit;
  Clear;
  DC := GetDC(0);
  try
    Proc := @EnumFontsProc;
    try
      if FDevice in [fdScreen, fdBoth] then
        EnumFonts(DC, nil, Proc, Pointer(Self));
      if FDevice in [fdPrinter, fdBoth] then
      try
        EnumFonts(Printer.Handle, nil, Proc, Pointer(Self));
      except
        // (p3) exception might be raised if no printer is installed, but ignore it here
      end;
    finally
      //      FreeProcInstance(Proc); // (p3) 16-bit not supported anyway
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

procedure TJvFontComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State, odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State, odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State, odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State, odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State, odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State, odGrayed);
    if bool(itemState and ODS_SELECTED) then
      Include(State, odSelected);
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
      aBmp := TrueTypeBMP
    else
    if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      aBmp := DeviceBMP
    else
      aBmp := FixBMP;
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
    if foWysiwyg in FOptions then
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

procedure TJvFontComboBox.WMFontChange(var Message: TMessage);
begin
  inherited;
  Reset;
end;

procedure TJvFontComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvFontComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), TrueTypeBMP.Height);
end;

procedure TJvFontComboBox.Click;
begin
  inherited Click;
  AutoSave.SaveValue(FontName);
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
  ItemIndex := Items.IndexOf(Value);
  if (ItemIndex = -1) and (foDisableVerify in Options) then
    ItemIndex := Items.AddObject(Value, TObject(TRUETYPE_FONTTYPE));
end;

procedure TJvFontComboBox.Loaded;
var
  S: string;
begin
  inherited Loaded;
  HandleNeeded;
  if HandleAllocated then
  begin
    GetFonts;
    S := FontName;
    if AutoSave.LoadValue(S) then
      FontName := S;
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

procedure TJvColorComboBox.DoNameMapChange(Sender: TObject);
begin
  Invalidate;
end;

end.

