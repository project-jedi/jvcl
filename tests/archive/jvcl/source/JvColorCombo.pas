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

{ Comboboxes for displaying colors and fonts }

unit JvColorCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs, Graphics, StdCtrls,
  Printers,
  JvComboBox;

type
  TJvNewColorEvent = procedure(Sender: TObject; Color: TColor; var DisplayName: string; var AllowAdd: Boolean) of
    object;
  TJvGetColorNameEvent = procedure(Sender: TObject; Index: Integer; Color: TColor; var DisplayName: string) of object;
  TJvColorComboOption = (coText, coHex, coRGB, coSysColors, coCustomColors);
  TJvColorComboOptions = set of TJvColorComboOption;

  TJvColorComboBox = class(TJvCustomComboBox)
  private
    FColorValue: TColor;
    FCustomColorCount: Integer;
    FHiliteColor: TColor;
    FHiliteText: TColor;
    FOptions: TJvColorComboOptions;
    FNewColorText: string;
    FColorDialogText: string;
    FColorWidth: Integer;
    FOnNewColor: TJvNewColorEvent;
    FOnGetDisplayName: TJvGetColorNameEvent;
    FColorNameMap: TStrings;
    FOnInsertColor: TJvNewColorEvent;
    FOnBeforeCustom: TNotifyEvent;
    procedure SetOptions(Value: TJvColorComboOptions);
    procedure SetColorDialogText(Value: string);
    procedure SetColWidth(Value: Integer);
    procedure SetColorValue(Value: TColor);
    procedure SetDroppedDownWidth(Value: Integer);
    function GetDroppedDownWidth: Integer;
    procedure ResetItemHeight;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure SetNewColorText(const Value: string);
    procedure SetColorNameMap(const Value: TStrings);
    procedure InitColorNames;
  protected
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure Click; override;

    function GetColorName(Colr: TColor; const Default: string): string;
    function DoNewColor(Color: TColor; var DisplayName: string): Boolean; virtual;
    procedure DoGetDisplayName(Index: Integer; Colr: TColor; var DisplayName: string); virtual;
    function DoInsertColor(AIndex: Integer; Colr: TColor; var DisplayName: string): Boolean; virtual;
    procedure DoBeforeCustom;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure GetColors; virtual;
    // Returns the current name for Colr. Note that this implicitly might call the
    // OnGetDisplayName event if the protected GetColorName returns an emtpy string
    function ColorName(Colr: TColor): string;

    procedure AddColor(Colr: TColor; const DisplayName: string);
    procedure ChangeColor(AIndex: Integer; Colr: TColor; const DisplayName: string);
    procedure InsertColor(AIndex: Integer; Colr: TColor; const DisplayName: string);
    property Text;
    property CustomColorCount: Integer read FCustomColorCount;
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
    // color name map is a TStrings property that can contain name/value mappings on the form
    // colorName=DisplayName
    // if the component finds a matching mapping, it will substitute the default value
    // with the value in the list, otherwise the default value wil be used
    // Example:
    // clBlack=Black
    property ColorNameMap: TStrings read FColorNameMap write SetColorNameMap;
    property ColorValue: TColor read FColorValue write SetColorValue default clBlack;
    property ColorDialogText: string read FColorDialogText write SetColorDialogText;
    property ColorWidth: Integer read FColorWidth write SetColWidth default 21;

    property NewColorText: string read FNewColorText write SetNewColorText;
    property Options: TJvColorComboOptions read FOptions write SetOptions default [coText];

    property DroppedDownWidth: Integer read GetDroppedDownWidth write SetDroppedDownWidth;
    property HiliteColor: TColor read FHiliteColor write FHiliteColor default clHighLight;
    property HiliteText: TColor read FHiliteText write FHiliteText default clHighLightText;
    // called before a new color is inserted as a result of displaying the Custom Colors dialog
    property OnNewColor: TJvNewColorEvent read FOnNewColor write FOnNewColor;
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
    FTrueTypeBMP: TBitmap;
    FFixBMP: TBitmap;
    FDeviceBMP: TBitmap;
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
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure WMFontChange(var Msg: TMessage); message WM_FONTCHANGE;
    function GetFontName: string;
    procedure SetFontName(const Value: string);
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  protected
    procedure GetFonts; virtual;
    procedure Click; override;

    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
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
    property HiliteColor: TColor read FHiliteColor write FHiliteColor default clHighLight;
    property HiliteText: TColor read FHiliteText write FHiliteText default clHighLightText;
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

implementation

{$R *.RES}

resourcestring
  SOtherCaption = 'Custom...';
  SNewColorPrefix = 'Custom';

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
  Fnt: HFONT;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    Fnt := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    SelectObject(DC, Fnt);
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
  CB: TJvFontComboBox;
begin
  CB := TJvFontComboBox(Data);
  Result := 0;
  if CB = nil then
    Exit;
  if IncludeFont(CB.Options, LogFont, FontType) then
  begin
    if CB.Items.IndexOf(LogFont.lfFaceName) = -1 then
      CB.Items.AddObject(LogFont.lfFaceName, TObject(FontType));
  end;
  Result := 1;
end;

constructor TJvColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorNameMap := TStringlist.Create;
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;
  FColorWidth := 21;
  FNewColorText := SNewColorPrefix;
  FColorDialogText := SOtherCaption;
  FOptions := [coText];
  FHiliteColor := clHighLight;
  FHiliteText := clHighLightText;
  AutoComplete := False;
  // make sure that if this is the first time the component is dropped on the form,
  // the default Name/Value map is created (thanks to Brian Cook on the borland NG's):
  if (Owner <> nil) and ([csDesigning, csLoading] * Owner.ComponentState = [csDesigning]) then
    InitColorNames;
end;

procedure TJvColorComboBox.GetColors;
var
  I: Integer;
  ColorName: string;
begin
  Clear;
  for I := 1 to ColCount do
  begin
    ColorName := GetColorName(ColorValues[I], '');
    AddColor(ColorValues[I], ColorName);
  end;
  if coSysColors in FOptions then
    for I := 1 to SysColCount do
    begin
      ColorName := GetColorName(SysColorValues[I], '');
      AddColor(SysColorValues[I], ColorName);
    end;
  DoBeforeCustom;
  if coCustomColors in FOptions then
    AddColor($000001, FColorDialogText);
  SetColorValue(FColorValue);
end;

procedure TJvColorComboBox.SetDroppedDownWidth(Value: Integer);
begin
  SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

function TJvColorComboBox.GetDroppedDownWidth: Integer;
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
      AddColor($000001, FColorDialogText);
    Invalidate;
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

procedure TJvColorComboBox.SetColWidth(Value: Integer);
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
  I := Items.IndexOfObject(TObject(Value));
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
    InsertColor(Items.Count - 1, Value, Format(FNewColorText, [FCustomColorCount]))
        //      Items.InsertObject(Items.Count, FNewColorText + IntToStr(FCustomColorCount), TObject(Value))
  else
    AddColor(Value, Format(FNewColorText, [FCustomColorCount]));
    //      Items.AddObject(FNewColorText + IntToStr(FCustomColorCount), TObject(Value));
  Inc(FCustomColorCount);
  ItemIndex := Items.Count - 2;
  FColorValue := Value;
end;

function TJvColorComboBox.DoNewColor(Color: TColor; var DisplayName: string): Boolean;
begin
  if Assigned(FOnNewColor) then
    FOnNewColor(Self, Color, DisplayName, Result)
  else
    Result := Items.IndexOfObject(TObject(Color)) = -1;
end;

procedure TJvColorComboBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
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

procedure TJvColorComboBox.DrawItem(Index: Integer; R: TRect;
  State: TOwnerDrawState);
var
  Rect: TRect;
  Colr: TColor;
  S: string;
begin
  Rect := R;
  Inc(Rect.Top, 2);
  Inc(Rect.Left, 2);
  Dec(Rect.Bottom, 2);
  if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) or
    ((coCustomColors in FOptions) and (Index = Items.Count - 1)) then
    Rect.Right := Rect.Left + FColorWidth

  else
    Dec(Rect.Right, 3);

  with Canvas do
  begin
    Colr := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := clGray;
    OffsetRect(Rect, 2, 2);
    FillRect(Rect);
    OffsetRect(Rect, -2, -2);
    Brush.Color := TColor(Items.Objects[Index]);
    try
      Rectangle(Rect);
    finally
      Brush.Color := Colr;
    end;
    if (coCustomColors in FOptions) and (Index = Items.Count - 1) then
    begin
      S := FColorDialogText;
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      Brush.Color := Self.Color;
      FillRect(R);
      R.Left := R.Left + 2;
      R.Right := R.Left + TextWidth(S) + 2;
      Brush.Color := Colr;
      FillRect(R);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    end
    else
    if ((coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions)) then
    begin
      S := Items[Index];
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      if (S <> FColorDialogText) then
      begin
        if (coHex in FOptions) then
          S := Format('0x%.6x', [ColorToRGB(TColor(Items.Objects[Index]))])
        else
        if (coRGB in Foptions) then
          S := Format('(%d,%d,%d)', [GetRValue(TColor(Items.Objects[Index])), GetGValue(TColor(Items.Objects[Index])),
            GetBValue(TColor(Items.Objects[Index]))]);
      end;
      R.Left := R.Left + FColorWidth + 6;
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
  if (ItemIndex = Items.Count - 1) and (coCustomColors in FOptions) then
  begin
    CD := TColorDialog.Create(Self);
    with CD do
    try
      CD.Color := ColorValue;
      Options := Options + [cdFullOpen, cdPreventFullOpen];
      S := FNewColorText;
      if Execute and DoNewColor(CD.Color, S) then
      begin
        Inc(FCustomColorCount);
        InsertColor(Items.Count - 1, CD.Color, S);
        ItemIndex := Items.Count - 2;
//        ColorValue := CD.Color;
      end;
//      else
      ColorValue := CD.Color;
    finally
      Free;
    end;
  end
  else
  if ItemIndex >= 0 then
    ColorValue := TColor(Items.Objects[ItemIndex]);
  inherited Click;
end;

procedure TJvColorComboBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvColorComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), 9);
end;

procedure TJvColorComboBox.SetNewColorText(const Value: string);
begin
  FNewColorText := Value;
end;

procedure TJvColorComboBox.AddColor(Colr: TColor;
  const DisplayName: string);
begin
  InsertColor(Items.Count, Colr, DisplayName);
end;

procedure TJvColorComboBox.DoGetDisplayName(Index: Integer; Colr: TColor;
  var DisplayName: string);
begin
  if Assigned(FOnGetDisplayName) then
    FOnGetDisplayName(Self, Index, Colr, DisplayName)
  else
    DisplayName := GetColorName(Colr, DisplayName);
end;

procedure TJvColorComboBox.InsertColor(AIndex: Integer; Colr: TColor;
  const DisplayName: string);
var
  S: string;
begin
  S := DisplayName;
  if DoInsertColor(AIndex, Colr, S) then
    Items.InsertObject(AIndex, S, TObject(Colr));
end;

procedure TJvColorComboBox.SetColorNameMap(const Value: TStrings);
begin
  FColorNameMap.Assign(Value);
  Invalidate;
end;

destructor TJvColorComboBox.Destroy;
begin
  FColorNameMap.Free;
  inherited Destroy;
end;

function TJvColorComboBox.GetColorName(Colr: TColor; const Default: string): string;
var
  Tmp: string;
begin
  if (Default <> '') then
  begin
    Result := Default;
    Exit;
  end;
  Tmp := ColorToString(Colr);
  Result := FColorNameMap.Values[Tmp];
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
  inherited Loaded;
  HandleNeeded;
  if HandleAllocated then
  begin
    AutoSave.LoadValue(Integer(FColorValue));
    GetColors;
  end;
end;

function TJvColorComboBox.DoInsertColor(AIndex: Integer; Colr: TColor;
  var DisplayName: string): Boolean;
begin
  Result := True;
  if Assigned(FOnInsertColor) then
    FOnInsertColor(Self, Colr, DisplayName, Result);
end;

procedure TJvColorComboBox.DoBeforeCustom;
begin
  if Assigned(FOnBeforeCustom) then
    FOnBeforeCustom(Self);
end;

procedure TJvColorComboBox.ChangeColor(AIndex: Integer; Colr: TColor;
  const DisplayName: string);
begin
  // raise Exception ?
  if (AIndex >= 0) and (AIndex < Items.Count - Ord(coCustomColors in Options)) then
  begin
    Items[AIndex] := DisplayName;
    Items.Objects[AIndex] := TObject(Colr);
  end;
end;

procedure TJvColorComboBox.Change;
begin
  if HandleAllocated then
  begin
    inherited Change;
    AutoSave.SaveValue(ColorValue);
  end;
end;

function TJvColorComboBox.ColorName(Colr: TColor): string;
begin
  Result := GetColorName(Colr, '');
  if Result = '' then
    DoGetDisplayName(-1, Colr, Result);
end;

{ TJvFontComboBox }

constructor TJvFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrueTypeBMP := LoadInternalBitmap('TTF_FONT');
  FFixBMP := LoadInternalBitmap('FIX_FONT');
  FDeviceBMP := LoadInternalBitmap('PRN_FONT');
  FHiliteColor := clHighLight;
  FHiliteText := clHighLightText;
  FDevice := fdScreen;
  FUseImages := True;
  Style := csOwnerDrawFixed;
  AutoComplete := False;
  ResetItemHeight;
end;

destructor TJvFontComboBox.Destroy;
begin
  FTrueTypeBMP.Free;
  FDeviceBMP.Free;
  FFixBMP.Free;
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

procedure TJvFontComboBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
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
  Bmp: TBitmap;
  Colr: TColor;
  W: Integer;
  Name: string;
begin
  with Canvas do
  begin
    Colr := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    //    aWidth  := 20;
    if (Integer(Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      Bmp := FTrueTypeBMP
    else
    if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      Bmp := FDeviceBMP
    else
      Bmp := FFixBMP;
    if not FUseImages then
      Bmp := nil;

    if Bmp <> nil then
    begin
      W := Bmp.Width;
      BrushCopy(Bounds(R.Left + 2, (R.Top + R.Bottom - Bmp.Height) div 2,
        Bmp.Width, Bmp.Height), Bmp, Bounds(0, 0, Bmp.Width, Bmp.Height), clFuchsia);
      R.Left := R.Left + W + 6;
    end;
    Brush.Color := Colr;
    Name := Canvas.Font.Name;
    if foWysiwyg in FOptions then
      Canvas.Font.Name := Items[Index];
    R.Right := R.Left + TextWidth(Items[Index]) + 6;
    FillRect(R);
    OffsetRect(R, 2, 0);
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    Canvas.Font.Name := Name;
    OffsetRect(R, -2, 0);
    if odSelected in State then
      DrawFocusRect(R);
  end;
end;

procedure TJvFontComboBox.WMFontChange(var Msg: TMessage);
begin
  inherited;
  Reset;
end;

procedure TJvFontComboBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvFontComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), FTrueTypeBMP.Height);
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
  inherited;
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

end.

