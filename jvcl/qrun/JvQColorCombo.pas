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
VisualCLX only. Ported from JVCL 2.1 !!!

If you set AutoComplete in TJvColorComboBox to True and use the same text for
all Custom colors, the inherited Change behaviour from TJvComboBox makes the *first*
custom color selected, not the last added as it should be thus AutoComplete is
set to default to False. (p3)



-----------------------------------------------------------------------------}
// $Id = $
{$I jvcl.inc}

unit JvQColorCombo;

{ Comboboxes for displaying colors and fonts }

interface

uses
  SysUtils, Classes,
  QForms, QControls, QDialogs, QGraphics, QStdCtrls, Qt,
  QWindows, JvQComboBox;

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
    FHexPrefix: string;
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
    procedure ResetItemHeight;
    procedure SetPrefix(const Value: string);
    procedure SetHexPrefix(const Value: string);
    procedure SetColorNameMap(const Value: TStrings);
    procedure InitColorNames;
  protected
    procedure FontChanged; override ;
    procedure ItemDraw(Sender: TObject;Index: Integer; R: TRect;
       State: TOwnerDrawState; var Handled: Boolean ) ;
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
//    property AutoSave;
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
    property HexPrefix: string read FHexPrefix  write SetHexPrefix;
    property NewColorText: string read FPrefix write SetPrefix;
    property Options: TJvColorComboOptions read FOptions write SetOptions default [coText];
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
    property DragMode;
    property Enabled;
    property Font;
    property InsertMode;
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

const
  ColCount = 20;
  ColorValues: array [1..ColCount] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clMoneyGreen, clSkyBlue, clCream, clMedGray);


  SysColCount = 58;
  SysColorValues: array [1..SysColCount] of TColor = (

    clForeground, clButton, clLight, clMidlight, clDark, clMid,
    clText, clBrightText, clButtonText, clBase, clBackground,
    clShadow, clHighlight, clHighlightedText,

    clInfoText, clInfoBk,

    clNormalForeground, clNormalButton, clNormalLight, clNormalMidlight, clNormalDark, clNormalMid,
    clNormalText, clNormalBrightText, clNormalButtonText, clNormalBase, clNormalBackground,
    clNormalShadow, clNormalHighlight, clNormalHighlightedText,

    clDisabledForeground, clDisabledButton, clDisabledLight, clDisabledMidlight, clDisabledDark, clDisabledMid,
    clDisabledText, clDisabledBrightText, clDisabledButtonText, clDisabledBase, clDisabledBackground,
    clDisabledShadow, clDisabledHighlight, clDisabledHighlightedText,

    clActiveForeground, clActiveButton, clActiveLight, clActiveMidlight, clActiveDark, clActiveMid,
    clActiveText, clActiveBrightText, clActiveButtonText, clActiveBase, clActiveBackground,
    clActiveShadow, clActiveHighlight, clActiveHighlightedText);


function LoadInternalBitmap(ResName: string): TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromResourceName(hInstance, ResName);
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
  Result := TM.tmHeight + 2;
end;

function Max(Val1, Val2: Integer): Integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
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
  FHexPrefix := '$';
  FOther := SOtherCaption;
  FOptions := [coText];
  Duplicates := dupAccept;
  FHiLiteColor := clHighLight;
  FHiLiteText := clHighLightText;
  AutoComplete := False;
  // make sure that if this is the first time the component is dropped on the form,
  // the default Name/Value map is created (thanks to Brian Cook on the borland NG's):
  if (Owner <> nil) and ([csDesigning, csLoading] * Owner.ComponentState = [csDesigning]) then
    InitColorNames;
  TStringlist(FColorNameMap).OnChange := DoNameMapChange;
  OnDrawItem := ItemDraw;
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
  end
  else
    AddColor(Value, Format(FPrefix, [FCustCnt]));
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


procedure TJvColorComboBox.ItemDraw(Sender: TObject; Index: Integer; R: TRect;    // OnDrawItem Handler
  State: TOwnerDrawState; var Handled: Boolean );
var
  aRect: TRect;
  aColor: TColor;
  S: string;
begin
  if R.Bottom < 0 then      // CLX bug fix
  begin
                                // when using up/down arrow keys
    R.Right := ClientWidth ;   // then R.Bottom and R.Right have value -2 !
                               // for index > DropDownCount
  end;
  R.Bottom := R.Top + ItemHeight;
  if (Index >= 0) and (odSelected in State) and DroppedDown then
  begin
    Canvas.Brush.Color := FHiLiteColor;
    Canvas.Font.Color := FHiLiteText;
  end
  else if not Focused then
    Canvas.Font.Color := Font.Color;
  if Index < 0 then
  begin
    Canvas.FillRect(R);
    exit;  // not Handled
  end;
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
      DrawText(Canvas, S, -1, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    end
    else
    if ((coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions)) then
    begin
      S := Items[Index];
      DoGetDisplayName(Index, TColor(Items.Objects[Index]), S);
      if S <> FOther then
      begin
        if coHex in FOptions then
          S := Format(HexPrefix + '%.6x', [ColorToRGB(TColor(Items.Objects[Index]))])
        else
        if coRGB in Foptions then
          S := Format('(%d,%d,%d)', [GetRValue(TColor(Items.Objects[Index])), GetGValue(TColor(Items.Objects[Index])),
            GetBValue(TColor(Items.Objects[Index]))]);
      end;
      R.Left := R.Left + FColWidth + 6;
      R.Right := R.Left + TextWidth(S) + 6;
      FillRect(R);
      OffsetRect(R, 2, 0);
      DrawText(Canvas, S, -1, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      OffsetRect(R, -2, 0);
    end
    else
      FrameRect(Canvas, R);
    if odSelected in State then
      DrawFocusRect(R);
  end;
  Handled := true ;
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

procedure TJvColorComboBox.FontChanged;
begin
//  inherited;
  if not (csRecreating in ControlState) then
    ResetItemHeight;
  Invalidate;
//  RecreateWnd;
end;

procedure TJvColorComboBox.ResetItemHeight;
(*
  function GetItemHeight: Integer;
  var
    FM: QFontMetricsH;
  begin
    FM := QFontMetrics_create(Font.Handle);
    try
      QWidget_FontMetrics(Handle, FM);
      Result := QFontMetrics_height(FM) + 3;
    finally
      QFontMetrics_destroy(FM);
    end;
  end;
*)
begin
  ItemHeight := GetItemHeight(Font);
end;


procedure TJvColorComboBox.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TJvColorComboBox.SetHexPrefix(const Value: string);
begin
  if Value <> FHexPrefix then
  begin
    FPrefix := Value;
    Invalidate;
  end;
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
  case AColor of
  clMoneyGreen : Result := 'MoneyGreen';
  clSkyBlue: Result := 'SkyBlue';
  clCream: Result := 'Cream';
  clMedGray: result := 'MedGray';
  end;
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
    FColorNameMap.Add('clForeground=Foreground');
    FColorNameMap.Add('clButton=Button');
    FColorNameMap.Add('clLight=Light');
    FColorNameMap.Add('clMidlight=Mid Light');
    FColorNameMap.Add('clDark=Drak');
    FColorNameMap.Add('clMid=Mid');
    FColorNameMap.Add('clText=Text');
    FColorNameMap.Add('clBrightText=Bright Text');
    FColorNameMap.Add('clButtonText=Button Text');
    FColorNameMap.Add('clBase=Base');
    FColorNameMap.Add('clBackground=BackGround');
    FColorNameMap.Add('clShadow=Shadow');
    FColorNameMap.Add('clHighlight=Highlight');
    FColorNameMap.Add('clHighlightedText=Highlighted Text');

    FColorNameMap.Add('clNormalForeground=Normal Foreground');
    FColorNameMap.Add('clNormalButton=Normal Button');
    FColorNameMap.Add('clNormalLight=Normal Light');
    FColorNameMap.Add('clNormalMidlight=Normal Mid Light');
    FColorNameMap.Add('clNormalDark=Normal Drak');
    FColorNameMap.Add('clNormalMid=Normal Mid');
    FColorNameMap.Add('clNormalText=Normal Text');
    FColorNameMap.Add('clNormalBrightText=Normal Bright Text');
    FColorNameMap.Add('clNormalButtonText=Normal Button Text');
    FColorNameMap.Add('clNormalBase=Normal Base');
    FColorNameMap.Add('clNormalBackground=Normal BackGround');
    FColorNameMap.Add('clNormalShadow=Normal Shadow');
    FColorNameMap.Add('clNormalHighlight=Normal Highlight');
    FColorNameMap.Add('clNormalHighlightedText=Normal Highlighted Text');

    FColorNameMap.Add('clDisabledForeground=Disabled Foreground');
    FColorNameMap.Add('clDisabledButton=Disabled Button');
    FColorNameMap.Add('clDisabledLight=Disabled Light');
    FColorNameMap.Add('clDisabledMidlight=Disabled Mid Light');
    FColorNameMap.Add('clDisabledDark=Disabled Drak');
    FColorNameMap.Add('clDisabledMid=Disabled Mid');
    FColorNameMap.Add('clDisabledText=Disabled Text');
    FColorNameMap.Add('clDisabledBrightText=Disabled Bright Text');
    FColorNameMap.Add('clDisabledButtonText=Disabled Button Text');
    FColorNameMap.Add('clDisabledBase=Disabled Base');
    FColorNameMap.Add('clDisabledBackground=Disabled BackGround');
    FColorNameMap.Add('clDisabledShadow=Disabled Shadow');
    FColorNameMap.Add('clDisabledHighlight=Disabled Highlight');
    FColorNameMap.Add('clDisabledHighlightedText=Disabled Highlighted Text');

    FColorNameMap.Add('clActiveForeground=Active Foreground');
    FColorNameMap.Add('clActiveButton=Active Button');
    FColorNameMap.Add('clActiveLight=Active Light');
    FColorNameMap.Add('clActiveMidlight=Active Mid Light');
    FColorNameMap.Add('clActiveDark=Active Drak');
    FColorNameMap.Add('clActiveMid=Active Mid');
    FColorNameMap.Add('clActiveText=Active Text');
    FColorNameMap.Add('clActiveBrightText=Active Bright Text');
    FColorNameMap.Add('clActiveButtonText=Active Button Text');
    FColorNameMap.Add('clActiveBase=Active Base');
    FColorNameMap.Add('clActiveBackground=Active BackGround');
    FColorNameMap.Add('clActiveShadow=Active Shadow');
    FColorNameMap.Add('clActiveHighlight=Active Highlight');
    FColorNameMap.Add('clActiveHighlightedText=Active Highlighted Text');
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
//    AutoSave.LoadValue(Integer(FColorValue));
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
//    AutoSave.SaveValue(ColorValue);
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


end.

