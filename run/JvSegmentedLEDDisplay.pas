{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSegmentedLEDDisplay.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jay Dubal

Last Modified: 2003-07-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  * Automatic unlit color calculation is not working properly. Maybe a function in JclGraphUtil
    can help out there.
  * Exceptions should be JVCL based
  * String literals need to be converted into resourcestrings
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSegmentedLEDDisplay;

interface

uses
  Classes, Graphics, Windows,
  JclBase,
  JvComponent;

// Additional color values for unlit color settings (TUnlitColor type)
const
  clDefaultBackground = TColor($20100001);
  clDefaultLitColor = TColor($20100002);

type
  TJvCustomSegmentedLEDDisplay = class;
  TJvSegmentedLEDDigits = class;
  TJvCustomSegmentedLEDDigit = class;
  TJvBaseSegmentedLEDCharacterMapper = class;

  TJvSegmentedLEDDigitClass = class of TJvCustomSegmentedLEDDigit;
  TJvSegmentedLEDCharacterMapperClass = class of TJvBaseSegmentedLEDCharacterMapper;

  TJvSegmentedLEDDigitClassName = type string;
  TUnlitColor = type TColor;
  TSlantAngle = 0 .. 44;

  TJvCustomSegmentedLEDDisplay = class(TJvGraphicControl)
  private
    FCharacterMapper: TJvBaseSegmentedLEDCharacterMapper;
    FDigitClass: TJvSegmentedLEDDigitClass;
    FDigits: TJvSegmentedLEDDigits;
    FFullRefreshNeeded: Boolean;
    FDigitHeight: Integer;
    FDigitSpacing: Integer;
    FDigitWidth: Integer;
    FMaxBaseTop: Integer;
    FSegmentLitColor: TColor;
    FSegmentSpacing: Integer;
    FSegmentThickness: Integer;
    FSegmentUnlitColor: TUnlitColor;
    FSlant: TSlantAngle;
    FText: string;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    function GetText: string;
    procedure SetText(Value: string);
    procedure SetCharacterMapper(Value: TJvBaseSegmentedLEDCharacterMapper);
    procedure SetDigitHeight(Value: Integer);
    procedure SetDigits(Value: TJvSegmentedLEDDigits);
    procedure SetDigitSpacing(Value: Integer);
    procedure SetDigitWidth(Value: Integer);
    procedure SetDigitClass(Value: TJvSegmentedLEDDigitClass);
    procedure SetSegmentLitColor(Value: TColor);
    procedure SetSegmentSpacing(Value: Integer);
    procedure SetSegmentThickness(Value: Integer);
    procedure SetSegmentUnlitColor(Value: TUnlitColor);
    procedure SetSlant(Value: TSlantAngle);
    function GetDigitClassName: TJvSegmentedLEDDigitClassName;
    procedure SetDigitClassName(Value: TJvSegmentedLEDDigitClassName);
    function GetRealUnlitColor: TColor;
    function CalcRealUnlitColorBackground: TColor;
    function CalcRealUnlitColorLitColor: TColor;
    procedure PrimSetText(Value: string);
    procedure BaseTopChanged;
    procedure HeightChanged;
    procedure UpdateDigitsPositions;
    procedure InvalidateDigits;
    procedure InvalidateView;
    procedure UpdateText;
    procedure UpdateBounds;

    property CharacterMapper: TJvBaseSegmentedLEDCharacterMapper read FCharacterMapper
      write SetCharacterMapper;
    property DigitClass: TJvSegmentedLEDDigitClass read FDigitClass write SetDigitClass;
    // Solely needed for design time support of DigitClass
    property DigitClassName: TJvSegmentedLEDDigitClassName read GetDigitClassName write SetDigitClassName;
    { Height of the digit. This is the height needed for the main segments. Actual height may be
      greater due to margin and/or special markers above/below the digit. }
    property DigitHeight: Integer read FDigitHeight write SetDigitHeight default 30;
    { Access to the digit collection. }
    property Digits: TJvSegmentedLEDDigits read FDigits write SetDigits;
    { Distance between two digits. }
    property DigitSpacing: Integer read FDigitSpacing write SetDigitSpacing default 2;
    { Width of the digit. This is the width needed for the main segments. Actual width may be
      greater due to margin and/or special markers to the left/right of the digit. }
    property DigitWidth: Integer read FDigitWidth write SetDigitWidth default 20;
    { Determines if a full repaint/refresh of the control is needed. }
    property FullRefreshNeeded: Boolean read FFullRefreshNeeded;
    { Specifies the color to use for a segment that is lit. }
    property SegmentLitColor: TColor read FSegmentLitColor write SetSegmentLitColor default clWindowText;
    { Specifies the spacing between adjacent segments. }
    property SegmentSpacing: Integer read FSegmentSpacing write SetSegmentSpacing default 2;
    { Specifies the thickness of each segment. Rounded down to an even value. }
    property SegmentThickness: Integer read FSegmentThickness write SetSegmentThickness default 2;
    { Specifies the color to use for a segment that is unlit. Specify clNone to use the background
      color of the control (i.e. no unlit segments are rendered), clDefaultLitColor to
      automatically determine the unlit color (i.e. 10% of the lit color w/respect to the background
      color of the control) or clDefaultBackground to automatically determine the color based on
      the background. }
    property SegmentUnlitColor: TUnlitColor read FSegmentUnlitColor write SetSegmentUnlitColor default clDefaultLitColor;
    { Slanting angle of the digits. 0 is upright, 0..44 is angle in degrees the the top is shifted
      to the right. Note that depending on the size of the digit and/or the thickness of the
      segments not all values may result in a nice looking digit. }
    property Slant: TSlantAngle read FSlant write SetSlant default 0;
    property Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TJvSegmentedLEDDisplay = class(TJvCustomSegmentedLEDDisplay)
  public
    property DigitClass;
  published
    property Color;
    property CharacterMapper;
    property DigitClassName;
    property DigitHeight;
    property Digits;
    property DigitSpacing;
    property DigitWidth;
    property SegmentLitColor;
    property SegmentSpacing;
    property SegmentThickness;
    property SegmentUnlitColor;
    property Slant;
    property Text;
  end;

  TJvSegmentedLEDDigits = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TJvCustomSegmentedLEDDigit;
    procedure SetItem(Index: Integer; Value: TJvCustomSegmentedLEDDigit);
    function Display: TJvCustomSegmentedLEDDisplay;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TJvCustomSegmentedLEDDigit read GetItem write SetItem; default;
  end;

  TSegmentRenderType = (srtNone, srtPolygon, srtRect, srtCircle);
  TPointArray = array of TPoint;
  TSegmentRenderInfo = record
    RenderType: TSegmentRenderType;
    Points: TPointArray;
  end;
  TSegmentRenderInfoArray = array of TSegmentRenderInfo;

  TJvCustomSegmentedLEDDigit = class(TCollectionItem)
  private
    FLeft: Integer;
    FRecalcNeeded: Boolean;
    FVertAdjust: Integer;
    FSegmentStates: Int64;
    FSegmentRenderInfo: TSegmentRenderInfoArray;
    FText: string;
  protected
    // Quick access to Display specified values (slant angle, digit spacing, etc)
    SegmentWidth: Integer;
    SlantAngle: Integer;
    Spacing: Integer;
    MaxSlantDif: Integer; // Max slant difference. Protected field to make access to it easier.
    function GetBaseTop: Integer; virtual;
    procedure SetBaseTop(Value: Integer); virtual;
    function GetHeight: Integer; virtual;
    function GetVertAdjust: Integer;
    procedure SetVertAdjust(Value: Integer);
    procedure SetIndex(Value: Integer); override;
    function GetLeft: Integer;
    procedure SetLeft(Value: Integer);
    function GetWidth: Integer; virtual;
    procedure SetText(Value: string); virtual;

    procedure SetSegmentRenderInfo(Index: Integer; RenderType: TSegmentRenderType; Points: array of TPoint);
    function GetSegmentState(Index: Integer): Boolean;
    procedure SetSegmentState(Index: Integer; Value: Boolean);
    procedure SetSegmentStates(Value: Int64);
    procedure UpdateText(Value: string);
    class function MapperClass: TJvSegmentedLEDCharacterMapperClass; virtual; abstract;
    procedure RecalcRefPoints; virtual; abstract;
    procedure RecalcSegments; virtual; abstract;
    class function SegmentCount: Integer; virtual; abstract;
    class function GetSegmentName(Index: Integer): string; virtual; abstract;
    class function GetSegmentIndex(Name: string): Integer; virtual; abstract;
    function GetLitSegColor(Index: Integer): TColor; virtual;
    function GetUnlitSegColor(Index: Integer): TColor; virtual;
    function GetSegmentColor(Index: Integer): TColor;
    function Display: TJvCustomSegmentedLEDDisplay;
    procedure Invalidate;
    procedure InvalidateStates;
    procedure InvalidateRefPoints; virtual;
    function NeedsPainting: Boolean;
    procedure Paint;
    procedure PaintSegment(Index: Integer);

    property BaseTop: Integer read GetBaseTop;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property VertAdjust: Integer read GetVertAdjust;
    property Width: Integer read GetWidth;
    property Text: string read FText write SetText stored False;

    { Needs to recalculate all reference points and refresh the entire view }
    property RecalcNeeded: Boolean read FRecalcNeeded;
  public
    constructor Create(Collection: TCollection); override;
  end;

  TJvBaseSegmentedLEDDigit = class(TJvCustomSegmentedLEDDigit)
  private
    FDPWidth: Integer;
    FUseDP: Boolean;
  protected
    // Reference points coordinates. Protected fields allows easier read/write access in descendants.
    FRefLeft: Integer;
    FRefCenterX: Integer;
    FRefRight: Integer;
    FRefTop: Integer;
    FRefCenterY: Integer;
    FRefBottom: Integer;
    procedure SetUseDP(Value: Boolean); virtual;
    function GetDPWidth: Integer;
    procedure SetDPWidth(Value: Integer);
    procedure UpdateDPWidth; virtual;
    procedure CalcASeg(Index: Integer); virtual;
    procedure CalcBSeg(Index: Integer); virtual;
    procedure CalcCSeg(Index: Integer); virtual;
    procedure CalcDSeg(Index: Integer); virtual;
    procedure CalcESeg(Index: Integer); virtual;
    procedure CalcFSeg(Index: Integer); virtual;
    procedure CalcGSeg(Index: Integer); virtual;
    procedure CalcDPSeg(Index: Integer); virtual;
    function GetWidth: Integer; override;
    procedure InvalidateRefPoints; override;
    procedure RecalcRefPoints; override;
    procedure RecalcSegments; override;
    class function SegmentCount: Integer; override;
    class function GetSegmentName(Index: Integer): string; override;
    class function GetSegmentIndex(Name: string): Integer; override;

    property DPWidth: Integer read GetDPWidth write SetDPWidth;
    property UseDP: Boolean read FUseDP write SetUseDP;
  end;

  TJvBaseSegmentedLEDCharacterMapper = class(TJvComponent)
  private
    FCurDigit: TJvCustomSegmentedLEDDigit;
    FTextForDigit: string;
    FSegMapRemoves: Boolean;
  protected
    function UpdateStates(var Segments: Int64; SegMask: Int64): Boolean;
    procedure HandleDecimalSeparator(var Text: PChar; var Segments: Int64); virtual;
    function CharToSegments(Ch: Char; var Segments: Int64): Boolean; virtual; abstract;
    procedure ControlItemToSegments(var ControlItem: PChar; var Segments: Int64); virtual;
    procedure MapControlItems(var Text: PChar; var Segments: Int64); virtual;
    procedure MapSimpleText(var Text: PChar; var Segments: Int64); virtual;
    procedure MapSegNamesToSegments(var Text: Pchar; var Segments: Int64); virtual;
    procedure PrimMapText(var Text: PChar; var Segments: Int64); virtual;

    property CurDigit: TJvCustomSegmentedLEDDigit read FCurDigit;
    property SegMapRemoves: Boolean read FSegMapRemoves write FSegMapRemoves;
    property TextForDigit: string read FTextForDigit write FTextForDigit;
  public
    procedure MapText(var Text: PChar; ADigit: TJvCustomSegmentedLEDDigit);
  end;

  // 7-segmented digit
  T7SegColonUsage = (scuNone { No Colon }, scuLowOnly { Low part only }, scuFull {Entire colon}, scuColonOnly);
  TJv7SegmentedLEDDigit = class(TJvBaseSegmentedLEDDigit)
  private
    FUseColon: T7SegColonUsage;
  protected
    function GetUseColon: T7SegColonUsage;
    procedure SetUseColon(Value: T7SegColonUsage);
    class function MapperClass: TJvSegmentedLEDCharacterMapperClass; override;
    class function SegmentCount: Integer; override;
    class function GetSegmentName(Index: Integer): string; override;
    class function GetSegmentIndex(Name: string): Integer; override;
    procedure RecalcSegments; override;
    procedure CalcCHSeg(Index: Integer); virtual;
    procedure CalcCLSeg(Index: Integer); virtual;
  public
  published
    property UseDP;
    property UseColon: T7SegColonUsage read GetUseColon write SetUseColon;
    property Text;
  end;

  // 7-segmented character mapper
  TJv7SegmentedLEDCharacterMapper = class(TJvBaseSegmentedLEDCharacterMapper)
  private
    FDefaultMapping: array[Char] of Word;
  protected
    function CharToSegments(Ch: Char; var Segments: Int64): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function IdentToUnlitColor(const Ident: string; var Int: Longint): Boolean;
function UnlitColorToIdent(Int: Longint; var Ident: string): Boolean;
{ Design time support to retrieve a list of digit classes available. Do not change the list in your
  own code! }
function DigitClassList: TThreadList;
procedure RegisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
procedure RegisterSegmentedLEDDigitClasses(DigitClasses: array of TJvSegmentedLEDDigitClass);
procedure UnregisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
procedure UnregisterSegmentedLEDDigitClasses(DigitClasses: array of TJvSegmentedLEDDigitClass);
procedure UnregisterModuleSegmentedLEDDigitClasses(Module: HMODULE);

implementation

uses
  SysUtils,
  JclGraphUtils;

var
  GDigitClassList: TThreadList;

//===DigitClass registration routines===============================================================

function DigitClassList: TThreadList;
begin
  Result := GDigitClassList;
end;

procedure RegisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
begin
  with DigitClassList.LockList do
  try
    if IndexOf(DigitClass) > -1 then
      raise Exception.Create('Duplicate DigitClass registered.');
    Add(DigitClass);
    Classes.RegisterClass(DigitClass);
  finally
    DigitClassList.UnlockList;
  end;
end;

procedure RegisterSegmentedLEDDigitClasses(DigitClasses: array of TJvSegmentedLEDDigitClass);
var
  I: Integer;
begin
  for I := Low(DigitClasses) to High(DigitClasses) do
    RegisterSegmentedLEDDigitClass(DigitClasses[I]);
end;

procedure UnregisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
begin
  DigitClassList.Remove(DigitClass);
end;

procedure UnregisterSegmentedLEDDigitClasses(DigitClasses: array of TJvSegmentedLEDDigitClass);
var
  I: Integer;
begin
  for I := Low(DigitClasses) to High(DigitClasses) do
    UnregisterSegmentedLEDDigitClass(DigitClasses[I]);
end;

procedure UnregisterModuleSegmentedLEDDigitClasses(Module: HMODULE);
var
  I: Integer;
  M: TMemoryBasicInformation;
begin
  with DigitClassList.LockList do
  try
    for I := Count - 1 downto 0 do
    begin
      VirtualQuery(Items[I], M, SizeOf(M));
      if (Module = 0) or (HMODULE(M.AllocationBase) = Module) then
        Delete(I);
    end;
  finally
    DigitClassList.UnlockList;
  end;
end;

//===Helper routine:AngleAdjustPoint================================================================

function AngleAdjustPoint(X, Y, Angle: Integer): TPoint;
begin
  Result.X := X - Trunc(ArcTan(Angle * Pi / 180.0) * Y);
  Result.Y := Y;
end;

//===TJvCustomSegmentedLEDDisplay===================================================================

procedure TJvCustomSegmentedLEDDisplay.Loaded; 
begin
  inherited Loaded;
  PrimSetText(Text);
end;

procedure TJvCustomSegmentedLEDDisplay.Paint;
var
  I: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psSolid;
  Canvas.FillRect(ClientRect);
  for I := 0 to FDigits.Count - 1 do
    Digits[I].Paint;
end;

function TJvCustomSegmentedLEDDisplay.GetText: string;
begin
  Result := FText;
end;

procedure TJvCustomSegmentedLEDDisplay.SetText(Value: string);
begin
  if Value <> Text then
    PrimSetText(Value);
end;

procedure TJvCustomSegmentedLEDDisplay.SetCharacterMapper(Value: TJvBaseSegmentedLEDCharacterMapper);
begin
  if CharacterMapper <> Value then
  begin
    if (csLoading in ComponentState) or (
      (Value = nil) or ((DigitClass <> nil) and Value.InheritsFrom(DigitClass.MapperClass))) then
    begin
      FCharacterMapper := Value;
      if not (csLoading in ComponentState) and (Value <> nil) then
        PrimSetText(Text);
    end
    else
    if DigitClass = nil then
      raise Exception.Create('Cannot specify mapper without DigitClass being set.')
    else
      raise Exception.Create('Invalid mapper class for current digit class.');
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetDigitHeight(Value: Integer);
var
  MaxHeight: Integer;
  I: Integer;
begin
  if Value <> DigitHeight then
  begin
    FDigitHeight := Value;
    MaxHeight := 0;
    for I := 0 to Digits.Count -1 do
    begin
      Digits[I].InvalidateRefPoints;
      if Digits[I].Height + Digits[I].GetVertAdjust > MaxHeight then
        MaxHeight := Digits[I].Height + Digits[I].GetVertAdjust;
    end;
    if MaxHeight = 0 then
      MaxHeight := 13;
    // Adjust control height
    if ClientHeight <> MaxHeight then
      ClientHeight := MaxHeight;
    InvalidateView;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetDigits(Value: TJvSegmentedLEDDigits);
begin
end;

procedure TJvCustomSegmentedLEDDisplay.SetDigitSpacing(Value: Integer);
begin
  if Value <> DigitSpacing then
  begin
    FDigitSpacing := Value;
    UpdateDigitsPositions;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetDigitWidth(Value: Integer);
begin
  if Value <> DigitWidth then
  begin
    FDigitWidth := Value;
    if Digits.Count > 0 then
    begin
      UpdateDigitsPositions;
      Digits[0].InvalidateRefPoints;
    end;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetDigitClass(Value: TJvSegmentedLEDDigitClass);
var
  I: Integer;
begin
  if (DigitClass <> Value) and (Value <> nil) then
  begin
    FDigitClass := Value;
    I := Digits.Count;
    FreeAndNil(FDigits);
    FDigits := TJvSegmentedLEDDigits.Create(Self);
    while (I > 0) do
    begin
      Digits.Add;
      Dec(I);
    end;
    if not (csLoading in ComponentState) and (CharacterMapper <> nil) and not CharacterMapper.InheritsFrom(DigitClass.MapperClass) then
      FCharacterMapper := nil
    else if not (csLoading in ComponentState) then
      PrimSetText(Text);
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetSegmentLitColor(Value: TColor);
begin
  if Value <> SegmentLitColor then
  begin
    FSegmentLitColor := Value;
    InvalidateView;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetSegmentSpacing(Value: Integer);
begin
  if Value <> SegmentSpacing then
  begin
    FSegmentSpacing := Value;
    InvalidateDigits;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetSegmentThickness(Value: Integer);
begin
  if Value <> SegmentThickness then
  begin
    FSegmentThickness := Value;
    InvalidateDigits;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetSegmentUnlitColor(Value: TUnlitColor);
begin
  if Value <> SegmentUnlitColor then
  begin
    FSegmentUnlitColor := Value;
    InvalidateView;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetSlant(Value: TSlantAngle);
begin
  if Value <> Slant then
  begin
    FSlant := Value;
    InvalidateDigits;
    UpdateDigitsPositions;
  end;
end;

function TJvCustomSegmentedLEDDisplay.GetDigitClassName: TJvSegmentedLEDDigitClassName;
begin
  if DigitClass <> nil then
    Result := DigitClass.ClassName
  else
    Result := '';
end;

procedure TJvCustomSegmentedLEDDisplay.SetDigitClassName(Value: TJvSegmentedLEDDigitClassName);
var
  AClass: TClass;
begin
  if not AnsiSameStr(Value, DigitClassName) then
  begin
    if Value <> '' then
    begin
      AClass := FindClass(Value);
      if AClass.InheritsFrom(TJvCustomSegmentedLEDDigit) then
        DigitClass := TJvSegmentedLEDDigitClass(FindClass(Value))
      else
        raise Exception.Create('Invalid class.');
    end
    else
      DigitClass := nil;
  end;
end;

function TJvCustomSegmentedLEDDisplay.GetRealUnlitColor: TColor;
begin
  if SegmentUnlitColor = clNone then
    Result := Color
  else if SegmentUnlitColor = clDefaultBackground then
    Result := CalcRealUnlitColorBackground
  else if SegmentUnlitColor = clDefaultLitColor then
    Result := CalcRealUnlitColorLitColor
  else
    Result := SegmentUnlitColor;
end;

function TJvCustomSegmentedLEDDisplay.CalcRealUnlitColorBackground: TColor;
var
  Int: Integer;
begin
  Int := Intensity(Color32(Color));
  if Int > 127 then
    { Light color; darken a little }
    Result := DarkColor(Color, 30)
  else
    { Dark color; lighten a little }
    Result := BrightColor(Color, 30);
end;

function TJvCustomSegmentedLEDDisplay.CalcRealUnlitColorLitColor: TColor;
begin
  if Intensity(Color32(SegmentLitColor)) > Intensity(Color32(Color)) then
    Result := DarkColor(SegmentLitColor, 70)
  else
    Result := BrightColor(SegmentLitColor, 70);
end;

procedure TJvCustomSegmentedLEDDisplay.PrimSetText(Value: string);
var
  P: PChar;
  I: Integer;
begin
  { Apply mapping of text. If any digit is changed Invalidate will be called. The stored value for
    FText will be the concatenation of each Digit's Text value. }
  if CharacterMapper <> nil then
  begin
    P := PChar(Value);
    for I := 0 to Digits.Count -1 do
      CharacterMapper.MapText(P, Digits[I]);
    UpdateText;
  end
  else
    FText := Value;
end;

procedure TJvCustomSegmentedLEDDisplay.BaseTopChanged;
var
  I: Integer;
  MaxHeight: Integer;
begin
  // Determine MaxBaseTop
  FMaxBaseTop := 0;
  for I := 0 to Digits.Count - 1 do
    if Digits[I].GetBaseTop > FMaxBaseTop then
      FMaxBaseTop := Digits[I].GetBaseTop;
  // Vertically adjust digits and determine maximum height
  MaxHeight := 0;
  for I := 0 to Digits.Count - 1 do
  begin
    Digits[I].SetVertAdjust(FMaxBaseTop - Digits[I].GetBaseTop);
    if Digits[I].Height + Digits[I].GetVertAdjust > MaxHeight then
      MaxHeight := Digits[I].Height + Digits[I].GetVertAdjust;
  end;
  if MaxHeight = 0 then
    MaxHeight := 13;
  // Adjust control height
  if ClientHeight <> MaxHeight then
  begin
    InvalidateView;
    ClientHeight := MaxHeight;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.HeightChanged;
var
  MaxHeight: Integer;
  I: Integer;
begin
  MaxHeight := 0;
  for I := 0 to Digits.Count - 1 do
    if Digits[I].Height + Digits[I].GetVertAdjust > MaxHeight then
      MaxHeight := Digits[I].Height + Digits[I].GetVertAdjust;
  if MaxHeight = 0 then
    MaxHeight := 13;
  // Adjust control height
  if ClientHeight <> MaxHeight then
  begin
    InvalidateView;
    ClientHeight := MaxHeight;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.UpdateDigitsPositions;
var
  I: Integer;
  X: Integer;
begin
  if Digits.Count > 0 then
  begin
    Digits[0].SetLeft(0);
    X := Digits[0].Width + DigitSpacing;
    for I := 1 to Digits.Count - 1 do
    begin
      Digits[I].SetLeft(X);
      Inc(X, Digits[I].Width + DigitSpacing);
    end;
    Dec(X, DigitSpacing);
    if ClientWidth <> X then
      ClientWidth := X;
    InvalidateView;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.InvalidateDigits;
var
  I: Integer;
begin
  for I := 0 to Digits.Count - 1 do
    Digits[I].InvalidateRefPoints;
end;

procedure TJvCustomSegmentedLEDDisplay.InvalidateView;
begin
  FFullRefreshNeeded := True;
  Invalidate;
end;

procedure TJvCustomSegmentedLEDDisplay.UpdateText;
var
  I: Integer;
begin
  FText := '';
  for I := 0 to Digits.Count - 1 do
    FText := FText + Digits[I].Text;
end;

procedure TJvCustomSegmentedLEDDisplay.UpdateBounds;
begin
  HeightChanged;
  UpdateDigitsPositions;
end;

constructor TJvCustomSegmentedLEDDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDigitClass := TJv7SegmentedLEDDigit;
  FDigits := TJvSegmentedLEDDigits.Create(Self);
  FDigitHeight := 30;
  FDigitSpacing := 2;
  FDigitWidth := 20;
  FSegmentLitColor := clWindowText;
  FSegmentSpacing := 2;
  FSegmentThickness := 2;
  FSegmentUnlitColor := clDefaultLitColor;
  ClientWidth := 20;
  ClientHeight := 30;
end;

destructor TJvCustomSegmentedLEDDisplay.Destroy;
begin
  FreeAndNil(FDigits);
  inherited Destroy;
end;

//===TJvSegmentedLEDDigits==========================================================================

function TJvSegmentedLEDDigits.GetItem(Index: Integer): TJvCustomSegmentedLEDDigit;
begin
  Result := TJvCustomSegmentedLEDDigit(inherited Items[Index]);
end;

procedure TJvSegmentedLEDDigits.SetItem(Index: Integer; Value: TJvCustomSegmentedLEDDigit);
begin
  inherited Items[Index] := Value;
end;

function TJvSegmentedLEDDigits.Display: TJvCustomSegmentedLEDDisplay;
begin
  Result := TJvCustomSegmentedLEDDisplay(GetOwner);
end;

procedure TJvSegmentedLEDDigits.Update(Item: TCollectionItem);
begin
  Assert(Display <> nil);
  Display.UpdateBounds;
end;

constructor TJvSegmentedLEDDigits.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvCustomSegmentedLEDDisplay(AOwner).DigitClass);
end;

//===TJvCustomSegmentedLEDDigit=====================================================================

function TJvCustomSegmentedLEDDigit.GetBaseTop: Integer;
begin
  Result := 0;
end;

procedure TJvCustomSegmentedLEDDigit.SetBaseTop(Value: Integer);
begin
end;

function TJvCustomSegmentedLEDDigit.GetHeight: Integer;
begin
  Result := Display.DigitHeight;
end;

function TJvCustomSegmentedLEDDigit.GetVertAdjust: Integer;
begin
  Result := FVertAdjust;
end;

procedure TJvCustomSegmentedLEDDigit.SetVertAdjust(Value: Integer);
begin
  if Value <> GetVertAdjust then
  begin
    FVertAdjust := Value;
    InvalidateRefPoints;
  end;
end;

procedure TJvCustomSegmentedLEDDigit.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  Display.UpdateDigitsPositions;
end;

function TJvCustomSegmentedLEDDigit.GetLeft: Integer;
begin
  Result := FLeft;
end;

procedure TJvCustomSegmentedLEDDigit.SetLeft(Value: Integer);
begin
  if Value <> Left then
  begin
    FLeft := Value;
    InvalidateRefPoints;
  end;
end;

function TJvCustomSegmentedLEDDigit.GetWidth: Integer;
begin
  Result := Display.DigitWidth + MaxSlantDif;
end;

procedure TJvCustomSegmentedLEDDigit.SetText(Value: string);
var
  P: PChar;
begin
  if Value <> Text then
  begin
    if Display.CharacterMapper <> nil then
    begin
      P := PChar(Value);
      Display.CharacterMapper.MapText(P, Self);
    end
    else
      UpdateText(Value);
    Display.UpdateText;
  end;
end;

procedure TJvCustomSegmentedLEDDigit.SetSegmentRenderInfo(Index: Integer;
  RenderType: TSegmentRenderType; Points: array of TPoint);
begin
  FSegmentRenderInfo[Index].RenderType := RenderType;
  SetLength(FSegmentRenderInfo[Index].Points, Length(Points));
  if Length(Points) > 0 then
    Move(Points[0], FSegmentRenderInfo[Index].Points[0], Length(Points) * SizeOf(Points[0]));
end;

function TJvCustomSegmentedLEDDigit.GetSegmentState(Index: Integer): Boolean;
begin
  Result := (FSegmentStates and (1 shl Index)) <> 0;
end;

procedure TJvCustomSegmentedLEDDigit.SetSegmentState(Index: Integer; Value: Boolean);
begin
  if Value <> GetSegmentState(Index) then
  begin
    FSegmentStates := FSegmentStates xor (1 shl Index);
    InvalidateStates;
  end;
end;

procedure TJvCustomSegmentedLEDDigit.SetSegmentStates(Value: Int64);
begin
  if Value <> FSegmentStates then
  begin
    FSegmentStates := Value;
    InvalidateStates;
  end;
end;

procedure TJvCustomSegmentedLEDDigit.UpdateText(Value: string);
begin
  if Value <> Text then
  begin
    FText := Value;
    Display.UpdateText;
  end;
end;

function TJvCustomSegmentedLEDDigit.GetLitSegColor(Index: Integer): TColor;
begin
  Result := Display.SegmentLitColor;
end;

function TJvCustomSegmentedLEDDigit.GetUnlitSegColor(Index: Integer): TColor;
begin
  Result := Display.GetRealUnlitColor;
end;

function TJvCustomSegmentedLEDDigit.GetSegmentColor(Index: Integer): TColor;
begin
  if GetSegmentState(Index) then
    Result := GetLitSegColor(Index)
  else
    Result := GetUnlitSegColor(Index);
end;

function TJvCustomSegmentedLEDDigit.Display: TJvCustomSegmentedLEDDisplay;
begin
  Assert(Collection <> nil);
  Result := TJvSegmentedLEDDigits(Collection).Display;
  Assert(Result <> nil);
end;

procedure TJvCustomSegmentedLEDDigit.Invalidate;
begin
  Display.Invalidate;
end;

procedure TJvCustomSegmentedLEDDigit.InvalidateStates;
begin
  Display.Invalidate;
end;

procedure TJvCustomSegmentedLEDDigit.InvalidateRefPoints;
begin
  SlantAngle := Display.Slant;
  Spacing := Display.SegmentSpacing;
  SegmentWidth := Display.SegmentThickness;
  
  MaxSlantDif := Trunc(Abs(ArcTan(SlantAngle * Pi / 180.0) * Display.DigitHeight));
  FRecalcNeeded := True;

  SetLength(FSegmentRenderInfo, 0);
  SetLength(FSegmentRenderInfo, SegmentCount);
  FillChar(FSegmentRenderInfo[0], SegmentCount * SizeOf(FSegmentRenderInfo[0]), 0);
  Display.InvalidateView;
end;

function TJvCustomSegmentedLEDDigit.NeedsPainting: Boolean;
begin
  Result := FRecalcNeeded or ((Display <> nil) and Display.FullRefreshNeeded);
end;

procedure TJvCustomSegmentedLEDDigit.Paint;
var
  I: Integer;
begin
  if RecalcNeeded then
  begin
    RecalcRefPoints;
    RecalcSegments;
    FRecalcNeeded := False;
  end;
  for I := 0 to SegmentCount - 1 do
    PaintSegment(I);
end;

procedure TJvCustomSegmentedLEDDigit.PaintSegment(Index: Integer);
var
  SegColor: TColor;
begin
  SegColor := GetSegmentColor(Index);
  Display.Canvas.Brush.Color := SegColor;
  Display.Canvas.Pen.Color := SegColor;
  case FSegmentRenderInfo[Index].RenderType of
    srtPolygon:
      Display.Canvas.Polygon(FSegmentRenderInfo[Index].Points);
    srtCircle:
      Display.Canvas.Ellipse(
        FSegmentRenderInfo[Index].Points[0].x, FSegmentRenderInfo[Index].Points[0].y,
        FSegmentRenderInfo[Index].Points[1].x, FSegmentRenderInfo[Index].Points[1].y);
    srtRect:
      Display.Canvas.Rectangle(
        FSegmentRenderInfo[Index].Points[0].x, FSegmentRenderInfo[Index].Points[0].y,
        FSegmentRenderInfo[Index].Points[1].x, FSegmentRenderInfo[Index].Points[1].y);
  end;
end;

constructor TJvCustomSegmentedLEDDigit.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  InvalidateRefPoints;
end;

//===TJvBaseSegmentedLEDDigit=======================================================================

procedure TJvBaseSegmentedLEDDigit.SetUseDP(Value: Boolean);
begin
  if Value <> UseDP then
  begin
    FUseDP := Value;
    UpdateDPWidth;
  end;
end;

function TJvBaseSegmentedLEDDigit.GetDPWidth: Integer;
begin
  Result := FDPWidth;
end;

procedure TJvBaseSegmentedLEDDigit.SetDPWidth(Value: Integer);
begin
  if Value <> DPWidth then
  begin
    FDPWidth := Value;
    Display.UpdateDigitsPositions;
  end;
end;

procedure TJvBaseSegmentedLEDDigit.UpdateDPWidth;
begin
  if UseDP then
  begin
    // Determine if width will suffice for the DP, otherwise set FDPWidth to the required additional width
    if MaxSlantDif < (Spacing + SegmentWidth) then
      DPWidth := Spacing + SegmentWidth - MaxSlantDif
    else
      DPWidth := 0;
  end
  else
    DPWidth := 0;
end;

procedure TJvBaseSegmentedLEDDigit.CalcASeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing div 2, FRefTop, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2, FRefTop, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth, FRefTop + SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth, FRefTop + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcBSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefRight, FRefTop + Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefRight, FRefCenterY - Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefRight - SegmentWidth, FRefCenterY - Spacing div 2 - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefRight - SegmentWidth, FRefTop + Spacing div 2 + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcCSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefRight, FRefCenterY + Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefRight, FRefBottom - Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefRight - SegmentWidth, FrefBottom - Spacing div 2 - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefRight - SegmentWidth, FRefCenterY + Spacing div 2 + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcDSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing div 2, FRefBottom, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2, FRefBottom, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth, FRefBottom - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth, FRefBottom - SegmentWidth, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcESeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft, FRefCenterY + Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefLeft, FRefBottom - Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefLeft + SegmentWidth, FRefBottom - Spacing div 2 - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefLeft + SegmentWidth, FRefCenterY + Spacing div 2 + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcFSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft, FRefTop + Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefLeft, FRefCenterY - Spacing div 2, SlantAngle),
    AngleAdjustPoint(FRefLeft + SegmentWidth, FRefCenterY - Spacing div 2 - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefLeft + SegmentWidth, FRefTop + Spacing div 2 + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcGSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing div 2, FRefCenterY, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth div 2, FRefCenterY - SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth div 2, FRefCenterY - SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2, FRefCenterY, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth div 2, FRefCenterY + SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth div 2, FRefCenterY + SegmentWidth div 2, SlantAngle)
  ]);
end;

procedure TJvBaseSegmentedLEDDigit.CalcDPSeg(Index: Integer);
var
  UpperLeftPoint: TPoint;
begin
  UpperLeftPoint := AngleAdjustPoint(FRefRight + Spacing, FRefBottom - SegmentWidth, SlantAngle);
  SetSegmentRenderInfo(Index, srtCircle, [
    UpperLeftPoint,
    Point(UpperLeftPoint.X + SegmentWidth, UpperLeftPoint.Y + SegmentWidth)
  ]);
end;

function TJvBaseSegmentedLEDDigit.GetWidth: Integer;
begin
  Result := inherited GetWidth + DPWidth;
end;

procedure TJvBaseSegmentedLEDDigit.InvalidateRefPoints;
begin
  inherited InvalidateRefPoints;
  UpdateDPWidth;
end;

procedure TJvBaseSegmentedLEDDigit.RecalcRefPoints;
begin
  FRefLeft := Left + MaxSlantDif;
  FRefCenterX := FRefLeft + (Display.DigitWidth - 1) div 2;
  FRefRight := FRefLeft + Display.DigitWidth - 1;
  FRefTop := GetVertAdjust;
  FRefCenterY := FRefTop + (Display.DigitHeight - 1) div 2;
  FRefBottom := FRefTop + (Display.DigitHeight - 1);
end;

procedure TJvBaseSegmentedLEDDigit.RecalcSegments;
begin
  CalcASeg(0);
  CalcBSeg(1);
  CalcCSeg(2);
  CalcDSeg(3);
  CalcESeg(4);
  CalcFSeg(5);
  CalcGSeg(6);
  if UseDP then
    CalcDPSeg(7);
end;

class function TJvBaseSegmentedLEDDigit.SegmentCount: Integer;
begin
  Result := 8;
end;

class function TJvBaseSegmentedLEDDigit.GetSegmentName(Index: Integer): string;
begin
  if Index < 7 then
    Result := Chr(Ord('A') + Index)
  else if Index = 7 then
    Result := 'DP'
  else
    Result := '';
end;

class function TJvBaseSegmentedLEDDigit.GetSegmentIndex(Name: string): Integer;
begin
  Result := -1;
  Name := UpperCase(Name);
  if Length(Name) = 1 then
  begin
    Result := Ord(Name[1]) - Ord('A');
    if Result > 6 then
      Result := -1;
  end
  else if Name = 'DP' then
    Result := 7;
end;

//===TJvBaseSegmentedLEDCharacterMapper=============================================================

function TJvBaseSegmentedLEDCharacterMapper.UpdateStates(var Segments: Int64;
  SegMask: Int64): Boolean;
var
  OldValue: Int64;
begin
  OldValue := Segments;
  if FSegMapRemoves then
    Segments := Segments and not SegMask
  else
    Segments := Segments or SegMask;
  Result := Segments <> OldValue;
end;

procedure TJvBaseSegmentedLEDCharacterMapper.HandleDecimalSeparator(var Text: PChar;
  var Segments: Int64);
begin
  if (CurDigit is TJvBaseSegmentedLEDDigit) and TJvBaseSegmentedLEDDigit(CurDigit).UseDP then
  begin
    if UpdateStates(Segments, 1 shl CurDigit.GetSegmentIndex('DP')) then
      TextForDigit := TextForDigit + DecimalSeparator;
    while Text[0] = DecimalSeparator do
      Inc(Text);
  end;
end;

procedure TJvBaseSegmentedLEDCharacterMapper.ControlItemToSegments(var ControlItem: PChar;
  var Segments: Int64);
var
  OrdValue: Byte;
begin
  case ControlItem^ of
    '+':
      begin
        if FSegMapRemoves then
          TextForDigit := TextForDigit + '+';
        FSegMapRemoves := False;
        Inc(ControlItem);
      end;
    '-':
      begin
        if not FSegMapRemoves then
          TextForDigit := TextForDigit + '-';
        FSegMapRemoves := True;
        Inc(ControlItem);
      end;
    '&':
      begin
        Inc(ControlItem);
        if CharToSegments(ControlItem^, Segments) then
          TextForDigit := TextForDigit + '&' + ControlItem[0];
        Inc(ControlItem);
      end;
    '#':
      begin
        Inc(ControlItem);
        OrdValue := 0;
        while ControlItem[0] in ['0' .. '9'] do
        begin
          if OrdValue >= 100 then
            OrdValue := OrdValue mod 100;
          if OrdValue >= 26 then
            OrdValue := OrdValue mod 10;
          OrdValue := OrdValue * 10 + (Ord(ControlItem[0]) - Ord('0'));
          Inc(ControlItem);
        end;
        if CharToSegments(Chr(OrdValue), Segments) then
        begin
          if OrdValue in [32 .. 127] then
            TextForDigit := TextForDigit + '&' + Chr(OrdValue)
          else
            TextForDigit := TextForDigit + '#' + IntToStr(OrdValue);
        end;
      end;
    else
        MapSegNamesToSegments(ControlItem, Segments);
  end;
  while ControlItem[0] = ';' do
    Inc(ControlItem);
end;

procedure TJvBaseSegmentedLEDCharacterMapper.MapControlItems(var Text: PChar; var Segments: Int64);
begin
  Inc(Text);
  TextForDigit := TextForDigit + '[';
  while not (Text^ in [#0, ']']) do
    ControlItemToSegments(Text, Segments);
  if Text^ = ']' then
  begin
    Inc(Text);
    TextForDigit := TextForDigit + ']';
  end;
  if Text[0] = DecimalSeparator then
    HandleDecimalSeparator(Text, Segments);
end;

procedure TJvBaseSegmentedLEDCharacterMapper.MapSimpleText(var Text: PChar; var Segments: Int64);
begin
  if CharToSegments(Text^, Segments) then
    TextForDigit := TextForDigit + Text^;
  Inc(Text);
  if Text[0] = DecimalSeparator then
    HandleDecimalSeparator(Text, Segments);
end;

procedure TJvBaseSegmentedLEDCharacterMapper.MapSegNamesToSegments(var Text: Pchar;
  var Segments: Int64);
var
  SortedSegNames: TStrings;
  I: Integer;
begin
  SortedSegNames := TStringList.Create;
   try
    for I := 0 to CurDigit.SegmentCount - 1 do
      SortedSegNames.Add(CurDigit.GetSegmentName(I));
    TStringList(SortedSegNames).Sort;

    while not (Text[0] in [#0, ']', ';']) do
    begin
      for I := SortedSegNames.Count - 1 downto 0 do
      begin
        if AnsiStrLIComp(Text, PChar(SortedSegNames[I]), Length(SortedSegNames[I])) = 0 then
        begin
          if UpdateStates(Segments, 1 shl CurDigit.GetSegmentIndex(SortedSegNames[I])) then
            TextForDigit := TextForDigit + SortedSegNames[I];
          Inc(Text, Length(SortedSegNames[I]));
          Break; // End the for loop
        end;
        if Text[0] = ',' then
          Inc(Text);
      end;
    end;
   finally
     FreeAndNil(SortedSegNames);
   end;
end;

procedure TJvBaseSegmentedLEDCharacterMapper.PrimMapText(var Text: PChar; var Segments: Int64);
begin
  case Text^ of
    #0:
      Exit;
    '[':
      MapControlItems(Text, Segments);
    else
      MapSimpleText(Text, Segments);
  end;
end;

procedure TJvBaseSegmentedLEDCharacterMapper.MapText(var Text: PChar;
  ADigit: TJvCustomSegmentedLEDDigit);
var
  States: Int64;
begin
  FCurDigit := ADigit;
  FTextForDigit := '';
  States := 0;
  FSegMapRemoves := False;
  PrimMapText(Text, States);
  CurDigit.SetSegmentStates(States);
  CurDigit.UpdateText(FTextForDigit);
end;

//===TJv7SegmentedLEDDigit==========================================================================

function TJv7SegmentedLEDDigit.GetUseColon: T7SegColonUsage;
begin
  Result := FUseColon;
end;

procedure TJv7SegmentedLEDDigit.SetUseColon(Value: T7SegColonUsage);
begin
  if Value <> UseColon then
  begin
    FUseColon := Value;
    InvalidateRefPoints;
  end;
end;

class function TJv7SegmentedLEDDigit.MapperClass: TJvSegmentedLEDCharacterMapperClass;
begin
  Result := TJv7SegmentedLEDCharacterMapper;
end;

class function TJv7SegmentedLEDDigit.SegmentCount: Integer;
begin
  Result := 10;
end;

class function TJv7SegmentedLEDDigit.GetSegmentName(Index: Integer): string;
begin
  if Index <= 7 then
    Result := inherited GetSegmentName(Index)
  else if Index = 8 then
    Result := 'CL'
  else if Index = 9 then
    Result := 'CH'
  else
    Result := '';
end;

class function TJv7SegmentedLEDDigit.GetSegmentIndex(Name: string): Integer;
begin
  Result := inherited GetSegmentIndex(Name);
  if Result = -1 then
  begin
    Name := UpperCase(Name);
    if Name = 'CL' then
      Result := 8
    else if Name = 'CH' then
      Result := 9;
  end;
end;

procedure TJv7SegmentedLEDDigit.RecalcSegments;
begin
  if UseColon <> scuColonOnly then
    inherited RecalcSegments;
  if UseColon in [scuLowOnly, scuFull, scuColonOnly] then
    CalcCLSeg(8);
  if UseColon in [scuFull, scuColonOnly] then
    CalcCHSeg(9);
end;

procedure TJv7SegmentedLEDDigit.CalcCHSeg(Index: Integer);
var
  UpperLeftPoint: TPoint;
begin
  UpperLeftPoint := AngleAdjustPoint(FRefCenterX - SegmentWidth div 2,
    (FRefCenterY - FRefTop) div 2 + FRefTop, SlantAngle);
  SetSegmentRenderInfo(Index, srtCircle, [
    UpperLeftPoint,
    Point(UpperLeftPoint.X + SegmentWidth, UpperLeftPoint.Y + SegmentWidth)
  ]);
end;

procedure TJv7SegmentedLEDDigit.CalcCLSeg(Index: Integer);
var
  UpperLeftPoint: TPoint;
begin
  UpperLeftPoint := AngleAdjustPoint(FRefCenterX - SegmentWidth div 2,
    (FRefBottom - FRefCenterY) div 2 + FRefCenterY - SegmentWidth div 2, SlantAngle);
  SetSegmentRenderInfo(Index, srtCircle, [
    UpperLeftPoint,
    Point(UpperLeftPoint.X + SegmentWidth, UpperLeftPoint.Y + SegmentWidth)
  ]);
end;

//===TJv7SegmentedLEDCharacterMapper================================================================

function TJv7SegmentedLEDCharacterMapper.CharToSegments(Ch: Char; var Segments: Int64): Boolean;
begin
  Result := UpdateStates(Segments, FDefaultMapping[Ch]) or (Ch = ' ');
end;

constructor TJv7SegmentedLEDCharacterMapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultMapping[' '] := 0;
  FDefaultMapping['0'] := 63;   // A + B + C + D + E + F;
  FDefaultMapping['1'] := 6;    // B + C;
  FDefaultMapping['2'] := 91;   // A + B + D + E + G
  FDefaultMapping['3'] := 79;   // A + B + C + D + G
  FDefaultMapping['4'] := 102;  // B + C + F + G;
  FDefaultMapping['5'] := 109;  // A + C + D + F + G;
  FDefaultMapping['6'] := 125;  // A + C + D + E + F + G;
  FDefaultMapping['7'] := 7;    // A + B + C;
  FDefaultMapping['8'] := 127;  // A + B + C + D + E + F + G;
  FDefaultMapping['9'] := 111;  // A + B + C + D + F + G;
  FDefaultMapping['A'] := 119;  // A + B + C + E + F + G;
  FDefaultMapping['a'] := 119;  // A + B + C + E + F + G;
  FDefaultMapping['B'] := 124;  // C + D + E + F + G;
  FDefaultMapping['b'] := 124;  // C + D + E + F + G;
  FDefaultMapping['C'] := 57;   // A + D + E + F;
  FDefaultMapping['c'] := 88;   // D + E + G;
  FDefaultMapping['D'] := 94;   // B + C + D + E + G;
  FDefaultMapping['d'] := 94;   // B + C + D + E + G;
  FDefaultMapping['E'] := 121;  // A + D + E + F + G;
  FDefaultMapping['e'] := 121;  // A + D + E + F + G;
  FDefaultMapping['F'] := 113;  // A + E + F + G;
  FDefaultMapping['f'] := 113;  // A + E + F + G;
  FDefaultMapping['H'] := 118;  // B + C + E + F + G;
  FDefaultMapping['h'] := 116;  // C + E + F + G;
  FDefaultMapping['L'] := 56;   // D + E + F;
  FDefaultMapping['l'] := 56;   // D + E + F;
  FDefaultMapping['O'] := 92;   // C + D + E + G;
  FDefaultMapping['o'] := 92;   // C + D + E + G;
  FDefaultMapping['P'] := 115;  // A + B + E + F + G;
  FDefaultMapping['p'] := 115;  // A + B + E + F + G;
  FDefaultMapping['R'] := 80;   // E + G;
  FDefaultMapping['R'] := 80;   // E + G;
  FDefaultMapping[''''] := 32;  // F
  FDefaultMapping['"'] := 34;   // B + F;
  FDefaultMapping['ø'] := 99;   // A + B + F + G;
  FDefaultMapping['°'] := 99;   // A + B + F + G;
  FDefaultMapping['-'] := 64;   // G;
  FDefaultMapping['U'] := 62;   // B + C + D + E + F;
  FDefaultMapping['u'] := 28;   // C + D + E;
  FDefaultMapping['y'] := 106;  // B + C + D + F + G;
  FDefaultMapping['.'] := 256;  // CL;
  FDefaultMapping[':'] := 768;  // CL + CH;
end;

procedure ModuleUnload(Instance: Longint);
begin
  UnregisterModuleSegmentedLEDDigitClasses(HMODULE(Instance));
end;

function TextIndex(S: string; const Strings: array of string): Integer;
begin
  Result := High(Strings);
  while (Result > -1) and not AnsiSameText(S, Strings[Result]) do
    Dec(Result);
end;

function IdentToUnlitColor(const Ident: string; var Int: Longint): Boolean;
begin
  Int := TextIndex(Ident, ['clDefaultBackground', 'clDefaultLitColor']);
  Result := Int > -1;
  if Result then
    Inc(Int, clDefaultBackground)
  else
  begin
    Result := True;
    Int := StringToColor(Ident);
  end;
end;

function UnlitColorToIdent(Int: Longint; var Ident: string): Boolean;
begin
  Result := True;
  case Int of
    clDefaultBackground:
      Ident := 'clDefaultBackground';
    clDefaultLitColor:
      Ident := 'clDefaultLitColor';
    else
      Ident := ColorToString(Int);
  end;
end;

initialization
  GDigitClassList := TThreadList.Create;
  AddModuleUnloadProc(ModuleUnload);

  RegisterSegmentedLEDDigitClasses([
    TJv7SegmentedLEDDigit
  ]);

  RegisterIntegerConsts(TypeInfo(TUnlitColor), IdentToUnlitColor, UnlitColorToIdent);
finalization
  UnregisterModuleSegmentedLEDDigitClasses(HInstance);
  FreeAndNil(GDigitClassList);
  RemoveModuleUnloadProc(ModuleUnload);
end.
