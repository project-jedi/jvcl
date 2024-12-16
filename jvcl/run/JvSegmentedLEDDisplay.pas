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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  * Automatic unlit color calculation is not working properly. Maybe a function in JclGraphUtil
    can help out there.
-----------------------------------------------------------------------------}
// $Id$

unit JvSegmentedLEDDisplay;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, Graphics,
  JvComponent, JvTypes;

// Additional color values for unlit color settings (TUnlitColor type)
// asn: does this work with clx/linux?
const
  clDefaultBackground = TColor($20100001);
  clDefaultLitColor = TColor($20100002);
  NullHandle = 0;

type
  TJvCustomSegmentedLEDDisplay = class;
  TJvSegmentedLEDDigits = class;
  TJvCustomSegmentedLEDDigit = class;
  TJvSegmentedLEDCharacterMapper = class;

  TJvSegmentedLEDDigitClass = class of TJvCustomSegmentedLEDDigit;

  TJvSegmentedLEDDigitClassName = type string;
  TUnlitColor = type TColor;
  TSlantAngle = 0 .. 44;
  TSLDHitInfo = (shiNowhere, shiDigit, shiDigitSegment, shiClientArea);
  {$IFNDEF RTL200_UP}
  TCharSet = set of Char;
  {$ENDIF ~RTL200_UP}
  TSegCharMapHeader = record
    ID: array[0..11] of AnsiChar;
    MappedChars: TCharSet;
    Flags: Longint;
  end;

  TSegmentRenderType = (srtNone, srtPolygon, srtRect, srtCircle);
  TPointArray = array of TPoint;

  TSegmentRenderInfo = record
    RenderType: TSegmentRenderType;
    Points: TPointArray;
  end;
  TSegmentRenderInfoArray = array of TSegmentRenderInfo;

  EJVCLSegmentedLEDException = class(EJVCLException);

  TJvCustomSegmentedLEDDisplay = class(TJvGraphicControl)
  private
    FCharacterMapper: TJvSegmentedLEDCharacterMapper;
    FDigitClass: TJvSegmentedLEDDigitClass;
    FDigits: TJvSegmentedLEDDigits;
    FDotSize: Integer;
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
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Paint; override;
    function GetText: string;
    procedure SetText(Value: string);
    procedure SetDigitHeight(Value: Integer);
    procedure SetDigits(Value: TJvSegmentedLEDDigits);
    procedure SetDigitSpacing(Value: Integer);
    procedure SetDigitWidth(Value: Integer);
    procedure SetDigitClass(Value: TJvSegmentedLEDDigitClass);
    procedure SetDotSize(Value: Integer);
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
    property AutoSize default True;
    property CharacterMapper: TJvSegmentedLEDCharacterMapper read FCharacterMapper;
    property DigitClass: TJvSegmentedLEDDigitClass read FDigitClass write SetDigitClass;
    // Solely needed for design time support of DigitClass
    property DigitClassName: TJvSegmentedLEDDigitClassName read GetDigitClassName write SetDigitClassName;
    property DigitHeight: Integer read FDigitHeight write SetDigitHeight default 30;
    property Digits: TJvSegmentedLEDDigits read FDigits write SetDigits;
    property DigitSpacing: Integer read FDigitSpacing write SetDigitSpacing default 2;
    property DigitWidth: Integer read FDigitWidth write SetDigitWidth default 20;
    property DotSize: Integer read FDotSize write SetDotSize default 4;
    property SegmentLitColor: TColor read FSegmentLitColor write SetSegmentLitColor default clWindowText;
    property SegmentSpacing: Integer read FSegmentSpacing write SetSegmentSpacing default 2;
    property SegmentThickness: Integer read FSegmentThickness write SetSegmentThickness default 2;
    property SegmentUnlitColor: TUnlitColor read FSegmentUnlitColor write SetSegmentUnlitColor default clDefaultLitColor;
    property Slant: TSlantAngle read FSlant write SetSlant default 0;
    property Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemapText;
    function GetHitInfo(X, Y: Integer): TSLDHitInfo; overload;
    function GetHitInfo(X, Y: Integer; out Digit: TJvCustomSegmentedLEDDigit;
      out SegmentIndex: Integer): TSLDHitInfo; overload;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvSegmentedLEDDisplay = class(TJvCustomSegmentedLEDDisplay)
  public
    property DigitClass;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Color;
    property DigitClassName;
    property DigitHeight;
    property Digits;
    property DigitSpacing;
    property DigitWidth;
    property DotSize;
    property ParentColor;
    property PopupMenu;
    property SegmentLitColor;
    property SegmentSpacing;
    property SegmentThickness;
    property SegmentUnlitColor;
    property Slant;
    property Text;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
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
    DotSize: Integer;
    SegmentWidth: Integer;
    SlantAngle: Integer;
    Spacing: Integer;
    MaxSlantDif: Integer;
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

    procedure EnableAllSegs; dynamic;
    function GetSegmentRenderInfo(Index: Integer; out RenderType: TSegmentRenderType;
      out Points: TPointArray): Boolean;
    procedure SetSegmentRenderInfo(Index: Integer; RenderType: TSegmentRenderType;
      Points: array of TPoint);
    function GetSegmentState(Index: Integer): Boolean;
    procedure SetSegmentState(Index: Integer; Value: Boolean);
    procedure SetSegmentStates(Value: Int64);
    procedure UpdateText(Value: string);
    procedure RecalcRefPoints; virtual; abstract;
    procedure RecalcSegments; virtual; abstract;
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

    class function MapperFileID: AnsiString; virtual;

    property BaseTop: Integer read GetBaseTop;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property VertAdjust: Integer read GetVertAdjust;
    property Width: Integer read GetWidth;
    property Text: string read FText write SetText stored False;

    property RecalcNeeded: Boolean read FRecalcNeeded;
  public
    constructor Create(Collection: TCollection); override;
    function GetHitInfo(X, Y: Integer): TSLDHitInfo; overload;
    function GetHitInfo(X, Y: Integer; out SegmentIndex: Integer): TSLDHitInfo; overload;
    function PtInSegment(SegmentIndex: Integer; Pt: TPoint): Boolean; virtual;
    class function SegmentCount: Integer; virtual;
    class function GetSegmentName(Index: Integer): string; virtual;
    class function GetSegmentIndex(Name: string): Integer; virtual;
    function GetSegmentStates: Int64;
    function GetSegmentString: string; virtual; abstract;
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
    procedure EnableAllSegs; override;
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

    property DPWidth: Integer read GetDPWidth write SetDPWidth;
    property UseDP: Boolean read FUseDP write SetUseDP;
  public
    class function SegmentCount: Integer; override;
    class function GetSegmentName(Index: Integer): string; override;
    class function GetSegmentIndex(Name: string): Integer; override;
    function GetSegmentString: string; override;
  end;

  TJvSegmentedLEDCharacterMapper = class(TPersistent)
  private
    FCurDigit: TJvCustomSegmentedLEDDigit;
    FTextForDigit: string;
    FSegMapRemoves: Boolean;
    FActiveMapping: array[Char] of Int64;
    FMappingChanged: Boolean;
    FDisplay: TJvCustomSegmentedLEDDisplay;
  protected
    function GetCharMapping(Chr: Char): Int64;
    procedure SetCharMapping(Chr: Char; Value: Int64);
    function MaxSegments: Integer; dynamic;
    function MapToSeparators: Boolean; dynamic;
    procedure PrimReadMapping(const HdrInfo: TSegCharMapHeader; Stream: TStream); dynamic;
    function UpdateStates(var Segments: Int64; SegMask: Int64): Boolean;
    procedure HandleDecimalSeparator(var Text: PChar; var Segments: Int64); virtual;
    function CharToSegments(Ch: Char; var Segments: Int64): Boolean; virtual;
    procedure ControlItemToSegments(var ControlItem: PChar; var Segments: Int64); virtual;
    procedure MapControlItems(var Text: PChar; var Segments: Int64); virtual;
    procedure MapSimpleText(var Text: PChar; var Segments: Int64); virtual;
    procedure MapSegNamesToSegments(var Text: PChar; var Segments: Int64); virtual;
    procedure PrimMapText(var Text: PChar; var Segments: Int64); virtual;
    procedure Modified;

    property CurDigit: TJvCustomSegmentedLEDDigit read FCurDigit;
    property Display: TJvCustomSegmentedLEDDisplay read FDisplay;
    property SegMapRemoves: Boolean read FSegMapRemoves write FSegMapRemoves;
    property TextForDigit: string read FTextForDigit write FTextForDigit;
    property MappingChanged: Boolean read FMappingChanged;
  public
    constructor Create(ADisplay: TJvCustomSegmentedLEDDisplay);
    procedure MapText(var Text: PChar; ADigit: TJvCustomSegmentedLEDDigit);
    procedure Clear;
    procedure LoadDefaultMapping; dynamic;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream); dynamic;

    property CharMapping[Chr: Char]: Int64 read GetCharMapping write SetCharMapping;
  end;

  // 7-segmented digit
  T7SegColonUsage = (scuNone, scuLowOnly, scuFull, scuColonOnly);
  TJv7SegmentedLEDDigit = class(TJvBaseSegmentedLEDDigit)
  private
    FUseColon: T7SegColonUsage;
  protected
    procedure EnableAllSegs; override;
    function GetUseColon: T7SegColonUsage;
    procedure SetUseColon(Value: T7SegColonUsage);
    procedure RecalcSegments; override;
    class function MapperFileID: AnsiString; override;
    procedure CalcCHSeg(Index: Integer); virtual;
    procedure CalcCLSeg(Index: Integer); virtual;
  public
    class function SegmentCount: Integer; override;
    class function GetSegmentName(Index: Integer): string; override;
    class function GetSegmentIndex(Name: string): Integer; override;
  published
    property UseDP;
    property UseColon: T7SegColonUsage read GetUseColon write SetUseColon;
    property Text;
  end;

  // 14-segmented digit
  TJv14SegmentedLEDDigit = class(TJvBaseSegmentedLEDDigit)
  protected
    procedure RecalcSegments; override;
    class function MapperFileID: AnsiString; override;
    procedure CalcG1Seg(Index: Integer); virtual;
    procedure CalcG2Seg(Index: Integer); virtual;
    procedure CalcHSeg(Index: Integer); virtual;
    procedure CalcISeg(Index: Integer); virtual;
    procedure CalcJSeg(Index: Integer); virtual;
    procedure CalcKSeg(Index: Integer); virtual;
    procedure CalcLSeg(Index: Integer); virtual;
    procedure CalcMSeg(Index: Integer); virtual;
  public
    class function SegmentCount: Integer; override;
    class function GetSegmentName(Index: Integer): string; override;
    class function GetSegmentIndex(Name: string): Integer; override;
  published
    property UseDP;
    property Text;
  end;

  // 16-segmented digit
  TJv16SegmentedLEDDigit = class(TJv14SegmentedLEDDigit)
  protected
    procedure RecalcSegments; override;
    class function MapperFileID: AnsiString; override;
    procedure CalcA1Seg(Index: Integer); virtual;
    procedure CalcA2Seg(Index: Integer); virtual;
    procedure CalcD1Seg(Index: Integer); virtual;
    procedure CalcD2Seg(Index: Integer); virtual;
    procedure CalcISeg(Index: Integer); override;
    procedure CalcLSeg(Index: Integer); override;
  public
    class function SegmentCount: Integer; override;
    class function GetSegmentName(Index: Integer): string; override;
    class function GetSegmentIndex(Name: string): Integer; override;
  published
    property UseDP;
    property Text;
  end;

// TUnlitColor support routines
function IdentToUnlitColor(const Ident: string; var Int: Longint): Boolean;
function UnlitColorToIdent(Int: Longint; var Ident: string): Boolean;
function StringToUnlitColor(const S: string): TUnlitColor;
function UnlitColorToString(const Color: TUnlitColor): string;
// DigitClass registration routines
function DigitClassList: TThreadList;
procedure RegisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
procedure RegisterSegmentedLEDDigitClasses(DigitClasses: array of TJvSegmentedLEDDigitClass);
procedure UnregisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
procedure UnregisterSegmentedLEDDigitClasses(DigitClasses: array of TJvSegmentedLEDDigitClass);
procedure UnregisterModuleSegmentedLEDDigitClasses(Module: HMODULE);

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
  Controls, SysUtils,
  JclAnsiStrings, JclGraphUtils,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvThemes, JvConsts, JvResources, JclSysUtils;

{$R JvSegmentedLEDDisplay.res}

var
  GDigitClassList: TThreadList = nil;

//=== DigitClass registration routines =======================================

function DigitClassList: TThreadList;
begin
  if GDigitClassList = nil then
    GDigitClassList := TThreadList.Create;
  Result := GDigitClassList;
end;

procedure RegisterSegmentedLEDDigitClass(DigitClass: TJvSegmentedLEDDigitClass);
begin
  with DigitClassList.LockList do
  try
    if IndexOf(DigitClass) > -1 then
      raise EJVCLSegmentedLEDException.CreateRes(@RsEDuplicateDigitClass);
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
{$IFDEF UNIX}
begin
  // ?
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  with DigitClassList.LockList do
  try
    for I := Count - 1 downto 0 do
      if (Module = 0) or (HMODULE(FindHInstance(Items[I])) = Module) then
        Delete(I);
  finally
    DigitClassList.UnlockList;
  end;
end;
{$ENDIF MSWINDOWS}


//=== Helper routine: AngleAdjustPoint =======================================

function AngleAdjustPoint(X, Y, Angle: Integer): TPoint;
begin
  Result.X := X - Trunc(ArcTan(Angle * Pi / 180.0) * Y);
  Result.Y := Y;
end;

//=== Helper routine: TextIndex ==============================================

function TextIndex(S: string; const Strings: array of string): Integer;
begin
  Result := High(Strings);
  while (Result > -1) and not AnsiSameText(S, Strings[Result]) do
    Dec(Result);
end;

//=== { TJvCustomSegmentedLEDDisplay } =======================================

constructor TJvCustomSegmentedLEDDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  AutoSize := True;
  FDigitClass := TJv7SegmentedLEDDigit;
  FCharacterMapper := TJvSegmentedLEDCharacterMapper.Create(Self);
  FDigits := TJvSegmentedLEDDigits.Create(Self);
  FDigitHeight := 30;
  FDigitSpacing := 2;
  FDigitWidth := 20;
  FDotSize := 4;
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
  FreeAndNil(FCharacterMapper);
  inherited Destroy;
end;

procedure TJvCustomSegmentedLEDDisplay.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('MapperData', CharacterMapper.LoadFromStream,
    CharacterMapper.SaveToStream, CharacterMapper.MappingChanged);
end;

procedure TJvCustomSegmentedLEDDisplay.Loaded;
begin
  inherited Loaded;
  RemapText;
end;

procedure TJvCustomSegmentedLEDDisplay.Paint;
var
  I: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psSolid;
  DrawThemedBackground(Self, Canvas, ClientRect);
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
    if AutoSize and not (Align in [alLeft, alRight, alClient]) and
      (Anchors * [akTop, akBottom] <> [akTop, akBottom]) and (ClientHeight <> MaxHeight) then
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
    if CharacterMapper <> nil then
      CharacterMapper.LoadDefaultMapping;
    if not (csLoading in ComponentState) then
      RemapText;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetDotSize(Value: Integer);
begin
  Value := Value and not 1;
  if Value <> DotSize then
  begin
    FDotSize := Value;
    InvalidateDigits;
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
  Value := Value and not 1;
  if Value <> SegmentSpacing then
  begin
    FSegmentSpacing := Value;
    InvalidateDigits;
  end;
end;

procedure TJvCustomSegmentedLEDDisplay.SetSegmentThickness(Value: Integer);
begin
  Value := Value and not 1;
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
        raise EJVCLSegmentedLEDException.CreateRes(@RsEInvalidClass);
    end
    else
      DigitClass := nil;
  end;
end;

function TJvCustomSegmentedLEDDisplay.GetRealUnlitColor: TColor;
begin
  if SegmentUnlitColor = clNone then
    Result := Color
  else
  if SegmentUnlitColor = clDefaultBackground then
    Result := CalcRealUnlitColorBackground
  else
  if SegmentUnlitColor = clDefaultLitColor then
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
  if AutoSize and not (Align in [alLeft, alRight, alClient]) and
    (Anchors * [akTop, akBottom] <> [akTop, akBottom]) and (ClientHeight <> MaxHeight) then
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
  if AutoSize and not (Align in [alLeft, alRight, alClient]) and
    (Anchors * [akTop, akBottom] <> [akTop, akBottom]) and (ClientHeight <> MaxHeight) then
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
    if AutoSize and not (Align in [alTop, alBottom, alClient]) and
      (Anchors * [akLeft, akRight] <> [akLeft, akRight]) and (ClientWidth <> X) then
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

procedure TJvCustomSegmentedLEDDisplay.RemapText;
begin
  PrimSetText(Text);
end;

function TJvCustomSegmentedLEDDisplay.GetHitInfo(X, Y: Integer): TSLDHitInfo;
var
  DummyDigit: TJvCustomSegmentedLEDDigit;
  DummyIndex: Integer;
begin
  Result := GetHitInfo(X, Y, DummyDigit, DummyIndex);
end;

function TJvCustomSegmentedLEDDisplay.GetHitInfo(X, Y: Integer;
  out Digit: TJvCustomSegmentedLEDDigit; out SegmentIndex: Integer): TSLDHitInfo;
var
  I: Integer;
begin
  Result := shiNowhere;
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    // Iterate over each digit and get the hit info from them
    I := Digits.Count;
    while (I > 0) and (Result = shiNowhere) do
    begin
      Dec(I);
      Result := Digits[I].GetHitInfo(X, Y, SegmentIndex);
    end;
    if Result <> shiNowhere then
      Digit := Digits[I]
    else // Result = shiNowhere, but we are in fact in the client area of the control (see outer if)
      Result := shiClientArea;
  end;
end;

//=== { TJvSegmentedLEDDigits } ==============================================

constructor TJvSegmentedLEDDigits.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvCustomSegmentedLEDDisplay(AOwner).DigitClass);
end;

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

//=== { TJvCustomSegmentedLEDDigit } =========================================

constructor TJvCustomSegmentedLEDDigit.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  InvalidateRefPoints;
end;

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

procedure TJvCustomSegmentedLEDDigit.EnableAllSegs;
begin
end;

function TJvCustomSegmentedLEDDigit.GetSegmentRenderInfo(Index: Integer;
  out RenderType: TSegmentRenderType; out Points: TPointArray): Boolean;
begin
  Result := (Index >= 0) and (Index < SegmentCount);
  if Result then
  begin
    RenderType := FSegmentRenderInfo[Index].RenderType;
    Points := FSegmentRenderInfo[Index].Points;
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
  DotSize := Display.DotSize;

  MaxSlantDif := Trunc(Abs(ArcTan(SlantAngle * Pi / 180.0) * Display.DigitHeight));
  FRecalcNeeded := True;

  SetLength(FSegmentRenderInfo, 0);
  SetLength(FSegmentRenderInfo, SegmentCount);
  FillChar(FSegmentRenderInfo[0], SegmentCount * SizeOf(FSegmentRenderInfo[0]), 0);
  Display.InvalidateView;
end;

function TJvCustomSegmentedLEDDigit.NeedsPainting: Boolean;
begin
  Result := FRecalcNeeded;
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
        FSegmentRenderInfo[Index].Points[0].X, FSegmentRenderInfo[Index].Points[0].Y,
        FSegmentRenderInfo[Index].Points[1].X, FSegmentRenderInfo[Index].Points[1].Y);
    srtRect:
      Display.Canvas.Rectangle(
        FSegmentRenderInfo[Index].Points[0].X, FSegmentRenderInfo[Index].Points[0].Y,
        FSegmentRenderInfo[Index].Points[1].X, FSegmentRenderInfo[Index].Points[1].Y);
  end;
end;

function TJvCustomSegmentedLEDDigit.GetHitInfo(X, Y: Integer): TSLDHitInfo;
var
  DummyIndex: Integer;
begin
  Result := GetHitInfo(X, Y, DummyIndex);
end;

function TJvCustomSegmentedLEDDigit.GetHitInfo(X, Y: Integer;
  out SegmentIndex: Integer): TSLDHitInfo;
begin
  Result := shiNowhere;
  if PtInRect(Rect(Left, 0, Width, Height + BaseTop), Point(X, Y)) then
  begin
    SegmentIndex := SegmentCount - 1;
    while (SegmentIndex >= 0) and not PtInSegment(SegmentIndex, Point(X, Y)) do
      Dec(SegmentIndex);
    if SegmentIndex > -1 then
      Result := shiDigitSegment
    else
      Result := shiDigit;
  end;
end;

function TJvCustomSegmentedLEDDigit.PtInSegment(SegmentIndex: Integer; Pt: TPoint): Boolean;
var
  SegType: TSegmentRenderType;
  SegPts: TPointArray;
  Rgn: HRGN;
begin
  if GetSegmentRenderInfo(SegmentIndex, SegType, SegPts) then
  begin
    case SegType of
      srtNone:
        Result := False;
      srtPolygon:
        begin
          Rgn := CreatePolygonRgn(SegPts[0], Length(SegPts), WINDING);
          try
            if Rgn <> NullHandle then
              Result := PtInRegion(Rgn, Pt.X, Pt.Y)
            else
              Result := False;
          finally
            DeleteObject(Rgn);
          end;
        end;
      srtRect:
        Result := PtInRect(Rect(SegPts[0].X, SegPts[0].Y, SegPts[1].X, SegPts[1].Y), Pt);
      srtCircle:
        begin
          Rgn := CreateEllipticRgn(SegPts[0].X, SegPts[0].Y, SegPts[1].X, SegPts[1].Y);
          try
            if Rgn <> NullHandle then
              Result := PtInRegion(Rgn, Pt.X, Pt.Y)
            else
              Result := False;
          finally
            DeleteObject(Rgn);
          end;
        end;
      else
        Result := False; // Call method to check additional render types?
    end;
  end
  else
    Result := False;
end;

function TJvCustomSegmentedLEDDigit.GetSegmentStates: Int64;
begin
  Result := FSegmentStates;
end;

class function TJvCustomSegmentedLEDDigit.MapperFileID: AnsiString;
begin
  // DO NOTHING.
  // THIS CAN'T BE AN ABSTRACT CLASS METHOD AS THIS IS NOT
  // SUPPORTED BY C++ BUILDER
end;

class function TJvCustomSegmentedLEDDigit.GetSegmentIndex(
  Name: string): Integer;
begin
  // DO NOTHING.
  // THIS CAN'T BE AN ABSTRACT CLASS METHOD AS THIS IS NOT
  // SUPPORTED BY C++ BUILDER
  Result := 0;
end;

class function TJvCustomSegmentedLEDDigit.GetSegmentName(
  Index: Integer): string;
begin
  // DO NOTHING.
  // THIS CAN'T BE AN ABSTRACT CLASS METHOD AS THIS IS NOT
  // SUPPORTED BY C++ BUILDER
  Result := '';
end;

class function TJvCustomSegmentedLEDDigit.SegmentCount: Integer;
begin
  // DO NOTHING.
  // THIS CAN'T BE AN ABSTRACT CLASS METHOD AS THIS IS NOT
  // SUPPORTED BY C++ BUILDER
  Result := 0;
end;

//=== { TJvBaseSegmentedLEDDigit } ===========================================

procedure TJvBaseSegmentedLEDDigit.EnableAllSegs;
begin
  inherited EnableAllSegs;
  UseDP := True;
end;

procedure TJvBaseSegmentedLEDDigit.SetUseDP(Value: Boolean);
begin
  if Value <> UseDP then
  begin
    FUseDP := Value;
    UpdateDPWidth;
    InvalidateRefPoints;
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
    if MaxSlantDif < (Spacing + DotSize) then
      DPWidth := Spacing + DotSize - MaxSlantDif
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
    AngleAdjustPoint(FRefRight - SegmentWidth, FRefBottom - Spacing div 2 - SegmentWidth, SlantAngle),
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
  UpperLeftPoint := AngleAdjustPoint(FRefRight + Spacing, FRefBottom - DotSize, SlantAngle);
  SetSegmentRenderInfo(Index, srtCircle, [
    UpperLeftPoint,
    Point(UpperLeftPoint.X + DotSize, UpperLeftPoint.Y + DotSize)
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
  else
  if Index = 7 then
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
  else
  if Name = 'DP' then
    Result := 7;
end;

function TJvBaseSegmentedLEDDigit.GetSegmentString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to SegmentCount - 1 do
  begin
    if GetSegmentState(I) then
    begin
      if Length(Result) > 0 then
        Result := Result + ',' + GetSegmentName(I)
      else
        Result := GetSegmentName(I);
    end;
  end;
end;

//=== { TJvSegmentedLEDCharacterMapper } =====================================

constructor TJvSegmentedLEDCharacterMapper.Create(ADisplay: TJvCustomSegmentedLEDDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
  LoadDefaultMapping;
end;

function TJvSegmentedLEDCharacterMapper.GetCharMapping(Chr: Char): Int64;
begin
  Result := FActiveMapping[Chr];
end;

procedure TJvSegmentedLEDCharacterMapper.SetCharMapping(Chr: Char; Value: Int64);
begin
  FActiveMapping[Chr] := Value;
  Modified;
end;

function TJvSegmentedLEDCharacterMapper.MaxSegments: Integer;
begin
  Result := Display.DigitClass.SegmentCount;
end;

function TJvSegmentedLEDCharacterMapper.MapToSeparators: Boolean;
begin
  Result := True;
end;

procedure TJvSegmentedLEDCharacterMapper.PrimReadMapping(const HdrInfo: TSegCharMapHeader;
  Stream: TStream);
var
  Chr: Char;
  MapSize: Byte;
  OldMapping: Int64;
begin
  Clear; // clear the mapping table
  MapSize := HdrInfo.Flags and 7;
  for Chr := #0 to #255 do
    if CharInSet(Chr, HdrInfo.MappedChars) then
      Stream.ReadBuffer(FActiveMapping[Chr], MapSize);
  if HdrInfo.Flags and 16 <> 0 then
  begin
    // Swap . for DecimalSeparator and , for ThousandSeparator
    if JclFormatSettings.DecimalSeparator <> '.' then
    begin
      OldMapping := FActiveMapping[JclFormatSettings.DecimalSeparator];
      FActiveMapping[JclFormatSettings.DecimalSeparator] := FActiveMapping['.'];
      FActiveMapping[JclFormatSettings.ThousandSeparator] := OldMapping;
    end;
  end;
end;

function TJvSegmentedLEDCharacterMapper.UpdateStates(var Segments: Int64;
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

procedure TJvSegmentedLEDCharacterMapper.HandleDecimalSeparator(var Text: PChar;
  var Segments: Int64);
begin
  if (CurDigit is TJvBaseSegmentedLEDDigit) and TJvBaseSegmentedLEDDigit(CurDigit).UseDP then
  begin
    if UpdateStates(Segments, 1 shl CurDigit.GetSegmentIndex('DP')) then
      TextForDigit := TextForDigit + JclFormatSettings.DecimalSeparator;
    while Text[0] = JclFormatSettings.DecimalSeparator do
      Inc(Text);
  end;
end;

function TJvSegmentedLEDCharacterMapper.CharToSegments(Ch: Char; var Segments: Int64): Boolean;
begin
  Result := UpdateStates(Segments, FActiveMapping[Ch]) or (Ch = ' ');
end;

procedure TJvSegmentedLEDCharacterMapper.ControlItemToSegments(var ControlItem: PChar;
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
        while CharInSet(ControlItem[0], DigitSymbols) do
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

procedure TJvSegmentedLEDCharacterMapper.MapControlItems(var Text: PChar; var Segments: Int64);
begin
  Inc(Text);
  TextForDigit := TextForDigit + '[';
  while not CharInSet(Text^, [#0, ']']) do
    ControlItemToSegments(Text, Segments);
  if Text^ = ']' then
  begin
    Inc(Text);
    TextForDigit := TextForDigit + ']';
  end;
  if Text[0] = JclFormatSettings.DecimalSeparator then
    HandleDecimalSeparator(Text, Segments);
end;

procedure TJvSegmentedLEDCharacterMapper.MapSimpleText(var Text: PChar; var Segments: Int64);
begin
  if CharToSegments(Text^, Segments) then
    TextForDigit := TextForDigit + Text^;
  Inc(Text);
  if Text[0] = JclFormatSettings.DecimalSeparator then
    HandleDecimalSeparator(Text, Segments);
end;

procedure TJvSegmentedLEDCharacterMapper.MapSegNamesToSegments(var Text: PChar;
  var Segments: Int64);
var
  SortedSegNames: TStringList;
  I: Integer;
begin
  SortedSegNames := TStringList.Create;
  try
    for I := 0 to CurDigit.SegmentCount - 1 do
      SortedSegNames.Add(CurDigit.GetSegmentName(I));
    SortedSegNames.Sort;

    while not CharInSet(Text[0], [#0, ']', ';']) do
    begin
      I := SortedSegNames.Count - 1;
      while I >= 0 do
      begin
        if AnsiStrLIComp(Text, PChar(SortedSegNames[I]), Length(SortedSegNames[I])) = 0 then
        begin
          if UpdateStates(Segments, 1 shl CurDigit.GetSegmentIndex(SortedSegNames[I])) then
            TextForDigit := TextForDigit + SortedSegNames[I];
          Inc(Text, Length(SortedSegNames[I]));
          Break; // End the for loop
        end;
        Dec(I);
      end;
      if I < 0 then
        Inc(Text);
      if Text[0] = ',' then
        Inc(Text);
    end;
   finally
     FreeAndNil(SortedSegNames);
   end;
end;

procedure TJvSegmentedLEDCharacterMapper.PrimMapText(var Text: PChar; var Segments: Int64);
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

procedure TJvSegmentedLEDCharacterMapper.Modified;
begin
  FMappingChanged := True;
  Display.RemapText;
end;

procedure TJvSegmentedLEDCharacterMapper.MapText(var Text: PChar;
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
  if FTextForDigit = '' then // assume a space was used
    FTextForDigit := ' ';
  CurDigit.UpdateText(FTextForDigit);
end;

procedure TJvSegmentedLEDCharacterMapper.Clear;
begin
  FillChar(FActiveMapping[#0], SizeOf(FActiveMapping), 0);
end;

procedure TJvSegmentedLEDCharacterMapper.LoadDefaultMapping;
var
  resName: AnsiString;
  Stream: TStream;
begin
  resName := Display.DigitClass.MapperFileID + '_DEFAULT';
  if FindResource(HInstance, PChar(string(resName)), RT_RCDATA) <> 0 then
  begin
    Stream := TResourceStream.Create(HInstance, string(Display.DigitClass.MapperFileID + '_DEFAULT'), RT_RCDATA);
    try
      LoadFromStream(Stream);
      FMappingChanged := False;
    finally
      FreeAndNil(Stream);
    end;
  end
  else
    Clear;
end;

procedure TJvSegmentedLEDCharacterMapper.LoadFromFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
   try
     LoadFromStream(FS);
   finally
     FS.Free;
   end;
end;

procedure TJvSegmentedLEDCharacterMapper.LoadFromStream(Stream: TStream);
var
  OrgPos: Integer;
  Hdr: TSegCharMapHeader;
begin
  OrgPos := Stream.Position;
  try
    Stream.ReadBuffer(Hdr, SizeOf(Hdr));
    if StrLICompA(Hdr.ID, PAnsiChar(Display.DigitClass.MapperFileID), Length(Display.DigitClass.MapperFileID)) = 0 then
      PrimReadMapping(Hdr, Stream)
    else
      raise EJVCLSegmentedLEDException.CreateRes(@RsEInvalidMappingFile);
  except
    Stream.Position := OrgPos;
    raise;
  end;
end;

procedure TJvSegmentedLEDCharacterMapper.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
   try
     SaveToStream(FS);
   finally
     FS.Free;
   end;
end;

procedure TJvSegmentedLEDCharacterMapper.SaveToStream(Stream: TStream);
var
  Hdr: TSegCharMapHeader;
  TmpID: AnsiString;
  MapSize: Byte;
  Chr: AnsiChar;
  TmpDot: Int64;
  TmpComma: Int64;
begin
  FillChar(Hdr, SizeOf(Hdr), 0);
  TmpID := Display.DigitClass.MapperFileID;
  Move(TmpID[1], Hdr.ID, Length(TmpID));
  Hdr.Flags := MaxSegments;
  MapSize := (Hdr.Flags div 8) + Ord((Hdr.Flags mod 8) <> 0);
  Hdr.Flags := MapSize;
  Hdr.Flags := Hdr.Flags or (16 * Ord(MapToSeparators));
  Hdr.MappedChars := [];
  TmpDot := FActiveMapping['.'];
  TmpComma := FActiveMapping[','];
  if JclFormatSettings.DecimalSeparator <> '.' then
  begin
    FActiveMapping['.'] := TmpComma;
    FActiveMapping[','] := TmpDot;
  end;
  try
    for Chr := #0 to #255 do
      if FActiveMapping[Chr] <> 0 then
        Include(Hdr.MappedChars, Chr);
    Stream.WriteBuffer(Hdr, SizeOf(Hdr));
    for Chr := #0 to #255 do
      if FActiveMapping[Chr] <> 0 then
        Stream.WriteBuffer(FActiveMapping[Chr], MapSize);
  finally
    if JclFormatSettings.DecimalSeparator <> '.' then
    begin
      FActiveMapping['.'] := TmpDot;
      FActiveMapping[','] := TmpComma;
    end;
  end;
end;

//=== { TJv7SegmentedLEDDigit } ==============================================

procedure TJv7SegmentedLEDDigit.EnableAllSegs;
begin
  inherited EnableAllSegs;
  UseColon := scuFull;
end;

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

class function TJv7SegmentedLEDDigit.SegmentCount: Integer;
begin
  Result := 10;
end;

class function TJv7SegmentedLEDDigit.GetSegmentName(Index: Integer): string;
begin
  if Index <= 7 then
    Result := inherited GetSegmentName(Index)
  else
  if Index = 8 then
    Result := 'CL'
  else
  if Index = 9 then
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
    else
    if Name = 'CH' then
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

class function TJv7SegmentedLEDDigit.MapperFileID: AnsiString;
begin
  Result := 'SLDCM_7SEG';
end;

procedure TJv7SegmentedLEDDigit.CalcCHSeg(Index: Integer);
var
  UpperLeftPoint: TPoint;
begin
  UpperLeftPoint := AngleAdjustPoint(FRefCenterX - DotSize div 2,
    (FRefCenterY - FRefTop) div 2 + FRefTop, SlantAngle);
  SetSegmentRenderInfo(Index, srtCircle,
    [UpperLeftPoint, Point(UpperLeftPoint.X + DotSize, UpperLeftPoint.Y + DotSize)]);
end;

procedure TJv7SegmentedLEDDigit.CalcCLSeg(Index: Integer);
var
  UpperLeftPoint: TPoint;
begin
  UpperLeftPoint := AngleAdjustPoint(FRefCenterX - DotSize div 2,
    (FRefBottom - FRefCenterY) div 2 + FRefCenterY - DotSize div 2, SlantAngle);
  SetSegmentRenderInfo(Index, srtCircle,
    [UpperLeftPoint, Point(UpperLeftPoint.X + DotSize, UpperLeftPoint.Y + DotSize)]);
end;

//=== { TJv14SegmentedLEDDigit } ==============================================

procedure TJv14SegmentedLEDDigit.CalcG1Seg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing div 2, FRefCenterY, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth div 2, FRefCenterY - SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2 - SegmentWidth div 4, FRefCenterY - SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2, FRefCenterY, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2 - SegmentWidth div 4, FRefCenterY + SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth div 2, FRefCenterY + SegmentWidth div 2, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcG2Seg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX + Spacing div 2, FRefCenterY, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing div 2 + SegmentWidth div 4, FRefCenterY - SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth div 2, FRefCenterY - SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2, FRefCenterY, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth div 2, FRefCenterY + SegmentWidth div 2, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing div 2 + SegmentWidth div 4, FRefCenterY + SegmentWidth div 2, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcHSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing + SegmentWidth, FRefTop + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing + SegmentWidth + SegmentWidth div 2, FRefTop + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing - SegmentWidth div 2, FRefCenterY - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing - SegmentWidth div 2, FRefCenterY - SegmentWidth div 2 - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing - SegmentWidth, FRefCenterY - SegmentWidth div 2 - Spacing, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing + SegmentWidth, FRefTop + SegmentWidth + SegmentWidth div 4 + Spacing, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcISeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefTop + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefCenterY - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX, FRefCenterY - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefCenterY - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefTop + SegmentWidth + Spacing, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcJSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefRight - Spacing - SegmentWidth, FRefTop + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing - SegmentWidth - SegmentWidth div 2, FRefTop + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing + SegmentWidth div 2, FRefCenterY - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing + SegmentWidth div 2, FRefCenterY - SegmentWidth div 2 - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing + SegmentWidth, FRefCenterY - SegmentWidth div 2 - Spacing, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing - SegmentWidth, FRefTop + SegmentWidth + SegmentWidth div 4 + Spacing, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcKSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing + SegmentWidth, FRefBottom - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing + SegmentWidth + SegmentWidth div 2, FRefBottom - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing - SegmentWidth div 2, FRefCenterY + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing - SegmentWidth div 2, FRefCenterY + SegmentWidth div 2 + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing - SegmentWidth, FRefCenterY + SegmentWidth div 2 + Spacing, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing + SegmentWidth, FRefBottom - SegmentWidth - SegmentWidth div 4 - Spacing, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcLSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefBottom - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefCenterY + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX, FRefCenterY + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefCenterY + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefBottom - SegmentWidth - Spacing, SlantAngle)
  ]);
end;

procedure TJv14SegmentedLEDDigit.CalcMSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefRight - Spacing - SegmentWidth, FRefBottom - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing - SegmentWidth - SegmentWidth div 2, FRefBottom - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing + SegmentWidth div 2, FRefCenterY + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing + SegmentWidth div 2, FRefCenterY + SegmentWidth div 2 + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing + SegmentWidth, FRefCenterY + SegmentWidth div 2 + Spacing, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing - SegmentWidth, FRefBottom - SegmentWidth - SegmentWidth div 4 - Spacing, SlantAngle)
  ]);
end;

class function TJv14SegmentedLEDDigit.GetSegmentIndex(Name: string): Integer;
begin
  Result := TextIndex(Name, ['A', 'B', 'C', 'D', 'E', 'F', 'G1', 'G2', 'H', 'I', 'J', 'K', 'L', 'M', 'DP']);
end;

class function TJv14SegmentedLEDDigit.GetSegmentName(Index: Integer): string;
begin
  if Index = 6 then
    Result := 'G1'
  else
  if Index = 7 then
    Result := 'G2'
  else
  if Index < 6 then
    Result := Chr(Ord('A') + Index)
  else
  if (Index > 7) and (Index < 14) then
    Result := Chr(Ord('A') + Index - 1)
  else
  if Index = 14 then
    Result := 'DP'
  else
    Result := '';
end;

class function TJv14SegmentedLEDDigit.MapperFileID: AnsiString;
begin
  Result := 'SLDCM_14SEG';
end;

procedure TJv14SegmentedLEDDigit.RecalcSegments;
begin
  CalcASeg(0);
  CalcBSeg(1);
  CalcCSeg(2);
  CalcDSeg(3);
  CalcESeg(4);
  CalcFSeg(5);
  CalcG1Seg(6);
  CalcG2Seg(7);
  CalcHSeg(8);
  CalcISeg(9);
  CalcJSeg(10);
  CalcKSeg(11);
  CalcLSeg(12);
  CalcMSeg(13);
  if UseDP then
    CalcDPSeg(14);
end;

class function TJv14SegmentedLEDDigit.SegmentCount: Integer;
begin
  Result := 15;
end;

//=== { TJv16SegmentedLEDDigit } ===============================================

const
  seg16Names: array[0..16] of string = (
    'A1', 'A2', 'B', 'C', 'D1', 'D2', 'E', 'F', 'G1', 'G2', 'H', 'I', 'J', 'K', 'L', 'M', 'DP');

procedure TJv16SegmentedLEDDigit.CalcA1Seg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing div 2, FRefTop, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2, FRefTop, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2 - SegmentWidth, FRefTop + SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth, FRefTop + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJv16SegmentedLEDDigit.CalcA2Seg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX + Spacing div 2, FRefTop, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2, FRefTop, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth, FRefTop + SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing div 2 + SegmentWidth, FRefTop + SegmentWidth, SlantAngle)
  ]);
end;

procedure TJv16SegmentedLEDDigit.CalcD1Seg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefLeft + Spacing div 2, FRefBottom, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2, FRefBottom, SlantAngle),
    AngleAdjustPoint(FRefCenterX - Spacing div 2 - SegmentWidth, FRefBottom - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefLeft + Spacing div 2 + SegmentWidth, FRefBottom - SegmentWidth, SlantAngle)
  ]);
end;

procedure TJv16SegmentedLEDDigit.CalcD2Seg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX + Spacing div 2, FRefBottom, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2, FRefBottom, SlantAngle),
    AngleAdjustPoint(FRefRight - Spacing div 2 - SegmentWidth, FRefBottom - SegmentWidth, SlantAngle),
    AngleAdjustPoint(FRefCenterX + Spacing div 2 + SegmentWidth, FRefBottom - SegmentWidth, SlantAngle)
  ]);
end;

procedure TJv16SegmentedLEDDigit.CalcISeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX, FRefTop + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefTop + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefCenterY - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX, FRefCenterY - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefCenterY - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefTop + SegmentWidth + Spacing, SlantAngle)
  ]);
end;

procedure TJv16SegmentedLEDDigit.CalcLSeg(Index: Integer);
begin
  SetSegmentRenderInfo(Index, srtPolygon, [
    AngleAdjustPoint(FRefCenterX, FRefBottom - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefBottom - SegmentWidth - Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX + SegmentWidth div 2, FRefCenterY + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX, FRefCenterY + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefCenterY + SegmentWidth + Spacing, SlantAngle),
    AngleAdjustPoint(FRefCenterX - SegmentWidth div 2, FRefBottom - SegmentWidth - Spacing, SlantAngle)
  ]);
end;

class function TJv16SegmentedLEDDigit.GetSegmentIndex(Name: string): Integer;
begin
  Result := TextIndex(Name, seg16Names);
end;

class function TJv16SegmentedLEDDigit.GetSegmentName(Index: Integer): string;
begin
  Result := seg16Names[Index];
end;

class function TJv16SegmentedLEDDigit.MapperFileID: AnsiString;
begin
  Result := 'SLDCM_16SEG';
end;

procedure TJv16SegmentedLEDDigit.RecalcSegments;
begin
  CalcA1Seg(0);
  CalcA2Seg(1);
  CalcBSeg(2);
  CalcCSeg(3);
  CalcD1Seg(4);
  CalcD2Seg(5);
  CalcESeg(6);
  CalcFSeg(7);
  CalcG1Seg(8);
  CalcG2Seg(9);
  CalcHSeg(10);
  CalcISeg(11);
  CalcJSeg(12);
  CalcKSeg(13);
  CalcLSeg(14);
  CalcMSeg(15);
  if UseDP then
    CalcDPSeg(16);
end;

class function TJv16SegmentedLEDDigit.SegmentCount: Integer;
begin
  Result := 17;
end;

//=== { initialization and support routines } =================================

procedure ModuleUnload(Instance: HINST);
begin
  UnregisterModuleSegmentedLEDDigitClasses(HMODULE(Instance));
end;

function IdentToUnlitColor(const Ident: string; var Int: Longint): Boolean;
begin
  Int := TextIndex(Ident, ['clDefaultBackground', 'clDefaultLitColor']);
  Result := Int > -1;
  if Result then
    Inc(Int, clDefaultBackground)
  else
    Result := IdentToColor(Ident, Int);
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
      Result := ColorToIdent(Int, Ident);
  end;
end;

function StringToUnlitColor(const S: string): TUnlitColor;
begin
  if not IdentToUnlitColor(S, Longint(Result)) then
    Result := StrToInt(S);
end;

function UnlitColorToString(const Color: TUnlitColor): string;
begin
  if not ColorToIdent(Color, Result) then
    Result := Format('%s%.8x', [HexDisplayPrefix, Color]);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  AddModuleUnloadProc(ModuleUnload);
  RegisterSegmentedLEDDigitClasses([TJv7SegmentedLEDDigit, TJv14SegmentedLEDDigit, TJv16SegmentedLEDDigit]);
  RegisterIntegerConsts(TypeInfo(TUnlitColor), IdentToUnlitColor, UnlitColorToIdent);

finalization
  UnregisterIntegerConsts(TypeInfo(TUnlitColor), IdentToUnlitColor, UnlitColorToIdent);
  UnregisterModuleSegmentedLEDDigitClasses(HInstance);
  FreeAndNil(GDigitClassList);
  RemoveModuleUnloadProc(ModuleUnload);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
