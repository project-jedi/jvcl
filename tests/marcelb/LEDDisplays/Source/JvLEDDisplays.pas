unit JvLEDDisplays;

interface

uses
  SysUtils, Windows, Classes, Graphics,
  JvComponent;


{
  7-Segment render code based on JDLed donated by Jay Dubal (http://delphisoft.topcities.com).
  Badly coded parts around it are written by me (Marcel Bestebroer) :-)

  
  7-segment: (displays 0-9, A-F, o, r and space; suitable for hexadecimals and the word Error;
              case insensitive
              new: added #248, ' and " to show degree, minutes and seconds
              new: added -
              new: added H, L and P, to display HELP
              new: Made Uppercase C and lowercase c)

        a
    ---------
   |         |
  f|         |b
   |         |
   |    g    |
    ---------
   |         |
  e|         |c
   |         |
   |         |
    ---------
        d


  14-segment: (displays space, 0-9, A-Z (uppercase only).
               new: added #0, #4 for alternative 0 and 4
               new: added #248, ' and " to show degrees, minutes and seconds
               new: added (, ), +, -, /, \, *
               changed: E and F now have a smaller middle bar)

        a
    ---------
   |\   |   /|
  f| \h |j /k|b
   |  \ | /  |
   | g1\|/g2 |
    ---- ----
   |   /|\   |
  e| n/ | \l |c
   | / m|  \ |
   |/   |   \|
    ---------
        d
           

  16-segment: (displays space, 0-9, A-Z (uppercase only).
               new: added #0, #4 for alternative 0 and 4
               new: added #248, ' and " to show degrees, minutes and seconds
               new: added (, ), +, -, /, \, *
               changed: E and F now have a smaller middle bar)

     a1   a2
    ---- ----
   |\   |   /|
  f| \h |j /k|b
   |  \ | /  |
   | g1\|/g2 |
    ---- ----
   |   /|\   |
  e| n/ | \l |c
   | / m|  \ |
   |/   |   \|
    ---- ----
     d1   d2

  see http://www.mitt-eget.com/displays/ for more display types to incorporate
}


type
  TJvCustomSegmentLEDDisplay = class;
  TJvSegmentLEDDigits = class;
  TJvSegmentLEDDigit = class;

  TJvSegmentLEDKind = (slk7, slk14, slk16);
  
  TJvCustomSegmentLEDDisplay = class(TJvGraphicControl)
  private
    FColorOn: TColor;
    FColorOff: TColor;
    FDigitHeight: Integer;
    FDigits: TList;
    FDigitWidth: Integer;
    FKind: TJvSegmentLEDKind;
    FSegmentWidth: Integer;
    FSpacing: Integer;
  protected
    function GetDigit(I: Integer): TJvSegmentLEDDigit;
    function GetDigitCount: Integer;
    function GetDigits: TJvSegmentLEDDigits;
    procedure SetDigitCount(Value: Integer);
    procedure SetDigits(Value: TJvSegmentLEDDigits);
    procedure SetKind(Value: TJvSegmentLEDKind);
    procedure SetColorOn(Value: TColor);
    procedure SetColorOff(Value: TColor);

    procedure Paint; override;

    property ColorOn: TColor read FColorOn write SetColorOn;
    property ColorOff: TColor read FColorOff write SetColorOff;
    property Digit[I: Integer]: TJvSegmentLEDDigit read GetDigit;
    property DigitCount: Integer read GetDigitCount write SetDigitCount;
    property DigitHeight: Integer read FDigitHeight write FDigitHeight;
    property Digits: TJvSegmentLEDDigits read GetDigits write SetDigits stored False;
    property DigitWidth: Integer read FDigitWidth write FDigitWidth;
    property Kind: TJvSegmentLEDKind read FKind write SetKind;
    property SegmentWidth: Integer read FSegmentWidth write FSegmentWidth;
    property Spacing: Integer read FSpacing write FSpacing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TJvSegmentLEDDisplay = class(TJvCustomSegmentLEDDisplay)
  public
    procedure DisplayString(S: string; const ResizeDisplay: Boolean = False);
    property Digit;
    property DigitCount;
    property DigitHeight;
    property DigitWidth;
    property SegmentWidth;
  published
    property Color;
    property ColorOn;
    property ColorOff;
    property Digits;
    property Kind;
  end;

  TJvSegmentLEDDigits = class(TPersistent)
  end;

  TJvSegmentLEDDigit = class(TPersistent)
  private
    FDigitIndex: Integer;
    FDisplay: TJvCustomSegmentLEDDisplay;
    FSegments: array of Boolean; // On state of each segment (where DP is the last segment in the array if it's used)
    FUseDP: Boolean;
  protected
    function GetSegments: string;
    procedure SetSegments(Value: string);
    function GetSegmentState(I: Integer): Boolean;
    procedure SetSegmentState(I: Integer; Value: Boolean);
    procedure SetUseDP(Value: Boolean);

    procedure Paint;

    procedure SetChar7(Ch: Char);
    procedure SetChar16(Ch: Char);

    property DigitIndex: Integer read FDigitIndex;
    property Display: TJvCustomSegmentLEDDisplay read FDisplay;
  public
    constructor Create(ADisplay: TJvCustomSegmentLEDDisplay; ADigitIndex: Integer);
    procedure SetChar(const Ch: Char);

    property SegmentState[I: Integer]: Boolean read GetSegmentState write SetSegmentState;
  published
    property Segments: string read GetSegments write SetSegments; // allows a binary string (0 or 1) or a string of segments that are on; always displays the segments that are on
    property UseDP: Boolean read FUseDP write SetUseDP;
  end;

implementation

uses
  contnrs;

function TextIndex(const Str: string; const Strings: array of string; const PartialAllowed: Boolean = False): Integer;
begin
  Result := High(Strings);
  while (Result > -1) and not AnsiSameText(Str, Strings[Result]) and (not PartialAllowed or not AnsiSameText(Str, Copy(Strings[Result], 1, Length(Str)))) do
    Dec(Result);
end;

function TJvCustomSegmentLEDDisplay.GetDigit(I: Integer): TJvSegmentLEDDigit;
begin
  Result := TJvSegmentLEDDigit(FDigits[I]);
end;

function TJvCustomSegmentLEDDisplay.GetDigitCount: Integer;
begin
  Result := FDigits.Count;
end;

function TJvCustomSegmentLEDDisplay.GetDigits: TJvSegmentLEDDigits;
begin
  Result := nil;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigitCount(Value: Integer);
begin
  if Value <> DigitCount then
  begin
    if Value < DigitCount then
      while Value < DigitCount do
        FDigits.Delete(DigitCount - 1)
    else
      while Value > DigitCount do
        FDigits.Add(TJvSegmentLEDDigit.Create(Self, DigitCount));
//    CountChanged;
    Width := DigitCount * DigitWidth;
    Height := DigitHeight;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigits(Value: TJvSegmentLEDDigits);
begin
  // nothing
end;

procedure TJvCustomSegmentLEDDisplay.SetKind(Value: TJvSegmentLEDKind);
begin
  if Value <> Kind then
  begin
    FKind := Value;
//    KindChanged;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetColorOn(Value: TColor);
begin
  if Value <> ColorOn then
  begin
    FColorOn := Value;
//    COlorOnChanged;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetColorOff(Value: TColor);
begin
  if Value <> ColorOff then
  begin
    FColorOff := Value;
//    COlorOffChanged;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.Paint;
var
  I: Integer;
begin
  for I := 0 to DigitCount - 1 do
    Digit[I].Paint;
end;

constructor TJvCustomSegmentLEDDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDigits := TObjectList.Create(True);
  FDigitHeight := 40;
  FDigitWidth := 24;
  FSpacing := 2;
  FSegmentWidth := 2; 
end;

destructor TJvCustomSegmentLEDDisplay.Destroy;
begin
  FDigits.Free;
  inherited Destroy;
end;

//===TJvSegmentLEDDisplay===========================================================================

procedure TJvSegmentLEDDisplay.DisplayString(S: string; const ResizeDisplay: Boolean);
var
  I: Integer;
begin
  if not ResizeDisplay then
  begin
    if Length(S) > DigitCount then
      Delete(S, DigitCount + 1, Length(S) - DigitCount)
  end
  else
    DigitCount := Length(S);
  for I := 0 to DigitCount - 1 do
    Digit[I].SetChar(S[I + 1]);
end;

//===TJvSegmentLEDDigit=============================================================================

const
  SegNames14: array[0..13] of string = (
    'A',  'B',  'C',  'D',  'E',  'F', 'G1',
    'G2', 'H',  'J',  'K',  'L',  'M',  'N'
  );
const
  SegNames16: array[0..15] of string = (
    'A1', 'A2', 'B',  'C',  'D1', 'D2', 'E',  'F',
    'G1', 'G2', 'H',  'J',  'K',  'L',  'M',  'N'
  );
  
function TJvSegmentLEDDigit.GetSegments: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(FSegments) do
  begin
    if FSegments[I] then
    begin
      if UseDP and (I = High(FSegments)) then
        Result := Result + ',DP'
      else
      begin
        case Display.Kind of
          slk7:
            Result := Result + ',' + Chr(I + Ord('A'));
          slk14:
            Result := Result + ',' + SegNames14[I];
          slk16:
            Result := Result + ',' + SegNames16[I];
        end;
      end;
    end;
  end;
  if Result <> '' then
    Delete(Result, 1, 1);
end;

procedure TJvSegmentLEDDigit.SetSegments(Value: string);
var
  IsBoolList: Boolean;
  I: Integer;
  SegIdx: Integer;
begin
  IsBoolList := (Value = '') or (Value[1] in ['0', '1']);
  FillChar(FSegments[0], SizeOf(FSegments[0]) * Length(FSegments), 0);
  if IsBoolList then
  begin
    for I := 1 to Length(Value) do
      FSegments[I - 1] := Value[I] = '1';
  end
  else
  begin
    I := 1;
    while (I <= Length(Value)) do
    begin
      if Display.Kind = slk14 then
      begin
        SegIdx := TextIndex(Value[I], SegNames14, False);
        if SegIdx < 0 then
          SegIdx := TextIndex(Copy(Value, I, 2), SegNames14, True);
        if SegIdx < 0 then
          SegIdx := TextIndex(Value[I], SegNames14, True);
        if (SegIdx < 0) and AnsiSameText(Copy(Value, I, 2), 'DP') then
          SegIdx := 14;
      end
      else
      if Display.Kind = slk16 then
      begin
        SegIdx := TextIndex(Value[I], SegNames16, False);
        if SegIdx < 0 then
          SegIdx := TextIndex(Copy(Value, I, 2), SegNames16, True);
        if SegIdx < 0 then
          SegIdx := TextIndex(Value[I], SegNames16, True);
        if (SegIdx < 0) and AnsiSameText(Copy(Value, I, 2), 'DP') then
          SegIdx := 16;
      end
      else
      begin
        if AnsiSameText(Copy(Value, I, 2), 'DP') then
          SegIdx := 7
        else
          SegIdx := Ord(UpCase(Value[I])) - Ord('A');
      end;
      if SegIdx < 0 then
        raise Exception.Create('Invalid string');
      FSegments[SegIdx] := True;
      if (Display.Kind = slk14) and ((SegIdx = 7) and not AnsiSameText(Copy(Value, I, 2), SegNames14[SegIdx])) then
        FSegments[SegIdx - 1] := True;
      if (Display.Kind = slk16) and ((SegIdx in [1, 5, 9]) and not AnsiSameText(Copy(Value, I, 2), SegNames16[SegIdx])) then
        FSegments[SegIdx - 1] := True;
      Inc(I);
      while (I <= Length(Value)) and (Value[I] in ['1', '2', ',', ';', 'P', #0 .. ' ']) do
        Inc(I);
    end;
  end;
//  Invalidate;
end;

function TJvSegmentLEDDigit.GetSegmentState(I: Integer): Boolean;
begin
  Result := FSegments[I];
end;

procedure TJvSegmentLEDDigit.SetSegmentState(I: Integer; Value: Boolean);
begin
  if Value <> FSegments[I] then
  begin
    FSegments[I] := Value;
//    Invalidate;
  end;
end;

procedure TJvSegmentLEDDigit.SetUseDP(Value: Boolean);
begin
  if Value <> UseDP then
  begin
    FUseDP := Value;
    if UseDP then
      SetLength(FSegments, Length(FSegments) + 1)
    else
      SetLength(FSegments, High(FSegments));
  end;
end;

procedure TJvSegmentLEDDigit.Paint;
var
  S,
  SX,
  X1,
  X2,
  X3,
  X4,
  X5,
  X6,
  X7,
  Z,
  Z1: Integer;
  Z2: Integer;

  procedure InitColor(StateOn: Boolean);
  begin
    with Display do
    begin
      if StateOn then
      begin
        Canvas.Brush.Color := ColorOn;
        Canvas.Pen.Color := ColorOn;
      end
      else
      begin
        Canvas.Brush.Color := ColorOff;
        Canvas.Pen.Color := ColorOff;
      end;
    end;
  end;
  
begin
  with Display do
  begin
    S := Spacing;
    X1 := DigitHeight - SegmentWidth - 2 - 2 * Spacing;
    X2 := DigitWidth - SegmentWidth - 2 - 2 * Spacing;
    X3 := SegmentWidth;
  end;
  SX := S + DigitIndex * Display.DigitWidth;
  Z := Abs(X3 div 2);
  Z1 := Abs(X1 div 2);
  Z2 := Abs(X2 div 2);
  X4 := Z1 - Z;
  X5 := Z1 + Z;
  X6 := Z1 - X3 - Z;
  X7 := Z1 + X3 + Z;

  with Display.Canvas do
  begin
    Brush.Color := Display.Color;
    Pen.Color := Display.Color;

    Rectangle(SX - S, 0, SX - S + Display.DigitWidth, Display.DigitHeight);

    if Display.Kind in [slk7, slk14] then
    begin
      InitColor(FSegments[0]);
      Polygon([Point(SX + Z, S), Point(SX + X2 - Z, S), Point(SX + X2 - X3 - Z, S + X3), Point(SX + X3 + Z, S + X3)]);
      InitColor(FSegments[1]);
      Polygon([Point(SX + X2 - X3, S + X3 + Z), Point(SX + X2, S + Z), Point(SX + X2, S + X4), Point(SX + X2 - X3, S + X6)]);
      InitColor(FSegments[2]);
      Polygon([Point(SX + X2 - X3, S + X7), Point(SX + X2, S + X5), Point(SX + X2, S + X1 - Z), Point(SX + X2 - X3, S + X1 - X3 - Z)]);
      InitColor(FSegments[3]);
      Polygon([Point(SX + X3 + Z, S + X1 - X3), Point(SX + X2 - X3 - Z, S + X1 - X3), Point(SX + X2 - Z, S + X1),
        Point(SX + X3 - Z, S + X1)]);
      InitColor(FSegments[4]);
      Polygon([Point(SX, S + X5), Point(SX + X3, S + X7), Point(SX + X3, S + X1 - X3 - Z), Point(SX, S + X1 - Z)]);
      InitColor(FSegments[5]);
      Polygon([Point(SX, S + Z),Point(SX + X3, S + X3 + Z), Point(SX + X3, S + X6), Point(SX, S + X4)]);
      InitColor(FSegments[6]);
      if Display.Kind = slk7 then
      begin
        Polygon([Point(SX + X3 - Abs(Z div 2), S + Z1), Point(SX + X3 + Z, S + Z1 - Z - Abs(Z div 4)),
          Point(SX + X2 - X3 - Z, S + Z1 - Z - Abs(Z div 4)), Point(SX + X2 - X3 + Abs(Z div 2), S + Z1)]);
        Polygon([Point(SX + X3 - Abs(Z div 2), S + Z1), Point(SX + X3 + Z, S + Z1 + Z + Abs(Z div 4)),
          Point(SX + X2 - X3 - Z, S + Z1 + Z + Abs(Z div 4)), Point(SX + X2 - X3 + Abs(Z div 2), S + Z1)]);
      end
      else
      begin
        InitColor(FSegments[6]);
        Polygon([Point(SX + X3 - Abs(Z div 2), S + Z1), Point(SX + X3 + Z, S + Z1 - Z - Abs(Z div 4)),
          Point(SX + Z2 - X3 - Z, S + Z1 - Z - Abs(Z div 4)), Point(SX + Z2 - X3 + Abs(Z div 2), S + Z1)]);
        Polygon([Point(SX + X3 - Abs(Z div 2), S + Z1), Point(SX + X3 + Z, S + Z1 + Z + Abs(Z div 4)),
          Point(SX + Z2 - X3 - Z, S + Z1 + Z + Abs(Z div 4)), Point(SX + Z2 - X3 + Abs(Z div 2), S + Z1)]);
        InitColor(FSegments[7]);
        Polygon([Point(SX + Z2 + X3 - Abs(Z div 2), S + Z1), Point(SX + Z2 + X3 + Z, S + Z1 - Z - Abs(Z div 4)),
          Point(SX + X2 - X3 - Z, S + Z1 - Z - Abs(Z div 4)), Point(SX + X2 - X3 + Abs(Z div 2), S + Z1)]);
        Polygon([Point(SX + Z2 + X3 - Abs(Z div 2), S + Z1), Point(SX + Z2 + X3 + Z, S + Z1 + Z + Abs(Z div 4)),
          Point(SX + X2 - X3 - Z, S + Z1 + Z + Abs(Z div 4)), Point(SX + X2 - X3 + Abs(Z div 2), S + Z1)]);
        InitColor(FSegments[8]);
        Polygon([
          Point(SX + X3 + Z, S + X3 + Z),
          Point(SX + X3 + X3 + Abs(Z div 2), S + X3 + Z),
          Point(SX + Z2 - X3 - Z, S + Z1 - X3 - X3 - Abs(Z div 2)),
          Point(SX + Z2 - X3 - Z, S + Z1 - X3 - Z),
          Point(SX + Z2 - X3 - X3 - Abs(Z div 2), S + Z1 - X3 - Z),
          Point(SX + X3 + Z, S + X3 + X3 + Abs(Z div 2))
        ]);
        InitColor(FSegments[9]);
        Polygon([
          Point(SX + Z2, S + X3 + Z),
          Point(SX + Z2 + X3 - Z, S + X3 + X3 {Abs(Z div 2)}),
          Point(SX + Z2 + X3 - Z, S + Z1 - X3),
          Point(SX + Z2, S + Z1 - X3 + Z),
          Point(Sx + Z2 - X3 + Z, S + Z1 - X3),
          Point(SX + Z2 - X3 + Z, S + X3 + X3{Abs(Z div 2)})
        ]);
        InitColor(FSegments[10]);
        Polygon([
          Point(SX + X2 - X3 - Z, S + X3 + Z),
          Point(SX + X2 - X3 - X3 - Abs(Z div 2), S + X3 + Z),
          Point(SX + Z2 + X3 + Z, S + Z1 - X3 - X3 - Abs(Z div 2)),
          Point(SX + Z2 + X3 + Z, S + Z1 - X3 - Z),
          Point(SX + Z2 + X3 + X3 + Abs(Z div 2), S + Z1 - X3 - Z),
          Point(SX + X2 - X3 - Z, S + X3 + X3 + Abs(Z div 2))
        ]);
        InitColor(FSegments[11]);
        Polygon([
          Point(SX + Z2 + X3 + Z, S + Z1 + X3 + Z),
          Point(SX + Z2 + X3 + X3 + Abs(Z div 2), S + Z1 + X3 + Z),
          Point(SX + X2 - X3 - Z, S + X1 - X3 - X3 - Abs(Z div 2)),
          Point(SX + X2 - X3 - Z, S + X1 - X3 - Z),
          Point(SX + X2 - X3 - X3 - Abs(Z div 2), S + X1 - X3 - Z),
          Point(SX + Z2 + X3 + Z, S + Z1 + X3 + X3 + Abs(Z div 2))
        ]);
        InitColor(FSegments[12]);
        Polygon([
          Point(SX + Z2, S + Z1),
          Point(SX + Z2 + X3 - Z, S + Z1 + X3 - Abs(Z div 2)),
          Point(SX + Z2 + X3 - Z, S + X1 - X3 - X3),
          Point(SX + Z2, S + Z1 + Z1 - X3 - Z),
          Point(Sx + Z2 - X3 + Z, S + X1 - X3 - X3),
          Point(SX + Z2 - X3 + Z, S + Z1 + X3 - Abs(Z div 2))
        ]);
        InitColor(FSegments[13]);
        Polygon([
          Point(SX + Z2 - X3 - Z, S + Z1 + X3 + Z),
          Point(SX + Z2 - X3 - X3 - Abs(Z div 2), S + Z1 + X3 + Z),

          Point(SX + X3 + Z, S + X1 - X3 - X3 - Abs(Z div 2)),
          Point(SX + X3 + Z, S + X1 - X3 - Z),
          Point(SX + X3 + X3 + Abs(Z div 2), S + X1 - X3 - Z),
          Point(SX + Z2 - X3 - Z, S + Z1 + X3 + X3 + Abs(Z div 2))
        ]);
      end;
    end
    else
    if Display.Kind = slk16 then
    begin
      InitColor(FSegments[0]);
      Polygon([Point(SX + Z, S), Point(SX + Z2 - Z, S), Point(SX + Z2 - X3 - Z, S + X3), Point(SX + X3 + Z, S + X3)]);
      InitColor(FSegments[1]);
      Polygon([Point(SX + Z2 + Z, S), Point(SX + X2 - Z, S), Point(SX + X2 - X3 - Z, S + X3), Point(SX + Z2 + X3 + Z, S + X3)]);
      InitColor(FSegments[2]);
      Polygon([Point(SX + X2 - X3, S + X3 + Z), Point(SX + X2, S + Z), Point(SX + X2, S + X4), Point(SX + X2 - X3, S + X6)]);
      InitColor(FSegments[3]);
      Polygon([Point(SX + X2 - X3, S + X7), Point(SX + X2, S + X5), Point(SX + X2, S + X1 - Z), Point(SX + X2 - X3, S + X1 - X3 - Z)]);
      InitColor(FSegments[4]);
      Polygon([Point(SX + X3 + Z, S + X1 - X3), Point(SX + Z2 - X3 - Z, S + X1 - X3), Point(SX + Z2 - Z, S + X1),
        Point(SX + X3 - Z, S + X1)]);
      InitColor(FSegments[5]);
      Polygon([Point(SX + Z2 + X3 + Z, S + X1 - X3), Point(SX + X2 - X3 - Z, S + X1 - X3), Point(SX + X2 - Z, S + X1),
        Point(SX + Z2 + X3 - Z, S + X1)]);
      InitColor(FSegments[6]);
      Polygon([Point(SX, S + X5), Point(SX + X3, S + X7), Point(SX + X3, S + X1 - X3 - Z), Point(SX, S + X1 - Z)]);
      InitColor(FSegments[7]);
      Polygon([Point(SX, S + Z),Point(SX + X3, S + X3 + Z), Point(SX + X3, S + X6), Point(SX, S + X4)]);
      InitColor(FSegments[8]);
      Polygon([Point(SX + X3 - Abs(Z div 2), S + Z1), Point(SX + X3 + Z, S + Z1 - Z - Abs(Z div 4)),
        Point(SX + Z2 - X3 - Z, S + Z1 - Z - Abs(Z div 4)), Point(SX + Z2 - X3 + Abs(Z div 2), S + Z1)]);
      Polygon([Point(SX + X3 - Abs(Z div 2), S + Z1), Point(SX + X3 + Z, S + Z1 + Z + Abs(Z div 4)),
        Point(SX + Z2 - X3 - Z, S + Z1 + Z + Abs(Z div 4)), Point(SX + Z2 - X3 + Abs(Z div 2), S + Z1)]);
      InitColor(FSegments[9]);
      Polygon([Point(SX + Z2 + X3 - Abs(Z div 2), S + Z1), Point(SX + Z2 + X3 + Z, S + Z1 - Z - Abs(Z div 4)),
        Point(SX + X2 - X3 - Z, S + Z1 - Z - Abs(Z div 4)), Point(SX + X2 - X3 + Abs(Z div 2), S + Z1)]);
      Polygon([Point(SX + Z2 + X3 - Abs(Z div 2), S + Z1), Point(SX + Z2 + X3 + Z, S + Z1 + Z + Abs(Z div 4)),
        Point(SX + X2 - X3 - Z, S + Z1 + Z + Abs(Z div 4)), Point(SX + X2 - X3 + Abs(Z div 2), S + Z1)]);
      InitColor(FSegments[10]);
      Polygon([
        Point(SX + X3 + Z, S + X3 + Z),
        Point(SX + X3 + X3 + Abs(Z div 2), S + X3 + Z),
        Point(SX + Z2 - X3 - Z, S + Z1 - X3 - X3 - Abs(Z div 2)),
        Point(SX + Z2 - X3 - Z, S + Z1 - X3 - Z),
        Point(SX + Z2 - X3 - X3 - Abs(Z div 2), S + Z1 - X3 - Z),
        Point(SX + X3 + Z, S + X3 + X3 + Abs(Z div 2))
      ]);
      InitColor(FSegments[11]);
      Polygon([
        Point(SX + Z2, S),
        Point(SX + Z2 + X3 - Z, S + X3 - Abs(Z div 2)),
        Point(SX + Z2 + X3 - Z, S + Z1 - X3),
        Point(SX + Z2, S + Z1 - X3 + Z),
        Point(Sx + Z2 - X3 + Z, S + Z1 - X3),
        Point(SX + Z2 - X3 + Z, S + X3 - Abs(Z div 2))
      ]);
      InitColor(FSegments[12]);
      Polygon([
        Point(SX + X2 - X3 - Z, S + X3 + Z),
        Point(SX + X2 - X3 - X3 - Abs(Z div 2), S + X3 + Z),
        Point(SX + Z2 + X3 + Z, S + Z1 - X3 - X3 - Abs(Z div 2)),
        Point(SX + Z2 + X3 + Z, S + Z1 - X3 - Z),
        Point(SX + Z2 + X3 + X3 + Abs(Z div 2), S + Z1 - X3 - Z),
        Point(SX + X2 - X3 - Z, S + X3 + X3 + Abs(Z div 2))
      ]);
      InitColor(FSegments[13]);
      Polygon([
        Point(SX + Z2 + X3 + Z, S + Z1 + X3 + Z),
        Point(SX + Z2 + X3 + X3 + Abs(Z div 2), S + Z1 + X3 + Z),
        Point(SX + X2 - X3 - Z, S + X1 - X3 - X3 - Abs(Z div 2)),
        Point(SX + X2 - X3 - Z, S + X1 - X3 - Z),
        Point(SX + X2 - X3 - X3 - Abs(Z div 2), S + X1 - X3 - Z),
        Point(SX + Z2 + X3 + Z, S + Z1 + X3 + X3 + Abs(Z div 2))
      ]);
      InitColor(FSegments[14]);
      Polygon([
        Point(SX + Z2, S + Z1),
        Point(SX + Z2 + X3 - Z, S + Z1 + X3 - Abs(Z div 2)),
        Point(SX + Z2 + X3 - Z, S + X1 - X3),
        Point(SX + Z2, S + Z1 + Z1 - X3 + Z),
        Point(Sx + Z2 - X3 + Z, S + X1 - X3),
        Point(SX + Z2 - X3 + Z, S + Z1 + X3 - Abs(Z div 2))
      ]);

      InitColor(FSegments[15]);
      Polygon([
        Point(SX + Z2 - X3 - Z, S + Z1 + X3 + Z),
        Point(SX + Z2 - X3 - X3 - Abs(Z div 2), S + Z1 + X3 + Z),

        Point(SX + X3 + Z, S + X1 - X3 - X3 - Abs(Z div 2)),
        Point(SX + X3 + Z, S + X1 - X3 - Z),
        Point(SX + X3 + X3 + Abs(Z div 2), S + X1 - X3 - Z),
        Point(SX + Z2 - X3 - Z, S + Z1 + X3 + X3 + Abs(Z div 2))
      ]);
    end;

    if UseDP then
    begin
      InitColor(FSegments[7 + 8 * Ord(Display.Kind=slk14) + 10 * Ord(Display.Kind=slk16)]);
      Ellipse(SX + X2 + 2, S + X1 - X3 + 2, SX + X2 + X3 + 4, S + X1 - X3 + X3 + 4);
    end;
  end;
end;

procedure TJvSegmentLEDDigit.SetChar7(Ch: Char);
begin
  if not (Upcase(Ch) in [' ', '0' .. '9', 'A' .. 'F', 'H', 'L', 'O', 'P', 'R', '''', '"', #248, '-']) then
    raise Exception.Create('Invalid character ''' + Ch + '''');
  FSegments[0] := (Upcase(Ch) in ['0', '2', '3', '5' .. '9', 'A', 'E', 'F', 'P', #248]) or (Ch = 'C'); // a
  FSegments[1] := UpCase(Ch) in ['0' .. '4', '7' .. '9', 'A', 'D', 'H', 'P', '"', #248]; // b
  FSegments[2] := UpCase(Ch) in ['0', '1', '3' .. '9', 'A', 'B', 'D', 'H', 'O']; // c
  FSegments[3] := UpCase(Ch) in ['0', '2', '3', '5', '6', '8', '9', 'B' .. 'E', 'L', 'O']; // d
  FSegments[4] := UpCase(Ch) in ['0', '2', '6', '8', 'A' .. 'F', 'H', 'L', 'O', 'P', 'R']; // e
  FSegments[5] := (UpCase(Ch) in ['0', '4' .. '6', '8', '9', 'A', 'B', 'E', 'F', 'H', 'L', 'P', '''', '"', #248]) or (Ch = 'C'); // f
  FSegments[6] := Ch in ['2' .. '6', '8', '9', 'A', 'B', 'D' .. 'F', 'a' .. 'f', 'H', 'h', 'O', 'o', 'P', 'p', 'R', 'r', #248, '-']; // g
end;

const
  ChMapToSegStr: array[#0.. #255] of string = (
    'ABCDEFKN', '0',        '0',        '0',        'FGJM',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '',         '0',        'BJ',
    '0',        '0',        '0',        '0',        'J',
    'KL',       'HN',       'GHJKLMN',  'GJM',      '0',
    'G',        '0',        'KN',       'ABCDEF',   'BC',
    'ABDEG',    'ABCDG',    'BCFG',     'ACDFG',    'ACDEFG',
    'ABC',      'ABCDEFG',  'ABCDFG',   '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    'ABCEFG',   'ABCDG2JM', 'ADEF',     'ABCDJM',   'ADEFG1',
    'AEFG1',     'ACDEFG2',  'BCEFG',    'ADJM',     'ABCDE',
    'EFG1KL',   'EFD',      'BCEFHK',   'BCEFHL',   'ABCDEF',
    'ABEFG',    'ABCDEFL',  'ABEFGL',   'ACDFG',    'AJM',
    'BCDEF',    'EFKN',     'BCEFLN',   'HKLN',     'HKM',
    'ADKN',     '0',        'HL',       '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        '0',        '0',
    '0',        '0',        '0',        'A2BG2J',   '0',
    '0',        '0',        '0',        '0',        '0',
    '0'
  );
  
procedure TJvSegmentLEDDigit.SetChar16(Ch: Char);
begin
  if ChMapToSegStr[Ch] = '0' then
    raise Exception.Create('Invalid character')
  else
    Segments := ChMapToSegStr[Ch];
end;

constructor TJvSegmentLEDDigit.Create(ADisplay: TJvCustomSegmentLEDDisplay; ADigitIndex: Integer);
begin
  inherited Create;
  FDigitIndex := ADigitIndex;
  FDisplay := ADisplay;
  SetLength(FSegments, 7 + 8 * Ord(Display.Kind = slk14) + 10 * Ord(Display.Kind = slk16));
end;

procedure TJvSegmentLEDDigit.SetChar(const Ch: Char);
begin
{  if Ch < ' ' then
    raise Exception.Create('Invalid character');}
  if Display.Kind = slk7 then
    SetChar7(Ch)
  else
    SetChar16(Ch);
end;

end.
