unit JvLEDDisplays;

interface

uses
  SysUtils, Windows, Classes, Graphics, Messages,
  JvComponent;


(*
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
               changed: E and F now have a smaller middle bar
               new: added [, ], {, })

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
               changed: E and F now have a smaller middle bar
               new: added [, ], {, })

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
*)


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
    FImg: TBitmap;
    FKind: TJvSegmentLEDKind;
    FSegmentWidth: Integer;
    FMargin: Integer;
    FSpacing: Integer;
    FRecalcPolygons: Boolean;
    FSlantAngle: Integer;
  protected
    function GetDigit(I: Integer): TJvSegmentLEDDigit;
    function GetDigitCount: Integer;
    function GetDigits: TJvSegmentLEDDigits;
    procedure SetColorOn(Value: TColor);
    procedure SetColorOff(Value: TColor);
    procedure SetDigitCount(Value: Integer);
    procedure SetDigitHeight(Value: Integer);
    procedure SetDigits(Value: TJvSegmentLEDDigits);
    procedure SetDigitWidth(Value: Integer);
    procedure SetKind(Value: TJvSegmentLEDKind);
    procedure SetMargin(Value: Integer);
    procedure SetSegmentWidth(Value: Integer);
    procedure SetSlantAngle(Value: Integer);
    procedure SetSpacing(Value: Integer);

    procedure Paint; override;
    procedure InvalidatePolygons;

    procedure WMEraseBkGnd(var M: TMessage); message WM_ERASEBKGND;

    property ColorOn: TColor read FColorOn write SetColorOn;
    property ColorOff: TColor read FColorOff write SetColorOff;
    property Digit[I: Integer]: TJvSegmentLEDDigit read GetDigit;
    property DigitCount: Integer read GetDigitCount write SetDigitCount;
    property DigitHeight: Integer read FDigitHeight write SetDigitHeight;
    property Digits: TJvSegmentLEDDigits read GetDigits write SetDigits stored False;
    property DigitWidth: Integer read FDigitWidth write SetDigitWidth;
    property Kind: TJvSegmentLEDKind read FKind write SetKind;
    property Margin: Integer read FMargin write SetMargin;
    property SegmentWidth: Integer read FSegmentWidth write FSegmentWidth;
    property SlantAngle: Integer read FSlantAngle write SetSlantAngle;
    property Spacing: Integer read FSpacing write SetSpacing;
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
    property Margin;
    property SlantAngle;
    property Spacing;
  end;

  TJvSegmentLEDDigits = class(TPersistent)
  end;

  TJvSegmentLEDDigit = class(TPersistent)
  private
    FDigitIndex: Integer;
    FDisplay: TJvCustomSegmentLEDDisplay;
    FSegments: array of Boolean; // On state of each segment (where DP is the last segment in the array if it's used)
    FUseDP: Boolean;
    FPolygons: array of array of TPoint;
  protected
    function GetSegments: string;
    procedure SetSegments(Value: string);
    function GetSegmentState(I: Integer): Boolean;
    procedure SetSegmentState(I: Integer; Value: Boolean);
    procedure SetUseDP(Value: Boolean);

    procedure CalcPolygons;
    procedure Paint(ACanvas: TCanvas);

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
  Controls, contnrs, math;

const
  MapChToSeg7: string =
    ' =|0=ABCDEF|1=BC|2=ABDEG|3=ABCDG|4=BCFG|5=ACDFG|6=ACDEFG|7=ABC|8=ABCDEFG|9=ABCDFG|' +
    'A=ABCEFG|a=ABCEFG|B=CDEFG|b=CDEFG|C=ADEF|c=DEG|D=BCDEG|d=BCDEG|E=ADEFG|e=ADEFG|' +
    'F=AEFG|f=AEFG|H=BCEFG|h=CEFG|L=DEF|l=DEF|O=CDEG|o=CDEG|P=ABEFG|p=ABEFG|R=EG|r=EG|' +
    '''=F|"=BF|'#248'=ABFG|-=G';

  MapChToSeg16: string =
    ' =|0=ABCDEF|1=BC|2=ABDEG|3=ABCDG|4=BCFG|5=ACDFG|6=ACDEFG|7=ABC|8=ABCDEFG|9=ABCDFG|' +
    #0'=ABCDEFKN|'#4'=FGJM|''=J|"=BJ|'#248'=A2BJG2|-=G|+=GJM|(=KL|)=HN|*=GHJKLN|/=KN|\=HL|' +
    'A=ABCEFG|B=ABCDG2JM|C=ADEF|D=ABCDJM|E=ADEFG1|F=AEFG1|G=ACDEFG2|H=BCEFG|I=ADJM|J=ABCDE|' +
    'K=EFG1KL|L=EFD|M=BCEFHK|N=BCEFHL|O=ABCDEF|P=ABEFG|Q=ABCDEFL|R=ABEFGL|S=ACDFG|T=AJM|' +
    'U=BCDEF|V=EFKN|W=BCEFLN|X=HKLN|Y=HKM|Z=ADKN|[=A2D2JM|]=A1D1JM|{=A2D2G1JM|}=A1D1G2JM';

  SegNames14: array[0..13] of string = (
    'A',  'B',  'C',  'D',  'E',  'F', 'G1',
    'G2', 'H',  'J',  'K',  'L',  'M',  'N'
  );

  SegNames16: array[0..15] of string = (
    'A1', 'A2', 'B',  'C',  'D1', 'D2', 'E',  'F',
    'G1', 'G2', 'H',  'J',  'K',  'L',  'M',  'N'
  );
  
function TextIndex(const Str: string; const Strings: array of string; const PartialAllowed: Boolean = False): Integer;
begin
  Result := High(Strings);
  while (Result > -1) and not AnsiSameText(Str, Strings[Result]) and (not PartialAllowed or not AnsiSameText(Str, Copy(Strings[Result], 1, Length(Str)))) do
    Dec(Result);
end;

function CharToSegString(const Ch: Char; const SegData: string): string;
var
  IStart: Integer;
  IEnd: Integer;
begin
  IStart := Pos(Ch + '=', SegData);
  if IStart = 0 then
    raise Exception.Create('Invalid character ''' + Ch + '''');
  IEnd := IStart + 2;
  While (IEnd <= Length(SegData)) and (SegData[IEnd] <> '|') do
    Inc(IEnd);
  Result := Copy(SegData, IStart + 2, IEnd - IStart - 2);
end;

function SmallPointEqual(const P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
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
    ClientWidth := DigitCount * DigitWidth;
    ClientHeight := DigitHeight;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigitHeight(Value: Integer);
begin
  if Value <> DigitHeight then
  begin
    FDigitHeight := Value;
    InvalidatePolygons;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigits(Value: TJvSegmentLEDDigits);
begin
  // nothing
end;

procedure TJvCustomSegmentLEDDisplay.SetDigitWidth(Value: Integer);
begin
  if Value <> DigitWidth then
  begin
    FDigitWidth := Value;
    InvalidatePolygons;
    Invalidate;
  end;
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

procedure TJvCustomSegmentLEDDisplay.SetMargin(Value: Integer);
begin
  if Value <> Margin then
  begin
    FMargin := Value;
    InvalidatePolygons;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetSegmentWidth(Value: Integer);
begin
  if Value <> SegmentWidth then
  begin
    FSegmentWidth := Value;
    InvalidatePolygons;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetSlantAngle(Value: Integer);
begin
  if Value <> SlantAngle then
  begin
    FSlantAngle := Value;
    InvalidatePolygons;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetSpacing(Value: Integer);
begin
  if Value <> Spacing then
  begin
    FSpacing := Value;
    InvalidatePolygons;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.InvalidatePolygons;
begin
  FRecalcPolygons := True;
end;

procedure TJvCustomSegmentLEDDisplay.Paint;
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    if FRecalcPolygons then
    begin
      FRecalcPolygons := False;
      for I := DigitCount - 1 downto 0 do
        Digit[I].CalcPolygons;
    end;
    FImg.Handle := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
    with FImg.Canvas do
    begin
      Brush.Color := Color;
      Pen.Color := Color;
      Rectangle(0, 0, ClientWidth + 1, ClientHeight + 1);
    end;
    for I := 0 to DigitCount - 1 do
      Digit[I].Paint(FImg.Canvas);
    BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FImg.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TJvCustomSegmentLEDDisplay.WMEraseBkGnd(var M: TMessage);
begin
  M.Result := 1;
end;

constructor TJvCustomSegmentLEDDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FDigits := TObjectList.Create(True);
  FDigitHeight := 40;
  FDigitWidth := 24;
  FMargin := 4;
  FSpacing := 2;
  FSegmentWidth := 2;
  FImg := TBitmap.Create;
end;

destructor TJvCustomSegmentLEDDisplay.Destroy;
begin
  FDigits.Free;
  FImg.Free;
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
  if Length(S) < DigitCount then
    S := S + StringOfChar(' ', DigitCount - Length(S));
  for I := 0 to DigitCount - 1 do
    Digit[I].SetChar(S[I + 1]);
  Invalidate;
end;

//===TJvSegmentLEDDigit=============================================================================

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

procedure TJvSegmentLEDDigit.CalcPolygons;
var
  SegT: Integer;
  Spc: Integer;
  DigM: Integer;
  DigX: Integer;
  DigW: Integer;
  DigH: Integer;
  SlantDiff: Integer;
  HalfSegT: Integer;
  HalfSpc: Integer;

  TopLeftX: Integer;
  TopLeftY: Integer;
  TopCenterX: Integer;
  TopCenterY: Integer;
  TopRightX: Integer;
  TopRightY: Integer;

  CenterLeftX: Integer;
  CenterLeftY: Integer;
  CenterCenterX: Integer;
  CenterCenterY: Integer;
  CenterRightX: Integer;
  CenterRightY: Integer;

  BottomLeftX: Integer;
  BottomLeftY: Integer;
  BottomCenterX: Integer;
  BottomCenterY: Integer;
  BottomRightX: Integer;
  BottomRightY: Integer;

  VertOffset: Integer;

  procedure SetPolygon(SegNum: Integer; Points: array of TPoint);
  begin
    SetLength(FPolygons[SegNum], Length(Points));
    Move(Points[0], FPolygons[SegNum][0], Length(Points) * SizeOf(TPoint));
  end;

begin
  with Display do
  begin
    SegT := SegmentWidth;
    Spc := Spacing;
    DigM := Margin;
    DigX := DigitIndex * DigitWidth;
    DigW := DigitWidth - 2 * DigM - SegT - 2;
    DigH := DigitHeight - 2 * DigM - SegT - 2;
    SlantDiff := Trunc(Abs(ArcTan(SlantAngle / 180.0 * Pi) * DigH));
    Dec(DigW, SlantDiff);
  end;
  HalfSegT := SegT div 2;
  HalfSpc := Spc div 2;

  TopLeftX := DigX + DigM + SlantDiff;
  TopLeftY := DigM;
  TopCenterX := TopLeftX + (DigW div 2);
  TopCenterY := TopLeftY;
  TopRightX := TopLeftX + DigW;
  TopRightY := TopLeftY;

  CenterLeftX := DigX + DigM + SlantDiff div 2;
  CenterLeftY := DigM + (DigH div 2);
  CenterCenterX := CenterLeftX + (DigW div 2);
  CenterCenterY := CenterLeftY;
  CenterRightX := CenterLeftX + DigW;
  CenterRightY := CenterLeftY;

  BottomLeftX := DigX + DigM;
  BottomLeftY := DigM + DigH;
  BottomCenterX := BottomLeftX + (DigW div 2);
  BottomCenterY := BottomLeftY;
  BottomRightX := BottomLeftX + DigW;
  BottomRightY := BottomLeftY;

  { J and M segments are special cases. 14 segment displays have them starting just below/above
    the A/D segments, whereas 16 segment displays have them starting at the same height as the
    two A/D segments.}
  if Display.Kind = slk16 then
    VertOffset := HalfSpc
  else
    VertOffset := SegT + HalfSpc;

  // Calculate and remember the polygons.
  SetLength(FPolygons, 19);
  SetPolygon(0, [ // A
    Point(TopLeftX + HalfSpc, TopLeftY),
    Point(TopRightX - HalfSpc, TopRightY),
    Point(TopRightX - HalfSpc - SegT, TopRightY + SegT),
    Point(TopLeftX + HalfSpc + SegT, TopLeftY + SegT)
  ]);
  SetPolygon(1, [ // A1
    Point(TopLeftX + HalfSpc, TopLeftY),
    Point(TopCenterX - HalfSpc, TopCenterY),
    Point(TopCenterX - HalfSpc - SegT, TopCenterY + SegT),
    Point(TopLeftX + HalfSpc + SegT, TopLeftY + SegT)
  ]);
  SetPolygon(2, [ // A2
    Point(TopCenterX + HalfSpc, TopCenterY),
    Point(TopRightX - HalfSpc, TopRightY),
    Point(TopRightX - HalfSpc - SegT, TopRightY + SegT),
    Point(TopCenterX + HalfSpc + SegT, TopCenterY + SegT)
  ]);
  SetPolygon(3, [ // B
    Point(TopRightX, TopRightY + HalfSpc),
    Point(CenterRightX, CenterRightY - HalfSpc),
    Point(CenterRightX - SegT, CenterRightY - HalfSpc - SegT),
    Point(TopRightX - SegT, TopRightY + HalfSpc + SegT)
  ]);
  SetPolygon(4, [ // C
    Point(CenterRightX, CenterRightY + HalfSpc),
    Point(BottomRightX, BottomRightY - HalfSpc),
    Point(BottomRightX - SegT, BottomRightY - HalfSpc - SegT),
    Point(CenterRightX - SegT, CenterRightY + HalfSpc + SegT)
  ]);
  SetPolygon(5, [ // D
    Point(BottomLeftX + HalfSpc, BottomLeftY),
    Point(BottomRightX - HalfSpc, BottomRightY),
    Point(BottomRightX - HalfSpc - SegT, BottomRightY - SegT),
    Point(BottomLeftX + HalfSpc + SegT, BottomLeftY - SegT)
  ]);
  SetPolygon(6, [ // D1
    Point(BottomLeftX + HalfSpc, BottomLeftY),
    Point(BottomCenterX - HalfSpc, BottomCenterY),
    Point(BottomCenterX - HalfSpc - SegT, BottomCenterY - SegT),
    Point(BottomLeftX + HalfSpc + SegT, BottomLeftY - SegT)
  ]);
  SetPolygon(7, [ // D2
    Point(BottomCenterX + HalfSpc, BottomCenterY),
    Point(BottomRightX - HalfSpc, BottomRightY),
    Point(BottomRightX - HalfSpc - SegT, BottomRightY - SegT),
    Point(BottomCenterX + HalfSpc + SegT, BottomCenterY - SegT)
  ]);
  SetPolygon(9, [ // F
    Point(TopLeftX, TopLeftY + HalfSpc),
    Point(CenterLeftX, CenterLeftY - HalfSpc),
    Point(CenterLeftX + SegT, CenterLeftY - HalfSpc - SegT),
    Point(TopLeftX + SegT, TopLeftY + HalfSpc + SegT)
  ]);
  SetPolygon(8, [ // E
    Point(CenterLeftX, CenterLeftY + HalfSpc),
    Point(BottomLeftX, BottomLeftY - HalfSpc),
    Point(BottomLeftX + SegT, BottomLeftY - HalfSpc - SegT),
    Point(CenterLeftX + SegT, CenterLeftY + HalfSpc + SegT)
  ]);
  SetPolygon(10, [ // G
    Point(CenterLeftX + HalfSpc, CenterLeftY),
    Point(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY - HalfSegT),
    Point(CenterRightX - HalfSpc - HalfSegT, CenterRightY - HalfSegT),
    Point(CenterRightX - HalfSpc, CenterRightY),
    Point(CenterRightX - HalfSpc - HalfSegT, CenterRightY + HalfSegT),
    Point(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY + HalfSegT)
  ]);
  SetPolygon(11, [ // G1
    Point(CenterLeftX + HalfSpc, CenterLeftY),
    Point(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY - HalfSegT),
    Point(CenterCenterX - HalfSpc - HalfSegT, CenterCenterY - HalfSegT),
    Point(CenterCenterX - HalfSpc, CenterCenterY),
    Point(CenterCenterX - HalfSpc - HalfSegT, CenterCenterY + HalfSegT),
    Point(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY + HalfSegT)
  ]);
  SetPolygon(12, [ // G2
    Point(CenterCenterX + HalfSpc, CenterCenterY),
    Point(CenterCenterX + HalfSpc + HalfSegT, CenterCenterY - HalfSegT),
    Point(CenterRightX - HalfSpc - HalfSegT, CenterRightY - HalfSegT),
    Point(CenterRightX - HalfSpc, CenterRightY),
    Point(CenterRightX - HalfSpc - HalfSegT, CenterRightY + HalfSegT),
    Point(CenterCenterX + HalfSpc + HalfSegT, CenterCenterY + HalfSegT)
  ]);
  SetPolygon(13, [ // H
    Point(TopLeftX + SegT + HalfSpc, TopLeftY + SegT + HalfSpc),
    Point(TopLeftX + SegT + HalfSpc + HalfSegT, TopLeftY + SegT + HalfSpc),
    Point(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY - SegT - HalfSpc),
    Point(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY - HalfSegT - HalfSpc),
    Point(CenterCenterX - SegT - HalfSpc, CenterCenterY - HalfSegT - HalfSpc),
    Point(TopLeftX + SegT + HalfSpc, TopLeftY + SegT + HalfSpc + HalfSegT)
  ]);
  SetPolygon(14, [ // J
    Point(TopCenterX, TopCenterY + VertOffset),
    Point(TopCenterX + HalfSegT, TopCenterY + VertOffset + HalfSegT),
    Point(CenterCenterX + HalfSegT, CenterCenterY - HalfSpc - HalfSegT),
    Point(CenterCenterX, CenterCenterY - HalfSpc),
    Point(CenterCenterX - HalfSegT, CenterCenterY - HalfSpc - HalfSegT),
    Point(TopCenterX - HalfSegT, TopCenterY + VertOffset + HalfSegT)
  ]);
  SetPolygon(15, [ // K
    Point(TopRightX - SegT - HalfSpc, TopRightY + SegT + HalfSpc),
    Point(TopRightX - SegT - HalfSpc - HalfSegT, TopRightY + SegT + HalfSpc),
    Point(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY - SegT - HalfSpc),
    Point(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY - HalfSegT - HalfSpc),
    Point(CenterCenterX + SegT + HalfSpc, CenterCenterY - HalfSegT - HalfSpc),
    Point(TopRightX - SegT - HalfSpc, TopRightY + SegT + HalfSpc + HalfSegT)
  ]);
  SetPolygon(16, [ // K
    Point(BottomRightX - SegT - HalfSpc, BottomRightY - SegT - HalfSpc),
    Point(BottomRightX - SegT - HalfSpc - HalfSegT, BottomRightY - SegT - HalfSpc),
    Point(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY + SegT + HalfSpc),
    Point(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY + HalfSegT + HalfSpc),
    Point(CenterCenterX + SegT + HalfSpc, CenterCenterY + HalfSegT + HalfSpc),
    Point(BottomRightX - SegT - HalfSpc, BottomRightY - SegT - HalfSpc - HalfSegT)
  ]);
  SetPolygon(17, [ // M
    Point(CenterCenterX, CenterCenterY + HalfSpc),
    Point(CenterCenterX + HalfSegT, CenterCenterY + HalfSpc + HalfSegT),
    Point(BottomCenterX + HalfSegT, BottomCenterY - VertOffset - HalfSegT),
    Point(BottomCenterX, BottomCenterY - VertOffset),
    Point(BottomCenterX - HalfSegT, BottomCenterY - VertOffset - HalfSegT),
    Point(CenterCenterX - HalfSegT, CenterCenterY + HalfSpc + HalfSegT)
  ]);
  SetPolygon(18, [ // N
    Point(BottomLeftX + SegT + HalfSpc, BottomLeftY - SegT - HalfSpc),
    Point(BottomLeftX + SegT + HalfSpc + HalfSegT, BottomLeftY - SegT - HalfSpc),
    Point(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY + SegT + HalfSpc),
    Point(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY + HalfSegT + HalfSpc),
    Point(CenterCenterX - SegT - HalfSpc, CenterCenterY + HalfSegT + HalfSpc),
    Point(BottomLeftX + SegT + HalfSpc, BottomLeftY - SegT - HalfSpc - HalfSegT)
  ]);
end;

procedure TJvSegmentLEDDigit.Paint(ACanvas: TCanvas);
var
  I: Integer;

  function ColorForState(SegIsOn: Boolean): TColor;
  begin
    if SegIsOn then
      Result := Display.ColorOn
    else
      Result := Display.ColorOff;
  end;

  procedure RenderSegment(SegName: string; const AColor: TColor);
  var
    SegIdx: Integer;
  begin
    SegIdx := Pos(UpperCase(Trim(SegName)), ' A A1A2B C D D1D2E F G G1G2H J K L M N') div 2;
    if SegIdx = 0 then
      raise Exception.Create('Illegal segment ''' + SegName + '''');
    if Length(FPolygons[SegIdx - 1]) > 1 then
    begin
      with ACanvas do
      begin
        Brush.Color := AColor;
        Pen.Color := AColor;
        Polygon(FPolygons[SegIdx - 1]);
      end;
    end;
  end;

begin
  case Display.Kind of
    slk7:
      begin
        for I := 6 downto 0 do
          RenderSegment(Chr(Ord('A') + I), ColorForState(FSegments[I]));
      end;
    slk14:
      begin
        for I := 13 downto 0 do
          RenderSegment(SegNames14[I], ColorForState(FSegments[I]));
      end;
    slk16:
      begin
        for I := 15 downto 0 do
          RenderSegment(SegNames16[I], ColorForState(FSegments[I]));
      end;
  end;
end;

procedure TJvSegmentLEDDigit.SetChar7(Ch: Char);
begin
  Segments := CharToSegString(Ch, MapChToSeg7);
end;

procedure TJvSegmentLEDDigit.SetChar16(Ch: Char);
begin
  Segments := CharToSegString(Ch, MapChToSeg16);
end;

constructor TJvSegmentLEDDigit.Create(ADisplay: TJvCustomSegmentLEDDisplay; ADigitIndex: Integer);
begin
  inherited Create;
  FDigitIndex := ADigitIndex;
  FDisplay := ADisplay;
  SetLength(FSegments, 7 + 8 * Ord(Display.Kind = slk14) + 10 * Ord(Display.Kind = slk16));
  CalcPolygons;
end;

procedure TJvSegmentLEDDigit.SetChar(const Ch: Char);
begin
  if Display.Kind = slk7 then
    SetChar7(Ch)
  else
    SetChar16(Ch);
end;

end.
