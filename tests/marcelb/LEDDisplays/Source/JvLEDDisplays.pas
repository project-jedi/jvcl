{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLEDDisplays.PAS, released on 2003-03-18.

The Initial Developer of the Original Code is Marcel Bestebroer [marcelb@zeelandnet.nl]
Portions created by Marcel Bestebroer are Copyright (C) 2003 Marcel Bestebroer.
All Rights Reserved.

Contributor(s):
  Jay Dubal [jaydubal@hotmail.com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvLEDDisplays;

interface

uses
  SysUtils, Windows, Classes, Graphics, Messages,
  JclBase, 
  JvComponent, JvTypes;


(* Comments to be removed later (when the help is done <g>):

  7-Segment render code used to be based on JDLed donated by Jay Dubal
  (http://delphisoft.topcities.com), but I rewrote it to allow slanted displays as well.


  7-segment: (displays 0-9, A-F, o, r and space; suitable for hexadecimals and the word Error;
              case insensitive
              new: added #248, ' and " to show degree, minutes and seconds
              new: added -
              new: added H, L and P, to display HELP
              new: Made Uppercase C and lowercase c)
              new: added U, u and y

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

  TJvSegmentLEDKind = (slk7Segments, slk14Segments, slk16Segments);

  EJvLEDDisplay = class(EJVCLException);

  TJvCustomSegmentLEDDisplay = class(TJvGraphicControl)
  private
    FAutoSize: Boolean;
    FColorOn: TColor;
    FColorOff: TColor;
    FDigitHeight: Integer;
    FDigits: TJvSegmentLEDDigits;
    FDigitWidth: Integer;
    FKind: TJvSegmentLEDKind;
    FSegmentWidth: Integer;
    FMargin: Integer;
    FSpacing: Integer;
    FRecalcPolygons: Boolean;
    FSlantAngle: Integer;
    FText: string;
  protected
    function GetDigit(I: Integer): TJvSegmentLEDDigit;
    function GetDigitCount: Integer;
    procedure SetAutoSize(Value: Boolean);
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
    procedure SetText(Value: string);
    procedure Paint; override;
    procedure InvalidatePolygons;
    procedure InvalidateSize;
    procedure WMEraseBkGnd(var M: TMessage); message WM_ERASEBKGND;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property ColorOn: TColor read FColorOn write SetColorOn;
    property ColorOff: TColor read FColorOff write SetColorOff;
    property Digits: TJvSegmentLEDDigits read FDigits write SetDigits;
    property DigitCount: Integer read GetDigitCount write SetDigitCount;
    property DigitHeight: Integer read FDigitHeight write SetDigitHeight;
    property DigitWidth: Integer read FDigitWidth write SetDigitWidth;
    property Kind: TJvSegmentLEDKind read FKind write SetKind;
    property Margin: Integer read FMargin write SetMargin;
    property SegmentWidth: Integer read FSegmentWidth write SetSegmentWidth;
    property SlantAngle: Integer read FSlantAngle write SetSlantAngle;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Text: string read FText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisplayString(S: string; const ResizeDisplay: Boolean = False);
    function DigitStringToSegments(S: string): TDynStringArray;
  published
  end;

  TJvSegmentLEDDisplay = class(TJvCustomSegmentLEDDisplay)
  public
  published
    property AutoSize;
    property Color;
    property ColorOn;
    property ColorOff;
    property DigitCount;
    property DigitHeight;
    property Digits;
    property DigitWidth;
    property Kind;
    property Margin;
    property SegmentWidth;
    property SlantAngle;
    property Spacing;
    property Text;
  end;

  TJvSegmentLEDDigits = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TJvSegmentLEDDigit;
    procedure SetItem(Index: Integer; Value: TJvSegmentLEDDigit);
    function Display: TJvCustomSegmentLEDDisplay; 
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvSegmentLEDDigit;
    function Insert(Index: Integer): TJvSegmentLEDDigit;
    property Items[Index: Integer]: TJvSegmentLEDDigit read GetItem write SetItem; default;
  end;
  
  TJvSegmentLEDDigit = class(TCollectionItem)
  private
    FDisplay: TJvCustomSegmentLEDDisplay;
    FSegments: array of Boolean;
    FDPRect: TRect;
    FUseDP: Boolean;
    FPolygons: array of array of TPoint;
  protected
    function GetSegments: string;
    procedure SetSegments(Value: string);
    function GetSegmentState(I: Integer): Boolean;
    procedure SetSegmentState(I: Integer; Value: Boolean);
    procedure SetUseDP(Value: Boolean);

    procedure CalcPolygons;
    procedure Invalidate;
    procedure Paint(ACanvas: TCanvas);

    property Display: TJvCustomSegmentLEDDisplay read FDisplay;
  public
    constructor Create(Collection: TCollection); override;
    procedure SetChar(const Ch: Char);

    property SegmentState[I: Integer]: Boolean read GetSegmentState write SetSegmentState;
  published
    property Segments: string read GetSegments write SetSegments;
    property UseDP: Boolean read FUseDP write SetUseDP;
  end;

function CharacterToSegmentString(Ch: Char; SegmentKind: TJvSegmentLEDKind): string;

const
  { New colors }
  clOrange = $0000A0FF;
  clOrangeOff = $0000415A;
  clLimeOff = $00004F00;
  clRedOff = $00000064;

implementation

uses
  Controls, contnrs, math;

const
  MapChToSeg7: string =
    ' =|0=ABCDEF|1=BC|2=ABDEG|3=ABCDG|4=BCFG|5=ACDFG|6=ACDEFG|7=ABC|8=ABCDEFG|9=ABCDFG|' +
    'A=ABCEFG|a=ABCEFG|B=CDEFG|b=CDEFG|C=ADEF|c=DEG|D=BCDEG|d=BCDEG|E=ADEFG|e=ADEFG|' +
    'F=AEFG|f=AEFG|H=BCEFG|h=CEFG|L=DEF|l=DEF|O=CDEG|o=CDEG|P=ABEFG|p=ABEFG|R=EG|r=EG|' +
    '''=F|"=BF|'#248'=ABFG|°=ABFG|-=G|U=BCDEF|u=CDE|y=BCDFG';

  MapChToSeg14: string =
    ' =|0=ABCDEF|1=BC|2=ABDEG|3=ABCDG|4=BCFG|5=ACDFG|6=ACDEFG|7=ABC|8=ABCDEFG|9=ABCDFG|' +
    #0'=ABCDEFKN|'#4'=FGJM|''=J|"=BJ|'#248'=ABFG|°=ABFG|-=G|+=GJM|(=KL|)=HN|*=GHJKLN|/=KN|\=HL|' +
    'A=ABCEFG|B=ABCDG2JM|C=ADEF|D=ABCDJM|E=ADEFG1|F=AEFG1|G=ACDEFG2|H=BCEFG|I=ADJM|J=ABCDE|' +
    'K=EFG1KL|L=EFD|M=BCEFHK|N=BCEFHL|O=ABCDEF|P=ABEFG|Q=ABCDEFL|R=ABEFGL|S=ACDG2H|T=AJM|' +
    'U=BCDEF|V=EFKN|W=BCEFLN|X=HKLN|Y=HKM|Z=ADKN|,=N';

  MapChToSeg16: string =
    ' =|0=ABCDEF|1=BC|2=ABDEG|3=ABCDG|4=BCFG|5=ACDFG|6=ACDEFG|7=ABC|8=ABCDEFG|9=ABCDFG|' +
    #0'=ABCDEFKN|'#4'=FGJM|''=J|"=BJ|'#248'=A2BJG2|°=A2BJG2|-=G|+=GJM|(=KL|)=HN|*=GHJKLN|/=KN|\=HL|' +
    'A=ABCEFG|B=ABCDG2JM|C=ADEF|D=ABCDJM|E=ADEFG1|F=AEFG1|G=ACDEFG2|H=BCEFG|I=ADJM|J=ABCDE|' +
    'K=EFG1KL|L=EFD|M=BCEFHK|N=BCEFHL|O=ABCDEF|P=ABEFG|Q=ABCDEFL|R=ABEFGL|S=ACDG2H|T=AJM|' +
    'U=BCDEF|V=EFKN|W=BCEFLN|X=HKLN|Y=HKM|Z=ADKN|[=A2D2JM|]=A1D1JM|{=A2D2G1JM|}=A1D1G2JM|' +
    '%=A1CD2FG1G2JKMN|,=N|:=G2D2|.=D2|&=ADEG1HKL';

  SegNames14: array[0..13] of string = (
    'A',  'B',  'C',  'D',  'E',  'F', 'G1',
    'G2', 'H',  'J',  'K',  'L',  'M',  'N'
  );

  SegNames16: array[0..15] of string = (
    'A1', 'A2', 'B',  'C',  'D1', 'D2', 'E',  'F',
    'G1', 'G2', 'H',  'J',  'K',  'L',  'M',  'N'
  );

resourcestring
  s_E_ld_InvalidString = 'Invalid string.';
  s_E_ld_IllegalChar = 'Invalid character ''%s'' (#%d).';
  s_E_ld_IllegalSegment = 'Invalid segment ''%s''.';

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
    raise EJvLEDDisplay.CreateFmt(s_E_ld_IllegalChar, [Ch, Ord(Ch)]);
  IEnd := IStart + 2;
  While (IEnd <= Length(SegData)) and (SegData[IEnd] <> '|') do
    Inc(IEnd);
  Result := Copy(SegData, IStart + 2, IEnd - IStart - 2);
end;

function CharacterToSegmentString(Ch: Char; SegmentKind: TJvSegmentLEDKind): string;
begin
  case SegmentKind of
    slk7Segments:
      Result := CharToSegString(Ch, MapChToSeg7);
    slk14Segments:
      Result := CharToSegString(Ch, MapChToSeg14);
    else
      Result := CharToSegString(Ch, MapChToSeg16);
  end;
end;

function TJvCustomSegmentLEDDisplay.GetDigit(I: Integer): TJvSegmentLEDDigit;
begin
  Result := TJvSegmentLEDDigit(FDigits[I]);
end;

function TJvCustomSegmentLEDDisplay.GetDigitCount: Integer;
begin
  Result := FDigits.Count;
end;

procedure TJvCustomSegmentLEDDisplay.SetAutoSize(Value: Boolean);
begin
  if Value <> AutoSize then
  begin
    FAutoSize := Value;
    if AutoSize then
      DisplayString(Text, True);
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetColorOn(Value: TColor);
begin
  if Value <> ColorOn then
  begin
    FColorOn := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetColorOff(Value: TColor);
begin
  if Value <> ColorOff then
  begin
    FColorOff := Value;
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
        FDigits.Add;
    InvalidateSize;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigitHeight(Value: Integer);
begin
  if Value <> DigitHeight then
  begin
    FDigitHeight := Value;
    InvalidatePolygons;
    InvalidateSize;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigits(Value: TJvSegmentLEDDigits);
begin
  FDigits.Assign(Value);
  InvalidateSize;
end;

procedure TJvCustomSegmentLEDDisplay.SetDigitWidth(Value: Integer);
begin
  if Value <> DigitWidth then
  begin
    FDigitWidth := Value;
    InvalidatePolygons;
    InvalidateSize;;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetKind(Value: TJvSegmentLEDKind);
begin
  if Value <> Kind then
  begin
    FKind := Value;
    InvalidatePolygons;
    Invalidate;
  end;
end;

procedure TJvCustomSegmentLEDDisplay.SetMargin(Value: Integer);
begin
  if Value <> Margin then
  begin
    FMargin := Value;
    InvalidatePolygons;
    InvalidateSize;
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

procedure TJvCustomSegmentLEDDisplay.InvalidateSize;
begin
  ClientWidth := DigitCount * DigitWidth;
  ClientHeight := DigitHeight;
  Invalidate;
end;

procedure TJvCustomSegmentLEDDisplay.SetText(Value: string);
begin
  if Value <> Text then
  begin
    DisplayString(Value, AutoSize);
    FText := Value;    
  end;
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
        Digits[I].CalcPolygons;
    end;
    with Canvas do
    begin
      Brush.Color := Color;
      Pen.Color := Color;
      Rectangle(0, 0, ClientWidth + 1, ClientHeight + 1);
    end;
    for I := 0 to DigitCount - 1 do
      Digits[I].Paint(Canvas);
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
  FDigits := TJvSegmentLEDDigits.Create(Self);
  FDigitHeight := 40;
  FDigitWidth := 24;
  FMargin := 4;
  FSpacing := 2;
  FSegmentWidth := 2;
  FColorOn := clRed;
  FColorOff := clRedOff;
end;

destructor TJvCustomSegmentLEDDisplay.Destroy;
begin
  FDigits.Free;
  inherited Destroy;
end;

procedure TJvCustomSegmentLEDDisplay.DisplayString(S: string; const ResizeDisplay: Boolean);
var
  I: Integer;
  Segs: TDynStringArray;
begin
  FText := S;
  Segs := DigitStringToSegments(S);
  if not ResizeDisplay then
  begin
    if Length(Segs) > DigitCount then
      SetLength(Segs, DigitCount);
  end
  else
    DigitCount := Length(Segs);
  if Length(Segs) < DigitCount then
    SetLength(Segs, DigitCount);
  for I := 0 to DigitCount - 1 do
    Digits[I].Segments := Segs[I];
  Invalidate;
end;

function TJvCustomSegmentLEDDisplay.DigitStringToSegments(S: string): TDynStringArray;
var
  Idx: Integer;
  IPos: Integer;
  ChVal: Integer;

  function ParseSpecifierString: string;
  begin
    Inc(IPos);
    Result := '';
    while (IPos <= Length(S)) and (S[IPos] <> ']') do
    begin
      if S[IPos] = '#' then // Following is the ordinal value of a character
      begin
        ChVal := 0;
        Inc(IPos);
        while (IPos <= Length(S)) and (S[IPos] in ['0' .. '9']) do
        begin
          ChVal := ChVal * 10 + StrToInt(S[IPos]);
          Inc(IPos);
        end;
        Dec(IPos);
        case Kind of
          slk7Segments:
            Result := Result + CharToSegString(Chr(ChVal), MapChToSeg7);
          slk14Segments:
            Result := Result + CharToSegString(Chr(ChVal), MapChToSeg14);
          slk16Segments:
            Result := Result + CharToSegString(Chr(ChVal), MapChToSeg16);
        end;
      end
      else
      if S[IPos] = '&' then // Following is a character
      begin
        Inc(IPos);
        case Kind of
          slk7Segments:
            Result := Result + CharToSegString(S[IPos], MapChToSeg7);
          slk14Segments:
            Result := Result + CharToSegString(S[IPos], MapChToSeg14);
          slk16Segments:
            Result := Result + CharToSegString(S[IPos], MapChToSeg16);
        end;
      end
      else  // Most likely a (part of a) segment identifier, copy as is
        Result := Result + S[IPos];
      Inc(IPos);
    end;
  end;

begin
  SetLength(Result, Length(S));
  Idx := 0;
  IPos := 1;
  while (IPos <= Length(S)) do
  begin
    if Idx = Length(Result) then
      SetLength(Result, Length(Result) + 16); // should never occur, but better safe than sorry
    if (S[IPos] = '[') and (IPos < Length(S)) and (S[IPos + 1] <> '[') then
      Result[Idx] := ParseSpecifierString
    else // current character is as is; translate to the right segment text and assign to the result
      case Kind of
        slk7Segments:
          Result[Idx] := CharToSegString(S[IPos], MapChToSeg7);
          slk14Segments:
            Result[Idx] := CharToSegString(S[IPos], MapChToSeg14);
        slk16Segments:
          Result[Idx] := CharToSegString(S[IPos], MapChToSeg16);
        else
          Result[Idx] := '';
      end;
    Inc(Idx);
    if S[IPos] = '[' then
      Inc(IPos);
    Inc(IPos);
  end;
  SetLength(Result, Idx);
end;

//===TJvSegmentLEDDigits============================================================================

function TJvSegmentLEDDigits.GetItem(Index: Integer): TJvSegmentLEDDigit;
begin
  Result := TJvSegmentLEDDigit(inherited Items[Index]);
end;

procedure TJvSegmentLEDDigits.SetItem(Index: Integer; Value: TJvSegmentLEDDigit);
begin
  inherited Items[Index] := Value;
end;

function TJvSegmentLEDDigits.Display: TJvCustomSegmentLEDDisplay;
begin
  Result := TJvCustomSegmentLEDDisplay(GetOwner);
end;

procedure TJvSegmentLEDDigits.Update(Item: TCollectionItem);
begin
  Display.Invalidate;
end;

constructor TJvSegmentLEDDigits.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvSegmentLEDDigit);
end;

function TJvSegmentLEDDigits.Add: TJvSegmentLEDDigit;
begin
  Result := TJvSegmentLEDDigit(inherited Add);
end;

function TJvSegmentLEDDigits.Insert(Index: Integer): TJvSegmentLEDDigit;
begin
  Result := TJvSegmentLEDDigit(inherited Insert(Index));
end;

//===TJvSegmentLEDDigit=============================================================================

function TJvSegmentLEDDigit.GetSegments: string;
var
  LastI: Integer;
  I: Integer;
begin
  Result := '';
  LastI := 7 + 7 * Ord(Display.Kind = slk14Segments) + 9 * Ord(Display.Kind = slk16Segments);
  for I := 0 to LastI do
  begin
    if FSegments[I] then
    begin
      if UseDP and (I = LastI) then
        Result := Result + ',DP'
      else
      begin
        case Display.Kind of
          slk7Segments:
            Result := Result + ',' + Chr(I + Ord('A'));
          slk14Segments:
            Result := Result + ',' + SegNames14[I];
          slk16Segments:
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
      if Display.Kind = slk14Segments then
      begin
        if AnsiSameText(Copy(Value, I, 2), 'DP') then
          SegIdx := 14
        else
        begin
          SegIdx := TextIndex(Value[I], SegNames14, False);
          if SegIdx < 0 then
            SegIdx := TextIndex(Copy(Value, I, 2), SegNames14, True);
          if SegIdx < 0 then
            SegIdx := TextIndex(Value[I], SegNames14, True);
        end;
      end
      else
      if Display.Kind = slk16Segments then
      begin
        if AnsiSameText(Copy(Value, I, 2), 'DP') then
          SegIdx := 16
        else
        begin
          SegIdx := TextIndex(Value[I], SegNames16, False);
          if SegIdx < 0 then
            SegIdx := TextIndex(Copy(Value, I, 2), SegNames16, True);
          if SegIdx < 0 then
            SegIdx := TextIndex(Value[I], SegNames16, True);
        end;
      end
      else
      begin
        if AnsiSameText(Copy(Value, I, 2), 'DP') then
          SegIdx := 7
        else
          SegIdx := Ord(UpCase(Value[I])) - Ord('A');
      end;
      if SegIdx < 0 then
        raise EJvLEDDisplay.Create(s_E_ld_InvalidString);
      FSegments[SegIdx] := True;
      if (Display.Kind = slk14Segments) and ((SegIdx = 7) and not AnsiSameText(Copy(Value, I, 2), SegNames14[SegIdx])) then
        FSegments[SegIdx - 1] := True;
      if (Display.Kind = slk16Segments) and ((SegIdx in [1, 5, 9]) and not AnsiSameText(Copy(Value, I, 2), SegNames16[SegIdx])) then
        FSegments[SegIdx - 1] := True;
      Inc(I);
      while (I <= Length(Value)) and (Value[I] in ['1', '2', ',', ';', 'P', #0 .. ' ']) do
        Inc(I);
    end;
  end;
  Invalidate;
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
    Invalidate;
  end;
end;

procedure TJvSegmentLEDDigit.SetUseDP(Value: Boolean);
begin
  if Value <> UseDP then
  begin
    FUseDP := Value;
    Invalidate;
  end;
end;

function AngleAdjustPoint(X, Y, Angle: Integer): TPoint;
begin
  Result.X := X + Trunc(ArcTan(Angle * Pi / 180.0) * Y);
  Result.Y := Y;
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
  SlantAngle: Integer;

  procedure SetPolygon(SegNum: Integer; Points: array of TPoint);
  begin
    SetLength(FPolygons[SegNum], Length(Points));
    Move(Points[0], FPolygons[SegNum][0], Length(Points) * SizeOf(TPoint));
  end;

begin
  SlantAngle := Display.SlantAngle;
  with Display do
  begin
    SegT := SegmentWidth;
    Spc := Spacing;
    DigM := Margin;
    DigX := Index * DigitWidth;
    DigW := DigitWidth - 2 * DigM - (Ord(UseDP) * SegT) - 2;
    DigH := DigitHeight - 2 * DigM - (SegT div 2);
    SlantDiff := Trunc(Abs(ArcTan(SlantAngle * Pi / 180.0) * DigH));
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

  CenterLeftX := TopLeftX; //DigX + DigM + SlantDiff div 2;
  CenterLeftY := DigM + (DigH div 2);
  CenterCenterX := CenterLeftX + (DigW div 2);
  CenterCenterY := CenterLeftY;
  CenterRightX := CenterLeftX + DigW;
  CenterRightY := CenterLeftY;

  BottomLeftX := TopLeftX; //DigX + DigM;
  BottomLeftY := DigM + DigH;
  BottomCenterX := BottomLeftX + (DigW div 2);
  BottomCenterY := BottomLeftY;
  BottomRightX := BottomLeftX + DigW;
  BottomRightY := BottomLeftY;

  { J and M segments are special cases. 14 segment displays have them starting just below/above
    the A/D segments, whereas 16 segment displays have them starting at the same height as the
    two A/D segments.}
  if Display.Kind = slk16Segments then
    VertOffset := HalfSpc
  else
    VertOffset := SegT + HalfSpc;

  // Calculate and remember the polygons.
  SetLength(FPolygons, 19);
  SetPolygon(0, [ // A
    AngleAdjustPoint(TopLeftX + HalfSpc, TopLeftY, -SlantAngle),
    AngleAdjustPoint(TopRightX - HalfSpc, TopRightY, -SlantAngle),
    AngleAdjustPoint(TopRightX - HalfSpc - SegT, TopRightY + SegT, -SlantAngle),
    AngleAdjustPoint(TopLeftX + HalfSpc + SegT, TopLeftY + SegT, -SlantAngle)
  ]);
  SetPolygon(1, [ // A1
    AngleAdjustPoint(TopLeftX + HalfSpc, TopLeftY, -SlantAngle),
    AngleAdjustPoint(TopCenterX - HalfSpc, TopCenterY, -SlantAngle),
    AngleAdjustPoint(TopCenterX - HalfSpc - SegT, TopCenterY + SegT, -SlantAngle),
    AngleAdjustPoint(TopLeftX + HalfSpc + SegT, TopLeftY + SegT, -SlantAngle)
  ]);
  SetPolygon(2, [ // A2
    AngleAdjustPoint(TopCenterX + HalfSpc, TopCenterY, -SlantAngle),
    AngleAdjustPoint(TopRightX - HalfSpc, TopRightY, -SlantAngle),
    AngleAdjustPoint(TopRightX - HalfSpc - SegT, TopRightY + SegT, -SlantAngle),
    AngleAdjustPoint(TopCenterX + HalfSpc + SegT, TopCenterY + SegT, -SlantAngle)
  ]);
  SetPolygon(3, [ // B
    AngleAdjustPoint(TopRightX, TopRightY + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterRightX, CenterRightY - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterRightX - SegT, CenterRightY - HalfSpc - SegT, -SlantAngle),
    AngleAdjustPoint(TopRightX - SegT, TopRightY + HalfSpc + SegT, -SlantAngle)
  ]);
  SetPolygon(4, [ // C
    AngleAdjustPoint(CenterRightX, CenterRightY + HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomRightX, BottomRightY - HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomRightX - SegT, BottomRightY - HalfSpc - SegT, -SlantAngle),
    AngleAdjustPoint(CenterRightX - SegT, CenterRightY + HalfSpc + SegT, -SlantAngle)
  ]);
  SetPolygon(5, [ // D
    AngleAdjustPoint(BottomLeftX + HalfSpc, BottomLeftY, -SlantAngle),
    AngleAdjustPoint(BottomRightX - HalfSpc, BottomRightY, -SlantAngle),
    AngleAdjustPoint(BottomRightX - HalfSpc - SegT, BottomRightY - SegT, -SlantAngle),
    AngleAdjustPoint(BottomLeftX + HalfSpc + SegT, BottomLeftY - SegT, -SlantAngle)
  ]);
  SetPolygon(6, [ // D1
    AngleAdjustPoint(BottomLeftX + HalfSpc, BottomLeftY, -SlantAngle),
    AngleAdjustPoint(BottomCenterX - HalfSpc, BottomCenterY, -SlantAngle),
    AngleAdjustPoint(BottomCenterX - HalfSpc - SegT, BottomCenterY - SegT, -SlantAngle),
    AngleAdjustPoint(BottomLeftX + HalfSpc + SegT, BottomLeftY - SegT, -SlantAngle)
  ]);
  SetPolygon(7, [ // D2
    AngleAdjustPoint(BottomCenterX + HalfSpc, BottomCenterY, -SlantAngle),
    AngleAdjustPoint(BottomRightX - HalfSpc, BottomRightY, -SlantAngle),
    AngleAdjustPoint(BottomRightX - HalfSpc - SegT, BottomRightY - SegT, -SlantAngle),
    AngleAdjustPoint(BottomCenterX + HalfSpc + SegT, BottomCenterY - SegT, -SlantAngle)
  ]);
  SetPolygon(8, [ // E
    AngleAdjustPoint(CenterLeftX, CenterLeftY + HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomLeftX, BottomLeftY - HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomLeftX + SegT, BottomLeftY - HalfSpc - SegT, -SlantAngle),
    AngleAdjustPoint(CenterLeftX + SegT, CenterLeftY + HalfSpc + SegT, -SlantAngle)
  ]);
  SetPolygon(9, [ // F
    AngleAdjustPoint(TopLeftX, TopLeftY + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterLeftX, CenterLeftY - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterLeftX + SegT, CenterLeftY - HalfSpc - SegT, -SlantAngle),
    AngleAdjustPoint(TopLeftX + SegT, TopLeftY + HalfSpc + SegT, -SlantAngle)
  ]);
  SetPolygon(10, [ // G
    AngleAdjustPoint(CenterLeftX + HalfSpc, CenterLeftY, -SlantAngle),
    AngleAdjustPoint(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterRightX - HalfSpc - HalfSegT, CenterRightY - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterRightX - HalfSpc, CenterRightY, -SlantAngle),
    AngleAdjustPoint(CenterRightX - HalfSpc - HalfSegT, CenterRightY + HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(11, [ // G1
    AngleAdjustPoint(CenterLeftX + HalfSpc, CenterLeftY, -SlantAngle),
    AngleAdjustPoint(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSpc - HalfSegT, CenterCenterY - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSpc, CenterCenterY, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSpc - HalfSegT, CenterCenterY + HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterLeftX + HalfSpc + HalfSegT, CenterLeftY + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(12, [ // G2
    AngleAdjustPoint(CenterCenterX + HalfSpc, CenterCenterY, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSpc + HalfSegT, CenterCenterY - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterRightX - HalfSpc - HalfSegT, CenterRightY - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterRightX - HalfSpc, CenterRightY, -SlantAngle),
    AngleAdjustPoint(CenterRightX - HalfSpc - HalfSegT, CenterRightY + HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSpc + HalfSegT, CenterCenterY + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(13, [ // H
    AngleAdjustPoint(TopLeftX + SegT + HalfSpc, TopLeftY + SegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(TopLeftX + SegT + HalfSpc + HalfSegT, TopLeftY + SegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY - SegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY - HalfSegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - SegT - HalfSpc, CenterCenterY - HalfSegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(TopLeftX + SegT + HalfSpc, TopLeftY + SegT + HalfSpc + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(14, [ // J
    AngleAdjustPoint(TopCenterX, TopCenterY + VertOffset, -SlantAngle),
    AngleAdjustPoint(TopCenterX + HalfSegT, TopCenterY + VertOffset + HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSegT, CenterCenterY - HalfSpc - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterCenterX, CenterCenterY - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSegT, CenterCenterY - HalfSpc - HalfSegT, -SlantAngle),
    AngleAdjustPoint(TopCenterX - HalfSegT, TopCenterY + VertOffset + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(15, [ // K
    AngleAdjustPoint(TopRightX - SegT - HalfSpc, TopRightY + SegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(TopRightX - SegT - HalfSpc - HalfSegT, TopRightY + SegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY - SegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY - HalfSegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + SegT + HalfSpc, CenterCenterY - HalfSegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(TopRightX - SegT - HalfSpc, TopRightY + SegT + HalfSpc + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(16, [ // L
    AngleAdjustPoint(BottomRightX - SegT - HalfSpc, BottomRightY - SegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomRightX - SegT - HalfSpc - HalfSegT, BottomRightY - SegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY + SegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSegT + HalfSpc, CenterCenterY + HalfSegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + SegT + HalfSpc, CenterCenterY + HalfSegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomRightX - SegT - HalfSpc, BottomRightY - SegT - HalfSpc - HalfSegT, -SlantAngle)
  ]);
  SetPolygon(17, [ // M
    AngleAdjustPoint(CenterCenterX, CenterCenterY + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX + HalfSegT, CenterCenterY + HalfSpc + HalfSegT, -SlantAngle),
    AngleAdjustPoint(BottomCenterX + HalfSegT, BottomCenterY - VertOffset - HalfSegT, -SlantAngle),
    AngleAdjustPoint(BottomCenterX, BottomCenterY - VertOffset, -SlantAngle),
    AngleAdjustPoint(BottomCenterX - HalfSegT, BottomCenterY - VertOffset - HalfSegT, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSegT, CenterCenterY + HalfSpc + HalfSegT, -SlantAngle)
  ]);
  SetPolygon(18, [ // N
    AngleAdjustPoint(BottomLeftX + SegT + HalfSpc, BottomLeftY - SegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomLeftX + SegT + HalfSpc + HalfSegT, BottomLeftY - SegT - HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY + SegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - HalfSegT - HalfSpc, CenterCenterY + HalfSegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(CenterCenterX - SegT - HalfSpc, CenterCenterY + HalfSegT + HalfSpc, -SlantAngle),
    AngleAdjustPoint(BottomLeftX + SegT + HalfSpc, BottomLeftY - SegT - HalfSpc - HalfSegT, -SlantAngle)
  ]);
  FDPRect.TopLeft := AngleAdjustPoint(BottomRightX + Spc, BottomRightY - SegT, -SlantAngle);
  FDPRect.BottomRight := AngleAdjustPoint(BottomRightX + Spc + SegT + HalfSegT + 1, BottomRightY + HalfSegT + 1, -SlantAngle);
end;

procedure TJvSegmentLEDDigit.Invalidate;
begin
  CalcPolygons;
  Display.Invalidate;
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
      raise EJvLEDDisplay.CreateFmt(s_E_ld_IllegalSegment, [SegName]);
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

  procedure RenderDP(const AColor: TColor);
  begin
    with ACanvas do
    begin
      Brush.Color := AColor;
      Pen.Color := AColor;
      Ellipse(FDPRect);
    end;
  end;

begin
  case Display.Kind of
    slk7Segments:
      begin
        for I := 6 downto 0 do
          RenderSegment(Chr(Ord('A') + I), ColorForState(FSegments[I]));
      end;
    slk14Segments:
      begin
        for I := 13 downto 0 do
          RenderSegment(SegNames14[I], ColorForState(FSegments[I]));
      end;
    slk16Segments:
      begin
        for I := 15 downto 0 do
          RenderSegment(SegNames16[I], ColorForState(FSegments[I]));
      end;
  end;
  if UseDP then
    RenderDP(ColorForState(FSegments[7 + 7 * Ord(Display.Kind = slk14Segments) + 9 * Ord(Display.Kind = slk16Segments)]));
end;

constructor TJvSegmentLEDDigit.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDisplay := (Collection as TJvSegmentLEDDigits).Display;
  SetLength(FSegments, 17);
  CalcPolygons;
end;

procedure TJvSegmentLEDDigit.SetChar(const Ch: Char);
begin
  case Display.Kind of
    slk7Segments:
      Segments := CharToSegString(Ch, MapChToSeg7);
    slk14Segments:
      Segments := CharToSegString(Ch, MapChToSeg14);
    else
      Segments := CharToSegString(Ch, MapChToSeg16);
  end;
end;

end.
