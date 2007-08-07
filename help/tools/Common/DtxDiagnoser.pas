unit DtxDiagnoser;

interface

uses
  Windows, Classes,
  JVCLHelpUtils, DtxParser, SysUtils;

type
  TDtxDiagnoser = class
  private
    FWords: TStringList;
    FIdentifiers: TStringList;
    FNotWhiteSpace: TSysCharSet;
    FIgnoreWordsContainingNumbers: Boolean;
    FRemoveBeginQuotes: Boolean;
    FRemoveEndQuotes: Boolean;
    FCollectIdentifiers: Boolean;
    FCollectWords: Boolean;
    function GetNotWhiteSpaceStr: string;
    procedure SetNotWhiteSpaceStr(const Value: string);
    function GetWhiteSpace: TSysCharSet;
    function GetWords: TStrings;
    function GetIdentifiers: TStrings;
  protected
    procedure AddWord(const S: string; const ContainsNumbers: Boolean);
    procedure AddString(const S: string);
    procedure ProcessTopic(ATopic: TDtxTopic);
    procedure ProcessSymbolList(ASymbolList: TSymbolList);
    procedure ProcessParameterList(AParameterList: TParameterList);
    procedure ProcessItems(AItems: TItemsSymbol);
    procedure ProcessTable(ATable: TDtxTable);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Collect(ADtxList: TDtxList): Boolean;
    procedure ShowStats;

    property NotWhiteSpaceStr: string read GetNotWhiteSpaceStr write SetNotWhiteSpaceStr;
    property NotWhiteSpace: TSysCharSet read FNotWhiteSpace write FNotWhiteSpace;
    property WhiteSpace: TSysCharSet read GetWhiteSpace;
    property IgnoreWordsContainingNumbers: Boolean read FIgnoreWordsContainingNumbers write
      FIgnoreWordsContainingNumbers;
    property RemoveEndQuotes: Boolean read FRemoveEndQuotes write FRemoveEndQuotes;
    property RemoveBeginQuotes: Boolean read FRemoveBeginQuotes write FRemoveBeginQuotes;

    property Words: TStrings read GetWords;
    property Identifiers: TStrings read GetIdentifiers;

    property CollectWords: Boolean read FCollectWords write FCollectWords;
    property CollectIdentifiers: Boolean read FCollectIdentifiers write FCollectIdentifiers;
  end;

implementation

{ TDtxDiagnoser }

procedure TDtxDiagnoser.AddString(const S: string);
//const
//  NotWhiteSpace = [#1..#47] - ['-', '_'] + ['\', '/', '[', ']', ':', ';', ',', '.'];
var
  P, Q, R: PChar;
  Word: string;
  AWhiteSpace: TSysCharSet;
  ContainsNumbers: Boolean;
begin
  AWhiteSpace := WhiteSpace;

  P := PChar(S);
  repeat
    while (P^ in AWhiteSpace) do
      Inc(P);
    Q := P;
    ContainsNumbers := False;
    while not (P^ in ([#0] + AWhiteSpace)) do
    begin
      ContainsNumbers := ContainsNumbers or (P^ in ['0'..'9']);
      Inc(P);
    end;
    if P > Q then
    begin
      R := P;
      if RemoveEndQuotes then
        while (Q < R) and ((R - 1)^ = '''') do
          Dec(R);
      if RemoveBeginQuotes then
        while (Q < R) and (Q^ = '''') do
          Inc(Q);
      if R > Q then
      begin
        SetString(Word, Q, R - Q);
        AddWord(Word, ContainsNumbers);
      end;
    end;
  until P^ = #0;
end;

function IsIdentifier(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 2 to Length(S) do
    if S[i] in ['A'..'Z', '_'] then
    begin
      Result := True;
      Break;
    end;
end;

procedure TDtxDiagnoser.AddWord(const S: string; const ContainsNumbers: Boolean);
begin
  if IsIdentifier(S) then
  begin
    if CollectIdentifiers then
      FIdentifiers.Add(S);
  end
  else
    if not (ContainsNumbers and IgnoreWordsContainingNumbers) then
  begin
    if CollectWords then
      FWords.Add(LowerCase(S));
  end;
end;

constructor TDtxDiagnoser.Create;
begin
  inherited Create;

  FWords := TStringList.Create;
  FWords.Sorted := True;
  FWords.Duplicates := dupIgnore;
  FWords.CaseSensitive := False;

  FIdentifiers := TStringList.Create;
  FIdentifiers.Sorted := True;
  FIdentifiers.Duplicates := dupIgnore;
  FIdentifiers.CaseSensitive := True;

  FRemoveBeginQuotes := True;
  FRemoveEndQuotes := True;
end;

destructor TDtxDiagnoser.Destroy;
begin
  FWords.Free;
  FIdentifiers.Free;
  inherited Destroy;
end;

function TDtxDiagnoser.Collect(ADtxList: TDtxList): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to ADtxList.Count - 1 do
    ProcessTopic(ADtxList[i]);
end;

procedure DiffStrings(Source, CheckList, Dest: TStrings);
var
  I, J: Integer;
  C: Integer;
begin
  I := 0;
  J := 0;
  while (I < Source.Count) and (J < CheckList.Count) do
  begin
    C := AnsiCompareText(Source[i], CheckList[J]);
    if C < 0 then
    begin
      Dest.Add(Source[i]);
      Inc(I);
    end
    else
      if C > 0 then
    begin
      Inc(J);
    end
    else
    begin
      Inc(I);
      Inc(J);
    end;
  end;

  while I < Source.Count do
  begin
    Dest.Add(Source[i]);
    Inc(I);
  end
end;

function TDtxDiagnoser.GetNotWhiteSpaceStr: string;
var
  Ch: Char;
begin
  Result := '';
  for Ch := Low(Char) to High(Char) do
    if Ch in NotWhiteSpace then
      Result := Result + Ch;
end;

procedure TDtxDiagnoser.ProcessItems(AItems: TItemsSymbol);
var
  I: Integer;
begin
  for I := 0 to AItems.Count - 1 do
    ProcessSymbolList(AItems.Symbols[i]);
end;

procedure TDtxDiagnoser.ProcessParameterList(
  AParameterList: TParameterList);
var
  I: Integer;
begin
  for I := 0 to AParameterList.Count - 1 do
    ProcessSymbolList(AParameterList.Params[i]);
end;

procedure TDtxDiagnoser.ProcessSymbolList(ASymbolList: TSymbolList);
var
  I: Integer;
  Symbol: TBaseSymbol;
begin
  if ASymbolList = nil then
    Exit;
  for I := 0 to ASymbolList.Count - 1 do
  begin
    Symbol := ASymbolList[i];
    if Symbol is TStringSymbol then
      AddString(TStringSymbol(Symbol).Value)
    else
    if Symbol is TItemsSymbol then
      ProcessItems(TItemsSymbol(Symbol))
    else
    if Symbol is TDtxTable then
      ProcessTable(TDtxTable(Symbol));
  end;
end;

procedure TDtxDiagnoser.ProcessTable(ATable: TDtxTable);
var
  Row, Col: Integer;
begin
  for Row := 0 to ATable.RowCount - 1 do
    for COl := 0 to ATable.ColCount - 1 do
      ProcessSymbolList(ATable.CellsSymbols[Col, Row]);
end;

procedure TDtxDiagnoser.ProcessTopic(ATopic: TDtxTopic);
begin
  with ATopic do
  begin
    ProcessSymbolList(Summary);
    ProcessSymbolList(Note);
    ProcessSymbolList(ReturnValue);
    ProcessSymbolList(Description);
    ProcessParameterList(Parameters);
  end;
end;

procedure TDtxDiagnoser.SetNotWhiteSpaceStr(const Value: string);
var
  I: Integer;
begin
  FNotWhiteSpace := [];
  for I := 1 to Length(Value) do
    Include(FNotWhiteSpace, Value[i]);
end;

function TDtxDiagnoser.GetWhiteSpace: TSysCharSet;
begin
  Result := [Low(Char)..High(Char)] -
    [#0] - ['A'..'Z'] - ['a'..'z'] - ['0'..'9'] - NotWhiteSpace;

  //Result := Result + ['-'];
end;

function TDtxDiagnoser.GetWords: TStrings;
begin
  Result := FWords;
end;

function TDtxDiagnoser.GetIdentifiers: TStrings;
begin
  Result := FIdentifiers;
end;

procedure TDtxDiagnoser.ShowStats;
begin
  if CollectWords then
    HintMsgFmt('%d words', [FWords.Count]);
  if CollectIdentifiers then
    HintMsgFmt('%d identifiers', [FIdentifiers.Count]);
end;

end.

