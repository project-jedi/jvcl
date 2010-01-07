unit DefinesConditionParser;

{$I jvcl.inc}

interface

uses
  Classes,
  JclSimpleXml,
  ConditionParser, GenerateDefines;

type
  TDefinesConditionParser = class (TConditionParser)
  private
    FDefinesList: TDefinesList;
    FTargetDefines: TStrings;
  protected
    procedure MissingRightParenthesis; override;
    function GetIdentValue(const Ident: String): Boolean; override;
  public
    constructor Create(incfile, ATargetDefines: TStrings);
    destructor Destroy; override;

    procedure EnsureCondition(lines: TStrings; Condition: string);
    function EnsurePFlagsCondition(const pflags: string): string;

    procedure Append(incfile, ATargetDefines: TStrings);

    property Defines: TDefinesList read FDefinesList;
  end;

implementation

uses
  SysUtils,
  JclStrings;

{ TDefinesConditionParser }

procedure TDefinesConditionParser.Append(incfile, ATargetDefines: TStrings);
begin
  FTargetDefines := ATargetDefines;
  FDefinesList.Append(incfile);
end;

constructor TDefinesConditionParser.Create(incfile, ATargetDefines: TStrings);
begin
  inherited Create;
  FTargetDefines := ATargetDefines;
  FDefinesList := TDefinesList.Create(incfile);
end;

destructor TDefinesConditionParser.Destroy;
begin
  FDefinesList.Free;
  inherited Destroy;
end;

procedure TDefinesConditionParser.MissingRightParenthesis;
begin
  raise Exception.Create('Missing ")" in conditional expression');
end;

function TDefinesConditionParser.GetIdentValue(const Ident: String): Boolean;
begin
  Result := FDefinesList.IsDefined(Ident, FTargetDefines);
end;

function TDefinesConditionParser.EnsurePFlagsCondition(const pflags: string): string;
var
  PFlagsList : TStringList;
  I : Integer;
  CurPFlag : string;
  Condition : string;
  ParensPos : Integer;
begin
  // If any of the PFLAGS is followed by a string between parenthesis
  // then this is considered to be a condition.
  // If the condition is not in the Defines list, then the
  // corresponding PFLAG is discarded. This has been done mostly for
  // packages that have extended functionality if USEJVCL is
  // activated and as such require the JCL dcp file.
  PFlagsList := TStringList.Create;
  Result := pflags;
  try
    StrToStrings(pflags, ' ', PFlagsList, False);
    for I := 0 to PFlagsList.Count-1 do
    begin
      CurPFlag := PFlagsList[I];
      ParensPos := Pos('(', CurPFlag);
      if ParensPos <> 0 then
      begin
        Condition := Copy(CurPFlag, ParensPos+1, Length(CurPFlag) - ParensPos -1);
        if not FDefinesList.IsDefined(Condition, FTargetDefines)  then
          PFlagsList[I] := ''
        else
          PFlagsList[I] := Copy(CurPFlag, 1, ParensPos-1);
      end;
    end;
    Result := StringsToStr(PFlagsList, ' ', False);
  finally
    PFlagsList.Free;
  end;
end;

procedure TDefinesConditionParser.EnsureCondition(lines: TStrings; Condition: string);
begin
  // if there is a condition
  if (Condition <> '') and not Parse(Condition) then
    lines.Clear;
end;

end.
