unit glLogics;

interface
uses classes, sysUtils, Graphics;

type

  TLogicRule = (lrEqual, lrBeginWith, lrEndWith, lrContains, lrContainsIn, ltNotEmpty);
const
  LogicRuleLabels: array[TLogicRule] of string = ('равно', 'начинается с', 'оканчивается на', 'содержит', 'содержится в', 'не пусто');

type
  TLogics = class;
  TLogicElement = class;
  TCommentAreas = class;
  TLogicVariants = class;
  TLogicVariant = class;
  TLogicProducer = class;

  TOnTraceMessage = procedure (Sender: TLogics; fStepResult: boolean; const StepResult, ParsedResult, Msg: string) of object;

  TLogicProducer = class(TComponent)
  private
    FLogics: TLogics;
    FCommentAreas: TCommentAreas;
    FIgnoreSpaces: boolean;

    procedure SetLogics(const Value: TLogics);
    procedure SetDictionary(const Value: TStrings);
    function GetDictionary: TStrings;
    procedure SetCommentAreas(const Value: TCommentAreas);
    procedure SetIgnoreSpaces(const Value: boolean);
    function GetIgnoreSpaces: boolean;
    procedure SetOnTraceMessage(const Value: TOnTraceMessage);
    function GetOnTraceMessage: TOnTraceMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Logics: TLogics read FLogics write SetLogics;
    property CommentAreas: TCommentAreas read FCommentAreas write SetCommentAreas;
    property Dictionary: TStrings read GetDictionary write SetDictionary;
    property IgnoreSpaces: boolean read GetIgnoreSpaces write SetIgnoreSpaces;
    property OnTraceMessage: TOnTraceMessage read GetOnTraceMessage write SetOnTraceMessage;
  end;

  TLogicElement = class(TCollectionItem)
  private
    FNextElementID: integer;
    FNextFalseElementID: integer;

    FNextElement: TLogicElement;
    FNextFalseElement: TLogicElement;

    FLeft: integer;
    FTop: integer;
    FCaption: string;
    FIsFirst: boolean;
    FValue: string;
    FExpression: string;
    FRule: TLogicRule;
    FTrueResult: string;
    FFalseResult: string;
    FLogicVariants: TLogicVariants;

    function GetNextElement: TLogicElement;
    function GetNextFalseElement: TLogicElement;

    procedure SetCaption(const Value: string);
    procedure SetIsFirst(const Value: boolean);

    procedure SetNextElement(const Value: TLogicElement);
    procedure SetNextFalseElement(const Value: TLogicElement);
    procedure SetExpression(const Value: string);
    procedure SetRule(const Value: TLogicRule);
    procedure SetValue(const Value: string);
    procedure SetFalseResult(const Value: string);
    procedure SetTrueResult(const Value: string);
    procedure SetLogicVariants(const Value: TLogicVariants);
  public
    IsTrue: boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Loaded;

    property NextElement: TLogicElement read GetNextElement write SetNextElement;
    property NextFalseElement: TLogicElement read GetNextFalseElement write SetNextFalseElement;

  published
    property ID;
    property NextElementID: integer read FNextElementID write FNextElementID default -1;
    property NextFalseElementID: integer read FNextFalseElementID write FNextFalseElementID default -1;

    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Caption: string read FCaption write SetCaption;
    property IsFirst: boolean read FIsFirst write SetIsFirst;

    property Expression: string read FExpression write SetExpression;
    property Rule: TLogicRule read FRule write SetRule;
    property Value: string read FValue write SetValue;
    property TrueResult: string read FTrueResult write SetTrueResult;
    property FalseResult: string read FFalseResult write SetFalseResult;
    property LogicVariants: TLogicVariants read FLogicVariants write SetLogicVariants;

  end;

  TLogics = class(TCollection)
  private
    FResult: string;
    FDictionary: TStrings;
    FIgnoreSpaces: boolean;
    FOnTraceMessage: TOnTraceMessage;

    function GetItem(Index: Integer): TLogicElement;
    procedure SetItem(Index: Integer; Value: TLogicElement);
    function GetItemResult(Item: TLogicElement; var LogicVariant: TLogicVariant): boolean;
    procedure SetDictionary(const Value: TStrings);
    function ParseExpression(const Value: string): string;

  public
    TraceItem: TLogicElement;

    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    procedure Loaded;

    procedure Analyze;
    procedure AnalyzeStep;
    procedure StartAnalyze;    
    //    procedure Assign(StylePairs: TLogics);
    function  Add: TLogicElement;
    function Insert(Index: Integer): TLogicElement;
    property Items[Index: Integer]: TLogicElement read GetItem  write SetItem; default;
    property Result: string read FResult write FResult;
  published
    property Dictionary: TStrings read FDictionary write SetDictionary;
    property IgnoreSpaces: boolean read FIgnoreSpaces write FIgnoreSpaces;
    property OnTraceMessage: TOnTraceMessage read FOnTraceMessage write FOnTraceMessage;
  end;

  TLogicVariant = class(TCollectionItem)
  private
//    FExpression: string;
    FValue: string;
    FTrueResult: string;
    FFalseResult: string;
  published
//    property Expression: string read FExpression write FExpression;
    property Value: string read FValue write FValue;
    property TrueResult: string read FTrueResult write FTrueResult;
    property FalseResult: string read FFalseResult write FFalseResult;
  end;

  TLogicVariants = class(TCollection)
  private
    function GetItem(Index: Integer): TLogicVariant;
    procedure SetItem(Index: Integer; Value: TLogicVariant);
  public
    function  Add: TLogicVariant;
    function Insert(Index: Integer): TLogicVariant;
    property Items[Index: Integer]: TLogicVariant read GetItem  write SetItem; default;
  end;

  TCommentArea = class(TCollectionItem)
  private
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FText: string;
    FColor: TColor;
  published
    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property Text: string read FText write FText;
    property Color: TColor read FColor write FColor;
  end;

  TCommentAreas = class(TCollection)
  private
    function GetItem(Index: Integer): TCommentArea;
    procedure SetItem(Index: Integer; Value: TCommentArea);
  public
    function  Add: TCommentArea;
    function Insert(Index: Integer): TCommentArea;
    property Items[Index: Integer]: TCommentArea read GetItem  write SetItem; default;
  end;

implementation
uses glUtils;

constructor TLogicElement.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Caption := 'Step ' + IntToStr(ID);
  FNextElementID := -1;
  FNextFalseElementID := -1;
  FLogicVariants := TLogicVariants.Create(TLogicVariant);
end;

destructor TLogicElement.Destroy;
begin
  FLogicVariants.Free;
  inherited;
end;

procedure TLogicElement.Loaded;
begin
  if FLogicVariants.Count = 0 then with FLogicVariants.Add do
  begin
//    FExpression := self.FExpression;
    FValue := self.FValue;
    FTrueResult := self.FTrueResult;
    FFalseResult := self.FFalseResult;
  end;
end;

constructor TLogics.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FDictionary := TStringList.Create;
end;

destructor TLogics.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

procedure TLogics.Loaded;
var i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].Loaded;
end;

function TLogics.GetItem(Index: Integer): TLogicElement;
begin
  Result := TLogicElement(inherited Items[Index]);
end;

procedure TLogics.SetItem(Index: Integer; Value: TLogicElement);
begin
  Items[Index].Assign(Value);
end;

function TLogics.Add: TLogicElement;
begin
  Result := TLogicElement(inherited Add);
end;

function TLogics.Insert(Index: Integer): TLogicElement;
begin
  Result := TLogicElement(inherited Insert(Index));
end;


function TLogicElement.GetNextElement: TLogicElement;
begin
  Result := TLogicElement(Collection.FindItemID(FNextElementID));
end;

function TLogicElement.GetNextFalseElement: TLogicElement;
begin
  Result := TLogicElement(Collection.FindItemID(FNextFalseElementID));
end;

procedure TLogicElement.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TLogicElement.SetExpression(const Value: string);
begin
  FExpression := Value;
end;

procedure TLogicElement.SetFalseResult(const Value: string);
begin
  FFalseResult := Value;
end;

procedure TLogicElement.SetIsFirst(const Value: boolean);
begin
  FIsFirst := Value;
end;

procedure TLogicElement.SetLogicVariants(const Value: TLogicVariants);
begin
  FLogicVariants.Assign(Value);
end;

procedure TLogicElement.SetNextElement(const Value: TLogicElement);
begin
  if Value = nil then FNextElementID := -1
                 else FNextElementID := Value.ID;
end;

procedure TLogicElement.SetNextFalseElement(const Value: TLogicElement);
begin
  if Value = nil then FNextFalseElementID := -1
                 else FNextFalseElementID := Value.ID;
end;

{ TLogicProducer }

constructor TLogicProducer.Create(AOwner: TComponent);
begin
  inherited;
  FLogics := TLogics.Create(TLogicElement);
  FCommentAreas := TCommentAreas.Create(TCommentArea);
end;

destructor TLogicProducer.Destroy;
begin
  inherited;
  FLogics.Free;
end;

function TLogicProducer.GetDictionary: TStrings;
begin
  Result := Logics.Dictionary;
end;

procedure TLogicProducer.SetCommentAreas(const Value: TCommentAreas);
begin
  FCommentAreas.Assign(Value);
end;

procedure TLogicProducer.SetDictionary(const Value: TStrings);
begin
  Logics.Dictionary.Assign(Value);
end;

procedure TLogicProducer.SetIgnoreSpaces(const Value: boolean);
begin
  Logics.IgnoreSpaces := Value;
end;

function TLogicProducer.GetIgnoreSpaces: boolean;
begin
  Result := Logics.IgnoreSpaces;
end;

procedure TLogicProducer.SetLogics(const Value: TLogics);
begin
  FLogics := Value;
end;

procedure TLogicElement.SetRule(const Value: TLogicRule);
begin
  FRule := Value;
end;

procedure TLogicElement.SetTrueResult(const Value: string);
begin
  FTrueResult := Value;
end;

procedure TLogicElement.SetValue(const Value: string);
begin
  FValue := Value;
end;

procedure TLogics.StartAnalyze;
begin
  if Count > 0 then TraceItem := Items[0] else TraceItem := nil;
end;

procedure TLogics.AnalyzeStep;
var
  f: boolean;
  Item: TLogicElement;
  LogicVariant: TLogicVariant;
begin
  LogicVariant := nil;

  if Assigned(TraceItem) then
  begin
    TraceItem.IsTrue := true;
    f := GetItemResult(TraceItem, LogicVariant);

    if f then Result := Result + ParseExpression(LogicVariant.TrueResult)
         else Result := Result + ParseExpression(LogicVariant.FalseResult);
    if f then TraceItem := TraceItem.NextElement
         else TraceItem := TraceItem.NextFalseElement;
  end;
end;

procedure TLogics.Analyze;
var
  i: integer;
  Item: TLogicElement;
begin
  for i := 0 to Count-1 do Items[i].IsTrue := false;

  Result := '';
  i := 0;
  TraceItem := Items[0];
  while Assigned(TraceItem) and (i<1000) do
  begin
    AnalyzeStep;
    inc(i);
  end;
end;

function TLogics.GetItemResult(Item: TLogicElement; var LogicVariant: TLogicVariant): boolean;
var
  Expr, Value: string;
  i: integer;
begin
  Expr := ParseExpression(Item.Expression);
  if IgnoreSpaces then Expr := trim(Expr);

  for i:=0 to Item.LogicVariants.Count-1 do
  begin
    Value := ParseExpression(Item.LogicVariants[i].Value);

    case Item.Rule of
      lrEqual     : Result := Expr = Value;
      lrBeginWith : Result := pos(Value, Expr) = 1;
      lrEndWith   : Result := copy(Expr, length(Expr)-length(Value)+1, length(Value)) = Value;
      lrContains  : Result := pos(Expr, Value) <> 1;
      lrContainsIn: Result := pos(Value, Expr) <> 1;
      ltNotEmpty  : Result := length(Expr) > 0;
    end;

    LogicVariant := Item.LogicVariants[i];
    if Result and (Item.LogicVariants[i].TrueResult > '') then break;
    if not Result and (Item.LogicVariants[i].FalseResult > '') then break;

  end;

  if Assigned(OnTraceMessage) then OnTraceMessage(self, Result, IIF(Result, Item.TrueResult, Item.FalseResult), ParseExpression(IIF(Result, Item.TrueResult, Item.FalseResult)), Item.Caption + '  :  ' + IIF(Result, 'TRUE', 'FALSE') + '  :  '  + IIF(Result, Item.TrueResult, Item.FalseResult));

end;

function TLogics.ParseExpression(const Value: string): string;
var i: integer;
begin
  Result := Value;
  Result := StringReplace(Result, '[RESULT]', self.Result, [rfReplaceAll, rfIgnoreCase]);
  for i := 0 to Dictionary.Count-1 do
    Result := StringReplace(Result, '[' + Dictionary.Names[i] + ']', Dictionary.Values[Dictionary.Names[i]], [rfReplaceAll, rfIgnoreCase]);

  i := 1;  
  while i <= length(Result) do
  begin
    if Result[i] = '[' then
    begin
      repeat
        Result[i] := '[';
        inc(i);
      until (i > length(Result)) or (Result[i] = ']');
      if (i <= length(Result)) and (Result[i] = ']') then Result[i] := '[';
    end;
    inc(i);
  end;
  Result := StringReplace(Result, '[', '', [rfReplaceAll]);
end;

procedure TLogics.SetDictionary(const Value: TStrings);
begin
  FDictionary.Assign(Value);
end;

procedure TLogicProducer.Loaded;
var i: integer;
begin
  inherited;
  Logics.Loaded;
end;

procedure TLogicProducer.SetOnTraceMessage(const Value: TOnTraceMessage);
begin
  Logics.OnTraceMessage := Value;
end;

function TLogicProducer.GetOnTraceMessage: TOnTraceMessage;
begin
  Result := Logics.OnTraceMessage;
end;

{ TCommentAreas }

function TCommentAreas.Add: TCommentArea;
begin
  Result := TCommentArea(inherited Add);
  Result.Text := 'Comments';
end;

function TCommentAreas.GetItem(Index: Integer): TCommentArea;
begin
  Result := TCommentArea(inherited Items[Index]);
end;

function TCommentAreas.Insert(Index: Integer): TCommentArea;
begin
  Result := TCommentArea(inherited Insert(Index));
end;

procedure TCommentAreas.SetItem(Index: Integer; Value: TCommentArea);
begin
  Items[Index].Assign(Value);
end;

{ TLogicVariants }

function TLogicVariants.Add: TLogicVariant;
begin
  Result := TLogicVariant(inherited Add);
end;

function TLogicVariants.GetItem(Index: Integer): TLogicVariant;
begin
  Result := TLogicVariant(inherited Items[Index]);
end;

function TLogicVariants.Insert(Index: Integer): TLogicVariant;
begin
  Result := TLogicVariant(inherited Insert(Index));
end;

procedure TLogicVariants.SetItem(Index: Integer; Value: TLogicVariant);
begin
  Items[Index].Assign(Value);
end;


end.

