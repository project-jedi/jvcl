{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLogics.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgLogics;

interface
uses classes,
  jvComponent,
  sysUtils,
  Graphics;

type

  TLogicRule = (lrEqual, lrBeginWith, lrEndWith, lrContains, lrContainsIn,
    ltNotEmpty);
const
  LogicRuleLabels: array[TLogicRule] of string = ('равно',
    'начинается с', 'оканчивается на', 'содержит', 'содержится в', 'не пусто');

type
  TJvgLogics = class;
  TJvgLogicElement = class;
  TJvgCommentAreas = class;
  TJvgLogicVariants = class;
  TJvgLogicVariant = class;
  TJvgLogicProducer = class;

  TOnTraceMessage = procedure(Sender: TJvgLogics; fStepResult: boolean; const
    StepResult, ParsedResult, Msg: string) of object;

  TJvgLogicProducer = class(TJvComponent)
  private
    FLogics: TJvgLogics;
    FCommentAreas: TJvgCommentAreas;
    FIgnoreSpaces: boolean;

    procedure SetLogics(const Value: TJvgLogics);
    procedure SetDictionary(const Value: TStrings);
    function GetDictionary: TStrings;
    procedure SetCommentAreas(const Value: TJvgCommentAreas);
    procedure SetIgnoreSpaces(const Value: boolean);
    function GetIgnoreSpaces: boolean;
    procedure SetOnTraceMessage(const Value: TOnTraceMessage);
    function GetOnTraceMessage: TOnTraceMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Logics: TJvgLogics read FLogics write SetLogics;
    property CommentAreas: TJvgCommentAreas read FCommentAreas write
      SetCommentAreas;
    property Dictionary: TStrings read GetDictionary write SetDictionary;
    property IgnoreSpaces: boolean read GetIgnoreSpaces write SetIgnoreSpaces;
    property OnTraceMessage: TOnTraceMessage read GetOnTraceMessage write
      SetOnTraceMessage;
  end;

  TJvgLogicElement = class(TCollectionItem)
  private
    FNextElementID: integer;
    FNextFalseElementID: integer;

    FNextElement: TJvgLogicElement;
    FNextFalseElement: TJvgLogicElement;

    FLeft: integer;
    FTop: integer;
    FCaption: string;
    FIsFirst: boolean;
    FValue: string;
    FExpression: string;
    FRule: TLogicRule;
    FTrueResult: string;
    FFalseResult: string;
    FLogicVariants: TJvgLogicVariants;

    function GetNextElement: TJvgLogicElement;
    function GetNextFalseElement: TJvgLogicElement;

    procedure SetCaption(const Value: string);
    procedure SetIsFirst(const Value: boolean);

    procedure SetNextElement(const Value: TJvgLogicElement);
    procedure SetNextFalseElement(const Value: TJvgLogicElement);
    procedure SetExpression(const Value: string);
    procedure SetRule(const Value: TLogicRule);
    procedure SetValue(const Value: string);
    procedure SetFalseResult(const Value: string);
    procedure SetTrueResult(const Value: string);
    procedure SetLogicVariants(const Value: TJvgLogicVariants);
  public
    IsTrue: boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Loaded;

    property NextElement: TJvgLogicElement read GetNextElement write
      SetNextElement;
    property NextFalseElement: TJvgLogicElement read GetNextFalseElement write
      SetNextFalseElement;

  published
    property ID;
    property NextElementID: integer read FNextElementID write FNextElementID
      default -1;
    property NextFalseElementID: integer read FNextFalseElementID write
      FNextFalseElementID default -1;

    property Left: integer read FLeft write FLeft;
    property Top: integer read FTop write FTop;
    property Caption: string read FCaption write SetCaption;
    property IsFirst: boolean read FIsFirst write SetIsFirst;

    property Expression: string read FExpression write SetExpression;
    property Rule: TLogicRule read FRule write SetRule;
    property Value: string read FValue write SetValue;
    property TrueResult: string read FTrueResult write SetTrueResult;
    property FalseResult: string read FFalseResult write SetFalseResult;
    property LogicVariants: TJvgLogicVariants read FLogicVariants write
      SetLogicVariants;

  end;

  TJvgLogics = class(TCollection)
  private
    FResult: string;
    FDictionary: TStrings;
    FIgnoreSpaces: boolean;
    FOnTraceMessage: TOnTraceMessage;

    function GetItem(Index: Integer): TJvgLogicElement;
    procedure SetItem(Index: Integer; Value: TJvgLogicElement);
    function GetItemResult(Item: TJvgLogicElement; var LogicVariant:
      TJvgLogicVariant): boolean;
    procedure SetDictionary(const Value: TStrings);
    function ParseExpression(const Value: string): string;

  public
    TraceItem: TJvgLogicElement;

    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    procedure Loaded;

    procedure Analyze;
    procedure AnalyzeStep;
    procedure StartAnalyze;
    //    procedure Assign(StylePairs: TJvgLogics);
    function Add: TJvgLogicElement;
    function Insert(Index: Integer): TJvgLogicElement;
    property Items[Index: Integer]: TJvgLogicElement read GetItem write
    SetItem; default;
    property Result: string read FResult write FResult;
  published
    property Dictionary: TStrings read FDictionary write SetDictionary;
    property IgnoreSpaces: boolean read FIgnoreSpaces write FIgnoreSpaces;
    property OnTraceMessage: TOnTraceMessage read FOnTraceMessage write
      FOnTraceMessage;
  end;

  TJvgLogicVariant = class(TCollectionItem)
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

  TJvgLogicVariants = class(TCollection)
  private
    function GetItem(Index: Integer): TJvgLogicVariant;
    procedure SetItem(Index: Integer; Value: TJvgLogicVariant);
  public
    function Add: TJvgLogicVariant;
    function Insert(Index: Integer): TJvgLogicVariant;
    property Items[Index: Integer]: TJvgLogicVariant read GetItem write
    SetItem; default;
  end;

  TJvgCommentArea = class(TCollectionItem)
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

  TJvgCommentAreas = class(TCollection)
  private
    function GetItem(Index: Integer): TJvgCommentArea;
    procedure SetItem(Index: Integer; Value: TJvgCommentArea);
  public
    function Add: TJvgCommentArea;
    function Insert(Index: Integer): TJvgCommentArea;
    property Items[Index: Integer]: TJvgCommentArea read GetItem write
    SetItem; default;
  end;

implementation
uses JvgUtils;

constructor TJvgLogicElement.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Caption := 'Step ' + IntToStr(ID);
  FNextElementID := -1;
  FNextFalseElementID := -1;
  FLogicVariants := TJvgLogicVariants.Create(TJvgLogicVariant);
end;

destructor TJvgLogicElement.Destroy;
begin
  FLogicVariants.Free;
  inherited;
end;

procedure TJvgLogicElement.Loaded;
begin
  if FLogicVariants.Count = 0 then
    with FLogicVariants.Add do
    begin
      //    FExpression := self.FExpression;
      FValue := self.FValue;
      FTrueResult := self.FTrueResult;
      FFalseResult := self.FFalseResult;
    end;
end;

constructor TJvgLogics.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FDictionary := TStringList.Create;
end;

destructor TJvgLogics.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

procedure TJvgLogics.Loaded;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Loaded;
end;

function TJvgLogics.GetItem(Index: Integer): TJvgLogicElement;
begin
  Result := TJvgLogicElement(inherited Items[Index]);
end;

procedure TJvgLogics.SetItem(Index: Integer; Value: TJvgLogicElement);
begin
  Items[Index].Assign(Value);
end;

function TJvgLogics.Add: TJvgLogicElement;
begin
  Result := TJvgLogicElement(inherited Add);
end;

function TJvgLogics.Insert(Index: Integer): TJvgLogicElement;
begin
  Result := TJvgLogicElement(inherited Insert(Index));
end;

function TJvgLogicElement.GetNextElement: TJvgLogicElement;
begin
  Result := TJvgLogicElement(Collection.FindItemID(FNextElementID));
end;

function TJvgLogicElement.GetNextFalseElement: TJvgLogicElement;
begin
  Result := TJvgLogicElement(Collection.FindItemID(FNextFalseElementID));
end;

procedure TJvgLogicElement.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TJvgLogicElement.SetExpression(const Value: string);
begin
  FExpression := Value;
end;

procedure TJvgLogicElement.SetFalseResult(const Value: string);
begin
  FFalseResult := Value;
end;

procedure TJvgLogicElement.SetIsFirst(const Value: boolean);
begin
  FIsFirst := Value;
end;

procedure TJvgLogicElement.SetLogicVariants(const Value: TJvgLogicVariants);
begin
  FLogicVariants.Assign(Value);
end;

procedure TJvgLogicElement.SetNextElement(const Value: TJvgLogicElement);
begin
  if Value = nil then
    FNextElementID := -1
  else
    FNextElementID := Value.ID;
end;

procedure TJvgLogicElement.SetNextFalseElement(const Value: TJvgLogicElement);
begin
  if Value = nil then
    FNextFalseElementID := -1
  else
    FNextFalseElementID := Value.ID;
end;

{ TJvgLogicProducer }

constructor TJvgLogicProducer.Create(AOwner: TComponent);
begin
  inherited;
  FLogics := TJvgLogics.Create(TJvgLogicElement);
  FCommentAreas := TJvgCommentAreas.Create(TJvgCommentArea);
end;

destructor TJvgLogicProducer.Destroy;
begin
  inherited;
  FLogics.Free;
end;

function TJvgLogicProducer.GetDictionary: TStrings;
begin
  Result := Logics.Dictionary;
end;

procedure TJvgLogicProducer.SetCommentAreas(const Value: TJvgCommentAreas);
begin
  FCommentAreas.Assign(Value);
end;

procedure TJvgLogicProducer.SetDictionary(const Value: TStrings);
begin
  Logics.Dictionary.Assign(Value);
end;

procedure TJvgLogicProducer.SetIgnoreSpaces(const Value: boolean);
begin
  Logics.IgnoreSpaces := Value;
end;

function TJvgLogicProducer.GetIgnoreSpaces: boolean;
begin
  Result := Logics.IgnoreSpaces;
end;

procedure TJvgLogicProducer.SetLogics(const Value: TJvgLogics);
begin
  FLogics := Value;
end;

procedure TJvgLogicElement.SetRule(const Value: TLogicRule);
begin
  FRule := Value;
end;

procedure TJvgLogicElement.SetTrueResult(const Value: string);
begin
  FTrueResult := Value;
end;

procedure TJvgLogicElement.SetValue(const Value: string);
begin
  FValue := Value;
end;

procedure TJvgLogics.StartAnalyze;
begin
  if Count > 0 then
    TraceItem := Items[0]
  else
    TraceItem := nil;
end;

procedure TJvgLogics.AnalyzeStep;
var
  f: boolean;
  Item: TJvgLogicElement;
  LogicVariant: TJvgLogicVariant;
begin
  LogicVariant := nil;

  if Assigned(TraceItem) then
  begin
    TraceItem.IsTrue := true;
    f := GetItemResult(TraceItem, LogicVariant);

    if f then
      Result := Result + ParseExpression(LogicVariant.TrueResult)
    else
      Result := Result + ParseExpression(LogicVariant.FalseResult);
    if f then
      TraceItem := TraceItem.NextElement
    else
      TraceItem := TraceItem.NextFalseElement;
  end;
end;

procedure TJvgLogics.Analyze;
var
  i: integer;
  Item: TJvgLogicElement;
begin
  for i := 0 to Count - 1 do
    Items[i].IsTrue := false;

  Result := '';
  i := 0;
  TraceItem := Items[0];
  while Assigned(TraceItem) and (i < 1000) do
  begin
    AnalyzeStep;
    inc(i);
  end;
end;

function TJvgLogics.GetItemResult(Item: TJvgLogicElement; var LogicVariant:
  TJvgLogicVariant): boolean;
var
  Expr, Value: string;
  i: integer;
begin
  Expr := ParseExpression(Item.Expression);
  if IgnoreSpaces then
    Expr := trim(Expr);

  for i := 0 to Item.LogicVariants.Count - 1 do
  begin
    Value := ParseExpression(Item.LogicVariants[i].Value);

    case Item.Rule of
      lrEqual: Result := Expr = Value;
      lrBeginWith: Result := pos(Value, Expr) = 1;
      lrEndWith: Result := copy(Expr, length(Expr) - length(Value) + 1,
          length(Value)) = Value;
      lrContains: Result := pos(Expr, Value) <> 1;
      lrContainsIn: Result := pos(Value, Expr) <> 1;
      ltNotEmpty: Result := length(Expr) > 0;
    end;

    LogicVariant := Item.LogicVariants[i];
    if Result and (Item.LogicVariants[i].TrueResult > '') then
      break;
    if not Result and (Item.LogicVariants[i].FalseResult > '') then
      break;

  end;

  if Assigned(OnTraceMessage) then
    OnTraceMessage(self, Result, IIF(Result, Item.TrueResult,
      Item.FalseResult), ParseExpression(IIF(Result, Item.TrueResult,
      Item.FalseResult)), Item.Caption + '  :  ' + IIF(Result, 'TRUE', 'FALSE') +
      '  :  ' + IIF(Result, Item.TrueResult, Item.FalseResult));

end;

function TJvgLogics.ParseExpression(const Value: string): string;
var
  i: integer;
begin
  Result := Value;
  Result := StringReplace(Result, '[RESULT]', self.Result, [rfReplaceAll,
    rfIgnoreCase]);
  for i := 0 to Dictionary.Count - 1 do
    Result := StringReplace(Result, '[' + Dictionary.Names[i] + ']',
      Dictionary.Values[Dictionary.Names[i]], [rfReplaceAll, rfIgnoreCase]);

  i := 1;
  while i <= length(Result) do
  begin
    if Result[i] = '[' then
    begin
      repeat
        Result[i] := '[';
        inc(i);
      until (i > length(Result)) or (Result[i] = ']');
      if (i <= length(Result)) and (Result[i] = ']') then
        Result[i] := '[';
    end;
    inc(i);
  end;
  Result := StringReplace(Result, '[', '', [rfReplaceAll]);
end;

procedure TJvgLogics.SetDictionary(const Value: TStrings);
begin
  FDictionary.Assign(Value);
end;

procedure TJvgLogicProducer.Loaded;
var
  i: integer;
begin
  inherited;
  Logics.Loaded;
end;

procedure TJvgLogicProducer.SetOnTraceMessage(const Value: TOnTraceMessage);
begin
  Logics.OnTraceMessage := Value;
end;

function TJvgLogicProducer.GetOnTraceMessage: TOnTraceMessage;
begin
  Result := Logics.OnTraceMessage;
end;

{ TJvgCommentAreas }

function TJvgCommentAreas.Add: TJvgCommentArea;
begin
  Result := TJvgCommentArea(inherited Add);
  Result.Text := 'Comments';
end;

function TJvgCommentAreas.GetItem(Index: Integer): TJvgCommentArea;
begin
  Result := TJvgCommentArea(inherited Items[Index]);
end;

function TJvgCommentAreas.Insert(Index: Integer): TJvgCommentArea;
begin
  Result := TJvgCommentArea(inherited Insert(Index));
end;

procedure TJvgCommentAreas.SetItem(Index: Integer; Value: TJvgCommentArea);
begin
  Items[Index].Assign(Value);
end;

{ TJvgLogicVariants }

function TJvgLogicVariants.Add: TJvgLogicVariant;
begin
  Result := TJvgLogicVariant(inherited Add);
end;

function TJvgLogicVariants.GetItem(Index: Integer): TJvgLogicVariant;
begin
  Result := TJvgLogicVariant(inherited Items[Index]);
end;

function TJvgLogicVariants.Insert(Index: Integer): TJvgLogicVariant;
begin
  Result := TJvgLogicVariant(inherited Insert(Index));
end;

procedure TJvgLogicVariants.SetItem(Index: Integer; Value: TJvgLogicVariant);
begin
  Items[Index].Assign(Value);
end;

end.
