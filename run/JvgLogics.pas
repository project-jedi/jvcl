{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLogics.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgLogics;

{$I jvcl.inc}

interface

uses
  {$IFDEF USEJVCL}
  Classes, SysUtils, Graphics,
  JvComponent, JvResources;
  {$ELSE}
  Classes, SysUtils, Graphics;
  {$ENDIF USEJVCL}

{$IFNDEF USEJVCL}
resourcestring
  RsEqualTo = 'equal to';
  RsStartingWith = 'starting with';
  RsEndsWith = 'ends with';
  RsContains = 'contains';
  RsIsContainedWithin = 'is contained within';
  RsNotEmpty = 'not empty';
  RsStep = 'Step ';
  RsComments = 'Comments';
{$ENDIF !USEJVCL}

type
  TLogicRule = (lrEqual, lrBeginWith, lrEndWith, lrContains, lrContainsIn,
    ltNotEmpty);

const
  LogicRuleLabels: array [TLogicRule] of string = (RsEqualTo,
    RsStartingWith, RsEndsWith, RsContains, RsIsContainedWithin, RsNotEmpty);

type
  TJvgLogics = class;
  TJvgLogicElement = class;
  TJvgCommentAreas = class;
  TJvgLogicVariants = class;
  TJvgLogicVariant = class;
  TJvgLogicProducer = class;

  TOnTraceMessage = procedure(Sender: TJvgLogics; AStepResult: Boolean;
    const StepResult, ParsedResult, Msg: string) of object;

  {$IFDEF USEJVCL}
  TJvgLogicProducer = class(TJvComponent)
  {$ELSE}
  TJvgLogicProducer = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FLogics: TJvgLogics;
    FCommentAreas: TJvgCommentAreas;
    // FIgnoreSpaces: Boolean;
    procedure SetLogics(const Value: TJvgLogics);
    procedure SetDictionary(const Value: TStrings);
    function GetDictionary: TStrings;
    procedure SetCommentAreas(const Value: TJvgCommentAreas);
    procedure SetIgnoreSpaces(const Value: Boolean);
    function GetIgnoreSpaces: Boolean;
    procedure SetOnTraceMessage(const Value: TOnTraceMessage);
    function GetOnTraceMessage: TOnTraceMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Logics: TJvgLogics read FLogics write SetLogics;
    property CommentAreas: TJvgCommentAreas read FCommentAreas write SetCommentAreas;
    property Dictionary: TStrings read GetDictionary write SetDictionary;
    property IgnoreSpaces: Boolean read GetIgnoreSpaces write SetIgnoreSpaces;
    property OnTraceMessage: TOnTraceMessage read GetOnTraceMessage write SetOnTraceMessage;
  end;

  TJvgLogicElement = class(TCollectionItem)
  private
    FNextElementID: Integer;
    FNextFalseElementID: Integer;
    // FNextElement: TJvgLogicElement;
    // FNextFalseElement: TJvgLogicElement;
    FLeft: Integer;
    FTop: Integer;
    FCaption: string;
    FIsFirst: Boolean;
    FValue: string;
    FExpression: string;
    FRule: TLogicRule;
    FTrueResult: string;
    FFalseResult: string;
    FLogicVariants: TJvgLogicVariants;
    FIsTrue: Boolean;
    function GetNextElement: TJvgLogicElement;
    function GetNextFalseElement: TJvgLogicElement;
    procedure SetCaption(const Value: string);
    procedure SetIsFirst(const Value: Boolean);
    procedure SetNextElement(const Value: TJvgLogicElement);
    procedure SetNextFalseElement(const Value: TJvgLogicElement);
    procedure SetExpression(const Value: string);
    procedure SetRule(const Value: TLogicRule);
    procedure SetValue(const Value: string);
    procedure SetFalseResult(const Value: string);
    procedure SetTrueResult(const Value: string);
    procedure SetLogicVariants(const Value: TJvgLogicVariants);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Loaded;
    property NextElement: TJvgLogicElement read GetNextElement write SetNextElement;
    property NextFalseElement: TJvgLogicElement read GetNextFalseElement write SetNextFalseElement;
    property IsTrue: Boolean read FIsTrue write FIsTrue;
  published
    property ID;
    property NextElementID: Integer read FNextElementID write FNextElementID default -1;
    property NextFalseElementID: Integer read FNextFalseElementID write FNextFalseElementID default -1;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Caption: string read FCaption write SetCaption;
    property IsFirst: Boolean read FIsFirst write SetIsFirst;
    property Expression: string read FExpression write SetExpression;
    property Rule: TLogicRule read FRule write SetRule;
    property Value: string read FValue write SetValue;
    property TrueResult: string read FTrueResult write SetTrueResult;
    property FalseResult: string read FFalseResult write SetFalseResult;
    property LogicVariants: TJvgLogicVariants read FLogicVariants write SetLogicVariants;
  end;

  TJvgLogics = class(TOwnedCollection)
  private
    FResult: string;
    FDictionary: TStringList;
    FIgnoreSpaces: Boolean;
    FOnTraceMessage: TOnTraceMessage;
    FTraceItem: TJvgLogicElement;
    function GetItem(Index: Integer): TJvgLogicElement;
    procedure SetItem(Index: Integer; Value: TJvgLogicElement);
    function GetItemResult(Item: TJvgLogicElement; var LogicVariant: TJvgLogicVariant): Boolean;
    function GetDictionary: TStrings;
    procedure SetDictionary(const Value: TStrings);
    function ParseExpression(const Value: string): string;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    procedure Loaded;
    procedure Analyze;
    procedure AnalyzeStep;
    procedure StartAnalyze;
    // procedure Assign(StylePairs: TJvgLogics);
    function Add: TJvgLogicElement;
    function Insert(Index: Integer): TJvgLogicElement;
    property Items[Index: Integer]: TJvgLogicElement read GetItem write SetItem; default;
    property Result: string read FResult write FResult;
    property TraceItem: TJvgLogicElement read FTraceItem write FTraceItem;
  published
    property Dictionary: TStrings read GetDictionary write SetDictionary;
    property IgnoreSpaces: Boolean read FIgnoreSpaces write FIgnoreSpaces;
    property OnTraceMessage: TOnTraceMessage read FOnTraceMessage write FOnTraceMessage;
  end;

  TJvgLogicVariant = class(TCollectionItem)
  private
    // FExpression: string;
    FValue: string;
    FTrueResult: string;
    FFalseResult: string;
  published
    // property Expression: string read FExpression write FExpression;
    property Value: string read FValue write FValue;
    property TrueResult: string read FTrueResult write FTrueResult;
    property FalseResult: string read FFalseResult write FFalseResult;
  end;

  TJvgLogicVariants = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvgLogicVariant;
    procedure SetItem(Index: Integer; Value: TJvgLogicVariant);
  public
    function Add: TJvgLogicVariant;
    function Insert(Index: Integer): TJvgLogicVariant;
    property Items[Index: Integer]: TJvgLogicVariant read GetItem write SetItem; default;
  end;

  TJvgCommentArea = class(TCollectionItem)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FText: string;
    FColor: TColor;
  published
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Text: string read FText write FText;
    property Color: TColor read FColor write FColor;
  end;

  TJvgCommentAreas = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvgCommentArea;
    procedure SetItem(Index: Integer; Value: TJvgCommentArea);
  public
    function Add: TJvgCommentArea;
    function Insert(Index: Integer): TJvgCommentArea;
    property Items[Index: Integer]: TJvgCommentArea read GetItem write SetItem; default;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvConsts,
  {$ENDIF USEJVCL}
  JvgUtils;

//=== { TJvgLogicElement } ===================================================

constructor TJvgLogicElement.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Caption := RsStep + IntToStr(ID);
  FNextElementID := -1;
  FNextFalseElementID := -1;
  FLogicVariants := TJvgLogicVariants.Create(Collection, TJvgLogicVariant);
end;

destructor TJvgLogicElement.Destroy;
begin
  FLogicVariants.Free;
  inherited Destroy;
end;

procedure TJvgLogicElement.Loaded;
begin
  if FLogicVariants.Count = 0 then
    with FLogicVariants.Add do
    begin
      // FExpression := Self.FExpression;
      FValue := Self.Value;
      FTrueResult := Self.TrueResult;
      FFalseResult := Self.FalseResult;
    end;
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

procedure TJvgLogicElement.SetIsFirst(const Value: Boolean);
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

//=== { TJvgLogics } =========================================================

constructor TJvgLogics.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FDictionary := TStringList.Create;
end;

destructor TJvgLogics.Destroy;
begin
  FDictionary.Free;
  inherited Destroy;
end;

procedure TJvgLogics.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Loaded;
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

procedure TJvgLogics.StartAnalyze;
begin
  if Count > 0 then
    TraceItem := Items[0]
  else
    TraceItem := nil;
end;

procedure TJvgLogics.AnalyzeStep;
var
  LogicVariant: TJvgLogicVariant;
begin
  LogicVariant := nil;
  if Assigned(TraceItem) then
  begin
    TraceItem.IsTrue := True;
    if GetItemResult(TraceItem, LogicVariant) then
    begin
      Result := Result + ParseExpression(LogicVariant.TrueResult);
      TraceItem := TraceItem.NextElement;
    end
    else
    begin
      Result := Result + ParseExpression(LogicVariant.FalseResult);
      TraceItem := TraceItem.NextFalseElement;
    end;
  end;
end;

procedure TJvgLogics.Analyze;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].IsTrue := False;

  Result := '';
  I := 0;
  TraceItem := Items[0];
  while Assigned(TraceItem) and (I < 1000) do
  begin
    AnalyzeStep;
    Inc(I);
  end;
end;

function TJvgLogics.GetItemResult(Item: TJvgLogicElement;
  var LogicVariant: TJvgLogicVariant): Boolean;
var
  Expr, Value: string;
  I: Integer;
begin
  Result := False;
  Expr := ParseExpression(Item.Expression);
  if IgnoreSpaces then
    Expr := Trim(Expr);

  for I := 0 to Item.LogicVariants.Count - 1 do
  begin
    Value := ParseExpression(Item.LogicVariants[I].Value);

    case Item.Rule of
      lrEqual:
        Result := Expr = Value;
      lrBeginWith:
        Result := Pos(Value, Expr) = 1;
      lrEndWith:
        Result := Copy(Expr, Length(Expr) - Length(Value) + 1, Length(Value)) = Value;
      lrContains:
        Result := Pos(Expr, Value) <> 1;
      lrContainsIn:
        Result := Pos(Value, Expr) <> 1;
      ltNotEmpty:
        Result := Length(Expr) > 0;
    end;

    LogicVariant := Item.LogicVariants[I];
    if Result and (Item.LogicVariants[I].TrueResult > '') then
      Break;
    if not Result and (Item.LogicVariants[I].FalseResult > '') then
      Break;
  end;

  if Assigned(FOnTraceMessage) then
    FOnTraceMessage(Self, Result,
      IIF(Result, Item.TrueResult, Item.FalseResult),
      ParseExpression(IIF(Result, Item.TrueResult, Item.FalseResult)),
      Item.Caption + '  :  ' + IIF(Result, 'TRUE', 'FALSE') +
      '  :  ' + IIF(Result, Item.TrueResult, Item.FalseResult));
end;

function TJvgLogics.ParseExpression(const Value: string): string;
var
  I: Integer;
begin
  Result := Value;
  Result := StringReplace(Result, '[RESULT]', Self.Result,
    [rfReplaceAll, rfIgnoreCase]);
  for I := 0 to Dictionary.Count - 1 do
    Result := StringReplace(Result, '[' + Dictionary.Names[I] + ']',
      Dictionary.Values[Dictionary.Names[I]], [rfReplaceAll, rfIgnoreCase]);

  I := 1;
  while I <= Length(Result) do
  begin
    if Result[I] = '[' then
    begin
      repeat
        Result[I] := '[';
        Inc(I);
      until (I > Length(Result)) or (Result[I] = ']');
      if (I <= Length(Result)) and (Result[I] = ']') then
        Result[I] := '[';
    end;
    Inc(I);
  end;
  Result := StringReplace(Result, '[', '', [rfReplaceAll]);
end;

function TJvgLogics.GetDictionary: TStrings;
begin
  Result := FDictionary;
end;

procedure TJvgLogics.SetDictionary(const Value: TStrings);
begin
  FDictionary.Assign(Value);
end;

//=== { TJvgLogicProducer } ==================================================

constructor TJvgLogicProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogics := TJvgLogics.Create(Self, TJvgLogicElement);
  FCommentAreas := TJvgCommentAreas.Create(Self, TJvgCommentArea);
end;

destructor TJvgLogicProducer.Destroy;
begin
  FLogics.Free;
  inherited Destroy;
end;

procedure TJvgLogicProducer.Loaded;
begin
  inherited Loaded;
  Logics.Loaded;
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

procedure TJvgLogicProducer.SetIgnoreSpaces(const Value: Boolean);
begin
  Logics.IgnoreSpaces := Value;
end;

function TJvgLogicProducer.GetIgnoreSpaces: Boolean;
begin
  Result := Logics.IgnoreSpaces;
end;

procedure TJvgLogicProducer.SetLogics(const Value: TJvgLogics);
begin
  FLogics := Value;
end;

procedure TJvgLogicProducer.SetOnTraceMessage(const Value: TOnTraceMessage);
begin
  Logics.OnTraceMessage := Value;
end;

function TJvgLogicProducer.GetOnTraceMessage: TOnTraceMessage;
begin
  Result := Logics.OnTraceMessage;
end;

//=== { TJvgCommentAreas } ===================================================

function TJvgCommentAreas.Add: TJvgCommentArea;
begin
  Result := TJvgCommentArea(inherited Add);
  Result.Text := RsComments;
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

//=== { TJvgLogicVariants } ==================================================

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

