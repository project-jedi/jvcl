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

UNIT JvgLogics;

INTERFACE
USES classes,
   jvComponent,
   sysUtils,
   Graphics;

TYPE

   TLogicRule = (lrEqual, lrBeginWith, lrEndWith, lrContains, lrContainsIn,
      ltNotEmpty);
CONST
   LogicRuleLabels               : ARRAY[TLogicRule] OF STRING = ('равно',
      'начинается с', 'оканчивается на', 'содержит', 'содержится в', 'не пусто');

TYPE
   TJvgLogics = CLASS;
   TJvgLogicElement = CLASS;
   TJvgCommentAreas = CLASS;
   TJvgLogicVariants = CLASS;
   TJvgLogicVariant = CLASS;
   TJvgLogicProducer = CLASS;

   TOnTraceMessage = PROCEDURE(Sender: TJvgLogics; fStepResult: boolean; CONST
      StepResult, ParsedResult, Msg: STRING) OF OBJECT;

   TJvgLogicProducer = CLASS(TJvComponent)
   PRIVATE
      FLogics: TJvgLogics;
      FCommentAreas: TJvgCommentAreas;
      FIgnoreSpaces: boolean;

      PROCEDURE SetLogics(CONST Value: TJvgLogics);
      PROCEDURE SetDictionary(CONST Value: TStrings);
      FUNCTION GetDictionary: TStrings;
      PROCEDURE SetCommentAreas(CONST Value: TJvgCommentAreas);
      PROCEDURE SetIgnoreSpaces(CONST Value: boolean);
      FUNCTION GetIgnoreSpaces: boolean;
      PROCEDURE SetOnTraceMessage(CONST Value: TOnTraceMessage);
      FUNCTION GetOnTraceMessage: TOnTraceMessage;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;
   PUBLISHED
      PROPERTY Logics: TJvgLogics READ FLogics WRITE SetLogics;
      PROPERTY CommentAreas: TJvgCommentAreas READ FCommentAreas WRITE
         SetCommentAreas;
      PROPERTY Dictionary: TStrings READ GetDictionary WRITE SetDictionary;
      PROPERTY IgnoreSpaces: boolean READ GetIgnoreSpaces WRITE SetIgnoreSpaces;
      PROPERTY OnTraceMessage: TOnTraceMessage READ GetOnTraceMessage WRITE
         SetOnTraceMessage;
   END;

   TJvgLogicElement = CLASS(TCollectionItem)
   PRIVATE
      FNextElementID: integer;
      FNextFalseElementID: integer;

      FNextElement: TJvgLogicElement;
      FNextFalseElement: TJvgLogicElement;

      FLeft: integer;
      FTop: integer;
      FCaption: STRING;
      FIsFirst: boolean;
      FValue: STRING;
      FExpression: STRING;
      FRule: TLogicRule;
      FTrueResult: STRING;
      FFalseResult: STRING;
      FLogicVariants: TJvgLogicVariants;

      FUNCTION GetNextElement: TJvgLogicElement;
      FUNCTION GetNextFalseElement: TJvgLogicElement;

      PROCEDURE SetCaption(CONST Value: STRING);
      PROCEDURE SetIsFirst(CONST Value: boolean);

      PROCEDURE SetNextElement(CONST Value: TJvgLogicElement);
      PROCEDURE SetNextFalseElement(CONST Value: TJvgLogicElement);
      PROCEDURE SetExpression(CONST Value: STRING);
      PROCEDURE SetRule(CONST Value: TLogicRule);
      PROCEDURE SetValue(CONST Value: STRING);
      PROCEDURE SetFalseResult(CONST Value: STRING);
      PROCEDURE SetTrueResult(CONST Value: STRING);
      PROCEDURE SetLogicVariants(CONST Value: TJvgLogicVariants);
   PUBLIC
      IsTrue: boolean;
      CONSTRUCTOR Create(Collection: TCollection); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Loaded;

      PROPERTY NextElement: TJvgLogicElement READ GetNextElement WRITE
         SetNextElement;
      PROPERTY NextFalseElement: TJvgLogicElement READ GetNextFalseElement WRITE
         SetNextFalseElement;

   PUBLISHED
      PROPERTY ID;
      PROPERTY NextElementID: integer READ FNextElementID WRITE FNextElementID
         DEFAULT -1;
      PROPERTY NextFalseElementID: integer READ FNextFalseElementID WRITE
         FNextFalseElementID DEFAULT -1;

      PROPERTY Left: integer READ FLeft WRITE FLeft;
      PROPERTY Top: integer READ FTop WRITE FTop;
      PROPERTY Caption: STRING READ FCaption WRITE SetCaption;
      PROPERTY IsFirst: boolean READ FIsFirst WRITE SetIsFirst;

      PROPERTY Expression: STRING READ FExpression WRITE SetExpression;
      PROPERTY Rule: TLogicRule READ FRule WRITE SetRule;
      PROPERTY Value: STRING READ FValue WRITE SetValue;
      PROPERTY TrueResult: STRING READ FTrueResult WRITE SetTrueResult;
      PROPERTY FalseResult: STRING READ FFalseResult WRITE SetFalseResult;
      PROPERTY LogicVariants: TJvgLogicVariants READ FLogicVariants WRITE
         SetLogicVariants;

   END;

   TJvgLogics = CLASS(TCollection)
   PRIVATE
      FResult: STRING;
      FDictionary: TStrings;
      FIgnoreSpaces: boolean;
      FOnTraceMessage: TOnTraceMessage;

      FUNCTION GetItem(Index: Integer): TJvgLogicElement;
      PROCEDURE SetItem(Index: Integer; Value: TJvgLogicElement);
      FUNCTION GetItemResult(Item: TJvgLogicElement; VAR LogicVariant:
         TJvgLogicVariant): boolean;
      PROCEDURE SetDictionary(CONST Value: TStrings);
      FUNCTION ParseExpression(CONST Value: STRING): STRING;

   PUBLIC
      TraceItem: TJvgLogicElement;

      CONSTRUCTOR Create(ItemClass: TCollectionItemClass);
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Loaded;

      PROCEDURE Analyze;
      PROCEDURE AnalyzeStep;
      PROCEDURE StartAnalyze;
      //    procedure Assign(StylePairs: TJvgLogics);
      FUNCTION Add: TJvgLogicElement;
      FUNCTION Insert(Index: Integer): TJvgLogicElement;
      PROPERTY Items[Index: Integer]: TJvgLogicElement READ GetItem WRITE
         SetItem; DEFAULT;
      PROPERTY Result: STRING READ FResult WRITE FResult;
   PUBLISHED
      PROPERTY Dictionary: TStrings READ FDictionary WRITE SetDictionary;
      PROPERTY IgnoreSpaces: boolean READ FIgnoreSpaces WRITE FIgnoreSpaces;
      PROPERTY OnTraceMessage: TOnTraceMessage READ FOnTraceMessage WRITE
         FOnTraceMessage;
   END;

   TJvgLogicVariant = CLASS(TCollectionItem)
   PRIVATE
      //    FExpression: string;
      FValue: STRING;
      FTrueResult: STRING;
      FFalseResult: STRING;
   PUBLISHED
      //    property Expression: string read FExpression write FExpression;
      PROPERTY Value: STRING READ FValue WRITE FValue;
      PROPERTY TrueResult: STRING READ FTrueResult WRITE FTrueResult;
      PROPERTY FalseResult: STRING READ FFalseResult WRITE FFalseResult;
   END;

   TJvgLogicVariants = CLASS(TCollection)
   PRIVATE
      FUNCTION GetItem(Index: Integer): TJvgLogicVariant;
      PROCEDURE SetItem(Index: Integer; Value: TJvgLogicVariant);
   PUBLIC
      FUNCTION Add: TJvgLogicVariant;
      FUNCTION Insert(Index: Integer): TJvgLogicVariant;
      PROPERTY Items[Index: Integer]: TJvgLogicVariant READ GetItem WRITE
         SetItem; DEFAULT;
   END;

   TJvgCommentArea = CLASS(TCollectionItem)
   PRIVATE
      FLeft: integer;
      FTop: integer;
      FWidth: integer;
      FHeight: integer;
      FText: STRING;
      FColor: TColor;
   PUBLISHED
      PROPERTY Left: integer READ FLeft WRITE FLeft;
      PROPERTY Top: integer READ FTop WRITE FTop;
      PROPERTY Width: integer READ FWidth WRITE FWidth;
      PROPERTY Height: integer READ FHeight WRITE FHeight;
      PROPERTY Text: STRING READ FText WRITE FText;
      PROPERTY Color: TColor READ FColor WRITE FColor;
   END;

   TJvgCommentAreas = CLASS(TCollection)
   PRIVATE
      FUNCTION GetItem(Index: Integer): TJvgCommentArea;
      PROCEDURE SetItem(Index: Integer; Value: TJvgCommentArea);
   PUBLIC
      FUNCTION Add: TJvgCommentArea;
      FUNCTION Insert(Index: Integer): TJvgCommentArea;
      PROPERTY Items[Index: Integer]: TJvgCommentArea READ GetItem WRITE
         SetItem; DEFAULT;
   END;

IMPLEMENTATION
USES JvgUtils;

CONSTRUCTOR TJvgLogicElement.Create(Collection: TCollection);
BEGIN
   INHERITED Create(Collection);
   Caption := 'Step ' + IntToStr(ID);
   FNextElementID := -1;
   FNextFalseElementID := -1;
   FLogicVariants := TJvgLogicVariants.Create(TJvgLogicVariant);
END;

DESTRUCTOR TJvgLogicElement.Destroy;
BEGIN
   FLogicVariants.Free;
   INHERITED;
END;

PROCEDURE TJvgLogicElement.Loaded;
BEGIN
   IF FLogicVariants.Count = 0 THEN
      WITH FLogicVariants.Add DO
      BEGIN
         //    FExpression := self.FExpression;
         FValue := self.FValue;
         FTrueResult := self.FTrueResult;
         FFalseResult := self.FFalseResult;
      END;
END;

CONSTRUCTOR TJvgLogics.Create(ItemClass: TCollectionItemClass);
BEGIN
   INHERITED Create(ItemClass);
   FDictionary := TStringList.Create;
END;

DESTRUCTOR TJvgLogics.Destroy;
BEGIN
   FDictionary.Free;
   INHERITED;
END;

PROCEDURE TJvgLogics.Loaded;
VAR
   i                          : integer;
BEGIN
   FOR i := 0 TO Count - 1 DO
      Items[i].Loaded;
END;

FUNCTION TJvgLogics.GetItem(Index: Integer): TJvgLogicElement;
BEGIN
   Result := TJvgLogicElement(INHERITED Items[Index]);
END;

PROCEDURE TJvgLogics.SetItem(Index: Integer; Value: TJvgLogicElement);
BEGIN
   Items[Index].Assign(Value);
END;

FUNCTION TJvgLogics.Add: TJvgLogicElement;
BEGIN
   Result := TJvgLogicElement(INHERITED Add);
END;

FUNCTION TJvgLogics.Insert(Index: Integer): TJvgLogicElement;
BEGIN
   Result := TJvgLogicElement(INHERITED Insert(Index));
END;

FUNCTION TJvgLogicElement.GetNextElement: TJvgLogicElement;
BEGIN
   Result := TJvgLogicElement(Collection.FindItemID(FNextElementID));
END;

FUNCTION TJvgLogicElement.GetNextFalseElement: TJvgLogicElement;
BEGIN
   Result := TJvgLogicElement(Collection.FindItemID(FNextFalseElementID));
END;

PROCEDURE TJvgLogicElement.SetCaption(CONST Value: STRING);
BEGIN
   FCaption := Value;
END;

PROCEDURE TJvgLogicElement.SetExpression(CONST Value: STRING);
BEGIN
   FExpression := Value;
END;

PROCEDURE TJvgLogicElement.SetFalseResult(CONST Value: STRING);
BEGIN
   FFalseResult := Value;
END;

PROCEDURE TJvgLogicElement.SetIsFirst(CONST Value: boolean);
BEGIN
   FIsFirst := Value;
END;

PROCEDURE TJvgLogicElement.SetLogicVariants(CONST Value: TJvgLogicVariants);
BEGIN
   FLogicVariants.Assign(Value);
END;

PROCEDURE TJvgLogicElement.SetNextElement(CONST Value: TJvgLogicElement);
BEGIN
   IF Value = NIL THEN
      FNextElementID := -1
   ELSE
      FNextElementID := Value.ID;
END;

PROCEDURE TJvgLogicElement.SetNextFalseElement(CONST Value: TJvgLogicElement);
BEGIN
   IF Value = NIL THEN
      FNextFalseElementID := -1
   ELSE
      FNextFalseElementID := Value.ID;
END;

{ TJvgLogicProducer }

CONSTRUCTOR TJvgLogicProducer.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FLogics := TJvgLogics.Create(TJvgLogicElement);
   FCommentAreas := TJvgCommentAreas.Create(TJvgCommentArea);
END;

DESTRUCTOR TJvgLogicProducer.Destroy;
BEGIN
   INHERITED;
   FLogics.Free;
END;

FUNCTION TJvgLogicProducer.GetDictionary: TStrings;
BEGIN
   Result := Logics.Dictionary;
END;

PROCEDURE TJvgLogicProducer.SetCommentAreas(CONST Value: TJvgCommentAreas);
BEGIN
   FCommentAreas.Assign(Value);
END;

PROCEDURE TJvgLogicProducer.SetDictionary(CONST Value: TStrings);
BEGIN
   Logics.Dictionary.Assign(Value);
END;

PROCEDURE TJvgLogicProducer.SetIgnoreSpaces(CONST Value: boolean);
BEGIN
   Logics.IgnoreSpaces := Value;
END;

FUNCTION TJvgLogicProducer.GetIgnoreSpaces: boolean;
BEGIN
   Result := Logics.IgnoreSpaces;
END;

PROCEDURE TJvgLogicProducer.SetLogics(CONST Value: TJvgLogics);
BEGIN
   FLogics := Value;
END;

PROCEDURE TJvgLogicElement.SetRule(CONST Value: TLogicRule);
BEGIN
   FRule := Value;
END;

PROCEDURE TJvgLogicElement.SetTrueResult(CONST Value: STRING);
BEGIN
   FTrueResult := Value;
END;

PROCEDURE TJvgLogicElement.SetValue(CONST Value: STRING);
BEGIN
   FValue := Value;
END;

PROCEDURE TJvgLogics.StartAnalyze;
BEGIN
   IF Count > 0 THEN
      TraceItem := Items[0]
   ELSE
      TraceItem := NIL;
END;

PROCEDURE TJvgLogics.AnalyzeStep;
VAR
   f                          : boolean;
   Item                       : TJvgLogicElement;
   LogicVariant               : TJvgLogicVariant;
BEGIN
   LogicVariant := NIL;

   IF Assigned(TraceItem) THEN
   BEGIN
      TraceItem.IsTrue := true;
      f := GetItemResult(TraceItem, LogicVariant);

      IF f THEN
         Result := Result + ParseExpression(LogicVariant.TrueResult)
      ELSE
         Result := Result + ParseExpression(LogicVariant.FalseResult);
      IF f THEN
         TraceItem := TraceItem.NextElement
      ELSE
         TraceItem := TraceItem.NextFalseElement;
   END;
END;

PROCEDURE TJvgLogics.Analyze;
VAR
   i                          : integer;
   Item                       : TJvgLogicElement;
BEGIN
   FOR i := 0 TO Count - 1 DO
      Items[i].IsTrue := false;

   Result := '';
   i := 0;
   TraceItem := Items[0];
   WHILE Assigned(TraceItem) AND (i < 1000) DO
   BEGIN
      AnalyzeStep;
      inc(i);
   END;
END;

FUNCTION TJvgLogics.GetItemResult(Item: TJvgLogicElement; VAR LogicVariant:
   TJvgLogicVariant): boolean;
VAR
   Expr, Value                : STRING;
   i                          : integer;
BEGIN
   Expr := ParseExpression(Item.Expression);
   IF IgnoreSpaces THEN
      Expr := trim(Expr);

   FOR i := 0 TO Item.LogicVariants.Count - 1 DO
   BEGIN
      Value := ParseExpression(Item.LogicVariants[i].Value);

      CASE Item.Rule OF
         lrEqual: Result := Expr = Value;
         lrBeginWith: Result := pos(Value, Expr) = 1;
         lrEndWith: Result := copy(Expr, length(Expr) - length(Value) + 1,
            length(Value)) = Value;
         lrContains: Result := pos(Expr, Value) <> 1;
         lrContainsIn: Result := pos(Value, Expr) <> 1;
         ltNotEmpty: Result := length(Expr) > 0;
      END;

      LogicVariant := Item.LogicVariants[i];
      IF Result AND (Item.LogicVariants[i].TrueResult > '') THEN
         break;
      IF NOT Result AND (Item.LogicVariants[i].FalseResult > '') THEN
         break;

   END;

   IF Assigned(OnTraceMessage) THEN
      OnTraceMessage(self, Result, IIF(Result, Item.TrueResult,
         Item.FalseResult), ParseExpression(IIF(Result, Item.TrueResult,
         Item.FalseResult)), Item.Caption + '  :  ' + IIF(Result, 'TRUE', 'FALSE') +
         '  :  ' + IIF(Result, Item.TrueResult, Item.FalseResult));

END;

FUNCTION TJvgLogics.ParseExpression(CONST Value: STRING): STRING;
VAR
   i                          : integer;
BEGIN
   Result := Value;
   Result := StringReplace(Result, '[RESULT]', self.Result, [rfReplaceAll,
      rfIgnoreCase]);
   FOR i := 0 TO Dictionary.Count - 1 DO
      Result := StringReplace(Result, '[' + Dictionary.Names[i] + ']',
         Dictionary.Values[Dictionary.Names[i]], [rfReplaceAll, rfIgnoreCase]);

   i := 1;
   WHILE i <= length(Result) DO
   BEGIN
      IF Result[i] = '[' THEN
      BEGIN
         REPEAT
            Result[i] := '[';
            inc(i);
         UNTIL (i > length(Result)) OR (Result[i] = ']');
         IF (i <= length(Result)) AND (Result[i] = ']') THEN
            Result[i] := '[';
      END;
      inc(i);
   END;
   Result := StringReplace(Result, '[', '', [rfReplaceAll]);
END;

PROCEDURE TJvgLogics.SetDictionary(CONST Value: TStrings);
BEGIN
   FDictionary.Assign(Value);
END;

PROCEDURE TJvgLogicProducer.Loaded;
VAR
   i                          : integer;
BEGIN
   INHERITED;
   Logics.Loaded;
END;

PROCEDURE TJvgLogicProducer.SetOnTraceMessage(CONST Value: TOnTraceMessage);
BEGIN
   Logics.OnTraceMessage := Value;
END;

FUNCTION TJvgLogicProducer.GetOnTraceMessage: TOnTraceMessage;
BEGIN
   Result := Logics.OnTraceMessage;
END;

{ TJvgCommentAreas }

FUNCTION TJvgCommentAreas.Add: TJvgCommentArea;
BEGIN
   Result := TJvgCommentArea(INHERITED Add);
   Result.Text := 'Comments';
END;

FUNCTION TJvgCommentAreas.GetItem(Index: Integer): TJvgCommentArea;
BEGIN
   Result := TJvgCommentArea(INHERITED Items[Index]);
END;

FUNCTION TJvgCommentAreas.Insert(Index: Integer): TJvgCommentArea;
BEGIN
   Result := TJvgCommentArea(INHERITED Insert(Index));
END;

PROCEDURE TJvgCommentAreas.SetItem(Index: Integer; Value: TJvgCommentArea);
BEGIN
   Items[Index].Assign(Value);
END;

{ TJvgLogicVariants }

FUNCTION TJvgLogicVariants.Add: TJvgLogicVariant;
BEGIN
   Result := TJvgLogicVariant(INHERITED Add);
END;

FUNCTION TJvgLogicVariants.GetItem(Index: Integer): TJvgLogicVariant;
BEGIN
   Result := TJvgLogicVariant(INHERITED Items[Index]);
END;

FUNCTION TJvgLogicVariants.Insert(Index: Integer): TJvgLogicVariant;
BEGIN
   Result := TJvgLogicVariant(INHERITED Insert(Index));
END;

PROCEDURE TJvgLogicVariants.SetItem(Index: Integer; Value: TJvgLogicVariant);
BEGIN
   Items[Index].Assign(Value);
END;

END.

