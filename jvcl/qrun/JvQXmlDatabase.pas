{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXMLDatabase.PAS, released on 2003-06-22.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2003 Sébastien Buysse.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  -You have to place the columns you test in the where clause in the select clause too
  -Where conditions *MUST* be enclosed between parenthesis as ... WHERE (Col = 5) AND (Col2 < Col3) ...
  -Update statements are limited to simple operations like ... SET Col1 = Col1 + 1, Col2 = 4 ...
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.Inc}

unit JvQXmlDatabase;

interface

uses
  SysUtils, Classes, Contnrs, Math, 
  DateUtils, 
  JvQTypes, JvQComponent, JvQSimpleXml;

type
  TJvXMLDatabase = class;
  TJvXMLQuery = class;
  TJvXMLQueryParser = class;
  TJvXMLDatabaseException = class(EJVCLException);

  TJvXMLTable = class
  public
    XML: TJvSimpleXML;
    Locked: Boolean;
    Filename: string;
  end;

  TJvXMLQueryTable = class
  public
    Name: string;
    Alias: string;
    constructor Create(const AValue: string);
  end;

  TJvXMLQueryColumn = class
  public
    Name: string;
    Table: string;
    constructor Create(const AValue: string);
  end;

  TJvXMLOrderConvertion = (ocNone, ocDate, ocInteger, ocFloat);
  TJvXMLQueryOrder = class
  public
    Column: string;
    Ascending: Boolean;
    Convertion: TJvXMLOrderConvertion;
    constructor Create(const AValue: string);
  end;

  TJvXMLSQLOperator = (opEquals, opGreater, opSmaller, opGreaterEquals,
    opSmallerEquals, opLike, opNot, opOr, opAnd, opXor, opLeftParenthesis,
    opRightParenthesis, opConstant, opColumn, opNull, opNone);
  TJvXMLQueryCondition = class
  public
    Condition: string;
    Operator: TJvXMLSQLOperator;
    constructor Create(AOperator: TJvXMLSQLOperator; const ACondition: string = '');
  end;

  TJvXMLSetKind = (skConstant, skColumn);
  TJvXMLSetOperator = (soNone, soAdd, soMultiply, soDivide, soSubstract);
  TJvXMLQueryAssignement = class
  public
    Column: string;
    ValueKind, SecondKind: TJvXMLSetKind;
    Operator: TJvXMLSetOperator;
    Value, SecondValue: string;
    constructor Create(AValue: string);
    procedure UpdateElem(AElement: TJvSimpleXMLElem);
  end;

  TJvXMLInstruction = (xiSelect, xiUpdate, xiInsert, xiDelete);
  TJvXMLQueryParser = class
  private
    FQuery: string;
    FTables: TObjectList;
    FColumns: TObjectList;
    FConditions: TObjectList;
    FOrders: TObjectList;
    FInstruction: TJvXMLInstruction;
    FInstructionStr: string;
    FTablesStr: string;
    FWhereStr: string;
    FColumnsStr: string;
    FLimitStr: string;
    FLimitBegin: Integer;
    FLimitCount: Integer;
    FOrderStr: string;
    FSetStr: string;
    FOrderTable: TJvSimpleXMLElem;
    FUpdates: TObjectList;
    FValuesStr: string;
    FValues: TStringList;
    function GetColumn(const AIndex: Integer): TJvXMLQueryColumn;
    function GetTable(const AIndex: Integer): TJvXMLQueryTable;
    function GetColumnsCount: Integer;
    function GetTablesCount: Integer;
    function GetCondition(const AIndex: Integer): TJvXMLQueryCondition;
    function GetConditionsCount: Integer;
    function OrderCallBack(Elems: TJvSimpleXMLElems; Index1, Index2: Integer): Integer;
    function GetValue(const AIndex: Integer): string;
    function GetValuesCount: Integer;
  protected
    function ReadToken: string;
    function ReadColumns(const AEndStatement: array of string; ACanTerminate: Boolean): string;
    function ReadTables(const AEndStatement: array of string): string;
    function ReadWhere(const AEndStatement: array of string): string;
    function ReadLimit(const AEndStatement: array of string): string;
    function ReadOrderBy(const AEndStatement: array of string): string;
    function ReadSet(const AEndStatement: array of string): string;
    function ReadValues(const AEndStatement: array of string): string;
    function ReadStatement(const AEndStatement: array of string; ACanTerminate: Boolean;
      var AValue: string): string;
    procedure DoValidateInstruction;
    procedure DoValidateColumns;
    procedure DoValidateTables;
    procedure DoValidateWhere;
    procedure DoValidateOrderBy;
    procedure DoValidateSet;
    procedure DoValidateValues;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Parse(const AQuery: string);
    function CheckConditions(AXMLElem: TJvSimpleXMLElem): Boolean;
    procedure LimitTable(var ATable: TJvSimpleXMLElem);
    procedure OrderTable(var ATable: TJvSimpleXMLElem);
    procedure UpdateRow(ARow: TJvSimpleXMLElem);
    property Instruction: TJvXMLInstruction read FInstruction write FInstruction;
    property Tables[const AIndex: Integer]: TJvXMLQueryTable read GetTable;
    property TablesCount: Integer read GetTablesCount;
    property Columns[const AIndex: Integer]: TJvXMLQueryColumn read GetColumn;
    property ColumnsCount: Integer read GetColumnsCount;
    property Condition[const AIndex: Integer]: TJvXMLQueryCondition read GetCondition;
    property ConditionsCount: Integer read GetConditionsCount;
    property Value[const AIndex: Integer]: string read GetValue;
    property ValuesCount: Integer read GetValuesCount;
  end;

  TJvXMLQuery = class
  private
    FParser: TJvXMLQueryParser;
    FDatabase: TJvXMLDatabase;
    FResults: TJvSimpleXMLElem;
    FTables: TList;
    FLastId: Integer;
  protected
    procedure Query(const AQuery: string);
  public
    constructor Create(AOwner: TJvXMLDatabase);
    destructor Destroy; override;

    property Results: TJvSimpleXMLElem read FResults;
    property LastId: Integer read FLastId;
  end;

  TJvXMLDatabase = class(TJvComponent)
  private
    FTablesPath: string;
    FTables: TObjectList;
  protected
    function GetTable(const AName: string): TJvSimpleXML;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure SaveTables;
    function Query(const AQuery: string): TJvXMLQuery;
    property TablesPath: string read FTablesPath write FTablesPath;
  end;

implementation

uses
  JvQJCLUtils, JvQResources;

constructor TJvXMLDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTables := TObjectList.Create;
end;
{**********************************************************************}
destructor TJvXMLDatabase.Destroy;
begin
  FTables.Free;
  inherited Destroy;
end;
{**********************************************************************}
function TJvXMLDatabase.GetTable(const AName: string): TJvSimpleXML;
var
  i: Integer;
  st: string;
  lTable: TJvXMLTable;
begin
  st := TablesPath + AName;
  for i := 0 to FTables.Count-1 do
    if TJvXMLTable(FTables[i]).Filename = st then
    begin
      Result := TJvXMLTable(FTables[i]).XML;
      Exit;
    end;

  lTable := TJvXMLTable.Create;
  lTable.XML := TJvSimpleXML.Create(nil);
  lTable.XML.LoadFromFile(st);
  lTable.Locked := False;
  lTable.Filename := st;
  FTables.Add(lTable);
  Result := lTable.XML;
end;
{**********************************************************************}
function TJvXMLDatabase.Query(const AQuery: string): TJvXMLQuery;
begin
  Result := TJvXMLQuery.Create(Self);
  Result.Query(AQuery);
end;
{**********************************************************************}
procedure TJvXMLDatabase.SaveTables;
var
  i: Integer;
begin
  for i := 0 to FTables.Count-1 do
    TJvXMLTable(FTables[i]).XML.SaveToFile(TJvXMLTable(FTables[i]).Filename);
end;
{**********************************************************************}

{ TJvXMLQuery }

{**********************************************************************}
constructor TJvXMLQuery.Create(AOwner: TJvXMLDatabase);
begin
  inherited Create;
  FDatabase := AOwner;
  FParser := TJvXMLQueryParser.Create;
  FResults := TJvSimpleXMLElemClassic.Create(nil);
  FTables := TList.Create;
end;
{**********************************************************************}
destructor TJvXMLQuery.Destroy;
begin
  FParser.Free;
  FResults.Free;
  FTables.Free;
  inherited Destroy;
end;
{**********************************************************************}
procedure TJvXMLQuery.Query(const AQuery: string);
var
  i, j, lMax: Integer;
  lElem: TJvSimpleXMLElemClassic;
  lValue: string;

  function IsColumnSelected(const ATable, AColumn: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to FParser.ColumnsCount-1 do
      if (FParser.Columns[i].Name = '*') or ((FParser.Columns[i].Name = AColumn) and
         ((FParser.Columns[i].Table = '') or (FParser.Columns[i].Table = ATable))) then
      begin
        Result := True;
        Break;
      end;
  end;

  procedure ConstructTable(AIndex: Integer; var AElem: TJvSimpleXMLElemClassic);
  var
    i, j: Integer;
    lElem: TJvSimpleXMLElemClassic;
  begin
    if AIndex >= FTables.Count then
    begin
      if FParser.CheckConditions(AElem) then
        FResults.Items.Add(AElem)
      else
        AElem.Free;
    end
    else
    begin
      with TJvSimpleXML(FTables[AIndex]) do
        for i := 0 to Root.Items.Count-1 do
        begin
          lElem := TJvSimpleXMLElemClassic.Create(nil);
          lElem.Assign(AElem);

          //Select columns to add
          for j := 0 to Root.Items[i].Properties.Count-1 do
            if IsColumnSelected(FParser.Tables[AIndex].Alias, Root.Items[i].Properties[j].Name) then
              lElem.Properties.Add(Root.Items[i].Properties[j].Name, Root.Items[i].Properties[j].Value);

          ConstructTable(AIndex + 1, lElem);
        end;
    end;
  end;

  procedure DeleteRows;
  var
   i, j: Integer;
  begin
    for i := 0 to FTables.Count-1 do
      for j := TJvSimpleXML(FTables[i]).Root.Items.Count-1 downto 0 do
        if FParser.CheckConditions(TJvSimpleXML(FTables[i]).Root.Items[j]) then
          TJvSimpleXML(FTables[i]).Root.Items.Delete(j);
  end;

  procedure UpdateRows;
  var
    i, j: Integer;
  begin
    for i := 0 to FTables.Count-1 do
      for j := TJvSimpleXML(FTables[i]).Root.Items.Count - 1 downto 0 do
        if FParser.CheckConditions(TJvSimpleXML(FTables[i]).Root.Items[j]) then
          FParser.UpdateRow(TJvSimpleXML(FTables[i]).Root.Items[j]);
  end;

begin
  //Parse
  FParser.Parse(AQuery);

  //Get all tables
  for i := 0 to FParser.TablesCount-1 do
    FTables.Add(FDatabase.GetTable(FParser.Tables[i].Name));

  //Execute
  case FParser.Instruction of
    xiSelect:
      begin
        lElem := TJvSimpleXMLElemClassic.Create(nil);
        lElem.Name := 'Item';
        FResults.Name := 'Results';
        ConstructTable(0, lElem);
      end;

    xiDelete:
      begin
        DeleteRows;
        FDatabase.SaveTables;
      end;

    xiUpdate:
      begin
        UpdateRows;
        FDatabase.SaveTables;
      end;

    xiInsert:
      begin
        if FTables.Count = 1 then
          with TJvSimpleXML(FTables[0]).Root.Items.Add('item') do
            for i := 0 to FParser.ColumnsCount-1 do
              if i < FParser.ValuesCount then
              begin
                lValue := FParser.Value[i];
                if lValue = 'NULL' then
                begin
                  lMax := 0;
                  for j := 0 to TJvSimpleXML(FTables[0]).Root.Items.Count-1 do
                    lMax := Max(lMax, TJvSimpleXML(FTables[0]).Root.Items[j].Properties.IntValue(FParser.Columns[i].Name, 0));
                  Inc(lMax);
                  lValue := IntToStr(lMax);
                  FLastId := lMax;
                end
                else
                if lValue = 'NOW' then
                  lValue := DateTimeToStr(Now)
                else
                if lValue = 'DATE' then
                  lValue := DateToStr(Now)
                else
                if lValue = 'TIME' then
                  lValue := TimeToStr(Now);
                Properties.Add(FParser.Columns[i].Name, lValue);
              end;
        FDatabase.SaveTables;
      end;
  end;

  FParser.OrderTable(FResults);
  FParser.LimitTable(FResults);
end;
{**********************************************************************}

{ TJvXMLQueryParser }

{**********************************************************************}
function TJvXMLQueryParser.CheckConditions(
  AXMLElem: TJvSimpleXMLElem): Boolean;
var
  i: Integer;

  function CheckCondition(var AIndex: Integer): Boolean;
  var
    lComp: TJvXMLSQLOperator;
    lValue, lValue2: string;
    lDate: TDateTime;
  begin
    Result := True;
    while AIndex < FConditions.Count do
    begin
      with TJvXMLQueryCondition(FConditions[AIndex]) do
        case Operator of
          opLeftParenthesis:
            begin
              Inc(AIndex);
              Result := Result and (CheckCondition(AIndex));
            end;
          opRightParenthesis:
            Exit;
          opNot:
            begin
              Inc(AIndex);
              Result := Result and (not CheckCondition(AIndex));
            end;
          opColumn, opConstant:
            begin
              if Operator = opConstant then
                lValue := Condition
              else
              begin
                if (Condition='daysbetweennow') then
                begin
                  Inc(AIndex, 2); // (
                  lValue := AXMLElem.Properties.Value(TJvXMLQueryCondition(FConditions[AIndex]).Condition);
                  Inc(AIndex); //)
                  lDate := StrToDateTimeDef(lValue, 0);
                  lValue := IntToStr(DaysBetween(Now, lDate));
                  if lDate < Now then
                    lValue := '-' + lValue;
                end
                else
                  lValue := AXMLElem.Properties.Value(Condition);
              end;
              Inc(AIndex, 2);
              if AIndex >= FConditions.Count then
              begin
                Result := False;
                Exit;
              end;
              lComp := TJvXMLQueryCondition(FConditions[AIndex - 1]).Operator;

              if TJvXMLQueryCondition(FConditions[AIndex]).Operator = opConstant then
                lValue2 := TJvXMLQueryCondition(FConditions[AIndex]).Condition
              else
              if TJvXMLQueryCondition(FConditions[AIndex]).Operator = opColumn then
              begin
                lValue2 := TJvXMLQueryCondition(FConditions[AIndex]).Condition;
                if AXMLElem.Properties.ItemNamed[lValue2]<>nil then
                  lValue2 := AXMLElem.Properties.Value(lValue2);
              end
              else
              if (TJvXMLQueryCondition(FConditions[AIndex]).Operator = opNull) and (lComp = opEquals) then
              begin
                Result := Result and (lValue = '');
                lComp := opNone;
              end
              else
              begin
                Result := False;
                lComp := opNone;
              end;

              try
                case lComp of
                  opEquals:
                    Result := Result and (lValue = lValue2);
                  opGreater:
                    Result := Result and (StrToFloat(lValue) > StrToFloat(lValue2));
                  opSmaller:
                    Result := Result and (StrToFloat(lValue) < StrToFloat(lValue2));
                  opGreaterEquals:
                    Result := Result and (StrToFloat(lValue) >= StrToFloat(lValue2));
                  opSmallerEquals:
                    Result := Result and (StrToFloat(lValue) <= StrToFloat(lValue2));
                  opLike:
                    begin
                      //Not implemented yet
                    end;
                end;
              except
                Result := False;
              end;
            end;
          opOr:
            begin
              Inc(AIndex);
              Result := Result or CheckCondition(AIndex);
            end;
          opAnd:
            begin
              Inc(AIndex);
              Result := Result and CheckCondition(AIndex);
            end;
          opXor:
            begin
              Inc(AIndex);
              Result := Result xor CheckCondition(AIndex);
            end;
        end;
      Inc(AIndex);
    end;
  end;

begin
  i := 0;
  Result := CheckCondition(i);
end;
{**********************************************************************}
constructor TJvXMLQueryParser.Create;
begin
  inherited Create;
  FTables := TObjectList.Create;
  FColumns := TObjectList.Create;
  FConditions := TObjectList.Create;
  FOrders := TObjectList.Create;
  FUpdates := TObjectList.Create;
  FValues := TStringList.Create;
  FLimitBegin := 0;
  FLimitCount := MaxInt;
end;
{**********************************************************************}
destructor TJvXMLQueryParser.Destroy;
begin
  FTables.Free;
  FColumns.Free;
  FConditions.Free;
  FOrders.Free;
  FUpdates.Free;
  FValues.Free;
  inherited Destroy;
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateColumns;
var
  i: Integer;
  lColumn: TJvXMLQueryColumn;
begin
  i := Pos(',', FColumnsStr);
  repeat
    if i <> 0 then
    begin
      lColumn := TJvXMLQueryColumn.Create(Trim(Copy(FColumnsStr, 1, i - 1)));
      FColumns.Add(lColumn);
      FColumnsStr := Trim(Copy(FColumnsStr, i + 1, MaxInt));
      i := Pos(',', FColumnsStr);
    end
    else
    if FColumnsStr <> '' then
    begin
      lColumn := TJvXMLQueryColumn.Create(Trim(FColumnsStr));
      FColumns.Add(lColumn);
      FColumnsStr := '';
    end;
  until FColumnsStr = '';
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateInstruction;
begin
  FInstructionStr := UpperCase(FInstructionStr);

  if FInstructionStr = 'SELECT' then
    FInstruction := xiSelect
  else
  if FInstructionStr = 'UPDATE' then
    FInstruction := xiUpdate
  else
  if FInstructionStr = 'INSERT' then
    FInstruction := xiInsert
  else
  if FInstructionStr = 'DELETE' then
    FInstruction := xiDelete
  else
    raise TJvXMLDatabaseException.CreateResFmt(@RsEUnknownInstruction, [FInstructionStr]);
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateOrderBy;
var
  i: Integer;
  lOrder: TJvXMLQueryOrder;
begin
  FOrderStr := Trim(UpperCase(FOrderStr));
  i := Pos(' ', FOrderStr);
  if i <> 0 then
    FOrderStr := Trim(Copy(FOrderStr, i + 1, MaxInt));

  i := Pos(',', FOrderStr);
  repeat
    if i <> 0 then
    begin
      lOrder := TJvXMLQueryOrder.Create(Trim(Copy(FOrderStr, 1, i - 1)));
      FOrders.Add(lOrder);
      FOrderStr := Trim(Copy(FOrderStr, i + 1, MaxInt));
      i := Pos(',', FOrderStr);
    end
    else
    if FOrderStr <> '' then
    begin
      lOrder := TJvXMLQueryOrder.Create(Trim(FOrderStr));
      FOrders.Add(lOrder);
      FOrderStr := '';
    end;
  until FOrderStr = '';
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateSet;
var
  i: Integer;
  lSet: TJvXMLQueryAssignement;
begin
  FSetStr := Trim(FSetStr);
  i := Pos(',', FSetStr);
  repeat
    if i <> 0 then
    begin
      lSet := TJvXMLQueryAssignement.Create(Trim(Copy(FSetStr, 1, i - 1)));
      FUpdates.Add(lSet);
      FSetStr := Trim(Copy(FSetStr, i + 1, MaxInt));
      i := Pos(',', FSetStr);
    end
    else
    if FSetStr <> '' then
    begin
      lSet := TJvXMLQueryAssignement.Create(Trim(FSetStr));
      FUpdates.Add(lSet);
      FSetStr := '';
    end;
  until FSetStr = '';
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateTables;
var
  i: Integer;
  lTable: TJvXMLQueryTable;
begin
  i := Pos(',', FTablesStr);
  repeat
    if i <> 0 then
    begin
      lTable := TJvXMLQueryTable.Create(Trim(Copy(FTablesStr, 1, i - 1)));
      FTables.Add(lTable);
      FTablesStr := Trim(Copy(FTablesStr, i + 1, MaxInt));
      i := Pos(',', FTablesStr);
    end
    else
    if FTablesStr <> '' then
    begin
      lTable := TJvXMLQueryTable.Create(Trim(FTablesStr));
      FTables.Add(lTable);
      FTablesStr := '';
    end;
  until FTablesStr = '';
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateValues;
var
  i: Integer;

  function ParseValue(const AValue: string): string;
  begin
    Result := Trim(AValue);

    //Escape quotes
    if (Result <> '') and (Result[1] in ['''','"']) then
      Result := Copy(Result, 2, Length(Result) - 2);

    if CompareText(Result, 'now') = 0 then
      Result := DateTimeToStr(Now);
  end;

begin
  i := Pos(',', FValuesStr);
  repeat
    if i <> 0 then
    begin
      FValues.Add(ParseValue(Trim(Copy(FValuesStr,1,i - 1))));
      FValuesStr := Trim(Copy(FValuesStr, i + 1, MaxInt));
      i := Pos(',', FValuesStr);
    end
    else
    if FValuesStr<>'' then
    begin
      FValues.Add(ParseValue(Trim(FValuesStr)));
      FValuesStr := '';
    end;
  until FValuesStr = '';
end;
{**********************************************************************}
procedure TJvXMLQueryParser.DoValidateWhere;
var
  lToken: string;
  i, WhereStrLen: Integer;
  lChar: Char;

  procedure AddToken(const AToken: string);
  begin
    lToken := LowerCase(lToken);

    if (lToken = 'and') then
      FConditions.Add(TJvXMLQueryCondition.Create(opAnd))
    else
    if (lToken = 'or') then
      FConditions.Add(TJvXMLQueryCondition.Create(opOr))
    else
    if (lToken = 'like') then
      FConditions.Add(TJvXMLQueryCondition.Create(opLike))
    else
    if (lToken = 'xor') then
      FConditions.Add(TJvXMLQueryCondition.Create(opXor))
    else
    if (lToken = 'is') then
      FConditions.Add(TJvXMLQueryCondition.Create(opEquals))
    else
    if (lToken = 'null') then
      FConditions.Add(TJvXMLQueryCondition.Create(opNull))
    else
      FConditions.Add(TJvXMLQueryCondition.Create(opColumn,lToken));
  end;

begin
  FWhereStr := FWhereStr + ' ';
  WhereStrLen := Length(FWhereStr);
  i := 1;
  lToken := '';
  while i < WhereStrLen do
  begin
    case FWhereStr[i] of
      '(':
        begin
          if lToken<>'' then
          begin
            AddToken(lToken);
            lToken := '';
          end;
          FConditions.Add(TJvXMLQueryCondition.Create(opLeftParenthesis));
        end;
      ')':
        begin
          if lToken<>'' then
          begin
            AddToken(lToken);
            lToken := '';
          end;
          FConditions.Add(TJvXMLQueryCondition.Create(opRightParenthesis));
        end;
      'a'..'z', 'A'..'Z', '0'..'9', '_':
        lToken := lToken + FWhereStr[i];
      ' ':
        if lToken <> '' then
        begin
          AddToken(lToken);
          lToken := '';
        end;
      '=':
        FConditions.Add(TJvXMLQueryCondition.Create(opEquals));
      '>':
        begin
          Inc(i);
          if i < WhereStrLen then
          begin
            if FWhereStr[i] = '=' then
              FConditions.Add(TJvXMLQueryCondition.Create(opGreaterEquals))
            else
            begin
              FConditions.Add(TJvXMLQueryCondition.Create(opGreater));
              Dec(i);
            end;
          end;
        end;
      '<':
        begin
          Inc(i);
          if i < WhereStrLen then
          begin
            if FWhereStr[i] = '=' then
              FConditions.Add(TJvXMLQueryCondition.Create(opSmallerEquals))
            else
            begin
              FConditions.Add(TJvXMLQueryCondition.Create(opSmaller));
              Dec(i);
            end;
          end;
        end;
      '''','"':
        begin
          lChar := FWhereStr[i];
          Inc(i);
          lToken := '';
          while (i < WhereStrLen) and (FWhereStr[i] <> lChar) do
          begin
            lToken := lToken + FWhereStr[i];
            Inc(i);
          end;
          FConditions.Add(TJvXMLQueryCondition.Create(opConstant,lToken));
          lToken := '';
        end;
    end;
    Inc(i);
  end;
end;
{**********************************************************************}
function TJvXMLQueryParser.GetColumn(
  const AIndex: Integer): TJvXMLQueryColumn;
begin
  Result := TJvXMLQueryColumn(FColumns[AIndex]);
end;
{**********************************************************************}
function TJvXMLQueryParser.GetColumnsCount: Integer;
begin
  Result := FColumns.Count;
end;
{**********************************************************************}
function TJvXMLQueryParser.GetCondition(
  const AIndex: Integer): TJvXMLQueryCondition;
begin
  Result := TJvXMLQueryCondition(FConditions[AIndex]);
end;
{**********************************************************************}
function TJvXMLQueryParser.GetConditionsCount: Integer;
begin
  Result := FConditions.Count;
end;
{**********************************************************************}
function TJvXMLQueryParser.GetTable(
  const AIndex: Integer): TJvXMLQueryTable;
begin
  Result := TJvXMLQueryTable(FTables[AIndex]);
end;
{**********************************************************************}
function TJvXMLQueryParser.GetTablesCount: Integer;
begin
  Result := FTables.Count;
end;
{**********************************************************************}
function TJvXMLQueryParser.GetValue(const AIndex: Integer): string;
begin
  Result := FValues[AIndex];
end;
{**********************************************************************}
function TJvXMLQueryParser.GetValuesCount: Integer;
begin
  Result := FValues.Count;
end;
{**********************************************************************}
procedure TJvXMLQueryParser.LimitTable(var ATable: TJvSimpleXMLElem);
begin
  while (FLimitBegin > 0) and (ATable.Items.Count > 0) do
  begin
    ATable.Items.Delete(0);
    Dec(FLimitBegin);
  end;
  while (ATable.Items.Count > FLimitCount) do
    ATable.Items.Delete(ATable.Items.Count - 1);
end;
{**********************************************************************}
function TJvXMLQueryParser.OrderCallBack(Elems: TJvSimpleXMLElems; Index1,
  Index2: Integer): Integer;
var
  i: Integer;
  lStr1, lStr2: string;
  lFloat1, lFloat2: Double;
begin
  Result := 0;

  for i := 0 to FOrders.Count-1 do
  begin
    lStr1 := FOrderTable.Items[Index1].Properties.Value(TJvXMLQueryOrder(FOrders[i]).Column);
    lStr2 := FOrderTable.Items[Index2].Properties.Value(TJvXMLQueryOrder(FOrders[i]).Column);
    if lStr1 <> lStr2 then
    begin
      //convert to date/int
      case TJvXMLQueryOrder(FOrders[i]).Convertion of
        ocNone:
          Result := CompareStr(lStr1, lStr2);
        ocDate:
          Result := CompareDateTime(StrToDateTimeDef(lStr1, 0), StrToDateTimeDef(lStr2, 0));
        ocInteger:
          Result := StrToIntDef(lStr1, 0) - StrToIntDef(lStr2, 0);
        ocFloat:
          begin
            lFloat1 := StrToFloatDef(lStr1, 0);
            lFloat2 := StrToFloatDef(lStr2, 0);
            if lFloat1 > lFloat2 then
              Result := 1
            else
            if lFloat1 < lFloat2 then
              Result := -1;
          end;
      end;

      if not TJvXMLQueryOrder(FOrders[i]).Ascending then
        Result := - Result;
      Exit;
    end;
  end;
end;
{**********************************************************************}
procedure TJvXMLQueryParser.OrderTable(var ATable: TJvSimpleXMLElem);
begin
  FOrderTable := ATable;
  ATable.Items.CustomSort(OrderCallBack);
end;
{**********************************************************************}
procedure TJvXMLQueryParser.Parse(const AQuery: string);
var
  st: string;
  lStatements: array of string;
  i, j: Integer;
begin
  FQuery := AQuery;

  FInstructionStr := ReadToken;
  DoValidateInstruction;

  case Instruction of
    xiSelect:
      begin
        st := ReadColumns(['FROM', 'WHERE', 'ORDER', 'LIMIT'], False);
        SetLength(lStatements, 4);
        lStatements[0] := 'FROM';
        lStatements[1] := 'WHERE';
        lStatements[2] := 'ORDER';
        lStatements[3] := 'LIMIT';
      end;

    xiDelete:
      begin
        ReadToken; //pass the FROM keyword
        st := 'FROM';
        SetLength(lStatements, 2);
        lStatements[0] := 'FROM';
        lStatements[1] := 'WHERE';
      end;

    xiUpdate:
      begin
        st := 'FROM';
        SetLength(lStatements, 3);
        lStatements[0] := 'FROM';
        lStatements[1] := 'SET';
        lStatements[2] := 'WHERE';
      end;

    xiInsert:
      begin
        st := 'FROM';
        SetLength(lStatements, 3);
        lStatements[0] := 'FROM';
        lStatements[1] := 'VALUES';
        lStatements[2] := 'COLUMNS';
        ReadToken; // Pass the into statement

        //Modify query for lightness of parser
        //INSERT INTO file.XML(Col1, Col2) VALUES(val1, val2)
        // into
        //INSERT INTO file.XML COLUMNS col1, col2 VALUES val1, val2
        FQuery := StringReplace(FQuery, '()', '', [rfReplaceAll]);
        FQuery := StringReplace(FQuery, ')', ' ', [rfReplaceAll]);
        FQuery := StringReplace(FQuery, '(', ' COLUMNS ', []);
        FQuery := StringReplace(FQuery, '(', ' ', []);
      end;
  end;

  while st <> '' do
  begin
    j := -1;
    for i := 0 to Length(lStatements)-1 do
      if lStatements[i] = st then
      begin
        lStatements[i] := ''; //Do not accept it anymore
        j := i;
        Break;
      end;
    if j = -1 then
      raise TJvXMLDatabaseException.CreateResFmt(@RsEUnexpectedStatement, [st]);

    if st = 'FROM' then
      st := ReadTables(lStatements)
    else
    if st = 'WHERE' then
      st := ReadWhere(lStatements)
    else
    if st = 'LIMIT' then
      st := ReadLimit(lStatements)
    else
    if st = 'ORDER' then
      st := ReadOrderBy(lStatements)
    else
    if st = 'SET' then
      st := ReadSet(lStatements)
    else
    if st = 'VALUES' then
      st := ReadValues(lStatements)
    else
    if st = 'COLUMNS' then
      st := ReadColumns(lStatements, False);
  end;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadColumns(const AEndStatement: array of string;
  ACanTerminate: Boolean): string;
begin
  Result := ReadStatement(AEndStatement, ACanTerminate, FColumnsStr);
  DoValidateColumns;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadLimit(const AEndStatement: array of string): string;
var
  i: Integer;
begin
  Result := ReadStatement(AEndStatement, True, FLimitStr);
  i := Pos(',', FLimitStr);
  if i = 0 then
    FLimitCount := StrToIntDef(FLimitStr, MaxInt)
  else
  begin
    FLimitCount := StrToIntDef(Trim(Copy(FLimitStr, i + 1, MaxInt)), MaxInt);
    FLimitBegin := StrToIntDef(Trim(Copy(FLimitStr, 1, i - 1)), 0);
  end;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadOrderBy(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FOrderStr);
  DoValidateOrderBy;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadSet(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FSetStr);
  DoValidateSet;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadStatement(const AEndStatement: array of string;
  ACanTerminate: Boolean; var AValue: string): string;
var
  st, upSt: string;
  lFound: Boolean;
  i: Integer;
begin
  AValue := '';
  lFound := False;
  Result := '';
  while not lFound do
    if (FQuery = '') and ACanTerminate then
      lFound := True
    else
    begin
      st := ReadToken;
      if st <> '' then
      begin
        upSt := UpperCase(st);
        for i := 0 to Length(AEndStatement) - 1 do
          if upSt = AEndStatement[i] then
          begin
            lFound := True;
            Break;
          end;
      end;

      if not lFound then
        AValue := AValue + ' ' + st
      else
        Result := st;
    end;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadTables(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FTablesStr);
  DoValidateTables;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadToken: string;
var
  i: Integer;
begin
  if FQuery='' then
    raise TJvXMLDatabaseException.CreateRes(@RsEUnexpectedEndOfQuery);

  FQuery := TrimLeft(FQuery);
  i := 1;
  while (i<Length(FQuery)) and not(FQuery[i] in [' '{,'(',')'}]) do
    Inc(i);
  if i>=Length(FQuery) then
  begin
    Result := Trim(FQuery);
    FQuery := '';
  end
  else
  begin
    Result := Copy(FQuery, 1, i - 1);
    FQuery := Copy(FQuery, i + 1, MaxInt);
  end;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadValues(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FValuesStr);
  DoValidateValues;
end;
{**********************************************************************}
function TJvXMLQueryParser.ReadWhere(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FWhereStr);
  DoValidateWhere;
end;
{**********************************************************************}
procedure TJvXMLQueryParser.UpdateRow(ARow: TJvSimpleXMLElem);
var
  i: Integer;
begin
  for i := 0 to FUpdates.Count - 1 do
    TJvXMLQueryAssignement(FUpdates[i]).UpdateElem(ARow);
end;
{**********************************************************************}

{ TJvXMLQueryColumn }

{**********************************************************************}
constructor TJvXMLQueryColumn.Create(const AValue: string);
var
  i: Integer;
begin
  inherited Create;
  i := Pos('.', AValue);
  if i <> 0 then
  begin
    Name := Copy(AValue, i + 1, MaxInt);
    Table := Copy(AValue, 1, i - 1);
  end
  else
    Name := AValue;
end;
{**********************************************************************}

{ TJvXMLQueryTable }

{**********************************************************************}
constructor TJvXMLQueryTable.Create(const AValue: string);
var
  i: Integer;
begin
  inherited Create;
  i := Pos(' ', AValue);
  if i <> 0 then
  begin
    Name := Copy(AValue, 1, i - 1);
    Alias := Trim(Copy(AValue, i + 1, MaxInt));
  end
  else
    Name := AValue;
end;
{**********************************************************************}

{ TJvXMLQueryCondition }

{**********************************************************************}
constructor TJvXMLQueryCondition.Create(AOperator: TJvXMLSQLOperator;
  const ACondition: string);
begin
  inherited Create;
  Self.Operator := AOperator;
  Self.Condition := ACondition;
end;
{**********************************************************************}

{ TJvXMLQueryOrder }

{**********************************************************************}
constructor TJvXMLQueryOrder.Create(const AValue: string);
var
  i: Integer;
  st: string;
begin
  inherited Create;
  Column := Trim(AValue);
  Ascending := True;
  Convertion := ocNone;

  i := Pos(' ', Column);
  if i <> 0 then
  begin
    SetLength(Column, i - 1);
    Ascending := Pos('ASC', UpperCase(AValue)) <> 0;
  end;

  i := Pos('(', Column);
  if i <> 0 then
  begin
    st := UpperCase(Copy(Column, 1, i - 1));
    Column := Copy(Column, i + 1, MaxInt);
    SetLength(Column, Length(Column) - 1);

    if st = 'DATE' then
      Convertion := ocDate
    else
    if (st = 'Integer') or (st = 'INT') then
      Convertion := ocInteger
    else
    if (st = 'FLOAT') then
      Convertion := ocFloat;
  end;
end;
{**********************************************************************}


{ TJvXMLQueryAssignement }

{**********************************************************************}
constructor TJvXMLQueryAssignement.Create(AValue: string);
var
  i, j: Integer;
  lDelimiter: Char;
begin
  inherited Create;
  i := Pos('=', AValue);
  if i = 0 then
    raise Exception.Create('')
  else
  begin
    Column := Trim(Copy(AValue, 1, i - 1));
    AValue := Trim(Copy(AValue, i + 1, MaxInt));

    if AValue = '' then
      raise Exception.Create('');

    //Determine if column or constant
    if (AValue[1] = '"') or (AValue[1] = '''') then
    begin
      lDelimiter := AValue[1];
      ValueKind := skConstant;
      AValue := Copy(AValue, 2, MaxInt);
      i := 0;
      for j := 1 to Length(AValue) do
        if AValue[j] = lDelimiter then
          if (j=1) or (AValue[j-1] <> '\') then
          begin
            i := j;
            Break;
          end;
      if i <> 0 then
      begin
        Value := Copy(AValue, 1, i - 1);
        Value := StringReplace(Value, '\' + lDelimiter, lDelimiter, [rfReplaceAll]);
        AValue := Trim(Copy(AValue, i + 1, MaxInt));
      end
      else
        raise Exception.Create('');        
    end
    else
    begin
      ValueKind := skColumn;
      i := Pos(' ', AValue);
      if i = 0 then
      begin
        Value := AValue;
        AValue := '';
      end
      else
      begin
        Value := Copy(AValue, 1, i - 1);
        AValue := Trim(Copy(AValue, i + 1, MaxInt));
      end;
    end;

    //Second kind and second value?
    if AValue = '' then
      Operator := soNone
    else
    begin
      case AValue[1] of
        '+': Operator := soAdd;
        '-': Operator := soSubstract;
        '*': Operator := soMultiply;
        '/': Operator := soDivide;
        else
          raise Exception.Create('');
      end;

      SecondValue := Trim(Copy(AValue, 2, MaxInt));
      if (SecondValue<>'') and (SecondValue[1] in ['''','"']) then
      begin
        SecondValue := Copy(SecondValue, 2, Length(SecondValue) - 2);
        SecondKind := skConstant;
      end
      else
        SecondKind := skColumn;
    end;
  end;
end;
{**********************************************************************}
procedure TJvXMLQueryAssignement.UpdateElem(AElement: TJvSimpleXMLElem);
var
  lValue, lValue2: string;

  function ParseValue(const AValue: string): string;
  begin
    if CompareText(AValue, 'now()') = 0 then
      Result := DateTimeToStr(Now)
    else
      Result := AValue;
  end;

begin
  if ValueKind = skConstant then
    lValue := Value
  else
    lValue := AElement.Properties.Value(Value, ParseValue(Value));

  if Operator <> soNone then
  begin
    if SecondKind = skConstant then
      lValue2 := SecondValue
    else
      lValue2 := AElement.Properties.Value(SecondValue, ParseValue(SecondValue));
    case Operator of
      soAdd:
        lValue := FloatToStr(StrToFloatDef(lValue,0) + StrToFloatDef(lValue2,0));
      soMultiply:
        lValue := FloatToStr(StrToFloatDef(lValue,0) * StrToFloatDef(lValue2,0));
      soDivide:
        lValue := FloatToStr(StrToFloatDef(lValue,0) / StrToFloatDef(lValue2,0));
      soSubstract:
        lValue := FloatToStr(StrToFloatDef(lValue,0) - StrToFloatDef(lValue2,0));
    end;
  end;

  AElement.Properties.Delete(Column);
  AElement.Properties.Add(Column, lValue);
end;
{**********************************************************************}

end.

