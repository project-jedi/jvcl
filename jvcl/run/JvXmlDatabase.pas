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

unit JvXmlDatabase;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Contnrs, Math,
  {$IFDEF COMPILER6_UP}
  DateUtils,
  {$ENDIF COMPILER6_UP}
  JvTypes, JvComponent, JvSimpleXml;

type
  TJvXMLDatabase = class;
  TJvXMLQuery = class;
  TJvXMLQueryParser = class;
  TJvXMLDatabaseException = class(EJVCLException);

  TJvXMLTable = class(TObject)
  public
    XML: TJvSimpleXML;
    Locked: Boolean;
    FileName: string;
  end;

  TJvXMLQueryTable = class(TObject)
  public
    Name: string;
    Alias: string;
    constructor Create(const AValue: string);
  end;

  TJvXMLQueryColumn = class(TObject)
  public
    Name: string;
    Table: string;
    constructor Create(const AValue: string);
  end;

  TJvXMLOrderConvertion = (ocNone, ocDate, ocInteger, ocFloat);

  TJvXMLQueryOrder = class(TObject)
  public
    Column: string;
    Ascending: Boolean;
    Convertion: TJvXMLOrderConvertion;
    constructor Create(const AValue: string);
  end;

  TJvXMLSQLOperator = (opEquals, opGreater, opSmaller, opGreaterEquals,
    opSmallerEquals, opLike, opNot, opOr, opAnd, opXor, opLeftParenthesis,
    opRightParenthesis, opConstant, opColumn, opNull, opNone);

  TJvXMLQueryCondition = class(TObject)
  public
    Condition: string;
    Operator: TJvXMLSQLOperator;
    constructor Create(AOperator: TJvXMLSQLOperator; const ACondition: string = '');
  end;

  TJvXMLSetKind = (skConstant, skColumn);
  TJvXMLSetOperator = (soNone, soAdd, soMultiply, soDivide, soSubstract);

  TJvXMLQueryAssignement = class(TObject)
  public
    Column: string;
    ValueKind: TJvXMLSetKind;
    SecondKind: TJvXMLSetKind;
    Operator: TJvXMLSetOperator;
    Value: string;
    SecondValue: string;
    constructor Create(AValue: string);
    procedure UpdateElem(AElement: TJvSimpleXMLElem);
  end;

  TJvXMLInstruction = (xiSelect, xiUpdate, xiInsert, xiDelete);
  TJvXMLQueryParser = class(TObject)
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
    function ReadStatement(const AEndStatement: array of string;
      ACanTerminate: Boolean; var AValue: string): string;
    procedure DoValidateInstruction;
    procedure DoValidateColumns;
    procedure DoValidateTables;
    procedure DoValidateWhere;
    procedure DoValidateOrderBy;
    procedure DoValidateSet;
    procedure DoValidateValues;
  public
    constructor Create;
    destructor Destroy; override;
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

  TJvXMLQuery = class(TObject)
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
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvJCLUtils, JvResources;

//=== { TJvXMLDatabase } =====================================================

constructor TJvXMLDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTables := TObjectList.Create;
end;

destructor TJvXMLDatabase.Destroy;
begin
  FTables.Free;
  inherited Destroy;
end;

function TJvXMLDatabase.GetTable(const AName: string): TJvSimpleXML;
var
  I: Integer;
  St: string;
  LTable: TJvXMLTable;
begin
  St := TablesPath + AName;
  for I := 0 to FTables.Count-1 do
    if TJvXMLTable(FTables[I]).FileName = St then
    begin
      Result := TJvXMLTable(FTables[I]).XML;
      Exit;
    end;

  LTable := TJvXMLTable.Create;
  LTable.XML := TJvSimpleXML.Create(nil);
  LTable.XML.LoadFromFile(St);
  LTable.Locked := False;
  LTable.FileName := St;
  FTables.Add(LTable);
  Result := LTable.XML;
end;

function TJvXMLDatabase.Query(const AQuery: string): TJvXMLQuery;
begin
  Result := TJvXMLQuery.Create(Self);
  Result.Query(AQuery);
end;

procedure TJvXMLDatabase.SaveTables;
var
  I: Integer;
begin
  for I := 0 to FTables.Count-1 do
    TJvXMLTable(FTables[I]).XML.SaveToFile(TJvXMLTable(FTables[I]).FileName);
end;

//=== { TJvXMLQuery } ========================================================

constructor TJvXMLQuery.Create(AOwner: TJvXMLDatabase);
begin
  inherited Create;
  FDatabase := AOwner;
  FParser := TJvXMLQueryParser.Create;
  FResults := TJvSimpleXMLElemClassic.Create(nil);
  FTables := TList.Create;
end;

destructor TJvXMLQuery.Destroy;
begin
  FParser.Free;
  FResults.Free;
  FTables.Free;
  inherited Destroy;
end;

procedure TJvXMLQuery.Query(const AQuery: string);
var
  I, J, lMax: Integer;
  LElem: TJvSimpleXMLElemClassic;
  LValue: string;

  function IsColumnSelected(const ATable, AColumn: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FParser.ColumnsCount-1 do
      if (FParser.Columns[I].Name = '*') or ((FParser.Columns[I].Name = AColumn) and
        ((FParser.Columns[I].Table = '') or (FParser.Columns[I].Table = ATable))) then
      begin
        Result := True;
        Break;
      end;
  end;

  procedure ConstructTable(AIndex: Integer; var AElem: TJvSimpleXMLElemClassic);
  var
    I, J: Integer;
    LElem: TJvSimpleXMLElemClassic;
  begin
    if AIndex >= FTables.Count then
    begin
      if FParser.CheckConditions(AElem) then
        FResults.Items.Add(AElem)
      else
        AElem.Free;
    end
    else
      with TJvSimpleXML(FTables[AIndex]) do
        for I := 0 to Root.Items.Count-1 do
        begin
          LElem := TJvSimpleXMLElemClassic.Create(nil);
          LElem.Assign(AElem);

          //Select columns to add
          for J := 0 to Root.Items[I].Properties.Count-1 do
            if IsColumnSelected(FParser.Tables[AIndex].Alias, Root.Items[I].Properties[J].Name) then
              LElem.Properties.Add(Root.Items[I].Properties[J].Name, Root.Items[I].Properties[J].Value);

          ConstructTable(AIndex + 1, LElem);
        end;
  end;

  procedure DeleteRows;
  var
    I, J: Integer;
  begin
    for I := 0 to FTables.Count-1 do
      for J := TJvSimpleXML(FTables[I]).Root.Items.Count-1 downto 0 do
        if FParser.CheckConditions(TJvSimpleXML(FTables[I]).Root.Items[J]) then
          TJvSimpleXML(FTables[I]).Root.Items.Delete(J);
  end;

  procedure UpdateRows;
  var
    I, J: Integer;
  begin
    for I := 0 to FTables.Count-1 do
      for J := TJvSimpleXML(FTables[I]).Root.Items.Count - 1 downto 0 do
        if FParser.CheckConditions(TJvSimpleXML(FTables[I]).Root.Items[J]) then
          FParser.UpdateRow(TJvSimpleXML(FTables[I]).Root.Items[J]);
  end;

begin
  //Parse
  FParser.Parse(AQuery);

  //Get all tables
  for I := 0 to FParser.TablesCount-1 do
    FTables.Add(FDatabase.GetTable(FParser.Tables[I].Name));

  //Execute
  case FParser.Instruction of
    xiSelect:
      begin
        LElem := TJvSimpleXMLElemClassic.Create(nil);
        LElem.Name := 'Item';
        FResults.Name := 'Results';
        ConstructTable(0, LElem);
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
            for I := 0 to FParser.ColumnsCount-1 do
              if I < FParser.ValuesCount then
              begin
                LValue := FParser.Value[I];
                if LValue = 'NULL' then
                begin
                  lMax := 0;
                  for J := 0 to TJvSimpleXML(FTables[0]).Root.Items.Count-1 do
                    lMax := Max(lMax, TJvSimpleXML(FTables[0]).Root.Items[J].Properties.IntValue(FParser.Columns[I].Name, 0));
                  Inc(lMax);
                  LValue := IntToStr(lMax);
                  FLastId := lMax;
                end
                else
                if LValue = 'NOW' then
                  LValue := DateTimeToStr(Now)
                else
                if LValue = 'DATE' then
                  LValue := DateToStr(Now)
                else
                if LValue = 'TIME' then
                  LValue := TimeToStr(Now);
                Properties.Add(FParser.Columns[I].Name, LValue);
              end;
        FDatabase.SaveTables;
      end;
  end;

  FParser.OrderTable(FResults);
  FParser.LimitTable(FResults);
end;

//=== { TJvXMLQueryParser } ==================================================

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

function TJvXMLQueryParser.CheckConditions(AXMLElem: TJvSimpleXMLElem): Boolean;
var
  I: Integer;

  function CheckCondition(var AIndex: Integer): Boolean;
  var
    LComp: TJvXMLSQLOperator;
    LValue, LValue2: string;
    LDate: TDateTime;
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
                LValue := Condition
              else
              begin
                if Condition = 'daysbetweennow' then
                begin
                  Inc(AIndex, 2);
                  LValue := AXMLElem.Properties.Value(TJvXMLQueryCondition(FConditions[AIndex]).Condition);
                  Inc(AIndex);
                  LDate := StrToDateTimeDef(LValue, 0);
                  LValue := IntToStr(DaysBetween(Now, LDate));
                  if LDate < Now then
                    LValue := '-' + LValue;
                end
                else
                  LValue := AXMLElem.Properties.Value(Condition);
              end;
              Inc(AIndex, 2);
              if AIndex >= FConditions.Count then
              begin
                Result := False;
                Exit;
              end;
              LComp := TJvXMLQueryCondition(FConditions[AIndex-1]).Operator;

              if TJvXMLQueryCondition(FConditions[AIndex]).Operator = opConstant then
                LValue2 := TJvXMLQueryCondition(FConditions[AIndex]).Condition
              else
              if TJvXMLQueryCondition(FConditions[AIndex]).Operator = opColumn then
              begin
                LValue2 := TJvXMLQueryCondition(FConditions[AIndex]).Condition;
                if AXMLElem.Properties.ItemNamed[LValue2] <> nil then
                  LValue2 := AXMLElem.Properties.Value(LValue2);
              end
              else
              if (TJvXMLQueryCondition(FConditions[AIndex]).Operator = opNull) and (LComp = opEquals) then
              begin
                Result := Result and (LValue = '');
                LComp := opNone;
              end
              else
              begin
                Result := False;
                LComp := opNone;
              end;

              try
                case LComp of
                  opEquals:
                    Result := Result and (LValue = LValue2);
                  opGreater:
                    Result := Result and (StrToFloat(LValue) > StrToFloat(LValue2));
                  opSmaller:
                    Result := Result and (StrToFloat(LValue) < StrToFloat(LValue2));
                  opGreaterEquals:
                    Result := Result and (StrToFloat(LValue) >= StrToFloat(LValue2));
                  opSmallerEquals:
                    Result := Result and (StrToFloat(LValue) <= StrToFloat(LValue2));
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
  I := 0;
  Result := CheckCondition(I);
end;

procedure TJvXMLQueryParser.DoValidateColumns;
var
  I: Integer;
  LColumn: TJvXMLQueryColumn;
begin
  I := Pos(',', FColumnsStr);
  repeat
    if I <> 0 then
    begin
      LColumn := TJvXMLQueryColumn.Create(Trim(Copy(FColumnsStr, 1, I - 1)));
      FColumns.Add(LColumn);
      FColumnsStr := Trim(Copy(FColumnsStr, I + 1, MaxInt));
      I := Pos(',', FColumnsStr);
    end
    else
    if FColumnsStr <> '' then
    begin
      LColumn := TJvXMLQueryColumn.Create(Trim(FColumnsStr));
      FColumns.Add(LColumn);
      FColumnsStr := '';
    end;
  until FColumnsStr = '';
end;

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

procedure TJvXMLQueryParser.DoValidateOrderBy;
var
  I: Integer;
  LOrder: TJvXMLQueryOrder;
begin
  FOrderStr := Trim(UpperCase(FOrderStr));
  I := Pos(' ', FOrderStr);
  if I <> 0 then
    FOrderStr := Trim(Copy(FOrderStr, I + 1, MaxInt));

  I := Pos(',', FOrderStr);
  repeat
    if I <> 0 then
    begin
      LOrder := TJvXMLQueryOrder.Create(Trim(Copy(FOrderStr, 1, I - 1)));
      FOrders.Add(LOrder);
      FOrderStr := Trim(Copy(FOrderStr, I + 1, MaxInt));
      I := Pos(',', FOrderStr);
    end
    else
    if FOrderStr <> '' then
    begin
      LOrder := TJvXMLQueryOrder.Create(Trim(FOrderStr));
      FOrders.Add(LOrder);
      FOrderStr := '';
    end;
  until FOrderStr = '';
end;

procedure TJvXMLQueryParser.DoValidateSet;
var
  I: Integer;
  LSet: TJvXMLQueryAssignement;
begin
  FSetStr := Trim(FSetStr);
  I := Pos(',', FSetStr);
  repeat
    if I <> 0 then
    begin
      LSet := TJvXMLQueryAssignement.Create(Trim(Copy(FSetStr, 1, I - 1)));
      FUpdates.Add(LSet);
      FSetStr := Trim(Copy(FSetStr, I + 1, MaxInt));
      I := Pos(',', FSetStr);
    end
    else
    if FSetStr <> '' then
    begin
      LSet := TJvXMLQueryAssignement.Create(Trim(FSetStr));
      FUpdates.Add(LSet);
      FSetStr := '';
    end;
  until FSetStr = '';
end;

procedure TJvXMLQueryParser.DoValidateTables;
var
  I: Integer;
  LTable: TJvXMLQueryTable;
begin
  I := Pos(',', FTablesStr);
  repeat
    if I <> 0 then
    begin
      LTable := TJvXMLQueryTable.Create(Trim(Copy(FTablesStr, 1, I - 1)));
      FTables.Add(LTable);
      FTablesStr := Trim(Copy(FTablesStr, I + 1, MaxInt));
      I := Pos(',', FTablesStr);
    end
    else
    if FTablesStr <> '' then
    begin
      LTable := TJvXMLQueryTable.Create(Trim(FTablesStr));
      FTables.Add(LTable);
      FTablesStr := '';
    end;
  until FTablesStr = '';
end;

procedure TJvXMLQueryParser.DoValidateValues;
var
  I: Integer;

  function ParseValue(const AValue: string): string;
  begin
    Result := Trim(AValue);

    //Escape quotes
    if (Result <> '') and (Result[1] in ['''','"']) then
      Result := Copy(Result, 2, Length(Result) - 2);

    if SameText(Result, 'now') then
      Result := DateTimeToStr(Now);
  end;

begin
  I := Pos(',', FValuesStr);
  repeat
    if I <> 0 then
    begin
      FValues.Add(ParseValue(Trim(Copy(FValuesStr,1,I - 1))));
      FValuesStr := Trim(Copy(FValuesStr, I + 1, MaxInt));
      I := Pos(',', FValuesStr);
    end
    else
    if FValuesStr<>'' then
    begin
      FValues.Add(ParseValue(Trim(FValuesStr)));
      FValuesStr := '';
    end;
  until FValuesStr = '';
end;

procedure TJvXMLQueryParser.DoValidateWhere;
var
  LToken: string;
  I, WhereStrLen: Integer;
  LChar: Char;

  procedure AddToken(const AToken: string);
  begin
    LToken := LowerCase(LToken);

    if LToken = 'and' then
      FConditions.Add(TJvXMLQueryCondition.Create(opAnd))
    else
    if LToken = 'or' then
      FConditions.Add(TJvXMLQueryCondition.Create(opOr))
    else
    if LToken = 'like' then
      FConditions.Add(TJvXMLQueryCondition.Create(opLike))
    else
    if LToken = 'xor' then
      FConditions.Add(TJvXMLQueryCondition.Create(opXor))
    else
    if LToken = 'is' then
      FConditions.Add(TJvXMLQueryCondition.Create(opEquals))
    else
    if LToken = 'null' then
      FConditions.Add(TJvXMLQueryCondition.Create(opNull))
    else
      FConditions.Add(TJvXMLQueryCondition.Create(opColumn,LToken));
  end;

begin
  FWhereStr := FWhereStr + ' ';
  WhereStrLen := Length(FWhereStr);
  I := 1;
  LToken := '';
  while I < WhereStrLen do
  begin
    case FWhereStr[I] of
      '(':
        begin
          if LToken<>'' then
          begin
            AddToken(LToken);
            LToken := '';
          end;
          FConditions.Add(TJvXMLQueryCondition.Create(opLeftParenthesis));
        end;
      ')':
        begin
          if LToken<>'' then
          begin
            AddToken(LToken);
            LToken := '';
          end;
          FConditions.Add(TJvXMLQueryCondition.Create(opRightParenthesis));
        end;
      'a'..'z', 'A'..'Z', '0'..'9', '_':
        LToken := LToken + FWhereStr[I];
      ' ':
        if LToken <> '' then
        begin
          AddToken(LToken);
          LToken := '';
        end;
      '=':
        FConditions.Add(TJvXMLQueryCondition.Create(opEquals));
      '>':
        begin
          Inc(I);
          if I < WhereStrLen then
          begin
            if FWhereStr[I] = '=' then
              FConditions.Add(TJvXMLQueryCondition.Create(opGreaterEquals))
            else
            begin
              FConditions.Add(TJvXMLQueryCondition.Create(opGreater));
              Dec(I);
            end;
          end;
        end;
      '<':
        begin
          Inc(I);
          if I < WhereStrLen then
          begin
            if FWhereStr[I] = '=' then
              FConditions.Add(TJvXMLQueryCondition.Create(opSmallerEquals))
            else
            begin
              FConditions.Add(TJvXMLQueryCondition.Create(opSmaller));
              Dec(I);
            end;
          end;
        end;
      '''', '"':
        begin
          LChar := FWhereStr[I];
          Inc(I);
          LToken := '';
          while (I < WhereStrLen) and (FWhereStr[I] <> LChar) do
          begin
            LToken := LToken + FWhereStr[I];
            Inc(I);
          end;
          FConditions.Add(TJvXMLQueryCondition.Create(opConstant,LToken));
          LToken := '';
        end;
    end;
    Inc(I);
  end;
end;

function TJvXMLQueryParser.GetColumn(const AIndex: Integer): TJvXMLQueryColumn;
begin
  Result := TJvXMLQueryColumn(FColumns[AIndex]);
end;

function TJvXMLQueryParser.GetColumnsCount: Integer;
begin
  Result := FColumns.Count;
end;

function TJvXMLQueryParser.GetCondition(const AIndex: Integer): TJvXMLQueryCondition;
begin
  Result := TJvXMLQueryCondition(FConditions[AIndex]);
end;

function TJvXMLQueryParser.GetConditionsCount: Integer;
begin
  Result := FConditions.Count;
end;

function TJvXMLQueryParser.GetTable(const AIndex: Integer): TJvXMLQueryTable;
begin
  Result := TJvXMLQueryTable(FTables[AIndex]);
end;

function TJvXMLQueryParser.GetTablesCount: Integer;
begin
  Result := FTables.Count;
end;

function TJvXMLQueryParser.GetValue(const AIndex: Integer): string;
begin
  Result := FValues[AIndex];
end;

function TJvXMLQueryParser.GetValuesCount: Integer;
begin
  Result := FValues.Count;
end;

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

function TJvXMLQueryParser.OrderCallBack(Elems: TJvSimpleXMLElems;
  Index1, Index2: Integer): Integer;
var
  I: Integer;
  LStr1, LStr2: string;
  LFloat1, LFloat2: Double;
begin
  Result := 0;

  for I := 0 to FOrders.Count-1 do
  begin
    LStr1 := FOrderTable.Items[Index1].Properties.Value(TJvXMLQueryOrder(FOrders[I]).Column);
    LStr2 := FOrderTable.Items[Index2].Properties.Value(TJvXMLQueryOrder(FOrders[I]).Column);
    if LStr1 <> LStr2 then
    begin
      //convert to date/int
      case TJvXMLQueryOrder(FOrders[I]).Convertion of
        ocNone:
          Result := AnsiCompareStr(LStr1, LStr2);
        ocDate:
          Result := CompareDateTime(StrToDateTimeDef(LStr1, 0), StrToDateTimeDef(LStr2, 0));
        ocInteger:
          Result := StrToIntDef(LStr1, 0) - StrToIntDef(LStr2, 0);
        ocFloat:
          begin
            LFloat1 := StrToFloatDef(LStr1, 0);
            LFloat2 := StrToFloatDef(LStr2, 0);
            if LFloat1 > LFloat2 then
              Result := 1
            else
            if LFloat1 < LFloat2 then
              Result := -1;
          end;
      end;

      if not TJvXMLQueryOrder(FOrders[I]).Ascending then
        Result := - Result;
      Exit;
    end;
  end;
end;

procedure TJvXMLQueryParser.OrderTable(var ATable: TJvSimpleXMLElem);
begin
  FOrderTable := ATable;
  ATable.Items.CustomSort(OrderCallBack);
end;

procedure TJvXMLQueryParser.Parse(const AQuery: string);
var
  St: string;
  LStatements: array of string;
  I, J: Integer;
begin
  FQuery := AQuery;

  FInstructionStr := ReadToken;
  DoValidateInstruction;

  case Instruction of
    xiSelect:
      begin
        St := ReadColumns(['FROM', 'WHERE', 'ORDER', 'LIMIT'], False);
        SetLength(LStatements, 4);
        LStatements[0] := 'FROM';
        LStatements[1] := 'WHERE';
        LStatements[2] := 'ORDER';
        LStatements[3] := 'LIMIT';
      end;
    xiDelete:
      begin
        ReadToken; //pass the FROM keyword
        St := 'FROM';
        SetLength(LStatements, 2);
        LStatements[0] := 'FROM';
        LStatements[1] := 'WHERE';
      end;
    xiUpdate:
      begin
        St := 'FROM';
        SetLength(LStatements, 3);
        LStatements[0] := 'FROM';
        LStatements[1] := 'SET';
        LStatements[2] := 'WHERE';
      end;
    xiInsert:
      begin
        St := 'FROM';
        SetLength(LStatements, 3);
        LStatements[0] := 'FROM';
        LStatements[1] := 'VALUES';
        LStatements[2] := 'COLUMNS';
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

  while St <> '' do
  begin
    J := -1;
    for I := 0 to Length(LStatements) - 1 do
      if LStatements[I] = St then
      begin
        LStatements[I] := ''; //Do not accept it anymore
        J := I;
        Break;
      end;
    if J = -1 then
      raise TJvXMLDatabaseException.CreateResFmt(@RsEUnexpectedStatement, [St]);

    if St = 'FROM' then
      St := ReadTables(LStatements)
    else
    if St = 'WHERE' then
      St := ReadWhere(LStatements)
    else
    if St = 'LIMIT' then
      St := ReadLimit(LStatements)
    else
    if St = 'ORDER' then
      St := ReadOrderBy(LStatements)
    else
    if St = 'SET' then
      St := ReadSet(LStatements)
    else
    if St = 'VALUES' then
      St := ReadValues(LStatements)
    else
    if St = 'COLUMNS' then
      St := ReadColumns(LStatements, False);
  end;
end;

function TJvXMLQueryParser.ReadColumns(const AEndStatement: array of string;
  ACanTerminate: Boolean): string;
begin
  Result := ReadStatement(AEndStatement, ACanTerminate, FColumnsStr);
  DoValidateColumns;
end;

function TJvXMLQueryParser.ReadLimit(const AEndStatement: array of string): string;
var
  I: Integer;
begin
  Result := ReadStatement(AEndStatement, True, FLimitStr);
  I := Pos(',', FLimitStr);
  if I = 0 then
    FLimitCount := StrToIntDef(FLimitStr, MaxInt)
  else
  begin
    FLimitCount := StrToIntDef(Trim(Copy(FLimitStr, I + 1, MaxInt)), MaxInt);
    FLimitBegin := StrToIntDef(Trim(Copy(FLimitStr, 1, I - 1)), 0);
  end;
end;

function TJvXMLQueryParser.ReadOrderBy(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FOrderStr);
  DoValidateOrderBy;
end;

function TJvXMLQueryParser.ReadSet(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FSetStr);
  DoValidateSet;
end;

function TJvXMLQueryParser.ReadStatement(const AEndStatement: array of string;
  ACanTerminate: Boolean; var AValue: string): string;
var
  St, UpSt: string;
  LFound: Boolean;
  I: Integer;
begin
  AValue := '';
  LFound := False;
  Result := '';
  while not LFound do
    if (FQuery = '') and ACanTerminate then
      LFound := True
    else
    begin
      St := ReadToken;
      if St <> '' then
      begin
        UpSt := UpperCase(St);
        for I := 0 to Length(AEndStatement) - 1 do
          if UpSt = AEndStatement[I] then
          begin
            LFound := True;
            Break;
          end;
      end;

      if not LFound then
        AValue := AValue + ' ' + St
      else
        Result := St;
    end;
end;

function TJvXMLQueryParser.ReadTables(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FTablesStr);
  DoValidateTables;
end;

function TJvXMLQueryParser.ReadToken: string;
var
  I: Integer;
begin
  if FQuery = '' then
    raise TJvXMLDatabaseException.CreateRes(@RsEUnexpectedEndOfQuery);

  FQuery := TrimLeft(FQuery);
  I := 1;
  while (I < Length(FQuery)) and not (FQuery[I] in [' ']) do  {,'(',')'}
    Inc(I);
  if I >= Length(FQuery) then
  begin
    Result := Trim(FQuery);
    FQuery := '';
  end
  else
  begin
    Result := Copy(FQuery, 1, I - 1);
    FQuery := Copy(FQuery, I + 1, MaxInt);
  end;
end;

function TJvXMLQueryParser.ReadValues(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FValuesStr);
  DoValidateValues;
end;

function TJvXMLQueryParser.ReadWhere(const AEndStatement: array of string): string;
begin
  Result := ReadStatement(AEndStatement, True, FWhereStr);
  DoValidateWhere;
end;

procedure TJvXMLQueryParser.UpdateRow(ARow: TJvSimpleXMLElem);
var
  I: Integer;
begin
  for I := 0 to FUpdates.Count - 1 do
    TJvXMLQueryAssignement(FUpdates[I]).UpdateElem(ARow);
end;

//=== { TJvXMLQueryColumn } ==================================================

constructor TJvXMLQueryColumn.Create(const AValue: string);
var
  I: Integer;
begin
  inherited Create;
  I := Pos('.', AValue);
  if I <> 0 then
  begin
    Name := Copy(AValue, I + 1, MaxInt);
    Table := Copy(AValue, 1, I - 1);
  end
  else
    Name := AValue;
end;

//=== { TJvXMLQueryTable } ===================================================

constructor TJvXMLQueryTable.Create(const AValue: string);
var
  I: Integer;
begin
  inherited Create;
  I := Pos(' ', AValue);
  if I <> 0 then
  begin
    Name := Copy(AValue, 1, I - 1);
    Alias := Trim(Copy(AValue, I + 1, MaxInt));
  end
  else
    Name := AValue;
end;

//=== { TJvXMLQueryCondition } ===============================================

constructor TJvXMLQueryCondition.Create(AOperator: TJvXMLSQLOperator;
  const ACondition: string);
begin
  inherited Create;
  Self.Operator := AOperator;
  Self.Condition := ACondition;
end;

//=== { TJvXMLQueryOrder } ===================================================

constructor TJvXMLQueryOrder.Create(const AValue: string);
var
  I: Integer;
  St: string;
begin
  inherited Create;
  Column := Trim(AValue);
  Ascending := True;
  Convertion := ocNone;

  I := Pos(' ', Column);
  if I <> 0 then
  begin
    SetLength(Column, I - 1);
    Ascending := Pos('ASC', UpperCase(AValue)) <> 0;
  end;

  I := Pos('(', Column);
  if I <> 0 then
  begin
    St := UpperCase(Copy(Column, 1, I - 1));
    Column := Copy(Column, I + 1, MaxInt);
    SetLength(Column, Length(Column) - 1);

    if St = 'DATE' then
      Convertion := ocDate
    else
    if (St = 'Integer') or (St = 'INT') then
      Convertion := ocInteger
    else
    if St = 'FLOAT' then
      Convertion := ocFloat;
  end;
end;

//=== { TJvXMLQueryAssignement } =============================================

constructor TJvXMLQueryAssignement.Create(AValue: string);
var
  I, J: Integer;
  LDelimiter: Char;
begin
  inherited Create;
  I := Pos('=', AValue);
  if I = 0 then
    // (rom) this definitely neds to be improved
    raise Exception.Create('')
  else
  begin
    Column := Trim(Copy(AValue, 1, I - 1));
    AValue := Trim(Copy(AValue, I + 1, MaxInt));

    if AValue = '' then
      raise Exception.Create('');

    //Determine if column or constant
    if (AValue[1] = '"') or (AValue[1] = '''') then
    begin
      LDelimiter := AValue[1];
      ValueKind := skConstant;
      AValue := Copy(AValue, 2, MaxInt);
      I := 0;
      for J := 1 to Length(AValue) do
        if AValue[J] = LDelimiter then
          if (J=1) or (AValue[J-1] <> '\') then
          begin
            I := J;
            Break;
          end;
      if I <> 0 then
      begin
        Value := Copy(AValue, 1, I - 1);
        Value := StringReplace(Value, '\' + LDelimiter, LDelimiter, [rfReplaceAll]);
        AValue := Trim(Copy(AValue, I + 1, MaxInt));
      end
      else
        raise Exception.Create('');        
    end
    else
    begin
      ValueKind := skColumn;
      I := Pos(' ', AValue);
      if I = 0 then
      begin
        Value := AValue;
        AValue := '';
      end
      else
      begin
        Value := Copy(AValue, 1, I - 1);
        AValue := Trim(Copy(AValue, I + 1, MaxInt));
      end;
    end;

    //Second kind and second value?
    if AValue = '' then
      Operator := soNone
    else
    begin
      case AValue[1] of
        '+':
          Operator := soAdd;
        '-':
          Operator := soSubstract;
        '*':
          Operator := soMultiply;
        '/':
          Operator := soDivide;
        else
          raise Exception.Create('');
      end;

      SecondValue := Trim(Copy(AValue, 2, MaxInt));
      if (SecondValue <> '') and (SecondValue[1] in ['''','"']) then
      begin
        SecondValue := Copy(SecondValue, 2, Length(SecondValue) - 2);
        SecondKind := skConstant;
      end
      else
        SecondKind := skColumn;
    end;
  end;
end;

procedure TJvXMLQueryAssignement.UpdateElem(AElement: TJvSimpleXMLElem);
var
  LValue, LValue2: string;

  function ParseValue(const AValue: string): string;
  begin
    if SameText(AValue, 'now()') then
      Result := DateTimeToStr(Now)
    else
      Result := AValue;
  end;

begin
  if ValueKind = skConstant then
    LValue := Value
  else
    LValue := AElement.Properties.Value(Value, ParseValue(Value));

  if Operator <> soNone then
  begin
    if SecondKind = skConstant then
      LValue2 := SecondValue
    else
      LValue2 := AElement.Properties.Value(SecondValue, ParseValue(SecondValue));
    case Operator of
      soAdd:
        LValue := FloatToStr(StrToFloatDef(LValue,0) + StrToFloatDef(LValue2,0));
      soMultiply:
        LValue := FloatToStr(StrToFloatDef(LValue,0) * StrToFloatDef(LValue2,0));
      soDivide:
        LValue := FloatToStr(StrToFloatDef(LValue,0) / StrToFloatDef(LValue2,0));
      soSubstract:
        LValue := FloatToStr(StrToFloatDef(LValue,0) - StrToFloatDef(LValue2,0));
    end;
  end;

  AElement.Properties.Delete(Column);
  AElement.Properties.Add(Column, LValue);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

