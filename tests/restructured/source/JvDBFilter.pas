{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBFilter.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDBFilter;

interface


{$IFDEF WIN32}
uses SysUtils, Windows, Messages, Classes, Controls, Forms,
  Graphics, Menus, StdCtrls, ExtCtrls, Bde, DB, DBTables, JvTypes, JvComponent;
{$ELSE}
uses SysUtils, WinTypes, WinProcs, Messages, Classes, Controls, Forms,
  Graphics, Menus, StdCtrls, ExtCtrls, DBITypes, DB, DBTables, JvTypes, JvComponent;
{$ENDIF}

type

{ TJvDBFilter }

  TFilterLogicCond = (flAnd, flOr); { for captured DataSet }
  TDBFilterOption = TFilterOption;
  TDBFilterOptions = TFilterOptions;

  TFilterEvent = function (Sender: TObject; DataSet: TDataSet): Boolean of object;

  TDataSetStorage = record { for internal use only }
    FBof: Boolean;
    FEof: Boolean;
    State: TDataSetState;
    CanModify: Boolean;
    BeforePost: TDataSetNotifyEvent;
    BeforeCancel: TDataSetNotifyEvent;
    BeforeInsert: TDataSetNotifyEvent;
    BeforeEdit: TDataSetNotifyEvent;
  end;

  TJvDBFilter = class(TJvComponent)
  private
    FParser: TObject;
    FDataLink: TDataLink;
    FIgnoreDataEvents: Boolean;
    FPriority: Word;
    FOptions: TDBFilterOptions;
    FLogicCond: TFilterLogicCond;
    FFilter: TStrings;
    FExprHandle: hDBIFilter;
    FFuncHandle: hDBIFilter;
    FDataHandle: hDBICur;
    FActive: Boolean;
    FCaptured: Boolean;
    FStreamedActive: Boolean;
    FActivating: Boolean;
    FStorage: TDataSetStorage;
    FOnFiltering: TFilterEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnSetCapture: TNotifyEvent;
    FOnReleaseCapture: TNotifyEvent;
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource;
    function BuildTree: Boolean;
    procedure DestroyTree;
    procedure SetFilter(Value: TStrings);
    procedure SetOptions(Value: TDBFilterOptions);
    procedure SetOnFiltering(const Value: TFilterEvent);
    procedure SetPriority(Value: Word);
    procedure SetLogicCond(Value: TFilterLogicCond);
    function GetFilterText: PChar;
    procedure FilterChanged(Sender: TObject);
    function CreateExprFilter: hDBIFilter;
    function CreateFuncFilter: hDBIFilter;
    procedure DropFilters;
    procedure SetFilterHandle(var Filter: HDBIFilter; Value: HDBIFilter);
    procedure RecreateExprFilter;
    procedure RecreateFuncFilter;
    procedure ActivateFilters;
    procedure DeactivateFilters;
    function RecordFilter(RecBuf: Pointer; RecNo: Longint): Smallint; {$IFDEF WIN32} stdcall; {$ENDIF WIN32}
    procedure BeforeDataPost(DataSet: TDataSet);
    procedure BeforeDataChange(DataSet: TDataSet);
    procedure BeforeDataCancel(DataSet: TDataSet);
    procedure SetActive(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoActivate; dynamic;
    procedure DoDeactivate; dynamic;
    procedure ActiveChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; virtual;
    procedure UpdateFuncFilter;
    procedure Activate;
    procedure Deactivate;
    procedure SetCapture;
    procedure ReleaseCapture;
    procedure ReadCaptureControls;
    property Captured: Boolean read FCaptured;
    property Handle: hDBIFilter read FExprHandle; { obsolete, use ExprFilter }
    property ExprFilter: hDBIFilter read FExprHandle;
    property FuncFilter: hDBIFilter read FFuncHandle;
  published
    property Active: Boolean read FActive write SetActive default False;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Filter: TStrings read FFilter write SetFilter;
    property LogicCond: TFilterLogicCond read FLogicCond write SetLogicCond default flAnd;
    property Options: TDBFilterOptions read FOptions write SetOptions default [];
    property Priority: Word read FPriority write SetPriority default 0;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnFiltering: TFilterEvent read FOnFiltering write SetOnFiltering;
    property OnSetCapture: TNotifyEvent read FOnSetCapture write FOnSetCapture;
    property OnReleaseCapture: TNotifyEvent read FOnReleaseCapture write FOnReleaseCapture;
  end;

  EFilterError = class(EJVCLException);

procedure DropAllFilters(DataSet: TDataSet);
{$IFNDEF COMPILER3_UP}
function SetLookupFilter(DataSet: TDataSet; Field: TField;
  const Value: string; CaseSensitive, Exact: Boolean): HDBIFilter;
{$ENDIF}

implementation

uses {$IFNDEF WIN32} DBIErrs, DBIProcs, JvStr16, {$ENDIF} DBConsts, Dialogs,
  {$IFDEF COMPILER3_UP} DbCommon, {$ENDIF} JvDConst, JvVCLUtils, JvDBUtils, JvBdeUtils;

procedure DropAllFilters(DataSet: TDataSet);
begin
  if (DataSet <> nil) and DataSet.Active then begin
{$IFDEF WIN32}
    DataSet.Filtered := False;
{$ENDIF}
    DbiDropFilter((DataSet as TBDEDataSet).Handle, nil);
    DataSet.CursorPosChanged;
    DataSet.Resync([]);
  end;
end;

{ JvDBFilter exceptions }

procedure FilterError(Ident: Word); near;
begin
  raise EFilterError.CreateRes(Ident);
end;

procedure FilterErrorFmt(Ident: Word; const Args: array of const); near;
begin
  raise EFilterError.CreateResFmt(Ident, Args);
end;

const
  SExprNothing = '""';   { nothing token name          }
  cQuota = '''';         { qoutas for string constants }
  cFldQuotaLeft = '[';   { left qouta for field names  }
  cFldQuotaRight = ']';  { right qouta for field names }

{$IFNDEF COMPILER3_UP} {DbCommon.pas}

{ TJvFilterExpr }

type
  TExprNodeKind = (enField, enConst, enOperator);

  PExprNode = ^TExprNode;
  TExprNode = record
    FNext: PExprNode;
    FKind: TExprNodeKind;
    FPartial: Boolean;
    FOperator: CanOp;
    FData: string;
    FLeft: PExprNode;
    FRight: PExprNode;
  end;

  TJvFilterExpr = class
  private
    FDataSet: TDataSet;
    FOptions: TDBFilterOptions;
    FNodes: PExprNode;
    FExprBuffer: PCANExpr;
    FExprBufSize: Integer;
    FExprNodeSize: Integer;
    FExprDataSize: Integer;
    function FieldFromNode(Node: PExprNode): TField;
    function GetExprData(Pos, Size: Integer): PChar;
    function PutCompareNode(Node: PExprNode): Integer;
    function PutConstStr(const Value: string): Integer;
    function PutConstNode(DataType: Integer; Data: PChar;
      Size: Integer): Integer;
    function PutData(Data: PChar; Size: Integer): Integer;
    function PutExprNode(Node: PExprNode): Integer;
    function PutFieldNode(Field: TField): Integer;
    function PutNode(NodeType: NodeClass; OpType: CanOp;
      OpCount: Integer): Integer;
    procedure SetNodeOp(Node, Index, Data: Integer);
  public
    constructor Create(DataSet: TDataSet; Options: TDBFilterOptions);
    destructor Destroy; override;
    function NewCompareNode(Field: TField; Operator: CanOp;
      const Value: string): PExprNode;
    function NewNode(Kind: TExprNodeKind; Operator: CanOp;
      const Data: string; Left, Right: PExprNode): PExprNode;
    function GetFilterData(Root: PExprNode): PCANExpr;
  end;

constructor TJvFilterExpr.Create(DataSet: TDataSet; Options: TDBFilterOptions);
begin
  FDataSet := DataSet;
  FOptions := Options;
end;

destructor TJvFilterExpr.Destroy;
var
  Node: PExprNode;
begin
  if (FExprBuffer <> nil) then FreeMem(FExprBuffer, FExprBufSize);
  while FNodes <> nil do begin
    Node := FNodes;
    FNodes := Node^.FNext;
    Dispose(Node);
  end;
end;

function TJvFilterExpr.FieldFromNode(Node: PExprNode): TField;
begin
  Result := FDataSet.FieldByName(Node^.FData);
  if Result.Calculated then
    FilterErrorFmt(SExprBadField, [Result.FieldName]);
end;

function TJvFilterExpr.GetExprData(Pos, Size: Integer): PChar;
begin
{$IFDEF WIN32}
  ReallocMem(FExprBuffer, FExprBufSize + Size);
{$ELSE}
  FExprBuffer := ReallocMem(FExprBuffer, FExprBufSize, FExprBufSize + Size);
{$ENDIF}
  Move(PChar(FExprBuffer)[Pos], PChar(FExprBuffer)[Pos + Size],
    FExprBufSize - Pos);
  Inc(FExprBufSize, Size);
  Result := PChar(FExprBuffer) + Pos;
end;

function TJvFilterExpr.GetFilterData(Root: PExprNode): PCANExpr;
begin
  FExprBufSize := SizeOf(CANExpr);
  GetMem(FExprBuffer, FExprBufSize);
  PutExprNode(Root);
  with FExprBuffer^ do begin
    iVer := CANEXPRVERSION;
    iTotalSize := FExprBufSize;
    iNodes := $FFFF;
    iNodeStart := SizeOf(CANExpr);
    iLiteralStart := FExprNodeSize + SizeOf(CANExpr);
  end;
  Result := FExprBuffer;
end;

function TJvFilterExpr.NewCompareNode(Field: TField; Operator: CanOp;
  const Value: string): PExprNode;
var
  Left, Right: PExprNode;
begin
  Left := NewNode(enField, canNOTDEFINED, Field.FieldName, nil, nil);
  Right := NewNode(enConst, canNOTDEFINED, Value, nil, nil);
  Result := NewNode(enOperator, Operator, EmptyStr, Left, Right);
end;

function TJvFilterExpr.NewNode(Kind: TExprNodeKind; Operator: CanOp;
  const Data: string; Left, Right: PExprNode): PExprNode;
begin
  New(Result);
  with Result^ do begin
    FNext := FNodes;
    FKind := Kind;
    FPartial := False;
    FOperator := Operator;
    FData := Data;
    FLeft := Left;
    FRight := Right;
  end;
  FNodes := Result;
end;

function TJvFilterExpr.PutCompareNode(Node: PExprNode): Integer;
const
  ReverseOperator: array[canEQ..canLE] of CanOp = (
    canEQ, canNE, canLT, canGT, canLE, canGE);
var
  Operator: CanOp;
  Left, Right, Temp: PExprNode;
  Field: TField;
  FieldPos, ConstPos, CaseInsensitive, PartialLength, L: Integer;
  S: string;
  Buf: PChar;
begin
  Operator := Node^.FOperator;
  Left := Node^.FLeft;
  Right := Node^.FRight;
  if (Left^.FKind <> enConst) and (Right^.FKind <> enConst) then begin
    if FDataSet.FindField(Left^.FData) = nil then
      Left^.FKind := enConst
    else if FDataSet.FindField(Right^.FData) = nil then
      Right^.FKind := enConst;
  end;
  if (Left^.FKind <> enField) and (Right^.FKind <> enField) then begin
    if FDataSet.FindField(Left^.FData) <> nil then
      Left^.FKind := enField
    else if FDataSet.FindField(Right^.FData) <> nil then
      Right^.FKind := enField;
  end;
  if Right^.FKind = enField then begin
    Temp := Left;
    Left := Right;
    Right := Temp;
    Operator := ReverseOperator[Operator];
  end;
  if (Left^.FKind <> enField) or (Right^.FKind <> enConst) then
    FilterError(SExprBadCompare);
  Field := FieldFromNode(Left);
  if Right^.FData = EmptyStr then
  begin
    case Operator of
      canEQ: Operator := canISBLANK;
      canNE: Operator := canNOTBLANK;
      else FilterError(SExprBadNullTest);
    end;
    Result := PutNode(nodeUNARY, Operator, 1);
    SetNodeOp(Result, 0, PutFieldNode(Field));
  end else
  begin
    if ((Operator = canEQ) or (Operator = canNE)) and
      (Field.DataType = ftString) then
    begin
      S := Right^.FData;
      L := Length(S);
      if L <> 0 then
      begin
        CaseInsensitive := 0;
        PartialLength := 0;
        if foCaseInsensitive in FOptions then CaseInsensitive := 1;
        if Node^.FPartial then PartialLength := L
        else begin
          if not (foNoPartialCompare in FOptions) and (L > 1) and
            (S[L] = '*') then
          begin
            Delete(S, L, 1);
            PartialLength := L - 1;
          end;
        end;
        if (CaseInsensitive <> 0) or (PartialLength <> 0) then begin
          Result := PutNode(nodeCOMPARE, Operator, 4);
          SetNodeOp(Result, 0, CaseInsensitive);
          SetNodeOp(Result, 1, PartialLength);
          SetNodeOp(Result, 2, PutFieldNode(Field));
          SetNodeOp(Result, 3, PutConstStr(S));
          Exit;
        end;
      end;
    end;
    Result := PutNode(nodeBINARY, Operator, 2);
    FieldPos := PutFieldNode(Field);
    S := Right^.FData;
    Buf := AllocMem(Field.DataSize);
    try
      ConvertStringToLogicType((FDataSet as TBDEDataSet).Locale,
        FieldLogicMap(Field.DataType), Field.DataSize, Field.FieldName,
        Right^.FData, Buf);
      ConstPos := PutConstNode(FieldLogicMap(Field.DataType), Buf,
        Field.DataSize);
      SetNodeOp(Result, 0, FieldPos);
      SetNodeOp(Result, 1, ConstPos);
    finally
      FreeMem(Buf, Field.DataSize);
    end;
  end;
end;

function TJvFilterExpr.PutConstNode(DataType: Integer; Data: PChar;
  Size: Integer): Integer;
begin
  Result := PutNode(nodeCONST, canCONST2, 3);
  SetNodeOp(Result, 0, DataType);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
end;

function TJvFilterExpr.PutConstStr(const Value: string): Integer;
var
  Buffer: array[0..255] of Char;
begin
  AnsiToNative((FDataSet as TBDEDataSet).Locale, Value, Buffer,
    SizeOf(Buffer) - 1);
  Result := PutConstNode(fldZSTRING, Buffer, StrLen(Buffer) + 1);
end;

function TJvFilterExpr.PutData(Data: PChar; Size: Integer): Integer;
begin
  Move(Data^, GetExprData(FExprBufSize, Size)^, Size);
  Result := FExprDataSize;
  Inc(FExprDataSize, Size);
end;

function TJvFilterExpr.PutExprNode(Node: PExprNode): Integer;
const
  BoolFalse: WordBool = False;
var
  Field: TField;
begin
  Result := 0;
  case Node^.FKind of
    enField:
      begin
        Field := FieldFromNode(Node);
        if Field.DataType <> ftBoolean then
          FilterErrorFmt(SExprNotBoolean, [Field.FieldName]);
        Result := PutNode(nodeBINARY, canNE, 2);
        SetNodeOp(Result, 0, PutFieldNode(Field));
        SetNodeOp(Result, 1, PutConstNode(fldBOOL, @BoolFalse,
          SizeOf(WordBool)));
      end;
    enOperator:
      case Node^.FOperator of
        canEQ..canLE:
          Result := PutCompareNode(Node);
        canAND, canOR:
          begin
            Result := PutNode(nodeBINARY, Node^.FOperator, 2);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft));
            SetNodeOp(Result, 1, PutExprNode(Node^.FRight));
          end;
        else
          Result := PutNode(nodeUNARY, canNOT, 1);
          SetNodeOp(Result, 0, PutExprNode(Node^.FLeft));
      end; { case Node^.FOperator }
    else FilterError(SExprIncorrect);
  end; { case Node^.FKind }
end;

function TJvFilterExpr.PutFieldNode(Field: TField): Integer;
var
  Buffer: array[0..255] of Char;
begin
  AnsiToNative((FDataSet as TBDEDataSet).Locale, Field.FieldName, Buffer,
    SizeOf(Buffer) - 1);
  Result := PutNode(nodeFIELD, canFIELD2, 2);
  SetNodeOp(Result, 0, Field.FieldNo);
  SetNodeOp(Result, 1, PutData(Buffer, StrLen(Buffer) + 1));
end;

function TJvFilterExpr.PutNode(NodeType: NodeClass; OpType: CanOp;
  OpCount: Integer): Integer;
var
  Size: Integer;
begin
  Size := SizeOf(CANHdr) + OpCount * SizeOf(Word);
  with PCANHdr(GetExprData(SizeOf(CANExpr) + FExprNodeSize, Size))^ do begin
    nodeClass := NodeType;
    canOp := OpType;
  end;
  Result := FExprNodeSize;
  Inc(FExprNodeSize, Size);
end;

procedure TJvFilterExpr.SetNodeOp(Node, Index, Data: Integer);
begin
  PWordArray(PChar(FExprBuffer) + (SizeOf(CANExpr) + Node +
    SizeOf(CANHdr)))^[Index] := Data;
end;

{ SetLookupFilter }

function SetLookupFilter(DataSet: TDataSet; Field: TField;
  const Value: string; CaseSensitive, Exact: Boolean): HDBIFilter;
var
  Options: TDBFilterOptions;
  Filter: TJvFilterExpr;
  Node: PExprNode;
begin
  if not CaseSensitive then Options := [foNoPartialCompare, foCaseInsensitive]
  else Options := [foNoPartialCompare];
  Filter := TJvFilterExpr.Create(DataSet, Options);
  try
    Node := Filter.NewCompareNode(Field, canEQ, Value);
    if not Exact then Node^.FPartial := True;
    Check(DbiAddFilter((DataSet as TBDEDataSet).Handle, 0, 2, False,
      Filter.GetFilterData(Node), nil, Result));
    DataSet.CursorPosChanged;
    DataSet.Resync([]);
  finally
    Filter.Free;
  end;
end;

{ TExprParser }

type
  TExprToken = (etEnd, etSymbol, etName, etLiteral, etLParen, etRParen,
    etEQ, etNE, etGE, etLE, etGT, etLT);

  TExprParser = class
  private
    FFilter: TJvFilterExpr;
    FText: PChar;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FToken: TExprToken;
    FFilterData: PCANExpr;
    FDataSize: Integer;
    procedure NextToken;
    function ParseExpr: PExprNode;
    function ParseExpr2: PExprNode;
    function ParseExpr3: PExprNode;
    function ParseExpr4: PExprNode;
    function ParseExpr5: PExprNode;
    function TokenName: string;
    function TokenSymbolIs(const S: string): Boolean;
  public
    constructor Create(DataSet: TDataSet; const Text: PChar;
      Options: TDBFilterOptions);
    destructor Destroy; override;
    property FilterData: PCANExpr read FFilterData;
    property DataSize: Integer read FDataSize;
  end;

constructor TExprParser.Create(DataSet: TDataSet; const Text: PChar;
  Options: TDBFilterOptions);
var
  Root: PExprNode;
begin
  FFilter := TJvFilterExpr.Create(DataSet, Options);
  FText := Text;
  FSourcePtr := Text;
  NextToken;
  Root := ParseExpr;
  if FToken <> etEnd then FilterError(SExprTermination);
  FFilterData := FFilter.GetFilterData(Root);
  FDataSize := FFilter.FExprBufSize;
end;

destructor TExprParser.Destroy;
begin
  FFilter.Free;
end;

procedure TExprParser.NextToken;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;

begin
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$fe:
      begin
        TokenStart := P;
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
      end;
    cFldQuotaLeft:
      begin
        Inc(P);
        TokenStart := P;
        while (P^ <> cFldQuotaRight) and (P^ <> #0) do Inc(P);
        if P^ = #0 then FilterError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    cQuota: { '''' }
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then FilterError(SExprStringError);
          if P^ = cQuota then begin
            Inc(P);
            if P^ <> cQuota then Break;
          end;
          if L < SizeOf(StrBuf) then begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etLiteral;
      end;
    '-', '0'..'9':
      begin
        TokenStart := P;
        Inc(P);
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etLiteral;
      end;
    '(':
      begin
        Inc(P);
        FToken := etLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := etRParen;
      end;
    '<':
      begin
        Inc(P);
        case P^ of
          '=':
            begin
              Inc(P);
              FToken := etLE;
            end;
          '>':
            begin
              Inc(P);
              FToken := etNE;
            end;
          else FToken := etLT;
        end;
      end;
    '=':
      begin
        Inc(P);
        FToken := etEQ;
      end;
    '>':
      begin
        Inc(P);
        if P^ = '=' then begin
          Inc(P);
          FToken := etGE;
        end
        else FToken := etGT;
      end;
    #0: FToken := etEnd;
    else FilterErrorFmt(SExprInvalidChar, [P^]);
  end;
  FSourcePtr := P;
end;

function TExprParser.ParseExpr: PExprNode;
begin
  Result := ParseExpr2;
  while TokenSymbolIs('OR') do begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canOR, EmptyStr,
      Result, ParseExpr2);
  end;
end;

function TExprParser.ParseExpr2: PExprNode;
begin
  Result := ParseExpr3;
  while TokenSymbolIs('AND') do begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canAND, EmptyStr,
      Result, ParseExpr3);
  end;
end;

function TExprParser.ParseExpr3: PExprNode;
begin
  if TokenSymbolIs('NOT') then begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canNOT, EmptyStr,
      ParseExpr4, nil);
  end 
  else Result := ParseExpr4;
end;

function TExprParser.ParseExpr4: PExprNode;
const
  Operators: array[etEQ..etLT] of CanOp = (
    canEQ, canNE, canGE, canLE, canGT, canLT);
var
  Operator: CanOp;
begin
  Result := ParseExpr5;
  if FToken in [etEQ..etLT] then begin
    Operator := Operators[FToken];
    NextToken;
    Result := FFilter.NewNode(enOperator, Operator, EmptyStr,
      Result, ParseExpr5);
  end;
end;

function TExprParser.ParseExpr5: PExprNode;
begin
  Result := nil;
  case FToken of
    etSymbol:
      if TokenSymbolIs('NULL') then
        Result := FFilter.NewNode(enConst, canNOTDEFINED, EmptyStr, nil, nil)
      else
        Result := FFilter.NewNode(enField, canNOTDEFINED, FTokenString, nil, nil);
    etName:
        Result := FFilter.NewNode(enField, canNOTDEFINED, FTokenString, nil, nil);
    etLiteral:
        Result := FFilter.NewNode(enConst, canNOTDEFINED, FTokenString, nil, nil);
    etLParen:
      begin
        NextToken;
        Result := ParseExpr;
        if FToken <> etRParen then FilterErrorFmt(SExprNoRParen, [TokenName]);
      end;
    else FilterErrorFmt(SExprExpected, [TokenName]);
  end;
  NextToken;
end;

function TExprParser.TokenName: string;
begin
  if (FSourcePtr = FTokenPtr) then Result := SExprNothing
  else begin
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
    Result := '''' + Result + '''';
  end;
end;

function TExprParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = etSymbol) and (CompareText(FTokenString, S) = 0);
end;

{$ENDIF COMPILER3_UP} {DbCommon.pas}

{$IFDEF WIN32}
  {$HINTS OFF}
{$ENDIF}

type
  THackDataSet = class(TDataSet);

{ TNastyDataSet }

{*******************************************************}
{ !! ATTENTION Nasty implementation                     }
{*******************************************************}
{                                                       }
{ These class definitions were copied from TDataSet     }
{ (DB.PAS) and TBDEDataSet (DBTABLES.PAS).              }
{ It is needed to access FState, FBOF, FEOF, FBuffers,  }
{ FRecordCount, FActiveRecord, FCanModify private       }
{ fields of TDataSet.                                   }
{                                                       }
{ Any changes in the underlying classes may cause       }
{ errors in this implementation!                        }
{                                                       }
{*******************************************************}

{$IFDEF COMPILER3_UP}

{$IFDEF COMPILER4_UP}

  PBufferList = TBufferList;

  TNastyDataSet = class(TComponent)
  private
    FFields: TFields;
    FAggFields: TFields;
    FFieldDefs: TFieldDefs;
    FFieldDefList: TFieldDefList;
    FFieldList: TFieldList;
    FDataSources: TList;
    FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FBuffers: TBufferList;
    FCalcBuffer: PChar;
    FBookmarkSize: Integer;
    FCalcFieldsSize: Integer;
    FDesigner: TDataSetDesigner;
    FDisableCount: Integer;
    FBlobFieldCount: Integer;
    FFilterText: string;
    FBlockReadSize: Integer;
    FConstraints: TCheckConstraints;
    FDataSetField: TDataSetField;
    FNestedDataSets: TList;
    FNestedDatasetClass: TClass;
    FReserved: Pointer;
    FFieldNoOfs: Integer;
    { Byte sized data members (for alignment) }
    FFilterOptions: TFilterOptions;
    FState: TDataSetState;
    FEnableEvent: TDataEvent;
    FDisableState: TDataSetState;
    FBOF: Boolean;
    FEOF: Boolean;
  end;

  TBDENastyDataSet = class(TDataSet)
  private
    FHandle: HDBICur;
    FStmtHandle: HDBIStmt;
    FRecProps: RecProps;
    FLocale: TLocale;
    FExprFilter: HDBIFilter;
    FFuncFilter: HDBIFilter;
    FFilterBuffer: PChar;
    FIndexFieldMap: DBIKey;
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
  end;

{$ELSE COMPILER4_UP}

  TNastyDataSet = class(TComponent)
  private
    FFields: TList;
    FFieldDefs: TFieldDefs;
    FDataSources: TList;
    FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FBuffers: PBufferList;
    FCalcBuffer: PChar;
    FBufListSize: Integer;
    FBookmarkSize: Integer;
    FCalcFieldsSize: Integer;
    FBOF: Boolean;
    FEOF: Boolean;
    FModified: Boolean;
    FStreamedActive: Boolean;
    FInternalCalcFields: Boolean;
    FState: TDataSetState;
  end;

  TBDENastyDataSet = class(TDataSet)
  private
    FHandle: HDBICur;
    FRecProps: RecProps;
    FLocale: TLocale;
    FExprFilter: HDBIFilter;
    FFuncFilter: HDBIFilter;
    FFilterBuffer: PChar;
    FIndexFieldMap: DBIKey;
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
  end;

{$ENDIF COMPILER4_UP}

{$ELSE COMPILER3_UP}

  TNastyDataSet = class(TComponent)
  private
    FFields: TList;
    FDataSources: TList;
    FFieldDefs: TFieldDefs;
    FBuffers: PBufferList;
    FBufListSize: Integer;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FHandle: HDBICur;
    FBOF: Boolean;
    FEOF: Boolean;
    FState: TDataSetState;
    FAutoCalcFields: Boolean;
    FDefaultFields: Boolean;
    FCanModify: Boolean;
  end;
  TBDENastyDataSet = TNastyDataSet;

{$ENDIF COMPILER3_UP}

{$IFDEF WIN32}
  {$HINTS ON}
{$ENDIF}

procedure dsSetState(DataSet: TDataSet; Value: TDataSetState);
begin
  TNastyDataSet(DataSet).FState := Value;
end;

procedure dsSetBOF(DataSet: TDataSet; Value: Boolean);
begin
  TNastyDataSet(DataSet).FBOF := Value;
end;

procedure dsSetEOF(DataSet: TDataSet; Value: Boolean);
begin
  TNastyDataSet(DataSet).FEOF := Value;
end;

{$IFDEF COMPILER4_UP}

procedure AssignBuffers(const Source: TBufferList; var Dest: TBufferList);
var
  Len: Integer;
begin
  Len := High(Source) + 1;
  SetLength(Dest, Len);
  Move(Pointer(Source)^, Pointer(Dest)^, Len * SizeOf(PChar));
end;

procedure dsGetBuffers(DataSet: TDataSet; var ABuf: TBufferList);
begin
  with TNastyDataSet(DataSet) do
    AssignBuffers(FBuffers, ABuf);
end;

procedure dsSetBuffers(DataSet: TDataSet; const Value: TBufferList);
begin
  AssignBuffers(Value, TNastyDataSet(DataSet).FBuffers);
end;

{$ELSE COMPILER4_UP}

procedure dsGetBuffers(DataSet: TDataSet; var ABuf: PBufferList);
begin
  ABuf := TNastyDataSet(DataSet).FBuffers;
end;

procedure dsSetBuffers(DataSet: TDataSet; const Value: PBufferList);
begin
  TNastyDataSet(DataSet).FBuffers := Value;
end;

{$ENDIF COMPILER4_UP}

function dsGetRecordCount(DataSet: TDataSet): Integer;
begin
  Result := TNastyDataSet(DataSet).FRecordCount;
end;

procedure dsSetRecordCount(DataSet: TDataSet; Value: Integer);
begin
  TNastyDataSet(DataSet).FRecordCount := Value;
end;

function dsGetActiveRecord(DataSet: TDataSet): Integer;
begin
  Result := TNastyDataSet(DataSet).FActiveRecord;
end;

procedure dsSetActiveRecord(DataSet: TDataSet; Value: Integer);
begin
  TNastyDataSet(DataSet).FActiveRecord := Value;
end;

function dsGetCanModify(DataSet: TBDEDataSet): Boolean;
begin
  Result := TBDENastyDataSet(DataSet).FCanModify;
end;

procedure dsSetCanModify(DataSet: TBDEDataSet; Value: Boolean);
begin
  TBDENastyDataSet(DataSet).FCanModify := Value;
end;

{ TJvFilterDataLink }

type
  TJvFilterDataLink = class(TDataLink)
  private
    FFilter: TJvDBFilter;
  protected
    procedure ActiveChanged; override;
  public
    constructor Create(Filter: TJvDBFilter);
    destructor Destroy; override;
  end;

constructor TJvFilterDataLink.Create(Filter: TJvDBFilter);
begin
  inherited Create;
  FFilter := Filter;
end;

destructor TJvFilterDataLink.Destroy;
begin
  FFilter := nil;
  inherited Destroy;
end;

procedure TJvFilterDataLink.ActiveChanged;
begin
  if FFilter <> nil then FFilter.ActiveChanged;
end;

{$IFNDEF WIN32}
type
  TFilterOption = TDBFilterOption;
  TFilterOptions = TDBFilterOptions;

function FilterCallback(pDBFilter: Longint; RecBuf: Pointer;
  RecNo: Longint): Smallint; export;
begin
  Result := TJvDBFilter(pDBFilter).RecordFilter(RecBuf, RecNo);
end;
{$ENDIF WIN32}

{ TJvDBFilter }

constructor TJvDBFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TJvFilterDataLink.Create(Self);
  FFilter := TStringList.Create;
  TStringList(FFilter).OnChange := FilterChanged;
  FLogicCond := flAnd;
  FIgnoreDataEvents := False;
end;

destructor TJvDBFilter.Destroy;
begin
  TStringList(FFilter).OnChange := nil;
  Deactivate;
  DropFilters;
  FFilter.Free;
  FDataLink.Free;
  inherited Destroy;
end;

procedure TJvDBFilter.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then Active := True;
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else raise;
  end;
end;

function TJvDBFilter.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBFilter.SetDataSource(Value: TDataSource);
var
  DSChange: Boolean;
begin
  if not (csLoading in ComponentState) then ReleaseCapture;
  DSChange := True;
  if (Value <> nil) and (DataSource <> nil) then
    DSChange := (Value.DataSet <> FDataLink.DataSet);
  FIgnoreDataEvents := not DSChange;
  try
    if not (csLoading in ComponentState) then ActiveChanged;
    FDataLink.DataSource := Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  finally
    FIgnoreDataEvents := False;
  end;
end;

procedure TJvDBFilter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) then begin
    if AComponent = DataSource then DataSource := nil;
  end;
end;

function TJvDBFilter.CreateExprFilter: hDBIFilter;
begin
  Result := nil;
  if (FFilter.Count > 0) then
    if BuildTree then
    try
      Check(DbiAddFilter((FDatalink.DataSet as TBDEDataSet).Handle,
        Longint(Self), FPriority, False, pCANExpr(TExprParser(FParser).FilterData), nil,
        Result));
      FDataHandle := TBDEDataSet(FDatalink.DataSet).Handle;
    finally
      DestroyTree;
    end;
end;

function TJvDBFilter.CreateFuncFilter: hDBIFilter;
var
  FuncPriority: Word;
begin
  if (FPriority < $FFFF) and (FExprHandle <> nil) then
    FuncPriority := FPriority + 1
  else FuncPriority := FPriority;
{$IFDEF WIN32}
  Check(DbiAddFilter((FDataLink.DataSet as TBDEDataSet).Handle, Longint(Self),
    FuncPriority, False, nil, PFGENFilter(@TJvDBFilter.RecordFilter),
    Result));
{$ELSE}
  Check(DbiAddFilter(FDataLink.DataSet.Handle, Longint(Self), FuncPriority,
    False, nil, FilterCallback, Result));
{$ENDIF WIN32}
  FDataHandle := TBDEDataSet(FDatalink.DataSet).Handle;
end;

procedure TJvDBFilter.SetFilterHandle(var Filter: HDBIFilter;
  Value: HDBIFilter);
var
  Info: FilterInfo;
begin
  if FActive and FDataLink.Active then begin
    FDataLink.DataSet.CursorPosChanged;
    DbiSetToBegin((FDataLink.DataSet as TBDEDataSet).Handle);
    if (Filter <> nil) and (Filter <> Value) then
      DbiDropFilter((FDataLink.DataSet as TBDEDataSet).Handle, Filter);
    Filter := Value;
    if Filter <> nil then
      DbiActivateFilter((FDataLink.DataSet as TBDEDataSet).Handle, Filter);
  end
  else if FActive and (Filter <> nil) and (FDataHandle <> nil) and
    (FDataLink.DataSet = nil) and (Value = nil) then
  begin
    if DbiGetFilterInfo(FDataHandle, Filter, 0, 0, Info) = DBIERR_NONE then
      DbiDeactivateFilter(FDataHandle, Filter);
    Filter := Value;
  end
  else begin
{$IFNDEF WIN32}
    if (Filter <> nil) and FDatalink.Active then
      DbiDropFilter((FDataLink.DataSet as TBDEDataSet).Handle, Filter);
{$ENDIF}
    Filter := Value;
  end;
end;

procedure TJvDBFilter.DropFilters;
begin
  SetFilterHandle(FExprHandle, nil);
  SetFilterHandle(FFuncHandle, nil);
  FDataHandle := nil;
  FActive := False;
end;

procedure TJvDBFilter.ActivateFilters;
begin
  if FExprHandle <> nil then
    DbiActivateFilter((FDataLink.DataSet as TBDEDataSet).Handle, FExprHandle);
  if FFuncHandle <> nil then
    DbiActivateFilter((FDataLink.DataSet as TBDEDataSet).Handle, FFuncHandle);
end;

procedure TJvDBFilter.DeactivateFilters;
begin
  if (FFuncHandle <> nil) then
    DbiDeactivateFilter(TBDEDataSet(FDatalink.DataSet).Handle, FFuncHandle);
  if (FExprHandle <> nil) then
    DbiDeactivateFilter(TBDEDataSet(FDatalink.DataSet).Handle, FExprHandle);
end;

function TJvDBFilter.RecordFilter(RecBuf: Pointer; RecNo: Longint): Smallint;
var
  ACanModify: Boolean;
  Buffers: PBufferList;
{$IFDEF COMPILER4_UP}
  BufPtr: TBufferList;
{$ENDIF}
  ActiveRecord: Integer;
  RecCount: Integer;
  DS: TBDEDataSet;
begin
  Result := Ord(True);
  if Assigned(FOnFiltering) and (FFuncHandle <> nil) then
  try
    DS := FDatalink.DataSet as TBDEDataSet;
    { save current DataSet's private fields values }
    dsGetBuffers(DS, Buffers);
    ActiveRecord := dsGetActiveRecord(DS);
    RecCount := dsGetRecordCount(DS);
    ACanModify := dsGetCanModify(DS);
    try
      dsSetActiveRecord(DS, 0);
      dsSetRecordCount(DS, 1); { FActiveRecord + 1 }
      dsSetCanModify(DS, False);
{$IFDEF COMPILER4_UP}
      SetLength(BufPtr, 1);
      BufPtr[0] := PChar(RecBuf);
      dsSetBuffers(DS, BufPtr);
{$ELSE}
      dsSetBuffers(DS, @PChar(RecBuf));
{$ENDIF}
      { call user defined function }
      Result := Ord(FOnFiltering(Self, DS));
    finally
      dsSetCanModify(DS, ACanModify);
      dsSetActiveRecord(DS, ActiveRecord);
      dsSetRecordCount(DS, RecCount);
      dsSetBuffers(DS, Buffers);
    end;
  except
    Application.HandleException(Self);
    Result := ABORT; { BDE constant, not SysUtils.pas procedure }
  end;
end;

procedure TJvDBFilter.FilterChanged(Sender: TObject);
begin
  RecreateExprFilter;
end;

procedure TJvDBFilter.SetOnFiltering(const Value: TFilterEvent);
begin
  if Assigned(FOnFiltering) <> Assigned(Value) then begin
    FOnFiltering := Value;
    RecreateFuncFilter;
  end else FOnFiltering := Value;
end;

procedure TJvDBFilter.RecreateFuncFilter;
var
  Filter: HDBIFilter;
begin
  if FDataLink.Active and not (csReading in ComponentState) then
  begin
    if not FCaptured then FDataLink.DataSet.CheckBrowseMode;
    if Assigned(FOnFiltering) then Filter := CreateFuncFilter
    else Filter := nil;
    SetFilterHandle(FFuncHandle, Filter);
  end;
  if FDataLink.Active and Active and not FCaptured then
    FDataLink.DataSet.First;
end;

procedure TJvDBFilter.RecreateExprFilter;
var
  Filter: HDBIFilter;
begin
  if FDataLink.Active and not (csReading in ComponentState) then begin
    if not FCaptured then FDataLink.DataSet.CheckBrowseMode;
    if (FFilter.Count > 0) then
      try
        Filter := CreateExprFilter;
      except
        if Active or FActivating then raise
        else Filter := nil;
      end
    else Filter := nil;
    SetFilterHandle(FExprHandle, Filter);
  end;
  if FDataLink.Active and Active and not FCaptured then
    FDataLink.DataSet.First;
end;

procedure TJvDBFilter.SetFilter(Value: TStrings);
begin
  FFilter.Assign(Value);
end;

procedure TJvDBFilter.SetOptions(Value: TDBFilterOptions);
begin
  if Value <> FOptions then begin
    FOptions := Value;
    RecreateExprFilter;
  end;
end;

procedure TJvDBFilter.SetLogicCond(Value: TFilterLogicCond);
begin
  FLogicCond := Value;
end;

procedure TJvDBFilter.SetPriority(Value: Word);
begin
  if FPriority <> Value then begin
    FPriority := Value;
    Update;
  end;
end;

function TJvDBFilter.GetFilterText: PChar;
var
  BufLen: Word;
  I: Integer;
  StrEnd: PChar;
  StrBuf: array[0..255] of Char;
begin
  BufLen := 1;
  for I := 0 to FFilter.Count - 1 do
    Inc(BufLen, Length(Filter.Strings[I]) + 1);
  Result := StrAlloc(BufLen);
  try
    StrEnd := Result;
    for I := 0 to Filter.Count - 1 do begin
      if Filter.Strings[I] <> '' then begin
        StrPCopy(StrBuf, Filter.Strings[I]);
        StrEnd := StrECopy(StrEnd, StrBuf);
        StrEnd := StrECopy(StrEnd, ' ');
      end;
    end;
  except
    StrDispose(Result);
    raise;
  end;
end;

procedure TJvDBFilter.DestroyTree;
begin
  if FParser <> nil then begin
    FParser.Free;
    FParser := nil;
  end;
end;

procedure TJvDBFilter.BeforeDataPost(DataSet: TDataSet);
begin
  ReadCaptureControls;
  ReleaseCapture;
  Activate;
  SysUtils.Abort;
end;

procedure TJvDBFilter.BeforeDataChange(DataSet: TDataSet);
begin
  FilterError(SCaptureFilter);
end;

procedure TJvDBFilter.BeforeDataCancel(DataSet: TDataSet);
begin
  ReleaseCapture;
end;

function TJvDBFilter.BuildTree: Boolean;
var
  Expr: PChar;
  I: Integer;
begin
  Result := True;
  if not FDataLink.Active then _DBError(SDataSetClosed);
  TStringList(FFilter).OnChange := nil;
  try
    for I := FFilter.Count - 1 downto 0 do
      if FFilter[I] = '' then FFilter.Delete(I);
  finally
    TStringList(FFilter).OnChange := FilterChanged;
  end;
  if FFilter.Count = 0 then begin
    Result := False;
    Exit;
  end;
  Expr := GetFilterText;
  try
    if StrLen(Expr) = 0 then begin
      Result := False;
      Exit;
    end;
    FParser := TExprParser.Create(FDataLink.DataSet, Expr,
      TFilterOptions(FOptions) {$IFDEF COMPILER4_UP}, [], '', nil {$ENDIF}
      {$IFDEF COMPILER5_UP}, FldTypeMap {$ENDIF});
  finally
    StrDispose(Expr);
  end;
end;

procedure TJvDBFilter.DoActivate;
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
end;

procedure TJvDBFilter.DoDeactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TJvDBFilter.SetActive(Value: Boolean);
var
  Bookmark: TBookmark;
begin
  if (csReading in ComponentState) then
    FStreamedActive := Value
  else if FDatalink.Active then begin
    FDatalink.DataSet.CheckBrowseMode;
    if FActive <> Value then begin
      if Value then begin
        FActivating := True;
        try
          if FCaptured then FilterError(SCaptureFilter);
          DbiSetToBegin((FDatalink.DataSet as TBDEDataSet).Handle);
          if FExprHandle = nil then RecreateExprFilter;
          if FFuncHandle = nil then RecreateFuncFilter;
          ActivateFilters;
          FDatalink.DataSet.First;
          FActive := Value;
          DoActivate;
        finally
          FActivating := False;
        end;
      end
      else begin
        if not IsDataSetEmpty(FDatalink.DataSet) then
          Bookmark := FDatalink.DataSet.GetBookmark
        else Bookmark := nil;
        try
          DbiSetToBegin((FDatalink.DataSet as TBDEDataSet).Handle);
          DeactivateFilters;
          if not SetToBookmark(FDatalink.DataSet, Bookmark) then
            FDatalink.DataSet.First;
        finally
          FDatalink.DataSet.FreeBookmark(Bookmark);
        end;
        FActive := Value;
        DoDeactivate;
      end;
      FActive := Value;
    end;
  end
  else FActive := Value;
end;

procedure TJvDBFilter.Activate;
begin
  SetActive(True);
end;

procedure TJvDBFilter.Deactivate;
begin
  SetActive(False);
end;

procedure TJvDBFilter.SetCapture;
begin
  if not FCaptured and (FDataLink <> nil) then begin
    if not FDataLink.Active then _DBError(SDataSetClosed);
    DataSource.DataSet.CheckBrowseMode;
    Deactivate;
    FIgnoreDataEvents := True;
    { store private fields values }
    with FStorage do begin
      FBof := DataSource.DataSet.Bof;
      FEof := DataSource.DataSet.Eof;
      State := DataSource.DataSet.State;
      CanModify := dsGetCanModify(FDatalink.DataSet as TBDEDataSet);
      BeforePost := DataSource.DataSet.BeforePost;
      BeforeCancel := DataSource.DataSet.BeforeCancel;
      BeforeInsert := DataSource.DataSet.BeforeInsert;
      BeforeEdit := DataSource.DataSet.BeforeEdit;
    end;
    DbiInitRecord((DataSource.DataSet as TBDEDataSet).Handle,
      DataSource.DataSet.ActiveBuffer);
    dsSetBOF(DataSource.DataSet, True);
    dsSetEOF(DataSource.DataSet, True);
    dsSetState(DataSource.DataSet, dsEdit);
    dsSetCanModify(DataSource.DataSet as TBDEDataSet, True);
    DataSource.DataSet.BeforeCancel := BeforeDataCancel;
    DataSource.DataSet.BeforePost := BeforeDataPost;
    DataSource.DataSet.BeforeInsert := BeforeDataChange;
    DataSource.DataSet.BeforeEdit := BeforeDataChange;
    THackDataSet(DataSource.DataSet).DataEvent(deUpdateState, 0);
    THackDataSet(DataSource.DataSet).DataEvent(deDataSetChange, 0);
    {DataSource.DataSet := DataSource.DataSet;}
    FCaptured := True;
    if Assigned(FOnSetCapture) then FOnSetCapture(Self);
  end;
end;

procedure TJvDBFilter.ReleaseCapture;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and FCaptured then
  begin
    { restore private fields values stored in SetCapture }
    with FStorage do begin
      dsSetBOF(DataSource.DataSet, FBof);
      dsSetEOF(DataSource.DataSet, FEof);
      dsSetState(DataSource.DataSet, State);
      dsSetCanModify(DataSource.DataSet as TBDEDataSet, CanModify);
      DataSource.DataSet.BeforePost := BeforePost;
      DataSource.DataSet.BeforeCancel := BeforeCancel;
      DataSource.DataSet.BeforeInsert := BeforeInsert;
      DataSource.DataSet.BeforeEdit := BeforeEdit;
    end;
    FCaptured := False;
    FIgnoreDataEvents := False;
    DataSource.DataSet.Resync([]);
    THackDataSet(DataSource.DataSet).DataEvent(deUpdateState, 0);
    THackDataSet(DataSource.DataSet).DataEvent(deDataSetChange, 0);
    {DataSource.DataSet := DataSource.DataSet;}
    if Assigned(FOnReleaseCapture) then FOnReleaseCapture(Self);
    ActiveChanged;
  end;
end;

procedure TJvDBFilter.ReadCaptureControls;
const
  LogicStr: array[TFilterLogicCond] of string[4] = (' AND', ' OR');
var
  I: Integer;
  Field: TField;
  S: string;
begin
  if FCaptured then begin
    FFilter.BeginUpdate;
    try
      FFilter.Clear;
      with FDatalink.DataSet do begin
        UpdateRecord;
        for I := 0 to FieldCount - 1 do begin
          Field := Fields[I];
          if not (Field.IsNull or Field.Calculated {$IFDEF WIN32}
            or Field.Lookup {$ENDIF}) then
          begin
            S := '(' + cFldQuotaLeft + Field.FieldName + cFldQuotaRight +
              '=' + cQuota + Field.AsString + cQuota + ')';
            if FFilter.Count > 0 then S := S + LogicStr[FLogicCond];
            FFilter.Insert(0, S);
          end;
        end;
      end;
    finally
      FFilter.EndUpdate;
    end;
  end
  else FilterError(SNotCaptureFilter);
end;

procedure TJvDBFilter.UpdateFuncFilter;
begin
  if FDataLink.Active and Active and (FFuncHandle <> nil) then
    with FDatalink.DataSet as TBDEDataSet do begin
      DisableControls;
      try
        DbiDeactivateFilter(Handle, FFuncHandle);
        DbiActivateFilter(Handle, FFuncHandle);
        {CursorPosChanged; Resync([]);}
        First;
      finally
        EnableControls;
      end;
    end;
end;

procedure TJvDBFilter.Update;
begin
  if FDataLink.Active and Active then begin
    FDatalink.DataSet.DisableControls;
    try
      RecreateExprFilter;
      RecreateFuncFilter;
      {DeactivateFilters; ActivateFilters;}
    finally
      FDatalink.DataSet.EnableControls;
    end;
  end
  else DeactivateFilters;
end;

procedure TJvDBFilter.ActiveChanged;
var
  WasActive: Boolean;
begin
  if not FIgnoreDataEvents then begin
    WasActive := Active;
    DropFilters;
    if not (csDestroying in ComponentState) then begin
      RecreateExprFilter;
      RecreateFuncFilter;
      if WasActive then Activate;
    end;
  end;
end;

end.
