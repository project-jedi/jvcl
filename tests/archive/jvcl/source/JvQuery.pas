{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQuery.PAS, released on 2002-07-04.

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

unit JvQuery;

interface

uses
  Bde,
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF}
  Classes, SysUtils, DB, DBTables,
  JvStrUtils, JvBdeUtils {, JvComponent};

{.$DEFINE DEBUG}

const
  DefaultMacroChar = '%';
  DefaultTermChar = '/';

type
  TQueryOpenStatus = (qsOpened, qsExecuted, qsFailed);

  TJvQuery = class(TQuery)
  private
    FDisconnectExpected: Boolean;
    FSaveQueryChanged: TNotifyEvent;
    FMacroChar: Char;
    FMacros: TParams;
    FSQLPattern: TStrings;
    FStreamPatternChanged: Boolean;
    FPatternChanged: Boolean;
    FOpenStatus: TQueryOpenStatus;
    {$IFNDEF WIN32}
    FParamCheck: Boolean;
    {$ENDIF}
    function GetMacros: TParams;
    procedure SetMacros(Value: TParams);
    procedure SetSQL(Value: TStrings);
    procedure PatternChanged(Sender: TObject);
    procedure QueryChanged(Sender: TObject);
    procedure RecreateMacros;
    procedure CreateMacros(List: TParams; const Value: PChar);
    procedure Expand(Query: TStrings);
    function GetMacroCount: Word;
    procedure SetMacroChar(Value: Char);
    function GetRealSQL: TStrings;
    {$IFDEF DEBUG}
    procedure SetRealSQL(Value: TStrings);
    {$ENDIF DEBUG}
  protected
    {$IFDEF COMPILER3_UP}
    procedure InternalFirst; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    {$ENDIF}
    procedure Loaded; override;
    function CreateHandle: HDBICur; override;
    procedure OpenCursor{$IFDEF COMPILER3_UP}(InfoQuery: Boolean){$ENDIF}; override;
    procedure Disconnect; override;
    {$IFDEF COMPILER5_UP}
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetTableName: string; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExpandMacros;
    procedure ExecSQL;
    procedure Prepare;
    procedure OpenOrExec(ChangeLive: Boolean);
    procedure ExecDirect;
    function MacroByName(const Value: string): TParam;
    {$IFNDEF COMPILER3_UP}
    function IsEmpty: Boolean;
    {$ENDIF COMPILER3_UP}
    property MacroCount: Word read GetMacroCount;
    property OpenStatus: TQueryOpenStatus read FOpenStatus;
    {$IFNDEF DEBUG}
    property RealSQL: TStrings read GetRealSQL;
    {$ENDIF DEBUG}
  published
    {$IFNDEF WIN32}
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    {$ENDIF}
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property SQL: TStrings read FSQLPattern write SetSQL;
    {$IFDEF DEBUG}
    property RealSQL: TStrings read GetRealSQL write SetRealSQL stored False;
    {$ENDIF DEBUG}
    property Macros: TParams read GetMacros write SetMacros;
  end;

  {$IFDEF WIN32}

  TRunQueryMode = (rqOpen, rqExecute, rqExecDirect, rqOpenOrExec);

  TJvQueryThread = class(TThread)
  private
    FData: TBDEDataSet;
    FMode: TRunQueryMode;
    FPrepare: Boolean;
    FException: TObject;
    procedure DoHandleException;
  protected
    procedure ModeError; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure HandleException; virtual;
  public
    constructor Create(Data: TBDEDataSet; RunMode: TRunQueryMode;
      Prepare, CreateSuspended: Boolean);
  end;

  {$ENDIF WIN32}

  TScriptAction = (saFail, saAbort, saRetry, saIgnore, saContinue);

  TScriptErrorEvent = procedure(Sender: TObject; E: EDatabaseError;
    LineNo, StatementNo: Integer; var Action: TScriptAction) of object;

  TJvSQLScript = class(TComponent)
  private
    FSQL: TStrings;
    FParams: TParams;
    FQuery: TJvQuery;
    FTransaction: Boolean;
    FSemicolonTerm: Boolean;
    FIgnoreParams: Boolean;
    FTerm: Char;
    FBeforeExec: TNotifyEvent;
    FAfterExec: TNotifyEvent;
    FOnScriptError: TScriptErrorEvent;
    {$IFDEF WIN32}
    function GetSessionName: string;
    procedure SetSessionName(const Value: string);
    function GetDBSession: TSession;
    function GetText: string;
    {$ENDIF WIN32}
    {$IFDEF COMPILER4_UP}
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    {$ENDIF COMPILER4_UP}
    function GetDatabase: TDatabase;
    function GetDatabaseName: string;
    procedure SetDatabaseName(const Value: string);
    procedure CreateParams(List: TParams; const Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure SetQuery(Value: TStrings);
    procedure SetParamsList(Value: TParams);
    function GetParamsCount: Cardinal;
  protected
    {$IFDEF COMPILER4_UP}
    procedure DefineProperties(Filer: TFiler); override;
    {$ENDIF COMPILER4_UP}
    procedure CheckExecQuery(LineNo, StatementNo: Integer);
    procedure ExecuteScript(StatementNo: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    procedure ExecStatement(StatementNo: Integer);
    function ParamByName(const Value: string): TParam;
    {$IFDEF WIN32}
    property DBSession: TSession read GetDBSession;
    property Text: string read GetText;
    {$ELSE}
    function GetText: PChar;
    {$ENDIF WIN32}
    property Database: TDatabase read GetDatabase;
    property ParamCount: Cardinal read GetParamsCount;
  published
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property IgnoreParams: Boolean read FIgnoreParams write FIgnoreParams default False;
    property SemicolonTerm: Boolean read FSemicolonTerm write FSemicolonTerm default True;
    {$IFDEF WIN32}
    property SessionName: string read GetSessionName write SetSessionName;
    {$ENDIF WIN32}
    property Term: Char read FTerm write FTerm default DefaultTermChar;
    property SQL: TStrings read FSQL write SetQuery;
    property Params: TParams read FParams write SetParamsList{$IFDEF COMPILER4_UP} stored False{$ENDIF};
    property Transaction: Boolean read FTransaction write FTransaction;
    property BeforeExec: TNotifyEvent read FBeforeExec write FBeforeExec;
    property AfterExec: TNotifyEvent read FAfterExec write FAfterExec;
    property OnScriptError: TScriptErrorEvent read FOnScriptError write FOnScriptError;
  end;

const
  dbfExecScript = dbfTable;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);

implementation

uses
  Consts, Forms,
  {$IFDEF COMPILER3_UP}
  BDEConst,
  {$ENDIF}
  {$IFNDEF WIN32}
  JvStr16,
  {$ENDIF}
  JvDBUtils, JvVCLUtils;

{ Parse SQL utility routines }

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := (C in [' ', ',', ';', ')', #13, #10]) or (C in Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then
    Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do
      begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then
            EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else
        Name := StrPas(StartPos + 1);
      if Assigned(List) then
      begin
        {$IFDEF COMPILER4_UP}
        if List.FindParam(Name) = nil then
        begin
        {$ENDIF COMPILER4_UP}
          if Macro then
            List.CreateParam(ftString, Name, ptInput).AsString := TrueExpr
          else
            List.CreateParam(ftUnknown, Name, ptUnknown);
        {$IFDEF COMPILER4_UP}
        end;
        {$ENDIF COMPILER4_UP}
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else
    if IsLiteral(CurChar) then
      Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

//=== TJvQuery ===============================================================

constructor TJvQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF WIN32}
  FParamCheck := True;
  {$ENDIF WIN32}
  FOpenStatus := qsFailed;
  FSaveQueryChanged := TStringList(inherited SQL).OnChange;
  TStringList(inherited SQL).OnChange := QueryChanged;
  FMacroChar := DefaultMacroChar;
  FSQLPattern := TStringList.Create;
  TStringList(SQL).OnChange := PatternChanged;
  FMacros := TParams.Create{$IFDEF COMPILER4_UP}(Self){$ENDIF};
end;

destructor TJvQuery.Destroy;
begin
  Destroying;
  Disconnect;
  FMacros.Free;
  FSQLPattern.Free;
  inherited Destroy;
end;

procedure TJvQuery.Loaded;
begin
  inherited Loaded;
  GetMacros; {!! trying this way}
end;

{$IFDEF COMPILER3_UP}

procedure TJvQuery.InternalFirst;
begin
  if not (UniDirectional and BOF) then
    inherited InternalFirst;
end;

function TJvQuery.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  //!!!!!!
  if UniDirectional and (GetMode in [gmPrior, gmNext]) then
    DoCheck := False;
  Result := inherited GetRecord(Buffer, GetMode, DoCheck);
end;

{$ENDIF}

function TJvQuery.CreateHandle: HDBICur;
begin
  FOpenStatus := qsFailed;
  Result := inherited CreateHandle;
  if Result = nil then
    FOpenStatus := qsExecuted
  else
    FOpenStatus := qsOpened;
end;

procedure TJvQuery.OpenCursor;
begin
  ExpandMacros;
  inherited OpenCursor{$IFDEF COMPILER3_UP}(InfoQuery){$ENDIF};
end;

procedure TJvQuery.ExecSQL;
begin
  ExpandMacros;
  inherited ExecSQL;
end;

procedure TJvQuery.Prepare;
begin
  ExpandMacros;
  inherited Prepare;
end;

procedure TJvQuery.OpenOrExec(ChangeLive: Boolean);

  procedure TryOpen;
  begin
    try
      Open;
    except
      if OpenStatus <> qsExecuted then
        raise;
    end;
  end;

begin
  try
    TryOpen;
  except
    on E: EDatabaseError do
      if RequestLive and ChangeLive then
      begin
        RequestLive := False;
        try
          TryOpen;
        except
          on E: EDatabaseError do
            if OpenStatus <> qsOpened then
              ExecDirect
            else
            begin
              FOpenStatus := qsFailed;
              raise;
            end;
        else
          raise;
        end;
      end
      else
      begin
        if OpenStatus <> qsOpened then
          ExecDirect
        else
        begin
          FOpenStatus := qsFailed;
          raise;
        end;
      end;
  else
    raise;
  end;
end;

procedure TJvQuery.ExecDirect;
{$IFNDEF WIN32}
var
  P: PChar;
{$ENDIF}
begin
  CheckInactive;
  SetDBFlag(dbfExecSQL, True);
  try
    if SQL.Count > 0 then
    begin
      FOpenStatus := qsFailed;
      {$IFDEF WIN32}
      Check(DbiQExecDirect(DBHandle, qryLangSQL, PChar(inherited SQL.Text),
        nil));
      {$ELSE}
      P := inherited SQL.GetText;
      try
        Check(DbiQExecDirect(DBHandle, qryLangSQL, P, nil));
      finally
        StrDispose(P);
      end;
      {$ENDIF WIN32}
      FOpenStatus := qsExecuted;
    end
    else
      _DBError(SEmptySQLStatement);
  finally
    SetDBFlag(dbfExecSQL, False);
  end;
end;

procedure TJvQuery.Disconnect;
var
  Strings: TStrings;
  Event1, Event2: TNotifyEvent;
begin
  inherited Disconnect;
  if csDestroying in ComponentState then
    Exit;
  Strings := inherited SQL;
  Event1 := TStringList(Strings).OnChange;
  Event2 := QueryChanged;
  if @Event1 <> @Event2 then
  begin
    if not FDisconnectExpected then
      SQL := inherited SQL;
    TStringList(inherited SQL).OnChange := QueryChanged;
  end;
end;

procedure TJvQuery.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then
  begin
    FMacroChar := Value;
    RecreateMacros;
  end;
end;

function TJvQuery.GetMacros: TParams;
begin
  if FStreamPatternChanged then
  begin
    FStreamPatternChanged := False;
    PatternChanged(nil);
  end;
  Result := FMacros;
end;

procedure TJvQuery.SetMacros(Value: TParams);
begin
  FMacros.AssignValues(Value);
end;

procedure TJvQuery.SetSQL(Value: TStrings);
begin
  inherited Disconnect;
  TStringList(FSQLPattern).OnChange := nil;
  FSQLPattern.Assign(Value);
  TStringList(FSQLPattern).OnChange := PatternChanged;
  PatternChanged(nil);
end;

procedure TJvQuery.PatternChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
  begin
    FStreamPatternChanged := True;
    Exit;
  end;
  inherited Disconnect;
  RecreateMacros;
  FPatternChanged := True;
  try
    ExpandMacros;
  finally
    FPatternChanged := False;
  end;
end;

procedure TJvQuery.QueryChanged(Sender: TObject);
{$IFNDEF WIN32}
var
  List: TParams;
  SaveParams: Boolean;
{$ENDIF}
begin
  {$IFDEF WIN32}
  FSaveQueryChanged(Sender);
  {$ELSE}
  SaveParams := not (ParamCheck or (csDesigning in ComponentState));
  if SaveParams then
    List := TParams.Create{$IFDEF COMPILER4_UP}(Self){$ENDIF};
  try
    if SaveParams then
      List.Assign(Params);
    FSaveQueryChanged(Sender);
    if SaveParams then
      Params.Assign(List);
  finally
    if SaveParams then
      List.Free;
  end;
  {$ENDIF WIN32}
  if not FDisconnectExpected then
  begin
    SQL := inherited SQL;
  end;
end;

procedure TJvQuery.ExpandMacros;
var
  ExpandedSQL: TStringList;
begin
  if not FPatternChanged and not FStreamPatternChanged and
    (MacroCount = 0) then
    Exit;
  ExpandedSQL := TStringList.Create;
  try
    Expand(ExpandedSQL);
    FDisconnectExpected := True;
    try
      inherited SQL := ExpandedSQL;
    finally
      FDisconnectExpected := False;
    end;
  finally
    ExpandedSQL.Free;
  end;
end;

procedure TJvQuery.RecreateMacros;
var
  List: TParams;
  {$IFNDEF WIN32}
  P: PChar;
  {$ENDIF}
begin
  {$IFDEF COMPILER4_UP}
  if not (csReading in ComponentState) then
  begin
    {$ENDIF COMPILER4_UP}
    List := TParams.Create{$IFDEF COMPILER4_UP}(Self){$ENDIF};
    try
      {$IFDEF WIN32}
      CreateMacros(List, PChar(FSQLPattern.Text));
      {$ELSE}
      P := FSQLPattern.GetText;
      try
        CreateMacros(List, P);
      finally
        StrDispose(P);
      end;
      {$ENDIF WIN32}
      List.AssignValues(FMacros);
    {$IFDEF COMPILER4_UP}
      FMacros.Clear;
      FMacros.Assign(List);
    finally
      {$ELSE}
      FMacros.Free;
      FMacros := List;
    except
    {$ENDIF COMPILER4_UP}
      List.Free;
    end;
  {$IFDEF COMPILER4_UP}
  end
  else
  begin
    FMacros.Clear;
    CreateMacros(FMacros, PChar(FSQLPattern.Text));
  end;
  {$ENDIF COMPILER4_UP}
end;

procedure TJvQuery.CreateMacros(List: TParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, True, MacroChar, ['.']);
end;

procedure TJvQuery.Expand(Query: TStrings);
var
  I: Integer;

  function ReplaceString(const S: string): string;
  var
    I, J, P, LiteralChars: Integer;
    Param: TParam;
    Found: Boolean;
  begin
    Result := S;
    for I := Macros.Count - 1 downto 0 do
    begin
      Param := Macros[I];
      if Param.DataType = ftUnknown then
        Continue;
      repeat
        P := Pos(MacroChar + Param.Name, Result);
        Found := (P > 0) and ((Length(Result) = P + Length(Param.Name)) or
          NameDelimiter(Result[P + Length(Param.Name) + 1], ['.']));
        if Found then
        begin
          LiteralChars := 0;
          for J := 1 to P - 1 do
            if IsLiteral(Result[J]) then
              Inc(LiteralChars);
          Found := LiteralChars mod 2 = 0;
          if Found then
          begin
            Result := Copy(Result, 1, P - 1) + Param.Text + Copy(Result,
              P + Length(Param.Name) + 1, MaxInt);
          end;
        end;
      until not Found;
    end;
  end;

begin
  for I := 0 to FSQLPattern.Count - 1 do
    Query.Add(ReplaceString(FSQLPattern[I]));
end;

function TJvQuery.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

function TJvQuery.MacroByName(const Value: string): TParam;
begin
  Result := FMacros.ParamByName(Value);
end;

{$IFNDEF COMPILER3_UP}
function TJvQuery.IsEmpty: Boolean;
begin
  Result := IsDataSetEmpty(Self);
end;
{$ENDIF COMPILER3_UP}

function TJvQuery.GetRealSQL: TStrings;
begin
  try
    ExpandMacros;
  except
  end;
  Result := inherited SQL;
end;

{$IFDEF COMPILER5_UP}

function TJvQuery.PSGetDefaultOrder: TIndexDef;
begin
  ExpandMacros;
  Result := inherited PSGetDefaultOrder;
end;

function TJvQuery.PSGetTableName: string;
begin
  ExpandMacros;
  Result := inherited PSGetTableName;
end;

procedure TJvQuery.PSExecute;
begin
  ExecSQL;
end;

{$ENDIF COMPILER5_UP}

{$IFDEF DEBUG}
procedure TJvQuery.SetRealSQL(Value: TStrings);
begin
end;
{$ENDIF DEBUG}

//=== TJvQueryThread =========================================================

{$IFDEF WIN32}

constructor TJvQueryThread.Create(Data: TBDEDataSet; RunMode: TRunQueryMode;
  Prepare, CreateSuspended: Boolean);
begin
  inherited Create(True);
  FData := Data;
  FMode := RunMode;
  FPrepare := Prepare;
  FreeOnTerminate := True;
  FData.DisableControls;
  if not CreateSuspended then
    Resume;
end;

procedure TJvQueryThread.DoTerminate;
begin
  Synchronize(FData.EnableControls);
  inherited DoTerminate;
end;

procedure TJvQueryThread.ModeError;
begin
  SysUtils.Abort;
end;

procedure TJvQueryThread.DoHandleException;
begin
  if (FException is Exception) and not (FException is EAbort) then
  begin
    if Assigned(Application.OnException) then
      Application.OnException(FData, Exception(FException))
    else
      Application.ShowException(Exception(FException));
  end;
end;

procedure TJvQueryThread.HandleException;
begin
  FException := TObject(ExceptObject);
  Synchronize(DoHandleException);
end;

procedure TJvQueryThread.Execute;
begin
  try
    if FPrepare and not (FMode in [rqExecDirect]) then
    begin
      if FData is TJvQuery then
        TJvQuery(FData).Prepare
      else
      if FData is TQuery then
        TQuery(FData).Prepare
      else
      if FData is TStoredProc then
        TStoredProc(FData).Prepare;
    end;
    case FMode of
      rqOpen:
        FData.Open;
      rqExecute:
        begin
          if FData is TJvQuery then
            TJvQuery(FData).ExecSQL
          else
          if FData is TQuery then
            TQuery(FData).ExecSQL
          else
          if FData is TStoredProc then
            TStoredProc(FData).ExecProc
          else
            ModeError;
        end;
      rqExecDirect:
        begin
          if FData is TJvQuery then
            TJvQuery(FData).ExecDirect
          else
            ModeError;
        end;
      rqOpenOrExec:
        begin
          if FData is TJvQuery then
            TJvQuery(FData).OpenOrExec(True)
          else
            FData.Open;
        end;
    end;
  except
    HandleException;
  end;
end;

{$ENDIF WIN32}

//=== TJvSQLScript ===========================================================

constructor TJvSQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TParams.Create{$IFDEF COMPILER4_UP}(Self){$ENDIF};
  FQuery := TJvQuery.Create(Self);
  FSemicolonTerm := True;
  FTerm := DefaultTermChar;
end;

destructor TJvSQLScript.Destroy;
begin
  FQuery.Free;
  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TJvSQLScript.GetDatabase: TDatabase;
begin
  Result := FQuery.Database;
end;

function TJvSQLScript.GetDatabaseName: string;
begin
  Result := FQuery.DatabaseName;
end;

procedure TJvSQLScript.SetDatabaseName(const Value: string);
begin
  FQuery.DatabaseName := Value;
end;

{$IFDEF WIN32}

function TJvSQLScript.GetSessionName: string;
begin
  Result := FQuery.SessionName;
end;

procedure TJvSQLScript.SetSessionName(const Value: string);
begin
  FQuery.SessionName := Value;
end;

function TJvSQLScript.GetDBSession: TSession;
begin
  Result := FQuery.DBSession;
end;

{$ENDIF WIN32}

procedure TJvSQLScript.CheckExecQuery(LineNo, StatementNo: Integer);
var
  Done: Boolean;
  Action: TScriptAction;
  I: Integer;
  Param: TParam;
  {$IFNDEF WIN32}
  Msg: array [0..255] of Char;
  {$ENDIF}
  S: string;
begin
  Done := False;
  repeat
    try
      if IgnoreParams then
        FQuery.ExecDirect
      else
      begin
        for I := 0 to FQuery.Params.Count - 1 do
        begin
          Param := FQuery.Params[I];
          Param.Assign(Params.ParamByName(Param.Name));
        end;
        FQuery.ExecSQL;
      end;
      Done := True;
    except
      on E: EDatabaseError do
      begin
        Action := saFail;
        S := Format(ResStr(SParseError), [ResStr(SMsgdlgError), LineNo]);
        if E is EDBEngineError then
          TDBError.Create(EDBEngineError(E), 0, LineNo,
            {$IFDEF WIN32} PChar(S) {$ELSE} StrPCopy(Msg, S) {$ENDIF})
        else
        begin
          if E.Message <> '' then
            E.Message := E.Message + '. ';
          E.Message := E.Message + S;
        end;
        if Assigned(FOnScriptError) then
          FOnScriptError(Self, E, LineNo, StatementNo, Action);
        if Action = saFail then
          raise;
        if Action = saAbort then
          SysUtils.Abort;
        if Action = saContinue then
        begin
          Application.HandleException(Self);
          Done := True;
        end
        else
        if Action = saIgnore then
          Done := True;
      end;
    end;
  until Done;
end;

procedure TJvSQLScript.ExecuteScript(StatementNo: Integer);
var
  S, LastStr: string;
  IsTrans, SQLFilled, StmtFound: Boolean;
  I, P, CurrStatement: Integer;
begin
  IsTrans := FTransaction {$IFNDEF WIN32} and Database.IsSQLBased {$ENDIF}
  and not TransActive(Database) and (StatementNo < 0);
  LastStr := '';
  try
    if IsTrans then
    begin
      {$IFDEF WIN32}
      if not Database.IsSQLBased then
        Database.TransIsolation := tiDirtyRead;
      {$ENDIF}
      Database.StartTransaction;
    end;
  except
    IsTrans := False;
  end;
  try
    I := 0;
    CurrStatement := 0;
    StmtFound := False;
    while I < SQL.Count do
    begin
      FQuery.SQL.BeginUpdate;
      try
        FQuery.SQL.Clear;
        SQLFilled := False;
        repeat
          if LastStr <> '' then
          begin
            FQuery.SQL.Add(LastStr);
            LastStr := '';
          end;
          if I < SQL.Count then
          begin
            S := Trim(SQL[I]);
            Inc(I);
            P := Pos(';', S);
            if (P > 0) and FSemicolonTerm then
            begin
              LastStr := Trim(Copy(S, P + 1, MaxInt));
              S := Copy(S, 1, P - 1);
              if S <> '' then
                FQuery.SQL.Add(S);
              SQLFilled := True;
            end
            else
            begin
              if S = Term then
                SQLFilled := True
              else
              if S <> '' then
                FQuery.SQL.Add(S);
            end;
          end
          else
            SQLFilled := True;
        until SQLFilled;
      finally
        FQuery.SQL.EndUpdate;
      end;
      if FQuery.SQL.Count > 0 then
      begin
        if (StatementNo < 0) or (StatementNo = CurrStatement) then
        begin
          StmtFound := True;
          CheckExecQuery(I - 1, CurrStatement);
          if StatementNo = CurrStatement then
            Break;
        end;
        Inc(CurrStatement);
      end;
    end;
    if not StmtFound then
    begin
      {$IFDEF COMPILER3_UP}
      DatabaseError(Format(SListIndexError, [StatementNo]));
      {$ELSE}
      DatabaseError(Format('%s: %d', [LoadStr(SListIndexError), StatementNo]));
      {$ENDIF COMPILER3_UP}
    end;
    if IsTrans then
      Database.Commit;
  except
    if IsTrans then
      Database.Rollback;
    raise;
  end;
end;

procedure TJvSQLScript.ExecStatement(StatementNo: Integer);
begin
  if FSQL.Count = 0 then
    _DBError(SEmptySQLStatement);
  FQuery.SetDBFlag(dbfExecScript, True);
  try
    if not Database.Connected then
      _DBError(SDatabaseClosed);
    if Assigned(FBeforeExec) then
      FBeforeExec(Self);
    ExecuteScript(StatementNo);
    if Assigned(FAfterExec) then
      FAfterExec(Self);
  finally
    FQuery.SetDBFlag(dbfExecScript, False);
  end;
end;

procedure TJvSQLScript.ExecSQL;
begin
  ExecStatement(-1);
end;

procedure TJvSQLScript.CreateParams(List: TParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, False, ':', []);
end;

procedure TJvSQLScript.SetQuery(Value: TStrings);
begin
  TStringList(SQL).OnChange := nil;
  FSQL.Assign(Value);
  TStringList(SQL).OnChange := QueryChanged;
  QueryChanged(nil);
end;

function TJvSQLScript.GetText: {$IFDEF WIN32} string {$ELSE} PChar {$ENDIF};
begin
  {$IFDEF WIN32}
  Result := SQL.Text;
  {$ELSE}
  Result := SQL.GetText;
  {$ENDIF}
end;

procedure TJvSQLScript.QueryChanged(Sender: TObject);
var
  List: TParams;
  {$IFNDEF WIN32}
  P: PChar;
  {$ENDIF}
begin
  {$IFDEF COMPILER4_UP}
  if not (csReading in ComponentState) then
  begin
    {$ENDIF COMPILER4_UP}
    List := TParams.Create{$IFDEF COMPILER4_UP}(Self){$ENDIF};
    try
      {$IFDEF WIN32}
      CreateParams(List, PChar(Text));
      {$ELSE}
      P := GetText;
      try
        CreateParams(List, P);
      finally
        StrDispose(P);
      end;
      {$ENDIF WIN32}
      List.AssignValues(FParams);
    {$IFDEF COMPILER4_UP}
      FParams.Clear;
      FParams.Assign(List);
    finally
    {$ELSE}
      FParams.Free;
      FParams := List;
    except
    {$ENDIF COMPILER4_UP}
      List.Free;
    end;
  {$IFDEF COMPILER4_UP}
  end
  else
  begin
    FParams.Clear;
    CreateParams(FParams, PChar(Text));
  end;
  {$ENDIF COMPILER4_UP}
end;

function TJvSQLScript.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TJvSQLScript.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

function TJvSQLScript.GetParamsCount: Cardinal;
begin
  Result := FParams.Count;
end;

{$IFDEF COMPILER4_UP}

procedure TJvSQLScript.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, True);
end;

procedure TJvSQLScript.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TJvSQLScript.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{$ENDIF COMPILER4_UP}

end.

