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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDEQuery;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, DB, DBTables, Bde,
  JvComponent, JVCLVer;

const
  DefaultMacroChar = '%';
  DefaultTermChar = '/';

type
  TQueryOpenStatus = (qsOpened, qsExecuted, qsFailed);

  TJvQuery = class(TQuery)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FDisconnectExpected: Boolean;
    FSaveQueryChanged: TNotifyEvent;
    FMacroChar: Char;
    FMacros: TParams;
    FSQL: TStringList;
    FStreamPatternChanged: Boolean;
    FPatternChanged: Boolean;
    FOpenStatus: TQueryOpenStatus;
    function GetMacros: TParams;
    procedure SetMacros(Value: TParams);
    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
    procedure PatternChanged(Sender: TObject);
    procedure QueryChanged(Sender: TObject);
    procedure RecreateMacros;
    procedure CreateMacros(List: TParams; const Value: PChar);
    procedure Expand(Query: TStrings);
    function GetMacroCount: Word;
    procedure SetMacroChar(Value: Char);
    function GetRealSQL: TStrings;
  protected
    procedure InternalFirst; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure Loaded; override;
    function CreateHandle: HDBICur; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure Disconnect; override;
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetTableName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExpandMacros;
    procedure ExecSQL;
    procedure Prepare;
    procedure OpenOrExec(ChangeLive: Boolean);
    procedure ExecDirect;
    function MacroByName(const Value: string): TParam;
    property MacroCount: Word read GetMacroCount;
    property OpenStatus: TQueryOpenStatus read FOpenStatus;
    property RealSQL: TStrings read GetRealSQL;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property SQL: TStrings read GetSQL write SetSQL;
    property Macros: TParams read GetMacros write SetMacros;
  end;

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

  TScriptAction = (saFail, saAbort, saRetry, saIgnore, saContinue);

  TScriptErrorEvent = procedure(Sender: TObject; E: EDatabaseError;
    LineNo, StatementNo: Integer; var Action: TScriptAction) of object;

  TJvSQLScript = class(TJvComponent)
  private
    FSQL: TStringList;
    FParams: TParams;
    FQuery: TJvQuery;
    FTransaction: Boolean;
    FSemicolonTerm: Boolean;
    FIgnoreParams: Boolean;
    FTerm: Char;
    FBeforeExec: TNotifyEvent;
    FAfterExec: TNotifyEvent;
    FOnScriptError: TScriptErrorEvent;
    function GetSessionName: string;
    procedure SetSessionName(const Value: string);
    function GetDBSession: TSession;
    function GetText: string;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    function GetDatabase: TDatabase;
    function GetDatabaseName: string;
    procedure SetDatabaseName(const Value: string);
    procedure CreateParams(List: TParams; const Value: PChar);
    procedure QueryChanged(Sender: TObject);
    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
    procedure SetParamsList(Value: TParams);
    function GetParamsCount: Cardinal;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure CheckExecQuery(LineNo, StatementNo: Integer);
    procedure ExecuteScript(StatementNo: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    procedure ExecStatement(StatementNo: Integer);
    function ParamByName(const Value: string): TParam;
    property DBSession: TSession read GetDBSession;
    property Text: string read GetText;
    property Database: TDatabase read GetDatabase;
    property ParamCount: Cardinal read GetParamsCount;
  published
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property IgnoreParams: Boolean read FIgnoreParams write FIgnoreParams default False;
    property SemicolonTerm: Boolean read FSemicolonTerm write FSemicolonTerm default True;
    property SessionName: string read GetSessionName write SetSessionName;
    property Term: Char read FTerm write FTerm default DefaultTermChar;
    property SQL: TStrings read GetSQL write SetSQL;
    property Params: TParams read FParams write SetParamsList stored False;
    property Transaction: Boolean read FTransaction write FTransaction;
    property BeforeExec: TNotifyEvent read FBeforeExec write FBeforeExec;
    property AfterExec: TNotifyEvent read FAfterExec write FAfterExec;
    property OnScriptError: TScriptErrorEvent read FOnScriptError write FOnScriptError;
  end;

const
  dbfExecScript = dbfTable;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TSysCharSet);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  Forms, Consts, BDEConst,
  JvDBUtils, JvBdeUtils;

{ Parse SQL utility routines }

function NameDelimiters(C: Char; Delims: TSysCharSet): Boolean;
begin
  Result := NameDelimiter(C) or (C in Delims);
end;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TSysCharSet);
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
      while (CurChar <> #0) and (Literal or not NameDelimiters(CurChar, Delims)) do
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
        if List.FindParam(Name) = nil then
        begin
          if Macro then
            List.CreateParam(ftString, Name, ptInput).AsString := TrueExpr
          else
            List.CreateParam(ftUnknown, Name, ptUnknown);
        end;
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

//=== { TJvQuery } ===========================================================

constructor TJvQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpenStatus := qsFailed;
  FSaveQueryChanged := TStringList(inherited SQL).OnChange;
  TStringList(inherited SQL).OnChange := QueryChanged;
  FMacroChar := DefaultMacroChar;
  FSQL := TStringList.Create;
  FSQL.OnChange := PatternChanged;
  FMacros := TParams.Create(Self);
end;

destructor TJvQuery.Destroy;
begin
  Destroying;
  Disconnect;
  FMacros.Free;
  FSQL.Free;
  inherited Destroy;
end;

procedure TJvQuery.Loaded;
begin
  inherited Loaded;
  GetMacros; {!! trying this way}
end;

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
  inherited OpenCursor(InfoQuery);
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
begin
  CheckInactive;
  SetDBFlag(dbfExecSQL, True);
  try
    if SQL.Count > 0 then
    begin
      FOpenStatus := qsFailed;
      Check(DbiQExecDirect(DBHandle, qryLangSQL, PChar(inherited SQL.Text),
        nil));
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

function TJvQuery.GetSQL: TStrings;
begin
  Result := FSQL;
end;

procedure TJvQuery.SetSQL(Value: TStrings);
begin
  inherited Disconnect;
  FSQL.OnChange := nil;
  FSQL.Assign(Value);
  FSQL.OnChange := PatternChanged;
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
begin
  FSaveQueryChanged(Sender);
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
begin
  if not (csReading in ComponentState) then
  begin
    List := TParams.Create(Self);
    try
      CreateMacros(List, PChar(FSQL.Text));
      List.AssignValues(FMacros);
      FMacros.Clear;
      FMacros.Assign(List);
    finally
      List.Free;
    end;
  end
  else
  begin
    FMacros.Clear;
    CreateMacros(FMacros, PChar(FSQL.Text));
  end;
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
          NameDelimiters(Result[P + Length(Param.Name) + 1], ['.']));
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
  Query.BeginUpdate;
  try
    for I := 0 to SQL.Count - 1 do
      Query.Add(ReplaceString(SQL[I]));
  finally
    Query.EndUpdate;
  end;
end;

function TJvQuery.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

function TJvQuery.MacroByName(const Value: string): TParam;
begin
  Result := FMacros.ParamByName(Value);
end;

function TJvQuery.GetRealSQL: TStrings;
begin
  try
    ExpandMacros;
  except
  end;
  Result := inherited SQL;
end;


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

//=== { TJvQueryThread } =====================================================

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

//=== { TJvSQLScript } =======================================================

constructor TJvSQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FSQL.OnChange := QueryChanged;
  FParams := TParams.Create(Self);
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

procedure TJvSQLScript.CheckExecQuery(LineNo, StatementNo: Integer);
var
  Done: Boolean;
  Action: TScriptAction;
  I: Integer;
  Param: TParam;
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
        S := Format(SParseError, [SMsgdlgError, LineNo]);
        if E is EDBEngineError then
          TDBError.Create(EDBEngineError(E), 0, LineNo,
            PChar(S))
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
  IsTrans := FTransaction and not TransActive(Database) and (StatementNo < 0);
  LastStr := '';
  try
    if IsTrans then
    begin
      if not Database.IsSQLBased then
        Database.TransIsolation := tiDirtyRead;
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
      DatabaseError(Format(SListIndexError, [StatementNo]));
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
  if SQL.Count = 0 then
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

function TJvSQLScript.GetSQL: TStrings;
begin
  Result := FSQL;
end;

procedure TJvSQLScript.SetSQL(Value: TStrings);
begin
  FSQL.OnChange := nil;
  FSQL.Assign(Value);
  FSQL.OnChange := QueryChanged;
  QueryChanged(nil);
end;

function TJvSQLScript.GetText: string;
begin
  Result := SQL.Text;
end;

procedure TJvSQLScript.QueryChanged(Sender: TObject);
var
  List: TParams;
begin
  if not (csReading in ComponentState) then
  begin
    List := TParams.Create(Self);
    try
      CreateParams(List, PChar(Text));
      List.AssignValues(FParams);
      FParams.Clear;
      FParams.Assign(List);
    finally
      List.Free;
    end;
  end
  else
  begin
    FParams.Clear;
    CreateParams(FParams, PChar(Text));
  end;
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

