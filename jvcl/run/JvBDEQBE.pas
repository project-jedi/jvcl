{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBQBE.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Additional credits and thanks goto AO ROSNO and
Master-Bank for there additions to this unit

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDEQBE;

interface

uses
  Classes, DB, DBTables, Bde;

const
  DefQBEStartParam = '#';

type
  TCheckType = (ctNone, ctCheck, ctCheckPlus, ctCheckDesc, ctCheckGroup);

  TJvQBEQuery = class(TDBDataSet)
  private
    FStmtHandle: HDBIStmt;
    FQBE: TStringList;
    FPrepared: Boolean;
    FParams: TParams;
    FStartParam: Char;
    FAuxiliaryTables: Boolean;
    FText: string;
    FRowsAffected: Integer;
    FConstrained: Boolean;
    FLocal: Boolean;
    FRequestLive: Boolean;
    FBlankAsZero: Boolean;
    FParamCheck: Boolean;
    function CreateCursor(GenHandle: Boolean): HDBICur;
    procedure ReplaceParams(QBEText: TStrings);
    procedure CreateParams(List: TParams; const Value: PChar);
    procedure FreeStatement;
    function GetQBE: TStrings;
    function GetQueryCursor(GenHandle: Boolean): HDBICur;
    procedure GetStatementHandle(QBEText: PChar);
    procedure PrepareQBE(Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure SetQBE(Value: TStrings);
    procedure SetParams(Value: TParams);
    procedure SetPrepared(Value: Boolean);
    procedure SetPrepare(Value: Boolean);
    procedure SetStartParam(Value: Char);
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    function GetRowsAffected: Integer;
  protected
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    function CreateHandle: HDBICur; override;
    procedure Disconnect; override;
    function GetParamsCount: Word;
    procedure DefineProperties(Filer: TFiler); override;
    function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetQBEText: PChar;
    procedure ExecQBE;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure RefreshQuery;
    procedure UnPrepare;
    property Local: Boolean read FLocal;
    property ParamCount: Word read GetParamsCount;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property StmtHandle: HDBIStmt read FStmtHandle;
    property Text: string read FText;
    property RowsAffected: Integer read GetRowsAffected;
  published
    property AutoRefresh;
    property AuxiliaryTables: Boolean read FAuxiliaryTables write FAuxiliaryTables default True;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property StartParam: Char read FStartParam write SetStartParam default DefQBEStartParam;
    { Ensure StartParam is declared before QBE }
    property QBE: TStrings read GetQBE write SetQBE;
    { Ensure QBE is declared before Params }
    property BlankAsZero: Boolean read FBlankAsZero write FBlankAsZero default False;
    property Params: TParams read FParams write SetParams stored False;
    property RequestLive: Boolean read FRequestLive write FRequestLive default False;
    property UpdateMode;
    property UpdateObject;
    property Constrained: Boolean read FConstrained write FConstrained default False;
    property Constraints stored ConstraintsStored;
  end;

implementation

uses
  SysUtils, DBConsts, BDEConst,
  JvDBUtils;

constructor TJvQBEQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQBE := TStringList.Create;
  FQBE.OnChange := QueryChanged;
  FParams := TParams.Create(Self);
  FStartParam := DefQBEStartParam;
  FParamCheck := True;
  FAuxiliaryTables := True;
  FRowsAffected := -1;
  FRequestLive := False;
end;

destructor TJvQBEQuery.Destroy;
begin
  Destroying;
  Disconnect;
  FQBE.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TJvQBEQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TJvQBEQuery.RefreshQuery;
var
  Bookmark: TBookmark;
begin
  DisableControls;
  Bookmark := GetBookmark;
  try
    Close;
    Open;
    try
      GotoBookmark(Bookmark);
    except
      { ignore exceptions }
    end;
  finally
    FreeBookmark(Bookmark);
    EnableControls;
  end;
end;

procedure TJvQBEQuery.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TJvQBEQuery.Prepare;
begin
  SetDBFlag(dbfPrepared, True);
  SetPrepared(True);
end;

procedure TJvQBEQuery.UnPrepare;
begin
  SetPrepared(False);
  SetDBFlag(dbfPrepared, False);
end;

procedure TJvQBEQuery.SetStartParam(Value: Char);
begin
  if Value <> FStartParam then
  begin
    FStartParam := Value;
    QueryChanged(nil);
  end;
end;

function TJvQBEQuery.GetQBE: TStrings;
begin
  Result := FQBE;
end;

procedure TJvQBEQuery.SetQBE(Value: TStrings);
begin
  if FQBE.Text <> Value.Text then
  begin
    Disconnect;
    FQBE.OnChange := nil;
    FQBE.Assign(Value);
    FQBE.OnChange := QueryChanged;
    QueryChanged(nil);
  end;
end;

procedure TJvQBEQuery.QueryChanged(Sender: TObject);
var
  List: TParams;
begin
  if not (csReading in ComponentState) then
  begin
    Disconnect;
    FText := QBE.Text;
    if ParamCheck or (csDesigning in ComponentState) then
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
    end;
    DataEvent(dePropertyChange, 0);
  end
  else
  begin
    FText := QBE.Text;
    FParams.Clear;
    CreateParams(FParams, PChar(Text));
  end;
end;

procedure TJvQBEQuery.SetParams(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

procedure TJvQBEQuery.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, True);
end;

procedure TJvQBEQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TJvQBEQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

function TJvQBEQuery.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

procedure TJvQBEQuery.ReplaceParams(QBEText: TStrings);
var
  I: Integer;

  function ReplaceString(const S: string): string;
  var
    I, J, P, LiteralChars: Integer;
    Param: TParam;
    Temp: string;
    Found: Boolean;
  begin
    Result := S;
    for I := Params.Count - 1 downto 0 do
    begin
      Param := Params[I];
      if Param.DataType = ftUnknown then
        Continue; { ignore undefined params }
      repeat
        P := Pos(StartParam + Param.Name, Result);
        Found := (P > 0) and ((Length(Result) = P + Length(Param.Name)) or
          NameDelimiter(Result[P + Length(Param.Name) + 1]));
        if Found then
        begin
          LiteralChars := 0;
          for J := 1 to P - 1 do
            if IsLiteral(Result[J]) then
              Inc(LiteralChars);
          Found := LiteralChars mod 2 = 0;
          if Found then
          begin
            Temp := Param.Text;
            if Temp = '' then
            begin
              if (Param.DataType = ftString) and not Param.IsNull then
                Temp := '""'
              else
                Temp := 'BLANK'; { special QBE operator }
            end;
            Result := Copy(Result, 1, P - 1) + Temp + Copy(Result,
              P + Length(Param.Name) + 1, MaxInt);
          end;
        end;
      until not Found;
    end;
  end;

begin
  QBEText.BeginUpdate;
  try
    for I := 0 to QBEText.Count - 1 do
      QBEText[I] := ReplaceString(QBEText[I]);
  finally
    QBEText.EndUpdate;
  end;
end;

procedure TJvQBEQuery.SetPrepared(Value: Boolean);
var
  TempQBE: TStrings;
  AText: PChar;
begin
  if Handle <> nil then
    _DBError(SDataSetOpen);
  if (Value <> Prepared) or (ParamCount > 0) then
  begin
    if Value then
    begin
      FRowsAffected := -1;
      if ParamCount > 0 then
      begin
        TempQBE := TStringList.Create;
        try
          TempQBE.Assign(QBE);
          ReplaceParams(TempQBE);
          AText := PChar(TempQBE.Text);
          try
            FreeStatement;
            if StrLen(AText) > 1 then
              PrepareQBE(AText)
            else
              _DBError(SEmptySQLStatement);
          finally
          end;
        finally
          TempQBE.Free;
        end;
      end
      else
      begin
        if StrLen(PChar(Text)) > 1 then
          PrepareQBE(PChar(Text))
        else
          _DBError(SEmptySQLStatement);
      end;
    end
    else
    begin
      FRowsAffected := RowsAffected;
      FreeStatement;
    end;
    FPrepared := Value;
  end;
end;

procedure TJvQBEQuery.FreeStatement;
begin
  if StmtHandle <> nil then
  begin
    DbiQFree(FStmtHandle);
    FStmtHandle := nil;
  end;
end;

function TJvQBEQuery.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TJvQBEQuery.CreateParams(List: TParams; const Value: PChar);
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
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = FStartParam) and not Literal and
      ((CurPos + 1)^ <> FStartParam) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar)) do
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
      if List.FindParam(Name) = nil then
        List.CreateParam(ftUnknown, Name, ptUnknown);
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else
    if (CurChar = FStartParam) and not Literal and
      ((CurPos + 1)^ = FStartParam) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else
    if IsLiteral(CurChar) then
      Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

function TJvQBEQuery.CreateCursor(GenHandle: Boolean): HDBICur;
begin
  if QBE.Count > 0 then
  begin
    SetPrepared(True);
    Result := GetQueryCursor(GenHandle);
  end
  else
    Result := nil;
end;

function TJvQBEQuery.CreateHandle: HDBICur;
begin
  Result := CreateCursor(True)
end;

procedure TJvQBEQuery.ExecQBE;
begin
  CheckInActive;
  SetDBFlag(dbfExecSQL, True);
  try
    CreateCursor(False);
  finally
    SetDBFlag(dbfExecSQL, False);
  end;
end;

function TJvQBEQuery.GetQueryCursor(GenHandle: Boolean): HDBICur;
var
  PCursor: phDBICur;
begin
  Result := nil;
  if GenHandle then
    PCursor := @Result
  else
    PCursor := nil;
  Check(DbiQExec(StmtHandle, PCursor));
end;

function TJvQBEQuery.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
var
  NewConnection: Boolean;
begin
  if Value then
  begin
    NewConnection := DBFlags = [];
    Result := inherited SetDBFlag(Flag, Value);
    if not (csReading in ComponentState) and NewConnection then
      FLocal := not Database.IsSQLBased;
  end
  else
  begin
    if DBFlags - [Flag] = [] then
      SetPrepared(False);
    Result := inherited SetDBFlag(Flag, Value);
  end;
end;

procedure TJvQBEQuery.PrepareQBE(Value: PChar);
begin
  GetStatementHandle(Value);
end;

procedure TJvQBEQuery.GetStatementHandle(QBEText: PChar);
const
  DataType: array [Boolean] of Longint = (Ord(wantCanned), Ord(wantLive));
begin
  Check(DbiQAlloc(DBHandle, qrylangQBE, FStmtHandle));
  try
    Check(DBiSetProp(hDbiObj(StmtHandle), stmtLIVENESS,
      DataType[RequestLive and not ForceUpdateCallback]));
    Check(DBiSetProp(hDbiObj(StmtHandle), stmtAUXTBLS, Longint(FAuxiliaryTables)));
    if Local and RequestLive and Constrained then
      Check(DBiSetProp(hDbiObj(StmtHandle), stmtCONSTRAINED, LongInt(True)));
    if FBlankAsZero then
      Check(DbiSetProp(hDbiObj(StmtHandle), stmtBLANKS, Longint(True)));
    while not CheckOpen(DbiQPrepare(FStmtHandle, QBEText)) do {Retry}
      ;
  except
    DbiQFree(FStmtHandle);
    FStmtHandle := nil;
    raise;
  end;
end;

function TJvQBEQuery.GetQBEText: PChar;
var
  BufLen: Word;
  I: Integer;
  StrEnd: PChar;
  StrBuf: array [0..255] of Char;
begin
  BufLen := 1;
  for I := 0 to QBE.Count - 1 do
    Inc(BufLen, Length(QBE.Strings[I]) + 1);
  Result := StrAlloc(BufLen);
  try
    StrEnd := Result;
    for I := 0 to QBE.Count - 1 do
    begin
      StrPCopy(StrBuf, QBE.Strings[I]);
      StrEnd := StrECopy(StrEnd, StrBuf);
      StrEnd := StrECopy(StrEnd, ' ');
    end;
  except
    StrDispose(Result);
    raise;
  end;
end;

function TJvQBEQuery.GetRowsAffected: Integer;
var
  Length: Word;
begin
  if Prepared then
  begin
    if DbiGetProp(hDBIObj(StmtHandle), stmtROWCOUNT, @Result, SizeOf(Result),
      Length) <> 0 then
      Result := -1;
  end
  else
    Result := FRowsAffected;
end;

{ TJvQBEQuery.IProviderSupport }

function TJvQBEQuery.PSGetParams: TParams;
begin
  Result := Params;
end;

procedure TJvQBEQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count <> 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TJvQBEQuery.PSExecute;
begin
  ExecQBE;
end;

procedure TJvQBEQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    QBE.Text := CommandText;
end;

end.

