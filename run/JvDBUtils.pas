{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBUtils.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Contributors:
tia

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBUtils;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils, DB,
  JvAppStorage;

type
  TCommit = (ctNone, ctStep, ctAll);
  TJvDBProgressEvent = procedure(UserData: Integer; var Cancel: Boolean; Line: Integer) of object;

  EJvScriptError = class(Exception)
  private
    FErrPos: Integer;
  public
    // The dummy parameter is only there for BCB compatibility so that
    // when the hpp file gets generated, this constructor generates
    // a C++ constructor that doesn't already exist
    constructor Create(const AMessage: string; AErrPos: Integer; DummyForBCB: Integer = 0); overload;
    property ErrPos: Integer read FErrPos;
  end;

  TJvLocateObject = class(TObject)
  private
    FDataSet: TDataSet;
    FLookupField: TField;
    FLookupValue: string;
    FLookupExact: Boolean;
    FCaseSensitive: Boolean;
    FBookmark: TBookmark;
    FIndexSwitch: Boolean;
    procedure SetDataSet(Value: TDataSet);
  protected
    function MatchesLookup(Field: TField): Boolean;
    procedure CheckFieldType(Field: TField); virtual;
    procedure ActiveChanged; virtual;
    function LocateFilter: Boolean; virtual;
    function LocateKey: Boolean; virtual;
    function LocateFull: Boolean; virtual;
    function UseKey: Boolean; virtual;
    function FilterApplicable: Boolean; virtual;
    property LookupField: TField read FLookupField;
    property LookupValue: string read FLookupValue;
    property LookupExact: Boolean read FLookupExact;
    property CaseSensitive: Boolean read FCaseSensitive;
    property Bookmark: TBookmark read FBookmark write FBookmark;
  public
    function Locate(const KeyField, KeyValue: string; Exact,
      CaseSensitive: Boolean): Boolean;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property IndexSwitch: Boolean read FIndexSwitch write FIndexSwitch;
  end;

  TCreateLocateObject = function: TJvLocateObject;

var
  CreateLocateObject: TCreateLocateObject = nil;

function CreateLocate(DataSet: TDataSet): TJvLocateObject;

{ Utility routines }

function IsDataSetEmpty(DataSet: TDataSet): Boolean;
procedure RefreshQuery(Query: TDataSet);
function DataSetSortedSearch(DataSet: TDataSet;
  const Value, FieldName: string; CaseInsensitive: Boolean): Boolean;
function DataSetSectionName(DataSet: TDataSet): string;
procedure InternalSaveFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage; const Path: string);
procedure InternalRestoreFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage;
  const Path: string; RestoreVisible: Boolean);
function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
(*
procedure SaveFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile);
procedure RestoreFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile;
  RestoreVisible: Boolean);
*)
procedure SaveFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage; const Path: string = '');
procedure RestoreFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage; const Path: string = '';
  RestoreVisible: Boolean = True);
procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
function ConfirmDelete: Boolean;
procedure ConfirmDataSetCancel(DataSet: TDataSet);
procedure CheckRequiredField(Field: TField);
procedure CheckRequiredFields(const Fields: array of TField);

{ SQL expressions }

function DateToSQL(Value: TDateTime): string;
function FormatSQLDateRange(Date1, Date2: TDateTime;
  const FieldName: string): string;
function FormatSQLDateRangeEx(Date1, Date2: TDateTime;
  const FieldName: string): string;
function FormatSQLNumericRange(const FieldName: string;
  LowValue, HighValue, LowEmpty, HighEmpty: Double; Inclusive: Boolean): string;
function StrMaskSQL(const Value: string): string;
function FormatSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;
function FormatAnsiSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;

const
  TrueExpr = '0=0';

const
  { Server Date formats}
  sdfStandard16 = '''"''mm''/''dd''/''yyyy''"'''; {"mm/dd/yyyy"}
  sdfStandard32 = '''''''dd/mm/yyyy'''''''; {'dd/mm/yyyy'}
  sdfOracle = '"TO_DATE(''"dd/mm/yyyy"'', ''DD/MM/YYYY'')"';
  sdfInterbase = '"CAST(''"mm"/"dd"/"yyyy"'' AS DATE)"';
  sdfMSSQL = '"CONVERT(datetime, ''"mm"/"dd"/"yyyy"'', 103)"';

const
  ServerDateFmt: string[50] = sdfStandard16;

{.$NODEFINE ftNonTextTypes}
(*$HPPEMIT 'namespace JvDBUtils'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT '#define ftNonTextTypes (System::Set<TFieldType, ftUnknown, ftCursor> () \'*)
(*$HPPEMIT '        << ftBytes << ftVarBytes << ftBlob << ftMemo << ftGraphic \'*)
(*$HPPEMIT '        << ftFmtMemo << ftParadoxOle << ftDBaseOle << ftTypedBinary << ftCursor )'*)
(*$HPPEMIT '}'*)

type
  Largeint = Longint;
  {$NODEFINE Largeint}

function NameDelimiter(C: Char): Boolean;
function IsLiteral(C: Char): Boolean;
procedure _DBError(const Msg: string);

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  DBConsts, Math, Controls, Forms, Dialogs,
  JvJVCLUtils, JvJCLUtils, JvTypes, JvConsts, JvResources;

{ Utility routines }

function NameDelimiter(C: Char): Boolean;
begin
  Result := C in [' ', ',', ';', ')', '.', Cr, Lf];
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

procedure _DBError(const Msg: string);
begin
  DatabaseError(Msg);
end;

constructor EJvScriptError.Create(const AMessage: string; AErrPos: Integer; DummyForBCB: Integer);
begin
  inherited Create(AMessage);
  FErrPos := AErrPos;
end;

// (rom) better use Windows dialogs which are localized

function ConfirmDelete: Boolean;
begin
  Screen.Cursor := crDefault;
  Result := MessageDlg(SDeleteRecordQuestion, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
end;

procedure ConfirmDataSetCancel(DataSet: TDataSet);
begin
  if DataSet.State in [dsEdit, dsInsert] then
  begin
    DataSet.UpdateRecord;
    if DataSet.Modified then
    begin
      case MessageDlg(RsConfirmSave, mtConfirmation, mbYesNoCancel, 0) of
        mrYes:
          DataSet.Post;
        mrNo:
          DataSet.Cancel;
      else
        SysUtils.Abort;
      end;
    end
    else
      DataSet.Cancel;
  end;
end;

function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): Boolean;
begin
  Result := False;
  with ADataSet do
    if Active and (ABookmark <> nil) and not (Bof and Eof) and
      BookmarkValid(ABookmark) then
    try
      ADataSet.GotoBookmark(ABookmark);
      Result := True;
    except
    end;
end;

{ Refresh Query procedure }

procedure RefreshQuery(Query: TDataSet);
var
  BookMk: TBookmark;
begin
  with Query do
  begin
    DisableControls;
    try
      if Active then
        BookMk := GetBookmark
      else
        BookMk := nil;
      try
        Close;
        Open;
        SetToBookmark(Query, BookMk);
      finally
        if BookMk <> nil then
          FreeBookmark(BookMk);
      end;
    finally
      EnableControls;
    end;
  end;
end;

procedure TJvLocateObject.SetDataSet(Value: TDataSet);
begin
  ActiveChanged;
  FDataSet := Value;
end;

function TJvLocateObject.LocateFull: Boolean;
begin
  Result := False;
  with DataSet do
  begin
    First;
    while not Eof do
    begin
      if MatchesLookup(FLookupField) then
      begin
        Result := True;
        Break;
      end;
      Next;
    end;
  end;
end;

function TJvLocateObject.LocateKey: Boolean;
begin
  Result := False;
end;

function TJvLocateObject.FilterApplicable: Boolean;
begin
  Result := FLookupField.FieldKind in [fkData, fkInternalCalc];
end;

function TJvLocateObject.LocateFilter: Boolean;
var
  SaveCursor: TCursor;
  Options: TLocateOptions;
  Value: Variant;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Options := [];
    if not FCaseSensitive then
      Include(Options, loCaseInsensitive);
    if not FLookupExact then
      Include(Options, loPartialKey);
    if FLookupValue = '' then
      VarClear(Value)
    else
      Value := FLookupValue;
    Result := DataSet.Locate(FLookupField.FieldName, Value, Options);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TJvLocateObject.CheckFieldType(Field: TField);
begin
end;

function TJvLocateObject.Locate(const KeyField, KeyValue: string;
  Exact, CaseSensitive: Boolean): Boolean;
var
  LookupKey: TField;

  function IsStringType(FieldType: TFieldType): Boolean;
  const
    cStringTypes = [ftString, ftWideString];
  begin
    Result := FieldType in cStringTypes;
  end;

begin
  if DataSet = nil then
  begin
    Result := False;
    Exit;
  end;
  DataSet.CheckBrowseMode;
  LookupKey := DataSet.FieldByName(KeyField);
  DataSet.CursorPosChanged;
  FLookupField := LookupKey;
  FLookupValue := KeyValue;
  FLookupExact := Exact;
  FCaseSensitive := CaseSensitive;
  if not IsStringType(FLookupField.DataType) then
  begin
    FCaseSensitive := True;
    try
      CheckFieldType(FLookupField);
    except
      Result := False;
      Exit;
    end;
  end
  else
    FCaseSensitive := CaseSensitive;
  FBookmark := DataSet.GetBookmark;
  try
    DataSet.DisableControls;
    try
      Result := MatchesLookup(FLookupField);
      if not Result then
      begin
        if UseKey then
          Result := LocateKey
        else
        begin
          if FilterApplicable then
            Result := LocateFilter
          else
            Result := LocateFull;
        end;
        if not Result then
          SetToBookmark(DataSet, FBookmark);
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    FLookupValue := '';
    FLookupField := nil;
    DataSet.FreeBookmark(FBookmark);
    FBookmark := nil;
  end;
end;

function TJvLocateObject.UseKey: Boolean;
begin
  Result := False;
end;

procedure TJvLocateObject.ActiveChanged;
begin
end;

function TJvLocateObject.MatchesLookup(Field: TField): Boolean;
var
  Temp: string;
begin
  Temp := Field.AsString;
  if not LookupExact then
    SetLength(Temp, Min(Length(FLookupValue), Length(Temp)));
  if CaseSensitive then
    Result := AnsiSameStr(Temp, LookupValue)
  else
    Result := AnsiSameText(Temp, LookupValue);
end;

function CreateLocate(DataSet: TDataSet): TJvLocateObject;
begin
  if Assigned(CreateLocateObject) then
    Result := CreateLocateObject
  else
    Result := TJvLocateObject.Create;
  if (Result <> nil) and (DataSet <> nil) then
    Result.DataSet := DataSet;
end;

{ DataSet locate routines }

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TList;
  Bookmark: TBookmarkStr;

  function CompareField(Field: TField; Value: Variant): Boolean;
  var
    S: string;
  begin
    if Field.DataType = ftString then
    begin
      if Value = Null then
        Result := Field.IsNull
      else
      begin
        S := Field.AsString;
        if loPartialKey in Options then
          Delete(S, Length(Value) + 1, MaxInt);
        if loCaseInsensitive in Options then
          Result := AnsiSameText(S, Value)
        else
          Result := AnsiSameStr(S, Value);
      end;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
      Result := CompareField(TField(Fields.First), KeyValues)
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
        Result := Result and CompareField(TField(Fields[I]), KeyValues[I]);
    end;
  end;

begin
  Result := False;
  with DataSet do
  begin
    CheckBrowseMode;
    if Bof and Eof then
      Exit;
  end;
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then
      Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.Bookmark;
      try
        with DataSet do
        begin
          First;
          while not Eof do
          begin
            Result := CompareRecord;
            if Result then
              Break;
            Next;
          end;
        end;
      finally
        if not Result and DataSet.BookmarkValid(PChar(Bookmark)) then
          DataSet.Bookmark := Bookmark;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

{ DataSetSortedSearch. Navigate on sorted DataSet routine. }

function DataSetSortedSearch(DataSet: TDataSet; const Value,
  FieldName: string; CaseInsensitive: Boolean): Boolean;
var
  L, H, I: Longint;
  CurrentPos: Longint;
  CurrentValue: string;
  BookMk: TBookmark;
  Field: TField;

  function UpStr(const Value: string): string;
  begin
    if CaseInsensitive then
      Result := AnsiUpperCase(Value)
    else
      Result := Value;
  end;

  function GetCurrentStr: string;
  begin
    Result := Field.AsString;
    if Length(Result) > Length(Value) then
      SetLength(Result, Length(Value));
    Result := UpStr(Result);
  end;

begin
  Result := False;
  if DataSet = nil then
    Exit;
  Field := DataSet.FindField(FieldName);
  if Field = nil then
    Exit;
  if Field.DataType = ftString then
  begin
    DataSet.DisableControls;
    BookMk := DataSet.GetBookmark;
    try
      L := 0;
      DataSet.First;
      CurrentPos := 0;
      H := DataSet.RecordCount - 1;
      if Value <> '' then
      begin
        while L <= H do
        begin
          I := (L + H) shr 1;
          if I <> CurrentPos then
            DataSet.MoveBy(I - CurrentPos);
          CurrentPos := I;
          CurrentValue := GetCurrentStr;
          if UpStr(Value) > CurrentValue then
            L := I + 1
          else
          begin
            H := I - 1;
            if UpStr(Value) = CurrentValue then
              Result := True;
          end;
        end;
        if Result then
        begin
          if L <> CurrentPos then
            DataSet.MoveBy(L - CurrentPos);
          while (L < DataSet.RecordCount) and
            (UpStr(Value) <> GetCurrentStr) do
          begin
            Inc(L);
            DataSet.MoveBy(1);
          end;
        end;
      end
      else
        Result := True;
      if not Result then
        SetToBookmark(DataSet, BookMk);
    finally
      DataSet.FreeBookmark(BookMk);
      DataSet.EnableControls;
    end;
  end
  else
    DatabaseErrorFmt(SFieldTypeMismatch, [Field.DisplayName]);
end;

{ Save and restore DataSet Fields layout }

function DataSetSectionName(DataSet: TDataSet): string;
begin
  with DataSet do
    if (Owner <> nil) and (Owner is TCustomForm) then
      Result := GetDefaultSection(Owner as TCustomForm)
    else
      Result := Name;
end;

function CheckSection(DataSet: TDataSet; const Section: string): string;
begin
  Result := Section;
  if Result = '' then
    Result := DataSetSectionName(DataSet);
end;

procedure InternalSaveFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage; const Path: string);
var
  I: Integer;
begin
  with DataSet do
  begin
    for I := 0 to FieldCount - 1 do
    begin
      AppStorage.WriteString(AppStorage.ConcatPaths([CheckSection(DataSet, Path),
        Name + Fields[I].FieldName]),
        Format('%d,%d,%d', [Fields[I].Index, Fields[I].DisplayWidth,
          Integer(Fields[I].Visible)]));
    end;
  end;
end;

procedure InternalRestoreFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage;
  const Path: string; RestoreVisible: Boolean);
type
  TFieldInfo = record
    Field: TField;
    EndIndex: Integer;
  end;
  PFieldArray = ^TFieldArray;
  TFieldArray = array [0..(65528 div SizeOf(TFieldInfo)) - 1] of TFieldInfo;
const
  Delims = [' ', ','];
var
  I, J: Integer;
  S: string;
  FieldArray: PFieldArray;
begin
  with DataSet do
  begin
    FieldArray := AllocMemo(FieldCount * SizeOf(TFieldInfo));
    try
      for I := 0 to FieldCount - 1 do
      begin
        S := AppStorage.ReadString(AppStorage.ConcatPaths([CheckSection(DataSet, Path),
          Name + Fields[I].FieldName]), '');
        FieldArray^[I].Field := Fields[I];
        FieldArray^[I].EndIndex := Fields[I].Index;
        if S <> '' then
        begin
          FieldArray^[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
            FieldArray^[I].EndIndex);
          Fields[I].DisplayWidth := StrToIntDef(ExtractWord(2, S, Delims),
            Fields[I].DisplayWidth);
          if RestoreVisible then
            Fields[I].Visible := Boolean(StrToIntDef(ExtractWord(3, S, Delims),
              Integer(Fields[I].Visible)));
        end;
      end;
      for I := 0 to FieldCount - 1 do
      begin
        for J := 0 to FieldCount - 1 do
        begin
          if FieldArray^[J].EndIndex = I then
          begin
            FieldArray^[J].Field.Index := FieldArray^[J].EndIndex;
            Break;
          end;
        end;
      end;
    finally
      FreeMemo(Pointer(FieldArray));
    end;
  end;
end;

procedure SaveFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage; const Path: string);
begin
  InternalSaveFields(DataSet, AppStorage, AppStorage.ConcatPaths([Path, DataSetSectionName(DataSet)]));
end;

procedure RestoreFields(DataSet: TDataSet; AppStorage: TJvCustomAppStorage; const Path: string;
  RestoreVisible: Boolean);
begin
  InternalRestoreFields(DataSet, AppStorage, AppStorage.ConcatPaths([DataSetSectionName(DataSet)]),
    RestoreVisible);
end;

function IsDataSetEmpty(DataSet: TDataSet): Boolean;
begin
  with DataSet do
    Result := (not Active) or (Eof and Bof);
end;

{ SQL expressions }

function DateToSQL(Value: TDateTime): string;
begin
  Result := IntToStr(Trunc(Value));
end;

function FormatSQLDateRange(Date1, Date2: TDateTime;
  const FieldName: string): string;
begin
  Result := TrueExpr;
  if (Date1 = Date2) and (Date1 <> NullDate) then
  begin
    Result := Format('%s = %s', [FieldName, FormatDateTime(ServerDateFmt,
        Date1)]);
  end
  else
  if (Date1 <> NullDate) or (Date2 <> NullDate) then
  begin
    if Date1 = NullDate then
      Result := Format('%s < %s', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date2, 1))])
    else
    if Date2 = NullDate then
      Result := Format('%s > %s', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date1, -1))])
    else
      Result := Format('(%s < %s) AND (%s > %s)',
        [FieldName, FormatDateTime(ServerDateFmt, IncDay(Date2, 1)),
        FieldName, FormatDateTime(ServerDateFmt, IncDay(Date1, -1))]);
  end;
end;

function FormatSQLDateRangeEx(Date1, Date2: TDateTime;
  const FieldName: string): string;
begin
  Result := TrueExpr;
  if (Date1 <> NullDate) or (Date2 <> NullDate) then
  begin
    if Date1 = NullDate then
      Result := Format('%s < %s', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date2, 1))])
    else
    if Date2 = NullDate then
      Result := Format('%s >= %s', [FieldName,
        FormatDateTime(ServerDateFmt, Date1)])
    else
      Result := Format('(%s < %s) AND (%s >= %s)',
        [FieldName, FormatDateTime(ServerDateFmt, IncDay(Date2, 1)),
        FieldName, FormatDateTime(ServerDateFmt, Date1)]);
  end;
end;

function FormatSQLNumericRange(const FieldName: string;
  LowValue, HighValue, LowEmpty, HighEmpty: Double; Inclusive: Boolean): string;
const
  Operators: array[Boolean, 1..2] of string[2] = (('>', '<'), ('>=', '<='));
begin
  Result := TrueExpr;
  if (LowValue = HighValue) and (LowValue <> LowEmpty) then
    Result := Format('%s = %g', [FieldName, LowValue])
  else
  if (LowValue <> LowEmpty) or (HighValue <> HighEmpty) then
  begin
    if LowValue = LowEmpty then
      Result := Format('%s %s %g', [FieldName, Operators[Inclusive, 2], HighValue])
    else
    if HighValue = HighEmpty then
      Result := Format('%s %s %g', [FieldName, Operators[Inclusive, 1], LowValue])
    else
      Result := Format('(%s %s %g) AND (%s %s %g)',
        [FieldName, Operators[Inclusive, 2], HighValue,
        FieldName, Operators[Inclusive, 1], LowValue]);
  end;
end;

function StrMaskSQL(const Value: string): string;
begin
  if (Pos('*', Value) = 0) and (Pos('?', Value) = 0) and (Value <> '') then
    Result := '*' + Value + '*'
  else
    Result := Value;
end;

function FormatSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;
var
  EmptyValue: Boolean;
  FieldValue: string;
  DateValue: TDateTime;
  LogicOperator: string;
begin
  FieldValue := '';
  DateValue := NullDate;
  Exact := Exact or not (FieldType in
    [ftString, ftDate, ftTime, ftDateTime]);
  if FieldType in [ftDate, ftTime, ftDateTime] then
  begin
    DateValue := StrToDateDef(Value, NullDate);
    EmptyValue := (DateValue = NullDate);
    FieldValue := FormatDateTime(ServerDateFmt, DateValue);
  end
  else
  begin
    FieldValue := Value;
    EmptyValue := FieldValue = '';
    if not (Exact or EmptyValue) then
      FieldValue := ReplaceStr(ReplaceStr(StrMaskSQL(FieldValue),
        '*', '%'), '?', '_');
    if FieldType = ftString then
      FieldValue := '''' + FieldValue + '''';
  end;
  LogicOperator := Operator;
  if LogicOperator = '' then
  begin
    if Exact then
      LogicOperator := '='
    else
    begin
      if FieldType = ftString then
        LogicOperator := 'LIKE'
      else
        LogicOperator := '>=';
    end;
  end;
  if EmptyValue then
    Result := TrueExpr
  else
  if (FieldType = ftDateTime) and Exact then
  begin
    DateValue := IncDay(DateValue, 1);
    Result := Format('(%s >= %s) and (%s < %s)', [FieldName, FieldValue,
      FieldName, FormatDateTime(ServerDateFmt, DateValue)]);
  end
  else
    Result := Format('%s %s %s', [FieldName, LogicOperator, FieldValue]);
end;

function FormatAnsiSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;
var
  S, Esc: string;
begin
  Esc := '';
  if not Exact and (FieldType = ftString) then
  begin
    S := ReplaceStr(ReplaceStr(ReplaceStr(Value, '/', '//'),
      '_', '/_'), '%', '/%');
    if S <> Value then
      Esc := ' ESCAPE''/''';
  end
  else
    S := Value;
  Result := FormatSQLCondition(FieldName, Operator, S, FieldType, Exact) + Esc;
end;

procedure CheckRequiredField(Field: TField);
begin
  with Field do
    if not ReadOnly and not Calculated and IsNull then
    begin
      FocusControl;
      DatabaseErrorFmt(SFieldRequired, [DisplayName]);
    end;
end;

procedure CheckRequiredFields(const Fields: array of TField);
var
  I: Integer;
begin
  for I := Low(Fields) to High(Fields) do
    CheckRequiredField(Fields[I]);
end;

procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
var
  I: Integer;
  F, FSrc: TField;
begin
  if not (Dest.State in dsEditModes) then
    _DBError(SNotEditing);
  if ByName then
  begin
    for I := 0 to Source.FieldCount - 1 do
    begin
      F := Dest.FindField(Source.Fields[I].FieldName);
      if (F <> nil) and (F.DataType <> ftAutoInc) then
        F.Value := Source.Fields[I].Value;
    end;
  end
  else
  begin
    for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do
    begin
      F := Dest.FindField(Dest.FieldDefs[I].Name);
      FSrc := Source.FindField(Source.FieldDefs[I].Name);
      if (F <> nil) and (FSrc <> nil) and (F.DataType <> ftAutoInc) then
        F.Value := FSrc.Value;
    end;
  end;
end;

end.

