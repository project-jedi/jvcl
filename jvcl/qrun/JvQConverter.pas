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

The Original Code is: JvDataConv.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQConverter;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,
  JvQComponent, JvQTypes;

type

  TDataType =
    (dtString, dtInteger, dtFloat, dtDateTime, dtDate, dtTime, dtBoolean);

  TTimeFormat = (tfHHMMSS, tfHMMSS, tfHHMM, tfHMM);

  TJvDateTimeFormat = class(TPersistent)
  private
    FAMString: string[7];
    FPMString: string[7];
    FDateOrder: TDateOrder;
    FTimeFormat: TTimeFormat;
    FTimeSeparator: Char;
    FDateSeparator: Char;
    FLongDate: Boolean;
    FFourDigitYear: Boolean;
    FLeadingZero: Boolean;
    function GetAMString: string;
    procedure SetAMString(const Value: string);
    function GetPMString: string;
    procedure SetPMString(const Value: string);
  protected
    function GetDateMask: string; virtual;
    function GetTimeMask: string; virtual;
    function GetMask: string; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure ResetDefault; virtual;
    property DateMask: string read GetDateMask;
    property TimeMask: string read GetTimeMask;
    property Mask: string read GetMask;
  published
    property AMString: string read GetAMString write SetAMString;
    property PMString: string read GetPMString write SetPMString;
    property DateOrder: TDateOrder read FDateOrder write FDateOrder;
    property TimeFormat: TTimeFormat read FTimeFormat write FTimeFormat;
    property TimeSeparator: Char read FTimeSeparator write FTimeSeparator;
    property DateSeparator: Char read FDateSeparator write FDateSeparator;
    property LongDate: Boolean read FLongDate write FLongDate default False;
    property FourDigitYear: Boolean read FFourDigitYear write FFourDigitYear default True;
    property LeadingZero: Boolean read FLeadingZero write FLeadingZero default False;
  end;

  TJvConverter = class(TJvComponent)
  private
    FData: string;
    FTextValues: array [Boolean] of string;
    FDataType: TDataType;
    FDateTimeFormat: TJvDateTimeFormat;
    FFloatFormat: TFloatFormat;
    FDigits: Integer;
    FPrecision: Integer;
    FRaiseOnError: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetDataType(Value: TDataType);
    procedure SetDateTimeFormat(Value: TJvDateTimeFormat);
    function GetDateTimeFormat: TJvDateTimeFormat;
    function GetString: string;
    procedure SetString(const Value: string);
    function GetDateTime: TDateTime;
    function GetBoolValues(Index: Integer): string;
    procedure SetBoolValues(Index: Integer; const Value: string);
    procedure CheckDataType;
    function BoolToStr(Value: Boolean): string;
    function FloatToString(Value: Double): string;
    function DateTimeToString(Value: TDateTime): string;
  protected
    procedure Change; dynamic;
    function GetAsBoolean: Boolean; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsDate: TDateTime; virtual;
    function GetAsTime: TDateTime; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsInteger: Longint; virtual;
    function GetAsString: string; virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    procedure SetAsDateTime(Value: TDateTime); virtual;
    procedure SetAsDate(Value: TDateTime); virtual;
    procedure SetAsTime(Value: TDateTime); virtual;
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsInteger(Value: Longint); virtual;
    procedure SetAsString(const Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function IsValidChar(Ch: Char): Boolean; virtual;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
  published
    property DataType: TDataType read FDataType write SetDataType default dtString;
    property DateTimeFormat: TJvDateTimeFormat read GetDateTimeFormat write SetDateTimeFormat;
    property Digits: Integer read FDigits write FDigits default 2;
    property DisplayFalse: string index 0 read GetBoolValues write SetBoolValues;
    property DisplayTrue: string index 1 read GetBoolValues write SetBoolValues;
    property FloatFormat: TFloatFormat read FFloatFormat write FFloatFormat default ffGeneral;
    property Precision: Integer read FPrecision write FPrecision default 15;
    property RaiseOnError: Boolean read FRaiseOnError write FRaiseOnError default False;
    property Text: string read GetString write SetAsString;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  JvQConsts, JvQResources;

//=== { TJvDateTimeFormat } ==================================================

constructor TJvDateTimeFormat.Create;
begin
  inherited Create;
  ResetDefault;
end;

procedure TJvDateTimeFormat.ResetDefault;
begin
  FAMString := TimeAMString;
  FPMString := TimePMString;
  FTimeSeparator := SysUtils.TimeSeparator;
  FDateSeparator := SysUtils.DateSeparator;
  FDateOrder := doDMY;
  FTimeFormat := tfHHMMSS;
  FLongDate := False;
  FFourDigitYear := True;
  FLeadingZero := False;
end;

procedure TJvDateTimeFormat.Assign(Source: TPersistent);
begin
  if Source is TJvDateTimeFormat then
  begin
    FAMString := TJvDateTimeFormat(Source).AMString;
    FPMString := TJvDateTimeFormat(Source).PMString;
    FDateOrder := TJvDateTimeFormat(Source).DateOrder;
    FTimeFormat := TJvDateTimeFormat(Source).TimeFormat;
    FTimeSeparator := TJvDateTimeFormat(Source).TimeSeparator;
    FDateSeparator := TJvDateTimeFormat(Source).DateSeparator;
    FLongDate := TJvDateTimeFormat(Source).LongDate;
    FFourDigitYear := TJvDateTimeFormat(Source).FourDigitYear;
    FLeadingZero := TJvDateTimeFormat(Source).LeadingZero;
  end
  else
    inherited Assign(Source);
end;

function TJvDateTimeFormat.GetAMString: string;
begin
  Result := FAMString;
end;

procedure TJvDateTimeFormat.SetAMString(const Value: string);
begin
  if Value = '' then
    FAMString := TimeAMString
  else
    FAMString := Value;
end;

function TJvDateTimeFormat.GetPMString: string;
begin
  Result := FPMString;
end;

procedure TJvDateTimeFormat.SetPMString(const Value: string);
begin
  if Value = '' then
    FPMString := TimePMString
  else
    FPMString := Value;
end;

function TJvDateTimeFormat.GetDateMask: string;
var
  S: array [1..3] of string[7];
  Separator: string[3];
begin
  Result := '';
  if LeadingZero then
  begin
    S[1] := 'dd';
    S[2] := 'mm';
  end
  else
  begin
    S[1] := 'd';
    S[2] := 'm';
  end;
  if LongDate then
  begin
    S[2] := 'mmmm';
    Separator := ' ';
  end
  else
    Separator := '"' + DateSeparator + '"';
  if FourDigitYear then
    S[3] := 'yyyy'
  else
    S[3] := 'yy';
  case DateOrder of
    doDMY:
      Result := S[1] + Separator + S[2] + Separator + S[3];
    doMDY:
      Result := S[2] + Separator + S[1] + Separator + S[3];
    doYMD:
      Result := S[3] + Separator + S[2] + Separator + S[1];
  end;
end;

function TJvDateTimeFormat.GetTimeMask: string;
var
  S: array [1..3] of string[7];
  Separator: string[3];
  AMPM: string[16];
begin
  Separator := '"' + TimeSeparator + '"';
  AMPM := ' ' + AMString + '/' + PMString;
  if LeadingZero then
  begin
    S[1] := 'hh';
    S[2] := 'nn';
    S[3] := 'ss';
  end
  else
  begin
    S[1] := 'h';
    S[2] := 'n';
    S[3] := 's';
  end;
  case TimeFormat of
    tfHHMMSS:
      Result := S[1] + Separator + S[2] + Separator + S[3];
    tfHMMSS:
      Result := S[1] + Separator + S[2] + Separator + S[3] + AMPM;
    tfHHMM:
      Result := S[1] + Separator + S[2];
    tfHMM:
      Result := S[1] + Separator + S[2] + AMPM;
  end;
end;

function TJvDateTimeFormat.GetMask: string;
begin
  Result := GetDateMask + ' ' + GetTimeMask;
end;

//=== { TJvConverter } =======================================================

constructor TJvConverter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := '';
  FDataType := dtString;
  FPrecision := 15;
  FDigits := 2;
  FDateTimeFormat := TJvDateTimeFormat.Create;
  FTextValues[False] := RsFalse;
  FTextValues[True] := RsTrue;
  FRaiseOnError := False;
end;

destructor TJvConverter.Destroy;
begin
  FDataType := dtString;
  //if (FData <> nil) and (FData^ <> '') then Dispose(FData);
  FDateTimeFormat.Free;
  inherited Destroy;
end;

procedure TJvConverter.Clear;
begin
  //if (FData <> nil) and (FData^ <> '') then Dispose(FData);
  FData := '';
  Change;
end;

procedure TJvConverter.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvConverter.GetString: string;
begin
  Result := FData;
end;

procedure TJvConverter.SetString(const Value: string);
begin
  FData := Value;
end;

function TJvConverter.GetDateTimeFormat: TJvDateTimeFormat;
begin
  Result := FDateTimeFormat;
end;

procedure TJvConverter.SetDateTimeFormat(Value: TJvDateTimeFormat);
begin
  FDateTimeFormat.Assign(Value);
end;

function TJvConverter.GetBoolValues(Index: Integer): string;
begin
  Result := FTextValues[Boolean(Index)];
end;

procedure TJvConverter.SetBoolValues(Index: Integer; const Value: string);
begin
  FTextValues[Index <> 0] := Value;
end;

function TJvConverter.BoolToStr(Value: Boolean): string;
begin
  Result := GetBoolValues(Ord(Value));
end;

function TJvConverter.FloatToString(Value: Double): string;
begin
  Result := FloatToStrF(Value, FloatFormat, Precision, Digits);
end;

function TJvConverter.DateTimeToString(Value: TDateTime): string;
begin
  case FDataType of
    dtDate:
      Result := FormatDateTime(DateTimeFormat.DateMask, Value);
    dtTime:
      Result := FormatDateTime(DateTimeFormat.TimeMask, Value);
  else
    Result := FormatDateTime(DateTimeFormat.Mask, Value);
  end;
end;

procedure TJvConverter.SetDataType(Value: TDataType);
begin
  if Value <> FDataType then
  begin
    FDataType := Value;
    try
      CheckDataType;
      Change;
    except
      Clear;
      if RaiseOnError then
        raise;
    end;
  end;
end;

function TJvConverter.IsValidChar(Ch: Char): Boolean;
begin
  case FDataType of
    dtString:
      Result := True;
    dtInteger:
      Result := Ch in DigitSymbols + SignSymbols;
    dtFloat:
      Result := Ch in DigitSymbols + SignSymbols + [DecimalSeparator, 'E', 'e'];
    dtDateTime, dtDate, dtTime:
      Result := True;
    dtBoolean:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TJvConverter.CheckDataType;
begin
  case FDataType of
    dtInteger, dtFloat:
      StrToFloat(GetString);
    dtDateTime, dtDate, dtTime:
      GetDateTime;
  end;
end;

function TJvConverter.GetAsBoolean: Boolean;
var
  S: string;
begin
  S := GetString;
  Result := (Length(S) > 0) and ((S[1] in ['T', 't', 'Y', 'y']) or
    (S = FTextValues[True]));
end;

function TJvConverter.GetDateTime: TDateTime;
var
  S: string;
  I: Integer;
  DateS, TimeS: set of Char;
begin
  S := GetString;
  DateS := ['/', '.'] + [DateTimeFormat.DateSeparator] -
    [DateTimeFormat.TimeSeparator];
  TimeS := [':', '-'] - [DateTimeFormat.DateSeparator] +
    [DateTimeFormat.TimeSeparator];
  for I := 1 to Length(S) do
  begin
    if S[I] in DateS then
      S[I] := DateSeparator
    else
    if S[I] in TimeS then
      S[I] := TimeSeparator;
  end;
  Result := StrToDateTime(S);
end;

function TJvConverter.GetAsDateTime: TDateTime;
begin
  try
    Result := GetDateTime;
  except
    Result := NullDate;
  end;
end;

function TJvConverter.GetAsDate: TDateTime;
var
  Year, Month, Day: Word;
begin
  try
    Result := GetAsDateTime;
    DecodeDate(Result, Year, Month, Day);
    Result := EncodeDate(Year, Month, Day);
  except
    Result := NullDate;
  end;
end;

function TJvConverter.GetAsTime: TDateTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  try
    Result := GetAsDateTime;
    DecodeTime(Result, Hour, Min, Sec, MSec);
    Result := EncodeTime(Hour, Min, Sec, MSec);
  except
    Result := NullDate;
  end;
end;

function TJvConverter.GetAsFloat: Double;
begin
  try
    case FDataType of
      dtDateTime:
        Result := GetAsDateTime;
      dtDate:
        Result := GetAsDate;
      dtTime:
        Result := GetAsTime;
    else
      Result := StrToFloat(GetString);
    end;
  except
    Result := 0.0;
  end;
end;

function TJvConverter.GetAsInteger: Longint;
begin
  Result := Round(GetAsFloat);
end;

function TJvConverter.GetAsString: string;
begin
  case FDataType of
    dtString:
      Result := GetString;
    dtInteger:
      Result := IntToStr(GetAsInteger);
    dtFloat:
      Result := FloatToString(GetAsFloat);
    dtDateTime:
      Result := DateTimeToString(GetAsDateTime);
    dtDate:
      Result := DateTimeToString(GetAsDate);
    dtTime:
      Result := DateTimeToString(GetAsTime);
    dtBoolean:
      Result := BoolToStr(GetAsBoolean);
  end;
end;

procedure TJvConverter.SetAsBoolean(Value: Boolean);
begin
  SetAsString(BoolToStr(Value));
end;

procedure TJvConverter.SetAsDateTime(Value: TDateTime);
begin
  SetAsString(DateTimeToStr(Value));
end;

procedure TJvConverter.SetAsDate(Value: TDateTime);
begin
  SetAsDateTime(Value);
end;

procedure TJvConverter.SetAsTime(Value: TDateTime);
begin
  SetAsDateTime(Value);
end;

procedure TJvConverter.SetAsFloat(Value: Double);
begin
  if FDataType in [dtDateTime, dtDate, dtTime] then
    SetAsDateTime(Value)
  else
    SetAsString(FloatToStr(Value));
end;

procedure TJvConverter.SetAsInteger(Value: Longint);
begin
  if FDataType = dtInteger then
    SetAsString(IntToStr(Value))
  else
    SetAsFloat(Value);
end;

procedure TJvConverter.SetAsString(const Value: string);
var
  S: string;
begin
  S := GetString;
  SetString(Value);
  try
    CheckDataType;
    Change;
  except
    SetString(S);
    if RaiseOnError then
      raise;
  end;
end;

end.

