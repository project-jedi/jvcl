{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{                                                       }
{*******************************************************}

unit Options;
{$J+}

interface

uses SysUtils, Classes, IniFiles, DB, DBTables;

const
  AutoActivate: Boolean = True;
  SystemTables: Boolean = False;
  ASCIIDelimited: Boolean = True;
  defFloatFormat: string = ',0.00';
  defDateFormat: string = 'dd/mm/yyyy';
  defTimeFormat: string = 'hh:mm:ss';
  defDateTimeFormat: string = 'dd/mm/yyyy hh:mm:ss';
  ASCIICharSet: string = 'ascii';
  SQLHistoryCapacity: Integer = 20;
  LiveQueries: Boolean = True;
  ShowExecTime: Boolean = False;
  SQLCalcCount: Boolean = False;
  QueryInThreads: Boolean = True;
  EnableQueryAbort: Boolean = True;
  SQLTraceBuffer: Integer = 256;
  SQLTraceFlags: TTraceFlags = [tfQExecute, tfError, tfConnect, tfTransact,
    tfBlob, tfMisc];
  MinSQLTraceBuffer = 32;

procedure SetKeepConnections(Value: Boolean);
procedure SaveOptions(IniFile: TIniFile);
procedure LoadOptions(IniFile: TIniFile);

implementation

uses Consts, JvJVCLUtils, JvBDEUtils, DBCbRest;

const
  siOptions = 'Options';
  siAutoActive = 'AutoActivate';
  siSystemTables = 'SystemTables';
  siKeepConnections = 'KeepConnections';
  siFloatFormat = 'FloatFormat';
  siDateTimeFormat = 'DateTimeFormat';
  siDateFormat = 'DateFormat';
  siTimeFormat = 'TimeFormat';
  siAsciiCharSet = 'ASCIICharSet';
  siAsciiFormat = 'ASCIIFormat';
  siSQLHistoryCapacity = 'SQLHistoryCapacity';
  siLiveQueries = 'LiveQueries';
  siQueryTime = 'ShowQueryTime';
  siQueryInThreads = 'RunQueryInSeparateThread';
  siEnableQueryAbort = 'EnableQueryAbort';
  siSQLTraceFlags = 'SQLTraceFlags';
  siEnableTrace = 'EnableTraceSQL';
  siSQLTraceBuffer = 'SQLTraceBufferSize';
  siShowRestWarnings = 'ShowRestuctureWarnings';
  siSQLCount = 'SQLCalcCount';

procedure SetKeepConnections(Value: Boolean);
var
  I: Integer;
begin
  Session.KeepConnections := Value;
  for I := 0 to Session.DatabaseCount - 1 do
    if Session.Databases[I] <> nil then
      Session.Databases[I].KeepConnection := Session.KeepConnections;
end;

procedure SaveOptions(IniFile: TIniFile);
begin
  with IniFile do begin
    WriteBool(siOptions, siAutoActive, AutoActivate);
    WriteBool(siOptions, siSystemTables, SystemTables);
    WriteBool(siOptions, siKeepConnections, Session.KeepConnections);
    WriteBool(siOptions, siLiveQueries, LiveQueries);
    WriteBool(siOptions, siQueryTime, ShowExecTime);
    WriteBool(siOptions, siSQLCount, SQLCalcCount);
    WriteBool(siOptions, siShowRestWarnings, ShowRestWarnings);
    WriteString(siOptions, siFloatFormat, defFloatFormat);
    WriteString(siOptions, siDateFormat, defDateFormat);
    WriteString(siOptions, siDateTimeFormat, defDateTimeFormat);
    WriteString(siOptions, siTimeFormat, defTimeFormat);
    WriteBool(siOptions, siAsciiFormat, ASCIIDelimited);
    WriteString(siOptions, siAsciiCharSet, ASCIICharSet);
    WriteInteger(siOptions, siSQLHistoryCapacity, SQLHistoryCapacity);
    WriteBool(siOptions, siQueryInThreads, QueryInThreads);
    WriteBool(siOptions, siEnableQueryAbort, EnableQueryAbort);
    WriteInteger(siOptions, siSQLTraceFlags, Word(SQLTraceFlags));
    WriteInteger(siOptions, siSQLTraceBuffer, SQLTraceBuffer);
  end;
end;

procedure LoadOptions(IniFile: TIniFile);
begin
  with IniFile do begin
    AutoActivate := ReadBool(siOptions, siAutoActive, AutoActivate);
    SystemTables := ReadBool(siOptions, siSystemTables, SystemTables);
    LiveQueries := ReadBool(siOptions, siLiveQueries, LiveQueries);
    ShowExecTime := ReadBool(siOptions, siQueryTime, ShowExecTime);
    SQLCalcCount := ReadBool(siOptions, siSQLCount, SQLCalcCount);
    ShowRestWarnings := ReadBool(siOptions, siShowRestWarnings, ShowRestWarnings);
    SetKeepConnections(ReadBool(siOptions, siKeepConnections,
      Session.KeepConnections));
    defFloatFormat := ReadString(siOptions, siFloatFormat, defFloatFormat);
    defDateFormat := ReadString(siOptions, siDateFormat, defDateFormat);
    defDateTimeFormat := ReadString(siOptions, siDateTimeFormat, defDateTimeFormat);
    defTimeFormat := ReadString(siOptions, siTimeFormat, defTimeFormat);
    ASCIIDelimited := ReadBool(siOptions, siAsciiFormat, ASCIIDelimited);
    ASCIICharSet := ReadString(siOptions, siAsciiCharSet, ASCIICharSet);
    SQLHistoryCapacity := ReadInteger(siOptions, siSQLHistoryCapacity, SQLHistoryCapacity);
    EnableQueryAbort := ReadBool(siOptions, siEnableQueryAbort, EnableQueryAbort);
    QueryInThreads := ReadBool(siOptions, siQueryInThreads, QueryInThreads);
    SQLTraceFlags := TTraceFlags(Word(ReadInteger(siOptions, siSQLTraceFlags, Word(SQLTraceFlags))));
    SQLTraceBuffer := ReadInteger(siOptions, siSQLTraceBuffer, SQLTraceBuffer);
  end;
end;

end.
