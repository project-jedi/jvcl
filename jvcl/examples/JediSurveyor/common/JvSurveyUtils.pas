{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

{$I jvcl.inc}

unit JvSurveyUtils;

interface
uses
  JvSurveyIntf, JvComputerInfoEx;

function DecodeChoice(const S: WideString; ASurveyType: TJvSurveyType): string;
function EncodeChoice(const S: WideString; ASurveyType: TJvSurveyType): string;

function DecodeResponse(const S: WideString; ASurveyType: TJvSurveyType): string;
function EncodeResponse(const S: string; ASurveyType: TJvSurveyType): WideString;
function DecodeString(const S: WideString): string;
function EncodeString(const S: WideString): string;
function DecodeType(const S: string): TJvSurveyType;
function EncodeType(AType: TJvSurveyType): string;

function YesNo(const ACaption, AText: string): boolean;
function IsChecked(Item: IJvSurveyItem; Index: integer): boolean;
function ISODateToStr(const ADate:TDateTime):string;
function ISOStrToDate(const S:string):TDateTime;


function MyAnsiLastChar(const S: string): PChar;

const
  cRecordSeparator = ';';
  cSurveyFileExt = '.jsf'; // Jedi Survey File
  cResponseFileExt = '.jrf'; // Jedi Response File
  cReportFileExt    = '.jsr';  // Jedi Survey Report

resourcestring
  SSurveyFileFilter = 'Survey files|*.jsf|All files|*.*';
  SResponseFileFilter = 'Response files|*.jrf|All files|*.*';
  SReportFileFilter = 'Report files|*.jsr|All files|*.*';


implementation
uses
  Windows, Classes, SysUtils, JclStrings;

function DecodeString(const S: WideString): string;
var
  I, L, J: Integer;
  NeedsSpace: boolean;
  tmp: string;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  if I > L then
    tmp := ''
  else
  begin
    while S[L] <= ' ' do
      Dec(L);
    tmp := Copy(S, I, L - I + 1);
  end;

  // now, trim within and unescape c-style control codes (\n and \t only)
  J := 0;
  SetLength(Result, Length(tmp));
  NeedsSpace := false;
  while I <= L do
  begin
    case tmp[i] of
      #0..#32:
        if NeedsSpace then
        begin
          Inc(j);
          Result[j] := tmp[i];
          NeedsSpace := false;
        end
        else
          NeedsSpace := true;
      '\':
        if (i < L) then
          case tmp[i + 1] of
            'n':
              begin
                Inc(j);
                Result[j] := #13;
                Inc(j);
                Result[j] := #10;
                Inc(i);
              end;
            't':
              begin
                Inc(j);
                Result[j] := #9;
              end;
            '\': Inc(i);
          end
        else
        begin
          Inc(j);
          Result[j] := '\';
        end;
    else
      begin
        Inc(j);
        Result[j] := tmp[i];
        NeedsSpace := true;
      end;
    end;
    Inc(i);
  end;
  SetLength(Result, j);
end;

function EncodeString(const S: WideString): string;
begin
  Result := StringReplace(S, '\n', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, ' ', [rfReplaceAll]);
end;

function DecodeChoice(const S: WideString; ASurveyType: TJvSurveyType): string;
begin
  if ASurveyType = stFreeForm then
    Result := trim(StringReplace(S, '\n', #13#10, [rfReplaceAll]))
  else
    Result := trim(StringReplace(S, cRecordSeparator, #13#10, [rfReplaceAll]));
end;

function EncodeChoice(const S: WideString; ASurveyType: TJvSurveyType): string;
var
  T: TStringlist;
begin
  Result := '';
  T := TStringlist.Create;
  try
    T.Text := S;
    if ASurveyType = stFreeForm then
      Result := StringReplace(trim(S),#13#10,'\n',[rfReplaceAll])
    else
      Result := StringReplace(trim(S),#13#10,cRecordSeparator,[rfReplaceAll]);
    if (MyAnsiLastChar(Result) = cRecordSeparator) then
      SetLength(Result, Length(Result) - 1);
  finally
    T.Free;
  end;
end;

function DecodeResponse(const S: WideString; ASurveyType: TJvSurveyType): string;
begin
  Result := DecodeChoice(S, ASurveyType);
end;

function EncodeResponse(const S: string; ASurveyType: TJvSurveyType): WideString;
begin
  Result := EncodeChoice(S, ASurveyType);
end;

function DecodeType(const S: string): TJvSurveyType;
begin
  if AnsiSameText(S, 'exclusive') then
    Result := stExclusive
  else if AnsiSameText(S, 'multiple') then
    Result := stMultiple
  else
    Result := stFreeForm;
end;

function EncodeType(AType: TJvSurveyType): string;
begin
  case AType of
    stExclusive:
      Result := 'exclusive';
    stMultiple:
      Result := 'multiple';
  else
    Result := 'freeform';
  end;
end;

function YesNo(const ACaption, AText: string): boolean;
begin
  Result := MessageBox(GetFocus, PChar(AText), PChar(ACaption), MB_YESNO) = IDYES;
end;

function IsChecked(Item: IJvSurveyItem; Index: integer): boolean;
var
  S: TStringlist;
begin
  S := TStringlist.Create;
  try
    S.Text := DecodeResponse(item.Responses, item.SurveyType);
    Result := ((Index >= 0) and (Index < S.Count) and (S[Index] = '1'))
      or ((item.SurveyType = stFreeForm) and (S.Count > 0));
  finally
    S.Free;
  end;
end;

function MyAnsiLastChar(const S: string): PChar;
begin
  if Length(S) = 0 then
    Result := #0
  else
    Result := AnsiLastChar(S);
end;

function ISODateToStr(const ADate:TDateTime):string;
begin
  Result := FormatDateTime('yyyy-MM-dd',ADate);
end;

function ISOStrToDate(const S:string):TDateTime;
begin
  Result := EncodeDate(StrToInt(Copy(S,1,4)),
    StrToInt(Copy(S,6,2)),StrToInt(Copy(S,9,2)));
end;

end.

