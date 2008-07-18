unit JvGnugettextD5;
// Delphi 5 optimized interface for gnugettext.pas
// This unit must only be used on Delphi 5. When you upgrade to Delphi 6 or
// later, you should remove this unit and replace all reference to gnugettextD5
// with refernces to gnugettext.

interface

uses
  Classes;

// Ansistring versions of the api
function _(const szMsgId: string): string;
function gettext(const szMsgId: string): string;
function dgettext(const szDomain: string; const szMsgId: string): string;
procedure TranslateComponent(AnObject: TComponent);



//*****************************************************************************
// Don't use anything in the interface below this line.
// It only contains code or gnugettext.pas to make it compile with Delphi 5.

type
  UTF8String = AnsiString;

const
  PathDelim = '\';
  sLineBreak = #13#10;

function GetEnvironmentVariable(const VarName: string): string;
function DirectoryExists(const FileName: string): Boolean;
function IncludeTrailingPathDelimiter(s: string): string;
function ExcludeTrailingPathDelimiter(s: string): string;
procedure RaiseLastOSError;
function StrToFloatDef(const S: string; Default: Extended): Extended;
function Utf8Decode(const S: UTF8String): WideString;
function Utf8Encode(const WS: WideString): UTF8String;

implementation

uses
  Windows, SysUtils,
  JvGnugettext;

function GetEnvironmentVariable(const VarName: string): string;
var Size: Integer;
begin
  Size := Windows.GetEnvironmentVariable(PChar(VarName), nil, 0);
  SetLength(Result, Size - 1);
  Windows.GetEnvironmentVariable(PChar(VarName), PChar(Result), Size);
end;

function DirectoryExists(const Filename: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Filename));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

function IncludeTrailingPathDelimiter(s: string): string;
begin
  Result := IncludeTrailingBackslash(s);
end;

function ExcludeTrailingPathDelimiter(s: string): string;
begin
  Result := ExcludeTrailingBackslash(s);
end;

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function StrToFloatDef(const S: string; Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, Count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;
  Count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (Count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if Count + 3 > MaxDestBytes then
          Break;
        Dest[Count] := Char($E0 or (c shr 12));
        Dest[Count + 1] := Char($80 or ((c shr 6) and $3F));
        Dest[Count + 2] := Char($80 or (c and $3F));
        Inc(Count, 3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if Count + 2 > MaxDestBytes then
          Break;
        Dest[Count] := Char($C0 or (c shr 6));
        Dest[Count + 1] := Char($80 or (c and $3F));
        Inc(Count, 2);
      end;
    end;
    if Count >= MaxDestBytes then
      Count := MaxDestBytes - 1;
    Dest[Count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(Count);
        Inc(Count);
      end;
      Inc(Count);
    end;
  end;
  Result := Count + 1; // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, Count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte

        Dest[Count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[Count] := WideChar(wc);
      Inc(Count);
    end;
    if Count >= MaxDestChars then
      Count := MaxDestChars - 1;
    Dest[Count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte
      end;
      Inc(Count);
    end;
  end;
  Result := Count + 1;
end;

function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp) + 1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then
    Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp) + 1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function _(const szMsgId: string): string;
begin
  Result := string(DefaultInstance.gettext(DefaultInstance.ansi2wide(szMsgId)));
end;

function gettext(const szMsgId: string): string;
begin
  Result := string(DefaultInstance.gettext(DefaultInstance.ansi2wide(szMsgId)));
end;

function dgettext(const szDomain: string; const szMsgId: string): string;
begin
  Result := string(DefaultInstance.dgettext(szDomain, DefaultInstance.ansi2wide(szMsgId)));
end;

procedure TranslateComponent(AnObject: TComponent);
begin
  JvGnugettext.TranslateComponent(AnObject);
end;

end.
