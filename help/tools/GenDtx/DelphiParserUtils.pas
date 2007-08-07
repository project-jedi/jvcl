unit DelphiParserUtils;

interface

uses
  Classes;

type
  TCaseSensitiveStringList = class(TStringList)
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

function GetRealFileName(const ADir, AFileName: string): string;
procedure DiffLists(Source1, Source2, InBoth, NotInSource1, NotInSource2: TStrings;
  const CaseSensitive: Boolean = False);
procedure ExcludeList(Source, RemoveList: TStrings; const CaseSensitive: Boolean = False);
procedure EnsureEndingCRLF(var S: string);
function RemoveStartEndCRLF(const S: string): string;

implementation

uses
  Windows, SysUtils;

//=== Global procedures ======================================================

procedure DiffLists(Source1, Source2, InBoth, NotInSource1, NotInSource2: TStrings; const CaseSensitive: Boolean =
  False);
var
  Index1, Index2: Integer;
  C: Integer;
begin
  if not Assigned(Source1) or not Assigned(Source2) then
    Exit;

  Index1 := 0;
  Index2 := 0;
  while (Index1 < Source1.Count) and (Index2 < Source2.Count) do
  begin
    if CaseSensitive then
      C := CompareStr(Source1[Index1], Source2[Index2])
    else
      C := AnsiCompareText(Source1[Index1], Source2[Index2]);
    if C = 0 then
    begin
      if Assigned(InBoth) then
        InBoth.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1);
      Inc(Index2);
    end
    else
      if C < 0 then
    begin
      if Assigned(NotInSource2) then
        NotInSource2.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1)
    end
    else
      if C > 0 then
    begin
      if Assigned(NotInSource1) then
        NotInSource1.AddObject(Source2[Index2], Source2.Objects[Index2]);
      Inc(Index2);
    end;
  end;

  if Assigned(NotInSource1) then
    while Index2 < Source2.Count do
    begin
      NotInSource1.AddObject(Source2[Index2], Source2.Objects[Index2]);
      Inc(Index2);
    end;
  if Assigned(NotInSource2) then
    while Index1 < Source1.Count do
    begin
      NotInSource2.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1);
    end;
end;

procedure EnsureEndingCRLF(var S: string);
var
  L: Integer;
begin
  { Ensure ending #13#10 }
  L := Length(S);
  while (L > 0) and (S[L] in [#10, #13]) do
    Dec(L);
  SetLength(S, L + 2);
  S[L + 1] := #13;
  S[L + 2] := #10;
end;

procedure ExcludeList(Source, RemoveList: TStrings; const CaseSensitive: Boolean = False);
var
  SourceIndex, RemoveIndex: Integer;
  C: Integer;
begin
  SourceIndex := 0;
  RemoveIndex := 0;
  while (SourceIndex < Source.Count) and (RemoveIndex < RemoveList.Count) do
  begin
    if CaseSensitive then
      C := CompareStr(Source[SourceIndex], RemoveList[RemoveIndex])
    else
      C := AnsiCompareText(Source[SourceIndex], RemoveList[RemoveIndex]);
    if C = 0 then
    begin
      Source.Delete(SourceIndex);
      Inc(RemoveIndex);
    end
    else
      if C < 0 then
      Inc(SourceIndex)
    else
      if C > 0 then
      Inc(RemoveIndex);
  end;
end;

function GetRealFileName(const ADir, AFileName: string): string;
var
  FindData: TWin32FindData;
  Handle: THandle;
  LFileName: string;
begin
  LFileName := IncludeTrailingPathDelimiter(ADir) + AFileName;

  Handle := FindFirstFile(PChar(LFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := ExtractFileName(FindData.cFileName);
  end
  else
    Result := AFileName;
end;

function RemoveStartEndCRLF(const S: string): string;
var
  I, J: Integer;
begin
  I := 1;
  J := Length(S);
  while (I <= J) and (S[I] in [#10, #13]) do
    Inc(I);
  while (I <= J) and (S[J] in [#10, #13]) do
    Dec(J);

  if I > J then
    Result := ''
  else
    Result := Copy(S, I, J - I + 1);
end;

//=== TCaseSensitiveStringList ===============================================

function TCaseSensitiveStringList.CompareStrings(const S1,
  S2: string): Integer;
begin
  Result := CompareStr(S1, S2);
end;

end.
