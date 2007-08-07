unit DtxFilesPreProcessor;

interface

uses
  JVCLHelpUtils,
  Classes;

type
  TDtxFilesPreProcessor = class(TTask)
  private
    FSourceDir: string;
    function ProcessFile(const DtxFileName: string): Boolean;
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property SourceDir: string read FSourceDir write FSourceDir;
  end;

implementation

uses SysUtils;

{ TDtxFilesPreProcessor }

function TDtxFilesPreProcessor.CanStart: Boolean;
begin
  Result := CheckDir(SourceDir);
end;

function TDtxFilesPreProcessor.DoExecute: Boolean;
var
  DtxFiles: TStringList;
  I: Integer;
begin
  Result := True;

  DtxFiles := TStringList.Create;
  try
    StatusMsg('Retrieving files from dtx directory..');
    GetAllFilesFrom(SourceDir, '*.dtx', DtxFiles);
//DtxFiles.Add('C:\Comp\help\Gen\Online\JvSpacer.dtx');

    StatusMsg('processing files..');
    for I := 0 to DtxFiles.Count - 1 do
    begin
      Progress(I, DtxFiles.Count);
      ProcessFile(DtxFiles[I]);
    end;
  finally
    DtxFiles.Free;
  end;
end;

function TDtxFilesPreProcessor.GetTaskDescription: string;
begin
  Result := 'Preprocessing dtx files..';
end;

const
  cWhiteSpace = [' ', #8, #13, #10];

function IsEmptyLine(const S: string): Boolean;
var
  P: PChar;
begin
  P := PChar(S);
  while P^ in cWhiteSpace do
    Inc(P);
  Result := P^ = #0;
end;

function Is1stDotLine(const S: string): Boolean;
begin
  Result := (Length(S) >= 3) and (CompareStr('  *', Copy(S, 1, 3)) = 0);
end;

function IsDotLine(const S: string): Boolean;
var
  P: PChar;
begin
  P := PChar(S);
  while P^ in cWhiteSpace do
    Inc(P);
  Result := P^ = '*';
end;

function IsOkDotLine(const S: string): Boolean;
var
  L: Integer;
begin
  L := Length(S);
  Result :=
    ((L = 3) and (CompareStr(S, '  *') = 0)) or
    ((L = 4) and (CompareStr(S, '  * ') = 0)) or
    ((L >= 5) and (CompareStr(Copy(S, 1, 4), '  * ') = 0) and not (S[5] in cWhiteSpace));
end;

function IsOkNonDotLine(const S: string): Boolean;
var
  L: Integer;
begin
  if IsEmptyLine(S) then
    Result := True
  else
  begin
    L := Length(S);
    Result :=
      (L >= 5) and (CompareStr(Copy(S, 1, 4), '    ') = 0) and not (S[5] in cWhiteSpace);
  end;
end;

procedure FormatDotLine(var S: string);
var
  P, Q: PChar;
  T: string;
begin
  if IsOkDotLine(S) then
    Exit;

  P := PChar(S);
  Q := P;
  while P^ in cWhiteSpace do
    Inc(P);
  if P^ <> '*' then
    Exit;
  Inc(P);
  while P^ in cWhiteSpace do
    Inc(P);

  SetString(T, P, Length(S) - (P - Q));
  S := '  * ' + T;
end;

procedure FormatNonDotLine(var S: string);
var
  P, Q: PChar;
  T: string;
begin
  if IsOkNonDotLine(S) then
    Exit;

  P := PChar(S);
  Q := P;
  while P^ in cWhiteSpace do
    Inc(P);
  if P^ = #0 then
    S := ''
  else
  begin
    SetString(T, P, Length(S) - (P - Q));
    S := '    ' + T;
  end;
end;

const
  cSections: array[0..2] of string = (
    'Note', 'See Also', 'Parameters'
  );
function IsSectionLine(const S: string): Boolean;
var
  P, Q: PChar;
  L: Integer;
  I: Integer;
  Section: string;
begin
  Q := PChar(S);
  P := PChar(S) + Length(S);
  while Q^ in cWhiteSpace do
    Inc(Q);
  while (Q < P) and ((P-1)^ in cWhiteSpace) do
    Dec(P);

  L := P-Q;
  for I := Low(cSections) to High(cSections) do
  begin
    Section := cSections[i];
    Result := (L = Length(Section)) and Comparemem(Q, PChar(Section), L);
    if Result then Exit;
  end;

  Result := False;
end;

function IsNotDotListLine(const S: string): Boolean;
begin
  Result := IsEmptyLine(S) or
    IsSectionLine(S);
end;

function TDtxFilesPreProcessor.ProcessFile(
  const DtxFileName: string): Boolean;
var
  AFile: TStringList;
  FileChanged: Boolean;
  LineIndex: Integer;
  CurrentLine: string;
  InDotList: Boolean;
begin
  Result := True;

  AFile := TStringList.Create;
  try
    AFile.LoadFromFile(DtxFilename);

    FileChanged := False;
    LineIndex := 0;
    InDotList := False;

    while LineIndex < AFile.Count do
    begin
      CurrentLine := AFile[LineIndex];

      if InDotList then
      begin
        if IsNotDotListLine(CurrentLine) then
          InDotList := False
        else
        if IsDotLine(CurrentLine) then
          FormatDotLine(CurrentLine)
        else
          FormatNonDotLine(CurrentLine)
      end
      else
      begin
        if Is1stDotLine(CurrentLine) then
        begin
          InDotList := True;
          FormatDotLine(CurrentLine)
        end;
      end;

      FileChanged := FileChanged or (CompareStr(CurrentLine, AFile[LineIndex]) <> 0);
      AFile[LineIndex] := CurrentLine;
      Inc(LineIndex);
    end;

    if FileChanged then
    begin
      AFile.SaveToFile(DtxFileName);
    end;
  finally
    AFile.Free;
  end;
end;

end.

