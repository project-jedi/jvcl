unit FileItem;

interface

uses Contnrs, Classes;

type
  TFileItem = class (TObject)
  private
    FName: string;
    FSummary: string;
    FDescription: string;
    FReturnValue: string;
    FAuthor: string;
    FSeeAlsoAsString: string;
    FParameters: string;
    FPreText: string;
    FJVCLInfo: string;
  public
    constructor Create(slist : TStringList; var curLineIndex : Integer);
    destructor Destroy; override;

    function GetRawText : string;

    property Name : string read FName write FName;
    property Summary : string read FSummary write FSummary;
    property Author : string read FAuthor write FAuthor;
    property Description : string read FDescription write FDescription;
    property ReturnValue : string read FReturnValue write FReturnValue;
    property SeeAlsoAsString : string read FSeeAlsoAsString write FSeeAlsoAsString;
    property Parameters : string read FParameters write FParameters;
    property PreText : string read FPreText write FPreText;
    property JVCLInfo : string read FJVCLInfo write FJVCLInfo; 
  end;

  TFileItemList = class(TObjectList)
  private
    function GetItems(Index: Integer): TFileItem;
    procedure SetItems(Index: Integer; const Value: TFileItem);
  public
    property Items[Index : Integer] : TFileItem read GetItems write SetItems; default;
  end;

implementation

uses JclStrings, JclLogic, SysUtils;

const
  WrappingColumn : Integer = 100;
  IndentationSpaces : Integer = 2;

function Unwrap(Text : string; ColumnBreak : Integer; Indentation : Integer) : string;
var
  Lines : TStringList;
  ResultLines : TStringList;
  curLine, prevLine : string;
  I : Integer;
begin
  Lines := TStringList.Create;
  ResultLines := TStringList.Create;
  try
    StrToStrings(Text, #13#10, Lines, True);

    for I := 0 to Lines.Count - 1 do
    begin
      // first remove all indentation (if necessary) and trim spaces
      // on the right
      if StrCompareRange(Lines[I], '  ', 1, Indentation) = 0 then
        curLine := Copy(Lines[I], Indentation+1, Length(Lines[I]))
      else
        curLine := TrimLeft(Lines[I]);
      curLine := TrimRight(curLine);

      // then do the unwrapping

      // find out if the line break at the end of the current line
      // must be ignored because it was done by the wordwrapping
      // process, not deliberately by the user.
      if (I > 0) and
        // The previous line before must be longer than 75%
        // of the ColumnBreak
        (Length(prevLine) > 75 * ColumnBreak div 100) and
        // The current line must not start with the same 2 characters
        // as the line before. This way, we prevent a list enumeration
        // to end up all on the same line
        ((Length(curLine) >= 2) and
         (Length(prevLine) >=2) and 
         (StrCompareRange(curLine, prevLine, 1, 2) <> 0)) and
        // The line before must not end with a point
        (prevLine[Length(prevLine)] <> '.') and
        // The current line must start with a lower case letter
        CharIsLower(curLine[1]) then
      begin
        ResultLines[ResultLines.Count-1] := ResultLines[ResultLines.Count-1] + ' ' + curLine;
      end
      else
        ResultLines.Add(curLine);

      // store the line that will become the previous on the next iteration
      prevLine := curLine;
    end;

    // Remove all empty lines at the end of the result
    while (ResultLines.Count > 0 ) and
      (ResultLines[ResultLines.Count-1] = '') do
      ResultLines.Delete(ResultLines.Count-1);

    Result := StringsToStr(ResultLines, #13#10, True);
  finally
    ResultLines.Free;
    Lines.Free;
  end;
end;

function Wrap(Text : string; ColumnBreak : Integer; Indentation : Integer) : string;
var
  Lines : TStringList;
  curLine: string;
  breakIndex : Integer;
  I : Integer;
begin
  Lines := TStringList.Create;
  try
    StrToStrings(Text, #13#10, Lines, True);

    I := 0;
    while I < Lines.Count do
    begin
      curLine := TrimRight(Lines[I]);

      // find out if the current line needs to be split
      // It needs it if its length is greater than the column
      // break index
      if Length(curLine) > ColumnBreak then
      begin
        // then find the column where we cut. It's the first
        // space, tab, or dash starting at the end of the line
        breakIndex := StrLastPos(' ', StrLeft(curLine, ColumnBreak));
        breakIndex := Max(breakIndex, StrLastPos(#9, StrLeft(curLine, ColumnBreak)));
        breakIndex := Max(breakIndex, StrLastPos('-', StrLeft(curLine, ColumnBreak)));

        // Do the splitting
        Lines[I] := StrLeft(curLine, breakIndex-1);
        Lines.Insert(I+1, StrRestOf(curLine, breakIndex+1));
      end;

      // apply the indentation
      Lines[I] := StrRepeat(' ', Indentation) + Lines[I];

      // go to next line
      Inc(I);
    end;

    Result := StringsToStr(Lines, #13#10, True) + #13#10;
  finally
    Lines.Free;
  end;
end;


{ TFileItem }

constructor TFileItem.Create(slist: TStringList;
  var curLineIndex: Integer);
var
  curLine : string;
  curSectionPtr : ^string;
  curSectionContent : string;
begin
  inherited Create;

  curLine := slist[curLineIndex];
  FName := Copy(curLine, 3, Length(curLine));

  curSectionPtr := nil;
  curSectionContent := '';

  repeat
    curLine := slist[curLineIndex];

    if AnsiCompareText(curLine, 'Summary') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FSummary;
    end
    else if AnsiCompareText(curLine, 'Author') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FAuthor;
    end
    else if AnsiCompareText(curLine, 'Description') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FDescription;
    end
    else if AnsiCompareText(curLine, 'Return value') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FReturnValue;
    end
    else if AnsiCompareText(curLine, 'See Also') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FSeeAlsoAsString;
    end
    else if AnsiCompareText(curLine, 'Parameters') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FParameters;
    end
    else if AnsiCompareText(curLine, 'JVCLInfo') = 0 then
    begin
      curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);
      curSectionContent := '';
      curSectionPtr := @FJVCLInfo;
    end
    else if (curSectionPtr <> nil) and not StrHasPrefix(curLine, ['----------']) then
    begin
      curSectionContent := curSectionContent + curLine + #13#10;
    end;

    if curSectionPtr = nil then
      curSectionPtr := @FPreText;
      
    Inc(curLineIndex)
  until (curLineIndex >= slist.count) or StrHasPrefix(trim(slist[curLineIndex]), ['@@']);

  // As we got out, we need to set the value of the section that
  // got pointed to last.
  curSectionPtr^ := Unwrap(curSectionContent, WrappingColumn, IndentationSpaces);;

  // go back one line
  Dec(curLineIndex);
end;

destructor TFileItem.Destroy;
begin

  inherited;
end;

function TFileItem.GetRawText: string;
  function FormattedOutput(str : string) : string;
  begin
    Result := Wrap(str, WrappingColumn, IndentationSpaces);
  end;
begin
  Result := '----------------------------------------------------------------------------------------------------'#13#10;
  Result := Result + '@@' + FName + #13#10;
  if FPreText <> '' then
    Result := Result + FPreText + #13#10;
    
  if FJVCLInfo <> '' then
  begin
    Result := Result + 'JVCLInfo'#13#10;
    Result := Result + FormattedOutput(FJVCLInfo);
  end;

  if FSummary <> '' then
  begin
    Result := Result + 'Summary'#13#10;
    Result := Result + FormattedOutput(FSummary);
  end;

  if FAuthor <> '' then
  begin
    Result := Result + 'Author'#13#10;
    Result := Result + FormattedOutput(FAuthor);
  end;

  if FDescription <> '' then
  begin
    Result := Result + 'Description'#13#10;
    Result := Result + FormattedOutput(FDescription);
  end;

  if FParameters <> '' then
  begin
    Result := Result + 'Parameters'#13#10;
    Result := Result + FormattedOutput(FParameters);
  end;

  if FReturnValue <> '' then
  begin
    Result := Result + 'Return value'#13#10;
    Result := Result + FormattedOutput(FReturnValue);
  end;

  if FSeeAlsoAsString <> '' then
  begin
    Result := Result + 'See Also'#13#10;
    Result := Result + FormattedOutput(FSeeAlsoAsString);
  end;
end;

{ TFileItemList }

function TFileItemList.GetItems(Index: Integer): TFileItem;
begin
  Result := TFileItem(inherited Items[Index]);
end;

procedure TFileItemList.SetItems(Index: Integer; const Value: TFileItem);
begin
  inherited Items[Index] := Value;
end;

end.
