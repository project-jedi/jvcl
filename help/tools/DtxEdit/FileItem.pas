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
  end;

  TFileItemList = class(TObjectList)
  private
    function GetItems(Index: Integer): TFileItem;
    procedure SetItems(Index: Integer; const Value: TFileItem);
  public
    property Items[Index : Integer] : TFileItem read GetItems write SetItems; default;
  end;

implementation

uses JclStrings, SysUtils;

{ TFileItem }

constructor TFileItem.Create(slist: TStringList;
  var curLineIndex: Integer);
var
  curLine : string;
  curValue : ^string;
begin
  inherited Create;

  curLine := slist[curLineIndex];
  FName := Copy(curLine, 3, Length(curLine));

  curValue := nil;

  repeat
    curLine := slist[curLineIndex];

    if AnsiCompareText(curLine, 'Summary') = 0 then
      curValue := @FSummary
    else if AnsiCompareText(curLine, 'Author') = 0 then
      curValue := @FAuthor
    else if AnsiCompareText(curLine, 'Description') = 0 then
      curValue := @FDescription
    else if AnsiCompareText(curLine, 'Return value') = 0 then
      curValue := @FReturnValue
    else if AnsiCompareText(curLine, 'See Also') = 0 then
      curValue := @FSeeAlsoAsString
    else if AnsiCompareText(curLine, 'Parameters') = 0 then
      curValue := @FParameters
    else if (curValue <> nil) and not StrHasPrefix(curLine, ['----------']) then
    begin
      if curValue^ = '' then
      begin
        if Copy(curLine, 1, 2) = '  ' then
          curValue^ := Copy(curLine, 3, Length(curLine))
        else
          curValue^ := curLine;
      end
      else
      begin
        if Copy(curLine, 1, 2) = '  ' then
          curValue^ := curValue^ + ' ' + Copy(curLine, 3, Length(curLine))
        else
          curValue^ := curValue^ + #13#10 + curLine;
      end;
    end;

    Inc(curLineIndex)
  until (curLineIndex >= slist.count) or StrHasPrefix(trim(slist[curLineIndex]), ['@@']);
  // go back one line
  Dec(curLineIndex);
end;

destructor TFileItem.Destroy;
begin

  inherited;
end;

function TFileItem.GetRawText: string;
begin
  Result := '----------------------------------------------------------------------------------------------------'#13#10;
  Result := '@@' + FName + #13#10;
  Result := Result + 'Summary'#13#10;
  Result := Result + '  ' + WrapText(FSummary, #13#10'  ', [' ', #9, '-'], 100) + #13#10;

  if FAuthor <> '' then
  begin
    Result := Result + 'Author'#13#10;
    Result := Result + '  ' + WrapText(FAuthor, #13#10'  ', [' ', #9, '-'], 100) + #13#10;
  end;

  if FDescription <> '' then
  begin
    Result := Result + 'Description'#13#10;
    Result := Result + '  ' + WrapText(FDescription, #13#10'  ', [' ', #9, '-'], 100) + #13#10;
  end;

  if FParameters <> '' then
  begin
    Result := Result + 'Parameters'#13#10;
    Result := Result + '  ' + WrapText(FParameters, #13#10'  ', [' ', #9, '-'], 100) + #13#10;
  end;

  if FReturnValue <> '' then
  begin
    Result := Result + 'Return value'#13#10;
    Result := Result + '  ' + WrapText(FReturnValue, #13#10'  ', [' ', #9, '-'], 100) + #13#10;
  end;

  if FSeeAlsoAsString <> '' then
  begin
    Result := Result + 'See Also'#13#10;
    Result := Result + '  ' + WrapText(FSeeAlsoAsString, #13#10'  ', [' ', #9, '-'], 100) + #13#10;
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
