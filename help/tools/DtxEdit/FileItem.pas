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

uses JclStrings, SysUtils;

{ TFileItem }

constructor TFileItem.Create(slist: TStringList;
  var curLineIndex: Integer);
var
  curLine : string;
  curValue : ^string;

  function NeedBreak(str : string) : Boolean;
  begin
    Result := (Length(trim(str)) > 0) and
              (((trim(str)[1] = '<') and (str[Length(str)] = '>')) or
               (str[Length(str)] = '.') or
               (str[Length(str)] = ':') or
               (Length(str) < 40) or
               (trim(str) = #13#10) );
  end;
  
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
    else if AnsiCompareText(curLine, 'JVCLInfo') = 0 then
      curValue := @FJVCLInfo
    else if (curValue <> nil) and not StrHasPrefix(curLine, ['----------']) then
    begin
      if Copy(curLine, 1, 2) = '  ' then
        curLine := Copy(curLine, 3, Length(curLine))
      else
        curLine := curLine;

      if curValue^ = '' then
      begin
        if NeedBreak(curLine) then
          curValue^ := curLine + #13#10
        else
          curValue^ := curLine;
      end
      else
      begin
        if NeedBreak(curLine) then
        begin
          if Copy(curValue^, Length(curValue^)-1, 2) = #13#10 then
            curValue^ := curValue^ + curLine + #13#10
          else
            curValue^ := curValue^ + ' ' + curLine + #13#10;
        end
        else
          curValue^ := curValue^ + ' ' + trim(curLine);
      end;
    end;

    if curValue = nil then
      curValue := @FPreText;
      
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
  function FormattedOutput(str : string) : string;
  begin
//    str := TrimRight(str);
    Result := '  ' + WrapText(str, #13#10, [' ', #9, '-'], 100) + #13#10;
    StrReplace(str, #13#10, #13#10'  ', [rfReplaceAll]);
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
