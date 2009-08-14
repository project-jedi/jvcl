unit WinHelpPostProcessor;

interface

uses
  JVCLHelpUtils,
  Classes;

type
  TWinHelpPostProcessor = class(TTask)
  private
    FWinHelpDir: string;
    FDoxFileName: string;
    function GetRTFFileName: string;
    function GetHPJFileName: string;

    function DetermineEnd(SS: TStrings; const OpenChar, CloseChar: Char;
      Count: Integer;
      const StartLineIndex, StartIndex: Integer; out EndLineIndex, EndIndex: Integer): Boolean;
    procedure RemoveLinks(SS: TStrings; const StartLineIndex, StartIndex, EndLineIndex, EndIndex: Integer);
  protected
    function ProcessHPJFile: BOolean;
    function ProcessRTFFile: BOolean;

    function ChangeJEDIJVCLLinks(SS: TStrings): Boolean;
    function ChangeEditLinks(SS: TStrings): Boolean;
    function ChangeClassHierarchie(SS: TStrings): Boolean;
    function ChangeFootnotes(SS: TStrings): Boolean;
    function ChangeTitles(SS: TStrings): Boolean;
    function RemoveLinksFromCode(SS: TStrings): Boolean;

    function RemoveTopicID(Strings: TStrings; StartIndex: Integer;
      const ATopicID: string; out AShellExecuteStr: string): Boolean;

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property WinHelpDir: string read FWinHelpDir write FWinHelpDir;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property RTFFileName: string read GetRTFFileName;
    property HPJFileName: string read GetHPJFileName;
  end;

implementation

uses
  SysUtils, JclStrings;

//=== { TWinHelpPostProcessor } ==============================================

function TWinHelpPostProcessor.CanStart: Boolean;
begin
  Result := FileExists(RTFFileName) and FileExists(HPJFileName);
end;

function EscapeStr(const S: string; const Ch: Char): string;
var
  P, Q: PChar;
  ChCount: Integer;
begin
  ChCount := 0;
  P := PChar(S);
  while P^ <> #0 do
  begin
    if P^ = '\' then
    begin
      Inc(P);
      if P^ = #0 then
        Break;
    end
    else
      if P^ = Ch then
      Inc(ChCount);
    Inc(P);
  end;
  if ChCount = 0 then
    Result := S
  else
  begin
    SetLength(Result, Length(S) + ChCount);

    P := PChar(S);
    Q := PChar(Result);
    while P^ <> #0 do
    begin
      if P^ = '\' then
      begin
        Q^ := P^;
        Inc(Q);
        Inc(P);
        if P^ = #0 then
          Break;
      end
      else
        if P^ = Ch then
      begin
        Q^ := '\';
        Inc(Q);
      end;
      Q^ := P^;
      Inc(Q);
      Inc(P);
    end;
  end;
end;

function DetermineCompleteText(const S, SubString: string; out CompleteText: string): Boolean;
var
  StartIndex, EndIndex: Integer;
begin
  StartIndex := StrIPos(SubString, S);
  Result := StartIndex >= 1;
  if not Result then
    Exit;

  EndIndex := StartIndex + Length(SubString);

  while (EndIndex <= Length(S)) and (S[EndIndex] <> ')') do
    Inc(EndIndex);
  Result := EndIndex <= Length(S);
  if not Result then
    Exit;

  CompleteText := Copy(S, StartIndex, EndIndex + 1 - StartIndex);
end;

function DetermineCompleteLink(const S, SubString: string; out CompleteLink: string; out TopicID: string): Boolean;
var
  StartIndex, EndIndex: Integer;
begin
  StartIndex := StrIPos(SubString, S);
  Result := StartIndex >= 1;
  if not Result then
    Exit;

  EndIndex := StartIndex + Length(SubString);

  while (EndIndex <= Length(S)) and (S[EndIndex] <> '>') do
    Inc(EndIndex);
  Result := EndIndex <= Length(S);
  if not Result then
    Exit;

  TopicID := Copy(S, StartIndex + Length(SubString) - 3,
    EndIndex + 1 - StartIndex - Length(SubString) + 2);

  while (EndIndex <= Length(S)) and (S[EndIndex] <> '}') do
    Inc(EndIndex);
  Result := EndIndex <= Length(S);
  if not Result then
    Exit;

  CompleteLink := Copy(S, StartIndex, EndIndex + 1 - StartIndex);
end;

function TWinHelpPostProcessor.ChangeClassHierarchie(
  SS: TStrings): Boolean;
const
  cSubString = '{\ul class hierarchy}'; // lowercase
  cNewString = '{\ul Hierarchy}';
var
  I: Integer;
begin
  Result := True;

  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    Progress(I, SS.Count);

    SS[I] := ChangeSubString(SS[I], cSubString, cNewString);

    I := LocateSubString(SS, I, cSubString);
  end;
end;

function TWinHelpPostProcessor.ChangeEditLinks(SS: TStrings): Boolean;
const
  //  cSubString = '{\ul editlink}{\v id_';
  cSubString = '{\uldb editlink}{\v id_';
  cShellExecuteStr =
    '{\ul Edit topic}{\v !ShellExecute("http://help.delphi-jedi.org/item.php?Name=%s", "" , NORMAL, "open", "", 900)}';
var
  S: string;
  I: Integer;
  CompleteLink, TopicID, ShellExecuteStr: string;
begin
  Result := True;
  // Change
  //    {\ul EditLink}{\v id_192}
  // to
  //    {\v !ShellExecute("http://help.delphi-jedi.org/item.php?Name=TJvDockableForm.DockableControl", "" , NORMAL, "open", "", 900)}

  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    Progress(I, SS.Count);

    S := SS[i];
    Result := DetermineCompleteLink(S, cSubString, CompleteLink, TopicID);
    if not Result then
    begin
      ErrorMsgFmt('Could not determine complete link at line %d', [i]);
      Exit;
    end;

    Result := RemoveTopicID(SS, I, TopicID, ShellExecuteStr);
    if not Result then
    begin
      ErrorMsgFmt('Could not locate or delete topic ID %s', [TopicID]);
      Exit;
    end;

    //      Result := DetermineTopicName(S, TopicName);
    //      if not Result then
    //      begin
    //        ErrorMsgFmt('Could not determine topic name at line %d', [i]);
    //        Exit;
    //      end;

    SS[I] := ChangeSubString(SS[I], CompleteLink, ShellExecuteStr);

    I := LocateSubString(SS, I, cSubString);
  end;
end;

function TWinHelpPostProcessor.ChangeFootnotes(SS: TStrings): Boolean;
const
  cSubString = '{\footnote';
var
  I: Integer;
  EscapedCompleteLink, CompleteLink: string;
  TopicID: string;
  S: string;
begin
  Result := True;
  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    Progress(I, SS.Count);

    S := SS[i];

    Result := DetermineCompleteLink(S, cSubString, CompleteLink, TopicID);
    if not Result then
    begin
      ErrorMsgFmt('Could not determine complete link at line %d', [i]);
      Exit;
    end;

    EscapedCompleteLink := EscapeStr(CompleteLink, ',');

    SS[I] := ChangeSubString(SS[I], CompleteLink, EscapedCompleteLink);

    I := LocateSubString(SS, I, cSubString);
  end;
end;

function TWinHelpPostProcessor.ChangeJEDIJVCLLinks(SS: TStrings): Boolean;
const
  cSubString = '{\uldb jedi-vcl}{\v id_';
  cNewText = '{\pard\plain \ql\fi0\li0\ri0\sb30\sa0\f0\fs16\cf1\b {\uldb JEDI-VCL Reference}{\v id_1>MAIN}}\par \pard';

  function IncludeTab(const S: string; var Link: string): Boolean;
  var
    StartIndex, EndIndex: Integer;
    Sub: string;
  begin
    StartIndex := StrIPos(Link, S);
    Result := StartIndex >= 1;
    if not Result then
      Exit;

    EndIndex := StartIndex + Length(Link);

    while (EndIndex <= Length(S)) and (S[EndIndex] = ' ') do
      Inc(EndIndex);
    if EndIndex <= Length(S) then
    begin
      Sub := Copy(S, EndIndex, 4);
      if SameText(Sub, '\tab') then
        Inc(EndIndex, 3)
      else
        Dec(EndIndex);
    end;
    Link := Copy(S, StartIndex, EndIndex + 1 - StartIndex);
  end;
var
  I: Integer;
  S: string;
  CompleteLink, TopicID: string;
begin
  // \{bmc C:\\Comp\\dev\\help\\images\\comp\\TJvXPStyleManager.bmp\} TJvXPStyleManager \pard\plain \keepn\ql\fi0\li0\ri0\sb30\sa0\f0\fs16\cf1 \par \tx0\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\tx10080\tx11520\tx12960{\ul Class Hierarchy}{\v id_453}\tab {\uldb Properties}{\v id_454>NAV}\tab {\uldb Methods}{\v id_455>NAV}\tab {\uldb Events}{\v id_456>NAV}\tab {\ul Package}{\v id_457}\tab {\uldb JEDI-VCL}{\v id_1>MAIN}\tab {\ul Edit topic}{\v !ShellExecute("http://help.delphi-jedi.org/item.php?Name=TJvXPStyleManager", "" , NORMAL, "open", "", 900)}
  //
  // to
  //
  // {\pard\plain \ql\fi0\li0\ri0\sb30\sa0\f0\fs16\cf1\b {\uldb JEDI-VCL Reference}{\v id_1>MAIN}}
  // \par \pard\{bmc C:\\Comp\\dev\\help\\images\\comp\\TJvXPCheckbox.bmp\} TJvXPCheckbox \pard\plain \keepn\ql\fi0\li0\ri0\sb30\sa0\f0\fs16\cf1 \par \tx0\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\tx10080\tx11520{\ul Class Hierarchy}{\v id_213}\tab {\uldb Properties}{\v id_214>NAV}\tab {\uldb Events}{\v id_215>NAV}\tab {\ul Package}{\v id_216}\tab {\ul Edit topic}{\v !ShellExecute("http://help.delphi-jedi.org/item.php?Name=TJvXPCheckbox", "" , NORMAL, "open", "", 900)}

  // : \tab {\uldb JEDI-VCL}{\v id_1>MAIN}

  Result := True;
  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    Progress(I, SS.Count);

    S := SS[i];

    Result := DetermineCompleteLink(S, cSubString, CompleteLink, TopicID);
    if not Result then
    begin
      ErrorMsgFmt('Could not determine complete link at line %d', [i]);
      Exit;
    end;

    Result := IncludeTab(S, CompleteLink);
    if not Result then
    begin
      ErrorMsgFmt('Could not include tab at line %d', [i]);
      Exit;
    end;

    SS[I] := ChangeSubString(SS[I], CompleteLink, '');

    SS.Insert(I, cNewText);

    I := LocateSubString(SS, I, cSubString);
  end;
end;

function StripTitle(const S: string): string;
var
  P, Q: PChar;
begin
  // xxyy(TJvBalloonHint.CustomAnimationStyle) ->
  // (TJvBalloonHint)

  P := PChar(S);
  while not (P^ in [#0, '(']) do
    Inc(P);

  Q := P;

  while not (P^ in [#0, '.']) do
    Inc(P);
  if P^ <> #0 then
    Inc(P);
  SetString(Result, Q, P - Q);

  if LEngth(Result) >= 1 then
    Result[Length(Result)] := ')';
end;

function TWinHelpPostProcessor.ChangeTitles(SS: TStrings): Boolean;
const
  cSubString = 'xxyy';
var
  I: Integer;
  StrippedCompleteText, CompleteText: string;
  S: string;
begin
  Result := True;
  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    Progress(I, SS.Count);

    S := SS[i];

    Result := DetermineCompleteText(S, cSubString, CompleteText);
    if not Result then
    begin
      ErrorMsgFmt('Could not determine complete text at line %d', [i]);
      Exit;
    end;

    StrippedCompleteText := StripTitle(CompleteText);

    SS[I] := ChangeSubString(SS[I], CompleteText, StrippedCompleteText);

    I := LocateSubString(SS, I, cSubString);
  end;
end;

function TWinHelpPostProcessor.DetermineEnd(SS: TStrings; const OpenChar,
  CloseChar: Char; Count: Integer; const StartLineIndex, StartIndex: Integer;
  out EndLineIndex, EndIndex: Integer): Boolean;
var
  P: PChar;
  LineIndex: Integer;
  LastWasEscape: Boolean;
begin
  LineIndex := StartLineIndex;
  LastWasEscape := False;

  P := PChar(SS[LineIndex]) + StartIndex - 1;

  while LineIndex < SS.Count do
  begin
    while P^ <> #0 do
    begin
      if not LastWasEscape then
      begin
        if P^ = OpenChar then
          Inc(Count)
        else
          if P^ = CloseChar then
        begin
          Dec(Count);
          if Count <= 0 then
          begin
            // done
            Result := True;
            EndLineIndex := LineIndex;
            EndIndex := P - PChar(SS[LineIndex]) + 1;
            Exit;
          end;
        end;
      end;

      LastWasEscape := not LastWasEscape and (P^ = '\');
      Inc(P);
    end;

    Inc(LineIndex);
    P := PChar(SS[LineIndex]);
  end;

  Result := False;
end;

function TWinHelpPostProcessor.DoExecute: Boolean;
begin
  Result := ProcessHPJFile;
  if not Result then
    Exit;

  Result := ProcessRTFFile;
  if not Result then
    Exit;
end;

function TWinHelpPostProcessor.GetHPJFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(WinHelpDir) +
    ChangeFileExt(ExtractFileName(DoxFileName), '.hpj');
end;

function TWinHelpPostProcessor.GetRTFFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(WinHelpDir) +
    ChangeFileExt(ExtractFileName(DoxFileName), '.rtf');
end;

function TWinHelpPostProcessor.GetTaskDescription: string;
begin
  Result := 'Post processing Windows Help files';
end;

function TWinHelpPostProcessor.ProcessHPJFile: BOolean;
var
  SS: TStringList;
  I, J: Integer;
begin
  StatusMsgFmt('Processing %s', [HPJFileName]);

  SS := TStringList.Create;
  try
    StatusMsg('Loading..');
    SS.LoadFromFile(HPJFileName);

    I := LocateString(SS, 0, '[WINDOWS]');
    Result := I >= 0;
    if not Result then
    begin
      ErrorMsg('Could not find [WINDOWS]');
      Exit;
    end;

    J := LocatePrefix(SS, I, 'MAIN=');
    Result := J >= 0;
    if not Result then
    begin
      ErrorMsg('Could not find MAIN in Windows section');
      Exit;
    end;
    SS[J] := ChangeSubString(SS[J], '27908', '28932');

    J := LocatePrefix(SS, I, 'NAV=');
    Result := J >= 0;
    if not Result then
    begin
      ErrorMsg('Could not find NAV in Windows section');
      Exit;
    end;
    SS[J] := ChangeSubString(SS[j], '19460', '4');

    if LocateString(SS, 0, 'BrowseButtons()') < 0 then
    begin
      SS.Add('[CONFIG]');
      SS.Add('BrowseButtons()');
    end
    else
      WarningMsg('Already has BrowseButtons() macro');

    StatusMsg('Saving..');
    SS.SaveToFile(HPJFileName);
  finally
    SS.Free;
  end;
end;

function TWinHelpPostProcessor.ProcessRTFFile: BOolean;
const
  //  cSubString = '{\ul editlink}{\v id_';
  cSubString = '{\uldb editlink}{\v id_';
  cShellExecuteStr =
    '{\ul Edit topic}{\v !ShellExecute("http://help.delphi-jedi.org/item.php?Name=%s", "" , NORMAL, "open", "", 900)}';
var
  SS: TStringList;
begin
  StatusMsgFmt('Processing %s', [RTFFileName]);

  SS := TStringList.Create;
  try
    StatusMsg('Loading..');
    SS.LoadFromFile(RTFFileName);

    StatusMsg('Processing..');
        Result := ChangeJEDIJVCLLinks(SS);
        if not Result then
          Exit;
    
        Result := ChangeEditLinks(SS);
        if not Result then
          Exit;
    
        Result := ChangeClassHierarchie(SS);
        if not Result then
          Exit;
    
   
        Result := ChangeTitles(SS);
        if not Result then
          Exit;

    Result := RemoveLinksFromCode(SS);
    if not Result then
      Exit;

    StatusMsg('Saving..');
    SS.SaveToFile(RTFFileName);
  finally
    SS.Free;
  end;
end;

procedure DetermineLinks(const S: string; const StartIndex: Integer;
  out CompleteLink, StrippedLink: string);
const
  cLinkLength = Length('{\uldb ');
var
  EndIndex: Integer;
  Count: Integer;
  L: Integer;
begin
  EndIndex := StartIndex;
  L := Length(S);
  Count := 2;
  while EndIndex <= L do
  begin
    if S[EndIndex] = '}' then
    begin
      Dec(Count);
      if Count = 1 then
        SetString(StrippedLink, PChar(S) + StartIndex + cLinkLength - 1, EndIndex - StartIndex - cLinkLength)
      else
        if Count = 0 then
      begin
        SetString(CompleteLink, PChar(S) + StartIndex - 1, EndIndex - StartIndex + 1);
        Exit;
      end;
    end;
    Inc(EndIndex);
  end;
  raise Exception.Create('DetermineLink: Count <> 0');
end;

//function StrippedLink(const S: string): string;
//begin
//  Result := '';
//end;

function RemoveLink(const S: string): string;
var
  StartIndex: Integer;
  CompleteLink, StrippedLink: string;
begin
  Result := S;
  StartIndex := StrIPos('{\uldb ', Result);
  while StartIndex > 0 do
  begin
    DetermineLinks(Result, StartIndex, CompleteLink, StrippedLink);
    Result := ChangeSubString(Result, CompleteLink, StrippedLink);

    StartIndex := StrIPos('{\uldb ', Result);
  end;
end;

procedure TWinHelpPostProcessor.RemoveLinks(SS: TStrings;
  const StartLineIndex, StartIndex, EndLineIndex,
  EndIndex: Integer);
var
  I: Integer;
  S: string;
begin
  try
    S := SS[StartLineIndex];
    SS[StartLineIndex] := Copy(S, 1, StartIndex - 1) + RemoveLink(Copy(S, StartIndex, MaxInt));

    for I := StartLineIndex + 1 to EndLineIndex - 1 do
      SS[i] := RemoveLink(SS[i]);

    if EndLineIndex > StartLineIndex then
    begin
      S := SS[EndLineIndex];
      SS[EndLineIndex] := RemoveLink(Copy(S, 1, EndIndex)) + Copy(S, EndIndex + 1, MaxInt);
    end;
  except
    ErrorMsgFmt('Index=%d', [StartLineIndex]);
    raise;
  end;
end;

function TWinHelpPostProcessor.RemoveLinksFromCode(SS: TStrings): Boolean;
const
  cSubString = '\par {\pard\plain \keep\ql\fi0\li0\ri0\sb0\sa0\f1\fs18\cf1';
var
  I: Integer;
  S: string;

  StartLineIndex, EndLineIndex: Integer;
  StartIndex, EndIndex: Integer;
begin
  Result := True;
  I := LocateSubString(SS, 0, cSubString);
  while I >= 0 do
  begin
    Progress(I, SS.Count);

    S := SS[i];

    StartLineIndex := I;
    StartIndex := StrIPos(cSubString, S) + Length(cSubString);

    Result := DetermineEnd(SS, '{', '}', 1, StartLineIndex, StartIndex, EndLineIndex, EndIndex);
    if not Result then
    begin
      ErrorMsgFmt('Could not determine end at line %d', [i]);
      Exit;
    end;

    RemoveLinks(SS, StartLineIndex, StartIndex, EndLineIndex, EndIndex);

    { !! +1 is eigelijk fout }
    I := LocateSubString(SS, EndLineIndex + 1, cSubString);
  end;
end;

function TWinHelpPostProcessor.RemoveTopicID(Strings: TStrings;
  StartIndex: Integer; const ATopicID: string;
  out AShellExecuteStr: string): Boolean;

  function DetermineTopicName(P: PChar; out TopicName: string): Boolean;
  const
    cNum = ['0'..'9'];
    cAlpha = ['a'..'z', 'A'..'Z'];
    cNumAlphaChars = cNum + cAlpha + ['.'];
  var
    Q: PChar;
  begin
    Q := P;
    while not (Q^ in cNumAlphaChars) do
      Inc(Q);
    P := Q;
    while P^ in cNumAlphaChars do
      Inc(P);

    Result := P > Q;
    if not Result then
      Exit;

    SetString(TopicName, Q, P - Q);
  end;
const
  cShellExecuteStr =
    '{\ul Edit topic}{\v !ShellExecute("http://help.delphi-jedi.org/item.php?Name=%s", "" , NORMAL, "open", "", 900)}';
  cSearchStr = '!shellexecute("http://help.delphi-jedi.org/item.php?name=';
var
  EndIndex: Integer;
  ShellExecuteIndex: Integer;

  TopicName: string;
  I: Integer;
  S: string;
  AStartIndex: Integer;
begin
  // DOM 4
  AStartIndex := LocateSubString(Strings, StartIndex, Format('{\footnote # %s}', [ATopicID]));
  if AStartIndex < 0 then
    // DOM 3
    AStartIndex := LocateSubString(Strings, StartIndex, Format('{\footnote %s}', [ATopicID]));
  StartIndex := AStartIndex;

  Result := StartIndex >= 0;
  if not Result then
    Exit;

  EndIndex := LocateSubString(Strings, StartIndex, '\page');
  Result := EndIndex >= 0;
  if not Result then
    Exit;

  ShellExecuteIndex := LocateSubString(Strings, StartIndex, cSearchStr);
  Result := (ShellExecuteIndex >= StartIndex) and (ShellExecuteIndex <= EndIndex);
  if not Result then
    Exit;

  S := Strings[ShellExecuteIndex];

  I := StrIPos(cSearchStr, S);
  Result := I >= 1;
  if not Result then
    Exit;

  Result := DetermineTopicName(PChar(S) + I - 1 + Length(cSearchStr), TopicName);
  if not Result then
    Exit;

  AShellExecuteStr := Format(cShellExecuteStr, [TopicName]);

  S := Strings[EndIndex];
  I := StrIPos('\page', S);
  Result := I >= 1;
  if not Result then
    Exit;

  Delete(S, 1, I + 4);
  if IsNullStr(S) then
    Strings.Delete(EndIndex)
  else
    Strings[EndIndex] := S;
  for I := EndIndex - 1 downto StartIndex do
    Strings.Delete(I);
end;

end.
