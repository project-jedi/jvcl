{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: VclClxCvt.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit VclClxCvt;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Contnrs, dpp_PascalParser, VclClxCvtUtils;

type
  TParseContext = record
    InImplementation, InInterfaceSection: Boolean;
    LastToken, CurToken: TTokenInfo;
  end;

  { TVCLConverter
    This class converts one file from VCL to CLX.

    If a unit name is found outside a VCL/VisualCLX block the unitname is
    translated by a TranslateUnit() call if it is not in the IgnoreUnits list.
    The TranslateUnit() method's default behaviour is to rename the unit by the
    UnitReplaceList[].

    The class searches for full qualified identifier (Unitname.unitmember), too.

    After the file is parsed it is stored with a filename returned by
    ChangeFileName() to the OutDirectory.
  }
  TVCLConverter = class(TObject)
  private
    FStatistics: TConverterStatistics;

    FIniDirectory: string;
    FKeepLines: Boolean;
    FReduceConditions: Boolean;
    FOutDirectory: string;
    FUnixLineBreak: Boolean;
    FForceOverwrite: Boolean;

    FIncludeFiles: TStringList;
    FUsesUnits: TStringList;
      { contains the units in the "uses" clause (sorted). This is used to find
        contructs like "type TMyType = MyUnit.TMyType;" or "JvJVCLUtils.func" }
    FLockedUsesUnits: TStringList; // if a UsesUnit's name is reused for a function, const, variable, ... (unsorted)
    FConditionStack: TConditionStack; // only valid while in Parse()
    FDefines: TStringList; // only valid while in Parse() (sorted)

    FUnitReplaceList: TUnitReplaceList;
    FIgnoreUnits: TStringList;
    FRemoveConditions: TStringList;
    FConvertProtected: TStringList; // list of Conditions where no unit names should be translated

    FFilename: string;
    FUnixPathDelim: Boolean; // currently parsed file

    procedure SetOutDirectory(const Value: string);
    procedure WriteFile(Lines: TStrings; const Filename: string; AllowBeforeSave: Boolean);

    procedure CheckDfmLine(var Line: string; const Control: string; Controls: TStrings);

    procedure CheckOption(Token: PTokenInfo);
      { Parses the compiler directives and allows the replacement of include
        file names. }
    procedure CheckCondition(Parser: TPascalParser; EndifToken: PTokenInfo);
      { Removes if necessary the condition blocks. }
    procedure CheckUses(Token: PTokenInfo; var Context: TParseContext);
      { Parses the uses-clause and allows the replacement of unit names.

        asn: the following is not required anymore. Required types are added to
             QWindows.

        If it finds a "Windows" in a non-IFDEF'ed area the "Types" unit will be
        added before the replaced "Windows"
        If no "Windows" unit is found the "Types" unit will be inserted before
        the replaced "Graphics" unit.
        "Types" will not be added when it already is in the uses list.
       }
    procedure CheckFileHead(Token: PTokenInfo; var Context: TParseContext);
      { Replaces the "unit", "program", ... name and adds the unit name to the
        UsedUnits list. }
    procedure CheckFunction(Token: PTokenInfo; var Context: TParseContext);
      { Parses procedure/function. }
    procedure CheckFunctionVarDecls(Token: PTokenInfo; var Context: TParseContext);
    function CaseParseContext(Token: PTokenInfo; var Context: TParseContext): Boolean;
    function GetLineBreak: string;
    function CheckFullQualifiedUnitIdentifier(Token: PTokenInfo;
      var Context: TParseContext): Boolean;
    function GetNextToken(Parser: TPascalParser; var Token: PTokenInfo;
      var Context: TParseContext): Boolean;
  protected
    procedure InitUnitReplaceList; virtual;
      { InitUnitReplaceList is called in the constructor after all sub objects
        are created. The function can load the unit replace list, ... }
    procedure TranslateUnit(var AName: string); virtual;
      { The parser calls TranslateInc() when ever a unit name is found in the
        source code. The method can change the unit name. The returned name
        should match the ChangeFileName() returned name for the unit itself. }
    procedure TranslateInc(var AName: string); virtual;
      { The parser calls TranslateInc() when it reaches an $I, $INCLUDE compiler
        directive. The method can change the included file name. }
    procedure TranslateResource(var AName: string); virtual;
      { TranslateResource is call when a {$R is found that is not encapsulated
        by a $IFDEF MSWINDOWS/LINUX $ENDIF. The AName contains all after the
        compiler directive name. }
    function ChangeFileName(const Name: string): string; virtual;
      { ChangeFileName() is called when the parser requires a CLX filename for
        the source code file. It is called twice. The first time when the
        "unit", "program", "library" and "packages" statements are parsed and
        the second time when the file is actually stored. }
    procedure BeforeSave(const Filename: string; Lines: TStrings); virtual;
      { BeforeSave() is called before the file is stored. Here you can modify
        the file lines. Time-dependend lines are not allowed. Filename is the
        CLX filename (changed by ChangeFileName). }
    procedure ChangeDfmLine(var Line: string; const Control: string; Controls: TStrings); virtual;

    procedure Parse(Parser: TPascalParser); virtual;

    function IsUnitIgnored(const AName: string): Boolean; virtual;
    function IsUsesUnit(const AName: string): Boolean;
    procedure ReplaceUnitName(Token: PTokenInfo);
    function IsProtectedByConditions: Boolean; virtual;
  public
    constructor Create(const AIniDirectory: string);
    destructor Destroy; override;

    procedure ParseDfmFile(const Filename: string);
    procedure ParsePasFile(const Filename: string);

    property Statistics: TConverterStatistics read FStatistics;
    property IniDirectory: string read FIniDirectory;
    property OutDirectory: string read FOutDirectory write SetOutDirectory;
      { Directory where the generated file should be stored. }

    property ReduceConditions: Boolean read FReduceConditions write FReduceConditions default True;
      { Removes VCL,COMPILER6,BCB6,BCB condition content and
        VisualCLX conditions. If False If False the ($I jvcl.inc) is replaced by
        ($I qjvcl.inc) }
    property KeepLines: Boolean read FKeepLines write FKeepLines default True;
      { In combination with ReduceConditions this will keep empty lines for the removed lines. }
    property UnitReplaceList: TUnitReplaceList read FUnitReplaceList;
      { Unit -> QUnit names e.g. Controls=QControls}
    property IgnoreUnits: TStringList read FIgnoreUnits;
      { These unit names are not touched. }
    property RemoveConditions: TStringList read FRemoveConditions;
      { All condition names that are in the RemoveConditions list will be swept
        out of the source code if ReduceConditions is True.
        A leading '!' char means the "NOT"-part should be removed. }
    property ConvertProtected: TStringList read FConvertProtected;
      { A list of Conditions where the unit names shouldn't be translated. }

    property UnixLineBreak: Boolean read FUnixLineBreak write FUnixLineBreak default False;
      { If UnixLineBreak is True the written files have #10 as line break else
        it uses #13#10. }
    property UnixPathDelim: Boolean read FUnixPathDelim write FUnixPathDelim default False;
      { Set UnixPathDelim to True if you want the converter to change every '\'
        in the "uses unitname in 'filename.pas'" filename to '/'. }
    property ForceOverwrite: Boolean read FForceOverwrite write FForceOverwrite default False;
      { If ForceOverwrite is True even unchanged files will be rewritten. }

    property Filename: string read FFilename; // currently parsed file
  end;

implementation

uses
  Utils, StrUtils;


{ TVCLConverter }

constructor TVCLConverter.Create(const AIniDirectory: string);
begin
  inherited Create;
  FStatistics := TConverterStatistics.Create;
  FIniDirectory := ExcludeTrailingPathDelimiter(AIniDirectory);

  FKeepLines := True;
  FReduceConditions := True;

  FIncludeFiles := TStringList.Create;
  FIncludeFiles.Sorted := True;
  FIncludeFiles.Duplicates := dupIgnore;

  FUsesUnits := TStringList.Create;
  FUsesUnits.Sorted := True;
  FUsesUnits.Duplicates := dupIgnore;
  // Must not be case sensetive under Linux !

  FLockedUsesUnits := TStringList.Create;
  // Must not be case sensetive under Linux !

  FIgnoreUnits := TStringList.Create;
  FIgnoreUnits.Sorted := True;
  FIgnoreUnits.Duplicates := dupIgnore;
  // Must not be case sensetive under Linux !

  FRemoveConditions := TStringList.Create;
  FRemoveConditions.Sorted := True;
  FRemoveConditions.Duplicates := dupIgnore;

  FUnitReplaceList := TUnitReplaceList.Create;
  FConvertProtected := TStringList.Create;
  InitUnitReplaceList;
end;

destructor TVCLConverter.Destroy;
begin
  FIncludeFiles.Free;
  FUsesUnits.Free;
  FLockedUsesUnits.Free;
  FIgnoreUnits.Free;
  FRemoveConditions.Free;
  FUnitReplaceList.Free;
  FConvertProtected.Free;
  FStatistics.Free;
  inherited Destroy;
end;

procedure TVCLConverter.SetOutDirectory(const Value: string);
begin
  FOutDirectory := ExcludeTrailingPathDelimiter(Value);
end;

function TVCLConverter.GetLineBreak: string;
begin
  if UnixLineBreak then
    Result := #10
  else
    Result := #13#10;
end;

procedure TVCLConverter.InitUnitReplaceList;
var
  Lines: TStrings;
  i: Integer;
  Filename: string;
begin
  Filename := IniDirectory + PathDelim + 'convertvcl.ini';
  if FileExists(Filename) then
    FUnitReplaceList.AddFromIni(Filename);
    
  Lines := TStringList.Create;
  try
    Filename := IniDirectory + PathDelim + 'convertprotected.ini';
    if FileExists(Filename) then
    begin
      Lines.LoadFromFile(Filename);
      for i := 0 to Lines.Count - 1 do
        if not IsEmptyStr(Lines[i]) then
          FConvertProtected.Add(Lines[i]);
    end;

    Filename := IniDirectory + PathDelim + 'ignorevcl.ini';
    if FileExists(Filename) then
    begin
      Lines.LoadFromFile(Filename);
      for i := 0 to Lines.Count - 1 do
        if not IsEmptyStr(Lines[i]) then
          FIgnoreUnits.Add(Lines[i]);
    end;

    Filename := IniDirectory + PathDelim + 'nointextreplace.ini';
    if FileExists(Filename) then
    begin
      Lines.LoadFromFile(Filename);
      for i := 0 to Lines.Count - 1 do
        if not IsEmptyStr(Lines[i]) then
          FLockedUsesUnits.Add(Lines[i]);
    end;
  finally
    Lines.Free;
  end;
end;

procedure TVCLConverter.TranslateUnit(var AName: string);
begin
  AName := UnitReplaceList.Find(AName);
end;

procedure TVCLConverter.TranslateInc(var AName: string);
begin
  // do nothing by default
end;

procedure TVCLConverter.TranslateResource(var AName: string);
begin
  if SameText(AName, '*.DFM') then
    AName := '*.xfm';
end;

procedure TVCLConverter.ParsePasFile(const Filename: string);
var
  Parser: TPascalParser;
  Lines: TStrings;
  FFilepath: string;
begin
  FFilename := Filename;
  FUsesUnits.Clear;
  FIncludeFiles.Clear;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    Parser := TPascalParser.Create(Filename, Lines.Text);
    try
      Lines.Clear; // reduce memory usage

      Parse(Parser);
      Statistics.IncParsedFiles; {statistic}

      Lines.Text := Parser.Text;
      FFilepath := FOutDirectory + PathDelim + ChangeFileName(ExtractFileName(Filename));
      WriteFile(Lines, FFilePath,True);
    finally
      Parser.Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TVCLConverter.WriteFile(Lines: TStrings; const Filename: string; AllowBeforeSave: Boolean);
var
  i: Integer;
  sb: IStringBuilder;
  lb, S, OldFileContent: string;
begin
  if AllowBeforeSave then
    BeforeSave(Filename, Lines);
  
  lb := GetLineBreak;
  sb := StringBuilder('');
  for i := 0 to Lines.Count - 1 do
  begin
    sb.Append(Lines[i]);
    sb.Append(lb);
  end;
  sb.GetValue(S);
  sb := nil;

  if not ForceOverwrite and FileExists(Filename) then
  begin
    ReadFileToString(Filename, OldFileContent);
    if OldFileContent = S then
      Exit; // file content is the same
  end;
  Statistics.IncWrittenFiles; {statistic}
  WriteFileFromString(Filename, S);
end;

function TVCLConverter.ChangeFileName(const Name: string): string;
begin
  if SameText(ExtractFileExt(Name), '.dfm') then
    Result := 'Q' + ChangeFileExt(Name, '.xfm')
  else
    Result := 'Q' + Name;
end;

function TVCLConverter.IsUnitIgnored(const AName: string): Boolean;
var
  Index, i: Integer;
begin
  Result := FIgnoreUnits.Find(AName, Index);
  if not Result then
  begin
    for i := FIncludeFiles.Count - 1 downto 0 do
    begin
      Result := FIgnoreUnits.Find(FIncludeFiles[i] + '::' + AName, Index);
      if Result then
        Break;
    end;
  end;
end;

function TVCLConverter.IsUsesUnit(const AName: string): Boolean;
var
  Index: Integer;
begin
  Result := FUsesUnits.Find(AName, Index);
  if Result then
    Result := FLockedUsesUnits.IndexOf(AName) < 0;
end;

procedure TVCLConverter.ReplaceUnitName(Token: PTokenInfo);
var
  UnitName: string;
begin
  UnitName := Token.Value;
  TranslateUnit(UnitName);
  if UnitName <> Token.Value then
  begin
    FStatistics.IncUnitReplacements;
    Token.Parser.ReplaceParseNext(Token, Token, UnitName);
  end;
end;

function TVCLConverter.CheckFullQualifiedUnitIdentifier(Token: PTokenInfo;
  var Context: TParseContext): Boolean;
var
  ParserIndex: Integer;
  Parser: TPascalParser;
  Tk: TTokenInfo;
begin
  Result := False;
  with Context do
  begin
    if (((Context.LastToken.Kind = tkSymbol) and (Context.LastToken.Value <> '.')) or
        (Context.LastToken.Kind <> tkSymbol)) and
       not IsProtectedByConditions and IsUsesUnit(Token.Value) then
    begin
      // "UnitName.xxx" but not ".Unitname.xxx"
      Tk := Token^;
      Parser := Token.Parser;
      ParserIndex := Parser.Index;
      if GetNextToken(Parser, Token, Context) and (Token.Kind = tkSymbol) and (Token.Value = '.') then
      begin
        if not IsUnitIgnored(Tk.Value) then
          ReplaceUnitName(@Tk);
        Result := True;
      end
      else
        Parser.Index := ParserIndex;
    end;
  end;
end;

function TVCLConverter.CaseParseContext(Token: PTokenInfo; var Context: TParseContext): Boolean;
var
  S: string;
begin
  Result := False;
  with Context do
  begin
    case Token.Kind of
      tkIdent:
        begin
          S := Token.Value;
          if InImplementation and
             (SameText(S, 'procedure') or
              SameText(S, 'function') or
              SameText(S, 'constructor') or
              SameText(S, 'destructor')) then
          begin
            CheckFunction(Token, Context);
            Result := True;
          end
          else
            Result := CheckFullQualifiedUnitIdentifier(Token, Context);
        end;
    end;
  end;
end;

procedure TVCLConverter.Parse(Parser: TPascalParser);
var
  Token: PTokenInfo;
  Context: TParseContext;
  S: string;
begin
  FConditionStack := nil;
  FDefines := nil;
  try
    FConditionStack := TConditionStack.Create;
    FDefines := TStringList.Create;
    FDefines.Sorted := True;
    FDefines.Duplicates := dupIgnore;

    with Context do
    begin
      FillChar(Context, SizeOf(Context), 0);
      InImplementation := False;
      InInterfaceSection := False;
      while GetNextToken(Parser, Token, Context) do
      begin
        case Token.Kind of
          tkIdent:
            begin
              if not CaseParseContext(Token, Context) then
              begin
                S := Token.Value;
                if SameText(S, 'uses') then
                  CheckUses(Token, Context)
                else
                if (not InInterfaceSection) and (not InImplementation) and
                   SameText(S, 'interface') then
                  InInterfaceSection := True
                else
                if (not InImplementation) and
                   (SameText(S, 'unit') or
                    SameText(S, 'program') or
                    SameText(S, 'package') or
                    SameText(S, 'library')) then
                begin
                  CheckFileHead(Token, Context);
                end
                else
                if SameText(S, 'implementation') then
                begin
                  InImplementation := True;
                  InInterfaceSection := False;
                end;
              end;
            end
        else
          CaseParseContext(Token, Context);
        end;
      end;
    end;
  finally
    FreeAndNil(FConditionStack);
  end;
end;

procedure TVCLConverter.CheckOption(Token: PTokenInfo);
  // handles the compiler directives
var
  Condition, S: string;
  IncFilename, OrgIncFilename: string;
  ResourceName, OrgResource: string;
  Index: Integer;
begin
  S := RemoveCommentChars(Token.Value);
  if AnsiStartsText('$I ', S) or AnsiStartsText('$INCLUDE ', S) then
  begin
    if AnsiStartsText('$I ', S) then
      IncFilename := TrimCopy(S, 4, MaxInt)
    else
      IncFilename := TrimCopy(S, 9, MaxInt);
    FIncludeFiles.Add(IncFilename);
    OrgIncFilename := IncFilename;
    TranslateInc(IncFilename);
    if IncFilename <> OrgIncFilename then
    begin
      S := StringReplace(Token.Value, OrgIncFilename, IncFilename, []);
      Token.Parser.ReplaceParseNext(Token, Token, S);
    end;
  end
  else
  begin
    if AnsiStartsText('$DEFINE ', S) then
      FDefines.Add(TrimCopy(S, 9, MaxInt))
    else
    if AnsiStartsText('$UNDEF ', S) then
    begin
      if FDefines.Find(TrimCopy(S, 8, MaxInt), Index) then
        FDefines.Delete(Index);
    end
    else
    if AnsiStartsText('$IFDEF ', S) then
    begin
      Condition := TrimCopy(S, 8, MaxInt);
      FConditionStack.Enter(Condition, Token.StartIndex, Token.EndIndex, Token.StartLine);
    end
    else
    if AnsiStartsText('$IFNDEF ', S) then
    begin
      Condition := TrimCopy(S, 9, MaxInt);
      FConditionStack.EnterNot(Condition, Token.StartIndex, Token.EndIndex, Token.StartLine);
    end
    else
    if AnsiStartsText('$ELSE', S) then // $ELSEIF ???
    begin
      FConditionStack.GoElse(Token.StartIndex, Token.EndIndex, Token.StartLine);
    end
    else
    if AnsiStartsText('$ENDIF', S) then
    begin
      CheckCondition(Token.Parser, Token); // accesses FConditionStack.Current
      FConditionStack.Leave;
    end
    else
    if AnsiStartsText('$R ', S) or AnsiStartsText('$RESOURCE ', S) then
    begin
      if ((FConditionStack.IsIn('LINUX') = 0) and
         (FConditionStack.IsIn('MSWINDOWS') = 0))
         or
         (SameText(S, '$R *.DFM')) then
      begin
        if SameText(S, '$R *.DFM') then
        begin
          if IsProtectedByConditions then
            Exit; // forced by condition block
        end;

        if AnsiStartsText('$R ', S) then
          ResourceName := TrimCopy(S, 4, MaxInt)
        else
          ResourceName := TrimCopy(S, 11, MaxInt);
        OrgResource := ResourceName;
        TranslateResource(ResourceName);
        if ResourceName <> OrgResource then
        begin
          S := StringReplace(Token.Value, OrgResource, ResourceName, []);
          Token.Parser.ReplaceParseNext(Token, Token, S);
        end;
      end;
    end;
  end;
end;

procedure TVCLConverter.CheckCondition(Parser: TPascalParser; EndifToken: PTokenInfo);
var
  Cond: TConditionStackItem;

  function LineClean(Index: Integer): Integer; // after LineClean the tokens are invalidt
  var
    StartIndex: Integer;
  begin
    Result := 0;
    StartIndex := Index;
    while Index > 0 do
    begin
      case Parser.Text[Index] of
        #0..#9: ;
        #10: // we read backward
          begin
            if Parser.Text[Index - 1] = #13 then
              Dec(Index);
            Break;
          end;
        #11, #12: ;
        #13:
            Break;
        #14..#32: ;
      else
        Exit;
      end;
      Dec(Index);
    end;
    Result := StartIndex - Index;
    Parser.Delete(Index, Result);
    Parser.Index := Index;
  end;

  procedure Remove(RemoveContent: Boolean);
  var
    S: string;
    ParserIndex: Integer;
  begin
    ParserIndex := Parser.Index;
    if not Cond.HasElse then
    begin
      if not RemoveContent then
      begin
        // remove $ENDIF before $IFDEF
        Dec(ParserIndex, EndifToken.EndIndex - EndifToken.StartIndex + 1);
        Parser.Replace(EndifToken, EndifToken, '');
        if not KeepLines then
          Dec(ParserIndex, LineClean(EndifToken.StartIndex - 1));
        Dec(ParserIndex, Cond.OpenEndIndex - Cond.OpenStartIndex + 1);
        Parser.ReplaceParseNext(Cond.OpenStartIndex, Cond.OpenEndIndex - Cond.OpenStartIndex + 1, '');
      end
      else
      begin
        if KeepLines then
          S := RepeatStr(GetLineBreak, EndifToken.EndLine - Cond.OpenLine)
        else
          S := '';
        Dec(ParserIndex, EndifToken.EndIndex - Cond.OpenStartIndex + 1);
        Parser.ReplaceParseNext(Cond.OpenStartIndex, EndifToken.EndIndex - Cond.OpenStartIndex + 1, S);
        Inc(ParserIndex, Length(S));
      end;
    end
    else
    begin
      if not RemoveContent then
      begin
        // remove $ENDIF before $IFDEF
        if KeepLines then
          S := RepeatStr(GetLineBreak, EndifToken.EndLine - Cond.ElseLine)
        else
          S := '';
        Dec(ParserIndex, EndifToken.EndIndex - Cond.ElseStartIndex + 1);
        Parser.ReplaceParseNext(Cond.ElseStartIndex, EndifToken.EndIndex - Cond.ElseStartIndex + 1, S);
        Inc(ParserIndex, Length(S));
        if not KeepLines then
          Dec(ParserIndex, LineClean(Cond.ElseStartIndex - 1));

        Dec(ParserIndex, Cond.OpenEndIndex - Cond.OpenStartIndex + 1);
        Parser.ReplaceParseNext(Cond.OpenStartIndex, Cond.OpenEndIndex - Cond.OpenStartIndex + 1, '');
      end
      else
      begin
        // remove $ENDIF before $IFDEF
        if KeepLines then
          S := RepeatStr(GetLineBreak, Cond.ElseLine - Cond.OpenLine)
        else
          S := '';
        Dec(ParserIndex, EndifToken.EndIndex - EndifToken.StartIndex + 1);
        Parser.Replace(EndifToken, EndifToken, '');
        if not KeepLines then
          Dec(ParserIndex, LineClean(EndifToken.StartIndex - 1));

        Dec(ParserIndex, Cond.ElseEndIndex - Cond.OpenStartIndex + 1);
        Parser.ReplaceParseNext(Cond.OpenStartIndex, Cond.ElseEndIndex - Cond.OpenStartIndex + 1, S);
        Inc(ParserIndex, Length(S));
      end;
    end;

    if not KeepLines then
      Dec(ParserIndex, LineClean(Parser.Index - 1));

    // restore next token start index  
    Parser.Index := ParserIndex;
  end;

var
  Index: Integer;
begin
  if not ReduceConditions then
    Exit; // do nothing here

  Cond := FConditionStack.Current;
  if Cond = nil then
  begin
    FStatistics.AddError('No IFDEF/IFNDEF open.');
    Exit;
  end;

  if FRemoveConditions.Find(Cond.Condition, Index) then  // "Condition"
    Remove(not Cond.Negative)
  else
  if FRemoveConditions.Find('!' + Cond.Condition, Index) then // "!Condition"
    Remove(Cond.Negative);
end;

procedure TVCLConverter.CheckUses(Token: PTokenInfo; var Context: TParseContext);
var
  Parser: TPascalParser;
  StartConditionStackCount: Integer;
//  InsertTypesUnitStartIndex: Integer;
  i: Integer;
  Changed: Boolean;
begin
//  InsertTypesUnitStartIndex := -1;
  StartConditionStackCount := FConditionStack.OpenCount;
  Parser := Token.Parser;
  while GetNextToken(Parser, Token, Context) do
  begin
    case Token.Kind of
      tkSymbol:
        if (Token.Value = ';') and (StartConditionStackCount <= FConditionStack.OpenCount) then
          Break; // finished
      tkIdent:
        begin
          if SameText(Token.Value, 'in') and UnixPathDelim then // uses unitname in 'filename.pas';
          begin
            if GetNextToken(Parser, Token, Context) then
            begin
              { Replace '\' by '/' in the filename }
              if Token.Kind = tkString then
              begin
                Changed := False;
                for i := 1 to Length(Token.Value) do
                  if Token.Value[i] = '\' then
                  begin
                    Token.Value[i] := '/';
                    Changed := True;
                  end;
                if Changed then
                  Parser.ReplaceParseNext(Token, Token, Token.Value);
              end;
            end;
            Continue;
          end;
          if SameText(Token.Value, 'type') or SameText(Token.Value, 'const') or
             SameText(Token.Value, 'resourcestring') or SameText(Token.Value, 'var') or
             SameText(Token.Value, 'function') or SameText(Token.Value, 'procedure') or
             SameText(Token.Value, 'implementation') or SameText(Token.Value, 'begin') then
          begin
            FStatistics.AddError('Wrong condition blocks in ' + Token.Parser.Filename);
            Parser.Index := Token.StartIndex; // reparse this token
            Break; // there is something wrong with the Condition-Blocks.
          end;

          FUsesUnits.Add(Token.Value);
          if not IsProtectedByConditions then
          begin
            // replace unit names, because we are outside a VCL/VisualCLX condition
            if not IsUnitIgnored(Token.Value) then
            begin
              {  asn: not required anymore.
              if SameText(Token.Value, 'Windows') then
                InsertTypesUnitStartIndex := Token.StartIndex;
              if (InsertTypesUnitStartIndex = -1) and SameText(Token.Value, 'Graphics') then
                InsertTypesUnitStartIndex := Token.StartIndex;
              }
              ReplaceUnitName(Token);
            end;
          end;
        end;
    end;
  end;
  {
  if (InsertTypesUnitStartIndex > 0) and Context.InInterfaceSection and
     (FUsesUnits.IndexOf('Types') = -1) then
  begin
    Parser.Insert(InsertTypesUnitStartIndex, 'Types, ');
    Parser.IndexNoClear := Parser.Index + 7;
  end;
  }
end;

procedure TVCLConverter.CheckFileHead(Token: PTokenInfo; var Context: TParseContext);
var
  Parser: TPascalParser;
  NewFilename, Filename, Ext: string;
begin
  if SameText(Token.Value, 'unit') then
    Ext := '.pas'
  else if SameText(Token.Value, 'package') then
    Ext := '.dpk'
  else
    Ext := '.dpr';

  Filename := '';
  Parser := Token.Parser;
  while GetNextToken(Parser, Token, Context) do
  begin
    if Token.Kind = tkIdent then
    begin
      // unit/program/library/package name
      if Filename = '' then // only the first identifier is the unit name, others are syntax errors
      begin
        FUsesUnits.Add(Token.Value);
        Filename := Token.Value + Ext;
        NewFilename := ChangeFileName(Filename);
        if NewFilename <> Filename then
        begin
          Filename := ChangeFileExt(ExtractFileName(NewFilename), '');
          Parser.ReplaceParseNext(Token, Token, Filename);
        end;
      end;
    end
    else
    if (Token.Kind = tkSymbol) and (Token.Value = ';') then
      Break; // finished
  end;
end;

procedure TVCLConverter.CheckFunction(Token: PTokenInfo; var Context: TParseContext);
var
  Parser: TPascalParser;
  LockedUnitStartCount: Integer;
  BeginBlockCount: Integer;
  InParams: Boolean;
  LastTokenValue: string;
begin
  Parser := Token.Parser;
  LockedUnitStartCount := FLockedUsesUnits.Count;
  try
   // procedure/function header
    InParams := False;
    while GetNextToken(Parser, Token, Context) do
    begin
      if not InParams then
      begin
        if Token.Kind = tkSymbol then
        begin
          if Token.Value = ';' then
            InParams := True; // no parameters
          if Token.Value = '(' then
            InParams := True;
        end;
      end
      else
      begin
        case Token.Kind of
          tkIdent:
            begin
              if (LastTokenValue <> ':') and IsUsesUnit(Token.Value) then
                FLockedUsesUnits.Add(Token.Value) // this unit name is redeclared as parameter
              else
              begin
                CaseParseContext(Token, Context);
                if SameText(Token.Value, 'external') or
                   SameText(Token.Value, 'forward') then
                  Exit; // this is only a procedure head
                if SameText(Token.Value, 'begin') or
                   SameText(Token.Value, 'var') or
                   SameText(Token.Value, 'const') or
                   SameText(Token.Value, 'type') or
                   SameText(Token.Value, 'resourcestring') then
                  Break;
                if SameText(Token.Value, 'end') then
                begin
                  FStatistics.AddError('"end" found but "begin", "var", "const", "type" or "resourcestring" expected.');
                  Exit; // something very strange happend
                end;
              end;
            end;
        else
          CaseParseContext(Token, Context);
        end;
      end;
      LastTokenValue := Token.Value;
    end;

    if Token = nil then
      Exit;

    if not SameText(Token.Value, 'begin') then
      CheckFunctionVarDecls(Token, Context);

    BeginBlockCount := 1;
    while GetNextToken(Parser, Token, Context) do
    begin
      if Token.Kind = tkIdent then
      begin
        if SameText(Token.Value, 'begin') then
          Inc(BeginBlockCount)
        else
        if SameText(Token.Value, 'end') then
        begin
          Dec(BeginBlockCount);
          if BeginBlockCount = 0 then
            Break; // function end
        end;
      end;
      CaseParseContext(Token, Context);
    end;
  finally
    // we leave the function so remove the locked local "unit name" variables
    while FLockedUsesUnits.Count > LockedUnitStartCount do
      FLockedUsesUnits.Delete(FLockedUsesUnits.Count - 1);
  end;
end;

procedure TVCLConverter.CheckFunctionVarDecls(Token: PTokenInfo;
  var Context: TParseContext);
var
  Parser: TPascalParser;
  LastTokenValue: string;
begin
  Parser := Token.Parser;
  while GetNextToken(Parser, Token, Context) do
  begin
    case Token.Kind of
      tkIdent:
        begin
          if (LastTokenValue <> ':') and IsUsesUnit(Token.Value) then
            FLockedUsesUnits.Add(Token.Value) // this unit name is redeclared as variable/const/resstring
          else
          begin
            if not CaseParseContext(Token, Context) then // meight find records, ...
            begin
              if SameText(Token.Value, 'begin') then
                Break;
              if SameText(Token.Value, 'end') then
              begin
                FStatistics.AddError('"end" found but "begin", "var", "const", "type" or "resourcestring" expected.');
                Exit; // something very strange happend
              end;
            end;
          end;
        end;
    else
      CaseParseContext(Token, Context);
    end;
    LastTokenValue := Token.Value;
  end;
end;

procedure TVCLConverter.BeforeSave(const Filename: string; Lines: TStrings);
begin
  // do nothing
end;

{
  Calls CheckDfmLine with parameter Control=
   * ClassName e.g. 'TListView'
   * ClassName.Property for Collections e.g: 'TListView.Columns'
   * ClassName.Property:item for Collection items: e.g. 'TListView.Columns:item'
}
procedure TVCLConverter.ParseDfmFile(const Filename: string);
var
  Lines: TStrings;
  i, ps: Integer;
  S, TrimS: string;
  Controls: TStringList;
begin
  Controls := TStringList.Create;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    if Lines.Count > 0 then
    begin
      if (Lines[0] <> '') and (Lines[0][1] < #32) or (Lines[0][2] < #32) then
      begin
        FStatistics.AddError(ExtractFileName(Filename) + ' is binary. Converting to text.');
        ConvertBinDfmToText(Filename);
        Lines.LoadFromFile(Filename);
      end;

      i := 0;
      while i < Lines.Count do
      begin
        S := Lines[i];
        TrimS := Trim(S);
        if TrimS <> '' then
        begin
          if TrimS = 'DesignSize = (' then
          begin
            Lines.Delete(i);
            Lines.Delete(i);
            Lines.Delete(i);
            Continue;
          end
          else
          begin
            if AnsiStartsText('object ', TrimS) then
            begin
              ps := Pos(':', TrimS);
              if ps > 0 then
                Controls.Add(Trim(Copy(TrimS, ps + 1, MaxInt)));
            end
            else
            if SameText(TrimS, 'end') and (Controls.Count > 0) then
              Controls.Delete(Controls.Count - 1)
            else
            if Controls.Count > 0 then
            begin
              if AnsiEndsText('= <', TrimS) then
              begin
                // collection
                Controls.Add(Controls[Controls.Count - 1] + '.' + Trim(Copy(TrimS, 1, Pos('=', TrimS) - 1)));
              end
              else
              if SameText(TrimS, 'end>') then
              begin
                Controls.Delete(Controls.Count - 1);
                Controls.Delete(Controls.Count - 1);
              end
              else if SameText(TrimS, 'item') then
                Controls.Add(Controls[Controls.Count - 1] + ':item')
              else
                CheckDfmLine(S, Controls[Controls.Count - 1], Controls);
              Lines[i] := S;
            end;
          end;
        end;
        Inc(i);
      end;

      WriteFile(Lines,
        FOutDirectory + PathDelim + ChangeFileName(ExtractFileName(Filename)),
        False);
    end
    else
      FStatistics.AddError(ExtractFileName(Filename) + ' is empty.');
  finally
    Lines.Free;
    Controls.Free;
  end;
end;

procedure TVCLConverter.CheckDfmLine(var Line: string; const Control: string; Controls: TStrings);
var
  S, OrgS: string;
begin
  Line := TrimRight(Line);
  if Line <> '' then
  begin
    S := TrimLeft(Line);
    OrgS := S;
    ChangeDfmLine(S, Control, Controls);
    if S <> OrgS then
      Line := StringReplace(Line, OrgS, S, []);
  end;
end;

procedure TVCLConverter.ChangeDfmLine(var Line: string; const Control: string; Controls: TStrings);
begin
  if (Controls.Count = 1) and AnsiStartsText('BorderStyle = ', Line) then
    Line := StringReplace(Line, ' bs', ' fbs', [rfIgnoreCase]);
  if AnsiStartsText('Ctl3D = ', Line) or
     AnsiStartsText('ParentCtl3D = ', Line) then
    Line := '';
  if AnsiStartsText('IsControl = True', Line) or
     AnsiStartsText('PageSize = 0', Line) or
     AnsiStartsText('DefaultMonitor = ', Line) or
     AnsiStartsText('RightClickSelect = True', Line) then
    Line := '';
  if (Control = 'TProgressBar') and AnsiStartsText('TabOrder = ', Line) then
    Line := '';
  if (Control = 'TComboBox') and (AnsiStartsText('AutoDropDown = ', Line) or 
                                  AnsiStartsText('AutoCloseUp = ', Line)) then
    Line := '';
  if (Control = 'TListView') then
  begin
    if AnsiStartsText('SmallImages = ', Line) then
      Line := StringReplace(Line, 'SmallImages = ', 'Images = ', [rfIgnoreCase])
    else if AnsiStartsText('OnCompare = ', Line) then
      Line := ''
    else if AnsiStartsText('SortType = ', Line) then
    begin
      if Pos('= stNone', Line) = 0 then
        Line := 'Sorted = True'
      else
        Line := ''
    end;
  end;
end;

function TVCLConverter.IsProtectedByConditions: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to ConvertProtected.Count - 1 do
    if FConditionStack.IsIn(ConvertProtected[i]) <> 0 then
      Exit;
  Result := False;
end;

function TVCLConverter.GetNextToken(Parser: TPascalParser;
  var Token: PTokenInfo; var Context: TParseContext): Boolean;
begin
  Context.LastToken := Context.CurToken;
  while Parser.GetToken(Token) and (Token.Kind = tkComment) do
  begin
    if Token.ExKind = tekOption then
      CheckOption(Token);
  end;
  Result := Token <> nil;
  if Result then
    Context.CurToken := Token^;
end;

end.
