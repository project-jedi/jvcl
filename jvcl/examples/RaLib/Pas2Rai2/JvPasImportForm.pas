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

unit JvPasImportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TJvPasImport = class(TForm)
    eSource: TEdit;
    bSource: TButton;
    Label1: TLabel;
    eDestination: TEdit;
    Label2: TLabel;
    bDestination: TButton;
    bImport: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    lbClasses: TListBox;
    bReadClasses: TButton;
    bParams: TButton;
    bAddToReg: TButton;
    Label4: TLabel;
    cbClasses: TCheckBox;
    cbFunctions: TCheckBox;
    cbConstants: TCheckBox;
    cbDirectCall: TCheckBox;
    procedure bSourceClick(Sender: TObject);
    procedure bDestinationClick(Sender: TObject);
    procedure bImportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure eSourceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bParamsClick(Sender: TObject);
    procedure bAddToRegClick(Sender: TObject);
  public
  end;

var
  PasImport: TJvPasImport;

implementation

uses
  JvJCLUtils, JvHLParser, JvInterpreter, JvDebugForm, JvRegClassesForm;

{$R *.dfm}

procedure TJvPasImport.bSourceClick(Sender: TObject);
var
  S: string;
begin
  OpenDialog.FileName := eSource.Text;
  if OpenDialog.Execute then
  begin
    eSource.Text := OpenDialog.FileName;
    S := ExtractFileName(eSource.Text);
    if ANSIStrLIComp(PChar(S), 'I_', 2) = 0 then
      Delete(S, 1, 2);
    eDestination.Text := ExtractFilePath(eDestination.Text) +
      'JvInterpreter_' + S;
  end;
end;

procedure TJvPasImport.bDestinationClick(Sender: TObject);
begin
  SaveDialog.FileName := eDestination.Text;
  if SaveDialog.Execute then
    eDestination.Text := SaveDialog.FileName;
end;

procedure TJvPasImport.bImportClick(Sender: TObject);
var
  i: Integer;
  P: Integer;
  Token: string;
  Parser: TJvIParser;
  S: string;
  Output: TStringList;
  Params: TStringList;
  ClassName: string;
  Adapter: TStringList;
  AdapterNames: TStringList;
  RClasses: TStrings;
  Year, Month, Day: Word;

  Name: string; { for all }
  Typ: string; { for functions and properties }
  IndexTyp: string; { for properties }
  IndexDefault: Boolean; { default indexed property }
  PropRead, PropWrite: Boolean; { for properties }
  Decl: string;
  Roll: Integer;
  DirectCall: Boolean;

const
  SetArgs = '(const Value: Variant; Args: TJvInterpreterArgs)';
  GetArgs = '(var Value: Variant; Args: TJvInterpreterArgs)';

  function CT(S: string): Boolean;
  begin
    Result := Cmp(Token, S);
  end;

  procedure Add(S: string);
  begin
    Output.Add(S);
    if DebugLog.cbDebug.Checked then
      DebugLog.memDebug.Lines.Add(S);
  end;

  function NextToken: string;
  begin
    Token := Parser.Token;
    if (Token = '') or CT('implementation') then
      Abort;
    P := Parser.Pos;
    if P mod 100 = 0 then
    try
      ProgressBar1.Position := Parser.Pos;
    except
    end;
    Result := Token;
    if Roll = 0 then
    begin
      if (Token[1] in [';', ':', ',', '(', ')']) or
        (Length(Decl) > 0) and (Decl[Length(Decl)] = '(') then
        Decl := Decl + Token
      else
        Decl := Decl + ' ' + Token;
    end
    else
      Dec(Roll);
  end;

  procedure RollBack(Count: Integer);
  begin
    Parser.RollBack(Count);
    Roll := Count;
  end;

  procedure DeleteAdapterLastLine;
  begin
    if (Adapter.Count > 0) and (Adapter[Adapter.Count - 1] = '') then
      Adapter.Delete(Adapter.Count - 1);
  end;

  function UnitNameStr: string;
  begin
    Result := ChangeFileExt(ExtractFileName(eSource.Text), '');
    if ANSIStrLIComp(PChar(Result), 'I_', 2) = 0 then
      Delete(Result, 1, 2);
  end;

  procedure NextPublicSection;
  begin
    while True do
    begin
      if CT('end') then
        Abort;
      if CT('public') then
        Break;
      NextToken;
    end; { while }
  end;

  procedure ReadParams;
  var
    VarParam: Boolean;
    ParamType: string;
    i, iBeg: Integer;
  begin
    while True do
    begin
      VarParam := False;
      NextToken;
      if Token = ')' then
        Break;
      if CT('var') then
      begin
        VarParam := True;
        NextToken;
      end;
      if CT('const') then
        NextToken;
      iBeg := Params.Count;
      while True do
      begin
        if Token = ';' then
          Break;
        if Token = ')' then
          Exit;
        if Token = ':' then
        begin
          ParamType := NextToken;
          while True do
          begin
            if Token[1] in [')', ';'] then
            begin
              RollBack(1);
              Break;
            end;
            NextToken;
          end;
          Break;
        end;
        if Token <> ',' then
        begin
          // Params.Add(Token + '|' + IntToStr(Integer(VarParam)));
          if VarParam then
            Params.Add('var ' + Token)
          else
            Params.Add(Token);
        end;
        NextToken;
      end;
      for i := iBeg to Params.Count - 1 do
      begin
        Params[i] := Params[i] + ': ' + ParamType;
      end;
    end;
  end;

  function ParamStr: string;
  var
    i: Integer;
  begin
    Result := '';
    if Params.Count = 0 then
      Exit;
    Result := '(';
    for i := 0 to Params.Count - 1 do
    begin
      // Result := Result + SubStr(Params[i], 0, '|');
      if Result <> '(' then
        Result := Result + '; ';
      Result := Result + Params[i]
    end;
    Result := Result + ')';
  end;

  function TypStr(const Typ: string; const RetEmty: Boolean): string;
  begin
    if Cmp(Typ, 'TObject') or (RClasses.IndexOf(Typ) > -1) then
      Result := 'varObject'
    else
    if Cmp(Typ, 'Integer') or Cmp(Typ, 'TColor') then
      Result := 'varInteger'
    else
    if Cmp(Typ, 'Pointer') then
      Result := 'varPointer'
    else
    if Cmp(Typ, 'Word') then
      Result := 'varSmallint'
    else
    if Cmp(Typ, 'Boolean') then
      Result := 'varBoolean'
    else
    if Cmp(Typ, 'String') then
      Result := 'varString'
    else
    if Cmp(Typ, 'Double') then
      Result := 'varDouble'
    else
    if RetEmty then
      Result := 'varEmpty'
    else
      Result := Typ;
  end;

  function ParamTypStr: string;
  var
    i: Integer;
  begin
    if Params.Count = 0 then
    begin
      Result := '[varEmpty]';
      Exit;
    end;
    Result := '[';
    for i := 0 to Params.Count - 1 do
    begin
      // Result := Result + SubStr(Params[i], 0, '|');
      if Result <> '[' then
        Result := Result + ', ';
      Result := Result + TypStr(Trim(SubStr(Params[i], 1, ':')), True);
      if SubStr(Params[i], 0, ' ') = 'var' then
        Result := Result + ' or varByRef';
    end;
    Result := Result + ']';
  end;

  procedure ReadFun;
  begin
    Name := NextToken;
    NextToken;
    Params.Clear;
    if Token = '(' then
    begin
      ReadParams;
      NextToken;
    end;
    if Token = ':' then
    begin
      Typ := NextToken;
      NextToken; { Decl := Decl + ';'}
    end;
  end;

  function ReadProp: Boolean;
  begin
    Result := False;
    Name := NextToken;
    if (Length(Name) > 2) and (Name[1] = 'O') and
      (Name[2] = 'n') and (Name[3] in ['A'..'Z']) then
      { Skip Event Handlers }
      Exit;

    NextToken;
    Params.Clear;
    PropRead := False;
    PropWrite := False;
    IndexTyp := '';
    IndexDefault := False;
    if Token = ';' then
    begin
      { we must reading property info from ancestor }
      { not implemented }
      Exit;
    end;
    if Token <> ':' then
    begin
      if Token <> '[' then
        { something going wrong }
        Exit;
      { indexed property }
      NextToken;
      if NextToken <> ':' then
        { more than one index - not implemented }
        Exit;
      IndexTyp := NextToken;
      if NextToken <> ']' then
        { something going wrong }
        Exit;
      NextToken;
    end;
    Typ := NextToken;
    while True do
    begin
      NextToken;
      if Token = ';' then
      begin
        NextToken;
        if CT('default') then
          IndexDefault := True
        else
          RollBack(1);
        Break;
      end;
      if CT('read') then
        PropRead := True;
      if CT('write') then
        PropWrite := True;
    end;
    Result := True;
  end;

  function V2Param(S: string; ParamType: string): string;
  begin
    Result := S;
    if Cmp(ParamType, 'TObject') then
      Result := 'V2O(' + Result + ')'
    else
    if lbClasses.Items.IndexOf(ParamType) > -1 then
      Result := 'V2O(' + Result + ') as ' + ParamType
    else
    if RClasses.IndexOf(ParamType) > -1 then
      Result := 'V2O(' + Result + ') as ' + ParamType
    else
    if Cmp(ParamType, 'PChar') then
      Result := 'PChar(string(' + Result + '))'
    else
    if Cmp(ParamType, 'Char') then
      Result := 'string(' + Result + ')[1]'
    else
    if Cmp(ParamType, 'Pointer') then
      Result := 'V2P(' + Result + ')'
  end;

  function Result2V(S: string): string;
  var
    ParamType: string;
  begin
    Result := S;
    ParamType := Trim(Typ);
    if Cmp(ParamType, 'TObject') then
      Result := 'O2V(' + S + ')'
    else
    if lbClasses.Items.IndexOf(ParamType) > -1 then
      Result := 'O2V(' + S + ')'
    else
    if RClasses.IndexOf(ParamType) > -1 then
      Result := 'O2V(' + Result + ')'
    else
    if Cmp(ParamType, 'PChar') then
      Result := 'string(' + S + ')'
    else
    if Cmp(ParamType, 'Pointer') then
      Result := 'P2V(' + S + ')'
  end;

  function ResVar: string;
  var
    ParamType: string;
    VType: Integer;
  begin
    ParamType := Trim(Typ);
    VType := TypeName2VarTyp(ParamType);
    case VType of
      varInteger:
        Result := 'varInteger';
      varSmallInt:
        Result := 'varSmallInt';
      varBoolean:
        Result := 'varBoolean';
      varDouble:
        Result := 'varDouble';
      varString:
        Result := 'varString';
      varDate:
        Result := 'varDate';
    else
      if (VType = varObject) or (lbClasses.Items.IndexOf(ParamType) > -1) or
        (RClasses.IndexOf(ParamType) > -1) then
        Result := 'varObject'
      else
        Result := 'varEmpty';
    end;
  end;

  function ConvertParams: string;
  var
    i: Integer;

    function VarCast(S: string): string;
    var
      Typ: string;
    begin
      Result := S;
      if SubStr(Params[i], 0, ' ') <> 'var' then
        Exit;
      Typ := Trim(SubStr(Params[i], 1, ':'));
      if Cmp(Typ, 'integer') then
        Result := 'TVarData(' + Result + ').VInteger'
      else
      if Cmp(Typ, 'smallint') then
        Result := 'TVarData(' + Result + ').VSmallint'
      else
      if Cmp(Typ, 'byte') then
        Result := 'TVarData(' + Result + ').VByte'
      else
      if Cmp(Typ, 'word') then
        Result := 'Word(TVarData(' + Result + ').VSmallint)'
      else
      if Cmp(Typ, 'string') then
        Result := 'string(TVarData(' + Result + ').VString)'
      else
      if Cmp(Typ, 'pointer') then
        Result := 'TVarData(' + Result + ').VPointer'
      else
      if Cmp(Typ, 'double') then
        Result := 'TVarData(' + Result + ').VDouble'
      else
      if Cmp(Typ, 'boolean') then
        Result := 'TVarData(' + Result + ').VBoolean'
      else
      if Cmp(Typ, 'currency') then
        Result := 'TVarData(' + Result + ').VCurrency'
    end;

  begin
    Result := '';
    if Params.Count = 0 then
      Exit;
    Result := '(';
    for i := 0 to Params.Count - 1 do
    begin
      if Result <> '(' then
        Result := Result + ', ';
      Result := Result + VarCast(V2Param('Args.Values[' + IntToStr(i) + ']',
        Trim(SubStr(Params[i], 1, ':'))));
    end;
    Result := Result + ')';
  end;

  procedure AddCons;
  begin
    ReadFun;
    Add('');
    Add('{ constructor ' + Name + ParamStr + ' }');
    Add('');
    Add('procedure ' + ClassName + '_' + Name + GetArgs + ';');
    Add('begin');
    Add('  Value := O2V(' + ClassName + '.' + Name + ConvertParams + ');');
    Add('end;');
    Adapter.Add('    AddGet(' + ClassName + ', ''' + Name + ''', ' +
      ClassName + '_' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr + ', ' + ResVar + ');');
  end;

  procedure AddFun;
  var
    PS, TS: string;
  begin
    ReadFun;
    PS := ParamTypStr;
    TS := TypStr(Typ, True);
    if DirectCall and (Pos('varEmpty', PS) = 0) then
    { direct call }
    begin
      Adapter.Add('    { ' + Decl + ' }');
      Adapter.Add('    AddDGet(' + ClassName + ', ''' + Name + ''', ' +
        '@' + ClassName + '.' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr +
        ', ' + TS + ', ' + ResVar + ', [ccFastCall]);');
    end
    else
    begin
      Add('');
      Add('{ ' + Decl + ' }');
      Add('');
      Add('procedure ' + ClassName + '_' + Name + GetArgs + ';');
      Add('begin');
      Add('  Value := ' + Result2V(ClassName + '(Args.Obj).' + Name + ConvertParams) + ';');
      Add('end;');
      Adapter.Add('    AddGet(' + ClassName + ', ''' + Name + ''', ' +
        ClassName + '_' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr + ', ' + ResVar + ');');
    end;
  end;

  procedure AddProc;
  var
    PS: string;
  begin
    ReadFun;
    PS := ParamTypStr;
    if DirectCall and (Pos('varEmpty', PS) = 0) then
    { direct call }
    begin
      Adapter.Add('    { ' + Decl + ' }');
      Adapter.Add('    AddDGet(' + ClassName + ', ''' + Name + ''', ' +
        '@' + ClassName + '.' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr +
        ', varEmpty, [ccFastCall]);');
    end
    else
    begin
      Add('');
      Add('{ ' + Decl + ' }');
      Add('');
      Add('procedure ' + ClassName + '_' + Name + GetArgs + ';');
      Add('begin');
      Add('  ' + ClassName + '(Args.Obj).' + Name + ConvertParams + ';');
      Add('end;');
      Adapter.Add('    AddGet(' + ClassName + ', ''' + Name + ''', ' +
        ClassName + '_' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr + ', ' + ResVar + ');');
    end;
  end;

  procedure AddFun2;
  var
    S: string;
  begin
    ReadFun;
    Add('');
    Add('{ ' + Decl + ' }');
    Add('');
    Add('procedure ' + 'JvInterpreter_' + Name + GetArgs + ';');
    Add('begin');
    Add('  Value := ' + Result2V(Name + ConvertParams) + ';');
    Add('end;');
    S := UnitNameStr;
    AdapterNames.Add(S);
    Adapter.Add('    AddFunction(c' + UnitNameStr + ', ''' + Name + ''', ' +
      'JvInterpreter_' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr + ', ' + ResVar + ');');
  end;

  procedure AddProc2;
  var
    S: string;
  begin
    ReadFun;
    Add('');
    Add('{ ' + Decl + ' }');
    Add('');
    Add('procedure ' + 'JvInterpreter_' + Name + GetArgs + ';');
    Add('begin');
    Add('  ' + Name + ConvertParams + ';');
    Add('end;');
    S := UnitNameStr;
    AdapterNames.Add(S);
    Adapter.Add('    AddFunction(c' + S + ', ''' + Name + ''', ' +
      ClassName + 'JvInterpreter_' + Name + ', ' + IntToStr(Params.Count) + ', ' + ParamTypStr + ', ' + ResVar + ');');
  end;

  procedure AddProp;
  begin
    if ReadProp then
    begin
      if PropRead then
        if IndexTyp = '' then
        begin
          Add('');
          Add('{ property Read ' + Name + ': ' + Typ + ' }');
          Add('');
          Add('procedure ' + ClassName + '_' + 'Read_' + Name + GetArgs + ';');
          Add('begin');
          Add('  Value := ' + Result2V(ClassName + '(Args.Obj).' + Name + ConvertParams) + ';');
          Add('end;');
          Adapter.Add('    AddGet(' + ClassName + ', ''' + Name + ''', ' +
            ClassName + '_' + 'Read_' + Name + ', 0, [varEmpty], ' + ResVar + ');');
        end
        else
        begin
          Add('');
          Add('{ property Read ' + Name + '[' + IndexTyp + ']: ' + Typ + ' }');
          Add('');
          Add('procedure ' + ClassName + '_' + 'Read_' + Name + GetArgs + ';');
          Add('begin');
          Add('  Value := ' + Result2V(ClassName + '(Args.Obj).' + Name +
            '[Args.Values[0]]' {+ ConvertParams}) + ';');
          Add('end;');
          Adapter.Add('    AddIGet(' + ClassName + ', ''' + Name + ''', ' +
            ClassName + '_' + 'Read_' + Name + ', 1, [varEmpty], ' + ResVar + ');');
          if IndexDefault then
            Adapter.Add('    AddIDGet(' + ClassName + ', ' +
              ClassName + '_' + 'Read_' + Name + ', 1, [varEmpty], ' + ResVar + ');');
        end;
      if PropWrite then
        if IndexTyp = '' then
        begin
          Add('');
          Add('{ property Write ' + Name + '(Value: ' + Typ + ') }');
          Add('');
          Add('procedure ' + ClassName + '_' + 'Write_' + Name + SetArgs + ';');
          Add('begin');
          Add('  ' + ClassName + '(Args.Obj).' + Name + ConvertParams +
            ' := ' + V2Param('Value', Typ) + ';');
          Add('end;');
          Adapter.Add('    AddSet(' + ClassName + ', ''' + Name + ''', ' +
            ClassName + '_' + 'Write_' + Name + ', 0, [' + ResVar + ']);');
        end
        else
        begin
          Add('');
          Add('{ property Write ' + Name + '[' + IndexTyp + ']: ' + Typ + ' }');
          Add('');
          Add('procedure ' + ClassName + '_' + 'Write_' + Name + SetArgs + ';');
          Add('begin');
          Add('  ' + ClassName + '(Args.Obj).' + Name +
            '[Args.Values[0]]' { + ConvertParams} +
            ' := ' + V2Param('Value', Typ) + ';');
          Add('end;');
          Adapter.Add('    AddISet(' + ClassName + ', ''' + Name + ''', ' +
            ClassName + '_' + 'Write_' + Name + ', 0, [varNull]);');
          if IndexDefault then
            Adapter.Add('    AddIDSet(' + ClassName + ', ' +
              ClassName + '_' + 'Write_' + Name + ', 0, [varNull]);');
        end;
    end;
  end;

  procedure ReadSection;
  begin
    while True do
    begin
      if CT('function') then
        AddFun;
      if CT('procedure') then
        AddProc;
      if CT('constructor') then
        AddCons;
      if CT('property') then
        AddProp;
      if CT('end') or CT('private') or CT('protected') then
        Exit;
      Decl := '';
      NextToken;
    end;
  end;

  procedure SkipClass;
  begin
    if Token = ';' then
      Exit;
    if Cmp(NextToken, 'of') then
      Exit;
    if Cmp(Token, 'end') then
      Exit;
    while not Cmp(NextToken, 'end') do
      if Token = '' then
        Exit;
  end;

  function ReadClass: Boolean;
  var
    S: string;
  begin
    Result := False;
    ClassName := Parser.History[3];
    if Token = '(' then
    begin
      while True do
      begin
        NextToken;
        if Token = ')' then
          Break;
      end;
      NextToken;
    end;
    if Sender = bReadClasses then
    begin
      lbClasses.Items.Add(ClassName);
      Exit;
    end;
    if (lbClasses.Items.Count > 0) and
      not lbClasses.Selected[lbClasses.Items.IndexOf(ClassName)] then
    begin
      SkipClass;
      Exit;
    end;
    Add('');
    Add('{ ' + ClassName + ' }');
    DeleteAdapterLastLine;
    Adapter.Add('    { ' + ClassName + ' }');
    S := UnitNameStr;
    AdapterNames.Add(S);
    Adapter.Add('    AddClass(c' + S + ', ' + ClassName +
      ', ' + '''' + ClassName + ''');');
    if Token = ';' then
      Exit;
    Decl := Token;
    try
      while True do
      begin
        ReadSection;
        NextPublicSection;
      end;
    except
      on E: EAbort do
        ;
      else
        raise;
    end;
    Result := True;
  end;

  procedure ReadEnum(SetName: string);
  var
    En: string;
    S: string;
  begin
    Name := SetName;
    DeleteAdapterLastLine;
    Adapter.Add('    { ' + Name + ' }');
    while True do
    begin
      En := NextToken;
      if not (NextToken[1] in [',', ')']) then
        Break;
      S := UnitNameStr;
      AdapterNames.Add(S);
      Adapter.Add('    AddConst(c' + S + ', ''' + En + ''', Ord(' + En + '));');
      if Token = ')' then
        Break;
    end;
    Adapter.Add('');
  end;

begin
  Parser := TJvIParser.Create;
  Output := TStringList.Create;
  Params := TStringList.Create;
  Adapter := TStringList.Create;
  AdapterNames := TStringList.Create;
  AdapterNames.Sorted := True;
  AdapterNames.Duplicates := dupIgnore;
  if Sender = bReadClasses then
    lbClasses.Items.Clear;
  RClasses := RegClasses.memClasses.Lines;
  DirectCall := cbDirectCall.Checked;
  Output.Clear;
  DebugLog.memDebug.Lines.Clear;
  try
    S := LoadTextFile(eSource.Text);
    Parser.pcProgram := PChar(S);
    Parser.pcPos := Parser.pcProgram;
    if ProgressBar1.Max = 0 then
    try
      ProgressBar1.Max := Length(S);
    except
    end;
    ProgressBar1.Visible := True;
    if Sender = bImport then
    begin
      DecodeDate(Now, Year, Month, Day);
      Add('{-----------------------------------------------------------------------------');
      Add('The contents of this file are subject to the Mozilla Public License');
      Add('Version 1.1 (the "License"); you may not use this file except in compliance');
      Add('with the License. You may obtain a copy of the License at');
      Add('http://www.mozilla.org/MPL/MPL-1.1.html');
      Add('');
      Add('Software distributed under the License is distributed on an "AS IS" basis,');
      Add('WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for');
      Add('the specific language governing rights and limitations under the License.');
      Add('');
      Add('The Original Code is: ' + ExtractFileName(eDestination.Text) +
        Format(', generated on %.4d-%.2d-%.2d.', [Year, Month, Day]));
      Add('');
      Add('The Initial Developer of the Original Code is: Andrei Prygounkov <a dott prygounkov att gmx dott de>');
      Add('Copyright (C) ' + Format('%.4d', [Year]) + ' Andrei Prygounkov.');
      Add('All Rights Reserved.');
      Add('');
      Add('Contributor(s):');
      Add('');
      Add('Last Modified:');
      Add('');
      Add('You may retrieve the latest version of this file at the Project JEDI''s JVCL home page,');
      Add('located at http://jvcl.sourceforge.net');
      Add('');
      Add('Description:');
      Add('  adapter unit - converts JvInterpreter calls to Delphi calls');
      Add('');
      Add('Known Issues:');
      Add('  if compiled with errors:');
      Add('   - to convert variant to object use function V2O');
      Add('   - to convert object to variant use function O2V');
      Add('   - to convert variant to pointer use function V2P');
      Add('   - to convert pointer to variant use function P2V');
      Add('   - to convert set to variant use function S2V and');
      Add('     typecasting such as:');
      Add('       Value := S2V(Byte(TFont(Args.Obj).Style))');
      Add('   - to convert variant to set use typecasting');
      Add('     and function V2S such as:');
      Add('       TFont(Args.Obj).Style := TFontStyles(Byte(V2S(Value))) ');
      Add('     depending on size of set (f.e. SizeOf(TFontStyles)),');
      Add('     try to use Byte, Word or Integer types in typecasting');
      Add('   - sets with more than 32 elements cannot be used in JvInterpreter');
      Add('-----------------------------------------------------------------------------}');
      Add('');
      Add('unit ' + ChangeFileExt(ExtractFileName(eDestination.Text), ';'));
      Add('');
      Add('interface');
      Add('');
      Add('uses');
      Add('  JvInterpreter;');
      Add('');
      Add('procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);');
      Add('');
      Add('implementation');
      Add('');
      Add('uses');
      Add('  ' + ChangeFileExt(ExtractFileName(eSource.Text), '') + ';');
    end;
    Roll := 0;
    NextToken;
    try
      while True do
      begin
        if CT('class') then
        begin
          if cbClasses.Checked or (Sender = bReadClasses) then
          begin
            NextToken;
            if (Token <> ';') and (Parser.History[2] = '=') and
              not CT('of') then
              ReadClass;
          end
          else
            SkipClass;
        end
        else
        if CT('interface') and (Parser.History[1] = '=') then
          SkipClass
        else
        if cbFunctions.Checked and (Sender = bImport) then
        begin
          Decl := Token;
          if CT('function') and
            (Parser.History[1] <> '=') and
            (Parser.History[1] <> ':') then
          begin
            AddFun2;
           // Abort;
          end
          else
          if CT('procedure') and
            (Parser.History[1] <> '=') and
            (Parser.History[1] <> ':') then
          begin
            AddProc2;
          //  Abort;
          end;
        end
        else
        if cbConstants.Checked and (Sender = bImport) then
        begin
          if (Token = '(') and (Parser.History[1] = '=') then
            ReadEnum(Parser.History[2])
          else
          if (Token = '(') and Cmp(Parser.History[1], 'of') and
            Cmp(Parser.History[2], 'set') and (Parser.History[3] = '=') then
            ReadEnum(Parser.History[4]);
        end;
        NextToken;
      end;
    except
      on E: EAbort do
        ;
    else
      raise;
    end;
    ProgressBar1.Max := ProgressBar1.Position;
    ProgressBar1.Position := 0;
    ProgressBar1.Visible := False;
    if Sender = bImport then
    begin
      DeleteAdapterLastLine;
      Adapter.Add('  end;');
      Adapter.Add('end;');
      Add('');
      Output.Add('procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);');
      if AdapterNames.Count > 0 then
      begin
        Output.Add('const');
        for I := 0 to AdapterNames.Count-1 do
          Output.Add('  c' + AdapterNames[I] + ' = ''' + AdapterNames[I] + ''';')
      end;
      Output.Add('begin');
      Output.Add('  with JvInterpreterAdapter do');
      Output.Add('  begin');
      Output.AddStrings(Adapter);
      if DebugLog.cbDebug.Checked then
        DebugLog.memDebug.Lines.AddStrings(Adapter);
      Add('');
      Add('end.');
      if (not FileExists(eDestination.Text) or
        (MessageDlg('File ''' + eDestination.Text + ''' already exists. Overwrite ?',
        mtWarning, [mbYes, mbNo, mbCancel], 0) = mrYes)) then
        Output.SaveToFile(eDestination.Text);
    end;
    if Sender = bReadClasses then
    begin
      for i := lbClasses.Items.Count - 1 downto 0 do
        lbClasses.Selected[i] := True;
    end;
  finally
    Parser.Free;
    Params.Free;
    Adapter.Free;
    Output.Free;
    AdapterNames.Free;
  end;
end;

procedure TJvPasImport.FormShow(Sender: TObject);
begin
  DebugLog.Show;
end;

procedure TJvPasImport.eSourceChange(Sender: TObject);
begin
  ProgressBar1.Max := 0;
  lbClasses.Items.Clear;
end;

procedure TJvPasImport.FormCreate(Sender: TObject);
begin
  eSourceChange(nil);
end;

procedure TJvPasImport.bParamsClick(Sender: TObject);
begin
  RegClasses.Show;
end;

procedure TJvPasImport.bAddToRegClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lbClasses.Items.Count - 1 do
    if lbClasses.Selected[i] and
      (RegClasses.memClasses.Lines.IndexOf(lbClasses.Items[i]) = -1) then
      RegClasses.memClasses.Lines.Add(lbClasses.Items[i]);
end;

end.

