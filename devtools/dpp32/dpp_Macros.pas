{**************************************************************************************************}
{                                                                                                  }
{ Delphi language Preprocessor (dpp32)                                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is dpp_Macros.pas                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Projects home page, located at           }
{ http://www.sourceforge.net/projects/dpp32                                                        }
{                                                                                                  }
{**************************************************************************************************}
unit dpp_Macros;
(*******************************************************************************
* Preprocessor for Delphi and Kylix compiler (dcc32, dcc)
* =======================================================
*
* This preprocessor make it possible for Delphi developers to use the power of
* macros in Object Pascal code. The macros acts like the C/C++ preprocessor
* macros with the exception of conditional compilation.
*
*
* Macro Syntax:
* {$define macroname}          // registers a macro
* {$undef macroname}           // unregisters a the macro
* {MACROINCLUDE mymacros.mac}  // does only include the macros from the file
*
* Examples:
* {$define macroname1   replacement text }
* {$define macroname2(arg1, arg2) WriteLn(#arg1); ReadLn(arg2 ## _Var) }
* {$undef macroname2}
*
*
*
* The macros in the interface sections of the USES units are not imported. For
* macro imports use MACROINCLUDE.
*
* For every unit in the uses-statement a MACROINCLUDE is automatically generated
* e.g. uses MyUnit; -> MyUnit.macros is included if it exists. For the current
* unit also the corresponding .macros file is also included. The .macros file
* must exist in the same directory as the unit itself and it cannot contain any
* source code. Only macro declarations ($define, $undef, MACROINCLUDE) are
* allowed. Other code is ignored.
*
* The preprocessor follows the compiler directive {$I filename} and
* {$INCLUDE filename}. For every included file a copy of the preprocessed files
* is generated to support differences in macro defined before the include
* directive.
*
*
* ISSUES:
*   - The compiler has a line limit of 1024 chars. For larger macros a unique
*     include file can be necessary. Splitting the macro in more lines will
*     break the error message lines. Prehaps the preprocessor can modify the
*     original file by inserting "{MACROPLACE}" comment lines after the long
*     line.
*
*   - No conditional expressions (Delphi 6+) are supported by the preprocessor.
*     All macros in such an expression are parsed and replaced. In order to
*     support this feature an expression parser is necessary. Furthermore
*     we must collect all *const* expressions. But how can we get the consts
*     from a compiled unit (.dcu).
*
*******************************************************************************)
{.$define HASHTABLE}
interface

uses
  Types, SysUtils, Classes, Contnrs, dpp_PascalParser, dpp_Utils;

const
  MaxFileRecursion = 30; // files can be opened at once

type
  TWarningEvent = procedure(Sender: TObject; const Filename, Msg: string;
    LineNum: Integer) of object;
  TErrorEvent = procedure(Sender: TObject; const Filename, Msg: string;
    LineNum: Integer) of object;
  TPredefineMacrosEvent = procedure(Sender: TObject) of object;
  TDefaultConditionalsEvent = procedure(Sender: TObject) of object;
  TBuiltInMacroEvent = function(Sender: TObject; Token: PTokenInfo;
    var Replacement: string; var IsBuiltIn: Boolean): string of object;

  TMacroCompare = function(const S1, S2: string): Integer;

  TParseType = (
    ptUnit,            // collect and replace macros in the whole file
    ptInclude,         // collect and replace macros in the whole file but use some special file handling
    ptInterfaceMacros  // collect macros from the interface-section of the file (only for MACROINCLUDE)
  );

 { TPascalParserEx make it easier to pass NoReplaceMacros across the
   TMacro.methods. }
  TPascalParserEx = class(TPascalParser)
  public
    NoReplaceMacros: Boolean;
  end;

  TMacros = class;
  TMacroList = class;

  IMacroFileSys = interface
  ['{F3CD3F56-F849-4C9E-BD57-3D76DE6E0C64}']
    { Called before the file is read. The file exists. }
    procedure BeforeFile(const FileName: string; IsIncludeFile: Boolean);
    { Called after the file was stored depending on Modified. Filename is a
      full qualified file name. The file and the new file exist. }
    procedure AfterFile(const FileName, NewFileName: string; IsIncludeFile,
      Modified: Boolean);
    { LoadFile must return the file content in *Content*. Filename is a
      full qualified file name. The file exists. }
    procedure LoadFile(const Filename: string; out Content: string;
      IsIncludeFile: Boolean);
    { SaveFile is called for saving the file's content. Filename is the original
      filename and NewFilename is the preprocessor's new file name. All file
      names are full qualified file names. The file exists but the new file
      doesn't. }
    procedure SaveFile(const Filename: string; var NewFilename: string;
      const Content: string; IsIncludeFile: Boolean);
    { FindFile is called if the file name is not a full qualified file
      name. Return the full qualified file name or '' if the file does not
      exist. }
    function FindFile(const Filename: string; IsIncludeFile: Boolean): string;
    { FileExists must return True if the given File exists. Filename is a
      full qualified file name. }
    function FileExists(const Filename: string): Boolean;

    { LinesMoved is called for macro replacements using more than one line.
      LineNum   : line where the macro is.
      AddedLines: number of inserted lines }
    procedure LinesMoved(const Filename: string; LineNum, AddedLines: Integer);
  end;

  TMacroItem = class(TObject)
  private
    FMacroList: TMacroList;
    FName: string;
    FReplacement: string;
    FHasBrackets: Boolean;
    FArguments: TStringDynArray;
    FInterfaceMacro: Boolean; // if TRUE the $ifdef/$ifndef must be modified
  public
    constructor Create(AMacroList: TMacroList);
    function Parse(const MacroNameArgReplacement: string; out ErrorMsg: string;
      AInterfaceMacro: Boolean): Boolean;
    function IsEqual(Item: TMacroItem): Boolean;
    procedure Assign(Item: TMacroItem);
    function IndexOfArg(const ArgName: string): Integer;

    property Name: string read FName;
    property Replacement: string read FReplacement;
    property HasBrackets: Boolean read FHasBrackets write FHasBrackets;
    property Arguments: TStringDynArray read FArguments;
    property MacroList: TMacroList read FMacroList;
    property InterfaceMacro: Boolean read FInterfaceMacro write FInterfaceMacro;
  end;

  TMacroList = class(TObjectList)
  private
    FMacros: TMacros;
    FHashTable: TRedirectTable;
    function GetItems(Index: Integer): TMacroItem;
  protected
    function IndexOfMacro(const Name: string): Integer;
  public
    constructor Create(Macros: TMacros);
    procedure Assign(MacroList: TMacroList);
    procedure Clear; override;

    function RegisterMacro(const Macro: string; AInterfaceMacro: Boolean): TMacroItem;  // in: 'test(x) x*x' // DEVINFO: Macro must be TrimLeft()
    procedure UnregisterMacro(const Name: string);            // in: 'test'        // DEVINFO: Macro must be Trim()
    function IsMacroRegistered(const Name: string): Boolean;                       // DEVINFO: Macro must be Trim()

    function FindMacro(const Name: string): TMacroItem;

    property Items[Index: Integer]: TMacroItem read GetItems;
  end;

  TMacros = class(TMacroList)
  private
    FCaseSensitive: Boolean;
    FConditionalParse: Boolean;
    FErrorMsg: string;

    FUnits: TStrings;        // contains all units (interface and implementation <uses>); "MacroFileExists:=Boolean(Objects[])"
    FIncludeFiles: TStrings; // contains all include files. Integer(Objects[]): how often the file is used

    FMacroMacroRecursion: TList; // used for in macro replacement see TMacros.ReplaceMacro()
    FFileRecursion: Integer;  // number of open include files

    FConditionals: TStrings; // $define, $ifdef, $ifndef - conditional compilation
    FCompilerOptions: TStrings; // $ifopt - conditional compilation
    FConditionalParseCode: TBooleanList; // .LastItem=True: parse code; .LastItem=False: ignore code and macros

    FAppType: string;
    FFileSys: IMacroFileSys;

    FCompare: TMacroCompare;
    FOnError: TErrorEvent;
    FOnWarning: TWarningEvent;
    FOnPredefineMacros: TPredefineMacrosEvent;
    FOnBuiltInMacro: TBuiltInMacroEvent;
    FOnDefaultConditionals: TDefaultConditionalsEvent;

    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Warning(const Msg, FileName: string; LineNum: Integer); overload;
    procedure Warning(const Msg: string; Token: PTokenInfo); overload;
    procedure Error(const Msg, FileName: string; LineNum: Integer); overload;
    procedure Error(const Msg: string; Token: PTokenInfo); overload;
    procedure PredefineMacros;
    function BuiltInMacro(Token: PTokenInfo; var Replacement: string): Boolean;
    procedure DefaultConditionals;

    function ParseUnitMacroFile(UnitIndex: Integer): Integer;
    function ParseFile(Filename: string; ParseType: TParseType;
      TestFileExistence: Boolean): string; // returns new filename (.i.pas, .i1.*, .i2.*, ...)
    function ParseString(var Text: string; const Filename: string;
      StartLineNum: Integer; ParseType: TParseType): Boolean;

    function NextToken(Parser: TPascalParserEx; out Token: PTokenInfo): Boolean; overload;
    function NextToken(Parser: TPascalParserEx): PTokenInfo; overload;
    function ParseConditionals(var Line: string; const Filename: string;
      StartLineNum: Integer): Boolean;
    function ParseComment(Token: PTokenInfo): Boolean;
    procedure ParseUsesIdent(Parser: TPascalParserEx);

    function GetReplacement(Item: TMacroItem; const Args: TStringDynArray;
      const Filename: string; StartLineNum: Integer): string;
    procedure ReplaceMacro(Parser: TPascalParserEx; Item: TMacroItem);

    function RegisterMacroByToken(const Macro: string; Token: PTokenInfo): TMacroItem;
  public
    constructor Create(AFileSys: IMacroFileSys);
    destructor Destroy; override;

    procedure Define(const Condition: string); // defines a condition (FConditionals)
    procedure Undefine(const Condition: string); // undefines a condition (FConditionals)
    procedure SetOption(const Option: string; Value: Boolean);
    function IsDefined(const Condition: string): Boolean; // return TRUE if the condition is defined (FConditionals)

    function Parse(const FileName: string; OnlyThisFile: Boolean): Boolean;

    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default True;
    property ConditionalParse: Boolean read FConditionalParse write FConditionalParse default False;
    property ErrorMsg: string read FErrorMsg;

    property OnWarning: TWarningEvent read FOnWarning write FOnWarning;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnPredefineMacros: TPredefineMacrosEvent read FOnPredefineMacros write FOnPredefineMacros;
    property OnDefaultConditionals: TDefaultConditionalsEvent read FOnDefaultConditionals write FOnDefaultConditionals;
    property OnBuiltInMacro: TBuiltInMacroEvent read FOnBuiltInMacro write FOnBuiltInMacro;
  end;

implementation

resourcestring
 // error messages
  SMacroArgumentsSyntaxError = 'No valid macro name/argument: %s';
  SMacroNotEnoughArguments = 'Not enough arguments for %s';
  SMacroSytaxError = 'Syntax error: %s';
  SEmptyMacroArgument = 'Macro argument is empty.';
  SNoFurtherToken = 'Unexpected file/macro end.';
  SCanOnlyMakeStringFromArguments = 'Can only use # on macro arguments.';
  SCombineError = 'Wrong usage for ##.';
  SNoArgumentSpecified = 'No argument specified for macro.';
  SMacroRedefinitionNotIdentical = 'Redeclaration of "%s" is not identical.';
  SToManyRecursions = 'To many file recursions.';
  SFindFile = 'File "%s" not found.';
  SConditionalSyntaxError = 'Syntax error in conditional directive.';

const
  SMacroStartString = '$DEFINE ';
  SUnmacroStartString = '$UNDEF ';
  SMacroIncludeString = 'MACROINCLUDE ';
  SMacroIncludeFileExt = '.macros';

  SBuiltInStartChars = '__'; // do not modify or localize 
  SBuiltIn_Line = '__LINE__';
  SBuiltIn_File = '__FILE__';
  SBuiltIn_Date = '__DATE__';
  SBuiltIn_Time = '__TIME__';

{ TMacroItem }

constructor TMacroItem.Create(AMacroList: TMacroList);
begin
  inherited Create;
  FMacroList := AMacroList;
end;

procedure TMacroItem.Assign(Item: TMacroItem);
var i: Integer;
begin
  FHasBrackets := Item.FHasBrackets;
  SetLength(FArguments, Length(Item.FArguments));
  FReplacement := Item.FReplacement;
  for i := 0 to High(FArguments) do
    FArguments[i] := Item.FArguments[i];
end;

{ IndexOfArg() returns the index of the macro argument with the name ArgName. }
function TMacroItem.IndexOfArg(const ArgName: string): Integer;
var cmp: TMacroCompare;
begin
  cmp := FMacroList.FMacros.FCompare;
  for Result := 0 to High(Arguments) do
    if cmp(ArgName, Arguments[Result]) = 0 then Exit;
  Result := -1;
end;

function TMacroItem.IsEqual(Item: TMacroItem): Boolean;
var
  i: Integer;
  cmp: TMacroCompare;
begin
  Result := False;
  if FHasBrackets <> Item.FHasBrackets then Exit;
  if Length(FArguments) <> Length(Item.FArguments) then Exit;
  if FReplacement <> Item.FReplacement then Exit;

  cmp := FMacroList.FMacros.FCompare;
  for i := 0 to High(FArguments) do
    if cmp(FArguments[i], Item.FArguments[i]) <> 0 then Exit;

  Result := True;
end;

procedure SetTrimString(out S: string; P: PChar; Count: Integer);
var
  F: PChar;
begin
  while {(P[0] <> #0) and }(P[0] <= #32) and (Count > 0) do
  begin
    Inc(P);
    Dec(Count);
  end;
  if Count > 0 then
  begin
    F := P;
    Inc(P, Count);
    while (P > F) and (P[0] <= #32) do Dec(P);
    SetString(S, F, P - F);
  end
  else
    S := '';
end;

{ Parse() parses the macro declaration and split it to its name, arguments and
  replacement. }
function TMacroItem.Parse(const MacroNameArgReplacement: string; out ErrorMsg: string;
  AInterfaceMacro: Boolean): Boolean;
var
  F, P: PChar;
  ArgCount: Integer;
  i: Integer;
begin
  FInterfaceMacro := AInterfaceMacro;
  FHasBrackets := False;

  Result := False;

 // parse Macro name string
  F := Pointer(MacroNameArgReplacement);
  if F = nil then Exit;
  P := F;
  while not (P[0] in [#0, '(', #9, #10, #13, ' ']) do Inc(P);
  SetString(FName, F, P - F);
 // test for valid identifier
  if not IsValidIdent(FName) then
  begin
    ErrorMsg := Format(SMacroArgumentsSyntaxError, [FName]);
    Exit;
  end;

 // parse Macro arguments
  if P[0] = '(' then
  begin
    FHasBrackets := True;

    F := P + 1;
    ArgCount := CountCharsStop(',', ')', F) + 1;
    SetLength(FArguments, ArgCount);

    for i := 0 to ArgCount - 1 do
    begin
      while not (P[0] in [#0, ',', ')']) do Inc(P);
      SetTrimString(FArguments[i], F, P - F);
{      SetString(FArguments[i], F, P - F);
      FArguments[i] := Trim(FArguments[i]); // trim it}
      Inc(P); // next char
      F := P;

     // test for valid identifier
      if not IsValidIdent(FArguments[i]) then
      begin
        ErrorMsg := Format(SMacroArgumentsSyntaxError, [FArguments[i]]);
        Exit;
      end;
    end;

   // only one argument which is empty -> free memory
    if (ArgCount = 1) and (Length(FArguments[0]) = 0) then
      SetLength(FArguments, 0);
    if P[0] = ')' then Inc(P);
  end;

  Result := True;
  
  if P[0] = #0 then Exit;
  while (P[0] <> #0) and (P[0] <= #32) do Inc(P);
  FReplacement := TrimRight(P);
end;

{ TMacroList }

constructor TMacroList.Create(Macros: TMacros);
begin
  inherited Create;
  FMacros := Macros;
end;

procedure TMacroList.Assign(MacroList: TMacroList);
var
  i: Integer;
  Item: TMacroItem;
begin
  Clear;
  for i := 0 to MacroList.Count - 1 do
  begin
    Item := TMacroItem.Create(Self);
    Item.Assign(MacroList.Items[i]);
    Add(Item);
    MakeStringHash(Item.Name, Integer(Item), FHashTable);
  end;
end;

function TMacroList.GetItems(Index: Integer): TMacroItem;
begin
  Result := TMacroItem(inherited Items[Index]);
end;

function TMacroList.IndexOfMacro(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if FMacros.FCompare(TMacroItem(inherited Items[Result]).Name, Name) = 0 then
      Exit;
  Result := -1;
end;

function TMacroList.FindMacro(const Name: string): TMacroItem;
{$ifndef HASHTABLE}
var
  i: Integer;
  cmp: TMacroCompare;
{$endif}
begin
{$ifdef HASHTABLE}
  Result := TMacroItem(FindStringHash(Name, FHashTable, FMacros.FCaseSensitive));
{$else}
  cmp := FMacros.FCompare;
  for i := 0 to Count - 1 do
  begin
    Result := TMacroItem(inherited Items[i]);
    if cmp(Result.Name, Name) = 0 then Exit;
  end;
  Result := nil;
{$endif}
end;

function TMacroList.IsMacroRegistered(const Name: string): Boolean;
begin
{$ifdef HASHTABLE}
  Result := FindStringHash(Name, FHashTable, FMacros.FCaseSensitive) <> 0;
{$else}
  Result := IndexOfMacro(Name) >= 0;
{$endif}
end;

{ RegisterMacro() registers a new macro. If the macro is already registered and
  the new version is different then the preprocessor warns the user. This
  warning is generated by the caller who test the property ErrorMsg.
  The Macro-string must be left trimmed. }
function TMacroList.RegisterMacro(const Macro: string; AInterfaceMacro: Boolean): TMacroItem;
var Item: TMacroItem;
begin
  FMacros.FErrorMsg := '';

  Result := TMacroItem.Create(Self);
  try
    if not Result.Parse(Macro, FMacros.FErrorMsg, AInterfaceMacro) then
    begin
      Result.Free;
      Result := nil;
      Exit;
    end;

    Item := FindMacro(Result.Name);
    if Item <> nil then
    begin
     // test if it is the same declaration
      if not Result.IsEqual(Item) then
      begin
        FMacros.FErrorMsg := Format(SMacroRedefinitionNotIdentical, [Result.Name]);
{$ifdef HASHTABLE}
        DelStringHash(Integer(Item), FHashTable);
{$endif}
        Remove(Item); // replace macro with the new one
      end;
    end;
{$ifdef HASHTABLE}
    MakeStringHash(Result.Name, Integer(Result), FHashTable);
{$endif}

    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TMacroList.UnregisterMacro(const Name: string);
{$ifdef HASHTABLE}
var Item: TMacroItem;
begin
  Item := TMacroItem(FindStringHash(Name, FHashTable, FMacros.FCaseSensitive));
  if Item <> nil then
  begin
    DelStringHash(Integer(Item), FHashTable);
    Delete(IndexOf(Item));
  end;
end;
{$else}
var Index: Integer;
begin
  Index := IndexOfMacro(Name);
  if Index >= 0 then
  begin
    DelStringHash(Integer(Items[Index]), FHashTable);
    Delete(Index);
  end;
end;
{$endif}

procedure TMacroList.Clear;
begin
{$ifdef HASHTABLE}
  SetLength(FHashTable, 0);
{$endif}
  inherited Clear;
end;

{ TMacros }

constructor TMacros.Create(AFileSys: IMacroFileSys);
begin
  inherited Create(Self);
  FUnits := TStringList.Create;
  FIncludeFiles := TStringList.Create;
  FMacroMacroRecursion := TList.Create;
  SetCaseSensitive(True);

  FConditionals := TStringList.Create;
  FCompilerOptions := TStringList.Create;
  FConditionalParseCode := TBooleanList.Create;
  FConditionalParse := False;

  FFileSys := AFileSys;
end;

destructor TMacros.Destroy;
begin
  FConditionalParseCode.Free;
  FCompilerOptions.Free;
  FConditionals.Free;
  FMacroMacroRecursion.Free;
  FIncludeFiles.Free;
  FUnits.Free;

  FFileSys := nil;
  inherited Destroy;
end;

procedure TMacros.SetCaseSensitive(const Value: Boolean);
begin
  FCaseSensitive := Value;
  if FCaseSensitive then FCompare := CompareStr else FCompare := CompareText;
end;

procedure TMacros.Error(const Msg, FileName: string; LineNum: Integer);
begin
  if Assigned(FOnError) then FOnError(Self, FileName, Msg, LineNum);
  Abort; // raise EAbort Exception -> exit all
end;

procedure TMacros.Error(const Msg: string; Token: PTokenInfo);
var
  Filename: string;
begin
  if Token = nil then
    Error(Msg, '', 0)
  else
  begin
    if Token.pFilename <> nil then
      Filename := Token.pFilename^;
    Error(Msg, FileName, Token.StartLine);
  end;
end;

procedure TMacros.Warning(const Msg, FileName: string; LineNum: Integer);
begin
  if Assigned(FOnError) then FOnWarning(Self, FileName, Msg, LineNum);
end;

procedure TMacros.Warning(const Msg: string; Token: PTokenInfo);
begin
  Warning(Msg, Token.pFileName^, Token.StartLine);
end;

procedure TMacros.PredefineMacros;
begin
  if Assigned(FOnPredefineMacros) then FOnPredefineMacros(Self);
end;

{ RegisterMacroByToken() registers a new macro and triggers error and warning
  messages if necessary. }
function TMacros.RegisterMacroByToken(const Macro: string; Token: PTokenInfo): TMacroItem;
begin
  Result := RegisterMacro(TrimLeft(Macro),
                          TPascalParserEx(Token^.Parser).NoReplaceMacros);  // sets ErrorMsg to '' or error message
  if (ErrorMsg <> '') then
  begin
    if Result = nil then Error(FErrorMsg, Token); // Abort
    Warning(FErrorMsg, Token);
    FErrorMsg := '';
  end;
end;

{ Parse() calls for every unit that it finds the ParseFile() method. }
function TMacros.Parse(const FileName: string; OnlyThisFile: Boolean): Boolean;
var i: Integer;
begin
  Result := True;
  FUnits.Clear;
  FIncludeFiles.Clear;
  FAppType := '';

  FUnits.AddObject(FileName, Pointer(0)); // Even if this file is no unit something has to be parsed
  try
   // parse all used units
    i := 0;
    while i < FUnits.Count do
    begin
      Clear; // clear macro list

      FMacroMacroRecursion.Clear; // reset macro macro recursion
      FFileRecursion := 0;        // reset file recursions

     // reset conditionals
      FConditionals.Clear;
      FConditionalParseCode.Clear; // Count=0 -> .LastItem=True
      DefaultConditionals;

     // get user predefined macros
      PredefineMacros;

     // parse macro include file for this unit
      ParseUnitMacroFile(i);
     // parse this unit
      ParseFile(FUnits[i], ptUnit, {TestFileExistence:=}False);

      Inc(i); // next unit

      if OnlyThisFile then Break; // do not parse other units
    end;
  except
    on EAbort do
      Result := False; // return False
  end;
end;


{ ParseUnitMacroFile() first checks if the macro file exists and then parses
  the file. }
function TMacros.ParseUnitMacroFile(UnitIndex: Integer): Integer;
var MacroFilename: string;
begin
 // Assert(UnitIndex >= 0);
  // Objects[]:
  //   -1: no macros file existance tested
  //    0: no macros file
  //    1: macros file exists
  Result := Integer(FUnits.Objects[UnitIndex]);

  case Result of
    -1:
      begin
        Result := 0;
        MacroFilename := ChangeFileExt(FUnits[UnitIndex], SMacroIncludeFileExt);
        if FFileSys.FileExists(MacroFilename) then
          if ParseFile(MacroFileName, ptInterfaceMacros,
                       {TestFileExistence:=}False) <> '' then
            Result := 1;
        FUnits.Objects[UnitIndex] := Pointer(Result);
      end;
    1:
      begin
        MacroFilename := ChangeFileExt(FUnits[UnitIndex], SMacroIncludeFileExt);
        ParseFile(MacroFileName, ptInterfaceMacros, {TestFileExistence:=}False);
      end;
  end;
end;


{ ParseFile() first checks for the existence of the file and then it calls
  ParseString() with the file content. If the file is an include file (called
  by ParseComments() ) then the file name is added to the FIncFiles list.
  The modified files are saved a .i.pas or .iX.* files (for more information
  see Utils.GetPreProcessedFilename() ). }
function TMacros.ParseFile(Filename: string; ParseType: TParseType;
  TestFileExistence: Boolean): string;
var
  S: string;
  Modified: Boolean;
  Index, IncludeIndex: Integer;
begin
  Result := '';

  Inc(FFileRecursion);
  try
    if FFileRecursion > MaxFileRecursion then // too many open files
      Error(SToManyRecursions, Filename, 1);

   // find the file
    if TestFileExistence then
    begin
      if not FFileSys.FileExists(Filename) then
      begin
        Filename := FFileSys.FindFile(Filename, ParseType = ptInclude); // try to get the file name
        if Length(Filename) = 0 then Exit;
      end;
    end;

    Modified := False;
    Result := Filename;
    if ParseType <> ptInterfaceMacros then
      FFileSys.BeforeFile(Filename, ParseType = ptInclude);
    try
      FFileSys.LoadFile(Filename, S, ParseType = ptInclude);  // load file into a string

      if S <> '' then
      begin
       // parse file and replace macros
        Modified := ParseString(S, Filename, {StartLinenum:=}1, ParseType);

        if ParseType = ptInterfaceMacros then
          Modified := False; // only read these files, do not write these files

        if (Modified) then
        begin
          if ParseType = ptInclude then
          begin
           // change file extension to .i1.*, .i2.* and so on for include files
            Index := IndexOfFilename(FIncludeFiles, Filename);
            if Index = -1 then
            begin
              FIncludeFiles.AddObject(Filename, Pointer(1));
              IncludeIndex := 1;
            end
            else
            begin
              IncludeIndex := Integer(FIncludeFiles.Objects[Index]) + 1;
              FIncludeFiles.Objects[Index] := Pointer(IncludeIndex);
            end;
          end
          else
            IncludeIndex := 0; // -> .i.pas

          Result := GetPreProcessedFilename(Filename, IncludeIndex);
          FFileSys.SaveFile(Filename, Result, S, ParseType = ptInclude); // save string to file
        end;
      end;

    finally
      if ParseType <> ptInterfaceMacros then
        FFileSys.AfterFile(Filename, Result, ParseType = ptInclude, Modified);
    end;
  finally
    Dec(FFileRecursion);
  end;
end;


{ ParseString() is the main parsing method. It is called by ParseComment() and
  ParseFile(). There is no interesting stuff in this function. }
function TMacros.ParseString(var Text: string; const Filename: string;
  StartLineNum: Integer; ParseType: TParseType): Boolean;
var
  Parser: TPascalParserEx;
  Token: PTokenInfo;
begin
  Parser := TPascalParserEx.Create(Filename, Text, StartLineNum);
  try
    Parser.NoReplaceMacros := ParseType = ptInterfaceMacros;
    while NextToken(Parser, Token) do // NextToken() replaces macros
    begin
      if Token.Kind = tkComment then
      begin
       // test for macro related comments
        ParseComment(Token); // FConditionalParseCode is tested in ParseComment()
      end
      else if Token.Kind = tkIdent then
      begin
        if FConditionalParse and not FConditionalParseCode.Last then // still in a "false condition"
          Continue;

        if (ParseType = ptInterfaceMacros) then
        begin
          if (SameText(Token.Value, 'implementation')) then
            Break; // do not parse any implementation macros
        end
        else
        begin
          if (SameText(Token.Value, 'uses')) or (SameText(Token.Value, 'contains')) then
            ParseUsesIdent(Parser); // parse USES statement
        end;
      end;
    end;
  finally
    Result := Parser.Modified;
    if Result then Text := Parser.Text; // replace Text
    Parser.Free;
  end;
end;


{ NextToken() returns the next token. If there is no further token the result
  value is FALSE else TRUE and the Token argument is NIL.
  For every token which is a macro name the ReplaceMacros() method is called. }
function TMacros.NextToken(Parser: TPascalParserEx; out Token: PTokenInfo): Boolean;
var
  Item: TMacroItem;
  Replacement: string;
begin
  if (Parser.NoReplaceMacros) or (FConditionalParse and not FConditionalParseCode.Last) then
  begin
   // just collect macros
    Result := Parser.GetToken(Token);
    Exit;
  end;
 // replace macros
  repeat
    Result := Parser.GetToken(Token);
    if (Result) and (Token.Kind = tkIdent) then
    begin
      if (Token.Value[1] = SBuiltInStartChars[1]) and
         (Token.Value[2] = SBuiltInStartChars[2]) and // may be point to #0 but this is no problem
         (BuiltInMacro(Token, Replacement)) then
      begin
       // built in simple macros
        Parser.ReplaceParseNext(Token, Token, Replacement);
      end
      else
      begin
        Item := FindMacro(Token.Value);
        if (Item <> nil) then
          ReplaceMacro(Parser, Item)
        else
          Break; // no macro -> return token
      end;
    end
    else
      Break; // no macro -> return token
  until False;
end;


{ NextToken(): no comment }
function TMacros.NextToken(Parser: TPascalParserEx): PTokenInfo;
begin
  NextToken(Parser, Result);
end;


{ ParseConditionals() parses all {$... compiler directives. It also interprets
  the $IFDEF, $IFNDEF, $ELSE, $ENDIF and $APPTYPE directives.

  $DEFINE und $UNDEF are directly handled by ParseComment().
}
function TMacros.ParseConditionals(var Line: string; const Filename: string;
  StartLineNum: Integer): Boolean;
type
  TConditionalWordType = (cwNone,
    cwIfdef, cwIfndef, {cwIfopt,} cwElse,
    cwEndif, {cwIf, cwElseif, cwIfend,} cwAppType
  );
const
  ConditionalWords: array[TConditionalWordType] of string = (
    '',
    'ifdef', 'ifndef', {'ifopt',}
    'else', 'endif',
    {'if', 'elseif', 'ifend'}
    'apptype'
  );
  procedure SyntaxError;
  begin
    Error(SConditionalSyntaxError, Filename, StartLineNum);
  end;

var
  Parser: TPascalParserEx;
  Token: PTokenInfo;
  ConditionalWord, Found: TConditionalWordType;
  Item: TMacroItem;
begin
  System.Delete(Line, 1, 1); // remove '$'
  if (Length(Line) = 0) or (Line[1] <= ' ') then SyntaxError;

  Parser := TPascalParserEx.Create(Filename, Line, StartLineNum);
  try
    if (not Parser.GetToken(Token)) or (Token.Kind <> tkIdent) then
      Token.Value := '';

    ConditionalWord := cwNone;
    for Found := Low(TConditionalWordType) to High(TConditionalWordType) do
      if SameText(Token.Value, ConditionalWords[Found]) then
      begin
        ConditionalWord := Found;
        Break;
      end;

    case ConditionalWord of
      cwIfdef, cwIfndef:
        begin
          if not FConditionalParse or FConditionalParseCode.Last then // can parse these line
          begin
            Token := Parser.GetToken;
            if Token = nil then SyntaxError;

            Item := FindMacro(Token.Value);
            case ConditionalWord of
              cwIfdef:
                begin
                  if (Item <> nil) and (Item.InterfaceMacro) then
                  begin
                   // replace the token by 'PREPROCESSOR'
                    FConditionalParseCode.Add(True);
                    Parser.ReplaceParseNext(Token, Token, 'PREPROCESSOR');
                  end
                  else
                    FConditionalParseCode.Add(IsDefined(Token.Value));
                end;

              cwIfndef:
                begin
                  if (Item <> nil) and (Item.InterfaceMacro) then
                  begin
                   // replace the token by 'NEVER_DEFINED'
                    FConditionalParseCode.Add(False);
                    Parser.ReplaceParseNext(Token, Token, 'NEVER_DEFINED');
                  end
                  else
                    FConditionalParseCode.Add(not IsDefined(Token.Value));
                end;
            end;
          end;
        end; // cwDefine, cwUndefine, cwIfdef, cwIfndef
      cwElse: FConditionalParseCode.ToggleLast;
      cwEndif: FConditionalParseCode.DeleteLast;
      cwAppType:
        begin
          if not FConditionalParse or FConditionalParseCode.Last then // can parse these line
          begin
            if (NextToken(Parser, Token)) and (Token.Kind = tkIdent) then
            begin
              FAppType := Token.Value;
              if SameText(FAppType, 'CONSOLE') then Define('CONSOLE')
                                               else Undefine('CONSOLE');
            end;
          end;
        end; // cwAppType

      else
        if not FConditionalParse or FConditionalParseCode.Last then // can parse these line
         // parse and replace macros
          while NextToken(Parser, Token) do ; // replace macros
    end; // case
  finally
    Result := Parser.Modified;
    if Result then Line := '$' + Parser.Text;
    Parser.Free;
  end;
end;


{ ParseComment() parses all comment tokens. Single line comments (//) are
  ignored. It registers all found MACRO statments. For include files $I and
  $INCLUDE the ParseFile() method is called. After parsing the include file
  the $I/$INCLUDE statment is replaced by the new filename returned by
  ParseFile().

  For compiler directives and conditional compilation macros are replaced. }
function TMacros.ParseComment(Token: PTokenInfo): Boolean;
var
  ps, BracketCount, ri, Len: Integer;
  Item: TMacroItem;
  s, Filename: string;
  IsCompilerDirective: Boolean;
begin
  Result := True;
  s := Token.Value;
  if s[1] = '/' then Exit; // single line comment are not parsed

 // remove comment brackets
  if s[1] = '(' then BracketCount := 2 else BracketCount := 1;
  System.Delete(s, 1, BracketCount);
  System.Delete(s, Length(s) - BracketCount + 1, BracketCount);
  if Pointer(s) = nil then Exit; // <==> if Length(s) = 0 then Exit;

  IsCompilerDirective := (s[1] = '$');

  if (IsCompilerDirective) and (not FConditionalParse or FConditionalParseCode.Last) and
     (StartsText(SMacroStartString, s)) then
  begin
   // register new macro
    System.Delete(s, 1, Length(SMacroStartString));
    Item := RegisterMacroByToken(s, Token);
    if Item <> nil then Define(Item.Name); // define also as conditional
  end

  else if (IsCompilerDirective) and
          (not FConditionalParse or FConditionalParseCode.Last) and
          (StartsText(SUnmacroStartString, s)) then
  begin
   // unregister macro
    System.Delete(s, 1, Length(SUnmacroStartString));
    s := Trim(s);
    UnregisterMacro(s);
    Undefine(s); // undefine conditional
  end

  else if (not IsCompilerDirective) and
          (not FConditionalParse or FConditionalParseCode.Last) and
          (s[1] in ['M', 'm']) and
          (StartsText(SMacroIncludeString, s)) then
  begin
   // parse macro include file

    System.Delete(s, 1, PosChar(' ', s));
    s := Trim(s);
    if s <> '' then
    begin
      if s[1] = '''' then
      begin
        System.Delete(s, 1, 1);
        System.Delete(s, Length(s), 1);
      end;

      FileName := ParseFile(s, ptInterfaceMacros, {TestFileExistence:=}True);
      if Filename = '' then
        Error(Format(SFindFile, [s]), Token);
    end;
  end

  else if (IsCompilerDirective) and
          (not FConditionalParse or FConditionalParseCode.Last) and
          ((StartsText('$I ', s)) or (StartsText('$INCLUDE ', s))) then
  begin
   // parse include file

    System.Delete(s, 1, PosChar(' ', s));
    s := Trim(s);
    if s <> '' then
    begin
      if s[1] = '''' then
      begin
        System.Delete(s, 1, 1);
        System.Delete(s, Length(s), 1);
      end;
     // parse include file
      if TPascalParserEx(Token.Parser).NoReplaceMacros then
        Filename := ParseFile(s, ptInterfaceMacros, {TestFileExistence:=}True)
      else
        Filename := ParseFile(s, ptInclude, {TestFileExistence:=}True);
      if Filename = '' then
        Error(Format(SFindFile, [s]), Token);
      if ExtractFileName(Filename) = ExtractFileName(s) then Exit; // file was not modified so no file name change

      if TPascalParserEx(Token.Parser).NoReplaceMacros then Exit;

     // replace old filename by new one
      ps := Pos(s, Token.Value); // find file name start position
      s := Token.Value;
      ri := ps - 1;
      while (ri > 1) and (not (s[ri] in ['''', ' '])) do Dec(ri);
      Len := Length(s);
      while (ps < Len) and (not (s[ps] in [s[ri], '}', '*'])) do Inc(ps);
      if s[ps] <> '''' then Dec(ps);
      if s[ri] = ' ' then Inc(ri);
      System.Delete(s, ri, ps - ri + 1);
      System.Insert('''' + Filename + '''', s, ri);

      Token.Parser.ReplaceParseNext(Token, Token, s); // replace token
      Token.Parser.ClearCache; // clear cache
    end;

  end else if (IsCompilerDirective) then
  begin
    // compiler directive / conditional compilation

    if ParseConditionals(s, Token.pFilename^, Token.StartLine) then
    begin
      if BracketCount = 1 then s := '{' + s + '}'
                          else s := '(*' + s + '*)';
      Token.Parser.ReplaceParseNext(Token, Token, s); // replace token
      Token.Parser.ClearCache; // clear cache
    end;
  end;
end;


{ ParseUsesIdent() parses the USES statement and add all found units who's
  file exists to the FUnits-List. No duplicate files are added. }
procedure TMacros.ParseUsesIdent(Parser: TPascalParserEx);
var
  Token: PTokenInfo;
  s: string;
  UnitIndex: Integer;
begin
  while NextToken(Parser, Token) do // NextToken() replaces macros
  begin
    if Token.Kind = tkSymbol then
    begin
      if Token.Value = ';' then Break
      else if Token.Value = ',' then Continue;
    end

    else if Token.Kind = tkIdent then
    begin
      if SameText(Token.Value, 'in') then
      begin
        Token := NextToken(Parser); // NextToken() replaces macros
        s := RemoveQuotes(Token.Value);
      end else s := Token.Value + '.pas';

      s := FFileSys.FindFile(s, {IsInclude:=}False);
      if s <> '' then
      begin
        UnitIndex := IndexOfFilename(FUnits, s);
       // add this unit to the unit-parse-list.
        if UnitIndex = -1 then
          UnitIndex := FUnits.AddObject(s, Pointer(-1));

       // Include the macros from the interface section of the the new unit.
        ParseUnitMacroFile(UnitIndex);

       // Include the macro include file for the new unit.
        ParseUnitMacroFile(UnitIndex);
      end;
    end;
  end;
end;


{ GetReplacement() is called for "Function Macros" only. It replaces the
  arguments of the macro item with Args[] which is created in ReplaceMacros().
  All macros in Args[] are replaced when entering this method. }
function TMacros.GetReplacement(Item: TMacroItem; const Args: TStringDynArray;
  const Filename: string; StartLineNum: Integer): string;

  function GetArg(Token: PTokenInfo; var Arg: string): Boolean;
  var ArgIndex: Integer;
  begin
   // do not set Args to '' here
    Result := False;
    if (Token = nil) or (Token.Kind <> tkIdent) then Exit;

    ArgIndex := Item.IndexOfArg(Token.Value);
    if ArgIndex >= 0 then
    begin
      Arg := Args[ArgIndex];
      Result := True;
    end;
  end;

var
  Parser: TPascalParserEx;
  StartToken, Token: PTokenInfo;
begin
 // parsing "Macro Replacement" text
  Parser := TPascalParserEx.Create(Filename, Item.Replacement, StartLineNum);
  try
    while Parser.GetToken(Token) do
    begin

      if (Token.Kind = tkIdent) then
      begin
        if GetArg(Token, Token.Value) then
        begin
          StartToken := Token;
         // parse the new content but do not clear cache so it is possilbe
         // for '##' to get the string as identifier
          Parser.ReplaceParseNext(StartToken, Token, Token.Value);
        end;
      end


      else if (Token.Kind = tkSymbol) then
      begin
       // make string
        if Token.Value = '#' then
        begin
          StartToken := Token;
          if (not Parser.GetToken(Token)) then
            Error(Format(SMacroSytaxError, [SNoFurtherToken]), StartToken); // Abort
          if (not GetArg(Token, Token.Value)) then
            Error(Format(SMacroSytaxError, [SCanOnlyMakeStringFromArguments]), StartToken); // Abort

          Token.Value := '''' + Token.Value + '''';
          Parser.ReplaceParseNext(StartToken, Token, Token.Value);
         // parse the new content but do not clear cache so it is possilbe for
         // '##' to get the string as identifier
          Token.StartIndex := StartToken.StartIndex; // adjust tkIdent-token StartIndex becoming PreToken
        end


        else if (Token.Value = '##') then
        begin
          StartToken := Parser.PreToken;
          if (StartToken = nil) or
             (StartToken.Kind <> tkIdent) or
             (not Parser.GetToken(Token)) or
             (Token.Kind <> tkIdent) then  // changes <Token>
            Error(Format(SMacroSytaxError, [SCombineError]), Token); // Abort

          GetArg(Token, Token.Value); // get argument replacement if available

         // parse the new content but do not clear cache so it is possilbe for
         // '##' to get the string as identifier
          Parser.ReplaceParseNext(StartToken, Token, TrimRight(StartToken.Value) + TrimLeft(Token.Value));
          Token.StartIndex := StartToken.StartIndex; // adjust tkIdent-token StartIndex becoming PreToken
        end;
      end; // if Token.Kind = tkSymbol

    end; // while

    Result := Parser.Text;
  finally
    Parser.Free;
  end;
end;


{ ReplaceMacro() creates the argument array and replaces all array items by
  its macro(-function). For macro functions GetReplacement() is called. }
procedure TMacros.ReplaceMacro(Parser: TPascalParserEx; Item: TMacroItem);
var
  Token: PTokenInfo;
  ReplStartIndex, ReplEndIndex, LastCommaIndex: Integer;
  Replacement: string;
  BracketNum: Integer;
  Args: TStringDynArray;
  ArgIndex: Integer;
  AddedLines: Integer;
  EndLineNum: Integer;
begin
  if FMacroMacroRecursion.IndexOf(Item) >= 0 then Exit; // do not replace the macro with itself
  FMacroMacroRecursion.Add(Item);
  try
    Token := Parser.CurToken;
    ReplStartIndex := Token.StartIndex;
    ReplEndIndex := Token.EndIndex;
    EndLineNum := Token.EndLine;
    if Item.HasBrackets then
    begin
     // macro with arguments

     // is '(' the next token
      if (not Parser.GetToken(Token)) or (Token.Value <> '(') then
        Error(Format(SMacroSytaxError, [SNoArgumentSpecified]), Parser.PreToken); // Abort


     // Here we use NextToken(), because the arguments can also be macros and
     // NextToken() replaces them.

     // get macro arguments
      SetLength(Args, Length(Item.Arguments));
      ArgIndex := 0;
      LastCommaIndex := Token.StartIndex;
      BracketNum := 1;
      while NextToken(Parser, Token) do // NextToken() replaces macros
      begin
        EndLineNum := Token.EndLine;
        if Token.Value = '(' then Inc(BracketNum)
        else if Token.Value = ')' then
        begin
          Dec(BracketNum);
          if BracketNum = 0 then
          begin
            if Length(Args) > 0 then
            begin
              Args[ArgIndex] := Trim(Parser.GetPlainText(LastCommaIndex + 1, Token.StartIndex - 1)); // save last argument
              if Length(Args[ArgIndex]) = 0 then
                Error(Format(SMacroSytaxError, [SEmptyMacroArgument]), Token);
            end;
            Break; // last bracket
          end;
        end
        else if (BracketNum = 1) and (Token.Value = ',') then
        begin
          Args[ArgIndex] := Parser.GetPlainText(LastCommaIndex + 1, Token.StartIndex - 1);
          if IsStrEmpty(Args[ArgIndex]) then
            Error(Format(SMacroSytaxError, [SEmptyMacroArgument]), Token);

          LastCommaIndex := Token.StartIndex;
         // new argument
          Inc(ArgIndex);
          if ArgIndex >= Length(Args) then
            Error(SMacroSytaxError, Token);
        end;
      end;
      SetLength(Args, ArgIndex + 1); // set to correct length
      ReplEndIndex := Token.EndIndex; // new end index

     // check arguments
      if Length(Args) <> Length(Item.Arguments) then
        Error(Format(SMacroNotEnoughArguments, [Item.Name]), Token);

      Replacement := GetReplacement(Item, Args, Parser.Filename, Token.StartLine);
    end
    else
      Replacement := Item.Replacement; // just a simple replacement

   // parse replacement
    ParseString(Replacement, Parser.Filename, Parser.LineNum, ptInclude);
   // replace text
    Parser.ReplaceParseNext(ReplStartIndex, ReplEndIndex - ReplStartIndex + 1, Replacement);
    Parser.ClearCache; // new parse start and clear token cache

   // lines moved
    AddedLines := CountChars(#10, Replacement);
    if AddedLines > 0 then
      FFileSys.LinesMoved(Token^.Parser.Filename, EndLineNum, AddedLines);
  finally
    FMacroMacroRecursion.Remove(Item);
  end;
end;


function TMacros.BuiltInMacro(Token: PTokenInfo; var Replacement: string): Boolean;
begin
 // __LINE__
  if FCompare(Token.Value, SBuiltIn_Line) = 0 then
  begin
    Result := True;
    Replacement := IntToStr(Token.StartLine);
  end

 // __FILE__
  else if FCompare(Token.Value, SBuiltIn_File) = 0 then
  begin
    Result := True;
    Replacement := '''' + Token.pFilename^ + ''''
  end

 // __DATE__
  else if FCompare(Token.Value, SBuiltIn_Date) = 0 then
  begin
    Result := True;
    Replacement := '''' + DateToStr(Date) + ''''
  end

 // __TIME__
  else if FCompare(Token.Value, SBuiltIn_Time) = 0 then
  begin
    Result := True;
    Replacement := '''' + TimeToStr(Time) + ''''
  end

  else
  begin
    Result := False;
    if Assigned(FOnBuiltInMacro) then
    begin
      Replacement := '';
      FOnBuiltInMacro(Self, Token, Replacement, Result);
    end;
  end;
end;

procedure TMacros.DefaultConditionals;
begin
  Define('PREPROCESSOR'); // always defined
 //  Define('CONDITIONALEXPRESSIONS');  not supported

{$ifdef VER130}         Define('VER130'); {Delphi 5} {$endif}
{$ifdef VER140}         Define('VER140'); {Delphi 6} {$endif}
{$ifdef VER150}         Define('VER150'); {Delphi 7} {$endif}
{$ifdef VER160}         Define('VER160'); {Delphi 8} {$endif}

{$ifdef MSWINDOWS}      Define('MSWINDOWS'); {$endif}
{$ifdef WIN32}          Define('WIN32'); {$endif}
{$ifdef LINUX}          Define('LINUX'); {$endif}
{$ifdef CPU386}         Define('CPU386'); {$endif}

  if SameText(FAppType, 'CONSOLE') then
    Define('CONSOLE');

 // user defined
  if Assigned(FOnDefaultConditionals) then
    FOnDefaultConditionals(Self);

  Undefine('CONDITIONALEXPRESSIONS'); // not supported
end;

procedure TMacros.Define(const Condition: string);
begin
  if (Condition <> '') and (FConditionals.IndexOf(Condition) = -1) then
    FConditionals.Add(Condition);
end;

procedure TMacros.Undefine(const Condition: string);
var Index: Integer;
begin
  Index := FConditionals.IndexOf(Condition);
  if Index >= 0 then FConditionals.Delete(Index);
end;

function TMacros.IsDefined(const Condition: string): Boolean;
begin
  Result := (Condition <> '') and (FConditionals.IndexOf(Condition) >= 0);
end;

procedure TMacros.SetOption(const Option: string; Value: Boolean);
begin
{TODO set options for $ifopt, be carefull with $R+/- and $RANGECHECKS ON/OFF and so on}
end;

end.
