{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: VclClxCvtUtils.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit VclClxCvtUtils;

interface

uses
  SysUtils, Classes, Contnrs;

type
  TUnitReplaceItem = class(TObject)
    UnitName: string;
    ReplaceName: string;
  end;

  TUnitReplaceList = class(TObject)
  private
    FItems: TObjectList;
    FNames: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AUnitName, AReplaceName: string);
    function Find(const AUnitName: string): string;

    procedure AddFromIni(const Filename: string);
  end;

  TConditionStackItem = class(TObject)
  private
    FCondition: string;
    FNegative: Boolean;
    FOpenStartIndex, FOpenEndIndex: Integer;
    FElseStartIndex, FElseEndIndex: Integer;
    FInElse: Boolean;
    FOpenLine: Integer;
    FElseLine: Integer;
    function GetInFalse: Boolean;
    function GetInTrue: Boolean;
    function GetHasElse: Boolean;
  public
    constructor Create(const ACondition: string; ANegative: Boolean);

    function IsIn: Boolean;
      // True: InTrue; False: InFalse

    property Condition: string read FCondition;
    property Negative: Boolean read FNegative;
    property InElse: Boolean read FInElse;
    property HasElse: Boolean read GetHasElse;

    property OpenStartIndex: Integer read FOpenStartIndex;
    property OpenEndIndex: Integer read FOpenEndIndex;
    property OpenLine: Integer read FOpenLine;
    property ElseStartIndex: Integer read FElseStartIndex;
    property ElseEndIndex: Integer read FElseEndIndex;
    property ElseLine: Integer read FElseLine;

    property InTrue: Boolean read GetInTrue;
    property InFalse: Boolean read GetInFalse;
  end;

  TConditionStack = class(TObject)
  private
    FStack: TObjectList;
    function GetCurrent: TConditionStackItem;
    function GetOpenCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter(const Condition: string; StartIndex, EndIndex, Line: Integer);
    procedure EnterNot(const Condition: string; StartIndex, EndIndex, Line: Integer);
    procedure GoElse(StartIndex, EndIndex, Line: Integer);
    procedure Leave;

    function IsIn(const Condition: string): Integer;
      { returns 0: not open;  1: inTrue;   -1: inFalse }

    property Current: TConditionStackItem read GetCurrent;
    property OpenCount: Integer read GetOpenCount;
  end;

  TConverterStatistics = class(TObject)
  private
    FParsedFiles: Integer;
    FWrittenFiles: Integer;
    FUnitReplacements: Integer;
    FErrors: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure IncParsedFiles;
    procedure IncWrittenFiles;
    procedure IncUnitReplacements;

    procedure AddError(const Msg: string);

    property ParsedFiles: Integer read FParsedFiles;
    property WrittenFiles: Integer read FWrittenFiles;
    property UnitReplacements: Integer read FUnitReplacements;
    property Errors: TStrings read FErrors;
  end;

implementation

{ TUnitReplaceList }

constructor TUnitReplaceList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  FNames := TStringList.Create;
  FNames.Sorted := True;
  FNames.Duplicates := dupError;
end;

destructor TUnitReplaceList.Destroy;
begin
  FItems.Free;
  FNames.Free;
  inherited Destroy;
end;

procedure TUnitReplaceList.Add(const AUnitName, AReplaceName: string);
var
  Item: TUnitReplaceItem;
begin
  Item := TUnitReplaceItem.Create;
  Item.UnitName := AUnitName;
  Item.ReplaceName := AReplaceName;
  FItems.Insert(FNames.Add(AUnitName), Item);
end;

function TUnitReplaceList.Find(const AUnitName: string): string;
var
  Index: Integer;
begin
  if FNames.Find(AUnitName, Index) then
    Result := TUnitReplaceItem(FItems[Index]).ReplaceName
  else
    Result := AUnitName;
end;

procedure TUnitReplaceList.AddFromIni(const Filename: string);
var
  List: TStrings;
  i: Integer;
begin
  List := TStringList.Create;
  try
    List.LoadFromFile(Filename);
    for i := 0 to List.Count - 1 do
      Add(List.Names[i], List.ValueFromIndex[i]);
  finally
    List.Free;
  end;
end;

{ TConditionStackItem }

constructor TConditionStackItem.Create(const ACondition: string;
  ANegative: Boolean);
begin
  inherited Create;
  FCondition := ACondition;
  FNegative := ANegative;
end;

function TConditionStackItem.GetHasElse: Boolean;
begin
  Result := FElseStartIndex > 0;
end;

function TConditionStackItem.GetInFalse: Boolean;
begin
  Result := not IsIn;
end;

function TConditionStackItem.GetInTrue: Boolean;
begin
  Result := IsIn;
end;

function TConditionStackItem.IsIn: Boolean;
begin
  Result := Negative xor not InElse;
end;

{ TConditionStack }

constructor TConditionStack.Create;
begin
  inherited Create;
  FStack := TObjectList.Create;
end;

destructor TConditionStack.Destroy;
begin
  FStack.Free;
  inherited Destroy;
end;

procedure TConditionStack.Enter(const Condition: string;
  StartIndex, EndIndex, Line: Integer);
var
  Item: TConditionStackItem;
begin
  Item := TConditionStackItem.Create(Condition, False);
  Item.FOpenStartIndex := StartIndex;
  Item.FOpenEndIndex := EndIndex;
  Item.FOpenLine := Line;
  FStack.Add(Item);
end;

procedure TConditionStack.EnterNot(const Condition: string;
  StartIndex, EndIndex, Line: Integer);
var
  Item: TConditionStackItem;
begin
  Item := TConditionStackItem.Create(Condition, True);
  Item.FOpenStartIndex := StartIndex;
  Item.FOpenEndIndex := EndIndex;
  Item.FOpenLine := Line;
  FStack.Add(Item);
end;

function TConditionStack.GetCurrent: TConditionStackItem;
begin
  if FStack.Count > 0 then
    Result := TConditionStackItem(FStack[FStack.Count - 1])
  else
    Result := nil;
end;

function TConditionStack.GetOpenCount: Integer;
begin
  Result := FStack.Count;
end;

procedure TConditionStack.GoElse(StartIndex, EndIndex, Line: Integer);
begin
  if FStack.Count > 0 then
  begin
    with Current do
    begin
      FInElse := True;
      FElseStartIndex := StartIndex;
      FElseEndIndex := EndIndex;
      FElseLine := Line;
    end;
  end;
end;

function TConditionStack.IsIn(const Condition: string): Integer;
var
  i: Integer;
  Item: TConditionStackItem;
begin
  Result := 0;
  for i := FStack.Count - 1 downto 0 do
  begin
    Item := TConditionStackItem(FStack[i]);
    if SameText(Item.Condition, Condition) then
    begin
      if Item.InTrue then
        Result := 1
      else
        Result := -1;
      Exit;
    end;
  end;
end;

procedure TConditionStack.Leave;
begin
  if FStack.Count > 0 then
    FStack.Delete(FStack.Count - 1);
end;

{ TConverterStatistics }

constructor TConverterStatistics.Create;
begin
  inherited Create;
  FErrors := TStringList.Create;
end;

destructor TConverterStatistics.Destroy;
begin
  FErrors.Free;
  inherited Destroy;
end;

procedure TConverterStatistics.AddError(const Msg: string);
begin
  FErrors.Add(Msg);
end;

procedure TConverterStatistics.IncParsedFiles;
begin
  Inc(FParsedFiles);
end;

procedure TConverterStatistics.IncWrittenFiles;
begin
  Inc(FWrittenFiles);
end;

procedure TConverterStatistics.IncUnitReplacements;
begin
  Inc(FUnitReplacements);
end;

end.
