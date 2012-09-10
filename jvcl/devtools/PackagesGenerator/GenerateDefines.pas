unit GenerateDefines;

{$I jvcl.inc}

interface

uses
  Contnrs, Classes,
  JclSimpleXml,
  ConditionParser;

type
  TDefine = class(TObject)
  private
    FName: string;
    FIfDefs: TStrings;
  public
    constructor Create(const Name: string; IfDefs : TStringList);
    destructor Destroy; override;

    property Name: string read FName write FName;
    property IfDefs: TStrings read FIfDefs;
  end;

  TDefinesList = class(TObjectList)
  private
    function GetItems(index: Integer): TDefine;
    procedure SetItems(index: Integer; const Value: TDefine);
  public
    constructor Create(incfile : TStrings); overload;

    procedure Append(incfile : TStrings);
    function Find(const DefineName: string; DefineLimit : Integer = -1; DefineStart: Integer = 0): TDefine;
    procedure RemoveDefine(const DefineName: string);

    function IsDefined(const Condition: string; TargetDefines: TStrings;
      DefineLimit : Integer = -1; DefineStart: Integer = 0): Boolean;

    property Items[index: Integer]: TDefine read GetItems write SetItems; default;
  end;

implementation

uses
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  SysUtils, JclStrings;

{ TDefine }

constructor TDefine.Create(const Name : string; IfDefs : TStringList);
begin
  inherited Create;

  FName := Name;
  FIfDefs := TStringList.Create;
  FIfDefs.Assign(IfDefs);
end;

destructor TDefine.Destroy;
begin
  FIfDefs.Free;

  inherited Destroy;
end;

{ TDefinesList }

procedure TDefinesList.Append(incfile: TStrings);
const
  IfDefMarker  : string = '{$IFDEF';
  IfNDefMarker : string = '{$IFNDEF';
  EndIfMarker  : string = '{$ENDIF';
  ElseMarker   : string = '{$ELSE';
  DefineMarker : string = '{$DEFINE';
  IfMarker : string = '{$IF';
var
  i: Integer;
  curLine: string;
  IfDefs : TStringList;
  ClosePos: Integer;
  LastIfIsIfDef: Boolean;
  PrefixUsed: Boolean;
begin
  LastIfIsIfDef := False;
  IfDefs := TStringList.Create;
  try
    if Assigned(incfile) then
      for i := 0 to incfile.Count - 1 do
      begin
        curLine := Trim(incfile[i]);

        while Pos('{', curLine) > 0 do
        begin
          PrefixUsed := False;
          if StrHasPrefix(curLine, [IfDefMarker]) then
          begin
            IfDefs.AddObject(Copy(curLine, Length(IfDefMarker)+2, Pos('}', curLine) - Length(IfDefMarker) - 2), TObject(True));
            LastIfIsIfDef := True;
            PrefixUsed := True;
          end
          else if StrHasPrefix(curLine, [IfNDefMarker]) then
          begin
            IfDefs.AddObject(Copy(curLine, Length(IfNDefMarker)+2, Pos('}', curLine) - Length(IfNDefMarker) - 2), TObject(False));
            LastIfIsIfDef := True;
            PrefixUsed := True;
          end
          else if StrHasPrefix(curLine, [ElseMarker]) then
          begin
            IfDefs.Objects[IfDefs.Count-1] := TObject(not Boolean(IfDefs.Objects[IfDefs.Count-1]));
            PrefixUsed := True;
          end
          else if StrHasPrefix(curLine, [EndIfMarker]) then
          begin
            if LastIfIsIfDef then
              IfDefs.Delete(IfDefs.Count-1);
            PrefixUsed := True;
          end
          else if StrHasPrefix(curLine, [DefineMarker]) then
          begin
            Add(TDefine.Create(Copy(curLine, Length(DefineMarker)+2, Pos('}', curLine) - Length(DefineMarker) - 2), IfDefs));
            PrefixUsed := True;
          end
          else if StrHasPrefix(curLine, [IfMarker]) then
          begin
            LastIfIsIfDef := False;
            PrefixUsed := True;
          end;

          ClosePos := Pos('}', curLine);
          if PrefixUsed and (ClosePos > 0) then
            curLine := Trim(Copy(curLine, ClosePos + 1, MaxInt))
          else
            curLine := '';
        end;
      end;
  finally
    IfDefs.Free;
  end;
end;

constructor TDefinesList.Create(incfile: TStrings);
begin
  inherited Create(True);

  Append(incfile);
end;

function TDefinesList.Find(const DefineName: string; DefineLimit : Integer = -1; DefineStart: Integer = 0): TDefine;
var
  I: Integer;
begin
  if DefineLimit = -1 then
    DefineLimit := Count
  else
  if DefineLimit > Count then
    DefineLimit := Count;

  Result := nil;
  for I := DefineStart to DefineLimit - 1 do
  begin
    if SameText(Items[I].Name, DefineName) then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TDefinesList.GetItems(index: integer): TDefine;
begin
  Result := TDefine(inherited Items[index]);
end;

function TDefinesList.IsDefined(const Condition: string; TargetDefines: TStrings;
  DefineLimit : Integer = -1; DefineStart: Integer = 0): Boolean;
var
  I : Integer;
  Define : TDefine;
  DefineIndex: Integer;
begin
  if DefineLimit = -1 then
    DefineLimit := Count
  else
  if DefineLimit > Count then
    DefineLimit := Count;

  Result := False;
  Define := nil;
  DefineIndex := -1;
  for i := DefineStart to DefineLimit - 1 do
  begin
    if SameText(Items[I].Name, Condition) then
    begin
      Result := True;
      Define := Items[I];
      DefineIndex := I;
      Break;
    end;
  end;

  // If the condition is not defined by its name, maybe it
  // is as a consequence of the target we use
  if not Result and Assigned(TargetDefines) then
    Result := TargetDefines.IndexOf(Condition) > -1;

  // If the condition is defined, then all the IfDefs in which
  // it is enclosed must also be defined but only before the
  // current define
  if Result and Assigned(Define) then
    for I := 0 to Define.IfDefs.Count - 1 do
    begin
      if Boolean(Define.IfDefs.Objects[I]) then
        Result := Result and IsDefined(Define.IfDefs[I], TargetDefines, DefineIndex)
      else
        Result := Result and not IsDefined(Define.IfDefs[I], TargetDefines, DefineIndex);
    end;

  if not Result and (DefineIndex >= 0) then
    Result := IsDefined(Condition, TargetDefines, -1, DefineIndex + 1);
end;

procedure TDefinesList.RemoveDefine(const DefineName: string);
begin
  Remove(Find(DefineName));
end;

procedure TDefinesList.SetItems(index: integer; const Value: TDefine);
begin
  inherited Items[index] := Value;
end;

end.
