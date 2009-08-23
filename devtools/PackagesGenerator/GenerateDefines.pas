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
    function IsDefined(const Condition: string; TargetDefines: TStrings;
      DefineLimit : Integer = -1): Boolean;

    property Items[index: Integer]: TDefine read GetItems write SetItems; default;
  end;

implementation

uses
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

constructor TDefinesList.Create(incfile: TStrings);
const
  IfDefMarker  : string = '{$IFDEF';
  IfNDefMarker : string = '{$IFNDEF';
  EndIfMarker  : string = '{$ENDIF';
  ElseMarker   : string = '{$ELSE';
  DefineMarker : string = '{$DEFINE';
var
  i: Integer;
  curLine: string;
  IfDefs : TStringList;
begin
  inherited Create(True);

  IfDefs := TStringList.Create;
  try
    if Assigned(incfile) then
      for i := 0 to incfile.Count - 1 do
      begin
        curLine := Trim(incfile[i]);

        if StrHasPrefix(curLine, [IfDefMarker]) then
          IfDefs.AddObject(Copy(curLine, Length(IfDefMarker)+2, Length(curLine)-Length(IfDefMarker)-2), TObject(True))
        else if StrHasPrefix(curLine, [IfNDefMarker]) then
          IfDefs.AddObject(Copy(curLine, Length(IfNDefMarker)+2, Length(curLine)-Length(IfNDefMarker)-2), TObject(False))
        else if StrHasPrefix(curLine, [ElseMarker]) then
          IfDefs.Objects[IfDefs.Count-1] := TObject(not Boolean(IfDefs.Objects[IfDefs.Count-1]))
        else if StrHasPrefix(curLine, [EndIfMarker]) then
          IfDefs.Delete(IfDefs.Count-1)
        else if StrHasPrefix(curLine, [DefineMarker]) then
          Add(TDefine.Create(Copy(curLine, Length(DefineMarker)+2, Length(curLine)-Length(DefineMarker)-2), IfDefs));
      end;
  finally
    IfDefs.Free;
  end;
end;

function TDefinesList.GetItems(index: integer): TDefine;
begin
  Result := TDefine(inherited Items[index]);
end;

function TDefinesList.IsDefined(const Condition: string; TargetDefines: TStrings;
  DefineLimit : Integer = -1): Boolean;
var
  I : Integer;
  Define : TDefine;
begin
  if DefineLimit = -1 then
    DefineLimit := Count
  else
  if DefineLimit > Count then
    DefineLimit := Count;

  Result := False;
  Define := nil;
  for i := 0 to DefineLimit - 1 do
  begin
    if SameText(Items[I].Name, Condition) then
    begin
      Result := True;
      Define := Items[I];
      Break;
    end;
  end;

  // If the condition is not defined by its name, maybe it
  // is as a consequence of the target we use
  if not Result then
    Result := TargetDefines.IndexOf(Condition) > -1;

  // If the condition is defined, then all the IfDefs in which
  // it is enclosed must also be defined but only before the
  // current define
  if Result and Assigned(Define) then
    for I := 0 to Define.IfDefs.Count - 1 do
    begin
      if Boolean(Define.IfDefs.Objects[I]) then
        Result := Result and IsDefined(Define.IfDefs[I], TargetDefines, IndexOf(Define))
      else
        Result := Result and not IsDefined(Define.IfDefs[I], TargetDefines, IndexOf(Define));
    end
end;

procedure TDefinesList.SetItems(index: integer; const Value: TDefine);
begin
  inherited Items[index] := Value;
end;

end.
