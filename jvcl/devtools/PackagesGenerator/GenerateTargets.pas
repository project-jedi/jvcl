unit GenerateTargets;

{$I jvcl.inc}

interface

uses
  Contnrs, Classes,
  JclSimpleXml;

type
  TTarget = class (TObject)
  private
    FName   : string;
    FDir    : string;
    FPName  : string;
    FPDir   : string;
    FEnv    : string;
    FVer    : string;
    FDefines: TStringList;
    FPathSep: string;
    FIsCLX  : Boolean;
    FIsBDS  : Boolean;
    FIsDotNet: Boolean;
    function GetDir: string;
    function GetEnv: string;
    function GetPDir: string;
    function GetVer: string;
  public
    constructor Create(Node : TJclSimpleXmlElem); overload;
    destructor Destroy; override;

    property Name   : string      read FName;
    property Dir    : string      read GetDir;
    property PName  : string      read FPName;
    property PDir   : string      read GetPDir;
    property Env    : string      read GetEnv;
    property Ver    : string      read GetVer;
    property Defines: TStringList read FDefines;
    property PathSep: string      read FPathSep;
    property IsCLX  : Boolean     read FIsCLX;
    property IsBDS  : Boolean     read FIsBDS;
    property IsDotNet : Boolean   read FIsDotNet;
  end;

  TTargetList = class (TObjectList)
  private
    function GetItemsByName(name: string): TTarget;
    function GetItems(index: integer): TTarget;
    procedure SetItems(index: integer; const Value: TTarget);
  public
    constructor Create(Node : TJclSimpleXmlElem); overload;
    procedure EnumerateTargets(targets : TStrings);

    property Items[index : integer] : TTarget read GetItems write SetItems;
    property ItemsByName[name : string] : TTarget read GetItemsByName; default;
  end;

implementation

uses
  SysUtils,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JclBase,
  JclStrings;

type
  TTargetDefine = record
    TargetName: string;
    Define: string;
  end;

const
  TargetDefines: array [0..16] of TTargetDefine =
                (
                  (TargetName: 'c6'; Define: 'VER140'),
                  (TargetName: 'd6'; Define: 'VER140'),
                  (TargetName: 'd7'; Define: 'VER150'),
                  (TargetName: 'd9'; Define: 'VER170'),
                  (TargetName: 'd10'; Define: 'VER180'),
                  (TargetName: 'd11'; Define: 'VER180,VER185'),
                  (TargetName: 'd12'; Define: 'VER200'),
                  (TargetName: 'd14'; Define: 'VER210'),
                  (TargetName: 'd15'; Define: 'VER220'),
                  (TargetName: 'd16'; Define: 'VER230'),
                  (TargetName: 'd17'; Define: 'VER240'),
                  (TargetName: 'd18'; Define: 'VER250'),
                  (TargetName: 'd19'; Define: 'VER260'),
                  (TargetName: 'd20'; Define: 'VER270'),
                  (TargetName: 'd21'; Define: 'VER280'),
                  (TargetName: 'd22'; Define: 'VER290'),
                  (TargetName: 'd23'; Define: 'VER300')
				  );

{ TTarget }

constructor TTarget.Create(Node: TJclSimpleXmlElem);
var
  I: Integer;
  Tmp: TStringList;
begin
  inherited Create;
  FName := AnsiLowerCase(Node.Properties.ItemNamed['name'].Value);
  if Assigned(Node.Properties.ItemNamed['dir']) then
    FDir := Node.Properties.ItemNamed['dir'].Value;
  if Assigned(Node.Properties.ItemNamed['pname']) then
    FPName := AnsiLowerCase(Node.Properties.ItemNamed['pname'].Value);
  if Assigned(Node.Properties.ItemNamed['pdir']) then
    FPDir := Node.Properties.ItemNamed['pdir'].Value;
  if Assigned(Node.Properties.ItemNamed['env']) then
    FEnv := AnsiUpperCase(Node.Properties.ItemNamed['env'].Value);
  if Assigned(Node.Properties.ItemNamed['ver']) then
    FVer := AnsiLowerCase(Node.Properties.ItemNamed['ver'].Value);

  FDefines := TStringList.Create;
  if Assigned(Node.Properties.ItemNamed['defines']) then
    StrToStrings(Node.Properties.ItemNamed['defines'].Value,
                 ',',
                 FDefines,
                 False);

  FPathSep := '\';
  if Assigned(Node.Properties.ItemNamed['pathsep']) then
    FPathSep := Node.Properties.ItemNamed['pathsep'].Value;
  FIsCLX := False;
  if Assigned(Node.Properties.ItemNamed['IsCLX']) then
    FIsCLX := Node.Properties.ItemNamed['IsCLX'].BoolValue;
  FIsBDS := False;
  if Assigned(Node.Properties.ItemNamed['IsBDS']) then
    FIsBDS := Node.Properties.ItemNamed['IsBDS'].BoolValue;
  FIsDotNet := False;
  if Assigned(Node.Properties.ItemNamed['IsDotNet']) then
    FIsDotNet := Node.Properties.ItemNamed['IsDotNet'].BoolValue;

  if StrHasSuffix(Name, ['_x64']) then
    FDefines.Add('WIN64')
  else
    FDefines.Add('WIN32');
  FDefines.Add('CONDITIONALEXPRESSIONS');

  for I := Low(TargetDefines) to High(TargetDefines) do
    if SameText(TargetDefines[I].TargetName, Name) then
    begin
      Tmp := TStringList.Create;
      try
        StrToStrings(TargetDefines[I].Define,
                 ',',
                 Tmp,
                 False);

        FDefines.AddStrings(Tmp);
      finally
        Tmp.Free;
      end;
    end;
end;

destructor TTarget.Destroy;
begin
  FDefines.Free;
  inherited Destroy;
end;

function TTarget.GetDir: string;
begin
  if FDir <> '' then
    Result := FDir
  else
    Result := Name;
end;

function TTarget.GetEnv: string;
var
  I: Integer;
begin
  if FEnv <> '' then
    Result := FEnv
  else if Length(Name) > 1 then
  begin
    I := 1;
    while (I < Length(Name)) and not CharInSet(Name[I], ['0'..'9']) do
      Inc(I);
    if CharInSet(Name[I], ['0'..'9']) then
      Dec(I);
    Result := AnsiUpperCase(Copy(Name,1,I));
  end
  else
    Result := '';
end;

function TTarget.GetPDir: string;
begin
  if FPDir <> '' then
    Result := FPDir
  else
    Result := FPName;
end;

function TTarget.GetVer: string;
var
  Start, I : Integer;
begin
  if FVer <> '' then
    Result := FVer
  else if Length(Name)>1 then
  begin
    Start := 2;
    while (Start < Length(Name)) and not CharInSet(Name[Start], ['0'..'9']) do
      Inc(Start);
    I := Start;
    while (I < Length(Name)) and CharInSet(Name[I], ['0'..'9']) do
      Inc(I);
    if I < Length(name) then
      Dec(I);
    Result := AnsiLowerCase(Copy(Name, Start, I-Start+1));
  end
  else
    Result := '';
end;

{ TTargetList }

constructor TTargetList.Create(Node: TJclSimpleXmlElem);
var
  i : integer;
begin
  inherited Create(True);
  if Assigned(Node) then
    for i := 0 to Node.Items.Count - 1 do
      Add(TTarget.Create(Node.Items[i]));
end;

procedure TTargetList.EnumerateTargets(targets: TStrings);
var
  i : integer;
begin
  targets.clear;
  for i := 0 to Count - 1 do
    targets.Add(Items[I].Name);
end;

function TTargetList.GetItems(index: integer): TTarget;
begin
  Result := TTarget(inherited Items[index]);
end;

function TTargetList.GetItemsByName(name: string): TTarget;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SameText(TTarget(Items[i]).Name, name) then
    begin
      Result := TTarget(Items[i]);
      Break;
    end;
end;

procedure TTargetList.SetItems(index: integer; const Value: TTarget);
begin
  inherited Items[index] := Value;
end;

end.
