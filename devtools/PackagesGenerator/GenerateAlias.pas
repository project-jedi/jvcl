unit GenerateAlias;

{$I jvcl.inc}

interface

uses
  Contnrs, Classes,
  JclSimpleXml,
  PackageInformation;

type
  TAlias = class (TObject)
  private
    FValue: string;
    FName: string;
    FValueAsTStrings : TStringList;
    function GetValueAsTStrings: TStrings;
  public
    constructor Create(Node : TJclSimpleXmlElem); overload;
    destructor Destroy; override;

    property Name  : string read FName;
    property Value : string read FValue;
    property ValueAsTStrings : TStrings read GetValueAsTStrings;
  end;

  TAliasList = class (TObjectList)
  private
    function GetItemsByName(name: string): TAlias;
    function GetItems(index: integer): TAlias;
    procedure SetItems(index: integer; const Value: TAlias);
  public
    constructor Create(Node : TJclSimpleXmlElem); overload;

    property Items[index : integer] : TAlias read GetItems write SetItems;
    property ItemsByName[name : string] : TAlias read GetItemsByName; default;
  end;

implementation

uses
  SysUtils,
  JclStrings;

{ TAlias }

constructor TAlias.Create(Node: TJclSimpleXmlElem);
begin
  inherited Create;
  FName := AnsiLowerCase(Node.Properties.ItemNamed['name'].Value);
  FValue := AnsiLowerCase(Node.Properties.ItemNamed['value'].Value);
  FValueAsTStrings := nil;
end;

destructor TAlias.Destroy;
begin
  FValueAsTStrings.Free;
  inherited Destroy;
end;

function TAlias.GetValueAsTStrings: TStrings;
begin
  if not Assigned(FValueAsTStrings) then
    FValueAsTStrings := TStringList.Create;

  StrToStrings(Value, ',', FValueAsTStrings, false);
  Result := FValueAsTStrings;
end;

{ TAliasList }

constructor TAliasList.Create(Node: TJclSimpleXmlElem);
var
  i : integer;
begin
  inherited Create(True);
  if Assigned(Node) then
    for i := 0 to Node.Items.Count - 1 do
      if Node.Items[i].Name = 'alias' then
    begin
      Add(TAlias.Create(Node.Items[i]));
    end;
end;

function TAliasList.GetItems(index: integer): TAlias;
begin
  Result := TAlias(inherited Items[index]);
end;

function TAliasList.GetItemsByName(name: string): TAlias;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SameText(TAlias(Items[i]).Name, name) then
    begin
      Result := TAlias(Items[i]);
      Break;
    end;
end;

procedure TAliasList.SetItems(index: integer; const Value: TAlias);
begin
  inherited Items[index] := Value;
end;

end.
