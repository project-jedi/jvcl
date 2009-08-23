unit GenerateReplacements;

{$I jvcl.inc}

interface

uses
  Contnrs, Classes,
  JclSimpleXml,
  PackageInformation;

type
  TClxReplacement = class (TObject)
  private
    FOriginal: string;
    FReplacement: string;
  public
    constructor Create(Node : TJclSimpleXmlElem); overload;
    function DoReplacement(const Filename: string): string;
    property Original  : string read FOriginal;
    property Replacement : string read FReplacement;
  end;

  TClxReplacementList = class (TObjectList)
  private
    IgnoredFiles: TStringList;

    function GetItems(index: integer): TClxReplacement;
    procedure SetItems(index: integer; const Value: TClxReplacement);
  public
    constructor Create(Node : TJclSimpleXmlElem); overload;
    destructor Destroy; override;

    function DoReplacement(const Filename: string): string;

    property Items[index : integer] : TClxReplacement read GetItems write SetItems;
  end;

implementation

uses
  SysUtils,
  JclStrings;
(*uses
  Windows, SysUtils, ShellApi, FileUtils,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JclBase,
  JclDateTime, JclStrings, JclFileUtils, JclSysUtils, JclLogic,
  ConditionParser;*)

{ TClxReplacement }

constructor TClxReplacement.Create(Node: TJclSimpleXmlElem);
begin
  inherited Create;
  FOriginal := Node.Properties.ItemNamed['original'].Value;
  FReplacement := Node.Properties.ItemNamed['replacement'].Value;
end;

function TClxReplacement.DoReplacement(const Filename: string): string;
var
  TmpResult: string;
begin
  TmpResult := Filename;
  StrReplace(TmpResult, Original, Replacement, [rfIgnoreCase]);
  Result := TmpResult;
end;

{ TClxReplacementList }

constructor TClxReplacementList.Create(Node: TJclSimpleXmlElem);
var
  i : integer;
begin
  inherited Create(True);
  IgnoredFiles := TStringList.Create;
  IgnoredFiles.Sorted := True;
  IgnoredFiles.Duplicates := dupIgnore;

  if Assigned(Node) then
    for i := 0 to Node.Items.Count - 1 do
    begin
      if Node.Items[i].Name = 'replacement' then
        Add(TClxReplacement.Create(Node.Items[i]))
      else if Node.Items[i].Name = 'ignoredFile' then
        IgnoredFiles.Add(ExtractFileName(Node.Items[i].Properties.Value('filename')));
    end;
end;

destructor TClxReplacementList.Destroy;
begin
  IgnoredFiles.Free;

  inherited Destroy;
end;

function TClxReplacementList.DoReplacement(
  const Filename: string): string;
var
  i : Integer;
begin
  Result := Filename;

  // Only do the replacement if the file is not to be ignored
  if not IgnoredFiles.Find(ExtractFileName(Filename), i) then
  begin
    for i := 0 to Count -1 do
      Result := Items[i].DoReplacement(Result);
  end;
end;

function TClxReplacementList.GetItems(
  index: integer): TClxReplacement;
begin
  Result := TClxReplacement(inherited Items[index]);
end;

procedure TClxReplacementList.SetItems(index: integer;
  const Value: TClxReplacement);
begin
  inherited Items[index] := Value;
end;

end.
