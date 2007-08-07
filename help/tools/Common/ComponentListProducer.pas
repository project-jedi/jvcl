unit ComponentListProducer;

interface

uses
  Classes, ContNrs, JVCLHelpUtils, DtxParser;

type
  TComponentListProducer = class
  private
    FComponentInfos: TComponentInfos;
  protected
    function CleanupComponents: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddPasFile(ADtxList: TDtxList): Boolean;
    function AddComponent(ADtxTopic: TDtxTopic): Boolean;
    property ComponentInfos: TComponentInfos read FComponentInfos;
  end;

implementation

uses
  SysUtils, ParserTypes;

const
  cifSymbolType = [cifComponent, cifClass, cifRoutine];

//=== Local procedures =======================================================

function HasDot(const S: string): Boolean;
begin
  Result := StrScan(PChar(S), '.') <> nil;
end;

//=== { TComponentListProducer } =============================================

constructor TComponentListProducer.Create;
begin
  inherited Create;
  FComponentInfos := TComponentInfos.Create;
end;

destructor TComponentListProducer.Destroy;
begin
  FComponentInfos.Free;
  inherited Destroy;
end;

function TComponentListProducer.AddComponent(ADtxTopic: TDtxTopic): Boolean;
var
  CompName: string;
  Comp: TComponentInfo;
begin
  CompName := ADtxTopic.Name;
  Result := not IsNullStr(CompName) and not HasDot(CompName);
  if not Result then
    Exit;

  Comp := FComponentInfos.Add(CompName);
  ParseGroupString(Comp, ADtxTopic.Group);
  if IsNullStr(ADtxTopic.TitleImg) then
    Comp.Image := 'NO_ICON'
  else
    Comp.Image := ADtxTopic.TitleImg;
  Comp.Flags := Comp.Flags + ADtxTopic.Flags;
  Comp.Summary := ADtxTopic.Summary;

  if (cifIgnore in Comp.Flags) or (Comp.Flags = []) then
  begin
    FComponentInfos.Remove(Comp);
    Exit;
  end;
  if (cifSymbolType * Comp.Flags = []) and (cifAssignedToGroup in Comp.Flags) then
    Comp.Flags := Comp.Flags + [cifComponent];
end;

function TComponentListProducer.AddPasFile(ADtxList: TDtxList): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to ADtxList.Count - 1 do
    if ADtxList[i].HasJVCLInfo then
      AddComponent(ADtxList[i]);
end;

function TComponentListProducer.CleanupComponents: Boolean;
var
  OrgCount: Integer;
begin
  Result := True;
  StatusMsg('Removing invalid components...');
  OrgCount := FComponentInfos.Count;
  FComponentInfos.RemoveInvalid;
  HintMsgFmt('Kept %d of %d components.', [FComponentInfos.Count, OrgCount]);
end;

end.
