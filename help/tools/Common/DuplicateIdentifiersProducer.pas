unit DuplicateIdentifiersProducer;

interface

uses
  Classes,
  JVCLHelpUtils, DtxDiagnoser;

type
  TDuplicateIdentifiersProducer = class(TTask)
  private
    FDuplicateIdentifiersFileName: string;
    FLowerCaseIdentifiers: TStringList;
    FDuplicateIdentifiers: TStringList;
    FDtxDiagnoser: TDtxDiagnoser;
  protected
    function Process: Boolean;
    function Save: Boolean;

    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;

    property DuplicateIdentifiersFileName: string read FDuplicateIdentifiersFileName write
      FDuplicateIdentifiersFileName;
    property DtxDiagnoser: TDtxDiagnoser read FDtxDiagnoser write FDtxDiagnoser;
  end;

implementation

{ TDuplicateIdentifiersProducer }

constructor TDuplicateIdentifiersProducer.Create(
  ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);

  FLowerCaseIdentifiers := TStringList.Create;
  FDuplicateIdentifiers := TStringList.Create;

  FDuplicateIdentifiers.Sorted := True;
  FDuplicateIdentifiers.CaseSensitive := True;
  FDuplicateIdentifiers.Duplicates := dupIgnore;

  FLowerCaseIdentifiers.Sorted := True;
  FLowerCaseIdentifiers.CaseSensitive := False;
  FLowerCaseIdentifiers.Duplicates := dupIgnore;
end;

destructor TDuplicateIdentifiersProducer.Destroy;
begin
  FDuplicateIdentifiers.Free;
  FLowerCaseIdentifiers.Free;

  inherited Destroy;
end;

function TDuplicateIdentifiersProducer.DoExecute: Boolean;
begin
  Result := Process and Save;
end;

function TDuplicateIdentifiersProducer.GetTaskDescription: string;
begin
  Result := 'Generating file with invalid words..';
end;

type
  TStringListAccess = class(TStringList);

function TDuplicateIdentifiersProducer.Process: Boolean;
var
  I: Integer;
  S: string;
  Index: Integer;
  Count: Integer;
begin
  Result := True;
  StatusMsg('Processing..');

  Count := 0;

  for I := 0 to DtxDiagnoser.Identifiers.Count - 1 do
  begin
    Progress(I, DtxDiagnoser.Identifiers.Count);
    S := DtxDiagnoser.Identifiers[i];
    if FLowerCaseIdentifiers.Find(S, Index) then
    begin
      Inc(Count);
      FDuplicateIdentifiers.Add(S);
      FDuplicateIdentifiers.Add(FLowerCaseIdentifiers[Index]);
    end
    else
      TStringListAccess(FLowerCaseIdentifiers).InsertItem(Index, S, nil);
  end;

  HintMsgFmt('%d duplicate identifiers', [Count]);
end;

function TDuplicateIdentifiersProducer.Save: Boolean;
begin
  Result := True;
  StatusMsg('Saving..');
  FDuplicateIdentifiers.SaveToFile(DuplicateIdentifiersFileName);
end;

end.

 