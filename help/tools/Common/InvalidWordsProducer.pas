unit InvalidWordsProducer;

interface

uses
  Windows, Classes,
  JVCLHelpUtils, DtxDiagnoser;

type
  TInvalidWordsProducer = class(TTask)
  private
    FDictionaryFileName: string;
    FInvalidWordsFileName: string;
    FDtxDiagnoser: TDtxDiagnoser;
    FDictionary: TStringList;
    FInvalidWords: TStringList;
    procedure DiffStrings(Source, CheckList, Dest: TStrings);
  protected
    function Init: Boolean;
    function Process: Boolean;
    function Save: Boolean;

    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;

    property DictionaryFileName: string read FDictionaryFileName write FDictionaryFileName;
    property InvalidWordsFileName: string read FInvalidWordsFileName write FInvalidWordsFileName;
    property DtxDiagnoser: TDtxDiagnoser read FDtxDiagnoser write FDtxDiagnoser;
  end;

implementation

uses SysUtils;

{ TInvalidWordsProducer }

constructor TInvalidWordsProducer.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);

  FDictionary := TStringList.Create;
  FInvalidWords := TStringList.Create;

  FDictionary.Sorted := True;
  FDictionary.Duplicates := dupIgnore;
  FDictionary.CaseSensitive := False;

  FInvalidWords.Sorted := True;
  FInvalidWords.Duplicates := dupIgnore;
  FInvalidWords.CaseSensitive := False;
end;

destructor TInvalidWordsProducer.Destroy;
begin
  FDictionary.Free;
  FInvalidWords.Free;

  inherited Destroy;
end;

procedure TInvalidWordsProducer.DiffStrings(Source, CheckList, Dest: TStrings);
var
  I, J: Integer;
  C: Integer;
begin
  I := 0;
  J := 0;
  while (I < Source.Count) and (J < CheckList.Count) do
  begin
    Progress(I + J, Source.Count + CheckList.Count);
    C := AnsiCompareText(Source[i], CheckList[J]);
    if C < 0 then
    begin
      Dest.Add(Source[i]);
      Inc(I);
    end
    else
      if C > 0 then
    begin
      Inc(J);
    end
    else
    begin
      Inc(I);
      Inc(J);
    end;
  end;

  while I < Source.Count do
  begin
    Progress(I, Source.Count);
    Dest.Add(Source[i]);
    Inc(I);
  end
end;

function TInvalidWordsProducer.DoExecute: Boolean;
begin
  Result := Init and Process and Save;
end;

function TInvalidWordsProducer.GetTaskDescription: string;
begin
  Result := 'Generating file with invalid words..';
end;

function TInvalidWordsProducer.Init: Boolean;
begin
  Result := True;
  StatusMsg('Initialization..');

  FDictionary.LoadFromFile(DictionaryFileName);
  HintMsgFmt('Dictionary loaded (%d words)', [FDictionary.Count]);
end;

function TInvalidWordsProducer.Process: Boolean;
begin
  Result := True;
  StatusMsg('Processing..');
  DiffStrings(DtxDiagnoser.Words, FDictionary, FInvalidWords);
  HintMsgFmt('%d invalid words', [FInvalidWords.Count]);
end;

function TInvalidWordsProducer.Save: Boolean;
begin
  Result := True;
  StatusMsg('Saving..');
  FInvalidWords.SaveToFile(InvalidWordsFileName);
end;

end.

 