unit RegisteredClassesProducer;

interface

uses
  Classes, JVCLHelpUtils;

type
  TRegisteredClassesProducer = class(TTask)
  private
    FJVCLxxDesignDir: string;
    FRegisteredClassesFileName: string;
    function ProcessFile(const AFileName: string; AList: TStrings): Boolean;
    procedure UpdateRegisteredClasses(NewList: TStrings);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property RegisteredClassesFileName: string read FRegisteredClassesFileName write FRegisteredClassesFileName;
    property JVCLxxDesignDir: string read FJVCLxxDesignDir write FJVCLxxDesignDir;
  end;

implementation

uses
  DelphiParser;

//=== { TRegisteredClassesProducer } =========================================

function TRegisteredClassesProducer.CanStart: Boolean;
begin
  Result := CheckDir(FJVCLxxDesignDir) and not IsNullStr(RegisteredClassesFileName);
end;

function TRegisteredClassesProducer.DoExecute: Boolean;
var
  JVCL3DesignPasFiles: TStringList;
  ARegisteredClasses: TStringList;
  I: Integer;
begin
  Result := True;
  
  JVCL3DesignPasFiles := TStringList.Create;
  ARegisteredClasses := TStringList.Create;
  try
    JVCL3DesignPasFiles.Sorted := True;
    ARegisteredClasses.Sorted := True;

    StatusMsg('Collecting files');
    GetAllFilesFrom(JVCLxxDesignDir, '*.pas', JVCL3DesignPasFiles);

    StatusMsg('Parsing design time files');
    for I := 0 to JVCL3DesignPasFiles.Count - 1 do
    begin
      Progress(I, JVCL3DesignPasFiles.Count);
      ProcessFile(JVCL3DesignPasFiles[I], ARegisteredClasses);
    end;
    UpdateRegisteredClasses(ARegisteredClasses);
  finally
    JVCL3DesignPasFiles.Free;
    ARegisteredClasses.Free;
  end;
end;

function TRegisteredClassesProducer.GetTaskDescription: string;
begin
  Result := 'Generate reg. classes file..';
end;

function TRegisteredClassesProducer.ProcessFile(
  const AFileName: string; AList: TStrings): Boolean;
begin
  with TRegisteredClassesParser.Create do
  try
    Result := ExecuteFile(AFileName);
    if Result then
      AList.AddStrings(List)
    else
      ErrorMsgFmt('Parse error %s - %s', [AFileName, ErrorMsg]);
  finally
    Free;
  end;
end;

procedure TRegisteredClassesProducer.UpdateRegisteredClasses(
  NewList: TStrings);
var
  OldList: TStringList;
  Added, Removed: TStringList;
  I: Integer;
begin
  OldList := TStringList.Create;
  try
    OldList.Sorted := True;
    OldList.LoadFromFile(RegisteredClassesFileName);

    Added := TStringList.Create;
    Removed := TStringList.Create;
    try
      Added.Sorted := True;
      Removed.Sorted := True;

      DiffLists(OldList, NewList, nil, Added, Removed);
      if (Added.Count = 0) and (Removed.Count = 0) then
        HintMsg('Nothing changed');
      if Removed.Count > 0 then
        for I := 0 to Removed.Count - 1 do
          HintMsgFmt('Removed <%s>', [Removed[i]]);
      if Added.Count > 0 then
        for I := 0 to Added.Count - 1 do
          HintMsgFmt('Added <%s>', [Added[i]]);

      NewList.SaveToFile(RegisteredClassesFileName);
    finally
      Added.Free;
      Removed.Free;
    end;
  finally
    OldList.Free;
  end;
end;

end.
