unit JVCLConvertUtils;

interface
uses
  SysUtils;

type
  TAppOptions = class
  private
    FFilename:string;
    FWholeWords: boolean;
    FSimulate: boolean;
    FReplaceFilenames: boolean;
    FBackup: boolean;
    FFileMasks: string;
    FRootDirectory: string;
    FFileMask: string;
    FDATFile: string;
  public
    constructor Create(const Filename:string);
    destructor Destroy; override;
    procedure LoadSettings;
    procedure SaveSettings;
  published
    property RootDirectory:string read FRootDirectory write FRootDirectory;
    property FileMask:string read FFileMask write FFileMask;
    property FileMasks:string read FFileMasks write FFileMasks;
    property DATFile:String read FDATFile write FDATFile;
    property Backup:boolean read FBackup write FBackup;
    property WholeWords:boolean read FWholeWords write FWholeWords;
    property ReplaceFilenames:boolean read FReplaceFilenames write FReplaceFilenames;
    property Simulate:boolean read FSimulate write FSimulate;
  end;

implementation
uses
  IniFiles;

{ TAppOptions }

constructor TAppOptions.Create(const Filename: string);
begin
  inherited Create;
  FFilename := Filename;
  LoadSettings;
end;

destructor TAppOptions.Destroy;
begin
  SaveSettings;
  inherited;
end;

procedure TAppOptions.LoadSettings;
begin
  with TIniFile.Create(FFilename) do
  try
    RootDirectory := ReadString('Settings', 'Path', '');
    FileMask := ReadString('Settings', 'Mask', '*.dpr;*.dpk;*.pas;*.dfm');
    DATFile := ReadString('Settings', 'DATFile', '');
    Backup  := ReadBool('Settings', 'Backup', true);
    WholeWords := ReadBool('Settings', 'WholeWords', true);
    ReplaceFileNames := ReadBool('Settings', 'ReplaceFileNames', true);
    Simulate := ReadBool('Settings', 'Simulate', false);
    FileMasks := StringReplace(ReadString('Settings', 'Filemasks',
        'Delphi files (*.dpr;*.dpk;*.pas;*.dfm)'#27'BCB files (*.dpr;*.bpk;*.pas;*.dfm;*.cpp;*.h;*.hpp)'#27'All files (*.*)'), #27, #13#10,[rfReplaceAll]);
  finally
    Free;
  end;
end;

procedure TAppOptions.SaveSettings;
begin
  with TIniFile.Create(FFilename) do
  try
    WriteString('Settings', 'Path', RootDirectory);
    WriteString('Settings', 'Mask', FileMask);
    WriteString('Settings', 'DATFile',DATFile);
    WriteBool('Settings', 'Backup', Backup);
    WriteBool('Settings', 'WholeWords', WholeWords);
    WriteBool('Settings', 'ReplaceFileNames', ReplaceFileNames);
    WriteBool('Settings', 'Simulate', Simulate);
    WriteString('Settings', 'Filemasks',StringReplace(FileMasks, #13#10, #27,[rfReplaceAll]));
  finally
    Free;
  end;
end;

end.
