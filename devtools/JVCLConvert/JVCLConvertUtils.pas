unit JVCLConvertUtils;

interface
uses
  SysUtils, Classes, JvPropertyStore;

type
  TAppOptions = class(TJvCustomPropertyStore)
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
    constructor Create(AOwner : TComponent); override;
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

{ TAppOptions }

constructor TAppOptions.Create(AOwner : TComponent);
begin
  inherited create (AOwner);
  RootDirectory :=  '';
  FileMask := '*.dpr;*.dpk;*.pas;*.dfm';
  DATFile := '';
  Backup  := true;
  WholeWords := true;
  ReplaceFileNames := true;
  Simulate := false;
  FileMasks := 'Delphi files (*.dpr;*.dpk;*.pas;*.dfm)'#27'BCB files (*.dpr;*.bpk;*.pas;*.dfm;*.cpp;*.h;*.hpp)'#27'All files (*.*)';
end;


end.
