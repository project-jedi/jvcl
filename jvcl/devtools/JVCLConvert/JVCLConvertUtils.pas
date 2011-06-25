unit JVCLConvertUtils;

interface

uses
  SysUtils, Classes, JvPropertyStore;

type
  TAppOptions = class(TJvCustomPropertyStore)
  private
    FBackup: Boolean;
    FDATFile: string;
    FFileMask: string;
    FFileMasks: string;
    FFilename: string;
    FIgnoreInsideStrings: Boolean;
    FIgnoreInsideComments: Boolean;
    FReplaceFilenames: Boolean;
    FRootDirectory: string;
    FSimulate: Boolean;
    FWholeWords: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Backup: Boolean read FBackup write FBackup default True;
    property DATFile: string read FDATFile write FDATFile;
    property FileMask: string read FFileMask write FFileMask;
    property FileMasks: string read FFileMasks write FFileMasks;
    property IgnoreInsideStrings: Boolean read FIgnoreInsideStrings write FIgnoreInsideStrings default True;
    property IgnoreInsideComments: Boolean read FIgnoreInsideComments write FIgnoreInsideComments default True;
    property ReplaceFilenames: Boolean read FReplaceFilenames write FReplaceFilenames default True;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property Simulate: Boolean read FSimulate write FSimulate default False;
    property WholeWords: Boolean read FWholeWords write FWholeWords default True;
  end;

implementation

{ TAppOptions }

constructor TAppOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootDirectory := '';
  FileMask := '*.dpr;*.dpk;*.pas;*.dfm';
  DATFile := '';
  Backup := True;
  WholeWords := True;
  ReplaceFilenames := True;
  Simulate := False;
  FileMasks :=
    'Delphi files (*.dpr;*.dpk;*.pas;*.dfm)'#27'BCB files (*.dpr;*.bpk;*.pas;*.dfm;*.cpp;*.h;*.hpp)'#27'All files (*.*)';
  FIgnoreInsideComments := True;
  FIgnoreInsideStrings := True;
end;

end.
