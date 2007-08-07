unit DtxFilesPreProcesssor;

interface

uses
  JVCLHelpUtils,
  Classes;

type
  TImagesProducer = class(TTask)
  private
    FDestDir: string;
    FSourceDir: string;
    FRegisteredClasses: TStringList;
    FRegisteredClassesFileName: string;
    function IsRegisteredClassImage(const AFileName: string): Boolean;
  protected
    function ProcessFile(const SrcFileName: string): Boolean;
    procedure CheckComplete(ImageFileNames: TStrings);

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property SourceDir: string read FSourceDir write FSourceDir;
    property DestDir: string read FDestDir write FDestDir;
    property RegisteredClassesFileName: string read FRegisteredClassesFileName write FRegisteredClassesFileName;
  end;

implementation

end.
 