unit ImagesProducer;

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

uses
  Graphics, SysUtils,
  pngimage;

//=== Local procedures =======================================================

procedure MakePNGAndSave(BMP: TBitmap; const FileName: string);
var
  PNG: TPngObject;
begin
  PNG := TPNGObject.Create;
  try
    PNG.Assign(BMP);
    PNG.CompressionLevel := 9;
    PNG.SaveToFile(FileName);
  finally
    PNG.Free;
  end;
end;

//=== { TImagesProducer } ====================================================

function TImagesProducer.CanStart: Boolean;
begin
  Result := CheckDir(SourceDir) and CheckDir(DestDir) and CheckFile(RegisteredClassesFileName);
end;

procedure TImagesProducer.CheckComplete(ImageFileNames: TStrings);
var
  ImageNames: TStringList;
  WithoutImage: TStringList;
  I: Integer;
begin
  StatusMsg('Checking whether the images are complete');
  ImageNames := TStringList.Create;
  WithoutImage := TStringList.Create;
  try
    FRegisteredClasses.LoadFromFile(RegisteredClassesFileName);
    for I := 0 to ImageFileNames.Count - 1 do
      ImageNames.Add(ChangeFileExt(ExtractFileName(ImageFileNames[i]), ''));

    ImageNames.Sorted := True;

    DiffLists(FRegisteredClasses, ImageNames, nil, nil, WithoutImage);
    for I := 0 to WithoutImage.Count - 1 do
      WarningMsgFmt('Registered class %s has no image', [WithoutImage[i]]);
  finally
    ImageNames.Free;
    WithoutImage.Free;
  end;
end;

constructor TImagesProducer.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FRegisteredClasses := TStringList.Create;
  FRegisteredClasses.Sorted := True;
  FRegisteredClasses.Duplicates := dupIgnore;
  FRegisteredClasses.CaseSensitive := False;
end;

destructor TImagesProducer.Destroy;
begin
  FRegisteredClasses.Free;
  inherited Destroy;
end;

function TImagesProducer.DoExecute: Boolean;
var
  ImageFiles: TStringList;
  I: Integer;
begin
  Result := True;

  ImageFiles := TStringList.Create;
  try
    StatusMsg('Retrieving files from image directory..');
    GetAllFilesFrom(SourceDir, '*.bmp', ImageFiles);

    CheckComplete(ImageFiles);

    StatusMsg('Generating images..');
    for I := 0 to ImageFiles.Count - 1 do
    begin
      Progress(I, ImageFiles.Count);
      ProcessFile(ImageFiles[I]);
    end;
  finally
    ImageFiles.Free;
  end;
end;

function TImagesProducer.GetTaskDescription: string;
begin
  Result := 'Generating images..';
end;

function TImagesProducer.IsRegisteredClassImage(
  const AFileName: string): Boolean;
var
  StrippedFileName: string;
begin
  StrippedFileName := ChangeFileExt(ExtractFileName(AFileName), '');

  Result := FRegisteredClasses.IndexOf(StrippedFileName) >= 0;
end;

function TImagesProducer.ProcessFile(const SrcFileName: string): Boolean;
var
  BMP: TBitmap;
  Col: TColor;
  X: Integer;
  Y: Integer;
  DestFileName: string;
begin
  Result := True;

  DestFileName := ExtractFileName(SrcFileName);
  if IsRegisteredClassImage(DestFileName) then
    DestFileName := RegisteredClassNameToImageFileName(DestFileName);

  BMP := TBitmap.Create;
  try
    BMP.LoadFromFile(SrcFileName);
    BMP.TransparentMode := tmAuto;
    Col := BMP.Canvas.Pixels[0, BMP.Height - 1];
    for Y := 0 to BMP.Height - 1 do
    begin
      for X := 0 to BMP.Width - 1 do
      begin
        if BMP.Canvas.Pixels[X, Y] = Col then
          BMP.Canvas.Pixels[X, Y] := clBtnFace;
      end;
    end;
    // Save new file
    BMP.SaveToFile(DestDir + DestFileName);
    // Transform to PNG and save
    MakePNGAndSave(BMP, DestDir + ChangeFileExt(DestFileName, '.png'));
  finally
    BMP.Free;
  end;
end;

end.

