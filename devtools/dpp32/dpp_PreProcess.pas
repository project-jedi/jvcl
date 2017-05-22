{**************************************************************************************************}
{                                                                                                  }
{ Delphi language Preprocessor (dpp32)                                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is dpp_PreProcess.pas                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Projects home page, located at           }
{ http://www.sourceforge.net/projects/dpp32                                                        }
{                                                                                                  }
{**************************************************************************************************}
unit dpp_PreProcess;
interface
uses
  SysUtils, Classes, dpp_Macros, dpp_FileInfos, dpp_Utils;
const
  PreProcessorVersion = '1.1';
type
  TErrorWarningEvent = procedure(Sender: TObject; const Message: string) of object;
  TErrorWarningExEvent = procedure(Sender: TObject; Error: Boolean;
    const Filename, Msg: string; Line: Integer) of object;

  { IPreProcessorFileSys: all file names are full qualified file names. }
  IDppVirtualFileSys = interface
    { InitPreProcessorFileSys is called for every BeginPreProcessing. }
    procedure InitPreProcessorFileSys;

    { IsVirtualFile should return True if the file is in an edit buffer. }
    function IsVirtualFile(const Filename: string): Boolean;
    { GetVirtualFileContent is called to obtain the edit buffer's content. }
    procedure GetVirtualFileContent(const Filename: string; out Content: string);
    { SetVirtualFileContent is called to replace the edit buffer's content. }
    procedure SetVirtualFileContent(const Filename: string; const Content: string);
    { RestoreVirtualFileContent is called after the compiler has done its work.
      So the edit buffer's content can be restored. }
    procedure RestoreVirtualFileContent(const Filename: string; const BackupedContent: string);

    { AdjustLines is called for content changes breaking line numbers.
      LineNum: line where the changes occure
      Count: number of inserted lines (always >0) }
    procedure AdjustLines(const Filename: string; LineNum, Count: Integer);
  end;

  TPreProcessor = class(TObject)
  private
    FPasFiles: TStrings; // .pas files
    FIncFiles: TStrings; // .i1, .i2, ... files which have to be deleted on termination
    FFileInfos: TFileInfoList;
    FVirtualFileList: TStrings; // contains every virtual file name
    FFilenameMapper: TFilenameMapper;

    FMacros: TMacros;
    FFileSys: IDppVirtualFileSys;

    FUnitPaths: string;
    FIncludePaths: string;
    FCompilePrepare: Boolean;
    FParseOnlyOneFile: Boolean;
    FConditionals: TStrings;
    FConditionalParse: Boolean;
    FCaseSensitive: Boolean;

    FOnError: TErrorWarningEvent;
    FOnWarning: TErrorWarningEvent;
    FOnErrorWarningEx: TErrorWarningExEvent;
  protected
    FProjectPath: string; // has no trailing path delimiter
  private
   // TMacros events
    procedure EvError(Sender: TObject; const Filename, Msg: string; LineNum: Integer);
    procedure EvWarning(Sender: TObject; const Filename, Msg: string; LineNum: Integer);
    procedure EvPredefineMacros(Sender: TObject);
    procedure EvDefaultConditionals(Sender: TObject);
  public
    constructor Create(FileSys: IDppVirtualFileSys);
    destructor Destroy; override;

    function BeginPreProcessing(const Filename: string): Boolean; // parses all files
    procedure EndPreProcessing; // restores the original files

    property CompilePrepare: Boolean read FCompilePrepare write FCompilePrepare;
    property ParseOnlyOneFile: Boolean read FParseOnlyOneFile write FParseOnlyOneFile;
    property IncludePaths: string read FIncludePaths write FIncludePaths; // WIN: path;path LINUX: path:path
    property UnitPaths: string read FUnitPaths write FUnitPaths;          // WIN: path;path LINUX: path:path
    property Conditionals: TStrings read FConditionals;

    property ConditionalParse: Boolean read FConditionalParse write FConditionalParse;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;


    property PasFiles: TStrings read FPasFiles;
    property IncFiles: TStrings read FIncFiles;

   // events - if not assigned: output to stdout
    property OnError: TErrorWarningEvent read FOnError write FOnError;
    property OnWarning: TErrorWarningEvent read FOnWarning write FOnWarning;
    property OnErrorWarningEx: TErrorWarningExEvent read FOnErrorWarningEx write FOnErrorWarningEx;
  end;

  TMacroFileSys = class(TInterfacedObject, IMacroFileSys)
  private
    FPreProcessor: TPreProcessor;
    function ExTestFilename(const Filename: string): Boolean;
  public
    constructor Create(PreProcessor: TPreProcessor);
   // IMacroFileSys
    procedure BeforeFile(const Filename: string; IsIncludeFile: Boolean);
    procedure AfterFile(const Filename, NewFilename: string; IsIncludeFile,
      Modified: Boolean);
    procedure LoadFile(const Filename: string; out Content: string;
      IsIncludeFile: Boolean);
    procedure SaveFile(const Filename: string; var NewFilename: string;
      const Content: string; IsIncludeFile: Boolean);
    function FindFile(const Filename: string; IsIncludeFile: Boolean): string;
    function FileExists(const Filename: string): Boolean;
    procedure LinesMoved(const Filename: string; LineNum, AddedLines: Integer);
  end;

  TNoVirtualFileSys = class(TInterfacedObject, IDppVirtualFileSys)
   // IDppVirtualFileSys
    procedure InitPreProcessorFileSys; virtual;
    function IsVirtualFile(const Filename: string): Boolean; virtual;
    procedure GetVirtualFileContent(const Filename: string; out Content: string); virtual;
    procedure SetVirtualFileContent(const Filename: string; const Content: string); virtual;
    procedure RestoreVirtualFileContent(const Filename: string; const BackupedContent: string); virtual;
    procedure AdjustLines(const Filename: string; LineNum, Count: Integer); virtual;
  end;

implementation
resourcestring
  SErrorMovingFile = 'Error moving file "%s" to "%s"';

{ TPreProcessor }

constructor TPreProcessor.Create(FileSys: IDppVirtualFileSys);
begin
  inherited Create;
  FPasFiles := TStringList.Create;
  FIncFiles := TStringList.Create;
  FFileInfos := TFileInfoList.Create;
  FVirtualFileList := TStringList.Create;
  FFilenameMapper := TFilenameMapper.Create;
  FFileSys := FileSys;

  FMacros := TMacros.Create(TMacroFileSys.Create(Self));
  FMacros.OnWarning := EvWarning;
  FMacros.OnError := EvError;
  FMacros.OnPredefineMacros := EvPredefineMacros;
  FMacros.OnDefaultConditionals := EvDefaultConditionals;

  FCompilePrepare := True;
  FParseOnlyOneFile := False;
  FConditionals := TStringList.Create;
  FConditionalParse := False;
  FCaseSensitive := True;
end;

destructor TPreProcessor.Destroy;
begin
  FConditionals.Free;
  FPasFiles.Free;
  FIncFiles.Free;
  FVirtualFileList.Free;
  FFileInfos.Free;
  FMacros.Free;
  FFilenameMapper.Free;
  FFileSys := nil;
  inherited Destroy;
end;

function TPreProcessor.BeginPreProcessing(const Filename: string): Boolean;
begin
  FProjectPath := ExtractFileDir(Filename);

  FPasFiles.Clear;
  FIncFiles.Clear;
  FFileInfos.Clear;
  FVirtualFileList.Clear;
  FFilenameMapper.Clear;

  FUnitPaths := FProjectPath + PathSep + FUnitPaths;
  FIncludePaths := FProjectPath + PathSep + FIncludePaths;

  FFileSys.InitPreProcessorFileSys;

  FMacros.ConditionalParse := FConditionalParse;
  FMacros.CaseSensitive := FCaseSensitive;

  Result := False;
  try
    Result := FMacros.Parse(Filename, FParseOnlyOneFile);
  finally
    if not Result then
      EndPreProcessing;
  end;
end;

procedure TPreProcessor.EndPreProcessing;
var
  i: Integer;
  dpp, BackupedContent: string;
begin
  try
    if FCompilePrepare then
    begin
      for i := 0 to FVirtualFileList.Count - 1 do
      begin
        try
          dpp := ChangeFileExt(FVirtualFileList.Strings[i], '.dpp');
          FileToString(dpp, BackupedContent);
          FFileSys.RestoreVirtualFileContent(FVirtualFileList.Strings[i], BackupedContent);
          DeleteFile(dpp);
        except
         // proceed on errors
        end;
      end;

     // restore .pas files (.dpp -> .pas)
      for i := 0 to FPasFiles.Count - 1 do
        MoveFile(ChangeFileExt(FPasFiles.Strings[i], '.dpp'), FPasFiles.Strings[i]);

     // delete .i1, .i2, ... files
      for i := 0 to FIncFiles.Count - 1 do
        DeleteFile(FIncFiles.Strings[i]);
    end;
  finally
    FFilenameMapper.Clear;
    FVirtualFileList.Clear;
    FPasFiles.Clear;
    FIncFiles.Clear;
    FFileInfos.Clear;
  end;
end;

procedure TPreProcessor.EvError(Sender: TObject; const Filename,
  Msg: string; LineNum: Integer);
var s: string;
begin
  if Assigned(FOnErrorWarningEx) then
  begin
    FOnErrorWarningEx(Self, True, Filename, Msg, LineNum);
  end
  else
  begin
    s := Format('Error in %s (%d): %s', [ExtractFileName(Filename), LineNum, Msg]);
    if Assigned(FOnError) then
      FOnError(Self, s)
    else
      WriteLn(ErrOutput, s);
  end;
end;

procedure TPreProcessor.EvWarning(Sender: TObject; const Filename,
  Msg: string; LineNum: Integer);
var s: string;
begin
  if Assigned(FOnErrorWarningEx) then
  begin
    FOnErrorWarningEx(Self, False, Filename, Msg, LineNum);
  end
  else
  begin
    s := Format('Warning in %s (%d): %s', [ExtractFileName(Filename), LineNum, Msg]);
    if Assigned(FOnError) then
      FOnWarning(Self, s)
    else
      WriteLn(ErrOutput, s);
  end;
end;

procedure TPreProcessor.EvPredefineMacros(Sender: TObject);
begin
 // not implemented yet

{TODO allow user pre-defined macros}
// FMacros.RegisterMacro('');
end;

procedure TPreProcessor.EvDefaultConditionals(Sender: TObject);
var i: Integer;
begin
  for i := 0 to FConditionals.Count - 1 do
    FMacros.Define(FConditionals.Strings[i]);
end;

{ TMacroFileSys }

constructor TMacroFileSys.Create(PreProcessor: TPreProcessor);
begin
  inherited Create;
  FPreProcessor := PreProcessor;
end;

procedure TMacroFileSys.BeforeFile(const Filename: string; IsIncludeFile: Boolean);
begin
  // do nothing
end;

procedure TMacroFileSys.AfterFile(const Filename,
  NewFilename: string; IsIncludeFile, Modified: Boolean);
begin
 // do nothing
end;

procedure TMacroFileSys.LoadFile(const Filename: string;
  out Content: string; IsIncludeFile: Boolean);
begin
  with FPreProcessor do
  begin
    if FFileSys.IsVirtualFile(Filename) then
    begin
      FFileSys.GetVirtualFileContent(Filename, Content);
      if (FCompilePrepare) and not (IsIncludeFile) then
        StringToFile(ChangeFileExt(Filename, '.dpp'), Content); // backup
      FVirtualFileList.Add(Filename);
    end
    else
    begin
      if (FCompilePrepare) then FFileInfos.SaveInfos(Filename);
      FileToString(Filename, Content);
    end;
  end; // with
end;

procedure TMacroFileSys.SaveFile(const Filename: string;
  var NewFilename: string; const Content: string; IsIncludeFile: Boolean);
var dpp: string;
begin
  with FPreProcessor do
  begin
    if IsIncludeFile then
      FIncFiles.Add(NewFilename)  // save include file name for later deletion
    else
      FPasFiles.Add(Filename);    // save unit file name for later restore

    if (not FCompilePrepare) then
    begin
      StringToFile(NewFilename, Content); // store virtual files to disk too
      Exit;
     // --------
    end;

    if (IsIncludeFile) then
    begin
      StringToFile(NewFilename, Content);
    end
    else
    begin
      if FFileSys.IsVirtualFile(Filename) then
      begin
       // Replace edit buffer's content. The original content is stored in a .dpp
       // file.
        FFileSys.SetVirtualFileContent(Filename, Content);
      end
      else
      begin
        dpp := ChangeFileExt(Filename, '.dpp');
        NewFilename := Filename; // save .i as .pas

       // .pas -> .dpp  <-- backup file
        if not MoveFile(Filename, dpp) then
          raise Exception.CreateFmt(SErrorMovingFile, [Filename, dpp]);

       // store content
        StringToFile(NewFilename, Content);
       // set file times and attributes to origial one's
        FFileInfos.RestoreInfos(Filename);
      end;
    end;
  end; // with
end;

function TMacroFileSys.ExTestFilename(const Filename: string): Boolean;
begin
  Result := FPreProcessor.FFileSys.IsVirtualFile(Filename);
end;

function TMacroFileSys.FindFile(const Filename: string; IsIncludeFile: Boolean): string;
begin
  with FPreProcessor do
  begin
   // is the file already mapped?
    if FFilenameMapper.FindFilename(Filename, Result) then Exit;

    Result := Filename;
    if ExtractFilePath(Result) = '' then
    begin
      if IsIncludeFile then
        Result := TestFilenames(FIncludePaths, Result, ExTestFilename)
      else
        Result := TestFilenames(FUnitPaths, Result, ExTestFilename);
    end
    else
    begin
      if Pos('.' + PathDelim, Result) > 0 then // relative path
        Result := FollowRelativePath(FProjectPath, Result);
      if not Self.FileExists(Result) then Result := '';
    end;
    FFilenameMapper.AddFilename(Filename, Result);
  end; // with
end;

function TMacroFileSys.FileExists(const Filename: string): Boolean;
begin
  with FPreProcessor do
  begin
    Result := (FFileSys.IsVirtualFile(Filename)) or (FileExistsX(Filename));
  end; // with
end;

procedure TMacroFileSys.LinesMoved(const Filename: string; LineNum, AddedLines: Integer);
begin
  with FPreProcessor do
  begin
//    if FFileSys.IsVirtualFile(Filename) then
      FFileSys.AdjustLines(Filename, LineNum, AddedLines);
  end;
end;


{ TNoVirtualFileSys }

procedure TNoVirtualFileSys.AdjustLines(const Filename: string; LineNum,
  Count: Integer);
begin
//  WriteLn('Filename: ', Filename, ' --- ', LineNum, ' +', Count);
end;

procedure TNoVirtualFileSys.GetVirtualFileContent(const Filename: string;
  out Content: string);
begin
end;

procedure TNoVirtualFileSys.InitPreProcessorFileSys;
begin
end;

function TNoVirtualFileSys.IsVirtualFile(const Filename: string): Boolean;
begin
  Result := False;
end;

procedure TNoVirtualFileSys.RestoreVirtualFileContent(const Filename: string;
  const BackupedContent: string);
begin
end;

procedure TNoVirtualFileSys.SetVirtualFileContent(const Filename, Content: string);
begin
end;

end.
