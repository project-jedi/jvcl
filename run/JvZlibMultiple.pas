{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvZlibMultiple.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S?stien Buysse [sbuysse att buypin dott com]
Portions created by S?stien Buysse are Copyright (C) 2001 S?stien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
    2004-07-27 - Read the 'ALL USERS READ THIS' section below.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvZlibMultiple;

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, // inline
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, Graphics, Controls, Dialogs,
  JclCompression,
  JvComponentBase;

// ----------------------------------------------------------------------------}
// 2004-07-27 *** ALL USERS READ THIS: (wpostma) ***
//
// I have added support for selective extraction and listing archive contents without
//  writing any files to disk. To do this we had to add some parameters to the component events.
//
//  This will break existing applications that use these events, until they update their
//  event declarations, to add the new parameters to your events.
//
//  This is something the Delphi IDE should do automatically, but does not do. <grin>
//
//  The old events for OnDecompressingFile, and OnDecompressedFile
//  look like this:
//      procedure <<TMyForm.MyEventHandlerName>>(Sender: TObject; const FileName: string)
//
//  The new events have an additional parameter each:
//
//     OnDecompressingFile -> (Sender: TObject; const FileName: string;
//                              {NEW!} var WriteFile: Boolean   )
//       OnDecompressedFile -> (Sender: TObject; const FileName: string;
//                              {NEW!} const FileSize: Longword )
//
// -----------------------------------------------------------------------------}

{ November 11, 2005 - yozey

  NOTE #1
  Added new procedures to pause and terminate the compression process.
  These would be very useful in a threaded environment.

  NOTE #2    December 22, 2005 - Johann Campbell
  - Added new procedure to list files stored in the zlib file ( basic rewrite of the decompression procedure )
  - Exposed the Pause and Terminate procedures

  See below.
}

type
  {NEW:}
  TFileBeforeWriteEvent = procedure(Sender: TObject; const FileName: string; var WriteFile: Boolean) of object;
  TFileAfterWriteEvent = procedure(Sender: TObject; const FileName: string; const FileSize: Longword) of object;

  TFileSkipEvent = procedure (Sender:Tobject;const Filename,errortype,errormessage:String);

  TFileEvent = procedure(Sender: TObject; const FileName: string) of object;
  TProgressEvent = procedure(Sender: TObject; Position, Total: Integer) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvZlibMultiple = class(TJvComponent)
  private
    FStorePaths: Boolean;
    FIgnoreExclusive: Boolean; // November 7, 2004 - USE WITH CAUTION !!!!!
    FCompressionLevel: TJclCompressionLevel;
    FOnProgress: TProgressEvent;
    FOnCompressingFile: TFileEvent;
    FOnCompressedFile: TFileEvent;
    FOnCompletedAction: TNotifyEvent;
    FOnFileSkip : TFileSkipEvent;
     
    // July 26, 2004: New improved event types for decompression: Allow user to
    // skip writing of files they want skipped on extraction, and if they
    // extract nothing, they can use this "nil extraction" to scan the contents
    // of the file, returning the file names and sizes inside.
    FOnDecompressingFile: TFileBeforeWriteEvent;
    FOnDecompressedFile: TFileAfterWriteEvent;
    FTerminateCompress: Boolean;  // Note #1
    FTerminateDecompress: Boolean;  // Note #1
    FCompressionPause: Boolean;  // Note #1
    FDecompressionPause: Boolean;   // Note #1
    FForceDirectoriesFlag: Boolean;
    procedure SetForceDirectoriesFlag(const Value: Boolean); // set true to force directories
  protected
    procedure AddFile(const FileName, Directory, FilePath: string; DestStream: TStream);
    procedure DoProgress(Position, Total: Integer); virtual;
    procedure DoStopCompression;   // Note #1
    procedure DoStopDecompression; // Note #1
  public
    constructor Create(AOwner: TComponent); override;
    // compresses a list of files (can contain wildcards)
    // NOTE: caller must free returned stream!
    function CompressFiles(Files: TStrings): TStream; overload;
    // compresses a list of files (can contain wildcards)
    // and saves the compressed result to FileName
    procedure CompressFiles(Files: TStrings; const FileName: string); overload;
    // compresses a Directory (recursing if Recursive is true)
    // NOTE: caller must free returned stream!
    function CompressDirectory(Directory: string; Recursive: Boolean): TStream; overload;
    // compresses a Directory (recursing if Recursive is true)
    // and saves the compressed result to FileName
    procedure CompressDirectory(const Directory: string; Recursive: Boolean; const FileName: string); overload;
    // decompresses FileName into Directory. If Overwrite is true, overwrites any existing files with
    // the same name as those in the compressed archive.
    // If RelativePaths is true, the paths in the compressed file are stripped from their drive letter
    procedure DecompressFile(const FileName, Directory: string; Overwrite: Boolean;
      const RelativePaths: Boolean = True);
    // decompresses Stream into Directory optionally overwriting any existing files
    // If RelativePaths is true, any paths in the stream are stripped from their drive letter
    procedure ListStoredFiles(const FileName: string; FileList: TStrings);  // Note #2
    procedure DecompressStream(Stream: TStream; Directory: string; Overwrite: Boolean;
      const RelativePaths: Boolean = True);
    procedure StopCompression;   // Note #1
    procedure StopDecompression; // Note #1
    property CompressionPaused: Boolean read FCompressionPause write FCompressionPause; // Note #1
    property DecompressionPaused: Boolean read FDecompressionPause write FDecompressionPause;  // Note #1

    property  OnFileSkip :TFileSkipEvent read FOnFileSkip write FOnFileSkip;

  published
    property StorePaths: Boolean read FStorePaths  write FStorePaths default True;
    // NOTE : This property allows you to override already opened files - USE WITH CAUTION!!! opened files may still be writing data
    //        causing stored files to be different from the final file.
    property IgnoreExclusive: Boolean read FIgnoreExclusive write FIgnoreExclusive default False;
    property CompressionLevel: TJclCompressionLevel read FCompressionLevel write FCompressionLevel default -1;
    property ForceDirectoriesFlag: Boolean read FForceDirectoriesFlag write SetForceDirectoriesFlag default True; // NEW MARCH 2007!
     // NOTE: Changed decompression event parameters. July 26 2004. -WPostma.
    property OnDecompressingFile: TFileBeforeWriteEvent read FOnDecompressingFile write FOnDecompressingFile;
    property OnDecompressedFile: TFileAfterWriteEvent read FOnDecompressedFile write FOnDecompressedFile;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnCompressingFile: TFileEvent read FOnCompressingFile write FOnCompressingFile;
    property OnCompressedFile: TFileEvent read FOnCompressedFile write FOnCompressedFile;
    property OnCompletedAction: TNotifyEvent read FOnCompletedAction write FOnCompletedAction;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvJCLUtils;

{*******************************************************}
{  Format of the File:                                  }
{   File Header                                         }
{    1 Byte    Size of the directory variable           }
{    x bytes   Directory of the file                    }
{    1 Byte    Size of the filename                     }
{    x bytes   Filename                                 }
{    4 bytes   Size of the file (uncompressed)          }
{    4 bytes   Size of the file (compressed)            }
{   Data chunk                                          }
{    x bytes   the compressed chunk                     }
{*******************************************************}

constructor TJvZlibMultiple.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStorePaths := True;
  FIgnoreExclusive := False;
  FCompressionLevel := -1;
  FForceDirectoriesFlag := true;
end;

function TJvZlibMultiple.CompressDirectory(Directory: string; Recursive: Boolean): TStream;

  procedure SearchDirectory(const SDirectory: string);
  var
    SearchRec: TSearchRec;
    Res: Integer;
    fn:String;
  begin
    // (rom) this may not work for network drives and compressed files
    // (rom) because of faAnyFile
    Res := FindFirst(Directory + SDirectory + AllFilesMask, faAnyFile, SearchRec);
    try
      while Res = 0 do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory) = 0 then begin
            try
              fn := Directory + SDirectory + SearchRec.Name;
              AddFile(SearchRec.Name, SDirectory, fn, Result)
            except
                on E:EFOpenError do begin
                    if Assigned(FOnFileSkip) then begin
                        FOnFileSkip(Self, fn, String(E.ClassName) ,E.Message );
                    end;
                end;


            end;
          end
          else
          if Recursive then
            SearchDirectory(SDirectory + SearchRec.Name + PathDelim);
        end;
        Res := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  end;

begin
  { (RB) Letting this function create a stream is not a good idea;
         see other CompressDirectory function that causes a memory leak }
  Result := TMemoryStream.Create;
  if Directory <> '' then // do not start with '\' if the caller specifies ''.
    Directory := IncludeTrailingPathDelimiter(Directory);
  SearchDirectory('');
  Result.Position := 0;
end;

procedure TJvZlibMultiple.AddFile(const FileName, Directory, FilePath: string;
  DestStream: TStream);
var
  Stream: TStream;
  FileStream: TFileStream;
  ZStream: TJclZLibCompressStream;
  Buffer: array [0..1023] of Byte;
  Count: Integer;
  FileStreamPos, FileStreamSize: Int64;

  procedure WriteFileRecord(const Directory, FileName: string; FileSize: Integer; CompressedSize: Integer);
  var
    B: Byte;
    AnsiStr: AnsiString;
  begin
    AnsiStr := AnsiString(Directory);
    if Length(AnsiStr) > 255 then
      SetLength(AnsiStr, 255);
    B := Length(AnsiStr);
    DestStream.Write(B, SizeOf(B));
    DestStream.Write(PAnsiChar(AnsiStr)^, B);

    AnsiStr := AnsiString(FileName);
    if Length(AnsiStr) > 255 then
      SetLength(AnsiStr, 255);
    B := Length(AnsiStr);
    DestStream.Write(B, SizeOf(B));
    DestStream.Write(PAnsiChar(AnsiStr)^, B);

    DestStream.Write(FileSize, SizeOf(FileSize));
    DestStream.Write(CompressedSize, SizeOf(CompressedSize));
  end;

begin
  Stream := TMemoryStream.Create;
  if not IgnoreExclusive then
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);


  if FileStream.Size=0 then begin
      Stream.Free;
      FileStream.Free;
      exit;
  end;

  try
    ZStream := TJclZLibCompressStream.Create(Stream, CompressionLevel);
    try
      if Assigned(FOnCompressingFile) then
        FOnCompressingFile(Self, FilePath);

      FileStreamPos := FileStream.Position;
      FileStreamSize := FileStream.Size;
      { (RB) ZStream has an OnProgress event, thus CopyFrom can be used }
      repeat
        Count := FileStream.Read(Buffer, SizeOf(Buffer));
        Inc(FileStreamPos, Count);
        if Count > 0 then
          ZStream.Write(Buffer, Count);
        DoProgress(FileStreamPos, FileStreamSize);
        while CompressionPaused do
          Sleep(1);
      until (Count = 0) or FTerminateCompress;
      ZStream.Flush; // Warren added.
    finally
      ZStream.Free;
    end;

    if Assigned(FOnCompressedFile) then
      FOnCompressedFile(Self, FilePath);

    if StorePaths then
      WriteFileRecord(Directory, FileName, FileStreamSize, Stream.Size)
    else
      WriteFileRecord('', FileName, FileStreamSize, Stream.Size);

    DestStream.CopyFrom(Stream, 0);
  finally
    FileStream.Free;
    Stream.Free;
  end;
end;

procedure TJvZlibMultiple.CompressDirectory(const Directory: string;
  Recursive: Boolean; const FileName: string);
var
  TmpStream: TStream;
begin
  // don't create file until we save it so we don't accidentally
  // try to compress ourselves!
  DeleteFile(FileName); // make sure we don't compress a previous archive into ourselves
  TmpStream := CompressDirectory(Directory, Recursive);
  try
    TMemoryStream(TmpStream).SaveToFile(FileName);
  finally
    TmpStream.Free;
  end;
end;

function TJvZlibMultiple.CompressFiles(Files: TStrings): TStream;
var
  I: Integer;
  S1, S2, Common: string;
begin
  FTerminateCompress := False;
  { (RB) Letting this function create a stream is not a good idea;
         see other CompressFiles function that causes a memory leak }
  Result := TMemoryStream.Create;
  if Files.Count = 0 then
    Exit;

  //Find the biggest Common part of all files
  S1 := UpperCase(Files[0]);
  for I := 1 to Files.Count - 1 do
  begin
    S2 := Files[I];
    while (Pos(S1, S2) = 0) and (S1 <> '') do
      S1 := Copy(S1, 1, Length(S1) - 1);
  end;
  { (RB) This should be Common := S1 (?) }
  Common := S2;

  //Add the files to the stream
  for I := 0 to Files.Count - 1 do
  begin
    S1 := ExtractFileName(Files[I]);
    S2 := ExtractFilePath(Files[I]);
    S2 := Copy(S2, 1, Length(Common));
    AddFile(S1, S2, Files[I], Result);
  end;

  Result.Position := 0;
end;

procedure TJvZlibMultiple.CompressFiles(Files: TStrings; const FileName: string);
var
  TmpStream: TStream;
begin
  TmpStream := CompressFiles(Files);
  try
    TMemoryStream(TmpStream).SaveToFile(FileName);
  finally
    TmpStream.Free;
  end;
  if Assigned(FOnCompletedAction) then
     FOnCompletedAction(Self);
end;

procedure TJvZlibMultiple.DecompressStream(Stream: TStream;
  Directory: string; Overwrite: Boolean; const RelativePaths: Boolean);
var
  FileStream: TFileStream;
  ZStream: TJclZLibDecompressStream;
  CStream: TMemoryStream;
  B, LastPos: Byte;
  AnsiS: AnsiString;
  S: string;
  Count, FileSize, I: Integer;
  Buffer: array [0..1023] of Byte;
  TotalByteCount: Longword;
  WriteMe: Boolean; // Allow skipping of files instead of writing them.
  FileStreamSize, StreamSize: Int64;
  fd: string; // name of directory to be made if it doesn't exist (unless we're skipping it)
begin
  if Directory <> '' then
    Directory := IncludeTrailingPathDelimiter(Directory);

  StreamSize := Stream.Size; // cache, to not FileSeek on every iteration
  while Stream.Position < StreamSize do
  begin
    //Read and force the directory
    Stream.Read(B, SizeOf(B));
    SetLength(AnsiS, B);
    if B > 0 then
      Stream.Read(AnsiS[1], B);
    S := string(AnsiS);

    fd := Directory + S;
    if (fd <> '') and (ForceDirectoriesFlag) then
      ForceDirectories(fd);

    if S <> '' then
      S := IncludeTrailingPathDelimiter(S);

    //This make files decompress either on Directory or Directory+SavedRelativePath
    if not RelativePaths then
      S := '';

    //Read filename
    Stream.Read(B, SizeOf(B));
    if B > 0 then
    begin
      AnsiS := AnsiString(S);
      LastPos := Length(AnsiS);
      SetLength(AnsiS, LastPos + B);
      Stream.Read(AnsiS[LastPos + 1], B);
      S := string(AnsiS);
    end;

    Stream.Read(FileSize, SizeOf(FileSize));
    Stream.Read(I, SizeOf(I));
    CStream := TMemoryStream.Create;

    try
      CStream.CopyFrom(Stream, I);
      CStream.Position := 0;

      //Decompress the file
      S := Directory + S;
      if Overwrite or not FileExists(S) then
      begin
        //This fails if Directory isn't empty
        WriteMe := True;
        if Assigned(FOnDecompressingFile) then
          FOnDecompressingFile(Self, S, WriteMe);

        if WriteMe then
          FileStream := TFileStream.Create(S, fmCreate or fmShareExclusive)
        else
          FileStream := nil; // skip it!

        ZStream := TJclZLibDecompressStream.Create(CStream);
        try
          TotalByteCount := 0;

          { (RB) ZStream has an OnProgress event, thus copyfrom can be used }
          FileStreamSize := 0;
          repeat
            Count := ZStream.Read(Buffer, SizeOf(Buffer));
            if Assigned(FileStream) then
            begin
              Inc(FileStreamSize, FileStream.Write(Buffer, Count));
              DoProgress(FileStreamSize, FileSize);
              while DecompressionPaused do
                Sleep(1);
            end;
            Inc(TotalByteCount, Count);
          until (Count = 0) or FTerminateDecompress;
          if Assigned(FOnDecompressedFile) then
            FOnDecompressedFile(Self, S, TotalByteCount);
        finally
          FreeAndNil(FileStream);
          ZStream.Free;
        end;
      end;
    finally
      CStream.Free;
    end;
  end;
end;

procedure TJvZlibMultiple.DecompressFile(const FileName, Directory: string;
  Overwrite: Boolean; const RelativePaths: Boolean);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Stream.Position := 0;
    DecompressStream(Stream, Directory, Overwrite, RelativePaths);
  finally
    Stream.Free;
  end;
 if Assigned(FOnCompletedAction) then
   FOnCompletedAction(Self);
end;

procedure TJvZlibMultiple.DoProgress(Position, Total: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Total);
end;

procedure TJvZlibMultiple.DoStopCompression;
begin
  FTerminateCompress := True;
end;

procedure TJvZlibMultiple.DoStopDecompression;
begin
  FTerminateDecompress := True;
end;

procedure TJvZlibMultiple.SetForceDirectoriesFlag(const Value: Boolean);
begin
  FForceDirectoriesFlag := Value;
end;

procedure TJvZlibMultiple.StopCompression;
begin
  DoStopCompression;
end;

procedure TJvZlibMultiple.StopDecompression;
begin
  DoStopDecompression;
end;

procedure TJvZLibMultiple.ListStoredFiles(const FileName: string; FileList: TStrings);
var
  ZStream: TFileStream;
  FHByte: Byte;
  FilePos, HeaderPos, CompressedSize, UnCompressedSize: Integer;
  AnsiFileInfo: AnsiString;
  FileInfo: string;
  ZStreamSize: Int64;
begin
  ZStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ZStreamSize := ZStream.Size;
    while ZStream.Position < ZStreamSize do
    begin
      ZStream.Read(FHByte, SizeOf(FHByte));
      SetLength(AnsiFileInfo, FHByte);
      if FHByte > 0 then
        ZStream.Read(AnsiFileInfo[1], FHByte);
      FileInfo := string(AnsiFileInfo);

      if FileInfo <> '' then
        FileInfo := IncludeTrailingPathDelimiter(FileInfo);
      ZStream.Read(FHByte, SizeOf(FHByte));
      if FHByte > 0 then
      begin
        AnsiFileInfo := AnsiString(FileInfo);
        HeaderPos := Length(AnsiFileInfo);
        SetLength(AnsiFileInfo, HeaderPos + FHByte);
        ZStream.Read(AnsiFileInfo[HeaderPos + 1], FHByte);
        FileInfo := string(AnsiFileInfo);
      end;

      FileList.Add(FileInfo);
      ZStream.Read(UncompressedSize, SizeOf(UncompressedSize));
      ZStream.Read(CompressedSize, SizeOf(CompressedSize));
      FilePos := ZStream.Position + CompressedSize;
      ZStream.Position := FilePos;
    end;
  finally
    ZStream.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
