{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvZlibMultiple.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQZlibMultiple;

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, QDialogs, Types, 
  JclZLib, JvQComponent;

type
  TFileEvent = procedure(Sender: TObject; const FileName: string) of object;
  TProgressEvent = procedure(Sender: TObject; Position, Total: Integer) of object;

  TJvZlibMultiple = class(TJvComponent)
  private
    FOnProgress: TProgressEvent;
    FOnCompressingFile: TFileEvent;
    FOnCompressedFile: TFileEvent;
    FOnDecompressingFile: TFileEvent;
    FOnDecompressedFile: TFileEvent;
  protected
    procedure AddFile(FileName, Directory, FilePath: string; DestStream: TStream);
    procedure DoProgress(Position, Total: Integer); virtual;
  public
    // compresses a list of files (can contain wildcards)
    // NOTE: caller must free returned stream!
    function CompressFiles(Files: TStrings): TStream; overload;
    // compresses a list of files (can contain wildcards)
    // and saves the compressed result to FileName
    procedure CompressFiles(Files: TStrings; FileName: string); overload;
    // compresses a Directory (recursing if Recursive is true)
    // NOTE: caller must free returned stream!
    function CompressDirectory(Directory: string; Recursive: Boolean): TStream; overload;
    // compresses a Directory (recursing if Recursive is true)
    // and saves the compressed result to FileName
    procedure CompressDirectory(Directory: string; Recursive: Boolean; FileName: string); overload;
    // decompresses FileName into Directory. If Overwrite is true, overwrites any existing files with
    // the same name as those in the compressed archive.
    // If RelativePaths is true, the paths in the compressed file are stripped from their drive letter
    procedure DecompressFile(FileName, Directory: string; Overwrite: Boolean; const RelativePaths: Boolean = True);
    // decompresses Stream into Directory optionally overwriting any existing files
    // If RelativePaths is true, any paths in the stream are stripped from their drive letter
    procedure DecompressStream(Stream: TStream; Directory: string; Overwrite: Boolean; const RelativePaths: Boolean = True);
  published
    property OnDecompressingFile: TFileEvent read FOnDecompressingFile write FOnDecompressingFile;
    property OnDecompressedFile: TFileEvent read FOnDecompressedFile write FOnDecompressedFile;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnCompressingFile: TFileEvent read FOnCompressingFile write FOnCompressingFile;
    property OnCompressedFile: TFileEvent read FOnCompressedFile write FOnCompressedFile;
  end;

implementation

uses 
  JvQJCLUtils;

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

function TJvZlibMultiple.CompressDirectory(Directory: string; Recursive: Boolean): TStream;

  procedure SearchDirectory(SDirectory: string);
  var
    SearchRec: TSearchRec;
    Res: Integer;
  begin
    // (rom) this may not work for network drives and compressed files
    // (rom) because of faAnyFile
    Res := FindFirst(Directory + SDirectory + AllFilesMask, faAnyFile, SearchRec);
    try
      while Res = 0 do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory) = 0 then
            AddFile(SearchRec.Name, SDirectory, Directory + SDirectory + SearchRec.Name, Result)
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
  Directory := IncludeTrailingPathDelimiter(Directory);
  SearchDirectory('');
  Result.Position := 0;
end;

procedure TJvZlibMultiple.AddFile(FileName, Directory, FilePath: string;
  DestStream: TStream);
var
  Stream: TStream;
  FileStream: TFileStream;
  ZStream: TJclZLibWriter;
  Buffer: array[0..1023] of Byte;
  Count: Integer;

  procedure WriteFileRecord(Directory, FileName: string; FileSize: Integer;
    CompressedSize: Integer);
  var
    B: Byte;
    Tab: array[1..256] of Char;
  begin
    { (RB) Can be improved }
    for B := 1 to Length(Directory) do
      Tab[B] := Directory[B];
    B := Length(Directory);
    DestStream.Write(B, SizeOf(B));
    DestStream.Write(Tab, B);

    { (RB) Can be improved }
    for B := 1 to Length(FileName) do
      Tab[B] := FileName[B];
    B := Length(FileName);
    DestStream.Write(B, SizeOf(B));
    DestStream.Write(Tab, B);

    DestStream.Write(FileSize, SizeOf(FileSize));
    DestStream.Write(CompressedSize, SizeOf(CompressedSize));
  end;

begin
  Stream := TMemoryStream.Create;
  FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    ZStream := TJclZLibWriter.Create(Stream);
    try
      if Assigned(FOnCompressingFile) then
        FOnCompressingFile(Self, FilePath);

      { (RB) ZStream has an OnProgress event, thus CopyFrom can be used }
      repeat
        Count := FileStream.Read(Buffer, SizeOf(Buffer));
        ZStream.Write(Buffer, Count);
        DoProgress(FileStream.Position, FileStream.Size);
      until Count = 0;
    finally
      ZStream.Free;
    end;

    if Assigned(FOnCompressedFile) then
      FOnCompressedFile(Self, FilePath);

    WriteFileRecord(Directory, FileName, FileStream.Size, Stream.Size);
    DestStream.CopyFrom(Stream, 0);
  finally
    FileStream.Free;
    Stream.Free;
  end;
end;

procedure TJvZlibMultiple.CompressDirectory(Directory: string;
  Recursive: Boolean; FileName: string);
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

procedure TJvZlibMultiple.CompressFiles(Files: TStrings; FileName: string);
var
  TmpStream: TStream;
begin
  TmpStream := CompressFiles(Files);
  try
    TMemoryStream(TmpStream).SaveToFile(FileName);
  finally
    TmpStream.Free;
  end;
end;

procedure TJvZlibMultiple.DecompressStream(Stream: TStream;
  Directory: string; Overwrite: Boolean; const RelativePaths: Boolean);
var
  FileStream: TFileStream;
  ZStream: TJclZLibReader;
  CStream: TMemoryStream;
  B, LastPos: Byte;
  S: string;
  Count, FileSize, I: Integer;
  Buffer: array[0..1023] of Byte;
begin
  if (Length(Directory) > 0) then
    Directory := IncludeTrailingPathDelimiter(Directory);

  while Stream.Position < Stream.Size do
  begin
    //Read and force the directory
    Stream.Read(B, SizeOf(B));
    SetLength(S, B);
    if B > 0 then
      Stream.Read(S[1], B);
    ForceDirectories(Directory + S);
    if (Length(S) > 0) then
      S := IncludeTrailingPathDelimiter(S);

    //This make files decompress either on Directory or Directory+SavedRelativePath
    if not RelativePaths then
      S := '';

    //Read filename
    Stream.Read(B, SizeOf(B));
    if B > 0 then
    begin
      LastPos := Length(S);
      SetLength(S, LastPos + B);
      Stream.Read(S[LastPos + 1], B);
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
        FileStream := TFileStream.Create(S, fmCreate or fmShareExclusive);
        ZStream := TJclZLibReader.Create(CStream);
        try
          if Assigned(FOnDecompressingFile) then
            FOnDecompressingFile(Self, S);

          { (RB) ZStream has an OnProgress event, thus copyfrom can be used }
          repeat
            Count := ZStream.Read(Buffer, SizeOf(Buffer));
            FileStream.Write(Buffer, Count);
            DoProgress(FileStream.Size, FileSize);
          until Count = 0;

          if Assigned(FOnDecompressedFile) then
            FOnDecompressedFile(Self, S);
        finally
          FileStream.Free;
          ZStream.Free;
        end;
      end;
    finally
      CStream.Free;
    end;
  end;
end;

procedure TJvZlibMultiple.DecompressFile(FileName, Directory: string;
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
end;

procedure TJvZlibMultiple.DoProgress(Position, Total: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Total);
end;

end.

