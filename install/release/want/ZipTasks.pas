(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.              *
 * All rights reserved.                                                     *
 *                                                                          *
 * This library is free software; you can redistribute it and/or            *
 * modify it under the terms of the GNU Lesser General Public               *
 * License as published by the Free Software Foundation; either             *
 * version 2.1 of the License, or (at your option) any later version.       *
 *                                                                          *
 * This library is distributed in the hope that it will be useful,          *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 * Lesser General Public License for more details.                          *
 *                                                                          *
 * You should have received a copy of the GNU Lesser General Public         *
 * License along with this library; if not, write to the Free Software      *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA *
 ****************************************************************************)
{
    @brief 

    @author Juanco Añez
}

unit ZipTasks;

interface
uses
  SysUtils,

  JalUtils,
  JalZipStreams,

  WildPaths,
  PatternSets,
  WantClasses,
  FileTasks;


type
  TZipTask = class(TFileSetTask)
  private
    FCompresslevel: TCompressionLevel;
  protected
    FZipFile   :TPath;
    FCompress  :boolean;

    FZipStream :TZipStream;
    FPreservePath: boolean;

  public
    constructor Create(Owner :TScriptElement); override;

    procedure Init; override;
    procedure DoFileset(Fileset :TFileSet); override;

    procedure Execute; override;
  published
    property basedir;

    property zipfile  :TPath   read FZipFile   write FZipFile;
    property compress :boolean read FCompress  write FCompress    default true;
    property compressionlevel:TCompressionLevel read FCompresslevel write FCompresslevel default zlDefault;

    property includes :string write AddCommaSeparatedIncludes;
    property excludes :string write AddCommaSeparatedExcludes;
    property preservePath :boolean read FPreservePath write FPreservePath default true;
  end;

  TUnzipTask = class(TTask)
  protected
    FZipFile   :TPath;
    FToDir     :TPath;

    FUnzipStream :TUnzipStream;

  public
    procedure Init; override;
    procedure Execute; override;
  published
    property zipfile  :TPath read FZipFile write FZipFile;
    property src      :TPath read FZipFile write FZipFile;

    property todir    :TPath read FToDir     write FToDir;
    property dest     :TPath read FToDir     write FToDir;
  end;

implementation

{ TZipTask }

constructor TZipTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FCompress := true;
  FCompresslevel := zlDefault;
  FPreservePath := true;
end;

procedure TZipTask.Init;
begin
  inherited Init;
  RequireAttribute('zipfile');
end;

procedure TZipTask.DoFileset(Fileset: TFileSet);
var
  Paths   :TPaths;
  p       :Integer;
begin
  Log(vlVerbose, Format('Fileset with basedir "%s"', [Fileset.dir]));
  Log(vlVerbose, CurrentDir);

  AboutToScratchPath(zipfile);

  Paths := FileSet.RelativePaths;

  if Length(Paths) = 0 then
    Log
  else
    Log(Format(' %4d files from %s', [Length(Paths), ToRelativePath(Fileset.dir)]));

  for p := Low(Paths) to High(Paths) do
  begin
    Log(vlDebug, Paths[p]);
    FZipStream.WriteFile(Paths[p],'',preservePath);
  end;
end;

procedure TZipTask.Execute;
begin
  Log(ToRelativePath(zipfile));
  AboutToScratchPath(zipfile);
  FZipStream := TZipStream.Create(zipfile);
  try
    if not compress then
      FZipStream.CompressionLevel := zlNone
    else
      FZipStream.CompressionLevel := FCompresslevel;
    inherited Execute;
  finally
    FreeAndNil(FZipStream);
  end
end;

{ TUnzipTask }

procedure TUnzipTask.Init;
begin
  inherited Init;
  if FZipFile = '' then
    TaskError('zipfile (or src) attribute is required');
end;

procedure TUnzipTask.Execute;
var
  ToPath, Entry :TPath;
  e :Integer;
begin
  Log(vlVerbose);
  ToPath := ToRelativePath(ToDir);
  FUnzipStream := TUnzipStream.Create(zipfile);
  try
    with FUnzipStream do
    begin
      if Entries.Count > 0 then
      begin
        Log('Unzipping %d files to "%s"', [Entries.Count, ToPath]);

        for e := 0 to Entries.Count-1 do
        begin
          Log(vlVerbose, Entries[e]);
          Entry := MovePath(Entries[e], ToPath);
          AboutToScratchPath(Entry);
          ExtractFile(Entries[e], ToPath);
        end;
      end;
    end;
  finally
    FreeAndNil(FUnzipStream);
  end
end;

initialization
  RegisterTasks([TZipTask, TUnzipTask]);
end.
