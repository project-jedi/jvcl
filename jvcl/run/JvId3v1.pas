{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvID3v1.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvID3v1;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  JvComponent;

type
  TID3v1Tag = packed record
    Identifier: array [0..2] of Char;
    SongName: array [0..29] of Char;
    Artist: array [0..29] of Char;
    Album: array [0..29] of Char;
    Year: array [0..3] of Char;
    Comment: array [0..29] of Char;
    Genre: Byte;
  end;

  TJvID3v1 = class(TJvComponent)
  private
    FSongName: string;
    FArtist: string;
    FAlbum: string;
    FComment: string;
    FYear: string;
    FGenre: Byte;
    FFileName: TFileName;
    FActive: Boolean;
    FAlbumTrack: Byte;
    FStreamedActive: Boolean;
    FHasTag: Boolean;
    FHasTagDirty: Boolean;
    function GetGenreAsString: string;
    function GetHasTag: Boolean;
    procedure Reset;
    procedure SetActive(const Value: Boolean);
    procedure SetFileName(const Value: TFileName);
    procedure SetGenreAsString(const Value: string);
  protected
    procedure CheckActive;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    function ReadTag: Boolean;
    procedure Loaded; override;
  public
    procedure Refresh;
    procedure Open;
    procedure Close;
    function Commit: Boolean;
    procedure Erase;
    property HasTag: Boolean read GetHasTag;
  published
    property Active: Boolean read FActive write SetActive;
    property FileName: TFileName read FFileName write SetFileName;
    { Do not store dummies }
    property SongName: string read FSongName write FSongName stored False;
    property Artist: string read FArtist write FArtist stored False;
    property Album: string read FAlbum write FAlbum stored False;
    property Year: string read FYear write FYear stored False;
    property Comment: string read FComment write FComment stored False;
    property Genre: Byte read FGenre write FGenre stored False;
    property GenreAsString: string read GetGenreAsString write SetGenreAsString stored False;
    property AlbumTrack: Byte read FAlbumTrack write FAlbumTrack stored False;
  end;

function HasID3v1Tag(const AFileName: string): Boolean;
function ReadID3v1Tag(const AFileName: string; var ATag: TID3v1Tag): Boolean;
procedure RemoveID3v1Tag(const AFileName: string);
function WriteID3v1Tag(const AFileName: string; const ATag: TID3v1Tag): Boolean;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvID3v2Types, JvTypes, JvResources;

const
  CID3v1Tag = 'TAG';  { do not change case }

  CTagSize = 128;
  CTagIDSize = 3;

//=== Global procedures ======================================================

function HasID3v1Tag(const AFileName: string): Boolean;
var
  TagID: array [0..CTagIDSize - 1] of Char;
begin
  try
    with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
    try
      Result := Size >= CTagSize;
      if not Result then
        Exit;

      Seek(-CTagSize, soFromEnd);
      Result := (Read(TagID, CTagIDSize) = CTagIDSize) and (TagID = CID3v1Tag);
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

function ReadID3v1Tag(const AFileName: string; var ATag: TID3v1Tag): Boolean;
begin
  try
    with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
    try
      Seek(-CTagSize, soFromEnd);
      Result := (Read(ATag, CTagSize) = CTagSize) and (ATag.Identifier = CID3v1Tag);
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

procedure RemoveID3v1Tag(const AFileName: string);
var
  TagID: array [0..CTagIDSize - 1] of Char;
begin
  with TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite) do
  try
    Seek(-CTagSize, soFromEnd);

    if (Read(TagID, CTagIDSize) = CTagIDSize) and (TagID = CID3v1Tag) then
      Size := Size - CTagSize;
  finally
    Free;
  end;
end;

function WriteID3v1Tag(const AFileName: string; const ATag: TID3v1Tag): Boolean;
var
  TagID: array [0..CTagIDSize - 1] of Char;
begin
  try
    Result := FileExists(AFileName);
    if not Result then
      Exit;

    with TFileStream.Create(AFileName, fmOpenReadWrite or fmShareExclusive) do
    try
      // Remove old Tag ?
      if Size >= CTagSize then
      begin
        Seek(-CTagSize, soFromEnd);
        if (Read(TagID, CTagIDSize) = CTagIDSize) and (TagID = CID3v1Tag) then
          Seek(-CTagIDSize, soFromCurrent)
        else
          Seek(0, soFromEnd);
      end
      else
        Seek(0, soFromEnd);

      // Write it
      Result := Write(ATag, CTagSize) = CTagSize;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

//=== Local procedures =======================================================

function TagToStr(P: PChar; MaxLength: Integer): string;
var
  Q: PChar;
begin
  Q := P;
  while (P - Q < MaxLength) and (P^ <> #0) do
    Inc(P);

  { [Q..P) is valid }
  SetString(Result, Q, P - Q);
end;

//=== { TJvID3v1 } ===========================================================

procedure TJvID3v1.Loaded;
begin
  inherited Loaded;

  FHasTagDirty := True;
  if FStreamedActive then
    SetActive(True);
end;

procedure TJvID3v1.CheckActive;
begin
  if not FActive then
    raise EJVCLException.CreateRes(@RsENotActive);
end;

procedure TJvID3v1.Close;
begin
  SetActive(False);
end;

function TJvID3v1.Commit: Boolean;
var
  Tag: TID3v1Tag;
begin
  CheckActive;

  FHasTagDirty := True;

  FillChar(Tag, CTagSize, #0);

  // Set new Tag
  Tag.Identifier := CID3v1Tag;
  Move(PChar(SongName)^, Tag.SongName[0], Min(30, Length(SongName)));
  Move(PChar(Artist)^, Tag.Artist[0], Min(30, Length(Artist)));
  Move(PChar(Album)^, Tag.Album[0], Min(30, Length(Album)));
  Move(PChar(Year)^, Tag.Year[0], Min(4, Length(Year)));
  Move(PChar(Comment)^, Tag.Comment[0], Min(30, Length(Comment)));
  Tag.Genre := FGenre;
  if Tag.Comment[28] = #0 then
    Tag.Comment[29] := Char(FAlbumTrack);

  Result := WriteID3v1Tag(FileName, Tag);
end;

procedure TJvID3v1.DoClose;
begin
  Reset;
end;

procedure TJvID3v1.DoOpen;
begin
  ReadTag;
end;

procedure TJvID3v1.Erase;
var
  SavedActive: Boolean;
begin
  FHasTagDirty := True;

  SavedActive := Active;
  Close;

  try
    RemoveID3v1Tag(FileName);
  finally
    if SavedActive then
      Open;
  end;
end;

function TJvID3v1.GetGenreAsString: string;
begin
  Result := ID3_IDToGenre(Genre);
end;

function TJvID3v1.GetHasTag: Boolean;
begin
  if FHasTagDirty then
  begin
    FHasTagDirty := False;
    FHasTag := HasID3v1Tag(FileName);
  end;

  Result := FHasTag;
end;

procedure TJvID3v1.Open;
begin
  SetActive(True);
end;

function TJvID3v1.ReadTag: Boolean;
var
  Tag: TID3v1Tag;
begin
  CheckActive;

  Result := ReadID3v1Tag(FileName, Tag);

  FHasTagDirty := False;
  FHasTag := Result;

  if Result then
  begin
    FSongName := TagToStr(PChar(@Tag.SongName), 30);
    FArtist := TagToStr(PChar(@Tag.Artist), 30);
    FAlbum := TagToStr(PChar(@Tag.Album), 30);
    FYear := TagToStr(PChar(@Tag.Year), 4);
    FComment := TagToStr(PChar(@Tag.Comment), 30);
    // (p3) missing genre added
    FGenre := Tag.Genre;
    if Tag.Comment[28] = #0 then
      FAlbumTrack := Byte(Tag.Comment[29])
    else
      FAlbumTrack := 0;
  end
  else
    Reset;
end;

procedure TJvID3v1.Refresh;
begin
  CheckActive;
  ReadTag;
end;

procedure TJvID3v1.Reset;
begin
  FSongName := '';
  FArtist := '';
  FAlbum := '';
  FYear := '';
  FComment := '';
  FGenre := 255;
end;

procedure TJvID3v1.SetActive(const Value: Boolean);
begin
  { Based on TCustomConnection.SetConnected }
  if (csReading in ComponentState) and Value then
    FStreamedActive := True
  else
  begin
    if Value = FActive then
      Exit;
    FActive := Value;
    if FActive then
      DoOpen
    else
      DoClose;
  end;
end;

procedure TJvID3v1.SetFileName(const Value: TFileName);
var
  SavedActive: Boolean;
begin
  if Value <> FFileName then
  begin
    SavedActive := Active;

    Close;

    FHasTagDirty := True;
    FFileName := Value;

    if SavedActive then
      Open;
  end;
end;

procedure TJvID3v1.SetGenreAsString(const Value: string);
begin
  Genre := ID3_GenreToID(Value);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
