{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvId3v1.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvId3v1;

interface

uses
  Windows, SysUtils, Classes,
  JvComponent;

type
  TGenre = (grBlues, grClassicRock, grCountry, grDance, grDisco, grFunk, grGrunge,
    grHipHop, grJazz, grMetal, grNewAge, grOldies, grOther, grPop, grRandB, grRap,
    grReggae, grRock, grTechno, grIndustrial, grAlternative, grSka,
    grDeathMetal, grPranks, grSoundtrack, grEuroTechno, grAmbient,
    grTripHop, grVocal, grJazzFunk, grFusion, grTrance, grClassical,
    grInstrumental, grAcid, grHouse, grGame, grSoundClip, grGospel,
    grNoise, grAlternRock, grBass, grSoul, grPunk, grSpace, grMeditative,
    grInstrumentalPop, grInstrumentalRock, grEthnic,
    grGothic, grDarkwave, grTechnoIndustrial, grElectronic,
    grPopFolk, grEurodance, grDream, grSouthernRock, grComedy,
    grCult, grGangsta, grTop40, grChristianRap, grPopFunk,
    grJungle, grNativeAmerican, grCabaret, grNewWave, grPsychadelic,
    grRave, grShowtunes, grTrailer, grLoFi, grTribal, grAcidPunk,
    grAcidJazz, grPolka, grRetro, grMusical, grRockandRoll,
    grHardRock, grFolk, grFolkRock, grNationalFolk, grSwing,
    grFastFusion, grBebob, grLatin, grRevival, grCeltic,
    grBluegrass, grAvantgarde, grGothicRock, grProgressiveRock,
    grPsychedelicRock, grSymphonicRock, grSlowRock, grBigBand,
    grChorus, grEasyListening, grAcoustic, grHumour, grSpeech,
    grChanson, grOpera, grChamberMusic, grSonata, grSymphony, grBootyBass,
    grPrimus, grPornGroove, grSatire, grSlowJam, grClub, grTango, grSamba,
    grFolklore, grBallad, grPowerBallad, grRhythmicSoul, grFreestyle,
    grDuet, grPunkRock, grDrumSolo, grAcapella, grEuroHouse, grDanceHall,
    grNone);

  TId3v1Tag = packed record
    Identifier: array [0..2] of Char;
    SongName: array [0..29] of Char;
    Artist: array [0..29] of Char;
    Album: array [0..29] of Char;
    Year: array [0..3] of Char;
    Comment: array [0..29] of Char;
    Genre: Byte;
  end;

  TJvId3v1 = class(TJvComponent)
  private
    FSongName: string;
    FArtist: string;
    FAlbum: string;
    FComment: string;
    FYear: string;
    FGenre: TGenre;
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  public
    function ReadTag: Boolean;
    function WriteTag: Boolean;
    procedure RemoveTag;
    function TagPresent: Boolean;
    function GenreToString(Genre: TGenre): string;
  published
    property FileName: TFileName read FFileName write SetFileName;
    { Do not store dummies }
    property SongName: string read FSongName write FSongName stored False;
    property Artist: string read FArtist write FArtist stored False;
    property Album: string read FAlbum write FAlbum stored False;
    property Year: string read FYear write FYear stored False;
    property Comment: string read FComment write FComment stored False;
    property Genre: TGenre read FGenre write FGenre stored False;
  end;

function HasID3v1Tag(const AFileName: string): Boolean;
function ReadID3v1Tag(const AFileName: string; var ATag: TId3v1Tag): Boolean;
procedure RemoveID3v1Tag(const AFileName: string);
function WriteID3v1Tag(const AFileName: string; const ATag: TId3v1Tag): Boolean;

implementation

uses
  Math;

const
  CID3v1Tag = 'TAG';
  CTagSize = 128;
  CTagIDSize = 3;

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

function ReadID3v1Tag(const AFileName: string; var ATag: TId3v1Tag): Boolean;
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

function WriteID3v1Tag(const AFileName: string; const ATag: TId3v1Tag): Boolean;
var
  TagID: array [0..CTagIDSize - 1] of Char;
begin
  try
    Result := FileExists(AFileName);
    if not Result then
      Exit;

    with TFileStream.Create(AFileName, fmOpenReadWrite or fmShareExclusive) do
    try
      //Remove old Tag ?
      if Size >= CTagSize then
      begin
        Seek(-CTagSize, soFromEnd);
        if (Read(TagID, CTagIDSize) = CTagIDSize) and (TagID = CID3v1Tag) then
          Seek(-CTagIDSize, soFromCurrent)
        else
          Seek(0, soFromEnd);
      end;

      //Write it
      Result := Write(ATag, CTagSize) = CTagSize;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

//=== TJvId3v1 ===============================================================

function TJvId3v1.GenreToString(Genre: TGenre): string;
const
  cGenreTexts: array [TGenre] of PChar =
  ('Blues', 'Classic Rock', 'Country', 'Dance', 'Disco', 'Funk', 'Grunge',
    'Hip-Hop', 'Jazz', 'Metal', 'New Age', 'Oldies', 'Other', 'Pop',
    'R&B', 'Rap', 'Reggae', 'Rock', 'Techno', 'Industrial', 'Alternative',
    'Ska', 'Death Metal', 'Pranks', 'Soundtrack', 'Euro-Techno', 'Ambient',
    'Trip-Hop', 'Vocal', 'Jazz+Funk', 'Fusion', 'Trance', 'Classical',
    'Instrumental', 'Acid', 'House', 'Game', 'Sound Clip', 'Gospel',
    'Noise', 'AlternRock', 'Bass', 'Soul', 'Punk', 'Space', 'Meditative',
    'Instrumental Pop', 'Instrumental Rock', 'Ethnic', 'Gothic', 'Darkwave',
    'Techno-Industrial', 'Electronic', 'Pop-Folk', 'Eurodance', 'Dream',
    'Southern Rock', 'Comedy', 'Cult', 'Gangsta', 'Top 40', 'Christian Rap',
    'Pop/Funk', 'Jungle', 'Native American', 'Cabaret', 'New Wave', 'Psychadelic',
    'Rave', 'Showtunes', 'Trailer', 'Lo-Fi', 'Tribal', 'Acid Punk', 'Acid Jazz',
    'Polka', 'Retro', 'Musical', 'Rock & Roll', 'Hard Rock', 'Folk', 'Folk/Rock',
    'National Folk', 'Swing', 'Fast Fusion', 'Bebob', 'Latin', 'Revival',
    'Celtic', 'Bluegrass', 'Avantgarde', 'Gothic Rock', 'Progressive Rock',
    'Psychedelic Rock', 'Symphonic Rock', 'Slow Rock', 'Big Band', 'Chorus',
    'Easy Listening', 'Acoustic', 'Humour', 'Speech', 'Chanson', 'Opera',
    'Chamber Music', 'Sonata', 'Symphony', 'Booty Bass', 'Primus',
    'Porn Groove', 'Satire', 'Slow Jam', 'Club', 'Tango', 'Samba', 'Folklore',
    'Ballad', 'Power Ballad', 'Rhythmic Soul', 'Freestyle', 'Duet', 'Punk Rock',
    'Drum Solo', 'Acapella', 'Euro-House', 'Dance Hall', 'none');
begin
  Result := cGenreTexts[Genre];
end;

function TJvId3v1.ReadTag: Boolean;
var
  Tag: TId3v1Tag;
begin
  Result := ReadID3v1Tag(FileName, Tag);
  if Result then
  begin
    FSongName := TagToStr(PChar(@Tag.SongName), 30);
    FArtist := TagToStr(PChar(@Tag.Artist), 30);
    FAlbum := TagToStr(PChar(@Tag.Album), 30);
    FYear := TagToStr(PChar(@Tag.Year), 4);
    FComment := TagToStr(PChar(@Tag.Comment), 30);
    // (p3) missing genre added
    if Tag.Genre <= Integer(High(TGenre)) then
      FGenre := TGenre(Tag.Genre)
    else
      FGenre := grNone;
  end
  else
  begin
    FSongName := '';
    FArtist := '';
    FAlbum := '';
    FYear := '';
    FComment := '';
    FGenre := grNone;
  end;
end;

procedure TJvId3v1.RemoveTag;
begin
  RemoveID3v1Tag(FileName);
  ReadTag;
end;

procedure TJvId3v1.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    ReadTag;
  end;
end;

function TJvId3v1.TagPresent: Boolean;
begin
  Result := HasID3v1Tag(FileName);
end;

function TJvId3v1.WriteTag: Boolean;
var
  Tag: TId3v1Tag;
begin
  FillChar(Tag, CTagSize, #0);

  //Set new Tag
  Tag.Identifier := CID3v1Tag;
  Move(SongName[1], Tag.SongName[0], Min(30, Length(SongName)));
  Move(Artist[1], Tag.Artist[0], Min(30, Length(Artist)));
  Move(Album[1], Tag.Album[0], Min(30, Length(Album)));
  Move(Year[1], Tag.Year[0], Min(4, Length(Year)));
  Move(Comment[1], Tag.Comment[0], Min(30, Length(Comment)));
  Tag.Genre := Byte(Genre);

  Result := WriteID3v1Tag(FileName, Tag);
end;

end.

