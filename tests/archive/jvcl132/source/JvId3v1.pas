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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvId3v1;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JvTypes, JvComponent;

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
    Identifier: array[0..2] of Char;
    SongName: array[1..30] of Char;
    Artist: array[1..30] of Char;
    Album: array[1..30] of Char;
    Year: array[0..3] of Char;
    Comment: array[1..30] of Char;
    Genre: TGenre;
  end;

  TJvId3v1 = class(TJvComponent)
  private
    FSongName: string;
    FArtist: string;
    FAlbum: string;
    FComment: string;
    FYear: string;
    FGenre: TGenre;
    FBidonS: string;
    FBidonG: TGenre;
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  published
    function ReadTag(FileName: string): Boolean;
    function WriteTag(FileName, SongName, Artist, Album, Year, Comment: string; Genre: TGenre): Boolean;
    procedure RemoveTag(FileName: string);
    function TagPresent(FileName: string): Boolean;
    property FileName: TFileName read FFileName write SetFileName;
    function GenreToString(Genre: TGenre): string;
    property SongName: string read FSongName write FBidonS;
    property Artist: string read FArtist write FBidonS;
    property Album: string read FAlbum write FBidonS;
    property Year: string read FYear write FBidonS;
    property Comment: string read FComment write FBidonS;
    property Genre: TGenre read FGenre write FBidonG;
  end;

implementation

{******************************************************************************}

function TJvId3v1.GenreToString(Genre: TGenre): string;
begin
  case Byte(Genre) of
    0: Result := 'Blues';
    1: Result := 'Classic Rock';
    2: Result := 'Country';
    3: Result := 'Dance';
    4: Result := 'Disco';
    5: Result := 'Funk';
    6: Result := 'Grunge';
    7: Result := 'Hip-Hop';
    8: Result := 'Jazz';
    9: Result := 'Metal';
    10: Result := 'New Age';
    11: Result := 'Oldies';
    12: Result := 'Other';
    13: Result := 'Pop';
    14: Result := 'R&B';
    15: Result := 'Rap';
    16: Result := 'Reggae';
    17: Result := 'Rock';
    18: Result := 'Techno';
    19: Result := 'Industrial';
    20: Result := 'Alternative';
    21: Result := 'Ska';
    22: Result := 'Death Metal';
    23: Result := 'Pranks';
    24: Result := 'Soundtrack';
    25: Result := 'Euro-Techno';
    26: Result := 'Ambient';
    27: Result := 'Trip-Hop';
    28: Result := 'Vocal';
    29: Result := 'Jazz+Funk';
    30: Result := 'Fusion';
    31: Result := 'Trance';
    32: Result := 'Classical';
    33: Result := 'Instrumental';
    34: Result := 'Acid';
    35: Result := 'House';
    36: Result := 'Game';
    37: Result := 'Sound Clip';
    38: Result := 'Gospel';
    39: Result := 'Noise';
    40: Result := 'AlternRock';
    41: Result := 'Bass';
    42: Result := 'Soul';
    43: Result := 'Punk';
    44: Result := 'Space';
    45: Result := 'Meditative';
    46: Result := 'Instrumental Pop';
    47: Result := 'Instrumental Rock';
    48: Result := 'Ethnic';
    49: Result := 'Gothic';
    50: Result := 'Darkwave';
    51: Result := 'Techno-Industrial';
    52: Result := 'Electronic';
    53: Result := 'Pop-Folk';
    54: Result := 'Eurodance';
    55: Result := 'Dream';
    56: Result := 'Southern Rock';
    57: Result := 'Comedy';
    58: Result := 'Cult';
    59: Result := 'Gangsta';
    60: Result := 'Top 40';
    61: Result := 'Christian Rap';
    62: Result := 'Pop/Funk';
    63: Result := 'Jungle';
    64: Result := 'Native American';
    65: Result := 'Cabaret';
    66: Result := 'New Wave';
    67: Result := 'Psychadelic';
    68: Result := 'Rave';
    69: Result := 'Showtunes';
    70: Result := 'Trailer';
    71: Result := 'Lo-Fi';
    72: Result := 'Tribal';
    73: Result := 'Acid Punk';
    74: Result := 'Acid Jazz';
    75: Result := 'Polka';
    76: Result := 'Retro';
    77: Result := 'Musical';
    78: Result := 'Rock & Roll';
    79: Result := 'Hard Rock';
    80: Result := 'Folk';
    81: Result := 'Folk/Rock';
    82: Result := 'National Folk';
    83: Result := 'Swing';
    84: Result := 'Fast Fusion';
    85: Result := 'Bebob';
    86: Result := 'Latin';
    87: Result := 'Revival';
    88: Result := 'Celtic';
    89: Result := 'Bluegrass';
    90: Result := 'Avantgarde';
    91: Result := 'Gothic Rock';
    92: Result := 'Progressive Rock';
    93: Result := 'Psychedelic Rock';
    94: Result := 'Symphonic Rock';
    95: Result := 'Slow Rock';
    96: Result := 'Big Band';
    97: Result := 'Chorus';
    98: Result := 'Easy Listening';
    99: Result := 'Acoustic';
    100: Result := 'Humour';
    101: Result := 'Speech';
    102: Result := 'Chanson';
    103: Result := 'Opera';
    104: Result := 'Chamber Music';
    105: Result := 'Sonata';
    106: Result := 'Symphony';
    107: Result := 'Booty Bass';
    108: Result := 'Primus';
    109: Result := 'Porn Groove';
    110: Result := 'Satire';
    111: Result := 'Slow Jam';
    112: Result := 'Club';
    113: Result := 'Tango';
    114: Result := 'Samba';
    115: Result := 'Folklore';
    116: Result := 'Ballad';
    117: Result := 'Power Ballad';
    118: Result := 'Rhythmic Soul';
    119: Result := 'Freestyle';
    120: Result := 'Duet';
    121: Result := 'Punk Rock';
    122: Result := 'Drum Solo';
    123: Result := 'Acapella';
    124: Result := 'Euro-House';
    125: Result := 'Dance Hall';
  else
    Result := '';
  end;
end;

{******************************************************************************}

function TJvId3v1.ReadTag(FileName: string): Boolean;
var
  fich: TFileStream;
  tag: TId3v1Tag;
begin
  try
    fich := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    fich.Position := fich.Size - 128;
    fich.Read(tag, SizeOf(tag));
    if tag.Identifier = 'TAG' then
    begin
      FSongName := tag.SongName;
      FArtist := tag.Artist;
      FAlbum := tag.Album;
      FYear := tag.Year;
      // (p3) missing genre added
      FGenre := tag.Genre;
      FComment := tag.Comment;
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

    fich.Free;

    Result := True;
  except
    Result := False;
  end;
end;

{*****************************************************************************}

procedure TJvId3v1.RemoveTag(FileName: string);
var
  fich: TFileStream;
  tag: array[0..2] of Char;
begin
  fich := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
  fich.Position := fich.Size - 128;
  fich.Read(tag, 3);

  if tag = 'TAG' then
    fich.Size := fich.Size - 128;

  fich.Free;
end;

{*****************************************************************************}

procedure TJvId3v1.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  ReadTag(Value);
end;

{*****************************************************************************}

function TJvId3v1.TagPresent(FileName: string): Boolean;
var
  fich: TFileStream;
  tag: array[0..2] of Char;
begin
  try
    fich := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
    fich.Position := fich.Size - 128;
    fich.Read(tag, 3);
    Result := tag = 'TAG';
    fich.Free;
  except
    Result := False;
  end;
end;

{*****************************************************************************}

function TJvId3v1.WriteTag(FileName, SongName, Artist, Album, Year, Comment: string; Genre: TGenre): Boolean;
var
  fich: TFileStream;
  tag, tag1: TId3v1Tag;
begin
  try
    ZeroMemory(@tag, SizeOf(tag));
    if FileExists(FileName) then
    begin
      fich := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive);

      //Remove old tag ?
      if fich.Size > 128 then
      begin
        fich.Position := fich.Size - 128;
        fich.Read(tag1, SizeOf(tag1));
        if tag1.Identifier = 'TAG' then
          fich.Position := fich.Size - 128;
      end;

      //Set new tag
      tag.Identifier := 'TAG';
      if SongName <> '' then
        CopyMemory(@tag.SongName, @SongName[1], 30);
      if Artist <> '' then
        CopyMemory(@tag.Artist, @Artist[1], 30);
      if Album <> '' then
        CopyMemory(@tag.Album, @Album[1], 30);
      if Year <> '' then
        CopyMemory(@tag.Year, @Year[1], 4);
      if Comment <> '' then
        CopyMemory(@tag.Comment, @Comment[1], 30);
      tag.Genre := Genre;

      //Write it
      fich.Write(tag, SizeOf(tag));

      fich.Free;

      Result := True;
    end
    else
      Result := False;
  except
    Result := False;
  end;
end;

end.
