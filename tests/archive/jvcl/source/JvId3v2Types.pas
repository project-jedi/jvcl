{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvId3v2Types.PAS, released on 2001-02-28.

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

unit JvId3v2Types;

{
Not yet coded (really complicated)
 USLT  4.9
 SYLT  4.10
 COMM  4.11
 RVAD  4.12
 EQUA  4.13
 RVRB  4.14
 GEOB  4.16
 RJvF  4.19
 AENC  4.20
 POSS  4.22
 USER  4.23
 COMR  4.25
Total : 12

ignored:
 SYTC
 ENCR
 GRID
 PRIV
 LINK
 MLLT
}

interface

type
  TId3v2Header = packed record
    Identifier: array [0..2] of Char;
    Version: Word;
    Flags: Byte;
    Size: Cardinal;
  end;

  TId3v2Frame = packed record
    Id: array [0..3] of Char;
    // (rom) changed to Cardinal sizes are usually unsigned
    Size: Cardinal;
    Flags: Word;
  end;

  TJvID3FrameID =
    (
    { ???? } fiNoFrame, { No known frame }
    { AENC } fiAudioCrypto, { Audio encryption }
    { APIC } fiPicture, { Attached picture }
    { ASPI } fiAudioSeekPoint, { Audio seek point index }
    { COMM } fiComment, { Comments }
    { COMR } fiCommercial, { Commercial frame }
    { ENCR } fiCryptoReg, { Encryption method registration }
    { EQU2 } fiEqualization2, { Equalisation (2) }
    { EQUA } fiEqualization, { Equalization }
    { ETCO } fiEventTiming, { Event timing codes }
    { GEOB } fiGeneralObject, { General encapsulated object }
    { GRID } fiGroupingReg, { Group identification registration }
    { IPLS } fiInvolvedPeople, { Involved people list }
    { LINK } fiLinkedInfo, { Linked information }
    { MCDI } fiCDID, { Music CD identifier }
    { MLLT } fiMPEGLookup, { MPEG location lookup table }
    { OWNE } fiOwnership, { Ownership frame }
    { PRIV } fiPrivate, { Private frame }
    { PCNT } fiPlayCounter, { Play counter }
    { POPM } fiPopularimeter, { Popularimeter }
    { POSS } fiPositionsync, { Position synchronisation frame }
    { RBUF } fiBufferSize, { Recommended buffer size }
    { RVA2 } fiVolumeAdj2, { Relative volume adjustment (2) }
    { RVAD } fiVolumeAdj, { Relative volume adjustment }
    { RVRB } fiReverb, { Reverb }
    { SEEK } fiSeekFrame, { Seek frame }
    { SIGN } fiSignature, { Signature frame }
    { SYLT } fiSyncedLyrics, { Synchronized lyric/text }
    { SYTC } fiSyncedTempo, { Synchronized tempo codes }
    { TALB } fiAlbum, { Album/Movie/Show title }
    { TBPM } fiBPM, { BPM (beats per minute) }
    { TCOM } fiComposer, { Composer }
    { TCON } fiContentType, { Content type }
    { TCOP } fiCopyright, { Copyright message }
    { TDAT } fiDate, { Date }
    { TDEN } fiEncodingTime, { Encoding time }
    { TDLY } fiPlaylistDelay, { Playlist delay }
    { TDOR } fiOrigReleaseTime, { Original release time }
    { TDRC } fiRecordingTime, { Recording time }
    { TDRL } fiReleaseTime, { Release time }
    { TDTG } fiTaggingTime, { Tagging time }
    { TIPL } fiInvolvedPeople2, { Involved people list }
    { TENC } fiEncodedBy, { Encoded by }
    { TEXT } fiLyricist, { Lyricist/Text writer }
    { TFLT } fiFileType, { File type }
    { TIME } fiTime, { Time }
    { TIT1 } fiContentGroup, { Content group description }
    { TIT2 } fiTitle, { Title/songname/content description }
    { TIT3 } fiSubTitle, { Subtitle/Description refinement }
    { TKEY } fiInitialKey, { Initial key }
    { TLAN } fiLanguage, { Language(s) }
    { TLEN } fiSongLen, { Length }
    { TMCL } fiMusicianCreditList, { Musician credits list }
    { TMED } fiMediaType, { Media type }
    { TMOO } fiMood, { Mood }
    { TOAL } fiOrigAlbum, { Original album/movie/show title }
    { TOFN } fiOrigFileName, { Original filename }
    { TOLY } fiOrigLyricist, { Original lyricist(s)/text writer(s) }
    { TOPE } fiOrigArtist, { Original artist(s)/performer(s) }
    { TORY } fiOrigYear, { Original release year }
    { TOWN } fiFileOwner, { File owner/licensee }
    { TPE1 } fiLeadArtist, { Lead performer(s)/Soloist(s) }
    { TPE2 } fiBand, { Band/orchestra/accompaniment }
    { TPE3 } fiConductor, { Conductor/performer refinement }
    { TPE4 } fiMixArtist, { Interpreted, remixed, or otherwise modified by }
    { TPOS } fiPartInSet, { Part of a set }
    { TPRO } fiProducedNotice, { Produced notice }
    { TPUB } fiPublisher, { Publisher }
    { TRCK } fiTrackNum, { Track number/Position in set }
    { TRDA } fiRecordingDates, { Recording dates }
    { TRSN } fiNetRadioStation, { Internet radio station name }
    { TRSO } fiNetRadioOwner, { Internet radio station owner }
    { TSIZ } fiSize, { Size }
    { TSOA } fiAlbumSortOrder, { Album sort order }
    { TSOP } fiPerformerSortOrder, { Performer sort order }
    { TSOT } fiTitleSortOrder, { Title sort order }
    { TSRC } fiISRC, { ISRC (international standard recording code) }
    { TSSE } fiEncoderSettings, { Software/Hardware and settings used for encoding }
    { TSST } fiSetSubTitle, { Set subtitle }
    { TXXX } fiUserText, { User defined text information }
    { TYER } fiYear, { Year }
    { UFID } fiUniqueFileID, { Unique file identifier }
    { USER } fiTermsOfUse, { Terms of use }
    { USLT } fiUnsyncedLyrics, { Unsynchronized lyric/text transcription }
    { WCOM } fiWWWCommercialInfo, { Commercial information }
    { WCOP } fiWWWCopyright, { Copyright/Legal infromation }
    { WOAF } fiWWWAudioFile, { Official audio file webpage }
    { WOAR } fiWWWArtist, { Official artist/performer webpage }
    { WOAS } fiWWWAudioSource, { Official audio source webpage }
    { WORS } fiWWWRadioPage, { Official internet radio station homepage }
    { WPAY } fiWWWPayment, { Payment }
    { WPUB } fiWWWPublisher, { Official publisher webpage }
    { WXXX } fiWWWUser, { User defined URL link }
    {      } fiMetaCrypto, { Encrypted meta frame (id3v2.2.x) }
    {      } fiMetaCompression { Compressed meta frame (id3v2.2.1) }
    );

function ID3_TextToFrameID(const S: string): TJvID3FrameID;

implementation

uses
  Classes;

type
  TJvListType = (ltID3LongText, ltID3ShortText);

  TJvID3TermFinder = class
  private
    FLists: array [TJvListType] of TStringList;
  protected
    procedure BuildList_ID3LongText;
    procedure BuildList_ID3ShortText;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Instance: TJvID3TermFinder;
    function ID3LongTextToFrameID(const S: string): TJvID3FrameID;
    function ID3ShortTextToFrameID(const S: string): TJvID3FrameID;
  end;

function ID3_TextToFrameID(const S: string): TJvID3FrameID;
var
  L: Integer;
begin
  L := Length(S);
  with TJvID3TermFinder.Instance do
    if L = 3 then
      Result := ID3ShortTextToFrameID(S)
    else
    if L = 4 then
      Result := ID3LongTextToFrameID(S)
    else
      Result := fiNoFrame;
end;

type
  TJvID3FrameDef = packed record
    ShortTextID: array [0..2] of Char;
    LongTextID: array [0..3] of Char;
  end;

const
  CID3FrameDefs: array [TJvID3FrameID] of TJvID3FrameDef = ( { Ver. 2 3 4 }
    (ShortTextID: ''; LongTextID: ''), { fiNoFrame                  - - - }
    (ShortTextID: 'CRA'; LongTextID: 'AENC'), { fiAudioCrypto       X X X }
    (ShortTextID: 'PIC'; LongTextID: 'APIC'), { fiPicture           X X X }
    (ShortTextID: ''; LongTextID: 'ASPI'), { fiAudioSeekPoint       - X X }
    (ShortTextID: 'COM'; LongTextID: 'COMM'), { fiComment           X X X }
    (ShortTextID: ''; LongTextID: 'COMR'), { fiCommercial           - X X }
    (ShortTextID: ''; LongTextID: 'ENCR'), { fiCryptoReg            - X X }
    (ShortTextID: ''; LongTextID: 'EQU2'), { fiEqualization2        - - X }
    (ShortTextID: 'EQU'; LongTextID: 'EQUA'), { fiEqualization      X X d }
    (ShortTextID: 'ETC'; LongTextID: 'ETCO'), { fiEventTiming       X X X }
    (ShortTextID: 'GEO'; LongTextID: 'GEOB'), { fiGeneralObject     X X X }
    (ShortTextID: ''; LongTextID: 'GRID'), { fiGroupingReg          - X X }
    (ShortTextID: 'IPL'; LongTextID: 'IPLS'), { fiInvolvedPeople    X X d }
    (ShortTextID: 'LNK'; LongTextID: 'LINK'), { fiLinkedInfo        X X X }
    (ShortTextID: 'MCI'; LongTextID: 'MCDI'), { fiCDID              X X X }
    (ShortTextID: 'MLL'; LongTextID: 'MLLT'), { fiMPEGLookup        X X X }
    (ShortTextID: ''; LongTextID: 'OWNE'), { fiOwnership            - X X }
    (ShortTextID: ''; LongTextID: 'PRIV'), { fiPrivate              - X X }
    (ShortTextID: 'CNT'; LongTextID: 'PCNT'), { fiPlayCounter       X X X }
    (ShortTextID: 'POP'; LongTextID: 'POPM'), { fiPopularimeter     X X X }
    (ShortTextID: ''; LongTextID: 'POSS'), { fiPositionsync         - X X }
    (ShortTextID: 'BUF'; LongTextID: 'RBUF'), { fiBufferSize        X X X }
    (ShortTextID: ''; LongTextID: 'RVA2'), { fiVolumeAdj2           - - X }
    (ShortTextID: 'RVA'; LongTextID: 'RVAD'), { fiVolumeAdj         X X d }
    (ShortTextID: 'REV'; LongTextID: 'RVRB'), { fiReverb            X X X }
    (ShortTextID: ''; LongTextID: 'SEEK'), { fiSeekFrame            - - X }
    (ShortTextID: ''; LongTextID: 'SIGN'), { fiSignature            - - X }
    (ShortTextID: 'SLT'; LongTextID: 'SYLT'), { fiSyncedLyrics      X X X }
    (ShortTextID: 'STC'; LongTextID: 'SYTC'), { fiSyncedTempo       X X X }
    (ShortTextID: 'TAL'; LongTextID: 'TALB'), { fiAlbum             X X X }
    (ShortTextID: 'TBP'; LongTextID: 'TBPM'), { fiBPM               X X X }
    (ShortTextID: 'TCM'; LongTextID: 'TCOM'), { fiComposer          X X X }
    (ShortTextID: 'TCO'; LongTextID: 'TCON'), { fiContentType       X X X }
    (ShortTextID: 'TCR'; LongTextID: 'TCOP'), { fiCopyright         X X X }
    (ShortTextID: 'TDA'; LongTextID: 'TDAT'), { fiDate              X X d }
    (ShortTextID: ''; LongTextID: 'TDEN'), { fiEncodingTime         - - X }
    (ShortTextID: 'TDY'; LongTextID: 'TDLY'), { fiPlaylistDelay     X X X }
    (ShortTextID: ''; LongTextID: 'TDOR'), { fiOrigReleaseTime      - - X }
    (ShortTextID: ''; LongTextID: 'TDRC'), { fiRecordingTime        - - X }
    (ShortTextID: ''; LongTextID: 'TDRL'), { fiReleaseTime          - - X }
    (ShortTextID: ''; LongTextID: 'TDTG'), { fiTaggingTime          - - X }
    (ShortTextID: ''; LongTextID: 'TIPL'), { fiInvolvedPeople2      - - X }
    (ShortTextID: 'TEN'; LongTextID: 'TENC'), { fiEncodedBy         X X X }
    (ShortTextID: 'TXT'; LongTextID: 'TEXT'), { fiLyricist          X X X }
    (ShortTextID: 'TFT'; LongTextID: 'TFLT'), { fiFileType          X X X }
    (ShortTextID: 'TIM'; LongTextID: 'TIME'), { fiTime              X X d }
    (ShortTextID: 'TT1'; LongTextID: 'TIT1'), { fiContentGroup      X X X }
    (ShortTextID: 'TT2'; LongTextID: 'TIT2'), { fiTitle             X X X }
    (ShortTextID: 'TT3'; LongTextID: 'TIT3'), { fiSubTitle          X X X }
    (ShortTextID: 'TKE'; LongTextID: 'TKEY'), { fiInitialKey        X X X }
    (ShortTextID: 'TLA'; LongTextID: 'TLAN'), { fiLanguage          X X X }
    (ShortTextID: 'TLE'; LongTextID: 'TLEN'), { fiSongLen           X X X }
    (ShortTextID: ''; LongTextID: 'TMCL'), { fiMusicianCreditList   - - X }
    (ShortTextID: 'TMT'; LongTextID: 'TMED'), { fiMediaType         X X X }
    (ShortTextID: ''; LongTextID: 'TMOO'), { fiMood                 - - X }
    (ShortTextID: 'TOT'; LongTextID: 'TOAL'), { fiOrigAlbum         X X X }
    (ShortTextID: 'TOF'; LongTextID: 'TOFN'), { fiOrigFileName      X X X }
    (ShortTextID: 'TOL'; LongTextID: 'TOLY'), { fiOrigLyricist      X X X }
    (ShortTextID: 'TOA'; LongTextID: 'TOPE'), { fiOrigArtist        X X X }
    (ShortTextID: 'TOR'; LongTextID: 'TORY'), { fiOrigYear          X X d }
    (ShortTextID: ''; LongTextID: 'TOWN'), { fiFileOwner            - X X }
    (ShortTextID: 'TP1'; LongTextID: 'TPE1'), { fiLeadArtist        X X X }
    (ShortTextID: 'TP2'; LongTextID: 'TPE2'), { fiBand              X X X }
    (ShortTextID: 'TP3'; LongTextID: 'TPE3'), { fiConductor         X X X }
    (ShortTextID: 'TP4'; LongTextID: 'TPE4'), { fiMixArtist         X X X }
    (ShortTextID: 'TPA'; LongTextID: 'TPOS'), { fiPartInSet         X X X }
    (ShortTextID: ''; LongTextID: 'TPRO'), { fiProducedNotice       - - X }
    (ShortTextID: 'TPB'; LongTextID: 'TPUB'), { fiPublisher         X X X }
    (ShortTextID: 'TRK'; LongTextID: 'TRCK'), { fiTrackNum          X X X }
    (ShortTextID: 'TRD'; LongTextID: 'TRDA'), { fiRecordingDates    X X d }
    (ShortTextID: 'TRN'; LongTextID: 'TRSN'), { fiNetRadioStation   X X X }
    (ShortTextID: 'TRO'; LongTextID: 'TRSO'), { fiNetRadioOwner     X X X }
    (ShortTextID: 'TSI'; LongTextID: 'TSIZ'), { fiSize              X X d }
    (ShortTextID: ''; LongTextID: 'TSOA'), { fiAlbumSortOrder       - - X }
    (ShortTextID: ''; LongTextID: 'TSOP'), { fiPerformerSortOrder   - - X }
    (ShortTextID: ''; LongTextID: 'TSOT'), { fiTitleSortOrder       - - X }
    (ShortTextID: 'TRC'; LongTextID: 'TSRC'), { fiISRC              X X X }
    (ShortTextID: ''; LongTextID: 'TSSE'), { fiEncoderSettings      - X X }
    (ShortTextID: 'TSS'; LongTextID: 'TSST'), { fiSetSubTitle       - - X }
    (ShortTextID: 'TXX'; LongTextID: 'TXXX'), { fiUserText          X X X }
    (ShortTextID: 'TYE'; LongTextID: 'TYER'), { fiYear              X X d }
    (ShortTextID: 'UFI'; LongTextID: 'UFID'), { fiUniqueFileID      X X X }
    (ShortTextID: ''; LongTextID: 'USER'), { fiTermsOfUse           - X X }
    (ShortTextID: 'ULT'; LongTextID: 'USLT'), { fiUnsyncedLyrics    X X X }
    (ShortTextID: 'WCM'; LongTextID: 'WCOM'), { fiWWWCommercialInfo X X X }
    (ShortTextID: 'WCP'; LongTextID: 'WCOP'), { fiWWWCopyright      X X X }
    (ShortTextID: 'WAF'; LongTextID: 'WOAF'), { fiWWWAudioFile      X X X }
    (ShortTextID: 'WAR'; LongTextID: 'WOAR'), { fiWWWArtist         X X X }
    (ShortTextID: 'WAS'; LongTextID: 'WOAS'), { fiWWWAudioSource    X X X }
    (ShortTextID: 'WRA'; LongTextID: 'WORS'), { fiWWWRadioPage      X X X }
    (ShortTextID: 'WPY'; LongTextID: 'WPAY'), { fiWWWPayment        X X X }
    (ShortTextID: 'WPB'; LongTextID: 'WPUB'), { fiWWWPublisher      X X X }
    (ShortTextID: 'WXX'; LongTextID: 'WXXX'), { fiWWWUser           X X X }
    (ShortTextID: 'CRM'; LongTextID: ''), { fiMetaCrypto            X - - }
    (ShortTextID: 'CDM'; LongTextID: '') { fiMetaCompressio         X - - }
    );

// === TJvID3TermFinder =====================================================

constructor TJvID3TermFinder.Create;
var
  ListType: TJvListType;
begin
  inherited Create;
  for ListType := Low(TJvListType) to High(TJvListType) do
    FLists[ListType] := nil;
end;

destructor TJvID3TermFinder.Destroy;
var
  ListType: TJvListType;
begin
  for ListType := Low(TJvListType) to High(TJvListType) do
    FLists[ListType].Free;
  inherited Destroy;
end;

procedure TJvID3TermFinder.BuildList_ID3LongText;
var
  FrameID: TJvID3FrameID;
begin
  if Assigned(FLists[ltID3LongText]) then
    Exit;

  FLists[ltID3LongText] := TStringList.Create;
  with FLists[ltID3LongText] do
  begin
    Duplicates := dupError;
    Sorted := True;

    for FrameID := Low(TJvID3FrameID) to High(TJvID3FrameID) do
      with CID3FrameDefs[FrameID] do
        if LongTextID[0] <> #0 then
          { TODO : Ipv ID een getal opslaan }
          AddObject(LongTextID, TObject(FrameID));
  end;
end;

procedure TJvID3TermFinder.BuildList_ID3ShortText;
var
  FrameID: TJvID3FrameID;
begin
  if Assigned(FLists[ltID3ShortText]) then
    Exit;

  FLists[ltID3ShortText] := TStringList.Create;
  with FLists[ltID3ShortText] do
  begin
    Duplicates := dupError;
    Sorted := True;

    for FrameID := Low(TJvID3FrameID) to High(TJvID3FrameID) do
      with CID3FrameDefs[FrameID] do
        if ShortTextID[0] <> #0 then
          AddObject(ShortTextID, TObject(FrameID));
  end;
end;

function TJvID3TermFinder.ID3LongTextToFrameID(const S: string): TJvID3FrameID;
var
  I: Integer;
begin
  BuildList_ID3LongText;

  I := FLists[ltID3LongText].IndexOf(S);
  if I < 0 then
    Result := fiNoFrame
  else
    Result := TJvID3FrameID(FLists[ltID3LongText].Objects[I]);
end;

function TJvID3TermFinder.ID3ShortTextToFrameID(const S: string): TJvID3FrameID;
var
  I: Integer;
begin
  BuildList_ID3ShortText;

  I := FLists[ltID3ShortText].IndexOf(S);
  if I < 0 then
    Result := fiNoFrame
  else
    Result := TJvID3FrameID(FLists[ltID3ShortText].Objects[I]);
end;

var
  GInstance: TJvID3TermFinder = nil;

class function TJvID3TermFinder.Instance: TJvID3TermFinder;
begin
  if not Assigned(GInstance) then
    GInstance := TJvID3TermFinder.Create;
  Result := GInstance;
end;

initialization

finalization
  GInstance.Free;

end.

