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

The Original Code is: JvID3v2Types.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com].
  Remko Bonte [remkobonte att myrealbox dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQID3v2Types;

{$I jvcl.inc}

interface

uses
  Classes;

type
  TJvID3TagSizeRestriction = (tsrMax128Frames_1MB, tsrMax64Frames_128KB, tsrMax32Frames_40KB,
    tsrMax32Frames_4KB);
  TJvID3TextEncodingRestriction = (terNoRestrictions, terOnlyISO_8859_1orUTF8);
  TJvID3TextFieldsSizeRestriction = (tfszNoRestrictions, tfszMax1024Char, tfszMax128Char,
    tfszMax30Char);
  TJvID3ImageEncodingRestriction = (ierNoRestrictions, ierOnlyPNGorJPEG);
  TJvID3ImageSizeRestriction = (isrNoRestrictions, isrMax256x256, isrMax64x64, isr64x64UnlessRequired);

  TJvID3Restrictions = record
    RTagSize: TJvID3TagSizeRestriction;
    RTextEncoding: TJvID3TextEncodingRestriction;
    RTextFieldsSize: TJvID3TextFieldsSizeRestriction;
    RImageEncoding: TJvID3ImageEncodingRestriction;
    RImageSize: TJvID3ImageSizeRestriction;
  end;

  TJvID3HeaderExtendedFlag = (hefTagIsAnUpdate, hefCRCDataPresent, hefTagRestrictions);
  TJvID3HeaderExtendedFlags = set of TJvID3HeaderExtendedFlag;

  TJvID3HeaderFlag = (hfUnsynchronisation, hfExtendedHeader, hfExperimentalIndicator, hfFooterPresent);
  TJvID3HeaderFlags = set of TJvID3HeaderFlag;

  TJvID3FrameHeaderFlag = (fhfOnTagAlterDiscardFrame, fhfOnFileAlterDiscardFrame,
    fhfReadOnly, fhfIsCompressed, fhfIsEncrypted, fhfContainsGroupInformation, fhfUnsynchronisationApplied,
    fhfDataLengthIndicator);
  TJvID3FrameHeaderFlags = set of TJvID3FrameHeaderFlag;

  { $00   ISO-8859-1 [ISO-8859-1]. Terminated with $00.
    $01   UTF-16 [UTF-16] encoded Unicode [UNICODE] with BOM. All
          strings in the same frame SHALL have the same byteorder.
          Terminated with $00 00.
    $02   UTF-16BE [UTF-16] encoded Unicode [UNICODE] without BOM.
          Terminated with $00 00.
    $03   UTF-8 [UTF-8] encoded Unicode [UNICODE]. Terminated with $00.
  }

  TJvID3ForceEncoding = (ifeDontCare, ifeISO_8859_1, ifeUTF_16, ifeUTF_16BE, ifeUTF_8);
  TJvID3Encoding = (ienISO_8859_1, ienUTF_16, ienUTF_16BE, ienUTF_8);
  TJvID3Encodings = set of TJvID3Encoding;

  TJvID3ForceVersion = (ifvDontCare, ifv2_2, ifv2_3, ifv2_4);
  TJvID3Version = (iveLowerThan2_2, ive2_2, ive2_3, ive2_4, iveHigherThan2_4);

const
  CForceEncodingToEncoding: array [TJvID3ForceEncoding] of TJvID3Encoding =
    (ienISO_8859_1, ienISO_8859_1, ienUTF_16, ienUTF_16BE, ienUTF_8);
  CForceVersionToVersion: array [TJvID3ForceVersion] of TJvID3Version =
    (ive2_3, ive2_2, ive2_3, ive2_4);

type
  TJvID3StringPair = record
    SA: string;
    SW: WideString;
  end;

  TID3v2HeaderRec = packed record
    Identifier: array [0..2] of Char;
    MajorVersion: Byte;
    RevisionNumber: Byte;
    Flags: Byte;
    Size: Cardinal;
  end;

  TID3v2FrameRec = packed record
    ID: array [0..3] of Char;
    // (rom) changed to Cardinal sizes are usually unsigned
    Size: Cardinal;
    Flag0: Byte;
    Flag1: Byte;
  end;

  TJvID3PictureType = (
    ptOther, { Other }
    ptFileIcon, { 32x32 pixels 'file icon' (PNG only) }
    ptOtherFileIcon, { Other file icon }
    ptCoverFront, { Cover (front) }
    ptCoverBack, { Cover (back) }
    ptLeafletPage, { Leaflet page }
    ptMedia, { Media (e.g. lable side of CD) }
    ptLeadArtist, { Lead artist/lead performer/soloist }
    ptArtist, { Artist/performer }
    ptConductor, { Conductor }
    ptBand, { Band/Orchestra }
    ptComposer, { Composer }
    ptLyricist, { Lyricist/text writer }
    ptRecordingLocation, { Recording Location }
    ptDuringRecording, { During recording }
    ptDuringPerformance, { During performance }
    ptMovieVideoScreenCapture, { Movie/video screen capture }
    ptBrightColouredFish, { A bright coloured fish }
    ptIllustration, { Illustration }
    ptBandLogotype, { Band/artist logotype }
    ptPublisherLogotype { Publisher/Studio logotype }
    );

  TJvID3FrameID =
    (
    { ---- } fiErrorFrame, { Erroneous frame, ie chars not in [a..z, A..Z, 0..9] }
    { #0   } fiPaddingFrame, { Padding }
    { ???? } fiUnknownFrame, { No known frame }
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
    { WCOP } fiWWWCopyright, { Copyright/Legal information }
    { WOAF } fiWWWAudioFile, { Official audio file webpage }
    { WOAR } fiWWWArtist, { Official artist/performer webpage }
    { WOAS } fiWWWAudioSource, { Official audio source webpage }
    { WORS } fiWWWRadioPage, { Official internet radio station homepage }
    { WPAY } fiWWWPayment, { Payment }
    { WPUB } fiWWWPublisher, { Official publisher webpage }
    { WXXX } fiWWWUser, { User defined URL link }
    {      } fiMetaCrypto, { Encrypted meta frame (ID3v2.2.x) }
    {      } fiMetaCompression { Compressed meta frame (ID3v2.2.1) }
    );
  TJvID3FrameIDs = set of TJvID3FrameID;

{ Frame ID procedures }
function ID3_StringToFrameID(const S: string): TJvID3FrameID;
function ID3_FrameIDToString(const ID: TJvID3FrameID; const Size: Integer = 4): string;

{ Genre procedures }
function ID3_GenreToID(const AGenre: string; const InclWinampGenres: Boolean = True): Integer;
{ searches for a genre that is a prefix for AGenreLong }
function ID3_LongGenreToID(const ALongGenre: string; const InclWinampGenres: Boolean = True): Integer;
function ID3_IDToGenre(const ID: Integer; const InclWinampGenres: Boolean = True): string;
procedure ID3_Genres(Strings: TStrings; const InclWinampGenres: Boolean = True);

{ Language ISO 639-2 procedures }
function ISO_639_2IsCode(const Code: string): Boolean;
function ISO_639_2CodeToName(const Code: string): string;
{ Known problem: some codes such as 'dut' and 'nld', have the same name value,
  thus ISO_639_2NameToCode('Dutch') = 'dut' not 'nld' }
function ISO_639_2NameToCode(const Name: string): string;
procedure ISO_639_2Names(Strings: TStrings);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math, SysUtils,
  JvQFinalize, JvQConsts, JvQResources, JvQTypes;

const
  sUnitName = 'JvId3v2Types';

type
  TJvListType =
    (ltID3LongText, ltID3ShortText, ltISO_639_2Code, ltISO_639_2Name, ltID3Genres);

  TJvID3FrameDef = packed record
    ShortTextID: array [0..2] of Char;
    LongTextID: array [0..3] of Char;
  end;

  { Note: When you change type of S or L to 'string' it will increase the exe size
          with minimal 475x8 bytes }

  TShortToLongName = record
    S: array [0..2] of Char;
    L: PChar;
  end;

  TJvID3TermFinder = class
  private
    FLists: array [TJvListType] of TStringList;
  protected
    procedure BuildList_ISO_639_2Name;
    procedure BuildList_ISO_639_2Code;
    procedure BuildList_ID3LongText;
    procedure BuildList_ID3ShortText;
    procedure BuildList_ID3Genres;
    function IsFrameOk(const S: string): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Instance: TJvID3TermFinder;

    function ID3LongTextToFrameID(const S: string): TJvID3FrameID;
    function ID3ShortTextToFrameID(const S: string): TJvID3FrameID;

    function ID3GenreToID(const AGenre: string; const InclWinampGenres: Boolean): Integer;
    procedure ID3Genres(AStrings: TStrings; const InclWinampGenres: Boolean);
    function ID3LongGenreToID(const ALongGenre: string; const InclWinampGenres: Boolean): Integer;

    function ISO_639_2CodeToIndex(const ACode: string): Integer;
    function ISO_639_2NameToIndex(const AName: string): Integer;
    procedure ISO_639_2Names(AStrings: TStrings);
  end;

const
  CID3FrameDefs: array [TJvID3FrameID] of TJvID3FrameDef = (   { Ver. 2 3 4 }
    (ShortTextID: '';    LongTextID: ''    ), { fiErrorFrame          - - - }
    (ShortTextID: '';    LongTextID: ''    ), { fiPaddingFrame        - - - }
    (ShortTextID: '';    LongTextID: ''    ), { fiUnknownFrame        - - - }
    (ShortTextID: 'CRA'; LongTextID: 'AENC'), { fiAudioCrypto         X X X }
    (ShortTextID: 'PIC'; LongTextID: 'APIC'), { fiPicture             X X X }
    (ShortTextID: '';    LongTextID: 'ASPI'), { fiAudioSeekPoint      - - X }
    (ShortTextID: 'COM'; LongTextID: 'COMM'), { fiComment             X X X }
    (ShortTextID: '';    LongTextID: 'COMR'), { fiCommercial          - X X }
    (ShortTextID: '';    LongTextID: 'ENCR'), { fiCryptoReg           - X X }
    (ShortTextID: '';    LongTextID: 'EQU2'), { fiEqualization2       - - X }
    (ShortTextID: 'EQU'; LongTextID: 'EQUA'), { fiEqualization        X X d }
    (ShortTextID: 'ETC'; LongTextID: 'ETCO'), { fiEventTiming         X X X }
    (ShortTextID: 'GEO'; LongTextID: 'GEOB'), { fiGeneralObject       X X X }
    (ShortTextID: '';    LongTextID: 'GRID'), { fiGroupingReg         - X X }
    (ShortTextID: 'IPL'; LongTextID: 'IPLS'), { fiInvolvedPeople      X X d }
    (ShortTextID: 'LNK'; LongTextID: 'LINK'), { fiLinkedInfo          X X X }
    (ShortTextID: 'MCI'; LongTextID: 'MCDI'), { fiCDID                X X X }
    (ShortTextID: 'MLL'; LongTextID: 'MLLT'), { fiMPEGLookup          X X X }
    (ShortTextID: '';    LongTextID: 'OWNE'), { fiOwnership           - X X }
    (ShortTextID: '';    LongTextID: 'PRIV'), { fiPrivate             - X X }
    (ShortTextID: 'CNT'; LongTextID: 'PCNT'), { fiPlayCounter         X X X }
    (ShortTextID: 'POP'; LongTextID: 'POPM'), { fiPopularimeter       X X X }
    (ShortTextID: '';    LongTextID: 'POSS'), { fiPositionsync        - X X }
    (ShortTextID: 'BUF'; LongTextID: 'RBUF'), { fiBufferSize          X X X }
    (ShortTextID: '';    LongTextID: 'RVA2'), { fiVolumeAdj2          - - X }
    (ShortTextID: 'RVA'; LongTextID: 'RVAD'), { fiVolumeAdj           X X d }
    (ShortTextID: 'REV'; LongTextID: 'RVRB'), { fiReverb              X X X }
    (ShortTextID: '';    LongTextID: 'SEEK'), { fiSeekFrame           - - X }
    (ShortTextID: '';    LongTextID: 'SIGN'), { fiSignature           - - X }
    (ShortTextID: 'SLT'; LongTextID: 'SYLT'), { fiSyncedLyrics        X X X }
    (ShortTextID: 'STC'; LongTextID: 'SYTC'), { fiSyncedTempo         X X X }
    (ShortTextID: 'TAL'; LongTextID: 'TALB'), { fiAlbum               X X X }
    (ShortTextID: 'TBP'; LongTextID: 'TBPM'), { fiBPM                 X X X }
    (ShortTextID: 'TCM'; LongTextID: 'TCOM'), { fiComposer            X X X }
    (ShortTextID: 'TCO'; LongTextID: 'TCON'), { fiContentType         X X X }
    (ShortTextID: 'TCR'; LongTextID: 'TCOP'), { fiCopyright           X X X }
    (ShortTextID: 'TDA'; LongTextID: 'TDAT'), { fiDate                X X d }
    (ShortTextID: '';    LongTextID: 'TDEN'), { fiEncodingTime        - - X }
    (ShortTextID: 'TDY'; LongTextID: 'TDLY'), { fiPlaylistDelay       X X X }
    (ShortTextID: '';    LongTextID: 'TDOR'), { fiOrigReleaseTime     - - X }
    (ShortTextID: '';    LongTextID: 'TDRC'), { fiRecordingTime       - - X }
    (ShortTextID: '';    LongTextID: 'TDRL'), { fiReleaseTime         - - X }
    (ShortTextID: '';    LongTextID: 'TDTG'), { fiTaggingTime         - - X }
    (ShortTextID: '';    LongTextID: 'TIPL'), { fiInvolvedPeople2     - - X }
    (ShortTextID: 'TEN'; LongTextID: 'TENC'), { fiEncodedBy           X X X }
    (ShortTextID: 'TXT'; LongTextID: 'TEXT'), { fiLyricist            X X X }
    (ShortTextID: 'TFT'; LongTextID: 'TFLT'), { fiFileType            X X X }
    (ShortTextID: 'TIM'; LongTextID: 'TIME'), { fiTime                X X d }
    (ShortTextID: 'TT1'; LongTextID: 'TIT1'), { fiContentGroup        X X X }
    (ShortTextID: 'TT2'; LongTextID: 'TIT2'), { fiTitle               X X X }
    (ShortTextID: 'TT3'; LongTextID: 'TIT3'), { fiSubTitle            X X X }
    (ShortTextID: 'TKE'; LongTextID: 'TKEY'), { fiInitialKey          X X X }
    (ShortTextID: 'TLA'; LongTextID: 'TLAN'), { fiLanguage            X X X }
    (ShortTextID: 'TLE'; LongTextID: 'TLEN'), { fiSongLen             X X X }
    (ShortTextID: '';    LongTextID: 'TMCL'), { fiMusicianCreditList  - - X }
    (ShortTextID: 'TMT'; LongTextID: 'TMED'), { fiMediaType           X X X }
    (ShortTextID: '';    LongTextID: 'TMOO'), { fiMood                - - X }
    (ShortTextID: 'TOT'; LongTextID: 'TOAL'), { fiOrigAlbum           X X X }
    (ShortTextID: 'TOF'; LongTextID: 'TOFN'), { fiOrigFileName        X X X }
    (ShortTextID: 'TOL'; LongTextID: 'TOLY'), { fiOrigLyricist        X X X }
    (ShortTextID: 'TOA'; LongTextID: 'TOPE'), { fiOrigArtist          X X X }
    (ShortTextID: 'TOR'; LongTextID: 'TORY'), { fiOrigYear            X X d }
    (ShortTextID: '';    LongTextID: 'TOWN'), { fiFileOwner           - X X }
    (ShortTextID: 'TP1'; LongTextID: 'TPE1'), { fiLeadArtist          X X X }
    (ShortTextID: 'TP2'; LongTextID: 'TPE2'), { fiBand                X X X }
    (ShortTextID: 'TP3'; LongTextID: 'TPE3'), { fiConductor           X X X }
    (ShortTextID: 'TP4'; LongTextID: 'TPE4'), { fiMixArtist           X X X }
    (ShortTextID: 'TPA'; LongTextID: 'TPOS'), { fiPartInSet           X X X }
    (ShortTextID: '';    LongTextID: 'TPRO'), { fiProducedNotice      - - X }
    (ShortTextID: 'TPB'; LongTextID: 'TPUB'), { fiPublisher           X X X }
    (ShortTextID: 'TRK'; LongTextID: 'TRCK'), { fiTrackNum            X X X }
    (ShortTextID: 'TRD'; LongTextID: 'TRDA'), { fiRecordingDates      X X d }
    (ShortTextID: 'TRN'; LongTextID: 'TRSN'), { fiNetRadioStation     X X X }
    (ShortTextID: 'TRO'; LongTextID: 'TRSO'), { fiNetRadioOwner       X X X }
    (ShortTextID: 'TSI'; LongTextID: 'TSIZ'), { fiSize                X X d }
    (ShortTextID: '';    LongTextID: 'TSOA'), { fiAlbumSortOrder      - - X }
    (ShortTextID: '';    LongTextID: 'TSOP'), { fiPerformerSortOrder  - - X }
    (ShortTextID: '';    LongTextID: 'TSOT'), { fiTitleSortOrder      - - X }
    (ShortTextID: 'TRC'; LongTextID: 'TSRC'), { fiISRC                X X X }
    (ShortTextID: '';    LongTextID: 'TSSE'), { fiEncoderSettings     - X X }
    (ShortTextID: 'TSS'; LongTextID: 'TSST'), { fiSetSubTitle         - - X }
    (ShortTextID: 'TXX'; LongTextID: 'TXXX'), { fiUserText            X X X }
    (ShortTextID: 'TYE'; LongTextID: 'TYER'), { fiYear                X X d }
    (ShortTextID: 'UFI'; LongTextID: 'UFID'), { fiUniqueFileID        X X X }
    (ShortTextID: '';    LongTextID: 'USER'), { fiTermsOfUse          - X X }
    (ShortTextID: 'ULT'; LongTextID: 'USLT'), { fiUnsyncedLyrics      X X X }
    (ShortTextID: 'WCM'; LongTextID: 'WCOM'), { fiWWWCommercialInfo   X X X }
    (ShortTextID: 'WCP'; LongTextID: 'WCOP'), { fiWWWCopyright        X X X }
    (ShortTextID: 'WAF'; LongTextID: 'WOAF'), { fiWWWAudioFile        X X X }
    (ShortTextID: 'WAR'; LongTextID: 'WOAR'), { fiWWWArtist           X X X }
    (ShortTextID: 'WAS'; LongTextID: 'WOAS'), { fiWWWAudioSource      X X X }
    (ShortTextID: 'WRA'; LongTextID: 'WORS'), { fiWWWRadioPage        X X X }
    (ShortTextID: 'WPY'; LongTextID: 'WPAY'), { fiWWWPayment          X X X }
    (ShortTextID: 'WPB'; LongTextID: 'WPUB'), { fiWWWPublisher        X X X }
    (ShortTextID: 'WXX'; LongTextID: 'WXXX'), { fiWWWUser             X X X }
    (ShortTextID: 'CRM'; LongTextID: ''    ), { fiMetaCrypto          X - - }
    (ShortTextID: 'CDM'; LongTextID: ''    )  { fiMetaCompressio      X - - }
    );

  { http://www.loc.gov/standards/iso639-2/englangn.html }

  CISO_639_2Data: array [0..474] of TShortToLongName =
  (
   {0}(S: 'aar'; L: 'Afar'),
    (S: 'abk'; L: 'Abkhazian'),
    (S: 'ace'; L: 'Achinese'),
    (S: 'ach'; L: 'Acoli'),
    (S: 'ada'; L: 'Adangme'),
    (S: 'afa'; L: 'Afro-Asiatic (Other)'),
    (S: 'afh'; L: 'Afrihili'),
    (S: 'afr'; L: 'Afrikaans'),
    (S: 'aka'; L: 'Akan'),
    (S: 'akk'; L: 'Akkadian'),
    {10}(S: 'alb'; L: 'Albanian'), // Also 'sqi'
    (S: 'ale'; L: 'Aleut'),
    (S: 'alg'; L: 'Algonquian languages'),
    (S: 'amh'; L: 'Amharic'),
    (S: 'ang'; L: 'English, Old (ca.450-1100)'),
    (S: 'apa'; L: 'Apache languages'),
    (S: 'ara'; L: 'Arabic'),
    (S: 'arc'; L: 'Aramaic'),
    (S: 'arg'; L: 'Aragonese'),
    (S: 'arm'; L: 'Armenian'), // Also 'hye'
    {20}(S: 'arn'; L: 'Araucanian'),
    (S: 'arp'; L: 'Arapaho'),
    (S: 'art'; L: 'Artificial (Other)'),
    (S: 'arw'; L: 'Arawak'),
    (S: 'asm'; L: 'Assamese'),
    (S: 'ast'; L: 'Asturian; Bable'),
    (S: 'ath'; L: 'Athapascan languages'),
    (S: 'aus'; L: 'Australian languages'),
    (S: 'ava'; L: 'Avaric'),
    (S: 'ave'; L: 'Avestan'),
    {30}(S: 'awa'; L: 'Awadhi'),
    (S: 'aym'; L: 'Aymara'),
    (S: 'aze'; L: 'Azerbaijani'),
    (S: 'bad'; L: 'Banda'),
    (S: 'bai'; L: 'Bamileke languages'),
    (S: 'bak'; L: 'Bashkir'),
    (S: 'bal'; L: 'Baluchi'),
    (S: 'bam'; L: 'Bambara'),
    (S: 'ban'; L: 'Balinese'),
    (S: 'baq'; L: 'Basque'), // Also 'eus'
    {40}(S: 'bas'; L: 'Basa'),
    (S: 'bat'; L: 'Baltic (Other)'),
    (S: 'bej'; L: 'Beja'),
    (S: 'bel'; L: 'Belarusian'),
    (S: 'bem'; L: 'Bemba'),
    (S: 'ben'; L: 'Bengali'),
    (S: 'ber'; L: 'Berber (Other)'),
    (S: 'bho'; L: 'Bhojpuri'),
    (S: 'bih'; L: 'Bihari'),
    (S: 'bik'; L: 'Bikol'),
    {50}(S: 'bin'; L: 'Bini'),
    (S: 'bis'; L: 'Bislama'),
    (S: 'bla'; L: 'Siksika'),
    (S: 'bnt'; L: 'Bantu (Other)'),
    (S: 'bod'; L: 'Tibetan'), // Also 'tib'
    (S: 'bos'; L: 'Bosnian'),
    (S: 'bra'; L: 'Braj'),
    (S: 'bre'; L: 'Breton'),
    (S: 'btk'; L: 'Batak (Indonesia)'),
    (S: 'bua'; L: 'Buriat'),
    {60}(S: 'bug'; L: 'Buginese'),
    (S: 'bul'; L: 'Bulgarian'),
    (S: 'bur'; L: 'Burmese'), // Also 'mya'
    (S: 'cad'; L: 'Caddo'),
    (S: 'cai'; L: 'Central American Indian (Other)'),
    (S: 'car'; L: 'Carib'),
    (S: 'cat'; L: 'Catalan'),
    (S: 'cau'; L: 'Caucasian (Other)'),
    (S: 'ceb'; L: 'Cebuano'),
    (S: 'cel'; L: 'Celtic (Other)'),
    {70}(S: 'ces'; L: 'Czech'), // Also 'cze'
    (S: 'cha'; L: 'Chamorro'),
    (S: 'chb'; L: 'Chibcha'),
    (S: 'che'; L: 'Chechen'),
    (S: 'chg'; L: 'Chagatai'),
    (S: 'chi'; L: 'Chinese'), // Also 'zho'
    (S: 'chk'; L: 'Chuukese'),
    (S: 'chm'; L: 'Mari'),
    (S: 'chn'; L: 'Chinook jargon'),
    (S: 'cho'; L: 'Choctaw'),
    {80}(S: 'chp'; L: 'Chipewyan'),
    (S: 'chr'; L: 'Cherokee'),
    (S: 'chu'; L: 'Old Bulgarian'),
    (S: 'chv'; L: 'Chuvash'),
    (S: 'chy'; L: 'Cheyenne'),
    (S: 'cmc'; L: 'Chamic languages'),
    (S: 'cop'; L: 'Coptic'),
    (S: 'cor'; L: 'Cornish'),
    (S: 'cos'; L: 'Corsican'),
    (S: 'cpe'; L: 'Creoles and pidgins, English-based (Other)'),
    {90}(S: 'cpf'; L: 'Creoles and pidgins, French-based (Other)'),
    (S: 'cpp'; L: 'Creoles and pidgins, Portuguese-based (Other)'),
    (S: 'cre'; L: 'Cree'),
    (S: 'crp'; L: 'Creoles and pidgins(Other)'),
    (S: 'cus'; L: 'Cushitic (Other)'),
    (S: 'cym'; L: 'Welsh'), // Also 'wel'
    (S: 'cze'; L: 'Czech'), // Also 'ces'
    (S: 'dak'; L: 'Dakota'),
    (S: 'dan'; L: 'Danish'),
    (S: 'dar'; L: 'Dargwa'),
    {100}(S: 'day'; L: 'Dayak'),
    (S: 'del'; L: 'Delaware'),
    (S: 'den'; L: 'Slave (Athapascan)'),
    (S: 'deu'; L: 'German'), // Also 'ger'
    (S: 'dgr'; L: 'Dogrib'),
    (S: 'din'; L: 'Dinka'),
    (S: 'div'; L: 'Divehi'),
    (S: 'doi'; L: 'Dogri'),
    (S: 'dra'; L: 'Dravidian (Other)'),
    (S: 'dua'; L: 'Duala'),
    {110}(S: 'dum'; L: 'Dutch, Middle (ca. 1050-1350)'),
    (S: 'dut'; L: 'Dutch'), // Also 'nld'
    (S: 'dyu'; L: 'Dyula'),
    (S: 'dzo'; L: 'Dzongkha'),
    (S: 'efi'; L: 'Efik'),
    (S: 'egy'; L: 'Egyptian (Ancient)'),
    (S: 'eka'; L: 'Ekajuk'),
    (S: 'ell'; L: 'Greek, Modern (1453-)'), // Also 'gre'
    (S: 'elx'; L: 'Elamite'),
    (S: 'eng'; L: 'English'),
    {120}(S: 'enm'; L: 'English, Middle (1100-1500)'),
    (S: 'epo'; L: 'Esperanto'),
    (S: 'est'; L: 'Estonian'),
    (S: 'eus'; L: 'Basque'), // Also 'baq'
    (S: 'ewe'; L: 'Ewe'),
    (S: 'ewo'; L: 'Ewondo'),
    (S: 'fan'; L: 'Fang'),
    (S: 'fao'; L: 'Faroese'),
    (S: 'fas'; L: 'Persian'), // Also 'per'
    (S: 'fat'; L: 'Fanti'),
    {130}(S: 'fij'; L: 'Fijian'),
    (S: 'fin'; L: 'Finnish'),
    (S: 'fiu'; L: 'Finno-Ugrian (Other)'),
    (S: 'fon'; L: 'Fon'),
    (S: 'fra'; L: 'French'), // Also 'fre'
    (S: 'fre'; L: 'French'), // Also 'fra'
    (S: 'frm'; L: 'French, Middle (ca.1400-1600)'),
    (S: 'fro'; L: 'French, Old (842-ca.1400)'),
    (S: 'fry'; L: 'Frisian'),
    (S: 'ful'; L: 'Fulah'),
    {140}(S: 'fur'; L: 'Friulian'),
    (S: 'gaa'; L: 'Ga'),
    (S: 'gay'; L: 'Gayo'),
    (S: 'gba'; L: 'Gbaya'),
    (S: 'gem'; L: 'Germanic (Other)'),
    (S: 'geo'; L: 'Georgian'), // Also 'kat'
    (S: 'ger'; L: 'German'), // Also 'deu'
    (S: 'gez'; L: 'Geez'),
    (S: 'gil'; L: 'Gilbertese'),
    (S: 'gla'; L: 'Gaelic; Scottish Gaelic'),
    {150}(S: 'gle'; L: 'Irish'),
    (S: 'glg'; L: 'Gallegan'),
    (S: 'glv'; L: 'Manx'),
    (S: 'gmh'; L: 'German, Middle High (ca.1050-1500)'),
    (S: 'goh'; L: 'German, Old High (ca.750-1050)'),
    (S: 'gon'; L: 'Gondi'),
    (S: 'gor'; L: 'Gorontalo'),
    (S: 'got'; L: 'Gothic'),
    (S: 'grb'; L: 'Grebo'),
    (S: 'grc'; L: 'Greek, Ancient (to 1453)'),
    {160}(S: 'gre'; L: 'Greek, Modern (1453-)'), // Also 'ell'
    (S: 'grn'; L: 'Guarani'),
    (S: 'guj'; L: 'Gujarati'),
    (S: 'gwi'; L: 'Gwich´in'),
    (S: 'hai'; L: 'Haida'),
    (S: 'hau'; L: 'Hausa'),
    (S: 'haw'; L: 'Hawaiian'),
    (S: 'heb'; L: 'Hebrew'),
    (S: 'her'; L: 'Herero'),
    (S: 'hil'; L: 'Hiligaynon'),
    {170}(S: 'him'; L: 'Himachali'),
    (S: 'hin'; L: 'Hindi'),
    (S: 'hit'; L: 'Hittite'),
    (S: 'hmn'; L: 'Hmong'),
    (S: 'hmo'; L: 'Hiri Motu'),
    (S: 'hrv'; L: 'Croatian'), // Also 'scr'
    (S: 'hun'; L: 'Hungarian'),
    (S: 'hup'; L: 'Hupa'),
    (S: 'hye'; L: 'Armenian'), // Also 'arm'
    (S: 'iba'; L: 'Iban'),
    {180}(S: 'ibo'; L: 'Igbo'),
    (S: 'ice'; L: 'Icelandic'), // Also 'isl'
    (S: 'ido'; L: 'Ido'),
    (S: 'iii'; L: 'Sichuan Yi'),
    (S: 'ijo'; L: 'Ijo'),
    (S: 'iku'; L: 'Inuktitut'),
    (S: 'ile'; L: 'Interlingue'),
    (S: 'ilo'; L: 'Iloko'),
    (S: 'ina'; L: 'Interlingua (International Auxiliary Language Association)'),
    (S: 'inc'; L: 'Indic (Other)'),
    {190}(S: 'ind'; L: 'Indonesian'),
    (S: 'ine'; L: 'Indo-European (Other)'),
    (S: 'inh'; L: 'Ingush'),
    (S: 'ipk'; L: 'Inupiaq'),
    (S: 'ira'; L: 'Iranian (Other)'),
    (S: 'iro'; L: 'Iroquoian languages'),
    (S: 'isl'; L: 'Icelandic'), // Also 'ice'
    (S: 'ita'; L: 'Italian'),
    (S: 'jav'; L: 'Javanese'),
    (S: 'jpn'; L: 'Japanese'),
    {200}(S: 'jpr'; L: 'Judeo-Persian'),
    (S: 'jrb'; L: 'Judeo-Arabic'),
    (S: 'kaa'; L: 'Kara-Kalpak'),
    (S: 'kab'; L: 'Kabyle'),
    (S: 'kac'; L: 'Kachin'),
    (S: 'kal'; L: 'Kalaallisut'),
    (S: 'kam'; L: 'Kamba'),
    (S: 'kan'; L: 'Kannada'),
    (S: 'kar'; L: 'Karen'),
    (S: 'kas'; L: 'Kashmiri'),
    {210}(S: 'kat'; L: 'Georgian'), // Also 'geo'
    (S: 'kau'; L: 'Kanuri'),
    (S: 'kaw'; L: 'Kawi'),
    (S: 'kaz'; L: 'Kazakh'),
    (S: 'kbd'; L: 'Kabardian'),
    (S: 'kha'; L: 'Khasi'),
    (S: 'khi'; L: 'Khoisan (Other)'),
    (S: 'khm'; L: 'Khmer'),
    (S: 'kho'; L: 'Khotanese'),
    (S: 'kik'; L: 'Kikuyu; Gikuyu'),
    {220}(S: 'kin'; L: 'Kinyarwanda'),
    (S: 'kir'; L: 'Kirghiz'),
    (S: 'kmb'; L: 'Kimbundu'),
    (S: 'kok'; L: 'Konkani'),
    (S: 'kom'; L: 'Komi'),
    (S: 'kon'; L: 'Kongo'),
    (S: 'kor'; L: 'Korean'),
    (S: 'kos'; L: 'Kosraean'),
    (S: 'kpe'; L: 'Kpelle'),
    (S: 'kro'; L: 'Kru'),
    {230}(S: 'kru'; L: 'Kurukh'),
    (S: 'kua'; L: 'Kuanyama; Kwanyama'),
    (S: 'kum'; L: 'Kumyk'),
    (S: 'kur'; L: 'Kurdish'),
    (S: 'kut'; L: 'Kutenai'),
    (S: 'lad'; L: 'Ladino'),
    (S: 'lah'; L: 'Lahnda'),
    (S: 'lam'; L: 'Lamba'),
    (S: 'lao'; L: 'Lao'),
    (S: 'lat'; L: 'Latin'),
    {240}(S: 'lav'; L: 'Latvian'),
    (S: 'lez'; L: 'Lezghian'),
    (S: 'lim'; L: 'Limburgan'),
    (S: 'lin'; L: 'Lingala'),
    (S: 'lit'; L: 'Lithuanian'),
    (S: 'lol'; L: 'Mongo'),
    (S: 'loz'; L: 'Lozi'),
    (S: 'ltz'; L: 'Luxembourgish'),
    (S: 'lua'; L: 'Luba-Lulua'),
    (S: 'lub'; L: 'Luba-Katanga'),
    {250}(S: 'lug'; L: 'Ganda'),
    (S: 'lui'; L: 'Luiseno'),
    (S: 'lun'; L: 'Lunda'),
    (S: 'luo'; L: 'Luo (Kenya and Tanzania)'),
    (S: 'lus'; L: 'Lushai'),
    (S: 'mac'; L: 'Macedonian'), // Also 'mkd'
    (S: 'mad'; L: 'Madurese'),
    (S: 'mag'; L: 'Magahi'),
    (S: 'mah'; L: 'Marshallese'),
    (S: 'mai'; L: 'Maithili'),
    {260}(S: 'mak'; L: 'Makasar'),
    (S: 'mal'; L: 'Malayalam'),
    (S: 'man'; L: 'Mandingo'),
    (S: 'mao'; L: 'Maori'), // Also 'mri'
    (S: 'map'; L: 'Austronesian (Other)'),
    (S: 'mar'; L: 'Marathi'),
    (S: 'mas'; L: 'Masai'),
    (S: 'may'; L: 'Malay'), // Also 'msa'
    (S: 'mdr'; L: 'Mandar'),
    (S: 'men'; L: 'Mende'),
    {270}(S: 'mga'; L: 'Irish, Middle (900-1200)'),
    (S: 'mic'; L: 'Micmac'),
    (S: 'min'; L: 'Minangkabau'),
    (S: 'mis'; L: 'Miscellaneous languages'),
    (S: 'mkd'; L: 'Macedonian'), // Also 'mac'
    (S: 'mkh'; L: 'Mon-Khmer (Other)'),
    (S: 'mlg'; L: 'Malagasy'),
    (S: 'mlt'; L: 'Maltese'),
    (S: 'mnc'; L: 'Manchu'),
    (S: 'mni'; L: 'Manipuri'),
    {280}(S: 'mno'; L: 'Manobo languages'),
    (S: 'moh'; L: 'Mohawk'),
    (S: 'mol'; L: 'Moldavian'),
    (S: 'mon'; L: 'Mongolian'),
    (S: 'mos'; L: 'Mossi'),
    (S: 'mri'; L: 'Maori'), // Also 'mao'
    (S: 'msa'; L: 'Malay'), // Also 'may'
    (S: 'mul'; L: 'Multiple languages'),
    (S: 'mun'; L: 'Munda languages'),
    (S: 'mus'; L: 'Creek'),
    {290}(S: 'mwr'; L: 'Marwari'),
    (S: 'mya'; L: 'Burmese'), // Also 'bur'
    (S: 'myn'; L: 'Mayan languages'),
    (S: 'nah'; L: 'Nahuatl'),
    (S: 'nai'; L: 'North American Indian (Other)'),
    (S: 'nap'; L: 'Neapolitan'),
    (S: 'nau'; L: 'Nauru'),
    (S: 'nav'; L: 'Navajo; Navaho'),
    (S: 'nbl'; L: 'Ndebele, South'),
    (S: 'nde'; L: 'Ndebele, North'),
    {300}(S: 'ndo'; L: 'Ndonga'),
    (S: 'nds'; L: 'German, Low'),
    (S: 'nep'; L: 'Nepali'),
    (S: 'new'; L: 'Newari'),
    (S: 'nia'; L: 'Nias'),
    (S: 'nic'; L: 'Niger-Kordofanian (Other)'),
    (S: 'niu'; L: 'Niuean'),
    (S: 'nld'; L: 'Dutch'), // Also 'dut'
    (S: 'nno'; L: 'Nynorsk, Norwegian'),
    (S: 'nob'; L: 'Bokmål, Norwegian'),
    {310}(S: 'non'; L: 'Norse, Old'),
    (S: 'nor'; L: 'Norwegian'),
    (S: 'nso'; L: 'Sotho, Northern'),
    (S: 'nub'; L: 'Nubian languages'),
    (S: 'nya'; L: 'Chichewa'),
    (S: 'nym'; L: 'Nyamwezi'),
    (S: 'nyn'; L: 'Nyankole'),
    (S: 'nyo'; L: 'Nyoro'),
    (S: 'nzi'; L: 'Nzima'),
    (S: 'oci'; L: 'Occitan (post 1500); Provençal'),
    {320}(S: 'oji'; L: 'Ojibwa'),
    (S: 'ori'; L: 'Oriya'),
    (S: 'orm'; L: 'Oromo'),
    (S: 'osa'; L: 'Osage'),
    (S: 'oss'; L: 'Ossetian'),
    (S: 'ota'; L: 'Turkish, Ottoman (1500-1928)'),
    (S: 'oto'; L: 'Otomian languages'),
    (S: 'paa'; L: 'Papuan (Other)'),
    (S: 'pag'; L: 'Pangasinan'),
    (S: 'pal'; L: 'Pahlavi'),
    {330}(S: 'pam'; L: 'Pampanga'),
    (S: 'pan'; L: 'Panjabi'),
    (S: 'pap'; L: 'Papiamento'),
    (S: 'pau'; L: 'Palauan'),
    (S: 'peo'; L: 'Persian, Old (ca.600-400)'),
    (S: 'per'; L: 'Persian'), // Also 'fas'
    (S: 'phi'; L: 'Philippine (Other)'),
    (S: 'phn'; L: 'Phoenician'),
    (S: 'pli'; L: 'Pali'),
    (S: 'pol'; L: 'Polish'),
    {340}(S: 'pon'; L: 'Pohnpeian'),
    (S: 'por'; L: 'Portuguese'),
    (S: 'pra'; L: 'Prakrit languages'),
    (S: 'pro'; L: 'Provençal, Old (to 1500)'),
    (S: 'pus'; L: 'Pushto'),
    (S: 'qtz'; L: 'Reserved for local user; qaa'),
    (S: 'que'; L: 'Quechua'),
    (S: 'raj'; L: 'Rajasthani'),
    (S: 'rap'; L: 'Rapanui'),
    (S: 'rar'; L: 'Rarotongan'),
    {350}(S: 'roa'; L: 'Romance (Other)'),
    (S: 'roh'; L: 'Raeto-Romance'),
    (S: 'rom'; L: 'Romany'),
    (S: 'ron'; L: 'Romanian'), // Also 'rum'
    (S: 'rum'; L: 'Romanian'), // Also 'ron'
    (S: 'run'; L: 'Rundi'),
    (S: 'rus'; L: 'Russian'),
    (S: 'sad'; L: 'Sandawe'),
    (S: 'sag'; L: 'Sango'),
    (S: 'sah'; L: 'Yakut'),
    {360}(S: 'sai'; L: 'South American Indian (Other)'),
    (S: 'sal'; L: 'Salishan languages'),
    (S: 'sam'; L: 'Samaritan Aramaic'),
    (S: 'san'; L: 'Sanskrit'),
    (S: 'sas'; L: 'Sasak'),
    (S: 'sat'; L: 'Santali'),
    (S: 'scc'; L: 'Serbian'), // Also 'srp'
    (S: 'sco'; L: 'Scots'),
    (S: 'scr'; L: 'Croatian'), // Also 'hrv'
    (S: 'sel'; L: 'Selkup'),
    {370}(S: 'sem'; L: 'Semitic (Other)'),
    (S: 'sga'; L: 'Irish, Old (to 900)'),
    (S: 'sgn'; L: 'Sign languages'),
    (S: 'shn'; L: 'Shan'),
    (S: 'sid'; L: 'Sidamo'),
    (S: 'sin'; L: 'Sinhalese'),
    (S: 'sio'; L: 'Siouan languages'),
    (S: 'sit'; L: 'Sino-Tibetan (Other)'),
    (S: 'sla'; L: 'Slavic (Other)'),
    (S: 'slk'; L: 'Slovak'), // Also 'slo'
    {380}(S: 'slo'; L: 'Slovak'), // Also 'slk'
    (S: 'slv'; L: 'Slovenian'),
    (S: 'sma'; L: 'Southern Sami'),
    (S: 'sme'; L: 'Northern Sami'),
    (S: 'smi'; L: 'Sami languages (Other)'),
    (S: 'smj'; L: 'Lule Sami'),
    (S: 'smn'; L: 'Inari Sami'),
    (S: 'smo'; L: 'Samoan'),
    (S: 'sms'; L: 'Skolt Sami'),
    (S: 'sna'; L: 'Shona'),
    {390}(S: 'snd'; L: 'Sindhi'),
    (S: 'snk'; L: 'Soninke'),
    (S: 'sog'; L: 'Sogdian'),
    (S: 'som'; L: 'Somali'),
    (S: 'son'; L: 'Songhai'),
    (S: 'sot'; L: 'Sotho, Southern'),
    (S: 'spa'; L: 'Spanish; Castilian'),
    (S: 'sqi'; L: 'Albanian'), // Also 'alb'
    (S: 'srd'; L: 'Sardinian'),
    (S: 'srp'; L: 'Serbian'), // Also 'scc'
    {400}(S: 'srr'; L: 'Serer'),
    (S: 'ssa'; L: 'Nilo-Saharan (Other)'),
    (S: 'ssw'; L: 'Swati'),
    (S: 'suk'; L: 'Sukuma'),
    (S: 'sun'; L: 'Sundanese'),
    (S: 'sus'; L: 'Susu'),
    (S: 'sux'; L: 'Sumerian'),
    (S: 'swa'; L: 'Swahili'),
    (S: 'swe'; L: 'Swedish'),
    (S: 'syr'; L: 'Syriac'),
    {410}(S: 'tah'; L: 'Tahitian'),
    (S: 'tai'; L: 'Tai (Other)'),
    (S: 'tam'; L: 'Tamil'),
    (S: 'tat'; L: 'Tatar'),
    (S: 'tel'; L: 'Telugu'),
    (S: 'tem'; L: 'Timne'),
    (S: 'ter'; L: 'Tereno'),
    (S: 'tet'; L: 'Tetum'),
    (S: 'tgk'; L: 'Tajik'),
    (S: 'tgl'; L: 'Tagalog'),
    {420}(S: 'tha'; L: 'Thai'),
    (S: 'tib'; L: 'Tibetan'), // Also 'bod'
    (S: 'tig'; L: 'Tigre'),
    (S: 'tir'; L: 'Tigrinya'),
    (S: 'tiv'; L: 'Tiv'),
    (S: 'tkl'; L: 'Tokelau'),
    (S: 'tli'; L: 'Tlingit'),
    (S: 'tmh'; L: 'Tamashek'),
    (S: 'tog'; L: 'Tonga (Nyasa)'),
    (S: 'ton'; L: 'Tonga (Tonga Islands)'),
    {430}(S: 'tpi'; L: 'Tok Pisin'),
    (S: 'tsi'; L: 'Tsimshian'),
    (S: 'tsn'; L: 'Tswana'),
    (S: 'tso'; L: 'Tsonga'),
    (S: 'tuk'; L: 'Turkmen'),
    (S: 'tum'; L: 'Tumbuka'),
    (S: 'tup'; L: 'Tupi languages'),
    (S: 'tur'; L: 'Turkish'),
    (S: 'tut'; L: 'Altaic (Other)'),
    (S: 'tvl'; L: 'Tuvalu'),
    {440}(S: 'twi'; L: 'Twi'),
    (S: 'tyv'; L: 'Tuvinian'),
    (S: 'uga'; L: 'Ugaritic'),
    (S: 'uig'; L: 'Uighur'),
    (S: 'ukr'; L: 'Ukrainian'),
    (S: 'umb'; L: 'Umbundu'),
    (S: 'und'; L: 'Undetermined'),
    (S: 'urd'; L: 'Urdu'),
    (S: 'uzb'; L: 'Uzbek'),
    (S: 'vai'; L: 'Vai'),
    {450}(S: 'ven'; L: 'Venda'),
    (S: 'vie'; L: 'Vietnamese'),
    (S: 'vol'; L: 'Volapük'),
    (S: 'vot'; L: 'Votic'),
    (S: 'wak'; L: 'Wakashan languages'),
    (S: 'wal'; L: 'Walamo'),
    (S: 'war'; L: 'Waray'),
    (S: 'was'; L: 'Washo'),
    (S: 'wel'; L: 'Welsh'), // Also 'cym'
    (S: 'wen'; L: 'Sorbian languages'),
    {460}(S: 'wln'; L: 'Walloon'),
    (S: 'wol'; L: 'Wolof'),
    (S: 'xho'; L: 'Xhosa'),
    (S: 'yao'; L: 'Yao'),
    (S: 'yap'; L: 'Yapese'),
    (S: 'yid'; L: 'Yiddish'),
    (S: 'yor'; L: 'Yoruba'),
    (S: 'ypk'; L: 'Yupik languages'),
    (S: 'zap'; L: 'Zapotec'),
    (S: 'zen'; L: 'Zenaga'),
    {470}(S: 'zha'; L: 'Zhuang; Chuang'),
    (S: 'zho'; L: 'Chinese'), // Also 'chi'
    (S: 'znd'; L: 'Zande'),
    (S: 'zul'; L: 'Zulu'),
    (S: 'zun'; L: 'Zuni')
    );

  CID3Genres: array[0..147] of PChar = (

    { The following genres are defined in ID3v1 }

    {0}'Blues',
    'Classic Rock',
    'Country',
    'Dance',
    'Disco',
    'Funk',
    'Grunge',
    'Hip-Hop',
    'Jazz',
    'Metal',
    {10}'New Age',
    'Oldies',
    'Other',     { <= Default }
    'Pop',
    'R&B',
    'Rap',
    'Reggae',
    'Rock',
    'Techno',
    'Industrial',
    {20}'Alternative',
    'Ska',
    'Death Metal',
    'Pranks',
    'Soundtrack',
    'Euro-Techno',
    'Ambient',
    'Trip-Hop',
    'Vocal',
    'Jazz+Funk',
    {30}'Fusion',
    'Trance',
    'Classical',
    'Instrumental',
    'Acid',
    'House',
    'Game',
    'Sound Clip',
    'Gospel',
    'Noise',
    {40}'AlternRock',
    'Bass',
    'Soul',
    'Punk',
    'Space',
    'Meditative',
    'Instrumental Pop',
    'Instrumental Rock',
    'Ethnic',
    'Gothic',
    {50}'Darkwave',
    'Techno-Industrial',
    'Electronic',
    'Pop-Folk',
    'Eurodance',
    'Dream',
    'Southern Rock',
    'Comedy',
    'Cult',
    'Gangsta',
    {60}'Top 40',
    'Christian Rap',
    'Pop/Funk',
    'Jungle',
    'Native American',
    'Cabaret',
    'New Wave',
    'Psychedelic', // = 'Psychadelic' in ID3 docs, 'Psychedelic' in winamp.
    'Rave',
    'Showtunes',
    {70}'Trailer',
    'Lo-Fi',
    'Tribal',
    'Acid Punk',
    'Acid Jazz',
    'Polka',
    'Retro',
    'Musical',
    'Rock & Roll',
    'Hard Rock',

    { The following genres are Winamp extensions }

    {80}'Folk',
    'Folk-Rock',
    'National Folk',
    'Swing',
    'Fast Fusion',
    'Bebob',
    'Latin',
    'Revival',
    'Celtic',
    'Bluegrass',
    {90}'Avantgarde',
    'Gothic Rock',
    'Progressive Rock',
    'Psychedelic Rock',
    'Symphonic Rock',
    'Slow Rock',           
    'Big Band',
    'Chorus',
    'Easy Listening',
    'Acoustic',
    {100}'Humour',
    'Speech',
    'Chanson',
    'Opera',
    'Chamber Music',
    'Sonata',
    'Symphony',
    'Booty Bass',
    'Primus',
    'Porn Groove',
    {110}'Satire',
    'Slow Jam',
    'Club',
    'Tango',
    'Samba',
    'Folklore',
    'Ballad',
    'Power Ballad',
    'Rhythmic Soul',
    'Freestyle',
    {120}'Duet',
    'Punk Rock',
    'Drum Solo',
    'A capella', // A Capella
    'Euro-House',
    'Dance Hall',

    { winamp ?? genres }

    'Goa',
    'Drum & Bass',
    'Club-House',
    'Hardcore',
    {130}'Terror',
    'Indie',
    'BritPop',
    'Negerpunk',
    'Polsk Punk',
    'Beat',
    'Christian Gangsta Rap',
    'Heavy Metal',
    'Black Metal',
    'Crossover',
    {140}'Contemporary Christian',
    'Christian Rock',

    { winamp 1.91 genres }

    'Merengue',
    'Salsa',
    'Trash Metal',

    { winamp 1.92 genres }

    'Anime',
    'JPop',
    'SynthPop'
   );

  CGenre_HighV1 = 79;
  CGenre_DefaultID = 12;

var
  GInstance: TJvID3TermFinder = nil;

//=== Local procedures =======================================================

function IndexOfLongString(Strings: TStrings; const ALongText: string): Integer;
{ Searches for a string in Strings that is a prefix of ALongText, this is used
  by the ID3 genres; problem is that some strings may have the same prefix, ie

  Pop
  Pop/Funk
  Pop-Folk

  If we search for a prefix for 'Pop/Funk or something' the binary search
  may return 'Pop', thus we use FindFrom to search some more

  Note:

  'Rock'      => Result = 17
  'Rocks'     => Result = 255 (nothing found)
  'Rock Rock' => Result = 17
}

  function IsPrefix(const S: string): Boolean;
  begin
    Result := (AnsiStrLIComp(PChar(S), PChar(ALongText), Length(S)) = 0);
  end;

  function HasSpaceAfterPrefix(const Prefix: string): Boolean;
  { PRE: IsPrefix(Prefix) = True }
  var
    C: Integer;
  begin
    C := Length(Prefix) - Length(ALongText);
    Result := (C = 0) or ((C < 0) and (ALongText[Length(Prefix) + 1] = ' '));
  end;

  function FindFrom(const Index: Integer): Integer;
  begin
    { Try to find I with IsPrefix(Strings[I]) and HasSpaceAfterPrefix(Strings[i]) }

    if Length(Strings[Index]) < Length(ALongText) then
    begin
      { Now is valid: IsPrefix(Strings[Result]) }

      Result := Index + 1;

      { Strings is sorted thus it's only usefull to look at higher indexes than
        Index ie only at higher indexes are possibly longer prefixes of ALongText }
      while (Result < Strings.Count) and IsPrefix(Strings[Result]) do
        Inc(Result);

      { Strings[Result] is not ok, Strings[Result-1] is ok }
      Dec(Result);

      { Now is valid: IsPrefix(Strings[Result]) }
    end
    else
    if Length(ALongText) < Length(Strings[Index]) then
    begin
      Result := Index - 1;

      while (Result >= 0) and (Length(Strings[Result]) > Length(ALongText)) do
        if AnsiStrLIComp(PChar(Strings[Result]), PChar(ALongText), Length(ALongText)) = 0 then
          Dec(Result)
        else
        begin
          { Not found }
          Result := -1;
          Exit;
        end;

      if (Result < 0) or not IsPrefix(Strings[Result]) then
      begin
        { Not found }
        Result := -1;
        Exit;
      end;

      { Now is valid: IsPrefix(Strings[Result]) }
    end
    else
    begin
      { Found }
      Result := Index;
      Exit;
    end;

    { Now is valid: IsPrefix(Strings[Result]) }

    if HasSpaceAfterPrefix(Strings[Result]) then
      { Found }
      Exit;

    Dec(Result);

    { Now go down until we find a string X with X + some separator is a prefix
      of ALongText }
    while Result >= 0 do
      if IsPrefix(Strings[Result]) then
      begin
        if HasSpaceAfterPrefix(Strings[Result]) then
          { Found }
          Exit
        else
          Dec(Result);
      end
      else
      begin
        { Not found }
        Result := -1;
        Exit;
      end;
  end;
var
  Top, Mid, C: Integer;
begin
  Result := 0;
  Top := Strings.Count - 1;
  while Result <= Top do
  begin
    Mid := (Result + Top) shr 1;
    C := AnsiStrLIComp(PChar(Strings[Mid]), PChar(ALongText),
      Min(Length(Strings[Mid]), Length(ALongText)));
    if C < 0 then
      Result := Mid + 1
    else
    if C = 0 then
    begin
      Result := FindFrom(Mid);
      Exit;
    end
    else { C > 0 }
      Top := Mid - 1;
  end;

  { Nothing found }
  Result := -1;
end;

//=== Global procedures ======================================================

function ID3_FrameIDToString(const ID: TJvID3FrameID; const Size: Integer): string;
begin
  case Size of
    3:
      Result := CID3FrameDefs[ID].ShortTextID;
    4:
      Result := CID3FrameDefs[ID].LongTextID;
  else
    raise EJVCLException.CreateRes(@RsEFrameIDSizeCanOnlyBe34);
  end;
end;

procedure ID3_Genres(Strings: TStrings; const InclWinampGenres: Boolean);
begin
  TJvID3TermFinder.Instance.ID3Genres(Strings, InclWinampGenres);
end;

function ID3_GenreToID(const AGenre: string; const InclWinampGenres: Boolean): Integer;
begin
  Result := TJvID3TermFinder.Instance.ID3GenreToID(AGenre, True);
end;

function ID3_IDToGenre(const ID: Integer; const InclWinampGenres: Boolean): string;
const
  HighValue: array [Boolean] of Byte = (CGenre_HighV1, High(CID3Genres));
begin
  { Note : In Winamp, ID = 255 then Genre = '' }
  if (ID >= Low(CID3Genres)) and (ID <= HighValue[InclWinampGenres]) then
    Result := CID3Genres[ID]
  else
    Result := '';
end;

function ID3_LongGenreToID(const ALongGenre: string; const InclWinampGenres: Boolean = True): Integer;
begin
  Result := TJvID3TermFinder.Instance.ID3LongGenreToID(ALongGenre, InclWinampGenres);
end;

function ID3_StringToFrameID(const S: string): TJvID3FrameID;
var
  L: Integer;
begin
  L := Length(S);
  with TJvID3TermFinder.Instance do
    case L of
      0:
        Result := fiPaddingFrame;
      3:
        if S = #0#0#0 then
          Result := fiPaddingFrame
        else
          Result := ID3ShortTextToFrameID(S);
      4:
        if S = #0#0#0#0 then
          Result := fiPaddingFrame
        else
          Result := ID3LongTextToFrameID(S);
    else
      Result := fiErrorFrame
    end;
end;

function ISO_639_2CodeToName(const Code: string): string;
var
  Index: Integer;
begin
  if Length(Code) <> 3 then
  begin
    Result := '';
    Exit;
  end;

  Index := TJvID3TermFinder.Instance.ISO_639_2CodeToIndex(Code);
  if Index >= Low(CISO_639_2Data) then
    Result := CISO_639_2Data[Index].L
  else
    Result := '';
end;

function ISO_639_2IsCode(const Code: string): Boolean;
begin
  Result := (Length(Code) = 3) and
    (TJvID3TermFinder.Instance.ISO_639_2CodeToIndex(Code) >= Low(CISO_639_2Data));
end;

procedure ISO_639_2Names(Strings: TStrings);
begin
  TJvID3TermFinder.Instance.ISO_639_2Names(Strings);
end;

function ISO_639_2NameToCode(const Name: string): string;
var
  Index: Integer;
begin
  Index := TJvID3TermFinder.Instance.ISO_639_2NameToIndex(Name);
  if (Index < Low(CISO_639_2Data)) or (Index > High(CISO_639_2Data)) then
    Result := ''
  else
    Result := CISO_639_2Data[Index].S;
end;

//=== { TJvID3TermFinder } ===================================================

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

procedure TJvID3TermFinder.BuildList_ID3Genres;
var
  I: Integer;
begin
  if Assigned(FLists[ltID3Genres]) then
    Exit;

  FLists[ltID3Genres] := TStringList.Create;
  with FLists[ltID3Genres] do
  begin
    { There are no duplicates in the list }
    Duplicates := dupError;
    Sorted := True;

    for I := Low(CID3Genres) to High(CID3Genres) do
      AddObject(CID3Genres[I], TObject(I));
  end;
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

procedure TJvID3TermFinder.BuildList_ISO_639_2Code;
var
  I: Integer;
begin
  if Assigned(FLists[ltISO_639_2Code]) then
    Exit;

  FLists[ltISO_639_2Code] := TStringList.Create;
  with FLists[ltISO_639_2Code] do
  begin
    { There are no duplicates in the list }
    Duplicates := dupError;
    Sorted := True;

    for I := Low(CISO_639_2Data) to High(CISO_639_2Data) do
      AddObject(CISO_639_2Data[I].S, TObject(I));
  end;
end;

procedure TJvID3TermFinder.BuildList_ISO_639_2Name;
var
  I: Integer;
begin
  if Assigned(FLists[ltISO_639_2Name]) then
    Exit;

  FLists[ltISO_639_2Name] := TStringList.Create;
  with FLists[ltISO_639_2Name] do
  begin
    { There are duplicates in the list }
    Duplicates := dupIgnore;
    Sorted := True;

    for I := Low(CISO_639_2Data) to High(CISO_639_2Data) do
      AddObject(CISO_639_2Data[I].L, TObject(I));
  end;
end;

procedure TJvID3TermFinder.ID3Genres(AStrings: TStrings;
  const InclWinampGenres: Boolean);
var
  I: Integer;
begin
  BuildList_ID3Genres;

  with FLists[ltID3Genres] do
  begin
    AStrings.BeginUpdate;
    try
      AStrings.Clear;

      { In Winamp, ID = 255 then Genre = '' }
      if InclWinampGenres then
        AStrings.AddObject('', TObject(255));

      for I := 0 to FLists[ltID3Genres].Count - 1 do
        if InclWinampGenres or (Integer(Objects[I]) <= CGenre_HighV1) then
          AStrings.AddObject(Strings[I], Objects[I]);
    finally
      AStrings.EndUpdate;
    end;
  end;
end;

function TJvID3TermFinder.ID3GenreToID(const AGenre: string; const InclWinampGenres: Boolean): Integer;
const
  { In Winamp, ID = 255 then Genre = '' }
  CDefaultGenre: array [Boolean] of Byte = (CGenre_DefaultID, 255);
begin
  BuildList_ID3Genres;

  if AGenre = '' then
    Result := CDefaultGenre[InclWinampGenres]
  else
  begin
    Result := FLists[ltID3Genres].IndexOf(AGenre);

    { Special case: 'Psychadelic' }
    if (Result < 0) and (AnsiCompareText(AGenre, 'psychadelic') = 0) then
      Result := FLists[ltID3Genres].IndexOf('Psychedelic');

    if not InclWinampGenres and (Result > CGenre_HighV1) then
      Result := -1;

    if Result >= 0 then
      Result := Integer(FLists[ltID3Genres].Objects[Result])
    else
      Result := CDefaultGenre[InclWinampGenres];
  end;
end;

function TJvID3TermFinder.ID3LongGenreToID(const ALongGenre: string;
  const InclWinampGenres: Boolean): Integer;
const
  { In Winamp, ID = 255 then Genre = '' }
  CDefaultGenre: array [Boolean] of Byte = (CGenre_DefaultID, 255);
begin
  BuildList_ID3Genres;

  if ALongGenre = '' then
  begin
    Result := CDefaultGenre[InclWinampGenres];
    Exit;
  end;

  Result := IndexOfLongString(FLists[ltID3Genres], ALongGenre);

  { Special case: 'Psychadelic' }
  if (Result < 0) and (AnsiStrLIComp(PChar(ALongGenre), 'psychadelic', Length(ALongGenre)) = 0) then
    Result := FLists[ltID3Genres].IndexOf('Psychedelic');

  if not InclWinampGenres and (Result > CGenre_HighV1) then
    Result := -1;

  if Result >= 0 then
    Result := Integer(FLists[ltID3Genres].Objects[Result])
  else
    Result := CDefaultGenre[InclWinampGenres];
end;

function TJvID3TermFinder.ID3LongTextToFrameID(
  const S: string): TJvID3FrameID;
var
  I: Integer;
begin
  if not IsFrameOk(S) then
  begin
    Result := fiErrorFrame;
    Exit;
  end;

  BuildList_ID3LongText;

  I := FLists[ltID3LongText].IndexOf(S);
  if I < 0 then
    Result := fiUnknownFrame
  else
    Result := TJvID3FrameID(FLists[ltID3LongText].Objects[I]);
end;

function TJvID3TermFinder.ID3ShortTextToFrameID(
  const S: string): TJvID3FrameID;
var
  I: Integer;
begin
  if not IsFrameOk(S) then
  begin
    Result := fiErrorFrame;
    Exit;
  end;

  BuildList_ID3ShortText;

  I := FLists[ltID3ShortText].IndexOf(S);
  if I < 0 then
    Result := fiUnknownFrame
  else
    Result := TJvID3FrameID(FLists[ltID3ShortText].Objects[I]);
end;

class function TJvID3TermFinder.Instance: TJvID3TermFinder;
begin
  if not Assigned(GInstance) then
  begin
    GInstance := TJvID3TermFinder.Create;
    AddFinalizeObjectNil(sUnitName, TObject(GInstance));
  end;
  Result := GInstance;
end;

function TJvID3TermFinder.IsFrameOk(const S: string): Boolean;
var
  I: Integer;
begin
  { The frame ID must be made out of the characters capital A-Z and 0-9. }
  Result := False;

  for I := 1 to Length(S) do
    if not (S[I] in (['A'..'Z'] + DigitChars)) then
      Exit;

  Result := True;
end;

function TJvID3TermFinder.ISO_639_2CodeToIndex(
  const ACode: string): Integer;
begin
  BuildList_ISO_639_2Code;

  Result := FLists[ltISO_639_2Code].IndexOf(AnsiLowerCase(ACode));
  if Result >= 0 then
    Result := Integer(FLists[ltISO_639_2Code].Objects[Result]);
end;

procedure TJvID3TermFinder.ISO_639_2Names(AStrings: TStrings);
begin
  BuildList_ISO_639_2Name;

  AStrings.Assign(FLists[ltISO_639_2Name]);
end;

function TJvID3TermFinder.ISO_639_2NameToIndex(
  const AName: string): Integer;
begin
  BuildList_ISO_639_2Name;

  Result := FLists[ltISO_639_2Name].IndexOf(AName);
  if Result >= 0 then
    Result := Integer(FLists[ltISO_639_2Name].Objects[Result]);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}


finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FinalizeUnit(sUnitName);

end.
