{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvId3v2.PAS, released on 2001-02-28.

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

unit JvId3v2;

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

{
 TJvId3v2
}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  JvId3v2Types, JvComponent;

type
  TJvID3FileType =
    (ftUNKNOWN, ftMPG, ftMPG1, ftMPG2, ftMPG3, ftMPG2_5, ftMPG_AAC, ftVQF, ftPCM);

  TJvID3MediaType =
    (mtUNKNOWN, mtDIG, mtDIG_ANALOG_TRANSFER, mtANA, mtANA_WAX_CYLINDER, mtANA_8_TRACK_TAPE, mtCD,
     mtCD_ANALOG, mtCD_DDD, mtCD_ADD, mtCD_AAD, mtLASERDISC, mtLASERDISC_ANALOG_TRANSFER, mtTURNTABLE,
     mtTURNTABLE_33, mtTURNTABLE_45, mtTURNTABLE_71, mtTURNTABLE_76, mtTURNTABLE_78, mtTURNTABLE_80,
     mtMINIDISC, mtMINIDISC_ANALOG_TRANSFER, mtDAT, mtDAT_ANALOG_TRANSFER, mtDAT_48KHZ_16B, mtDAT_32KHZ_16B,
     mtDAT_32KHZ_12B, mtDAT_44KHZ_16B, mtDAT_44KHZ_16B_WIDE, mtDCC, mtDCC_ANALOG_TRANSFER, mtDVD,
     mtDVD_ANALOG_TRANSFER, mtTV, mtTV_PAL, mtTV_NTSC, mtTV_SECAM,
     mtVID, mtVID_PAL, mtVID_NTSC, mtVID_SECAM, mtVID_VHS, mtVID_SVHS, mtVID_BETA, mtRAD, mtRAD_FM,
     mtRAD_AM, mtRAD_LW, mtRAD_MW, mtTEL, mtTEL_ISDN, mtMC, mtMC_4, mtMC_9, mtMC_I, mtMC_II, mtMC_III,
     mtMC_IV, mtREE, mtREE_9, mtREE_19, mtREE_38, mtREE_76, mtREE_I, mtREE_II, mtREE_III, mtREE_IV,
     mtDAT_32KHZ_12B_4CH);

  TJvId3Text = class(TPersistent)
  private
    FBPM: Integer;
    FAlbum: string;
    FComposer: TStrings;
    FPlaylistDelay: Integer;
    FCopyright: string;
    FEncodedBy: string;
    FDate: string;
    FLyricists: TStrings;
    FFileType: TJvID3FileType;
    FTime: string;
    FInitialKey: string;
    FContentGroup: string;
    FSubTitle: string;
    FContentDescription: string;
    FLanguages: TStrings;
    FLength: string;
    FMediaType: TJvID3MediaType;
    FOriginalTitle: string;
    FOriginalFileName: string;
    FOriginalLyricists: TStrings;
    FOriginalArtists: TStrings;
    FOwner: string;
    FOriginalReleaseYear: string;
    FPerformers: TStrings;
    FConductor: string;
    FModifiedBy: string;
    FBand: string;
    FPartOf: string;
    FPublisher: string;
    FTrackNumber: string;
    FRecordingDate: string;
    FInternetRadioName: string;
    FInternetRadioOwner: string;
    FSize: Integer;
    FISRC: string;
    FEncodingSoftware: string;
    FYear: string;
    FContent: string;
    procedure ResetFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    { Do not store dummies }
    property Album: string read FAlbum write FAlbum stored False;
    property BPM: Integer read FBPM write FBPM stored False;
    property Composer: TStrings read FComposer write FComposer stored False;
    property Copyright: string read FCopyright write FCopyright stored False;
    property Date: string read FDate write FDate stored False;
    property PlaylistDelay: Integer read FPlaylistDelay write FPlaylistDelay stored False;
    property EncodedBy: string read FEncodedBy write FEncodedBy stored False;
    property Lyricists: TStrings read FLyricists write FLyricists stored False;
    property FileType: TJvID3FileType read FFileType write FFileType stored False;
    property Time: string read FTime write FTime stored False;
    property ContentGroup: string read FContentGroup write FContentGroup stored False;
    property ContentDescription: string read FContentDescription write FContentDescription stored False;
    property SubTitle: string read FSubTitle write FSubTitle stored False;
    property InitialKey: string read FInitialKey write FInitialKey stored False;
    property Languages: TStrings read FLanguages write FLanguages stored False;
    property Length: string read FLength write FLength stored False;
    property MediaType: TJvID3MediaType read FMediaType write FMediaType stored False;
    property OriginalTitle: string read FOriginalTitle write FOriginalTitle stored False;
    property OriginalFileName: string read FOriginalFileName write FOriginalFileName stored False;
    property OriginalLyricists: TStrings read FOriginalLyricists write FOriginalLyricists stored False;
    property OriginalArtists: TStrings read FOriginalArtists write FOriginalArtists stored False;
    property OriginalReleaseYear: string read FOriginalReleaseYear write FOriginalReleaseYear stored False;
    property FileOwner: string read FOwner write FOwner stored False;
    property Performers: TStrings read FPerformers write FPerformers stored False;
    property Band: string read FBand write FBand stored False;
    property Conductor: string read FConductor write FConductor stored False;
    property ModifiedBy: string read FModifiedBy write FModifiedBy stored False;
    property PartOf: string read FPartOf write FPartOf stored False;
    property Publisher: string read FPublisher write FPublisher stored False;
    property TrackNumber: string read FTrackNumber write FTrackNumber stored False;
    property RecordingDate: string read FRecordingDate write FRecordingDate stored False;
    property InternetRadioName: string read FInternetRadioName write FInternetRadioName stored False;
    property InternetRadioOwner: string read FInternetRadioOwner write FInternetRadioOwner stored False;
    property Size: Integer read FSize write FSize stored False;
    property ISRC: string read FISRC write FISRC stored False;
    property EncodingSoftware: string read FEncodingSoftware write FEncodingSoftware stored False;
    property Year: string read FYear write FYear stored False;
    property Content: string read FContent write FContent stored False;
  end;

  TJvId3Web = class(TPersistent)
  private
    FOfficialAudio: string;
    FInternetRadioStation: string;
    FPublishers: string;
    FOfficialAudioSource: string;
    FPayment: string;
    FLegalInfo: string;
    FCommercialInfo: string;
    FOfficialArtist: string;
    procedure ResetFields;
  published
    { Do not store dummies }
    property CommercialInfo: string read FCommercialInfo write FCommercialInfo stored False;
    property LegalInfo: string read FLegalInfo write FLegalInfo stored False;
    property OfficialAudio: string read FOfficialAudio write FOfficialAudio stored False;
    property OfficialArtist: string read FOfficialArtist write FOfficialArtist stored False;
    property OfficialAudioSource: string read FOfficialAudioSource write FOfficialAudioSource stored False;
    property InternetRadioStation: string read FInternetRadioStation write FInternetRadioStation stored False;
    property Payment: string read FPayment write FPayment stored False;
    property Publishers: string read FPublishers write FPublishers stored False;
  end;

  TJvID3UDText = class(TPersistent)
  private
    FItemCount: Integer;
    FItemIndex: Integer;
    FDescription: string;
    FValue: string;
    FDummyI: Integer;
    FStrings: TStringList;
    FDescriptions: TStringList;
    procedure ResetFields;
    procedure SetItem(const Value: Integer);
    procedure AddItem(Desc, Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ItemIndex: Integer read FItemIndex write SetItem;
    { Do not store dummies }
    property Description: string read FDescription write FDescription stored False;
    property Value: string read FValue write FValue stored False;
    property ItemCount: Integer read FItemCount write FDummyI stored False;
  end;

  TJvID3UDUrl = class(TPersistent)
  private
    FItemCount: Integer;
    FItemIndex: Integer;
    FDescription: string;
    FURL: string;
    FDummyI: Integer;
    FURLList: TStringList;
    FDescriptions: TStringList;
    procedure ResetFields;
    procedure SetItem(const Value: Integer);
    procedure AddItem(Desc, Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ItemIndex: Integer read FItemIndex write SetItem;
    { Do not store dummies }
    property Description: string read FDescription write FDescription stored False;
    property URL: string read FURL write FURL stored False;
    property ItemCount: Integer read FItemCount write FDummyI stored False;
  end;

  TJvIdPictures = class(TPersistent)
  private
    FMedia: TPicture;
    FLyricist: TPicture;
    FFileIcon: TPicture;
    FComposer: TPicture;
    FRecordingLocation: TPicture;
    FOther: TPicture;
    FColouredFish: TPicture;
    FCoverFront: TPicture;
    FBandLogo: TPicture;
    FConductor: TPicture;
    FPublisherLogo: TPicture;
    FDuringRecording: TPicture;
    FArtist: TPicture;
    FLeaflet: TPicture;
    FDPerformance: TPicture;
    FIllustration: TPicture;
    FBand: TPicture;
    FMovieCapture: TPicture;
    FCoverBack: TPicture;
    FOtherIcon: TPicture;
    FLeadArtist: TPicture;
    procedure SetArtist(const Value: TPicture);
    procedure SetBand(const Value: TPicture);
    procedure SetBandLogo(const Value: TPicture);
    procedure SetComposer(const Value: TPicture);
    procedure SetConductor(const Value: TPicture);
    procedure SetCoverBack(const Value: TPicture);
    procedure SetCoverFront(const Value: TPicture);
    procedure SetDuringRecording(const Value: TPicture);
    procedure SetColouredFish(const Value: TPicture);
    procedure SetFileIcon(const Value: TPicture);
    procedure SetIllustration(const Value: TPicture);
    procedure SetLeaflet(const Value: TPicture);
    procedure SetRecordingLocation(const Value: TPicture);
    procedure SetLyricist(const Value: TPicture);
    procedure SetMedia(const Value: TPicture);
    procedure SetMovieCapture(const Value: TPicture);
    procedure SetOther(const Value: TPicture);
    procedure SetPerformance(const Value: TPicture);
    procedure SetPublisherLogo(const Value: TPicture);
    procedure SetOtherIcon(const Value: TPicture);
    procedure SetLeadArtist(const Value: TPicture);
  public
    constructor Create;
    destructor Destroy; override;
  published
    { Do not store dummies }
    property Other: TPicture read FOther write SetOther stored False;
    property FileIcon: TPicture read FFileIcon write SetFileIcon stored False;
    property OtherIcon: TPicture read FOtherIcon write SetOtherIcon stored False;
    property CoverFront: TPicture read FCoverFront write SetCoverFront stored False;
    property CoverBack: TPicture read FCoverBack write SetCoverBack stored False;
    property Leaflet: TPicture read FLeaflet write SetLeaflet stored False;
    property Media: TPicture read FMedia write SetMedia stored False;
    property Artist: TPicture read FArtist write SetArtist stored False;
    property Conductor: TPicture read FConductor write SetConductor stored False;
    property Band: TPicture read FBand write SetBand stored False;
    property Composer: TPicture read FComposer write SetComposer stored False;
    property Lyricist: TPicture read FLyricist write SetLyricist stored False;
    property RecordingLocation: TPicture read FRecordingLocation write SetRecordingLocation stored False;
    property DuringRecording: TPicture read FDuringRecording write SetDuringRecording stored False;
    property DuringPerformance: TPicture read FDPerformance write SetPerformance stored False;
    property MovieCapture: TPicture read FMovieCapture write SetMovieCapture stored False;
    property ColouredFish: TPicture read FColouredFish write SetColouredFish stored False;
    property Illustration: TPicture read FIllustration write SetIllustration stored False;
    property BandLogo: TPicture read FBandLogo write SetBandLogo stored False;
    property PublisherLogo: TPicture read FPublisherLogo write SetPublisherLogo stored False;
    property LeadArtist: TPicture read FLeadArtist write SetLeadArtist stored False;
  end;

  TJvIdPicturesDesc = class(TPersistent)
  private
    FLeaflet: string;
    FOtherIcon: string;
    FArtist: string;
    FFileIcon: string;
    FCoverFront: string;
    FPublisherLogo: string;
    FLeadArtist: string;
    FConductor: string;
    FDuringRecording: string;
    FBandLogo: string;
    FLyricist: string;
    FCoverBack: string;
    FIllustration: string;
    FBand: string;
    FMovieCapture: string;
    FDuringPerformance: string;
    FComposer: string;
    FMedia: string;
    FColouredFish: string;
    FRecordingLocation: string;
    FOther: string;
  published
    { Do not store dummies }
    property Other: string read FOther write FOther stored False;
    property FileIcon: string read FFileIcon write FFileIcon stored False;
    property OtherIcon: string read FOtherIcon write FOtherIcon stored False;
    property CoverFront: string read FCoverFront write FCoverFront stored False;
    property CoverBack: string read FCoverBack write FCoverBack stored False;
    property Leaflet: string read FLeaflet write FLeaflet stored False;
    property Media: string read FMedia write FMedia stored False;
    property Artist: string read FArtist write FArtist stored False;
    property Conductor: string read FConductor write FConductor stored False;
    property Band: string read FBand write FBand stored False;
    property Composer: string read FComposer write FComposer stored False;
    property Lyricist: string read FLyricist write FLyricist stored False;
    property RecordingLocation: string read FRecordingLocation write FRecordingLocation stored False;
    property DuringRecording: string read FDuringRecording write FDuringRecording stored False;
    property DuringPerformance: string read FDuringPerformance write FDuringPerformance stored False;
    property MovieCapture: string read FMovieCapture write FMovieCapture stored False;
    property ColouredFish: string read FColouredFish write FColouredFish stored False;
    property Illustration: string read FIllustration write FIllustration stored False;
    property BandLogo: string read FBandLogo write FBandLogo stored False;
    property PublisherLogo: string read FPublisherLogo write FPublisherLogo stored False;
    property LeadArtist: string read FLeadArtist write FLeadArtist stored False;
  end;

  TJvIdImages = class(TPersistent)
  private
    FPictures: TJvIdPictures;
    FInfos: TJvIdPicturesDesc;
    procedure ResetFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Pictures: TJvIdPictures read FPictures;
    property Infos: TJvIdPicturesDesc read FInfos;
  end;

  TJvIdIpl = class(TPersistent)
  private
    FDummyI: Integer;
    FItemCount: Integer;
    FItemIndex: Integer;
    FJob: string;
    FPerson: string;
    FJobs: TStringList;
    FPersons: TStringList;
    procedure SetItemIndex(const Value: Integer);
    procedure AddItem(Job, Person: string);
    procedure ResetFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    { Do not store dummies }
    property Job: string read FJob write FJob stored False;
    property Person: string read FPerson write FPerson stored False;
    property ItemCount: Integer read FItemCount write FDummyI stored False;
  end;

  TJvId3Owner = class(TPersistent)
  private
    FPrice: string;
    FDatePurchased: TDate;
    FSeller: string;
    procedure ResetFields;
  published
    { Do not store dummies }
    property Price: string read FPrice write FPrice stored False;
    property DatePurchased: TDate read FDatePurchased write FDatePurchased stored False;
    property Seller: string read FSeller write FSeller stored False;
  end;

  TJvId3Popularimeter = class(TPersistent)
  private
    FCounter: Integer;
    FRating: Byte;
    FUserEmail: string;
    procedure ResetFields;
  published
    { Do not store dummies }
    property UserEmail: string read FUserEmail write FUserEmail stored False;
    property Rating: Byte read FRating write FRating stored False;
    property Counter: Integer read FCounter write FCounter stored False;
  end;

  TJvId3EventType = (etPADDING, etEND_OF_INITIAL_SILENCE, etINTRO_START, etMAINPART_START,
    etOUTRO_START, etOUTRO_END, etVERSE_START, etREFRAIN_START, etINTERLUDE_START,
    etTHEME_START, etVARIATION_START, etKEY_CHANGE, eTTime_CHANGE, etUNWANTED_NOISE,
    etSUSTAINED_NOISE, etSUSTAINED_NOISE_END, etINTRO_END, etMAINPART_END, etVERSE_END,
    etREFRAIN_END, etTHEME_END, etAUDIO_END, etFILE_END);

  TEventTiming = procedure(Sender: TObject; TimeStamp: Integer; EventType: TJvId3EventType) of object;

  TJvId3Events = record
    TimeStamp: Integer;
    EventType: TJvId3EventType;
  end;

  TJvId3v2 = class(TJvComponent)
  private
    FEvents: array [0..1000] of TJvId3Events;
    FEventsCount: Integer;
    FEventsTiming: Integer;
    FCount: Integer;
    FTagPresent: Boolean;
    FVersion: Real;
    FFileName: TFileName;
    FUnsynchronisation: Boolean;
    FExperimental: Boolean;
    FExtendedHeader: Boolean;
    FSize: Integer;
    FTexts: TJvId3Text;
    FWeb: TJvId3Web;
    FUserDefinedText: TJvID3UDText;
    FUserDefinedWeb: TJvID3UDUrl;
    FInvolvedPeople: TJvIdIpl;
    FImages: TJvIdImages;
    FOwner: TJvId3Owner;
    FPlayCounter: Cardinal;
    FPopularimeter: TJvId3Popularimeter;
    FEventTiming: TEventTiming;
    // unuseful variables
    FDummyB: Boolean;
    FDummyR: Real;
    FDummyI: Integer;
    FTagSize: Integer;
    procedure LoadFromFile(FileName: string);
    procedure SetFileName(const Value: TFileName);
    procedure ResetProp;
    procedure ReadAndUnsynchro(var Source, Dest: TStream; taille: Integer);
    procedure ReadText(Frame: TId3v2Frame; Source: TStream);
    procedure ReadWeb(Frame: TId3v2Frame; Source: TStream);
    procedure ReadIpls(Frame: TId3v2Frame; Source: TStream);
    procedure ReadImg(Frame: TId3v2Frame; Source: TStream);
    procedure ReadCounter(Frame: TId3v2Frame; Source: TStream);
    procedure ReadPop(Frame: TId3v2Frame; Source: TStream);
    procedure ReadOwner(Frame: TId3v2Frame; Source: TStream);
    procedure ReadEvent(Frame: TId3v2Frame; Source: TStream);
    procedure SkipFrame(Frame: TId3v2Frame; var Source: TStream);
    function Iso639ToName(code: string): string;
  public
    MusicCDIdentifier: array [0..803] of Byte;
    MusicCDIdentifierLength: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure CheckEvent(CurrentTime: Integer);
  published
    { Do not store dummies }
    property TagPresent: Boolean read FTagPresent write FDummyB stored False;
    property FileName: TFileName read FFileName write SetFileName;
    property Version: Real read FVersion write FDummyR stored False;
    property Unsynchronisation: Boolean read FUnsynchronisation write FDummyB stored False;
    property ExtendedHeader: Boolean read FExtendedHeader write FDummyB stored False;
    property Experimental: Boolean read FExperimental write FDummyB stored False;
    property Texts: TJvId3Text read FTexts;
    property UserDefinedText: TJvID3UDText read FUserDefinedText;
    property Web: TJvId3Web read FWeb;
    property UserDefinedWeb: TJvID3UDUrl read FUserDefinedWeb;
    property InvolvedPeople: TJvIdIpl read FInvolvedPeople;
    property Images: TJvIdImages read FImages;
    property PlayCounter: Cardinal read FPlayCounter write FPlayCounter stored False;
    property Owner: TJvId3Owner read FOwner;
    property Popularimeter: TJvId3Popularimeter read FPopularimeter;
    property TagSize: Integer read FTagSize write FDummyI stored False;
    property OnEventTiming: TEventTiming read FEventTiming write FEventTiming;
  end;

implementation

//=== TJvId3v2 ===============================================================

constructor TJvId3v2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagSize := 0;
  FCount := 0;
  FTagPresent := False;
  FTexts := TJvId3Text.Create;
  FWeb := TJvId3Web.Create;
  FUserDefinedText := TJvID3UDText.Create;
  FUserDefinedWeb := TJvID3UDUrl.Create;
  FInvolvedPeople := TJvIdIpl.Create;
  FImages := TJvIdImages.Create;
  FOwner := TJvId3Owner.Create;
  FPopularimeter := TJvId3Popularimeter.Create;
  ResetProp;
end;

destructor TJvId3v2.Destroy;
begin
  FTexts.Free;
  FWeb.Free;
  FUserDefinedText.Free;
  FUserDefinedWeb.Free;
  FInvolvedPeople.Free;
  FImages.Free;
  FOwner.Free;
  FPopularimeter.Free;
  inherited Destroy;
end;

procedure TJvId3v2.CheckEvent(CurrentTime: Integer);
var
  I: Integer;
begin
  if (FEventsCount > 0) and Assigned(FEventTiming) then
  begin
    if CurrentTime < FCount then
      FCount := 0;
    for I := 0 to FEventsCount - 1 do
      if FEvents[I].TimeStamp in [FCount..CurrentTime] then
        FEventTiming(Self, FEvents[I].TimeStamp, FEvents[I].EventType);
  end;
  FCount := CurrentTime;
end;

type
  TShortToLongName = record
    S: PChar;
    L: PChar;
  end;

const
  cShortToLongNameTable: array [1..427] of TShortToLongName =
    (
     (S: 'aar'; L: 'Afar'),
     (S: 'abk'; L: 'Abkhazian'),
     (S: 'ace'; L: 'Achinese'),
     (S: 'ach'; L: 'Acoli'),
     (S: 'ada'; L: 'Adangme'),
     (S: 'afa'; L: 'Afro-Asiatic (Other)'),
     (S: 'afh'; L: 'Afrihili'),
     (S: 'afr'; L: 'Afrikaans'),
     (S: 'aka'; L: 'Akan'),
     (S: 'akk'; L: 'Akkadian'),
     (S: 'alb'; L: 'Albanian'),
     (S: 'ale'; L: 'Aleut'),
     (S: 'alg'; L: 'Algonquian Languages'),
     (S: 'amh'; L: 'Amharic'),
     (S: 'ang'; L: 'English, Old (ca. 450-1100)'),
     (S: 'apa'; L: 'Apache Languages'),
     (S: 'ara'; L: 'Arabic'),
     (S: 'arc'; L: 'Aramaic'),
     (S: 'arm'; L: 'Armenian'),
     (S: 'arn'; L: 'Araucanian'),
     (S: 'arp'; L: 'Arapaho'),
     (S: 'art'; L: 'Artificial (Other)'),
     (S: 'arw'; L: 'Arawak'),
     (S: 'asm'; L: 'Assamese'),
     (S: 'ath'; L: 'Athapascan Languages'),
     (S: 'ava'; L: 'Avaric'),
     (S: 'ave'; L: 'Avestan'),
     (S: 'awa'; L: 'Awadhi'),
     (S: 'aym'; L: 'Aymara'),
     (S: 'aze'; L: 'Azerbaijani'),
     (S: 'bad'; L: 'Banda'),
     (S: 'bai'; L: 'Bamileke Languages'),
     (S: 'bak'; L: 'Bashkir'),
     (S: 'bal'; L: 'Baluchi'),
     (S: 'bam'; L: 'Bambara'),
     (S: 'ban'; L: 'Balinese'),
     (S: 'baq'; L: 'Basque'),
     (S: 'bas'; L: 'Basa'),
     (S: 'bat'; L: 'Baltic (Other)'),
     (S: 'bej'; L: 'Beja'),
     (S: 'bel'; L: 'Byelorussian'),
     (S: 'bem'; L: 'Bemba'),
     (S: 'ben'; L: 'Bengali'),
     (S: 'ber'; L: 'Berber (Other)'),
     (S: 'bho'; L: 'Bhojpuri'),
     (S: 'bih'; L: 'Bihari'),
     (S: 'bik'; L: 'Bikol'),
     (S: 'bin'; L: 'Bini'),
     (S: 'bis'; L: 'Bislama'),
     (S: 'bla'; L: 'Siksika'),
     (S: 'bnt'; L: 'Bantu (Other)'),
     (S: 'bod'; L: 'Tibetan'),
     (S: 'bra'; L: 'Braj'),
     (S: 'bre'; L: 'Breton'),
     (S: 'bua'; L: 'Buriat'),
     (S: 'bug'; L: 'Buginese'),
     (S: 'bul'; L: 'Bulgarian'),
     (S: 'bur'; L: 'Burmese'),
     (S: 'cad'; L: 'Caddo'),
     (S: 'cai'; L: 'Central American Indian (Other)'),
     (S: 'car'; L: 'Carib'),
     (S: 'cat'; L: 'Catalan'),
     (S: 'cau'; L: 'Caucasian (Other)'),
     (S: 'ceb'; L: 'Cebuano'),
     (S: 'cel'; L: 'Celtic (Other)'),
     (S: 'ces'; L: 'Czech'),
     (S: 'cha'; L: 'Chamorro'),
     (S: 'chb'; L: 'Chibcha'),
     (S: 'che'; L: 'Chechen'),
     (S: 'chg'; L: 'Chagatai'),
     (S: 'chi'; L: 'Chinese'),
     (S: 'chm'; L: 'Mari'),
     (S: 'chn'; L: 'Chinook jargon'),
     (S: 'cho'; L: 'Choctaw'),
     (S: 'chr'; L: 'Cherokee'),
     (S: 'chu'; L: 'Church Slavic'),
     (S: 'chv'; L: 'Chuvash'),
     (S: 'chy'; L: 'Cheyenne'),
     (S: 'cop'; L: 'Coptic'),
     (S: 'cor'; L: 'Cornish'),
     (S: 'cos'; L: 'Corsican'),
     (S: 'cpe'; L: 'Creoles and Pidgins, English-based (Other)'),
     (S: 'cpf'; L: 'Creoles and Pidgins, French-based (Other)'),
     (S: 'cpp'; L: 'Creoles and Pidgins, Portuguese-based (Other)'),
     (S: 'cre'; L: 'Cree'),
     (S: 'crp'; L: 'Creoles and Pidgins (Other)'),
     (S: 'cus'; L: 'Cushitic (Other)'),
     (S: 'cym'; L: 'Welsh'),
     (S: 'cze'; L: 'Czech'),
     (S: 'dak'; L: 'Dakota'),
     (S: 'dan'; L: 'Danish'),
     (S: 'del'; L: 'Delaware'),
     (S: 'deu'; L: 'German'),
     (S: 'din'; L: 'Dinka'),
     (S: 'div'; L: 'Divehi'),
     (S: 'doi'; L: 'Dogri'),
     (S: 'dra'; L: 'Dravidian (Other)'),
     (S: 'dua'; L: 'Duala'),
     (S: 'dum'; L: 'Dutch, Middle (ca. 1050-1350)'),
     (S: 'dut'; L: 'Dutch'),
     (S: 'dyu'; L: 'Dyula'),
     (S: 'dzo'; L: 'Dzongkha'),
     (S: 'efi'; L: 'Efik'),
     (S: 'egy'; L: 'Egyptian (Ancient)'),
     (S: 'eka'; L: 'Ekajuk'),
     (S: 'ell'; L: 'Greek, Modern (1453-)'),
     (S: 'elx'; L: 'Elamite'),
     (S: 'eng'; L: 'English'),
     (S: 'enm'; L: 'English, Middle (ca. 1100-1500)'),
     (S: 'epo'; L: 'Esperanto'),
     (S: 'esk'; L: 'Eskimo (Other)'),
     (S: 'esl'; L: 'Spanish'),
     (S: 'est'; L: 'Estonian'),
     (S: 'eus'; L: 'Basque'),
     (S: 'ewe'; L: 'Ewe'),
     (S: 'ewo'; L: 'Ewondo'),
     (S: 'fan'; L: 'Fang'),
     (S: 'fao'; L: 'Faroese'),
     (S: 'fas'; L: 'Persian'),
     (S: 'fat'; L: 'Fanti'),
     (S: 'fij'; L: 'Fijian'),
     (S: 'fin'; L: 'Finnish'),
     (S: 'fiu'; L: 'Finno-Ugrian (Other)'),
     (S: 'fon'; L: 'Fon'),
     (S: 'fra'; L: 'French'),
     (S: 'fre'; L: 'French'),
     (S: 'frm'; L: 'French, Middle (ca. 1400-1600)'),
     (S: 'fro'; L: 'French, Old (842- ca. 1400)'),
     (S: 'fry'; L: 'Frisian'),
     (S: 'ful'; L: 'Fulah'),
     (S: 'gaa'; L: 'Ga'),
     (S: 'gae'; L: 'Gaelic (Scots)'),
     (S: 'gai'; L: 'Irish'),
     (S: 'gay'; L: 'Gayo'),
     (S: 'gdh'; L: 'Gaelic (Scots)'),
     (S: 'gem'; L: 'Germanic (Other)'),
     (S: 'geo'; L: 'Georgian'),
     (S: 'ger'; L: 'German'),
     (S: 'gez'; L: 'Geez'),
     (S: 'gil'; L: 'Gilbertese'),
     (S: 'glg'; L: 'Gallegan'),
     (S: 'gmh'; L: 'German, Middle High (ca. 1050-1500)'),
     (S: 'goh'; L: 'German, Old High (ca. 750-1050)'),
     (S: 'gon'; L: 'Gondi'),
     (S: 'got'; L: 'Gothic'),
     (S: 'grb'; L: 'Grebo'),
     (S: 'grc'; L: 'Greek, Ancient (to 1453)'),
     (S: 'gre'; L: 'Greek, Modern (1453-)'),
     (S: 'grn'; L: 'Guarani'),
     (S: 'guj'; L: 'Gujarati'),
     (S: 'hai'; L: 'Haida'),
     (S: 'hau'; L: 'Hausa'),
     (S: 'haw'; L: 'Hawaiian'),
     (S: 'heb'; L: 'Hebrew'),
     (S: 'her'; L: 'Herero'),
     (S: 'hil'; L: 'Hiligaynon'),
     (S: 'him'; L: 'Himachali'),
     (S: 'hin'; L: 'Hindi'),
     (S: 'hmo'; L: 'Hiri Motu'),
     (S: 'hun'; L: 'Hungarian'),
     (S: 'hup'; L: 'Hupa'),
     (S: 'hye'; L: 'Armenian'),
     (S: 'iba'; L: 'Iban'),
     (S: 'ibo'; L: 'Igbo'),
     (S: 'ice'; L: 'Icelandic'),
     (S: 'ijo'; L: 'Ijo'),
     (S: 'iku'; L: 'Inuktitut'),
     (S: 'ilo'; L: 'Iloko'),
     (S: 'ina'; L: 'Interlingua (International Auxiliary language Association)'),
     (S: 'Inc'; L: 'Indic (Other)'),
     (S: 'ind'; L: 'Indonesian'),
     (S: 'ine'; L: 'Indo-European (Other)'),
     (S: 'ine'; L: 'Interlingue'),
     (S: 'ipk'; L: 'Inupiak'),
     (S: 'ira'; L: 'Iranian (Other)'),
     (S: 'iri'; L: 'Irish'),
     (S: 'iro'; L: 'Iroquoian uages'),
     (S: 'isl'; L: 'Icelandic'),
     (S: 'ita'; L: 'Italian'),
     (S: 'jav'; L: 'Javanese'),
     (S: 'jaw'; L: 'Javanese'),
     (S: 'jpn'; L: 'Japanese'),
     (S: 'jpr'; L: 'Judeo-Persian'),
     (S: 'jrb'; L: 'Judeo-Arabic'),
     (S: 'kaa'; L: 'Kara-Kalpak'),
     (S: 'kab'; L: 'Kabyle'),
     (S: 'kac'; L: 'Kachin'),
     (S: 'kal'; L: 'Greenlandic'),
     (S: 'kam'; L: 'Kamba'),
     (S: 'kan'; L: 'Kannada'),
     (S: 'kar'; L: 'Karen'),
     (S: 'kas'; L: 'Kashmiri'),
     (S: 'kat'; L: 'Georgian'),
     (S: 'kau'; L: 'Kanuri'),
     (S: 'kaw'; L: 'Kawi'),
     (S: 'kaz'; L: 'Kazakh'),
     (S: 'kha'; L: 'Khasi'),
     (S: 'khi'; L: 'Khoisan (Other)'),
     (S: 'khm'; L: 'Khmer'),
     (S: 'kho'; L: 'Khotanese'),
     (S: 'kik'; L: 'Kikuyu'),
     (S: 'kin'; L: 'Kinyarwanda'),
     (S: 'kir'; L: 'Kirghiz'),
     (S: 'kok'; L: 'Konkani'),
     (S: 'kom'; L: 'Komi'),
     (S: 'kon'; L: 'Kongo'),
     (S: 'kor'; L: 'Korean'),
     (S: 'kpe'; L: 'Kpelle'),
     (S: 'kro'; L: 'Kru'),
     (S: 'kru'; L: 'Kurukh'),
     (S: 'kua'; L: 'Kuanyama'),
     (S: 'kum'; L: 'Kumyk'),
     (S: 'kur'; L: 'Kurdish'),
     (S: 'kus'; L: 'Kusaie'),
     (S: 'kut'; L: 'Kutenai'),
     (S: 'lad'; L: 'Ladino'),
     (S: 'lah'; L: 'Lahnda'),
     (S: 'lam'; L: 'Lamba'),
     (S: 'lao'; L: 'Lao'),
     (S: 'lat'; L: 'Latin'),
     (S: 'lav'; L: 'Latvian'),
     (S: 'lez'; L: 'Lezghian'),
     (S: 'lin'; L: 'Lingala'),
     (S: 'lit'; L: 'Lithuanian'),
     (S: 'lol'; L: 'Mongo'),
     (S: 'loz'; L: 'Lozi'),
     (S: 'ltz'; L: 'Letzeburgesch'),
     (S: 'lub'; L: 'Luba-Katanga'),
     (S: 'lug'; L: 'Ganda'),
     (S: 'lui'; L: 'Luiseno'),
     (S: 'lun'; L: 'Lunda'),
     (S: 'luo'; L: 'Luo (Kenya and Tanzania)'),
     (S: 'mac'; L: 'Macedonian'),
     (S: 'mad'; L: 'Madurese'),
     (S: 'mag'; L: 'Magahi'),
     (S: 'mah'; L: 'Marshall'),
     (S: 'mai'; L: 'Maithili'),
     (S: 'mak'; L: 'Macedonian'),
     (S: 'mak'; L: 'Makasar'),
     (S: 'mal'; L: 'Malayalam'),
     (S: 'man'; L: 'Mandingo'),
     (S: 'mao'; L: 'Maori'),
     (S: 'map'; L: 'Austronesian (Other)'),
     (S: 'mar'; L: 'Marathi'),
     (S: 'mas'; L: 'Masai'),
     (S: 'max'; L: 'Manx'),
     (S: 'may'; L: 'Malay'),
     (S: 'men'; L: 'Mende'),
     (S: 'mga'; L: 'Irish, Middle (900 - 1200)'),
     (S: 'mic'; L: 'Micmac'),
     (S: 'min'; L: 'Minangkabau'),
     (S: 'mis'; L: 'Miscellaneous (Other)'),
     (S: 'mkh'; L: 'Mon-Kmer (Other)'),
     (S: 'mlg'; L: 'Malagasy'),
     (S: 'mlt'; L: 'Maltese'),
     (S: 'mni'; L: 'Manipuri'),
     (S: 'mno'; L: 'Manobo Languages'),
     (S: 'moh'; L: 'Mohawk'),
     (S: 'mol'; L: 'Moldavian'),
     (S: 'mon'; L: 'Mongolian'),
     (S: 'mos'; L: 'Mossi'),
     (S: 'mri'; L: 'Maori'),
     (S: 'msa'; L: 'Malay'),
     (S: 'mul'; L: 'Multiple Languages'),
     (S: 'mun'; L: 'Munda Languages'),
     (S: 'mus'; L: 'Creek'),
     (S: 'mwr'; L: 'Marwari'),
     (S: 'mya'; L: 'Burmese'),
     (S: 'myn'; L: 'Mayan Languages'),
     (S: 'nah'; L: 'Aztec'),
     (S: 'nai'; L: 'North American Indian (Other)'),
     (S: 'nau'; L: 'Nauru'),
     (S: 'nav'; L: 'Navajo'),
     (S: 'nbl'; L: 'Ndebele, South'),
     (S: 'nde'; L: 'Ndebele, North'),
     (S: 'ndo'; L: 'Ndongo'),
     (S: 'nep'; L: 'Nepali'),
     (S: 'new'; L: 'Newari'),
     (S: 'nic'; L: 'Niger-Kordofanian (Other)'),
     (S: 'niu'; L: 'Niuean'),
     (S: 'nla'; L: 'Dutch'),
     (S: 'nno'; L: 'Norwegian (Nynorsk)'),
     (S: 'non'; L: 'Norse, Old'),
     (S: 'nor'; L: 'Norwegian'),
     (S: 'nso'; L: 'Sotho, Northern'),
     (S: 'nub'; L: 'Nubian Languages'),
     (S: 'nya'; L: 'Nyanja'),
     (S: 'nym'; L: 'Nyamwezi'),
     (S: 'nyn'; L: 'Nyankole'),
     (S: 'nyo'; L: 'Nyoro'),
     (S: 'nzi'; L: 'Nzima'),
     (S: 'oci'; L: 'Langue d''Oc (post 1500)'),
     (S: 'oji'; L: 'Ojibwa'),
     (S: 'ori'; L: 'Oriya'),
     (S: 'orm'; L: 'Oromo'),
     (S: 'osa'; L: 'Osage'),
     (S: 'oss'; L: 'Ossetic'),
     (S: 'ota'; L: 'Turkish, Ottoman (1500 - 1928)'),
     (S: 'oto'; L: 'Otomian Languages'),
     (S: 'paa'; L: 'Papuan-Australian (Other)'),
     (S: 'pag'; L: 'Pangasinan'),
     (S: 'pal'; L: 'Pahlavi'),
     (S: 'pam'; L: 'Pampanga'),
     (S: 'pan'; L: 'Panjabi'),
     (S: 'pap'; L: 'Papiamento'),
     (S: 'pau'; L: 'Palauan'),
     (S: 'peo'; L: 'Persian, Old (ca 600 - 400 B.C.)'),
     (S: 'per'; L: 'Persian'),
     (S: 'phn'; L: 'Phoenician'),
     (S: 'pli'; L: 'Pali'),
     (S: 'pol'; L: 'Polish'),
     (S: 'pon'; L: 'Ponape'),
     (S: 'por'; L: 'Portuguese'),
     (S: 'pra'; L: 'Prakrit uages'),
     (S: 'pro'; L: 'Provencal, Old (to 1500)'),
     (S: 'pus'; L: 'Pushto'),
     (S: 'que'; L: 'Quechua'),
     (S: 'raj'; L: 'Rajasthani'),
     (S: 'rar'; L: 'Rarotongan'),
     (S: 'roa'; L: 'Romance (Other)'),
     (S: 'roh'; L: 'Rhaeto-Romance'),
     (S: 'rom'; L: 'Romany'),
     (S: 'ron'; L: 'Romanian'),
     (S: 'rum'; L: 'Romanian'),
     (S: 'run'; L: 'Rundi'),
     (S: 'rus'; L: 'Russian'),
     (S: 'sad'; L: 'Sandawe'),
     (S: 'sag'; L: 'Sango'),
     (S: 'sah'; L: 'Yakut'),
     (S: 'sai'; L: 'South American Indian (Other)'),
     (S: 'sal'; L: 'Salishan Languages'),
     (S: 'sam'; L: 'Samaritan Aramaic'),
     (S: 'san'; L: 'Sanskrit'),
     (S: 'sco'; L: 'Scots'),
     (S: 'scr'; L: 'Serbo-Croatian'),
     (S: 'sel'; L: 'Selkup'),
     (S: 'sem'; L: 'Semitic (Other)'),
     (S: 'sga'; L: 'Irish, Old (to 900)'),
     (S: 'shn'; L: 'Shan'),
     (S: 'sid'; L: 'Sidamo'),
     (S: 'sin'; L: 'Singhalese'),
     (S: 'sio'; L: 'Siouan Languages'),
     (S: 'sit'; L: 'Sino-Tibetan (Other)'),
     (S: 'sla'; L: 'Slavic (Other)'),
     (S: 'slk'; L: 'Slovak'),
     (S: 'slo'; L: 'Slovak'),
     (S: 'slv'; L: 'Slovenian'),
     (S: 'smi'; L: 'Sami Languages'),
     (S: 'smo'; L: 'Samoan'),
     (S: 'sna'; L: 'Shona'),
     (S: 'snd'; L: 'Sindhi'),
     (S: 'sog'; L: 'Sogdian'),
     (S: 'som'; L: 'Somali'),
     (S: 'son'; L: 'Songhai'),
     (S: 'sot'; L: 'Sotho, Southern'),
     (S: 'spa'; L: 'Spanish'),
     (S: 'sqi'; L: 'Albanian'),
     (S: 'srd'; L: 'Sardinian'),
     (S: 'srr'; L: 'Serer'),
     (S: 'ssa'; L: 'Nilo-Saharan (Other)'),
     (S: 'ssw'; L: 'Siswant'),
     (S: 'ssw'; L: 'Swazi'),
     (S: 'suk'; L: 'Sukuma'),
     (S: 'sun'; L: 'Sudanese'),
     (S: 'sus'; L: 'Susu'),
     (S: 'sux'; L: 'Sumerian'),
     (S: 'sve'; L: 'Swedish'),
     (S: 'swa'; L: 'Swahili'),
     (S: 'swe'; L: 'Swedish'),
     (S: 'syr'; L: 'Syriac'),
     (S: 'tah'; L: 'Tahitian'),
     (S: 'tam'; L: 'Tamil'),
     (S: 'tat'; L: 'Tatar'),
     (S: 'tel'; L: 'Telugu'),
     (S: 'tem'; L: 'Timne'),
     (S: 'ter'; L: 'Tereno'),
     (S: 'tgk'; L: 'Tajik'),
     (S: 'tgl'; L: 'Tagalog'),
     (S: 'tha'; L: 'Thai'),
     (S: 'tib'; L: 'Tibetan'),
     (S: 'tig'; L: 'Tigre'),
     (S: 'tir'; L: 'Tigrinya'),
     (S: 'tiv'; L: 'Tivi'),
     (S: 'tli'; L: 'Tlingit'),
     (S: 'tmh'; L: 'Tamashek'),
     (S: 'tog'; L: 'Tonga (Nyasa)'),
     (S: 'ton'; L: 'Tonga (Tonga Islands)'),
     (S: 'tru'; L: 'Truk'),
     (S: 'tsi'; L: 'Tsimshian'),
     (S: 'tsn'; L: 'Tswana'),
     (S: 'tso'; L: 'Tsonga'),
     (S: 'tuk'; L: 'Turkmen'),
     (S: 'tum'; L: 'Tumbuka'),
     (S: 'tur'; L: 'Turkish'),
     (S: 'tut'; L: 'Altaic (Other)'),
     (S: 'twi'; L: 'Twi'),
     (S: 'tyv'; L: 'Tuvinian'),
     (S: 'uga'; L: 'Ugaritic'),
     (S: 'uig'; L: 'Uighur'),
     (S: 'ukr'; L: 'Ukrainian'),
     (S: 'umb'; L: 'Umbundu'),
     (S: 'und'; L: 'Undetermined'),
     (S: 'urd'; L: 'Urdu'),
     (S: 'uzb'; L: 'Uzbek'),
     (S: 'vai'; L: 'Vai'),
     (S: 'ven'; L: 'Venda'),
     (S: 'vie'; L: 'Vietnamese'),
     (S: 'vol'; L: 'Volapük'),
     (S: 'vot'; L: 'Votic'),
     (S: 'wak'; L: 'Wakashan Languages'),
     (S: 'wal'; L: 'Walamo'),
     (S: 'war'; L: 'Waray'),
     (S: 'was'; L: 'Washo'),
     (S: 'wel'; L: 'Welsh'),
     (S: 'wen'; L: 'Sorbian Languages'),
     (S: 'wol'; L: 'Wolof'),
     (S: 'xho'; L: 'Xhosa'),
     (S: 'yao'; L: 'Yao'),
     (S: 'yap'; L: 'Yap'),
     (S: 'yid'; L: 'Yiddish'),
     (S: 'yor'; L: 'Yoruba'),
     (S: 'zap'; L: 'Zapotec'),
     (S: 'zen'; L: 'Zenaga'),
     (S: 'zha'; L: 'Zhuang'),
     (S: 'zho'; L: 'Chinese'),
     (S: 'zul'; L: 'Zulu'),
     (S: 'zun'; L: 'Zuni')
    );

function TJvId3v2.Iso639ToName(code: string): string;
var
  I: Integer;
begin
  Result := '';
  if Length(code) <> 3 then
    Exit;
  code := LowerCase(code);
  for I := Low(cShortToLongNameTable) to High(cShortToLongNameTable) do
    if code = cShortToLongNameTable[I].S then
    begin
      Result := cShortToLongNameTable[I].L;
      Break;
    end;
end;

procedure TJvId3v2.LoadFromFile(FileName: string);
var
  fich: TFileStream;
begin
  ResetProp;
  if FileExists(FileName) then
  begin
    fich := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(fich);
    fich.Free;
  end;
end;

procedure TJvId3v2.LoadFromStream(Stream: TStream);
var
  Header: TId3v2Header;
  Frame: TId3v2Frame;
  TagesNode: TStream;
begin
  FEventsCount := 0;
  ResetProp;

  Stream.ReadBuffer(Header, 10);
  FTagPresent := Header.Identifier = 'ID3';
  if FTagPresent then
  begin
    //Version
    FVersion := (Header.Version and $00FF);
    FVersion := FVersion + ((Header.Version and $FF00) / 256 / 100);

    //Flags
    FUnsynchronisation := (Header.Flags and $80) <> 0;
    FExtendedHeader := (Header.Flags and $40) <> 0;
    FExperimental := (Header.Flags and $20) <> 0;
    // (rom) undocumented assembler. EEK!
    asm
         mov eax,Header.Size

         xor ecx,ecx
         mov cl,al
         shl ecx,7
         or cl,ah
         shl ecx,7
         shr eax,16
         or cl,al
         shl ecx,7
         or cl,ah

         mov Header.Size,ecx
    end;
    FSize := Header.Size;
    FTagSize := FSize;

    TagesNode := TMemoryStream.Create;
    ReadAndUnsynchro(Stream, TagesNode, Header.Size);

    //Read extended TagesNode
    if FExtendedHeader then
    begin

    end;

    while TagesNode.Size - TagesNode.Position > 10 do
    begin
      TagesNode.Read(Frame, 10);
      asm
            mov ax,Frame.Flags
            mov dh,al
            mov al,ah
            mov ah,dh
            mov Frame.Flags,ax

            mov eax,Frame.Size
            mov cl,al
            shl ecx,8
            mov cl,ah
            shl ecx,8
            shr eax,16
            mov cl,al
            shl ecx,8
            mov cl,ah
            mov Frame.Size,ecx
      end;

      if Frame.Id[0] = 'T' then
        ReadText(Frame, TagesNode)
      else
      if Frame.Id[0] = 'W' then
        ReadWeb(Frame, TagesNode)
      else
      if Frame.Id = 'IPLS' then
        ReadIpls(Frame, TagesNode)
      else
      if Frame.Id = 'MCDI' then
      begin
        TagesNode.Read(MusicCDIdentifier, Frame.Size);
        MusicCDIdentifierLength := Frame.Size;
      end
      else
      if Frame.Id = 'APIC' then
        ReadImg(Frame, TagesNode)
      else
      if Frame.Id = 'PCNT' then
        ReadCounter(Frame, TagesNode)
      else
      if Frame.Id = 'POPM' then
        ReadPop(Frame, TagesNode)
      else
      if Frame.Id = 'OWNE' then
        ReadOwner(Frame, TagesNode)
      else
      if Frame.Id = 'ETCO' then
        ReadEvent(Frame, TagesNode)
      else
        SkipFrame(Frame, TagesNode);
      //XXX
    end;

    TagesNode.Free;
  end;
end;

procedure TJvId3v2.ReadAndUnsynchro(var Source, Dest: TStream; taille: Integer);
var
  I, J, Cnt: Integer;
  Buf: array [0..499] of Byte;
  buf2: array [0..999] of Byte;
begin
  while taille > 0 do
  begin
    if taille > 500 then
    begin
      Cnt := Source.Read(Buf, 500);
      if Cnt < 500 then
        taille := 0
      else
        taille := taille - Cnt;
    end
    else
    begin
      Cnt := Source.Read(Buf, taille);
      taille := 0;
    end;
    J := 0;
    I := 0;
    while I < Cnt do
    begin
      if Buf[I] = $FF then
      begin
        if I + 1 < Cnt then
        begin
          if Buf[I + 1] = $00 then
          begin
            buf2[J] := $FF;
            Inc(I);
          end
          else
            buf2[J] := $FF;
        end
        else
        begin
          Inc(taille);
          Source.Position := Source.Position - 1;
        end;
      end
      else
        buf2[J] := Buf[I];
      Inc(J);
      Inc(I);
    end;
    Dest.Write(buf2, J);
  end;
  Dest.Position := 0;
end;

procedure TJvId3v2.ReadCounter(Frame: TId3v2Frame; Source: TStream);
begin
  if Frame.Size > 4 then
    FPlayCounter := 0
  else
  begin
    Source.Read(FPlayCounter, 4);
    asm
         mov eax,FPlayCounter
         mov cl,al
         shl ecx,8
         mov cl,ah
         shl ecx,8
         shr eax,16
         mov cl,al
         shl ecx,8
         mov cl,ah
         mov ebx,offset FPlayCounter
         mov [ebx],ecx
    end;
  end;
end;

procedure TJvId3v2.ReadEvent(Frame: TId3v2Frame; Source: TStream);
var
  B: Byte;
  I: Integer;
begin
  Source.Read(B, 1);
  Dec(Frame.Size);

  FEventsCount := 0;
  FEventsTiming := B;

  while Frame.Size > 0 do
  begin
    Source.Read(B, 1);
    Dec(Frame.Size, 5);
    case B of
      $00:
        FEvents[FEventsCount].EventType := etPADDING;
      $01:
       FEvents[FEventsCount].EventType := etEND_OF_INITIAL_SILENCE;
      $02:
        FEvents[FEventsCount].EventType := etINTRO_START;
      $03:
        FEvents[FEventsCount].EventType := etMAINPART_START;
      $04:
        FEvents[FEventsCount].EventType := etOUTRO_START;
      $05:
        FEvents[FEventsCount].EventType := etOUTRO_END;
      $06:
        FEvents[FEventsCount].EventType := etVERSE_START;
      $07:
        FEvents[FEventsCount].EventType := etREFRAIN_START;
      $08:
        FEvents[FEventsCount].EventType := etINTERLUDE_START;
      $09:
        FEvents[FEventsCount].EventType := etTHEME_START;
      $0A:
        FEvents[FEventsCount].EventType := etVARIATION_START;
      $0B:
        FEvents[FEventsCount].EventType := etKEY_CHANGE;
      $0C:
        FEvents[FEventsCount].EventType := eTTime_CHANGE;
      $0D:
        FEvents[FEventsCount].EventType := etUNWANTED_NOISE;
      $0E:
        FEvents[FEventsCount].EventType := etSUSTAINED_NOISE;
      $0F:
        FEvents[FEventsCount].EventType := etSUSTAINED_NOISE_END;
      $10:
        FEvents[FEventsCount].EventType := etINTRO_END;
      $11:
        FEvents[FEventsCount].EventType := etMAINPART_END;
      $12:
        FEvents[FEventsCount].EventType := etVERSE_END;
      $13:
        FEvents[FEventsCount].EventType := etREFRAIN_END;
      $14:
        FEvents[FEventsCount].EventType := etTHEME_END;
      $FD:
        FEvents[FEventsCount].EventType := etAUDIO_END;
      $FE:
        FEvents[FEventsCount].EventType := etFILE_END;
    end;
    Source.Read(I, 4);
    asm
          mov eax,I
          mov cl,al
          shl ecx,8
          mov cl,ah
          shl ecx,8
          shr eax,16
          mov cl,al
          shl ecx,8
          mov cl,ah
          mov ebx,offset I
          mov [ebx],ecx
    end;
    FEvents[FEventsCount].TimeStamp := I;
    Inc(FEventsCount);
  end;
end;

procedure TJvId3v2.ReadImg(Frame: TId3v2Frame; Source: TStream);
var
  B, C, Encoding: Byte;
  Buf: array [0..64000] of Char;
  Mime, Description: string;
  TypeByte: Byte;
  Picture: TMemoryStream;
  SizeLeft: Integer;
  TempPath: array [0..250] of Char;
  TempPat: string;
  FilePath: string;
begin
  Source.Read(Encoding, 1);
  SizeLeft := Frame.Size - 1;

  //Read Mime type
  Mime := '';
  Source.Read(B, 1);
  Dec(SizeLeft);
  while B <> $00 do
  begin
    Mime := Mime + Char(B);
    Source.Read(B, 1);
    Dec(SizeLeft);
  end;

  //Read Picture type
  Source.Read(TypeByte, 1);
  Dec(SizeLeft);

  //Read Description
  if Encoding = $00 then
  begin
    //ISO
    Description := '';
    Source.Read(B, 1);
    Dec(SizeLeft);
    while B <> $00 do
    begin
      Description := Description + Char(B);
      Source.Read(B, 1);
      Dec(SizeLeft);
    end;
  end
  else
  begin
    //Unicode
    Description := '';
    Source.Read(B, 1);
    Source.Read(C, 1);
    if C <> $00 then
    begin
      Source.Read(C, 1);
      Dec(SizeLeft, 3);
      while B <> $00 do
      begin
        Description := Description + Char(B);
        Source.Read(B, 1);
        Source.Read(C, 1);
        Dec(SizeLeft, 2);
      end;
    end
    else
      Dec(SizeLeft, 2);
  end;

  //Picture data
  Picture := TMemoryStream.Create;

  while SizeLeft > 64000 do
  begin
    Source.Read(Buf, 64000);
    Dec(SizeLeft, 64000);
    Picture.Write(Buf, 64000);
  end;
  Source.Read(Buf, SizeLeft);
  Picture.Write(Buf, SizeLeft);

  GetTempPath(250, TempPath);
  TempPat := TempPath;
  if TempPat[Length(TempPat)] <> '\' then
    TempPat := TempPat + '\';

  FilePath := temppat + 'tmp.';
  Mime := Copy(Mime, Pos('/', Mime) + 1, Length(Mime));
  if UpperCase(Mime) = 'JPEG' then
    filePath := FilePath + 'jpg'
  else
    FilePath := FilePath + Mime;
  Picture.SaveToFile(FilePath);

  case TypeByte of
    $00:
      begin
        FImages.Pictures.Other.LoadFromFile(FilePath);
        FImages.Infos.Other := Description;
      end;
    $01:
      begin
        FImages.Pictures.FileIcon.LoadFromFile(FilePath);
        FImages.Infos.FileIcon := Description;
      end;
    $02:
      begin
        FImages.Pictures.OtherIcon.LoadFromFile(FilePath);
        FImages.Infos.OtherIcon := Description;
      end;
    $03:
      begin
        FImages.Pictures.CoverFront.LoadFromFile(FilePath);
        FImages.Infos.CoverFront := Description;
      end;
    $04:
      begin
        FImages.Pictures.CoverBack.LoadFromFile(FilePath);
        FImages.Infos.CoverBack := Description;
      end;
    $05:
      begin
        FImages.Pictures.Leaflet.LoadFromFile(FilePath);
        FImages.Infos.Leaflet := Description;
      end;
    $06:
      begin
        FImages.Pictures.Media.LoadFromFile(FilePath);
        FImages.Infos.Media := Description;
      end;
    $07:
      begin
        FImages.Pictures.LeadArtist.LoadFromFile(FilePath);
        FImages.Infos.LeadArtist := Description;
      end;
    $08:
      begin
        FImages.Pictures.Artist.LoadFromFile(FilePath);
        FImages.Infos.Artist := Description;
      end;
    $09:
      begin
        FImages.Pictures.Conductor.LoadFromFile(FilePath);
        FImages.Infos.Conductor := Description;
      end;
    $0A:
      begin
        FImages.Pictures.Band.LoadFromFile(FilePath);
        FImages.Infos.Band := Description;
      end;
    $0B:
      begin
        FImages.Pictures.Composer.LoadFromFile(FilePath);
        FImages.Infos.Composer := Description;
      end;
    $0C:
      begin
        FImages.Pictures.Lyricist.LoadFromFile(FilePath);
        FImages.Infos.Lyricist := Description;
      end;
    $0D:
      begin
        FImages.Pictures.RecordingLocation.LoadFromFile(FilePath);
        FImages.Infos.RecordingLocation := Description;
      end;
    $0E:
      begin
        FImages.Pictures.DuringRecording.LoadFromFile(FilePath);
        FImages.Infos.DuringRecording := Description;
      end;
    $0F:
      begin
        FImages.Pictures.DuringPerformance.LoadFromFile(FilePath);
        FImages.Infos.DuringPerformance := Description;
      end;
    $10:
      begin
        FImages.Pictures.MovieCapture.LoadFromFile(FilePath);
        FImages.Infos.MovieCapture := Description;
      end;
    $11:
      begin
        FImages.Pictures.ColouredFish.LoadFromFile(FilePath);
        FImages.Infos.ColouredFish := Description;
      end;
    $12:
      begin
        FImages.Pictures.Illustration.LoadFromFile(FilePath);
        FImages.Infos.Illustration := Description;
      end;
    $13:
      begin
        FImages.Pictures.BandLogo.LoadFromFile(FilePath);
        FImages.Infos.BandLogo := Description;
      end;
    $14:
      begin
        FImages.Pictures.PublisherLogo.LoadFromFile(FilePath);
        FImages.Infos.PublisherLogo := Description;
      end;
  end;
  DeleteFile(FilePath);
  Picture.Free;
end;

procedure TJvId3v2.ReadIpls(Frame: TId3v2Frame; Source: TStream);
var
  Encoding: Byte;
  Buf: array [0..64000] of Char;
  St, St2: string;
  I: Integer;
  Second: Boolean;
begin
  Source.Read(Encoding, 1);
  Source.Read(Buf, Frame.Size - 1);
  if Encoding = 0 then
  begin
    //Iso
    St := '';
    St2 := '';
    Second := False;
    for I := 0 to Frame.Size - 2 do
    begin
      if Buf[I] = #0 then
      begin
        if Second then
        begin
          FInvolvedPeople.AddItem(St, St2);
          St := '';
          St2 := '';
        end;
        Second := not Second;
      end
      else
        if Second then
        St2 := St2 + Buf[I]
      else
        St := St + Buf[I];
    end;
    if (St <> '') or (St2 <> '') then
      FInvolvedPeople.AddItem(St, St2);
  end
  else
  begin
    //unicode
    St := '';
    St2 := '';
    Second := False;
    I := 2;
    while I < Frame.Size - 1 do
    begin
      if Buf[I] = #0 then
      begin
        if Second then
        begin
          FInvolvedPeople.AddItem(St, St2);
          St := '';
          St2 := '';
        end;
        Second := not Second;
        Inc(I, 2);
      end
      else
      if Second then
        St2 := St2 + Buf[I]
      else
        St := St + Buf[I];
      Inc(I, 2);
    end;
    if (St <> '') or (St2 <> '') then
      FInvolvedPeople.AddItem(St, St2);
  end;
end;

procedure TJvId3v2.ReadOwner(Frame: TId3v2Frame; Source: TStream);
var
  Encoding: Byte;
  St: string;
  B: Byte;
  y, d, m: Integer;
begin
  Source.Read(Encoding, 1);
  Dec(Frame.Size);
  St := '';

  //Read price payed
  Source.Read(B, 1);
  while B <> $00 do
  begin
    St := St + Char(B);
    Source.Read(B, 1);
    Dec(Frame.Size);
  end;
  FOwner.Price := St;

  //Read date of purchased
  Source.Read(B, 1);
  y := B - Ord('0');
  y := y * 10;
  Source.Read(B, 1);
  y := y + (B - Ord('0'));
  y := y * 10;
  Source.Read(B, 1);
  y := y + (B - Ord('0'));
  y := y * 10;
  Source.Read(B, 1);
  y := y + (B - Ord('0'));

  Source.Read(B, 1);
  m := B - Ord('0');
  m := m * 10;
  Source.Read(B, 1);
  m := m + (B - Ord('0'));

  Source.Read(B, 1);
  d := B - Ord('0');
  d := d * 10;
  Source.Read(B, 1);
  d := d + (B - Ord('0'));

  FOwner.DatePurchased := EncodeDate(y, m, d);
  Dec(Frame.Size, 8);

  //Read seller
  St := '';
  if Encoding = $00 then
  begin
    while Frame.Size > 0 do
    begin
      Source.Read(B, 1);
      St := St + Char(B);
      Dec(Frame.Size);
    end;
  end
  else
  begin
    //Unicode
    if Frame.Size > 2 then
    begin
      Source.Read(B, 1);
      Source.Read(B, 1);
      Dec(Frame.Size, 2);
      while Frame.Size > 0 do
      begin
        Source.Read(B, 1);
        St := St + Char(B);
        Source.Read(B, 1);
        Dec(Frame.Size, 2);
      end;
    end;
  end;
  FOwner.Seller := St;
end;

procedure TJvId3v2.ReadPop(Frame: TId3v2Frame; Source: TStream);
var
  St: string;
  B: Byte;
  I: Integer;
begin
  Source.Read(B, 1);
  St := '';
  while B <> $00 do
  begin
    St := St + Char(B);
    Source.Read(B, 1);
  end;
  FPopularimeter.UserEmail := St;

  Source.Read(B, 1);
  FPopularimeter.Rating := B;

  Source.Read(I, 4);
  asm
      mov eax,I
      mov cl,al
      shl ecx,8
      mov cl,ah
      shl ecx,8
      shr eax,16
      mov cl,al
      shl ecx,8
      mov cl,ah
      mov ebx,offset I
      mov [ebx],ecx
  end;
  FPopularimeter.Counter := I;
end;

procedure TJvId3v2.ReadText(Frame: TId3v2Frame; Source: TStream);
var
  Encoding: Byte;
  Buf: array [0..64000] of Char;
  Stw, Stw2: WideString;
  St, St2: string;
  I: Integer;
  Second: Boolean;
begin
  if Frame.Id = 'TXXX' then
  begin
    Source.Read(Encoding, 1);
    Second := False;
    if Encoding = 1 then
    begin
      //Unicode
      Source.Read(Buf, Frame.Size - 1);
      Stw := '';
      Stw2 := '';
      I := 2;
      while I < Frame.Size - 2 do
      begin
        if Second then
          Stw2 := Stw2 + Buf[I]
        else
          if Buf[I] = #0 then
        begin
          Second := True;
          Inc(I, 2);
        end
        else
          Stw := Stw + Buf[I];
        Inc(I, 2);
      end;
      St := Stw;
      St2 := Stw2;
    end
    else
    begin
      Source.Read(Buf, Frame.Size - 1);
      St := '';
      St2 := '';
      for I := 0 to Frame.Size - 2 do
      begin
        if Second then
          St2 := St2 + Buf[I]
        else
          if Buf[I] = #0 then
          Second := True
        else
          St := St + Buf[I];
      end;
    end;
    FUserDefinedText.AddItem(St, St2);
  end
  else
  begin
    Source.Read(Encoding, 1);
    if Encoding = 0 then
    begin
      //ISO-8859-1
      Source.Read(Buf, Frame.Size - 1);
      St := '';
      for I := 0 to Frame.Size - 2 do
        St := St + Buf[I];
    end
    else
    begin
      //Unicode
   // (rom) better do real Unicode conversion here
      Source.Read(Buf, Frame.Size - 1);
      Stw := '';
      I := 2;
      while I < Frame.Size - 2 do
      begin
        Stw := Stw + Buf[I];
        Inc(I, 2);
      end;
      St := Stw;
    end;

    if Frame.Id = 'TALB' then
      FTexts.Album := St
    else
      if Frame.id = 'TBPM' then
      FTexts.BPM := StrToInt(St)
    else
      if Frame.id = 'TCOM' then
    begin
      while Pos('/', St) <> 0 do
      begin
        FTexts.Composer.Add(Copy(St, 1, Pos('/', St) - 1));
        St := Copy(St, Pos('/', St) + 1, Length(St));
      end;
      FTexts.Composer.Add(St);
    end
    else
    if Frame.id = 'TCON' then
      FTexts.Content := St
    else
    if Frame.id = 'TCOP' then
      FTexts.Copyright := St
    else
    if Frame.id = 'TDAT' then
      FTexts.Date := St
    else
    if Frame.id = 'TDLY' then
      FTexts.PlaylistDelay := StrToInt(St)
    else
    if Frame.id = 'TENC' then
      FTexts.EncodedBy := St
    else
    if Frame.id = 'TEXT' then
    begin
      while Pos('/', St) <> 0 do
      begin
        FTexts.Lyricists.Add(Copy(St, 1, Pos('/', St) - 1));
        St := Copy(St, Pos('/', St) + 1, Length(St));
      end;
      FTexts.Lyricists.Add(St);
    end
    else
    if Frame.id = 'TFLT' then
    begin
      if St = 'MPG' then
        FTexts.FileType := ftMPG
      else
      if St = 'MPG/1' then
        FTexts.FileType := ftMPG1
      else
      if St = 'MPG/2' then
        FTexts.FileType := ftMPG2
      else
      if St = 'MPG/3' then
        FTexts.FileType := ftMPG3
      else
      if St = 'MPG/2.5' then
        FTexts.FileType := ftMPG2_5
      else
      if St = 'MPG/AAC' then
        FTexts.FileType := ftMPG_AAC
      else
      if St = 'VQF' then
        FTexts.FileType := ftVQF
      else
      if St = 'PCM' then
        FTexts.FileType := ftPCM
      else
        FTexts.FileType := ftUNKNOWN;
    end
    else
    if Frame.id = 'TIME' then
      FTexts.Time := St
    else
    if Frame.id = 'TIT1' then
      FTexts.ContentGroup := St
    else
    if Frame.id = 'TIT2' then
      FTexts.ContentDescription := St
    else
    if Frame.id = 'TIT3' then
      FTexts.SubTitle := St
    else
    if Frame.id = 'TKEY' then
      FTexts.InitialKey := St
    else
    if Frame.id = 'TLAN' then
    begin
      while Length(St) > 3 do
      begin
        St2 := Copy(St, 1, 3);
        FTexts.Languages.Add(Iso639ToName(St2));
        St := Copy(St, 4, Length(St));
      end;
      FTexts.Languages.Add(Iso639ToName(St));
    end
    else
    if Frame.id = 'TLEN' then
      FTexts.Length := St
    else
    if Frame.id = 'TMED' then
    begin
      if St = 'DIG' then
        FTexts.MediaType := mtDIG
      else
      if St = 'DIG/A' then
        FTexts.MediaType := mtDIG_ANALOG_TRANSFER
      else
      if St = 'ANA' then
        FTexts.MediaType := mtANA
      else
      if St = 'ANA/WAC' then
        FTexts.MediaType := mtANA_WAX_CYLINDER
      else
      if St = 'ANA/8CA' then
        FTexts.MediaType := mtANA_8_TRACK_TAPE
      else
      if St = 'CD' then
        FTexts.MediaType := mtCD
      else
      if St = 'CD/A' then
        FTexts.MediaType := mtCD_ANALOG
      else
      if St = 'CD/DD' then
        FTexts.MediaType := mtCD_DDD
      else
      if St = 'CD/AD' then
        FTexts.MediaType := mtCD_ADD
      else
      if St = 'CD/AA' then
        FTexts.MediaType := mtCD_AAD
      else
      if St = 'LD' then
        FTexts.MediaType := mtLASERDISC
      else
      if St = 'LD/A' then
        FTexts.MediaType := mtLASERDISC_ANALOG_TRANSFER
      else
      if St = 'TT' then
        FTexts.MediaType := mtTURNTABLE
      else
      if St = 'TT/33' then
        FTexts.MediaType := mtTURNTABLE_33
      else
      if St = 'TT/45' then
        FTexts.MediaType := mtTURNTABLE_45
      else
      if St = 'TT/71' then
        FTexts.MediaType := mtTURNTABLE_71
      else
      if St = 'TT/76' then
        FTexts.MediaType := mtTURNTABLE_76
      else
      if St = 'TT/78' then
        FTexts.MediaType := mtTURNTABLE_78
      else
      if St = 'TT/80' then
        FTexts.MediaType := mtTURNTABLE_80
      else
      if St = 'MD' then
        FTexts.MediaType := mtMINIDISC
      else
      if St = 'MD/A' then
        FTexts.MediaType := mtMINIDISC_ANALOG_TRANSFER
      else
      if St = 'DAT' then
        FTexts.MediaType := mtDAT
      else
      if St = 'DAT/A' then
        FTexts.MediaType := mtDAT_ANALOG_TRANSFER
      else
      if St = 'DAT/1' then
        FTexts.MediaType := mtDAT_48KHZ_16B
      else
      if St = 'DAT/2' then
        FTexts.MediaType := mtDAT_32KHZ_16B
      else
      if St = 'DAT/3' then
        FTexts.MediaType := mtDAT_32KHZ_12B
      else
      if St = 'DAT/4' then
        FTexts.MediaType := mtDAT_32KHZ_12B_4CH
      else
      if St = 'DAT/5' then
        FTexts.MediaType := mtDAT_44KHZ_16B
      else
      if St = 'DAT/6' then
        FTexts.MediaType := mtDAT_44KHZ_16B_WIDE
      else
      if St = 'DCC' then
        FTexts.MediaType := mtDCC
      else
      if St = 'DCC/A' then
        FTexts.MediaType := mtDCC_ANALOG_TRANSFER
      else
      if St = 'DVD' then
        FTexts.MediaType := mtDVD
      else
      if St = 'DVD/A' then
        FTexts.MediaType := mtDVD_ANALOG_TRANSFER
      else
      if St = 'TV' then
        FTexts.MediaType := mtTV
      else
      if St = 'TV/PAL' then
        FTexts.MediaType := mtTV_PAL
      else
      if St = 'TV/NTSC' then
        FTexts.MediaType := mtTV_NTSC
      else
      if St = 'TV/SECAM' then
        FTexts.MediaType := mtTV_SECAM
      else
      if St = 'VID' then
        FTexts.MediaType := mtVID
      else
      if St = 'VID/PAL' then
        FTexts.MediaType := mtVID_PAL
      else
      if St = 'VID/NTSC' then
        FTexts.MediaType := mtVID_NTSC
      else
      if St = 'VID/SECAM' then
        FTexts.MediaType := mtVID_SECAM
      else
      if St = 'VID/VHS' then
        FTexts.MediaType := mtVID_VHS
      else
      if St = 'VID/SVHS' then
        FTexts.MediaType := mtVID_SVHS
      else
      if St = 'VID/BETA' then
        FTexts.MediaType := mtVID_BETA
      else
      if St = 'RAD' then
        FTexts.MediaType := mtRAD
      else
      if St = 'RAD/FM' then
        FTexts.MediaType := mtRAD_FM
      else
      if St = 'RAD/AM' then
        FTexts.MediaType := mtRAD_AM
      else
      if St = 'RAD/LW' then
        FTexts.MediaType := mtRAD_LW
      else
      if St = 'RAD/MW' then
        FTexts.MediaType := mtRAD_MW
      else
      if St = 'TEL' then
        FTexts.MediaType := mtTEL
      else
      if St = 'TEL/I' then
        FTexts.MediaType := mtTEL_ISDN
      else
      if St = 'MC' then
        FTexts.MediaType := mtMC
      else
      if St = 'MC/4' then
        FTexts.MediaType := mtMC_4
      else
      if St = 'MC/9' then
        FTexts.MediaType := mtMC_9
      else
      if St = 'MC/I' then
        FTexts.MediaType := mtMC_I
      else
      if St = 'MC/II' then
        FTexts.MediaType := mtMC_II
      else
      if St = 'MC/III' then
        FTexts.MediaType := mtMC_III
      else
      if St = 'MC/IV' then
        FTexts.MediaType := mtMC_IV
      else
      if St = 'REE' then
        FTexts.MediaType := mtREE
      else
      if St = 'REE/9' then
        FTexts.MediaType := mtREE_9
      else
      if St = 'REE/19' then
        FTexts.MediaType := mtREE_19
      else
      if St = 'REE/38' then
        FTexts.MediaType := mtREE_38
      else
      if St = 'REE/76' then
        FTexts.MediaType := mtREE_76
      else
      if St = 'REE/I' then
        FTexts.MediaType := mtREE_I
      else
      if St = 'REE/II' then
        FTexts.MediaType := mtREE_II
      else
      if St = 'REE/III' then
        FTexts.MediaType := mtREE_III
      else
      if St = 'REE/IV' then
        FTexts.MediaType := mtREE_IV
      else
        FTexts.MediaType := mtUNKNOWN;
    end
    else
    if Frame.id = 'TOAL' then
      FTexts.OriginalTitle := St
    else
    if Frame.id = 'TOFN' then
      FTexts.OriginalFileName := St
    else
    if Frame.id = 'TOLY' then
    begin
      while Pos('/', St) <> 0 do
      begin
        FTexts.OriginalLyricists.Add(Copy(St, 1, Pos('/', St) - 1));
        St := Copy(St, Pos('/', St) + 1, Length(St));
      end;
      FTexts.OriginalLyricists.Add(St);
    end
    else
    if Frame.id = 'TOPE' then
    begin
      while Pos('/', St) <> 0 do
      begin
        FTexts.OriginalArtists.Add(Copy(St, 1, Pos('/', St) - 1));
        St := Copy(St, Pos('/', St) + 1, Length(St));
      end;
      FTexts.OriginalArtists.Add(St);
    end
    else
    if Frame.id = 'TORY' then
      FTexts.OriginalReleaseYear := St
    else
    if Frame.id = 'TOWN' then
      FTexts.FileOwner := St
    else
    if Frame.id = 'TPE1' then
    begin
      while Pos('/', St) <> 0 do
      begin
        FTexts.Performers.Add(Copy(St, 1, Pos('/', St) - 1));
        St := Copy(St, Pos('/', St) + 1, Length(St));
      end;
      FTexts.Performers.Add(St);
    end
    else
    if Frame.id = 'TPE2' then
      FTexts.Band := St
    else
    if Frame.id = 'TPE3' then
      FTexts.Conductor := St
    else
    if Frame.id = 'TPE4' then
      FTexts.ModifiedBy := St
    else
    if Frame.id = 'TPOS' then
      FTexts.PartOf := St
    else
    if Frame.id = 'TPUB' then
      FTexts.Publisher := St
    else
    if Frame.id = 'TRCK' then
      FTexts.TrackNumber := St
    else
    if Frame.id = 'TRDA' then
      FTexts.RecordingDate := St
    else
    if Frame.id = 'TRSN' then
      FTexts.InternetRadioName := St
    else
    if Frame.id = 'TRSO' then
      FTexts.InternetRadioOwner := St
    else
    if Frame.id = 'TSIZ' then
      FTexts.Size := StrToInt(St)
    else
    if Frame.id = 'TSRC' then
      FTexts.ISRC := St
    else
    if Frame.id = 'TSSE' then
      FTexts.EncodingSoftware := St
    else
    if Frame.id = 'TYER' then
      FTexts.Year := St;
  end;
end;

procedure TJvId3v2.ReadWeb(Frame: TId3v2Frame; Source: TStream);
var
  Buf: array [0..64000] of Char;
  St, St2: string;
  I: Integer;
begin
  Source.Read(Buf, Frame.Size - 1);
  St := '';
  St2 := '';
  if Frame.Id = 'WXXX' then
  begin
    if Buf[0] = #0 then
    begin
      I := 1;
      while Buf[I] <> #0 do
      begin
        St := St + Buf[I];
        Inc(I);
      end;
    end
    else
    begin
      //unicode Description
      I := 3;
      while Buf[I] <> #0 do
      begin
        St := St + Buf[I];
        Inc(I, 2);
      end;
    end;
    Inc(I);
    while I < Frame.Size do
    begin
      St2 := St2 + Buf[I];
      Inc(I);
    end;
    FUserDefinedWeb.AddItem(St, St2);
  end
  else
  begin
    for I := 0 to Frame.Size - 2 do
      St := St + Buf[I];
    if Frame.Id = 'WCOM' then
      FWeb.CommercialInfo := St
    else
    if Frame.Id = 'WCOP' then
      FWeb.LegalInfo := St
    else
    if Frame.Id = 'WOAF' then
      FWeb.OfficialAudio := St
    else
    if Frame.Id = 'WOAR' then
      FWeb.OfficialArtist := St
    else
    if Frame.Id = 'WOAS' then
      FWeb.OfficialAudioSource := St
    else
    if Frame.Id = 'WORS' then
      FWeb.InternetRadioStation := St
    else
    if Frame.Id = 'WPAY' then
      FWeb.Payment := St
    else
    if Frame.Id = 'WPUB' then
      FWeb.Publishers := St;
  end;
end;

procedure TJvId3v2.ResetProp;
begin
  FVersion := 0.0;
  FUnsynchronisation := False;
  FExperimental := False;
  FExtendedHeader := False;

  FTexts.ResetFields;
  FUserDefinedText.ResetFields;
  FWeb.ResetFields;
  FUserDefinedWeb.ResetFields;
  FInvolvedPeople.ResetFields;
  FImages.ResetFields;
  FOwner.ResetFields;
  FPopularimeter.ResetFields;

  FPlayCounter := 0;
  FillChar(MusicCDIdentifier, SizeOf(MusicCDIdentifier), 0);
  MusicCDIdentifierLength := 0;
end;

procedure TJvId3v2.SetFileName(const Value: TFileName);
begin
  FTagSize := 0;
  FFileName := Value;
  LoadFromFile(Value);
end;

procedure TJvId3v2.SkipFrame(Frame: TId3v2Frame; var Source: TStream);
begin
  Source.Position := Source.Position + Frame.Size;
end;

//=== TJvId3Text =============================================================

constructor TJvId3Text.Create;
begin
  inherited Create;
  FComposer := TStringList.Create;
  FLyricists := TStringList.Create;
  FLanguages := TStringList.Create;
  FOriginalLyricists := TStringList.Create;
  FOriginalArtists := TStringList.Create;
  FPerformers := TStringList.Create;
  ResetFields;
end;

destructor TJvId3Text.Destroy;
begin
  FComposer.Free;
  FLyricists.Free;
  FLanguages.Free;
  FOriginalLyricists.Free;
  FOriginalArtists.Free;
  FPerformers.Free;
  inherited Destroy;
end;

procedure TJvId3Text.ResetFields;
begin
  FBPM := 0;
  FAlbum := '';
  FComposer.Clear;
  FPlaylistDelay := 0;
  FCopyright := '';
  FEncodedBy := '';
  FDate := '';
  FLyricists.Clear;
  FFileType := ftUNKNOWN;
  FTime := '';
  FInitialKey := '';
  FContentGroup := '';
  FSubTitle := '';
  FContentDescription := '';
  FLanguages.Clear;
  FLength := '';
  FMediaType := mtUNKNOWN;
  FOriginalTitle := '';
  FOriginalFileName := '';
  FOriginalLyricists.Clear;
  FOriginalArtists.Clear;
  FOwner := '';
  FOriginalReleaseYear := '';
  FPerformers.Clear;
  FConductor := '';
  FModifiedBy := '';
  FBand := '';
  FPartOf := '';
  FPublisher := '';
  FTrackNumber := '';
  FRecordingDate := '';
  FInternetRadioName := '';
  FInternetRadioOwner := '';
  FSize := 0;
  FISRC := '';
  FEncodingSoftware := '';
  FYear := '';
  FContent := '';
end;

//=== TJvId3Web ==============================================================

procedure TJvId3Web.ResetFields;
begin
  FOfficialAudio := '';
  FInternetRadioStation := '';
  FPublishers := '';
  FOfficialAudioSource := '';
  FPayment := '';
  FLegalInfo := '';
  FCommercialInfo := '';
  FOfficialArtist := '';
end;

//=== TJvID3UDText ===========================================================

constructor TJvID3UDText.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FDescriptions := TStringList.Create;
end;

destructor TJvID3UDText.Destroy;
begin
  FStrings.Free;
  FDescriptions.Free;
  inherited Destroy;
end;

procedure TJvID3UDText.AddItem(Desc, Value: string);
begin
  FStrings.Add(Value);
  FDescriptions.Add(Desc);
  SetItem(0);
  FItemCount := FStrings.Count;
end;

procedure TJvID3UDText.ResetFields;
begin
  FStrings.Clear;
  FDescriptions.Clear;
end;

procedure TJvID3UDText.SetItem(const Value: Integer);
begin
  FItemIndex := Value;
  if Value < FStrings.Count then
  begin
    FDescription := FDescriptions[Value];
    FValue := FStrings[Value];
  end
  else
  begin
    FDescription := '';
    FValue := '';
  end;
end;

//=== TJvID3UDUrl ============================================================

constructor TJvID3UDUrl.Create;
begin
  inherited Create;
  FURLList := TStringList.Create;
  FDescriptions := TStringList.Create;
end;

destructor TJvID3UDUrl.Destroy;
begin
  FURLList.Free;
  FDescriptions.Free;
  inherited Destroy;
end;

procedure TJvID3UDUrl.AddItem(Desc, Value: string);
begin
  FURLList.Add(Value);
  FDescriptions.Add(Desc);
  SetItem(0);
  FItemCount := FURLList.Count;
end;

procedure TJvID3UDUrl.ResetFields;
begin
  FURLList.Clear;
  FDescriptions.Clear;
end;

procedure TJvID3UDUrl.SetItem(const Value: Integer);
begin
  FItemIndex := Value;
  if Value < FURLList.Count then
  begin
    FDescription := FDescriptions[Value];
    FURL := FURLList[Value];
  end
  else
  begin
    FDescription := '';
    FURL := '';
  end;
end;

//=== TJvIdPictures ==========================================================

constructor TJvIdPictures.Create;
begin
  inherited Create;
  FMedia := TPicture.Create;
  FLyricist := TPicture.Create;
  FFileIcon := TPicture.Create;
  FComposer := TPicture.Create;
  FRecordingLocation := TPicture.Create;
  FOther := TPicture.Create;
  FColouredFish := TPicture.Create;
  FCoverFront := TPicture.Create;
  FBandLogo := TPicture.Create;
  FConductor := TPicture.Create;
  FPublisherLogo := TPicture.Create;
  FDuringRecording := TPicture.Create;
  FArtist := TPicture.Create;
  FLeaflet := TPicture.Create;
  FOther := TPicture.Create;
  FDPerformance := TPicture.Create;
  FIllustration := TPicture.Create;
  FBand := TPicture.Create;
  FMovieCapture := TPicture.Create;
  FCoverBack := TPicture.Create;
  FOtherIcon := TPicture.Create;
  FLeadArtist := TPicture.Create;
end;

destructor TJvIdPictures.Destroy;
begin
  FMedia.Free;
  FLyricist.Free;
  FFileIcon.Free;
  FComposer.Free;
  FRecordingLocation.Free;
  FOther.Free;
  FColouredFish.Free;
  FCoverFront.Free;
  FBandLogo.Free;
  FConductor.Free;
  FPublisherLogo.Free;
  FDuringRecording.Free;
  FArtist.Free;
  FLeaflet.Free;
  FDPerformance.Free;
  FIllustration.Free;
  FBand.Free;
  FMovieCapture.Free;
  FCoverBack.Free;
  FOtherIcon.Free;
  FLeadArtist.Free;
  inherited Destroy;
end;

procedure TJvIdPictures.SetArtist(const Value: TPicture);
begin
  FArtist.Assign(Value);
end;

procedure TJvIdPictures.SetBand(const Value: TPicture);
begin
  FBand.Assign(Value);
end;

procedure TJvIdPictures.SetBandLogo(const Value: TPicture);
begin
  FBandLogo.Assign(Value);
end;

procedure TJvIdPictures.SetComposer(const Value: TPicture);
begin
  FComposer.Assign(Value);
end;

procedure TJvIdPictures.SetConductor(const Value: TPicture);
begin
  FConductor.Assign(Value);
end;

procedure TJvIdPictures.SetCoverBack(const Value: TPicture);
begin
  FCoverBack.Assign(Value);
end;

procedure TJvIdPictures.SetCoverFront(const Value: TPicture);
begin
  FCoverFront.Assign(Value);
end;

procedure TJvIdPictures.SetDuringRecording(const Value: TPicture);
begin
  FDuringRecording.Assign(Value);
end;

procedure TJvIdPictures.SetColouredFish(const Value: TPicture);
begin
  FColouredFish.Assign(Value);
end;

procedure TJvIdPictures.SetFileIcon(const Value: TPicture);
begin
  FFileIcon.Assign(Value);
end;

procedure TJvIdPictures.SetIllustration(const Value: TPicture);
begin
  FIllustration.Assign(Value);
end;

procedure TJvIdPictures.SetLeadArtist(const Value: TPicture);
begin
  FLeadArtist.Assign(Value);
end;

procedure TJvIdPictures.SetLeaflet(const Value: TPicture);
begin
  FLeaflet.Assign(Value);
end;

procedure TJvIdPictures.SetRecordingLocation(const Value: TPicture);
begin
  FRecordingLocation.Assign(Value);
end;

procedure TJvIdPictures.SetLyricist(const Value: TPicture);
begin
  FLyricist.Assign(Value);
end;

procedure TJvIdPictures.SetMedia(const Value: TPicture);
begin
  FMedia.Assign(Value);
end;

procedure TJvIdPictures.SetMovieCapture(const Value: TPicture);
begin
  FMovieCapture.Assign(Value);
end;

procedure TJvIdPictures.SetOther(const Value: TPicture);
begin
  FOther.Assign(Value);
end;

procedure TJvIdPictures.SetOtherIcon(const Value: TPicture);
begin
  FOtherIcon.Assign(Value);
end;

procedure TJvIdPictures.SetPerformance(const Value: TPicture);
begin
  FDPerformance.Assign(Value);
end;

procedure TJvIdPictures.SetPublisherLogo(const Value: TPicture);
begin
  FPublisherLogo.Assign(Value);
end;

//=== TJvIdImages ============================================================

constructor TJvIdImages.Create;
begin
  inherited Create;
  FPictures := TJvIdPictures.Create;
  FInfos := TJvIdPicturesDesc.Create;
end;

destructor TJvIdImages.Destroy;
begin
  FPictures.Free;
  FInfos.Free;
  inherited Destroy;
end;

procedure TJvIdImages.ResetFields;
begin
  FPictures.Other.Assign(nil);
  FPictures.FileIcon.Assign(nil);
  FPictures.OtherIcon.Assign(nil);
  FPictures.CoverFront.Assign(nil);
  FPictures.CoverBack.Assign(nil);
  FPictures.Leaflet.Assign(nil);
  FPictures.Media.Assign(nil);
  FPictures.Artist.Assign(nil);
  FPictures.Conductor.Assign(nil);
  FPictures.Band.Assign(nil);
  FPictures.Composer.Assign(nil);
  FPictures.Lyricist.Assign(nil);
  FPictures.RecordingLocation.Assign(nil);
  FPictures.DuringRecording.Assign(nil);
  FPictures.DuringPerformance.Assign(nil);
  FPictures.MovieCapture.Assign(nil);
  FPictures.ColouredFish.Assign(nil);
  FPictures.Illustration.Assign(nil);
  FPictures.BandLogo.Assign(nil);
  FPictures.PublisherLogo.Assign(nil);
  FPictures.LeadArtist.Assign(nil);
  FInfos.Other := '';
  FInfos.FileIcon := '';
  FInfos.OtherIcon := '';
  FInfos.CoverFront := '';
  FInfos.CoverBack := '';
  FInfos.Leaflet := '';
  FInfos.Media := '';
  FInfos.Artist := '';
  FInfos.Conductor := '';
  FInfos.Band := '';
  FInfos.Composer := '';
  FInfos.Lyricist := '';
  FInfos.RecordingLocation := '';
  FInfos.DuringRecording := '';
  FInfos.DuringPerformance := '';
  FInfos.MovieCapture := '';
  FInfos.ColouredFish := '';
  FInfos.Illustration := '';
  FInfos.BandLogo := '';
  FInfos.PublisherLogo := '';
  FInfos.LeadArtist := '';
end;

//=== TJvIdIpl ===============================================================

constructor TJvIdIpl.Create;
begin
  inherited Create;
  FJobs := TStringList.Create;
  FPersons := TStringList.Create;
end;

destructor TJvIdIpl.Destroy;
begin
  FJobs.Free;
  FPersons.Free;
  inherited Destroy;
end;

procedure TJvIdIpl.AddItem(Job, Person: string);
begin
  FJobs.Add(Job);
  FPersons.Add(Person);
  SetItemIndex(0);
  Inc(FItemCount);
end;

procedure TJvIdIpl.ResetFields;
begin
  FJobs.Clear;
  FPersons.Clear;
end;

procedure TJvIdIpl.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
  if Value < FJobs.Count then
  begin
    FJob := FJobs[Value];
    FPerson := FPersons[Value];
  end
  else
  begin
    FJob := '';
    FPerson := '';
  end;
end;

//=== TJvId3Owner ============================================================

procedure TJvId3Owner.ResetFields;
begin
  FSeller := '';
  FPrice := '';
  FDatePurchased := EncodeDate(1900, 1, 1);
end;

procedure TJvId3Popularimeter.ResetFields;
begin
  FRating := 0;
  FCounter := 0;
  FUserEmail := '';
end;

end.

