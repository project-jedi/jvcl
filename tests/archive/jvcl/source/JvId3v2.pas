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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

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

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, extctrls, Dialogs, JPEG, JvId3v2Types, JvComponent;

type
  TJvID3FileType = (ftUNKNOWN, ftMPG, ftMPG1, ftMPG2, ftMPG3, ftMPG2_5, ftMPG_AAC, ftVQF, ftPCM);
{$EXTERNALSYM TJvID3FileType}
  TJvID3MediaType = (mtUNKNOWN, mtDIG, mtDIG_ANALOG_TRANSFER, mtANA, mtANA_WAX_CYLINDER, mtANA_8_TRACK_TAPE, mtCD,
    mtCD_ANALOG, mtCD_DDD, mtCD_ADD, mtCD_AAD, mtLASERDISC, mtLASERDISC_ANALOG_TRANSFER, mtTURNTABLE,
    mtTURNTABLE_33, mtTURNTABLE_45, mtTURNTABLE_71, mtTURNTABLE_76, mtTURNTABLE_78, mtTURNTABLE_80,
    mtMINIDISC, mtMINIDISC_ANALOG_TRANSFER, mtDAT, mtDAT_ANALOG_TRANSFER, mtDAT_48KHZ_16B, mtDAT_32KHZ_16B,
    mtDAT_32KHZ_12B, mtDAT_44KHZ_16B, mtDAT_44KHZ_16B_WIDE, mtDCC, mtDCC_ANALOG_TRANSFER, mtDVD,
    mtDVD_ANALOG_TRANSFER, mtTV, mtTV_PAL, mtTV_NTSC, mtTV_SECAM,
    mtVID, mtVID_PAL, mtVID_NTSC, mtVID_SECAM, mtVID_VHS, mtVID_SVHS, mtVID_BETA, mtRAD, mtRAD_FM,
    mtRAD_AM, mtRAD_LW, mtRAD_MW, mtTEL, mtTEL_ISDN, mtMC, mtMC_4, mtMC_9, mtMC_I, mtMC_II, mtMC_III,
    mtMC_IV, mtREE, mtREE_9, mtREE_19, mtREE_38, mtREE_76, mtREE_I, mtREE_II, mtREE_III, mtREE_IV,
    mtDAT_32KHZ_12B_4CH);
{$EXTERNALSYM TJvID3MediaType}

  TJvId3Text = class(TPersistent)
  private
    FBPM: Integer;
    FAlbum: string;
    FComposer: TStringList;
    FDelay: Integer;
    FCopyright: string;
    FEncodedBy: string;
    FDate: string;
    FLyricist: TStringList;
    FFileType: TJvID3FileType;
    FTime: string;
    FInitialKey: string;
    FContent: string;
    FSubTitle: string;
    FContentD: string;
    FLanguages: TStringList;
    FLength: string;
    FMediaType: TJvID3MediaType;
    FOriginal: string;
    FFileName: string;
    FOLyricists: TStringList;
    FOArtist: TStringList;
    FOwner: string;
    FOYear: string;
    FPerformers: TStringList;
    FConductor: string;
    FModified: string;
    FBand: string;
    FPart: string;
    FPublisher: string;
    FTrackNumber: string;
    FRecording: string;
    FInternet: string;
    FIOwner: string;
    FSize: Integer;
    FISRC: string;
    FEncoding: string;
    FYear: string;
    procedure ResetFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Album: string read FAlbum write FAlbum;
    property BPM: Integer read FBPM write FBPM;
    property Composer: TStringList read FComposer write FComposer;
    property Copyright: string read FCopyright write FCopyright;
    property Date: string read FDate write FDate;
    property PlaylistDelay: Integer read FDelay write FDelay;
    property EncodedBy: string read FEncodedBy write FEncodedBy;
    property Lyricists: TStringList read FLyricist write FLyricist;
    property FileType: TJvID3FileType read FFileType write FFileType;
    property Time: string read FTime write FTime;
    property ContentGroup: string read FContent write FContent;
    property ContentDescription: string read FContentD write FContentD;
    property SubTitle: string read FSubTitle write FSubTitle;
    property InitialKey: string read FInitialKey write FInitialKey;
    property Languages: TStringList read FLanguages write FLanguages;
    property Length: string read FLength write FLength;
    property MediaType: TJvID3MediaType read FMediaType write FMediaType;
    property OriginalTitle: string read FOriginal write FOriginal;
    property OriginalFileName: string read FFileName write FFileName;
    property OriginalLyricists: TStringList read FOLyricists write FOLyricists;
    property OriginalArtist: TStringList read FOArtist write FOArtist;
    property OriginalReleaseYear: string read FOYear write FOYear;
    property FileOwner: string read FOwner write FOwner;
    property Performers: TStringList read FPerformers write FPerformers;
    property Band: string read FBand write FBand;
    property Conductor: string read FConductor write FConductor;
    property ModifiedBy: string read FModified write FModified;
    property PartOf: string read FPart write FPart;
    property Publisher: string read FPublisher write FPublisher;
    property TrackNumber: string read FTrackNumber write FTrackNumber;
    property RecordingDate: string read FRecording write FRecording;
    property InternetRadioName: string read FInternet write FInternet;
    property InternetRadioOwner: string read FIOwner write FIOwner;
    property Size: Integer read FSize write FSize;
    property ISRC: string read FISRC write FISRC;
    property EncodingSoftware: string read FEncoding write FEncoding;
    property Year: string read FYear write FYear;
    property Content: string read FContent write FContent;
  end;

  TJvId3Web = class(TPersistent)
  private
    FAudio: string;
    FStation: string;
    FPublishers: string;
    FASource: string;
    FPayment: string;
    FLegal: string;
    FCommercial: string;
    FArtist: string;
    procedure ResetFields;
  published
    property CommercialInfo: string read FCommercial write FCommercial;
    property LegalInfo: string read FLegal write FLegal;
    property OfficialAudio: string read FAudio write FAudio;
    property OfficialArtist: string read FArtist write FArtist;
    property OfficialAudioSource: string read FASource write FASource;
    property InternetRadioStation: string read FStation write FStation;
    property Payment: string read FPayment write FPayment;
    property Publishers: string read FPublishers write FPublishers;
  end;

  TJvID3UDText = class(TPersistent)
  private
    FCount: Integer;
    FItems: Integer;
    FDescription: string;
    FValue: string;
    FBidonI: Integer;
    FStrings: TStringList;
    FDescriptions: TStringList;

    procedure ResetFields;
    procedure SetItem(const Value: Integer);
    procedure AddItem(desc, Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ItemIndex: Integer read FItems write SetItem;
    property Description: string read FDescription write FDescription;
    property Value: string read FValue write FValue;
    property ItemCount: Integer read FCount write FBidonI;
  end;

  TJvID3UDUrl = class(TPersistent)
  private
    FCount: Integer;
    FItems: Integer;
    FDescription: string;
    FValue: string;
    FBidonI: Integer;
    FStrings: TStringList;
    FDescriptions: TStringList;

    procedure ResetFields;
    procedure SetItem(const Value: Integer);
    procedure AddItem(desc, Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ItemIndex: Integer read FItems write SetItem;
    property Description: string read FDescription write FDescription;
    property URL: string read FValue write FValue;
    property ItemCount: Integer read FCount write FBidonI;
  end;

  TJvIdPictures = class(TPersistent)
  private
    FMedia: TPicture;
    FLyricist: TPicture;
    FIcon: TPicture;
    FComposer: TPicture;
    FRLocation: TPicture;
    FOther: TPicture;
    FFish: TPicture;
    FCoverFront: TPicture;
    FBandLogo: TPicture;
    FConductor: TPicture;
    FPublisherLogo: TPicture;
    FDRecording: TPicture;
    FArtist: TPicture;
    FLeaflet: TPicture;
    FDPerformance: TPicture;
    FIllustration: TPicture;
    FBand: TPicture;
    FMove: TPicture;
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
    procedure SetDRecording(const Value: TPicture);
    procedure SetFish(const Value: TPicture);
    procedure SetIcon(const Value: TPicture);
    procedure SetIllustration(const Value: TPicture);
    procedure SetLeaflet(const Value: TPicture);
    procedure SetLocation(const Value: TPicture);
    procedure SetLyricist(const Value: TPicture);
    procedure SetMedia(const Value: TPicture);
    procedure SetMovie(const Value: TPicture);
    procedure SetOther(const Value: TPicture);
    procedure SetPerformance(const Value: TPicture);
    procedure SetPublisherLogo(const Value: TPicture);
    procedure SetOtherIcon(const Value: TPicture);
    procedure SetLeadArtist(const Value: TPicture);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Other: TPicture read FOther write SetOther;
    property FileIcon: TPicture read FIcon write SetIcon;
    property OtherIcon: TPicture read FOtherIcon write SetOtherIcon;
    property CoverFront: TPicture read FCoverFront write SetCoverFront;
    property CoverBack: TPicture read FCoverBack write SetCoverBack;
    property Leaflet: TPicture read FLeaflet write SetLeaflet;
    property Media: TPicture read FMedia write SetMedia;
    property Artist: TPicture read FArtist write SetArtist;
    property Conductor: TPicture read FConductor write SetConductor;
    property Band: TPicture read FBand write SetBand;
    property Composer: TPicture read FComposer write SetComposer;
    property Lyricist: TPicture read FLyricist write SetLyricist;
    property recordingLocation: TPicture read FRLocation write SetLocation;
    property DuringRecording: TPicture read FDRecording write SetDRecording;
    property DuringPerformance: TPicture read FDPerformance write SetPerformance;
    property MovieCapture: TPicture read FMove write SetMovie;
    property ColouredFish: TPicture read FFish write SetFish;
    property Illustration: TPicture read FIllustration write SetIllustration;
    property BandLogo: TPicture read FBandLogo write SetBandLogo;
    property PublisherLogo: TPicture read FPublisherLogo write SetPublisherLogo;
    property LeadArtist: TPicture read FLeadArtist write SetLeadArtist;
  end;

  TJvIdPicturesDesc = class(TPersistent)
  private
    FLeaflet: string;
    FOtherIcon: string;
    FArtist: string;
    FIcon: string;
    FCoverFront: string;
    FPublisherLogo: string;
    FLeadArtist: string;
    FConductor: string;
    FDRecording: string;
    FBandLogo: string;
    FLyricist: string;
    FCoverBack: string;
    FIllustration: string;
    FBand: string;
    FMovie: string;
    FDPerformance: string;
    FComposer: string;
    FMedia: string;
    FFish: string;
    FRLocation: string;
    FOther: string;
  published
    property Other: string read FOther write FOther;
    property FileIcon: string read FIcon write FICon;
    property OtherIcon: string read FOtherIcon write FOtherIcon;
    property CoverFront: string read FCoverFront write FCoverFront;
    property CoverBack: string read FCoverBack write FCoverBack;
    property Leaflet: string read FLeaflet write FLeaflet;
    property Media: string read FMedia write FMedia;
    property Artist: string read FArtist write FArtist;
    property Conductor: string read FConductor write FConductor;
    property Band: string read FBand write FBand;
    property Composer: string read FComposer write FComposer;
    property Lyricist: string read FLyricist write FLyricist;
    property recordingLocation: string read FRLocation write FRLocation;
    property DuringRecording: string read FDRecording write FDRecording;
    property DuringPerformance: string read FDPerformance write FDPerformance;
    property MovieCapture: string read FMovie write FMovie;
    property ColouredFish: string read FFish write FFish;
    property Illustration: string read FIllustration write FIllustration;
    property BandLogo: string read FBandLogo write FBandLogo;
    property PublisherLogo: string read FPublisherLogo write FPublisherLogo;
    property LeadArtist: string read FLeadArtist write FLeadArtist;
  end;

  TJvIdImages = class(TPErsistent)
  private
    FPictures: TJvIdPictures;
    FInfos: TJvIdPicturesDesc;
    procedure ResetFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Pictures: TJvIdPictures read FPictures write FPictures;
    property Infos: TJvIdPicturesDesc read FInfos write FInfos;
  end;

  TJvIdIpl = class(TPersistent)
  private
    FBidonI: Integer;
    FCount: Integer;
    FItems: Integer;
    FJob: string;
    FPerson: string;
    FJobs: TStringList;
    FPersons: TStringList;
    procedure SetItem(const Value: Integer);
    procedure AddItem(Job, Person: string);
    procedure ResetFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ItemIndex: Integer read FItems write SetItem;
    property Job: string read FJob write FJob;
    property Person: string read FPerson write FPerson;
    property ItemCount: Integer read FCount write FBidonI;
  end;

  TJvId3Owner = class(TPersistent)
  private
    FSeller: string;
    FPrice: string;
    FDate: TDate;
    procedure ResetFields;
  published
    property Price: string read FPrice write FPrice;
    property DatePurchased: TDate read FDate write FDate;
    property Seller: string read FSeller write FSeller;
  end;

  TJvId3Popularimeter = class(TPersistent)
  private
    FRating: Byte;
    FCounter: Integer;
    FEmail: string;
    procedure ResetFields;
  published
    property UserEmail: string read FEmail write FEmail;
    property Rating: Byte read FRating write FRating;
    property Counter: Integer read FCounter write FCounter;
  end;

  TJvId3EventType = (etPADDING, etEND_OF_INITIAL_SILENCE, etINTRO_START, etMAINPART_START,
    etOUTRO_START, etOUTRO_END, etVERSE_START, etREFRAIN_START, etINTERLUDE_START,
    etTHEME_START, etVARIATION_START, etKEY_CHANGE, eTTime_CHANGE, etUNWANTED_NOISE,
    etSUSTAINED_NOISE, etSUSTAINED_NOISE_END, etINTRO_END, etMAINPART_END, etVERSE_END,
    etREFRAIN_END, etTHEME_END, etAUDIO_END, etFILE_END);
{$EXTERNALSYM TJvId3EventType}

  TEventTiming = procedure(Sender: TObject; TimeStamp: Integer; EventType: TJvId3EventType) of object;

  TJvId3Events = record
    TimeStamp: Integer;
    EventType: TJvId3EventType;
  end;
{$EXTERNALSYM TJvId3Events}

  TJvId3v2 = class(TJvComponent)
  private
    FEvents: array[0..1000] of TJvId3Events;
    FEventsCount: Integer;
    FEventsTiming: Integer;
    FCount: Integer;

    FPresent: Boolean;
    FVersion: Real;
    FFileName: TFileName;
    FUnsynchro: Boolean;
    FExperimental: Boolean;
    FExtended: Boolean;
    FSize: Integer;
    FId3Text: TJvId3Text;
    FWeb: TJvId3Web;
    FUDText: TJvID3UDText;
    FUDWeb: TJvID3UDUrl;
    FInvolved: TJvIdIpl;
    FImages: TJvIdImages;
    FOwner: TJvId3Owner;
    FCounter: Cardinal;
    FPopula: TJvId3Popularimeter;
    FEventTiming: TEventTiming;

    //unusefull variables
    FBidonB: Boolean;
    FBidonR: Real;
    FBIdonI: Integer;
    FTagSize: Integer;
    procedure LoadFromFile(FileName: string);
    procedure SetFileName(const Value: TFileName);
    procedure ResetProp;
    procedure ReadAndUnsynchro(var source, dest: TStream; taille: Integer);

    procedure ReadText(frame: TId3v2Frame; source: TStream);
    procedure ReadWeb(frame: TId3v2Frame; source: TStream);
    procedure ReadIpls(frame: TId3v2Frame; source: TStream);
    procedure ReadImg(frame: TId3v2Frame; source: TStream);
    procedure ReadCounter(frame: TId3v2Frame; source: TStream);
    procedure ReadPop(frame: TId3v2Frame; source: TStream);
    procedure ReadOwner(frame: TId3v2Frame; source: TStream);
    procedure ReadEvent(frame: TId3v2Frame; source: TStream);

    procedure SkipFrame(frame: TId3v2Frame; var source: TStream);
    function Iso639ToName(code: string): string;
  protected
  public
    MusicCDIdentifier: array[0..803] of Byte;
    MusicCDIdentifierLength: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TagPresent: Boolean read FPresent write FBidonB;
    property FileName: TFileName read FFileName write SetFileName;
    property Version: Real read FVersion write FBidonR;
    property Unsynchronisation: Boolean read FUnsynchro write FBidonB;
    property ExtendedHeader: Boolean read FExtended write FBidonB;
    property Experimental: Boolean read FExperimental write FBidonB;
    property Texts: TJvId3Text read FId3Text write FId3Text;
    property UserDefinedText: TJvID3UDText read FUDText write FUDText;
    property Web: TJvId3Web read FWeb write FWeb;
    property UserDefinedWeb: TJvID3UDUrl read FUDWeb write FUDWeb;
    property InvolvedPeople: TJvIdIpl read FInvolved write FInvolved;
    property Images: TJvIdImages read FImages write FImages;
    property PlayCounter: Cardinal read FCounter write FCounter;
    property Owner: TJvId3Owner read FOwner write FOwner;
    property Popularimeter: TJvId3Popularimeter read FPopula write FPopula;
    property TagSize: Integer read FTagSize write FBidonI;

    procedure LoadFromStream(stream: TStream);
    procedure CheckEvent(CurrentTime: Integer);
    property OnEventTiming: TEventTiming read FEventTiming write FEventTiming;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvId3v2
///////////////////////////////////////////////////////////

procedure TJvId3v2.CheckEvent(CurrentTime: Integer);
var
  i: Integer;
begin
  if (FEventsCount > 0) and Assigned(FEventTiming) then
  begin
    if CurrentTime < FCount then
      FCount := 0;
    for i := 0 to FEventsCount - 1 do
      if FEvents[i].TimeStamp in [FCount..CurrentTime] then
        FEventTiming(Self, FEvents[i].TimeStamp, FEvents[i].EventType);
  end;
  FCount := CurrentTime;
end;

{**************************************************}

constructor TJvId3v2.Create(AOwner: TComponent);
begin
  inherited;
  FTagSize := 0;
  FCount := 0;
  FPresent := False;
  FId3Text := TJvId3Text.Create;
  FWeb := TJvId3Web.Create;
  FUDText := TJvID3UDText.Create;
  FUDWeb := TJvID3UDUrl.Create;
  FInvolved := TJvIdIpl.Create;
  FImages := TJvIdImages.Create;
  FOwner := TJvId3Owner.Create;
  FPopula := TJvId3Popularimeter.Create;
  ResetProp;
end;

{**************************************************}

destructor TJvId3v2.Destroy;
begin
  FId3Text.Free;
  FWeb.Free;
  FUDText.Free;
  FUDWeb.Free;
  FInvolved.Free;
  FImages.Free;
  FOwner.Free;
  FPopula.Free;
  inherited;
end;

{**************************************************}

// (rom) this function is clumsy

function TJvId3v2.Iso639ToName(code: string): string;
begin
  code := LowerCase(code);
  if code = 'aar' then
    Result := 'Afar'
  else if code = 'abk' then
    Result := 'Abkhazian'
  else if code = 'ace' then
    Result := 'Achinese'
  else if code = 'ach' then
    Result := 'Acoli'
  else if code = 'ada' then
    Result := 'Adangme'
  else if code = 'afa' then
    Result := 'Afro-Asiatic (Other)'
  else if code = 'afh' then
    Result := 'Afrihili'
  else if code = 'afr' then
    Result := 'Afrikaans'
  else if code = 'aka' then
    Result := 'Akan'
  else if code = 'akk' then
    Result := 'Akkadian'
  else if code = 'alb' then
    Result := 'Albanian'
  else if code = 'ale' then
    Result := 'Aleut'
  else if code = 'alg' then
    Result := 'Algonquian Languages'
  else if code = 'amh' then
    Result := 'Amharic'
  else if code = 'ang' then
    Result := 'English, Old (ca. 450-1100)'
  else if code = 'apa' then
    Result := 'Apache Languages'
  else if code = 'ara' then
    Result := 'Arabic'
  else if code = 'arc' then
    Result := 'Aramaic'
  else if code = 'arm' then
    Result := 'Armenian'
  else if code = 'arn' then
    Result := 'Araucanian'
  else if code = 'arp' then
    Result := 'Arapaho'
  else if code = 'art' then
    Result := 'Artificial (Other)'
  else if code = 'arw' then
    Result := 'Arawak'
  else if code = 'asm' then
    Result := 'Assamese'
  else if code = 'ath' then
    Result := 'Athapascan Languages'
  else if code = 'ava' then
    Result := 'Avaric'
  else if code = 'ave' then
    Result := 'Avestan'
  else if code = 'awa' then
    Result := 'Awadhi'
  else if code = 'aym' then
    Result := 'Aymara'
  else if code = 'aze' then
    Result := 'Azerbaijani'
  else if code = 'bad' then
    Result := 'Banda'
  else if code = 'bai' then
    Result := 'Bamileke Languages'
  else if code = 'bak' then
    Result := 'Bashkir'
  else if code = 'bal' then
    Result := 'Baluchi'
  else if code = 'bam' then
    Result := 'Bambara'
  else if code = 'ban' then
    Result := 'Balinese'
  else if code = 'baq' then
    Result := 'Basque'
  else if code = 'bas' then
    Result := 'Basa'
  else if code = 'bat' then
    Result := 'Baltic (Other)'
  else if code = 'bej' then
    Result := 'Beja'
  else if code = 'bel' then
    Result := 'Byelorussian'
  else if code = 'bem' then
    Result := 'Bemba'
  else if code = 'ben' then
    Result := 'Bengali'
  else if code = 'ber' then
    Result := 'Berber (Other)'
  else if code = 'bho' then
    Result := 'Bhojpuri'
  else if code = 'bih' then
    Result := 'Bihari'
  else if code = 'bik' then
    Result := 'Bikol'
  else if code = 'bin' then
    Result := 'Bini'
  else if code = 'bis' then
    Result := 'Bislama'
  else if code = 'bla' then
    Result := 'Siksika'
  else if code = 'bnt' then
    Result := 'Bantu (Other)'
  else if code = 'bod' then
    Result := 'Tibetan'
  else if code = 'bra' then
    Result := 'Braj'
  else if code = 'bre' then
    Result := 'Breton'
  else if code = 'bua' then
    Result := 'Buriat'
  else if code = 'bug' then
    Result := 'Buginese'
  else if code = 'bul' then
    Result := 'Bulgarian'
  else if code = 'bur' then
    Result := 'Burmese'
  else if code = 'cad' then
    Result := 'Caddo'
  else if code = 'cai' then
    Result := 'Central American Indian (Other)'
  else if code = 'car' then
    Result := 'Carib'
  else if code = 'cat' then
    Result := 'Catalan'
  else if code = 'cau' then
    Result := 'Caucasian (Other)'
  else if code = 'ceb' then
    Result := 'Cebuano'
  else if code = 'cel' then
    Result := 'Celtic (Other)'
  else if code = 'ces' then
    Result := 'Czech'
  else if code = 'cha' then
    Result := 'Chamorro'
  else if code = 'chb' then
    Result := 'Chibcha'
  else if code = 'che' then
    Result := 'Chechen'
  else if code = 'chg' then
    Result := 'Chagatai'
  else if code = 'chi' then
    Result := 'Chinese'
  else if code = 'chm' then
    Result := 'Mari'
  else if code = 'chn' then
    Result := 'Chinook jargon'
  else if code = 'cho' then
    Result := 'Choctaw'
  else if code = 'chr' then
    Result := 'Cherokee'
  else if code = 'chu' then
    Result := 'Church Slavic'
  else if code = 'chv' then
    Result := 'Chuvash'
  else if code = 'chy' then
    Result := 'Cheyenne'
  else if code = 'cop' then
    Result := 'Coptic'
  else if code = 'cor' then
    Result := 'Cornish'
  else if code = 'cos' then
    Result := 'Corsican'
  else if code = 'cpe' then
    Result := 'Creoles and Pidgins, English-based (Other)'
  else if code = 'cpf' then
    Result := 'Creoles and Pidgins, French-based (Other)'
  else if code = 'cpp' then
    Result := 'Creoles and Pidgins, Portuguese-based (Other)'
  else if code = 'cre' then
    Result := 'Cree'
  else if code = 'crp' then
    Result := 'Creoles and Pidgins (Other)'
  else if code = 'cus' then
    Result := 'Cushitic (Other)'
  else if code = 'cym' then
    Result := 'Welsh'
  else if code = 'cze' then
    Result := 'Czech'
  else if code = 'dak' then
    Result := 'Dakota'
  else if code = 'dan' then
    Result := 'Danish'
  else if code = 'del' then
    Result := 'Delaware'
  else if code = 'deu' then
    Result := 'German'
  else if code = 'din' then
    Result := 'Dinka'
  else if code = 'div' then
    Result := 'Divehi'
  else if code = 'doi' then
    Result := 'Dogri'
  else if code = 'dra' then
    Result := 'Dravidian (Other)'
  else if code = 'dua' then
    Result := 'Duala'
  else if code = 'dum' then
    Result := 'Dutch, Middle (ca. 1050-1350)'
  else if code = 'dut' then
    Result := 'Dutch'
  else if code = 'dyu' then
    Result := 'Dyula'
  else if code = 'dzo' then
    Result := 'Dzongkha'
  else if code = 'efi' then
    Result := 'Efik'
  else if code = 'egy' then
    Result := 'Egyptian (Ancient)'
  else if code = 'eka' then
    Result := 'Ekajuk'
  else if code = 'ell' then
    Result := 'Greek, Modern (1453-)'
  else if code = 'elx' then
    Result := 'Elamite'
  else if code = 'eng' then
    Result := 'English'
  else if code = 'enm' then
    Result := 'English, Middle (ca. 1100-1500)'
  else if code = 'epo' then
    Result := 'Esperanto'
  else if code = 'esk' then
    Result := 'Eskimo (Other)'
  else if code = 'esl' then
    Result := 'Spanish'
  else if code = 'est' then
    Result := 'Estonian'
  else if code = 'eus' then
    Result := 'Basque'
  else if code = 'ewe' then
    Result := 'Ewe'
  else if code = 'ewo' then
    Result := 'Ewondo'
  else if code = 'fan' then
    Result := 'Fang'
  else if code = 'fao' then
    Result := 'Faroese'
  else if code = 'fas' then
    Result := 'Persian'
  else if code = 'fat' then
    Result := 'Fanti'
  else if code = 'fij' then
    Result := 'Fijian'
  else if code = 'fin' then
    Result := 'Finnish'
  else if code = 'fiu' then
    Result := 'Finno-Ugrian (Other)'
  else if code = 'fon' then
    Result := 'Fon'
  else if code = 'fra' then
    Result := 'French'
  else if code = 'fre' then
    Result := 'French'
  else if code = 'frm' then
    Result := 'French, Middle (ca. 1400-1600)'
  else if code = 'fro' then
    Result := 'French, Old (842- ca. 1400)'
  else if code = 'fry' then
    Result := 'Frisian'
  else if code = 'ful' then
    Result := 'Fulah'
  else if code = 'gaa' then
    Result := 'Ga'
  else if code = 'gae' then
    Result := 'Gaelic (Scots)'
  else if code = 'gai' then
    Result := 'Irish'
  else if code = 'gay' then
    Result := 'Gayo'
  else if code = 'gdh' then
    Result := 'Gaelic (Scots)'
  else if code = 'gem' then
    Result := 'Germanic (Other)'
  else if code = 'geo' then
    Result := 'Georgian'
  else if code = 'ger' then
    Result := 'German'
  else if code = 'gez' then
    Result := 'Geez'
  else if code = 'gil' then
    Result := 'Gilbertese'
  else if code = 'glg' then
    Result := 'Gallegan'
  else if code = 'gmh' then
    Result := 'German, Middle High (ca. 1050-1500)'
  else if code = 'goh' then
    Result := 'German, Old High (ca. 750-1050)'
  else if code = 'gon' then
    Result := 'Gondi'
  else if code = 'got' then
    Result := 'Gothic'
  else if code = 'grb' then
    Result := 'Grebo'
  else if code = 'grc' then
    Result := 'Greek, Ancient (to 1453)'
  else if code = 'gre' then
    Result := 'Greek, Modern (1453-)'
  else if code = 'grn' then
    Result := 'Guarani'
  else if code = 'guj' then
    Result := 'Gujarati'
  else if code = 'hai' then
    Result := 'Haida'
  else if code = 'hau' then
    Result := 'Hausa'
  else if code = 'haw' then
    Result := 'Hawaiian'
  else if code = 'heb' then
    Result := 'Hebrew'
  else if code = 'her' then
    Result := 'Herero'
  else if code = 'hil' then
    Result := 'Hiligaynon'
  else if code = 'him' then
    Result := 'Himachali'
  else if code = 'hin' then
    Result := 'Hindi'
  else if code = 'hmo' then
    Result := 'Hiri Motu'
  else if code = 'hun' then
    Result := 'Hungarian'
  else if code = 'hup' then
    Result := 'Hupa'
  else if code = 'hye' then
    Result := 'Armenian'
  else if code = 'iba' then
    Result := 'Iban'
  else if code = 'ibo' then
    Result := 'Igbo'
  else if code = 'ice' then
    Result := 'Icelandic'
  else if code = 'ijo' then
    Result := 'Ijo'
  else if code = 'iku' then
    Result := 'Inuktitut'
  else if code = 'ilo' then
    Result := 'Iloko'
  else if code = 'ina' then
    Result := 'Interlingua (International Auxiliary language Association)'
  else if code = 'Inc' then
    Result := 'Indic (Other)'
  else if code = 'ind' then
    Result := 'Indonesian'
  else if code = 'ine' then
    Result := 'Indo-European (Other)'
  else if code = 'ine' then
    Result := 'Interlingue'
  else if code = 'ipk' then
    Result := 'Inupiak'
  else if code = 'ira' then
    Result := 'Iranian (Other)'
  else if code = 'iri' then
    Result := 'Irish'
  else if code = 'iro' then
    Result := 'Iroquoian uages'
  else if code = 'isl' then
    Result := 'Icelandic'
  else if code = 'ita' then
    Result := 'Italian'
  else if code = 'jav' then
    Result := 'Javanese'
  else if code = 'jaw' then
    Result := 'Javanese'
  else if code = 'jpn' then
    Result := 'Japanese'
  else if code = 'jpr' then
    Result := 'Judeo-Persian'
  else if code = 'jrb' then
    Result := 'Judeo-Arabic'
  else if code = 'kaa' then
    Result := 'Kara-Kalpak'
  else if code = 'kab' then
    Result := 'Kabyle'
  else if code = 'kac' then
    Result := 'Kachin'
  else if code = 'kal' then
    Result := 'Greenlandic'
  else if code = 'kam' then
    Result := 'Kamba'
  else if code = 'kan' then
    Result := 'Kannada'
  else if code = 'kar' then
    Result := 'Karen'
  else if code = 'kas' then
    Result := 'Kashmiri'
  else if code = 'kat' then
    Result := 'Georgian'
  else if code = 'kau' then
    Result := 'Kanuri'
  else if code = 'kaw' then
    Result := 'Kawi'
  else if code = 'kaz' then
    Result := 'Kazakh'
  else if code = 'kha' then
    Result := 'Khasi'
  else if code = 'khi' then
    Result := 'Khoisan (Other)'
  else if code = 'khm' then
    Result := 'Khmer'
  else if code = 'kho' then
    Result := 'Khotanese'
  else if code = 'kik' then
    Result := 'Kikuyu'
  else if code = 'kin' then
    Result := 'Kinyarwanda'
  else if code = 'kir' then
    Result := 'Kirghiz'
  else if code = 'kok' then
    Result := 'Konkani'
  else if code = 'kom' then
    Result := 'Komi'
  else if code = 'kon' then
    Result := 'Kongo'
  else if code = 'kor' then
    Result := 'Korean'
  else if code = 'kpe' then
    Result := 'Kpelle'
  else if code = 'kro' then
    Result := 'Kru'
  else if code = 'kru' then
    Result := 'Kurukh'
  else if code = 'kua' then
    Result := 'Kuanyama'
  else if code = 'kum' then
    Result := 'Kumyk'
  else if code = 'kur' then
    Result := 'Kurdish'
  else if code = 'kus' then
    Result := 'Kusaie'
  else if code = 'kut' then
    Result := 'Kutenai'
  else if code = 'lad' then
    Result := 'Ladino'
  else if code = 'lah' then
    Result := 'Lahnda'
  else if code = 'lam' then
    Result := 'Lamba'
  else if code = 'lao' then
    Result := 'Lao'
  else if code = 'lat' then
    Result := 'Latin'
  else if code = 'lav' then
    Result := 'Latvian'
  else if code = 'lez' then
    Result := 'Lezghian'
  else if code = 'lin' then
    Result := 'Lingala'
  else if code = 'lit' then
    Result := 'Lithuanian'
  else if code = 'lol' then
    Result := 'Mongo'
  else if code = 'loz' then
    Result := 'Lozi'
  else if code = 'ltz' then
    Result := 'Letzeburgesch'
  else if code = 'lub' then
    Result := 'Luba-Katanga'
  else if code = 'lug' then
    Result := 'Ganda'
  else if code = 'lui' then
    Result := 'Luiseno'
  else if code = 'lun' then
    Result := 'Lunda'
  else if code = 'luo' then
    Result := 'Luo (Kenya and Tanzania)'
  else if code = 'mac' then
    Result := 'Macedonian'
  else if code = 'mad' then
    Result := 'Madurese'
  else if code = 'mag' then
    Result := 'Magahi'
  else if code = 'mah' then
    Result := 'Marshall'
  else if code = 'mai' then
    Result := 'Maithili'
  else if code = 'mak' then
    Result := 'Macedonian'
  else if code = 'mak' then
    Result := 'Makasar'
  else if code = 'mal' then
    Result := 'Malayalam'
  else if code = 'man' then
    Result := 'Mandingo'
  else if code = 'mao' then
    Result := 'Maori'
  else if code = 'map' then
    Result := 'Austronesian (Other)'
  else if code = 'mar' then
    Result := 'Marathi'
  else if code = 'mas' then
    Result := 'Masai'
  else if code = 'max' then
    Result := 'Manx'
  else if code = 'may' then
    Result := 'Malay'
  else if code = 'men' then
    Result := 'Mende'
  else if code = 'mga' then
    Result := 'Irish, Middle (900 - 1200)'
  else if code = 'mic' then
    Result := 'Micmac'
  else if code = 'min' then
    Result := 'Minangkabau'
  else if code = 'mis' then
    Result := 'Miscellaneous (Other)'
  else if code = 'mkh' then
    Result := 'Mon-Kmer (Other)'
  else if code = 'mlg' then
    Result := 'Malagasy'
  else if code = 'mlt' then
    Result := 'Maltese'
  else if code = 'mni' then
    Result := 'Manipuri'
  else if code = 'mno' then
    Result := 'Manobo Languages'
  else if code = 'moh' then
    Result := 'Mohawk'
  else if code = 'mol' then
    Result := 'Moldavian'
  else if code = 'mon' then
    Result := 'Mongolian'
  else if code = 'mos' then
    Result := 'Mossi'
  else if code = 'mri' then
    Result := 'Maori'
  else if code = 'msa' then
    Result := 'Malay'
  else if code = 'mul' then
    Result := 'Multiple Languages'
  else if code = 'mun' then
    Result := 'Munda Languages'
  else if code = 'mus' then
    Result := 'Creek'
  else if code = 'mwr' then
    Result := 'Marwari'
  else if code = 'mya' then
    Result := 'Burmese'
  else if code = 'myn' then
    Result := 'Mayan Languages'
  else if code = 'nah' then
    Result := 'Aztec'
  else if code = 'nai' then
    Result := 'North American Indian (Other)'
  else if code = 'nau' then
    Result := 'Nauru'
  else if code = 'nav' then
    Result := 'Navajo'
  else if code = 'nbl' then
    Result := 'Ndebele, South'
  else if code = 'nde' then
    Result := 'Ndebele, North'
  else if code = 'ndo' then
    Result := 'Ndongo'
  else if code = 'nep' then
    Result := 'Nepali'
  else if code = 'new' then
    Result := 'Newari'
  else if code = 'nic' then
    Result := 'Niger-Kordofanian (Other)'
  else if code = 'niu' then
    Result := 'Niuean'
  else if code = 'nla' then
    Result := 'Dutch'
  else if code = 'nno' then
    Result := 'Norwegian (Nynorsk)'
  else if code = 'non' then
    Result := 'Norse, Old'
  else if code = 'nor' then
    Result := 'Norwegian'
  else if code = 'nso' then
    Result := 'Sotho, Northern'
  else if code = 'nub' then
    Result := 'Nubian Languages'
  else if code = 'nya' then
    Result := 'Nyanja'
  else if code = 'nym' then
    Result := 'Nyamwezi'
  else if code = 'nyn' then
    Result := 'Nyankole'
  else if code = 'nyo' then
    Result := 'Nyoro'
  else if code = 'nzi' then
    Result := 'Nzima'
  else if code = 'oci' then
    Result := 'Langue d''Oc (post 1500)'
  else if code = 'oji' then
    Result := 'Ojibwa'
  else if code = 'ori' then
    Result := 'Oriya'
  else if code = 'orm' then
    Result := 'Oromo'
  else if code = 'osa' then
    Result := 'Osage'
  else if code = 'oss' then
    Result := 'Ossetic'
  else if code = 'ota' then
    Result := 'Turkish, Ottoman (1500 - 1928)'
  else if code = 'oto' then
    Result := 'Otomian Languages'
  else if code = 'paa' then
    Result := 'Papuan-Australian (Other)'
  else if code = 'pag' then
    Result := 'Pangasinan'
  else if code = 'pal' then
    Result := 'Pahlavi'
  else if code = 'pam' then
    Result := 'Pampanga'
  else if code = 'pan' then
    Result := 'Panjabi'
  else if code = 'pap' then
    Result := 'Papiamento'
  else if code = 'pau' then
    Result := 'Palauan'
  else if code = 'peo' then
    Result := 'Persian, Old (ca 600 - 400 B.C.)'
  else if code = 'per' then
    Result := 'Persian'
  else if code = 'phn' then
    Result := 'Phoenician'
  else if code = 'pli' then
    Result := 'Pali'
  else if code = 'pol' then
    Result := 'Polish'
  else if code = 'pon' then
    Result := 'Ponape'
  else if code = 'por' then
    Result := 'Portuguese'
  else if code = 'pra' then
    Result := 'Prakrit uages'
  else if code = 'pro' then
    Result := 'Provencal, Old (to 1500)'
  else if code = 'pus' then
    Result := 'Pushto'
  else if code = 'que' then
    Result := 'Quechua'
  else if code = 'raj' then
    Result := 'Rajasthani'
  else if code = 'rar' then
    Result := 'Rarotongan'
  else if code = 'roa' then
    Result := 'Romance (Other)'
  else if code = 'roh' then
    Result := 'Rhaeto-Romance'
  else if code = 'rom' then
    Result := 'Romany'
  else if code = 'ron' then
    Result := 'Romanian'
  else if code = 'rum' then
    Result := 'Romanian'
  else if code = 'run' then
    Result := 'Rundi'
  else if code = 'rus' then
    Result := 'Russian'
  else if code = 'sad' then
    Result := 'Sandawe'
  else if code = 'sag' then
    Result := 'Sango'
  else if code = 'sah' then
    Result := 'Yakut'
  else if code = 'sai' then
    Result := 'South American Indian (Other)'
  else if code = 'sal' then
    Result := 'Salishan Languages'
  else if code = 'sam' then
    Result := 'Samaritan Aramaic'
  else if code = 'san' then
    Result := 'Sanskrit'
  else if code = 'sco' then
    Result := 'Scots'
  else if code = 'scr' then
    Result := 'Serbo-Croatian'
  else if code = 'sel' then
    Result := 'Selkup'
  else if code = 'sem' then
    Result := 'Semitic (Other)'
  else if code = 'sga' then
    Result := 'Irish, Old (to 900)'
  else if code = 'shn' then
    Result := 'Shan'
  else if code = 'sid' then
    Result := 'Sidamo'
  else if code = 'sin' then
    Result := 'Singhalese'
  else if code = 'sio' then
    Result := 'Siouan Languages'
  else if code = 'sit' then
    Result := 'Sino-Tibetan (Other)'
  else if code = 'sla' then
    Result := 'Slavic (Other)'
  else if code = 'slk' then
    Result := 'Slovak'
  else if code = 'slo' then
    Result := 'Slovak'
  else if code = 'slv' then
    Result := 'Slovenian'
  else if code = 'smi' then
    Result := 'Sami Languages'
  else if code = 'smo' then
    Result := 'Samoan'
  else if code = 'sna' then
    Result := 'Shona'
  else if code = 'snd' then
    Result := 'Sindhi'
  else if code = 'sog' then
    Result := 'Sogdian'
  else if code = 'som' then
    Result := 'Somali'
  else if code = 'son' then
    Result := 'Songhai'
  else if code = 'sot' then
    Result := 'Sotho, Southern'
  else if code = 'spa' then
    Result := 'Spanish'
  else if code = 'sqi' then
    Result := 'Albanian'
  else if code = 'srd' then
    Result := 'Sardinian'
  else if code = 'srr' then
    Result := 'Serer'
  else if code = 'ssa' then
    Result := 'Nilo-Saharan (Other)'
  else if code = 'ssw' then
    Result := 'Siswant'
  else if code = 'ssw' then
    Result := 'Swazi'
  else if code = 'suk' then
    Result := 'Sukuma'
  else if code = 'sun' then
    Result := 'Sudanese'
  else if code = 'sus' then
    Result := 'Susu'
  else if code = 'sux' then
    Result := 'Sumerian'
  else if code = 'sve' then
    Result := 'Swedish'
  else if code = 'swa' then
    Result := 'Swahili'
  else if code = 'swe' then
    Result := 'Swedish'
  else if code = 'syr' then
    Result := 'Syriac'
  else if code = 'tah' then
    Result := 'Tahitian'
  else if code = 'tam' then
    Result := 'Tamil'
  else if code = 'tat' then
    Result := 'Tatar'
  else if code = 'tel' then
    Result := 'Telugu'
  else if code = 'tem' then
    Result := 'Timne'
  else if code = 'ter' then
    Result := 'Tereno'
  else if code = 'tgk' then
    Result := 'Tajik'
  else if code = 'tgl' then
    Result := 'Tagalog'
  else if code = 'tha' then
    Result := 'Thai'
  else if code = 'tib' then
    Result := 'Tibetan'
  else if code = 'tig' then
    Result := 'Tigre'
  else if code = 'tir' then
    Result := 'Tigrinya'
  else if code = 'tiv' then
    Result := 'Tivi'
  else if code = 'tli' then
    Result := 'Tlingit'
  else if code = 'tmh' then
    Result := 'Tamashek'
  else if code = 'tog' then
    Result := 'Tonga (Nyasa)'
  else if code = 'ton' then
    Result := 'Tonga (Tonga Islands)'
  else if code = 'tru' then
    Result := 'Truk'
  else if code = 'tsi' then
    Result := 'Tsimshian'
  else if code = 'tsn' then
    Result := 'Tswana'
  else if code = 'tso' then
    Result := 'Tsonga'
  else if code = 'tuk' then
    Result := 'Turkmen'
  else if code = 'tum' then
    Result := 'Tumbuka'
  else if code = 'tur' then
    Result := 'Turkish'
  else if code = 'tut' then
    Result := 'Altaic (Other)'
  else if code = 'twi' then
    Result := 'Twi'
  else if code = 'tyv' then
    Result := 'Tuvinian'
  else if code = 'uga' then
    Result := 'Ugaritic'
  else if code = 'uig' then
    Result := 'Uighur'
  else if code = 'ukr' then
    Result := 'Ukrainian'
  else if code = 'umb' then
    Result := 'Umbundu'
  else if code = 'und' then
    Result := 'Undetermined'
  else if code = 'urd' then
    Result := 'Urdu'
  else if code = 'uzb' then
    Result := 'Uzbek'
  else if code = 'vai' then
    Result := 'Vai'
  else if code = 'ven' then
    Result := 'Venda'
  else if code = 'vie' then
    Result := 'Vietnamese'
  else if code = 'vol' then
    Result := 'Volapük'
  else if code = 'vot' then
    Result := 'Votic'
  else if code = 'wak' then
    Result := 'Wakashan Languages'
  else if code = 'wal' then
    Result := 'Walamo'
  else if code = 'war' then
    Result := 'Waray'
  else if code = 'was' then
    Result := 'Washo'
  else if code = 'wel' then
    Result := 'Welsh'
  else if code = 'wen' then
    Result := 'Sorbian Languages'
  else if code = 'wol' then
    Result := 'Wolof'
  else if code = 'xho' then
    Result := 'Xhosa'
  else if code = 'yao' then
    Result := 'Yao'
  else if code = 'yap' then
    Result := 'Yap'
  else if code = 'yid' then
    Result := 'Yiddish'
  else if code = 'yor' then
    Result := 'Yoruba'
  else if code = 'zap' then
    Result := 'Zapotec'
  else if code = 'zen' then
    Result := 'Zenaga'
  else if code = 'zha' then
    Result := 'Zhuang'
  else if code = 'zho' then
    Result := 'Chinese'
  else if code = 'zul' then
    Result := 'Zulu'
  else if code = 'zun' then
    Result := 'Zuni'
  else
    Result := '';
end;

{**************************************************}

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

{**************************************************}

procedure TJvId3v2.LoadFromStream(stream: TStream);
var
  header: TId3v2Header;
  frame: TId3v2Frame;
  tag: TStream;
begin
  FEventsCount := 0;
  ResetProp;

  stream.ReadBuffer(header, 10);
  FPresent := header.Identifier = 'ID3';
  if FPresent then
  begin
    //Version
    FVersion := (header.Version and $00FF);
    FVersion := FVersion + ((header.Version and $FF00) / 256 / 100);

    //Flags
    FUnsynchro := (header.Flags and $80) <> 0;
    FExtended := (header.Flags and $40) <> 0;
    FExperimental := (header.Flags and $20) <> 0;
    // (rom) undocumented assembler. EEK!
    asm
         mov eax,header.Size

         xor ecx,ecx
         mov cl,al
         shl ecx,7
         or cl,ah
         shl ecx,7
         shr eax,16
         or cl,al
         shl ecx,7
         or cl,ah

         mov header.Size,ecx
    end;
    FSize := header.Size;
    FTagSize := FSize;

    tag := TMemoryStream.Create;
    ReadAndUnsynchro(stream, tag, header.Size);

    //Read extended tag
    if FExtended then
    begin

    end;

    while (tag.Size - tag.position > 10) do
    begin
      tag.Read(frame, 10);
      asm
            mov ax,frame.Flags
            mov dh,al
            mov al,ah
            mov ah,dh
            mov frame.Flags,ax

            mov eax,frame.Size
            mov cl,al
            shl ecx,8
            mov cl,ah
            shl ecx,8
            shr eax,16
            mov cl,al
            shl ecx,8
            mov cl,ah
            mov frame.Size,ecx
      end;

      if frame.Id[0] = 'T' then
        ReadText(frame, tag)
      else if frame.Id[0] = 'W' then
        ReadWeb(frame, tag)
      else if frame.Id = 'IPLS' then
        ReadIpls(frame, tag)
      else if frame.Id = 'MCDI' then
      begin
        tag.Read(MusicCDIdentifier, frame.Size);
        MusicCDIdentifierLength := frame.Size;
      end
      else if frame.Id = 'APIC' then
        ReadImg(frame, tag)
      else if frame.Id = 'PCNT' then
        ReadCounter(frame, tag)
      else if frame.Id = 'POPM' then
        ReadPop(frame, tag)
      else if frame.Id = 'OWNE' then
        ReadOwner(frame, tag)
      else if frame.Id = 'ETCO' then
        ReadEvent(frame, tag)
      else
        SkipFrame(frame, tag);

      //XXX
    end;

    tag.Free;
  end;
end;

{**************************************************}

procedure TJvId3v2.ReadAndUnsynchro(var source, dest: TStream; taille: Integer);
var
  i, j, cnt: Integer;
  buf: array[0..499] of Byte;
  buf2: array[0..999] of Byte;
begin
  while taille > 0 do
  begin
    if taille > 500 then
    begin
      cnt := source.Read(buf, 500);
      if cnt < 500 then
        taille := 0
      else
        taille := taille - cnt;
    end
    else
    begin
      cnt := source.Read(buf, taille);
      taille := 0;
    end;
    j := 0;
    i := 0;
    while i < cnt do
    begin
      if buf[i] = $FF then
      begin
        if i + 1 < cnt then
        begin
          if buf[i + 1] = $00 then
          begin
            buf2[j] := $FF;
            Inc(i);
          end
          else
            buf2[j] := $FF;
        end
        else
        begin
          Inc(taille);
          source.Position := source.Position - 1;
        end;
      end
      else
        buf2[j] := buf[i];
      Inc(j);
      Inc(i);
    end;
    dest.Write(buf2, j);
  end;
  dest.Position := 0;
end;

{**************************************************}

procedure TJvId3v2.ReadCounter(frame: TId3v2Frame; source: TStream);
begin
  if frame.Size > 4 then
    FCounter := 0
  else
  begin
    source.Read(FCounter, 4);
    asm
         mov eax,FCounter
         mov cl,al
         shl ecx,8
         mov cl,ah
         shl ecx,8
         shr eax,16
         mov cl,al
         shl ecx,8
         mov cl,ah
         mov ebx,offset FCounter
         mov [ebx],ecx
    end;
  end;
end;

{**************************************************}

procedure TJvId3v2.ReadEvent(frame: TId3v2Frame; source: TStream);
var
  b: Byte;
  i: Integer;
begin
  source.Read(b, 1);
  Dec(frame.Size);

  FEventsCount := 0;
  FEventsTiming := b;

  while frame.Size > 0 do
  begin
    source.Read(b, 1);
    Dec(frame.Size, 5);
    case b of
      $00: FEvents[FEventsCount].EventType := etPADDING;
      $01: FEvents[FEventsCount].EventType := etEND_OF_INITIAL_SILENCE;
      $02: FEvents[FEventsCount].EventType := etINTRO_START;
      $03: FEvents[FEventsCount].EventType := etMAINPART_START;
      $04: FEvents[FEventsCount].EventType := etOUTRO_START;
      $05: FEvents[FEventsCount].EventType := etOUTRO_END;
      $06: FEvents[FEventsCount].EventType := etVERSE_START;
      $07: FEvents[FEventsCount].EventType := etREFRAIN_START;
      $08: FEvents[FEventsCount].EventType := etINTERLUDE_START;
      $09: FEvents[FEventsCount].EventType := etTHEME_START;
      $0A: FEvents[FEventsCount].EventType := etVARIATION_START;
      $0B: FEvents[FEventsCount].EventType := etKEY_CHANGE;
      $0C: FEvents[FEventsCount].EventType := eTTime_CHANGE;
      $0D: FEvents[FEventsCount].EventType := etUNWANTED_NOISE;
      $0E: FEvents[FEventsCount].EventType := etSUSTAINED_NOISE;
      $0F: FEvents[FEventsCount].EventType := etSUSTAINED_NOISE_END;
      $10: FEvents[FEventsCount].EventType := etINTRO_END;
      $11: FEvents[FEventsCount].EventType := etMAINPART_END;
      $12: FEvents[FEventsCount].EventType := etVERSE_END;
      $13: FEvents[FEventsCount].EventType := etREFRAIN_END;
      $14: FEvents[FEventsCount].EventType := etTHEME_END;
      $FD: FEvents[FEventsCount].EventType := etAUDIO_END;
      $FE: FEvents[FEventsCount].EventType := etFILE_END;
    end;
    source.Read(i, 4);
    asm
          mov eax,i
          mov cl,al
          shl ecx,8
          mov cl,ah
          shl ecx,8
          shr eax,16
          mov cl,al
          shl ecx,8
          mov cl,ah
          mov ebx,offset i
          mov [ebx],ecx
    end;
    FEvents[FEventsCount].TimeStamp := i;
    Inc(FEventsCount);
  end;
end;

{**************************************************}

procedure TJvId3v2.ReadImg(frame: TId3v2Frame; source: TStream);
var
  b, c, encoding: Byte;
  buf: array[0..64000] of Char;
  mime, description: string;
  ptype: Byte;
  picture: TMemoryStream;
  sizeleft: Integer;
  TempPath: array[0..250] of Char;
  TempPat: string;
  FilePath: string;
begin
  source.Read(encoding, 1);
  sizeleft := frame.Size - 1;

  //Read mime type
  mime := '';
  source.Read(b, 1);
  Dec(sizeleft);
  while b <> $00 do
  begin
    mime := mime + Char(b);
    source.Read(b, 1);
    Dec(sizeleft);
  end;

  //Read picture type
  source.Read(ptype, 1);
  Dec(sizeleft);

  //Read description
  if encoding = $00 then
  begin
    //ISO
    description := '';
    source.Read(b, 1);
    Dec(sizeleft);
    while b <> $00 do
    begin
      description := description + Char(b);
      source.Read(b, 1);
      Dec(sizeleft);
    end;
  end
  else
  begin
    //Unicode
    description := '';
    source.Read(b, 1);
    source.Read(c, 1);
    if c <> $00 then
    begin
      source.Read(c, 1);
      Dec(sizeleft, 3);
      while b <> $00 do
      begin
        description := description + Char(b);
        source.Read(b, 1);
        source.Read(c, 1);
        Dec(sizeleft, 2);
      end;
    end
    else
      Dec(sizeleft, 2);
  end;

  //Picture data
  picture := TMemoryStream.Create;

  while (sizeleft > 64000) do
  begin
    source.Read(buf, 64000);
    Dec(sizeleft, 64000);
    picture.Write(buf, 64000);
  end;
  source.Read(buf, sizeleft);
  picture.Write(buf, sizeleft);

  GetTempPath(250, TempPath);
  TempPat := TempPath;
  if TempPat[Length(TempPat)] <> '\' then
    TempPat := TempPat + '\';

  FilePath := temppat + 'tmp.';
  mime := Copy(mime, Pos('/', mime) + 1, Length(mime));
  if UpperCase(mime) = 'JPEG' then
    filePath := FilePath + 'jpg'
  else
    FilePath := FilePath + mime;
  Picture.SaveToFile(FilePath);

  case ptype of
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

  picture.Free;
end;

{**************************************************}

procedure TJvId3v2.ReadIpls(frame: TId3v2Frame; source: TStream);
var
  encoding: Byte;
  buf: array[0..64000] of Char;
  st, st2: string;
  i: Integer;
  second: Boolean;
begin
  source.Read(encoding, 1);
  source.Read(buf, frame.Size - 1);
  if encoding = 0 then
  begin
    //Iso
    st := '';
    st2 := '';
    second := False;
    for i := 0 to frame.Size - 2 do
    begin
      if buf[i] = #0 then
      begin
        if second then
        begin
          FInvolved.AddItem(st, st2);
          st := '';
          st2 := '';
        end;
        second := not second;
      end
      else if second then
        st2 := st2 + buf[i]
      else
        st := st + buf[i];
    end;
    if (st <> '') or (st2 <> '') then
      FInvolved.AddItem(st, st2);
  end
  else
  begin
    //unicode
    st := '';
    st2 := '';
    second := False;
    i := 2;
    while i < frame.Size - 1 do
    begin
      if buf[i] = #0 then
      begin
        if second then
        begin
          FInvolved.AddItem(st, st2);
          st := '';
          st2 := '';
        end;
        second := not second;
        Inc(i, 2);
      end
      else if second then
        st2 := st2 + buf[i]
      else
        st := st + buf[i];
      Inc(i, 2);
    end;
    if (st <> '') or (st2 <> '') then
      FInvolved.AddItem(st, st2);
  end;
end;

{**************************************************}

procedure TJvId3v2.ReadOwner(frame: TId3v2Frame; source: TStream);
var
  encoding: Byte;
  st: string;
  b: Byte;
  y, d, m: Integer;
begin
  source.Read(encoding, 1);
  Dec(frame.Size);
  st := '';

  //Read price payed
  source.Read(b, 1);
  while b <> $00 do
  begin
    st := st + Char(b);
    source.Read(b, 1);
    Dec(frame.Size);
  end;
  FOwner.Price := st;

  //Read date of purchased
  source.Read(b, 1);
  y := b - Ord('0');
  y := y * 10;
  source.Read(b, 1);
  y := y + (b - Ord('0'));
  y := y * 10;
  source.Read(b, 1);
  y := y + (b - Ord('0'));
  y := y * 10;
  source.Read(b, 1);
  y := y + (b - Ord('0'));

  source.Read(b, 1);
  m := b - Ord('0');
  m := m * 10;
  source.Read(b, 1);
  m := m + (b - Ord('0'));

  source.Read(b, 1);
  d := b - Ord('0');
  d := d * 10;
  source.Read(b, 1);
  d := d + (b - Ord('0'));

  FOwner.DatePurchased := EncodeDate(y, m, d);
  Dec(Frame.Size, 8);

  //Read seller
  st := '';
  if encoding = $00 then
  begin
    while Frame.Size > 0 do
    begin
      source.Read(b, 1);
      st := st + Char(b);
      Dec(frame.Size);
    end;
  end
  else
  begin
    //Unicode
    if Frame.Size > 2 then
    begin
      source.Read(b, 1);
      source.Read(b, 1);
      Dec(frame.Size, 2);
      while Frame.Size > 0 do
      begin
        source.Read(b, 1);
        st := st + Char(b);
        source.Read(b, 1);
        Dec(frame.Size, 2);
      end;
    end;
  end;
  FOwner.Seller := st;
end;

{**************************************************}

procedure TJvId3v2.ReadPop(frame: TId3v2Frame; source: TStream);
var
  st: string;
  b: Byte;
  i: Integer;
begin
  source.Read(b, 1);
  st := '';
  while b <> $00 do
  begin
    st := st + Char(b);
    source.Read(b, 1);
  end;
  FPopula.UserEmail := st;

  source.Read(b, 1);
  FPopula.Rating := b;

  source.Read(i, 4);
  asm
      mov eax,i
      mov cl,al
      shl ecx,8
      mov cl,ah
      shl ecx,8
      shr eax,16
      mov cl,al
      shl ecx,8
      mov cl,ah
      mov ebx,offset i
      mov [ebx],ecx
  end;
  FPopula.Counter := i;
end;

{**************************************************}

procedure TJvId3v2.ReadText(frame: TId3v2Frame; source: TStream);
var
  encoding: Byte;
  buf: array[0..64000] of Char;
  stw, stw2: WideString;
  st, st2: string;
  i: Integer;
  second: Boolean;
begin
  if frame.Id = 'TXXX' then
  begin
    source.Read(encoding, 1);
    second := False;
    if encoding = 1 then
    begin
      //Unicode
      source.Read(buf, frame.Size - 1);
      stw := '';
      stw2 := '';
      i := 2;
      while i < frame.Size - 2 do
      begin
        if second then
          stw2 := stw2 + buf[i]
        else if buf[i] = #0 then
        begin
          second := True;
          Inc(i, 2);
        end
        else
          stw := stw + buf[i];
        Inc(i, 2);
      end;
      st := stw;
      st2 := stw2;
    end
    else
    begin
      source.Read(buf, frame.Size - 1);
      st := '';
      st2 := '';
      for i := 0 to frame.Size - 2 do
      begin
        if second then
          st2 := st2 + buf[i]
        else if buf[i] = #0 then
          second := True
        else
          st := st + buf[i];
      end;
    end;
    FUDText.AddItem(st, st2);
  end
  else
  begin
    source.Read(encoding, 1);
    if encoding = 0 then
    begin
      //ISO-8859-1
      source.Read(buf, frame.Size - 1);
      st := '';
      for i := 0 to frame.Size - 2 do
        st := st + buf[i];
    end
    else
    begin
      //Unicode
   // (rom) better do real Unicode conversion here
      source.Read(buf, frame.Size - 1);
      stw := '';
      i := 2;
      while i < frame.Size - 2 do
      begin
        stw := stw + buf[i];
        Inc(i, 2);
      end;
      st := stw;
    end;

    if frame.Id = 'TALB' then
      FId3Text.Album := st
    else if frame.id = 'TBPM' then
      FID3Text.BPM := StrToInt(st)
    else if frame.id = 'TCOM' then
    begin
      while Pos('/', st) <> 0 do
      begin
        FId3Text.Composer.Add(Copy(st, 1, Pos('/', st) - 1));
        st := Copy(st, Pos('/', st) + 1, Length(st));
      end;
      FId3Text.Composer.Add(st);
    end
    else if frame.id = 'TCON' then
      FId3Text.Content := st
    else if frame.id = 'TCOP' then
      FId3Text.Copyright := st
    else if frame.id = 'TDAT' then
      FId3Text.Date := st
    else if frame.id = 'TDLY' then
      FId3Text.PlaylistDelay := StrToInt(st)
    else if frame.id = 'TENC' then
      FId3Text.EncodedBy := st
    else if frame.id = 'TEXT' then
    begin
      while Pos('/', st) <> 0 do
      begin
        FId3Text.Lyricists.Add(Copy(st, 1, Pos('/', st) - 1));
        st := Copy(st, Pos('/', st) + 1, Length(st));
      end;
      FId3Text.Lyricists.Add(st);
    end
    else if frame.id = 'TFLT' then
    begin
      if st = 'MPG' then
        FId3Text.FileType := ftMPG
      else if st = 'MPG/1' then
        FId3Text.FileType := ftMPG1
      else if st = 'MPG/2' then
        FId3Text.FileType := ftMPG2
      else if st = 'MPG/3' then
        FId3Text.FileType := ftMPG3
      else if st = 'MPG/2.5' then
        FId3Text.FileType := ftMPG2_5
      else if st = 'MPG/AAC' then
        FId3Text.FileType := ftMPG_AAC
      else if st = 'VQF' then
        FId3Text.FileType := ftVQF
      else if st = 'PCM' then
        FId3Text.FileType := ftPCM
      else
        FId3Text.FileType := ftUNKNOWN;
    end
    else if frame.id = 'TIME' then
      FId3Text.Time := st
    else if frame.id = 'TIT1' then
      FId3Text.ContentGroup := st
    else if frame.id = 'TIT2' then
      FId3Text.ContentDescription := st
    else if frame.id = 'TIT3' then
      FId3Text.SubTitle := st
    else if frame.id = 'TKEY' then
      FId3Text.InitialKey := st
    else if frame.id = 'TLAN' then
    begin
      while Length(st) > 3 do
      begin
        st2 := Copy(st, 1, 3);
        Fid3Text.Languages.Add(Iso639ToName(st2));
        st := Copy(st, 4, Length(st));
      end;
      Fid3Text.Languages.Add(Iso639ToName(st));
    end
    else if frame.id = 'TLEN' then
      FId3Text.Length := st
    else if frame.id = 'TMED' then
    begin
      if st = 'DIG' then
        FId3Text.MediaType := mtDIG
      else if st = 'DIG/A' then
        FId3Text.MediaType := mtDIG_ANALOG_TRANSFER
      else if st = 'ANA' then
        FId3Text.MediaType := mtANA
      else if st = 'ANA/WAC' then
        FId3Text.MediaType := mtANA_WAX_CYLINDER
      else if st = 'ANA/8CA' then
        FId3Text.MediaType := mtANA_8_TRACK_TAPE
      else if st = 'CD' then
        FId3Text.MediaType := mtCD
      else if st = 'CD/A' then
        FId3Text.MediaType := mtCD_ANALOG
      else if st = 'CD/DD' then
        FId3Text.MediaType := mtCD_DDD
      else if st = 'CD/AD' then
        FId3Text.MediaType := mtCD_ADD
      else if st = 'CD/AA' then
        FId3Text.MediaType := mtCD_AAD
      else if st = 'LD' then
        FId3Text.MediaType := mtLASERDISC
      else if st = 'LD/A' then
        FId3Text.MediaType := mtLASERDISC_ANALOG_TRANSFER
      else if st = 'TT' then
        FId3Text.MediaType := mtTURNTABLE
      else if st = 'TT/33' then
        FId3Text.MediaType := mtTURNTABLE_33
      else if st = 'TT/45' then
        FId3Text.MediaType := mtTURNTABLE_45
      else if st = 'TT/71' then
        FId3Text.MediaType := mtTURNTABLE_71
      else if st = 'TT/76' then
        FId3Text.MediaType := mtTURNTABLE_76
      else if st = 'TT/78' then
        FId3Text.MediaType := mtTURNTABLE_78
      else if st = 'TT/80' then
        FId3Text.MediaType := mtTURNTABLE_80
      else if st = 'MD' then
        FId3Text.MediaType := mtMINIDISC
      else if st = 'MD/A' then
        FId3Text.MediaType := mtMINIDISC_ANALOG_TRANSFER
      else if st = 'DAT' then
        FId3Text.MediaType := mtDAT
      else if st = 'DAT/A' then
        FId3Text.MediaType := mtDAT_ANALOG_TRANSFER
      else if st = 'DAT/1' then
        FId3Text.MediaType := mtDAT_48KHZ_16B
      else if st = 'DAT/2' then
        FId3Text.MediaType := mtDAT_32KHZ_16B
      else if st = 'DAT/3' then
        FId3Text.MediaType := mtDAT_32KHZ_12B
      else if st = 'DAT/4' then
        FId3Text.MediaType := mtDAT_32KHZ_12B_4CH
      else if st = 'DAT/5' then
        FId3Text.MediaType := mtDAT_44KHZ_16B
      else if st = 'DAT/6' then
        FId3Text.MediaType := mtDAT_44KHZ_16B_WIDE
      else if st = 'DCC' then
        FId3Text.MediaType := mtDCC
      else if st = 'DCC/A' then
        FId3Text.MediaType := mtDCC_ANALOG_TRANSFER
      else if st = 'DVD' then
        FId3Text.MediaType := mtDVD
      else if st = 'DVD/A' then
        FId3Text.MediaType := mtDVD_ANALOG_TRANSFER
      else if st = 'TV' then
        FId3Text.MediaType := mtTV
      else if st = 'TV/PAL' then
        FId3Text.MediaType := mtTV_PAL
      else if st = 'TV/NTSC' then
        FId3Text.MediaType := mtTV_NTSC
      else if st = 'TV/SECAM' then
        FId3Text.MediaType := mtTV_SECAM
      else if st = 'VID' then
        FId3Text.MediaType := mtVID
      else if st = 'VID/PAL' then
        FId3Text.MediaType := mtVID_PAL
      else if st = 'VID/NTSC' then
        FId3Text.MediaType := mtVID_NTSC
      else if st = 'VID/SECAM' then
        FId3Text.MediaType := mtVID_SECAM
      else if st = 'VID/VHS' then
        FId3Text.MediaType := mtVID_VHS
      else if st = 'VID/SVHS' then
        FId3Text.MediaType := mtVID_SVHS
      else if st = 'VID/BETA' then
        FId3Text.MediaType := mtVID_BETA
      else if st = 'RAD' then
        FId3Text.MediaType := mtRAD
      else if st = 'RAD/FM' then
        FId3Text.MediaType := mtRAD_FM
      else if st = 'RAD/AM' then
        FId3Text.MediaType := mtRAD_AM
      else if st = 'RAD/LW' then
        FId3Text.MediaType := mtRAD_LW
      else if st = 'RAD/MW' then
        FId3Text.MediaType := mtRAD_MW
      else if st = 'TEL' then
        FId3Text.MediaType := mtTEL
      else if st = 'TEL/I' then
        FId3Text.MediaType := mtTEL_ISDN
      else if st = 'MC' then
        FId3Text.MediaType := mtMC
      else if st = 'MC/4' then
        FId3Text.MediaType := mtMC_4
      else if st = 'MC/9' then
        FId3Text.MediaType := mtMC_9
      else if st = 'MC/I' then
        FId3Text.MediaType := mtMC_I
      else if st = 'MC/II' then
        FId3Text.MediaType := mtMC_II
      else if st = 'MC/III' then
        FId3Text.MediaType := mtMC_III
      else if st = 'MC/IV' then
        FId3Text.MediaType := mtMC_IV
      else if st = 'REE' then
        FId3Text.MediaType := mtREE
      else if st = 'REE/9' then
        FId3Text.MediaType := mtREE_9
      else if st = 'REE/19' then
        FId3Text.MediaType := mtREE_19
      else if st = 'REE/38' then
        FId3Text.MediaType := mtREE_38
      else if st = 'REE/76' then
        FId3Text.MediaType := mtREE_76
      else if st = 'REE/I' then
        FId3Text.MediaType := mtREE_I
      else if st = 'REE/II' then
        FId3Text.MediaType := mtREE_II
      else if st = 'REE/III' then
        FId3Text.MediaType := mtREE_III
      else if st = 'REE/IV' then
        FId3Text.MediaType := mtREE_IV
      else
        FId3Text.MediaType := mtUNKNOWN;
    end
    else if frame.id = 'TOAL' then
      FId3Text.OriginalTitle := st
    else if frame.id = 'TOFN' then
      FId3Text.OriginalFileName := st
    else if frame.id = 'TOLY' then
    begin
      while Pos('/', st) <> 0 do
      begin
        FId3Text.OriginalLyricists.Add(Copy(st, 1, Pos('/', st) - 1));
        st := Copy(st, Pos('/', st) + 1, Length(st));
      end;
      FId3Text.OriginalLyricists.Add(st);
    end
    else if frame.id = 'TOPE' then
    begin
      while Pos('/', st) <> 0 do
      begin
        FId3Text.OriginalArtist.Add(Copy(st, 1, Pos('/', st) - 1));
        st := Copy(st, Pos('/', st) + 1, Length(st));
      end;
      FId3Text.OriginalArtist.Add(st);
    end
    else if frame.id = 'TORY' then
      FId3Text.OriginalReleaseYear := st
    else if frame.id = 'TOWN' then
      FId3Text.FileOwner := st
    else if frame.id = 'TPE1' then
    begin
      while Pos('/', st) <> 0 do
      begin
        FId3Text.Performers.Add(Copy(st, 1, Pos('/', st) - 1));
        st := Copy(st, Pos('/', st) + 1, Length(st));
      end;
      FId3Text.Performers.Add(st);
    end
    else if frame.id = 'TPE2' then
      FId3Text.Band := st
    else if frame.id = 'TPE3' then
      FId3Text.Conductor := st
    else if frame.id = 'TPE4' then
      FId3Text.ModifiedBy := st
    else if frame.id = 'TPOS' then
      FId3Text.PartOf := st
    else if frame.id = 'TPUB' then
      FId3Text.Publisher := st
    else if frame.id = 'TRCK' then
      FId3Text.TrackNumber := st
    else if frame.id = 'TRDA' then
      FId3Text.RecordingDate := st
    else if frame.id = 'TRSN' then
      FId3Text.InternetRadioName := st
    else if frame.id = 'TRSO' then
      FId3Text.InternetRadioOwner := st
    else if frame.id = 'TSIZ' then
      FId3Text.Size := StrToInt(st)
    else if frame.id = 'TSRC' then
      FId3Text.ISRC := st
    else if frame.id = 'TSSE' then
      FId3Text.EncodingSoftware := st
    else if frame.id = 'TYER' then
      FId3Text.Year := st;
  end;
end;

{**************************************************}

procedure TJvId3v2.ReadWeb(frame: TId3v2Frame; source: TStream);
var
  buf: array[0..64000] of Char;
  st, st2: string;
  i: Integer;
begin
  source.Read(buf, frame.Size - 1);
  st := '';
  st2 := '';
  if frame.Id = 'WXXX' then
  begin
    if buf[0] = #0 then
    begin
      i := 1;
      while buf[i] <> #0 do
      begin
        st := st + buf[i];
        Inc(i);
      end;
    end
    else
    begin
      //unicode description
      i := 3;
      while buf[i] <> #0 do
      begin
        st := st + buf[i];
        Inc(i, 2);
      end;
    end;
    Inc(i);
    while i < frame.Size do
    begin
      st2 := st2 + buf[i];
      Inc(i);
    end;
    FUDWeb.AddItem(st, st2);
  end
  else
  begin
    for i := 0 to frame.Size - 2 do
      st := st + buf[i];
    if frame.Id = 'WCOM' then
      FWeb.CommercialInfo := st
    else if frame.Id = 'WCOP' then
      FWeb.LegalInfo := st
    else if frame.Id = 'WOAF' then
      FWeb.OfficialAudio := st
    else if frame.Id = 'WOAR' then
      FWeb.OfficialArtist := st
    else if frame.Id = 'WOAS' then
      FWeb.OfficialAudioSource := st
    else if frame.Id = 'WORS' then
      FWeb.InternetRadioStation := st
    else if frame.Id = 'WPAY' then
      FWeb.Payment := st
    else if frame.Id = 'WPUB' then
      FWeb.Publishers := st;
  end;
end;

{**************************************************}

procedure TJvId3v2.ResetProp;
begin
  FVersion := 0.0;
  FUnsynchro := False;
  FExperimental := False;
  FExtended := False;

  FId3Text.ResetFields;
  FUDText.ResetFields;
  FWeb.ResetFields;
  FUDWeb.ResetFields;
  FInvolved.ResetFields;
  FImages.ResetFields;
  FOwner.ResetFields;
  FPopula.ResetFields;

  FCounter := 0;
  FillChar(MusicCDIdentifier, SizeOf(MusicCDIdentifier), 0);
  MusicCDIdentifierLength := 0;
end;

{**************************************************}

procedure TJvId3v2.SetFileName(const Value: TFileName);
begin
  FTagSize := 0;
  FFileName := Value;
  LoadFromFile(Value);
end;

{**************************************************}

procedure TJvId3v2.SkipFrame(frame: TId3v2Frame; var source: TStream);
begin
  source.Position := source.Position + frame.Size;
end;

///////////////////////////////////////////////////////////
// TJvId3Text
///////////////////////////////////////////////////////////

constructor TJvId3Text.Create;
begin
  inherited;
  FComposer := TStringList.Create;
  FLyricist := TStringList.Create;
  FLanguages := TStringList.Create;
  FOLyricists := TStringList.Create;
  FOArtist := TStringList.Create;
  FPerformers := TStringList.Create;
end;

{**************************************************}

destructor TJvId3Text.Destroy;
begin
  FComposer.Free;
  FLyricist.Free;
  FLanguages.Free;
  FOLyricists.Free;
  FOArtist.Free;
  FPerformers.Free;
  inherited;
end;

{**************************************************}

procedure TJvId3Text.ResetFields;
begin
  FBPM := 0;
  FAlbum := '';
  FComposer.Clear;
  FDelay := 0;
  FCopyright := '';
  FEncodedBy := '';
  FDate := '';
  FLyricist.Clear;
  FFileType := ftUNKNOWN;
  FTime := '';
  FInitialKey := '';
  FContent := '';
  FSubTitle := '';
  FContentD := '';
  FLanguages.Clear;
  FLength := '';
  FMediaType := mtUNKNOWN;
  FOriginal := '';
  FFileName := '';
  FOLyricists.Clear;
  FOArtist.Clear;
  FOwner := '';
  FOYear := '';
  FPerformers.Clear;
  FConductor := '';
  FModified := '';
  FBand := '';
  FPart := '';
  FPublisher := '';
  FTrackNumber := '';
  FRecording := '';
  FInternet := '';
  FIOwner := '';
  FSize := 0;
  FISRC := '';
  FEncoding := '';
  FYear := '';
  FContent := '';
end;

///////////////////////////////////////////////////////////
// TJvId3Web
///////////////////////////////////////////////////////////

procedure TJvId3Web.ResetFields;
begin
  FAudio := '';
  FStation := '';
  FPublishers := '';
  FASource := '';
  FPayment := '';
  FLegal := '';
  FCommercial := '';
  FArtist := '';
end;

///////////////////////////////////////////////////////////
// TJvID3UDText
///////////////////////////////////////////////////////////

procedure TJvID3UDText.AddItem(desc, Value: string);
begin
  FStrings.Add(Value);
  FDescriptions.Add(desc);
  SetItem(0);
  FCount := FStrings.Count;
end;

{**************************************************}

constructor TJvID3UDText.Create;
begin
  FStrings := TStringList.Create;
  FDescriptions := TStringList.Create;
end;

{**************************************************}

destructor TJvID3UDText.Destroy;
begin
  FStrings.Free;
  FDescriptions.Free;
  inherited;
end;

{**************************************************}

procedure TJvID3UDText.ResetFields;
begin
  FStrings.Clear;
  FDescriptions.Clear;
end;

{**************************************************}

procedure TJvID3UDText.SetItem(const Value: Integer);
begin
  FItems := Value;
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

///////////////////////////////////////////////////////////
// TJvID3UDUrl
///////////////////////////////////////////////////////////

procedure TJvID3UDUrl.AddItem(desc, Value: string);
begin
  FStrings.Add(Value);
  FDescriptions.Add(desc);
  SetItem(0);
  FCount := FStrings.Count;
end;

{**************************************************}

constructor TJvID3UDUrl.Create;
begin
  FStrings := TStringList.Create;
  FDescriptions := TStringList.Create;
end;

{**************************************************}

destructor TJvID3UDUrl.Destroy;
begin
  FStrings.Free;
  FDescriptions.Free;
  inherited;
end;

{**************************************************}

procedure TJvID3UDUrl.ResetFields;
begin
  FStrings.Clear;
  FDescriptions.Clear;
end;

{**************************************************}

procedure TJvID3UDUrl.SetItem(const Value: Integer);
begin
  FItems := Value;
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

///////////////////////////////////////////////////////////
// TJvIdPictures
///////////////////////////////////////////////////////////

constructor TJvIdPictures.Create;
begin
  FMedia := TPicture.Create;
  FLyricist := TPicture.Create;
  FIcon := TPicture.Create;
  FComposer := TPicture.Create;
  FRLocation := TPicture.Create;
  FOther := TPicture.Create;
  FFish := TPicture.Create;
  FCoverFront := TPicture.Create;
  FBandLogo := TPicture.Create;
  FConductor := TPicture.Create;
  FPublisherLogo := TPicture.Create;
  FDRecording := TPicture.Create;
  FArtist := TPicture.Create;
  FLeaflet := TPicture.Create;
  FOther := TPicture.Create;
  FDPerformance := TPicture.Create;
  FIllustration := TPicture.Create;
  FBand := TPicture.Create;
  FMove := TPicture.Create;
  FCoverBack := TPicture.Create;
  FOtherIcon := TPicture.Create;
  FLeadArtist := TPicture.Create;
end;

{**************************************************}

destructor TJvIdPictures.Destroy;
begin
  FMedia.Free;
  FLyricist.Free;
  FIcon.Free;
  FComposer.Free;
  FRLocation.Free;
  FOther.Free;
  FFish.Free;
  FCoverFront.Free;
  FBandLogo.Free;
  FConductor.Free;
  FPublisherLogo.Free;
  FDRecording.Free;
  FArtist.Free;
  FLeaflet.Free;
  FDPerformance.Free;
  FIllustration.Free;
  FBand.Free;
  FMove.Free;
  FCoverBack.Free;
  FOtherIcon.Free;
  FLeadArtist.Free;
  inherited;
end;

{**************************************************}

procedure TJvIdPictures.SetArtist(const Value: TPicture);
begin
  FArtist.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetBand(const Value: TPicture);
begin
  FBand.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetBandLogo(const Value: TPicture);
begin
  FBandLogo.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetComposer(const Value: TPicture);
begin
  FComposer.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetConductor(const Value: TPicture);
begin
  FConductor.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetCoverBack(const Value: TPicture);
begin
  FCoverBack.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetCoverFront(const Value: TPicture);
begin
  FCoverFront.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetDRecording(const Value: TPicture);
begin
  FDRecording.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetFish(const Value: TPicture);
begin
  FFish.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetIcon(const Value: TPicture);
begin
  FIcon.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetIllustration(const Value: TPicture);
begin
  FIllustration.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetLeadArtist(const Value: TPicture);
begin
  FLeadArtist.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetLeaflet(const Value: TPicture);
begin
  FLeaflet.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetLocation(const Value: TPicture);
begin
  FRLocation.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetLyricist(const Value: TPicture);
begin
  FLyricist.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetMedia(const Value: TPicture);
begin
  FMedia.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetMovie(const Value: TPicture);
begin
  FMove.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetOther(const Value: TPicture);
begin
  FOther.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetOtherIcon(const Value: TPicture);
begin
  FOtherIcon.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetPerformance(const Value: TPicture);
begin
  FDPerformance.Assign(Value);
end;

{**************************************************}

procedure TJvIdPictures.SetPublisherLogo(const Value: TPicture);
begin
  FPublisherLogo.Assign(Value);
end;

///////////////////////////////////////////////////////////
// TJvIdImages
///////////////////////////////////////////////////////////

constructor TJvIdImages.Create;
begin
  FPictures := TJvIdPictures.Create;
  FInfos := TJvIdPicturesDesc.Create;
end;

{**************************************************}

destructor TJvIdImages.Destroy;
begin
  FPictures.Free;
  FInfos.Free;
  inherited;
end;

{**************************************************}

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

///////////////////////////////////////////////////////////
// TJvIdIpl
///////////////////////////////////////////////////////////

procedure TJvIdIpl.AddItem(Job, Person: string);
begin
  FJobs.Add(Job);
  FPersons.Add(person);
  SetItem(0);
  Inc(FCount);
end;

{**************************************************}

constructor TJvIdIpl.Create;
begin
  FJobs := TStringList.Create;
  FPersons := TStringList.Create;
end;

{**************************************************}

destructor TJvIdIpl.Destroy;
begin
  FJobs.Free;
  FPersons.Free;
  inherited;
end;

{**************************************************}

procedure TJvIdIpl.ResetFields;
begin
  FJobs.Clear;
  FPersons.Clear;
end;

{**************************************************}

procedure TJvIdIpl.SetItem(const Value: Integer);
begin
  FItems := Value;
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

///////////////////////////////////////////////////////////
// TJvId3Owner
///////////////////////////////////////////////////////////

procedure TJvId3Owner.ResetFields;
begin
  FSeller := '';
  FPrice := '';
  FDate := EncodeDate(1900, 1, 1);
end;

///////////////////////////////////////////////////////////
// TJvId3Popularimeter
///////////////////////////////////////////////////////////

procedure TJvId3Popularimeter.ResetFields;
begin
  FRating := 0;
  FCounter := 0;
  FEmail := '';
end;

end.
