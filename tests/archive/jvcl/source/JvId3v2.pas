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
  * Can only read tags, not write
  * Doesn't support all tags
  * Need to convert to TCollection to be able to write tags
  * No real unicode support
  * Only support for v2.3
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvId3v2;

{
Not yet coded (really complicated)
 USLT  4.9
 SYLT  4.10
 //COMM  4.11
 RVAD  4.12
 EQUA  4.13
 RVRB  4.14
 GEOB  4.16
 RBUF  4.19
 AENC  4.20
 POSS  4.22
 //USER  4.23
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

const
  cJvID3LowIndex = $00;
  cJvID3HighIndex = $14;

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

  TJvID3Text = class(TPersistent)
  private
    FPlaylistDelay: Integer;
    FSize: Integer;
    FBPM: Integer;
    FContentGroup: string;
    FInitialKey: string;
    FContentDescription: string;
    FInternetRadioOwner: string;
    FTime: string;
    FISRC: string;
    FAlbum: string;
    FCopyright: string;
    FEncodedBy: string;
    FOriginalFileName: string;
    FContent: string;
    FTrackNumber: string;
    FConductor: string;
    FModifiedBy: string;
    FSubTitle: string;
    FDate: string;
    FBand: string;
    FRecordingDate: string;
    FPartOf: string;
    FOriginalReleaseYear: string;
    FPublisher: string;
    FInternetRadioName: string;
    FOriginalTitle: string;
    FLength: string;
    FYear: string;
    FFileOwner: string;
    FEncodingSoftware: string;
    FFileType: TJvID3FileType;
    FMediaType: TJvID3MediaType;
    FLyricists: TStrings;
    FOriginalArtists: TStrings;
    FComposer: TStrings;
    FOriginalLyricists: TStrings;
    FPerformers: TStrings;
    FLanguages: TStrings;
    procedure ResetFields;
    procedure DummyProcedureInt(const Value: Integer);
    procedure DummyProcedureStr(const Value: string);
    procedure DummyProcedureFT(const Value: TJvID3FileType);
    procedure DummyProcedureMT(const Value: TJvID3MediaType);
  public
    constructor Create;
    destructor Destroy; override;
  published
    { Do not store dummies }
    property Album: string read FAlbum write DummyProcedureStr stored False;
    property BPM: Integer read FBPM write DummyProcedureInt stored False;
    property Composer: TStrings read FComposer stored False;
    property Copyright: string read FCopyright write DummyProcedureStr stored False;
    property Date: string read FDate write DummyProcedureStr stored False;
    property PlaylistDelay: Integer read FPlaylistDelay write DummyProcedureInt stored False;
    property EncodedBy: string read FEncodedBy write DummyProcedureStr stored False;
    property Lyricists: TStrings read FLyricists stored False;
    property FileType: TJvID3FileType read FFileType write DummyProcedureFT stored False;
    property Time: string read FTime write DummyProcedureStr stored False;
    property ContentGroup: string read FContentGroup write DummyProcedureStr stored False;
    property ContentDescription: string read FContentDescription write DummyProcedureStr stored False;
    property SubTitle: string read FSubTitle write DummyProcedureStr stored False;
    property InitialKey: string read FInitialKey write DummyProcedureStr stored False;
    property Languages: TStrings read FLanguages stored False;
    property Length: string read FLength write DummyProcedureStr stored False;
    property MediaType: TJvID3MediaType read FMediaType write DummyProcedureMT stored False;
    property OriginalTitle: string read FOriginalTitle write DummyProcedureStr stored False;
    property OriginalFileName: string read FOriginalFileName write DummyProcedureStr stored False;
    property OriginalLyricists: TStrings read FOriginalLyricists stored False;
    property OriginalArtists: TStrings read FOriginalArtists stored False;
    property OriginalReleaseYear: string read FOriginalReleaseYear write DummyProcedureStr stored False;
    property FileOwner: string read FFileOwner write DummyProcedureStr stored False;
    property Performers: TStrings read FPerformers stored False;
    property Band: string read FBand write DummyProcedureStr stored False;
    property Conductor: string read FConductor write DummyProcedureStr stored False;
    property ModifiedBy: string read FModifiedBy write DummyProcedureStr stored False;
    property PartOf: string read FPartOf write DummyProcedureStr stored False;
    property Publisher: string read FPublisher write DummyProcedureStr stored False;
    property TrackNumber: string read FTrackNumber write DummyProcedureStr stored False;
    property RecordingDate: string read FRecordingDate write DummyProcedureStr stored False;
    property InternetRadioName: string read FInternetRadioName write DummyProcedureStr stored False;
    property InternetRadioOwner: string read FInternetRadioOwner write DummyProcedureStr stored False;
    property Size: Integer read FSize write DummyProcedureInt stored False;
    property ISRC: string read FISRC write DummyProcedureStr stored False;
    property EncodingSoftware: string read FEncodingSoftware write DummyProcedureStr stored False;
    property Year: string read FYear write DummyProcedureStr stored False;
    property Content: string read FContent write DummyProcedureStr stored False;
  end;

  TJvID3Web = class(TPersistent)
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
    FPictures: array [cJvID3LowIndex..cJvID3HighIndex] of TPicture;
    function GetPicture(const Index: Integer): TPicture;
    procedure DummyProcedure(const Index: Integer; const Value: TPicture);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    { TODO : Setters necessary ?? }
    property Other: TPicture index $00 read GetPicture write DummyProcedure stored False;
    property FileIcon: TPicture index $01 read GetPicture write DummyProcedure stored False;
    property OtherIcon: TPicture index $02 read GetPicture write DummyProcedure stored False;
    property CoverFront: TPicture index $03 read GetPicture write DummyProcedure stored False;
    property CoverBack: TPicture index $04 read GetPicture write DummyProcedure stored False;
    property Leaflet: TPicture index $05 read GetPicture write DummyProcedure stored False;
    property Media: TPicture index $06 read GetPicture write DummyProcedure stored False;
    property LeadArtist: TPicture index $07 read GetPicture write DummyProcedure stored False;
    property Artist: TPicture index $08 read GetPicture write DummyProcedure stored False;
    property Conductor: TPicture index $09 read GetPicture write DummyProcedure stored False;
    property Band: TPicture index $0A read GetPicture write DummyProcedure stored False;
    property Composer: TPicture index $0B read GetPicture write DummyProcedure stored False;
    property Lyricist: TPicture index $0C read GetPicture write DummyProcedure stored False;
    property RecordingLocation: TPicture index $0D read GetPicture write DummyProcedure stored False;
    property DuringRecording: TPicture index $0E read GetPicture write DummyProcedure stored False;
    property DuringPerformance: TPicture index $0F read GetPicture write DummyProcedure stored False;
    property MovieCapture: TPicture index $10 read GetPicture write DummyProcedure stored False;
    property ColouredFish: TPicture index $11 read GetPicture write DummyProcedure stored False;
    property Illustration: TPicture index $12 read GetPicture write DummyProcedure stored False;
    property BandLogo: TPicture index $13 read GetPicture write DummyProcedure stored False;
    property PublisherLogo: TPicture index $14 read GetPicture write DummyProcedure stored False;
  end;

  TJvIdPicturesDesc = class(TPersistent)
  private
    FStrings: array [cJvID3LowIndex..cJvID3HighIndex] of string;
    function GetDescription(const Index: Integer): string;
    procedure DummyProcedure(const Index: Integer; const Value: string);
    procedure SetDescription(const Index: Integer; const Value: string);
  published
    property Other: string index $00 read GetDescription write DummyProcedure stored False;
    property FileIcon: string index $01 read GetDescription write DummyProcedure stored False;
    property OtherIcon: string index $02 read GetDescription write DummyProcedure stored False;
    property CoverFront: string index $03 read GetDescription write DummyProcedure stored False;
    property CoverBack: string index $04 read GetDescription write DummyProcedure stored False;
    property Leaflet: string index $05 read GetDescription write DummyProcedure stored False;
    property Media: string index $06 read GetDescription write DummyProcedure stored False;
    property LeadArtist: string index $07 read GetDescription write DummyProcedure stored False;
    property Artist: string index $08 read GetDescription write DummyProcedure stored False;
    property Conductor: string index $09 read GetDescription write DummyProcedure stored False;
    property Band: string index $0A read GetDescription write DummyProcedure stored False;
    property Composer: string index $0B read GetDescription write DummyProcedure stored False;
    property Lyricist: string index $0C read GetDescription write DummyProcedure stored False;
    property RecordingLocation: string index $0D read GetDescription write DummyProcedure stored False;
    property DuringRecording: string index $0E read GetDescription write DummyProcedure stored False;
    property DuringPerformance: string index $0F read GetDescription write DummyProcedure stored False;
    property MovieCapture: string index $10 read GetDescription write DummyProcedure stored False;
    property ColouredFish: string index $11 read GetDescription write DummyProcedure stored False;
    property Illustration: string index $12 read GetDescription write DummyProcedure stored False;
    property BandLogo: string index $13 read GetDescription write DummyProcedure stored False;
    property PublisherLogo: string index $14 read GetDescription write DummyProcedure stored False;
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

  TJvID3Owner = class(TPersistent)
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

  TJvID3Popularimeter = class(TPersistent)
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

  TJvID3EventType = (etPADDING, etEND_OF_INITIAL_SILENCE, etINTRO_START, etMAINPART_START,
    etOUTRO_START, etOUTRO_END, etVERSE_START, etREFRAIN_START, etINTERLUDE_START,
    etTHEME_START, etVARIATION_START, etKEY_CHANGE, eTTime_CHANGE, etUNWANTED_NOISE,
    etSUSTAINED_NOISE, etSUSTAINED_NOISE_END, etINTRO_END, etMAINPART_END, etVERSE_END,
    etREFRAIN_END, etTHEME_END, etAUDIO_END, etFILE_END);

  TEventTiming = procedure(Sender: TObject; TimeStamp: Integer; EventType: TJvID3EventType) of object;

  TJvID3Events = record
    TimeStamp: Integer;
    EventType: TJvID3EventType;
  end;

  TJvID3Stream = class(TMemoryStream)
  private
    FEndMarker: PChar;
    FInFrame: Boolean;
    procedure MoveToNextFrame;
    function GetBytesToRead: Longint;
  public
    procedure StartFrame(const AFrameSize: Integer);
    procedure EndFrame;
    function ReadDate(var ADate: TDateTime): Longint;
    function ReadLanguage(var Language: string): Longint;
    function ReadNumber(var AValue: Cardinal): Longint;
    function ReadString(var SA: string): Longint;
    function ReadUserString(var SA1, SA2: string): Longint;
    function ReadWideString(var SW: WideString): Longint;
    function ReadUserWideString(var SW1, SW2: WideString): Longint;
    function ReadIsWideString: Boolean;
    procedure ReadFromStream(AStream: TStream; const ASize: Integer);
    function WriteBinaryDataToFile(const AFileName: string): Boolean;
    property BytesToRead: Longint read GetBytesToRead;
  end;

  TJvID3v2 = class(TJvComponent)
  private
    FEvents: array [0..1000] of TJvID3Events;
    FEventsCount: Integer;
    FEventsTiming: Integer;
    FCount: Integer;
    FTagPresent: Boolean;
    FVersion: Real;
    FFileName: TFileName;
    FUnsynchronisation: Boolean;
    FExperimental: Boolean;
    FExtendedHeader: Boolean;
    FID3Text: TJvID3Text;
    FWeb: TJvID3Web;
    FUserDefinedText: TJvID3UDText;
    FUserDefinedWeb: TJvID3UDUrl;
    FInvolvedPeople: TJvIdIpl;
    FImages: TJvIdImages;
    FOwner: TJvID3Owner;
    FPlayCounter: Cardinal;
    FPopularimeter: TJvID3Popularimeter;
    FEventTiming: TEventTiming;

    FFrameStream: TJvID3Stream;
    FCurrentFrame: TID3v2Frame;
    FCurrentFrameID: TJvID3FrameID;
    // unuseful variables
    FDummyB: Boolean;
    FDummyR: Real;
    FDummyI: Integer;
    FTagSize: Integer;
    procedure SetFileName(const Value: TFileName);
    procedure ResetProp;
  protected
    procedure ReadAndUnsynchro(Source, Dest: TStream; BytesToRead: Integer);
    procedure ReadText;
    procedure ReadWeb;
    procedure ReadInvolvedPeople;
    procedure ReadImg;
    procedure ReadPlayCounter;
    procedure ReadPopulariMeter;
    procedure ReadOwnership;
    procedure ReadEventTiming;
    procedure ReadComment;
    procedure ReadUnsyncedLyrics;
    procedure ReadTermsOfUse;
    procedure ReadGeneralObject;
    { (rb) this must be a function, not a method }
    // (rom) class function as compromise
    class function Iso639ToName(Code: string): string;
  public
    MusicCDIdentifier: array [0..803] of Byte;
    MusicCDIdentifierLength: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    procedure CheckEvent(CurrentTime: Integer);
  published
    { Do not store dummies }
    property TagPresent: Boolean read FTagPresent write FDummyB stored False;
    property FileName: TFileName read FFileName write SetFileName;
    property Version: Real read FVersion write FDummyR stored False;
    property Unsynchronisation: Boolean read FUnsynchronisation write FDummyB stored False;
    property ExtendedHeader: Boolean read FExtendedHeader write FDummyB stored False;
    property Experimental: Boolean read FExperimental write FDummyB stored False;
    property Texts: TJvID3Text read FID3Text;
    property UserDefinedText: TJvID3UDText read FUserDefinedText;
    property Web: TJvID3Web read FWeb;
    property UserDefinedWeb: TJvID3UDUrl read FUserDefinedWeb;
    property InvolvedPeople: TJvIdIpl read FInvolvedPeople;
    property Images: TJvIdImages read FImages;
    property PlayCounter: Cardinal read FPlayCounter write FPlayCounter stored False;
    property Owner: TJvID3Owner read FOwner;
    property Popularimeter: TJvID3Popularimeter read FPopularimeter;
    property TagSize: Integer read FTagSize write FDummyI stored False;
    property OnEventTiming: TEventTiming read FEventTiming write FEventTiming;
  end;

implementation

uses
  Math,
  JclUnicode, JclFileUtils;

// (rom) avoid assembler if possible

procedure ConvertHeaderSize(var P: Cardinal);
{             Byte1    Byte2    Byte3    Byte4
  Pre : P = Xabcdefg-Xhijklmn-Xopqrstu-XvwxyzAB  X = don't care;
  Post: P = 0000vwxy-zABopqrs-tuhijklm-nabcdefg  a..z,A,B = bits   }
var
  D: Cardinal;
begin
  D := 0;
  D := D or (P and $7F);
  P := P shr 8;
  D := D shl 7;
  D := D or (P and $7F);
  P := P shr 8;
  D := D shl 7;
  D := D or (P and $7F);
  P := P shr 8;
  D := D shl 7;
  D := D or (P and $7F);
  P := D;
end;

procedure ConvertID3Cardinal(var P: Cardinal);
{             Byte1    Byte2    Byte3    Byte4
  Pre : P = Xabcdefg-Xhijklmn-Xopqrstu-XvwxyzAB  X = don't care;
  Post: P = 0000vwxy-zABopqrs-tuhijklm-nabcdefg  a..z,A,B = bits   }
var
  D: Cardinal;
begin
  D := 0;
  D := D or (P and $FF);
  P := P shr 8;
  D := D shl 8;
  D := D or (P and $FF);
  P := P shr 8;
  D := D shl 8;
  D := D or (P and $FF);
  P := P shr 8;
  D := D shl 8;
  D := D or (P and $FF);
  P := D;
end;

(*
procedure ConvertHeaderSize(var P);
{             Byte1    Byte2    Byte3    Byte4
  Pre : P = Xabcdefg-Xhijklmn-Xopqrstu-XvwxyzAB  X = don't care;
  Post: P = 0000vwxy-zABopqrs-tuhijklm-nabcdefg  a..z,A,B = bits   }
asm
    MOV    EDX, DWORD PTR [P]
    XOR    ECX, ECX
    OR     CL, DL
    SHL    ECX, 7
    OR     CL, DH
    SHL    ECX, 7
    SHR    EDX, 16
    OR     CL, DL
    SHL    ECX, 7
    OR     CL, DH
    MOV    DWORD PTR [P],ECX
end;

procedure ConvertID3Cardinal(var P);
{
  Pre : P = B1-B2-B3-B4   B1,B2,B3,B4 = bytes
  Post: P = B4-B3-B2-B1                       }
asm
    MOV    EDX, DWORD PTR [P]
    MOV    CL, DL
    SHL    ECX, 8
    MOV    CL, DH
    SHL    ECX, 8
    SHR    EDX, 16
    MOV    CL, DL
    SHL    ECX, 8
    MOV    CL, DH
    MOV    DWORD PTR [P],ECX
end;
*)

function ExtractStrings(Source: string; Strings: TStrings): Integer;
var
  Head, Tail: PChar;
  Item: string;
begin
  Result := 0;
  if (Source = '') or (Strings = nil) then
    Exit;
  Tail := PChar(Source);

  Strings.BeginUpdate;
  try
    repeat
      Head := Tail;
      while not (Tail^ in ['/', #0]) do
        Inc(Tail);
      if Head^ <> #0 then
      begin
        if Strings <> nil then
        begin
          SetString(Item, Head, Tail - Head);
          Strings.Add(Item);
        end;
        Inc(Result);
      end;
      if Tail^ <> #0 then
        Inc(Tail);
    until Tail^ = #0;
  finally
    Strings.EndUpdate;
  end;
end;

//=== TJvID3Stream ===========================================================

procedure TJvID3Stream.EndFrame;
begin
  if not FInFrame then
    raise Exception.Create('Not reading frame');
  MoveToNextFrame;
  FInFrame := False;
end;

function TJvID3Stream.GetBytesToRead: Longint;
begin
  if FInFrame then
    Result := FEndMarker - PChar(Memory) - Position
  else
    Result := 0;
end;

procedure TJvID3Stream.MoveToNextFrame;
begin
  Seek(BytesToRead, soFromCurrent);
end;

function TJvID3Stream.ReadDate(var ADate: TDateTime): Longint;
var
  Year, Month, Day: Word;
  P: PChar;
begin
  P := PChar(Memory) + Position;

  Year := 0;
  Month := 0;
  Day := 0;
  Result := 0;

  while (Result < 8) and (P < FEndMarker) and (P^ in ['0'..'9']) do
  begin
    { Use Day as temp variable }
    Day := Day * 10 + Ord(P^) - Ord('0');

    { Format = YYYYMMDD }
    case Result of
      3:
        begin
          Year := Day;
          Day := 0;
        end;
      5:
        begin
          Month := Day;
          Day := 0;
        end;
    end;
    Inc(P);
    Inc(Result);
  end;

  if Result = 8 then
  begin
    Seek(Result, soFromCurrent);
    try
      ADate := EncodeDate(Year, Month, Day);
    except
      on EConvertError do
        ADate := 0;
    end;
  end
  else
  begin
    Result := 0;
    ADate := 0;
  end;
end;

function TJvID3Stream.ReadNumber(var AValue: Cardinal): Longint;
begin
  if BytesToRead = 4 then
  begin
    Result := Read(AValue, 4);
    ConvertID3Cardinal(AValue);
  end
  else
  begin
    { Error ( BytesToRead < 4) or not implemented ( BytesToRead > 4) }
    AValue := 0;
    Result := 0;
  end;
end;

function TJvID3Stream.ReadIsWideString: Boolean;
var
  Ch: Char;
begin
  if BytesToRead > 0 then
  begin
    Read(Ch, 1);
    Result := Ch = #1;
  end
  else
    Result := False;
end;

function TJvID3Stream.ReadLanguage(var Language: string): Longint;
begin
  if BytesToRead < 3 then
    Result := 0
  else
  begin
    SetLength(Language, 3);
    Result := Read(Language[1], 3);
  end;

  if Result < 3 then
  begin
    Language := '';
    Exit;
  end;
end;

function TJvID3Stream.ReadString(var SA: string): Longint;
var
  P, StartPos: PChar;
begin
  StartPos := PChar(Memory) + Position;
  P := StartPos;

  while (P^ <> #0) and (P < FEndMarker) do
    Inc(P);
  Result := P - StartPos;

  SetString(SA, StartPos, Result);

  { Skip terminator }
  if P < FEndMarker then
    Inc(Result);

  Seek(Result, soFromCurrent);
end;

function TJvID3Stream.ReadUserString(var SA1, SA2: string): Longint;
begin
  Result := ReadString(SA1);

  if BytesToRead > 0 then
    Result := Result + ReadString(SA2)
  else
    SA2 := '';
end;

function TJvID3Stream.ReadUserWideString(var SW1, SW2: WideString): Longint;
begin
  Result := ReadWideString(SW1);

  if BytesToRead > 0 then
    Result := Result + ReadWideString(SW2)
  else
    SW2 := '';
end;

function TJvID3Stream.ReadWideString(var SW: WideString): Longint;
var
  Order: WideChar;
  P, StartPos: PChar;
  TerminatorFound: Boolean;
  WideCharCount: Integer;
begin
  if BytesToRead < 2 then
  begin
    SW := '';
    Result := 0;
    Exit;
  end;

  Result := Read(Order, 2);
  if (Order <> BOM_LSB_FIRST) and (Order <> BOM_MSB_FIRST) then
  begin
    SW := '';
    Exit;
  end;

  StartPos := PChar(Memory) + Position;
  P := StartPos;

  { Read until #0#0 found or until FEndMarker }
  TerminatorFound := False;
  while (P < FEndMarker) and (not TerminatorFound or (P^ <> #0)) do
  begin
    TerminatorFound := P^ = #0;
    Inc(P);
  end;

  Result := Result + P - StartPos;
  TerminatorFound := TerminatorFound and (P < FEndMarker);

  { Do not include the BOM, thus -2 }
  WideCharCount := (Result - 2) div 2;
  if TerminatorFound then
    { #0#0 found }
    Dec(WideCharCount);

  SetLength(SW, WideCharCount);
  if WideCharCount > 0 then
    Move(StartPos[0], SW[1], 2 * WideCharCount);
  if Order = BOM_MSB_FIRST then
    StrSwapByteOrder(PWideChar(SW));

  { Skip Terminator }
  if TerminatorFound then
    Inc(Result, 2);

  Seek(Result, soFromCurrent);
end;

procedure TJvID3Stream.StartFrame(const AFrameSize: Integer);
begin
  if FInFrame then
    raise Exception.Create('Already reading frame');
  FEndMarker := PChar(Memory) + Position + AFrameSize;
  FInFrame := True;
end;

function TJvID3Stream.WriteBinaryDataToFile(const AFileName: string): Boolean;
var
  FileStream: TFileStream;
begin
  Result := not FileExists(AFileName);
  if not Result then
    Exit;
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    if BytesToRead > 0 then
      FileStream.WriteBuffer((PChar(Memory) + Position)^, BytesToRead);
  finally
    FileStream.Free;
  end;
end;

procedure TJvID3Stream.ReadFromStream(AStream: TStream;
  const ASize: Integer);
//var
//  Count: Longint;
begin
  Position := 0;
  //Count := Stream.Size;
  SetSize(ASize);
  if ASize <> 0 then
    AStream.ReadBuffer(Memory^, ASize);
end;

//=== TJvID3v2 ===============================================================

constructor TJvID3v2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagSize := 0;
  FCount := 0;
  FTagPresent := False;
  FID3Text := TJvID3Text.Create;
  FWeb := TJvID3Web.Create;
  FUserDefinedText := TJvID3UDText.Create;
  FUserDefinedWeb := TJvID3UDUrl.Create;
  FInvolvedPeople := TJvIdIpl.Create;
  FImages := TJvIdImages.Create;
  FOwner := TJvID3Owner.Create;
  FPopularimeter := TJvID3Popularimeter.Create;
  ResetProp;
end;

destructor TJvID3v2.Destroy;
begin
  FID3Text.Free;
  FWeb.Free;
  FUserDefinedText.Free;
  FUserDefinedWeb.Free;
  FInvolvedPeople.Free;
  FImages.Free;
  FOwner.Free;
  FPopularimeter.Free;
  inherited Destroy;
end;

procedure TJvID3v2.CheckEvent(CurrentTime: Integer);
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

class function TJvID3v2.Iso639ToName(Code: string): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Code) <> 3 then
    Exit;
  Code := LowerCase(Code);
  for I := Low(cShortToLongNameTable) to High(cShortToLongNameTable) do
    if Code = cShortToLongNameTable[I].S then
    begin
      Result := cShortToLongNameTable[I].L;
      Break;
    end;
end;

procedure TJvID3v2.LoadFromFile(FileName: string);
var
  FileStream: TFileStream;
begin
  if FileExists(FileName) then
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TJvID3v2.LoadFromStream(Stream: TStream);
var
  Header: TID3v2Header;
  ExtendedHeaderSize: Cardinal;
begin
  ResetProp;
  FEventsCount := 0;

  { Read the headers }
  Stream.ReadBuffer(Header, 10);

  FTagPresent := Header.Identifier = 'ID3';
  if not FTagPresent then
    Exit;

  //Version
  FVersion := (Header.Version and $00FF);
  FVersion := FVersion + (Header.Version shr 8) / 100;

  //Flags
  FUnsynchronisation := (Header.Flags and $80) <> 0;
  FExtendedHeader := (Header.Flags and $40) <> 0;
  FExperimental := (Header.Flags and $20) <> 0;

  ConvertHeaderSize(Header.Size);

  { The ID3v2 tag size is the size of the complete tag after unsychronisation,
    including padding, excluding the header but not excluding the extended
    header }
  FTagSize := Header.Size;

  { Only version 2.3 is supported for now }
  if FVersion <> 3 then
    Exit;

  { Read the frames }
  FFrameStream := TJvID3Stream.Create;
  try
    if FUnsynchronisation then
      ReadAndUnsynchro(Stream, FFrameStream, FTagSize)
    else
      FFrameStream.ReadFromStream(Stream, FTagSize);

    FFrameStream.Position := 0;

    { Read extended tag }
    if FExtendedHeader then
    begin
      FFrameStream.StartFrame(4);
      FFrameStream.ReadNumber(ExtendedHeaderSize);
      FFrameStream.EndFrame;

      FFrameStream.StartFrame(ExtendedHeaderSize);
      { Just skip it }
      FFrameStream.EndFrame;
    end;

    while (FFrameStream.Size - FFrameStream.Position > 10) do
    begin
      FFrameStream.Read(FCurrentFrame, 10);
      ConvertID3Cardinal(FCurrentFrame.Size);

      { Not implemented: Flags }
      FCurrentFrameID := ID3_TextToFrameID(FCurrentFrame.ID);

      FFrameStream.StartFrame(FCurrentFrame.Size);
      try
        case FCurrentFrameID of
          fiAlbum, fiBPM, fiComposer, fiContentType, fiCopyright, fiDate, fiPlaylistDelay,
            fiEncodedBy, fiLyricist, fiFileType, fiTime, fiContentGroup, fiTitle, fiSubTitle,
            fiInitialKey, fiLanguage, fiSongLen, fiMediaType, fiOrigAlbum, fiOrigFileName,
            fiOrigLyricist, fiOrigArtist, fiOrigYear, fiFileOwner, fiLeadArtist, fiBand,
            fiConductor, fiMixArtist, fiPartInSet, fiPublisher, fiTrackNum, fiRecordingDates,
            fiNetRadioStation, fiNetRadioOwner, fiSize, fiISRC, fiEncoderSettings, fiUserText,
            fiYear:
            ReadText;
          fiWWWCommercialInfo, fiWWWCopyright, fiWWWAudioFile, fiWWWArtist, fiWWWAudioSource,
            fiWWWRadioPage, fiWWWPayment, fiWWWPublisher, fiWWWUser:
            ReadWeb;
          fiInvolvedPeople:
            ReadInvolvedPeople;
          fiComment:
            { This frame is indended for any kind of full text information that
              does not fit in any other frame. It consists of a frame header
              followed by encoding, language and content descriptors and is ended
              with the actual comment as a text string. Newline characters are
              allowed in the comment text string. There may be more than one
              comment frame in each tag, but only one with the same language and
              content descriptor. }
            ReadComment;
          fiCDID:
            { This frame is intended for music that comes from a CD, so that the CD
              can be identified in databases such as the CDDB. The frame consists of
              a binary dump of the Table Of Contents, TOC, from the CD, which is a
              header of 4 bytes and then 8 bytes/track on the CD plus 8 bytes for the
              'lead out' making a maximum of 804 bytes. The offset to the beginning of
              every track on the CD should be described with a four bytes absolute
              CD-frame address per track, and not with absolute time. This frame requires
              a present and valid "TRCK" frame, even if the CD's only got one track.
              There may only be one "MCDI" frame in each tag. }
            begin
              FFrameStream.Read(MusicCDIdentifier, FCurrentFrame.Size);
              MusicCDIdentifierLength := FCurrentFrame.Size;
            end;
          fiGeneralObject:
            ReadGeneralObject;
          fiTermsOfUse:
            ReadTermsOfUse;
          fiPicture:
            ReadImg;
          fiPlayCounter:
            ReadPlayCounter;
          fiPopularimeter:
            ReadPopulariMeter;
          fiOwnership:
            ReadOwnership;
          fiEventTiming:
            ReadEventTiming;
          fiUnsyncedLyrics:
            ReadUnsyncedLyrics;
          fiNoFrame:
            { Do nothing };
        end;
      finally
        FFrameStream.EndFrame;
      end;
    end;
  finally
    FFrameStream.Free;
  end;
end;

procedure TJvID3v2.ReadAndUnsynchro(Source, Dest: TStream; BytesToRead: Integer);
const
  CBufferSize = 500;
var
  LastWasFF: Boolean;
  BytesRead: Integer;
  SourcePtr, DestPtr: Integer;
  SourceBuf, DestBuf: array [0..CBufferSize - 1] of Byte;
begin
  { Replace $FF 00 with $FF }

  LastWasFF := False;
  while BytesToRead > 0 do
  begin
    { Read at max CBufferSize bytes from the stream }
    BytesRead := Source.Read(SourceBuf, Min(CBufferSize, BytesToRead));
    Dec(BytesToRead, BytesRead);

    DestPtr := 0;
    SourcePtr := 0;

    while SourcePtr < BytesRead do
    begin
      { If previous was $FF and current is $00 then skip.. }
      if not LastWasFF or (SourceBuf[SourcePtr] <> $00) then
      begin
        { ..otherwise copy }
        DestBuf[DestPtr] := SourceBuf[SourcePtr];
        Inc(DestPtr);
      end;

      LastWasFF := SourceBuf[SourcePtr] = $FF;
      Inc(SourcePtr);
    end;
    Dest.Write(DestBuf, DestPtr);
  end;
end;

procedure TJvID3v2.ReadComment;
var
  IsUnicode: Boolean;
  Language: string;
  SW1, SW2: WideString;
  S1, S2: string;
begin
  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    ReadLanguage(Language);
    if IsUnicode then
    begin
      ReadWideString(SW1);
      ReadWideString(SW2);
      S1 := WideStringToStringEx(SW1, CP_ACP);
      S2 := WideStringToStringEx(SW2, CP_ACP);
    end
    else
    begin
      ReadString(S1);
      ReadString(S2);
    end;
  end;
end;

procedure TJvID3v2.ReadGeneralObject;
var
  IsUnicode: Boolean;
  MIMEType: string;
  FileName, ContentDesc: string;
  FileNameW, ContentDescW: WideString;
begin
  {  In this frame any type of file can be encapsulated. After the header,
     'Frame size' and 'Encoding' follows 'MIME type' [MIME] represented as
     as a terminated string encoded with ISO 8859-1 [ISO-8859-1]. The
     filename is case sensitive and is encoded as 'Encoding'. Then follows
     a content description as terminated string, encoded as 'Encoding'.
     The last thing in the frame is the actual object. The first two
     strings may be omitted, leaving only their terminations. MIME type is
     always an ISO-8859-1 text string. There may be more than one "GEOB"
     frame in each tag, but only one with the same content descriptor.

     Text encoding          $xx
     MIME type              <text string> $00
     Filename               <text string according to encoding> $00 (00)
     Content description    <text string according to encóding> $00 (00)
     Encapsulated object    <binary data>
  }

  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    ReadString(MIMEType); // may be empty
    if IsUnicode then
    begin
      ReadWideString(FileNameW); // may be empty
      ReadWideString(ContentDescW);
      FileName := WideStringToStringEx(FileNameW, CP_ACP);
      ContentDesc := WideStringToStringEx(ContentDescW, CP_ACP);
      { Skip : Read binary }
    end
    else
    begin
      ReadString(FileName); // may be empty
      ReadString(ContentDesc);
      { Skip : Read binary }
    end;
  end;
end;

procedure TJvID3v2.ReadTermsOfUse;
var
  Language: string;
  IsUnicode: Boolean;
  S: string;
  SW: WideString;
begin
  { This frame contains a brief description of the terms of use and
    ownership of the file. More detailed information concerning the legal
    terms might be available through the "WCOP" frame. Newlines are
    allowed in the text. There may only be one "USER" frame in a tag.

       Text encoding        $xx
       Language             $xx xx xx
       The actual text      <text string according to encoding>
  }
  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    ReadLanguage(Language);
    if IsUnicode then
    begin
      ReadWideString(SW);
      S := WideStringToStringEx(SW, CP_ACP);
    end
    else
      ReadString(S);
  end;
end;

procedure TJvID3v2.ReadUnsyncedLyrics;
var
  IsUnicode: Boolean;
  S1, S2: string;
  SW1, SW2: WideString;
  Language: string;
begin
  { Text encoding        $xx
    Language             $xx xx xx
    Content descriptor   <text string according to encoding> $00 (00)
    Lyrics/text          <full text string according to encoding>
  }
  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    ReadLanguage(Language);
    if IsUnicode then
    begin
      ReadWideString(SW1);
      ReadWideString(SW2);
      S1 := WideStringToStringEx(SW1, CP_ACP);
      S2 := WideStringToStringEx(SW2, CP_ACP);
    end
    else
    begin
      ReadString(S1);
      ReadString(S2);
    end;
  end;
end;

procedure TJvID3v2.ReadPlayCounter;
begin
  { This is simply a counter of the number of times a file has been played. The
    value is increased by one every time the file begins to play. There may only
    be one "PCNT" frame in each tag. When the counter reaches all one's, one byte
    is inserted in front of the counter thus making the counter eight bits bigger.
    The counter must be at least 32-bits long to begin with. }
  FFrameStream.ReadNumber(FPlayCounter);
end;

procedure TJvID3v2.ReadEventTiming;
var
  TypeOfEvent: Byte;
  TimeStamp: Cardinal;
begin
  if FFrameStream.BytesToRead < 1 then
    Exit;

  FFrameStream.Read(FEventsTiming, 1);
  FEventsCount := 0;

  while FFrameStream.BytesToRead > 0 do
  begin
    FFrameStream.Read(TypeOfEvent, 1);
    with FEvents[FEventsCount] do
      case TypeOfEvent of
        $00:
          EventType := etPADDING;
        $01:
          EventType := etEND_OF_INITIAL_SILENCE;
        $02:
          EventType := etINTRO_START;
        $03:
          EventType := etMAINPART_START;
        $04:
          EventType := etOUTRO_START;
        $05:
          EventType := etOUTRO_END;
        $06:
          EventType := etVERSE_START;
        $07:
          EventType := etREFRAIN_START;
        $08:
          EventType := etINTERLUDE_START;
        $09:
          EventType := etTHEME_START;
        $0A:
          EventType := etVARIATION_START;
        $0B:
          EventType := etKEY_CHANGE;
        $0C:
          EventType := eTTime_CHANGE;
        $0D:
          EventType := etUNWANTED_NOISE;
        $0E:
          EventType := etSUSTAINED_NOISE;
        $0F:
          EventType := etSUSTAINED_NOISE_END;
        $10:
          EventType := etINTRO_END;
        $11:
          EventType := etMAINPART_END;
        $12:
          EventType := etVERSE_END;
        $13:
          EventType := etREFRAIN_END;
        $14:
          EventType := etTHEME_END;
        $FD:
          EventType := etAUDIO_END;
        $FE:
          EventType := etFILE_END;
      end;

    FFrameStream.ReadNumber(TimeStamp);
    FEvents[FEventsCount].TimeStamp := TimeStamp;

    Inc(FEventsCount);
  end;
end;

procedure TJvID3v2.ReadImg;
var
  TmpFileName: string;
  IsUnicode: Boolean;
  MIMEType, MIMEExt: string;
  PictureType: Byte;
  Description: string;
  DescriptionW: WideString;
begin
  { This frame contains a picture directly related to the audio file. Image format
    is the MIME type and subtype for the image. In the event that the MIME media
    type name is omitted, "image/" will be implied. The "image/png" or "image/jpeg"
    picture format should be used when interoperability is wanted. Description is
    a short description of the picture, represented as a terminated textstring.
    The description has a maximum length of 64 characters, but may be empty.
    There may be several pictures attached to one file, each in their individual
    "APIC" frame, but only one with the same content descriptor. There may only
    be one picture with the picture type declared as picture type $01 and $02
    respectively. There is the possibility to put only a link to the image file
    by using the 'MIME type' "-->" and having a complete URL instead of picture
    data. The use of linked files should however be used sparingly since there
    is the risk of separation of files.

     Text encoding      $xx
     MIME type          <text string> $00
     Picture type       $xx
     Description        <text string according to encoding> $00 (00)
     Picture data       <binary data>
  }
  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    ReadString(MIMEType);
    if BytesToRead < 1 then
      Exit;

    Read(PictureType, 1);
    if IsUnicode then
    begin
      ReadWideString(DescriptionW);
      Description := WideStringToStringEx(DescriptionW, CP_ACP);
    end
    else
      ReadString(Description);

    TmpFileName := JclFileUtils.FileGetTempName('JvID3Pic');

    if MIMEType = '-->' then
    begin
      { Link }
      FImages.Infos.SetDescription(PictureType, Description);
      Exit;
    end;

    { Not a very reliable method; maybe use Indy's TIdMimeTable
      in IdGlobal.pas
    }

    { See: ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/media-types }

    { image/jpeg   .jpg    preferred     supported
      image/png    .png    preferred
      image/gif    .gif
      image/tiff   .tif
      image/x-pict .pic
      image/bitmap .bmp                  supported
    }
    MIMEExt := Copy(MIMEType, Pos('/', MIMEType) + 1, Length(MIMEType));
    MIMEExt := LowerCase(MIMEExt);
    if MIMEExt = 'jpeg' then
      MIMEExt := '.jpg'
    else
    if MIMEExt = 'x-png' then
      MIMEExt := '.png'
    else
    if (MIMEExt = 'bitmap') or (MIMEExt = 'x-ms-bmp') then
      MIMEExt := '.bmp'
    else
    if MIMEExt = 'tiff' then
      MIMEExt := '.tif'
    else
    if MIMEExt = 'x-pict' then
      MIMEExt := '.pic'
    else
      MIMEExt := '.' + MIMEExt;

    TmpFileName := ChangeFileExt(TmpFileName, MIMEExt);

    if WriteBinaryDataToFile(TmpFileName) then
    try
      FImages.Infos.SetDescription(PictureType, Description);
      try
        FImages.Pictures.GetPicture(PictureType).LoadFromFile(TmpFileName);
      except
        on EInvalidGraphic do
          ; { Do nothing }
      end
    finally
      DeleteFile(TmpFileName);
    end;
  end;
end;

procedure TJvID3v2.ReadInvolvedPeople;
var
  IsUnicode: Boolean;
  S1, S2: string;
  SW1, SW2: WideString;
begin
  { Since there might be a lot of people contributing to an audio file in various
    ways, such as musicians and technicians, the 'Text information frames' are often
    insufficient to list everyone involved in a project. The 'Involved people list'
    is a frame containing the names of those involved, and how they were involved.
    The body simply contains a terminated string with the involvement directly
    followed by a terminated string with the involvee followed by a new involvement
    and so on. There may only be one "IPLS" frame in each tag. }

  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    if IsUnicode then
      { Exspect at least 2 #0#0 terminators }
      while BytesToRead > 4 do
      begin
        ReadWideString(SW1);
        ReadWideString(SW2);
        S1 := WideStringToStringEx(SW1, CP_ACP);
        S2 := WideStringToStringEx(SW2, CP_ACP);
        FInvolvedPeople.AddItem(S1, S2);
      end
    else
      { Expect at least 2 #0 terminators }
      while BytesToRead > 2 do
      begin
        ReadString(S1);
        ReadString(S2);
        FInvolvedPeople.AddItem(S1, S2);
      end;
  end;
end;

procedure TJvID3v2.ReadOwnership;
var
  IsUnicode: Boolean;
  SA: string;
  SW: WideString;
  D: TDateTime;
begin
  { The ownership frame might be used as a reminder of a made transaction or, if
    signed, as proof. Note that the "USER" and "TOWN" frames are good to use in
    conjunction with this one. The frame begins, after the frame ID, size and
    encoding fields, with a 'price payed' field. The first three characters of
    this field contains the currency used for the transaction, encoded according
    to ISO-4217 alphabetic currency code. Concatenated to this is the actual price
    payed, as a numerical string using "." as the decimal separator. Next is an 8
    character date string (YYYYMMDD) followed by a string with the name of the
    seller as the last field in the frame. There may only be one "OWNE" frame in
    a tag. }

  IsUnicode := FFrameStream.ReadIsWideString;

  FFrameStream.ReadString(SA);
  FOwner.Price := SA;

  FFrameStream.ReadDate(D);
  FOwner.DatePurchased := D;

  if IsUnicode then
  begin
    FFrameStream.ReadWideString(SW);
    SA := WideStringToStringEx(SW, CP_ACP);
  end
  else
    FFrameStream.ReadString(SA);
  FOwner.Seller := SA;
end;

procedure TJvID3v2.ReadPopulariMeter;
var
  S: string;
  B: Byte;
  Counter: Cardinal;
begin
  { The purpose of this frame is to specify how good an audio file is. Many
    interesting applications could be found to this frame such as a playlist that
    features better audiofiles more often than others or it could be used to profile
    a person's taste and find other 'good' files by comparing people's profiles.
    The frame is very simple. It contains the email address to the user, one rating
    byte and a four byte play counter, intended to be increased with one for every
    time the file is played. The email is a terminated string. The rating is 1-255
    where 1 is worst and 255 is best. 0 is unknown. If no personal counter is wanted
    it may be omitted. When the counter reaches all one's, one byte is inserted in
    front of the counter thus making the counter eight bits bigger in the same away
    as the play counter ("PCNT"). There may be more than one "POPM" frame in each
    tag, but only one with the same email address. }

  with FFrameStream do
  begin
    ReadString(S);
    FPopularimeter.UserEmail := S;

    if BytesToRead < 1 then
      Exit;

    Read(B, 1);
    FPopularimeter.Rating := B;

    ReadNumber(Counter);
    ConvertID3Cardinal(Counter);
    FPopularimeter.Counter := Counter;
  end;
end;

procedure TJvID3v2.ReadText;
var
  FieldW1, FieldW2: WideString;
  Field1, Field2: string;
begin
  { The text information frames are the most important frames, containing information
    like artist, album and more. There may only be one text information frame of its
    kind in an tag. If the textstring is followed by a termination ($00 (00)) all the
    following information should be ignored and not be displayed. All text frame
    identifiers begin with "T". Only text frame identifiers begin with "T", with the
    exception of the "TXXX" frame. All the text information frames have the following
    format: }

  if FCurrentFrameID = fiUserText then { TXXX }
  begin
    { This frame is intended for one-string text information concerning the audiofile
      in a similar way to the other "T"-frames. The frame body consists of a description
      of the string, represented as a terminated string, followed by the actual string.
      There may be more than one "TXXX" frame in each tag, but only one with the same
      description. }
    if FFrameStream.ReadIsWideString then
    begin
      FFrameStream.ReadUserWideString(FieldW1, FieldW2);
      Field1 := WideStringToStringEx(FieldW1, CP_ACP);
      Field2 := WideStringToStringEx(FieldW2, CP_ACP);
    end
    else
      FFrameStream.ReadUserString(Field1, Field2);

    FUserDefinedText.AddItem(Field1, Field2);
  end
  else
  begin
    if FFrameStream.ReadIsWideString then
    begin
      FFrameStream.ReadWideString(FieldW1);
      Field1 := WideStringToStringEx(FieldW1, CP_ACP);
    end
    else
      //ISO-8859-1
      FFrameStream.ReadString(Field1);

    case FCurrentFrameID of
      fiAlbum:
        { The 'Album/Movie/Show title' frame is intended for the title of the
          recording(/FCurrentFrame of sound) which the audio in the file is taken from. }
        FID3Text.FAlbum := Field1;
      fiBPM:
        { The 'BPM' frame contains the number of beats per minute in the mainpart
          of the audio. The BPM is an integer and represented as a numerical string. }
        FID3Text.FBPM := StrToIntDef(Field1, 0);
      fiComposer:
        { The 'Composer(s)' frame is intended for the name of the composer(s).
          They are seperated with the "/" character. }
        ExtractStrings(Field1, FID3Text.FComposer);
      fiContentType:
        FID3Text.FContent := Field1;
      fiCopyright:
        { The 'Copyright message' frame, which must begin with a year and a space
          character (making five characters), is intended for the copyright holder
          of the original sound, not the audio file itself. The absence of this frame
          means only that the copyright information is unavailable or has been removed,
          and must not be interpreted to mean that the sound is public domain. Every
          time this field is displayed the field must be preceded with "Copyright © ". }
        FID3Text.FCopyright := Field1;
      fiDate:
        { The 'Date' frame is a numeric string in the DDMM format containing the
          date for the recording. This field is always four characters long. }
        FID3Text.FDate := Field1;
      fiPlaylistDelay:
        { The 'Playlist delay' defines the numbers of milliseconds of silence between
          every song in a playlist. The player should use the "ETC" frame, if present,
          to skip initial silence and silence at the end of the audio to match the
          'Playlist delay' time. The time is represented as a numeric string. }
        FID3Text.FPlaylistDelay := StrToIntDef(Field1, 0);
      fiEncodedBy:
        { The 'Encoded by' frame contains the name of the person or organisation that
          encoded the audio file. This field may contain a copyright message, if
          the audio file also is copyrighted by the encoder. }
        FID3Text.FEncodedBy := Field1;
      fiLyricist:
        { The 'Lyricist(s)/Text writer(s)' frame is intended for the writer(s) of
          the text or lyrics in the recording. They are seperated with the "/" character. }
        ExtractStrings(Field1, FID3Text.FLyricists);
      fiFileType:
        { The 'File type' frame indicates which type of audio this tag defines.
          The following type and refinements are defined:

          MPG  MPEG Audio
           /1  MPEG 1/2 layer I
           /2  MPEG 1/2 layer II
           /3  MPEG 1/2 layer III
         /2.5  MPEG 2.5
         /AAC  Advanced audio compression
          VQF  Transform-domain Weighted Interleave Vector Quantization
          PCM  Pulse Code Modulated audio

          but other types may be used, not for these types though. This is used in
          a similar way to the predefined types in the "TMED" frame, but without
          parentheses. If this frame is not present audio type is assumed to be "MPG". }
        begin
          if Field1 = 'MPG' then
            FID3Text.FFileType := ftMPG
          else
          if Field1 = 'MPG/1' then
            FID3Text.FFileType := ftMPG1
          else
          if Field1 = 'MPG/2' then
            FID3Text.FFileType := ftMPG2
          else
          if Field1 = 'MPG/3' then
            FID3Text.FFileType := ftMPG3
          else
          if Field1 = 'MPG/2.5' then
            FID3Text.FFileType := ftMPG2_5
          else
          if Field1 = 'MPG/AAC' then
            FID3Text.FFileType := ftMPG_AAC
          else
          if Field1 = 'VQF' then
            FID3Text.FFileType := ftVQF
          else
          if Field1 = 'PCM' then
            FID3Text.FFileType := ftPCM
          else
            FID3Text.FFileType := ftUNKNOWN;
        end;
      fiTime:
        { The 'Time' frame is a numeric string in the HHMM format containing the
          time for the recording. This field is always four characters long. }
        FID3Text.FTime := Field1;
      fiContentGroup:
        { The 'Content group description' frame is used if the sound belongs to a
          larger category of sounds/music. For example, classical music is often
          sorted in different musical sections (e.g. "Piano Concerto", "Weather -
          Hurricane"). }
        FID3Text.FContentGroup := Field1;
      fiTitle:
        { The 'Title/Songname/Content description' frame is the actual name of the
          piece (e.g. "Adagio", "Hurricane Donna"). }
        FID3Text.FContentDescription := Field1;
      fiSubTitle:
        { The 'Subtitle/Description refinement' frame is used for information
          directly related to the contents title (e.g. "Op. 16" or "Performed live
          at Wembley"). }
        FID3Text.FSubTitle := Field1;
      fiInitialKey:
        { The 'Initial key' frame contains the musical key in which the sound starts.
          It is represented as a string with a maximum length of three characters.
          The ground keys are represented with "A","B","C","D","E", "F" and "G"
          and halfkeys represented with "b" and "#". Minor is represented as "m".
          Example "Cbm". Off key is represented with an "o" only. }
        FID3Text.FInitialKey := Field1;
      fiLanguage:
        { The 'Language(s)' frame should contain the languages of the text or
          lyrics spoken or sung in the audio. The language is represented with
          three characters according to ISO-639-2. If more than one language is
          used in the text their language codes should follow according to their
          usage. }
        begin
          while Length(Field1) > 3 do
          begin
            FID3Text.FLanguages.Add(Iso639ToName(Copy(Field1, 1, 3)));
            Field1 := Copy(Field1, 4, Length(Field1));
          end;
          FID3Text.FLanguages.Add(Iso639ToName(Field1));
        end;
      fiSongLen:
        { The 'Length' frame contains the length of the audiofile in milliseconds,
          represented as a numeric string. }
        FID3Text.FLength := Field1;
      fiMediaType:
        { The 'Media type' frame describes from which media the sound originated.
          This may be a text string or a reference to the predefined media types
          found in the list below. References are made within "(" and ")" and are
          optionally followed by a text refinement, e.g. "(MC) with four channels".
          If a text refinement should begin with a "(" character it should be replaced
          with "((" in the same way as in the "TCO" frame. Predefined refinements
          is appended after the media type, e.g. "(CD/A)" or "(VID/PAL/VHS)". }
        begin
          { TODO : Adjust }
          if Field1 = 'DIG' then
            FID3Text.FMediaType := mtDIG
          else
          if Field1 = 'DIG/A' then
            FID3Text.FMediaType := mtDIG_ANALOG_TRANSFER
          else
          if Field1 = 'ANA' then
            FID3Text.FMediaType := mtANA
          else
          if Field1 = 'ANA/WAC' then
            FID3Text.FMediaType := mtANA_WAX_CYLINDER
          else
          if Field1 = 'ANA/8CA' then
            FID3Text.FMediaType := mtANA_8_TRACK_TAPE
          else
          if Field1 = 'CD' then
            FID3Text.FMediaType := mtCD
          else
          if Field1 = 'CD/A' then
            FID3Text.FMediaType := mtCD_ANALOG
          else
          if Field1 = 'CD/DD' then
            FID3Text.FMediaType := mtCD_DDD
          else
          if Field1 = 'CD/AD' then
            FID3Text.FMediaType := mtCD_ADD
          else
          if Field1 = 'CD/AA' then
            FID3Text.FMediaType := mtCD_AAD
          else
          if Field1 = 'LD' then
            FID3Text.FMediaType := mtLASERDISC
          else
          if Field1 = 'LD/A' then
            FID3Text.FMediaType := mtLASERDISC_ANALOG_TRANSFER
          else
          if Field1 = 'TT' then
            FID3Text.FMediaType := mtTURNTABLE
          else
          if Field1 = 'TT/33' then
            FID3Text.FMediaType := mtTURNTABLE_33
          else
          if Field1 = 'TT/45' then
            FID3Text.FMediaType := mtTURNTABLE_45
          else
          if Field1 = 'TT/71' then
            FID3Text.FMediaType := mtTURNTABLE_71
          else
          if Field1 = 'TT/76' then
            FID3Text.FMediaType := mtTURNTABLE_76
          else
          if Field1 = 'TT/78' then
            FID3Text.FMediaType := mtTURNTABLE_78
          else
          if Field1 = 'TT/80' then
            FID3Text.FMediaType := mtTURNTABLE_80
          else
          if Field1 = 'MD' then
            FID3Text.FMediaType := mtMINIDISC
          else
          if Field1 = 'MD/A' then
            FID3Text.FMediaType := mtMINIDISC_ANALOG_TRANSFER
          else
          if Field1 = 'DAT' then
            FID3Text.FMediaType := mtDAT
          else
          if Field1 = 'DAT/A' then
            FID3Text.FMediaType := mtDAT_ANALOG_TRANSFER
          else
          if Field1 = 'DAT/1' then
            FID3Text.FMediaType := mtDAT_48KHZ_16B
          else
          if Field1 = 'DAT/2' then
            FID3Text.FMediaType := mtDAT_32KHZ_16B
          else
          if Field1 = 'DAT/3' then
            FID3Text.FMediaType := mtDAT_32KHZ_12B
          else
          if Field1 = 'DAT/4' then
            FID3Text.FMediaType := mtDAT_32KHZ_12B_4CH
          else
          if Field1 = 'DAT/5' then
            FID3Text.FMediaType := mtDAT_44KHZ_16B
          else
          if Field1 = 'DAT/6' then
            FID3Text.FMediaType := mtDAT_44KHZ_16B_WIDE
          else
          if Field1 = 'DCC' then
            FID3Text.FMediaType := mtDCC
          else
          if Field1 = 'DCC/A' then
            FID3Text.FMediaType := mtDCC_ANALOG_TRANSFER
          else
          if Field1 = 'DVD' then
            FID3Text.FMediaType := mtDVD
          else
          if Field1 = 'DVD/A' then
            FID3Text.FMediaType := mtDVD_ANALOG_TRANSFER
          else
          if Field1 = 'TV' then
            FID3Text.FMediaType := mtTV
          else
          if Field1 = 'TV/PAL' then
            FID3Text.FMediaType := mtTV_PAL
          else
          if Field1 = 'TV/NTSC' then
            FID3Text.FMediaType := mtTV_NTSC
          else
          if Field1 = 'TV/SECAM' then
            FID3Text.FMediaType := mtTV_SECAM
          else
          if Field1 = 'VID' then
            FID3Text.FMediaType := mtVID
          else
          if Field1 = 'VID/PAL' then
            FID3Text.FMediaType := mtVID_PAL
          else
          if Field1 = 'VID/NTSC' then
            FID3Text.FMediaType := mtVID_NTSC
          else
          if Field1 = 'VID/SECAM' then
            FID3Text.FMediaType := mtVID_SECAM
          else
          if Field1 = 'VID/VHS' then
            FID3Text.FMediaType := mtVID_VHS
          else
          if Field1 = 'VID/SVHS' then
            FID3Text.FMediaType := mtVID_SVHS
          else
          if Field1 = 'VID/BETA' then
            FID3Text.FMediaType := mtVID_BETA
          else
          if Field1 = 'RAD' then
            FID3Text.FMediaType := mtRAD
          else
          if Field1 = 'RAD/FM' then
            FID3Text.FMediaType := mtRAD_FM
          else
          if Field1 = 'RAD/AM' then
            FID3Text.FMediaType := mtRAD_AM
          else
          if Field1 = 'RAD/LW' then
            FID3Text.FMediaType := mtRAD_LW
          else
          if Field1 = 'RAD/MW' then
            FID3Text.FMediaType := mtRAD_MW
          else
          if Field1 = 'TEL' then
            FID3Text.FMediaType := mtTEL
          else
          if Field1 = 'TEL/I' then
            FID3Text.FMediaType := mtTEL_ISDN
          else
          if Field1 = 'MC' then
            FID3Text.FMediaType := mtMC
          else
          if Field1 = 'MC/4' then
            FID3Text.FMediaType := mtMC_4
          else
          if Field1 = 'MC/9' then
            FID3Text.FMediaType := mtMC_9
          else
          if Field1 = 'MC/I' then
            FID3Text.FMediaType := mtMC_I
          else
          if Field1 = 'MC/II' then
            FID3Text.FMediaType := mtMC_II
          else
          if Field1 = 'MC/III' then
            FID3Text.FMediaType := mtMC_III
          else
          if Field1 = 'MC/IV' then
            FID3Text.FMediaType := mtMC_IV
          else
          if Field1 = 'REE' then
            FID3Text.FMediaType := mtREE
          else
          if Field1 = 'REE/9' then
            FID3Text.FMediaType := mtREE_9
          else
          if Field1 = 'REE/19' then
            FID3Text.FMediaType := mtREE_19
          else
          if Field1 = 'REE/38' then
            FID3Text.FMediaType := mtREE_38
          else
          if Field1 = 'REE/76' then
            FID3Text.FMediaType := mtREE_76
          else
          if Field1 = 'REE/I' then
            FID3Text.FMediaType := mtREE_I
          else
          if Field1 = 'REE/II' then
            FID3Text.FMediaType := mtREE_II
          else
          if Field1 = 'REE/III' then
            FID3Text.FMediaType := mtREE_III
          else
          if Field1 = 'REE/IV' then
            FID3Text.FMediaType := mtREE_IV
          else
            FID3Text.FMediaType := mtUNKNOWN;
        end;
      fiOrigAlbum:
        { The 'Original album/movie/show title' frame is intended for the title of
          the original recording (or FCurrentFrame of sound), if for example the music in
          the file should be a cover of a previously released song. }
        FID3Text.FOriginalTitle := Field1;
      fiOrigFileName:
        { The 'Original filename' frame contains the preferred filename for the file,
          since some media doesn't allow the desired length of the filename. The
          filename is case sensitive and includes its suffix. }
        FID3Text.FOriginalFileName := Field1;
      fiOrigLyricist:
        { The 'Original lyricist(s)/text writer(s)' frame is intended for the text
          writer(s) of the original recording, if for example the music in the file
          should be a cover of a previously released song. The text writers are
          seperated with the "/" character. }
        ExtractStrings(Field1, FID3Text.FOriginalLyricists);
      fiOrigArtist:
        { The 'Original artist(s)/performer(s)' frame is intended for the performer(s)
          of the original recording, if for example the music in the file should be
          a cover of a previously released song. The performers are seperated with
          the "/" character. }
        ExtractStrings(Field1, FID3Text.FOriginalArtists);
      fiOrigYear:
        { The 'Original release year' frame is intended for the year when the original
          recording, if for example the music in the file should be a cover of a
          previously released song, was released. The field is formatted as in the
          "TYER" frame. }
        FID3Text.FOriginalReleaseYear := Field1;
      fiFileOwner:
        { The 'File owner/licensee' frame contains the name of the owner or licensee
          of the file and it's contents. }
        FID3Text.FFileOwner := Field1;
      fiLeadArtist:
        { The 'Lead artist(s)/Lead performer(s)/Soloist(s)/Performing group' is used
          for the main artist(s). They are seperated with the "/" character. }
        ExtractStrings(Field1, FID3Text.FPerformers);
      fiBand:
        { The 'Band/Orchestra/Accompaniment' frame is used for additional information
          about the performers in the recording. }
        FID3Text.FBand := Field1;
      fiConductor:
        { The 'Conductor' frame is used for the name of the conductor. }
        FID3Text.FConductor := Field1;
      fiMixArtist:
        { The 'Interpreted, remixed, or otherwise modified by' frame contains more
          information about the people behind a remix and similar interpretations
          of another existing piece. }
        FID3Text.FModifiedBy := Field1;
      fiPartInSet:
        { The 'Part of a set' frame is a numeric string that describes which part
          of a set the audio came from. This frame is used if the FCurrentFrame described
          in the "TALB" frame is divided into several mediums, e.g. a double CD.
          The value may be extended with a "/" character and a numeric string
          containing the total number of parts in the set. E.g. "1/2". }
        FID3Text.FPartOf := Field1;
      fiPublisher:
        { The 'Publisher' frame simply contains the name of the label or publisher. }
        FID3Text.FPublisher := Field1;
      fiTrackNum:
        { The 'Track number/Position in set' frame is a numeric string containing
          the order number of the audio-file on its original recording. This may be
          extended with a "/" character and a numeric string containing the total
          numer of tracks/elements on the original recording. E.g. "4/9". }
        FID3Text.FTrackNumber := Field1;
      fiRecordingDates:
        { The 'Recording dates' frame is a intended to be used as complement to the
          "TYER", "TDAT" and "TIME" frames. E.g. "4th-7th June, 12th June" in combination
          with the "TYER" frame. }
        FID3Text.FRecordingDate := Field1;
      fiNetRadioStation:
        { The 'Internet radio station name' frame contains the name of the internet
          radio station from which the audio is streamed. }
        FID3Text.FInternetRadioName := Field1;
      fiNetRadioOwner:
        { The 'Internet radio station owner' frame contains the name of the owner
          of the internet radio station from which the audio is streamed. }
        FID3Text.FInternetRadioOwner := Field1;
      fiSize:
        { The 'Size' frame contains the size of the audiofile in bytes, excluding
          the ID3v2 tag, represented as a numeric string. }
        FID3Text.FSize := StrToInt(Field1);
      fiISRC:
        { The 'ISRC' frame should contain the International Standard Recording
          Code (ISRC) (12 characters). }
        FID3Text.FISRC := Field1;
      fiEncoderSettings:
        { The 'Software/Hardware and settings used for encoding' frame includes
          the used audio encoder and its settings when the file was encoded. Hardware
          refers to hardware encoders, not the computer on which a program was run. }
        FID3Text.FEncodingSoftware := Field1;
      fiYear:
        { The 'Year' frame is a numeric string with a year of the recording. This
          frames is always four characters long (until the year 10000) }
        FID3Text.FYear := Field1;
    end;
  end;
end;

procedure TJvID3v2.ReadWeb;
var
  S1, S2: string;
  SW1, SW2: WideString;
  IsUnicode: Boolean;
begin
  { With these frames dynamic data such as webpages with touring information, price
    information or plain ordinary news can be added to the tag. There may only be
    one URL link frame of its kind in an tag, except when stated otherwise in the
    frame description. If the textstring is followed by a termination ($00 (00))
    all the following information should be ignored and not be displayed. All URL
    link frame identifiers begins with "W". Only URL link frame identifiers begins
    with "W". }

  with FFrameStream do
  begin
    IsUnicode := ReadIsWideString;
    if FCurrentFrameID = fiWWWUSER then
    begin
      if IsUnicode then
      begin
        ReadWideString(SW1);
        ReadWideString(SW2);
        S1 := WideStringToStringEx(SW1, CP_ACP);
        S2 := WideStringToStringEx(SW2, CP_ACP);
      end
      else
      begin
        ReadString(S1);
        ReadString(S2);
      end;
      FUserDefinedWeb.AddItem(S1, S2);
    end
    else
    begin
      if IsUnicode then
      begin
        ReadWideString(SW1);
        S1 := WideStringToStringEx(SW1, CP_ACP);
      end
      else
        ReadString(S1);

      case FCurrentFrameID of
        fiWWWCommercialInfo:
          { The 'Commercial information' frame is a URL pointing at a webpage with
            information such as where the album can be bought. There may be more
            than one "WCOM" frame in a tag, but not with the same content. }
          FWeb.CommercialInfo := S1;
        fiWWWCopyright:
          { The 'Copyright/Legal information' frame is a URL pointing at a webpage
            where the terms of use and ownership of the file is described. }
          FWeb.LegalInfo := S1;
        fiWWWAudioFile:
          { The 'Official audio file webpage' frame is a URL pointing at a file specific
            webpage. }
          FWeb.OfficialAudio := S1;
        fiWWWArtist:
          { The 'Official artist/performer webpage' frame is a URL pointing at the
            artists official webpage. There may be more than one "WOAR" frame in a
            tag if the audio contains more than one performer, but not with the same
            content. }
          FWeb.OfficialArtist := S1;
        fiWWWAudioSource:
          { The 'Official audio FCurrentFrame webpage' frame is a URL pointing at the official
            webpage for the FCurrentFrame of the audio file, e.g. a movie. }
          FWeb.OfficialAudioSource := S1;
        fiWWWRadioPage:
          { The 'Official internet radio station homepage' contains a URL pointing
            at the homepage of the internet radio station. }
          FWeb.InternetRadioStation := S1;
        fiWWWPayment:
          { The 'Payment' frame is a URL pointing at a webpage that will handle the
            process of paying for this file. }
          FWeb.Payment := S1;
        fiWWWPublisher:
          { The 'Publishers official webpage' frame is a URL pointing at the official
            wepage for the publisher. }
          FWeb.Publishers := S1;
      end;
    end;
  end;
end;

procedure TJvID3v2.ResetProp;
begin
  FVersion := 0.0;
  FUnsynchronisation := False;
  FExperimental := False;
  FExtendedHeader := False;

  FID3Text.ResetFields;
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

procedure TJvID3v2.SetFileName(const Value: TFileName);
begin
  FTagSize := 0;
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== TJvID3Text =============================================================

constructor TJvID3Text.Create;
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

destructor TJvID3Text.Destroy;
begin
  FComposer.Free;
  FLyricists.Free;
  FLanguages.Free;
  FOriginalLyricists.Free;
  FOriginalArtists.Free;
  FPerformers.Free;
  inherited Destroy;
end;

procedure TJvID3Text.DummyProcedureFT(const Value: TJvID3FileType);
begin
  { Do nothing }
end;

procedure TJvID3Text.DummyProcedureInt(const Value: Integer);
begin
  { Do nothing }
end;

procedure TJvID3Text.DummyProcedureMT(const Value: TJvID3MediaType);
begin
  { Do nothing }
end;

procedure TJvID3Text.DummyProcedureStr(const Value: string);
begin
  { Do nothing }
end;

procedure TJvID3Text.ResetFields;
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
  FFileOwner := '';
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

//=== TJvID3Web ==============================================================

procedure TJvID3Web.ResetFields;
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
var
  Index: Byte;
begin
  inherited Create;
  for Index := Low(FPictures) to High(FPictures) do
    FPictures[Index] := TPicture.Create;
end;

destructor TJvIdPictures.Destroy;
var
  Index: Byte;
begin
  for Index := Low(FPictures) to High(FPictures) do
    FPictures[Index].Free;
  inherited Destroy;
end;

procedure TJvIdPictures.DummyProcedure(const Index: Integer;
  const Value: TPicture);
begin
  { Do nothing }
end;

function TJvIdPictures.GetPicture(const Index: Integer): TPicture;
begin
  Result := FPictures[Index];
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
var
  Index: Integer;
begin
  for Index := cJvID3LowIndex to cJvID3HighIndex do
  begin
    FPictures.GetPicture(Index).Assign(nil);
    FInfos.SetDescription(Index, '');
  end;
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

//=== TJvID3Owner ============================================================

procedure TJvID3Owner.ResetFields;
begin
  FSeller := '';
  FPrice := '';
  FDatePurchased := EncodeDate(1900, 1, 1);
end;

//=== TJvID3Popularimeter ====================================================

procedure TJvID3Popularimeter.ResetFields;
begin
  FRating := 0;
  FCounter := 0;
  FUserEmail := '';
end;

//=== TJvIdPicturesDesc ======================================================

procedure TJvIdPicturesDesc.DummyProcedure(const Index: Integer;
  const Value: string);
begin
  { Do nothing }
end;

function TJvIdPicturesDesc.GetDescription(const Index: Integer): string;
begin
  Result := FStrings[Index];
end;

procedure TJvIdPicturesDesc.SetDescription(const Index: Integer;
  const Value: string);
begin
  FStrings[Index] := Value;
end;

end.

