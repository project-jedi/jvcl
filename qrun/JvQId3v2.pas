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

The Original Code is: JvID3v2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQID3v2;

{$I jvcl.inc}

interface

uses
  Classes, QGraphics, QControls,
  JvQID3v2Types, JvQID3v2Base;

type
  TJvID3Persistent = class(TPersistent)
  private
    FController: TJvID3Controller;
  public
    constructor Create(AController: TJvID3Controller);
  end;

  TJvID3Text = class(TJvID3Persistent)
  private
    FDummyList: TStringList;
    function GetDateTime(const FrameID: Integer{TJvID3FrameID}): TDateTime;
    function GetList(const FrameID: Integer{TJvID3FrameID}): TStrings;
    function GetNumber(const FrameID: Integer{TJvID3FrameID}): Cardinal;
    function GetText(const FrameID: Integer{TJvID3FrameID}): string;
    procedure SetDateTime(const FrameID: Integer{TJvID3FrameID}; const Value: TDateTime);
    procedure SetList(const FrameID: Integer{TJvID3FrameID}; const Value: TStrings);
    procedure SetNumber(const FrameID: Integer{TJvID3FrameID}; const Value: Cardinal);
    procedure SetText(const FrameID: Integer{TJvID3FrameID}; const Value: string);
  public
    constructor Create(AController: TJvID3Controller);
    destructor Destroy; override;
  published
    { Do not store dummies }
    property Album: string index fiAlbum read GetText write SetText stored False;
    property AlbumSortOrder: string index fiAlbumSortOrder read GetText write SetText stored False;
    property Band: string index fiBand read GetText write SetText stored False;
    property BPM: Cardinal index fiBPM read GetNumber write SetNumber stored False;
    property Composer: TStrings index fiComposer read GetList write SetList stored False;
    property Conductor: string index fiConductor read GetText write SetText stored False;
    property ContentType: string index fiContentType read GetText write SetText stored False;
    property ContentGroup: string index fiContentGroup read GetText write SetText stored False;
    property Copyright: string index fiCopyright read GetText write SetText stored False;
    property Date: string index fiDate read GetText write SetText stored False;
    property EncodedBy: string index fiEncodedBy read GetText write SetText stored False;
    property EncoderSettings: string index fiEncoderSettings read GetText write SetText stored False;
    property EncodingTime: TDateTime index fiEncodingTime read GetDateTime write SetDateTime stored False;
    property FileOwner: string index fiFileOwner read GetText write SetText stored False;
    property FileType: string index fiFileType read GetText write SetText stored False;
    property InitialKey: string index fiInitialKey read GetText write SetText stored False;
    property ISRC: string index fiISRC read GetText write SetText stored False;
    property Language: TStrings index fiLanguage read GetList write SetList stored False;
    property LeadArtist: TStrings index fiLeadArtist read GetList write SetList stored False;
    property Lyricist: TStrings index fiLyricist read GetList write SetList stored False;
    property MediaType: string index fiMediaType read GetText write SetText stored False;
    property MixArtist: string index fiMixArtist read GetText write SetText stored False;
    property Mood: string index fiMood read GetText write SetText stored False;
    property NetRadioOwner: string index fiNetRadioOwner read GetText write SetText stored False;
    property NetRadioStation: string index fiNetRadioStation read GetText write SetText stored False;
    property OrigAlbum: string index fiOrigAlbum read GetText write SetText stored False;
    property OrigArtist: TStrings index fiOrigArtist read GetList write SetList stored False;
    property OrigFileName: string index fiOrigFileName read GetText write SetText stored False;
    property OrigLyricist: TStrings index fiOrigLyricist read GetList write SetList stored False;
    property OrigReleaseTime: TDateTime index fiOrigReleaseTime read GetDateTime write SetDateTime stored False;
    property OrigYear: Cardinal index fiOrigYear read GetNumber write SetNumber stored False;
    property PartInSet: string index fiPartInSet read GetText write SetText stored False;
    property PerformerSortOrder: string index fiPerformerSortOrder read GetText write SetText stored False;
    property PlaylistDelay: Cardinal index fiPlaylistDelay read GetNumber write SetNumber stored False;
    property ProducedNotice: string index fiProducedNotice read GetText write SetText stored False;
    property Publisher: string index fiPublisher read GetText write SetText stored False;
    property RecordingDates: string index fiRecordingDates read GetText write SetText stored False;
    property RecordingTime: TDateTime index fiRecordingTime read GetDateTime write SetDateTime stored False;
    property ReleaseTime: TDateTime index fiReleaseTime read GetDateTime write SetDateTime stored False;
    property SetSubTitle: string index fiSetSubTitle read GetText write SetText stored False;
    property Size: Cardinal index fiSize read GetNumber write SetNumber stored False;
    property SongLen: Cardinal index fiSongLen read GetNumber write SetNumber stored False;
    property SubTitle: string index fiSubTitle read GetText write SetText stored False;
    property TaggingTime: TDateTime index fiTaggingTime read GetDateTime write SetDateTime stored False;
    property Time: string index fiTime read GetText write SetText stored False;
    property Title: string index fiTitle read GetText write SetText stored False;
    property TitleSortOrder: string index fiTitleSortOrder read GetText write SetText stored False;
    property TrackNum: string index fiTrackNum read GetText write SetText stored False;
    property Year: Cardinal index fiYear read GetNumber write SetNumber stored False;
  end;

  TJvID3Web = class(TJvID3Persistent)
  private
    function GetText(const FrameID: Integer{TJvID3FrameID}): string;
    procedure SetText(const FrameID: Integer{TJvID3FrameID}; const Value: string);
  published
    { Do not store dummies }
    property Artist: string index fiWWWArtist read GetText write SetText stored False;
    property AudioFile: string index fiWWWAudioFile read GetText write SetText stored False;
    property AudioSource: string index fiWWWAudioSource read GetText write SetText stored False;
    property CommercialInfo: string index fiWWWCommercialInfo read GetText write SetText stored False;
    property Copyright: string index fiWWWCopyright read GetText write SetText stored False;
    property Payment: string index fiWWWPayment read GetText write SetText stored False;
    property Publisher: string index fiWWWPublisher read GetText write SetText stored False;
    property RadioPage: string index fiWWWRadioPage read GetText write SetText stored False;
  end;

  TJvID3UDText = class(TJvID3Persistent)
  private
    FDummyI: Integer;
    FItemIndex: Integer;
    function GetDescription: string;
    function GetItemCount: Integer;
    function GetItemIndex: Integer;
    function GetValue: string;
    procedure SetDescription(const Value: string);
    procedure SetItemIndex(const Value: Integer);
    procedure SetValue(const Value: string);
  public
    procedure Add(const ADescription, AValue: string);
  published
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    { Do not store dummies }
    property Description: string read GetDescription write SetDescription stored False;
    property Value: string read GetValue write SetValue stored False;
    property ItemCount: Integer read GetItemCount write FDummyI stored False;
  end;

  TJvID3UDUrl = class(TJvID3Persistent)
  private
    FItemIndex: Integer;
    FDummyI: Integer;
    function GetDescription: string;
    function GetItemCount: Integer;
    function GetItemIndex: Integer;
    function GetURL: string;
    procedure SetDescription(const Value: string);
    procedure SetItemIndex(const Value: Integer);
    procedure SetURL(const Value: string);
  public
    procedure Add(const ADescription, AURL: string);
  published
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    { Do not store dummies }
    property Description: string read GetDescription write SetDescription stored False;
    property URL: string read GetURL write SetURL stored False;
    property ItemCount: Integer read GetItemCount write FDummyI stored False;
  end;

  TJvID3Pictures = class(TJvID3Persistent)
  private
    FPictures: array[TJvID3PictureType] of TPicture;
    FUpdating: Boolean;
    function GetPicture(const AType: Integer{TJvID3PictureType}): TPicture;
    procedure SetPicture(const AType: Integer{TJvID3PictureType}; const Value: TPicture);
    procedure PictureChanged(Sender: TObject);
    procedure PictureToFrame(const AType: TJvID3PictureType);
    procedure RetrievePictures;
    procedure RemovePictures;
  public
    constructor Create(AController: TJvID3Controller); virtual;
    destructor Destroy; override;
  published
    property Other: TPicture index ptOther read GetPicture write SetPicture stored False;
    property FileIcon: TPicture index ptFileIcon read GetPicture write SetPicture stored False;
    property OtherFileIcon: TPicture index ptOtherFileIcon read GetPicture write SetPicture stored False;
    property CoverFront: TPicture index ptCoverFront read GetPicture write SetPicture stored False;
    property CoverBack: TPicture index ptCoverBack read GetPicture write SetPicture stored False;
    property LeafletPage: TPicture index ptLeafletPage read GetPicture write SetPicture stored False;
    property Media: TPicture index ptMedia read GetPicture write SetPicture stored False;
    property LeadArtist: TPicture index ptLeadArtist read GetPicture write SetPicture stored False;
    property Artist: TPicture index ptArtist read GetPicture write SetPicture stored False;
    property Conductor: TPicture index ptConductor read GetPicture write SetPicture stored False;
    property Band: TPicture index ptBand read GetPicture write SetPicture stored False;
    property Composer: TPicture index ptComposer read GetPicture write SetPicture stored False;
    property Lyricist: TPicture index ptLyricist read GetPicture write SetPicture stored False;
    property RecordingLocation: TPicture index ptRecordingLocation read GetPicture write SetPicture stored False;
    property DuringRecording: TPicture index ptDuringRecording read GetPicture write SetPicture stored False;
    property DuringPerformance: TPicture index ptDuringPerformance read GetPicture write SetPicture stored False;
    property MovieVideoScreenCapture: TPicture index ptMovieVideoScreenCapture read GetPicture write SetPicture stored
      False;
    property BrightColouredFish: TPicture index ptBrightColouredFish read GetPicture write SetPicture stored False;
    property Illustration: TPicture index ptIllustration read GetPicture write SetPicture stored False;
    property BandLogotype: TPicture index ptBandLogotype read GetPicture write SetPicture stored False;
    property PublisherLogotype: TPicture index ptPublisherLogotype read GetPicture write SetPicture stored False;
  end;

  TJvID3PicturesDesc = class(TJvID3Persistent)
  private
    function GetText(const AType: Integer{TJvID3PictureType}): string;
    procedure SetText(const AType: Integer{TJvID3PictureType}; const Value: string);
  published
    property Other: string index ptOther read GetText write SetText stored False;
    property FileIcon: string index ptFileIcon read GetText write SetText stored False;
    property OtherFileIcon: string index ptOtherFileIcon read GetText write SetText stored False;
    property CoverFront: string index ptCoverFront read GetText write SetText stored False;
    property CoverBack: string index ptCoverBack read GetText write SetText stored False;
    property LeafletPage: string index ptLeafletPage read GetText write SetText stored False;
    property Media: string index ptMedia read GetText write SetText stored False;
    property LeadArtist: string index ptLeadArtist read GetText write SetText stored False;
    property Artist: string index ptArtist read GetText write SetText stored False;
    property Conductor: string index ptConductor read GetText write SetText stored False;
    property Band: string index ptBand read GetText write SetText stored False;
    property Composer: string index ptComposer read GetText write SetText stored False;
    property Lyricist: string index ptLyricist read GetText write SetText stored False;
    property RecordingLocation: string index ptRecordingLocation read GetText write SetText stored False;
    property DuringRecording: string index ptDuringRecording read GetText write SetText stored False;
    property DuringPerformance: string index ptDuringPerformance read GetText write SetText stored False;
    property MovieVideoScreenCapture: string index ptMovieVideoScreenCapture read GetText write SetText stored False;
    property BrightColouredFish: string index ptBrightColouredFish read GetText write SetText stored False;
    property Illustration: string index ptIllustration read GetText write SetText stored False;
    property BandLogotype: string index ptBandLogotype read GetText write SetText stored False;
    property PublisherLogotype: string index ptPublisherLogotype read GetText write SetText stored False;
  end;

  TJvID3Images = class(TJvID3Persistent)
  private
    FPictures: TJvID3Pictures;
    FInfos: TJvID3PicturesDesc;
  public
    constructor Create(AController: TJvID3Controller);
    destructor Destroy; override;
  published
    property Pictures: TJvID3Pictures read FPictures;
    property Infos: TJvID3PicturesDesc read FInfos;
  end;

  TJvID3Ipl = class(TJvID3Persistent)
  private
    FDummyI: Integer;
    FItemIndex: Integer;
    function GetItemCount: Integer;
    function GetJob: string;
    function GetPerson: string;
    procedure SetItemIndex(const Value: Integer);
    procedure SetJob(const Value: string);
    procedure SetPerson(const Value: string);
  published
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    { Do not store dummies }
    property Job: string read GetJob write SetJob stored False;
    property Person: string read GetPerson write SetPerson stored False;
    property ItemCount: Integer read GetItemCount write FDummyI stored False;
  end;

  TJvID3Owner = class(TJvID3Persistent)
  private
    function GetDatePurchased: TDateTime;
    function GetPrice: string;
    function GetSeller: string;
    procedure SetDatePurchased(const Value: TDateTime);
    procedure SetPrice(const Value: string);
    procedure SetSeller(const Value: string);
  published
    { Do not store dummies }
    property Price: string read GetPrice write SetPrice stored False;
    property DatePurchased: TDateTime read GetDatePurchased write SetDatePurchased stored False;
    property Seller: string read GetSeller write SetSeller stored False;
  end;

  TJvID3Popularimeter = class(TJvID3Persistent)
  private
    function GetCounter: Cardinal;
    function GetRating: Byte;
    function GetEMailAddress: string;
    procedure SetCounter(const Value: Cardinal);
    procedure SetRating(const Value: Byte);
    procedure SetEMailAddress(const Value: string);
  published
    { Do not store dummies }
    property EMailAddress: string read GetEMailAddress write SetEMailAddress stored False;
    property Rating: Byte read GetRating write SetRating stored False;
    property Counter: Cardinal read GetCounter write SetCounter stored False;
  end;

  TJvID3v2 = class(TJvID3Controller)
  private
    FID3Text: TJvID3Text;
    FWeb: TJvID3Web;
    FUserDefinedText: TJvID3UDText;
    FUserDefinedWeb: TJvID3UDUrl;
    FInvolvedPeople: TJvID3Ipl;
    FImages: TJvID3Images;
    FOwner: TJvID3Owner;
    FPopularimeter: TJvID3Popularimeter;
    function GetPlayCounter: Cardinal;
    procedure SetPlayCounter(const Value: Cardinal);
  protected
    procedure ActiveChanged(Sender: TObject; Activated: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Header;
    property ExtendedHeader;
  published
    { Do not store dummies }
    property Texts: TJvID3Text read FID3Text;
    property UserDefinedText: TJvID3UDText read FUserDefinedText;
    property Web: TJvID3Web read FWeb;
    property UserDefinedWeb: TJvID3UDUrl read FUserDefinedWeb;
    property InvolvedPeople: TJvID3Ipl read FInvolvedPeople;
    property Images: TJvID3Images read FImages;
    property PlayCounter: Cardinal read GetPlayCounter write SetPlayCounter stored False;
    property Owner: TJvID3Owner read FOwner;
    property Popularimeter: TJvID3Popularimeter read FPopularimeter;
    property Version;
    property FileInfo;
  end;

implementation

uses
  SysUtils, Math,
  JvQResources;

//=== Local procedures =======================================================

function ExtractMIMETypeFromClassName(AClassName: string): string;
begin
  AClassName := AnsiLowerCase(AClassName);

  if (Pos('jpg', AClassName) > 0) or (Pos('jpeg', AClassName) > 0) then
    Result := 'image/jpeg'
  else
  if (Pos('bmp', AClassName) > 0) or (Pos('bitmap', AClassName) > 0) then
    Result := 'image/bitmap'
  else
  if (Pos('gif', AClassName) > 0) then
    Result := 'image/gif'
  else
    Result := 'image/';
end;

//=== { TJvID3Images } =======================================================

constructor TJvID3Images.Create(AController: TJvID3Controller);
begin
  inherited Create(AController);
  FPictures := TJvID3Pictures.Create(AController);
  FInfos := TJvID3PicturesDesc.Create(AController);
end;

destructor TJvID3Images.Destroy;
begin
  FPictures.Free;
  FInfos.Free;
  inherited Destroy;
end;

//=== { TJvID3Ipl } ==========================================================

function TJvID3Ipl.GetItemCount: Integer;
var
  Frame: TJvID3DoubleListFrame;
begin
  if not FController.Active then
    Result := 0
  else
  begin
    Frame := TJvID3DoubleListFrame.Find(FController, fiInvolvedPeople);
    if Assigned(Frame) then
      Result := Frame.List.Count
    else
      Result := 0;
  end;
end;

function TJvID3Ipl.GetJob: string;
var
  Frame: TJvID3DoubleListFrame;
begin
  if ItemIndex < 0 then
    Result := ''
  else
  begin
    Frame := TJvID3DoubleListFrame.Find(FController, fiInvolvedPeople);
    if Assigned(Frame) and (ItemIndex < Frame.List.Count) then
      Result := Frame.Values[ItemIndex]
    else
      Result := '';
  end;
end;

function TJvID3Ipl.GetPerson: string;
var
  Frame: TJvID3DoubleListFrame;
begin
  if ItemIndex < 0 then
    Result := ''
  else
  begin
    Frame := TJvID3DoubleListFrame.Find(FController, fiInvolvedPeople);
    if Assigned(Frame) and (ItemIndex < Frame.List.Count) then
      Result := Frame.List.Names[ItemIndex]
    else
      Result := '';
  end;
end;

procedure TJvID3Ipl.SetItemIndex(const Value: Integer);
begin
  if Value <> FItemIndex then
  begin
    FItemIndex := Min(Value, ItemCount - 1);
  end;
end;

procedure TJvID3Ipl.SetJob(const Value: string);
var
  LPerson: string;
  Frame: TJvID3DoubleListFrame;
begin
  if FController.Active and (ItemIndex >= 0) then
  begin
    Frame := TJvID3DoubleListFrame.FindOrCreate(FController, fiInvolvedPeople);
    if (0 <= ItemIndex) and (ItemIndex < Frame.List.Count) then
    begin
      LPerson := Frame.List.Names[ItemIndex];
      Frame.List[ItemIndex] := Format('%s=%s', [LPerson, Value]);
    end;
  end;
end;

procedure TJvID3Ipl.SetPerson(const Value: string);
var
  LJob: string;
  Frame: TJvID3DoubleListFrame;
begin
  if FController.Active and (ItemIndex >= 0) then
  begin
    Frame := TJvID3DoubleListFrame.FindOrCreate(FController, fiInvolvedPeople);
    if (0 <= ItemIndex) and (ItemIndex < Frame.List.Count) then
    begin
      LJob := Frame.Values[ItemIndex];
      Frame.List[ItemIndex] := Format('%s=%s', [Value, LJob]);
    end;
  end;
end;

//=== { TJvID3Owner } ========================================================

function TJvID3Owner.GetDatePurchased: TDateTime;
var
  Frame: TJvID3OwnershipFrame;
begin
  Frame := TJvID3OwnershipFrame.Find(FController);
  if Assigned(Frame) then
    Result := Frame.DateOfPurch
  else
    Result := 0;
end;

function TJvID3Owner.GetPrice: string;
var
  Frame: TJvID3OwnershipFrame;
begin
  Frame := TJvID3OwnershipFrame.Find(FController);
  if Assigned(Frame) then
    Result := Frame.PricePayed
  else
    Result := '';
end;

function TJvID3Owner.GetSeller: string;
var
  Frame: TJvID3OwnershipFrame;
begin
  Frame := TJvID3OwnershipFrame.Find(FController);
  if Assigned(Frame) then
    Result := Frame.Seller
  else
    Result := '';
end;

procedure TJvID3Owner.SetDatePurchased(const Value: TDateTime);
begin
  if FController.Active then
    TJvID3OwnershipFrame.FindOrCreate(FController).DateOfPurch := Value;
end;

procedure TJvID3Owner.SetPrice(const Value: string);
begin
  if FController.Active then
    TJvID3OwnershipFrame.FindOrCreate(FController).PricePayed := Value;
end;

procedure TJvID3Owner.SetSeller(const Value: string);
begin
  if FController.Active then
    TJvID3OwnershipFrame.FindOrCreate(FController).Seller := Value;
end;

//=== { TJvID3Persistent } ===================================================

constructor TJvID3Persistent.Create(AController: TJvID3Controller);
begin
  inherited Create;
  FController := AController;
end;

//=== { TJvID3Pictures } =====================================================

constructor TJvID3Pictures.Create(AController: TJvID3Controller);
var
  Index: TJvID3PictureType;
begin
  inherited Create(AController);

  for Index := Low(TJvID3PictureType) to High(TJvID3PictureType) do
  begin
    FPictures[Index] := TPicture.Create;
    FPictures[Index].OnChange := PictureChanged;
  end;
end;

destructor TJvID3Pictures.Destroy;
var
  Index: TJvID3PictureType;
begin
  for Index := Low(TJvID3PictureType) to High(TJvID3PictureType) do
    FPictures[Index].Free;
  inherited Destroy;
end;

function TJvID3Pictures.GetPicture(const AType: Integer{TJvID3PictureType}): TPicture;
begin
  Result := FPictures[TJvID3PictureType(AType)];
end;

procedure TJvID3Pictures.PictureChanged(Sender: TObject);
var
  Index: TJvID3PictureType;
begin
  if FUpdating then
    Exit;

  for Index := Low(TJvID3PictureType) to High(TJvID3PictureType) do
    if FPictures[Index] = Sender then
    begin
      PictureToFrame(Index);
      Exit;
    end;
end;

procedure TJvID3Pictures.PictureToFrame(const AType: TJvID3PictureType);
var
  Frame: TJvID3PictureFrame;
begin
  if not FController.Active then
    Exit;

  Frame := TJvID3PictureFrame.FindOrCreate(FController, AType);
  Frame.Assign(FPictures[AType]);

  { Borland has made it hard for us to determine the type of picture; let's
    just look at the Picture.Graphic classname :) This is no way a reliable
    method thus I don't recommend using TJvID3v2 for pictures }

  Frame.MIMEType := ExtractMIMETypeFromClassName(FPictures[AType].Graphic.ClassName);
end;

procedure TJvID3Pictures.RemovePictures;
var
  Index: TJvID3PictureType;
begin
  FUpdating := True;
  try
    for Index := Low(TJvID3PictureType) to High(TJvID3PictureType) do
      FPictures[Index].Assign(nil);
  finally
    FUpdating := False;
  end;
end;

procedure TJvID3Pictures.RetrievePictures;
var
  Frame: TJvID3PictureFrame;
  Index: TJvID3PictureType;
begin
  FUpdating := True;
  try
    for Index := Low(TJvID3PictureType) to High(TJvID3PictureType) do
    begin
      Frame := TJvID3PictureFrame.Find(FController, Index);
      FPictures[Index].Assign(Frame);
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TJvID3Pictures.SetPicture(const AType: Integer{TJvID3PictureType};
  const Value: TPicture);
begin
  FPictures[TJvID3PictureType(AType)].Assign(Value);
  //ChangePicture(AType);
end;

//=== { TJvID3PicturesDesc } =================================================

function TJvID3PicturesDesc.GetText(const AType: Integer{TJvID3PictureType}): string;
var
  Frame: TJvID3PictureFrame;
begin
  Frame := TJvID3PictureFrame.Find(FController, TJvID3PictureType(AType));
  if Assigned(Frame) then
    Result := Frame.Description
  else
    Result := '';
end;

procedure TJvID3PicturesDesc.SetText(const AType: Integer{TJvID3PictureType};
  const Value: string);
begin
  if FController.Active then
    TJvID3PictureFrame.FindOrCreate(FController, TJvID3PictureType(AType)).Description := Value;
end;

//=== { TJvID3Popularimeter } ================================================

function TJvID3Popularimeter.GetCounter: Cardinal;
var
  Frame: TJvID3PopularimeterFrame;
begin
  Frame := TJvID3PopularimeterFrame.Find(FController);
  if Assigned(Frame) then
    Result := Frame.Counter
  else
    Result := 0;
end;

function TJvID3Popularimeter.GetEMailAddress: string;
var
  Frame: TJvID3PopularimeterFrame;
begin
  Frame := TJvID3PopularimeterFrame.Find(FController);
  if Assigned(Frame) then
    Result := Frame.EMailAddress
  else
    Result := '';
end;

function TJvID3Popularimeter.GetRating: Byte;
var
  Frame: TJvID3PopularimeterFrame;
begin
  Frame := TJvID3PopularimeterFrame.Find(FController);
  if Assigned(Frame) then
    Result := Frame.Rating
  else
    Result := 0;
end;

procedure TJvID3Popularimeter.SetCounter(const Value: Cardinal);
begin
  if FController.Active then
    TJvID3PopularimeterFrame.FindOrCreate(FController).Counter := Value;
end;

procedure TJvID3Popularimeter.SetEMailAddress(const Value: string);
begin
  if FController.Active then
    TJvID3PopularimeterFrame.FindOrCreate(FController).EMailAddress := Value;
end;

procedure TJvID3Popularimeter.SetRating(const Value: Byte);
begin
  if FController.Active then
    TJvID3PopularimeterFrame.FindOrCreate(FController).Rating := Value;
end;

//=== { TJvID3Text } =========================================================

constructor TJvID3Text.Create(AController: TJvID3Controller);
begin
  inherited Create(AController);
  FDummyList := TStringList.Create;
end;

destructor TJvID3Text.Destroy;
begin
  FDummyList.Free;
  inherited Destroy;
end;

function TJvID3Text.GetDateTime(const FrameID: Integer{TJvID3FrameID}): TDateTime;
var
  Frame: TJvID3TimestampFrame;
begin
  Frame := TJvID3TimestampFrame.Find(FController, TJvID3FrameID(FrameID));
  if Assigned(Frame) then
    Result := Frame.Value
  else
    Result := 0;
end;

function TJvID3Text.GetList(const FrameID: Integer{TJvID3FrameID}): TStrings;
begin
  if FController.Active then
    Result := TJvID3SimpleListFrame.FindOrCreate(FController, TJvID3FrameID(FrameID)).List
  else
  begin
    Result := FDummyList;
    Result.Clear;
  end;
end;

function TJvID3Text.GetNumber(const FrameID: Integer{TJvID3FrameID}): Cardinal;
var
  Frame: TJvID3NumberFrame;
begin
  Frame := TJvID3NumberFrame.Find(FController, TJvID3FrameID(FrameID));
  if Assigned(Frame) then
    Result := Frame.Value
  else
    Result := 0;
end;

function TJvID3Text.GetText(const FrameID: Integer{TJvID3FrameID}): string;
var
  Frame: TJvID3TextFrame;
begin
  Frame := TJvID3TextFrame.Find(FController, TJvID3FrameID(FrameID));
  if Assigned(Frame) then
    Result := Frame.Text
  else
    Result := '';
end;

procedure TJvID3Text.SetDateTime(const FrameID: Integer{TJvID3FrameID};
  const Value: TDateTime);
begin
  if FController.Active then
    TJvID3TimestampFrame.FindOrCreate(FController, TJvID3FrameID(FrameID)).Value := Value;
end;

procedure TJvID3Text.SetList(const FrameID: Integer{TJvID3FrameID};
  const Value: TStrings);
begin
  if FController.Active then
    TJvID3SimpleListFrame.FindOrCreate(FController, TJvID3FrameID(FrameID)).List.Assign(Value);
end;

procedure TJvID3Text.SetNumber(const FrameID: Integer{TJvID3FrameID};
  const Value: Cardinal);
begin
  if FController.Active then
    TJvID3NumberFrame.FindOrCreate(FController, TJvID3FrameID(FrameID)).Value := Value;
end;

procedure TJvID3Text.SetText(const FrameID: Integer{TJvID3FrameID}; const Value: string);
begin
  if FController.Active then
    TJvID3TextFrame.FindOrCreate(FController, TJvID3FrameID(FrameID)).Text := Value;
end;

//=== { TJvID3UDText } =======================================================

procedure TJvID3UDText.Add(const ADescription, AValue: string);
begin
  if not Assigned(FController) then
    ID3Error(RsEID3NoController);

  with TJvID3UserFrame(FController.AddFrame(fiUserText)) do
  begin
    Description := ADescription;
    Value := AValue;
  end;
end;

function TJvID3UDText.GetDescription: string;
var
  Frame: TJvID3UserFrame;
begin
  if ItemIndex < 0 then
    Result := ''
  else
  begin
    Frame := TJvID3UserFrame.Find(FController, ItemIndex);
    if Assigned(Frame) then
      Result := Frame.Description
    else
      Result := '';
  end;
end;

function TJvID3UDText.GetItemCount: Integer;
begin
  if not FController.Active then
    Result := 0
  else
    Result := FController.GetFrameCountFor(fiUserText);
end;

function TJvID3UDText.GetItemIndex: Integer;
begin
  if not FController.Active then
    FItemIndex := -1
  else
    FItemIndex := Min(FItemIndex, ItemCount - 1);
  Result := FItemIndex;
end;

function TJvID3UDText.GetValue: string;
var
  Frame: TJvID3UserFrame;
begin
  if ItemIndex < 0 then
    Result := ''
  else
  begin
    Frame := TJvID3UserFrame.Find(FController, ItemIndex);
    if Assigned(Frame) then
      Result := Frame.Value
    else
      Result := '';
  end;
end;

procedure TJvID3UDText.SetDescription(const Value: string);
begin
  if FController.Active and (ItemIndex >= 0) and (ItemIndex < ItemCount) then
    TJvID3UserFrame.Find(FController, ItemIndex).Description := Value;
end;

procedure TJvID3UDText.SetItemIndex(const Value: Integer);
begin
  if Value <> FItemIndex then
    FItemIndex := Min(Value, ItemCount - 1);
end;

procedure TJvID3UDText.SetValue(const Value: string);
begin
  if FController.Active and (ItemIndex >= 0) and (ItemIndex < ItemCount) then
    TJvID3UserFrame.Find(FController, ItemIndex).Value := Value;
end;

//=== { TJvID3UDUrl } ========================================================

procedure TJvID3UDUrl.Add(const ADescription, AURL: string);
begin
  if not Assigned(FController) then
    ID3Error(RsEID3NoController);

  with TJvID3URLUserFrame(FController.AddFrame(fiWWWUser)) do
  begin
    Description := ADescription;
    URL := AURL;
  end;
end;

function TJvID3UDUrl.GetDescription: string;
var
  Frame: TJvID3URLUserFrame;
begin
  if ItemIndex < 0 then
    Result := ''
  else
  begin
    Frame := TJvID3URLUserFrame.Find(FController, ItemIndex);
    if Assigned(Frame) then
      Result := Frame.Description
    else
      Result := '';
  end;
end;

function TJvID3UDUrl.GetItemCount: Integer;
begin
  if not FController.Active then
    Result := 0
  else
    Result := FController.GetFrameCountFor(fiWWWUser);
end;

function TJvID3UDUrl.GetItemIndex: Integer;
begin
  if not FController.Active then
    FItemIndex := -1
  else
    FItemIndex := Min(FItemIndex, ItemCount - 1);
  Result := FItemIndex;
end;

function TJvID3UDUrl.GetURL: string;
var
  Frame: TJvID3URLUserFrame;
begin
  if ItemIndex < 0 then
    Result := ''
  else
  begin
    Frame := TJvID3URLUserFrame.Find(FController, ItemIndex);
    if Assigned(Frame) then
      Result := Frame.URL
    else
      Result := '';
  end;
end;

procedure TJvID3UDUrl.SetDescription(const Value: string);
begin
  if FController.Active and (ItemIndex >= 0) then
    TJvID3URLUserFrame.Find(FController, ItemIndex).Description := Value;
end;

procedure TJvID3UDUrl.SetItemIndex(const Value: Integer);
begin
  if Value <> FItemIndex then
    FItemIndex := Min(Value, ItemCount - 1);
end;

procedure TJvID3UDUrl.SetURL(const Value: string);
begin
  if FController.Active and (ItemIndex >= 0) then
    TJvID3URLUserFrame.Find(FController, ItemIndex).URL := Value;
end;

//=== { TJvID3v2 } ===========================================================

constructor TJvID3v2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RegisterClient(Self, ActiveChanged);

  FID3Text := TJvID3Text.Create(Self);
  FWeb := TJvID3Web.Create(Self);
  FUserDefinedText := TJvID3UDText.Create(Self);
  FUserDefinedWeb := TJvID3UDUrl.Create(Self);
  FInvolvedPeople := TJvID3Ipl.Create(Self);
  FImages := TJvID3Images.Create(Self);
  FOwner := TJvID3Owner.Create(Self);
  FPopularimeter := TJvID3Popularimeter.Create(Self);

  { This ensures that possible unicode tags will be translated to ansi }
  WriteEncodingAs := ifeISO_8859_1;
  ReadEncodingAs := ifeISO_8859_1;

  Options := [coAutoCorrect, coRemoveEmptyFrames];
end;

destructor TJvID3v2.Destroy;
begin
  UnRegisterClient(Self);

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

procedure TJvID3v2.ActiveChanged(Sender: TObject; Activated: Boolean);
begin
  if Activated then
    FImages.Pictures.RetrievePictures
  else
    FImages.Pictures.RemovePictures;
end;

function TJvID3v2.GetPlayCounter: Cardinal;
var
  Frame: TJvID3PlayCounterFrame;
begin
  Frame := TJvID3PlayCounterFrame.Find(Self);
  if Assigned(Frame) then
    Result := Frame.Counter
  else
    Result := 0;
end;

procedure TJvID3v2.SetPlayCounter(const Value: Cardinal);
begin
  if Active then
    TJvID3PlayCounterFrame.FindOrCreate(Self).Counter := Value;
end;

//=== { TJvID3Web } ==========================================================

function TJvID3Web.GetText(const FrameID: Integer{TJvID3FrameID}): string;
var
  Frame: TJvID3URLFrame;
begin
  Frame := TJvID3URLFrame.Find(FController, TJvID3FrameID(FrameID));
  if Assigned(Frame) then
    Result := Frame.URL
  else
    Result := '';
end;

procedure TJvID3Web.SetText(const FrameID: Integer{TJvID3FrameID}; const Value: string);
begin
  if FController.Active then
    TJvID3URLFrame.FindOrCreate(FController, TJvID3FrameID(FrameID)).URL := Value;
end;

end.
