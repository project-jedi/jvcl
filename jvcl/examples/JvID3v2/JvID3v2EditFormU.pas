{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvID3v2EditFormU;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnList, StdCtrls,
  JvID3v2Base, JvId3v2, ExtCtrls, JvComponent, ImgList, ToolWin;

type
  TJvID3v2EditForm = class(TForm)
    PageControl1: TPageControl;
    lsbNavigator: TListBox;
    ToolBar1: TToolBar;
    tshWinampTags: TTabSheet;
    tshLyrics: TTabSheet;
    tshPictures: TTabSheet;
    lblTitle: TLabel;
    lblArtist: TLabel;
    lblAlbum: TLabel;
    lblYear: TLabel;
    lblComposer: TLabel;
    lblOrigArtist: TLabel;
    lblCopyright: TLabel;
    lblURL: TLabel;
    lblEncodedBy: TLabel;
    edtTitle: TEdit;
    edtArtist: TEdit;
    edtAlbum: TEdit;
    edtYear: TEdit;
    edtComposer: TEdit;
    edtOrigArtist: TEdit;
    edtCopyright: TEdit;
    edtURL: TEdit;
    edtEncodedBy: TEdit;
    cmbGenre: TComboBox;
    lblGenre: TLabel;
    memComment: TMemo;
    lblComment: TLabel;
    acl16: TActionList;
    iml16: TImageList;
    actOK: TAction;
    actCancel: TAction;
    actRemove: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    JvID3v21: TJvID3v2;
    lblLanguage: TLabel;
    cmbLanguage: TComboBox;
    memLyrics: TMemo;
    lblDescription: TLabel;
    lblWriter: TLabel;
    edtDescription: TEdit;
    edtWriter: TEdit;
    lsvPictures: TListView;
    imgPicture: TImage;
    actAddPicture: TAction;
    actDeletePicture: TAction;
    actSavePicture: TAction;
    lblPictureName: TLabel;
    lblPictureType: TLabel;
    edtPictureName: TEdit;
    cmbPictureType: TComboBox;
    tshAllFrames: TTabSheet;
    lsvAllFrames: TListView;
    btnChange: TButton;
    actChangePicture: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    actCopyTov1: TAction;
    actCopyFromv1: TAction;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure actAddPictureExecute(Sender: TObject);
    procedure actDeletePictureExecute(Sender: TObject);
    procedure actSavePictureExecute(Sender: TObject);
    procedure lsvPicturesClick(Sender: TObject);
    procedure lsbNavigatorClick(Sender: TObject);
    procedure actChangePictureExecute(Sender: TObject);
    procedure ItemSelected(Sender: TObject);
    procedure actCopyTov1Execute(Sender: TObject);
    procedure actCopyFromv1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lsvAllFramesInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
  private
    FTagDeleted: Boolean;
  protected
    procedure Init;
    procedure Final;

    procedure InitAllFramesTab;

    procedure TagToCtrls;
    procedure CtrlsToTag;
    procedure FillPictureTypes(Strings: TStrings);

    class function Instance: TJvID3v2EditForm;
  public
    class function Execute(const AFileName: string): Boolean;
  end;

implementation

uses
  ExtDlgs,
  JvID3v2Types;

{$R *.dfm}

var
  CFrameDescriptions: array[TJvID3FrameID] of string = (
    'Error', {fiErrorFrame}
    'Padding', {fiPaddingFrame}
    'No known frame', {fiNoFrame}
    'Audio encryption', {fiAudioCrypto}
    'Attached picture', {fiPicture}
    'Audio seek point index', {fiAudioSeekPoint}
    'Comments', {fiComment}
    'Commercial frame', {fiCommercial}
    'Encryption method registration', {fiCryptoReg}
    'Equalisation (2)', {fiEqualization2}
    'Equalization', {fiEqualization}
    'Event timing codes', {fiEventTiming}
    'General encapsulated object', {fiGeneralObject}
    'Group identification registration', {fiGroupingReg}
    'Involved people list', {fiInvolvedPeople}
    'Linked information', {fiLinkedInfo}
    'Music CD identifier', {fiCDID}
    'MPEG location lookup table', {fiMPEGLookup}
    'Ownership frame', {fiOwnership}
    'Private frame', {fiPrivate}
    'Play counter', {fiPlayCounter}
    'Popularimeter', {fiPopularimeter}
    'Position synchronisation frame', {fiPositionsync}
    'Recommended buffer size', {fiBufferSize}
    'Relative volume adjustment (2)', {fiVolumeAdj2}
    'Relative volume adjustment', {fiVolumeAdj}
    'Reverb', {fiReverb}
    'Seek frame', {fiSeekFrame}
    'Signature frame', {fiSignature}
    'Synchronized lyric/text', {fiSyncedLyrics}
    'Synchronized tempo codes', {fiSyncedTempo}
    'Album/Movie/Show title', {fiAlbum}
    'BPM (beats per minute)', {fiBPM}
    'Composer', {fiComposer}
    'Content type', {fiContentType}
    'Copyright message', {fiCopyright}
    'Date', {fiDate}
    'Encoding time', {fiEncodingTime}
    'Playlist delay', {fiPlaylistDelay}
    'Original release time', {fiOrigReleaseTime}
    'Recording time', {fiRecordingTime}
    'Release time', {fiReleaseTime}
    'Tagging time', {fiTaggingTime}
    'Involved people list', {fiInvolvedPeople2}
    'Encoded by', {fiEncodedBy}
    'Lyricist/Text writer', {fiLyricist}
    'File type', {fiFileType}
    'Time', {fiTime}
    'Content group description', {fiContentGroup}
    'Title/songname/content description', {fiTitle}
    'Subtitle/Description refinement', {fiSubTitle}
    'Initial key', {fiInitialKey}
    'Language(s)', {fiLanguage}
    'Length', {fiSongLen}
    'Musician credits list', {fiMusicianCreditList}
    'Media type', {fiMediaType}
    'Mood', {fiMood}
    'Original album/movie/show title', {fiOrigAlbum}
    'Original filename', {fiOrigFileName}
    'Original lyricist(s)/text writer(s)', {fiOrigLyricist}
    'Original artist(s)/performer(s)', {fiOrigArtist}
    'Original release year', {fiOrigYear}
    'File owner/licensee', {fiFileOwner}
    'Lead performer(s)/Soloist(s)', {fiLeadArtist}
    'Band/orchestra/accompaniment', {fiBand}
    'Conductor/performer refinement', {fiConductor}
    'Interpreted, remixed, or otherwise modified by', {fiMixArtist}
    'Part of a set', {fiPartInSet}
    'Produced notice', {fiProducedNotice}
    'Publisher', {fiPublisher}
    'Track number/Position in set', {fiTrackNum}
    'Recording dates', {fiRecordingDates}
    'Internet radio station name', {fiNetRadioStation}
    'Internet radio station owner', {fiNetRadioOwner}
    'Size', {fiSize}
    'Album sort order', {fiAlbumSortOrder}
    'Performer sort order', {fiPerformerSortOrder}
    'Title sort order', {fiTitleSortOrder}
    'ISRC (international standard recording code)', {fiISRC}
    'Software/Hardware and settings used for encoding', {fiEncoderSettings}
    'Set subtitle', {fiSetSubTitle}
    'User defined text information', {fiUserText}
    'Year', {fiYear}
    'Unique file identifier', {fiUniqueFileID}
    'Terms of use', {fiTermsOfUse}
    'Unsynchronized lyric/text transcription', {fiUnsyncedLyrics}
    'Commercial information', {fiWWWCommercialInfo}
    'Copyright/Legal information', {fiWWWCopyright}
    'Official audio file webpage', {fiWWWAudioFile}
    'Official artist/performer webpage', {fiWWWArtist}
    'Official audio source webpage', {fiWWWAudioSource}
    'Official internet radio station homepage', {fiWWWRadioPage}
    'Payment', {fiWWWPayment}
    'Official publisher webpage', {fiWWWPublisher}
    'User defined URL link', {fiWWWUser}
    'Encrypted meta frame', {fiMetaCrypto}
    'Compressed meta frame' {fiMetaCompression}
    );

  CPictureTypeStr: array[TJvID3PictureType] of string = (
    'Other',
    '32x32 pixels ''file icon'' (PNG only)',
    'Other file icon',
    'Cover (front)',
    'Cover (back)',
    'Leaflet page',
    'Media (e.g. lable side of CD)',
    'Lead artist/lead performer/soloist',
    'Artist/performer',
    'Conductor',
    'Band/Orchestra',
    'Composer',
    'Lyricist/text writer',
    'Recording Location',
    'During recording',
    'During performance',
    'Movie/video screen capture',
    'A bright coloured fish',
    'Illustration',
    'Band/artist logotype',
    'Publisher/Studio logotype'
    );

procedure SetPictureListItemTo(ListItem: TListItem; Frame: TJvID3PictureFrame);
begin
  with ListItem, Frame do
  begin
    Caption := Description;
    while SubItems.Count < 3 do
      SubItems.Add('');
    SubItems[0] := CPictureTypeStr[PictureType]; //Type
    SubItems[1] := MIMEType; //Format
    SubItems[2] := IntToStr(DataSize); //Size
    Data := Frame;
  end;
end;

procedure TJvID3v2EditForm.FormCreate(Sender: TObject);
begin
  FillPictureTypes(cmbPictureType.Items);
  ISO_639_2Names(cmbLanguage.Items);
  ID3_Genres(cmbGenre.Items);
end;

procedure TJvID3v2EditForm.actOKExecute(Sender: TObject);
var
  HasTag: Boolean;
  Version: TJvID3Version;
  Cursor: TCursor;
begin
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    CtrlsToTag;
    JvID3v21.Frames.RemoveEmptyFrames;

    HasTag := True;
    if JvID3v21.FrameCount = 0 then
      GetID3v2Version(JvID3v21.FileName, HasTag, Version);

    if HasTag then
      JvID3v21.Commit;
    ModalResult := mrOk;
  finally
    Screen.Cursor := Cursor;
  end;
end;

procedure TJvID3v2EditForm.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TJvID3v2EditForm.actRemoveExecute(Sender: TObject);
var
  Cursor: TCursor;
begin
  if MessageDlg('Remove tag?', mtConfirmation, mbOKCancel, 0) <> mrOk then
    Exit;

  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    JvID3v21.Erase;
    FTagDeleted := True;
    TagToCtrls;
  finally
    Screen.Cursor := Cursor;
  end;
end;

class function TJvID3v2EditForm.Execute(const AFileName: string): Boolean;
begin
  with TJvID3v2EditForm.Instance do
  try
    JvID3v21.FileName := AFileName;
    Init;
    try
      Result := (ShowModal = mrOk) or FTagDeleted;
    finally
      Final;
    end;
  finally
    Hide;
  end;
end;

procedure TJvID3v2EditForm.Init;
begin
  Caption := Format('Edit ''%s''', [ExtractFileName(JvID3v21.FileName)]);

  JvID3v21.Open;

  TagToCtrls;

  imgPicture.Picture.Assign(nil);

  lsbNavigator.ItemIndex := 0;
  PageControl1.ActivePage := tshWinampTags;
end;

function ChangeYear(const ADateTime: TDateTime; const NewYear: Word): TDateTime;
var
  OldYear, Month, Day: Word;
begin
  DecodeDate(ADateTime, OldYear, Month, Day);
  Result := EncodeDate(NewYear, Month, Day);
end;

procedure TJvID3v2EditForm.CtrlsToTag;

  function SetFirstOfList(Strings: TStrings; const S: string): string;
  begin
    if Strings.Count > 0 then
      Strings[0] := S
    else
      Strings.Add(S);
  end;
begin
  { WinAmp tags }

  { WinAmp treats some tags as single line tags; mimic this behaviour by
    using function SetFirstOfList }
  JvID3v21.Texts.Title := edtTitle.Text;
  SetFirstOfList(JvID3v21.Texts.LeadArtist, edtArtist.Text);
  JvID3v21.Texts.Album := edtAlbum.Text;
  { The 'year' tag is replaced by the 'recordingtime' tag in v2.4 }
  if JvID3v21.WriteVersion = ive2_4 then
    JvID3v21.Texts.RecordingTime := ChangeYear(JvID3v21.Texts.RecordingTime, StrToIntDef(edtYear.Text, 0))
  else
    JvID3v21.Texts.Year := StrToIntDef(edtYear.Text, 0);
  JvID3v21.Texts.ContentType := NiceGenreToGenre(cmbGenre.Text);
  { Note that WinAmp doesn't care about other properties than Text of TJvID3ContentFrame }
  TJvID3ContentFrame.FindOrCreate(JvID3v21, fiComment).Text := memComment.Lines.Text;
  SetFirstOfList(JvID3v21.Texts.Composer, edtComposer.Text);
  SetFirstOfList(JvID3v21.Texts.OrigArtist, edtOrigArtist.Text);
  JvID3v21.Texts.Copyright := edtCopyright.Text;
  { Note that WinAmp doesn't care about other properties than URL of TJvID3URLUserFrame }
  TJvID3URLUserFrame.FindOrCreate(JvID3v21, 0).URL := edtURL.Text;
  JvID3v21.Texts.EncodedBy := edtEncodedBy.Text;

  { Lyrics }
  with TJvID3ContentFrame.FindOrCreate(JvID3v21, fiUnsyncedLyrics) do
  begin
    Language := ISO_639_2NameToCode(cmbLanguage.Text);
    Text := memLyrics.Lines.Text;
    Description := edtDescription.Text;
  end;
  SetFirstOfList(JvID3v21.Texts.Lyricist, edtWriter.Text);
end;

function YearOf(const ADateTime: TDateTime): Word;
var
  D1, D2: Word;
begin
  DecodeDate(ADateTime, Result, D1, D2);
end;

procedure TJvID3v2EditForm.TagToCtrls;

  function GetFirstOfList(Strings: TStrings): string;
  begin
    if Strings.Count > 0 then
      Result := Strings[0]
    else
      Result := '';
  end;

var
  Frame: TJvID3Frame;
begin
  { Determine which frames are in the tag before calls to JvID3v21.Texts.xxx and
    FindOrCreate because those functions might create frames. }
  InitAllFramesTab;

  { WinAmp tags }

  { WinAmp treats some tags as single line tags; mimic this behaviour by
    using function GetFirstOfList }
  edtTitle.Text := JvID3v21.Texts.Title;
  edtArtist.Text := GetFirstOfList(JvID3v21.Texts.LeadArtist);
  edtAlbum.Text := JvID3v21.Texts.Album;
  { The 'year' tag is replaced by the 'recordingtime' tag in v2.4 }
  if JvID3v21.Version = ive2_4 then
    edtYear.Text := IntToStr(YearOf(JvID3v21.Texts.RecordingTime))
  else
    edtYear.Text := IntToStr(JvID3v21.Texts.Year);
  cmbGenre.Text := GenreToNiceGenre(JvID3v21.Texts.ContentType);
  { Note that WinAmp doesn't care about other properties than Text of TJvID3ContentFrame }
  memComment.Lines.Text := TJvID3ContentFrame.FindOrCreate(JvID3v21, fiComment).Text;
  edtComposer.Text := GetFirstOfList(JvID3v21.Texts.Composer);
  edtOrigArtist.Text := GetFirstOfList(JvID3v21.Texts.OrigArtist);
  edtCopyright.Text := JvID3v21.Texts.Copyright;
  { Note that WinAmp doesn't care about other properties than URL of TJvID3URLUserFrame }
  edtURL.Text := TJvID3URLUserFrame.FindOrCreate(JvID3v21, 0).URL;
  edtEncodedBy.Text := JvID3v21.Texts.EncodedBy;

  { Lyrics }
  with TJvID3ContentFrame.FindOrCreate(JvID3v21, fiUnsyncedLyrics) do
  begin
    cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(ISO_639_2CodeToName(Language));
    memLyrics.Lines.Text := Text;
    edtDescription.Text := Description;
  end;
  edtWriter.Text := GetFirstOfList(JvID3v21.Texts.Lyricist);

  { Pictures }
  lsvPictures.Items.BeginUpdate;
  try
    lsvPictures.Items.Clear;
    if JvID3v21.FindFirstFrame(fiPicture, Frame) then
      repeat
        if Frame is TJvID3PictureFrame then
          SetPictureListItemTo(lsvPictures.Items.Add, TJvID3PictureFrame(Frame));
      until not JvID3v21.FindNextFrame(fiPicture, Frame);
  finally
    lsvPictures.Items.EndUpdate;
  end;
end;

procedure TJvID3v2EditForm.actAddPictureExecute(Sender: TObject);
var
  Frame: TJvID3PictureFrame;
begin
  if cmbPictureType.ItemIndex < 0 then
  begin
    MessageDlg('Select a picture type', mtError, [mbOK], 0);
    FocusControl(cmbPictureType);
    Exit;
  end;

  with TOpenPictureDialog.Create(Application) do
  try
    if not Execute then
      Exit;

    Frame := TJvID3PictureFrame(JvID3v21.AddFrame(fiPicture));
    with Frame do
    begin
      with cmbPictureType do
        PictureType := TJvID3PictureType(Items.Objects[ItemIndex]);
      Description := edtPictureName.Text;
      MIMEType := ExtToMIMEType(ExtractFileExt(FileName));
      LoadFromFile(FileName);

      lsvPictures.Items.BeginUpdate;
      try
        SetPictureListItemTo(lsvPictures.Items.Add, Frame);
      finally
        lsvPictures.Items.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TJvID3v2EditForm.actDeletePictureExecute(Sender: TObject);
begin
  if not Assigned(lsvPictures.Selected) then
    Exit;

  JvID3v21.Frames.Remove(TJvID3Frame(lsvPictures.Selected.Data));
  lsvPictures.Items.Delete(lsvPictures.Selected.Index);
  imgPicture.Picture.Assign(nil);
end;

procedure TJvID3v2EditForm.actSavePictureExecute(Sender: TObject);
var
  Frame: TJvID3PictureFrame;
begin
  if not Assigned(lsvPictures.Selected) then
    Exit;

  Frame := TJvID3PictureFrame(lsvPictures.Selected.Data);

  if Assigned(Frame) and (Frame.DataSize > 0) and (Frame.MIMEType <> '-->') then
    with TSavePictureDialog.Create(Application) do
    try
      if Execute then
        Frame.SaveToFile(FileName);
    finally
      Free;
    end;
end;

procedure TJvID3v2EditForm.lsvPicturesClick(Sender: TObject);
var
  Frame: TJvID3PictureFrame;
begin
  if Assigned(lsvPictures.Selected) then
    Frame := TJvID3PictureFrame(lsvPictures.Selected.Data)
  else
    Frame := nil;

  if Assigned(Frame) then
  begin
    edtPictureName.Text := Frame.Description;
    with cmbPictureType do
      ItemIndex := Items.IndexOfObject(TObject(Frame.PictureType));
  end;

  imgPicture.Picture.Assign(Frame);
end;

procedure TJvID3v2EditForm.lsbNavigatorClick(Sender: TObject);
begin
  case lsbNavigator.ItemIndex of
    0: PageControl1.ActivePage := tshWinampTags;
    1: PageControl1.ActivePage := tshLyrics;
    2: PageControl1.ActivePage := tshPictures;
    3: PageControl1.ActivePage := tshAllFrames;
  end;
end;

procedure TJvID3v2EditForm.FillPictureTypes(Strings: TStrings);
var
  PictureType: TJvID3PictureType;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for PictureType := Low(TJvID3PictureType) to High(TJvID3PictureType) do
      Strings.AddObject(CPictureTypeStr[PictureType], TObject(PictureType));
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvID3v2EditForm.InitAllFramesTab;
var
  I: Integer;
  ListItem: TListItem;
begin
  lsvAllFrames.Items.BeginUpdate;
  try
    lsvAllFrames.Items.Clear;
    for I := 0 to JvID3v21.FrameCount - 1 do
      with JvID3v21.Frames[I] do
      begin
        ListItem := lsvAllFrames.Items.Add;
        ListItem.Caption := FrameName;
        if ClassType <> TJvID3SkipFrame then
          ListItem.SubItems.Add('Yes')
        else
          ListItem.SubItems.Add('No');
        ListItem.SubItems.Add(CFrameDescriptions[FrameID]);
        ListItem.Data := JvID3v21.Frames[I];
      end;
  finally
    lsvAllFrames.Items.EndUpdate;
  end;
end;

procedure TJvID3v2EditForm.actChangePictureExecute(Sender: TObject);
var
  Frame: TJvID3PictureFrame;
begin
  if not Assigned(lsvPictures.Selected) then
    Exit;

  if cmbPictureType.ItemIndex < 0 then
  begin
    MessageDlg('Select a picture type', mtError, [mbOK], 0);
    FocusControl(cmbPictureType);
    Exit;
  end;

  Frame := TJvID3PictureFrame(lsvPictures.Selected.Data);

  with Frame do
  begin
    with cmbPictureType do
      PictureType := TJvID3PictureType(Items.Objects[ItemIndex]);
    Description := edtPictureName.Text;

    lsvPictures.Items.BeginUpdate;
    try
      SetPictureListItemTo(lsvPictures.Selected, Frame);
    finally
      lsvPictures.Items.EndUpdate;
    end;
  end;
end;

procedure TJvID3v2EditForm.ItemSelected(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := Assigned(lsvPictures.Selected);
end;

procedure TJvID3v2EditForm.actCopyTov1Execute(Sender: TObject);
begin
  if not JvID3v21.CopyToID3v1 then
    ShowMessage('Error');
end;

procedure TJvID3v2EditForm.actCopyFromv1Execute(Sender: TObject);
begin
  if JvID3v21.CopyFromID3v1 then
    TagToCtrls
  else
    ShowMessage('Error');
end;

var
  GInstance: TJvID3v2EditForm = nil;

class function TJvID3v2EditForm.Instance: TJvID3v2EditForm;
begin
  if not Assigned(GInstance) then
    GInstance := TJvID3v2EditForm.Create(Application);

  Result := GInstance;
end;

procedure TJvID3v2EditForm.Final;
begin
  JvID3v21.Close;
end;

procedure TJvID3v2EditForm.lsvAllFramesInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: String);
var
  Frame: TJvID3Frame;
  S: string;
begin
  Frame := TJvID3Frame(Item.Data);
  if Frame is TJvID3TextFrame then
    InfoTip := TJvID3TextFrame(Frame).Text
  else if Frame is TJvID3NumberFrame then
    InfoTip := IntToStr(TJvID3NumberFrame(Frame).Value)
  else if Frame is TJvID3UserFrame then
    with Frame as TJvID3UserFrame do
      InfoTip := Format('%s: %s', [Description, Value])
  else if Frame is TJvID3PictureFrame then
    with Frame as TJvID3PictureFrame do
      InfoTIp := Format('%s (%s) %d bytes', [Description, MIMEType, DataSize])
  else if Frame is TJvID3TimestampFrame then
    InfoTip := DateTimeToStr(TJvID3TimestampFrame(Frame).Value)
  else if Frame is TJvID3ContentFrame then
    InfoTip := TJvID3ContentFrame(Frame).Text
  else if Frame is TJvID3SimpleListFrame then
  begin
    S := TJvID3SimpleListFrame(Frame).List.GetText;
    Delete(S, Length(S) - 1, 2);
    InfoTip := S;
  end;
end;

end.

