{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvID3v2Editor.PAS, released on 2003-04-16.

The Initial Developer of the Original Code is Remko Bonte [remkobonte att myrealbox dott com]
Portions created by Remko Bonte are Copyright (C) 2003 Remko Bonte.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvID3v2EditorForm;

{$I jvcl.inc}

interface

{ Most code is copied from DSDesign.pas }

uses
  Classes, Windows, Messages, Forms, StdCtrls, ExtCtrls, Menus, Controls,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus, DesignWindows,
  {$ELSE}
  DsgnIntf, DsgnWnds,
  {$ENDIF COMPILER6_UP}
  JvID3v2Base, JvID3v2Types, JvDsgnTypes;

type
  TSelectionProc = function(AFrame: TJvID3Frame): Boolean of object;

  TFSDesigner = class;

  TJvID3FramesEditor = class(TDesignWindow)
    FrameListBox: TListBox;
    LocalMenu: TPopupMenu;
    NewItem: TMenuItem;
    N1: TMenuItem;
    DeleteItem: TMenuItem;
    SelectAllItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBoxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure NewFrameClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
  private
    FFSDesigner: TFSDesigner;
    FController: TJvID3Controller;
    FFocusRectItem: Integer;
    FMinWidth: Integer;
    FMinHeight: Integer;
    procedure MoveFrames(MoveOffset: Integer);
    procedure RemoveFrames;
    procedure SelectAll;
    procedure RestoreSelection(var Selection: TStringList;
      ItemIndex, TopIndex: Integer; RestoreUpdate: Boolean);
    procedure SaveSelection(var Selection: TStringList;
      var ItemIndex, TopIndex: Integer; NoUpdate: Boolean);
    procedure SetController(Value: TJvID3Controller);
    procedure UpdateDisplay;
    procedure UpdateCaption;
    procedure UpdateFrameList;
    procedure UpdateSelection;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  protected
    procedure Activated; override;
    function UniqueName(Component: TComponent): string; override;
  public
    function ForEachSelection(Proc: TSelectionProc): Boolean;
    function DoNewFrame: TJvID3Frame;
    {$IFDEF COMPILER6_UP}
    function EditAction(Action: TEditAction): Boolean; override;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    {$ELSE}
    procedure SelectionChanged(ASelection: TDesignerSelectionList); override;
    procedure EditAction(Action: TEditAction); override;
    procedure ComponentDeleted(Component: IPersistent); override;
    procedure FormModified; override;
    {$ENDIF COMPILER6_UP}
    property Controller: TJvID3Controller read FController write SetController;
  end;

  TJvID3ControllerEditor = class(TComponentEditor)
  protected
    procedure Commit;
    procedure RemoveTag;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TFSDesigner = class(TJvID3ControllerDesigner)
  private
    FFramesEditor: TJvID3FramesEditor;
    function GetFrameDescription(const FrameID: TJvID3FrameID): string;
  public
    destructor Destroy; override;
    procedure ID3Event(Event: TJvID3Event; Info: Longint); override;
    property FramesEditor: TJvID3FramesEditor read FFramesEditor;
    property FrameDescription[const FrameID: TJvID3FrameID]: string read GetFrameDescription;
  end;

  TJvID3FileInfoEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure ShowFramesEditor(Designer: IJvFormDesigner; AController: TJvID3Controller);
function CreateFramesEditor(Designer: IJvFormDesigner;
  AController: TJvID3Controller; var Shared: Boolean): TJvID3FramesEditor;
procedure ShowFileInfo(AController: TJvID3Controller);

implementation

uses
  Dialogs,
  Math, SysUtils,
  JvID3v2DefineForm, JvTypes, JvDsgnConsts;


{$R *.dfm}

type
  TJvID3ControllerAccess = class(TJvID3Controller);

const
  cFrameDescriptions: array [TJvID3FrameID] of string =
   (
    RsfiErrorFrame,
    RsfiPaddingFrame,
    RsfiNoFrame,
    RsfiAudioCrypto,
    RsfiPicture,
    RsfiAudioSeekPoint,
    RsfiComment,
    RsfiCommercial,
    RsfiCryptoReg,
    RsfiEqualization2,
    RsfiEqualization,
    RsfiEventTiming,
    RsfiGeneralObject,
    RsfiGroupingReg,
    RsfiInvolvedPeople,
    RsfiLinkedInfo,
    RsfiCDID,
    RsfiMPEGLookup,
    RsfiOwnership,
    RsfiPrivate,
    RsfiPlayCounter,
    RsfiPopularimeter,
    RsfiPositionsync,
    RsfiBufferSize,
    RsfiVolumeAdj2,
    RsfiVolumeAdj,
    RsfiReverb,
    RsfiSeekFrame,
    RsfiSignature,
    RsfiSyncedLyrics,
    RsfiSyncedTempo,
    RsfiAlbum,
    RsfiBPM,
    RsfiComposer,
    RsfiContentType,
    RsfiCopyright,
    RsfiDate,
    RsfiEncodingTime,
    RsfiPlaylistDelay,
    RsfiOrigReleaseTime,
    RsfiRecordingTime,
    RsfiReleaseTime,
    RsfiTaggingTime,
    RsfiInvolvedPeople2,
    RsfiEncodedBy,
    RsfiLyricist,
    RsfiFileType,
    RsfiTime,
    RsfiContentGroup,
    RsfiTitle,
    RsfiSubTitle,
    RsfiInitialKey,
    RsfiLanguage,
    RsfiSongLen,
    RsfiMusicianCreditList,
    RsfiMediaType,
    RsfiMood,
    RsfiOrigAlbum,
    RsfiOrigFileName,
    RsfiOrigLyricist,
    RsfiOrigArtist,
    RsfiOrigYear,
    RsfiFileOwner,
    RsfiLeadArtist,
    RsfiBand,
    RsfiConductor,
    RsfiMixArtist,
    RsfiPartInSet,
    RsfiProducedNotice,
    RsfiPublisher,
    RsfiTrackNum,
    RsfiRecordingDates,
    RsfiNetRadioStation,
    RsfiNetRadioOwner,
    RsfiSize,
    RsfiAlbumSortOrder,
    RsfiPerformerSortOrder,
    RsfiTitleSortOrder,
    RsfiISRC,
    RsfiEncoderSettings,
    RsfiSetSubTitle,
    RsfiUserText,
    RsfiYear,
    RsfiUniqueFileID,
    RsfiTermsOfUse,
    RsfiUnsyncedLyrics,
    RsfiWWWCommercialInfo,
    RsfiWWWCopyright,
    RsfiWWWAudioFile,
    RsfiWWWArtist,
    RsfiWWWAudioSource,
    RsfiWWWRadioPage,
    RsfiWWWPayment,
    RsfiWWWPublisher,
    RsfiWWWUser,
    RsfiMetaCrypto,
    RsfiMetaCompression
   );

procedure ShowFramesEditor(Designer: IJvFormDesigner; AController: TJvID3Controller);
var
  FramesEditor: TJvID3FramesEditor;
  VShared: Boolean;
begin
  FramesEditor := CreateFramesEditor(Designer, AController, VShared);
  if FramesEditor <> nil then
    FramesEditor.Show;
end;

function CreateFramesEditor(Designer: IJvFormDesigner;
  AController: TJvID3Controller; var Shared: Boolean): TJvID3FramesEditor;
begin
  Shared := True;
  if AController.Designer <> nil then
    Result := (AController.Designer as TFSDesigner).FFramesEditor
  else
  begin
    Result := TJvID3FramesEditor.Create(Application);
    Result.Designer := Designer;
    Result.Controller := AController;
    Shared := False;
  end;
end;

procedure ShowFileInfo(AController: TJvID3Controller);
const
  cVersion: array [TJvMPEGVersion] of string =
    (RsMPEG25, RsMPEGUnknown, RsMPEG20, RsMPEG10);
  cLayer: array [TJvMPEGLayer] of string =
    (RsLayerUnknown, RsLayer3, RsLayer2, RsLayer1);
  cChannelMode: array [TJvMPEGChannelMode] of string =
    (RsChannelModeStereo, RsChannelModeJointStereo,
     RsChannelModeDualChannel, RsChannelModeMono);
  cEmphasis: array [TJvMPEGEmphasis] of string =
    (RsEmphasisNone, RsEmphasisMicrosec, RsEmphasisUnknown, RsEmphasisCCITTJ17);
  cBool: array [Boolean] of string =
    (RsBoolNo, RsBoolYes);
  cVbr: array [Boolean] of string =
    (RsVbrNo, RsVbrVbr);
var
  Msg: string;
  SavedActive: Boolean;
begin
  SavedActive := AController.Active;
  try
    with TJvID3ControllerAccess(AController) do
    begin
      if FileName = '' then
      begin
        MessageDlg(RsID3Err_NoFileSpecified, mtError, [mbOK], 0);
        Exit;
      end;

      if not FileExists(FileName) then
      begin
        MessageDlg(Format(RSID3Err_FileDoesNotExists, [FileName]),
          mtError, [mbOK], 0);
        Exit;
      end;

      Active := True;

      with FileInfo do
      begin
        if not IsValid then
        begin
          MessageDlg(RSID3Err_NoValidMPEGTag, mtError, [mbOK], 0);
          Exit;
        end;

        Msg := Format(RsIDV2FileInfoFmt, [FileSize, HeaderFoundAt, LengthInSec,
          cVersion[Version], cLayer[Layer], Bitrate, cVbr[IsVbr], FrameCount,
          SamplingRateFrequency, cChannelMode[ChannelMode],
          cBool[mbProtection in Bits], cBool[mbCopyrighted in Bits],
          cBool[mbOriginal in Bits], cEmphasis[Emphasis]]);
      end;
    end;

    { We don't use MessageDlg now, because we want a custom caption }
    with CreateMessageDialog(Msg, mtCustom, [mbOK]) do
    try
      Position := poScreenCenter;
      Caption := RsIDV2FileInfoCaption;
      ShowModal;
    finally
      Free;
    end;
  finally
    AController.Active := SavedActive;
  end;
end;

//=== { TJvID3FrameEditor } ==================================================

procedure TJvID3FramesEditor.Activated;
begin
  {$IFDEF COMPILER6_UP}
  Designer.Activate;
  {$ENDIF COMPILER6_UP}
  try
    UpdateSelection;
  except
    FrameListBox.Items.Clear;
  end;
end;

{$IFNDEF COMPILER6_UP}
procedure TJvID3FramesEditor.ComponentDeleted(Component: IPersistent);
var
  P: TPersistent;
begin
  P := ExtractPersistent(Component);
  if P = Controller then
    Controller := nil
  else
  if (P is TJvID3Frame) and (TJvID3Frame(P).Controller = Controller) then
    UpdateDisplay;
end;
{$ENDIF COMPILER6_UP}

function TJvID3FramesEditor.DoNewFrame: TJvID3Frame;
var
  DefineFrame: TJvID3DefineDlg;
begin
  Result := nil;
  DefineFrame := TJvID3DefineDlg.Create(Application);
  try
    DefineFrame.FSDesigner := FFSDesigner;
    DefineFrame.Designer := Designer;
    DefineFrame.Controller := Controller;
    if DefineFrame.ShowModal = mrOk then
    begin
      Result := DefineFrame.Frame;
      if Visible then
        UpdateDisplay;
      Designer.Modified;
    end;
  finally
    DefineFrame.Release;
  end;
end;

{$IFDEF COMPILER6_UP}
function TJvID3FramesEditor.EditAction(Action: TEditAction): Boolean;
{$ELSE}
procedure TJvID3FramesEditor.EditAction(Action: TEditAction);
{$ENDIF COMPILER6_UP}
begin
  {$IFDEF COMPILER6_UP}
  Result := True;
  {$ENDIF COMPILER6_UP}
  case Action of
    {eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;}
    eaDelete:
      RemoveFrames;
    eaSelectAll:
      begin
        SelectAll;
        UpdateSelection;
      end;
  {$IFDEF COMPILER6_UP}
  else
    Result := False;
  {$ENDIF COMPILER6_UP}
  end;
end;

function TJvID3FramesEditor.ForEachSelection(Proc: TSelectionProc): Boolean;
var
  Frame: TJvID3Frame;
  I: Integer;
begin
  Result := False;
  with FrameListBox do
    for I := 0 to Items.Count - 1 do
      if Selected[I] then
      begin
        Frame := TJvID3Frame(Items.Objects[I]);
        if (Frame <> nil) and not Proc(Frame) then
          Exit;
      end;
  Result := True;
end;

{$IFDEF COMPILER6_UP}
procedure TJvID3FramesEditor.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if AItem = Controller then
    Controller := nil
  else
  if (AItem is TJvID3Frame) and (TJvID3Frame(AItem).Controller = Controller) then
    UpdateDisplay;
end;
{$ENDIF COMPILER6_UP}

{$IFDEF COMPILER6_UP}
procedure TJvID3FramesEditor.ItemsModified(const Designer: IDesigner);
{$ELSE}
procedure TJvID3FramesEditor.FormModified;
{$ENDIF COMPILER6_UP}
begin
  UpdateCaption;
end;

procedure TJvID3FramesEditor.MoveFrames(MoveOffset: Integer);
var
  I, E: Integer;
begin
  try
    FFSDesigner.BeginDesign;
    try
      with FrameListBox do
      begin
        I := 0;
        E := Items.Count;
        if MoveOffset > 0 then
        begin
          I := E - 1;
          E := -1;
        end;
        while I <> E do
        begin
          if Selected[I] then
            with TJvID3Frame(Items.Objects[I]) do
              Index := Index + MoveOffset;
          Inc(I, -MoveOffset);
        end;
      end;
    finally
      FFSDesigner.EndDesign;
    end;
  finally
    UpdateDisplay;
    Designer.Modified;
  end;
end;

procedure TJvID3FramesEditor.RemoveFrames;
var
  I, Focused: Integer;
begin
  try
    FFSDesigner.BeginDesign;
    try
      Focused := FrameListBox.ItemIndex;
      with FrameListBox do
        for I := Items.Count - 1 downto 0 do
          if Selected[I] then
            TJvID3Frame(Items.Objects[I]).Free;
    finally
      FFSDesigner.EndDesign;
      Designer.Modified;
    end;
  finally
    UpdateDisplay;
  end;
  if Focused <> -1 then
  begin
    Focused := Min(Focused, FrameListBox.Items.Count - 1);
    FrameListBox.ItemIndex := Focused;
    FrameListBox.Selected[Focused] := True;
    UpdateSelection;
  end;
  FrameListBox.SetFocus;
end;

procedure TJvID3FramesEditor.RestoreSelection(var Selection: TStringList;
  ItemIndex, TopIndex: Integer; RestoreUpdate: Boolean);
var
  I: Integer;
begin
  try
    with FrameListBox do
      for I := 0 to Items.Count - 1 do
        Selected[I] := Selection.IndexOf(TComponent(Items.Objects[I]).Name) <> -1;
    if TopIndex <> -1 then
      FrameListBox.TopIndex := TopIndex;
    if ItemIndex <> -1 then
      FrameListBox.ItemIndex := ItemIndex;
  finally
    if RestoreUpdate then
      FrameListBox.Items.EndUpdate;
    FrameListBox.Invalidate;
    Selection.Free;
    Selection := nil;
    UpdateSelection;
  end;
end;

procedure TJvID3FramesEditor.SaveSelection(var Selection: TStringList;
  var ItemIndex, TopIndex: Integer; NoUpdate: Boolean);
var
  I: Integer;
begin
  { Name of a frame is unique, thus fill Selection with these names
    of frames that are selected }
  Selection := TStringList.Create;
  try
    ItemIndex := FrameListBox.ItemIndex;
    TopIndex := FrameListBox.TopIndex;
    with FrameListBox do
      for I := 0 to Items.Count - 1 do
        if Selected[I] then
          Selection.Add(TComponent(Items.Objects[I]).Name);
    if NoUpdate then
      FrameListBox.Items.BeginUpdate;
  except
    Selection.Free;
    Selection := nil;
  end;
end;

procedure TJvID3FramesEditor.SelectAll;
var
  I: Integer;
begin
  with FrameListBox do
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
end;

{$IFDEF COMPILER6_UP}
procedure TJvID3FramesEditor.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
{$ELSE}
procedure TJvID3FramesEditor.SelectionChanged(ASelection: TDesignerSelectionList);
{$ENDIF COMPILER6_UP}
var
  I: Integer;
  S: Boolean;

  function InSelection(Component: TComponent): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if ASelection <> nil then
      with ASelection do
        for I := 0 to Count - 1 do
          if Component = Items[I] then
            Exit;
    Result := False;
  end;

begin
  with FrameListBox do
    for I := 0 to Items.Count - 1 do
    begin
      S := InSelection(TComponent(Items.Objects[I]));
      if Selected[I] <> S then
        Selected[I] := S;
    end;
end;

procedure TJvID3FramesEditor.SetController(Value: TJvID3Controller);
begin
  if FController <> Value then
  begin
    if FController <> nil then
      FreeAndNil(FFSDesigner);
    FController := Value;
    if FController <> nil then
    begin
      FFSDesigner := TFSDesigner.Create(Value);
      FFSDesigner.FFramesEditor := Self;
      UpdateDisplay;
    end
    else
    begin
      if not (csDestroying in ComponentState) then
        Release;
    end;
  end;
end;

function TJvID3FramesEditor.UniqueName(Component: TComponent): string;
var
  FrameName: string;
begin
  if Component is TJvID3Frame then
    FrameName := TJvID3Frame(Component).FrameName
  else
    FrameName := '';
  Result := CreateUniqueName(Controller, FrameName,
    TJvID3FrameClass(Component.ClassType), Component)
end;

procedure TJvID3FramesEditor.UpdateCaption;
const
  cFrameEditor = '%s%s%s';
var
  NewCaption: string;
begin
  if (Controller <> nil) and (Controller.Owner <> nil) then
    NewCaption := Format(cFrameEditor,
      [Controller.Owner.Name, '.', Controller.Name]);
  if Caption <> NewCaption then
    Caption := NewCaption;
end;

procedure TJvID3FramesEditor.UpdateDisplay;
begin
  UpdateFrameList;
  UpdateCaption;
  UpdateSelection;
end;

procedure TJvID3FramesEditor.UpdateFrameList;
var
  ItemIndex, TopIndex: Integer;
  Selection: TStringList;
  EnableList: Boolean;
  I: Integer;
  Frame: TJvID3Frame;
  FrameName: string;
begin
  SaveSelection(Selection, ItemIndex, TopIndex, True);
  try
    FrameListBox.Clear;
    EnableList := False;
    try
      if Controller = nil then
        Exit;
      for I := 0 to Controller.Frames.Count - 1 do
      begin
        Frame := Controller.Frames[I];
        if not (csDestroying in Frame.ComponentState) then
        begin
          FrameName := Frame.FrameName;
          if FrameName = '' then
            FrameName := Format('<%s>', [Controller.Frames[I].Name]);
          FrameName := FrameName + ' - ' + cFrameDescriptions[Frame.FrameID];
          FrameListBox.Items.AddObject(FrameName, Frame);
        end;
      end;

      EnableList := True;
    finally
      FrameListBox.Enabled := EnableList;
    end;
  finally
    RestoreSelection(Selection, ItemIndex, TopIndex, True)
  end;
end;

procedure TJvID3FramesEditor.UpdateSelection;
var
  I: Integer;
  Frame: TJvID3Frame;
  {$IFDEF COMPILER6_UP}
  ComponentList: IDesignerSelections;
  {$ELSE}
  ComponentList: TDesignerSelectionList;
  {$ENDIF COMPILER6_UP}
begin
  if Active then
  begin
    {$IFDEF COMPILER6_UP}
    ComponentList := TDesignerSelections.Create;
    {$ELSE}
    ComponentList := TDesignerSelectionList.Create;
    {$ENDIF COMPILER6_UP}
    try
      with FrameListBox do
        for I := 0 to Items.Count - 1 do
          if Selected[I] then
          begin
            Frame := TJvID3Frame(Items.Objects[I]);
            if Frame <> nil then
              ComponentList.Add(Frame);
          end;
      if ComponentList.Count = 0 then
        ComponentList.Add(Controller);
    except
      {$IFNDEF COMPILER6_UP}
      // In D6 up it's an interface, so no need to free up
      ComponentList.Free;
      {$ENDIF COMPILER6_UP}
      raise;
    end;
    {$IFDEF COMPILER6_UP}
    Designer.SetSelections(ComponentList);
    {$ELSE}
    SetSelection(ComponentList);
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TJvID3FramesEditor.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  with Msg.MinMaxInfo^.ptMinTrackSize do
  begin
    X := FMinWidth;
    Y := FMinHeight;
  end;
end;

//=== { TJvID3ControllerEditor } =============================================

procedure TJvID3ControllerEditor.Commit;
begin
  if MessageDlg(RsCommit, mtConfirmation, mbOKCancel, 0) = mrOk then
    TJvID3Controller(Component).Commit;
end;

procedure TJvID3ControllerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowFramesEditor(Designer, TJvID3Controller(Component));
    1:
      RemoveTag;
    2:
      ShowFileInfo(TJvID3Controller(Component));
    3:
      Commit;
  end;
end;

function TJvID3ControllerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RSID3FrameEditorTag;
    1:
      Result := RSID3RemoveTag;
    2:
      Result := RSID3FileInfoTag;
    3:
      Result := RSID3CommitTag;
  end;
end;

function TJvID3ControllerEditor.GetVerbCount: Integer;
begin
  Result := 3;

  with TJvID3Controller(Component) do
    if Active and Modified then
      Inc(Result);
end;

procedure TJvID3FramesEditor.FormCreate(Sender: TObject);
begin
  FMinWidth := Width;
  FMinHeight := Height;
end;

procedure TJvID3ControllerEditor.RemoveTag;
begin
  with TJvID3Controller(Component) do
  begin
    if FileName = '' then
    begin
      MessageDlg(RsID3Err_NoFileSpecified, mtError, [mbOK], 0);
      Exit;
    end;

    if not FileExists(FileName) then
    begin
      MessageDlg(Format(RSID3Err_FileDoesNotExists, [FileName]), mtError, [mbOK], 0);
      Exit;
    end;

    if MessageDlg(RSID3RemoveTagConfirmation, mtConfirmation, mbOKCancel, 0) = mrOk then
      Erase;
  end;
end;

//=== { TFSDesigner } ========================================================

destructor TFSDesigner.Destroy;
var
  F: TJvID3FramesEditor;
begin
  if FFramesEditor <> nil then
  begin
    F := FFramesEditor;
    FFramesEditor := nil;
    F.FFSDesigner := nil;
    { (rb) DSDesign.pas uses Release, but that gave problems, with recompiling }
    F.Free;
  end;
  inherited Destroy;
end;

procedure TJvID3FramesEditor.ListBoxClick(Sender: TObject);
begin
  UpdateSelection;
end;

function TFSDesigner.GetFrameDescription(const FrameID: TJvID3FrameID): string;
begin
  Result := cFrameDescriptions[FrameID];
end;

procedure TFSDesigner.ID3Event(Event: TJvID3Event; Info: Longint);
begin
  if Event in [ideFrameListChange, ideID3Change] then
    FFramesEditor.UpdateFrameList;
end;

procedure TJvID3FramesEditor.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_INSERT:
      NewFrameClick(Self);
    VK_DELETE:
      RemoveFrames;
    VK_UP:
      if (ssCtrl in Shift) and (Sender = FrameListBox) then
        MoveFrames(-1)
      else
        Exit;
    VK_DOWN:
      if (ssCtrl in Shift) and (Sender = FrameListBox) then
        MoveFrames(1)
      else
        Exit;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TJvID3FramesEditor.ListBoxDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  F: TJvID3Frame;
  I: Integer;
begin
  try
    FFSDesigner.BeginDesign;
    try
      with FrameListBox do
      begin
        F := TJvID3Frame(Items.Objects[ItemAtPos(Point(X, Y), True)]);
        for I := 0 to Items.Count - 1 do
          if Selected[I] then
            TJvID3Frame(Items.Objects[I]).Index := F.Index;
      end;
    finally
      FFSDesigner.EndDesign;
    end;
  finally
    UpdateDisplay;
    Designer.Modified;
  end;
end;

procedure TJvID3FramesEditor.ListBoxDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: Integer;

  procedure DrawRect(Item: Integer);
  begin
    if Item <> -1 then
      with FrameListBox do
        Canvas.DrawFocusRect(ItemRect(Item));
    FFocusRectItem := Item;
  end;

begin
  Item := FrameListBox.ItemAtPos(Point(X, Y), False);
  Accept := (Source = FrameListBox) and
    (Item >= 0) and (Item < FrameListBox.Items.Count) and
    not FrameListBox.Selected[Item];
  if State = dsDragEnter then
    FFocusRectItem := -1;
  if (State = dsDragLeave) or not Accept then
    Item := -1;
  DrawRect(FFocusRectItem);
  DrawRect(Item);
end;

procedure TJvID3FramesEditor.FormDestroy(Sender: TObject);
begin
  if FFSDesigner <> nil then
  begin
    { Destroy the designer if the editor is destroyed }
    FFSDesigner.FFramesEditor := nil;
    FFSDesigner.Free;
    FFSDesigner := nil;
  end;
end;

procedure TJvID3FramesEditor.NewFrameClick(Sender: TObject);
var
  Selection: TStringList;
  Frame: TJvID3Frame;
begin
  Frame := DoNewFrame;
  if Frame <> nil then
  begin
    Selection := TStringList.Create;
    try
      Selection.Add(Frame.Name);
    finally
      RestoreSelection(Selection, -1, -1, False);
    end;
  end;
  FrameListBox.SetFocus;
end;

procedure TJvID3FramesEditor.DeleteClick(Sender: TObject);
begin
  RemoveFrames;
end;

procedure TJvID3FramesEditor.SelectAllClick(Sender: TObject);
begin
  SelectAll;
  UpdateSelection;
end;

//=== { TJvID3FileInfoEditor } ===============================================

procedure TJvID3FileInfoEditor.Edit;
var
  P: TPersistent;
begin
  P := TPersistent(GetComponent(0));
  if P is TJvID3Controller then
    ShowFileInfo(TJvID3Controller(P));
end;

function TJvID3FileInfoEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.

