{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Menus, StdActns, ActnList, ImgList,
  JvPageScroller, JvComCtrls, JvStatusBar, JvDateTimePicker, JvCombobox, JvLinkLabel,
  JvEdit, JvSurveyIntf, JvDialogs, JvImage, JvCheckBox, JvValidateEdit,
  JvRichEdit, JvExExtCtrls, JvExStdCtrls, JvExComCtrls;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    JvStatusBar1: TJvStatusBar;
    tvItems: TJvTreeView;
    nbDetails: TNotebook;
    Label1: TLabel;
    edTitle: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edHREF: TEdit;
    Label4: TLabel;
    edRecipientName: TEdit;
    Label5: TLabel;
    edRecipientEMail: TEdit;
    dtpReleaseDate: TJvDateTimePicker;
    Label6: TLabel;
    lblExpDate: TLabel;
    dtpExpirationDate: TJvDateTimePicker;
    Label8: TLabel;
    edItemTitle: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    cbItemType: TJvComboBox;
    reItemChoices: TJvRichEdit;
    reItemDescription: TJvRichEdit;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Edit7: TMenuItem;
    Help1: TMenuItem;
    alMain: TActionList;
    acOpen: TAction;
    acSave: TAction;
    acNew: TAction;
    acSaveAs: TAction;
    acExit: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    acCopy: TEditCopy;
    acCut: TEditCut;
    acPaste: TEditPaste;
    acSelectAll: TEditSelectAll;
    acUndo: TEditUndo;
    acNew1: TMenuItem;
    acOpen1: TMenuItem;
    acSave1: TMenuItem;
    acSaveAs1: TMenuItem;
    N1: TMenuItem;
    acExit1: TMenuItem;
    Undo1: TMenuItem;
    N2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N3: TMenuItem;
    SelectAll1: TMenuItem;
    acAbout: TAction;
    acHelp: TAction;
    il16: TImageList;
    About1: TMenuItem;
    N4: TMenuItem;
    Help2: TMenuItem;
    popTree: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    N5: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    Items1: TMenuItem;
    Add2: TMenuItem;
    Delete2: TMenuItem;
    N6: TMenuItem;
    MoveUp2: TMenuItem;
    MoveDown2: TMenuItem;
    Label14: TLabel;
    edID: TJvValidateEdit;
    OpenSurveyDialog: TJvOpenDialog;
    SaveSurveyDialog: TJvSaveDialog;
    acPrev: TAction;
    acNext: TAction;
    Panel2: TPanel;
    lblPage: TLabel;
    JvImage1: TJvImage;
    Previous1: TMenuItem;
    Next1: TMenuItem;
    N7: TMenuItem;
    reDescription: TJvRichEdit;
    chkRequired: TJvCheckBox;
    acCopyItem: TAction;
    acPreview: TAction;
    N8: TMenuItem;
    Preview1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure tvItemsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure acPrevExecute(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure SurveyChanged(Sender: TObject);
    procedure nbDetailsPageChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acCopyItemExecute(Sender: TObject);
    procedure acPreviewExecute(Sender: TObject);
    procedure WMUser1(var Msg:TMessage);message WM_USER + 1;
  private
    FModified: boolean;
    FFilename: string;
    procedure SetModified(const Value: boolean);
  private
    FSurvey: IJvSurvey;
    FLastNode: TTreeNode;
    procedure SaveData(Node: TTreeNode);
    procedure LoadData(Node: TTreeNode);

    function CheckSave: boolean;
    procedure CreateEverything;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure FreeEverything;

    procedure Clear;
    procedure LoadView;
    function AddItem(Parent: TTreeNode; Item: IJvSurveyItem): TTreeNode;

    procedure LoadFromFile(const Filename: string);
    function SaveFile: boolean;
    procedure SaveToFile(const Filename: string;Format:TJvSurveyFileFormat);
    procedure UpdateStatus;
    property Filename: string read FFilename write FFilename;
    property Modified: boolean read FModified write SetModified;
  public
  end;

var
  frmMain: TfrmMain;

resourcestring
  SPageCaptionGlobal = 'General Survey Settings';
  SPageCaptionItem = 'Survey Item Settings';
  SModified = 'Modified';
  SReady = 'Ready';
  SSavePromptText = 'Save changes to current survey?';
  SSurveyModifiedCaption = 'Survey modified';
  SNewItemTitle = 'New item';
  SItemCopy     = ' (copy)';
  SSurveyorNotFoundFmt = 'Unable to find JEDI Surveyor (%s). Please fix and try again.';
  SAboutText = 'JEDI Surveyor Builder, version 1.0';
  SAboutTitle = 'About Builder...';

const
  cSurveyItemImageIndex = 22;

implementation
uses
  JvSurveyUtils, JclMiscel;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CreateEverything;
  LoadSettings;
  // set Modified to false after all updates are finished
  PostMessage(Handle,WM_USER + 1, 0, 0);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CheckSave then
  begin
    SaveSettings;
    FreeEverything;
  end
  else
    CanClose := false;
end;

procedure TfrmMain.CreateEverything;
begin
  FSurvey := CreateSurvey;
end;

procedure TfrmMain.FreeEverything;
begin
  FSurvey := nil;
end;

procedure TfrmMain.LoadFromFile(const Filename: string);
begin
  if not CheckSave then Exit;
  Clear;
  FSurvey.LoadFromFile(Filename);
  self.Filename := Filename;
  LoadView;
  Modified := false;
  UpdateStatus;
end;

procedure TfrmMain.LoadSettings;
begin
  dtpReleaseDate.Date := Date;
  dtpExpirationDate.Date := Date;
end;

procedure TfrmMain.SaveSettings;
begin

end;

procedure TfrmMain.SaveToFile(const Filename: string;Format:TJvSurveyFileFormat);
var
  N: TTreeNode;
  i: integer;
begin
  // DONE: update ID's of survey items
  // and reset response
  N := tvItems.Items.GetFirstNode;
  if N <> nil then
    N := N.getFirstChild;
  i := 1;
  while Assigned(N) do
  begin
    if N.Data <> nil then
    begin
      IJvSurveyItem(N.Data).ID := i;
      IJvSurveyItem(N.Data).Responses := '';
      Inc(i);
    end;
    N := N.getNextSibling;
  end;
  FSurvey.SurveyTaker.ID := '';
  FSurvey.SurveyTaker.UserName := '';
  FSurvey.SurveyTaker.MailAddress := '';
  FSurvey.SurveyTaker.Notes := '';
  FSurvey.SaveToFile(Filename,Format);
  self.Filename := Filename;
  Modified := false;
  UpdateStatus;
end;

function TfrmMain.CheckSave: boolean;
begin
  Result := true;
  if Modified then
    case MessageBox(GetFocus, PChar(SSavePromptText), PChar(SSurveyModifiedCaption), MB_YESNOCANCEL) of
      IDYES:
        Result := SaveFile;
      IDNO: ;
      IDCANCEL:
        Result := false;
    end;
end;

procedure TfrmMain.acOpenExecute(Sender: TObject);
begin
  OpenSurveyDialog.Filename := Filename;
  if OpenSurveyDialog.Execute then
    LoadFromFile(OpenSurveyDialog.Filename);
end;

procedure TfrmMain.acSaveExecute(Sender: TObject);
begin
  SaveData(tvItems.Selected);
  SaveFile;
end;

function TfrmMain.SaveFile: boolean;
const
  aFormat: array [1..3] of TJvSurveyFileFormat = (ffBinary,ffText,ffText);
begin
  Result := true;
  if (Filename = '') then
    Result := acSaveAs.Execute
  else
    SaveToFile(Filename,aFormat[SaveSurveyDialog.FilterIndex]);
end;

procedure TfrmMain.Clear;
begin
  tvItems.Items.GetFirstNode.DeleteChildren;
  nbDetails.PageIndex := 0;
  // DONE: clear edits etc
  LoadData(nil);
  Modified := true;
end;

procedure TfrmMain.LoadView;
var
  N: TTreeNode;
  i: integer;
begin
  N := tvItems.Items.GetFirstNode;
  if N <> nil then
  begin
    N.Data := Pointer(FSurvey);
    for i := 0 to FSurvey.Items.Count - 1 do
      AddItem(N, FSurvey.Items[i])
  end;
  LoadData(N);
  tvItems.Selected := nil;
  FLastNode := nil;
  tvItems.FullExpand;
  tvItems.Selected := N;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.tvItemsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  SaveData(tvItems.Selected);
end;

procedure TfrmMain.tvItemsChange(Sender: TObject; Node: TTreeNode);
begin
  LoadData(Node);
end;

procedure TfrmMain.LoadData(Node: TTreeNode);
var
  item: IJvSurveyItem;
  tmp: boolean;
begin
  nbDetails.PageIndex := 0;
  tmp := Modified;
  try
    if (Node = nil) then // empty
    begin
      edTitle.Text := '';
      edID.Value := 1;
      reDescription.Lines.Text := '';
      edHREF.Text := '';
      edRecipientName.Text := '';
      edRecipientEMail.Text := '';
      dtpReleaseDate.DateTime := Date;
      dtpExpirationDate.DateTime := Date;
    end
    else if (Node.Parent = nil) and (Node.Data <> nil) then // root
    begin
      edTitle.Text := FSurvey.Title;
      edID.Value := FSurvey.ID;
      reDescription.Lines.Text := StringReplace(StringReplace(FSurvey.Description, #13#10, ' ', [rfReplaceAll]), '<br>',
        #13#10, [rfReplaceAll]);
      edHREF.Text := FSurvey.ResultHRef;
      edRecipientName.Text := FSurvey.Recipient;
      edRecipientEMail.Text := FSurvey.RecipientMail;
      dtpReleaseDate.DateTime := FSurvey.ReleaseDate;
      dtpExpirationDate.DateTime := FSurvey.ExpiryDate;
      nbDetails.PageIndex := 0;
    end
    else if Node.Data <> nil then
    begin
      item := IJvSurveyItem(Node.Data);
      edItemTitle.Text := item.Title;
      reItemDescription.Lines.Text := item.Description;
      cbItemType.ItemIndex := Ord(item.SurveyType);
      chkRequired.Checked := item.Required;
      //      reItemChoices.Lines.CommaText := item.Choices;
      reItemChoices.Lines.Text := DecodeChoice(item.Choices, item.SurveyType);
      nbDetails.PageIndex := 1;
    end;
  finally
    Modified := tmp;
  end;
  FLastNode := Node;
end;

procedure TfrmMain.SaveData(Node: TTreeNode);
var
  item: IJvSurveyItem;
begin
  if (Node <> FLastNode) or not Modified then Exit;
  if (Node = nil) then
    // do nothing
  else if (Node.Parent = nil) and (Node.Data <> nil) then // root
  begin
    if Node = nil then Exit;
    FSurvey.Title := edTitle.Text;
    FSurvey.ID := edID.Value;
    FSurvey.Description := StringReplace(reDescription.Lines.Text, #13#10, '<br>', [rfReplaceAll]);
    FSurvey.ResultHRef := edHREF.Text;
    FSurvey.Recipient := edRecipientName.Text;
    FSurvey.RecipientMail := edRecipientEMail.Text;
    FSurvey.ReleaseDate := dtpReleaseDate.DateTime;
    FSurvey.ExpiryDate := dtpExpirationDate.DateTime;
  end
  else if Node.Data <> nil then
  begin
    item := IJvSurveyItem(Node.Data);
    item.Title := edItemTitle.Text;
    item.Required := chkRequired.Checked;
    Node.Text := item.Title;
    item.Description := reItemDescription.Lines.Text;
    item.SurveyType := TJvSurveyType(cbItemType.ItemIndex);
    item.Choices := EncodeChoice(reItemChoices.Lines.Text, item.SurveyType);
  end;
end;

procedure TfrmMain.tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TfrmMain.acPrevExecute(Sender: TObject);
begin
  tvItems.Selected := tvItems.Selected.getPrev;
end;

procedure TfrmMain.acNextExecute(Sender: TObject);
begin
  tvItems.Selected := tvItems.Selected.GetNext;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acMoveUp.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getPrevSibling <> nil);
  acMoveDown.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getNextSibling <> nil);
  acPrev.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getPrev <> nil);
  acNext.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.getNext <> nil);
  acDelete.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.Parent <> nil);
  acCopyItem.Enabled := (tvItems.Selected <> nil) and (tvItems.Selected.Parent <> nil) and (tvItems.Focused);
  if acCopyItem.Enabled then
    acCopy.ShortCut := 0
  else
    acCopy.ShortCut := acCopyItem.ShortCut;
end;

procedure TfrmMain.acAddExecute(Sender: TObject);
var
  item: IJvSurveyItem;
begin
  item := FSurvey.Items.Add;
  item.Title := SNewItemTitle;
  tvItems.Selected := AddItem(tvItems.Items.GetFirstNode, item);
  Modified := true;
end;

procedure TfrmMain.acMoveUpExecute(Sender: TObject);
begin
  tvItems.Selected.MoveTo(tvItems.Selected.getPrevSibling, naInsert);
  Modified := true;
end;

procedure TfrmMain.acMoveDownExecute(Sender: TObject);
begin
  tvItems.Selected.getNextSibling.MoveTo(tvItems.Selected, naInsert);
  Modified := true;
end;

function TfrmMain.AddItem(Parent: TTreeNode;
  Item: IJvSurveyItem): TTreeNode;
begin
  Result := tvItems.Items.AddChildObject(Parent, Item.Title, Pointer(Item));
  Result.ImageIndex := cSurveyItemImageIndex;
  Result.SelectedIndex := Result.ImageIndex;
  Modified := true;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
var
  N: TTreeNode;
  item:IJvSurveyItem;
  i:integer;
begin
  N := tvItems.Selected.GetNext;
  if N = nil then
    N := tvItems.Selected.GetPrev;
  item := IJvSurveyItem(tvItems.Selected.Data);
  tvItems.Selected.Data := nil;
  for i := 0 to FSurvey.Items.Count - 1 do
    if FSurvey.Items[i] = item then
    begin
      FSurvey.Items.Delete(i);
      Break;
    end;
  tvItems.Selected.Delete;
  tvItems.Selected := N;
  Modified := true;
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
begin
  if not CheckSave then Exit;
  Clear;
  Filename := '';
  Modified := false;
end;

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
const
  aFormat:array [1..3] of TJvSurveyFileFormat = (ffBinary,ffText,ffText);
begin
  SaveSurveyDialog.FileName := Filename;
  if SaveSurveyDialog.Execute then
    SaveToFile(SaveSurveyDialog.Filename,aFormat[SaveSurveyDialog.FilterIndex]);
end;

procedure TfrmMain.UpdateStatus;
begin
  JvStatusBar1.Panels[0].Text := Filename;
  if Modified then
    JvStatusBar1.Panels[1].Text := SModified
  else
    JvStatusBar1.Panels[1].Text := SReady;
end;

procedure TfrmMain.SetModified(const Value: boolean);
begin
  FModified := Value;
  UpdateStatus;
end;

procedure TfrmMain.SurveyChanged(Sender: TObject);
begin
  Modified := true;
end;

procedure TfrmMain.nbDetailsPageChanged(Sender: TObject);
begin
  case nbDetails.PageIndex of
    0:
      lblPage.Caption := SPageCaptionGlobal;
    1:
      lblPage.Caption := SPageCaptionItem;
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
var
  tmp: boolean;
begin
  tmp := Modified;
  try
    with dtpReleaseDate do
      SetBounds(8, Top, nbDetails.ClientWidth div 2 - 16, Height);
    with dtpExpirationDate do
      SetBounds(dtpReleaseDate.Left + dtpReleaseDate.Width + 8, Top, dtpReleaseDate.Width + 10, Height);
    lblExpDate.Left := dtpExpirationDate.Left;
    reDescription.WordWrap := reDescription.Height > edTitle.Height;
  finally
    Modified := tmp;
  end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  MessageBox(GetFocus,PChar(SAboutText),PChar(SAboutTitle),MB_OK or MB_ICONINFORMATION);
end;

procedure TfrmMain.acCopyItemExecute(Sender: TObject);
var item1,item2:IJvSurveyItem;
begin
  item1 := IJvSurveyItem(tvItems.Selected.Data);
  if item1 <> nil then
  begin
    item2 := FSurvey.Items.Add;
    item2.ID := FSurvey.Items.Count;
    item2.Title := item1.Title + SItemCopy;
    item2.Description := item1.Description;
    item2.Required    := item1.Required;
    item2.SurveyType  := item1.SurveyType;
    item2.Choices     := item1.Choices;
    item2.Responses   := item1.Responses;
    tvItems.Selected := AddItem(tvItems.Items.GetFirstNode,item2);
    if tvItems.Selected <> nil then
      tvItems.Selected.MakeVisible;
  end;
end;

procedure TfrmMain.acPreviewExecute(Sender: TObject);
var S:string;
begin
  S := ExtractFilePath(Application.ExeName) + 'js.exe';
  if not FileExists(S) then
  begin
    ShowMessageFmt(SSurveyorNotFoundFmt,[S]);
    Exit;
  end;
  if SaveFile then
    WinExec32AndWait(S + ' ' + Filename, SW_SHOWNORMAL);
end;

procedure TfrmMain.WMUser1(var Msg: TMessage);
begin
  Modified := false;
end;

end.

