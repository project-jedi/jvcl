{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Peter Thörnqvist

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
{$I jvcl.inc}
unit MainFrm;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList, Menus, HTTPApp,
  {$IFDEF COMPILER6_UP}HTTPProd,{$ENDIF}
  JvExExtCtrls, JvNetscapeSplitter, JvExComCtrls, JvListView, JvExStdCtrls,
  JvRichEdit, JvRadioGroup, JvDotNetControls;

type
  TfrmMain = class(TForm)
    alMain: TActionList;
    acAdd: TAction;
    acDelete: TAction;
    acExportHTML: TAction;
    acSave: TAction;
    acLoad: TAction;
    acReplace: TAction;
    acSaveAs: TAction;
    pnlLeft: TPanel;
    vertSplitter: TJvNetscapeSplitter;
    lvItems: TJvDotNetListView;
    pnlRight: TPanel;
    Label2: TLabel;
    reAnswer: TJvDotNetRichEdit;
    reQuestion: TJvDotNetRichEdit;
    Label1: TLabel;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAsHTML1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Add1: TMenuItem;
    Replace1: TMenuItem;
    Delete1: TMenuItem;
    acAbout: TAction;
    acOptions: TAction;
    About1: TMenuItem;
    N2: TMenuItem;
    Options1: TMenuItem;
    horzSplitter: TJvNetscapeSplitter;
    sbStatus: TStatusBar;
    ppHTMLBuilder: TPageProducer;
    acNew: TAction;
    New1: TMenuItem;
    SaveAs1: TMenuItem;
    N3: TMenuItem;
    acMoveUp: TAction;
    acMoveDown: TAction;
    popItems: TPopupMenu;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    acHelp: TAction;
    Label3: TLabel;
    acNext: TAction;
    acPrev: TAction;
    acNextItem: TAction;
    acPrevItem: TAction;
    N4: TMenuItem;
    Next1: TMenuItem;
    Previous1: TMenuItem;
    Bevel2: TBevel;
    acFullScreen: TAction;
    FullScreen1: TMenuItem;
    N5: TMenuItem;
    Help2: TMenuItem;
    N6: TMenuItem;
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acLoadExecute(Sender: TObject);
    procedure acExportHTMLExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure reAnswerEnter(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormResize(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure ppHTMLBuilderHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: string; TagParams: TStrings;
      var ReplaceText: string);
    procedure acAboutExecute(Sender: TObject);
    procedure lvItemsDeletion(Sender: TObject; Item: TListItem);
    procedure acNewExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure reQuestionEnter(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure acPrevExecute(Sender: TObject);
    procedure acNextItemExecute(Sender: TObject);
    procedure acPrevItemExecute(Sender: TObject);
    procedure acFullScreenExecute(Sender: TObject);
    procedure JvRadioGroup1ItemHint(Sender: TObject; Index: Integer;
      var AHint: String);
  private
    { Private declarations }
    LastFilename, LastHTMLFile: string;
    Modified: boolean;
    CurrentQuestion, CurrentAnswer: string;
    CurrentIndex: integer;
    procedure ShowPreview(const Filename: string);
    procedure UpdateStatus;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure ExportToHTML(const Filename: string);
    function CheckSave: boolean;
    procedure SaveItem(lv:TListItem);
    procedure LoadItem(lv:TListItem);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  IniFiles, OptionsFrm, FAQGlobals, ShellAPI, JvJVCLUtils;

resourcestring
  SFAQFileFilter = 'FAQ files (*.faq)|*.faq|All files (*.*)|*.*';
  SHTMLFilter = 'HTML files (*.htm;*,html)|*.htm;*.html|All files (*.*)|*.*';

{$R *.dfm}

procedure TfrmMain.LoadFromFile(const Filename: string);
var
  M: TIniFile;
  i, j: integer;
  S: TStringlist;
  tmp: string;
begin
  lvItems.Items.Clear;
  M := TIniFile.Create(Filename);
  try
    j := M.ReadInteger('FAQ', 'Count', 0);
    //    FTitle := M.ReadString('FAQ','Title',FTitle);
    //    FStylesheet := M.ReadString('FAQ','Stylesheet',FStylesheet);
    for i := 0 to j - 1 do
    begin
      tmp := DoLoadReplace(M.ReadString('FAQ', Format('Q%d', [i]), ''));
      S := TStringlist.Create;
      S.Text := DoLoadReplace(M.ReadString('FAQ', Format('A%d', [i]), ''));
      with lvItems.Items.Add do
      begin
        Caption := tmp;
        Data := S;
      end;
    end;
  finally
    M.Free;
  end;
  Modified := false;
  UpdateStatus;
end;

procedure TfrmMain.SaveToFile(const Filename: string);
var
  M: TIniFile;
  i, j: integer;
begin
  SaveItem(lvItems.Selected);
  j := lvItems.Items.Count;
  M := TIniFile.Create(Filename);
  try
    M.EraseSection('FAQ');
    M.WriteInteger('FAQ', 'Count', j);
    //    M.WriteString('FAQ','Title',FTitle);
    //    M.writeString('FAQ','Stylesheet',FStylesheet);
    for i := 0 to j - 1 do
    begin
      M.WriteString('FAQ', Format('Q%d', [i]), DoSaveReplace(lvItems.Items[i].Caption));
      M.WriteString('FAQ', Format('A%d', [i]), DoSaveReplace(TStrings(lvItems.Items[i].Data).Text));
    end;
  finally
    M.Free;
  end;
  Modified := false;
  UpdateStatus;
end;

procedure TfrmMain.ExportToHTML(const Filename: string);
var
  S: TStringlist;
  i: integer;
begin
  SaveItem(lvItems.Selected);
  S := TStringlist.Create;
  try
    ppHTMLBuilder.HTMLDoc := FAQOptions.Header;
    S.Add(ppHTMLBuilder.Content);
    ppHTMLBuilder.HTMLDoc := FAQOptions.Item;
    for i := 0 to lvItems.Items.Count - 1 do
    begin
      CurrentIndex := i;
      CurrentQuestion := StringReplace(lvItems.Items[i].Caption, #13#10,'<br>',[rfReplaceAll]);
      CurrentAnswer := StringReplace(TStrings(lvItems.Items[i].Data).Text,#13#10,'<br>',[rfReplaceAll]);
      S.Add(ppHTMLBuilder.Content);
    end;
    ppHTMLBuilder.HTMLDoc := FAQOptions.Footer;
    S.Add(ppHTMLBuilder.Content);
    S.SaveToFile(Filename);
  finally
    S.Free;
  end;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  acDelete.Enabled := (lvItems.ItemIndex < lvItems.Items.Count) and (lvItems.ItemIndex > -1);
  acAdd.Enabled := (reQuestion.Lines.Text <> '');
  acReplace.Enabled := acDelete.Enabled and acAdd.Enabled;
  acMoveUp.Enabled := (lvItems.ItemIndex > 0);
  acMoveDown.Enabled := (lvItems.ItemIndex < lvItems.Items.Count - 1); 
  UpdateStatus;
end;

procedure TfrmMain.acAddExecute(Sender: TObject);
var
  lv: TlistItem;
begin
  lv := lvItems.FindCaption(0, FlattenText(reQuestion.Lines), false, true, true);
  if lv = nil then
  begin
    lv := lvItems.Items.Add;
    SaveItem(lv);
  end;
  lv.Selected := true;
  lv.Focused := true;
  Modified := true;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
var
  lv: TListItem;
  i: integer;
begin
  lv := lvItems.Selected;
  i := lv.Index;
  lv.Delete;
  if (i > 0) and (i < lvItems.Items.Count) then
    lvItems.Items[i - 1].Selected := true
  else if lvItems.Items.Count > 0 then
    lvItems.Items[0].Selected := true;
  lvItems.ItemFocused := lvItems.Selected;
  lvItemsSelectItem(Sender, lvItems.Selected, true);
  Modified := true;
end;

procedure TfrmMain.acLoadExecute(Sender: TObject);
begin
  if CheckSave then
  begin
    ForceCurrentDirectory := true;
    with TOpenDialog.Create(nil) do
    try
      Filename := LastFilename;
      Filter := SFAQFileFilter;
      if Execute then
      begin
        LastFilename := Filename;
        LoadFromFile(LastFilename);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.acExportHTMLExecute(Sender: TObject);
begin
  ForceCurrentDirectory := true;
  with TSaveDialog.Create(nil) do
  try
    Options := Options + [ofOverWritePrompt];
    Filter := SHTMLFilter;
    Filename := LastHTMLFile;
    if Execute then
    begin
      ExportToHTML(Filename);
      LastHTMLFile := Filename;
    end;
    ShowPreview(Filename);
  finally
    Free;
  end;
end;

procedure TfrmMain.acSaveExecute(Sender: TObject);
begin
  if LastFileName = '' then
    acSaveAs.Execute
  else
    SaveToFile(LastFileName);
end;

procedure TfrmMain.acReplaceExecute(Sender: TObject);
var
  lv: TListItem;
begin
  lv := lvItems.Selected;
  if lv <> nil then
  begin
    lv.Caption := FlattenText(reQuestion.Lines);
    TStrings(lv.Data).Assign(reAnswer.Lines);
  end;
  Modified := true;
end;

procedure TfrmMain.reAnswerEnter(Sender: TObject);
begin
  reAnswer.SelectAll;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSave;
end;

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
begin
  ForceCurrentDirectory := true;
  with TSaveDialog.Create(nil) do
  try
    Options := Options + [ofOverWritePrompt];
    Filename := LastFilename;
    Filter := SFAQFileFilter;
    DefaultExt := 'faq';
    if Execute then
    begin
      LastFilename := Filename;
      acSave.Execute;
    end;
  finally
    Free;
  end;
end;

procedure TfrmMain.lvItemsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  reQuestion.Clear;
  reAnswer.Clear;
  if (Item <> nil) and Selected then
    LoadItem(Item);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  lvItems.Columns[0].Width := -2;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acOptionsExecute(Sender: TObject);
begin
  TfrmOptions.Edit;
end;

procedure TfrmMain.UpdateStatus;
const
  cModified: array[boolean] of PChar = ('  Ready', '  Modified');
begin
  if LastFilename = '' then
    sbStatus.Panels[0].Text := '  New'
  else
    sbStatus.Panels[0].Text := '  ' + LastFilename;
  sbStatus.Panels[1].Text := Format('  %d item(s)', [lvItems.Items.Count]);
  sbStatus.Panels[2].Text := cModified[Modified];
end;

procedure TfrmMain.ppHTMLBuilderHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: string; TagParams: TStrings; var ReplaceText: string);
begin
  if AnsiSameText(TagString, 'TITLE') then
    ReplaceText := FAQOptions.Title
  else if AnsiSameText(TagString, 'STYLESHEET') then
    ReplaceText := FAQOptions.StylePath + FAQOptions.Stylesheet
  else if AnsiSameText(TagString, 'Q_IMAGE') then
    ReplaceText := FAQOptions.ImagePath + FAQOptions.QImage
  else if AnsiSameText(TagString, 'A_IMAGE') then
    ReplaceText := FAQOptions.ImagePath + FAQOptions.AImage
  else if AnsiSameText(TagString, 'Q_ITEM') then
    ReplaceText := CurrentQuestion
  else if AnsiSameText(TagString, 'A_ITEM') then
    ReplaceText := CurrentAnswer
  else if AnsiSameText(TagString, 'FAQINDEX') then
    ReplaceText := IntToStr(CurrentIndex);
end;

procedure TfrmMain.ShowPreview(const Filename: string);
begin
  if MessageBox('Show HTML Preview?', 'Preview HTML', MB_YESNO) = IDYES then
    ShellExecute(Handle, 'open', PChar(Filename), nil, nil, SW_NORMAL);
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  ShowMessage('FAQ Builder 1.00'#13'Copyright (c) 2004 by Peter Thörnqvist; all rights reserved.'#13'Part of the JEDI-JVCL tools and released under MPL.');
end;

function TfrmMain.CheckSave: boolean;
var
  S: string;
begin
  Result := true;
  if Modified then
  begin
    if LastFilename = '' then
      S := 'Save changes to current FAQ file?'
    else
      S := Format('Save changes to "%s"?', [LastFilename]);
    case MessageBox(S, 'Confirm save', MB_YESNOCANCEL) of
      IDYES:
        Result := acSave.Execute;
      IDNO: ;
      IDCANCEL:
        Result := false;
    end;
  end;
end;

procedure TfrmMain.lvItemsDeletion(Sender: TObject; Item: TListItem);
begin
  if (Item <> nil) and (Item.Data <> nil) then
  begin
    TObject(Item.Data).Free;
    Item.Data := nil;
  end;
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
begin
  if CheckSave then
  begin
    lvItems.Items.Clear;
    reQuestion.Clear;
    reAnswer.Clear;
    LastFilename := '';
    Modified := false;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmMain.acMoveUpExecute(Sender: TObject);
begin
  if lvItems.ItemIndex < 0 then Exit;
  SaveItem(lvItems.Selected);
  lvItems.MoveUp(lvItems.ItemIndex);
  Modified := true;
end;

procedure TfrmMain.acMoveDownExecute(Sender: TObject);
begin
  if lvItems.ItemIndex < 0 then Exit;
  SaveItem(lvItems.Selected);
  lvItems.MoveDown(lvItems.ItemIndex);
  Modified := true;
end;

procedure TfrmMain.LoadItem(lv: TListItem);
begin
  if lv <> nil then
  begin
    reQuestion.Lines.Text := lv.Caption;
    if lv.Data <> nil then
      reAnswer.Lines.Assign(TStrings(lv.Data));
  end;
end;

procedure TfrmMain.SaveItem(lv: TListItem);
begin
  if lv <> nil then
  begin
    if lv.Data = nil then
    begin
      lv.Caption := FlattenText(reQuestion.Lines);
      lv.Data := TStringlist.Create;
    end;
    TStringlist(lv.Data).Assign(reAnswer.Lines);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FAQOptions.LoadFromFile(ChangeFileExt(Application.Exename,'.ini'));
end;

procedure TfrmMain.acHelpExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(ExtractFilePath(Application.Exename) + 'help.htm'),nil,nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.reQuestionEnter(Sender: TObject);
begin
  reQuestion.SelStart := 0;
end;

procedure TfrmMain.acNextExecute(Sender: TObject);
begin
  SelectNext(ActiveControl, true, true);
end;

procedure TfrmMain.acPrevExecute(Sender: TObject);
begin
  SelectNext(ActiveControl, false, true);
end;

procedure TfrmMain.acNextItemExecute(Sender: TObject);
begin
  lvItems.SelectNextItem;
end;

procedure TfrmMain.acPrevItemExecute(Sender: TObject);
begin
  lvItems.SelectPrevItem;
end;

procedure TfrmMain.acFullScreenExecute(Sender: TObject);
var WP:TWindowPlacement;
begin
  acFullScreen.Checked := not acFullScreen.Checked;
  WP.length := sizeof(WP);
  GetWindowPlacement(Handle,@WP);
  if acFullScreen.Checked then
  begin
    if lvItems.Focused then
      reQuestion.SetFocus;
    vertSplitter.Minimized := true;
    horzSplitter.Maximized := true;
    WP.showCmd := SW_MAXIMIZE;
  end
  else
  begin
    vertSplitter.Minimized := false;
    horzSplitter.Maximized := false;
    WP.showCmd := SW_RESTORE;
  end;
  SetWindowPlacement(Handle,@WP);

end;

procedure TfrmMain.JvRadioGroup1ItemHint(Sender: TObject; Index: Integer;
  var AHint: String);
begin
  AHint := Format('This is the hint for item %d',[Index + 1]);
end;

end.

