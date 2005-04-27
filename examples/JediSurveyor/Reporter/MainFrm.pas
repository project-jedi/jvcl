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

{$I jvcl.inc}

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdActns, ActnList, Menus, ExtCtrls, ComCtrls, JvStatusBar,
  JvComCtrls, JvSurveyIntf, JvDialogs, StdCtrls, JvListView, HTTPApp,
  JvComponent, JvImageSquare {$IFDEF COMPILER6_UP}, HTTPProd, JvExComCtrls{$ENDIF};

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    sbStatus: TJvStatusBar;
    tvItems: TJvTreeView;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    alMain: TActionList;
    acOpen: TAction;
    acSaveReport: TAction;
    acExit: TAction;
    acAddResponse: TAction;
    acOpen1: TMenuItem;
    acSaveAs1: TMenuItem;
    N1: TMenuItem;
    acExit1: TMenuItem;
    acAbout: TAction;
    acHelp: TAction;
    il16: TImageList;
    About1: TMenuItem;
    N4: TMenuItem;
    Help2: TMenuItem;
    Add2: TMenuItem;
    acPrintPreview: TAction;
    N5: TMenuItem;
    Print1: TMenuItem;
    N6: TMenuItem;
    OpenSurveyDialog: TJvOpenDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    SaveReportDialog: TJvSaveDialog;
    acLoadReport: TAction;
    LoadReport1: TMenuItem;
    N7: TMenuItem;
    ppPrintPreview: TPageProducer;
    Panel2: TPanel;
    nbDetails: TNotebook;
    lvGlobalStats: TListView;
    lvItemStats: TJvListView;
    reFreeForm: TRichEdit;
    il24: TImageList;
    View1: TMenuItem;
    acComments: TAction;
    Comments1: TMenuItem;
    acDupeWarning: TAction;
    acDupeWarning1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acOpenExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure acAddResponseExecute(Sender: TObject);
    procedure acPrinterSettingsExecute(Sender: TObject);
    procedure acPrintPreviewExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acSaveReportExecute(Sender: TObject);
    procedure acLoadReportExecute(Sender: TObject);
    procedure ppPrintPreviewHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: string; TagParams: TStrings;
      var ReplaceText: string);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acCommentsExecute(Sender: TObject);
    procedure acDupeWarningExecute(Sender: TObject);
  private
    FFilename: string;
    FResponses: TStringlist;
    FCurrentItem: IJvSurveyItem;
    FSurvey: IJvSurvey;
    procedure LoadView;
    procedure LoadData(Node: TTreeNode);
    function GetResponseValue(item: IJvSurveyItem; Index: integer): integer;
    function AddItem(Parent: TTreeNode; item: IJvSurveyItem): TTreeNode;
    procedure AddResponses(item: IJvSurveyItem; Index: integer; const SurveyTaker: IJvSurveyTaker);
    procedure LoadFromFile(const Filename: string; ClearResponses: boolean);
    procedure LoadFromResponse(const Filename: string);
    procedure SaveReport(const Filename: string; Format: TJvSurveyFileFormat);
    property Filename: string read FFilename write FFilename;
    function GetReportHTMLContent: string;
    function GetReportHTMLSummary: string;
    procedure UpdateStatusBar;
  public
  end;

var
  frmMain: TfrmMain;
resourcestring
  SOpenSurveyTitle = 'Open Survey...';
  SAddUserResponseTitle = 'Add User Response File(s)';

implementation

uses
  JvSurveyUtils, JclStrings, Math, JvSimpleXML, JvJCLUtils, CommentsFrm;

{$R *.dfm}

resourcestring
  SFmtInvalidResponseFile =
    'The file "%s" is not compatible with the currently loaded survey: please select another response file.';
  SFmtResponseAlreadyLoaded = 'Responses from the file "%s" or user "%s" has already been added to the report.';
  SFmtUnmatchedSurveyType = 'SurveyTypes does not match (index %d)';
  SFmtResponse = '%0:s\nResponse from %1:s:\n%0:s\n\n%2:s\n\n';
  SFmtComment = '%0:s\nComments from %1:s:\n%0:s\n\n%2:s\n\n';
  SFmtTemplateNotFound = 'Unable to find print template (%s)';
  SAboutText = 'JEDI Surveyor Reporter, version 1.0';
  SAboutTitle = 'About Reporter...';

  SFmtHTMLTableSurveySummary =
    '<table class="TableSurveySummary">' +
    '<tr class="TRSurveySummary"><th class="THSurveySummary">Title</th><th class="THSurveySummary">ReleaseDate</th>' +
    '<th class="THSurveySummary">ExpiryDate</th><th class="THSurveySummary">Responses</th><th class="THSurveySummary">Questions</th></tr>' +
    '<tr class="TRSurveySummary"><td class="TDSurveySummary">%s</td><td class="TDSurveySummary">%s&nbsp;</td>' +
    '<td class="TDSurveySummary">%s</td><td class="TDSurveySummary">%d</td><td class="TDSurveySummary">%d</td></tr></table>';
  SHTMLNoItemsToDisplay =
    '<h4>There are no items in this survey: nothing to display</h4>';
  SFmtHTMLTableSurveyItemHeader =
    '<h4>Item #%d</h4><table class="TableSurveyItemHeader"><tr class="TRSurveyItemHeader">' +
    '<th class="THSurveyItemHeader">Title</th>' +
    '<th class="THSurveyItemHeader">Description</th>' +
    '<th class="THSurveyItemHeader">Type</th></tr>' +
    '<tr class="TRSurveyItemHeader">' +
    '<td class="TDSurveyItemHeader">%s&nbsp;</td>' +
    '<td class="TDSurveyItemHeader">%s&nbsp;</td>' +
    '<td class="TDSurveyItemHeader">%s&nbsp;</td></tr><tr><td colspan="3">';
  SFmtHTMLTableSurveyItemDetail =
    '<table class="TableSurveyItemDetail">' +
    '<tr class="TRSurveyItemDetail"><th  class="THSurveyItemDetail">Responses</th></tr>' +
    '<tr class="TRSurveyItemDetail"><td class="TDSurveyItemDetail">%s&nbsp;</td></tr></table>';
  SFmtHTMLTableSurveyItemFooter = '</td></tr></table>';
  STableSurveyItemDetail =
    '<table class="TableSurveyItemDetail"><tr class="TRSurveyItemDetail">' +
    '<th class="THSurveyItemDetail">Choices</th><th class="THSurveyItemDetail">Responses</th></tr>';
  SFmtHTMLTRSurveyItemDetail = '<tr  class="TRSurveyItemDetail"><td  class="TDSurveyItemDetail">%s&nbsp;</td>' +
    '<td  class="TDSurveyItemDetail">%s&nbsp;</td></tr>';
  SHTMLSpacer = '&nbsp;';
  SHTMLTableEnd = '</table>';
  STableCommentHeader = '<table class="TableSurveyItemDetail"><tr colspan="3" class="TRSurveyItemDetail"><th class="THSurveyItemDetail">Comments</th></tr>';
  SFmtTableCommentDetail = '<tr colspan="3" class="TRSurveyItemDetail"><td class="TDSurveyItemDetail">%s</td></tr>';

const
  cSurveyItemImageIndex = 22;
  cDelimChar = '=';
  cDelimLength = 60;
  cReportFileExt = '.jsr';
  cPrintReportExt = '.htm';
  cPrintTemplate = 'Data\SurveyTemplate.htt';

function MakeString(Ch: char; Count: integer): string;
begin
  SetLength(Result, Count);
  if Count > 0 then
    FillChar(Result[1], Count, Ch);
end;

procedure TfrmMain.LoadFromFile(const Filename: string; ClearResponses: boolean);
var
  i: integer;
begin
  FSurvey.LoadFromFile(Filename);
  self.Filename := Filename;
  // clear any responses added to survey (but no to report)
  if ClearResponses then
  begin
    FResponses.Clear;
    for i := 0 to FSurvey.Items.Count - 1 do
      FSurvey.Items[i].Responses := '';
  end
  else
    // load any previous survey takers already added to the report
    FResponses.CommaText := FSurvey.SurveyTaker.ID;
  LoadView;
  SaveReportDialog.Filename := ChangeFileExt(Filename, cReportFileExt);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSurvey := CreateSurvey;
  FResponses := TStringlist.Create;
  FResponses.Sorted := acDupeWarning.Checked;
  nbDetails.PageIndex := 0;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FResponses.Free;
  FSurvey := nil;
end;

procedure TfrmMain.LoadView;
var
  N: TTreeNode;
  i: integer;
begin
  N := tvItems.Items.GetFirstNode;
  if N <> nil then
    N.DeleteChildren;
  N.Data := Pointer(FSurvey);
  for i := 0 to FSurvey.Items.Count - 1 do
    AddItem(N, FSurvey.Items[i]);
  LoadData(N);
  tvItems.FullExpand;
end;

function TfrmMain.AddItem(Parent: TTreeNode;
  item: IJvSurveyItem): TTreeNode;
begin
  Result := tvItems.Items.AddChildObject(Parent,
    item.Title, Pointer(item));
  Result.ImageIndex := cSurveyItemImageIndex;
  Result.SelectedIndex := Result.ImageIndex;
end;

procedure TfrmMain.acOpenExecute(Sender: TObject);
begin
  OpenSurveyDialog.Filter := SSurveyFileFilter;
  OpenSurveyDialog.Filename := Filename;
  OpenSurveyDialog.Title := SOpenSurveyTitle;
  if OpenSurveyDialog.Execute then
    LoadFromFile(OpenSurveyDialog.Filename, true);
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

function TfrmMain.GetResponseValue(item: IJvSurveyItem;
  Index: integer): integer;
var
  S: TStringlist;
begin
  Result := 0;
  if item.SurveyType = stFreeForm then Exit;
  S := TStringlist.Create;
  try
    S.Text := DecodeResponse(item.Responses, item.SurveyType);
    if (Index < 0) or (Index >= S.Count) then Exit;
    Result := StrToIntDef(S[Index], 0);
  finally
    S.Free;
  end;
end;

procedure TfrmMain.LoadData(Node: TTreeNode);
var
  li: TListItem;
  S: TStringlist;
  i: integer;
begin
  FCurrentItem := nil;
  if (Node = nil) or not FileExists(Filename) then Exit;
  if Node.Parent = nil then // root
  begin
    nbDetails.PageIndex := 0;
    lvGlobalStats.Items[0].SubItems[0] := FSurvey.Title;
    lvGlobalStats.Items[1].SubItems[0] := DateToStr(FSurvey.ReleaseDate);
    lvGlobalStats.Items[2].SubItems[0] := DateToStr(FSurvey.ExpiryDate);
    lvGlobalStats.Items[3].SubItems[0] := IntToStr(FResponses.Count);
    lvGlobalStats.Items[4].SubItems[0] := IntToStr(FSurvey.Items.Count);
  end
  else if Node.Data <> nil then // item
  begin
    FCurrentItem := IJvSurveyItem(Node.Data);
    lvItemStats.Items.Clear;
    if FCurrentItem.SurveyType <> stFreeForm then
    begin
      S := TStringlist.Create;
      try
        S.Text := DecodeChoice(FCurrentItem.Choices, FCurrentItem.SurveyType);
        for i := 0 to S.Count - 1 do
        begin
          if S[i] = '' then Continue;
          li := lvItemStats.Items.Add;
          li.Caption := S[i];
          li.SubItems.Add(IntToStr(GetResponseValue(FCurrentItem, i)));
        end;
      finally
        S.Free;
      end;
      nbDetails.PageIndex := 1;
    end
    else
    begin
      nbDetails.PageIndex := 2;
      reFreeForm.Lines.Text := DecodeString(FCurrentItem.Responses);
    end;
  end;
  UpdateStatusBar;
end;

procedure TfrmMain.tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TfrmMain.tvItemsChange(Sender: TObject; Node: TTreeNode);
begin
  LoadData(Node);
end;

procedure TfrmMain.acAddResponseExecute(Sender: TObject);
var
  i: integer;
begin
  OpenSurveyDialog.Filter := SResponseFileFilter;
//  OpenSurveyDialog.Filename := Filename;
  OpenSurveyDialog.Title := SAddUserResponseTitle;
  OpenSurveyDialog.Options := OpenSurveyDialog.Options + [ofAllowMultiSelect];
  if OpenSurveyDialog.Execute then
  begin
    for i := 0 to OpenSurveyDialog.Files.Count - 1 do
      LoadFromResponse(OpenSurveyDialog.Files[i]);
    LoadData(tvItems.Selected);
  end;
  OpenSurveyDialog.Options := OpenSurveyDialog.Options - [ofAllowMultiSelect];
end;

procedure TfrmMain.LoadFromResponse(const Filename: string);
var
  ASurvey: IJvSurvey;
  i: integer;
begin
  ASurvey := CreateSurvey;
  ASurvey.LoadFromFile(Filename);
  if ASurvey.ID <> FSurvey.ID then
    raise
      Exception.CreateFmt(SFmtInvalidResponseFile, [Filename]);
  if (FResponses.IndexOf(ASurvey.SurveyTaker.ID) > -1) and acDupeWarning.Checked then
    ShowMessageFmt(SFmtResponseAlreadyLoaded, [Filename, ASurvey.SurveyTaker.ID])
  else
  begin
    FResponses.Add(ASurvey.SurveyTaker.ID);
    for i := 0 to ASurvey.Items.Count - 1 do
      AddResponses(ASurvey.Items[i], i, ASurvey.SurveyTaker);
  end;
end;

procedure TfrmMain.AddResponses(item: IJvSurveyItem; Index: integer; const SurveyTaker: IJvSurveyTaker);
var
  S, tmp: string;


  function Decode(S: WideString): TList;
  var
    ST: TStringlist;
    i: integer;
  begin
    Result := TList.Create;
    ST := TStringlist.Create;
    try
      StrTokenToStrings(S, cRecordSeparator, ST);
      for i := 0 to ST.Count - 1 do
        Result.Add(Pointer(StrToIntDef(ST[i], 0)));
    finally
      ST.Free;
    end;
  end;

  procedure MergeResponses(item1, item2: IJvSurveyItem);
  var
    S1, S2: TList;
    i: integer;
    tmp: string;
  begin
    S1 := Decode(item1.Responses);
    S2 := Decode(item2.Responses);
    S1.Count := Max(S1.Count, S2.Count);
    S2.Count := Max(S1.Count, S2.Count);
    for i := 0 to S2.Count - 1 do
      S1[i] := Pointer(integer(S1[i]) + integer(S2[i]));
    tmp := '';
    for i := 0 to S1.Count - 1 do
      tmp := tmp + IntToStr(integer(S1[i])) + cRecordSeparator;
    if MyAnsiLastChar(tmp) = cRecordSeparator then
      SetLength(tmp, Length(tmp) - 1);
    item1.Responses := tmp;
    S1.Free;
    S2.Free;
  end;
begin
  if (Index < 0) or (Index >= FSurvey.Items.Count) then Exit;
  if (FSurvey.Items[Index].SurveyType <> item.SurveyType) then
    raise Exception.CreateFmt(SFmtUnmatchedSurveyType, [Index]);
  if FSurvey.Items[Index].SurveyType = stFreeForm then
  begin
    S := trim(FSurvey.Items[Index].Responses);
    tmp := Format(SFmtResponse, [MakeString(cDelimChar, cDelimLength), SurveyTaker.UserName, trim(item.Responses)]);
    if (S = '') and (trim(item.Comments) <> '') then
      S := tmp
    else if trim(item.Responses) <> '' then
      S := S + tmp;
    FSurvey.Items[Index].Responses := S;
  end
  else
    MergeResponses(FSurvey.Items[Index], item);
  //add comments
  S := trim(FSurvey.Items[Index].Comments);
  tmp := Format(SFmtComment, [MakeString(cDelimChar, cDelimLength), SurveyTaker.UserName, trim(item.Comments)]);
  if (S = '') and (trim(item.Comments) <> '') then
    S := tmp
  else if trim(item.Comments) <> '' then
    S := S + tmp;
  FSurvey.Items[Index].Comments := S;
end;

procedure TfrmMain.acPrinterSettingsExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TfrmMain.acPrintPreviewExecute(Sender: TObject);
var
  S: string;
begin
  S := ExtractFilePath(Application.ExeName) + cPrintTemplate;
  if not FileExists(S) then
    raise Exception.CreateFmt(SFmtTemplateNotFound, [S]);
  ppPrintPreview.HTMLFile := S;
  // generate and save report HTML file
  with TStringlist.Create do
  try
    Text := ppPrintPreview.Content;
    S := ChangeFileExt(S, cPrintReportExt);
    SaveToFile(S);
    // open in browser
    OpenObject(S);
  finally
    Free;
  end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  Windows.MessageBox(GetActiveWindow, PChar(SAboutText), PChar(SAboutTitle), MB_OK or MB_ICONINFORMATION);
end;

procedure TfrmMain.acSaveReportExecute(Sender: TObject);
const
  aFormat: array[1..3] of TJvSurveyFileFormat = (ffBinary, ffText, ffText);
begin
  if SaveReportDialog.Execute then
    SaveReport(SaveReportDialog.Filename, aFormat[SaveReportDialog.FilterIndex]);
end;

procedure TfrmMain.SaveReport(const Filename: string; Format: TJvSurveyFileFormat);
//var
//  i: integer;
//  X: TJvSimpleXML;
//  elem:TJvSimpleXMLElem;
begin
  FSurvey.SurveyTaker.UserName := '';
  FSurvey.SurveyTaker.MailAddress := '';
  // save all loaded respones as  a comma-separated lsit
  FSurvey.SurveyTaker.ID := FResponses.CommaText;
  FSurvey.SaveToFile(Filename, Format);
end;

procedure TfrmMain.acLoadReportExecute(Sender: TObject);
begin
  OpenSurveyDialog.Filter := SReportFileFilter;
  OpenSurveyDialog.FileName := SaveReportDialog.Filename;
  if OpenSurveyDialog.Execute then
    LoadFromFile(OpenSurveyDialog.Filename, false);
end;

function TfrmMain.GetReportHTMLSummary: string;
begin
  Result := Format(SFmtHTMLTableSurveySummary,
    [FSurvey.Title, DateToStr(FSurvey.ReleaseDate), DateToStr(FSurvey.ExpiryDate),
    FResponses.Count, FSurvey.Items.Count]);
end;

function TfrmMain.GetReportHTMLContent: string;
var
  i, j: integer;
  C, R: TStringlist;
  function ConvertCRLFToBR(const S: string): string;
  begin
    Result := StringReplace(S, '\n', '<br>', [rfReplaceAll]);
    Result := StringReplace(Result, #13#10, '<br>', [rfReplaceAll]);
  end;
begin
  if FSurvey.Items.Count = 0 then
  begin
    Result := SHTMLNoItemsToDisplay;
    Exit;
  end;
  C := TStringlist.Create;
  R := TStringlist.Create;
  try
    for i := 0 to FSurvey.Items.Count - 1 do
    begin
      FSurvey.Items[i].SortResponses;
      // TODO: add comments
      Result := Result + Format(SFmtHTMLTableSurveyItemHeader,
        [i + 1, FSurvey.Items[i].Title, FSurvey.Items[i].Description, EncodeType(FSurvey.Items[i].SurveyType)]);
      C.Text := DecodeChoice(FSurvey.Items[i].Choices, FSurvey.Items[i].SurveyType);
      R.Text := DecodeResponse(FSurvey.Items[i].Responses, FSurvey.Items[i].SurveyType);
      if FSurvey.Items[i].SurveyType = stFreeForm then
        Result := Result + Format(SFmtHTMLTableSurveyItemDetail, [ConvertCRLFToBR(R.Text)])
      else
      begin
        while C.Count > R.Count do
          R.Add(SHTMLSpacer);
        Result := Result + STableSurveyItemDetail;
        for j := 0 to C.Count - 1 do
          Result := Result + Format(SFmtHTMLTRSurveyItemDetail, [ConvertCRLFToBR(C[j]), ConvertCRLFToBR(R[j])]);
        Result := Result + SHTMLTableEnd;
      end;
      C.Text := ConvertCRLFToBR(FSurvey.Items[i].Comments);
      if C.Count > 0 then
      begin
        Result := Result + STableCommentHeader;
        for j := 0 to C.Count - 1 do
          Result := Result + Format(SFmtTableCommentDetail,[ConvertCRLFToBR(C[j])]);
        Result := Result + SHTMLTableEnd;
      end;
      Result := Result + SFmtHTMLTableSurveyItemFooter;
    end;
  finally
    R.Free;
    C.Free;
  end;
end;

procedure TfrmMain.ppPrintPreviewHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: string; TagParams: TStrings; var ReplaceText: string);
begin
  if AnsiSameText(TagString, 'SURVEYTITLE') then // DO NOT LOCALIZE
    ReplaceText := FSurvey.Title
  else if AnsiSameText(TagString, 'SURVEYSUMMARY') then // DO NOT LOCALIZE
    ReplaceText := GetReportHTMLSummary
  else if AnsiSameText(TagString, 'SURVEYCONTENT') then // DO NOT LOCALIZE
    ReplaceText := GetReportHTMLContent;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
const
  // TODO: localize
  cRequired: array[boolean] of PChar = ('  Optional', '  Required');
  cType: array[TJvSurveyType] of PChar = ('  Exclusive', '  Multiple', '  Free Form');
begin
  sbStatus.Panels[0].Width := Canvas.TextWidth(FFilename) + 8;
  if sbStatus.Panels[0].Width < 100 then
    sbStatus.Panels[0].Width := 100;
  sbStatus.Panels[0].Text := '  ' + FFilename;
  if FCurrentItem <> nil then
  begin
    acComments.Enabled := true;
    sbStatus.Panels[1].Text := cType[FCurrentItem.SurveyType];
    sbStatus.Panels[2].Text := cRequired[FCurrentItem.Required];
  end
  else
  begin
    acComments.Enabled := false;
    sbStatus.Panels[1].Text := '';
    sbStatus.Panels[2].Text := '';
  end;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  alMain.UpdateAction(nil);
end;

procedure TfrmMain.acCommentsExecute(Sender: TObject);
begin
  TfrmComments.Comments(FCurrentItem.Title, FCurrentItem.Comments);
end;

procedure TfrmMain.acDupeWarningExecute(Sender: TObject);
begin
  acDupeWarning.Checked := not acDupeWarning.Checked;
  FResponses.Sorted := acDupeWarning.Checked;
end;

end.

