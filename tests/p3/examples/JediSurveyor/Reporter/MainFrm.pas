unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdActns, ActnList, Menus, ExtCtrls, ComCtrls, JvStatusBar,
  JvComCtrls, JvSurveyIntf, JvDialogs, StdCtrls, JvListView, HTTPApp,
  HTTPProd, JvComponent, JvImageWindow;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    JvStatusBar1: TJvStatusBar;
    tvItems: TJvTreeView;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Edit7: TMenuItem;
    Help1: TMenuItem;
    alMain: TActionList;
    acOpen: TAction;
    acSaveReport: TAction;
    acExit: TAction;
    acAddResponse: TAction;
    acCopy: TEditCopy;
    acCut: TEditCut;
    acPaste: TEditPaste;
    acSelectAll: TEditSelectAll;
    acUndo: TEditUndo;
    acOpen1: TMenuItem;
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
    pnlTop: TPanel;
    isSurveyType: TJvImageSquare;
    il24: TImageList;
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
  private
    { Private declarations }
    FFilename: string;
    FResponses: TStringlist;
    FSurvey: IJvSurvey;
    procedure LoadView;
    procedure LoadData(Node: TTreeNode);
    function GetResponseValue(item: IJvSurveyItem; Index: integer): integer;
    function AddItem(Parent: TTreeNode; item: IJvSurveyItem): TTreeNode;
    procedure AddResponses(item: IJvSurveyItem; Index: integer);
    procedure LoadFromFile(const Filename: string; ClearResponses: boolean);
    procedure LoadFromResponse(const Filename: string);
    procedure SaveReport(const Filename: string);
    property Filename: string read FFilename write FFilename;
    function GetReportHTMLContent: string;
    function GetReportHTMLSummary: string;
    procedure UpdateViews(Node: TTreeNode);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  JvSurveyUtils, JclStrings, Math, JvSimpleXML, JvFunctions;

{$R *.DFM}

const
  cSurveyItemImageIndex = 22;


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
  SaveReportDialog.Filename := ChangeFileExt(Filename, '.jsr');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSurvey := CreateSurvey;
  isSurveyType.Anchors := [akRight, akTop];
  FResponses := TStringlist.Create;
  FResponses.Sorted := true;
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
  item: IJvSurveyItem;
  li: TListItem;
  S: TStringlist;
  i: integer;
begin
//  UpdateViews(Node);
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
    item := IJvSurveyItem(Node.Data);
    lvItemStats.Items.Clear;
    if item.SurveyType <> stFreeForm then
    begin
      S := TStringlist.Create;
      try
        S.Text := DecodeChoice(item.Choices, item.SurveyType);
        for i := 0 to S.Count - 1 do
        begin
          if S[i] = '' then Continue;
          li := lvItemStats.Items.Add;
          li.Caption := S[i];
          li.SubItems.Add(IntToStr(GetResponseValue(item, i)));
        end;
      finally
        S.Free;
      end;
      nbDetails.PageIndex := 1;
    end
    else
    begin
      nbDetails.PageIndex := 2;
      reFreeForm.Lines.Text := DecodeString(item.Responses);
    end;
  end;
end;

procedure TfrmMain.tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TfrmMain.UpdateViews(Node: TTreeNode);
begin
  isSurveyType.ImageIndex := -1;
  if Node <> nil then
  begin
    pnlTop.Caption := '  ' + Node.Text;
    if Node.Parent <> nil then
      isSurveyType.ImageIndex := Ord(IJvSurveyItem(Node.Data).SurveyType)
  end
  else
    pnlTop.Caption := '  ' + Application.Title;
  // TODO: update statusbar (filename , required, surveytype)
end;

procedure TfrmMain.tvItemsChange(Sender: TObject; Node: TTreeNode);
begin
  LoadData(Node);
end;

procedure TfrmMain.acAddResponseExecute(Sender: TObject);
var i:integer;
begin
  OpenSurveyDialog.Filter := SResponseFileFilter;
//  OpenSurveyDialog.Filename := Filename;
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
      Exception.CreateFmt('The file (%s) is not compatible with the currently loaded survey: please select another response file.',
        [Filename]);
  if FResponses.IndexOf(ASurvey.SurveyTaker.ID) > -1 then
    raise Exception.CreateFmt('Responses from this file (%s) or user (%s) has already been added to the report.',[Filename,ASurvey.SurveyTaker.ID]);
  FResponses.Add(ASurvey.SurveyTaker.ID);
  for i := 0 to ASurvey.Items.Count - 1 do
    AddResponses(ASurvey.Items[i], i);
end;

procedure TfrmMain.AddResponses(item: IJvSurveyItem; Index: integer);
var
  S: string;

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
    raise Exception.CreateFmt('SurveyTypes does not match (index %d)', [Index]);
  if FSurvey.Items[Index].SurveyType = stFreeForm then
  begin
    S := trim(FSurvey.Items[Index].Responses);
    if S = '' then
      S := trim(FSurvey.Items[Index].Responses)
    else if trim(FSurvey.Items[Index].Responses) <> '' then
      S := S + '\n===========================\n' + trim(FSurvey.Items[Index].Responses);
    FSurvey.Items[Index].Responses := S;
  end
  else
    MergeResponses(FSurvey.Items[Index], item);
end;

procedure TfrmMain.acPrinterSettingsExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TfrmMain.acPrintPreviewExecute(Sender: TObject);
var
  S: string;
begin
  S := ExtractFilePath(Application.ExeName) + 'SurveyTemplate.htt';
  if not FileExists(S) then
    raise Exception.CreateFmt('Unable to find print template (%s)', [S]);
  ppPrintPreview.HTMLFile := S;
  // generate and save report HTML file
  with TStringlist.Create do
  try
    Text := ppPrintPreview.Content;
    S := ChangeFileExt(S, '.htm');
    SaveToFile(S);
    // open in browser
    OpenObject(S);
  finally
    Free;
  end;

end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  MessageBox(GetFocus, PChar('JEDI Surveyor Reporter, version 1.0'), PChar('About...'), MB_OK or MB_ICONINFORMATION);
end;

procedure TfrmMain.acSaveReportExecute(Sender: TObject);
begin
  if SaveReportDialog.Execute then
    SaveReport(SaveReportDialog.Filename);
end;

procedure TfrmMain.SaveReport(const Filename: string);
//var
//  i: integer;
//  X: TJvSimpleXML;
//  elem:TJvSimpleXMLElem;
begin
  FSurvey.SurveyTaker.UserName := '';
  FSurvey.SurveyTaker.MailAddress := '';
  // save all loaded respones as  a comma-separated lsit
  FSurvey.SurveyTaker.ID := FResponses.CommaText;
  FSurvey.SaveToFile(Filename);
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
  Result := Format(
    '<table class="TableSurveySummary">' +
    '<tr class="TRSurveySummary"><th class="THSurveySummary">Title</th><th class="THSurveySummary">ReleaseDate</th>' +
    '<th class="THSurveySummary">ExpiryDate</th><th class="THSurveySummary">Responses</th><th class="THSurveySummary">Questions</th></tr>' +
    '<tr class="TRSurveySummary"><td class="TDSurveySummary">%s</td><td class="TDSurveySummary">%s&nbsp;</td>' +
    '<td class="TDSurveySummary">%s</td><td class="TDSurveySummary">%d</td><td class="TDSurveySummary">%d</td></tr></table>',
    [FSurvey.Title, DateToStr(FSurvey.ReleaseDate), DateToStr(FSurvey.ExpiryDate),
    FResponses.Count, FSurvey.Items.Count]);
end;

function TfrmMain.GetReportHTMLContent: string;
var
  i, j: integer;
  C, R: TStringlist;
begin
  if FSurvey.Items.Count = 0 then
  begin
    Result := '<h4>There are no items in this survey: nothing to display</h4>';
    Exit;
  end;
  C := TStringlist.Create;
  R := TStringlist.Create;
  try
    for i := 0 to FSurvey.Items.Count - 1 do
    begin
      FSurvey.Items[i].SortResponses;
      Result := Result + Format(
        '<h4>Question %d</h4><table class="TableSurveyItemHeader"><tr class="TRSurveyItemHeader">' +
        '<th class="THSurveyItemHeader">Title</th><th class="THSurveyItemHeader">Description</th>' +
        '<th class="THSurveyItemHeader">Type</th></tr>' +
        '<tr class="TRSurveyItemHeader"><td class="TDSurveyItemHeader">%s&nbsp;</td>' +
        '<td class="TDSurveyItemHeader">%s&nbsp;</td><td class="TDSurveyItemHeader">%s&nbsp;</td></tr></table>',
        [i + 1, FSurvey.Items[i].Title, FSurvey.Items[i].Description, EncodeType(FSurvey.Items[i].SurveyType)]);
      C.Text := DecodeChoice(FSurvey.Items[i].Choices, FSurvey.Items[i].SurveyType);
      R.Text := DecodeResponse(FSurvey.Items[i].Responses, FSurvey.Items[i].SurveyType);
      if FSurvey.Items[i].SurveyType = stFreeForm then
        Result := Result + Format('<table class="TableSurveyItemDetail">' +
          '<tr class="TRSurveyItemDetail"><th  class="THSurveyItemDetail">Responses</th></tr>' +
          '<tr class="TRSurveyItemDetail"><td class="TDSurveyItemDetail">%s&nbsp;</td></tr></table>', [trim(R.Text)])
      else
      begin
        while C.Count > R.Count do
          R.Add('&nbsp;');
        Result := Result + '<table class="TableSurveyItemDetail"><tr class="TRSurveyItemDetail">' +
          '<th class="THSurveyItemDetail">Choices</th><th class="THSurveyItemDetail">Responses</th></tr>';
        for j := 0 to C.Count - 1 do
          Result := Result + Format('<tr  class="TRSurveyItemDetail"><td  class="TDSurveyItemDetail">%s&nbsp;</td>' +
            '<td  class="TDSurveyItemDetail">%s&nbsp;</td></tr>', [C[j], R[j]]);
        Result := Result + '</table>';
      end;
    end;
  finally
    R.Free;
    C.Free;
  end;
end;

procedure TfrmMain.ppPrintPreviewHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: string; TagParams: TStrings; var ReplaceText: string);
begin
  if AnsiSameText(TagString, 'SURVEYTITLE') then
    ReplaceText := FSurvey.Title
  else if AnsiSameText(TagString, 'SURVEYSUMMARY') then
    ReplaceText := GetReportHTMLSummary
  else if AnsiSameText(TagString, 'SURVEYCONTENT') then
    ReplaceText := GetReportHTMLContent;
end;

end.

